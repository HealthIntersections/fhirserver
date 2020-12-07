unit fhir_pathengine;

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
  SysUtils, Classes, Character,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_objects;

type

  TFHIRTypeDetailsV = class (TFslObject)
  public
    function link : TFHIRTypeDetailsV;
  end;

  TFHIRPathExpressionNodeV = class;
  TTreeDataPointer = record
    expr : TFHIRPathExpressionNodeV;
    isOp : boolean;
  end;
  PTreeDataPointer = ^TTreeDataPointer;

  {$IFNDEF FPC}
  TFHIRPathExpressionNodeVisitProc = reference to procedure(context : pointer; item : TFHIRPathExpressionNodeV);
  {$ENDIF}

  TFHIRPathExpressionNodeV = class (TFslObject)
  private
    FTag : integer;
  protected
    FUniqueId : integer;
    function sizeInBytesV : cardinal; override;
  public
    function link : TFHIRPathExpressionNodeV;
    property tag : integer read FTag write FTag;
    property uniqueId : integer read FUniqueId;

    function summary : String; virtual; abstract;

    {$IFNDEF FPC}
    procedure visitAll(context : pointer; proc : TFHIRPathExpressionNodeVisitProc); virtual; abstract;
    {$ENDIF}
    function nodeOpName : String; virtual; abstract;
    function nodeName : String; virtual; abstract;
    function nodeChildCount : integer; virtual; abstract;
    function nodeOpNext : TFHIRPathExpressionNodeV; virtual; abstract;
    function nodeGetChild(nodeIndex : integer; var offset : integer) : TFHIRPathExpressionNodeV; virtual; abstract;
  end;

  TFHIRPathVersion = (fpV1, fpV2);

  TFHIRPathLexer = class abstract (TFslObject)
  private
    FVersion : TFHIRPathVersion;
    FCursor : integer;
    FCurrentLocation : TSourceLocation;
    FCurrent : String;
    FCurrentStart : integer;
    FCurrentStartLocation : TSourceLocation;
    FId : integer;
    FPath : String;
    FSourceName : String;
    flast13 : boolean;
    FComments : TStringList;

    FMarkedCursor : integer;
    FMarkedCurrentLocation : TSourceLocation;
    FMarkedCurrent : String;
    FMarkedCurrentStart : integer;
    FMarkedCurrentStartLocation : TSourceLocation;
    FMarkedLast13 : boolean;

    procedure nextChar;
    procedure prevChar;
  protected
    function collapseEmptyLists : boolean; virtual;
    function opCodes : TArray<String>; virtual; abstract;
  public
    constructor Create(ver : TFHIRPathVersion; path : String); overload;
    constructor Create(ver : TFHIRPathVersion; path : String; offset : integer); overload;
    destructor Destroy; override;
    procedure next; virtual;
    property Cursor : integer read FCursor;
    property current : String read FCurrent write FCurrent;
    property CurrentStart : integer read FCurrentStart;
    function done : boolean;
    function take : String;

    procedure mark;
    procedure revert;

    function hasComment: boolean;
    procedure skipWhitespaceAndComments;
    procedure checkArithmeticPrefixes;

    function nextId : integer;
    function error(msg : String) : Exception; overload;
    function error(msg : String; offset : integer) : Exception; overload;
    function error(msg : String; location : TSourceLocation) : Exception; overload;

    function isConstant(incDoubleQuotes : boolean = false) : boolean;
    function isStringConstant: boolean;
    function readConstant(desc: String): String;
    function readIdentifier(desc : String) : String;
    class function processConstant(s: String): String; overload;
    class function endingToken(s : String) : boolean;

    function isOp : boolean;
    function isNumericalConstant : boolean;
    function isToken : boolean; overload;
    function isNameToken : boolean;
    function isUnit() : boolean;
    function hasToken(kw : String) : boolean; overload;
    function hasToken(kw : Array of String) : boolean; overload;
    function takeToken(kw : String) : boolean; overload;
    procedure token(kw : String); overload;
    procedure skiptoken(kw : String); overload;
    function takeDottedToken: String;
    function readTo(token : char; include : boolean) : String;
    function readToWS : String;
    property CurrentLocation : TSourceLocation read FCurrentLocation;
    property CurrentStartLocation : TSourceLocation read FCurrentStartLocation;
    property path : String read FPath;
    function processConstant : TFHIRObject; overload; virtual; abstract;

    property SourceName : String read FSourceName write FSourceName;
  end;

  TFHIRPathExecutionContext = class (TFslObject)
  private
    FAppInfo : TFslObject;
    FResource : TFHIRObject;
    FContext : TFHIRObject;
    FTotal : TFHIRSelectionList;
     FThis : TFHIRObject;
    procedure SetTotal(const Value: TFHIRSelectionList);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(appInfo : TFslObject; resource : TFHIRObject; context : TFHIRObject);
    destructor Destroy; override;
    function Link : TFHIRPathExecutionContext; overload;
    property appInfo : TFslObject read FappInfo;
    property resource : TFHIRObject read FResource;
    property context : TFHIRObject read Fcontext;
    property total : TFHIRSelectionList read FTotal write SetTotal;
    property this : TFHIRObject read FThis write FThis;
    function changeThis(this : TFHIRObject) : TFHIRPathExecutionContext;
  end;

  TFHIRPathDebugPackage = class (TFslObject)
  private
    FSourceEnd: TSourceLocation;
    Fcontext: TFHIRPathExecutionContext;
    Finput2: TFHIRSelectionList;
    Finput1: TFHIRSelectionList;
    FExpression: TFHIRPathExpressionNodeV;
    FSourceStart: TSourceLocation;
    Foutcome: TFHIRSelectionList;
    FIsOperation: boolean;
    procedure Setcontext(const Value: TFHIRPathExecutionContext);
    procedure SetExpression(const Value: TFHIRPathExpressionNodeV);
    procedure Setinput1(const Value: TFHIRSelectionList);
    procedure Setinput2(const Value: TFHIRSelectionList);
    procedure Setoutcome(const Value: TFHIRSelectionList);
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    function Link : TFHIRPathDebugPackage; overload;
    property SourceStart : TSourceLocation read FSourceStart write FSourceStart;
    property SourceEnd : TSourceLocation read FSourceEnd write FSourceEnd;
    property Expression : TFHIRPathExpressionNodeV read FExpression write SetExpression;
    property IsOperation : boolean read FIsOperation write FIsOperation;
    property context : TFHIRPathExecutionContext read Fcontext write Setcontext;
    property input1 : TFHIRSelectionList read Finput1 write Setinput1;
    property input2 : TFHIRSelectionList read Finput2 write Setinput2;
    property outcome : TFHIRSelectionList read Foutcome write Setoutcome;
  end;

  TFHIRPathEngineV = class;

  TFHIRPathDebugEvent = procedure (source : TFHIRPathEngineV; package : TFHIRPathDebugPackage) of object;
  TFHIRResolveReferenceEvent = function (source : TFHIRPathEngineV; appInfo : TFslObject; url : String) : TFHIRObject of object;

  TFHIRPathEngineExtension = class abstract (TFslObject)
  protected
    function executeV(engine : TFHIRPathEngineV; context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNodeV; atEntry : boolean) : TFHIRSelectionList; overload; virtual;
    function executeV(engine : TFHIRPathEngineV; context : TFHIRPathExecutionContext; item : TFHIRObject; exp : TFHIRPathExpressionNodeV; atEntry : boolean) : TFHIRSelectionList; overload; virtual;
  public
    function resolveConstant(context : TFHIRPathExecutionContext; s : String; var obj : TFHIRObject) : boolean; virtual; abstract;
    function isValidFunction(name : String) : boolean; virtual; abstract;
    function functionApplies(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; name : String): boolean; virtual; abstract;
    function execute(context : TFHIRPathExecutionContext; focus: TFHIRObject; name : String; params : TFslList<TFHIRPathExpressionNodeV>; engine : TFHIRPathEngineV): TFHIRSelectionList; virtual; abstract;
  end;

  TFHIRPathEngineV = class (TFslObject)
  private
    FOndebug : TFHIRPathDebugEvent;
    FAllowPolymorphicNames: boolean;
    function findPath(path : String; loc : TSourceLocation; context : TArray<TFHIRObject>; base : TFHIRObject; var focus : TArray<TFHIRObject>) : String;
  protected
    FExtensions : TFslList<TFHIRPathEngineExtension>;
    FOnResolveReference: TFHIRResolveReferenceEvent;
    function executeV(context : TFHIRPathExecutionContext; focus : TFHIRSelectionList; exp : TFHIRPathExpressionNodeV; atEntry : boolean) : TFHIRSelectionList; overload; virtual; abstract;
    function executeV(context : TFHIRPathExecutionContext; item : TFHIRObject; exp : TFHIRPathExpressionNodeV; atEntry : boolean) : TFHIRSelectionList; overload; virtual; abstract;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRPathEngineV; overload;

    procedure registerExtension(extension : TFHIRPathEngineExtension);
    property Ondebug : TFHIRPathDebugEvent read FOndebug write FOndebug;
    function parseV(source : String) : TFHIRPathExpressionNodeV; virtual; abstract;
    function evaluate(appInfo : TFslObject; base : TFHIRObject; path : String) : TFHIRSelectionList; overload; virtual; abstract;
    function evaluate(appInfo : TFslObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : TFHIRSelectionList; overload; virtual; abstract;
    function evaluate(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; path : String) : TFHIRSelectionList; overload; virtual; abstract;
    function evaluate(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : TFHIRSelectionList; overload; virtual; abstract;
    function evaluateToBoolean(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; path : String) : boolean; overload; virtual; abstract;
    function evaluateToBoolean(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : boolean; overload; virtual; abstract;
    function evaluateToString(appInfo : TFslObject; base : TFHIRObject; path : String) : string; overload; virtual; abstract;
    function evaluateToString(appInfo : TFslObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : string; overload; virtual; abstract;
    function convertToString(list : TFHIRSelectionList) : string; overload; virtual; abstract;
    function check(appInfo : TFslObject; resourceType, context, path : String; expr : TFHIRPathExpressionNodeV; xPathStartsWithValueRef : boolean) : TFHIRTypeDetailsV; overload; virtual; abstract;

    function extractPath(pathBase : String; loc : TSourceLocation; base : TFHIRObject) : String; overload;
    function extractPath(pathBase : String; loc : TSourceLocation; base : TFHIRObject; var pathObjects : TArray<TFHIRObject>) : String; overload;
    property OnResolveReference : TFHIRResolveReferenceEvent read FOnResolveReference write FOnResolveReference;

    property allowPolymorphicNames : boolean read FAllowPolymorphicNames write FAllowPolymorphicNames;
  end;

const
  ID_CHAR : array [TFHIRPathVersion] of char = ('"', '`');

implementation


{ TFHIRPathEngineV }

function TFHIRPathEngineV.extractPath(pathBase: String; loc: TSourceLocation; base: TFHIRObject): String;
var
  pathObjects : TArray<TFHIRObject>;
begin
  result := findPath(pathBase, loc, nil, base, pathObjects);
end;

constructor TFHIRPathEngineV.Create;
begin
  inherited;
  FExtensions := TFslList<TFHIRPathEngineExtension>.create;
end;

destructor TFHIRPathEngineV.Destroy;
begin
  FExtensions.Free;
  inherited;
end;

function TFHIRPathEngineV.extractPath(pathBase: String; loc: TSourceLocation; base: TFHIRObject; var pathObjects: TArray<TFHIRObject>): String;
begin
  result := findPath(pathBase, loc, nil, base, pathObjects);
end;

function TFHIRPathEngineV.findPath(path: String; loc: TSourceLocation; context: TArray<TFHIRObject>; base: TFHIRObject; var focus: TArray<TFHIRObject>): String;
var
  i, j : integer;
  pl : TFHIRPropertyList;
  p : TFHIRProperty;
  list : TArray<TFHIRObject>;
begin
  setlength(list, length(context) + 1);
  for i := 0 to length(context) - 1 do
    list[i] := context[i];
  list[length(list)-1] := base;

  if loc >= base.LocationData.parseFinish then
  begin
    result := path;
    focus := list;
  end
  else
  begin
    result := '';
    pl := base.createPropertyList(false);
    try
      for i := pl.Count - 1 downto 0 do
      begin
        p := pl[i];
        if (p.hasValue) then
        begin
          if loc >= p.Values[0].LocationData.ParseStart then
          begin
            path := path + '.'+p.Name;
            if p.IsList then
            begin
              for j := p.Values.Count - 1 downto 0 do
                if (result = '') and (loc >= p.Values[j].LocationData.ParseStart) then
                  result := findPath(path+'['+inttostr(j)+']', loc, list, p.Values[j], focus);
            end
            else
              result := findPath(path, loc, list, p.Values[0], focus);
            break;
          end;
        end;
      end;
    finally
      pl.Free;
    end;
  end;
  if (result = '') and (loc >= base.LocationData.ParseStart) and (loc <= base.LocationData.ParseFinish)  then
  begin
    result := path;
    focus := list;
  end;
end;

function TFHIRPathEngineV.link: TFHIRPathEngineV;
begin
  result := TFHIRPathEngineV(inherited link);
end;

procedure TFHIRPathEngineV.registerExtension(extension: TFHIRPathEngineExtension);
begin
  FExtensions.Add(extension);
end;

function TFHIRPathEngineV.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FExtensions.sizeInBytes);
end;

{ TFHIRPathExpressionNodeV }

function TFHIRPathExpressionNodeV.link: TFHIRPathExpressionNodeV;
begin
  result := TFHIRPathExpressionNodeV(inherited link);
end;

function TFHIRPathExpressionNodeV.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TFHIRPathExecutionContext }

constructor TFHIRPathExecutionContext.Create(appInfo: TFslObject; resource: TFHIRObject; context: TFHIRObject);
begin
  inherited Create;
  FAppInfo := appInfo;
  FResource := resource;
  FContext := context;
end;

destructor TFHIRPathExecutionContext.Destroy;
begin
  FAppInfo.Free;
  FResource.Free;
  FContext.Free;
  inherited;
end;

function TFHIRPathExecutionContext.Link: TFHIRPathExecutionContext;
begin
  result := TFHIRPathExecutionContext(inherited Link);
end;

function TFHIRPathExecutionContext.changeThis(this: TFHIRObject): TFHIRPathExecutionContext;
begin
  result := TFHIRPathExecutionContext.Create(FAppinfo.Link, FResource.Link, FContext.Link);
  try
    result.FThis := this;
    result.total := FTotal.Link;
    result.link;
  finally
    result.free;
  end;
end;

procedure TFHIRPathExecutionContext.SetTotal(const Value: TFHIRSelectionList);
begin
  FTotal.free;
  FTotal := Value;
end;

function TFHIRPathExecutionContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAppInfo.sizeInBytes);
  inc(result, FResource.sizeInBytes);
  inc(result, FContext.sizeInBytes);
  inc(result, FTotal.sizeInBytes);
  inc(result, FThis.sizeInBytes);
end;

{ TFHIRPathDebugPackage }

destructor TFHIRPathDebugPackage.destroy;
begin
  Fcontext.Free;
  Finput2.Free;
  Finput1.Free;
  FExpression.Free;
  Foutcome.Free;
  inherited;
end;

function TFHIRPathDebugPackage.Link: TFHIRPathDebugPackage;
begin
  result := TFHIRPathDebugPackage(inherited Link);
end;

procedure TFHIRPathDebugPackage.Setcontext(const Value: TFHIRPathExecutionContext);
begin
  Fcontext.Free;
  Fcontext := Value;
end;

procedure TFHIRPathDebugPackage.SetExpression(const Value: TFHIRPathExpressionNodeV);
begin
  FExpression.Free;
  FExpression := Value;
end;

procedure TFHIRPathDebugPackage.Setinput1(const Value: TFHIRSelectionList);
begin
  Finput1.Free;
  Finput1 := Value;
end;

procedure TFHIRPathDebugPackage.Setinput2(const Value: TFHIRSelectionList);
begin
  Finput2.Free;
  Finput2 := Value;
end;

procedure TFHIRPathDebugPackage.Setoutcome(const Value: TFHIRSelectionList);
begin
  Foutcome.Free;
  Foutcome := Value;
end;


function TFHIRPathDebugPackage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcontext.sizeInBytes);
  inc(result, Finput2.sizeInBytes);
  inc(result, Finput1.sizeInBytes);
  inc(result, FExpression.sizeInBytes);
  inc(result, Foutcome.sizeInBytes);
end;

{ TFHIRTypeDetailsV }

function TFHIRTypeDetailsV.link: TFHIRTypeDetailsV;
begin
  result := TFHIRTypeDetailsV(inherited Link);
end;


{ TFHIRPathLexer }

procedure TFHIRPathLexer.prevChar;
begin
  dec(FCursor);
  dec(FCurrentLocation.col);
end;

class function TFHIRPathLexer.processConstant(s: String) : String;
var
  b : TStringBuilder;
  i : integer;
  ch : char;
  u : String;
begin
  b := TStringBuilder.Create;
  try
    i := 2;
    while i < length(s) do
    begin
      ch := s[i];
      if ch = '\' then
      begin
        inc(i);
        ch := s[i];
        case ch of
          't': b.Append(#9);
          'r': b.Append(#13);
          'n': b.Append(#10);
          'f': b.Append(#12);
          '\': b.Append('\');
          '/': b.Append('/');
          '''': b.Append('''');
          '"': b.Append('"');
          '`': b.Append('`');
          'u':
            begin
            if i < length(s) - 4 then
            begin
              u := s.Substring(i, 4);
              b.Append(char(StrToInt('$'+u)));
              inc(i,4);
            end
            else
              raise EFHIRException.create('Improper unicode escape in '+s);
            end
        else
          raise EFHIRException.create('Unknown character escape \'+ch);
        end;
        inc(i);
      end
      else
      begin
        b.Append(ch);
        inc(i);
      end;
    end;
    result := b.toString;
  finally
    b.Free;
  end;
end;

procedure TFHIRPathLexer.checkArithmeticPrefixes;
begin
  if (Current = '-') and CharInSet(FPath[FCursor], ['0'..'9']) then
  begin
    take;
    Fcurrent := '-' + Fcurrent;
  end;

  if (Current = '+') and CharInSet(FPath[FCursor], ['0'..'9']) then
  begin
    take;
    Fcurrent := '+' + Fcurrent;
  end;
end;

function TFHIRPathLexer.collapseEmptyLists: boolean;
begin
  result := true;
end;

constructor TFHIRPathLexer.Create(ver : TFHIRPathVersion; path: String; offset: integer);
begin
  inherited Create;
  FVersion := ver;
  FPath := path;
  FCursor := offset;
  FCurrentLocation := TSourceLocation.Create;
  FComments := TStringList.create;
  next;
end;

constructor TFHIRPathLexer.Create(ver : TFHIRPathVersion; path: String);
begin
  inherited Create;
  FVersion := ver;
  FPath := path;
  FCursor := 1;
  FCurrentLocation := TSourceLocation.Create;
  FComments := TStringList.create;
  next;
end;

destructor TFHIRPathLexer.Destroy;
begin
  FComments.Free;
  inherited;
end;

function isWhitespace(ch : char) : Boolean;
begin
  result := CharInSet(ch, [#9, #10, #13, ' ']);
end;

function isDateChar(ch : char) : Boolean;
begin
 result := CharInSet(ch, ['-', ':', 'T', '+', 'Z', '0'..'9', '.']);
end;

procedure TFHIRPathLexer.nextChar();
begin
  if FPath[FCursor] = #13 then
  begin
    FCurrentLocation.incLine;
    flast13 := true;
  end
  else if not flast13 and (FPath[FCursor] = #10) then
  begin
    FCurrentLocation.incLine;
    flast13 := false;
  end
  else
  begin
    flast13 := false;
    FCurrentLocation.incCol;
  end;
  inc(FCursor);
end;

procedure TFHIRPathLexer.next;
  procedure Grab(length : Integer);
  var
    i : integer;
  begin
    FCurrent := copy(FPath, FCurrentStart, length);
    for i := 1 to length do
      nextChar;
  end;
var
  ch : char;
  escape, dotted : boolean;
begin
  FCurrent := '';
  skipWhitespaceAndComments;
  FCurrentStart := FCursor;
  FCurrentStartLocation := FCurrentLocation;
  if (FCursor <= FPath.Length) then
  begin
    ch := FPath[FCursor];
    if charInSet(ch, ['!', '>', '<', ':', '=', '-']) then
    begin
      if (FCursor < FPath.Length) and (charInSet(FPath[FCursor+1], ['=', '~', '-']) or ((ch = '-') and (FPath[FCursor+1] = '>'))) then
        Grab(2)
      else
        Grab(1);
    end
    else if CharInSet(ch, ['0'..'9']) then
    begin
      nextChar;
      dotted := false;
      while (FCursor <= FPath.Length) and (CharInSet(FPath[FCursor], ['0'..'9']) or (not dotted and (FPath[FCursor] = '.'))) do
      begin
        if (FPath[FCursor] = '.') then
          dotted := true;
        nextChar;
      end;
      if (FPath[FCursor-1] = '.') then
        prevChar;
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if CharInSet(ch, ['A'..'Z', 'a'..'z']) then
    begin
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do
        nextChar;
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '%') then
    begin
      nextChar;
      if (FPath[FCursor] = ID_CHAR[FVersion]) then
      begin
        nextChar;
        while (FCursor <= FPath.Length) and (FPath[FCursor] <> ID_CHAR[FVersion]) do
          nextChar;
        nextChar;
      end
      else
        while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['A'..'Z', 'a'..'z', '0'..'9', ':', '-']) do
          nextChar;
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '/') then
    begin
      nextChar;
      if (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['*']) then
      begin
        while (FCursor <= FPath.Length) and not ((FPath[FCursor-1] = '*') and (FPath[FCursor] = '/'))  do
          nextChar;
        if (FCursor <= FPath.Length) then
          nextChar;
      end
      else if (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['/']) then
      begin
        nextChar;
        while (FCursor <= FPath.Length) and not CharInSet(FPath[FCursor], [#13, #10]) do
          nextChar;
      end;
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '$') then
    begin
      nextChar;
      while (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['a'..'z']) do
        nextChar;
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '{') then
    begin
      nextChar;
      if (FCursor <= FPath.Length) and CharInSet(FPath[FCursor], ['}']) and collapseEmptyLists then
        nextChar;
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else if (ch = '"') then
    begin
      nextChar;
      escape := false;
      while (FCursor <= FPath.length) and (escape or (FPath[FCursor] <> '"')) do
      begin
        if (escape) then
          escape := false
        else
          escape := (FPath[FCursor] = '\');
        if CharInSet(FPath[FCursor], [#13, #10, #9]) then
          raise EFHIRPath.create('illegal character in string');
        nextChar;
      end;
      if (FCursor > FPath.length) then
        raise error('Unterminated string');
      nextChar;
      FCurrent := '"'+copy(FPath, FCurrentStart+1, FCursor-FCurrentStart-2)+'"';
    end
    else if (ch = '`') and (FVersion = fpV2) then
    begin
      nextChar;
      escape := false;
      while (FCursor <= FPath.length) and (escape or (FPath[FCursor] <> '`')) do
      begin
        if (escape) then
          escape := false
        else
          escape := (FPath[FCursor] = '\');
        if CharInSet(FPath[FCursor], [#13, #10, #9]) then
          raise EFHIRPath.create('illegal character in string');
        nextChar;
      end;
      if (FCursor > FPath.length) then
        raise error('Unterminated string');
      nextChar;
      FCurrent := '`'+copy(FPath, FCurrentStart+1, FCursor-FCurrentStart-2)+'`';
    end
    else if (ch = '''') then
    begin
      nextChar;
      escape := false;
      while (FCursor <= FPath.length) and (escape or (FPath[FCursor] <> ch)) do
      begin
        if (escape) then
          escape := false
        else
          escape := (FPath[FCursor] = '\');
        if CharInSet(FPath[FCursor], [#13, #10, #9]) then
          raise EFHIRPath.create('illegal character in string');
        nextChar;
      end;
      if (FCursor > FPath.length) then
        raise error('Unterminated string');
      nextChar;
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
      FCurrent := ''''+copy(FCurrent, 2, FCurrent.Length - 2)+'''';
    end
    else if (ch = '@') then
    begin
      nextChar;
      while (FCursor <= FPath.length) and isDateChar(FPath[FCursor]) do
        nextChar;
      if (FPath[FCursor-1] = '.') then
        prevChar;
      FCurrent := copy(FPath, FCurrentStart, FCursor-FCurrentStart);
    end
    else // if CharInSet(ch, ['.', ',', '(', ')', '=']) then
      Grab(1);
  end;
end;


function TFHIRPathLexer.nextId: integer;
begin
  inc(FId);
  result := FId;
end;

function TFHIRPathLexer.hasComment : boolean;
begin
  result := not done() and (FCurrent.startsWith('//') or (FCurrent.startsWith('/*')));
end;

function TFHIRPathLexer.hasToken(kw: array of String): boolean;
var
  s : String;
begin
  result := false;
  for s in kw do
    if hasToken(s) then
      exit(true);
end;

procedure TFHIRPathLexer.skipWhitespaceAndComments;
var
  last13 : boolean;
  done : boolean;
  start : integer;
  s : String;
begin
  FComments.clear();
  last13 := false;
  done := false;
  while (FCursor <= FPath.length) and not done do
  begin
    if (FCursor < FPath.length) and ('//' = FPath.substring(FCursor-1, 2)) then
    begin
      start := FCursor + 2;
      while (FCursor <= FPath.length) and not CharInSet(FPath[cursor], [#13, #10]) do
        inc(FCursor);
      s := FPath.Substring(start, FCursor-start).trim();
      FComments.add(s);
    end
    else if (FCursor < FPath.length) and ('/*' = FPath.substring(cursor-1, 2)) then
    begin
      start := FCursor + 2;
      while (FCursor <= FPath.length) and ('*/' <> FPath.substring(cursor-1, 2)) do
      begin
        last13 := FCurrentLocation.checkChar(FPath[cursor], last13);
        inc(fCursor);
      end;
      if (FCursor > FPath.length) then
      begin
        raise EFHIRPath.create('Unfinished comment');
      end
      else
      begin
        FComments.add(FPath.substring(start, FCursor).trim());
        FCursor := FCursor + 2;
      end
    end
    else if (TCharacter.isWhitespace(FPath[FCursor])) then
    begin
      last13 := FCurrentLocation.checkChar(FPath[cursor], last13);
      inc(fCursor);
    end
    else
    begin
      done := true;
    end;
  end;
end;

procedure TFHIRPathLexer.skiptoken(kw: String);
begin
  if (kw = current) then
    next();
end;

function TFHIRPathLexer.done: boolean;
begin
  result := FCurrentStart > FPath.Length;
end;

class function TFHIRPathLexer.endingToken(s: String): boolean;
begin
  result := StringArrayExistsSensitive([']', ')', '}', ','], s)
end;

function TFHIRPathLexer.error(msg: String; location: TSourceLocation): Exception;
begin
  result := location.exception('Error "'+msg+'"');
end;

function TFHIRPathLexer.error(msg: String; offset: integer): Exception;
begin
  result := EFHIRPath.Create('Error "'+msg+'" at '+inttostr(offset)+' in "'+FPath+'"');
end;

function TFHIRPathLexer.error(msg: String): Exception;
begin
  result := error(msg, FCurrentLocation);
end;

function TFHIRPathLexer.isConstant(incDoubleQuotes : boolean): boolean;
begin
  if FCurrent = '' then
    result := false
  else if (FCurrent[1] = '''') or (FCurrent[1] = '%') or (FCurrent[1] = '@') then
    result := true
  else if incDoubleQuotes and (FCurrent[1] = '"') then
    result := true
  else if StringIsDecimal(FCurrent) then
    result := true
  else
    result := StringArrayExistsSensitive(['true', 'false', '{}'], FCurrent);
end;

function TFHIRPathLexer.isNameToken: boolean;
var
  i : integer;
begin
  result := false;
  if isToken and CharInSet(FCurrent[1], ['a'..'z', 'A'..'Z']) then
  begin
    result := true;
    for i := 2 to length(FCurrent) do
      if not CharInSet(FCurrent[i], ['a'..'z', 'A'..'Z', '0'..'9', '_']) then
        exit(false);
  end;
end;

function TFHIRPathLexer.isNumericalConstant: boolean;
begin
  result := (FCurrent <> '') and StringIsDecimal(FCurrent);
end;

function TFHIRPathLexer.isOp: boolean;
begin
  if current = '' then
    result := false
  else
    result := StringArrayExistsSensitive(opCodes, current);
end;

function TFHIRPathLexer.hasToken(kw: String): boolean;
begin
  result := not done() and (kw = current);
end;

function TFHIRPathLexer.isToken: boolean;
var
  i : integer;
begin
  if current = '' then
    result := false
  else if current.StartsWith('$') then
    result := true
  else if StringArrayExistsSensitive(['*', '**'], current) then
    result := true
  else if CharInSet(current[1], ['A'..'Z', 'a'..'z']) then
  begin
    result := true;
    for i := 1 to length(current) do
      result := result and (CharInSet(current[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']) or ((i = current.Length) and (current[i] = '*')));
  end
  else
    result := false;
end;

function TFHIRPathLexer.isUnit(): boolean;
begin
  result := StringArrayExistsSensitive(['year', 'month', 'week', 'day', 'hour', 'minute', 'second', 'millisecond'], current)
         or StringArrayExistsSensitive(['years', 'months', 'weeks', 'days', 'hours', 'minutes', 'seconds', 'milliseconds'], current);
end;

procedure TFHIRPathLexer.mark;
begin
  FMarkedCursor := FCursor;
  FMarkedCurrentLocation := FCurrentLocation;
  FMarkedCurrent := FCurrent;
  FMarkedCurrentStart := FCurrentStart;
  FMarkedCurrentStartLocation := FCurrentStartLocation;
  FMarkedLast13 := flast13;
end;

function TFHIRPathLexer.take: String;
begin
  result := current;
  next;
end;

procedure TFHIRPathLexer.token(kw: String);
begin
  if (kw <> current) then
    raise error('Found "'+current+'" expecting "'+kw+'"');
  next();
end;

function TFHIRPathLexer.readConstant(desc : String): String;
begin
  if (not isStringConstant()) then
    raise error('Found '+current+' expecting "['+desc+']"');

  result := processConstant(take);
end;

function TFHIRPathLexer.readIdentifier(desc: String): String;
begin
  if (FVersion = fpV1) and (current.startsWith('"')) then
    result := readConstant(desc)
  else if (FVersion = fpV2) and (current.startsWith('`')) then
    result := readConstant(desc)
  else
    result := take;
end;

function TFHIRPathLexer.readTo(token: char; include: boolean): String;
begin
  result := Current;
  repeat
    result := result + FPath[FCursor];
    nextChar;
  until done or (FPath[FCursor] = token);
  if include then
    result := result + FPath[FCursor];
  nextChar;
  next;
end;

function TFHIRPathLexer.readToWS: String;
begin
  result := Current;
  repeat
    result := result + FPath[FCursor];
    nextChar;
  until done or isWhitespace(FPath[FCursor]);
  next;
end;

procedure TFHIRPathLexer.revert;
begin
  FCursor := FMarkedCursor;
  FCurrentLocation := FMarkedCurrentLocation;
  FCurrent := FMarkedCurrent;
  FCurrentStart := FMarkedCurrentStart;
  FCurrentStartLocation := FMarkedCurrentStartLocation;
  flast13 := FMarkedLast13;
end;

function TFHIRPathLexer.isStringConstant : boolean;
begin
  result := (current[1] = '''') or (current[1] = '"') or (current[1] = '`');
end;


function TFHIRPathLexer.takeDottedToken() : String;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.create;
  try
    b.append(take());
    while not done() and (FCurrent = '.') do
    begin
      b.append(take());
      b.append(take());
    end;
    result := b.toString();
  finally
    b.free;
  end;
end;



function TFHIRPathLexer.takeToken(kw: String): boolean;
begin
  result := not done() and (kw = current);
  if result then
    token(kw);
end;

{ TFHIRPathEngineExtension }

function TFHIRPathEngineExtension.executeV(engine: TFHIRPathEngineV; context: TFHIRPathExecutionContext; item: TFHIRObject; exp: TFHIRPathExpressionNodeV; atEntry: boolean): TFHIRSelectionList;
begin
  result := engine.executeV(context, item, exp, atEntry);
end;

function TFHIRPathEngineExtension.executeV(engine: TFHIRPathEngineV; context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; exp: TFHIRPathExpressionNodeV; atEntry: boolean): TFHIRSelectionList;
begin
  result := engine.executeV(context, focus, exp, atEntry);
end;

End.
