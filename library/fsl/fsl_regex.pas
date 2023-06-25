unit fsl_regex;

interface

uses
  SysUtils, Classes,
  {$IFDEF FPC} RegExpr, {$ELSE} System.RegularExpressions{$ENDIF},
  fsl_base;

type
  {$IFDEF FPC}
  TRegExOption = (roNone, roIgnoreCase, roMultiLine, roExplicitCapture, roCompiled, roSingleLine, roIgnorePatternSpace, roNotEmpty);
  TRegExOptions = set of TRegExOption;
  {$ELSE}
  TRegExOption = System.RegularExpressions.TRegExOption;
  TRegExOptions = System.RegularExpressions.TRegExOptions;

const
  roNone = System.RegularExpressions.roNone;
  roIgnoreCase = System.RegularExpressions.roIgnoreCase;
  roMultiLine = System.RegularExpressions.roMultiLine;
  roExplicitCapture = System.RegularExpressions.roExplicitCapture;
  roCompiled = System.RegularExpressions.roCompiled;
  roSingleLine = System.RegularExpressions.roSingleLine;
  roIgnorePatternSpace = System.RegularExpressions.roIgnorePatternSpace;
  roNotEmpty = System.RegularExpressions.roNotEmpty;
  {$ENDIF}

type
  { TRegularExpression }
  TRegularExpression = class (TFslObject)
  private
    FPattern : String;
    {$IFDEF FPC}
    FImpl : (Regexpr.TRegExpr)
    {$ELSE}
    FImpl : TRegex;
    {$ENDIF}
  public
    constructor Create(const Pattern: string); overload;
    constructor Create(const Pattern: string; Options: TRegExOptions); overload;
    destructor Destroy; override;

    function IsMatch(const Input: string): Boolean; overload;
    function IsFullMatch(const Input: string): Boolean; overload;
    function replace(const input, repl: string): String; overload;
  end;

implementation

(*
constructor TRegExpression.Create(const Pattern: string; Options: TRegExOptions);
begin
  inherited Create(pattern);
end;

function TRegExpression.IsMatch(const Input: string): Boolean;
begin
  result := Exec(input);
end;

function TRegExpression.IsFullMatch(const Input: string): Boolean;
begin

end;

class function TRegExpression.isMatch(const input, pattern : string): Boolean;
var
  this : TRegEx;
begin
  this := TRegEx.create(pattern);
  try
    result := this.isMatch(input);
  finally
    this.free;
  end;
end;

class function TRegExpression.replace(const input, pattern, repl: string): String;
var
  this : TRegEx;
begin
  this := TRegEx.create(pattern);
  try
    result := this.replace(input, repl);
  finally
    this.free;
  end;
end;

          {$IFNDEF FPC}
{ TRegExpression }

class function TRegExpression.isFullMatch(const Input, Pattern: string): Boolean;
begin

end;
{$ENDIF}
*)
{ TRegularExpression }

constructor TRegularExpression.Create(const Pattern: string; Options: TRegExOptions);
begin
  inherited create;
  FPattern := pattern;
  {$IFDEF FPC}
  FImpl := inherited Create(pattern, options);
  {$ELSE}
  FImpl := TRegex.Create(pattern, options);
  {$ENDIF}
end;

constructor TRegularExpression.Create(const Pattern: string);
begin
  inherited create;
  FPattern := pattern;
  {$IFDEF FPC}
    FImpl : (Regexpr.TRegExpr)
  {$ELSE}
  FImpl := TRegex.Create(pattern);
  {$ENDIF}
end;

destructor TRegularExpression.Destroy;
begin
  {$IFDEF FPC}
  FImpl : (Regexpr.TRegExpr)
  {$ENDIF}
  inherited;
end;

function TRegularExpression.IsFullMatch(const Input: string): Boolean;
begin
  {$IFDEF FPC}
  result := FImpl.Exec(input);
  if (result) then
    result := FImpl.MatchLen[0] = input.length;
  {$ELSE}
  result := FImpl.isMatch(Input, '\A'+FPattern+'\z');
  {$ENDIF}
end;

function TRegularExpression.IsMatch(const Input: string): Boolean;
begin
  {$IFDEF FPC}
  FImpl : (Regexpr.TRegExpr)
  {$ELSE}
  result := FImpl.isMatch(Input);
  {$ENDIF}
end;

function TRegularExpression.replace(const input, repl: string): String;
begin
  {$IFDEF FPC}
  FImpl : (Regexpr.TRegExpr)
  {$ELSE}
  result := FImpl.Replace(input, repl);
  {$ENDIF}
end;

end.
