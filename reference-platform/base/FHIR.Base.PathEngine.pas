unit FHIR.Base.PathEngine;

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

interface

uses
  SysUtils,
  FHIR.Support.Objects, FHIR.Support.Text,
  FHIR.Base.Objects;

type
  TFHIRTypeDetailsV = class (TFslObject)
  public
    function link : TFHIRTypeDetailsV;
  end;

  TFHIRPathExpressionNodeV = class;

  TFHIRPathExpressionNodeVisitProc = reference to procedure(item : TFHIRPathExpressionNodeV);

  TFHIRPathExpressionNodeV = class (TFslObject)
  private
    FTag : integer;
  protected
    FUniqueId : integer;
  public
    function link : TFHIRPathExpressionNodeV;
    property tag : integer read FTag write FTag;
    property uniqueId : integer read FUniqueId;

    function summary : String; virtual; abstract;

    procedure visitAll(proc : TFHIRPathExpressionNodeVisitProc); virtual; abstract;
    function nodeOpName : String; virtual; abstract;
    function nodeName : String; virtual; abstract;
    function nodeChildCount : integer; virtual; abstract;
    function nodeOpNext : TFHIRPathExpressionNodeV; virtual; abstract;
    function nodeGetChild(nodeIndex : integer; var offset : integer) : TFHIRPathExpressionNodeV; virtual; abstract;
  end;

  TFHIRPathExecutionContext = class (TFslObject)
  private
    FAppInfo : TFslObject;
    FResource : TFHIRObject;
    FContext : TFHIRObject;
    FTotal : TFHIRSelectionList;
     FThis : TFHIRObject;
    procedure SetTotal(const Value: TFHIRSelectionList);
  public
    Constructor Create(appInfo : TFslObject; resource : TFHIRObject; context : TFHIRObject);
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

  TFHIRPathEngineV = class (TFslObject)
  private
    FOndebug : TFHIRPathDebugEvent;
    function findPath(path : String; loc : TSourceLocation; context : TArray<TFHIRObject>; base : TFHIRObject; var focus : TArray<TFHIRObject>) : String;
  public
    property Ondebug : TFHIRPathDebugEvent read FOndebug write FOndebug;
    function parseV(source : String) : TFHIRPathExpressionNodeV; virtual; abstract;
    function evaluate(appInfo : TFslObject; base : TFHIRObject; path : String) : TFHIRSelectionList; overload; virtual; abstract;
    function evaluate(appInfo : TFslObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : TFHIRSelectionList; overload; virtual; abstract;
    function evaluate(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; path : String) : TFHIRSelectionList; overload; virtual; abstract;
    function evaluate(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : TFHIRSelectionList; overload; virtual; abstract;
    function convertToString(list : TFHIRSelectionList) : string; overload; virtual; abstract;
    function check(appInfo : TFslObject; resourceType, context, path : String; expr : TFHIRPathExpressionNodeV; xPathStartsWithValueRef : boolean) : TFHIRTypeDetailsV; overload; virtual; abstract;

    function extractPath(pathBase : String; loc : TSourceLocation; base : TFHIRObject) : String; overload;
    function extractPath(pathBase : String; loc : TSourceLocation; base : TFHIRObject; var pathObjects : TArray<TFHIRObject>) : String; overload;
  end;


implementation

{ TFHIRPathEngineV }


{ TFHIRPathEngineV }

function TFHIRPathEngineV.extractPath(pathBase: String; loc: TSourceLocation; base: TFHIRObject): String;
var
  pathObjects : TArray<TFHIRObject>;
begin
  result := findPath(pathBase, loc, [], base, pathObjects);
end;

function TFHIRPathEngineV.extractPath(pathBase: String; loc: TSourceLocation; base: TFHIRObject; var pathObjects: TArray<TFHIRObject>): String;
begin
  result := findPath(pathBase, loc, [], base, pathObjects);
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

  if locGreatorOrEqual(loc, base.LocationEnd) then
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
          if locGreatorOrEqual(loc, p.Values[0].LocationStart) then
          begin
            path := path + '.'+p.Name;
            if p.IsList then
            begin
              for j := p.Values.Count - 1 downto 0 do
                if (result = '') and locGreatorOrEqual(loc, p.Values[j].LocationStart) then
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
  if (result = '') and locGreatorOrEqual(loc, base.LocationStart) and locLessOrEqual(loc, base.LocationEnd)  then
  begin
    result := path;
    focus := list;
  end;
end;

{ TFHIRPathExpressionNodeV }

function TFHIRPathExpressionNodeV.link: TFHIRPathExpressionNodeV;
begin
  result := TFHIRPathExpressionNodeV(inherited link);
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


{ TFHIRTypeDetailsV }

function TFHIRTypeDetailsV.link: TFHIRTypeDetailsV;
begin
  result := TFHIRTypeDetailsV(inherited Link);
end;

end.
