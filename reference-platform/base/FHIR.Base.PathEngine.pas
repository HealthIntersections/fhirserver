unit FHIR.Base.PathEngine;

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

    function summary : String; virtual;

    procedure visitAll(proc : TFHIRPathExpressionNodeVisitProc); virtual;
    function nodeOpName : String; virtual;
    function nodeName : String; virtual;
    function nodeChildCount : integer; virtual;
    function nodeOpNext : TFHIRPathExpressionNodeV; virtual;
    function nodeGetChild(nodeIndex : integer; var offset : integer) : TFHIRPathExpressionNodeV; virtual;
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
  public
    property Ondebug : TFHIRPathDebugEvent read FOndebug write FOndebug;
    function parseV(source : String) : TFHIRPathExpressionNodeV; virtual;
    function evaluate(appInfo : TFslObject; base : TFHIRObject; path : String) : TFHIRSelectionList; overload; virtual;
    function evaluate(appInfo : TFslObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : TFHIRSelectionList; overload; virtual;
    function evaluate(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; path : String) : TFHIRSelectionList; overload; virtual;
    function evaluate(appInfo : TFslObject; resource : TFHIRObject; base : TFHIRObject; expr : TFHIRPathExpressionNodeV) : TFHIRSelectionList; overload; virtual;
    function check(appInfo : TFslObject; resourceType, context, path : String; expr : TFHIRPathExpressionNodeV; xPathStartsWithValueRef : boolean) : TFHIRTypeDetailsV; overload; virtual;
  end;


implementation

{ TFHIRPathEngineV }

function TFHIRPathEngineV.evaluate(appInfo: TFslObject; base: TFHIRObject; path: String): TFHIRSelectionList;
begin
  raise Exception.Create('TFHIRPathEngineV.evaluate must be overriden in a version specific instantiation');
end;

function TFHIRPathEngineV.evaluate(appInfo: TFslObject; base: TFHIRObject; expr: TFHIRPathExpressionNodeV): TFHIRSelectionList;
begin
  raise Exception.Create('TFHIRPathEngineV.evaluate must be overriden in a version specific instantiation');
end;

function TFHIRPathEngineV.check(appInfo: TFslObject; resourceType, context, path: String; expr: TFHIRPathExpressionNodeV; xPathStartsWithValueRef: boolean): TFHIRTypeDetailsV;
begin
  raise Exception.Create('TFHIRPathEngineV.check must be overriden in a version specific instantiation');
end;

function TFHIRPathEngineV.evaluate(appInfo: TFslObject; resource, base: TFHIRObject; expr: TFHIRPathExpressionNodeV): TFHIRSelectionList;
begin
  raise Exception.Create('TFHIRPathEngineV.evaluate must be overriden in a version specific instantiation');
end;

function TFHIRPathEngineV.evaluate(appInfo: TFslObject; resource, base: TFHIRObject; path: String): TFHIRSelectionList;
begin
  raise Exception.Create('TFHIRPathEngineV.evaluate must be overriden in a version specific instantiation');
end;

function TFHIRPathEngineV.parseV(source: String): TFHIRPathExpressionNodeV;
begin
  raise Exception.Create('TFHIRPathEngineV.parse must be overriden in a version specific instantiation');
end;

{ TFHIRPathExpressionNodeV }

function TFHIRPathExpressionNodeV.link: TFHIRPathExpressionNodeV;
begin
  result := TFHIRPathExpressionNodeV(inherited link);
end;

function TFHIRPathExpressionNodeV.nodeChildCount: integer;
begin
  raise Exception.Create('TFHIRPathExpressionNodeV.nodeChildCount must be overriden in a version specific instantiation');
end;

function TFHIRPathExpressionNodeV.nodeGetChild(nodeIndex: integer; var offset: integer): TFHIRPathExpressionNodeV;
begin
  raise Exception.Create('TFHIRPathExpressionNodeV.nodeName must be overriden in a version specific instantiation');
end;

function TFHIRPathExpressionNodeV.nodeName: String;
begin
  raise Exception.Create('TFHIRPathExpressionNodeV.nodeName must be overriden in a version specific instantiation');
end;

function TFHIRPathExpressionNodeV.nodeOpNext: TFHIRPathExpressionNodeV;
begin
  raise Exception.Create('TFHIRPathExpressionNodeV.nodeOpNext must be overriden in a version specific instantiation');
end;

function TFHIRPathExpressionNodeV.nodeOpName: String;
begin
  raise Exception.Create('TFHIRPathExpressionNodeV.nodeOpName must be overriden in a version specific instantiation');
end;

function TFHIRPathExpressionNodeV.summary: String;
begin
  raise Exception.Create('TFHIRPathExpressionNodeV.visit must be overriden in a version specific instantiation');
end;

procedure TFHIRPathExpressionNodeV.visitAll(proc: TFHIRPathExpressionNodeVisitProc);
begin
  raise Exception.Create('TFHIRPathExpressionNodeV.visit must be overriden in a version specific instantiation');
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
