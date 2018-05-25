unit FHIR.XVersion.Resources;

interface

uses
  SysUtils,
  FHIR.Support.Objects, FHIR.Support.Generics,
  FHIR.Base.Objects;

Type
  TBundleHandler = class (TFslObject)
  private
    FResource : TFhirResourceV;
  public
    constructor Create(bnd : TFHIRResourceV);
    destructor Destroy; override;
    property resource : TFHIRResourceV read FResource;
    function next : String; overload;
    function next(bnd : TFHIRResourceV) : String; overload; virtual;
    procedure addEntries(bnd : TFHIRResourceV); virtual;
    procedure clearLinks; virtual;
  end;

  TBundleHandlerClass = class of TBundleHandler;

  TFHIRXVersionElementWrapper = class (TFslObject)
  protected
    FElement : TFHIRObject;
  public
    constructor Create(elem : TFHIRObject);
    destructor Destroy; override;

    property Element : TFHIRObject read FElement;
  end;

  TFhirOperationOutcomeIssueW = class (TFHIRXVersionElementWrapper)
  public
    function link : TFhirOperationOutcomeIssueW; overload;


    function display : String; virtual;
    function severity : TIssueSeverity; virtual;
     {diagnostics;
  if (issue.details <> nil) and (issue.details.text <> '') then
    msg := issue.details.text;            }
  end;

  TFHIRXVersionResourceWrapper = class (TFslObject)
  protected
    FRes : TFHIRResourceV;
  public
    constructor Create(res : TFHIRResourceV);
    destructor Destroy; override;
    property Resource : TFHIRResourceV read FRes;
  end;

  TFhirOperationOutcomeW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFhirOperationOutcomeW; overload;

    function hasText : boolean; virtual;
    function text : String; virtual;
    function code :  TExceptionType; virtual;
  end;
  TFhirOperationOutcomeWClass = class of TFhirOperationOutcomeW;

  TFHIRCapabilityStatementW = class (TFHIRXVersionResourceWrapper)
  public
    function link : TFHIRCapabilityStatementW; overload;

    function hasRest : boolean; virtual;
    function hasSecurity(system, code : String) : boolean; virtual;
    procedure readSmartExtension(var authorize, token, register: String); virtual;
    function hasFormat(fmt : String) : boolean; virtual;
  end;

  TFhirParametersParameterW = class (TFHIRXVersionElementWrapper)
  protected
    FList : TFslList<TFhirParametersParameterW>;
    function GetValue: TFHIRObject; virtual;
    procedure SetValue(const Value: TFHIRObject); virtual;
    procedure populateList; virtual;
    function GetParameterParameter(name: String): TFhirParametersParameterW;  virtual;
    function GetResourceParameter(name: String): TFHIRResourceV;  virtual;
    function GetStringParameter(name: String): String;  virtual;
  public
    destructor Destroy; override;
    function link : TFhirParametersParameterW; overload;

    function name : String; virtual;
    function hasValue : boolean;  virtual;
    property value : TFHIRObject read GetValue write SetValue;

    property res[name : String] : TFHIRResourceV read GetResourceParameter;
    property str[name : String] : String read GetStringParameter;
    property param[name : String] : TFhirParametersParameterW read GetParameterParameter;

    function partList : TFslList<TFhirParametersParameterW>;
    function appendPart(name : String) : TFhirParametersParameterW; virtual;
  end;

  TFHIRParametersW = class (TFHIRXVersionResourceWrapper)
  protected
    FList : TFslList<TFhirParametersParameterW>;
    procedure populateList; virtual;
  public
    destructor Destroy; override;
    function link : TFHIRParametersW; overload;

    function parameterList : TFslList<TFhirParametersParameterW>;
    function appendParameter(name : String) : TFhirParametersParameterW; virtual;
  end;

  TFhirValueSetW =  class (TFHIRXVersionResourceWrapper);

  TStructureDefinitionKind = (sdkPrimitive, sdkDataType, sdkExtension, sdkResource);

  TFhirStructureDefinitionW =  class (TFHIRXVersionResourceWrapper)
  public
    function kind : TStructureDefinitionKind; virtual;
    function name : String; virtual;
    function url : String; virtual;
  end;

implementation

{ TFHIRXVersionResourceWrapper }

constructor TFHIRXVersionResourceWrapper.create(res: TFHIRResourceV);
begin
  inherited create;
  FRes := res;
end;

destructor TFHIRXVersionResourceWrapper.Destroy;
begin
  FRes.Free;
  inherited;
end;

{ TFhirOperationOutcomeW }

function TFhirOperationOutcomeW.code: TExceptionType;
begin
  raise Exception.Create('Must override code in '+className);
end;

function TFhirOperationOutcomeW.hasText: boolean;
begin
  raise Exception.Create('Must override hasText in '+className);
end;

function TFhirOperationOutcomeW.link: TFhirOperationOutcomeW;
begin
  result := TFhirOperationOutcomeW(inherited Link);
end;

function TFhirOperationOutcomeW.text: String;
begin
  raise Exception.Create('Must override text in '+className);
end;

{ TFhirOperationOutcomeIssueW }

function TFhirOperationOutcomeIssueW.display: String;
begin
  raise Exception.Create('Need to override '+className+'.display');
end;

function TFhirOperationOutcomeIssueW.link: TFhirOperationOutcomeIssueW;
begin
  result := TFhirOperationOutcomeIssueW(inherited link);
end;

function TFhirOperationOutcomeIssueW.severity: TIssueSeverity;
begin
  raise Exception.Create('Need to override '+className+'.display');
end;


{ TFHIRCapabilitiesStatementW }

function TFHIRCapabilityStatementW.hasFormat(fmt: String): boolean;
begin
  result := false;
end;

function TFHIRCapabilityStatementW.hasRest: boolean;
begin
  result := false;
end;

function TFHIRCapabilityStatementW.hasSecurity(system, code: String): boolean;
begin
  result := false;
end;

function TFHIRCapabilityStatementW.link: TFHIRCapabilityStatementW;
begin
  result := TFHIRCapabilityStatementW(inherited Link);
end;

procedure TFHIRCapabilityStatementW.readSmartExtension(var authorize, token, register: String);
begin
end;

{ TBundleHandler }

procedure TBundleHandler.addEntries(bnd: TFHIRResourceV);
begin
  raise Exception.Create('Must override addEntries in '+ClassName);
end;

procedure TBundleHandler.clearLinks;
begin
  raise Exception.Create('Must override clearLinks in '+ClassName);
end;

constructor TBundleHandler.Create(bnd: TFHIRResourceV);
begin
  inherited Create;
  FResource := bnd;
end;

destructor TBundleHandler.Destroy;
begin
  FResource.Free;
  inherited;
end;

function TBundleHandler.next(bnd: TFHIRResourceV): String;
begin
  raise Exception.Create('Must override next in '+ClassName);
end;

function TBundleHandler.next: String;
begin
  result := next(FResource);
end;

{ TFhirParametersParameterW }

function TFhirParametersParameterW.appendPart(name: String): TFhirParametersParameterW;
begin
  raise Exception.Create('Must override appendPart in '+ClassName);
end;

destructor TFhirParametersParameterW.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirParametersParameterW.GetParameterParameter(name: String): TFhirParametersParameterW;
begin
  raise Exception.Create('Must override GetParameterParameter in '+ClassName);
end;

function TFhirParametersParameterW.GetResourceParameter(name: String): TFHIRResourceV;
begin
  raise Exception.Create('Must override GetResourceParameter in '+ClassName);
end;

function TFhirParametersParameterW.GetStringParameter(name: String): String;
begin
  raise Exception.Create('Must override GetStringParameter in '+ClassName);
end;

function TFhirParametersParameterW.GetValue: TFHIRObject;
begin
  raise Exception.Create('Must override GetValue in '+ClassName);
end;

function TFhirParametersParameterW.hasValue: boolean;
begin
  raise Exception.Create('Must override hasValue in '+ClassName);
end;

function TFhirParametersParameterW.link: TFhirParametersParameterW;
begin
  result := TFhirParametersParameterW(inherited link);
end;

function TFhirParametersParameterW.name: String;
begin
  raise Exception.Create('Must override name in '+ClassName);
end;

function TFhirParametersParameterW.partList: TFslList<TFhirParametersParameterW>;
begin
  populateList;
  result := FList;
end;

procedure TFhirParametersParameterW.populateList;
begin
  if FList = nil then
    FList := TFslList<TFhirParametersParameterW>.create;
  FList.Clear;
end;

procedure TFhirParametersParameterW.SetValue(const Value: TFHIRObject);
begin
  raise Exception.Create('Must override SetValue in '+ClassName);
end;


{ TFHIRXVersionElementWrapper }

constructor TFHIRXVersionElementWrapper.Create(elem : TFHIRObject);
begin
  inherited create;
  FElement := elem;
end;

destructor TFHIRXVersionElementWrapper.Destroy;
begin
  FElement.Free;
  inherited;
end;

{ TFHIRParametersW }

function TFHIRParametersW.appendParameter(name: String): TFhirParametersParameterW;
begin
  raise Exception.Create('Must override appendParameter in '+ClassName);
end;

destructor TFHIRParametersW.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFHIRParametersW.link: TFHIRParametersW;
begin
  result := TFHIRParametersW(inherited Link);

end;

function TFHIRParametersW.parameterList: TFslList<TFhirParametersParameterW>;
begin
  populateList;
  result := FList;
end;

procedure TFHIRParametersW.populateList;
begin
  if FList = nil then
    FList := TFslList<TFhirParametersParameterW>.create;
  FList.Clear;
end;

{ TFhirStructureDefinitionW }

function TFhirStructureDefinitionW.kind: TStructureDefinitionKind;
begin
  raise Exception.Create('Must override kind in '+className);
end;

function TFhirStructureDefinitionW.name: String;
begin
  raise Exception.Create('Must override name in '+className);
end;

function TFhirStructureDefinitionW.url: String;
begin
  raise Exception.Create('Must override url in '+className);
end;

end.
