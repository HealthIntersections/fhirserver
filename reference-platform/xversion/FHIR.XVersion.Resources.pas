unit FHIR.XVersion.Resources;

interface

uses
  SysUtils,
  FHIR.Support.Objects,
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

  TFhirOperationOutcomeIssueW = class (TFslObject)
  private
    FIssue : TFHIRObject;
  public
    constructor Create(iss : TFHIRObject);
    destructor Destroy; override;

    function link : TFhirOperationOutcomeIssueW; overload;
    property issue : TFHIRObject read FIssue;

    function display : String; virtual;
    function severity : TIssueSeverity; virtual;
     {diagnostics;
  if (issue.details <> nil) and (issue.details.text <> '') then
    msg := issue.details.text;            }
  end;

  TFHIRXVersionResoourceWrapper = class (TFslObject)
  protected
    FRes : TFHIRResourceV;
  public
    constructor Create(res : TFHIRResourceV);
    destructor Destroy; override;
  end;

  TFhirOperationOutcomeW = class (TFHIRXVersionResoourceWrapper)
  public
    function link : TFhirOperationOutcomeW; overload;

    function hasText : boolean; virtual;
    function text : String; virtual;
    function code :  TExceptionType; virtual;
  end;
  TFhirOperationOutcomeWClass = class of TFhirOperationOutcomeW;


  TFHIRExtensionW = class (TFslObject);

  TFHIRCapabilityStatementW = class (TFHIRXVersionResoourceWrapper)
  public
    function link : TFHIRCapabilityStatementW; overload;

    function hasRest : boolean; virtual;
    function hasSecurity(system, code : String) : boolean; virtual;
    procedure readSmartExtension(var authorize, token, register: String); virtual;
    function hasFormat(fmt : String) : boolean; virtual;
  end;

implementation

{ TFHIRXVersionResoourceWrapper }

constructor TFHIRXVersionResoourceWrapper.create(res: TFHIRResourceV);
begin
  inherited create;
  FRes := res;
end;

destructor TFHIRXVersionResoourceWrapper.Destroy;
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

constructor TFhirOperationOutcomeIssueW.Create(iss: TFHIRObject);
begin
  inherited create;
  FIssue := iss;
end;

destructor TFhirOperationOutcomeIssueW.Destroy;
begin
  FIssue.Free;
  inherited;
end;

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

end.
