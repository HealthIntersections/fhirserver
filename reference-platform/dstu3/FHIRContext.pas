unit FHIRContext;

interface

uses
  AdvObjects, AdvGenerics,
  FHIRTypes, FHIRResources;

type
  TValidationResult = class (TAdvObject)
  private
    FSeverity : TFhirIssueSeverityEnum;
    FMessage  : String;
    FDisplay: String;
  public
    constructor Create; overload; override;
    constructor Create(Severity : TFhirIssueSeverityEnum; Message : String); overload; virtual;
    constructor Create(display : String); overload; virtual;
    Property Severity : TFhirIssueSeverityEnum read FSeverity write FSeverity;
    Property Message : String read FMessage write FMessage;
    Property Display : String read FDisplay write FDisplay;
    function isOk : boolean;
  end;

  TFHIRCustomResourceInformation = class (TAdvObject)
  private
    FName: String;
    FSearchParameters: TAdvList<TFHIRSearchParameter>;
    FDefinition: TFHIRStructureDefinition;
  public
    Constructor Create(definition : TFHIRStructureDefinition);
    Destructor Destroy; override;
    function Link : TFHIRCustomResourceInformation; overload;
    property Name : String read FName;
    property Definition : TFHIRStructureDefinition read FDefinition;
    property SearchParameters : TAdvList<TFHIRSearchParameter> read FSearchParameters;
  end;


  TWorkerContext = class abstract (TAdvObject)
  public
    function link : TWorkerContext; overload;

    function allStructures : TAdvMap<TFHIRStructureDefinition>.TValueCollection; virtual; abstract;
    function getResourceNames : TAdvStringSet; virtual; abstract;
    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; virtual; abstract;
    function expand(vs : TFhirValueSet) : TFHIRValueSet; virtual; abstract;
    function supportsSystem(system : string) : boolean; virtual; abstract;
    function validateCode(system, code, display : String) : TValidationResult; overload; virtual; abstract;
    function validateCode(system, code, version : String; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; virtual; abstract;
    function getChildMap(profile : TFHIRStructureDefinition; element : TFhirElementDefinition) : TFHIRElementDefinitionList; virtual;  abstract;
    function getStructure(url : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getStructure(ns, name : String) : TFHIRStructureDefinition; overload; virtual; abstract;
    function getCustomResource(name : String) : TFHIRCustomResourceInformation; virtual; abstract;
    function hasCustomResource(name : String) : boolean; virtual; abstract;
    function allResourceNames : TArray<String>; virtual; abstract;
    function nonSecureResourceNames : TArray<String>; virtual; abstract;
  end;

implementation

{ TValidationResult }

constructor TValidationResult.Create(Severity: TFhirIssueSeverityEnum; Message: String);
begin
  inherited create;
  FSeverity := Severity;
  FMessage := Message;
end;

constructor TValidationResult.Create;
begin
  Inherited Create;
end;

constructor TValidationResult.Create(display: String);
begin
  inherited Create;
  FDisplay := display;
end;

function TValidationResult.isOk: boolean;
begin
  result := not (Severity in [IssueSeverityError, IssueSeverityFatal]);
end;

{ TWorkerContext }

function TWorkerContext.link: TWorkerContext;
begin
  result := TWorkerContext(inherited link);
end;

{ TFHIRCustomResourceInformation }

constructor TFHIRCustomResourceInformation.Create(definition: TFHIRStructureDefinition);
begin
  inherited Create;
  FDefinition := definition;
  FName := definition.snapshot.elementList[0].path;
  FSearchParameters := TAdvList<TFHIRSearchParameter>.create;
end;

destructor TFHIRCustomResourceInformation.Destroy;
begin
  FSearchParameters.Free;
  FDefinition.Free;
  inherited;
end;

function TFHIRCustomResourceInformation.Link: TFHIRCustomResourceInformation;
begin
  result := TFHIRCustomResourceInformation(inherited Link);
end;


end.
