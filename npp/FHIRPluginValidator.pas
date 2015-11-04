unit FHIRPluginValidator;

interface

Uses
  SysUtils, Classes, ActiveX, ComObj,
  IdSoapXml, IdSoapMsXml, MsXmlParser, AltovaXMLLib_TLB,
  StringSupport,
  AdvObjects, AdvBuffers, AdvNameBuffers, AdvMemories, AdvVclStreams, AdvZipReaders, AdvZipParts,
  FHIRTypes, FHIRResources, FHIRValidator, FHIRParser, FHIRUtilities, FHIRProfileUtilities, ProfileManager;

Type
  TFHIRValidator = class (TFHIRBaseValidator)
  private

  protected
    procedure SeeResource(r : TFhirResource); override;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Link : TFHIRValidator; overload;

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet) : TFHIRValueSet; override;
    function supportsSystem(system : string) : boolean; override;
    function validateCode(system, code, display : String) : TValidationResult; override;
    function validateCode(system, code, version : String; vs : TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;
  end;


implementation

{ TFHIRValidator }

constructor TFHIRValidator.Create;
begin
  inherited;

end;

destructor TFHIRValidator.Destroy;
begin

  inherited;
end;

function TFHIRValidator.expand(vs: TFhirValueSet): TFHIRValueSet;
begin
  result := nil;
end;

function TFHIRValidator.fetchResource(t: TFhirResourceType; url: String): TFhirResource;
begin
  result := inherited fetchResource(t, url);

end;

function TFHIRValidator.Link: TFHIRValidator;
begin
  result := TFHIRValidator(inherited Link);
end;

procedure TFHIRValidator.SeeResource(r: TFhirResource);
begin
  inherited;
end;

function TFHIRValidator.supportsSystem(system: string): boolean;
begin
  result := false;
end;

function TFHIRValidator.validateCode(system, code, display: String): TValidationResult;
begin
  result := TValidationResult.Create;
  result.Severity := IssueSeverityInformation;
end;

function TFHIRValidator.validateCode(system, code, version: String; vs: TFHIRValueSet): TValidationResult;
begin
  result := TValidationResult.Create;
  result.Severity := IssueSeverityInformation;
end;

function TFHIRValidator.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
begin
  result := TValidationResult.Create;
  result.Severity := IssueSeverityInformation;
end;

function TFHIRValidator.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
begin
  result := TValidationResult.Create;
  result.Severity := IssueSeverityInformation;
end;

end.
