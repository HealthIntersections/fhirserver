unit FHIRPluginValidator;

interface

Uses
  SysUtils, Classes, ActiveX, ComObj,
  IdSoapXml, IdSoapMsXml, MsXmlParser, AltovaXMLLib_TLB,
  StringSupport,
  AdvObjects, AdvBuffers, AdvNameBuffers, AdvMemories, AdvVclStreams, AdvZipReaders, AdvZipParts, AdvGenerics,
  FHIRTypes, FHIRResources, FHIRValidator, FHIRParser, FHIRUtilities, FHIRProfileUtilities, ProfileManager, FHIRConstants, FHIRClient;

Type
  TFHIRValidator = class (TFHIRBaseValidator)
  private
    FUrl : String;
    FServer : TFHIRClient;
    FConfStmt : TFHIRConformance;
    FValueSets : TAdvMap<TFHIRValueSet>;
    FCodeSystems : TAdvMap<TFHIRValueSet>;
    procedure checkClient;
  protected
    procedure SeeResource(r : TFhirResource); override;
  public
    Constructor Create(terminologyServer : String); virtual;
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

procedure TFHIRValidator.checkClient;
begin
  if (FServer = nil) then
  begin
    FServer := TFhirClient.Create(FUrl, true);
    FConfStmt := FServer.conformance(true);
    if FConfStmt.fhirVersion <> FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION then
      raise Exception.Create('Terminology Server / Plug-in Version mismatch ('+FConfStmt.fhirVersion+' / '+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+')');
  end;
end;

constructor TFHIRValidator.Create(terminologyServer : String);
begin
  inherited Create;
  FValueSets := TAdvMap<TFHIRValueSet>.create;
  FCodeSystems := TAdvMap<TFHIRValueSet>.create;
  FUrl := terminologyServer;
end;

destructor TFHIRValidator.Destroy;
begin
  FValueSets.Free;
  FServer.Free;
  FConfStmt.Free;
  FCodeSystems.Free;
  inherited;
end;

function TFHIRValidator.expand(vs: TFhirValueSet): TFHIRValueSet;
begin
  CheckClient;
  result := nil;
end;

function TFHIRValidator.fetchResource(t: TFhirResourceType; url: String): TFhirResource;
begin
  if (t = frtValueSet) then
    result := FValueSets[url]
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRValidator.Link: TFHIRValidator;
begin
  result := TFHIRValidator(inherited Link);
end;

procedure TFHIRValidator.SeeResource(r: TFhirResource);
var
  vs : TFhirValueset;
begin
  if (r.ResourceType = frtValueSet) then
  begin
    vs := (r as TFHIRValueSet);
    FValueSets.Add(vs.url, vs.Link);
    if (vs.codeSystem <> nil) then
      FCodeSystems.Add(vs.codeSystem.system, vs.Link)
  end
  else
    inherited;
end;

function TFHIRValidator.supportsSystem(system: string): boolean;
var
  ex : TFhirExtension;
begin
  result := FCodeSystems.ContainsKey(system);
  if (not result) then
    for ex in FConfStmt.extensionList do
      if (ex.url = 'http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system') and (ex.value is TFHIRString) and (TFHIRString(ex.value).value = system) then
        result := true;
end;

function TFHIRValidator.validateCode(system, code, display: String): TValidationResult;
var
  pIn, pOut : TFhirParameters;
begin
  checkClient;
  pIn := TFhirParameters.Create;
  try
    pIn.AddParameter('system', system);
    pIn.AddParameter('code', code);
    pIn.AddParameter('display', display);
    pOut := FServer.operation(frtValueSet, 'validate-code', pIn) as TFhirParameters;
    try
      if pOut.bool['result'] then
        result := TValidationResult.Create(IssueSeverityInformation, pOut.str['message'])
      else
        result := TValidationResult.Create(IssueSeverityInformation, pOut.str['message']);
    finally
      pOut.Free;
    end;
  finally
    pIn.Free;
  end;
end;

function TFHIRValidator.validateCode(system, code, version: String; vs: TFHIRValueSet): TValidationResult;
var
  pIn, pOut : TFhirParameters;
begin
  checkClient;
  pIn := TFhirParameters.Create;
  try
    pIn.AddParameter('system', system);
    pIn.AddParameter('code', code);
    pIn.AddParameter('version', version);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(frtValueSet, 'validate-code', pIn) as TFhirParameters;
    try
      if pOut.bool['result'] then
        result := TValidationResult.Create(IssueSeverityInformation, pOut.str['message'])
      else
        result := TValidationResult.Create(IssueSeverityInformation, pOut.str['message']);
    finally
      pOut.Free;
    end;
  finally
    pIn.Free;
  end;
end;

function TFHIRValidator.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
var
  pIn, pOut : TFhirParameters;
begin
  checkClient;
  pIn := TFhirParameters.Create;
  try
    pIn.AddParameter('codeableConcept', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(frtValueSet, 'validate-code', pIn) as TFhirParameters;
    try
      if pOut.bool['result'] then
        result := TValidationResult.Create(IssueSeverityInformation, pOut.str['message'])
      else
        result := TValidationResult.Create(IssueSeverityInformation, pOut.str['message']);
    finally
      pOut.Free;
    end;
  finally
    pIn.Free;
  end;
end;

function TFHIRValidator.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
var
  pIn, pOut : TFhirParameters;
begin
  checkClient;
  pIn := TFhirParameters.Create;
  try
    pIn.AddParameter('coding', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(frtValueSet, 'validate-code', pIn) as TFhirParameters;
    try
      if pOut.bool['result'] then
        result := TValidationResult.Create(IssueSeverityInformation, pOut.str['message'])
      else
        result := TValidationResult.Create(IssueSeverityInformation, pOut.str['message']);
    finally
      pOut.Free;
    end;
  finally
    pIn.Free;
  end;
end;

end.
