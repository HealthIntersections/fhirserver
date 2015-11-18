unit FHIRPluginValidator;

interface

Uses
  SysUtils, Classes, ActiveX, ComObj,
  MsXml, MsXmlParser,
  StringSupport,
  AdvObjects, AdvBuffers, AdvNameBuffers, AdvMemories, AdvVclStreams, AdvZipReaders, AdvZipParts, AdvGenerics,
  FHIRTypes, FHIRResources, FHIRValidator, FHIRParser, FHIRUtilities, FHIRProfileUtilities,
  FHIRConstants, FHIRClient, FHIRBase;

Type
  TFHIRPluginValidatorContext = class (TValidatorServiceProvider)
  private
    FUrl : String;
    FServer : TFHIRClient;
    FConfStmt : TFHIRConformance;
    FValueSets : TAdvMap<TFHIRValueSet>;
    FCodeSystems : TAdvMap<TFHIRValueSet>;
    procedure checkClient;
    function  findCode(vs : TFhirValueSetCodeSystemConceptList; code : String; caseSensitive : boolean) : TFhirValueSetCodeSystemConcept;
  protected
    procedure SeeResource(r : TFhirResource); override;
  public
    Constructor Create(terminologyServer : String); virtual;
    Destructor Destroy; Override;

    Function Link : TFHIRPluginValidatorContext; overload;

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet) : TFHIRValueSet; override;
    function supportsSystem(system : string) : boolean; override;
    function validateCode(system, code, display : String) : TValidationResult; override;
    function validateCode(system, code, version : String; vs : TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;
  end;


implementation

{ TFHIRPluginValidatorContext }

procedure TFHIRPluginValidatorContext.checkClient;
begin
  if (FServer = nil) then
  begin
    FServer := TFhirClient.Create(FUrl, true);
    FConfStmt := FServer.conformance(true);
    if FConfStmt.fhirVersion <> FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION then
      raise Exception.Create('Terminology Server / Plug-in Version mismatch ('+FConfStmt.fhirVersion+' / '+FHIR_GENERATED_VERSION+'-'+FHIR_GENERATED_REVISION+')');
  end;
end;

constructor TFHIRPluginValidatorContext.Create(terminologyServer : String);
begin
  inherited Create;
  FValueSets := TAdvMap<TFHIRValueSet>.create;
  FCodeSystems := TAdvMap<TFHIRValueSet>.create;
  FUrl := terminologyServer;
end;

destructor TFHIRPluginValidatorContext.Destroy;
begin
  FValueSets.Free;
  FServer.Free;
  FConfStmt.Free;
  FCodeSystems.Free;
  inherited;
end;

function TFHIRPluginValidatorContext.expand(vs: TFhirValueSet): TFHIRValueSet;
var
  pIn : TFhirParameters;
begin
  CheckClient;
  pIn := TFhirParameters.Create;
  try
    pIn.AddParameter('valueSet', vs.Link);
    pIn.AddParameter('_incomplete', true);
    pIn.AddParameter('_limit', '10');
    result := FServer.operation(frtValueSet, 'expand', pIn) as TFhirValueSet;
  finally
    pIn.Free;
  end;

end;

function TFHIRPluginValidatorContext.fetchResource(t: TFhirResourceType; url: String): TFhirResource;
begin
  if (t = frtValueSet) then
    result := FValueSets[url]
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRPluginValidatorContext.findCode(vs: TFhirValueSetCodeSystemConceptList; code: String; caseSensitive : boolean): TFhirValueSetCodeSystemConcept;
var
  d, d1 : TFhirValueSetCodeSystemConcept;
begin
  result := nil;
  for d in vs do
  begin
    if (caseSensitive and (d.code = code)) then
    begin
      result := d;
      exit;
    end;
    if (not caseSensitive and (d.code.ToLower = code.ToLower)) then
    begin
      result := d;
      exit;
    end;
    result := findCode(d.conceptList, code, caseSensitive);
    if (result <> nil) then
      exit;
  end;
end;

function TFHIRPluginValidatorContext.Link: TFHIRPluginValidatorContext;
begin
  result := TFHIRPluginValidatorContext(inherited Link);
end;

procedure TFHIRPluginValidatorContext.SeeResource(r: TFhirResource);
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

function TFHIRPluginValidatorContext.supportsSystem(system: string): boolean;
var
  ex : TFhirExtension;
begin
  result := FCodeSystems.ContainsKey(system);
  if (not result) then
    for ex in FConfStmt.extensionList do
      if (ex.url = 'http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system') and (ex.value is TFHIRString) and (TFHIRString(ex.value).value = system) then
        result := true;
end;

function TFHIRPluginValidatorContext.validateCode(system, code, display: String): TValidationResult;
var
  pIn, pOut : TFhirParameters;
  vs : TFHIRValueSet;
  def : TFhirValueSetCodeSystemConcept;
begin
  if FCodeSystems.ContainsKey(system) then
  begin
    vs := FCodeSystems[system];
    def := FindCode(vs.codeSystem.conceptList, code, vs.codeSystem.caseSensitive);
    if (def = nil) then
      result := TValidationResult.Create(IssueSeverityError, 'Unknown code ' +code)
    else
      result := TValidationResult.Create(def.display);
  end
  else
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
end;

function TFHIRPluginValidatorContext.validateCode(system, code, version: String; vs: TFHIRValueSet): TValidationResult;
var
  pIn, pOut : TFhirParameters;
  def : TFhirValueSetCodeSystemConcept;
begin
  if (vs.codeSystem <> nil) and (vs.compose = nil) and (system = SYSTEM_NOT_APPLICABLE) then
  begin
    def := FindCode(vs.codeSystem.conceptList, code, vs.codeSystem.caseSensitive);
    if (def = nil) then
      result := TValidationResult.Create(IssueSeverityError, 'Unknown code ' +code)
    else
      result := TValidationResult.Create(def.display);
  end
  else
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
end;

function TFHIRPluginValidatorContext.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
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

function TFHIRPluginValidatorContext.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
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
