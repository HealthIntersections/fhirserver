Unit ServerValidator;

Interface

Uses
  SysUtils, Classes, ActiveX, ComObj,
  MsXml, MsXmlParser,
  StringSupport, kCritSct,
  AdvObjects, AdvGenerics, AdvBuffers, AdvNameBuffers, AdvMemories, AdvVclStreams, AdvZipReaders, AdvZipParts,
  FHIRTypes, FHIRResources, FHIRValidator, FHIRParser, FHIRUtilities, FHIRProfileUtilities, FHIRConstants,
  TerminologyServer;

Type

  TFHIRServerValidatorContext = class (TValidatorServiceProvider)
  private
    FTerminologyServer : TTerminologyServer;
    FProfile : TFhirExpansionProfile;
    FLock : TCriticalSection;
    FQuestionnaires : TAdvMap<TFhirQuestionnaire>;

    procedure SetTerminologyServer(const Value: TTerminologyServer);
    function getQuestionnaire(url : string) : TFhirQuestionnaire;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Link : TFHIRServerValidatorContext; overload;

    procedure SeeResource(r : TFhirResource); override;

    Property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet) : TFHIRValueSet; override;
    function supportsSystem(system : string) : boolean; override;
    function validateCode(system, code, display : String) : TValidationResult; override;
    function validateCode(system, code, version : String; vs : TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;
  end;


Implementation

{ TFHIRServerValidatorContext }

constructor TFHIRServerValidatorContext.Create;
begin
  inherited;
  FLock := TCriticalSection.Create('Validation.questionnaire');
  FProfile := TFhirExpansionProfile.create;
  FProfile.includeDefinition := false;
  FProfile.limitedExpansion := false;
  FQuestionnaires := TAdvMap<TFhirQuestionnaire>.create;
end;

destructor TFHIRServerValidatorContext.Destroy;
begin
  FQuestionnaires.Free;
  FProfile.Free;
  FTerminologyServer.Free;
  FLock.Free;
  inherited;
end;


function TFHIRServerValidatorContext.Link: TFHIRServerValidatorContext;
begin
  result := TFHIRServerValidatorContext(inherited Link);
end;

procedure TFHIRServerValidatorContext.SeeResource(r : TFhirResource);
begin
  r.checkNoImplicitRules('Repository.SeeResource', 'Resource');
  TFhirDomainResource(r).checkNoModifiers('Repository.SeeResource', 'Resource');
  if (r.ResourceType in [frtValueSet, frtConceptMap {$IFDEF FHIR_DSTU3}, frtCodeSystem{$ENDIF}]) then
    FTerminologyServer.SeeSpecificationResource(r)
  else if r.resourceType = frtQuestionnaire then
  begin
    FLock.lock;
    try
      if FQuestionnaires.ContainsKey(r.id) then
        FQuestionnaires[r.id] := (r as TFhirQuestionnaire).link
      else
        FQuestionnaires.add(r.id, (r as TFhirQuestionnaire).link)
    finally
      FLock.Unlock;
    end;
  end
  else
    inherited SeeResource(r);
end;

function TFHIRServerValidatorContext.validateCode(system, code, version: String; vs: TFHIRValueSet): TValidationResult;
var
  c : TFHIRCoding;
  p : TFHIRParameters;
begin
  c := TFhirCoding.Create;
  try
    c.system := system;
    c.code := code;
    c.version := version;
    p := FTerminologyServer.validate(vs, c, false);
    try
      result := TValidationResult.Create;
      try
        if not p.bool['result'] then
        begin
          result.Severity := IssueSeverityError;
          result.Message := p.str['message'];
        end;
        result.Link;
      finally
        result.free;
      end;
    finally
      p.Free;
    end;
  finally
    c.Free;
  end;
end;

procedure TFHIRServerValidatorContext.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

function TFHIRServerValidatorContext.fetchResource(t : TFhirResourceType; url : String) : TFhirResource;
begin
  if t = frtValueSet then
    result := FTerminologyServer.getValueSetByUrl(url)
  else if t = frtQuestionnaire then
    result := getQuestionnaire(url)
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRServerValidatorContext.getQuestionnaire(url: string): TFhirQuestionnaire;
var
  q : TFhirQuestionnaire;
begin
  result := nil;
  if url.StartsWith('Questionnaire/') then
    url := url.Substring(14);
  FLock.lock;
  try
    if FQuestionnaires.TryGetValue(url, q) then
      exit(q.Link)
    else
      exit(nil);
  finally
    FLock.Unlock;
  end;
end;

function TFHIRServerValidatorContext.expand(vs : TFhirValueSet) : TFHIRValueSet;
begin
  result := FTerminologyServer.expandVS(vs, '', FProfile, '', 0, 0, 0);
end;

function TFHIRServerValidatorContext.supportsSystem(system : string) : boolean;
begin
  result := FTerminologyServer.supportsSystem(system);
end;

function TFHIRServerValidatorContext.validateCode(system, code, display : String) : TValidationResult;
var
  op : TFHIROperationOutcome;
begin
  op := TFHIROperationOutcome.create;
  try
    result := TValidationResult.Create;
    try
      if FTerminologyServer.checkCode(op, '', code, system, display) then
        result.Severity := IssueSeverityNull
      else if op.issueList.Count = 1 then
      begin
        result.Severity := op.issueList[0].severity;
        if op.issueList[0].details = nil then
          result.Message := op.issueList[0].diagnostics
        else
          result.Message := op.issueList[0].details.text;
      end
      else
      begin
        result.Severity := IssueSeverityError;
        result.Message := '??';
      end;
      result.Link;
    finally
      result.Free;
    end;
  finally
    op.Free;
  end;
end;


function TFHIRServerValidatorContext.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
var
  p : TFhirParameters;
begin
  result := TValidationResult.Create;
  try
    p := FTerminologyServer.validate(vs, code, false);
    try
      result.Message := p.str['result'];
      if p.bool['result'] then
        result.Severity := IssueSeverityInformation
      else
        result.Severity := IssueSeverityError;
    finally
      p.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;


function TFHIRServerValidatorContext.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
var
  p : TFhirParameters;
begin
  result := TValidationResult.Create;
  try
    p := FTerminologyServer.validate(vs, code, false);
    try
      result.Message := p.str['message'];
      if p.bool['result'] then
        result.Severity := IssueSeverityInformation
      else
        result.Severity := IssueSeverityError;
    finally
      p.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

end.

