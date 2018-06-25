unit FHIR.Server.ValidatorR2;

interface

Uses
  SysUtils, Classes,
  FHIR.Support.Utilities, FHIR.Support.Threads,
  FHIR.Support.Base, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.Common,
  FHIR.Tx.Service,
  FHIR.R2.Types, FHIR.R2.Resources, FHIR.R2.Context, FHIR.R2.Profiles, FHIR.R2.Client,
  FHIR.Tools.ValueSets,
  FHIR.Tx.Server;

Type
  TFHIRServerWorkerContextR2 = class (TBaseWorkerContextR2)
  private
    FTerminologyServer : TTerminologyServer;
    FProfile : TFhirExpansionParams;
    FLock : TFslLock;
    FQuestionnaires : TFslMap<TFhirQuestionnaire>;

    procedure SetTerminologyServer(const Value: TTerminologyServer);
    function getQuestionnaire(url : string) : TFhirQuestionnaire;
  protected
    procedure SeeResource(r : TFhirResource); override;
  public
    Constructor Create(factory : TFHIRFactory); Override;
    Destructor Destroy; Override;

    Function Link : TFHIRServerWorkerContextR2; overload;

    procedure checkResource(r : TFhirResource);

    Property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet) : TFhirValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;
  end;

implementation

{ TFHIRServerWorkerContextR2 }

procedure TFHIRServerWorkerContextR2.checkResource(r: TFhirResource);
begin
  r.checkNoImplicitRules('Repository.SeeResource', 'Resource');
  Factory.checkNoModifiers(r, 'Repository.SeeResource', 'Resource');
end;

constructor TFHIRServerWorkerContextR2.Create(factory : TFHIRFactory);
begin
  inherited;
  FLock := TFslLock.Create('Validation.questionnaire');
  FProfile := TFhirExpansionParams.create;
  FProfile.includeDefinition := false;
  FProfile.limitedExpansion := false;
  FQuestionnaires := TFslMap<TFhirQuestionnaire>.create;
end;

destructor TFHIRServerWorkerContextR2.Destroy;
begin
  FQuestionnaires.Free;
  FProfile.Free;
  FTerminologyServer.Free;
  FLock.Free;
  inherited;
end;


function TFHIRServerWorkerContextR2.Link: TFHIRServerWorkerContextR2;
begin
  result := TFHIRServerWorkerContextR2(inherited Link);
end;

procedure TFHIRServerWorkerContextR2.SeeResource(r : TFhirResource);
begin
  checkResource(r);
  if (r.ResourceType in [frtValueSet, frtConceptMap]) then
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

function TFHIRServerWorkerContextR2.validateCode(system, version, code: String; vs: TFhirValueSet): TValidationResult;
var
  c : TFHIRCodingW;
  p : TFHIRParametersW;
  vsw : TFHIRValueSetW;
begin
  vsw := factory.wrapValueSet(vs.Link);
  try
    c := factory.wrapCoding(factory.makeByName('Coding'));
    try
      c.system := system;
      c.code := code;
      c.version := version;
      p := FTerminologyServer.validate(vsw, c, FProfile, false);
      try
        result := TValidationResult.Create;
        try
          if not p.bool('result') then
          begin
            result.Severity := isError;
            result.Message := p.str('message');
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
  finally
    vsw.free;
  end;
end;

procedure TFHIRServerWorkerContextR2.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

function TFHIRServerWorkerContextR2.fetchResource(t : TFhirResourceType; url : String) : TFhirResource;
begin
  if t = frtValueSet then
    result := FTerminologyServer.getValueSetByUrl(url).Resource as TFhirResource
  else if t = frtQuestionnaire then
    result := getQuestionnaire(url)
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRServerWorkerContextR2.getQuestionnaire(url: string): TFhirQuestionnaire;
var
  q : TFhirQuestionnaire;
begin
  result := nil;
  if url.StartsWith('Questionnaire/') then
    url := url.Substring(12);
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

function TFHIRServerWorkerContextR2.expand(vs : TFhirValueSet) : TFhirValueSet;
var
  vsw, res : TFHIRValueSetW;
begin
  vsw := factory.wrapValueSet(vs.Link);
  try
    res := FTerminologyServer.expandVS(vsw, '', FProfile, '', 0, 0, 0);
    try
      result := res.Resource as TFhirValueSet;
    finally
      res.Free;
    end;
  finally
    vsw.Free;
  end;
end;

function TFHIRServerWorkerContextR2.supportsSystem(system, version : string) : boolean;
begin
  result := FTerminologyServer.supportsSystem(system, version);
end;

function TFHIRServerWorkerContextR2.validateCode(system, version, code, display : String) : TValidationResult;
var
  op : TFHIROperationOutcomeW;
begin
  op := factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
  try
    result := TValidationResult.Create;
    try
      if FTerminologyServer.checkCode(op, 'en', '', code, system, version, display) then
        result.Severity := isNull
      else if op.issueCount = 1 then
      begin
        result.Severity := op.severity;
        result.Message := op.text;
      end
      else
      begin
        result.Severity := isError;
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


function TFHIRServerWorkerContextR2.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
var
  p : TFhirParametersW;
  vsw : TFHIRValueSetW;
  c : TFhirCodingW;
begin
  vsw := factory.wrapValueSet(vs.Link);
  try
    result := TValidationResult.Create;
    try
      c := factory.wrapCoding(code.Link);
      try
        p := FTerminologyServer.validate(vsw, c, nil, false);
        try
          result.Message := p.str('message');
          if p.bool('result') then
            result.Severity := isInformation
          else
            result.Severity := isError;
        finally
          p.Free;
        end;
      finally
        c.Free;
      end;
      result.Link;
    finally
      result.Free;
    end;
  finally
    vsw.Free;
  end;
end;


function TFHIRServerWorkerContextR2.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
var
  p : TFhirParametersW;
  vsw : TFHIRValueSetW;
  c : TFhirCodingW;
begin
  vsw := factory.wrapValueSet(vs.Link);
  try
    result := TValidationResult.Create;
    try
      c := factory.wrapCoding(code.Link);
      try
        p := FTerminologyServer.validate(vsw, c, FProfile, false);
        try
          result.Message := p.str('message');
          if p.bool('result') then
            result.Severity := isInformation
          else
            result.Severity := isError;
        finally
          p.Free;
        end;
      finally
        c.Free;
      end;
      result.Link;
    finally
      result.Free;
    end;
  finally
    vsw.Free;
  end;
end;

end.
