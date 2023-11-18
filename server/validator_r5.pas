unit validator_r5;

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

{$I fhir.inc}

interface

Uses
  SysUtils, Classes,
  fsl_utilities, fsl_threads, fsl_npm_cache,
  fsl_base, fsl_stream,
  fhir_objects, fhir_factory, fhir_common,
  ftx_service,
  fhir5_enums, fhir5_types, fhir5_resources_base, fhir5_resources, fhir5_context, fhir5_profiles, fhir5_client, fhir5_utilities,
  fhir5_pathnode, fhir5_pathengine,
  fhir_valuesets,
  tx_server;

type

  { TFHIRServerWorkerContextR5 }

  TFHIRServerWorkerContextR5 = class (TBaseWorkerContextR5)
  private
    FTerminologyServer : TTerminologyServer;
    FProfile : TFhirExpansionParams;
    FLock : TFslLock;
    FQuestionnaires : TFslMap<TFhirQuestionnaire>;
    FSearchParameters : TFslMap<TFhirSearchParameter>;
    FCompartments : TFslMap<TFhirCompartmentDefinition>;
    FPatientIdExpressions : array [TFhirResourceType] of TFHIRPathExpressionNode;

    procedure SetTerminologyServer(const Value: TTerminologyServer);
    function getQuestionnaire(url : string) : TFhirQuestionnaire;
    procedure loadPatientCompartment;
  public
    constructor Create(factory : TFHIRFactory; pc : TFHIRPackageManager); Override;
    destructor Destroy; Override;

    Function Link : TFHIRServerWorkerContextR5; overload;
    procedure Unload; override;

    procedure SeeResourceProxy(r : TFhirResourceProxy); override;
    procedure checkResource(r : TFhirResource);

    Property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;

    function fetchResource(t : TFhirResourceType; url, version : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFhirValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
    procedure LoadingFinished; override;

    function PatientIdExpression(rt : TFhirResourceType) : TFHIRPathExpressionNode;
  end;

implementation

{ TFHIRServerWorkerContextR5 }

procedure TFHIRServerWorkerContextR5.checkResource(r: TFhirResource);
begin
  r.checkNoImplicitRules('Repository.SeeResource', 'Resource');
  Factory.checkNoModifiers(r, 'Repository.SeeResource', 'Resource');
end;

constructor TFHIRServerWorkerContextR5.Create(factory : TFHIRFactory; pc : TFHIRPackageManager);
begin
  inherited;
  FLock := TFslLock.Create('Validation.questionnaire r5');
  FProfile := TFhirExpansionParams.Create;
  FProfile.includeDefinition := false;
  FProfile.limitedExpansion := false;
  FQuestionnaires := TFslMap<TFhirQuestionnaire>.Create('ctxt.q');
  FSearchParameters := TFslMap<TFhirSearchParameter>.Create('ctxt.sp');
  FCompartments := TFslMap<TFhirCompartmentDefinition>.Create('ctxt.comp');
  FSearchParameters.defaultValue := nil;
  FCompartments.defaultValue := nil;
end;

destructor TFHIRServerWorkerContextR5.Destroy;
var
  a : TFhirResourceType;
begin
  for a := low(TFhirResourceType) to High(TFhirResourceType) do
     FPatientIdExpressions[a].free;
  FSearchParameters.free;
  FCompartments.free;
  FQuestionnaires.free;
  FProfile.free;
  FTerminologyServer.free;
  FLock.free;
  inherited;
end;


function TFHIRServerWorkerContextR5.Link: TFHIRServerWorkerContextR5;
begin
  result := TFHIRServerWorkerContextR5(inherited Link);
end;

procedure TFHIRServerWorkerContextR5.Unload;
begin
  inherited Unload;
  FQuestionnaires.clear;
end;

procedure TFHIRServerWorkerContextR5.LoadingFinished;
begin
  inherited;
  loadPatientCompartment;
end;

procedure TFHIRServerWorkerContextR5.loadPatientCompartment;
var
  comp : TFhirCompartmentDefinition;
  r : TFhirCompartmentDefinitionResource;
  rt : TFhirResourceType;
  first : boolean;
  s : TFHIRString;
  exp : String;
  sp : TFhirSearchParameter;
  fpe : TFHIRPathEngine;
begin
  fpe := TFHIRPathEngine.Create(nil, nil);
  try
    comp := FCompartments['http://hl7.org/fhir/CompartmentDefinition/patient'];
    if comp <> nil then
    begin
      for r in comp.resourceList do
      begin
        rt := ResourceTypeByName(CODES_TFhirResourceTypesEnum[r.code]);
        first := true;
        exp := '';
        for s in r.paramList do
        begin
          sp := FSearchParameters[CODES_TFhirResourceTypesEnum[r.code]+'.'+s.value];
          if (sp <> nil) and (sp.expression <> '') then
          begin
            if first then first := false else exp := exp + ' | ';
            exp := exp + sp.expression;
          end;
        end;
        if exp <> '' then
          FPatientIdExpressions[rt] := fpe.parse(exp);
      end;
    end;
  finally
    fpe.free;
  end;
end;

function TFHIRServerWorkerContextR5.PatientIdExpression(rt: TFhirResourceType): TFHIRPathExpressionNode;
begin
  result := FPatientIdExpressions[rt];
end;

procedure TFHIRServerWorkerContextR5.SeeResourceProxy(r : TFhirResourceProxy);
var
  sp : TFhirSearchParameter;
  b : TFhirEnum;
  s : string;
begin
  if StringArrayExists(TERMINOLOGY_RESOURCES, r.fhirType) then
    FTerminologyServer.SeeSpecificationResource(r)
  else if r.fhirType = 'SearchParameter' then
  begin
    sp := r.resource as TFhirSearchParameter;
    for b in sp.base do
    begin
      s := b.value+'.'+sp.code;
      if not FSearchParameters.ContainsKey(s) then
        FSearchParameters.Add(s, sp.link)
    end;
  end
  else if r.fhirType = 'CompartmentDefinition' then
    FCompartments.Add(r.url, TFhirCompartmentDefinition(r.resource).link)
  else if r.fhirType = 'Questionnaire' then
  begin
    FLock.lock;
    try
      if FQuestionnaires.ContainsKey(r.id) then
        FQuestionnaires[r.id] := (r.resource as TFhirQuestionnaire).link
      else
        FQuestionnaires.add(r.id, (r.resource as TFhirQuestionnaire).link)
    finally
      FLock.Unlock;
    end;
  end
  else
    inherited SeeResourceProxy(r);
end;

function TFHIRServerWorkerContextR5.validateCode(system, version, code: String; vs: TFhirValueSet): TValidationResult;
var
  c : TFHIRCodingW;
  p : TFHIRParametersW;
  vsw : TFHIRValueSetW;
  summary : string;
begin
  vsw := factory.wrapValueSet(vs.Link);
  try
    c := factory.wrapCoding(factory.makeByName('Coding'));
    try
      c.systemUri := system;
      c.code := code;
      c.version := version;
      p := FTerminologyServer.validate(vsw, c, FProfile, false, true, nil, summary);
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
        p.free;
      end;
    finally
      c.free;
    end;
  finally
    vsw.free;
  end;
end;

procedure TFHIRServerWorkerContextR5.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.free;
  FTerminologyServer := Value;
end;

function TFHIRServerWorkerContextR5.fetchResource(t : TFhirResourceType; url, version : String) : TFhirResource;
var
  vsw : TFHIRValueSetW;
begin
  if t = frtValueSet then
  begin
    vsw := FTerminologyServer.getValueSetByUrl(url, version);
    if vsw <> nil then
    begin
      try
        result := vsw.Resource.link as TFhirResource;
      finally
        vsw.free;
      end;
    end
    else
      result := nil;
  end
  else if t = frtQuestionnaire then
    result := getQuestionnaire(url)
  else
    result := inherited fetchResource(t, url, version);
end;

function TFHIRServerWorkerContextR5.getQuestionnaire(url: string): TFhirQuestionnaire;
var
  q : TFhirQuestionnaire;
begin
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

function TFHIRServerWorkerContextR5.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := FSearchParameters[resourceType+'.'+name];
end;

function TFHIRServerWorkerContextR5.expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFhirValueSet;
var
  vsw, res : TFHIRValueSetW;
  limit : integer;
begin
  vsw := factory.wrapValueSet(vs.Link);
  try
    limit := 0;
    if expOptLimited in options then
      limit := 100;
    res := FTerminologyServer.expandVS(vsw, '', FProfile, '', limit, 0, 0, nil);
    try
      result := res.Resource as TFhirValueSet;
    finally
      res.free;
    end;
  finally
    vsw.free;
  end;
end;

function TFHIRServerWorkerContextR5.supportsSystem(system, version : string) : boolean;
begin
  result := FTerminologyServer.supportsSystem(system, version);
end;

function TFHIRServerWorkerContextR5.validateCode(system, version, code, display : String) : TValidationResult;
var
  op : TFHIROperationOutcomeW;
begin
  op := factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
  try
    result := TValidationResult.Create;
    try
      if FTerminologyServer.checkCode(op, langList, '', code, system, version, display) then
        result.Severity := isNull
      else if op.issueCount = 1 then
      begin
        result.Severity := op.severity;
        result.Message := op.text;
      end
      else
      begin
        result.Severity := isError;
        result.Message := 'val5';
      end;
      result.Link;
    finally
      result.free;
    end;
  finally
    op.free;
  end;
end;


function TFHIRServerWorkerContextR5.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
var
  p : TFhirParametersW;
  vsw : TFHIRValueSetW;
  c : TFhirCodingW;
  summary : string;
begin
  vsw := factory.wrapValueSet(vs.Link);
  try
    result := TValidationResult.Create;
    try
      c := factory.wrapCoding(code.Link);
      try
        p := FTerminologyServer.validate(vsw, c, nil, false, true, nil, summary);
        try
          result.Message := p.str('message');
          if p.bool('result') then
            result.Severity := isInformation
          else
            result.Severity := isError;
        finally
          p.free;
        end;
      finally
        c.free;
      end;
      result.Link;
    finally
      result.free;
    end;
  finally
    vsw.free;
  end;
end;


function TFHIRServerWorkerContextR5.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
var
  p : TFhirParametersW;
  vsw : TFHIRValueSetW;
  c : TFhirCodeableConceptW;
  summary : string;
begin
  vsw := factory.wrapValueSet(vs.Link);
  try
    result := TValidationResult.Create;
    try
      c := factory.wrapCodeableConcept(code.Link);
      try
        p := FTerminologyServer.validate('CodeableConcept', vsw, c, FProfile, false, true, false, nil, summary);
        try
          result.Message := p.str('message');
          if p.bool('result') then
            result.Severity := isInformation
          else
            result.Severity := isError;
        finally
          p.free;
        end;
      finally
        c.free;
      end;
      result.Link;
    finally
      result.free;
    end;
  finally
    vsw.free;
  end;
end;

end.
