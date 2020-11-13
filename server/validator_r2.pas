unit validator_r2;

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
  fsl_utilities, fsl_threads,
  fsl_base, fsl_stream,
  fhir_objects, fhir_factory, fhir_common,
  ftx_service,
  fhir2_types, fhir2_context, fhir2_profiles, fhir2_client,
  fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_other,
  fhir_valuesets,
  tx_server;

Type
  TFHIRServerWorkerContextR2 = class (TBaseWorkerContextR2)
  private
    FTerminologyServer : TTerminologyServer;
    FProfile : TFhirExpansionParams;
    FLock : TFslLock;
    FQuestionnaires : TFslMap<TFhirQuestionnaire>;

    procedure SetTerminologyServer(const Value: TTerminologyServer);
    function getQuestionnaire(url : string) : TFhirQuestionnaire;
  public
    constructor Create(factory : TFHIRFactory); Override;
    destructor Destroy; Override;

    Function Link : TFHIRServerWorkerContextR2; overload;

    procedure checkResource(r : TFhirResource);
    procedure SeeResource(r : TFhirResource); override;

    Property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFhirValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;
    procedure LoadingFinished; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
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
  FQuestionnaires := TFslMap<TFhirQuestionnaire>.create('questionnaires');
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

procedure TFHIRServerWorkerContextR2.LoadingFinished;
begin
  inherited;
  // nothing
end;

procedure TFHIRServerWorkerContextR2.SeeResource(r : TFhirResource);
begin
  checkResource(r);
  if r is TFHIRDomainResource then
    TFHIRDomainResource(r).text := nil;
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
      c.systemUri := system;
      c.code := code;
      c.version := version;
      p := FTerminologyServer.validate(vsw, c, FProfile, false, true, nil);
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
var
  vsw : TFHIRValueSetW;
begin
  if t = frtValueSet then
  begin
    vsw := FTerminologyServer.getValueSetByUrl(url);
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
    result := inherited fetchResource(t, url);
end;

function TFHIRServerWorkerContextR2.getQuestionnaire(url: string): TFhirQuestionnaire;
var
  q : TFhirQuestionnaire;
begin
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

function TFHIRServerWorkerContextR2.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TFHIRServerWorkerContextR2.expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFhirValueSet;
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
      if FTerminologyServer.checkCode(op, lang, '', code, system, version, display) then
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
        p := FTerminologyServer.validate(vsw, c, nil, false, true, nil);
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
  c : TFhirCodeableConceptW;
begin
  vsw := factory.wrapValueSet(vs.Link);
  try
    result := TValidationResult.Create;
    try
      c := factory.wrapCodeableConcept(code.Link);
      try
        p := FTerminologyServer.validate(vsw, c, FProfile, false, true, nil);
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
