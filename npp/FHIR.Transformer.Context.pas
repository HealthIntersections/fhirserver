unit FHIR.Transformer.Context;

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

uses
  SysUtils, Classes,
  fsl_utilities, fsl_threads, fsl_base, fsl_stream,
  fhir_objects, fhir_factory, fhir_common, fhir_oids,
  fhir4_types, fhir4_resources, fhir4_resources_base, fhir4_context, fhir4_profiles, fhir4_client, fhir4_utilities;

type
  TFHIRTransformerContext = class (TBaseWorkerContextR4)
  private
    FQuestionnaires : TFslMap<TFhirQuestionnaire>;
    FValueSets : TFslMap<fhir4_resources.TFHIRValueSet>;
    FCodeSystems : TFslMap<fhir4_resources.TFHIRCodeSystem>;
    FConceptMaps : TFslMap<fhir4_resources.TFHIRConceptMap>;

    function getQuestionnaire(url : string) : TFhirQuestionnaire;
  public
    constructor Create(factory : TFHIRFactory); Override;
    destructor Destroy; Override;

    Function Link : TFHIRTransformerContext; overload;

    procedure SeeResource(r : TFhirResource); override;
    procedure checkResource(r : TFhirResource);

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFhirValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
  end;

implementation

{ TFHIRTransformerContext }

procedure TFHIRTransformerContext.checkResource(r: TFhirResource);
begin
  r.checkNoImplicitRules('Repository.SeeResource', 'Resource');
  Factory.checkNoModifiers(r, 'Repository.SeeResource', 'Resource');
end;

constructor TFHIRTransformerContext.Create(factory : TFHIRFactory);
begin
  inherited;
  FValueSets := TFslMap<fhir4_resources.TFHIRValueSet>.create('Value Sets');
  FCodeSystems := TFslMap<fhir4_resources.TFHIRCodeSystem>.create('Code Systems');
  FQuestionnaires := TFslMap<TFhirQuestionnaire>.create('Questionnaires');
  FConceptMaps := TFslMap<fhir4_resources.TFHIRConceptMap>.create('Concept Maps');
end;

destructor TFHIRTransformerContext.Destroy;
begin
  FValueSets.Free;
  FCodeSystems.Free;
  FQuestionnaires.Free;
  FConceptMaps.Free;
  inherited;
end;


function TFHIRTransformerContext.Link: TFHIRTransformerContext;
begin
  result := TFHIRTransformerContext(inherited Link);
end;

procedure TFHIRTransformerContext.SeeResource(r : TFhirResource);
begin
  checkResource(r);
  if (r.ResourceType = frtValueSet) then
    FValueSets.AddOrSetValue(TFhirValueSet(r).url, TFhirValueSet(r).Link)
  else if (r.ResourceType = frtCodeSystem) then
    FCodeSystems.AddOrSetValue(TFhirCodeSystem(r).url, TFhirCodeSystem(r).Link)
  else if (r.ResourceType = frtConceptMap) then
    FConceptMaps.AddOrSetValue(TFhirConceptMap(r).url, TFhirConceptMap(r).Link)
  else if r.resourceType = frtQuestionnaire then
    FQuestionnaires.AddOrSetValue(TFhirQuestionnaire(r).url, TFhirQuestionnaire(r).Link)
  else
    inherited SeeResource(r);
end;

function TFHIRTransformerContext.validateCode(system, version, code: String; vs: TFhirValueSet): TValidationResult;
//var
//  c : TFHIRCodingW;
//  p : TFHIRParametersW;
//  vsw : TFHIRValueSetW;
begin
  raise Exception.Create('Not done yet');
//  vsw := factory.wrapValueSet(vs.Link);
//  try
//    c := factory.wrapCoding(factory.makeByName('Coding'));
//    try
//      c.system := system;
//      c.code := code;
//      c.version := version;
//      p := FTerminologyServer.validate(vsw, c, FProfile, false, true);
//      try
//        result := TValidationResult.Create;
//        try
//          if not p.bool('result') then
//          begin
//            result.Severity := isError;
//            result.Message := p.str('message');
//          end;
//          result.Link;
//        finally
//          result.free;
//        end;
//      finally
//        p.Free;
//      end;
//    finally
//      c.Free;
//    end;
//  finally
//    vsw.free;
//  end;
end;

function TFHIRTransformerContext.fetchResource(t : TFhirResourceType; url : String) : TFhirResource;
begin
  if (t in [frtNull, frtValueSet]) and FValueSets.ContainsKey(url) then
    result := FValueSets[url].Link
  else if (t in [frtNull, frtCodeSystem]) and FCodeSystems.ContainsKey(url) then
    result := FCodeSystems[url].Link
  else if (t in [frtNull, frtConceptMap]) and FConceptMaps.ContainsKey(url) then
    result := FConceptMaps[url].Link
  else if (t in [frtNull, frtQuestionnaire]) and FQuestionnaires.ContainsKey(url) then
    result := FQuestionnaires[url].Link
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRTransformerContext.getQuestionnaire(url: string): TFhirQuestionnaire;
//var
//  q : TFhirQuestionnaire;
begin
  raise Exception.Create('Not done yet');
//  result := nil;
//  if url.StartsWith('Questionnaire/') then
//    url := url.Substring(14);
//  FLock.lock;
//  try
//    if FQuestionnaires.TryGetValue(url, q) then
//      exit(q.Link)
//    else
//      exit(nil);
//  finally
//    FLock.Unlock;
//  end;
end;

function TFHIRTransformerContext.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TFHIRTransformerContext.expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFhirValueSet;
//var
//  vsw, res : TFHIRValueSetW;
//  limit : integer;
begin
  raise Exception.Create('Not done yet');
//  vsw := factory.wrapValueSet(vs.Link);
//  try
//    limit := 0;
//    if expOptLimited in options then
//      limit := 100;
//    res := FTerminologyServer.expandVS(vsw, '', FProfile, '', limit, 0, 0);
//    try
//      result := res.Resource as TFhirValueSet;
//    finally
//      res.Free;
//    end;
//  finally
//    vsw.Free;
//  end;
end;

function TFHIRTransformerContext.supportsSystem(system, version : string) : boolean;
begin
  raise Exception.Create('Not done yet');
//  result := FTerminologyServer.supportsSystem(system, version);
end;

function TFHIRTransformerContext.validateCode(system, version, code, display : String) : TValidationResult;
//var
//  op : TFHIROperationOutcomeW;
begin
  raise Exception.Create('Not done yet');
//  op := factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
//  try
//    result := TValidationResult.Create;
//    try
//      if FTerminologyServer.checkCode(op, THTTPLanguages.create('en'), '', code, system, version, display) then
//        result.Severity := isNull
//      else if op.issueCount = 1 then
//      begin
//        result.Severity := op.severity;
//        result.Message := op.text;
//      end
//      else
//      begin
//        result.Severity := isError;
//        result.Message := '??';
//      end;
//      result.Link;
//    finally
//      result.Free;
//    end;
//  finally
//    op.Free;
//  end;
end;


function TFHIRTransformerContext.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
//var
//  p : TFhirParametersW;
//  vsw : TFHIRValueSetW;
//  c : TFhirCodingW;
begin
  raise Exception.Create('Not done yet');
//  vsw := factory.wrapValueSet(vs.Link);
//  try
//    result := TValidationResult.Create;
//    try
//      c := factory.wrapCoding(code.Link);
//      try
//        p := FTerminologyServer.validate(vsw, c, nil, false, true);
//        try
//          result.Message := p.str('message');
//          if p.bool('result') then
//            result.Severity := isInformation
//          else
//            result.Severity := isError;
//        finally
//          p.Free;
//        end;
//      finally
//        c.Free;
//      end;
//      result.Link;
//    finally
//      result.Free;
//    end;
//  finally
//    vsw.Free;
//  end;
end;


function TFHIRTransformerContext.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
//var
//  p : TFhirParametersW;
//  vsw : TFHIRValueSetW;
//  c : TFhirCodeableConceptW;
begin
  raise Exception.Create('Not done yet');
//  vsw := factory.wrapValueSet(vs.Link);
//  try
//    result := TValidationResult.Create;
//    try
//      c := factory.wrapCodeableConcept(code.Link);
//      try
//        p := FTerminologyServer.validate(vsw, c, FProfile, false, true);
//        try
//          result.Message := p.str('message');
//          if p.bool('result') then
//            result.Severity := isInformation
//          else
//            result.Severity := isError;
//        finally
//          p.Free;
//        end;
//      finally
//        c.Free;
//      end;
//      result.Link;
//    finally
//      result.Free;
//    end;
//  finally
//    vsw.Free;
//  end;
end;

end.
