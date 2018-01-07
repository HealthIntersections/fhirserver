Unit ServerValidator;

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


Interface

Uses
  SysUtils, Classes,
  StringSupport, kCritSct,
  AdvObjects, AdvGenerics, AdvBuffers, AdvNameBuffers, AdvMemories, AdvVclStreams, AdvZipReaders, AdvZipParts,
  FHIRTypes, FHIRResources, FHIRValidator, FHIRParser, FHIRContext, FHIRUtilities, FHIRSupport, FHIRProfileUtilities, FHIRConstants,
  TerminologyServer;

Type

  TFHIRServerWorkerContext = class (TBaseWorkerContext)
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

    Function Link : TFHIRServerWorkerContext; overload;

    procedure checkResource(r : TFhirResource);
    procedure SeeResource(r : TFhirResource); override;

    Property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet) : TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;

  end;

Implementation

{ TFHIRServerWorkerContext }

procedure TFHIRServerWorkerContext.checkResource(r: TFhirResource);
begin
  r.checkNoImplicitRules('Repository.SeeResource', 'Resource');
  TFhirDomainResource(r).checkNoModifiers('Repository.SeeResource', 'Resource');
end;

constructor TFHIRServerWorkerContext.Create;
begin
  inherited;
  FLock := TCriticalSection.Create('Validation.questionnaire');
  FProfile := TFhirExpansionProfile.create;
  FProfile.includeDefinition := false;
  FProfile.limitedExpansion := false;
  FQuestionnaires := TAdvMap<TFhirQuestionnaire>.create;
end;

destructor TFHIRServerWorkerContext.Destroy;
begin
  FQuestionnaires.Free;
  FProfile.Free;
  FTerminologyServer.Free;
  FLock.Free;
  inherited;
end;


function TFHIRServerWorkerContext.Link: TFHIRServerWorkerContext;
begin
  result := TFHIRServerWorkerContext(inherited Link);
end;

procedure TFHIRServerWorkerContext.SeeResource(r : TFhirResource);
begin
  checkResource(r);
  if (r.ResourceType in [frtValueSet, frtConceptMap {$IFDEF FHIR3}, frtCodeSystem{$ENDIF}]) then
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

function TFHIRServerWorkerContext.validateCode(system, version, code: String; vs: TFHIRValueSet): TValidationResult;
var
  c : TFHIRCoding;
  p : TFHIRParameters;
begin
  c := TFhirCoding.Create;
  try
    c.system := system;
    c.code := code;
    c.version := version;
    p := FTerminologyServer.validate(vs, c, FProfile, false);
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

procedure TFHIRServerWorkerContext.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

function TFHIRServerWorkerContext.fetchResource(t : TFhirResourceType; url : String) : TFhirResource;
begin
  if t = frtValueSet then
    result := FTerminologyServer.getValueSetByUrl(url)
  else if t = frtQuestionnaire then
    result := getQuestionnaire(url)
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRServerWorkerContext.getQuestionnaire(url: string): TFhirQuestionnaire;
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

function TFHIRServerWorkerContext.expand(vs : TFhirValueSet) : TFHIRValueSet;
begin
  result := FTerminologyServer.expandVS(vs, '', FProfile, '', 0, 0, 0);
end;

function TFHIRServerWorkerContext.supportsSystem(system, version : string) : boolean;
begin
  result := FTerminologyServer.supportsSystem(system, version);
end;

function TFHIRServerWorkerContext.validateCode(system, version, code, display : String) : TValidationResult;
var
  op : TFHIROperationOutcome;
begin
  op := TFHIROperationOutcome.create;
  try
    result := TValidationResult.Create;
    try
      if FTerminologyServer.checkCode(op, 'en', '', code, system, version, display) then
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


function TFHIRServerWorkerContext.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
var
  p : TFhirParameters;
begin
  result := TValidationResult.Create;
  try
    p := FTerminologyServer.validate(vs, code, nil, false);
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


function TFHIRServerWorkerContext.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
var
  p : TFhirParameters;
begin
  result := TValidationResult.Create;
  try
    p := FTerminologyServer.validate(vs, code, FProfile, false);
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


