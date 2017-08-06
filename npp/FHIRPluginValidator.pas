unit FHIRPluginValidator;


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

interface

Uses
  SysUtils, Classes, ActiveX, ComObj,
  MsXml, MsXmlParser,
  StringSupport,
  AdvObjects, AdvBuffers, AdvNameBuffers, AdvMemories, AdvVclStreams, AdvZipReaders, AdvZipParts, AdvGenerics,
  FHIRTypes, FHIRResources, FHIRValidator, FHIRParser, FHIRUtilities, FHIRProfileUtilities,
  FHIRConstants, FHIRClient, FHIRBase, FHIRContext;

Type
  TFHIRPluginValidatorContext = class (TBaseWorkerContext)
  private
    FUrl : String;
    FServer : TFhirHTTPClient;
    FCapabilityStatement : TFHIRCapabilityStatement;
    FValueSets : TAdvMap<TFHIRValueSet>;
    FCodeSystems : TAdvMap<TFHIRCodeSystem>;
    procedure checkClient;
    function  findCode(list : TFhirCodeSystemConceptList; code : String; caseSensitive : boolean) : TFhirCodeSystemConcept;
  protected
    procedure SeeResource(r : TFhirResource); override;
  public
    Constructor Create(terminologyServer : String); virtual;
    Destructor Destroy; Override;

    Function Link : TFHIRPluginValidatorContext; overload;

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet) : TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;
  end;


implementation

{ TFHIRPluginValidatorContext }

procedure TFHIRPluginValidatorContext.checkClient;
begin
  if (FServer = nil) or (FCapabilityStatement = nil) then
  begin
    if FServer <> nil then
      FServer.Free;
    FServer := TFhirHTTPClient.Create(self.link, FUrl, true);
    FServer.timeout := 5000;
    FServer.allowR2 := true;
    FCapabilityStatement := FServer.conformance(true);
    if FCapabilityStatement.fhirVersion <> FHIR_GENERATED_VERSION then
      raise Exception.Create('Terminology Server / Plug-in Version mismatch ('+FCapabilityStatement.fhirVersion+' / '+FHIR_GENERATED_VERSION+')');
  end;
end;

constructor TFHIRPluginValidatorContext.Create(terminologyServer : String);
begin
  inherited Create;
  FValueSets := TAdvMap<TFHIRValueSet>.create;
  FCodeSystems := TAdvMap<TFHIRCodeSystem>.create;
  FUrl := terminologyServer;
end;

destructor TFHIRPluginValidatorContext.Destroy;
begin
  FValueSets.Free;
  FServer.Free;
  FCapabilityStatement.Free;
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
    result := FValueSets[url].link
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRPluginValidatorContext.findCode(list: TFhirCodeSystemConceptList; code: String; caseSensitive : boolean): TFhirCodeSystemConcept;
var
  d, d1 : TFhirCodeSystemConcept;
begin
  result := nil;
  for d in list do
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
    {$IFDEF FHIR2}
    if (vs.codeSystem <> nil) then
      FCodeSystems.Add(vs.codeSystem.system, vs.Link)
    {$ENDIF}
  end
  {$IFDEF FHIR3}
  else if (r.ResourceType = frtCodeSystem) then
    FCodeSystems.Add(TFHIRCodeSystem(r).url, TFHIRCodeSystem(r).Link)
  {$ENDIF}
  else
    inherited;
end;

function TFHIRPluginValidatorContext.supportsSystem(system, version: string): boolean;
var
  ex : TFhirExtension;
begin
  CheckClient;
  result := FCodeSystems.ContainsKey(system);
  if (not result) then
    for ex in FCapabilityStatement.extensionList do
      if (ex.url = 'http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system') and (ex.value is TFHIRString) and (TFHIRString(ex.value).value = system) then
        result := true;
end;

function TFHIRPluginValidatorContext.validateCode(system, version, code, display: String): TValidationResult;
var
  pIn, pOut : TFhirParameters;
  cs : TFHIRCodeSystem;
  def : TFhirCodeSystemConcept;
begin
  if FCodeSystems.ContainsKey(system) then
  begin
    cs := FCodeSystems[system];
    def := FindCode(cs.conceptList, code, cs.caseSensitive);
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

function TFHIRPluginValidatorContext.validateCode(system, version, code: String; vs: TFHIRValueSet): TValidationResult;
var
  pIn, pOut : TFhirParameters;
  def : TFhirCodeSystemConcept;
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
