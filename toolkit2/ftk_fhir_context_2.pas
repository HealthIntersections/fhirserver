unit ftk_fhir_context_2;

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

{$i fhir.inc}


interface

uses
  Classes, SysUtils,
  fsl_utilities, fsl_base, fsl_lang, fsl_npm_cache,
  fhir_objects, fhir_factory, fhir_client, fhir_common,
  ftx_service,
  fhir2_types, fhir2_resources, fhir2_resources_base, fhir2_context, fhir2_profiles, fhir2_client,
  fhir_valuesets;

Type

  { TToolkitValidatorContextR2 }

  TToolkitValidatorContextR2 = class (TBaseWorkerContextR2)
  private
    FLanguages : TIETFLanguageDefinitions;
    FUrl : String;
    FServer : TFhirClient2;
    FCapabilityStatement : TFhirConformance;
    FValueSets : TFslMap<TFHIRValueSet>;
    FCodeSystems : TFslMap<TFHIRValueSet>;
    procedure checkClient;
    function  findCode(list : TFhirValueSetCodeSystemConceptList; code : String; caseSensitive : boolean) : TFhirValueSetCodeSystemConcept;
    function validateInternally(system, version, code: String; vs: TFHIRValueSet; var res : TValidationResult) : boolean;
    function doGetVs(sender : TObject; url : String) : TFHIRValueSetW;
    function doGetCs(sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
    procedure doGetList(sender : TObject; url : String; list : TStringList);
  protected
    procedure SeeResourceProxy(r : TFhirResourceProxy); override;
  public
    constructor Create(factory : TFHIRFactory; languages : TIETFLanguageDefinitions; TerminologyServer : String; pcm : TFHIRPackageManager); virtual;
    destructor Destroy; Override;

    Function Link : TToolkitValidatorContextR2; overload;
    procedure Unload; override;

    function fetchResource(t : TFhirResourceType; url : String) : TFhirResource; override;

    function expand(vs : TFhirValueSet; options : TExpansionOperationOptionSet = []) : TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
  end;

implementation

uses
  fhir_codesystem_service, fhir2_constants, fhir2_utilities;

{ TToolkitValidatorContextR2 }

procedure TToolkitValidatorContextR2.checkClient;
begin
  if (FServer = nil) or (FCapabilityStatement = nil) then
  begin
    if FServer <> nil then
      FServer.Free;
    FServer := Factory.makeClient(self.link, FUrl, fctWinInet, ffJson, 5000) as TFhirClient2;
    FCapabilityStatement := FServer.conformance(true);
    if FCapabilityStatement.fhirVersion <> FHIR_GENERATED_VERSION then
      raise EFHIRException.create('Terminology Server / Toolkit Version mismatch ('+FCapabilityStatement.fhirVersion+' / '+FHIR_GENERATED_VERSION+')');
  end;
end;

constructor TToolkitValidatorContextR2.Create(factory : TFHIRFactory; languages : TIETFLanguageDefinitions; TerminologyServer : String; pcm : TFHIRPackageManager);
begin
  inherited Create(factory, pcm);
  FLanguages := languages;
  FValueSets := TFslMap<TFHIRValueSet>.create('ValueSets');
  FCodeSystems := TFslMap<TFHIRValueSet>.create('CodeSystems');
  FUrl := TerminologyServer;
end;

destructor TToolkitValidatorContextR2.Destroy;
begin
  FValueSets.Free;
  FServer.Free;
  FCapabilityStatement.Free;
  FCodeSystems.Free;
  FLanguages.Free;
  inherited;
end;

function TToolkitValidatorContextR2.doGetCs(sender: TObject; url, version: String; params: TFHIRExpansionParams; nullOk : boolean): TCodeSystemProvider;
var
   cs : TFHIRValueSet;
begin
  cs := FCodeSystems[url];
  if cs = nil then
    raise ETerminologyError.create('Unable to resolve code system '+url);
  result := TFhirCodeSystemProvider.create(FLanguages.link, Factory.link, TFHIRCodeSystemEntry.Create(Factory.wrapCodeSystem(cs.link)));
end;

procedure TToolkitValidatorContextR2.doGetList(sender: TObject; url: String; list: TStringList);
begin
  // todo...
end;

function TToolkitValidatorContextR2.doGetVs(sender: TObject; url: String): TFHIRValueSetW;
var
  vs : TFhirValueSet;
begin
  vs := FValueSets[url];
  if vs = nil then
    raise ETerminologyError.create('Unable to resolve value set '+url);
  result := Factory.wrapValueSet(vs.link);
end;

function TToolkitValidatorContextR2.expand(vs: TFhirValueSet; options : TExpansionOperationOptionSet = []): TFHIRValueSet;
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

function TToolkitValidatorContextR2.fetchResource(t: TFhirResourceType; url: String): TFhirResource;
begin
  if (t = frtValueSet) then
    result := FValueSets[url].link
  else
    result := inherited fetchResource(t, url);
end;

function TToolkitValidatorContextR2.findCode(list: TFhirValueSetCodeSystemConceptList; code: String; caseSensitive : boolean): TFhirValueSetCodeSystemConcept;
var
  d, d1 : TFhirValueSetCodeSystemConcept;
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

function TToolkitValidatorContextR2.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TToolkitValidatorContextR2.Link: TToolkitValidatorContextR2;
begin
  result := TToolkitValidatorContextR2(inherited Link);
end;

procedure TToolkitValidatorContextR2.Unload;
begin
  inherited Unload;
end;

procedure TToolkitValidatorContextR2.SeeResourceProxy(r: TFhirResourceProxy);
var
  vs : TFhirValueset;
begin
  if (r.resource.ResourceType = frtNull) then
  begin
    vs := (r.resource as TFHIRValueSet);
    FValueSets.Add(vs.url, vs.Link);
    if (vs.codeSystem <> nil) then
      FCodeSystems.Add(vs.codeSystem.system, vs.Link)
  end
  else
    inherited;
end;

function TToolkitValidatorContextR2.supportsSystem(system, version: string): boolean;
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

function TToolkitValidatorContextR2.validateCode(system, version, code, display: String): TValidationResult;
var
  pIn, pOut : TFhirParameters;
  cs : TFHIRValueSet;
  def : TFhirValueSetCodeSystemConcept;
begin
  if FCodeSystems.ContainsKey(system) then
  begin
    cs := FCodeSystems[system];
    def := FindCode(cs.conceptList, code, cs.codeSystem.caseSensitive);
    if (def = nil) then
      result := TValidationResult.Create(isError, 'Unknown code ' +code)
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
          result := TValidationResult.Create(isInformation, pOut.str['message'])
        else
          result := TValidationResult.Create(isError, pOut.str['message']);
      finally
        pOut.Free;
      end;
    finally
      pIn.Free;
    end;
  end;
end;

function TToolkitValidatorContextR2.validateCode(system, version, code: String; vs: TFHIRValueSet): TValidationResult;
var
  pIn, pOut : TFhirParameters;
begin
  if validateInternally(system, version, code, vs, result) then
    exit;

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
        result := TValidationResult.Create(isInformation, pOut.str['message'])
      else
        result := TValidationResult.Create(isError, pOut.str['message']);
    finally
      pOut.Free;
    end;
  finally
    pIn.Free;
  end;
end;

function TToolkitValidatorContextR2.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
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
        result := TValidationResult.Create(isInformation, pOut.str['message'])
      else
        result := TValidationResult.Create(isError, pOut.str['message']);
    finally
      pOut.Free;
    end;
  finally
    pIn.Free;
  end;
end;

function TToolkitValidatorContextR2.validateInternally(system, version, code: String; vs: TFHIRValueSet; var res: TValidationResult): boolean;
var
  vsw : TFhirValueSetW;
  validator : TValueSetChecker;
  p : TFHIRParametersW;
  params : TFHIRExpansionParams;
begin
  try
    vsw := Factory.wrapValueSet(vs.Link);
    try
      validator := TValueSetChecker.Create(Factory.link, doGetVs, doGetCs, doGetList, nil, nil, FLanguages.link, '', nil);
      try
        params := TFHIRExpansionParams.Create;
        try
          validator.prepare(vsw, params);
          p := validator.check('code', system, version, code, false);
          try
            res := TValidationResult.create;
            if p.bool('result') then
              res.Severity := isInformation
            else
            begin
              res.Severity := isError;
              res.Message := p.str('message');
            end;
          finally
            p.Free;
          end;
        finally
          params.Free;
        end;
      finally
        validator.Free;
      end;
    finally
      vsw.Free;
    end;
    result := true;
  except
    result := false;
  end;
end;

function TToolkitValidatorContextR2.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
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
        result := TValidationResult.Create(isInformation, pOut.str['message'])
      else
        result := TValidationResult.Create(isError, pOut.str['message']);
    finally
      pOut.Free;
    end;
  finally
    pIn.Free;
  end;
end;

end.

