unit FHIR.Npp.Validator;


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
  fsl_utilities, fsl_base,
  fhir_objects, fhir_factory, fhir_client, fhir_common, 
  ftx_service,
  fhir2_types, fhir2_resources, fhir2_resources_base, fhir2_context, fhir2_profiles, fhir2_client,
  fhir3_types, fhir3_resources, fhir3_resources_base, fhir3_context, fhir3_profiles, fhir3_client,
  fhir4_types, fhir4_resources, fhir4_resources_base, fhir4_context, fhir4_profiles, fhir4_client,
  fhir_valuesets.pas;

Type
  TFHIRPluginValidatorContextR2 = class (TBaseWorkerContextR2)
  private
    FUrl : String;
    FServer : fhir2_client.TFhirClient2;
    FCapabilityStatement : fhir2_resources.TFhirConformance;
    FValueSets : TFslMap<fhir2_resources.TFHIRValueSet>;
    FCodeSystems : TFslMap<fhir2_resources.TFHIRValueSet>;
    procedure checkClient;
    function  findCode(list : fhir2_resources.TFhirValueSetCodeSystemConceptList; code : String; caseSensitive : boolean) : fhir2_resources.TFhirValueSetCodeSystemConcept;
    function validateInternally(system, version, code: String; vs: fhir2_resources.TFHIRValueSet; var res : TValidationResult) : boolean;
    function doGetVs(sender : TObject; url : String) : TFHIRValueSetW;
    function doGetCs(sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
  protected
    procedure SeeResource(r : fhir2_resources.TFhirResource); override;
  public
    constructor Create(factory : TFHIRFactory; TerminologyServer : String); virtual;
    destructor Destroy; Override;

    Function Link : TFHIRPluginValidatorContextR2; overload;

    function fetchResource(t : fhir2_resources.TFhirResourceType; url : String) : fhir2_resources.TFhirResource; override;

    function expand(vs : fhir2_resources.TFhirValueSet; options : TExpansionOperationOptionSet = []) : fhir2_resources.TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : fhir2_resources.TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : fhir2_types.TFHIRCoding; vs : fhir2_resources.TFhirValueSet) : TValidationResult; override;
    function validateCode(code : fhir2_types.TFHIRCodeableConcept; vs : fhir2_resources.TFhirValueSet) : TValidationResult; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
  end;

  TFHIRPluginValidatorContextR3 = class (TBaseWorkerContextR3)
  private
    FUrl : String;
    FServer : fhir3_client.TFhirClient3;
    FCapabilityStatement : fhir3_resources.TFHIRCapabilityStatement;
    FValueSets : TFslMap<fhir3_resources.TFHIRValueSet>;
    FCodeSystems : TFslMap<fhir3_resources.TFHIRCodeSystem>;
    procedure checkClient;
    function  findCode(list : fhir3_resources.TFhirCodeSystemConceptList; code : String; caseSensitive : boolean) : fhir3_resources.TFhirCodeSystemConcept;
    function validateInternally(system, version, code: String; vs: fhir3_resources.TFHIRValueSet; var res : TValidationResult) : boolean;
    function doGetVs(sender : TObject; url : String) : TFHIRValueSetW;
    function doGetCs(sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
  protected
    procedure SeeResource(r : fhir3_resources.TFhirResource); override;
  public
    constructor Create(factory : TFHIRFactory; TerminologyServer : String); virtual;
    destructor Destroy; Override;

    Function Link : TFHIRPluginValidatorContextR3; overload;

    function fetchResource(t : fhir3_resources.TFhirResourceType; url : String) : fhir3_resources.TFhirResource; override;

    function expand(vs : fhir3_resources.TFhirValueSet; options : TExpansionOperationOptionSet = []) : fhir3_resources.TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : fhir3_resources.TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : fhir3_types.TFHIRCoding; vs : fhir3_resources.TFhirValueSet) : TValidationResult; override;
    function validateCode(code : fhir3_types.TFHIRCodeableConcept; vs : fhir3_resources.TFhirValueSet) : TValidationResult; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
  end;

  TFHIRPluginValidatorContextR4 = class (TBaseWorkerContextR4)
  private
    FUrl : String;
    FServer : fhir4_client.TFhirClient4;
    FCapabilityStatement : fhir4_resources.TFHIRCapabilityStatement;
    FValueSets : TFslMap<fhir4_resources.TFHIRValueSet>;
    FCodeSystems : TFslMap<fhir4_resources.TFHIRCodeSystem>;
    procedure checkClient;
    function findCode(list : fhir4_resources.TFhirCodeSystemConceptList; code : String; caseSensitive : boolean) : fhir4_resources.TFhirCodeSystemConcept;
    function validateInternally(system, version, code: String; vs: fhir4_resources.TFHIRValueSet; var res : TValidationResult) : boolean;
    function doGetVs(sender : TObject; url : String) : TFHIRValueSetW;
    function doGetCs(sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
  protected
    procedure SeeResource(r : fhir4_resources.TFhirResource); override;
  public
    constructor Create(factory : TFHIRFactory; TerminologyServer : String); virtual;
    destructor Destroy; Override;

    Function Link : TFHIRPluginValidatorContextR4; overload;

    function fetchResource(t : fhir4_resources.TFhirResourceType; url : String) : fhir4_resources.TFhirResource; override;

    function expand(vs : fhir4_resources.TFhirValueSet; options : TExpansionOperationOptionSet = []) : fhir4_resources.TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : fhir4_resources.TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : fhir4_types.TFHIRCoding; vs : fhir4_resources.TFhirValueSet) : TValidationResult; override;
    function validateCode(code : fhir4_types.TFHIRCodeableConcept; vs : fhir4_resources.TFhirValueSet) : TValidationResult; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
  end;

  TFHIRPluginValidatorContext = class
  public
    class function create(factory : TFHIRFactory; TerminologyServer : String) : TFHIRWorkerContextWithFactory;
  end;

implementation

uses
  fhir_codesystem_service,
  fhir2_constants, fhir2_utilities,
  fhir3_constants, fhir3_utilities,
  fhir4_constants, fhir4_utilities;

{ TFHIRPluginValidatorContextR2 }

procedure TFHIRPluginValidatorContextR2.checkClient;
begin
  if (FServer = nil) or (FCapabilityStatement = nil) then
  begin
    if FServer <> nil then
      FServer.Free;
    FServer := Factory.makeClient(self.link, FUrl, fctWinInet, ffJson, 5000) as fhir2_client.TFhirClient2;
    FCapabilityStatement := FServer.conformance(true);
    if FCapabilityStatement.fhirVersion <> fhir2_constants.FHIR_GENERATED_VERSION then
      raise EFHIRException.create('Terminology Server / Plug-in Version mismatch ('+FCapabilityStatement.fhirVersion+' / '+fhir2_constants.FHIR_GENERATED_VERSION+')');
  end;
end;

constructor TFHIRPluginValidatorContextR2.Create(factory : TFHIRFactory; TerminologyServer : String);
begin
  inherited Create(factory);
  FValueSets := TFslMap<fhir2_resources.TFHIRValueSet>.create('ValueSets');
  FCodeSystems := TFslMap<fhir2_resources.TFHIRValueSet>.create('CodeSystems');
  FUrl := TerminologyServer;
end;

destructor TFHIRPluginValidatorContextR2.Destroy;
begin
  FValueSets.Free;
  FServer.Free;
  FCapabilityStatement.Free;
  FCodeSystems.Free;
  inherited;
end;

function TFHIRPluginValidatorContextR2.doGetCs(sender: TObject; url, version: String; params: TFHIRExpansionParams; nullOk : boolean): TCodeSystemProvider;
var
  cs : fhir2_resources.TFHIRValueSet;
begin
  cs := FCodeSystems[url];
  if cs = nil then
    raise ETerminologyError.create('Unable to resolve code system '+url);
  result := TFhirCodeSystemProvider.create(Factory.link, TFHIRCodeSystemEntry.Create(Factory.wrapCodeSystem(cs.link)));
end;

function TFHIRPluginValidatorContextR2.doGetVs(sender: TObject; url: String): TFHIRValueSetW;
var
  vs : fhir2_resources.TFhirValueSet;
begin
  vs := FValueSets[url];
  if vs = nil then
    raise ETerminologyError.create('Unable to resolve value set '+url);
  result := Factory.wrapValueSet(vs.link);
end;

function TFHIRPluginValidatorContextR2.expand(vs: fhir2_resources.TFhirValueSet; options : TExpansionOperationOptionSet = []): fhir2_resources.TFHIRValueSet;
var
  pIn : fhir2_resources.TFhirParameters;
begin
  CheckClient;
  pIn := fhir2_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('valueSet', vs.Link);
    pIn.AddParameter('_incomplete', true);
    pIn.AddParameter('_limit', '10');
    result := FServer.operation(fhir2_resources_base.frtValueSet, 'expand', pIn) as fhir2_resources.TFhirValueSet;
  finally
    pIn.Free;
  end;

end;

function TFHIRPluginValidatorContextR2.fetchResource(t: fhir2_resources.TFhirResourceType; url: String): fhir2_resources.TFhirResource;
begin
  if (t = fhir2_resources_base.frtValueSet) then
    result := FValueSets[url].link
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRPluginValidatorContextR2.findCode(list: fhir2_resources.TFhirValueSetCodeSystemConceptList; code: String; caseSensitive : boolean): fhir2_resources.TFhirValueSetCodeSystemConcept;
var
  d, d1 : fhir2_resources.TFhirValueSetCodeSystemConcept;
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

function TFHIRPluginValidatorContextR2.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TFHIRPluginValidatorContextR2.Link: TFHIRPluginValidatorContextR2;
begin
  result := TFHIRPluginValidatorContextR2(inherited Link);
end;

procedure TFHIRPluginValidatorContextR2.SeeResource(r: fhir2_resources.TFhirResource);
var
  vs : fhir2_resources.TFhirValueset;
begin
  if (r.ResourceType = fhir2_resources_base.frtNull) then
  begin
    vs := (r as fhir2_resources.TFHIRValueSet);
    FValueSets.Add(vs.url, vs.Link);
    if (vs.codeSystem <> nil) then
      FCodeSystems.Add(vs.codeSystem.system, vs.Link)
  end
  else
    inherited;
end;

function TFHIRPluginValidatorContextR2.supportsSystem(system, version: string): boolean;
var
  ex : fhir2_types.TFhirExtension;
begin
  CheckClient;
  result := FCodeSystems.ContainsKey(system);
  if (not result) then
    for ex in FCapabilityStatement.extensionList do
      if (ex.url = 'http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system') and (ex.value is fhir2_types.TFHIRString) and (fhir2_types.TFHIRString(ex.value).value = system) then
        result := true;
end;

function TFHIRPluginValidatorContextR2.validateCode(system, version, code, display: String): TValidationResult;
var
  pIn, pOut : fhir2_resources.TFhirParameters;
  cs : fhir2_resources.TFHIRValueSet;
  def : fhir2_resources.TFhirValueSetCodeSystemConcept;
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
    pIn := fhir2_resources.TFhirParameters.Create;
    try
      pIn.AddParameter('system', system);
      pIn.AddParameter('code', code);
      pIn.AddParameter('display', display);
      pOut := FServer.operation(fhir2_resources_base.frtValueSet, 'validate-code', pIn) as fhir2_resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR2.validateCode(system, version, code: String; vs: fhir2_resources.TFHIRValueSet): TValidationResult;
var
  pIn, pOut : fhir2_resources.TFhirParameters;
begin
  if validateInternally(system, version, code, vs, result) then
    exit;

  checkClient;
  pIn := fhir2_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('system', system);
    pIn.AddParameter('code', code);
    pIn.AddParameter('version', version);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(fhir2_resources_base.frtValueSet, 'validate-code', pIn) as fhir2_resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR2.validateCode(code: fhir2_types.TFHIRCodeableConcept; vs: fhir2_resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : fhir2_resources.TFhirParameters;
begin
  checkClient;
  pIn := fhir2_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('codeableConcept', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(fhir2_resources_base.frtValueSet, 'validate-code', pIn) as fhir2_resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR2.validateInternally(system, version, code: String; vs: fhir2_resources.TFHIRValueSet; var res: TValidationResult): boolean;
var
  vsw : TFhirValueSetW;
  validator : TValueSetChecker;
  p : TFHIRParametersW;
  params : TFHIRExpansionParams;
begin
  try
    vsw := Factory.wrapValueSet(vs.Link);
    try
      validator := TValueSetChecker.Create(Factory.link, doGetVs, doGetCs, nil, '');
      try
        params := TFHIRExpansionParams.Create;
        try
          validator.prepare(vsw, params);
          p := validator.check(system, version, code, false);
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

function TFHIRPluginValidatorContextR2.validateCode(code: fhir2_types.TFHIRCoding; vs: fhir2_resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : fhir2_resources.TFhirParameters;
begin
  checkClient;
  pIn := fhir2_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('coding', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(fhir2_resources_base.frtValueSet, 'validate-code', pIn) as fhir2_resources.TFhirParameters;
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

{ TFHIRPluginValidatorContextR3 }

procedure TFHIRPluginValidatorContextR3.checkClient;
begin
  if (FServer = nil) or (FCapabilityStatement = nil) then
  begin
    if FServer <> nil then
      FServer.Free;
    FServer := Factory.makeClient(self.link, FUrl, fctWinInet, ffJson, 5000) as fhir3_client.TFhirClient3;
    FCapabilityStatement := FServer.conformance(true);
    if FCapabilityStatement.fhirVersion <> fhir3_constants.FHIR_GENERATED_VERSION then
      raise EFHIRException.create('Terminology Server / Plug-in Version mismatch ('+FCapabilityStatement.fhirVersion+' / '+fhir3_constants.FHIR_GENERATED_VERSION+')');
  end;
end;

constructor TFHIRPluginValidatorContextR3.Create(factory : TFHIRFactory; TerminologyServer : String);
begin
  inherited Create(factory);
  FValueSets := TFslMap<fhir3_resources.TFHIRValueSet>.create('Value Sets');
  FCodeSystems := TFslMap<fhir3_resources.TFHIRCodeSystem>.create('Code Systems');
  FUrl := TerminologyServer;
end;

destructor TFHIRPluginValidatorContextR3.Destroy;
begin
  FValueSets.Free;
  FServer.Free;
  FCapabilityStatement.Free;
  FCodeSystems.Free;
  inherited;
end;

function TFHIRPluginValidatorContextR3.doGetCs(sender: TObject; url, version: String; params: TFHIRExpansionParams; nullOk : boolean): TCodeSystemProvider;
var
  cs : fhir3_resources.TFHIRCodeSystem;
begin
  cs := FCodeSystems[url];
  if cs = nil then
    raise ETerminologyError.create('Unable to resolve code system '+url);
  result := TFhirCodeSystemProvider.create(Factory.link, TFHIRCodeSystemEntry.Create(Factory.wrapCodeSystem(cs.link)));
end;

function TFHIRPluginValidatorContextR3.doGetVs(sender: TObject; url: String): TFHIRValueSetW;
var
  vs : fhir3_resources.TFhirValueSet;
begin
  vs := FValueSets[url];
  if vs = nil then
    raise ETerminologyError.create('Unable to resolve value set '+url);
  result := Factory.wrapValueSet(vs.link);
end;

function TFHIRPluginValidatorContextR3.expand(vs: fhir3_resources.TFhirValueSet; options : TExpansionOperationOptionSet = []): fhir3_resources.TFHIRValueSet;
var
  pIn : fhir3_resources.TFhirParameters;
begin
  CheckClient;
  pIn := fhir3_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('valueSet', vs.Link);
    pIn.AddParameter('_incomplete', true);
    pIn.AddParameter('_limit', '10');
    result := FServer.operation(fhir3_resources_base.frtValueSet, 'expand', pIn) as fhir3_resources.TFhirValueSet;
  finally
    pIn.Free;
  end;

end;

function TFHIRPluginValidatorContextR3.fetchResource(t: fhir3_resources.TFhirResourceType; url: String): fhir3_resources.TFhirResource;
begin
  if (t = fhir3_resources_base.frtValueSet) then
    result := FValueSets[url].link
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRPluginValidatorContextR3.findCode(list: fhir3_resources.TFhirCodeSystemConceptList; code: String; caseSensitive : boolean): fhir3_resources.TFhirCodeSystemConcept;
var
  d, d1 : fhir3_resources.TFhirCodeSystemConcept;
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

function TFHIRPluginValidatorContextR3.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TFHIRPluginValidatorContextR3.Link: TFHIRPluginValidatorContextR3;
begin
  result := TFHIRPluginValidatorContextR3(inherited Link);
end;

procedure TFHIRPluginValidatorContextR3.SeeResource(r: fhir3_resources.TFhirResource);
var
  vs : fhir3_resources.TFhirValueset;
begin
  if (r.ResourceType = fhir3_resources_base.frtValueSet) then
  begin
    vs := (r as fhir3_resources.TFHIRValueSet);
    FValueSets.Add(vs.url, vs.Link);
  end
  else if (r.ResourceType = fhir3_resources_base.frtCodeSystem) then
    FCodeSystems.Add(fhir3_resources.TFHIRCodeSystem(r).url, fhir3_resources.TFHIRCodeSystem(r).Link)
  else
    inherited;
end;

function TFHIRPluginValidatorContextR3.supportsSystem(system, version: string): boolean;
var
  ex : fhir3_types.TFhirExtension;
begin
  CheckClient;
  result := FCodeSystems.ContainsKey(system);
  if (not result) then
    for ex in FCapabilityStatement.extensionList do
      if (ex.url = 'http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system') and (ex.value is fhir3_types.TFHIRString) and (fhir3_types.TFHIRString(ex.value).value = system) then
        result := true;
end;

function TFHIRPluginValidatorContextR3.validateCode(system, version, code, display: String): TValidationResult;
var
  pIn, pOut : fhir3_resources.TFhirParameters;
  cs : fhir3_resources.TFHIRCodeSystem;
  def : fhir3_resources.TFhirCodeSystemConcept;
begin
  if FCodeSystems.ContainsKey(system) then
  begin
    cs := FCodeSystems[system];
    def := FindCode(cs.conceptList, code, cs.caseSensitive);
    if (def = nil) then
      result := TValidationResult.Create(isError, 'Unknown code ' +code)
    else
      result := TValidationResult.Create(def.display);
  end
  else
  begin
    checkClient;
    pIn := fhir3_resources.TFhirParameters.Create;
    try
      pIn.AddParameter('system', system);
      pIn.AddParameter('code', code);
      pIn.AddParameter('display', display);
      pOut := FServer.operation(fhir3_resources_base.frtValueSet, 'validate-code', pIn) as fhir3_resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR3.validateCode(system, version, code: String; vs: fhir3_resources.TFHIRValueSet): TValidationResult;
var
  pIn, pOut : fhir3_resources.TFhirParameters;
  def : TFhirCodeSystemConcept;
begin
  if validateInternally(system, version, code, vs, result) then
    exit;

  checkClient;
  pIn := fhir3_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('system', system);
    pIn.AddParameter('code', code);
    pIn.AddParameter('version', version);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(fhir3_resources_base.frtValueSet, 'validate-code', pIn) as fhir3_resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR3.validateCode(code: fhir3_types.TFHIRCodeableConcept; vs: fhir3_resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : fhir3_resources.TFhirParameters;
begin
  checkClient;
  pIn := fhir3_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('codeableConcept', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(fhir3_resources_base.frtValueSet, 'validate-code', pIn) as fhir3_resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR3.validateInternally(system, version, code: String; vs: fhir3_resources.TFHIRValueSet; var res: TValidationResult): boolean;
var
  vsw : TFhirValueSetW;
  validator : TValueSetChecker;
  p : TFHIRParametersW;
  params : TFHIRExpansionParams;
begin
  try
    vsw := Factory.wrapValueSet(vs.Link);
    try
      validator := TValueSetChecker.Create(Factory.link, doGetVs, doGetCs, nil, '');
      try
        params := TFHIRExpansionParams.Create;
        try
          validator.prepare(vsw, params);
          p := validator.check(system, version, code, false);
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

function TFHIRPluginValidatorContextR3.validateCode(code: fhir3_types.TFHIRCoding; vs: fhir3_resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : fhir3_resources.TFhirParameters;
begin
  checkClient;
  pIn := fhir3_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('coding', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(fhir3_resources_base.frtValueSet, 'validate-code', pIn) as fhir3_resources.TFhirParameters;
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

{ TFHIRPluginValidatorContextR4 }

procedure TFHIRPluginValidatorContextR4.checkClient;
begin
  if (FServer = nil) or (FCapabilityStatement = nil) then
  begin
    if FServer <> nil then
      FServer.Free;
    FServer := Factory.makeClient(self.link, FUrl, fctWinInet, ffJson, 5000) as fhir4_client.TFhirClient4;
    FCapabilityStatement := FServer.conformance(true);
    if FCapabilityStatement.fhirVersion <> FHIR_ENUM_VERSIONS[factory.version] then
      raise EFHIRException.create('Terminology Server / Plug-in Version mismatch ('+CODES_TFhirFHIRVersionEnum[FCapabilityStatement.fhirVersion]+' / '+CODES_TFHIRVersion[factory.version]+')');
  end;
end;

constructor TFHIRPluginValidatorContextR4.Create(factory : TFHIRFactory; TerminologyServer : String);
begin
  inherited Create(factory);
  FValueSets := TFslMap<fhir4_resources.TFHIRValueSet>.create('R4.ValueSets');
  FCodeSystems := TFslMap<fhir4_resources.TFHIRCodeSystem>.create('R4.CodeSystems');
  FUrl := TerminologyServer;
end;

destructor TFHIRPluginValidatorContextR4.Destroy;
begin
  FValueSets.Free;
  FServer.Free;
  FCapabilityStatement.Free;
  FCodeSystems.Free;
  inherited;
end;

function TFHIRPluginValidatorContextR4.doGetCs(sender: TObject; url, version: String; params: TFHIRExpansionParams; nullOk : boolean): TCodeSystemProvider;
var
  cs : fhir4_resources.TFHIRCodeSystem;
begin
  cs := FCodeSystems[url];
  if cs = nil then
    raise ETerminologyError.create('Unable to resolve code system '+url);
  result := TFhirCodeSystemProvider.create(Factory.link, TFHIRCodeSystemEntry.Create(Factory.wrapCodeSystem(cs.link)));
end;

function TFHIRPluginValidatorContextR4.doGetVs(sender: TObject; url: String): TFHIRValueSetW;
var
  vs : fhir4_resources.TFhirValueSet;
begin
  vs := FValueSets[url];
  if vs = nil then
    raise ETerminologyError.create('Unable to resolve value set '+url);
  result := Factory.wrapValueSet(vs.link);
end;

function TFHIRPluginValidatorContextR4.expand(vs: fhir4_resources.TFhirValueSet; options : TExpansionOperationOptionSet = []): fhir4_resources.TFHIRValueSet;
var
  pIn : fhir4_resources.TFhirParameters;
begin
  CheckClient;
  pIn := fhir4_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('valueSet', vs.Link);
    pIn.AddParameter('_incomplete', true);
    pIn.AddParameter('_limit', '10');
    result := FServer.operation(fhir4_resources_base.frtValueSet, 'expand', pIn) as fhir4_resources.TFhirValueSet;
  finally
    pIn.Free;
  end;

end;

function TFHIRPluginValidatorContextR4.fetchResource(t: fhir4_resources.TFhirResourceType; url: String): fhir4_resources.TFhirResource;
begin
  if (t = fhir4_resources_base.frtValueSet) then
    result := FValueSets[url].link
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRPluginValidatorContextR4.findCode(list: fhir4_resources.TFhirCodeSystemConceptList; code: String; caseSensitive : boolean): fhir4_resources.TFhirCodeSystemConcept;
var
  d, d1 : fhir4_resources.TFhirCodeSystemConcept;
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

function TFHIRPluginValidatorContextR4.getSearchParameter(resourceType, name: String): TFHIRResourceV;
begin
  result := nil;
end;

function TFHIRPluginValidatorContextR4.Link: TFHIRPluginValidatorContextR4;
begin
  result := TFHIRPluginValidatorContextR4(inherited Link);
end;

procedure TFHIRPluginValidatorContextR4.SeeResource(r: fhir4_resources.TFhirResource);
var
  vs : fhir4_resources.TFhirValueset;
begin
  if (r.ResourceType = fhir4_resources_base.frtValueSet) then
  begin
    vs := (r as fhir4_resources.TFHIRValueSet);
    FValueSets.Add(vs.url, vs.Link);
  end
  else if (r.ResourceType = fhir4_resources_base.frtCodeSystem) then
    FCodeSystems.Add(fhir4_resources.TFHIRCodeSystem(r).url, fhir4_resources.TFHIRCodeSystem(r).Link)
  else
    inherited;
end;

function TFHIRPluginValidatorContextR4.supportsSystem(system, version: string): boolean;
var
  ex : fhir4_types.TFhirExtension;
begin
  CheckClient;
  result := FCodeSystems.ContainsKey(system);
  if (not result) then
    for ex in FCapabilityStatement.extensionList do
      if (ex.url = 'http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system') and (ex.value is fhir4_types.TFHIRString) and (fhir4_types.TFHIRString(ex.value).value = system) then
        result := true;
end;

function TFHIRPluginValidatorContextR4.validateCode(system, version, code, display: String): TValidationResult;
var
  pIn, pOut : fhir4_resources.TFhirParameters;
  cs : fhir4_resources.TFHIRCodeSystem;
  def : fhir4_resources.TFhirCodeSystemConcept;
begin
  if FCodeSystems.ContainsKey(system) then
  begin
    cs := FCodeSystems[system];
    def := FindCode(cs.conceptList, code, cs.caseSensitive);
    if (def = nil) then
      result := TValidationResult.Create(isError, 'Unknown code ' +code)
    else
      result := TValidationResult.Create(def.display);
  end
  else
  begin
    checkClient;
    pIn := fhir4_resources.TFhirParameters.Create;
    try
      pIn.AddParameter('system', system);
      pIn.AddParameter('code', code);
      pIn.AddParameter('display', display);
      pOut := FServer.operation(fhir4_resources_base.frtValueSet, 'validate-code', pIn) as fhir4_resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR4.validateCode(system, version, code: String; vs: fhir4_resources.TFHIRValueSet): TValidationResult;
var
  pIn, pOut : fhir4_resources.TFhirParameters;
  def : TFhirCodeSystemConcept;
begin
  if validateInternally(system, version, code, vs, result) then
    exit;

  checkClient;
  pIn := fhir4_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('system', system);
    pIn.AddParameter('code', code);
    pIn.AddParameter('version', version);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(fhir4_resources_base.frtValueSet, 'validate-code', pIn) as fhir4_resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR4.validateCode(code: fhir4_types.TFHIRCodeableConcept; vs: fhir4_resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : fhir4_resources.TFhirParameters;
begin
  checkClient;
  pIn := fhir4_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('codeableConcept', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(fhir4_resources_base.frtValueSet, 'validate-code', pIn) as fhir4_resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR4.validateInternally(system, version, code: String; vs: fhir4_resources.TFHIRValueSet; var res: TValidationResult): boolean;
var
  vsw : TFhirValueSetW;
  validator : TValueSetChecker;
  p : TFHIRParametersW;
  params : TFHIRExpansionParams;
begin
  try
    vsw := Factory.wrapValueSet(vs.Link);
    try
      validator := TValueSetChecker.Create(Factory.link, doGetVs, doGetCs, nil, '');
      try
        params := TFHIRExpansionParams.Create;
        try
          validator.prepare(vsw, params);
          p := validator.check(system, version, code, false);
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

function TFHIRPluginValidatorContextR4.validateCode(code: fhir4_types.TFHIRCoding; vs: fhir4_resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : fhir4_resources.TFhirParameters;
begin
  checkClient;
  pIn := fhir4_resources.TFhirParameters.Create;
  try
    pIn.AddParameter('coding', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(fhir4_resources_base.frtValueSet, 'validate-code', pIn) as fhir4_resources.TFhirParameters;
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

{ TFHIRPluginValidatorContext }

class function TFHIRPluginValidatorContext.create(factory: TFHIRFactory; TerminologyServer: String): TFHIRWorkerContextWithFactory;
begin
  case factory.version of
    fhirVersionRelease2 : result := TFHIRPluginValidatorContextR2.Create(factory, TerminologyServer);
    fhirVersionRelease3 : result := TFHIRPluginValidatorContextR3.Create(factory, TerminologyServer);
    fhirVersionRelease4 : result := TFHIRPluginValidatorContextR4.Create(factory, TerminologyServer);
  else
    raise EFHIRException.create('Unexpected version');
  end;
end;

end.
