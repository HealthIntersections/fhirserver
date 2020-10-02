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
  FHIR.Support.Utilities, FHIR.Support.Base,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Client.Base, FHIR.Base.Common, FHIR.Base.Lang,
  FHIR.Tx.Service,
  FHIR.R2.Types, FHIR.R2.Resources, FHIR.R2.Resources.Base, FHIR.R2.Context, FHIR.R2.Profiles, FHIR.R2.Client,
  FHIR.R3.Types, FHIR.R3.Resources, FHIR.R3.Resources.Base, FHIR.R3.Context, FHIR.R3.Profiles, FHIR.R3.Client,
  FHIR.R4.Types, FHIR.R4.Resources, FHIR.R4.Resources.Base, FHIR.R4.Context, FHIR.R4.Profiles, FHIR.R4.Client,
  FHIR.Tools.ValueSets;

Type
  TFHIRPluginValidatorContextR2 = class (TBaseWorkerContextR2)
  private
    FUrl : String;
    FServer : FHIR.R2.Client.TFhirClient2;
    FCapabilityStatement : FHIR.R2.Resources.TFhirConformance;
    FValueSets : TFslMap<FHIR.R2.Resources.TFHIRValueSet>;
    FCodeSystems : TFslMap<FHIR.R2.Resources.TFHIRValueSet>;
    procedure checkClient;
    function  findCode(list : FHIR.R2.Resources.TFhirValueSetCodeSystemConceptList; code : String; caseSensitive : boolean) : FHIR.R2.Resources.TFhirValueSetCodeSystemConcept;
    function validateInternally(system, version, code: String; vs: FHIR.R2.Resources.TFHIRValueSet; var res : TValidationResult) : boolean;
    function doGetVs(sender : TObject; url : String) : TFHIRValueSetW;
    function doGetCs(sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
  protected
    procedure SeeResource(r : FHIR.R2.Resources.TFhirResource); override;
  public
    constructor Create(factory : TFHIRFactory; TerminologyServer : String); virtual;
    destructor Destroy; Override;

    Function Link : TFHIRPluginValidatorContextR2; overload;

    function fetchResource(t : FHIR.R2.Resources.TFhirResourceType; url : String) : FHIR.R2.Resources.TFhirResource; override;

    function expand(vs : FHIR.R2.Resources.TFhirValueSet; options : TExpansionOperationOptionSet = []) : FHIR.R2.Resources.TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : FHIR.R2.Resources.TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : FHIR.R2.Types.TFHIRCoding; vs : FHIR.R2.Resources.TFhirValueSet) : TValidationResult; override;
    function validateCode(code : FHIR.R2.Types.TFHIRCodeableConcept; vs : FHIR.R2.Resources.TFhirValueSet) : TValidationResult; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
  end;

  TFHIRPluginValidatorContextR3 = class (TBaseWorkerContextR3)
  private
    FUrl : String;
    FServer : FHIR.R3.Client.TFhirClient3;
    FCapabilityStatement : FHIR.R3.Resources.TFHIRCapabilityStatement;
    FValueSets : TFslMap<FHIR.R3.Resources.TFHIRValueSet>;
    FCodeSystems : TFslMap<FHIR.R3.Resources.TFHIRCodeSystem>;
    procedure checkClient;
    function  findCode(list : FHIR.R3.Resources.TFhirCodeSystemConceptList; code : String; caseSensitive : boolean) : FHIR.R3.Resources.TFhirCodeSystemConcept;
    function validateInternally(system, version, code: String; vs: FHIR.R3.Resources.TFHIRValueSet; var res : TValidationResult) : boolean;
    function doGetVs(sender : TObject; url : String) : TFHIRValueSetW;
    function doGetCs(sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
  protected
    procedure SeeResource(r : FHIR.R3.Resources.TFhirResource); override;
  public
    constructor Create(factory : TFHIRFactory; TerminologyServer : String); virtual;
    destructor Destroy; Override;

    Function Link : TFHIRPluginValidatorContextR3; overload;

    function fetchResource(t : FHIR.R3.Resources.TFhirResourceType; url : String) : FHIR.R3.Resources.TFhirResource; override;

    function expand(vs : FHIR.R3.Resources.TFhirValueSet; options : TExpansionOperationOptionSet = []) : FHIR.R3.Resources.TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : FHIR.R3.Resources.TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : FHIR.R3.Types.TFHIRCoding; vs : FHIR.R3.Resources.TFhirValueSet) : TValidationResult; override;
    function validateCode(code : FHIR.R3.Types.TFHIRCodeableConcept; vs : FHIR.R3.Resources.TFhirValueSet) : TValidationResult; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
  end;

  TFHIRPluginValidatorContextR4 = class (TBaseWorkerContextR4)
  private
    FUrl : String;
    FServer : FHIR.R4.Client.TFhirClient4;
    FCapabilityStatement : FHIR.R4.Resources.TFHIRCapabilityStatement;
    FValueSets : TFslMap<FHIR.R4.Resources.TFHIRValueSet>;
    FCodeSystems : TFslMap<FHIR.R4.Resources.TFHIRCodeSystem>;
    procedure checkClient;
    function findCode(list : FHIR.R4.Resources.TFhirCodeSystemConceptList; code : String; caseSensitive : boolean) : FHIR.R4.Resources.TFhirCodeSystemConcept;
    function validateInternally(system, version, code: String; vs: FHIR.R4.Resources.TFHIRValueSet; var res : TValidationResult) : boolean;
    function doGetVs(sender : TObject; url : String) : TFHIRValueSetW;
    function doGetCs(sender : TObject; url, version : String; params : TFHIRExpansionParams; nullOk : boolean) : TCodeSystemProvider;
  protected
    procedure SeeResource(r : FHIR.R4.Resources.TFhirResource); override;
  public
    constructor Create(factory : TFHIRFactory; TerminologyServer : String); virtual;
    destructor Destroy; Override;

    Function Link : TFHIRPluginValidatorContextR4; overload;

    function fetchResource(t : FHIR.R4.Resources.TFhirResourceType; url : String) : FHIR.R4.Resources.TFhirResource; override;

    function expand(vs : FHIR.R4.Resources.TFhirValueSet; options : TExpansionOperationOptionSet = []) : FHIR.R4.Resources.TFHIRValueSet; override;
    function supportsSystem(system, version : string) : boolean; override;
    function validateCode(system, version, code, display : String) : TValidationResult; override;
    function validateCode(system, version, code : String; vs : FHIR.R4.Resources.TFHIRValueSet) : TValidationResult; override;
    function validateCode(code : FHIR.R4.Types.TFHIRCoding; vs : FHIR.R4.Resources.TFhirValueSet) : TValidationResult; override;
    function validateCode(code : FHIR.R4.Types.TFHIRCodeableConcept; vs : FHIR.R4.Resources.TFhirValueSet) : TValidationResult; override;
    function getSearchParameter(resourceType, name : String) : TFHIRResourceV; override;
  end;

  TFHIRPluginValidatorContext = class
  public
    class function create(factory : TFHIRFactory; TerminologyServer : String) : TFHIRWorkerContextWithFactory;
  end;

implementation

uses
  FHIR.Tools.CodeSystemProvider,
  FHIR.R2.Constants, FHIR.R2.Utilities,
  FHIR.R3.Constants, FHIR.R3.Utilities,
  FHIR.R4.Constants, FHIR.R4.Utilities;

{ TFHIRPluginValidatorContextR2 }

procedure TFHIRPluginValidatorContextR2.checkClient;
begin
  if (FServer = nil) or (FCapabilityStatement = nil) then
  begin
    if FServer <> nil then
      FServer.Free;
    FServer := Factory.makeClient(self.link, FUrl, fctWinInet, ffJson, 5000) as FHIR.R2.Client.TFhirClient2;
    FCapabilityStatement := FServer.conformance(true);
    if FCapabilityStatement.fhirVersion <> FHIR.R2.Constants.FHIR_GENERATED_VERSION then
      raise EFHIRException.create('Terminology Server / Plug-in Version mismatch ('+FCapabilityStatement.fhirVersion+' / '+FHIR.R2.Constants.FHIR_GENERATED_VERSION+')');
  end;
end;

constructor TFHIRPluginValidatorContextR2.Create(factory : TFHIRFactory; TerminologyServer : String);
begin
  inherited Create(factory);
  FValueSets := TFslMap<FHIR.R2.Resources.TFHIRValueSet>.create('ValueSets');
  FCodeSystems := TFslMap<FHIR.R2.Resources.TFHIRValueSet>.create('CodeSystems');
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
  cs : FHIR.R2.Resources.TFHIRValueSet;
begin
  cs := FCodeSystems[url];
  if cs = nil then
    raise ETerminologyError.create('Unable to resolve code system '+url);
  result := TFhirCodeSystemProvider.create(Factory.link, TFHIRCodeSystemEntry.Create(Factory.wrapCodeSystem(cs.link)));
end;

function TFHIRPluginValidatorContextR2.doGetVs(sender: TObject; url: String): TFHIRValueSetW;
var
  vs : FHIR.R2.Resources.TFhirValueSet;
begin
  vs := FValueSets[url];
  if vs = nil then
    raise ETerminologyError.create('Unable to resolve value set '+url);
  result := Factory.wrapValueSet(vs.link);
end;

function TFHIRPluginValidatorContextR2.expand(vs: FHIR.R2.Resources.TFhirValueSet; options : TExpansionOperationOptionSet = []): FHIR.R2.Resources.TFHIRValueSet;
var
  pIn : FHIR.R2.Resources.TFhirParameters;
begin
  CheckClient;
  pIn := FHIR.R2.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('valueSet', vs.Link);
    pIn.AddParameter('_incomplete', true);
    pIn.AddParameter('_limit', '10');
    result := FServer.operation(FHIR.R2.Resources.Base.frtValueSet, 'expand', pIn) as FHIR.R2.Resources.TFhirValueSet;
  finally
    pIn.Free;
  end;

end;

function TFHIRPluginValidatorContextR2.fetchResource(t: FHIR.R2.Resources.TFhirResourceType; url: String): FHIR.R2.Resources.TFhirResource;
begin
  if (t = FHIR.R2.Resources.Base.frtValueSet) then
    result := FValueSets[url].link
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRPluginValidatorContextR2.findCode(list: FHIR.R2.Resources.TFhirValueSetCodeSystemConceptList; code: String; caseSensitive : boolean): FHIR.R2.Resources.TFhirValueSetCodeSystemConcept;
var
  d, d1 : FHIR.R2.Resources.TFhirValueSetCodeSystemConcept;
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

procedure TFHIRPluginValidatorContextR2.SeeResource(r: FHIR.R2.Resources.TFhirResource);
var
  vs : FHIR.R2.Resources.TFhirValueset;
begin
  if (r.ResourceType = FHIR.R2.Resources.Base.frtNull) then
  begin
    vs := (r as FHIR.R2.Resources.TFHIRValueSet);
    FValueSets.Add(vs.url, vs.Link);
    if (vs.codeSystem <> nil) then
      FCodeSystems.Add(vs.codeSystem.system, vs.Link)
  end
  else
    inherited;
end;

function TFHIRPluginValidatorContextR2.supportsSystem(system, version: string): boolean;
var
  ex : FHIR.R2.Types.TFhirExtension;
begin
  CheckClient;
  result := FCodeSystems.ContainsKey(system);
  if (not result) then
    for ex in FCapabilityStatement.extensionList do
      if (ex.url = 'http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system') and (ex.value is FHIR.R2.Types.TFHIRString) and (FHIR.R2.Types.TFHIRString(ex.value).value = system) then
        result := true;
end;

function TFHIRPluginValidatorContextR2.validateCode(system, version, code, display: String): TValidationResult;
var
  pIn, pOut : FHIR.R2.Resources.TFhirParameters;
  cs : FHIR.R2.Resources.TFHIRValueSet;
  def : FHIR.R2.Resources.TFhirValueSetCodeSystemConcept;
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
    pIn := FHIR.R2.Resources.TFhirParameters.Create;
    try
      pIn.AddParameter('system', system);
      pIn.AddParameter('code', code);
      pIn.AddParameter('display', display);
      pOut := FServer.operation(FHIR.R2.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R2.Resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR2.validateCode(system, version, code: String; vs: FHIR.R2.Resources.TFHIRValueSet): TValidationResult;
var
  pIn, pOut : FHIR.R2.Resources.TFhirParameters;
begin
  if validateInternally(system, version, code, vs, result) then
    exit;

  checkClient;
  pIn := FHIR.R2.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('system', system);
    pIn.AddParameter('code', code);
    pIn.AddParameter('version', version);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(FHIR.R2.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R2.Resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR2.validateCode(code: FHIR.R2.Types.TFHIRCodeableConcept; vs: FHIR.R2.Resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : FHIR.R2.Resources.TFhirParameters;
begin
  checkClient;
  pIn := FHIR.R2.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('codeableConcept', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(FHIR.R2.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R2.Resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR2.validateInternally(system, version, code: String; vs: FHIR.R2.Resources.TFHIRValueSet; var res: TValidationResult): boolean;
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

function TFHIRPluginValidatorContextR2.validateCode(code: FHIR.R2.Types.TFHIRCoding; vs: FHIR.R2.Resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : FHIR.R2.Resources.TFhirParameters;
begin
  checkClient;
  pIn := FHIR.R2.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('coding', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(FHIR.R2.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R2.Resources.TFhirParameters;
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
    FServer := Factory.makeClient(self.link, FUrl, fctWinInet, ffJson, 5000) as FHIR.R3.Client.TFhirClient3;
    FCapabilityStatement := FServer.conformance(true);
    if FCapabilityStatement.fhirVersion <> FHIR.R3.Constants.FHIR_GENERATED_VERSION then
      raise EFHIRException.create('Terminology Server / Plug-in Version mismatch ('+FCapabilityStatement.fhirVersion+' / '+FHIR.R3.Constants.FHIR_GENERATED_VERSION+')');
  end;
end;

constructor TFHIRPluginValidatorContextR3.Create(factory : TFHIRFactory; TerminologyServer : String);
begin
  inherited Create(factory);
  FValueSets := TFslMap<FHIR.R3.Resources.TFHIRValueSet>.create('Value Sets');
  FCodeSystems := TFslMap<FHIR.R3.Resources.TFHIRCodeSystem>.create('Code Systems');
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
  cs : FHIR.R3.Resources.TFHIRCodeSystem;
begin
  cs := FCodeSystems[url];
  if cs = nil then
    raise ETerminologyError.create('Unable to resolve code system '+url);
  result := TFhirCodeSystemProvider.create(Factory.link, TFHIRCodeSystemEntry.Create(Factory.wrapCodeSystem(cs.link)));
end;

function TFHIRPluginValidatorContextR3.doGetVs(sender: TObject; url: String): TFHIRValueSetW;
var
  vs : FHIR.R3.Resources.TFhirValueSet;
begin
  vs := FValueSets[url];
  if vs = nil then
    raise ETerminologyError.create('Unable to resolve value set '+url);
  result := Factory.wrapValueSet(vs.link);
end;

function TFHIRPluginValidatorContextR3.expand(vs: FHIR.R3.Resources.TFhirValueSet; options : TExpansionOperationOptionSet = []): FHIR.R3.Resources.TFHIRValueSet;
var
  pIn : FHIR.R3.Resources.TFhirParameters;
begin
  CheckClient;
  pIn := FHIR.R3.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('valueSet', vs.Link);
    pIn.AddParameter('_incomplete', true);
    pIn.AddParameter('_limit', '10');
    result := FServer.operation(FHIR.R3.Resources.Base.frtValueSet, 'expand', pIn) as FHIR.R3.Resources.TFhirValueSet;
  finally
    pIn.Free;
  end;

end;

function TFHIRPluginValidatorContextR3.fetchResource(t: FHIR.R3.Resources.TFhirResourceType; url: String): FHIR.R3.Resources.TFhirResource;
begin
  if (t = FHIR.R3.Resources.Base.frtValueSet) then
    result := FValueSets[url].link
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRPluginValidatorContextR3.findCode(list: FHIR.R3.Resources.TFhirCodeSystemConceptList; code: String; caseSensitive : boolean): FHIR.R3.Resources.TFhirCodeSystemConcept;
var
  d, d1 : FHIR.R3.Resources.TFhirCodeSystemConcept;
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

procedure TFHIRPluginValidatorContextR3.SeeResource(r: FHIR.R3.Resources.TFhirResource);
var
  vs : FHIR.R3.Resources.TFhirValueset;
begin
  if (r.ResourceType = FHIR.R3.Resources.Base.frtValueSet) then
  begin
    vs := (r as FHIR.R3.Resources.TFHIRValueSet);
    FValueSets.Add(vs.url, vs.Link);
  end
  else if (r.ResourceType = FHIR.R3.Resources.Base.frtCodeSystem) then
    FCodeSystems.Add(FHIR.R3.Resources.TFHIRCodeSystem(r).url, FHIR.R3.Resources.TFHIRCodeSystem(r).Link)
  else
    inherited;
end;

function TFHIRPluginValidatorContextR3.supportsSystem(system, version: string): boolean;
var
  ex : FHIR.R3.Types.TFhirExtension;
begin
  CheckClient;
  result := FCodeSystems.ContainsKey(system);
  if (not result) then
    for ex in FCapabilityStatement.extensionList do
      if (ex.url = 'http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system') and (ex.value is FHIR.R3.Types.TFHIRString) and (FHIR.R3.Types.TFHIRString(ex.value).value = system) then
        result := true;
end;

function TFHIRPluginValidatorContextR3.validateCode(system, version, code, display: String): TValidationResult;
var
  pIn, pOut : FHIR.R3.Resources.TFhirParameters;
  cs : FHIR.R3.Resources.TFHIRCodeSystem;
  def : FHIR.R3.Resources.TFhirCodeSystemConcept;
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
    pIn := FHIR.R3.Resources.TFhirParameters.Create;
    try
      pIn.AddParameter('system', system);
      pIn.AddParameter('code', code);
      pIn.AddParameter('display', display);
      pOut := FServer.operation(FHIR.R3.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R3.Resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR3.validateCode(system, version, code: String; vs: FHIR.R3.Resources.TFHIRValueSet): TValidationResult;
var
  pIn, pOut : FHIR.R3.Resources.TFhirParameters;
  def : TFhirCodeSystemConcept;
begin
  if validateInternally(system, version, code, vs, result) then
    exit;

  checkClient;
  pIn := FHIR.R3.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('system', system);
    pIn.AddParameter('code', code);
    pIn.AddParameter('version', version);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(FHIR.R3.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R3.Resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR3.validateCode(code: FHIR.R3.Types.TFHIRCodeableConcept; vs: FHIR.R3.Resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : FHIR.R3.Resources.TFhirParameters;
begin
  checkClient;
  pIn := FHIR.R3.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('codeableConcept', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(FHIR.R3.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R3.Resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR3.validateInternally(system, version, code: String; vs: FHIR.R3.Resources.TFHIRValueSet; var res: TValidationResult): boolean;
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

function TFHIRPluginValidatorContextR3.validateCode(code: FHIR.R3.Types.TFHIRCoding; vs: FHIR.R3.Resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : FHIR.R3.Resources.TFhirParameters;
begin
  checkClient;
  pIn := FHIR.R3.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('coding', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(FHIR.R3.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R3.Resources.TFhirParameters;
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
    FServer := Factory.makeClient(self.link, FUrl, fctWinInet, ffJson, 5000) as FHIR.R4.Client.TFhirClient4;
    FCapabilityStatement := FServer.conformance(true);
    if FCapabilityStatement.fhirVersion <> FHIR_ENUM_VERSIONS[factory.version] then
      raise EFHIRException.create('Terminology Server / Plug-in Version mismatch ('+CODES_TFhirFHIRVersionEnum[FCapabilityStatement.fhirVersion]+' / '+CODES_TFHIRVersion[factory.version]+')');
  end;
end;

constructor TFHIRPluginValidatorContextR4.Create(factory : TFHIRFactory; TerminologyServer : String);
begin
  inherited Create(factory);
  FValueSets := TFslMap<FHIR.R4.Resources.TFHIRValueSet>.create('R4.ValueSets');
  FCodeSystems := TFslMap<FHIR.R4.Resources.TFHIRCodeSystem>.create('R4.CodeSystems');
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
  cs : FHIR.R4.Resources.TFHIRCodeSystem;
begin
  cs := FCodeSystems[url];
  if cs = nil then
    raise ETerminologyError.create('Unable to resolve code system '+url);
  result := TFhirCodeSystemProvider.create(Factory.link, TFHIRCodeSystemEntry.Create(Factory.wrapCodeSystem(cs.link)));
end;

function TFHIRPluginValidatorContextR4.doGetVs(sender: TObject; url: String): TFHIRValueSetW;
var
  vs : FHIR.R4.Resources.TFhirValueSet;
begin
  vs := FValueSets[url];
  if vs = nil then
    raise ETerminologyError.create('Unable to resolve value set '+url);
  result := Factory.wrapValueSet(vs.link);
end;

function TFHIRPluginValidatorContextR4.expand(vs: FHIR.R4.Resources.TFhirValueSet; options : TExpansionOperationOptionSet = []): FHIR.R4.Resources.TFHIRValueSet;
var
  pIn : FHIR.R4.Resources.TFhirParameters;
begin
  CheckClient;
  pIn := FHIR.R4.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('valueSet', vs.Link);
    pIn.AddParameter('_incomplete', true);
    pIn.AddParameter('_limit', '10');
    result := FServer.operation(FHIR.R4.Resources.Base.frtValueSet, 'expand', pIn) as FHIR.R4.Resources.TFhirValueSet;
  finally
    pIn.Free;
  end;

end;

function TFHIRPluginValidatorContextR4.fetchResource(t: FHIR.R4.Resources.TFhirResourceType; url: String): FHIR.R4.Resources.TFhirResource;
begin
  if (t = FHIR.R4.Resources.Base.frtValueSet) then
    result := FValueSets[url].link
  else
    result := inherited fetchResource(t, url);
end;

function TFHIRPluginValidatorContextR4.findCode(list: FHIR.R4.Resources.TFhirCodeSystemConceptList; code: String; caseSensitive : boolean): FHIR.R4.Resources.TFhirCodeSystemConcept;
var
  d, d1 : FHIR.R4.Resources.TFhirCodeSystemConcept;
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

procedure TFHIRPluginValidatorContextR4.SeeResource(r: FHIR.R4.Resources.TFhirResource);
var
  vs : FHIR.R4.Resources.TFhirValueset;
begin
  if (r.ResourceType = FHIR.R4.Resources.Base.frtValueSet) then
  begin
    vs := (r as FHIR.R4.Resources.TFHIRValueSet);
    FValueSets.Add(vs.url, vs.Link);
  end
  else if (r.ResourceType = FHIR.R4.Resources.Base.frtCodeSystem) then
    FCodeSystems.Add(FHIR.R4.Resources.TFHIRCodeSystem(r).url, FHIR.R4.Resources.TFHIRCodeSystem(r).Link)
  else
    inherited;
end;

function TFHIRPluginValidatorContextR4.supportsSystem(system, version: string): boolean;
var
  ex : FHIR.R4.Types.TFhirExtension;
begin
  CheckClient;
  result := FCodeSystems.ContainsKey(system);
  if (not result) then
    for ex in FCapabilityStatement.extensionList do
      if (ex.url = 'http://hl7.org/fhir/StructureDefinition/conformance-common-supported-system') and (ex.value is FHIR.R4.Types.TFHIRString) and (FHIR.R4.Types.TFHIRString(ex.value).value = system) then
        result := true;
end;

function TFHIRPluginValidatorContextR4.validateCode(system, version, code, display: String): TValidationResult;
var
  pIn, pOut : FHIR.R4.Resources.TFhirParameters;
  cs : FHIR.R4.Resources.TFHIRCodeSystem;
  def : FHIR.R4.Resources.TFhirCodeSystemConcept;
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
    pIn := FHIR.R4.Resources.TFhirParameters.Create;
    try
      pIn.AddParameter('system', system);
      pIn.AddParameter('code', code);
      pIn.AddParameter('display', display);
      pOut := FServer.operation(FHIR.R4.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R4.Resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR4.validateCode(system, version, code: String; vs: FHIR.R4.Resources.TFHIRValueSet): TValidationResult;
var
  pIn, pOut : FHIR.R4.Resources.TFhirParameters;
  def : TFhirCodeSystemConcept;
begin
  if validateInternally(system, version, code, vs, result) then
    exit;

  checkClient;
  pIn := FHIR.R4.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('system', system);
    pIn.AddParameter('code', code);
    pIn.AddParameter('version', version);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(FHIR.R4.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R4.Resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR4.validateCode(code: FHIR.R4.Types.TFHIRCodeableConcept; vs: FHIR.R4.Resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : FHIR.R4.Resources.TFhirParameters;
begin
  checkClient;
  pIn := FHIR.R4.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('codeableConcept', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(FHIR.R4.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R4.Resources.TFhirParameters;
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

function TFHIRPluginValidatorContextR4.validateInternally(system, version, code: String; vs: FHIR.R4.Resources.TFHIRValueSet; var res: TValidationResult): boolean;
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

function TFHIRPluginValidatorContextR4.validateCode(code: FHIR.R4.Types.TFHIRCoding; vs: FHIR.R4.Resources.TFhirValueSet): TValidationResult;
var
  pIn, pOut : FHIR.R4.Resources.TFhirParameters;
begin
  checkClient;
  pIn := FHIR.R4.Resources.TFhirParameters.Create;
  try
    pIn.AddParameter('coding', code.Link);
    pIn.AddParameter('valueSet', vs.Link);
    pOut := FServer.operation(FHIR.R4.Resources.Base.frtValueSet, 'validate-code', pIn) as FHIR.R4.Resources.TFhirParameters;
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
