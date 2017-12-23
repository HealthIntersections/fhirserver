unit TerminologyOperations;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

uses
  SysUtils,
  StringSupport, GuidSupport, DateSupport,
  AdvExceptions, AdvGenerics,
  KDBManager,
  FHIRBase, FHIRTypes, FHIRResources, FHIRUtilities, FHIRSupport, FHIRLang, FHIROperations,
  FHIRStorageService, TerminologyServices, TerminologyServerStore, TerminologyServer, ClosureManager;

type
  TFhirTerminologyOperation = class (TFhirOperation)
  protected
    FServer : TTerminologyServer;
    function buildExpansionProfile(request: TFHIRRequest; manager: TFHIROperationEngine; params : TFhirParameters) : TFHIRExpansionProfile;
  public
    Constructor Create(server : TTerminologyServer);
    Destructor Destroy; override;
  end;

  TFhirExpandValueSetOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirValueSetValidationOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  {$IFNDEF FHIR2}
  TFhirCodeSystemComposeOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirSubsumesOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;
  {$ENDIF}

  TFhirConceptMapTranslationOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;


  TFhirLookupCodeSystemOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
  public
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;


  TFhirConceptMapClosureOperation = class (TFhirTerminologyOperation)
  private
    FConnection : TKDBConnection;
  protected
    function isWrite : boolean; override;
    function owningResource : TFhirResourceType; override;
    function checkName(request: TFHIRRequest; response : TFHIRResponse; var name : String) : boolean;
  public
    Constructor Create(server : TTerminologyServer; Connection : TKDBConnection);
    function Name : String; override;
    function Types : TFhirResourceTypeSet; override;
    function CreateDefinition(base : String) : TFHIROperationDefinition; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;


implementation

{ TFhirTerminologyOperation }

function TFhirTerminologyOperation.buildExpansionProfile(request: TFHIRRequest; manager: TFHIROperationEngine; params: TFhirParameters): TFHIRExpansionProfile;
var
  needSecure : boolean;
  exp : TFhirExpansionProfile;
  res : boolean;
begin
  res := false;
  {$IFNDEF FHIR2}
  exp := params.res['profile'] as TFHIRExpansionProfile;
  if exp <> nil then
    res := true
  else
    if params.str['profile'].StartsWith('http:') or params.str['profile'].StartsWith('https:') then
      exp := manager.getResourceByUrl(frtExpansionProfile, params.str['profile'], '', true, needSecure) as TFhirExpansionProfile
    else if params.str['profile'] <> '' then
      exp := manager.GetResourceById(request, 'ExpansionProfile', params.str['profile'], request.baseUrl, needSecure) as TFhirExpansionProfile
    else
  {$ENDIF}
     exp := nil;

  try
   if exp = nil then
     result := TFhirExpansionProfile.Create
   else
     result := exp.Clone;
   try
     if (not res) and (params.str['profile'] = 'http://www.healthintersections.com.au/fhir/expansion/no-details') then
       result.includeDefinition := true;
     if (params.str['_incomplete'] <> '') then
       result.limitedExpansion := StrToBoolDef(params.str['_incomplete'], false);
     if (params.str['limitedExpansion'] <> '') then
       result.limitedExpansion := StrToBoolDef(params.str['limitedExpansion'], false);
     if (params.str['displayLanguage'] <> '') then
       result.displayLanguage := params.str['displayLanguage'];
     if (params.str['includeDesignations'] <> '') then
       result.includeDesignations := StrToBoolDef(params.str['includeDesignations'], false);
     if (params.str['includeDefinition'] <> '') then
       result.includeDefinition := StrToBoolDef(params.str['includeDefinition'], false);
     if (params.str['activeOnly'] <> '') then
       result.activeOnly := StrToBoolDef(params.str['activeOnly'], false);
     if (params.str['excludeNested'] <> '') then
       result.excludeNested := StrToBoolDef(params.str['excludeNested'], false);
     if (params.str['excludeNotForUI'] <> '') then
       result.excludeNotForUI := StrToBoolDef(params.str['excludeNotForUI'], false);
     if (params.str['excludePostCoordinated'] <> '') then
       result.excludePostCoordinated := StrToBoolDef(params.str['excludePostCoordinated'], false);
     {$IFNDEF FHIR2}
     if (result.url = '') and not res then
       result.url := params.str['profile'];
     {$ENDIF}

     result.Link;
   finally
     result.free;
   end;
  finally
    if not res then
      exp.free;
  end;
end;

constructor TFhirTerminologyOperation.Create(server: TTerminologyServer);
begin
  inherited Create;
  FServer := server;
end;

destructor TFhirTerminologyOperation.Destroy;
begin
  FServer.Free;
  inherited;
end;

{ TFhirExpandValueSetOperation }

function TFhirExpandValueSetOperation.Name: String;
begin
  result := 'expand';
end;

function TFhirExpandValueSetOperation.owningResource: TFhirResourceType;
begin
  result := frtValueSet;
end;

function TFhirExpandValueSetOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtValueSet];
end;

function TFhirExpandValueSetOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirExpandValueSetOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  vs, dst : TFHIRValueSet;
  resourceKey, versionKey : integer;
  url, cacheId, filter : String;
  profile : TFhirExpansionProfile;
  limit, count, offset : integer;
  params : TFhirParameters;
  needSecure : boolean;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        cacheId := '';
        params := makeParams(request);
        vs := nil;
        try
          // first, we have to identify the value set.
          if request.Id <> '' then // and it must exist, because of the check above
          begin
            vs := manager.GetResourceById(request, 'ValueSet', request.Id, request.baseUrl, needSecure) as TFHIRValueSet;
            cacheId := vs.url;
            if vs.version <> '' then
              cacheId := cacheId + vs.version;
          end
          else if params.hasParameter('url') then
          begin
            url := params.str['url'];
            if (url.startsWith('ValueSet/')) then
              vs := manager.GetResourceById(request, 'ValueSet', url.substring(9), request.baseUrl, needSecure) as TFHIRValueSet
            else if (url.startsWith(request.baseURL+'ValueSet/')) then
              vs := manager.GetResourceById(request, 'ValueSet', url.substring(9), request.baseUrl, needSecure) as TFHIRValueSet
            else if not FServer.isKnownValueSet(url, vs) then
              vs := manager.GetResourceByUrl(frtValueSet, request.Parameters.getvar('url'), request.Parameters.getvar('version'), false, needSecure) as TFHIRValueSet;
            cacheId := vs.url;
            if vs.version <> '' then
              cacheId := cacheId + vs.version;
          end
          else if params.hasParameter('valueSet') then
            vs := params['valueSet'].Link as TFhirValueSet
          else if (request.Resource <> nil) and (request.Resource is TFHIRValueSet) then
            vs := request.Resource.Link as TFhirValueSet
          else if params.hasParameter('context') then
            raise Exception.Create('the "context" parameter is not yet supported')
          else
            raise Exception.Create('Unable to find value set to expand (not provided by id, identifier, or directly)');

          vs.checkNoImplicitRules('ExpandValueSet', 'ValueSet');
          vs.checkNoModifiers('ExpandValueSet', 'ValueSet');

          profile := buildExpansionProfile(request, manager, params);
          try
            filter := params.str['filter'];
            count := StrToIntDef(params.str['count'], 0);
            offset := StrToIntDef(params.str['offset'], 0);
            limit := StrToIntDef(params.str['_limit'], 0);
            if profile.displayLanguage = '' then
              profile.displayLanguage := request.Lang;

            dst := FServer.expandVS(vs, cacheId, profile, filter, limit, count, offset);
            try
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := dst.Link;
              // response.categories.... no tags to go on this resource
            finally
              dst.free;
            end;
          finally
            profile.Free;
          end;
        finally
          vs.free;
          params.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirExpandValueSetOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/ValueSet-expand';
end;

function TFhirExpandValueSetOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirValueSetValidationOperation }

function TFhirValueSetValidationOperation.Name: String;
begin
  result := 'validate-code';
end;

function TFhirValueSetValidationOperation.owningResource: TFhirResourceType;
begin
  result := frtValueSet;
end;

function TFhirValueSetValidationOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtValueSet];
end;

function TFhirValueSetValidationOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirValueSetValidationOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirValueSetValidationOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  vs : TFHIRValueSet;
  resourceKey, versionKey : integer;
  cacheId  : String;
  coded : TFhirCodeableConcept;
  coding : TFhirCoding;
  abstractOk : boolean;
  params, pout : TFhirParameters;
  needSecure : boolean;
  profile : TFhirExpansionProfile;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        cacheId := '';
        params := makeParams(request);
        try
          vs := nil;
          try
            // first, we have to identify the value set.
            if request.Id <> '' then // and it must exist, because of the check above
            begin
              vs := manager.GetResourceById(request, 'ValueSet', request.Id, request.baseUrl, needSecure) as TFHIRValueSet;
              cacheId := vs.url;
            end
            else if params.hasParameter('url') then
            begin
              if not FServer.isKnownValueSet(params.str['url'], vs) then
                vs := manager.GetResourceByUrl(frtValueSet, params.str['url'], params.str['version'], false, needSecure) as TFHIRValueSet;
              if vs = nil then
                raise ETerminologySetup.Create('Error Message');
              cacheId := vs.url;
            end
            else if params.hasParameter('valueSet') then
              vs := (params.res['valueSet'] as TFhirValueSet).Link
            else if (request.Resource <> nil) and (request.Resource is TFHIRValueSet) then
              vs := request.Resource.Link as TFhirValueSet
            else
              vs := nil;
              // raise Exception.Create('Unable to find valueset to validate against (not provided by id, identifier, or directly)');

            coded := nil;
            try
              // ok, now we need to find the source code to validate
              if (request.form <> nil) and request.form.hasParam('coding') then
              begin
                coded := TFhirCodeableConcept.Create;
                coded.codingList.add(LoadDTFromFormParam(request.Context, request.form.getParam('coding'), request.lang, 'coding', TFhirCoding) as TFhirCoding)
              end
              else if (request.form <> nil) and request.form.hasParam('codeableConcept') then
                coded := LoadDTFromFormParam(request.Context, request.form.getParam('codeableConcept'), request.lang, 'codeableConcept', TFhirCodeableConcept) as TFhirCodeableConcept
              else if request.Parameters.VarExists('code') and request.Parameters.VarExists('system') then
              begin
                coded := TFhirCodeableConcept.Create;
                coding := coded.codingList.Append;
                coding.system := request.Parameters.GetVar('system');
                coding.version := request.Parameters.GetVar('version');
                coding.code := request.Parameters.GetVar('code');
                coding.display := request.Parameters.GetVar('display');
              end

              else if ((request.resource <> nil) and (request.Resource.ResourceType = frtParameters)) then
              begin
                params := request.Resource as TFhirParameters;
                if params.hasParameter('coding') then
                begin
                  coded := TFhirCodeableConcept.Create;
                  coded.codingList.Add(params['coding'].Link);
                end
                else if params.hasParameter('codeableConcept') then
                  coded := params['codeableConcept'].Link as TFhirCodeableConcept
                else if params.hasParameter('code') and params.hasParameter('system') then
                begin
                  coded := TFhirCodeableConcept.Create;
                  coding := coded.codingList.Append;
                  coding.system := TFHIRPrimitiveType(params['system']).StringValue;
                  if params.hasParameter('version') then
                    coding.version := TFHIRPrimitiveType(params['version']).StringValue;
                  coding.code := TFHIRPrimitiveType(params['code']).StringValue;
                  if params.hasParameter('display') then
                    coding.display := TFHIRPrimitiveType(params['display']).StringValue;
                end
                else
                  raise Exception.Create('Unable to find code to validate (params. coding | codeableConcept | code');
              end
              else
                raise Exception.Create('Unable to find code to validate (coding | codeableConcept | code');
              abstractOk := params.hasParameter('abstract') and TFHIRBoolean(params['abstract']).Value;

              if (coded = nil) then
                raise Exception.Create('Unable to find code to validate (coding | codeableConcept | code');

              if vs <> nil then
              begin
                vs.checkNoImplicitRules('ValueSetValidation', 'ValueSet');
                vs.checkNoModifiers('ValueSetValidation', 'ValueSet');
              end;

              profile := buildExpansionProfile(request, manager, params);
              try
                if profile.displayLanguage = '' then
                  profile.displayLanguage := request.Lang;
                try
                  response.resource := FServer.validate(vs, coded, profile, abstractOk);
                except
                  on e : Exception do
                  begin
                    pout := TFHIRParameters.create;
                    response.resource := pout;
                    pout.AddParameter('result', false);
                    pout.AddParameter('message', e.Message);
                    pout.AddParameter('cause', 'unknown');
                  end;
                end;
                response.HTTPCode := 200;
                response.Message := 'OK';
                response.Body := '';
                response.LastModifiedDate := now;
              finally
                profile.free;
              end;
            finally
              coded.Free;
            end;
          finally
            vs.free;
          end;
        finally
          params.free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirValueSetValidationOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/Resource-validate';
end;

  {$IFNDEF FHIR2}
{ TFhirCodeSystemComposeOperation }

function TFhirCodeSystemComposeOperation.Name: String;
begin
  result := 'compose';
end;

function TFhirCodeSystemComposeOperation.owningResource: TFhirResourceType;
begin
  result := frtCodeSystem;
end;

function TFhirCodeSystemComposeOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtCodeSystem];
end;

function TFhirCodeSystemComposeOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirCodeSystemComposeOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TFHIRComposeOpRequest;
  resp : TFHIRComposeOpResponse;
  resourceKey, versionKey : integer;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        req := TFHIRComposeOpRequest.Create;
        try
          if (request.Resource <> nil) and (request.Resource is TFHIRParameters) then
            req.load(request.Resource as TFHIRParameters)
          else
            req.load(request.Parameters);

          // first, we have to identify the Code System
          if request.Id <> '' then // and it must exist, because of the check above
            raise Exception.Create('Specifying a code system is not supported (only snomed-ct is supported)');
          if req.system <> 'http://snomed.info/sct' then
            raise Exception.Create('Only snomed-ct is supported)');
          // ok, it's snomed
          resp := TFHIRComposeOpResponse.Create;
          try
            try
              FServer.composeCode(req, resp);
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := resp.asParams;
              response.HTTPCode := 200;
              response.Message := 'OK';
            except
              on e : Exception do
              begin
                response.HTTPCode := 400;
                response.Message := 'Error';
                response.Resource := BuildOperationOutcome(request.Lang, e, IssueTypeCodeInvalid);
              end;
            end;
          finally
            resp.Free;
          end;
        finally
          req.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirCodeSystemComposeOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/CodeSystem-compose';
end;

function TFhirCodeSystemComposeOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirSubsumesSystemOperation }

function TFhirSubsumesOperation.Name: String;
begin
  result := 'subsumes';
end;

function TFhirSubsumesOperation.owningResource: TFhirResourceType;
begin
  result := frtCodeSystem;
end;

function TFhirSubsumesOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtCodeSystem];
end;

function TFhirSubsumesOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirSubsumesOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TFHIRSubsumesOpRequest;
  resp : TFHIRSubsumesOpResponse;
  resourceKey, versionKey : integer;
  cs : TFhirCodeSystem;
  cacheId : string;
  needSecure : boolean;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        req := TFHIRSubsumesOpRequest.create();
        try
          if (request.Resource <> nil) and (request.Resource is TFHIRParameters) then
            req.load(request.Resource as TFHIRParameters)
          else
            req.load(request.Parameters);

          response.Body := '';
          response.LastModifiedDate := now;
          resp := TFHIRSubsumesOpResponse.Create;
          try
            try
              if (request.Id = '') and (req.system <> '') and (req.codeA <> '') and (req.codeB <> '') then
              begin
                if not FServer.isValidCode(req.system, req.codeA) or not FServer.isValidCode(req.system, req.codeB) then
                  raise Exception.Create('Invalid code')
                else if (req.codeA = req.codeB) then
                  resp.outcome := 'equivalent'
                else if FServer.subsumes(req.system, req.codeA, req.system, req.codeB) then
                  resp.outcome := 'subsumes'
                else if FServer.subsumes(req.system, req.codeB, req.system, req.codeA) then
                  resp.outcome := 'subsumed-by'
                else
                  resp.outcome := 'not-subsumed';
              end
              else
              begin
                // first, we have to identify the Code System
                if request.Id <> '' then // and it must exist, because of the check above
                  cs := manager.GetResourceById(request, 'CodeSystem', request.Id, request.baseUrl, needSecure) as TFhirCodeSystem
                else if req.system <> '' then
                  cs := manager.GetResourceByUrl(frtCodeSystem, req.system, req.version, false, needSecure) as TFhirCodeSystem
                else
                  raise Exception.Create('No CodeSystem Identified (need a system parameter, or execute the operation on a CodeSystem resource');

                cacheId := cs.url;
                if (req.codingA = nil) and (req.codeA <> '') then
                  req.codingA := TFhirCoding.Create(cs.url, req.codeA);
                if (req.codingB = nil) and (req.codeB <> '') then
                  req.codingB := TFhirCoding.Create(cs.url, req.codeB);
                if req.codingA = nil then
                  raise Exception.Create('No codeA or codingA parameter found');
                if req.codingB = nil then
                  raise Exception.Create('No codeB or codingB parameter found');

                resp.outcome := FServer.subsumes(cs, req.codingA, req.codingB);
              end;
              response.Resource := resp.asParams;
              response.HTTPCode := 200;
              response.Message := 'OK';
            except
              on e : Exception do
              begin
                response.HTTPCode := 400;
                response.Message := 'Error';
                response.Resource := BuildOperationOutcome(request.Lang, e, IssueTypeCodeInvalid);
              end;
            end;
          finally
            resp.Free;
          end;
        finally
          req.Free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirSubsumesOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/CodeSystem-subsumes';
end;

function TFhirSubsumesOperation.isWrite: boolean;
begin
  result := false;
end;

{$ENDIF}

{ TFhirConceptMapTranslationOperation }

function TFhirConceptMapTranslationOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtConceptMap];
end;

function TFhirConceptMapTranslationOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

function TFhirConceptMapTranslationOperation.Name: String;
begin
  result := 'translate';
end;

function TFhirConceptMapTranslationOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirConceptMapTranslationOperation.owningResource: TFhirResourceType;
begin
  result := frtConceptMap;
end;

procedure TFhirConceptMapTranslationOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  cm : TLoadedConceptMap;
//  op : TFhirOperationOutcome;
//  resourceKey : integer;
  coded : TFhirCodeableConcept;
  coding : TFhirCoding;
//  abstractOk : boolean;
  params : TFhirParameters;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
        params := makeParams(request);
        try
          // we have to find the right concept map
          // it doesn't matter whether the value sets are actually defined or not
          if request.id <> '' then
            cm := FServer.getConceptMapById(request.id)
          else
            cm := FServer.getConceptMapBySrcTgt(params.str['valueset'], params.str['target']);
          if cm = nil then
            raise Exception.Create('Unable to find concept map to use');
          try
            // ok, now we need to find the source code to validate
            coded := nil;
            if params.hasParameter('coding') then
            begin
              coded := TFhirCodeableConcept.Create;
              coded.codingList.add(LoadDTFromParam(request.Context, params.str['coding'], request.lang, 'coding', TFhirCoding) as TFhirCoding)
            end
            else if params.hasParameter('codeableConcept') then
              coded := LoadDTFromParam(request.Context, params.str['codeableConcept'], request.lang, 'codeableConcept', TFhirCodeableConcept) as TFhirCodeableConcept
            else if params.hasParameter('code') and params.hasParameter('system') then
            begin
              coded := TFhirCodeableConcept.Create;
              coding := coded.codingList.Append;
              coding.system := params.str['system'];
              coding.version := params.str['version'];
              coding.code := params.str['code'];
              coding.display := params.str['display'];
            end
            else
              raise Exception.Create('Unable to find code to translate (coding | codeableConcept | code');
            try
              response.resource := FServer.translate(request.Lang, cm, coded.codingList[0]);
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
            finally
              coded.Free;
            end;
          finally
            cm.free;
          end;
        finally
          params.free;
        end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirConceptMapTranslationOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/ConceptMap-translate';
end;

{ TFhirLookupCodeSystemOperation }

function TFhirLookupCodeSystemOperation.Name: String;
begin
  result := 'lookup';
end;

function TFhirLookupCodeSystemOperation.owningResource: TFhirResourceType;
begin
  result := frtCodeSystem;
end;

function TFhirLookupCodeSystemOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtCodeSystem];
end;

function TFhirLookupCodeSystemOperation.CreateDefinition(base : String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirLookupCodeSystemOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TFHIRLookupOpRequest;
  resp : TFHIRLookupOpResponse;
  lang : String;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      if (request.id <> '') then
        raise Exception.Create('Lookup does not take an identified resource');
      req := TFHIRLookupOpRequest.create();
      try
        if (request.Resource <> nil) and (request.Resource is TFHIRParameters) then
          req.load(request.Resource as TFHIRParameters)
        else
          req.load(request.Parameters);

        if (req.coding = nil) and (req.system <> '') then
        begin
          req.coding := TFhirCoding.Create;
          req.coding.system := req.system;
          req.coding.code := req.code;
          req.coding.version := req.version;
        end;
        if req.coding = nil then
          raise Exception.Create('Unable to find a code to lookup (need coding or system/code)');
        lang := request.lang;
        {$IFNDEF FHIR2}
        if req.displayLanguage <> '' then
          lang := req.displayLanguage;
        {$ENDIF}

        response.Body := '';
        response.LastModifiedDate := now;
        resp := TFHIRLookupOpResponse.Create;
        try
          try
            FServer.lookupCode(req.coding, lang, {$IFNDEF FHIR2}req.property_List{$ELSE} nil {$ENDIF}, resp);  // currently, we ignore the date
            response.Resource := resp.asParams;
            response.HTTPCode := 200;
            response.Message := 'OK';
          except
            on e : Exception do
            begin
              response.HTTPCode := 400;
              response.Message := 'Error';
              response.Resource := BuildOperationOutcome(request.Lang, e, IssueTypeCodeInvalid);
            end;
          end;
        finally
          resp.Free;
        end;
      finally
        req.Free;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirLookupCodeSystemOperation.formalURL: String;
begin
  {$IFNDEF FHIR2}
  result := 'http://hl7.org/fhir/OperationDefinition/CodeSystem-lookup';
  {$ELSE}
  result := 'http://hl7.org/fhir/OperationDefinition/ValueSet-lookup';
  {$ENDIF}
end;

function TFhirLookupCodeSystemOperation.isWrite: boolean;
begin
  result := false;
end;

{ TFhirConceptMapClosureOperation }

function TFhirConceptMapClosureOperation.checkName(request: TFHIRRequest; response: TFHIRResponse; var name: String) : boolean;
begin
  if request.Session.UserEvidence = userAnonymous then
    result := IsGuid(name)
  else
  begin
    result := IsId(name);
    if result and not IsGUID(name) then
      name := inttostr(request.Session.UserKey)+'|'+name;
  end;
  if not result then
  begin
    response.HTTPCode := 400;
    response.Message := StringFormat('invalid closure name %s', [request.ResourceName+':'+request.Id]);
    response.Body := response.Message;
    response.Resource := BuildOperationOutcome(request.lang, response.Message);
  end;
end;

constructor TFhirConceptMapClosureOperation.Create(server: TTerminologyServer; Connection: TKDBConnection);
begin
  inherited Create(server);
  FConnection := Connection;
end;

function TFhirConceptMapClosureOperation.CreateDefinition(base: String): TFHIROperationDefinition;
begin
  result := nil;
end;

procedure TFhirConceptMapClosureOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  params : TFhirParameters;
  p : TFhirParametersParameter;
  n, v : String;
  cm : TClosureManager;
  map : TFhirConceptMap;
  concepts : TAdvList<TFHIRCoding>;
  procedure errorResp(code : integer; message : String);
  begin
    response.HTTPCode := code;
    response.Message := message;
    response.Body := response.Message;
    response.Resource := BuildOperationOutcome(request.lang, response.Message);
  end;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), IssueTypeForbidden) then
    begin
      params := makeParams(request);
      cm := nil;
      map := nil;
      try
        n := params.str['name'];
        if checkName(request, response, n) then
        begin
          v := params.str['version'];
          if (v = '') and not params.hasParameter('concept') then
          begin
            v := FServer.InitClosure(n);
            map := TFhirConceptMap.Create;
            response.resource := map.Link;
            map.id := NewGuidId;
            map.version := v;
            map.status := PublicationStatusActive;
            map.experimental := true; // for now
            map.date := TDateTimeEx.makeUTC;
            map.name := 'Closure Table '+n+' initialized';
            response.HTTPCode := 200;
            response.Message := 'OK';
            response.Body := '';
          end
          else
          begin
            if not FServer.UseClosure(n, cm) then
              errorResp(404, StringFormat('closure name ''%s'' not known', [n]))
            else if (v <> '') and params.hasParameter('concept') then
             errorResp(404, StringFormat('closure ''%s'': cannot combine version and concept', [n]))
            else if (v <> '') and not StringIsInteger32(v) then
              errorResp(404, StringFormat('closure ''%s'': version %s is not valid', [n, v]))
            else
            begin
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              map := TFhirConceptMap.Create;
              response.resource := map.Link;
              map.id := NewGuidId;
              map.version := inttostr(cm.version);
              map.status := PublicationStatusActive;
              map.experimental := true; // for now
              map.date := TDateTimeEx.makeUTC;
              map.name := 'Updates for Closure Table '+n;
              if (v <> '') then
              begin
                map.name := 'Replay for Closure Table '+n+' from version '+v;
                cm.rerun(Fconnection, map, StrToInt(v))
              end
              else
              begin
                map.name := 'Updates for Closure Table '+n;
                concepts := TAdvList<TFHIRCoding>.create;
                try
                  for p in params.parameterList do
                    if p.Name = 'concept' then
                      concepts.Add((p.value as TFHIRCoding).link);
                  cm.processConcepts(FConnection, concepts, map);
                finally
                  concepts.Free;
                end;
              end;
            end;
          end;
        end;
      finally
        params.free;
        cm.Free;
        map.Free;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFhirConceptMapClosureOperation.formalURL: String;
begin
  result := 'http://hl7.org/fhir/OperationDefinition/ConceptMap-closure';
end;

function TFhirConceptMapClosureOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirConceptMapClosureOperation.Name: String;
begin
  result := 'closure';
end;

function TFhirConceptMapClosureOperation.owningResource: TFhirResourceType;
begin
  result := frtNull;
end;

function TFhirConceptMapClosureOperation.Types: TFhirResourceTypeSet;
begin
  result := [frtNull];
end;


end.
