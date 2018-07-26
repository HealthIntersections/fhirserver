unit FHIR.Tx.Operations;

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
  FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Database.Manager,
  FHIR.Base.Objects, FHIR.Base.Lang, FHIR.Base.Utilities, FHIR.Base.Common, FHIR.Base.Factory,
  FHIR.Tools.ValueSets,
  FHIR.Server.Session, FHIR.Server.Storage, FHIR.Tx.Service, FHIR.Tx.Manager, FHIR.Tx.Server, FHIR.Server.ClosureMgr;

type
  TFhirTerminologyOperation = class (TFhirOperation)
  protected
    FServer : TTerminologyServer;
    function buildExpansionParams(request: TFHIRRequest; manager: TFHIROperationEngine; params : TFhirParametersW) : TFHIRExpansionParams;
    function loadCoded(request : TFHIRRequest) : TFhirCodeableConceptW;
  public
    Constructor Create(factory : TFHIRFactory; server : TTerminologyServer);
    Destructor Destroy; override;
  end;

  TFhirExpandValueSetOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirValueSetValidationOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

(**
  TFhirCodeSystemComposeOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;
*)

  TFhirSubsumesOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;

  TFhirConceptMapTranslationOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;


  TFhirLookupCodeSystemOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;


  TFhirConceptMapClosureOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
    function checkName(request: TFHIRRequest; response : TFHIRResponse; var name : String) : boolean;
  public
    Constructor Create(factory : TFHIRFactory; server : TTerminologyServer);
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    procedure Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse); override;
    function formalURL : String; override;
  end;


implementation

{ TFhirExpandValueSetOperation }

function TFhirExpandValueSetOperation.Name: String;
begin
  result := 'expand';
end;

function TFhirExpandValueSetOperation.owningResource: String;
begin
  result := 'ValueSet';
end;

function TFhirExpandValueSetOperation.Types: TArray<String>;
begin
  result := ['ValueSet'];
end;

function TFhirExpandValueSetOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

procedure TFhirExpandValueSetOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  vs, dst : TFHIRValueSetW;
  resourceKey, versionKey : integer;
  url, cacheId, filter, id : String;
  profile : TFHIRExpansionParams;
  limit, count, offset : integer;
  params : TFhirParametersW;
  needSecure : boolean;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
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
            vs := FFactory.wrapValueSet(manager.GetResourceById(request, 'ValueSet', request.Id, request.baseUrl, needSecure));
            cacheId := vs.url;
            if vs.version <> '' then
              cacheId := cacheId + vs.version;
          end
          else if params.has('url') then
          begin
            url := params.str('url');
            if (url.startsWith('ValueSet/')) then
              vs := FFactory.wrapValueSet(manager.GetResourceById(request, 'ValueSet', url.substring(9), request.baseUrl, needSecure))
            else if (url.startsWith(request.baseURL+'ValueSet/')) then
              vs := FFactory.wrapValueSet(manager.GetResourceById(request, 'ValueSet', url.substring(9), request.baseUrl, needSecure))
            else if not FServer.isKnownValueSet(url, vs) then
              vs := FFactory.wrapValueSet(manager.GetResourceByUrl('ValueSet', url, request.Parameters.getvar('version'), false, needSecure));
            cacheId := vs.url;
            if vs.version <> '' then
              cacheId := cacheId + vs.version;
          end
          else if params.has('valueSet') then
            vs := FFactory.wrapValueSet(params.obj('valueSet').Link as TFHIRResourceV)
          else if (request.Resource <> nil) and (request.Resource.fhirType = 'ValueSet') then
            vs := FFactory.wrapValueSet(request.Resource.Link)
          else if params.has('context') then
          begin
            id := params.str('context');
            if params.has('operation') then
              id := id+'-'+params.str('operation');
            vs := FFactory.wrapValueSet(manager.GetResourceById(request, 'ValueSet', id, request.baseUrl, needSecure));
            if vs = nil then
              raise ETerminologyError.create('The context '+id+' was not understood');
            cacheId := vs.url;
            if vs.version <> '' then
              cacheId := cacheId + vs.version;
          end
          else
            raise ETerminologyError.create('Unable to find value set to expand (not provided by id, identifier, or directly)');

          vs.checkNoImplicitRules('ExpandValueSet', 'ValueSet');
          FFactory.checkNoModifiers(vs.Resource, 'ExpandValueSet', 'ValueSet');

          profile := buildExpansionParams(request, manager, params);
          try
            filter := params.str('filter');
            count := StrToIntDef(params.str('count'), 0);
            offset := StrToIntDef(params.str('offset'), 0);
            limit := StrToIntDef(params.str('_limit'), 0);
            if profile.displayLanguage = '' then
              profile.displayLanguage := request.Lang;

            dst := FServer.expandVS(vs, cacheId, profile, filter, limit, count, offset);
            try
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
              response.Resource := dst.Resource.Link;
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

function TFhirValueSetValidationOperation.owningResource: String;
begin
  result := 'ValueSet';
end;

function TFhirValueSetValidationOperation.Types: TArray<String>;
begin
  result := ['ValueSet', 'CodeSystem'];
end;

function TFhirValueSetValidationOperation.isWrite: boolean;
begin
  result := false;
end;

function TFhirValueSetValidationOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

procedure TFhirValueSetValidationOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  vs : TFHIRValueSetW;
  resourceKey, versionKey : integer;
  cacheId  : String;
  coded : TFhirCodeableConceptW;
  coding : TFhirCodingW;
  abstractOk : boolean;
  params, pout : TFhirParametersW;
  needSecure : boolean;
  profile : TFhirExpansionParams;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
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
              vs := FFactory.wrapValueSet(manager.GetResourceById(request, 'ValueSet', request.Id, request.baseUrl, needSecure));
              cacheId := vs.url;
            end
            else if params.has('url') then
            begin
              if not FServer.isKnownValueSet(params.str('url'), vs) then
                vs := FFactory.wrapValueSet(manager.GetResourceByUrl('ValueSet', params.str('url'), params.str('version'), false, needSecure));
              if vs = nil then
                raise ETerminologySetup.Create('Error Message');
              cacheId := vs.url;
            end
            else if params.has('valueSet') then
              vs := FFactory.wrapValueSet(params.obj('valueSet').Link as TFHIRResourceV)
            else if (request.Resource <> nil) and (request.Resource.fhirType = 'ValueSet') then
              vs := FFactory.wrapValueSet(request.Resource.Link)
            else
              vs := nil;
              // raise ETerminologyError.create('Unable to find valueset to validate against (not provided by id, identifier, or directly)');

            coded := loadCoded(request);
            try
              abstractOk := params.str('abstract') = 'true';

              if (coded = nil) then
                raise ETerminologyError.create('Unable to find code to validate (coding | codeableConcept | code');

              if vs <> nil then
              begin
                vs.checkNoImplicitRules('ValueSetValidation', 'ValueSet');
                FFactory.checkNoModifiers(vs.Resource, 'ValueSetValidation', 'ValueSet');
              end;

              profile := buildExpansionParams(request, manager, params);
              try
                if profile.displayLanguage = '' then
                  profile.displayLanguage := request.Lang;
                try
                  pout := FServer.validate(vs, coded, profile, abstractOk);
                  try
                    response.resource := pout.Resource.link;
                  finally
                    pOut.free;
                  end;
                except
                  on e : Exception do
                  begin
                    pout := FFactory.wrapParams(ffactory.makeResource('Parameters'));
                    try
                      response.resource := pout.Resource.link;
                      pout.addParamBool('result', false);
                      pout.addParamStr('message', e.Message);
                      pout.addParamStr('cause', 'unknown');
                    finally
                      pOut.Free;
                    end;
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

(*
{ TFhirCodeSystemComposeOperation }

function TFhirCodeSystemComposeOperation.Name: String;
begin
  result := 'compose';
end;

function TFhirCodeSystemComposeOperation.owningResource: String;
begin
  result := 'CodeSystem';
end;

function TFhirCodeSystemComposeOperation.Types: TArray<String>;
begin
  result := ['CodeSystem'];
end;

function TFhirCodeSystemComposeOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
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
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
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
            raise ETerminologyError.create('Specifying a code system is not supported (only snomed-ct is supported)');
          if req.system <> 'http://snomed.info/sct' then
            raise ETerminologyError.create('Only snomed-ct is supported)');
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
*)

{ TFhirConceptMapTranslationOperation }

function TFhirConceptMapTranslationOperation.Types: TArray<String>;
begin
  result := ['ConceptMap'];
end;

function TFhirConceptMapTranslationOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
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

function TFhirConceptMapTranslationOperation.owningResource: String;
begin
  result := 'ConceptMap';
end;

procedure TFhirConceptMapTranslationOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  cm : TLoadedConceptMap;
//  op : TFhirOperationOutcome;
//  resourceKey : integer;
  coded : TFhirCodeableConceptW;
  coding : TFslList<TFhirCodingW>;
//  abstractOk : boolean;
  params, pOut : TFhirParametersW;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
        params := makeParams(request);
        try
          // we have to find the right concept map
          // it doesn't matter whether the value sets are actually defined or not
          if request.id <> '' then
            cm := FServer.getConceptMapById(request.id)
          else
            cm := FServer.getConceptMapBySrcTgt(params.str('valueset'), params.str('target'));
          if cm = nil then
            raise ETerminologyError.create('Unable to find concept map to use');
          try
            // ok, now we need to find the source code to validate
            coded := loadCoded(request);
(*            if params.has('coding') then
            begin
              coded := TFhirCodeableConcept.Create;
              coded.codingList.add(LoadDTFromParam(request.Context, params.str['coding'], request.lang, 'coding', TFhirCoding) as TFhirCoding)
            end
            else if params.has('codeableConcept') then
              coded := LoadDTFromParam(request.Context, params.str['codeableConcept'], request.lang, 'codeableConcept', TFhirCodeableConcept) as TFhirCodeableConcept
            else if params.has('code') and params.has('system') then
            begin
              coded := TFhirCodeableConcept.Create;
              coding := coded.codingList.Append;
              coding.system := params.str['system'];
              coding.version := params.str['version'];
              coding.code := params.str['code'];
              coding.display := params.str['display'];
            end
            else
              raise ETerminologyError.create('Unable to find code to translate (coding | codeableConcept | code');
              *)
            try
              coding := coded.codings;
              try
                pOut := FServer.translate(request.Lang, cm, coding[0]);
                try
                  response.resource := pOut.Resource.link;
                  response.HTTPCode := 200;
                  response.Message := 'OK';
                  response.Body := '';
                  response.LastModifiedDate := now;
                finally
                  pOut.Free;
                end;
              finally
                coding.Free;
              end;
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

function TFhirLookupCodeSystemOperation.owningResource: String;
begin
  result := 'CodeSystem';
end;

function TFhirLookupCodeSystemOperation.Types: TArray<String>;
begin
  result := ['CodeSystem'];
end;

function TFhirLookupCodeSystemOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

procedure TFhirLookupCodeSystemOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TFHIRLookupOpRequestW;
  resp : TFHIRLookupOpResponseW;
  c : TFhirCodingW;
  lang : String;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (request.id <> '') then
        raise ETerminologyError.create('Lookup does not take an identified resource');
      req := ffactory.makeOpReqLookup;
      try
        if (request.Resource <> nil) and (request.Resource.fhirType = 'Parameters') then
          req.load(request.Resource)
        else
          req.load(request.Parameters);
        req.loadCoding;
        lang := request.lang;
        if req.displayLanguage <> '' then
          lang := req.displayLanguage;

        response.Body := '';
        response.LastModifiedDate := now;
        resp := ffactory.makeOpRespLookup;
        try
          try
            c := req.coding;
            try
              FServer.lookupCode(c, lang, req.propList, resp);  // currently, we ignore the date
            finally
              c.Free;
            end;
            response.Resource := resp.asParams;
            response.HTTPCode := 200;
            response.Message := 'OK';
          except
            on e : Exception do
            begin
              response.HTTPCode := 400;
              response.Message := 'Error';
              response.Resource := FFactory.BuildOperationOutcome(request.Lang, e, itInvalid);
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
  if FFactory.version = fhirVersionRelease2 then
    result := 'http://hl7.org/fhir/OperationDefinition/CodeSystem-lookup'
  else
    result := 'http://hl7.org/fhir/OperationDefinition/ValueSet-lookup';
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
    response.Resource := FFactory.BuildOperationOutcome(request.lang, response.Message);
  end;
end;

constructor TFhirConceptMapClosureOperation.Create(factory : TFHIRFactory; server: TTerminologyServer);
begin
  inherited Create(factory, server);
end;

function TFhirConceptMapClosureOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

procedure TFhirConceptMapClosureOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  params : TFhirParametersW;
  p : TFhirParametersParameterW;
  n, v : String;
  cm : TClosureManager;
  map : TFhirConceptMapW;
  concepts : TFslList<TFHIRCodingW>;
  procedure errorResp(code : integer; message : String);
  begin
    response.HTTPCode := code;
    response.Message := message;
    response.Body := response.Message;
    response.Resource := FFactory.BuildOperationOutcome(request.lang, response.Message);
  end;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      params := makeParams(request);
      cm := nil;
      map := nil;
      try
        n := params.str('name');
        if checkName(request, response, n) then
        begin
          v := params.str('version');
          if (v = '') and not params.has('concept') then
          begin
            v := FServer.InitClosure(n);
            map := FFactory.wrapConceptMap(FFactory.makeResource('ConceptMap'));
            response.resource := map.Resource.Link;
            map.id := NewGuidId;
            map.version := v;
            map.status := psActive;
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
            else if (v <> '') and params.has('concept') then
             errorResp(404, StringFormat('closure ''%s'': cannot combine version and concept', [n]))
            else if (v <> '') and not StringIsInteger32(v) then
              errorResp(404, StringFormat('closure ''%s'': version %s is not valid', [n, v]))
            else
            begin
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              map := FFactory.wrapConceptMap(FFactory.makeResource('ConceptMap'));
              response.resource := map.Resource.Link;
              map.id := NewGuidId;
              map.version := inttostr(cm.version);
              map.status := psActive;
              map.date := TDateTimeEx.makeUTC;
              map.name := 'Updates for Closure Table '+n;
              if (v <> '') then
              begin
                map.name := 'Replay for Closure Table '+n+' from version '+v;
                // cm.rerun(Fconnection, map, StrToInt(v))
              end
              else
              begin
                map.name := 'Updates for Closure Table '+n;
                concepts := TFslList<TFHIRCodingW>.create;
                try
                  for p in params.parameterList do
                    if p.Name = 'concept' then
                      concepts.Add(FFactory.wrapCoding(p.value.Link));
                  // cm.processConcepts(FConnection, concepts, map);
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

function TFhirConceptMapClosureOperation.owningResource: String;
begin
  result := '';
end;

function TFhirConceptMapClosureOperation.Types: TArray<String>;
begin
  result := [];
end;

{ TFhirSubsumesSystemOperation }

function TFhirSubsumesOperation.Name: String;
begin
  result := 'subsumes';
end;

function TFhirSubsumesOperation.owningResource: String;
begin
  result := 'CodeSystem';
end;

function TFhirSubsumesOperation.Types: TArray<String>;
begin
  result := ['CodeSystem'];
end;

function TFhirSubsumesOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

procedure TFhirSubsumesOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse);
var
  req : TFHIRSubsumesOpRequestW;
  resp : TFHIRSubsumesOpResponseW;
  resourceKey, versionKey : integer;
  cs : TFhirCodeSystemW;
  ca, cb : TFhirCodingW;
  cacheId : string;
  needSecure : boolean;
begin
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.lang), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        req := FFactory.makeOpReqSubsumes;
        try
          if (request.Resource <> nil) and (request.Resource.fhirType = 'Parameters') then
            req.load(request.Resource)
          else
            req.load(request.Parameters);

          response.Body := '';
          response.LastModifiedDate := now;
          resp := FFactory.makeOpRespSubsumes;
          try
            try
              if (request.Id = '') and (req.system <> '') and (req.codeA <> '') and (req.codeB <> '') then
              begin
                if not FServer.isValidCode(req.system, req.codeA) or not FServer.isValidCode(req.system, req.codeB) then
                  raise ETerminologyError.create('Invalid code')
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
                  cs := FFactory.wrapCodeSystem(manager.GetResourceById(request, 'CodeSystem', request.Id, request.baseUrl, needSecure))
                else if req.system <> '' then
                  cs := FFactory.wrapCodeSystem(manager.GetResourceByUrl('CodeSystem', req.system, req.version, false, needSecure))
                else
                  raise ETerminologyError.create('No CodeSystem Identified (need a system parameter, or execute the operation on a CodeSystem resource');

                cacheId := cs.url;
                ca := req.codingA;
                cb := req.codingB;
                try
                  if (ca = nil) and (req.codeA <> '') then
                    ca := FFactory.wrapCoding(FFactory.makeCoding(cs.url, req.codeA));
                  if (cb = nil) and (req.codeB <> '') then
                    cb := FFactory.wrapCoding(FFactory.makeCoding(cs.url, req.codeB));
                  if ca = nil then
                    raise ETerminologyError.create('No codeA or codingA parameter found');
                  if cb = nil then
                    raise ETerminologyError.create('No codeB or codingB parameter found');

                  resp.outcome := FServer.subsumes(cs, ca, cb);
                finally
                  ca.Free;
                  cb.Free;
                end;
              end;
              response.Resource := resp.asParams;
              response.HTTPCode := 200;
              response.Message := 'OK';
            except
              on e : Exception do
              begin
                response.HTTPCode := 400;
                response.Message := 'Error';
                response.Resource := FFactory.BuildOperationOutcome(request.Lang, e, itInvalid);
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


{ TFhirTerminologyOperation }

function TFhirTerminologyOperation.buildExpansionParams(request: TFHIRRequest; manager: TFHIROperationEngine; params: TFhirParametersW): TFHIRExpansionParams;
var
  needSecure : boolean;
  p : TFhirParametersParameterW;
  exp : TFhirExpansionParams;
begin
  result := TFHIRExpansionParams.Create;
  try
//    if FFactory.version = fhirVersionRelease3 then
//    begin
//      exp := nil;
//      try
//        if params.has('profile') then
//        begin
//          p := params.param['profile'];
//          if p.hasResource then
//            exp := p.resource.link as FHIR.R3.Resources.TFhirExpansionProfile
//          else if params.str('profile').StartsWith('http:') or params.str('profile').StartsWith('https:') then
//           exp := manager.getResourceByUrl('ExpansionProfile', params.str('profile'), '', true, needSecure) as FHIR.R3.Resources.TFhirExpansionProfile
//         else if params.str('profile') <> '' then
//           exp := manager.GetResourceById(request, 'ExpansionProfile', params.str('profile'), request.baseUrl, needSecure) as FHIR.R3.Resources.TFhirExpansionProfile
//        end;
//        if (exp <> nil) then
//        begin
//          if (exp.limitedExpansionElement <> nil) then
//            result.limitedExpansion := exp.limitedExpansion;
//          if (exp.displayLanguageElement <> nil) then
//            result.displayLanguage := exp.displayLanguage;
//          if (exp.includeDesignationsElement <> nil) then
//            result.includeDesignations := exp.includeDesignations;
//          if (exp.includeDefinitionElement <> nil) then
//            result.includeDefinition := exp.includeDefinition;
//          if (exp.activeOnlyElement <> nil) then
//            result.activeOnly := exp.activeOnly;
//          if (exp.excludeNestedElement <> nil) then
//            result.excludeNested := exp.excludeNested;
//          if (exp.excludeNotForUIElement <> nil) then
//            result.excludeNotForUI := exp.excludeNotForUI;
//          if (exp.excludePostCoordinatedElement <> nil) then
//            result.excludePostCoordinated := exp.excludePostCoordinated;
//        end;
//      finally
//        exp.Free;
//      end;
//    end;
    if (params.str('_incomplete') <> '') then
      result.limitedExpansion := StrToBoolDef(params.str('_incomplete'), false);
    if (params.str('limitedExpansion') <> '') then
      result.limitedExpansion := StrToBoolDef(params.str('limitedExpansion'), false);
    if (params.str('displayLanguage') <> '') then
      result.displayLanguage := params.str('displayLanguage');
    if (params.str('includeDesignations') <> '') then
      result.includeDesignations := StrToBoolDef(params.str('includeDesignations'), false);
    if (params.str('includeDefinition') <> '') then
      result.includeDefinition := StrToBoolDef(params.str('includeDefinition'), false);
    if (params.str('activeOnly') <> '') then
      result.activeOnly := StrToBoolDef(params.str('activeOnly'), false);
    if (params.str('excludeNested') <> '') then
      result.excludeNested := StrToBoolDef(params.str('excludeNested'), false);
    if (params.str('excludeNotForUI') <> '') then
      result.excludeNotForUI := StrToBoolDef(params.str('excludeNotForUI'), false);
    if (params.str('excludePostCoordinated') <> '') then
      result.excludePostCoordinated := StrToBoolDef(params.str('excludePostCoordinated'), false);
    result.link;
  finally
    result.free;
  end;
end;

constructor TFhirTerminologyOperation.Create(factory : TFHIRFactory; server: TTerminologyServer);
begin
  inherited Create(factory);
  FServer := server;
end;

destructor TFhirTerminologyOperation.Destroy;
begin
  FServer.Free;
  inherited;
end;

function TFhirTerminologyOperation.loadCoded(request : TFHIRRequest): TFhirCodeableConceptW;
var
  coding : TFhirCodingW;
  params : TFhirParametersW;
begin
  // ok, now we need to find the source code to validate
  if (request.form <> nil) and request.form.hasParam('coding') then
  begin
    result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
    coding := FFactory.makeDtFromForm(request.form.getParam('coding'), request.lang, 'coding', 'Coding') as TFHIRCodingW;
    try
      result.addCoding(coding);
    finally
      coding.free;
    end;
  end
  else if (request.form <> nil) and request.form.hasParam('codeableConcept') then
    result := FFactory.makeDtFromForm(request.form.getParam('codeableConcept'), request.lang, 'codeableConcept', 'CodeableConcept') as TFhirCodeableConceptW
  else if request.Parameters.VarExists('code') and request.Parameters.VarExists('system') then
  begin
    result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
    coding := result.addCoding;
    coding.system := request.Parameters.GetVar('system');
    coding.version := request.Parameters.GetVar('version');
    coding.code := request.Parameters.GetVar('code');
    coding.display := request.Parameters.GetVar('display');
  end
  else if ((request.resource <> nil) and (request.Resource.fhirType = 'Parameters')) then
  begin
    params := FFactory.wrapParams(request.Resource.link);
    try
      if params.has('coding') then
      begin
        result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
        coding := FFactory.wrapCoding(params.obj('coding').Link);
        try
          result.addCoding(coding);
        finally
          coding.free;
        end;
      end
      else if params.has('codeableConcept') then
        result := FFactory.wrapCodeableConcept(params.obj('codeableConcept').Link)
      else if params.has('code') and params.has('system') then
      begin
        result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
        coding := result.addCoding;
        try
          coding.system := params.str('system');
          if params.has('version') then
            coding.version := params.str('version');
          coding.code := params.str('code');
          if params.has('display') then
            coding.display := params.str('display');
        finally
          coding.free;
        end;
      end
      else
        raise ETerminologyError.create('Unable to find code to validate (params. coding | codeableConcept | code');
    finally
      params.free;
    end;
  end
  else
    raise ETerminologyError.create('Unable to find code to validate (coding | codeableConcept | code');
end;

end.
