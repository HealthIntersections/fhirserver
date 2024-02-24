unit tx_operations;

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

{$I fhir.inc}

interface

uses
  SysUtils,
  fsl_base, fsl_utilities, fsl_logging, fsl_http, fsl_lang,
  fdb_manager,
  fhir_objects,  fhir_utilities, fhir_common, fhir_factory,
  fhir_valuesets,
  session, storage, ftx_service, tx_manager, tx_server, closuremanager, time_tracker;

type

  { TFhirTerminologyOperation }

  TFhirTerminologyOperation = class (TFhirOperation)
  protected
    FServer : TTerminologyServer;

    function isValidation : boolean; virtual;
    procedure processExpansionParams(request: TFHIRRequest; manager: TFHIROperationEngine; params : TFhirParametersW; result : TFHIRExpansionParams);
    function buildExpansionParams(request: TFHIRRequest; manager: TFHIROperationEngine; params : TFhirParametersW) : TFHIRExpansionParams;
    function loadCoded(request : TFHIRRequest; isValueSet : boolean; var issuePath : string; var mode : TValidationCheckMode) : TFhirCodeableConceptW;
    function processAdditionalResources(context : TOperationContext; manager: TFHIROperationEngine; mr : TFHIRMetadataResourceW; params : TFHIRParametersW) : TFslMetadataResourceList;
  public
    constructor Create(factory : TFHIRFactory; server : TTerminologyServer; languages : TIETFLanguageDefinitions);
    destructor Destroy; override;
  end;

  TFhirExpandValueSetOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
    function readValueSetUri(manager: TFHIROperationEngine; url : String; op : String) : String;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse; tt : TTimeTracker) : String; override;
    function formalURL : String; override;
  end;

  { TFhirValueSetValidationOperation }

  TFhirValueSetValidationOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;  
    function isValidation : boolean; override;
  public
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse; tt : TTimeTracker) : String; override;
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
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse; tt : TTimeTracker) : String; override;
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
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse; tt : TTimeTracker) : String; override;
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
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse; tt : TTimeTracker) : String; override;
    function formalURL : String; override;
  end;


  TFhirConceptMapClosureOperation = class (TFhirTerminologyOperation)
  protected
    function isWrite : boolean; override;
    function owningResource : String; override;
    function checkName(request: TFHIRRequest; response : TFHIRResponse; var name : String) : boolean;
  public
    constructor Create(factory : TFHIRFactory; server : TTerminologyServer; languages : TIETFLanguageDefinitions);
    function Name : String; override;
    function Types : TArray<String>; override;
    function CreateDefinition(base : String) : TFHIROperationDefinitionW; override;
    function Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response : TFHIRResponse; tt : TTimeTracker) : String; override;
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

function TFhirExpandValueSetOperation.readValueSetUri(manager: TFHIROperationEngine; url, op: String): String;
var
  sd : TFhirStructureDefinitionW;
  ed : TFHIRElementDefinitionW;
  u, p, t : String;
  needSecure : boolean;
begin
  if url.Contains('#') then
    StringSplit(url, '#', u, p)
  else
  begin
    if not url.Contains('.') then
      raise EFSLException.Create('Unable to understand url "'+url+'"');
    StringSplit(url,'.',  u, t);
    u := 'http://hl7.org/fhir/StructureDefinition/'+u;
    p := url;
  end;
  sd := FFactory.wrapStructureDefinition(manager.GetResourceByUrl('StructureDefinition', u, '', false, needSecure));
  try
    ed := sd.getDefinition(p, edsSNAPSHOT);
    if ed = nil then
      raise EFSLException.Create('Unable to resolve element "'+p+'" in "'+u+'"');
    try
      if (ed.valueSet = '')  then
        raise EFSLException.Create('No value set for element "'+p+'" in "'+u+'"');
      result := ed.valueSet;
    finally
      ed.free;
    end;
  finally
    sd.free;
  end;
end;

function TFhirExpandValueSetOperation.Types: TArray<String>;
begin
  result := ['ValueSet'];
end;

function TFhirExpandValueSetOperation.CreateDefinition(base : String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirExpandValueSetOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse; tt : TTimeTracker) : String;
var
  vs, dst : TFHIRValueSetW;
  resourceKey, versionKey : integer;
  url, cacheId, filter, id, version : String;
  profile : TFHIRExpansionParams;
  limit, count, offset : integer;
  params : TFhirParametersW;
  needSecure : boolean;
  txResources : TFslMetadataResourceList;
  mr : TFHIRMetadataResourceW;
begin
  result := 'Expand ValueSet';
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.langList, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.langList), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        cacheId := '';
        params := makeParams(request);
        vs := nil;
        txResources := nil;
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
            version := request.Parameters['valueSetVersion'];
            txResources := processAdditionalResources(context, manager, nil, params);
            for mr in txResources do
              if (mr.url = url) and (mr is TFHIRValueSetW) then
              begin
                vs := (mr as TFHIRValueSetW).link;
                break;
              end;
            if (vs = nil) then
            begin
              if (url.startsWith('ValueSet/')) then
                vs := FFactory.wrapValueSet(manager.GetResourceById(request, 'ValueSet', url.substring(9), request.baseUrl, needSecure))
              else if (url.startsWith(request.baseURL+'ValueSet/')) then
                vs := FFactory.wrapValueSet(manager.GetResourceById(request, 'ValueSet', url.substring(9+request.baseURL.Length), request.baseUrl, needSecure))
              else
              begin
                vs := FServer.getValueSetByUrl(url, version);
                if vs = nil then
                  vs := FFactory.wrapValueSet(manager.getResourceByUrl('ValueSet', url, '', true, needSecure));
                if vs = nil then
                  if not FServer.isKnownValueSet(url, vs) then
                    vs := FFactory.wrapValueSet(manager.GetResourceByUrl('ValueSet', url, version, false, needSecure));
              end;
            end;
            if vs = nil then
            raise ETerminologyError.Create('Unable to find value set for URL "'+url+'"', itUnknown);

            cacheId := vs.url;
            if vs.version <> '' then
              cacheId := cacheId + vs.version;
          end
          else if params.has('valueSet') then
          begin
            vs := FFactory.wrapValueSet(params.obj('valueSet').Link as TFHIRResourceV);
            vs.tagInt := 1;
            txResources := processAdditionalResources(context, manager, vs, params);
          end
          else if (request.Resource <> nil) and (request.Resource.fhirType = 'ValueSet') then
          begin
            vs := FFactory.wrapValueSet(request.Resource.Link);
            vs.tagInt := 1;
            txResources := processAdditionalResources(context, manager, vs, params);
          end
          else if params.has('context') then
          begin
            id := params.str('context');
            id := readValueSetUri(manager, id, params.str('operation'));
            vs := FFactory.wrapValueSet(manager.getResourceByUrl('ValueSet', id, '', false, needSecure));
            if vs = nil then
              raise ETerminologyError.Create('The context '+id+' was not understood', itInvalid);
            cacheId := vs.url;
            if vs.version <> '' then
              cacheId := cacheId + vs.version;
          end
          else
            raise ETerminologyError.Create('Unable to find value set to expand (not provided by id, identifier, or directly)', itUnknown);

          if vs.getId <> '' then
            result := 'Expand ValueSet '+vs.getId+' on '+vs.source
          else if vs.url <> '' then
            result := 'Expand ValueSet '+vs.url+' on '+vs.source
          else
            result := 'Expand inline ValueSet  on '+vs.source;
          vs.checkNoImplicitRules('ExpandValueSet', 'ValueSet');
          FFactory.checkNoModifiers(vs.Resource, 'ExpandValueSet', 'ValueSet');

          profile := buildExpansionParams(request, manager, params);
          try
            filter := params.str('filter');
            count := StrToIntDef(params.str('count'), -1);
            offset := StrToIntDef(params.str('offset'), -1);
            limit := StrToIntDef(params.str('_limit'), -1);
            if (limit < -1) then
              limit := -1
            else if limit > UPPER_LIMIT_TEXT then
              limit := UPPER_LIMIT_TEXT; // can't ask for more than this externally, though you can internally

            if (txResources = nil) then
              txResources := processAdditionalResources(context, manager, nil, params);
            dst := FServer.expandVS(vs, cacheId, profile, filter, limit, count, offset, txResources, params.str('no-cache') = 'please');
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
            profile.free;
          end;
        finally
          txResources.free;
          vs.free;
          params.free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
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

function TFhirValueSetValidationOperation.isValidation: boolean;
begin
  Result := true;
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

function canonicalMatches(mr : TFHIRMetadataResourceW; canonical, version : String) : boolean;
var
  l, r : String;
begin
  if canonical.Contains('|') then
  begin
    StringSplit(canonical, '|', l, r);
    if (version <> '') and (l <> version) then
      exit(false);
  end
  else
  begin
    l := canonical;
    r := version;
  end;

  result := (mr.url = l) and ((r = '') or (r = mr.version));
end;

function TFhirValueSetValidationOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse; tt : TTimeTracker) : String;
var
  vs : TFHIRValueSetW;
  resourceKey, versionKey : integer;
  cacheId, url, summary, issuePath, version, msg : String;
  coded : TFhirCodeableConceptW;
//  coding : TFhirCodingW;
  abstractOk, inferSystem : boolean;
  params, pout : TFhirParametersW;
  oOut : TFHIROperationOutcomeW;
  needSecure, isValueSet : boolean;
  mode : TValidationCheckMode;
  profile : TFhirExpansionParams;
  txResources : TFslMetadataResourceList;
  mr : TFHIRMetadataResourceW;        
begin
  isValueSet := request.ResourceName = 'ValueSet';

  result := 'Validate Code';
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.langList, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.langList), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (request.id = '') or ((length(request.id) <= ID_LENGTH) and manager.FindResource(request.ResourceName, request.Id, [], resourceKey, versionKey, request, response, nil)) then
      begin
        cacheId := '';
        params := makeParams(request);
        try
          vs := nil;
          txResources := nil;
          pout := nil;
          oOut := nil;
          profile := nil;
          try
            profile := buildExpansionParams(request, manager, params);
            coded := loadCoded(request, isValueSet, issuePath, mode);
            try
              result := 'Validate Code '+coded.renderText;
              try
                if isValueSet then
                begin
                  // first, we have to identify the value set.
                  if request.Id <> '' then // and it must exist, because of the check above
                  begin
                    vs := FFactory.wrapValueSet(manager.GetResourceById(request, 'ValueSet', request.Id, request.baseUrl, needSecure));
                    cacheId := vs.url;
                    result := result+' in vs '+request.id;
                  end
                  else if params.has('url')  then
                  begin
                    url := params.str('url');
                    version := params.str('valueSetVersion');
                    if (version = '') then
                      result := result+' in vs '+url+'|'+version+' (ref)'
                    else
                      result := result+' in vs '+url+' (ref)';
                    txResources := processAdditionalResources(context, manager, nil, params);
                    for mr in txResources do
                      if (canonicalMatches(mr, url, version)) and (mr is TFHIRValueSetW) then
                      begin
                        vs := (mr as TFHIRValueSetW).link;
                        break;
                      end;
                    if vs = nil then
                      vs := FServer.getValueSetByUrl(url, version);
                    if vs = nil then
                      if not FServer.isKnownValueSet(url, vs) then
                        vs := FFactory.wrapValueSet(manager.GetResourceByUrl('ValueSet', url, version, false, needSecure));
                    if vs = nil then
                    begin
                      msg := FServer.i18n.translate('Unable_to_resolve_value_Set_', profile.languages, [url]);
                      oOut := FFactory.wrapOperationOutcome(FFactory.makeResource('OperationOutcome'));
                      oOut.addIssue(isError, itNotFound, '', msg, oicNotFound);
                    end
                    else
                      cacheId := vs.vurl;
                  end
                  else if params.has('valueSet') then
                  begin
                    if not (params.obj('valueSet') is TFHIRResourceV) then
                      raise ETerminologyError.Create('Error with valueSet parameter - must be a value set', itInvalid);
                    vs := FFactory.wrapValueSet(params.obj('valueSet').Link as TFHIRResourceV);
                    result := result+' in vs '+vs.url+' (param)';
                    txResources := processAdditionalResources(context, manager, vs, params);
                  end
                  else if (request.Resource <> nil) and (request.Resource.fhirType = 'ValueSet') then
                  begin
                    vs := FFactory.wrapValueSet(request.Resource.Link);
                    result := result+' in vs '+vs.url+' (res)';
                    txResources := processAdditionalResources(context, manager, vs, params);
                  end
                  // else
                  // raise ETerminologyError.Create('Unable to find valueset to validate against (not provided by id, identifier, or directly)');
                end;

                abstractOk := params.str('abstract') = 'true';
                inferSystem := (params.str('inferSystem') = 'true') or (params.str('implySystem') = 'true');

                if (oOut = nil) and (pout = nil) then
                begin
                  if (coded = nil) then
                    raise ETerminologyError.Create('Unable to find code to validate (looked for coding | codeableConcept | code in parameters ='+params.names+')', itNotFound);

                  if vs <> nil then
                  begin
                    vs.checkNoImplicitRules('ValueSetValidation', 'ValueSet');
                    FFactory.checkNoModifiers(vs.Resource, 'ValueSetValidation', 'ValueSet');
                  end;
                  if txResources = nil then
                    txResources := processAdditionalResources(context, manager, nil, params);

                  pout := FServer.validate(issuePath, vs, coded, profile, abstractOk, inferSystem, mode, txResources, summary);
                end;
                if summary <> '' then
                  result := result + ': '+summary;
                if (oOut <> nil) then
                  response.resource := oOut.Resource.link
                else
                  response.resource := pout.Resource.link;
              finally
                pOut.free;
                oOut.free;
              end;
              response.HTTPCode := 200;
              response.Message := 'OK';
              response.Body := '';
              response.LastModifiedDate := now;
            finally
              coded.free;
            end;
          finally
            profile.free;
            vs.free;
            txResources.free;
          end;
        finally
          params.free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
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
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.lang, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.langList), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
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
            raise ETerminologyError.Create('Specifying a code system is not supported (only snomed-ct is supported)');
          if req.system <> URI_SNOMED then
            raise ETerminologyError.Create('Only snomed-ct is supported)');
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
            resp.free;
          end;
        finally
          req.free;
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

function TFhirConceptMapTranslationOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse; tt : TTimeTracker) : String;
var
  cm : TLoadedConceptMap;
//  op : TFhirOperationOutcome;
//  resourceKey : integer;
  coded : TFhirCodeableConceptW;
  coding : TFslList<TFhirCodingW>;
  dummy : TValidationCheckMode;
  params, pOut : TFhirParametersW;
  issuePath : String;
begin
  result := 'Translate';
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.langList, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.langList), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
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
            raise ETerminologyError.Create('Unable to find concept map to use', itNotFound);
          try
            // ok, now we need to find the source code to validate
            coded := loadCoded(request, true, issuePath, dummy);
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
              raise ETerminologyError.Create('Unable to find code to translate (looked for coding | codeableConcept | code in parameters ='+params.names+')');
              *)
            try
              coding := coded.codings;
              try
                pOut := FServer.translate(request.langList, cm, coding[0]);
                try
                  response.resource := pOut.Resource.link;
                  response.HTTPCode := 200;
                  response.Message := 'OK';
                  response.Body := '';
                  response.LastModifiedDate := now;
                finally
                  pOut.free;
                end;
              finally
                coding.free;
              end;
            finally
              coded.free;
            end;
          finally
            cm.free;
          end;
        finally
          params.free;
        end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
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

function TFhirLookupCodeSystemOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse; tt : TTimeTracker) : String;
var
  req : TFHIRLookupOpRequestW;
  resp : TFHIRLookupOpResponseW;
  c : TFhirCodingW;
  langList : THTTPLanguageList;
begin
  result := 'lookup code';
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.langList, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.langList), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
    begin
      if (request.id <> '') then
        raise ETerminologyError.Create('Lookup does not take an identified resource', itInvalid);
      req := ffactory.makeOpReqLookup;
      try
        if (request.Resource <> nil) and (request.Resource.fhirType = 'Parameters') then
          req.load(request.Resource)
        else
          req.load(request.Parameters);
        req.loadCoding;
        if req.displayLanguage <> '' then
          langList := THTTPLanguageList.Create(req.displayLanguage, false)
        else
          langList := request.langList.Link;
        try
          result := 'lookup code '+req.coding.renderText;

          response.Body := '';
          response.LastModifiedDate := now;
          resp := ffactory.makeOpRespLookup;
          try
            try
              FServer.lookupCode(req.coding, langList, req.propList, resp);  // currently, we ignore the date
              response.CacheControl := cacheNotAtAll;
              response.Resource := resp.asParams;
              response.HTTPCode := 200;
              response.Message := 'OK';
            except
              on e : Exception do
              begin
                response.HTTPCode := 400;
                response.Message := 'Error';
                response.Resource := FFactory.BuildOperationOutcome(request.LangList, e, itInvalid);
              end;
            end;
          finally
            resp.free;
          end;
        finally
          langList.free;
        end;
      finally
        req.free;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
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
    response.Resource := FFactory.BuildOperationOutcome(request.langList, response.Message);
  end;
end;

constructor TFhirConceptMapClosureOperation.Create(factory : TFHIRFactory; server: TTerminologyServer; languages : TIETFLanguageDefinitions);
begin
  inherited Create(factory, server, languages);
end;

function TFhirConceptMapClosureOperation.CreateDefinition(base: String): TFHIROperationDefinitionW;
begin
  result := nil;
end;

function TFhirConceptMapClosureOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse; tt : TTimeTracker) : String;
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
    response.Resource := FFactory.BuildOperationOutcome(request.langList, response.Message);
  end;
begin
  result := 'Closure';
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.langList, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.langList), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
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
            map.date := TFslDateTime.makeUTC;
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
              map.date := TFslDateTime.makeUTC;
              map.name := 'Updates for Closure Table '+n;
              if (v <> '') then
              begin
                map.name := 'Replay for Closure Table '+n+' from version '+v;
                // cm.rerun(Fconnection, map, StrToInt(v))
              end
              else
              begin
                map.name := 'Updates for Closure Table '+n;
                concepts := TFslList<TFHIRCodingW>.Create;
                try
                  for p in params.parameterList do
                    if p.Name = 'concept' then
                      concepts.Add(FFactory.wrapCoding(p.value.Link));
                  // cm.processConcepts(FConnection, concepts, map);
                finally
                  concepts.free;
                end;
              end;
            end;
          end;
        end;
      finally
        params.free;
        cm.free;
        map.free;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
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

function TFhirSubsumesOperation.Execute(context : TOperationContext; manager: TFHIROperationEngine; request: TFHIRRequest; response: TFHIRResponse; tt : TTimeTracker) : String;
var
  req : TFHIRSubsumesOpRequestW;
  resp : TFHIRSubsumesOpResponseW;
  resourceKey, versionKey : integer;
  cs : TFhirCodeSystemW;
  ca, cb : TFhirCodingW;
  cacheId : string;
  needSecure : boolean;
begin
  result := 'Subsumes';
  try
    manager.NotFound(request, response);
    if manager.check(response, manager.opAllowed(request.ResourceName, request.CommandType), 400, manager.langList, StringFormat(GetFhirMessage('MSG_OP_NOT_ALLOWED', manager.langList), [CODES_TFHIRCommandType[request.CommandType], request.ResourceName]), itForbidden) then
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
              if (request.Id = '') and (req.systemUri <> '') and (req.codeA <> '') and (req.codeB <> '') then
              begin
                if not FServer.isValidCode(req.systemUri, req.codeA) or not FServer.isValidCode(req.systemUri, req.codeB) then
                  raise ETerminologyError.Create('Invalid code', itNotFound)
                else if (req.codeA = req.codeB) then
                  resp.outcome := 'equivalent'
                else if FServer.subsumes(req.systemUri, req.codeA, req.systemUri, req.codeB) then
                  resp.outcome := 'subsumes'
                else if FServer.subsumes(req.systemUri, req.codeB, req.systemUri, req.codeA) then
                  resp.outcome := 'subsumed-by'
                else
                  resp.outcome := 'not-subsumed';
              end
              else
              begin
                // first, we have to identify the Code System
                if request.Id <> '' then // and it must exist, because of the check above
                  cs := FFactory.wrapCodeSystem(manager.GetResourceById(request, 'CodeSystem', request.Id, request.baseUrl, needSecure))
                else if req.systemUri <> '' then
                  cs := FFactory.wrapCodeSystem(manager.GetResourceByUrl('CodeSystem', req.systemUri, req.version, false, needSecure))
                else
                  raise ETerminologyError.Create('No CodeSystem Identified (need a system parameter, or execute the operation on a CodeSystem resource', itUnknown);

                cacheId := cs.url;
                ca := req.codingA;
                cb := req.codingB;
                try
                  if (ca = nil) and (req.codeA <> '') then
                    ca := FFactory.wrapCoding(FFactory.makeCoding(cs.url, req.codeA));
                  if (cb = nil) and (req.codeB <> '') then
                    cb := FFactory.wrapCoding(FFactory.makeCoding(cs.url, req.codeB));
                  if ca = nil then
                    raise ETerminologyError.Create('No codeA or codingA parameter found', itNotFound);
                  if cb = nil then
                    raise ETerminologyError.Create('No codeB or codingB parameter found', itNotFound);

                  resp.outcome := FServer.subsumes(cs, ca, cb);
                finally
                  ca.free;
                  cb.free;
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
                response.Resource := FFactory.BuildOperationOutcome(request.langList, e, itInvalid);
              end;
            end;
          finally
            resp.free;
          end;
        finally
          req.free;
        end;
      end;
    end;
    manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, response.httpCode, '', response.message, []);
  except
    on e: exception do
    begin
      manager.AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, request.ResourceName, request.id, response.versionId, 0, request.CommandType, request.Provenance, request.OperationName, 500, '', e.message, []);
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

function TFhirTerminologyOperation.processAdditionalResources(context : TOperationContext; manager: TFHIROperationEngine; mr : TFHIRMetadataResourceW; params: TFHIRParametersW): TFslMetadataResourceList;
var
  p : TFhirParametersParameterW;
  list : TFslMetadataResourceList;
  cacheId : String;
  vs : TFHIRValueSetW;
  cs : TFHIRCodeSystemW;
begin
  cacheId := '';
  list := TFslMetadataResourceList.Create;
  try
    if (mr <> nil) then
      list.Add(mr.link);
    for p in params.parameterList do
    begin
      if (p.name = 'cache-id') then
      begin
        cacheId := p.valueString;
      end;
      if (p.name = 'tx-resource') then
      begin
        if p.resource.fhirType = 'ValueSet' then
        begin
          vs := FFactory.wrapValueSet(p.resource.link);
          list.Add(vs);
          vs.TagInt := 1; // marks it as not stored
        end
        else if p.resource.fhirType = 'CodeSystem' then
        begin
          cs := FFactory.wrapCodeSystem(p.resource.link);
          list.Add(cs);
          cs.TagInt := 1; // marks it as not stored
        end;
      end;
    end;
    if cacheId = '' then
    begin
      result := list.link
    end
    else
    begin
      context.CacheResponse := false; // no point caching these, they'll never be seen again
      result := manager.clientCacheManager.processResources(cacheId, list);
    end;
  finally
    list.free;
  end;
end;

function TFhirTerminologyOperation.isValidation: boolean;
begin
  result := false;
end;

procedure TFhirTerminologyOperation.processExpansionParams(request: TFHIRRequest; manager: TFHIROperationEngine; params: TFhirParametersW; result : TFHIRExpansionParams);
var
  p : TFhirParametersParameterW;
  obj : TFHIRObject;
  pp : TFHIRParametersW;
begin
  result.generateNarrative := false; // todo...?

  if (params.str('no-cache') = 'true') then
    result.uid := NewGuidId;
  if (params.str('_incomplete') <> '') then
    result.limitedExpansion := StrToBoolDef(params.str('_incomplete'), false);
  if (params.str('limitedExpansion') <> '') then
    result.limitedExpansion := StrToBoolDef(params.str('limitedExpansion'), false);
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
  if (params.str('default-to-latest-version') <> '') then
    result.defaultToLatestVersion := StrToBoolDef(params.str('default-to-latest-version'), false);
  if (params.str('incomplete-ok') <> '') then
    result.incompleteOK := StrToBoolDef(params.str('incomplete-ok'), false);
  for p in params.parameterList do
  begin
    if (p.name = 'system-version') then
      result.seeVersionRule(p.valueString, fvmDefault)
    else if (p.name = 'check-system-version') then
      result.seeVersionRule(p.valueString, fvmCheck)
    else  if (p.name = 'force-system-version') then
      result.seeVersionRule(p.valueString, fvmOverride)
    else if (p.name = 'displayLanguage') then
      result.languages := THTTPLanguageList.create(p.valueString, not isValidation)
    else if (p.name = 'property') then
      result.properties.add(p.valueString)
    else if (p.name = 'lenient-display-validation') and (p.valueString = 'true') then
      result.displayWarning := true
    else if (p.name = 'valueset-membership-only') and (p.valueString = 'true') then
      result.membershipOnly := true
    else if (p.name = 'includeAlternateCodes') then
      result.altCodeRules.seeParam(p.valueString)
    else if (p.name = 'designation') then
      result.designations.add(p.valueString);
  end;
  if params.has('profile') then
  begin
    obj := params.obj('profile');
    if (obj <> nil) and ((obj.fhirType = 'Parameters') or (obj.fhirType = 'ExpansionProfile')) then
    begin
      pp := FFactory.wrapParams(obj.link as TFHIRResourceV);
      try
        processExpansionParams(request, manager, pp, result);
      finally
        pp.free;
      end;
    end
  end;

  if not result.hasLanguages and (request.ContentLanguage <> '') then
    result.languages := THTTPLanguageList.create(request.ContentLanguage, not isValidation);;
  if not result.hasLanguages and (request.LangList <> nil) and (request.LangList.source <> '') then
    result.languages := THTTPLanguageList.create(request.LangList.source, not isValidation);
end;

function TFhirTerminologyOperation.buildExpansionParams(request: TFHIRRequest; manager: TFHIROperationEngine; params: TFhirParametersW): TFHIRExpansionParams;
begin
  result := TFHIRExpansionParams.Create;
  try
    processExpansionParams(request, manager, params, result);
    result.link;
  finally
    result.free;
  end;
end;

constructor TFhirTerminologyOperation.Create(factory : TFHIRFactory; server: TTerminologyServer; languages : TIETFLanguageDefinitions);
begin
  inherited Create(factory, languages);
  FServer := server;
end;

destructor TFhirTerminologyOperation.Destroy;
begin
  FServer.free;
  inherited;
end;

function TFhirTerminologyOperation.loadCoded(request : TFHIRRequest; isValueSet : boolean; var issuePath : string; var mode : TValidationCheckMode): TFhirCodeableConceptW;
var
  coding : TFhirCodingW;
  params : TFhirParametersW;
begin
  // ok, now we need to find the source code to validate
  if (request.form <> nil) and request.form.hasParam('coding') then
  begin
    result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
    coding := FFactory.makeDtFromForm(request.form.getParam('coding'), request.langList, 'coding', 'Coding') as TFHIRCodingW;
    try
      result.addCoding(coding);
    finally
      coding.free;
    end;
    issuePath := 'Coding';
    mode := vcmCoding;
  end
  else if (request.form <> nil) and request.form.hasParam('codeableConcept') then
  begin
    mode := vcmCodeableConcept;
    result := FFactory.makeDtFromForm(request.form.getParam('codeableConcept'), request.langList, 'codeableConcept', 'CodeableConcept') as TFhirCodeableConceptW;
    issuePath := 'CodeableConcept';
  end
  else if request.Parameters.has('code') and (request.Parameters.has('system') or request.Parameters.has('inferSystem') or request.Parameters.has('implySystem')) then
  begin
    issuePath := '';
    mode := vcmCode;
    result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
    coding := result.addCoding;
    try
      coding.systemUri := request.Parameters['system'];
      coding.version := request.Parameters['systemVersion'];
      if (coding.version = '') then
        coding.version := request.Parameters['version'];
      coding.code := request.Parameters['code'];
      coding.display := request.Parameters['display'];
    finally
      coding.free;
    end;
  end
  else if not isValueSet and request.Parameters.has('code') and request.Parameters.has('url') then
  begin
    issuePath := '';
    mode := vcmCode;
    result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
    coding := result.addCoding;
    try
      coding.systemUri := request.Parameters['url'];
      coding.version := request.Parameters['version'];
      coding.code := request.Parameters['code'];
      coding.display := request.Parameters['display'];
    finally
      coding.free;
    end;
  end
  else if ((request.resource <> nil) and (request.Resource.fhirType = 'Parameters')) then
  begin
    params := FFactory.wrapParams(request.Resource.link);
    try
      if params.obj('coding') <> nil then
      begin
        mode := vcmCoding;
        result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
        issuePath := 'Coding';
        coding := FFactory.wrapCoding(params.obj('coding').Link);
        try
          result.addCoding(coding);
        finally
          coding.free;
        end;
      end
      else if params.has('codeableConcept') then
      begin
        mode := vcmCodeableConcept;
        result := FFactory.wrapCodeableConcept(params.obj('codeableConcept').Link);
        issuePath := 'CodeableConcept';
      end
      else if (params.has('code') and (params.has('system')) or (isValueSet and (params.has('code') and (params.bool('inferSystem') or params.bool('implySystem'))))) then
      begin
        issuePath := '';
        mode := vcmCode;
        result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
        coding := result.addCoding;
        try
          if params.has('system') then
            coding.systemUri := params.str('system');
          if params.has('systemVersion') then
            coding.version := params.str('systemVersion');
          if (coding.version = '') and params.has('version') then
            coding.version := params.str('version');
          coding.code := params.str('code');
          if params.has('display') then
            coding.display := params.str('display');
        finally
          coding.free;
        end;
      end
      else if not isValueSet and (params.has('code') and params.has('url')) then
      begin
        issuePath := '';
        mode := vcmCode;
        result := FFactory.wrapCodeableConcept(fFactory.makeByName('CodeableConcept'));
        coding := result.addCoding;
        try
          coding.systemUri := params.str('url');
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
        raise ETerminologyError.Create('Unable to find code to validate (looked for coding | codeableConcept | code in parameters ='+params.names+')', itNotFound);
    finally
      params.free;
    end;
  end
  else
    raise ETerminologyError.Create('Unable to find code to validate (looked for coding | codeableConcept | code+system in parameters ='+request.Parameters.Source+')', itNotFound);
end;

end.
