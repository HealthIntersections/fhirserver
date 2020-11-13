unit cds_hooks_service;

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
  SysUtils, Classes,
  IdContext,
  fsl_base, fsl_json,
   fhir_common,
  session,
  fhir_cdshooks,
  cds_hooks_server, server_context, storage;

type
(*  TCDAHooksCodeViewService = class (TCDSHooksService)
  public
    function hook : string; override;
    function name : String; override;
    function description : String; override;

    function HandleRequest(server: TFHIRServerContext; secure : boolean; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest) : TCDSHookResponse; override;
  end;
*)
  TCDAHooksIdentifierViewService = class (TCDSHooksService)
  private
//    procedure addNamingSystemInfo(ns: TFHIRNamingSystemW; baseURL : String; resp: TCDSHookResponse);
//    procedure addSystemCard(resp: TCDSHookResponse; name, publisher, responsible, type_, usage, realm: String);
  public
    function hook : string; override;
    function name : String; override;
    function description : String; override;

    function HandleRequest(server: TFHIRServerContext; secure : boolean; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest) : TCDSHookResponse; override;
  end;
(*
  TCDAHooksPatientViewService = class (TCDSHooksService)
  private
    function identifyPatient(engine : TFHIROperationEngine; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest; var needSecure : boolean) : TFHIRPatientW;
    function buildPatientView(server: TFHIRServerContext; engine : TFHIROperationEngine; base : String; secure : boolean; patient : TFHIRPatientW; session : TFHIRSession) : TCDSHookResponse;
  public
    function hook : string; override;
    function name : String; override;
    function description : String; override;

    function HandleRequest(server: TFHIRServerContext; secure : boolean; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest) : TCDSHookResponse; override;
  end;

  TCDAHackingHealthOrderService = class (TCDSHooksService)
  private
    function check(issues : TStringList; condition : boolean; message : String) : boolean;
  public
    constructor Create; override;
    function hook : string; override;
    function name : String; override;
    function id : String; override;
    function description : String; override;
    procedure registerPreFetch(json : TJsonObject); override;

    function HandleRequest(server: TFHIRServerContext; secure : boolean; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest) : TCDSHookResponse; override;
  end;
*)


implementation

//uses
//  FHIR.Server.HackingHealth;
(*
{ TCDAHooksCodeViewService }

function TCDAHooksCodeViewService.name: String;
begin
  result := 'code-view';
end;

function TCDAHooksCodeViewService.description: String;
begin
  result := 'View details about a code';
end;

function TCDAHooksCodeViewService.HandleRequest(server: TFHIRServerContext; secure: boolean; session: TFHIRSession; context: TIdContext; request: TCDSHookRequest): TCDSHookResponse;
var
  params: TFHIRParametersW;
  code : TFhirObject;
  card : TCDSHookCard;
begin
  require((request.context.Count = 1) and (request.context[0] is TFhirParameters), 'Must have a single parameters resource as the context');
  params := request.context[0] as TFhirParameters;
  code := params.NamedParameter['code'] as TFHIRType;
  require(code <> nil, 'No "code" parameter found');
  require((code is TFHIRCoding) or (code is TFHIRCodeableConcept), '"code" parameter has wrong type '+code.fhirType);
  result := TCDSHookResponse.Create;
  try
    if code is TFhirCoding then
      server.TerminologyServer.getCodeView(request.Lang, code as TFHIRCoding, result)
    else
      server.TerminologyServer.getCodeView(request.Lang, code as TFHIRCodeableConcept, result);
    for card in result.cards do
    begin
      card.sourceLabel := server.OwnerName;
      card.sourceURL := request.baseUrl;
      card.indicator := 'info';
    end;
    result.Link;
  finally
    result.Free;
  end;
end;


function TCDAHooksCodeViewService.hook: string;
begin
  result := 'code-view';
end;

{ TCDAHackingHealthOrderService }

function TCDAHackingHealthOrderService.check(issues: TStringList; condition: boolean; message: String): boolean;
begin
  if not condition then
    issues.Add(message);
  result := condition;
end;

constructor TCDAHackingHealthOrderService.Create;
begin
  inherited;
  !{$IFNDEF FHIR2}
  FEngines.add(THackingHealthBNPLogic);
  {$ENDIF}
end;

function TCDAHackingHealthOrderService.description: String;
begin
  result := 'Implements the Hacking Health interface. This simply inspects the call and returns a card that evaluates the completeness of the information provided';
end;

function TCDAHackingHealthOrderService.hook: string;
begin
  result := 'order-review';
end;

function TCDAHackingHealthOrderService.id: String;
begin
  result := 'hacking-health';
end;

function TCDAHackingHealthOrderService.name: String;
begin
  result := 'Hacking Health';
end;

procedure TCDAHackingHealthOrderService.registerPreFetch(json: TJsonObject);
var
  pf : TJsonObject;
begin
  pf := json.forceObj['prefetch'];
  pf.str['patient'] := 'Patient/{{Patient.id}}';
  pf.str['encounter'] := 'Encounter/{{Encounter.id}}';
  pf.str['problems'] := 'Condition?patient={{Patient.id}}&_list=$current-problems';
end;

function TCDAHackingHealthOrderService.HandleRequest(server: TFHIRServerContext; secure: boolean; session: TFHIRSession; context: TIdContext; request: TCDSHookRequest): TCDSHookResponse;
var
  issues : TStringList;
  res : TFHIRResource;
  pr : TFhirProcedureRequest;
  be : TFhirBundleEntry;
  c : TFhirCondition;
  card : TCDSHookCard;
  b : TStringBuilder;
  s : String;
  e : TFhirEncounter;
begin
  issues := TStringList.create;
  try
    check(issues, request.hook = Self.hook, 'hook must be '+self.hook+' but is '+request.hook);
    check(issues, request.hookInstance <> '', 'hookInstance must be provided');
    check(issues, request.fhirServer = '', 'fhirServer must be blank (no call back to EHR for Hacking Health)');
    check(issues, request.oauth = nil, 'oAuth should not be present (no call back to EHR for Hacking Health)');
    check(issues, request.redirect = '', 'redirect must be blank (no call back to EHR for Hacking Health)');
    check(issues, request.user <> '', 'user must not be blank (want user id - for consistency?)');
    check(issues, request.patient <> '', 'patient is required');
    check(issues, request.encounter <> '', 'encounter is required');
    check(issues, request.context.Count > 0, 'at least one resource is required in context');
  raise EFHIRException.create('to do');

(*
!{$IFNDEF FHIR2}
   for res in request.context do
    begin
      if check(issues, res.ResourceType = frtProcedureRequest, 'Context resources must be a ProcedureRequest') then
      begin
        pr := res as TFhirProcedureRequest;
        if check(issues, (pr.subject <> nil) and (pr.subject.reference <> ''), 'ProcedureRequest must have a subject') then
          check(issues, pr.subject.reference = 'Patient/'+request.patient, 'ProcedureRequest subject ('+pr.subject.reference+') must match request.subject ('+request.patient+')');
        if check(issues, (pr.context <> nil) and (pr.context.reference <> ''), 'ProcedureRequest must have a context') then
          check(issues, pr.context.reference = 'Encounter/'+request.encounter, 'ProcedureRequest context ('+pr.context.reference+') must match request.encounter ('+request.encounter+')');
        check(issues, pr.code <> nil, 'ProcedureRequest must have a code');
      end;
    end;

    if check(issues, request.prefetch.ContainsKey('patient'), 'A patient must be present in the prefetch data') then
    begin
      if check(issues, request.preFetch['patient'].resource <> nil, 'Patient resource must be present on the entry') then
      begin
        check(issues, request.preFetch['patient'].resource.ResourceType = frtPatient, 'Patient resource must be a patient');
        check(issues, request.patient = request.preFetch['patient'].resource.id, 'Patient resource id ('+request.patient+') must match patient in request ('+request.preFetch['patient'].resource.id+')');
      end;
    end;
    if check(issues, request.prefetch.ContainsKey('encounter'), 'An encounter must be present in the prefetch data') then
    begin
      if check(issues, request.preFetch['encounter'].resource <> nil, 'Patient Encounter resource must be present on the entry') then
      begin
        if check(issues, request.preFetch['encounter'].resource.ResourceType = frtEncounter, 'Patient encounter must be an encounter') then
        begin
          check(issues, request.encounter = request.preFetch['encounter'].resource.id, 'Encounter resource id ('+request.encounter+') must match encounter in request ('+request.preFetch['encounter'].resource.id+')');
          e := TFhirEncounter(request.preFetch['encounter'].resource);
          if check(issues, (e.subject <> nil) and (e.subject.reference <> ''), 'Encounter must have a subject') then
            check(issues, e.subject.reference = 'Patient/'+request.patient, 'Encounter subject ('+e.subject.reference+') must match request.subject ('+request.patient+')');
        end;
      end;
    end;
    if check(issues, request.prefetch.ContainsKey('problems'), 'A problems list must be present in the prefetch data') then
      if check(issues, request.preFetch['problems'].resource <> nil, 'Problems List must have a resource in the entry') then
        if check(issues, request.preFetch['problems'].resource.ResourceType = frtBundle, 'Problems List must be a bundle') then
          for be in TFHIRBundle(request.preFetch['problems'].resource).entryList do
            if check(issues, be.resource <> nil, 'Problem List Bundle Entries must have a resource') and
              check(issues, be.resource is TFhirCondition, 'problems must be a condition') then
            begin
              c := be.resource as TFhirCondition;
              if check(issues, (c.subject <> nil) and (c.subject.reference <> ''), 'Condition must have a subject') then
                check(issues, c.subject.reference = 'Patient/'+request.patient, 'Condition subject ('+c.subject.reference+') must match request.subject ('+request.patient+')');
              check(issues, c.code <> nil, 'Condition must have a code');
            end;
    {$ENDIF}
    result := TCDSHookResponse.Create;
    card := Result.addCard;
    if issues.Count = 0 then
    begin
      card.summary := 'order-review request is ok';
      card.detail := 'All tests passed';
      card.indicator := 'success';
      ProcessRequestEngines(server, secure, session, context, request, result);
    end
    else
    begin
      card.summary := 'order-review request: '+inttostr(issues.Count)+' issues found';
      b := TStringBuilder.Create;
      try
        for s in issues do
          b.Append('* '+s+#13#10);
        card.detail := b.ToString;
      finally
        b.Free;
      end;
      card.indicator := 'warning';
    end; *.)
  finally
    issues.Free;
  end;
end;
  *)
{ TCDAHooksIdentifierViewService }

function TCDAHooksIdentifierViewService.description: String;
begin
  result := 'View details about an identifier';
end;

//procedure TCDAHooksIdentifierViewService.addSystemCard(resp: TCDSHookResponse; name, publisher, responsible, type_, usage, realm : String);
//var
//  card : TCDSHookCard;
//  b : TStringBuilder;
//begin
//  card := resp.addCard;
//  b := TStringBuilder.Create;
//  try
//    b.append('* Identifier System Name: '+name+#13#10);
//    if publisher <> '' then
//      b.append('* Publisher: '+publisher+#13#10);
//    if responsible <> '' then
//      b.append('* Responsible: '+responsible+#13#10);
//    if type_ <> '' then
//      b.append('* Type: '+type_+#13#10);
//    if usage <> '' then
//      b.append('* Usage Notes: '+usage+#13#10);
//
//    b.append(#13#10);
//
//    if realm > '' then
//    begin
//      b.Append('Contexts of Use'#13#10#13#10);
//      b.Append('* '+realm+#13#10);
//      b.append(#13#10);
//    end;
//
//    card.detail := b.ToString;
//  finally
//    b.Free;
//  end;
//
//end;
//
//procedure TCDAHooksIdentifierViewService.addNamingSystemInfo(ns: TFHIRNamingSystemW; baseURL : String; resp: TCDSHookResponse);
//var
//  card : TCDSHookCard;
//  b : TStringBuilder;
////  cp : TFhirNamingSystemContact;
////  !{$IFNDEF FHIR2}
////  uc : TFhirUsageContext;
////  {$ENDIF}
////  cc : TFhirCodeableConcept;
//begin
//  card := resp.addCard;
//  card.addLink('Further Detail', baseURL+'/open/NamingSystem/'+ns.id);
//  b := TStringBuilder.Create;
//  try
//    b.Append('todo');
//(*    b.append('* Identifier System Name: '+ns.name+#13#10);
//    if ns.publisher <> '' then
//      b.append('* Publisher: '+ns.publisher+#13#10);
//    if ns.responsible <> '' then
//      b.append('* Responsible: '+ns.responsible+#13#10);
//    if ns.type_ <> nil then
//      b.append('* Type: '+gen(ns.type_)+#13#10);
//    if ns.usage <> '' then
//      b.append('* Usage Notes: '+ns.usage+#13#10);
//
//    b.append(#13#10);
//
//    if (ns.useContextList.Count > 0) !{$IFNDEF FHIR2}or (ns.jurisdictionList.Count > 0){$ENDIF} then
//    begin
//      b.Append('Contexts of Use'#13#10#13#10);
//      !{$IFNDEF FHIR2}
//      for uc in ns.useContextList do
//        b.Append('* '+gen(uc.code)+':'+gen(uc.value)+#13#10);
//      for cc in ns.jurisdictionList do
//        b.Append('* Jurisdiction: '+gen(cc)+#13#10);
//      {$ELSE}
//      for cc in ns.useContextList do
//        b.Append('* '+gen(cc)+#13#10);
//      {$ENDIF}
//      b.append(#13#10);
//    end;
//
//    if ns.contactList.Count > 0 then
//    begin
//      b.Append('Contacts'#13#10#13#10);
//      for cp in ns.contactList do
//        b.Append('* '+cp.name+#13#10);
//      b.append(#13#10);
//    end;                       *)
//
//    card.detail := b.ToString;
//  finally
//    b.Free;
//  end;
//end;

function TCDAHooksIdentifierViewService.HandleRequest(server: TFHIRServerContext; secure: boolean; session: TFHIRSession; context: TIdContext; request: TCDSHookRequest): TCDSHookResponse;
var
  params: TFHIRParametersW;
//  id : TFhirIdentifier;
//  systems : TFslList<TFHIRResource>;
  card : TCDSHookCard;
//  engine : TFHIROperationEngine;
//  needSecure: boolean;
//  r : TFhirResource;
begin
  require((request.context.Count = 1) and (request.context[0].fhirType = 'Parameters'), 'Must have a single parameters resource as the context');
  params := server.Factory.wrapParams(request.context[0].link);
  try
//    require(params.param('identifier') <> nil, 'No "identifier" parameter found');
//    require((params.param('identifier').fhirType = 'Identifier', '"identifier" parameter has wrong type '+params.param('identifier').fhirType);
//{    id := params.NamedParameter['identifier'] as TFHIRIdentifier;
    result := TCDSHookResponse.Create;
    try
//      if (id.type_ <> nil) then
//        server.TerminologyServer.getCodeView(request.Lang, id.type_, result);
//
//      if (id.system <> '') then
//      begin
//        engine := server.Storage.createOperationContext(request.lang);
//        try
//          systems := engine.getResourcesByParam(frtNamingSystem, 'value', id.system, needSecure);
//          try
//            for r in systems do
//              addNamingSystemInfo(r as TFHIRNamingSystem, request.baseUrl, result);
//          finally
//            systems.Free;
//          end;
//
//        finally
//          server.Storage.Yield(engine, nil);
//        end;
//      end;
//      }
//      if (id.system = 'urn:ietf:rfc:3986') then
//        addSystemCard(result, 'URI', '', 'W3C', '(any)', 'For when the identifier is any valid URI', '');

      for card in result.cards do
      begin
        card.sourceLabel := server.Globals.OwnerName;
        card.sourceURL := request.baseUrl;
        card.indicator := 'info';
      end;
      result.Link;
    finally
      result.Free;
    end;
  finally
    params.free;
  end;
end;

function TCDAHooksIdentifierViewService.hook: string;
begin
  result := 'identifier-view';
end;

function TCDAHooksIdentifierViewService.name: String;
begin
  result := 'identifier-view';
end;

(*
{ TCDAHooksPatientViewService }

function TCDAHooksPatientViewService.description: String;
begin
  result := 'return details about the patient';
end;

function TCDAHooksPatientViewService.hook: string;
begin
  result := 'patient-view';
end;

function TCDAHooksPatientViewService.name: String;
begin
  result := 'patient-view';
end;

function TCDAHooksPatientViewService.HandleRequest(server: TFHIRServerContext; secure: boolean; session: TFHIRSession; context: TIdContext; request: TCDSHookRequest): TCDSHookResponse;
var
  engine : TFHIROperationEngine;
  patient : TFHIRPatient;
  needSecure : boolean;
begin
  engine := server.Storage.createOperationContext(request.lang);
  try
    patient := identifyPatient(engine, session, context, request, needSecure);
    try
      if (patient <> nil) and (secure or not needSecure) then
        result := buildPatientView(server, engine, request.baseURL, secure, patient, session)
      else
        result := nil;
    finally
      patient.Free;
    end;
  finally
    server.Storage.Yield(engine, nil);
  end;
end;

function TCDAHooksPatientViewService.identifyPatient( engine: TFHIROperationEngine; session: TFHIRSession; context: TIdContext; request: TCDSHookRequest; var needSecure: boolean): TFHIRPatient;
var
  key, versionKey : integer;
  be1 : TFhirBundleEntry;
  res : TFHIRResource;
  pat : TFhirPatient;
  be : TFhirBundleEntry;
  matches, m : TMatchingResourceList;
  id : TFhirIdentifier;
  ok : boolean;
begin
  raise EFHIRException.create('to do');

(*  result := nil;
  // do we know that patient?
  if Session = nil then
    ok := engine.FindResource('Patient', request.patient, [], key, versionKey, nil,  nil, nil)
  else
    ok := engine.FindResource('Patient', request.patient, [], key, versionKey, nil,  nil, session.Compartments);

  if ok then
    result := engine.GetResourceByKey(key, needSecure) as TFhirPatient
  else
  begin
    // is a matching resource located amongst the pre-fetch?
    pat := nil;
    for be1 in request.preFetch.Values do
    begin
      res := be1.resource;
      if (res.fhirType = 'Patient') and (res.id = request.patient) then
        pat := res as TFhirPatient
      else if res is TFhirBundle then
        for be in TFhirBundle(res).entryList do
          if (be.resource <> nil) and (be.resource.fhirType = 'Patient') and (be.resource.id = request.patient) then
            pat := be.resource as TFhirPatient;
    end;
    if pat <> nil then
    begin
      matches := TMatchingResourceList.create;
      try
        for id in pat.identifierList do
        begin
          m := engine.ResolveSearchId('Patient', nil, session.Compartments, request.baseURL, 'identifier='+id.system+'|'+id.value);
          try
            matches.AddAll(m);
          finally
            m.Free;
          end;
        end;
        if matches.Count = 1 then
          result := engine.GetResourceByKey(matches[0].key, needSecure) as TFhirPatient;
      finally
        matches.Free;
      end;
    end;
  end; *.)
end;

function TCDAHooksPatientViewService.buildPatientView(server: TFHIRServerContext; engine: TFHIROperationEngine; base : String; secure : boolean; patient: TFHIRPatient; session : TFHIRSession): TCDSHookResponse;
var
  m : TMatchingResourceList;
  i : integer;
  flag : TFhirFlag;
  needSecure : boolean;
  card : TCDSHookCard;
  comp : TFHIRCompartmentId;
begin
  result := TCDSHookResponse.Create;
  try
    comp := TFHIRCompartmentId.Create(frtPatient, patient.id);
    try
      if session = nil then
        m := engine.ResolveSearchId('Flag', comp, nil, base, 'active=true')
      else
        m := engine.ResolveSearchId('Flag', comp, session.Compartments, base, 'active=true');
      try
        for i := 0 to m.Count - 1 do
        begin
          flag := engine.GetResourceByKey(m[i].key, needSecure) as TFhirFlag;
          if (flag.status = FlagStatusActive) and (secure or not needSecure) then
          begin
            card := result.addCard;
            card.indicator := 'info';
            if flag.author <> nil then
            begin
              card.sourceLabel := flag.author.display;
              card.sourceURL := flag.author.reference;
            end;
            if card.sourceLabel = '' then
              card.sourceLabel := server.OwnerName;
            if card.sourceURL = '' then
              card.sourceURL := base;
            if flag.code.text <> '' then
              card.summary := flag.code.text
            else if flag.code.codingList.Count > 0 then
              card.summary := flag.code.codingList[0].display
          end;
        end;
      finally
        m.Free;
      end;
    finally
      comp.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

*)

end.

