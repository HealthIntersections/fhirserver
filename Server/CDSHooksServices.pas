unit CDSHooksServices;

interface

uses
  SysUtils, Classes,
  IdContext,
  FHIRSupport, FHIRTypes, FHIRResources, FHIRUtilities,
  CDSHooksUtilities,
  CDSHooksServer;

type
  TCDAHooksConceptService = class (TCDSHooksService)
  public
    function hook : string; override;
    function name : String; override;
    function description : String; override;

    function HandleRequest(secure : boolean; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest) : TCDSHookResponse; override;
  end;

  TCDAHackingHealthOrderService = class (TCDSHooksService)
  private
    function check(issues : TStringList; condition : boolean; message : String) : boolean;
  public
    function hook : string; override;
    function name : String; override;
    function id : String; override;
    function description : String; override;

    function HandleRequest(secure : boolean; session : TFHIRSession; context: TIdContext; request: TCDSHookRequest) : TCDSHookResponse; override;
  end;

implementation

{ TCDAHooksConceptService }

function TCDAHooksConceptService.name: String;
begin
  result := 'code-view';
end;

function TCDAHooksConceptService.description: String;
begin
  result := 'View details about a code';
end;

function TCDAHooksConceptService.HandleRequest(secure: boolean; session: TFHIRSession; context: TIdContext; request: TCDSHookRequest): TCDSHookResponse;
var
  params: TFHIRParameters;
  p : TFhirType;
begin
  require((request.context.Count = 1) and (request.context[0] is TFhirParameters), 'Must have a single parameters resource as the context');
  params := request.context[0] as TFhirParameters;
  p := params.NamedParameter['code'] as TFHIRType;
  require(p <> nil, 'No "code" parameter found');
  result := TCDSHookResponse.Create;
  result.addCard.summary := 'Found Code, but code-view is not implemented yet';
end;

function TCDAHooksConceptService.hook: string;
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

function TCDAHackingHealthOrderService.HandleRequest(secure: boolean; session: TFHIRSession; context: TIdContext; request: TCDSHookRequest): TCDSHookResponse;
var
  issues : TStringList;
  res : TFHIRResource;
  pr : TFhirProcedureRequest;
  be : TFhirBundleEntry;
  pat, epe, cnd : boolean;
  c : TFhirCondition;
  card : TCDSHookCard;
  b : TStringBuilder;
  s : String;
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
    {$IFNDEF FHIR2}
    for res in request.context do
    begin
      if check(issues, res.ResourceType = frtProcedureRequest, 'Context resources must be a ProcedureRequest') then
      begin
        pr := res as TFhirProcedureRequest;
        if check(issues, (pr.subject <> nil) and (pr.subject.reference <> ''), 'ProcedureRequest must have a subject') then
          check(issues, pr.subject.reference = request.patient, 'ProcedureRequest subject must match request.subject');
        if check(issues, (pr.context <> nil) and (pr.context.reference <> ''), 'ProcedureRequest must have a context') then
          check(issues, pr.context.reference = request.encounter, 'ProcedureRequest context must match request.encounter');
        check(issues, pr.code <> nil, 'ProcedureRequest must have a code');
      end;
    end;
    if (check(issues, request.preFetchData <> nil, 'Prefetch data must be provided')) then
    begin
      pat := false;
      epe := false;
      cnd := false;
      for be in request.preFetchData.entryList do
        if check(issues, be.resource <> nil, 'Bundle Entries must have a resource') then
        begin
          if be.resource is TFhirPatient then
          begin
            check(issues, not pat, 'Patient can only appeaar once in pre-fetch data');
            pat := true;
            check(issues, request.patient = 'Patient/'+TFHIRPatient(be.resource).id, 'Patient resource id must match patient in request');
          end;
          if be.resource is TFhirEncounter then
          begin
            check(issues, not epe, 'Encounter can only appeaar once in pre-fetch data');
            epe := true;
            check(issues, request.encounter = 'Encounter/'+TFHIRPatient(be.resource).id, 'Encounter resource id must match encounter in request');
          end;
          if be.resource is TFhirCondition then
          begin
            cnd := true;
            c := be.resource as TFhirCondition;
            if check(issues, (c.subject <> nil) and (c.subject.reference <> ''), 'Condition must have a subject') then
              check(issues, c.subject.reference = request.patient, 'Condition subject must match request.subject');
            check(issues, c.code <> nil, 'Condition must have a code');
          end;
        end;
      check(issues, pat, 'Patient information is required in pre-fetch data');
      check(issues, epe, 'Encounter information is required in pre-fetch data');
      check(issues, cnd, 'At least one Condition is required in pre-fetch data (problem list)');
    end;
    {$ENDIF}
    result := TCDSHookResponse.Create;
    card := Result.addCard;
    if issues.Count = 0 then
    begin
      card.summary := 'order-review request is ok';
      card.detail := 'All tests passed';
      card.indicator := 'success';
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
    end;
  finally
    issues.Free;
  end;
end;

end.
