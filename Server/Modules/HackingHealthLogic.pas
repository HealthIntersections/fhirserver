unit HackingHealthLogic;

interface

uses
  FHIRResources,
  CDSHooksServer;

type
  THackingHealthBNPLogic = class (TCDSHooksProcessor)
  private
    function aTestIsBNP : boolean;
    function clinicIsED : boolean;
    function hasDyspnoea : boolean;
    function isDyspnoea(cond : TFhirCondition) : boolean;
  public
    function execute : boolean; override;
  end;

implementation

{ THackingHealthBNPLogic }

function THackingHealthBNPLogic.aTestIsBNP: boolean;
var
  res : TFHIRResource;
  req : TFhirProcedureRequest;
begin
  result := false;
  for res in request.context do
    if res is TFhirProcedureRequest then
    begin
      req := res as TFhirProcedureRequest;
      if req.code.text = 'BNP' then
        exit(true);
    end;
end;

function THackingHealthBNPLogic.clinicIsED: boolean;
var
  enc : TFhirEncounter;
begin
  if not request.preFetch.ContainsKey('encounter') then
    result := false
  else
  begin
    enc := request.preFetch['encounter'].resource as TFhirEncounter;
    result := (enc.class_ <> nil) and (enc.class_.code = 'emergency');
  end;
end;

function THackingHealthBNPLogic.execute: boolean;
begin
  result := false;
  if (aTestIsBNP) then
  begin
    if not clinicIsED then
      addCard('BNP will not be paid by Medicare because this is not an emergency presentation', '', 'warning', 'MBS rules', '')
    else if not hasDyspnoea then
      addCard('BNP will not be paid by Medicare if the patient does not have Dyspnoea', '', 'warning', 'MBS rules', '');
  end;
end;

function THackingHealthBNPLogic.hasDyspnoea: boolean;
var
  bnd : TFhirBundle;
  be : TFhirBundleEntry;
begin
  bnd := request.preFetch['problems'].resource as TFhirBundle;
  result := false;
  for be in bnd.entryList do
    if (be.resource <> nil) and (be.resource is TFhirCondition) then
      if isDyspnoea(be.resource as TFhirCondition) then
        exit(true);
end;

function THackingHealthBNPLogic.isDyspnoea(cond: TFhirCondition): boolean;
begin
  result := cond.code.text = 'Dyspnoea';
end;

end.
