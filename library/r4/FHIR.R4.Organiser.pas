unit FHIR.R4.Organiser;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  SysUtils,
  FHIR.Support.Base,
  FHIR.R4.Resources, FHIR.R4.Resources.Base;

type
  TFHIRResourceOrganiser = class (TFslObject)
  private
    procedure organiseCS(res : TFhirCapabilityStatement);
    function sortCSRest(pA, pB: Pointer): Integer;
    function sortCSRInteraction(pA, pB: Pointer): Integer;
  public
    function canOrganise(res : TFhirResourceType) : boolean;
    procedure organise(res : TFhirResource);
  end;

implementation

{ TFHIRResourceOrganiser }

function TFHIRResourceOrganiser.canOrganise(res: TFhirResourceType): boolean;
begin
  result := res in [frtCapabilityStatement];
end;

procedure TFHIRResourceOrganiser.organise(res: TFhirResource);
begin
  case res.ResourceType of
    frtCapabilityStatement: organiseCS(res as TFhirCapabilityStatement);
  else
    // nothing to do;
  end;
end;

Function TFHIRResourceOrganiser.sortCSRInteraction(pA, pB : Pointer) : Integer;
var
  rA, rB : TFhirCapabilityStatementRestResourceInteraction;
begin
  rA := TFhirCapabilityStatementRestResourceInteraction(pA);
  rB := TFhirCapabilityStatementRestResourceInteraction(pB);
  result := ord(rA.code) - ord(rb.code);
end;

Function TFHIRResourceOrganiser.sortCSRest(pA, pB : Pointer) : Integer;
var
  rA, rB : TFhirCapabilityStatementRestResource;
begin
  rA := TFhirCapabilityStatementRestResource(pA);
  rB := TFhirCapabilityStatementRestResource(pB);
  result := ord(rA.type_) - ord(rb.type_);
end;

procedure TFHIRResourceOrganiser.organiseCS(res: TFhirCapabilityStatement);
var
  csr : TFhirCapabilityStatementRest;
  csrr : TFhirCapabilityStatementRestResource;
begin
  for csr in res.restList do
  begin
    csr.resourceList.SortedBy(sortCSRest);
    for csrr in csr.resourceList do
    begin
      csrr.interactionList.SortedBy(sortCSRInteraction);
    end;
  end;
end;

end.

