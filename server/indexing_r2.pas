unit indexing_r2;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{
outstanding search issues:
* text searching

combinations to enable:
  name[family eq x and given eq y]


// todo: bundle is special...

}
uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_xml, fsl_http,
  fhir_objects, fhir_xhtml, fhir_common,  fhir_utilities, fhir_pathengine,
  fhir2_types, fhir2_constants, fhir2_indexinfo, fhir2_utilities, fhir2_pathengine, fhir2_context,
  fhir2_resources_base, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_canonical, fhir2_resources_other,
  fhir_indexing,
  ftx_ucum_services,
  session, indexing, tags, utilities, server_constants;

Type
  {$HINTS OFF}
  TFhirIndexManager2 = class (TFhirIndexManager)
  private
    FMasterKey : Integer;
    FforTesting : boolean;

    procedure GetBoundaries(value : String; comparator: TFhirQuantityComparatorEnum; var low, high : String);

    function EncodeXhtml(r : TFhirDomainResource) : TBytes;
    procedure recordSpace(space : string; key : integer);
    function TypeForKey(key : integer) : string;

              // addToCompartment
    procedure patientCompartment(key : integer; reference : TFhirReference); overload;
    procedure patientCompartmentNot(key : integer; type_, id : String); overload;
    procedure patientCompartment(key : integer; type_, id : String); overload;

    // very primitives
    procedure index(aType : String; key, parent : integer; value1, value2, name : String); overload;
    procedure index(aType : String; key, parent : integer; value, name : String); overload;
    procedure index2(aType : String; key, parent : integer; value, name : String); overload;

    // primitives
    procedure index(aType : String; key, parent : integer; value : Boolean; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirString; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirUri; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirEnum; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirInteger; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirDecimal; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirBoolean; name : String); overload;

    // intervals of time
    procedure index(aType : String; key, parent : integer; min, max : TDateTime; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirInstant; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirDateTime; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirDate; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirPeriod; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirTiming; name : String); overload;

    // complexes
    procedure index(aType : String; key, parent : integer; value : TFhirRatio; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirQuantity; name : String; units : string = ''); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirRange; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirSampledData; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirCoding; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirCodingList; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirCodeableConcept; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirCodeableConceptList; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirIdentifier; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirIdentifierList; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirHumanName; name, phoneticName : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirAddress; name : String); overload;
    procedure index(aType : String; key, parent : integer; value : TFhirContactPoint; name : String); overload;
    procedure index(context : TFhirResource; aType : String; key, parent : integer; value : TFhirReference; name : String; specificType : String = ''); overload;
    procedure index(context : TFhirResource; aType : String; key, parent : integer; value : TFhirReferenceList; name : String; specificType : String = ''); overload;

    // structure holder
    function index(aType : String; key, parent : integer; name : String) : Integer; overload;

    { resource functionality }
    procedure buildIndexValues(key : integer; id : String; context, resource : TFhirResource);
    procedure buildIndexValuesBundle(key : integer; id : string; context : TFhirResource; resource : TFhirBundle);
    procedure buildIndexValuesFlag(key : integer; id : string; context : TFhirResource; resource : TFhirFlag);
    procedure buildIndexValuesFamilyMemberHistory(key : integer; id : string; context : TFhirResource; resource : TFhirFamilyMemberHistory);
    procedure BuildIndexValuesStructureDefinition(key : integer; id : string; context : TFhirResource; resource : TFHirStructureDefinition);
    procedure BuildIndexValuesAuditEvent(key : integer; id : string; context : TFhirResource; resource : TFhirAuditEvent);
    procedure buildIndexValuesAllergyIntolerance(key : integer; id : string; context : TFhirResource; resource : TFhirAllergyIntolerance);
    procedure buildIndexValuesBinary(key : integer; id : string; context : TFhirResource; resource : TFhirBinary);
    procedure BuildIndexValuesCarePlan(key : integer; id : string; context : TFhirResource; resource : TFhirCarePlan);
    procedure BuildIndexValuesCondition(key : integer; id : string; context : TFhirResource; resource : TFhirCondition);
    procedure BuildIndexValuesConformance(key : integer; id : string; context : TFhirResource; resource : TFhirConformance);
    procedure BuildIndexValuesDevice(key : integer; id : string; context : TFhirResource; resource : TFhirDevice);
    procedure BuildIndexValuesDiagnosticOrder(key : integer; id : string; context : TFhirResource; resource : TFhirDiagnosticOrder);
    procedure BuildIndexValuesDiagnosticReport(key : integer; id : string; context : TFhirResource; resource : TFhirDiagnosticReport);
    procedure BuildIndexValuesComposition(key : integer; id : string; context : TFhirResource; resource : TFhirComposition);
    procedure BuildIndexValuesDocumentReference(key : integer; id : string; context : TFhirResource; resource : TFhirDocumentReference);
    procedure BuildIndexValuesDocumentManifest(key : integer; id : string; context : TFhirResource; resource : TFhirDocumentManifest);
    procedure BuildIndexValuesEncounter(key : integer; id : string; context : TFhirResource; resource : TFhirEncounter);
    procedure BuildIndexValuesGroup(key : integer; id : string; context : TFhirResource; resource : TFhirGroup);
    procedure BuildIndexValuesImagingStudy(key : integer; id : string; context : TFhirResource; resource : TFhirImagingStudy);
    procedure BuildIndexValuesImmunization(key : integer; id : string; context : TFhirResource; resource : TFhirImmunization);
    procedure buildIndexValuesImmunizationRecommendation(key : integer; id : string; context : TFhirResource; resource : TFhirImmunizationRecommendation);
    procedure BuildIndexValuesList(key : integer; id : string; context : TFhirResource; resource : TFhirList);
    procedure BuildIndexValuesLocation(key : integer; id : string; context : TFhirResource; resource : TFhirLocation);
    procedure BuildIndexValuesMedia(key : integer; id : string; context : TFhirResource; resource : TFhirMedia);
    procedure BuildIndexValuesMedication(key : integer; id : string; context : TFhirResource; resource : TFhirMedication);
    procedure BuildIndexValuesMedicationAdministration(key : integer; id : string; context : TFhirResource; resource : TFhirMedicationAdministration);
    procedure BuildIndexValuesMedicationDispense(key : integer; id : string; context : TFhirResource; resource : TFhirMedicationDispense);
    procedure BuildIndexValuesMedicationOrder(key : integer; id : string; context : TFhirResource; resource : TFhirMedicationOrder);
    procedure BuildIndexValuesMedicationStatement(key : integer; id : string; context : TFhirResource; resource : TFhirMedicationStatement);
    procedure BuildIndexValuesMessageHeader(key : integer; id : string; context : TFhirResource; resource : TFhirMessageHeader);
    procedure BuildIndexValuesObservation(key : integer; id : string; context : TFhirResource; resource : TFhirObservation);
    procedure BuildIndexValuesOperationOutcome(key : integer; id : string; context : TFhirResource; resource : TFhirOperationOutcome);
    procedure BuildIndexValuesOrder(key : integer; id : string; context : TFhirResource; resource : TFhirOrder);
    procedure BuildIndexValuesOrderResponse(key : integer; id : string; context : TFhirResource; resource : TFhirOrderResponse);
    procedure BuildIndexValuesOrganization(key : integer; id : string; context : TFhirResource; resource : TFhirOrganization);
    procedure BuildIndexValuesPatient(key : integer; id : string; context : TFhirResource; resource : TFhirPatient);
    procedure BuildIndexValuesParameters(key : integer; id : string; context : TFhirResource; resource : TFhirParameters);
    procedure BuildIndexValuesPractitioner(key : integer; id : string; context : TFhirResource; resource : TFhirPractitioner);
    procedure buildIndexValuesProcedure(key : integer; id : string; context : TFhirResource; resource : TFhirProcedure);
    procedure BuildIndexValuesProvenance(key : integer; id : string; context : TFhirResource; resource : TFhirProvenance);
    procedure BuildIndexValuesQuestionnaire(key : integer; id : string; context : TFhirResource; resource : TFhirQuestionnaire);
    procedure buildIndexValuesSpecimen(key : integer; id : string; context : TFhirResource; resource : TFhirSpecimen);
    procedure buildIndexValuesSubstance(key : integer; id : string; context : TFhirResource; resource : TFhirSubstance);
    procedure BuildIndexValuesValueSet(key : integer; id : string; context : TFhirResource; resource : TFhirValueSet);
    procedure BuildIndexValuesConceptMap(key : integer; id : string; context : TFhirResource; resource : TFhirConceptMap);
    procedure BuildIndexValuesRelatedPerson(key : integer; id : string; context : TFhirResource; resource : TFhirRelatedPerson);
    procedure BuildIndexValuesSupplyDelivery(key : integer; id : string; context : TFhirResource; resource : TFhirSupplyDelivery);
    procedure BuildIndexValuesSupplyRequest(key : integer; id : string; context : TFhirResource; resource : TFhirSupplyRequest);
    procedure BuildIndexValuesBasic(key : integer; id : string; context : TFhirResource; resource : TFhirBasic);
    procedure BuildIndexValuesQuestionnaireResponse(key : integer; id : string; context : TFhirResource; resource : TFhirQuestionnaireResponse);
    procedure BuildIndexValuesBodySite(key : integer; id : string; context : TFhirResource; resource : TFhirBodySite);
    procedure BuildIndexValuesSlot(key : integer; id : string; context : TFhirResource; resource : TFhirSlot);
    procedure BuildIndexValuesAppointment(key : integer; id : string; context : TFhirResource; resource : TFhirAppointment);
    procedure BuildIndexValuesSchedule(key : integer; id : string; context : TFhirResource; resource : TFhirSchedule);
    procedure BuildIndexValuesAppointmentResponse(key : integer; id : string; context : TFhirResource; resource : TFhirAppointmentResponse);
    procedure BuildIndexValuesHealthcareService(key : integer; id : string; context : TFhirResource; resource : TFhirHealthcareService);
    procedure BuildIndexValuesDataElement(key : integer; id : string; context : TFhirResource; resource : TFhirDataElement);
    procedure BuildIndexValuesTestScript(key : integer; id : string; context : TFhirResource; resource : TFhirTestScript);
    procedure BuildIndexValuesNamingSystem(key : integer; id : string; context : TFhirResource; resource : TFhirNamingSystem);
    procedure BuildIndexValuesSubscription(key : integer; id : string; context : TFhirResource; resource : TFhirSubscription);
    procedure BuildIndexValuesDetectedIssue(key : integer; id : string; context : TFhirResource; resource : TFhirDetectedIssue);
    procedure BuildIndexValuesRiskAssessment(key : integer; id : string; context : TFhirResource; resource : TFhirRiskAssessment);
    procedure BuildIndexValuesOperationDefinition(key : integer; id : string; context : TFhirResource; resource : TFhirOperationDefinition);
    procedure BuildIndexValuesReferralRequest(key : integer; id : string; context : TFhirResource; resource : TFhirReferralRequest);
    procedure BuildIndexValuesNutritionOrder(key : integer; id : string; context : TFhirResource; resource : TFhirNutritionOrder);
    procedure BuildIndexValuesCoverage(key : integer; id : string; context : TFhirResource; resource : TFhirCoverage);
    procedure BuildIndexValuesClaimResponse(key : integer; id : string; context : TFhirResource; resource : TFhirClaimResponse);
    procedure BuildIndexValuesClaim(key : integer; id : string; context : TFhirResource; resource : TFhirClaim);
    procedure BuildIndexValuesContract(key : integer; id : string; context : TFhirResource; resource : TFhirContract);
    procedure BuildIndexValuesClinicalImpression(key : integer; id : string; context : TFhirResource; resource : TFhirClinicalImpression);
    procedure BuildIndexValuesCommunication(key : integer; id : string; context : TFhirResource; resource : TFhirCommunication);
    procedure BuildIndexValuesCommunicationRequest(key : integer; id : string; context : TFhirResource; resource : TFhirCommunicationRequest);
    procedure BuildIndexValuesDeviceComponent(key : integer; id : string; context : TFhirResource; resource : TFhirDeviceComponent);
    procedure BuildIndexValuesDeviceMetric(key : integer; id : string; context : TFhirResource; resource : TFhirDeviceMetric);
    procedure BuildIndexValuesDeviceUseStatement(key : integer; id : string; context : TFhirResource; resource : TFhirDeviceUseStatement);
    procedure BuildIndexValuesDeviceUseRequest(key : integer; id : string; context : TFhirResource; resource : TFhirDeviceUseRequest);
    procedure BuildIndexValuesEligibilityRequest(key : integer; id : string; context : TFhirResource; resource : TFhirEligibilityRequest);
    procedure BuildIndexValuesEligibilityResponse(key : integer; id : string; context : TFhirResource; resource : TFhirEligibilityResponse);
    procedure BuildIndexValuesEnrollmentRequest(key : integer; id : string; context : TFhirResource; resource : TFhirEnrollmentRequest);
    procedure BuildIndexValuesEnrollmentResponse(key : integer; id : string; context : TFhirResource; resource : TFhirEnrollmentResponse);
    procedure BuildIndexValuesEpisodeOfCare(key : integer; id : string; context : TFhirResource; resource : TFhirEpisodeOfCare);
    procedure BuildIndexValuesExplanationOfBenefit(key : integer; id : string; context : TFhirResource; resource : TFhirExplanationOfBenefit);
    procedure BuildIndexValuesGoal(key : integer; id : string; context : TFhirResource; resource : TFhirGoal);
    procedure BuildIndexValuesImagingObjectSelection(key : integer; id : string; context : TFhirResource; resource : TFhirImagingObjectSelection);
    procedure BuildIndexValuesPaymentNotice(key : integer; id : string; context : TFhirResource; resource : TFhirPaymentNotice);
    procedure BuildIndexValuesPerson(key : integer; id : string; context : TFhirResource; resource : TFhirPerson);
    procedure BuildIndexValuesProcedureRequest(key : integer; id : string; context : TFhirResource; resource : TFhirProcedureRequest);
    procedure BuildIndexValuesSearchParameter(key : integer; id : string; context : TFhirResource; resource : TFhirSearchParameter);
    procedure BuildIndexValuesVisionPrescription(key : integer; id : string; context : TFhirResource; resource : TFhirVisionPrescription);
    procedure BuildIndexValuesProcessRequest(key : integer; id : string; context : TFhirResource; resource : TFhirProcessRequest);
    procedure BuildIndexValuesProcessResponse(key : integer; id : string; context : TFhirResource; resource : TFhirProcessResponse);
    procedure BuildIndexValuesPaymentReconciliation(key : integer; id : string; context : TFhirResource; resource : TFhirPaymentReconciliation);
    procedure BuildIndexValuesAccount(key : integer; id : string; context : TFhirResource; resource : TFhirAccount);
    procedure BuildIndexValuesImplementationGuide(key : integer; id : string; context : TFhirResource; resource : TFhirImplementationGuide);

    procedure processCompartmentTags(key : integer; id: String; tags : TFHIRTagList);
    procedure processUnCompartmentTags(key : integer; id: String; tags : TFHIRTagList);
    procedure checkTags(resource : TFhirResource; tags : TFHIRTagList);
    function transform(base : TFHIRObject; uri : String) : TFHIRObject;
  public
    function execute(key : integer; id: String; res : TFhirResourceV; tags : TFHIRTagList; appInfo : TFslObject) : TFslList<TFHIRCompartmentId>; override;
  end;

implementation

Function EncodeNYSIISValue(value : TFhirString) : String; overload;
begin
  if value = nil then
    result := ''
  else
  result := EncodeNYSIIS(value.value);
end;


{ TFhirIndexManager }

function TFhirIndexManager2.EncodeXhtml(r: TFhirDomainResource): TBytes;
var
  x, body : TFhirXHtmlNode;
  xc : TFslXmlBuilder;
begin
    if r.ResourceType <> frtBinary then
    begin
      x := TFhirXHtmlNode.Create;
      try
        x.NodeType := fhntElement;
        x.Name := 'html';
        x.AddChild('head').AddChild('title').AddText(CODES_TFHIRResourceType[r.ResourceType]);
        body := x.AddChild('body');
        if (r.language = '') then
          body.SetAttribute('lang', 'en')
        else
          body.SetAttribute('lang', r.language);
        if (r.text <> nil) and (r.text.div_ <> nil) then
          body.ChildNodes.Add(r.text.div_.Link);
        result := TEncoding.UTF8.GetBytes(TFHIRXhtmlParser.compose(x)); // don't compress, sql server has to read it.
      finally
        x.Free;
      end;
    end;
end;


procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirString; name: String);
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirUri; name: String);
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirCodeableConcept; name: String);
var
  i : integer;
begin
  if value <> nil then
  begin
    for i := 0 to value.codingList.count - 1 do
      index(aType, key, parent, value.codingList[i], name);
    if value.text <> '' then
      index2(aType, key, parent, value.text, name);
  End;
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value, name: String);
var
  ndx : TFhirIndex;
  types : TFhirSearchParamTypeList;

begin
  if (value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);

  if StringIsInteger32(value) then
    types := [sptString, sptToken, sptDate, sptReference, sptNumber, sptUri]
  else
    types := [sptString, sptToken, sptDate, sptReference, sptUri];
  if not (ndx.SearchType in types) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing string');
//  if ndx.SearchType = sptString then
    value := lowercase(RemoveAccents(copy(value, 1, INDEX_ENTRY_LENGTH)));
//  else if (length(value) > INDEX_ENTRY_LENGTH) then
//     raise EFHIRException.create('string too long for indexing: '+value+ ' ('+inttostr(length(value))+' chars)');
  FEntries.add(FConnection, key, parent, ndx, 0, value, '', 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager2.index2(aType : String; key, parent : integer; value, name: String);
var
  ndx : TFhirIndex;
begin
  if (value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [sptToken, sptReference]) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing string');
  value := lowercase(RemoveAccents(copy(value, 1, INDEX_ENTRY_LENGTH)));
  FEntries.add(FConnection, key, parent, ndx, 0, '', value, 0, '', sptString);
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value1, value2, name: String);
var
  ndx : TFhirIndex;
begin
  if (value1 = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [sptString, sptToken, sptDate]) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing string');

  if ndx.SearchType = sptString then
    value1 := lowercase(RemoveAccents(copy(value1, 1, INDEX_ENTRY_LENGTH)))
  else  if (length(value1) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('string too long for indexing: '+value1+ ' ('+inttostr(length(value1))+' chars)');

  if ndx.SearchType = sptString then
    value2 := lowercase(RemoveAccents(copy(value2, 1, INDEX_ENTRY_LENGTH)))
  else if (length(value2) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('string too long for indexing: '+value2+ ' ('+inttostr(length(value2))+' chars)');

  FEntries.add(FConnection, key, parent, ndx, 0, value1, value2, 0, '', ndx.SearchType);
end;


procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: Boolean; name: String);
var
  ndx : TFhirIndex;
  concept : integer;
begin
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [sptToken, sptString]) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' of type '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing enumeration on '+aType);
  concept := TerminologyServer.enterIntoClosure(FConnection, aType+'.'+name, 'http://hl7.org/fhir/special-values', BooleanToString(value));
  assert(concept <> 0);
  FEntries.add(FConnection, key, parent, ndx, 0, BooleanToString(value), '', 0, '', ndx.SearchType, false, concept);
end;


procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirEnum; name: String);
var
  ndx : TFhirIndex;
  concept : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;

  if (value.system = '') then
    exit;
//    raise EFHIRException.create('no system provided');

  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [sptToken]) then //todo: fix up text
    raise EFHIRException.create('Unsuitable index '+name+' of type '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing enumeration on '+aType);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
     raise EFHIRException.create('string too long for indexing: '+value.value+ ' ('+inttostr(length(value.value))+' chars)');
  if value.system <> '' then
  begin
    concept := TerminologyServer.enterIntoClosure(FConnection, aType+'.'+name, value.system, value.value);
    assert(concept <> 0);
  end
  else
    concept := 0;

  FEntries.add(FConnection, key, parent, ndx, 0, value.value, '', 0, '', ndx.SearchType, false, concept);
end;


procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirInstant; name: String);
begin
  if (value <> nil) and (value.value.notNull) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

function TFhirIndexManager2.transform(base: TFHIRObject; uri: String): TFHIRObject;
begin
  raise EFHIRTodo.create('TFhirIndexManager2.transform');
end;

function TFhirIndexManager2.TypeForKey(key: integer): string;
var
  t : TFHIRResourceConfig;
begin
  result := '';
  for t in FResConfig.Values do
    if t.key = key then
      result := t.name;
end;

function TFhirIndexManager2.execute(key : integer; id : String; res : TFhirResourceV; tags : TFHIRTagList; appInfo : TFslObject) : TFslList<TFHIRCompartmentId>;
var
  i : integer;
  entry : TFhirIndexEntry;
  dummy : string;
  keys : string;
  comps: TFslList<TFHIRCompartmentId>;
  resource : TFhirResource;
begin
  result := nil;
  resource := res as TFHIRResource;

  checkTags(resource, tags);
  FforTesting := tags.hasTestingTag;
  FEntries.clear;
  FEntries.KeyEvent := FKeyEvent;

  FMasterKey := key;
  keys := '';
  FConnection.sql := 'select ResourceKey from Ids where MasterResourceKey = '+inttostr(key);
  FConnection.prepare;
  FConnection.execute;
  while FConnection.fetchnext do
    CommaAdd(keys, FConnection.ColStringByName['ResourceKey']);
  FConnection.terminate;

  if keys <> '' then
  begin
    FConnection.ExecSQL('delete from Compartments where ResourceKey in ('+keys+')');
    FConnection.ExecSQL('update IndexEntries set Flag = 2 where ResourceKey in ('+keys+') or Target in ('+keys+')');
    FConnection.ExecSQL('delete from SearchEntries where ResourceKey in ('+keys+')');
  end;
  FConnection.ExecSQL('update Ids set deleted = 1 where MasterResourceKey = '+inttostr(key));
  FCompartments.Clear;

  processCompartmentTags(key, id, tags);
  // base indexes
  index(resource.fhirType, key, 0, id, '_id');
//  if (resource.languageElement <> nil) then
//    index(resource.ResourceType, key, 0, resource.language, '_language');

  index(resource.fhirType, key, 0, resource.implicitRulesElement, '_rules');
  if resource.meta <> nil then
  begin
//    index(resource.fhirType, key, 0, resource.meta.versionId, '_versionId');
    index(resource.fhirType, key, 0, resource.meta.lastUpdatedElement, '_lastUpdated');
    for i := 0 to resource.meta.profileList.Count - 1 do
      index(resource.fhirType, key, 0, resource.meta.profileList[i], '_profile');
    for i := 0 to resource.meta.tagList.Count - 1 do
      index(resource.fhirType, key, 0, resource.meta.tagList[i], '_tag');
    for i := 0 to resource.meta.securityList.Count - 1 do
      index(resource.fhirType, key, 0, resource.meta.securityList[i], '_security');
  end;

  buildIndexValues(key, id, resource, resource);

  processUnCompartmentTags(key, id, tags);

  if resource is TFhirDomainResource then
  begin
    FConnection.SQL := 'insert into IndexEntries (EntryKey, IndexKey, ResourceKey, SrcTesting, Flag, Extension, Xhtml) values (:k, :i, :r, :ft, 1, ''html'', :xb)';
    FConnection.prepare;
    FConnection.BindInteger('k', FKeyEvent(FConnection, ktEntries, '', dummy));
    FConnection.BindInteger('i', FInfo.NarrativeIndex);
    FConnection.BindInteger('r', key);
    FConnection.BindIntegerFromBoolean('ft', FforTesting);
    FConnection.BindBlob('xb', EncodeXhtml(TFhirDomainResource(resource)));
    FConnection.execute;
    FConnection.terminate;
  end;

  FConnection.SQL := 'insert into IndexEntries (EntryKey, IndexKey, ResourceKey, Parent, MasterResourceKey, SpaceKey, Value, Value2, SrcTesting, Flag, target, concept) values (:k, :i, :r, :p, :m, :s, :v, :v2, :ft, :f, :t, :c)';
  FConnection.prepare;
  for i := 0 to FEntries.Count - 1 Do
  begin
    entry := FEntries[i];
    FConnection.BindInteger('k', FEntries[i].EntryKey);
    FConnection.BindInteger('i', entry.IndexKey);
    FConnection.BindInteger('r', entry.key);
    if entry.parent = 0 then
      FConnection.BindNull('p')
    else
      FConnection.BindInteger('p', entry.parent);
    if entry.key <> key then
      FConnection.BindInteger('m', key)
    else
      FConnection.BindNull('m');
    if entry.Flag then
      FConnection.BindInteger('f', 1)
    else
      FConnection.BindInteger('f', 0);
    if entry.concept = 0 then
      FConnection.BindNull('c')
    else
      FConnection.BindInteger('c', entry.concept);

    if entry.RefType = 0 then
      FConnection.BindNull('s')
    else
      FConnection.BindInteger('s', entry.RefType);
    FConnection.BindIntegerFromBoolean('ft', FforTesting);
    FConnection.BindString('v', entry.Value1);
    FConnection.BindString('v2', entry.Value2);
    if (entry.Target = 0) or (entry.Target = FMasterKey) then
      FConnection.BindNull('t')
    else
      FConnection.BindInteger('t', entry.target);
    try
      FConnection.execute;
    except
      on e:exception do
        raise EFHIRException.create('Exception storing values "'+entry.Value1+'" and "'+entry.Value2+'": '+e.message);
    end;
  end;
  FConnection.terminate;

  comps := TFslList<TFHIRCompartmentId>.create;
  try
    if FCompartments.Count > 0 then
    begin
      FConnection.SQL := 'insert into Compartments (ResourceCompartmentKey, ResourceKey, TypeKey, CompartmentKey, Id) values (:pk, :r, :ct, :ck, :id)';
      FConnection.prepare;
      for i := 0 to FCompartments.Count - 1 Do
      begin
        comps.Add(TFhirCompartmentId.Create(FCompartments[i].Enum, FCompartments[i].Id));
        FConnection.BindInteger('pk', FKeyEvent(FConnection, ktCompartment, '', dummy));
        FConnection.BindInteger('r', FCompartments[i].key);
        FConnection.BindInteger('ct', FCompartments[i].typekey);
        FConnection.BindString('id', FCompartments[i].id);
        if FCompartments[i].ckey > 0 then
          FConnection.BindInteger('ck', FCompartments[i].ckey)
        else
          FConnection.BindNull('ck');
        FConnection.execute;
      end;
      FConnection.terminate;
    end;
    result := comps.link;
  finally
    result.free;
  end;
end;


procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirCoding; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
  concept : integer;
begin
  if (value = nil) or (value.code = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if ndx.SearchType <> sptToken then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Coding');
  if (value.system <> '') then
  begin
    if not FSpaces.ResolveSpace(value.system, ref) then
      RecordSpace(value.system, ref);
    concept := TerminologyServer.enterIntoClosure(FConnection, aType+'.'+name, value.system, value.code);
  end
  else
  begin
    ref := 0;
    concept := 0;
  end;

  if (length(value.code) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('code too long for indexing: '+value.code);
  if value.display <> '' then
    FEntries.add(FConnection, key, parent, ndx, ref, value.code, lowercase(RemoveAccents(copy(value.display, 1, INDEX_ENTRY_LENGTH))), 0, '', ndx.SearchType, false, concept)
  else
    FEntries.add(FConnection, key, parent, ndx, ref, value.code, '', 0, '', ndx.SearchType, false, concept);
end;

Function ComparatorPrefix(v : String; c : TFhirQuantityComparatorEnum) : String;
begin
  case c of
    QuantityComparatorLessThan : result := '<'+v;
    QuantityComparatorLessOrEquals : result := '<='+v;
    QuantityComparatorGreaterOrEquals : result := '>='+v;
    QuantityComparatorGreaterThan : result := '>'+v;
  else
    result := v;
  end;
end;

procedure TFhirIndexManager2.GetBoundaries(value : String; comparator: TFhirQuantityComparatorEnum; var low, high : String);
var
  dec : TFslDecimal;
begin
  dec := TFslDecimal.ValueOf(value);
  case comparator of
    QuantityComparatorNull :
      begin
      low := dec.lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
      high := dec.upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    QuantityComparatorLessThan :
      begin
      low := TFslDecimal.makeInfinity.Negated.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      high := dec.upperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    QuantityComparatorLessOrEquals :
      begin
      low := TFslDecimal.makeInfinity.Negated.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      high := dec.immediateLowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    QuantityComparatorGreaterOrEquals :
      begin
      low := dec.lowerBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
      high := TFslDecimal.makeInfinity.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
    QuantityComparatorGreaterThan :
      begin
      low := dec.immediateUpperBound.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, false);
      high := TFslDecimal.makeInfinity.normaliseDecimal(INDEX_DIGITS, INDEX_DECIMALS, true);
      end;
  end;
end;


procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value : TFhirRange; name : String);
var
  ndx : TFhirIndex;
  v1, v2, crap : String;
  ref : integer;
  specified, canonical : TUcumPair;
begin
  if value = nil then
    exit;

  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join: "'+name+'"');
  if not (ndx.SearchType in [sptToken, sptNumber, sptQuantity]) then
    raise EFHIRException.create('Unsuitable index "'+name+'" '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing range');

  GetBoundaries(value.low.value, QuantityComparatorNull, v1, crap);
  GetBoundaries(value.high.value, QuantityComparatorNull, crap, v2);

  if (length(v1) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if (length(v2) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if not FSpaces.ResolveSpace(value.low.unit_, ref) then
    recordSpace(value.low.unit_, ref);
  FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType);
  if value.low.system <> '' then
  begin
    if not FSpaces.ResolveSpace(value.low.system+'#'+value.low.code, ref) then
      recordSpace(value.low.system, ref);
    FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType);
  end;

  // ok, if there's a ucum code:
  if (value.low.code <> '') and (value.low.system = 'http://unitsofmeasure.org') and (FTerminologyServer.CommonTerminologies.Ucum <> nil) then
  begin
    specified := TUcumPair.create;
    try
      specified.Value := TFslDecimal.ValueOf(value.low.value);
      specified.UnitCode := value.low.code;
      canonical := FTerminologyServer.CommonTerminologies.Ucum.getCanonicalForm(specified);
      try
        GetBoundaries(canonical.Value.AsString, QuantityComparatorNull, v1, v2);
        if (length(v1) > INDEX_ENTRY_LENGTH) then
          raise EFHIRException.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if (length(v2) > INDEX_ENTRY_LENGTH) then
          raise EFHIRException.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if not FSpaces.ResolveSpace('urn:ucum-canonical#'+canonical.UnitCode, ref) then
          recordSpace('urn:ucum-canonical#'+canonical.UnitCode, ref);
        FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType, true);
      finally
        canonical.free;
      end;
    finally
      specified.free;
    end;
  end;
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value : TFhirQuantity; name : String; units : string = '');
var
  ndx : TFhirIndex;
  v1, v2 : String;
  ref : integer;
  specified, canonical : TUcumPair;
begin
  if value = nil then
    exit;
  if value.value = '' then
    exit;

  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join: "'+name+'"');
  if not (ndx.SearchType in [sptToken, sptNumber, sptQuantity]) then
    raise EFHIRException.create('Unsuitable index "'+name+'" '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing quantity');

  GetBoundaries(value.value, value.comparator, v1, v2);

  if (length(v1) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if (length(v2) > INDEX_ENTRY_LENGTH) then
      raise EFHIRException.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if not FSpaces.ResolveSpace(value.unit_, ref) then
    recordSpace(value.unit_, ref);
  FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType);
  if value.system <> '' then
  begin
    if not FSpaces.ResolveSpace(value.system+'#'+value.code, ref) then
      recordSpace(value.system+'#'+value.code, ref);
    FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType);
  end;

  // ok, if there's a ucum code:
  if (value.code <> '') and (value.system = 'http://unitsofmeasure.org') and (FTerminologyServer.CommonTerminologies.Ucum <> Nil) then
  begin
    specified := TUcumPair.create;
    try
      specified.Value := TFslDecimal.ValueOf(value.value);
      specified.UnitCode := value.code;
      canonical := FTerminologyServer.CommonTerminologies.Ucum.getCanonicalForm(specified);
      try
        GetBoundaries(canonical.Value.AsString, value.comparator, v1, v2);
        if (length(v1) > INDEX_ENTRY_LENGTH) then
          raise EFHIRException.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if (length(v2) > INDEX_ENTRY_LENGTH) then
          raise EFHIRException.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if not FSpaces.ResolveSpace('urn:ucum-canonical#'+canonical.UnitCode, ref) then
          recordSpace('urn:ucum-canonical#'+canonical.UnitCode, ref);
        FEntries.add(FConnection, key, parent, ndx, ref, v1, v2, 0, '', ndx.SearchType, true);
      finally
        canonical.free;
      end;
    finally
      specified.free;
    end;
  end;
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value : TFhirPeriod; name : String);
begin
  if (value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value : TFhirTiming; name : String);
begin
  if (value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirDateTime; name: String);
begin
  if (value <> nil) and (value.value.notNull) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; min, max : TDateTime; name: String);
var
  ndx : TFhirIndex;
begin
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType = sptDate) then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing date');
  FEntries.add(FConnection, key, parent, ndx, 0, TFslDateTime.make(min, dttzUnknown).toHL7, TFslDateTime.make(max, dttzUnknown).toHL7, 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirIdentifier; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index an identifier in an index that is a resource join, index name '+name);
  if not (ndx.SearchType in [sptToken]) then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Identifier');
  ref := 0;
  if (value.system <> '') then
    if not FSpaces.ResolveSpace(value.system, ref) then
      recordSpace(value.system, ref);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('id too long for indexing: '+value.value);
  FEntries.add(FConnection, key, parent, ndx, ref, value.value, '', 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirAddress; name: String);
var
  i : integer;
begin
  if (value = nil) then
    exit;
  for i := 0 to value.lineList.count - 1 do
    index(aType, key, parent, value.lineList[i], name);
  index(aType, key, parent, value.cityElement, name);
  index(aType, key, parent, value.stateElement, name);
  index(aType, key, parent, value.countryElement, name);
  index(aType, key, parent, value.postalCodeElement, name);

  index(aType, key, parent, value.cityElement, name+'-city');
  index(aType, key, parent, value.countryElement, name+'-country');
  index(aType, key, parent, value.postalCodeElement, name+'-postalcode');
  index(aType, key, parent, value.stateElement, name+'-state');
  index(aType, key, parent, value.useElement, name+'-use');
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirContactPoint; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [sptToken, sptString]) then
    raise EFHIRException.create('Unsuitable index '+name+':'+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Contact on '+aType);
  ref := 0;
  if (value.systemElement <> nil) and (value.systemElement.value <> '') then
    if not FSpaces.ResolveSpace(value.systemElement.value, ref) then
      recordSpace(value.systemElement.value, ref);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('contact value too long for indexing: '+value.value);
  FEntries.add(FConnection, key, parent, ndx, ref, value.value, '', 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: TFhirIdentifierList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: TFhirCodingList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: TFhirCodeableConceptList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: TFhirSampledData; name: String);
begin
 // todo
end;

procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: TFhirRatio; name: String);
begin
  // don't have a clue what to do here
end;

procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirHumanName; name, phoneticName: String);
var
  i : integer;
  s : String;
  n : TFhirString;
begin
  if (value = nil) then
    exit;
  if (name <> '') then
  begin
    index(aType, key, parent, value.text, name);
    for n in value.familyList do
      for s in n.value.Split([' ', '-']) do
        index(aType, key, parent, s, name);
    for i := 0 to value.givenList.count - 1 do
      index(aType, key, parent, value.givenList[i], name);
    for i := 0 to value.prefixList.count - 1 do
      index(aType, key, parent, value.prefixList[i], name);
    for i := 0 to value.suffixList.count - 1 do
      index(aType, key, parent, value.suffixList[i], name);
  end;

  if phoneticName <> '' then
  begin
    for n in value.familyList do
      for s in n.value.Split([' ', '-']) do
        index(aType, key, parent, EncodeNYSIIS(s), phoneticName);
    for i := 0 to value.givenList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.givenList[i].value), phoneticName);
    for i := 0 to value.prefixList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.prefixList[i].value), phoneticName);
    for i := 0 to value.suffixList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.suffixList[i].value), phoneticName);
  end;
end;

{
procedure TFhirIndexManager2.index(aType : String; key, parent : integer; value: TFhirDecimal; name: String);
var
  ndx : TFhirIndex;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [sptString, sptToken]) then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing decimal');
  FEntries.add(FConnection, key, ndx, 0, value.value, '', 0, ndx.SearchType);
end;
}

function isLocalTypeReference(url : String; var type_, id : String) : boolean;
var
  p : TArray<String>;
  i : TFhirResourceType;
begin
  p := url.Split(['/']);
  if (length(p) = 2) or ((length(p) = 4) and (p[2] = '_history')) then
  begin
    result := isResourceName(p[0]) and IsId(p[1]);
    if result then
    begin
      type_ := p[0];
      id := p[1];
    end;
  end
  else
    result := false;
end;

function sumContainedResources(resource : TFhirDomainResource) : string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to resource.containedList.Count - 1 do
    result := result + ',' + resource.containedList[i].xmlId;
  delete(result, 1, 1);
end;

procedure TFhirIndexManager2.index(context : TFhirResource; aType : String; key, parent : integer; value: TFhirReference; name: String; specificType : String = '');
var
  ndx : TFhirIndex;
  ref, i : integer;
  target : integer;
  type_, id : String;
  contained : TFhirResource;
  url : String;
  ok : boolean;
  ttype : TFhirResourceType;
begin
  ttype := frtNull;
  if (value = nil) then
    exit;
  if (value.reference = '') and (value.display <> '') then
  begin
    index(aType, key, parent, value.displayElement, name);
    exit;
  end;
  if (value.reference = '') then
    exit;

  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) and (name = 'patient') then
    ndx := FInfo.Indexes.getByName(aType, 'subject');
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
// ggtodo - until target types are sorted out....
//  if (ndx.TargetTypes = []) then
//    raise EFHIRException.create('Attempt to index a resource join in an index ('+aType+'/'+name+') that is a not a join (has no target types)');
  if ndx.SearchType <> sptReference then
    raise EFHIRException.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing reference on a '+aType);

  if (length(value.reference) > INDEX_ENTRY_LENGTH) then
    raise EFHIRException.create('resource url too long for indexing: '+value.reference);

 {
  ! if the value has a value, then we need to index the value, even though we don't actually have it as a resource
  ! what we do is construct it with a fictional GUID id and index that
  }

  target := 0;
  ref := 0;
  ok := false;

  if StringStartsWith(value.reference, '#') then
  begin
    if context is TFhirDomainResource then
      contained := FindContainedResource(TFhirDomainResource(context), value)
    else
      raise EFHIRException.create('Reference to contained resource found in a resource that does not have contained resources"');
    if contained = nil then
      raise EFHIRException.create('No contained resource found in resource for "'+value.reference+'", list from '+CODES_TFHIRResourceType[context.ResourceType]+' = "'+sumContainedResources(TFhirDomainResource(context))+'"');
    if (specificType = '') or (contained.fhirType = specificType) then
    begin
      ttype := contained.ResourceType;
      if not FSpaces.ResolveSpace(CODES_TFHIRResourceType[contained.ResourceType], ref) then
        recordSpace(CODES_TFHIRResourceType[contained.ResourceType], ref);
      target := FKeyEvent(FConnection, ktResource, contained.fhirType, id);
      FConnection.execSql('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(ref)+' and LastId < '+id);
      FConnection.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, MasterResourceKey, ForTesting) values (:k, :r, :i, null, '+inttostr(FMasterKey)+', :ft)';
      FConnection.Prepare;
      FConnection.BindInteger('k', target);
      FConnection.BindInteger('r', ref);
      FConnection.BindString('i', id);
      FConnection.BindIntegerFromBoolean('ft', FforTesting);
      FConnection.Execute;
      FConnection.Terminate;
      buildIndexValues(target, '', context, contained);
      ok := true;
    end;
  end
  else
  begin
    url := value.reference;
    for i := 0 to FBases.Count -1 do
    begin
      if StringStartsWith(url, FBases[i]+'/') then
        url := copy(Url, length(FBases[i])+2, $FFFF);
    end;
    if isLocalTypeReference(url, type_, id) then
    begin
      if (specificType = '') or (type_ = specificType) then
      begin
        ttype := ResourceTypeByName(type_);
        if not FSpaces.ResolveSpace(type_, ref) then
          recordSpace(type_, ref);

        FConnection.sql := 'Select ResourceKey from Ids as i, Types as t where i.ResourceTypeKey = t.ResourceTypeKey and ResourceName = :t and Id = :id';
        FConnection.Prepare;
        FConnection.BindString('t', type_);
        FConnection.BindString('id', id);
        FConnection.Execute;
        if FConnection.FetchNext then
          target := FConnection.ColIntegerByName['ResourceKey']; // otherwise we try and link it up if we ever see the resource that this refers to
        FConnection.Terminate;
        ok := true;
      end;
    end
    else if url.startsWith('http:') or url.startsWith('https:') then
    begin
      id := url;
      ok := true;
    end;
  end;

  if ok then
    FEntries.add(FConnection, key, parent, ndx, ref, id, '', target, CODES_TFHIRResourceType[ttype], ndx.SearchType);
end;


procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: TFhirInteger; name: String);
var
  ndx : TFhirIndex;
  v1, v2 : String;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [sptString, sptNumber, sptToken]) then
    raise EFHIRException.create('Unsuitable index '+name+' : '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing integer');
  GetBoundaries(value.value, QuantityComparatorNull, v1, v2);
  FEntries.add(FConnection, key, parent, ndx, 0, v1, v2, 0, '', ndx.SearchType);
end;




procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: TFhirDate; name: String);
begin
  if (value <> nil) and (value.value.notNull) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: TFhirBoolean; name: String);
var
  ndx : TFhirIndex;
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager2.patientCompartment(key : integer; reference: TFhirReference);
var
  sid : string;
begin
  if reference = nil then
    exit;
  if reference.reference = '' then
    exit;
  if StringStartsWith(reference.reference, '#') then
    exit; // what to do in this case?
  if not StringStartsWith(reference.reference, 'Patient/') then
    exit; // what to do in this case?
  sid := copy(reference.reference, 9, $FF);
  if (pos('/', sid) > 0) then
    sid := copy(sid, 1, pos('/', sid) - 1);
  patientCompartment(key, 'Patient', sid);
end;


procedure TFhirIndexManager2.patientCompartment(key : integer; type_, id : String);
begin
  FConnection.sql := 'Select i.ResourceTypeKey, ResourceKey from Ids as i, Types as t where i.ResourceTypeKey = t.ResourceTypeKey and ResourceName = :t and Id = :id';
  FConnection.Prepare;
  FConnection.BindString('t', type_);
  FConnection.BindString('id', id);
  FConnection.Execute;
  if FConnection.FetchNext then
    FCompartments.add(key, FConnection.ColIntegerByName['ResourceTypeKey'], FConnection.ColIntegerByName['ResourceKey'], TypeForKey(FConnection.ColIntegerByName['ResourceTypeKey']), id);
  FConnection.Terminate;
end;

procedure TFhirIndexManager2.patientCompartmentNot(key : integer; type_, id : String);
begin
  FCompartments.removeById(id);
end;

procedure TFhirIndexManager2.processCompartmentTags(key: integer; id: String; tags: TFHIRTagList);
var
  i : integer;
begin
  for i := 0 to tags.Count - 1 do
    if (tags[i].system = TAG_FHIR_SYSTEM) and (tags[i].code = TAG_COMPARTMENT_IN) then
      patientCompartment(key, 'Patient', tags[i].display);

end;

procedure TFhirIndexManager2.processUnCompartmentTags(key: integer; id: String; tags: TFHIRTagList);
var
  i : integer;
begin
  for i := 0 to tags.Count - 1 do
    if (tags[i].system = TAG_FHIR_SYSTEM) and (tags[i].code = TAG_COMPARTMENT_OUT) then
      patientCompartmentNot(key, 'Patient', tags[i].display);

end;

procedure TFhirIndexManager2.recordSpace(space: string; key: integer);
begin
  FConnection.SQL := 'insert into Spaces (SpaceKey, Space) values ('+inttostr(key)+', :s)';
  FConnection.prepare;
  FConnection.BindString('s', space);
  FConnection.execute;
  FConnection.terminate;
end;

function TFhirIndexManager2.index(aType: String; key, parent: integer; name: String): Integer;
var
  ndx : TFhirComposite;
begin
  ndx := FInfo.Composites.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown composite index '+name+' on type '+aType);
  if (ndx.Key = 0) then
    raise EFHIRException.create('unknown composite index '+ndx.Name);
  result := FEntries.add(FConnection, key, parent, ndx);
end;

procedure TFhirIndexManager2.index(aType: String; key, parent: integer; value: TFhirDecimal; name: String);
var
  ndx : TFhirIndex;
  v1,v2 : String;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.Indexes.getByName(aType, name);
  if (ndx = nil) then
    raise EFHIRException.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise EFHIRException.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [sptString, sptNumber, sptToken]) then
    raise EFHIRException.create('Unsuitable index '+name+' : '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing integer');
  GetBoundaries(value.value, QuantityComparatorNull, v1, v2);
  FEntries.add(FConnection, key, parent, ndx, 0, v1, v2, 0, '', ndx.SearchType);
end;

procedure TFhirIndexManager2.index(context: TFhirResource; aType: String; key, parent: integer; value: TFhirReferenceList; name: String; specificType : String = '');
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(context, atype, key, parent, value[i], name, specificType);
end;


procedure TFhirIndexManager2.checkTags(resource: TFhirResource; tags: TFHIRTagList);
var
  c : integer;
begin
  c := 0;
  if (resource.meta <> nil) then
    c := resource.meta.tagList.Count + resource.meta.securityList.Count + resource.meta.profileList.Count;
  if c <> tags.Count then
    raise EFHIRException.create('Tags out of sync');
end;


procedure TFhirIndexManager2.buildIndexValues(key : integer; id : string; context, resource: TFhirResource);
begin
  case resource.ResourceType of
    frtBinary : buildIndexValuesBinary(key, id, context, TFhirBinary(resource));
    frtAllergyIntolerance : buildIndexValuesAllergyIntolerance(key, id, context, TFhirAllergyIntolerance(resource));
    frtCarePlan : buildIndexValuesCarePlan(key, id, context, TFhirCarePlan(resource));
    frtConformance : buildIndexValuesConformance(key, id, context, TFhirConformance(resource));
    frtDevice : buildIndexValuesDevice(key, id, context, TFhirDevice(resource));
    frtDiagnosticReport : buildIndexValuesDiagnosticReport(key, id, context, TFhirDiagnosticReport(resource));
    frtDiagnosticOrder : buildIndexValuesDiagnosticOrder(key, id, context, TFhirDiagnosticOrder(resource));
    frtComposition : buildIndexValuesComposition(key, id, context, TFhirComposition(resource));
    frtDocumentReference : buildIndexValuesDocumentReference(key, id, context, TFhirDocumentReference(resource));
    frtDocumentManifest : buildIndexValuesDocumentManifest(key, id, context, TFhirDocumentManifest(resource));
    frtGroup : buildIndexValuesGroup(key, id, context, TFhirGroup(resource));
    frtImagingStudy : buildIndexValuesImagingStudy(key, id, context, TFhirImagingStudy(resource));
    frtImmunization : buildIndexValuesImmunization(key, id, context, TFhirImmunization(resource));
    frtImmunizationRecommendation : buildIndexValuesImmunizationRecommendation(key, id, context, TFhirImmunizationRecommendation(resource));
    frtOperationOutcome : buildIndexValuesOperationOutcome(key, id, context, TFhirOperationOutcome(resource));
    frtList : buildIndexValuesList(key, id, context, TFhirList(resource));
    frtLocation : buildIndexValuesLocation(key, id, context, TFhirLocation(resource));
    frtMedication : buildIndexValuesMedication(key, id, context, TFhirMedication(resource));
    frtMedicationAdministration : buildIndexValuesMedicationAdministration(key, id, context, TFhirMedicationAdministration(resource));
    frtMedicationOrder : buildIndexValuesMedicationOrder(key, id, context, TFhirMedicationOrder(resource));
    frtMedicationDispense : buildIndexValuesMedicationDispense(key, id, context, TFhirMedicationDispense(resource));
    frtMedicationStatement : buildIndexValuesMedicationStatement(key, id, context, TFhirMedicationStatement(resource));
    frtMessageHeader : buildIndexValuesMessageHeader(key, id, context, TFhirMessageHeader(resource));
    frtObservation : buildIndexValuesObservation(key, id, context, TFhirObservation(resource));
    frtOrder : buildIndexValuesOrder(key, id, context, TFhirOrder(resource));
    frtOrderResponse : buildIndexValuesOrderResponse(key, id, context, TFhirOrderResponse(resource));
    frtOrganization : buildIndexValuesOrganization(key, id, context, TFhirOrganization(resource));
    frtPatient : buildIndexValuesPatient(key, id, context, TFhirPatient(resource));
    frtParameters : buildIndexValuesParameters(key, id, context, TFhirParameters(resource));
    frtMedia : buildIndexValuesMedia(key, id, context, TFhirMedia(resource));
    frtPractitioner : buildIndexValuesPractitioner(key, id, context, TFhirPractitioner(resource));
    frtCondition : buildIndexValuesCondition(key, id, context, TFhirCondition(resource));
    frtProcedure : buildIndexValuesProcedure(key, id, context, TFhirProcedure(resource));
    frtProvenance : buildIndexValuesProvenance(key, id, context, TFhirProvenance(resource));
    frtQuestionnaire : buildIndexValuesQuestionnaire(key, id, context, TFhirQuestionnaire(resource));
    frtSpecimen : buildIndexValuesSpecimen(key, id, context, TFhirSpecimen(resource));
    frtSubstance : buildIndexValuesSubstance(key, id, context, TFhirSubstance(resource));
    frtValueSet : buildIndexValuesValueSet(key, id, context, TFhirValueSet(resource));
    frtConceptMap : buildIndexValuesConceptMap(key, id, context, TFhirConceptMap(resource));
    frtEncounter : buildIndexValuesEncounter(key, id, context, TFhirEncounter(resource));
    frtRelatedPerson : buildIndexValuesRelatedPerson(key, id, context, TFhirRelatedPerson(resource));
    frtSupplyDelivery : buildIndexValuesSupplyDelivery(key, id, context, TFhirSupplyDelivery(resource));
    frtSupplyRequest : buildIndexValuesSupplyRequest(key, id, context, TFhirSupplyRequest(resource));
    frtFlag : buildIndexValuesFlag(key, id, context, TFhirFlag(resource));
    frtFamilyMemberHistory : buildIndexValuesFamilyMemberHistory(key, id, context, TFhirFamilyMemberHistory(resource));
    frtStructureDefinition : buildIndexValuesStructureDefinition(key, id, context, TFHirStructureDefinition(resource));
    frtAuditEvent : buildIndexValuesAuditEvent(key, id, context, TFhirAuditEvent(resource));
    frtBundle : buildIndexValuesBundle(key, id, context, TFhirBundle(resource));
    frtBodySite : buildIndexValuesBodySite(key, id, context, TFhirBodySite(resource));
    frtClinicalImpression : buildIndexValuesClinicalImpression(key, id, context, TFhirClinicalImpression(resource));
    frtCommunication : buildIndexValuesCommunication(key, id, context, TFhirCommunication(resource));
    frtCommunicationRequest : buildIndexValuesCommunicationRequest(key, id, context, TFhirCommunicationRequest(resource));
    frtDeviceComponent : buildIndexValuesDeviceComponent(key, id, context, TFhirDeviceComponent(resource));
    frtDeviceMetric : buildIndexValuesDeviceMetric(key, id, context, TFhirDeviceMetric(resource));
    frtDeviceUseStatement : buildIndexValuesDeviceUseStatement(key, id, context, TFhirDeviceUseStatement(resource));
    frtDeviceUseRequest : buildIndexValuesDeviceUseRequest(key, id, context, TFhirDeviceUseRequest(resource));
    frtEligibilityRequest : buildIndexValuesEligibilityRequest(key, id, context, TFhirEligibilityRequest(resource));
    frtEligibilityResponse : buildIndexValuesEligibilityResponse(key, id, context, TFhirEligibilityResponse(resource));
    frtEnrollmentRequest : buildIndexValuesEnrollmentRequest(key, id, context, TFhirEnrollmentRequest(resource));
    frtEnrollmentResponse : buildIndexValuesEnrollmentResponse(key, id, context, TFhirEnrollmentResponse(resource));
    frtEpisodeOfCare : buildIndexValuesEpisodeOfCare(key, id, context, TFhirEpisodeOfCare(resource));
    frtExplanationOfBenefit : buildIndexValuesExplanationOfBenefit(key, id, context, TFhirExplanationOfBenefit(resource));
    frtGoal : buildIndexValuesGoal(key, id, context, TFhirGoal(resource));
    frtImagingObjectSelection : buildIndexValuesImagingObjectSelection(key, id, context, TFhirImagingObjectSelection(resource));
    frtClaim : buildIndexValuesClaim(key, id, context, TFhirClaim(resource));
    frtPaymentNotice : buildIndexValuesPaymentNotice(key, id, context, TFhirPaymentNotice(resource));
    frtPerson : buildIndexValuesPerson(key, id, context, TFhirPerson(resource));
    frtProcedureRequest : buildIndexValuesProcedureRequest(key, id, context, TFhirProcedureRequest(resource));
    frtSearchParameter : buildIndexValuesSearchParameter(key, id, context, TFhirSearchParameter(resource));
    frtVisionPrescription : buildIndexValuesVisionPrescription(key, id, context, TFhirVisionPrescription(resource));
    frtProcessRequest : buildIndexValuesProcessRequest(key, id, context, TFhirProcessRequest(resource));
    frtProcessResponse : buildIndexValuesProcessResponse(key, id, context, TFhirProcessResponse(resource));
    frtPaymentReconciliation : buildIndexValuesPaymentReconciliation(key, id, context, TFhirPaymentReconciliation(resource));
    frtAccount : buildIndexValuesAccount(key, id, context, TFhirAccount(resource));
    frtImplementationGuide : buildIndexValuesImplementationGuide(key, id, context, TFhirImplementationGuide(resource));
    frtCoverage : buildIndexValuesCoverage(key, id, context, TFhirCoverage(resource));
    frtClaimResponse : buildIndexValuesClaimResponse(key, id, context, TFhirClaimResponse(resource));
    frtContract : buildIndexValuesContract(key, id, context, TFhirContract(resource));
    frtBasic : buildIndexValuesBasic(key, id, context, TFhirBasic(resource));
    frtQuestionnaireResponse : buildIndexValuesQuestionnaireResponse(key, id, context, TFhirQuestionnaireResponse(resource));
    frtSlot : BuildIndexValuesSlot(key, id, context, TFhirSlot(resource));
    frtAppointment : BuildIndexValuesAppointment(key, id, context, TFhirAppointment(resource));
    frtSchedule : BuildIndexValuesSchedule(key, id, context, TFhirSchedule(resource));
    frtAppointmentResponse : BuildIndexValuesAppointmentResponse(key, id, context, TFhirAppointmentResponse(resource));
    frtHealthcareService : BuildIndexValuesHealthcareService(key, id, context, TFhirHealthcareService(resource));
    frtDataElement : BuildIndexValuesDataElement(key, id, context, TFhirDataElement(resource));
    frtTestScript : BuildIndexValuesTestScript(key, id, context, TFhirTestScript(resource));
    frtNamingSystem : BuildIndexValuesNamingSystem(key, id, context, TFhirNamingSystem(resource));
    frtSubscription : BuildIndexValuesSubscription(key, id, context, TFhirSubscription(resource));
    frtDetectedIssue : BuildIndexValuesDetectedIssue(key, id, context, TFhirDetectedIssue(resource));
    frtRiskAssessment : BuildIndexValuesRiskAssessment(key, id, context, TFhirRiskAssessment(resource));
    frtOperationDefinition : BuildIndexValuesOperationDefinition(key, id, context, TFhirOperationDefinition(resource));
    frtReferralRequest : BuildIndexValuesReferralRequest(key, id, context, TFhirReferralRequest(resource));
    frtNutritionOrder : BuildIndexValuesNutritionOrder(key, id, context, TFhirNutritionOrder(resource));
  else
    raise EFHIRException.create('resource type indexing not implemented yet for '+CODES_TFHIRResourceType[resource.ResourceType]);
  end;
end;

procedure TFhirIndexManager2.buildIndexValuesClaim(key: integer; id : String; context : TFhirResource; resource: TFhirClaim);
begin
  index('Claim', key, 0, resource.identifierList, CODES_TSearchParamsClaim[spClaim_identifier]);
  index('Claim', key, 0, resource.priorityElement, CODES_TSearchParamsClaim[spClaim_priority]);
  index('Claim', key, 0, resource.useElement, CODES_TSearchParamsClaim[spClaim_use]);
  index(context, 'Claim', key, 0, resource.patientElement, CODES_TSearchParamsClaim[spClaim_patient]);
  index(context, 'Claim', key, 0, resource.provider, CODES_TSearchParamsClaim[spClaim_provider]);
  patientCompartment(key, resource.patient);
end;


procedure TFhirIndexManager2.buildIndexValuesCoverage(key: integer; id : String; context : TFhirResource; resource: TFhirCoverage);
begin
  index('Coverage', key, 0, resource.dependentElement, CODES_TSearchParamsCoverage[spCoverage_dependent]);
  index('Coverage', key, 0, resource.groupElement, CODES_TSearchParamsCoverage[spCoverage_group]);
  index('Coverage', key, 0, resource.identifierList, CODES_TSearchParamsCoverage[spCoverage_identifier]);
  index('Coverage', key, 0, resource.planElement, CODES_TSearchParamsCoverage[spCoverage_plan]);
  index('Coverage', key, 0, resource.sequenceElement, CODES_TSearchParamsCoverage[spCoverage_sequence]);
  index('Coverage', key, 0, resource.subplanElement, CODES_TSearchParamsCoverage[spCoverage_subplan]);
  index('Coverage', key, 0, resource.type_Element, CODES_TSearchParamsCoverage[spCoverage_type]);
//  index(context, 'Coverage', key, 0, resource.subjectList, CODES_TSearchParamsCoverage[spCoverage_subject]);
  index(context, 'Coverage', key, 0, resource.issuerElement, CODES_TSearchParamsCoverage[spCoverage_issuer]);
end;

procedure TFhirIndexManager2.buildIndexValuesClaimResponse(key: integer; id : String; context : TFhirResource; resource: TFhirClaimResponse);
begin
  index('ClaimResponse', key, 0, resource.identifierList, CODES_TSearchParamsClaimResponse[spClaimResponse_identifier]);
//  index(context, 'ClaimResponse', key, 0, resource.request, CODES_TSearchParamsClaimResponse[spClaimResponse_request]);
end;

procedure TFhirIndexManager2.buildIndexValuesEligibilityRequest(key: integer; id : String; context : TFhirResource; resource: TFhirEligibilityRequest);
begin
  index('EligibilityRequest', key, 0, resource.identifierList, CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_identifier]);
end;


procedure TFhirIndexManager2.buildIndexValuesEligibilityResponse(key: integer; id : String; context : TFhirResource; resource: TFhirEligibilityResponse);
begin
  index('EligibilityResponse', key, 0, resource.identifierList, CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_identifier]);
end;

procedure TFhirIndexManager2.buildIndexValuesEnrollmentRequest(key: integer; id : String; context : TFhirResource; resource: TFhirEnrollmentRequest);
begin
  index('EnrollmentRequest', key, 0, resource.identifierList, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_identifier]);
  index(context, 'EnrollmentRequest', key, 0, resource.subject, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_subject]);
  index(context, 'EnrollmentRequest', key, 0, resource.subject, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_patient]);
  patientCompartment(key, resource.subject);
end;


procedure TFhirIndexManager2.buildIndexValuesEnrollmentResponse(key: integer; id : String; context : TFhirResource; resource: TFhirEnrollmentResponse);
begin
  index('EnrollmentResponse', key, 0, resource.identifierList, CODES_TSearchParamsEnrollmentResponse[spEnrollmentResponse_identifier]);
end;


procedure TFhirIndexManager2.buildIndexValuesExplanationOfBenefit(key: integer; id : String; context : TFhirResource; resource: TFhirExplanationOfBenefit);
var
  i : integer;
begin
  index('ExplanationOfBenefit', key, 0, resource.identifierList, CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_identifier]);
end;


procedure TFhirIndexManager2.buildIndexValuesPaymentNotice(key: integer; id : String; context : TFhirResource; resource: TFhirPaymentNotice);
var
  i : integer;
begin
  index('PaymentNotice', key, 0, resource.identifierList, CODES_TSearchParamsPaymentNotice[spPaymentNotice_identifier]);
end;



procedure TFhirIndexManager2.buildIndexValuesPaymentReconciliation(key: integer; id : String; context : TFhirResource; resource: TFhirPaymentReconciliation);
var
  i : integer;
begin
  index('PaymentReconciliation', key, 0, resource.identifierList, CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_identifier]);
end;


procedure TFhirIndexManager2.buildIndexValuesBasic(key: integer; id : String; context : TFhirResource; resource: TFhirBasic);
begin
  index('Basic', key, 0, resource.createdElement, CODES_TSearchParamsBasic[spBasic_created]);
  index('Basic', key, 0, resource.code, CODES_TSearchParamsBasic[spBasic_code]);
  index(context, 'Basic', key, 0, resource.subject, CODES_TSearchParamsBasic[spBasic_subject]);
  index(context, 'Basic', key, 0, resource.subject, CODES_TSearchParamsBasic[spBasic_patient]);
  index(context, 'Basic', key, 0, resource.author, CODES_TSearchParamsBasic[spBasic_author]);
  index('Basic', key, 0, resource.identifierList, CODES_TSearchParamsBasic[spBasic_identifier]);
  patientCompartment(key, resource.subject);
end;

procedure TFhirIndexManager2.buildIndexValuesContract(key: integer; id : String; context : TFhirResource; resource: TFhirContract);
var
  i  : integer;
begin
  for i := 0 to resource.subjectList.Count - 1 do
  begin
    index(context, 'Contract', key, 0, resource.subjectList[i], CODES_TSearchParamsContract[spContract_subject]);
    patientCompartment(key, resource.subjectList[i]);
    index(context, 'Contract', key, 0, resource.subjectList[i], CODES_TSearchParamsContract[spContract_patient]);
  end;
  for i := 0 to resource.actorList.Count - 1 do
  begin
    index(context, 'Contract', key, 0, resource.actorList[i].entity, CODES_TSearchParamsContract[spContract_actor]);
  end;
  for i := 0 to resource.signerList.Count - 1 do
  begin
    index(context, 'Contract', key, 0, resource.signerList[i].party, CODES_TSearchParamsContract[spContract_signer]);
    patientCompartment(key, resource.signerList[i].party);
  end;
  index('Contract', key, 0, resource.identifier, CODES_TSearchParamsContract[spContract_identifier]);
end;


procedure TFhirIndexManager2.buildIndexValuesSupplyDelivery(key: integer; id : String; context : TFhirResource; resource: TFhirSupplyDelivery);
var
  i : integer;
begin
  index('SupplyDelivery', key, 0, resource.identifier, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_identifier]);
  index(context, 'SupplyDelivery', key, 0, resource.patient, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_patient]);
  patientCompartment(key, resource.patient);
  index(context, 'SupplyDelivery', key, 0, resource.receiverList, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_receiver]);
  index('SupplyDelivery', key, 0, resource.statusElement, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_status]);
  index(context, 'SupplyDelivery', key, 0, resource.supplier, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_supplier]);
end;

procedure TFhirIndexManager2.buildIndexValuesSupplyRequest(key: integer; id : String; context : TFhirResource; resource: TFhirSupplyRequest);
var
  i : integer;
begin
  index('SupplyRequest', key, 0, resource.identifier, CODES_TSearchParamsSupplyRequest[spSupplyRequest_identifier]);
  index('SupplyRequest', key, 0, resource.kind, CODES_TSearchParamsSupplyRequest[spSupplyRequest_kind]);
  index('SupplyRequest', key, 0, resource.dateElement, CODES_TSearchParamsSupplyRequest[spSupplyRequest_date]);
  index(context, 'SupplyRequest', key, 0, resource.source, CODES_TSearchParamsSupplyRequest[spSupplyRequest_source]);
  index(context, 'SupplyRequest', key, 0, resource.supplierList, CODES_TSearchParamsSupplyRequest[spSupplyRequest_supplier]);
  index('SupplyRequest', key, 0, resource.statusElement, CODES_TSearchParamsSupplyRequest[spSupplyRequest_status]);
  index(context, 'SupplyRequest', key, 0, resource.patient, CODES_TSearchParamsSupplyRequest[spSupplyRequest_patient]);
  patientCompartment(key, resource.patient);
end;


procedure TFhirIndexManager2.buildIndexValuesRelatedPerson(key: integer; id : String; context : TFhirResource; resource: TFhirRelatedPerson);
var
  i : integer;
begin
  for i := 0 to resource.addressList.count - 1 do
    index('RelatedPerson', key, 0, resource.addressList[i], CODES_TSearchParamsRelatedPerson[spRelatedPerson_address]);
  index('RelatedPerson', key, 0, resource.genderElement, CODES_TSearchParamsRelatedPerson[spRelatedPerson_gender]);

  for i := 0 to resource.identifierList.count - 1 do
    index('RelatedPerson', key, 0, resource.identifierList[i], CODES_TSearchParamsRelatedPerson[spRelatedPerson_identifier]);
  index('RelatedPerson', key, 0, resource.name, 'name', CODES_TSearchParamsRelatedPerson[spRelatedPerson_phonetic]);
  index('RelatedPerson', key, 0, resource.birthDateElement, CODES_TSearchParamsRelatedPerson[spRelatedPerson_birthdate]);
  index(context, 'RelatedPerson', key, 0, resource.patient, CODES_TSearchParamsRelatedPerson[spRelatedPerson_patient]);
  patientCompartment(key, resource.patient);
  for i := 0 to resource.telecomList.count - 1 do
  begin
    index('RelatedPerson', key, 0, resource.telecomList[i], CODES_TSearchParamsRelatedPerson[spRelatedPerson_telecom]);
    if (resource.telecomList[i].system = ContactPointSystemEmail) then
      index('RelatedPerson', key, 0, resource.telecomList[i].value, CODES_TSearchParamsRelatedPerson[spRelatedPerson_email]);
    if (resource.telecomList[i].system = ContactPointSystemPhone) then
      index('RelatedPerson', key, 0, resource.telecomList[i].value, CODES_TSearchParamsRelatedPerson[spRelatedPerson_phone]);

  end;
end;

// --------- actual indexes -----------------------------------------------------------------------------------------------

procedure TFhirIndexManager2.buildIndexValuesConformance(key : integer; id : String; context : TFhirResource; resource: TFhirConformance);
var
  i : integer;
  j : integer;
begin
  index('Conformance', key, 0, resource.dateElement, CODES_TSearchParamsConformance[spConformance_date]);
  index('Conformance', key, 0, resource.nameElement, CODES_TSearchParamsConformance[spConformance_name]);
  index('Conformance', key, 0, resource.statusElement, CODES_TSearchParamsConformance[spConformance_status]);
  index('Conformance', key, 0, resource.descriptionElement, CODES_TSearchParamsConformance[spConformance_description]);
  index('Conformance', key, 0, resource.publisherElement, CODES_TSearchParamsConformance[spConformance_publisher]);
  if resource.software <> nil then
    index('Conformance', key, 0, resource.software.nameElement, CODES_TSearchParamsConformance[spConformance_software]);
  index('Conformance', key, 0, resource.versionElement, CODES_TSearchParamsConformance[spConformance_version]);
  index('Conformance', key, 0, resource.fhirversionElement, CODES_TSearchParamsConformance[spConformance_fhirversion]);
  index('Conformance', key, 0, resource.urlElement, CODES_TSearchParamsConformance[spConformance_url]);


  for j := 0 to resource.formatList.Count - 1 do
    index('Conformance', key, 0, resource.formatList[j], CODES_TSearchParamsConformance[spConformance_format]);

  for j := 0 to resource.restList.Count - 1 do
  begin
    if resource.restList[j].security <> nil then
    begin
      for i := 0 to resource.restList[j].security.serviceList.count - 1 do
        index('Conformance', key, 0, resource.restList[j].security.serviceList[i], CODES_TSearchParamsConformance[spConformance_security]);
    end;
  end;


  for j := 0 to resource.restList.Count - 1 do
  begin
    for i := 0 to resource.restList[j].resourceList.count - 1 do
    begin
      index(context, 'Conformance', key, 0, resource.restList[j].resourceList[i].profile, CODES_TSearchParamsConformance[spConformance_profile]);
      index('Conformance', key, 0, resource.restList[j].resourceList[i].type_Element, CODES_TSearchParamsConformance[spConformance_resource]);
    end;
    index('Conformance', key, 0, resource.restList[j].modeElement, CODES_TSearchParamsConformance[spConformance_mode]);
  end;

  for j := 0 to resource.messagingList.Count - 1 Do
  begin
    for i := 0 to resource.messagingList[j].EventList.count - 1 do
    begin
      index('Conformance', key, 0, resource.messagingList[j].EventList[i].focusElement, CODES_TSearchParamsConformance[spConformance_resource]);
      index(context, 'Conformance', key, 0, resource.messagingList[j].EventList[i].request, CODES_TSearchParamsConformance[spConformance_profile]);
      index(context, 'Conformance', key, 0, resource.messagingList[j].EventList[i].response, CODES_TSearchParamsConformance[spConformance_profile]);
      index('Conformance', key, 0, resource.messagingList[j].EventList[i].modeElement, CODES_TSearchParamsConformance[spConformance_mode]);
      index('Conformance', key, 0, resource.messagingList[j].EventList[i].code, CODES_TSearchParamsConformance[spConformance_event]);
    end;
  end;

  for i := 0 to resource.DocumentList.count - 1 do
    index(context, 'Conformance', key, 0, resource.DocumentList[i].profile, CODES_TSearchParamsConformance[spConformance_profile]);
  for i := 0 to resource.profileList.count - 1 do
    index(context, 'Conformance', key, 0, resource.ProfileList[i], CODES_TSearchParamsConformance[spConformance_profile]);

end;

procedure TFhirIndexManager2.buildIndexValuesEncounter(key: integer; id : String; context : TFhirResource; resource: TFhirEncounter);
var
  i : integer;
begin
  index(context, 'Encounter', key, 0, resource.patient, CODES_TSearchParamsEncounter[spEncounter_patient]);
  index('Encounter', key, 0, resource.type_List, CODES_TSearchParamsEncounter[spEncounter_type]);
  for i := 0 to resource.participantList.count - 1 do
  begin
    index(context, 'Encounter', key, 0, resource.participantList[i].individual, CODES_TSearchParamsEncounter[spEncounter_participant]);
    index(context, 'Encounter', key, 0, resource.participantList[i].individual, CODES_TSearchParamsEncounter[spEncounter_practitioner], 'Practitioner');
    index('Encounter', key, 0, resource.participantList[i].type_List, CODES_TSearchParamsEncounter[spEncounter_participanttype]);
  end;
  index(context, 'Encounter', key, 0, resource.partOf, CODES_TSearchParamsEncounter[spEncounter_partof]);
  index('Encounter', key, 0, resource.reasonList, CODES_TSearchParamsEncounter[spEncounter_reason]);
  index(context, 'Encounter', key, 0, resource.appointment, CODES_TSearchParamsEncounter[spEncounter_appointment]);
  index(context, 'Encounter', key, 0, resource.incomingReferralList, CODES_TSearchParamsEncounter[spEncounter_incomingreferral]);
  index(context, 'Encounter', key, 0, resource.episodeOfCareList, CODES_TSearchParamsEncounter[spEncounter_episodeofcare]);
  index(context, 'Encounter', key, 0, resource.indicationList, CODES_TSearchParamsEncounter[spEncounter_indication]);
  index(context, 'Encounter', key, 0, resource.indicationList, CODES_TSearchParamsEncounter[spEncounter_condition], 'Condition');
  index(context, 'Encounter', key, 0, resource.indicationList, CODES_TSearchParamsEncounter[spEncounter_procedure], 'Procedure');

  if resource.hospitalization <> nil then
    index('Encounter', key, 0, resource.hospitalization.specialArrangementList, CODES_TSearchParamsEncounter[spEncounter_specialarrangement]);
  patientCompartment(key, resource.patient);
  for i := 0 to resource.indicationList.count - 1 do
    index(context, 'Encounter', key, 0, resource.indicationList[i], CODES_TSearchParamsEncounter[spEncounter_indication]);
  index('Encounter', key, 0, resource.statusElement, CODES_TSearchParamsEncounter[spEncounter_status]);
  index('Encounter', key, 0, resource.periodElement, CODES_TSearchParamsEncounter[spEncounter_date]);
  index('Encounter', key, 0, resource.lengthElement, CODES_TSearchParamsEncounter[spEncounter_length]);
  for i := 0 to resource.identifierList.count - 1 do
    index('Encounter', key, 0, resource.identifierList[i], CODES_TSearchParamsEncounter[spEncounter_identifier]);
  for i := 0 to resource.locationList.count - 1 do
  begin
    index(context, 'Encounter', key, 0, resource.locationList[i].locationElement, CODES_TSearchParamsEncounter[spEncounter_location]);
    index('Encounter', key, 0, resource.locationList[i].periodElement, CODES_TSearchParamsEncounter[spEncounter_locationperiod]);
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesLocation(key: integer; id : String; context : TFhirResource; resource: TFhirLocation);

var
  i : integer;

begin
  index('Location', key, 0, resource.addressElement, CODES_TSearchParamsLocation[spLocation_address]);
  index('Location', key, 0, resource.NameElement, CODES_TSearchParamsLocation[spLocation_name]);
  index('Location', key, 0, resource.statusElement, CODES_TSearchParamsLocation[spLocation_status]);
  index('Location', key, 0, resource.type_Element, CODES_TSearchParamsLocation[spLocation_type]);
  for i := 0 to resource.identifierList.Count - 1 do
    index('Location', key, 0, resource.identifierList, CODES_TSearchParamsLocation[spLocation_identifier]);
  index(context, 'Location', key, 0, resource.managingOrganizationElement, CODES_TSearchParamsLocation[spLocation_organization]);
  index(context, 'Location', key, 0, resource.partOf, CODES_TSearchParamsLocation[spLocation_partof]);
  if resource.position <> nil then
  begin
    if (resource.position.longitude <> '') and (resource.position.latitude <> '') then
      index('Location', key, 0, resource.position.longitude, resource.position.latitude, CODES_TSearchParamsLocation[spLocation_near]);
  end
//    spLocation_Near_distance, { A distance quantity to limit the near search to locations within a specific distance }
end;

procedure TFhirIndexManager2.BuildIndexValuesDocumentReference(key: integer;id : String; context : TFhirResource; resource: TFhirDocumentReference);
var
  i, p : integer;
begin
  index(context, 'DocumentReference', key, 0, resource.authenticator, CODES_TSearchParamsDocumentReference[spDocumentReference_authenticator]);
  for i := 0 to resource.authorList.count - 1 do
    index(context, 'DocumentReference', key, 0, resource.authorList[i], CODES_TSearchParamsDocumentReference[spDocumentReference_author]);
  index('DocumentReference', key, 0, resource.securityLabelList, CODES_TSearchParamsDocumentReference[spDocumentReference_securitylabel]);
  index('DocumentReference', key, 0, resource.createdElement, CODES_TSearchParamsDocumentReference[spDocumentReference_created]);
  index(context, 'DocumentReference', key, 0, resource.custodian, CODES_TSearchParamsDocumentReference[spDocumentReference_custodian]);
  index('DocumentReference', key, 0, resource.descriptionElement, CODES_TSearchParamsDocumentReference[spDocumentReference_description]);
  if resource.context <> nil then
  begin
    for i := 0 to resource.context.eventList.count - 1 do
      index('DocumentReference', key, 0, resource.context.eventList[i], CODES_TSearchParamsDocumentReference[spDocumentReference_event]);
    index('DocumentReference', key, 0, resource.context.facilityType, CODES_TSearchParamsDocumentReference[spDocumentReference_facility]);
    index('DocumentReference', key, 0, resource.context.period, CODES_TSearchParamsDocumentReference[spDocumentReference_period]);

    index('DocumentReference', key, 0, resource.context.practiceSetting, CODES_TSearchParamsDocumentReference[spDocumentReference_setting]);
    for i := 0 to resource.context.relatedList.Count - 1 do
    begin
      index('DocumentReference', key, 0, resource.context.relatedList[i].identifier, CODES_TSearchParamsDocumentReference[spDocumentReference_relatedid]);
      index(context, 'DocumentReference', key, 0, resource.context.relatedList[i].ref, CODES_TSearchParamsDocumentReference[spDocumentReference_relatedref]);
    end;
  index(context, 'DocumentReference', key, 0, resource.context.encounter, CODES_TSearchParamsDocumentReference[spDocumentReference_encounter]);
  end;
  for i := 0 to resource.contentList.count - 1 do
    for p := 0 to resource.contentList[i].formatList.count - 1 do
      index('DocumentReference', key, 0, resource.contentList[i].formatList[p], CODES_TSearchParamsDocumentReference[spDocumentReference_format]);
  index('DocumentReference', key, 0, resource.masterIdentifier, CODES_TSearchParamsDocumentReference[spDocumentReference_identifier]);
  for i := 0 to resource.identifierList.count - 1 do
    index('DocumentReference', key, 0, resource.identifierList[i], CODES_TSearchParamsDocumentReference[spDocumentReference_identifier]);
  index('DocumentReference', key, 0, resource.indexedElement, CODES_TSearchParamsDocumentReference[spDocumentReference_indexed]);
  index('DocumentReference', key, 0, resource.statusElement, CODES_TSearchParamsDocumentReference[spDocumentReference_status]);
  index(context, 'DocumentReference', key, 0, resource.subject, CODES_TSearchParamsDocumentReference[spDocumentReference_subject]);
  index(context, 'DocumentReference', key, 0, resource.subject, CODES_TSearchParamsDocumentReference[spDocumentReference_patient]);
  patientCompartment(key, resource.subject);
  for i := 0 to resource.relatesToList.Count - 1 do
  begin
    p := index('DocumentReference', key, 0, CODES_TSearchParamsDocumentReference[spDocumentReference_relatesTo]);
    index(context, 'DocumentReference', key, p, resource.relatesToList[i].target, CODES_TSearchParamsDocumentReference[spDocumentReference_relatesTo]);
    index('DocumentReference', key, p, resource.relatesToList[i].codeElement, CODES_TSearchParamsDocumentReference[spDocumentReference_relation]);
  end;
  index('DocumentReference', key, 0, resource.type_, CODES_TSearchParamsDocumentReference[spDocumentReference_type]);
  index('DocumentReference', key, 0, resource.class_, CODES_TSearchParamsDocumentReference[spDocumentReference_class]);
  for i := 0 to resource.contentList.Count - 1 do
  begin
    if (resource.contentList[i].attachment <> nil) then
    begin
      index('DocumentReference', key, 0, resource.contentList[i].attachment.languageElement, CODES_TSearchParamsDocumentReference[spDocumentReference_language]);
      index('DocumentReference', key, 0, resource.contentList[i].attachment.urlElement, CODES_TSearchParamsDocumentReference[spDocumentReference_location]);
    end;
  end;
end;


procedure TFhirIndexManager2.BuildIndexValuesDocumentManifest(key: integer;id : String; context : TFhirResource; resource: TFhirDocumentManifest);
var
  i : integer;
begin
  for i := 0 to resource.authorList.count - 1 do
    index(context, 'DocumentManifest', key, 0, resource.authorList[i], CODES_TSearchParamsDocumentManifest[spDocumentManifest_author]);

  index('DocumentManifest', key, 0, resource.createdElement, CODES_TSearchParamsDocumentManifest[spDocumentManifest_created]);
  index('DocumentManifest', key, 0, resource.descriptionElement, CODES_TSearchParamsDocumentManifest[spDocumentManifest_description]);
  index('DocumentManifest', key, 0, resource.masterIdentifier, CODES_TSearchParamsDocumentManifest[spDocumentManifest_identifier]);
  for i := 0 to resource.identifierList.count - 1 do
    index('DocumentManifest', key, 0, resource.identifierList[i], CODES_TSearchParamsDocumentManifest[spDocumentManifest_identifier]);
  index('DocumentManifest', key, 0, resource.statusElement, CODES_TSearchParamsDocumentManifest[spDocumentManifest_status]);
  index(context, 'DocumentManifest', key, 0, resource.subject, CODES_TSearchParamsDocumentManifest[spDocumentManifest_subject]);
  index(context, 'DocumentManifest', key, 0, resource.subject, CODES_TSearchParamsDocumentManifest[spDocumentManifest_patient]);
  index('DocumentManifest', key, 0, resource.sourceElement, CODES_TSearchParamsDocumentManifest[spDocumentManifest_source]);
  for i := 0 to resource.contentList.count - 1 do
    if resource.contentList[i].p is TFhirReference then
      index(context, 'DocumentManifest', key, 0, resource.contentList[i].p as TFhirReference, CODES_TSearchParamsDocumentManifest[spDocumentManifest_contentref]);

  index('DocumentManifest', key, 0, resource.type_, CODES_TSearchParamsDocumentManifest[spDocumentManifest_type]);
  for i := 0 to resource.recipientList.count - 1 do
    index(context, 'DocumentManifest', key, 0, resource.recipientList[i], CODES_TSearchParamsDocumentManifest[spDocumentManifest_recipient]);


  for i := 0 to resource.relatedList.Count - 1 do
  begin
    index('DocumentManifest', key, 0, resource.relatedList[i].identifier, CODES_TSearchParamsDocumentManifest[spDocumentManifest_relatedid]);
    index(context, 'DocumentManifest', key, 0, resource.relatedList[i].ref, CODES_TSearchParamsDocumentManifest[spDocumentManifest_relatedref]);
  end;
end;

procedure TFhirIndexManager2.buildIndexValuesBundle(key: integer; id : String; context : TFhirResource; resource: TFhirBundle);
var
  inner : TFhirResource;
  ref, target : integer;
  name : String;
  ndx : TFhirIndex;
begin
  index('Bundle', key, 0, resource.type_Element, CODES_TSearchParamsBundle[spBundle_type]);
  if (resource.type_ = BundleTypeDocument) then
  begin
    name := 'composition';
    inner := resource.entryList[0].resource
  end
  else if (resource.type_ = BundleTypeMessage) then
  begin
    name := 'message';
    inner := resource.entryList[0].resource
  end
  else
    inner := nil;

  if inner <> nil then
  begin
    ndx := FInfo.Indexes.getByName('Bundle', name);
    if (ndx = nil) then
      raise EFHIRException.create('Unknown index Bundle.'+name);
    if (length(ndx.TargetTypes) = 0) then
      raise EFHIRException.create('Attempt to index a resource join in an index (Bundle.'+name+') that is a not a join (has no target types)');
    if ndx.SearchType <> sptReference then
      raise EFHIRException.create('Unsuitable index Bundle.'+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing inner');

    if not FSpaces.ResolveSpace(CODES_TFHIRResourceType[inner.ResourceType], ref) then
      recordSpace(CODES_TFHIRResourceType[inner.ResourceType], ref);

    // ignore the existing id because this is a virtual entry; we don't want the real id to appear twice if the resource also really exists
    target := FKeyEvent(FConnection, ktResource, inner.FHIRType, id); //FConnection.CountSQL('select Max(ResourceKey) from Ids') + 1;
    FConnection.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, MasterResourceKey, ForTesting) values (:k, :r, :i, null, '+inttostr(FMasterKey)+', 0)';
    FConnection.Prepare;
    FConnection.BindInteger('k', target);
    FConnection.BindInteger('r', ref);
    FConnection.BindString('i', id);
    FConnection.Execute;
    FConnection.Terminate;
    buildIndexValues(target, '', context, inner);
    FEntries.add(FConnection, key, 0, ndx, ref, id, '', target, '', ndx.SearchType);
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesFlag(key: integer; id : String; context : TFhirResource; resource: TFhirFlag);
begin
  index(context, 'Flag', key, 0, resource.subject, CODES_TSearchParamsFlag[spFlag_subject]);
  index(context, 'Flag', key, 0, resource.subject, CODES_TSearchParamsFlag[spFlag_patient], 'Patient');
  index(context, 'Flag', key, 0, resource.author, CODES_TSearchParamsFlag[spFlag_author]);
  index('Flag', key, 0, resource.period, CODES_TSearchParamsFlag[spFlag_date]);
  patientCompartment(key, resource.subject);
end;



procedure TFhirIndexManager2.buildIndexValuesAllergyIntolerance(key: integer; id : String; context : TFhirResource; resource: TFhirAllergyIntolerance);
var
  i : integer;
begin
  index('AllergyIntolerance', key, 0, resource.categoryElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_category]);
  index('AllergyIntolerance', key, 0, resource.criticalityElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_criticality]);
  index('AllergyIntolerance', key, 0, resource.recordedDateElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_date]);
  index('AllergyIntolerance', key, 0, resource.identifierList, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_identifier]);
  index(context, 'AllergyIntolerance', key, 0, resource.recorderElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_recorder]);
  index(context, 'AllergyIntolerance', key, 0, resource.reporterElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_reporter]);
  index('AllergyIntolerance', key, 0, resource.lastOccurenceElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_lastdate]);
  index('AllergyIntolerance', key, 0, resource.statusElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_status]);
  index('AllergyIntolerance', key, 0, resource.type_Element, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_type]);
  index(context, 'AllergyIntolerance', key, 0, resource.patient, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_patient]);
  index('AllergyIntolerance', key, 0, resource.substanceElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_substance]);

  for i := 0 to resource.reactionList.Count - 1 do
  begin
    index('AllergyIntolerance', key, 0, resource.reactionList[i].substanceElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_substance]);
    index('AllergyIntolerance', key, 0, resource.reactionList[i].onsetElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_onset]);
    index('AllergyIntolerance', key, 0, resource.reactionList[i].exposureRouteElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_route]);
    index('AllergyIntolerance', key, 0, resource.reactionList[i].manifestationList, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_manifestation]);
    index('AllergyIntolerance', key, 0, resource.reactionList[i].severityElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_severity]);
  end;
  patientCompartment(key, resource.patient);
end;


procedure TFhirIndexManager2.buildIndexValuesSubstance(key: integer; id : String; context : TFhirResource; resource: TFhirSubstance);
var
  i : integer;
begin
  index('Substance', key, 0, resource.categoryList, CODES_TSearchParamsSubstance[spSubstance_category]);
  index('Substance', key, 0, resource.code, CODES_TSearchParamsSubstance[spSubstance_code]);
  index('Substance', key, 0, resource.identifierList, CODES_TSearchParamsSubstance[spSubstance_identifier]);
  for i := 0 to resource.instanceList.Count - 1 do
    index('Substance', key, 0, resource.instanceList[i].expiryElement, CODES_TSearchParamsSubstance[spSubstance_expiry]);
  for i := 0 to resource.ingredientList.count - 1 do
  begin
    index('Substance', key, 0, resource.ingredientList[i].quantity, CODES_TSearchParamsSubstance[spSubstance_quantity]);
    index(context, 'Substance', key, 0, resource.ingredientList[i].substance, CODES_TSearchParamsSubstance[spSubstance_substance]);
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesComposition(key : integer; id : String; context : TFhirResource; resource: TFhirComposition);
  procedure indexSection(section : TFhirCompositionSection);
  var
    i : integer;
  begin

    index('Composition', key, 0, section.code, CODES_TSearchParamsComposition[spComposition_section]);
    index(context, 'Composition', key, 0, section.entryList, CODES_TSearchParamsComposition[spComposition_entry]);
    for i := 0 to section.SectionList.count - 1 do
      indexSection(section.SectionList[i]);
  end;
var
  i, j : integer;
begin
  index('Composition', key, 0, resource.dateElement, CODES_TSearchParamsComposition[spComposition_date]);
  index('Composition', key, 0, resource.identifier, CODES_TSearchParamsComposition[spComposition_identifier]);
  index(context, 'Composition', key, 0, resource.subject, CODES_TSearchParamsComposition[spComposition_subject]);
  index(context, 'Composition', key, 0, resource.subject, CODES_TSearchParamsComposition[spComposition_patient], 'Patient');
  index(context, 'Composition', key, 0, resource.encounter, CODES_TSearchParamsComposition[spComposition_encounter]);
  patientCompartment(key, resource.subject);
  index('Composition', key, 0, resource.titleElement, CODES_TSearchParamsComposition[spComposition_title]);
  index('Composition', key, 0, resource.type_Element, CODES_TSearchParamsComposition[spComposition_type]);
  index('Composition', key, 0, resource.class_Element, CODES_TSearchParamsComposition[spComposition_class]);
  index('Composition', key, 0, resource.confidentialityElement, CODES_TSearchParamsComposition[spComposition_confidentiality]);
  index('Composition', key, 0, resource.statusElement, CODES_TSearchParamsComposition[spComposition_status]);
  for j := 0 to resource.eventList.Count - 1 do
    for i := 0 to resource.eventList[j].codeList.Count - 1 do
    begin
      index('Composition', key, 0, resource.eventList[j].period, CODES_TSearchParamsComposition[spComposition_period]);
      index('Composition', key, 0, resource.eventList[j].codeList[i], CODES_TSearchParamsComposition[spComposition_context]);
    end;

  for i := 0 to resource.authorList.count - 1 do
  begin
    index(context, 'Composition', key, 0, resource.authorList[i], CODES_TSearchParamsComposition[spComposition_author]);
    patientCompartment(key, resource.authorList[i]);
  end;
  for i := 0 to resource.attesterList.count - 1 do
    index(context, 'Composition', key, 0, resource.attesterList[i].party, CODES_TSearchParamsComposition[spComposition_attester]);
  for i := 0 to resource.SectionList.count - 1 do
    indexSection(resource.SectionList[i]);
end;


procedure TFhirIndexManager2.buildIndexValuesMessageHeader(key : integer; id : String; context : TFhirResource; resource: TFhirMessageHeader);
var
  i : integer;
begin

  if (resource.response <> nil) then
  begin
    index('MessageHeader', key, 0, resource.response.codeElement, CODES_TSearchParamsMessageHeader[spMessageHeader_code]);
    index('MessageHeader', key, 0, resource.response.id, CODES_TSearchParamsMessageHeader[spMessageHeader_responseid]);
  end;
  for i := 0 to resource.dataList.Count - 1 do
    index(context, 'MessageHeader', key, 0, resource.dataList[i], CODES_TSearchParamsMessageHeader[spMessageHeader_data]);
  index(context, 'MessageHeader', key, 0, resource.receiver, CODES_TSearchParamsMessageHeader[spMessageHeader_receiver]);
  index(context, 'MessageHeader', key, 0, resource.author, CODES_TSearchParamsMessageHeader[spMessageHeader_author]);
  index(context, 'MessageHeader', key, 0, resource.enterer, CODES_TSearchParamsMessageHeader[spMessageHeader_enterer]);
  index(context, 'MessageHeader', key, 0, resource.responsible, CODES_TSearchParamsMessageHeader[spMessageHeader_responsible]);
  index('MessageHeader', key, 0, resource.timestampElement, CODES_TSearchParamsMessageHeader[spMessageHeader_timestamp]);
  for i := 0 to resource.destinationList.Count - 1 do
  begin
    index('MessageHeader', key, 0, resource.destinationList[i].nameElement, CODES_TSearchParamsMessageHeader[spMessageHeader_destination]);
    index('MessageHeader', key, 0, resource.destinationList[i].endpointElement, CODES_TSearchParamsMessageHeader[spMessageHeader_destinationuri]);
    index(context, 'MessageHeader', key, 0, resource.destinationList[i].target, CODES_TSearchParamsMessageHeader[spMessageHeader_target]);
  end;
  if resource.source <> nil then
  begin
    index('MessageHeader', key, 0, resource.source.nameElement, CODES_TSearchParamsMessageHeader[spMessageHeader_source]);
    index('MessageHeader', key, 0, resource.source.endpointElement, CODES_TSearchParamsMessageHeader[spMessageHeader_sourceuri]);
  end;
  index('MessageHeader', key, 0, resource.event, CODES_TSearchParamsMessageHeader[spMessageHeader_event]);
end;

procedure TFhirIndexManager2.buildIndexValuesPractitioner(key : integer; id : String; context : TFhirResource; resource: TFhirPractitioner);
var
  i, j : integer;
begin
  for i := 0 to resource.identifierList.count - 1 do
    index('Practitioner', key, 0, resource.identifierList[i], CODES_TSearchParamsPractitioner[spPractitioner_identifier]);
  if (resource.name <> nil) then
  begin
    index('Practitioner', key, 0, resource.name, 'name', CODES_TSearchParamsPractitioner[spPractitioner_phonetic]);
    for j := 0 to resource.name.givenList.count - 1 do
      index('Practitioner', key, 0, resource.name.givenList[j], CODES_TSearchParamsPractitioner[spPractitioner_given]);
    for j := 0 to resource.name.familyList.count - 1 do
      index('Practitioner', key, 0, resource.name.familyList[j], CODES_TSearchParamsPractitioner[spPractitioner_family]);
  end;
  for i := 0 to resource.telecomList.count - 1 do
  begin
    index('Practitioner', key, 0, resource.telecomList[i], CODES_TSearchParamsPractitioner[spPractitioner_telecom]);
    if (resource.telecomList[i].system = ContactPointSystemPhone) then
      index('Practitioner', key, 0, resource.telecomList[i].value, CODES_TSearchParamsPractitioner[spPractitioner_phone]);
    if (resource.telecomList[i].system = ContactPointSystemEmail) then
      index('Practitioner', key, 0, resource.telecomList[i].value, CODES_TSearchParamsPractitioner[spPractitioner_email]);

  end;
  index('Practitioner', key, 0, resource.genderElement, CODES_TSearchParamsPractitioner[spPractitioner_gender]);
  for i := 0 to resource.addressList.Count - 1 do
    index('Practitioner', key, 0, resource.addressList[i], CODES_TSearchParamsPractitioner[spPractitioner_address]);
  index('Practitioner', key, 0, resource.communicationList, CODES_TSearchParamsPractitioner[spPractitioner_communication]);
  for j := 0 to resource.practitionerRoleList.Count -1 do
  begin
    for i := 0 to resource.practitionerRoleList[j].locationList.count - 1 do
      index(context, 'Practitioner', key, 0, resource.practitionerRoleList[j].locationList[i], CODES_TSearchParamsPractitioner[spPractitioner_location]);
    index(context, 'Practitioner', key, 0, resource.practitionerRoleList[j].managingOrganization, CODES_TSearchParamsPractitioner[spPractitioner_organization]);
    index('Practitioner', key, 0, resource.practitionerRoleList[j].roleElement, CODES_TSearchParamsPractitioner[spPractitioner_role]);
    for i := 0 to resource.practitionerRoleList[j].specialtyList.count - 1 do
      index('Practitioner', key, 0, resource.practitionerRoleList[j].specialtyList[i], CODES_TSearchParamsPractitioner[spPractitioner_specialty]);
  end;
end;



procedure TFhirIndexManager2.buildIndexValuesOrganization(key : integer;  id : String; context : TFhirResource; resource: TFhirOrganization);
var
  i : integer;
begin
  index('Organization', key, 0, resource.active, CODES_TSearchParamsOrganization[spOrganization_active]);
  index('Organization', key, 0, resource.NameElement, CODES_TSearchParamsOrganization[spOrganization_name]);
  index('Organization', key, 0, EncodeNYSIISValue(resource.nameElement), CODES_TSearchParamsOrganization[spOrganization_phonetic]);
  index('Organization', key, 0, resource.type_, CODES_TSearchParamsOrganization[spOrganization_type]);
  for i := 0 to resource.addressList.Count - 1 Do
    index('Organization', key, 0, resource.addressList[i], CODES_TSearchParamsOrganization[spOrganization_address]);
  for i := 0 to resource.IdentifierList.Count - 1 Do
    if resource.IdentifierList[i] <> nil then
      index('Organization', key, 0, resource.IdentifierList[i], CODES_TSearchParamsOrganization[spOrganization_identifier]);
  index(context, 'Organization', key, 0, resource.partOf, CODES_TSearchParamsOrganization[spOrganization_partOf]);

end;

procedure TFhirIndexManager2.buildIndexValuesGroup(key : integer;  id : String; context : TFhirResource; resource: TFhirGroup);
var
  i, p : integer;
begin
  index('Group', key, 0, resource.actual, CODES_TSearchParamsGroup[spGroup_actual]);
  index('Group', key, 0, resource.code, CODES_TSearchParamsGroup[spGroup_code]);
  index('Group', key, 0, resource.type_Element, CODES_TSearchParamsGroup[spGroup_type]);
  index('Group', key, 0, resource.identifierList, CODES_TSearchParamsGroup[spGroup_identifier]);

  for i := 0 to resource.memberList.Count - 1 Do
    index(context, 'Group', key, 0, resource.memberList[i].entity, CODES_TSearchParamsGroup[spGroup_member]);

  for i := 0 to resource.characteristicList.Count - 1 Do
  begin
    p := index('Group', key, 0, CODES_TSearchParamsGroup[spGroup_characteristic]);
    index('Group', key, p, resource.characteristicList[i].code, CODES_TSearchParamsGroup[spGroup_characteristic]);
    index('Group', key, 0, resource.characteristicList[i].exclude, CODES_TSearchParamsGroup[spGroup_exclude]);
    if resource.characteristicList[i].value is TFhirBoolean then
      index('Group', key, p, TFhirBoolean(resource.characteristicList[i].value).value, CODES_TSearchParamsGroup[spGroup_value])
    else if resource.characteristicList[i].value is TFhirString then
      index('Group', key, p, TFhirString(resource.characteristicList[i].value), CODES_TSearchParamsGroup[spGroup_value])
    else if resource.characteristicList[i].value is TFhirCodeableConcept then
      index('Group', key, p, TFhirCodeableConcept(resource.characteristicList[i].value), CODES_TSearchParamsGroup[spGroup_value])
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesObservation(key : integer;  id : String; context : TFhirResource; resource: TFhirObservation);
var
  i, p : integer;
  procedure indexValue(value : TFhirType; prefix : string);
  begin
    if value is TFhirQuantity then
      index('Observation', key, 0, TFhirQuantity(value), prefix+CODES_TSearchParamsObservation[spObservation_Valuequantity])
    else if value is TFhirSampledData then
      index('Observation', key, 0, TFhirSampledData(value), prefix+CODES_TSearchParamsObservation[spObservation_valuequantity])
    else if value is TFhirRatio then
      index('Observation', key, 0, TFhirRatio(value), prefix+CODES_TSearchParamsObservation[spObservation_valuequantity])
    else if value is TFhirCodeableConcept then
      index('Observation', key, 0, TFhirCodeableConcept(value), prefix+CODES_TSearchParamsObservation[spObservation_valueconcept])
    else if value is TFhirPeriod then
      index('Observation', key, 0, TFhirPeriod(value), prefix+CODES_TSearchParamsObservation[spObservation_valuedate])
    else if value is TFhirString then
      index('Observation', key, 0, TFhirString(value), prefix+CODES_TSearchParamsObservation[spObservation_valuestring]);
  end;
begin
  index('Observation', key, 0, resource.code, CODES_TSearchParamsObservation[spObservation_code]);
  index('Observation', key, 0, resource.category, CODES_TSearchParamsObservation[spObservation_category]);


  index(context, 'Observation', key, 0, resource.subject, CODES_TSearchParamsObservation[spObservation_subject]);
  index(context, 'Observation', key, 0, resource.subject, CODES_TSearchParamsObservation[spObservation_patient], 'Patient');
  patientCompartment(key, resource.subject);
  if resource.effective is TFhirDateTime then
    index('Observation', key, 0, TFhirDateTime(resource.effective), CODES_TSearchParamsObservation[spObservation_date])
  else if resource.effective is TFhirPeriod then
    index('Observation', key, 0, TFhirPeriod(resource.effective), CODES_TSearchParamsObservation[spObservation_date]);
  index('Observation', key, 0, resource.statusElement, CODES_TSearchParamsObservation[spObservation_status]);
  for i := 0 to resource.performerList.Count - 1 Do
    index(context, 'Observation', key, 0, resource.performerList[i], CODES_TSearchParamsObservation[spObservation_performer]);
  index(context, 'Observation', key, 0, resource.specimen, CODES_TSearchParamsObservation[spObservation_specimen]);

  indexValue(resource.value, '');
  index(context, 'Observation', key, 0, resource.encounter, CODES_TSearchParamsObservation[spObservation_encounter]);
  index('Observation', key, 0, resource.identifierList, CODES_TSearchParamsObservation[spObservation_identifier]);
  index('Observation', key, 0, resource.dataAbsentReasonElement, CODES_TSearchParamsObservation[spObservation_dataabsentreason]);
  index(context, 'Observation', key, 0, resource.deviceElement, CODES_TSearchParamsObservation[spObservation_device]);


  for i := 0 to resource.relatedList.Count - 1 Do
  begin
    p := index('Observation', key, 0, CODES_TSearchParamsObservation[spObservation_related]);
    index('Observation', key, p, resource.relatedList[i].type_Element, CODES_TSearchParamsObservation[spObservation_relatedtype]);
    index(context, 'Observation', key, p, resource.relatedList[i].target, CODES_TSearchParamsObservation[spObservation_relatedtarget]);
  end;
  for i := 0 to resource.componentList.Count - 1 Do
  begin
    p := 0; //todo index('Observation', key, 0, CODES_TSearchParamsObservation[spObservation_component]);
    index('Observation', key, p, resource.componentList[i].code, CODES_TSearchParamsObservation[spObservation_code]);
    index('Observation', key, 0, resource.componentList[i].dataAbsentReasonElement, CODES_TSearchParamsObservation[spObservation_componentdataabsentreason]);
    indexValue(resource.componentList[i].value, 'component-');
  end;

//  TODO:    spObservation_Code_value_x, { Both code and one of the value parameters }
//  TODO:    spObservation_CMPOnnet_Code_value_x, { Both code and one of the value parameters }

end;


procedure TFhirIndexManager2.buildIndexValuesStructureDefinition(key : integer; id : String; context : TFhirResource; resource: TFHirStructureDefinition);
var
  i, j : integer;
  procedure indexElement(element : TFhirElementDefinition);
  begin
    index('StructureDefinition', key, 0, element.path, CODES_TSearchParamsStructureDefinition[spStructureDefinition_path]);
    if (element.base <> nil) then
      index('StructureDefinition', key, 0, element.base.path, CODES_TSearchParamsStructureDefinition[spStructureDefinition_basepath]);
    if (element.binding <> nil) then
      if element.binding.valueSet is TFhirUri then
        index('StructureDefinition', key, 0, TFhirUri(element.binding.valueset), CODES_TSearchParamsStructureDefinition[spStructureDefinition_Valueset])
      else
        index(context, 'StructureDefinition', key, 0, TFhirReference(element.binding.valueset), CODES_TSearchParamsStructureDefinition[spStructureDefinition_valueset]);
  end;
begin
  index('StructureDefinition', key, 0, resource.identifierList, CODES_TSearchParamsStructureDefinition[spStructureDefinition_identifier]);
  index('StructureDefinition', key, 0, resource.urlElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_url]);
  index('StructureDefinition', key, 0, resource.baseElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_base]);
  index('StructureDefinition', key, 0, resource.constrainedTypeElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_type]);
  index('StructureDefinition', key, 0, resource.nameElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_name]);
  index('StructureDefinition', key, 0, resource.useContextList, CODES_TSearchParamsStructureDefinition[spStructureDefinition_context]);
  index('StructureDefinition', key, 0, resource.contextTypeElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_contexttype]);
  for i := 0 to resource.contextList.Count - 1 do
    index('StructureDefinition', key, 0, resource.contextList[i], CODES_TSearchParamsStructureDefinition[spStructureDefinition_extcontext]);

  index('StructureDefinition', key, 0, resource.dateElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_date]);
  index('StructureDefinition', key, 0, resource.abstractElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_abstract]);
  index('StructureDefinition', key, 0, resource.descriptionElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_description]);
  index('StructureDefinition', key, 0, resource.experimentalElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_experimental]);
  index('StructureDefinition', key, 0, resource.displayElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_display]);
  index('StructureDefinition', key, 0, resource.statusElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_status]);
  index('StructureDefinition', key, 0, resource.versionElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_version]);
  index('StructureDefinition', key, 0, resource.publisherElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_publisher]);
  for i := 0 to resource.CodeList.count - 1 Do
    index('StructureDefinition', key, 0, resource.CodeList[i], CODES_TSearchParamsStructureDefinition[spStructureDefinition_code]);
  index('StructureDefinition', key, 0, resource.kindElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_kind]);
  if resource.snapshot <> nil then
    for j := 0 to resource.snapshot.elementList.Count - 1 do
      indexElement(resource.snapshot.elementList[j]);
  if resource.differential <> nil then
    for j := 0 to resource.differential.elementList.Count - 1 do
      indexElement(resource.differential.elementList[j]);
end;


procedure TFhirIndexManager2.buildIndexValuesParameters(key : integer; id : String; context : TFhirResource; resource: TFhirParameters);
begin
end;


procedure TFhirIndexManager2.buildIndexValuesPatient(key : integer; id : String; context : TFhirResource; resource: TFhirPatient);
var
  i, j : integer;
  ex : TFhirExtension;
begin
  for i := 0 to resource.IdentifierList.Count - 1 Do
    if resource.IdentifierList[i] <> nil then
      index('Patient', key, 0, resource.IdentifierList[i], CODES_TSearchParamsPatient[spPatient_identifier]);
  for i := 0 to resource.nameList.count - 1 do
  begin
    index('Patient', key, 0, resource.nameList[i], 'name', CODES_TSearchParamsPatient[spPatient_phonetic]);
    for j := 0 to resource.nameList[i].givenList.count - 1 do
      index('Patient', key, 0, resource.nameList[i].givenList[j], CODES_TSearchParamsPatient[spPatient_given]);
    for j := 0 to resource.nameList[i].familyList.count - 1 do
      index('Patient', key, 0, resource.nameList[i].familyList[j], CODES_TSearchParamsPatient[spPatient_family]);
  end;

  for i := 0 to resource.telecomList.Count - 1 do
  begin
    index('Patient', key, 0, resource.telecomList[i], CODES_TSearchParamsPatient[spPatient_telecom]);
    if resource.telecomList[i].system = ContactPointSystemPhone then
      index('Patient', key, 0, resource.telecomList[i].valueElement, CODES_TSearchParamsPatient[spPatient_phone]);
    if resource.telecomList[i].system = ContactPointSystemEmail then
      index('Patient', key, 0, resource.telecomList[i].valueElement, CODES_TSearchParamsPatient[spPatient_email]);

  end;
  for i := 0 to resource.AddressList.Count - 1 Do
    index('Patient', key, 0, resource.AddressList[i], CODES_TSearchParamsPatient[spPatient_address]);
  index('Patient', key, 0, resource.genderElement, CODES_TSearchParamsPatient[spPatient_gender]);
  if (resource.deceased is TFhirBoolean) then
    index('Patient', key, 0, resource.deceased as TFhirBoolean, CODES_TSearchParamsPatient[spPatient_deceased])
  else if (resource.deceased is TFhirDateTime) then
    index('Patient', key, 0, resource.deceased as TFhirDateTime, CODES_TSearchParamsPatient[spPatient_deathdate]);
  for i := 0 to resource.communicationList.Count - 1 Do
    index('Patient', key, 0, resource.communicationList[i].language, CODES_TSearchParamsPatient[spPatient_language]);

  index('Patient', key, 0, resource.birthDateElement, CODES_TSearchParamsPatient[spPatient_birthdate]);

  index(context, 'Patient', key, 0, resource.managingOrganization, CODES_TSearchParamsPatient[spPatient_organization]);
  for i := 0 to resource.careProviderList.Count - 1 Do
    index(context, 'Patient', key, 0, resource.careProviderList[i], CODES_TSearchParamsPatient[spPatient_careprovider]);

  for i := 0 to resource.link_List.count - 1 do
    index(context, 'Patient', key, 0, resource.link_List[i].other, CODES_TSearchParamsPatient[spPatient_link]);

  index('Patient', key, 0, resource.activeElement, CODES_TSearchParamsPatient[spPatient_active]);

  if (resource.animal <> nil) then
  begin
    index('Patient', key, 0, resource.animal.species, CODES_TSearchParamsPatient[spPatient_animalspecies]);
    index('Patient', key, 0, resource.animal.breed, CODES_TSearchParamsPatient[spPatient_animalbreed]);
  end;
  patientCompartment(key, 'patient', id);

  // DAF / HL7 extensions:
  if resource.multipleBirth is TFhirBoolean then
    index('Patient', key, 0, BoolToStr((resource.multipleBirth as TFhirBoolean).value, true).ToLower, 'birthOrderBoolean')
  else if resource.multipleBirth is TFhirInteger then
    index('Patient', key, 0, (resource.multipleBirth as TFhirInteger).value, 'birthOrderBoolean');
  for ex in resource.extensionList do
  begin
    if ex.url = 'http://hl7.org/fhir/StructureDefinition/patient-mothersMaidenName' then
      index('Patient', key, 0, ex.value as TFhirString, 'mothersMaidenName');
    if ex.url = 'http://hl7.org/fhir/StructureDefinition/us-core-race' then
      if ex.value is TFhirCodeableConcept then
        index('Patient', key, 0, ex.value as TFhirCodeableConcept, CODES_TSearchParamsPatient[spPatient_race]);
    if ex.url = 'http://hl7.org/fhir/StructureDefinition/us-core-ethnicity' then
      index('Patient', key, 0, ex.value as TFhirCodeableConcept, CODES_TSearchParamsPatient[spPatient_ethnicity]);
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesDiagnosticReport(key : integer; id : String; context : TFhirResource; resource: TFhirDiagnosticReport);
var
  i, j, k : integer;
begin
  index('DiagnosticReport', key, 0, resource.statusElement, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_status]);
  index('DiagnosticReport', key, 0, resource.identifierList, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_identifier]);

  index(context, 'DiagnosticReport', key, 0, resource.requestList, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_request]);

  index('DiagnosticReport', key, 0, resource.code, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_code]);
  for j := 0 to resource.resultList.count - 1 do
  begin
    index(context, 'DiagnosticReport', key, 0, resource.resultList[j], CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_result]);
  end;

  index(context, 'DiagnosticReport', key, 0, resource.subject, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_subject]);
  index(context, 'DiagnosticReport', key, 0, resource.subject, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_patient], 'Patient');
  patientCompartment(key, resource.subject);
  index(context, 'DiagnosticReport', key, 0, resource.performer, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_performer]);

  index(context, 'DiagnosticReport', key, 0, resource.encounter, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_encounter]);

  index('DiagnosticReport', key, 0, resource.issuedElement, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_issued]);
  index('DiagnosticReport', key, 0, resource.category, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_category]);
  if resource.effective is TFhirPeriod then
    index('DiagnosticReport', key, 0, TFhirPeriod(resource.effective), CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_date])
  else
    index('DiagnosticReport', key, 0, TFhirDateTime(resource.effective), CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_date]);

  for i := 0 to resource.specimenList.Count - 1 Do
    index(context, 'DiagnosticReport', key, 0, resource.specimenList[i], CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_specimen]);

  for i := 0 to resource.imageList.Count - 1 Do
    index(context, 'DiagnosticReport', key, 0, resource.imageList[i].link_, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_image]);
  for i := 0 to resource.codedDiagnosisList.Count - 1 Do
    index('DiagnosticReport', key, 0, resource.codedDiagnosisList[i], CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_diagnosis]);
end;

procedure TFhirIndexManager2.buildIndexValuesDiagnosticOrder(key : integer; id : String; context : TFhirResource; resource: TFhirDiagnosticOrder);
var
  i, j, k, p, p1 : integer;
begin
  index(context, 'DiagnosticOrder', key, 0, resource.subject, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_subject]);
  index(context, 'DiagnosticOrder', key, 0, resource.subject, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_patient]);
  patientCompartment(key, resource.subject);
  index(context, 'DiagnosticOrder', key, 0, resource.orderer, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_orderer]);
  index(context, 'DiagnosticOrder', key, 0, resource.Encounter, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_encounter]);
  for i := 0 to resource.specimenList.Count - 1 do
    index(context, 'DiagnosticOrder', key, 0, resource.specimenList[i], CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_specimen]);
  index('DiagnosticOrder', key, 0, resource.statusElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_status]);
  for i := 0 to resource.identifierList.Count - 1 do
    index('DiagnosticOrder', key, 0, resource.identifierList[i], CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_identifier]);

  for j := 0 to resource.eventList.count - 1 do
  begin
    p := index('DiagnosticOrder', key, 0, 'event');
    index(context, 'DiagnosticOrder', key, p, resource.eventList[j].actor, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_actor]);
    index('DiagnosticOrder', key, p, resource.eventList[j].statusElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_eventstatus]);
    index('DiagnosticOrder', key, p, resource.eventList[j].dateTimeElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_eventdate]);
  end;

  for k := 0 to resource.itemList.count - 1 do
  begin
    p := index('DiagnosticOrder', key, 0, 'item');
    index('DiagnosticOrder', key, p, resource.itemList[k].code, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_code]);
    for i := 0 to resource.itemList[k].specimenList.Count - 1 do
      index(context, 'DiagnosticOrder', key, 0, resource.itemList[k].specimenList[i], CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_specimen]);

    index('DiagnosticOrder', key, p, resource.itemList[k].statusElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_itemstatus]);
    for j := 0 to resource.itemList[k].eventList.count - 1 do
    begin
      p1 := index('DiagnosticOrder', key, p, 'item-event');
      index(context, 'DiagnosticOrder', key, p1, resource.itemList[k].eventList[j].actor, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_actor]);
      index('DiagnosticOrder', key, p1, resource.itemList[k].eventList[j].statusElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_itempaststatus]);
      index('DiagnosticOrder', key, p1, resource.itemList[k].eventList[j].dateTimeElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_itemdate]);
    end;
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesValueset(key : integer; id : String; context : TFhirResource; resource: TFhirValueset);
  procedure indexConcepts(list : TFhirValueSetCodeSystemConceptList);
  var
    i : integer;
  begin
    for i := 0 to list.Count - 1 do
    begin
      index('ValueSet', key, 0, list[i].codeElement, CODES_TSearchParamsValueSet[spValueSet_code]);
      indexConcepts(list[i].conceptList);
    end;
  end;
var
  i : integer;
begin
  if (resource.codeSystem <> nil) then
  begin
    index('ValueSet', key, 0, resource.codeSystem.systemElement, CODES_TSearchParamsValueSet[spValueSet_system]);
    indexConcepts(resource.codeSystem.conceptList);
  end;

  index('ValueSet', key, 0, resource.identifierElement, CODES_TSearchParamsValueSet[spValueSet_identifier]);
  index('ValueSet', key, 0, resource.versionElement, CODES_TSearchParamsValueSet[spValueSet_version]);
  index('ValueSet', key, 0, resource.nameElement, CODES_TSearchParamsValueSet[spValueSet_name]);

  index('ValueSet', key, 0, resource.statusElement, CODES_TSearchParamsValueSet[spValueSet_status]);
  index('ValueSet', key, 0, resource.urlElement, CODES_TSearchParamsValueSet[spValueSet_url]);
  index('ValueSet', key, 0, resource.useContextList, CODES_TSearchParamsValueSet[spValueSet_context]);

  index('ValueSet', key, 0, resource.dateElement, CODES_TSearchParamsValueSet[spValueSet_date]);
  index('ValueSet', key, 0, resource.publisherElement, CODES_TSearchParamsValueSet[spValueSet_publisher]);
  index('ValueSet', key, 0, resource.descriptionElement, CODES_TSearchParamsValueSet[spValueSet_description]);
  if resource.compose <> nil then
  begin
    for i := 0 to resource.compose.importList.Count - 1 do
      index('ValueSet', key, 0, resource.compose.importList[i], CODES_TSearchParamsValueSet[spValueSet_reference]);
    for i := 0 to resource.compose.includeList.Count - 1 do
      index('ValueSet', key, 0, resource.compose.includeList[i].systemElement, CODES_TSearchParamsValueSet[spValueSet_reference]);
    for i := 0 to resource.compose.excludeList.Count - 1 do
      index('ValueSet', key, 0, resource.compose.excludeList[i].systemElement, CODES_TSearchParamsValueSet[spValueSet_reference]);
  end;
  if (resource.expansion <> nil) then
    index('ValueSet', key, 0, resource.expansion.identifier, CODES_TSearchParamsValueSet[spValueSet_expansion]);
end;

procedure TFhirIndexManager2.buildIndexValuesConceptMap(key : integer; id : String; context : TFhirResource; resource: TFhirConceptMap);
var
  i, j, k, l : integer;
  list : TFhirConceptMapElementList;
begin
  index('ConceptMap', key, 0, resource.identifierElement, CODES_TSearchParamsConceptMap[spConceptMap_identifier]);
  index('ConceptMap', key, 0, resource.versionElement, CODES_TSearchParamsConceptMap[spConceptMap_version]);
  index('ConceptMap', key, 0, resource.nameElement, CODES_TSearchParamsConceptMap[spConceptMap_name]);
  index('ConceptMap', key, 0, resource.statusElement, CODES_TSearchParamsConceptMap[spConceptMap_status]);
  index('ConceptMap', key, 0, resource.dateElement, CODES_TSearchParamsConceptMap[spConceptMap_date]);
  index('ConceptMap', key, 0, resource.publisherElement, CODES_TSearchParamsConceptMap[spConceptMap_publisher]);
  index('ConceptMap', key, 0, resource.descriptionElement, CODES_TSearchParamsConceptMap[spConceptMap_description]);

  index('ConceptMap', key, 0, resource.useContextList, CODES_TSearchParamsConceptMap[spConceptMap_context]);
  if resource.source is TFhirReference then
    index(context, 'ConceptMap', key, 0, TFhirReference(resource.source), CODES_TSearchParamsConceptMap[spConceptMap_sourceuri])
  else
    index('ConceptMap', key, 0, TFhirURI(resource.source), CODES_TSearchParamsConceptMap[spConceptMap_source]);
  if resource.target is TFhirReference then
    index(context, 'ConceptMap', key, 0, TFhirReference(resource.target), CODES_TSearchParamsConceptMap[spConceptMap_target])
  else
    index('ConceptMap', key, 0, TFhirURI(resource.target), CODES_TSearchParamsConceptMap[spConceptMap_target]);
  list := resource.elementList;
  index('ConceptMap', key, 0, resource.urlElement, CODES_TSearchParamsConceptMap[spConceptMap_url]);


  for i := 0 to list.count - 1 do
  begin
    index('ConceptMap', key, 0, list[i].systemElement, CODES_TSearchParamsConceptMap[spConceptMap_sourcesystem]);
    index('ConceptMap', key, 0, list[i].code, CODES_TSearchParamsConceptMap[spConceptMap_sourcecode]);
    k := 0; // todo index('ConceptMap', key, 0, CODES_TSearchParamsConceptMap[spConceptMap_target]);
    for j := 0 to list[i].targetList.Count - 1 do
    begin
      for l := 0 to  list[i].targetList[j].dependsOnList.Count - 1 do
        index('ConceptMap', key, k, list[i].targetList[j].dependsOnList[l].codeElement, CODES_TSearchParamsConceptMap[spConceptMap_dependson]);
      index('ConceptMap', key, k, list[i].targetList[j].systemElement, CODES_TSearchParamsConceptMap[spConceptMap_targetsystem]);
      index('ConceptMap', key, k, list[i].targetList[j].codeElement, CODES_TSearchParamsConceptMap[spConceptMap_targetcode]);
      for l := 0 to  list[i].targetList[j].productList.Count - 1 do
        index('ConceptMap', key, k, list[i].targetList[j].productList[l].codeElement, CODES_TSearchParamsConceptMap[spConceptMap_product]);
    end;
  end;
end;




procedure TFhirIndexManager2.buildIndexValuesDevice(key : integer; id : String; context : TFhirResource; resource: TFhirDevice);
var
  i : integer;
begin
  for i  := 0 to resource.identifierList.count - 1 do
    index('Device', key, 0, resource.identifierList[i], CODES_TSearchParamsDevice[spDevice_identifier]);
  index(context, 'Device', key, 0, resource.location, CODES_TSearchParamsDevice[spDevice_location]);
  index('Device', key, 0, resource.manufacturerElement, CODES_TSearchParamsDevice[spDevice_manufacturer]);
  index('Device', key, 0, resource.modelElement, CODES_TSearchParamsDevice[spDevice_model]);
  index(context, 'Device', key, 0, resource.owner, CODES_TSearchParamsDevice[spDevice_organization]);
  index(context, 'Device', key, 0, resource.patient, CODES_TSearchParamsDevice[spDevice_patient]);
  index('Device', key, 0, resource.udiElement, CODES_TSearchParamsDevice[spDevice_udi]);
  index('Device', key, 0, resource.urlElement, CODES_TSearchParamsDevice[spDevice_url]);
  index('Device', key, 0, resource.type_, CODES_TSearchParamsDevice[spDevice_type]);
  patientCompartment(key, resource.patient);
end;



procedure TFhirIndexManager2.buildIndexValuesAuditEvent(key : integer; id : String; context : TFhirResource; resource: TFhirAuditEvent);
var
  i, j : integer;
begin
  index('AuditEvent', key, 0, resource.event.type_, CODES_TSearchParamsAuditEvent[spAuditEvent_type]);
  index('AuditEvent', key, 0, resource.event.actionElement, CODES_TSearchParamsAuditEvent[spAuditEvent_action]);
  index('AuditEvent', key, 0, resource.event.dateTimeElement, CODES_TSearchParamsAuditEvent[spAuditEvent_date]);
  for i := 0 to resource.event.subTypeList.count - 1 do
    index('AuditEvent', key, 0, resource.event.subtypeList[i], CODES_TSearchParamsAuditEvent[spAuditEvent_subtype]);

  for i := 0 to resource.participantList.count - 1 do
  begin
    index(context, 'AuditEvent', key, 0, resource.participantList[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_participant]);
    index(context, 'AuditEvent', key, 0, resource.participantList[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_patient], 'Patient');
    index('AuditEvent', key, 0, resource.participantList[i].userIdElement, CODES_TSearchParamsAuditEvent[spAuditEvent_user]);
    index('AuditEvent', key, 0, resource.participantList[i].altIdElement, CODES_TSearchParamsAuditEvent[spAuditEvent_altid]);
    index('AuditEvent', key, 0, resource.participantList[i].nameElement, CODES_TSearchParamsAuditEvent[spAuditEvent_name]);
    for j := 0 to resource.participantList[i].policyList.Count - 1 do
      index('AuditEvent', key, 0, resource.participantList[i].policyList[j], CODES_TSearchParamsAuditEvent[spAuditEvent_policy]);
    if resource.participantList[i].network <> nil then
      index('AuditEvent', key, 0, resource.participantList[i].network.addressElement, CODES_TSearchParamsAuditEvent[spAuditEvent_address]);
  end;

  if resource.source <> nil Then
  begin
    index('AuditEvent', key, 0, resource.source.identifierElement, CODES_TSearchParamsAuditEvent[spAuditEvent_source]);
    index('AuditEvent', key, 0, resource.source.siteElement, CODES_TSearchParamsAuditEvent[spAuditEvent_site]);
  end;

  for i := 0 to resource.object_List.count - 1 do
  begin
    index('AuditEvent', key, 0, resource.object_List[i].type_, CODES_TSearchParamsAuditEvent[spAuditEvent_objecttype]);
    index('AuditEvent', key, 0, resource.object_List[i].identifier, CODES_TSearchParamsAuditEvent[spAuditEvent_identity]);
    index(context, 'AuditEvent', key, 0, resource.object_List[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_reference]);
    patientCompartment(key, resource.object_List[i].reference);
    index('AuditEvent', key, 0, resource.object_List[i].nameElement, CODES_TSearchParamsAuditEvent[spAuditEvent_desc]);
  end;
end;




procedure TFhirIndexManager2.buildIndexValuesCondition(key : integer; id : String; context : TFhirResource; resource: TFhirCondition);
var
  i : integer;
begin
  index('Condition', key, 0, resource.code, CODES_TSearchParamsCondition[spCondition_code]);
  index('Condition', key, 0, resource.clinicalStatusElement, CODES_TSearchParamsCondition[spCondition_clinicalstatus]);
  index('Condition', key, 0, resource.severity, CODES_TSearchParamsCondition[spCondition_severity]);
  index('Condition', key, 0, resource.category, CODES_TSearchParamsCondition[spCondition_category]);
  index('Condition', key, 0, resource.identifierList, CODES_TSearchParamsCondition[spCondition_identifier]);
  index(context, 'Condition', key, 0, resource.patient, CODES_TSearchParamsCondition[spCondition_patient]);
  patientCompartment(key, resource.patient);

  index(context, 'Condition', key, 0, resource.Encounter, CODES_TSearchParamsCondition[spCondition_encounter]);
  index(context, 'Condition', key, 0, resource.asserter, CODES_TSearchParamsCondition[spCondition_asserter]);
  if (resource.onsetElement is TFHIRDateTime) then
    index('Condition', key, 0, resource.onsetElement as TFHIRDateTime, CODES_TSearchParamsCondition[spCondition_onset])
  else if (resource.onsetElement is TFHIRPeriod) then
    index('Condition', key, 0, resource.onsetElement as TFHIRPeriod, CODES_TSearchParamsCondition[spCondition_onset])
  else if (resource.onsetElement is TFhirQuantity) then
    index('Condition', key, 0, resource.onsetElement as TFhirQuantity, CODES_TSearchParamsCondition[spCondition_age])
  else if (resource.onsetElement is TFhirRange) then
    index('Condition', key, 0, resource.onsetElement as TFhirRange, CODES_TSearchParamsCondition[spCondition_onsetinfo])
  else if (resource.onsetElement is TFhirString) then
    index('Condition', key, 0, resource.onsetElement as TFhirString, CODES_TSearchParamsCondition[spCondition_onsetinfo]);


  index('Condition', key, 0, resource.dateRecordedElement, CODES_TSearchParamsCondition[spCondition_daterecorded]);
// todo  index('Condition', key, 0, resource.onset, CODES_TSearchParamsCondition[spCondition_onset]);
  for i := 0 to resource.evidenceList.count - 1 do
    index('Condition', key, 0, resource.evidenceList[i].code, CODES_TSearchParamsCondition[spCondition_evidence]);
  if resource.stage <> nil then
    index('Condition', key, 0, resource.stage.summary, CODES_TSearchParamsCondition[spCondition_stage]);

  index('Condition', key, 0, resource.bodySiteList, CODES_TSearchParamsCondition[spCondition_bodysite]);

    // DAF:
    index('Condition', key, 0, resource.identifierList, CODES_TSearchParamsCondition[spCondition_identifier]);
end;


procedure TFhirIndexManager2.buildIndexValuesOperationOutcome(key : integer; id : String; context : TFhirResource; resource: TFhirOperationOutcome);
begin
end;



procedure TFhirIndexManager2.buildIndexValuesBinary(key : integer; id : String; context : TFhirResource; resource: TFhirBinary);
begin

  index('Binary', key, 0, resource.contentType, CODES_TSearchParamsBinary[spBinary_contentType]);

end;

procedure TFhirIndexManager2.buildIndexValuesProvenance(key : integer; id : String; context : TFhirResource; resource: TFhirProvenance);
var
  i : integer;
begin
  for i := 0 to resource.targetList.Count - 1 do
  begin
    index(context, 'Provenance', key, 0, resource.targetList[i], CODES_TSearchParamsProvenance[spProvenance_target]);
    index(context, 'Provenance', key, 0, resource.targetList[i], CODES_TSearchParamsProvenance[spProvenance_patient], 'Patient');
  end;
  if (resource.period <> nil) then
  begin
    index('Provenance', key, 0, resource.period.startElement, CODES_TSearchParamsProvenance[spProvenance_start]);
    index('Provenance', key, 0, resource.period.end_Element, CODES_TSearchParamsProvenance[spProvenance_end]);
  end;
  index(context, 'Provenance', key, 0, resource.location, CODES_TSearchParamsProvenance[spProvenance_location]);

  for i := 0 to resource.signatureList.Count - 1 do
    index('Provenance', key, 0, resource.signatureList[i].type_List, CODES_TSearchParamsProvenance[spProvenance_sigtype]);

  for i := 0 to resource.agentList.Count - 1 do
  begin
    index(context, 'Provenance', key, 0, resource.agentList[i].actor, CODES_TSearchParamsProvenance[spProvenance_agent]);
    index('Provenance', key, 0, resource.agentList[i].userId, CODES_TSearchParamsProvenance[spProvenance_userid]);
  end;

  for i := 0 to resource.entityList.Count - 1 do
  begin
    index('Provenance', key, 0, resource.entityList[i].referenceElement, CODES_TSearchParamsProvenance[spProvenance_entity]);
    index('Provenance', key, 0, resource.entityList[i].type_Element, CODES_TSearchParamsProvenance[spProvenance_Entitytype]);
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesMedication(key : integer; id : String; context : TFhirResource; resource: TFhirMedication);
var
  i : integer;
begin
  index('Medication', key, 0, resource.code, CODES_TSearchParamsMedication[spMedication_code]);
  index(context, 'Medication', key, 0, resource.manufacturer, CODES_TSearchParamsMedication[spMedication_manufacturer]);
  if (resource.package <> nil) then
  begin
    index('Medication', key, 0, resource.package.container, CODES_TSearchParamsMedication[spMedication_container]);
    for i := 0 to resource.package.contentList.count - 1 do
      index(context, 'Medication', key, 0, resource.package.contentList[i].item, CODES_TSearchParamsMedication[spMedication_content]);
  end;
  if (resource.product <> nil) then
  begin
    index('Medication', key, 0, resource.product.form, CODES_TSearchParamsMedication[spMedication_form]);
    for i := 0 to resource.product.ingredientList.count - 1 do
      index(context, 'Medication', key, 0, resource.product.ingredientList[i].item, CODES_TSearchParamsMedication[spMedication_Ingredient])
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesMedicationAdministration(key : integer; id : String; context : TFhirResource; resource: TFhirMedicationAdministration);
var
  i : integer;
begin
  index(context, 'MedicationAdministration', key, 0, resource.patient, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_patient]);
  patientCompartment(key, resource.patient);
  index(context, 'MedicationAdministration', key, 0, resource.Encounter, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_encounter]);
  index(context, 'MedicationAdministration', key, 0, resource.prescription, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_prescription]);
  index(context, 'MedicationAdministration', key, 0, resource.practitioner, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_practitioner]);
  index('MedicationAdministration', key, 0, resource.wasNotGiven, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_Notgiven]);
  if resource.effectiveTime is TFhirPeriod then
    index('MedicationAdministration', key, 0, TFhirPeriod(resource.effectiveTime), CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_effectivetime])
  else
    index('MedicationAdministration', key, 0, TFhirDateTime(resource.effectiveTime), CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_effectivetime]);


  index('MedicationAdministration', key, 0, resource.statusElement, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_status]);
  if resource.medication is TFhirReference then
    index(context, 'MedicationAdministration', key, 0, resource.medication as TFhirReference, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_medication])
  else
    index('MedicationAdministration', key, 0, resource.medication as TFhirCodeableConcept, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_code]);

  for i := 0 to resource.identifierList.Count - 1 do
    index('MedicationAdministration', key, 0, resource.identifierList[i], CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_identifier]);
  if resource.Encounter <> nil then
    index(context, 'MedicationAdministration', key, 0, resource.Encounter, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_encounter]);
  for i := 0 to resource.deviceList.Count - 1 do
    index(context, 'MedicationAdministration', key, 0, resource.deviceList[i], CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_device]);
end;

procedure TFhirIndexManager2.buildIndexValuesMedicationOrder(key : integer; id : String; context : TFhirResource; resource: TFhirMedicationOrder);
var
  i : integer;
begin
  index('MedicationOrder', key, 0, resource.statusElement, CODES_TSearchParamsMedicationOrder[spMedicationOrder_status]);
  index(context, 'MedicationOrder', key, 0, resource.patient, CODES_TSearchParamsMedicationOrder[spMedicationOrder_patient]);
  index(context, 'MedicationOrder', key, 0, resource.prescriber, CODES_TSearchParamsMedicationOrder[spMedicationOrder_prescriber]);
  patientCompartment(key, resource.patient);
  index(context, 'MedicationOrder', key, 0, resource.Encounter, CODES_TSearchParamsMedicationOrder[spMedicationOrder_encounter]);
  if resource.medication is TFhirReference then
    index(context, 'MedicationOrder', key, 0, resource.medication as TFhirReference, CODES_TSearchParamsMedicationOrder[spMedicationOrder_medication])
  else
    index('MedicationOrder', key, 0, resource.medication as TFhirCodeableConcept, CODES_TSearchParamsMedicationOrder[spMedicationOrder_code]);

  for i := 0 to resource.identifierList.Count - 1 do
    index('MedicationOrder', key, 0, resource.identifierList[i], CODES_TSearchParamsMedicationOrder[spMedicationOrder_identifier]);
  index('MedicationOrder', key, 0, resource.dateWrittenElement, CODES_TSearchParamsMedicationOrder[spMedicationOrder_datewritten]);
end;


procedure TFhirIndexManager2.buildIndexValuesMedicationDispense(key : integer; id : String; context : TFhirResource; resource: TFhirMedicationDispense);
var
  i, j : integer;
begin
  index('MedicationDispense', key, 0, resource.statusElement, CODES_TSearchParamsMedicationDispense[spMedicationDispense_status]);
  index(context, 'MedicationDispense', key, 0, resource.patient, CODES_TSearchParamsMedicationDispense[spMedicationDispense_patient]);
  patientCompartment(key, resource.patient);
  index(context, 'MedicationDispense', key, 0, resource.dispenser, CODES_TSearchParamsMedicationDispense[spMedicationDispense_dispenser]);
  index('MedicationDispense', key, 0, resource.identifier, CODES_TSearchParamsMedicationDispense[spMedicationDispense_identifier]);
  for i := 0 to resource.authorizingPrescriptionList.Count - 1 do
    index(context, 'MedicationDispense', key, 0, resource.authorizingPrescriptionList[i], CODES_TSearchParamsMedicationDispense[spMedicationDispense_prescription]);
  index('MedicationDispense', key, 0, resource.identifier, CODES_TSearchParamsMedicationDispense[spMedicationDispense_identifier]);
  index(context, 'MedicationDispense', key, 0, resource.destination, CODES_TSearchParamsMedicationDispense[spMedicationDispense_destination]);
  if resource.medication is TFhirReference then
    index(context, 'MedicationDispense', key, 0, resource.medication as TFhirReference, CODES_TSearchParamsMedicationDispense[spMedicationDispense_medication])
  else
    index('MedicationDispense', key, 0, resource.medication as TFhirCodeableConcept, CODES_TSearchParamsMedicationDispense[spMedicationDispense_code]);
  index(context, 'MedicationDispense', key, 0, resource.receiverList, CODES_TSearchParamsMedicationDispense[spMedicationDispense_receiver]);
  index('MedicationDispense', key, 0, resource.type_, CODES_TSearchParamsMedicationDispense[spMedicationDispense_type]);
  index('MedicationDispense', key, 0, resource.whenPreparedElement, CODES_TSearchParamsMedicationDispense[spMedicationDispense_whenprepared]);
  index('MedicationDispense', key, 0, resource.whenHandedOverElement, CODES_TSearchParamsMedicationDispense[spMedicationDispense_whenhandedover]);

  if resource.substitution <> nil then
  begin
    for i := 0 to resource.substitution.responsiblePartyList.count - 1 do
      index(context, 'MedicationDispense', key, 0, resource.substitution.responsiblePartyList[i], CODES_TSearchParamsMedicationDispense[spMedicationDispense_responsibleparty]);
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesMedicationStatement(key : integer; id : String; context : TFhirResource; resource: TFhirMedicationStatement);
var
  i : integer;
begin
  for i := 0 to resource.identifierList.Count - 1 do
    index('MedicationDispense', key, 0, resource.identifierList[i], CODES_TSearchParamsMedicationStatement[spMedicationStatement_identifier]);
  if resource.medication is TFhirReference then
    index(context, 'MedicationStatement', key, 0, resource.medication as TFhirReference, CODES_TSearchParamsMedicationStatement[spMedicationStatement_medication])
  else
    index('MedicationStatement', key, 0, resource.medication as TFhirCodeableConcept, CODES_TSearchParamsMedicationStatement[spMedicationStatement_code]);
  index(context, 'MedicationStatement', key, 0, resource.patient, CODES_TSearchParamsMedicationStatement[spMedicationStatement_patient]);

  index(context, 'MedicationStatement', key, 0, resource.informationSource, CODES_TSearchParamsMedicationStatement[spMedicationStatement_source]);
  index('MedicationStatement', key, 0, resource.statusElement, CODES_TSearchParamsMedicationStatement[spMedicationStatement_status]);

  patientCompartment(key, resource.patient);
  if resource.effectiveElement is TFhirPeriod then
    index('MedicationStatement', key, 0, TFhirPeriod(resource.effectiveElement), CODES_TSearchParamsMedicationStatement[spMedicationStatement_Effectivedate])
  else
    index('MedicationStatement', key, 0, TFhirDateTime(resource.effectiveElement), CODES_TSearchParamsMedicationStatement[spMedicationStatement_Effectivedate]);

end;

procedure TFhirIndexManager2.buildIndexValuesList(key : integer; id : String; context : TFhirResource; resource: TFhirList);
var
  i : integer;
begin
  index(context, 'List', key, 0, resource.source, CODES_TSearchParamsList[spList_source]);
  for i := 0 to resource.entryList.count - 1 do
    index(context, 'List', key, 0, resource.entryList[i].item, CODES_TSearchParamsList[spList_item]);
  index('List', key, 0, resource.emptyReason, CODES_TSearchParamsList[spList_emptyreason]);
  index('List', key, 0, resource.dateElement, CODES_TSearchParamsList[spList_date]);
  index('List', key, 0, resource.codeElement, CODES_TSearchParamsList[spList_code]);
  index(context, 'List', key, 0, resource.subject, CODES_TSearchParamsList[spList_subject]);
  index(context, 'List', key, 0, resource.subject, CODES_TSearchParamsList[spList_patient], 'Patient');
  patientCompartment(key, resource.subject);
  index(context, 'List', key, 0, resource.encounter, CODES_TSearchParamsList[spList_encounter]);
  index('List', key, 0, resource.noteElement, CODES_TSearchParamsList[spList_notes]);
  index('List', key, 0, resource.titleElement, CODES_TSearchParamsList[spList_title]);
  index('List', key, 0, resource.statusElement, CODES_TSearchParamsList[spList_status]);
end;


procedure TFhirIndexManager2.buildIndexValuesCarePlan(key: integer; id : String; context : TFhirResource; resource: TFhirCarePlan);
var
  i, j, k : integer;
begin
  index(context, 'Careplan', key, 0, resource.subject, CODES_TSearchParamsCareplan[spCareplan_patient]);
  index(context, 'Careplan', key, 0, resource.subject, CODES_TSearchParamsCareplan[spCareplan_subject]);
  patientCompartment(key, resource.subject);
  index(context, 'Careplan', key, 0, resource.addressesList, CODES_TSearchParamsCareplan[spCareplan_condition]);
  index(context, 'Careplan', key, 0, resource.goalList, CODES_TSearchParamsCareplan[spCareplan_goal]);
  index('Careplan', key, 0, resource.period, CODES_TSearchParamsCareplan[spCareplan_date]);
  for i := 0 to resource.participantList.Count - 1 do
  begin
    index(context, 'Careplan', key, 0, resource.participantList[i].member, CODES_TSearchParamsCareplan[spCareplan_participant]);
  end;
  for i := 0 to resource.activityList.Count - 1 do
  begin
    k := 0; //ci index('Careplan', key, 0, CODES_TSearchParamsCareplan[spCareplan_activity]);
    index(context, 'Careplan', key, 0, resource.activityList[i].reference, CODES_TSearchParamsCareplan[spCareplan_activityreference]);
    if resource.activityList[i].detail <> nil then
    begin
      index('Careplan', key, 0, resource.activityList[i].detail.code, CODES_TSearchParamsCareplan[spCareplan_activitycode]);
      index(context, 'Careplan', key, 0, resource.activityList[i].detail.performerList, CODES_TSearchParamsCareplan[spCareplan_performer]);
      for j := 0 to resource.activityList[i].detail.performerList.Count - 1 do
      begin
      end;
      if (resource.activityList[i].detail.scheduled is TFhirTiming) then
        index('Careplan', key, 0, TFhirTiming(resource.activityList[i].detail.scheduled), CODES_TSearchParamsCareplan[spCareplan_activitydate])
      else if (resource.activityList[i].detail.scheduled is TFhirPeriod) then
        index('Careplan', key, 0, TFhirPeriod(resource.activityList[i].detail.scheduled), CODES_TSearchParamsCareplan[spCareplan_activitydate]);
    end;
  end;
  for i := 0 to resource.relatedPlanList.Count - 1 do
  begin
    k := index('Careplan', key, 0, CODES_TSearchParamsCareplan[spCareplan_related]);
    index('Careplan', key, 0, resource.relatedPlanList[i].codeElement, CODES_TSearchParamsCareplan[spCareplan_relatedcode]);
    index(context, 'Careplan', key, 0, resource.relatedPlanList[i].planElement, CODES_TSearchParamsCareplan[spCareplan_relatedplan]);
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesImagingStudy(key: integer; id : String; context : TFhirResource; resource: TFhirImagingStudy);
var
  i, j : integer;
  series : TFhirImagingStudySeries;
  image : TFhirImagingStudySeriesInstance;
begin
  index('ImagingStudy', key, 0, resource.accession, CODES_TSearchParamsImagingStudy[spImagingStudy_accession]);
  index(context, 'ImagingStudy', key, 0, resource.patient, CODES_TSearchParamsImagingStudy[spImagingStudy_patient]);
  patientCompartment(key, resource.patient);

  index(context, 'ImagingStudy', key, 0, resource.orderList, CODES_TSearchParamsImagingStudy[spImagingStudy_order]);
  index('ImagingStudy', key, 0, resource.startedElement, CODES_TSearchParamsImagingStudy[spImagingStudy_started]);

  index('ImagingStudy', key, 0, resource.uidElement, CODES_TSearchParamsImagingStudy[spImagingStudy_study]);
  for i := 0 to resource.seriesList.count -1 do
  begin
    series := resource.seriesList[i];
    index('ImagingStudy', key, 0, series.bodySite, CODES_TSearchParamsImagingStudy[spImagingStudy_bodySite]);

    index('ImagingStudy', key, 0, series.uidElement, CODES_TSearchParamsImagingStudy[spImagingStudy_series]);
    index('ImagingStudy', key, 0, series.ModalityElement, CODES_TSearchParamsImagingStudy[spImagingStudy_modality]);
//    index('ImagingStudy', key, 0, resource., CODES_TSearchParamsImagingStudy[spImagingStudy_size]);
    for j := 0 to series.instanceList.count - 1 do
    begin
      image := series.instanceList[j];
      index('ImagingStudy', key, 0, image.uidElement, CODES_TSearchParamsImagingStudy[spImagingStudy_uid]);
      index('ImagingStudy', key, 0, image.sopclassElement, CODES_TSearchParamsImagingStudy[spImagingStudy_dicomclass]);
    end;
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesImmunization(key: integer; id : String; context : TFhirResource; resource: TFhirImmunization);
var
  i : integer;
begin
  index('Immunization', key, 0, resource.vaccineCode, CODES_TSearchParamsImmunization[spImmunization_vaccinecode]);
  index('Immunization', key, 0, resource.dateElement, CODES_TSearchParamsImmunization[spImmunization_date]);
  if resource.explanation <> nil then
  begin
    for i := 0 to resource.explanation.reasonNotGivenList.count - 1 do
      index('Immunization', key, 0, resource.explanation.reasonNotGivenList[i], CODES_TSearchParamsImmunization[spImmunization_reasonnotgiven]);

    for i := 0 to resource.explanation.reasonList.count - 1 do
      index('Immunization', key, 0, resource.explanation.reasonList[i], CODES_TSearchParamsImmunization[spImmunization_reason]);
  end;
  for i := 0 to resource.identifierList.count - 1 do
      index('Immunization', key, 0, resource.identifierList[i], CODES_TSearchParamsImmunization[spImmunization_identifier]);
  index('Immunization', key, 0, resource.lotNumberElement, CODES_TSearchParamsImmunization[spImmunization_lotnumber]);
  index('Immunization', key, 0, resource.wasNotGivenElement, CODES_TSearchParamsImmunization[spImmunization_notgiven]);
  index(context, 'Immunization', key, 0, resource.patient, CODES_TSearchParamsImmunization[spImmunization_patient]);
  index('Immunization', key, 0, resource.statusElement, CODES_TSearchParamsImmunization[spImmunization_status]);
  patientCompartment(key, resource.patient);

  index(context, 'Immunization', key, 0, resource.manufacturer, CODES_TSearchParamsImmunization[spImmunization_manufacturer]);
  index(context, 'Immunization', key, 0, resource.location, CODES_TSearchParamsImmunization[spImmunization_location]);
  index(context, 'Immunization', key, 0, resource.performer, CODES_TSearchParamsImmunization[spImmunization_performer]);
  index(context, 'Immunization', key, 0, resource.requester, CODES_TSearchParamsImmunization[spImmunization_requester]);
  for i := 0 to resource.reactionList.count - 1 do
  begin
    index(context, 'Immunization', key, 0, resource.reactionList[i].detail, CODES_TSearchParamsImmunization[spImmunization_reaction]);
    index('Immunization', key, 0, resource.reactionList[i].dateElement, CODES_TSearchParamsImmunization[spImmunization_reactiondate]);
  end;
  for i := 0 to resource.vaccinationProtocolList.count - 1 do
    index('Immunization', key, 0, resource.vaccinationProtocolList[i].doseSequenceElement, CODES_TSearchParamsImmunization[spImmunization_dosesequence]);
end;

procedure TFhirIndexManager2.buildIndexValuesOrder(key: integer; id : String; context : TFhirResource; resource: TFhirOrder);
var
  i : integer;
begin
  index('Order', key, 0, resource.dateElement, CODES_TSearchParamsOrder[spOrder_date]);
  index(context, 'Order', key, 0, resource.subject, CODES_TSearchParamsOrder[spOrder_subject]);
  index(context, 'Order', key, 0, resource.subject, CODES_TSearchParamsOrder[spOrder_patient], 'Patient');
  index('Order', key, 0, resource.identifierList, CODES_TSearchParamsOrder[spOrder_identifier]);
  patientCompartment(key, resource.subject);
  index(context, 'Order', key, 0, resource.source, CODES_TSearchParamsOrder[spOrder_source]);
  index(context, 'Order', key, 0, resource.target, CODES_TSearchParamsOrder[spOrder_target]);
  if resource.when <> nil then
  begin
    index('Order', key, 0, resource.when.code, CODES_TSearchParamsOrder[spOrder_when_code]);
    index('Order', key, 0, resource.when.schedule, CODES_TSearchParamsOrder[spOrder_when]);
  end;
  for i := 0 to resource.detailList.count - 1 do
    index(context, 'Order', key, 0, resource.detailList[i], CODES_TSearchParamsOrder[spOrder_detail]);
end;


procedure TFhirIndexManager2.buildIndexValuesOrderResponse(key: integer; id : String; context : TFhirResource; resource: TFhirOrderResponse);
var
  i : integer;
begin
  index(context, 'OrderResponse', key, 0, resource.request, CODES_TSearchParamsOrderResponse[spOrderResponse_request]);
  index('OrderResponse', key, 0, resource.identifierList, CODES_TSearchParamsOrderResponse[spOrderResponse_identifier]);
  index('OrderResponse', key, 0, resource.dateElement, CODES_TSearchParamsOrderResponse[spOrderResponse_date]);
  index(context, 'OrderResponse', key, 0, resource.who, CODES_TSearchParamsOrderResponse[spOrderResponse_who]);
  index('OrderResponse', key, 0, resource.orderStatusElement, CODES_TSearchParamsOrderResponse[spOrderResponse_code]);

  for i := 0 to resource.fulfillmentList.count - 1 do
    index(context, 'OrderResponse', key, 0, resource.fulfillmentList[i], CODES_TSearchParamsOrderResponse[spOrderResponse_fulfillment]);
end;

procedure TFhirIndexManager2.buildIndexValuesMedia(key: integer; id : String; context : TFhirResource; resource: TFhirMedia);
var
  i : integer;
begin
  index(context, 'Media', key, 0, resource.subject, CODES_TSearchParamsMedia[spMedia_subject]);
  patientCompartment(key, resource.subject);
  for i := 0 to resource.identifierList.count - 1 do
    index('Media', key, 0, resource.identifierList[i], CODES_TSearchParamsMedia[spMedia_identifier]);
  index(context, 'Media', key, 0, resource.operator, CODES_TSearchParamsMedia[spMedia_operator]);
  index('Media', key, 0, resource.type_Element, CODES_TSearchParamsMedia[spMedia_type]);
  index('Media', key, 0, resource.subtype, CODES_TSearchParamsMedia[spMedia_subtype]);
  if resource.content <> nil then
    index('Media', key, 0, resource.content.creationElement, CODES_TSearchParamsMedia[spMedia_created]);

//  index('Media', key, 0, resource.size, CODES_TSearchParamsMedia[spMedia_size]);
  index('Media', key, 0, resource.view, CODES_TSearchParamsMedia[spMedia_view]);
end;



procedure TFhirIndexManager2.buildIndexValuesFamilyMemberHistory(key: integer; id : String; context : TFhirResource; resource: TFhirFamilyMemberHistory);
var
  cond : TFhirFamilyMemberHistoryCondition;
begin
  index('FamilyMemberHistory', key, 0, resource.dateElement, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_date]);
  index('FamilyMemberHistory', key, 0, resource.genderElement, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_gender]);
  index(context, 'FamilyMemberHistory', key, 0, resource.patient, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_patient]);
  patientCompartment(key, resource.patient);
  index('FamilyMemberHistory', key, 0, resource.identifierList, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_identifier]);

  for cond in resource.conditionList do
  begin
    index('FamilyMemberHistory', key, 0, cond.code, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_code]);
    index('FamilyMemberHistory', key, 0, cond.code, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_condition]);

  // DAF:
    index('FamilyMemberHistory', key, 0, cond.code, 'familymemberhistorycondition');
  end;
  index('FamilyMemberHistory', key, 0, resource.relationship, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_relationship]);
end;




procedure TFhirIndexManager2.buildIndexValuesProcedure(key: integer; id : String; context : TFhirResource; resource: TFhirProcedure);
var
  i : integer;
begin
  index('Procedure', key, 0, resource.code, CODES_TSearchParamsProcedure[spProcedure_code]);
  if resource.performed is TFhirDateTime then
    index('Procedure', key, 0, resource.performed as TFhirDateTime, CODES_TSearchParamsProcedure[spProcedure_date])
  else
    index('Procedure', key, 0, resource.performed as TFhirPeriod, CODES_TSearchParamsProcedure[spProcedure_date]);

  index(context, 'Procedure', key, 0, resource.subject, CODES_TSearchParamsProcedure[spProcedure_subject]);
  index(context, 'Procedure', key, 0, resource.subject, CODES_TSearchParamsProcedure[spProcedure_patient], 'Patient');
  patientCompartment(key, resource.subject);

  index(context, 'Procedure', key, 0, resource.location, CODES_TSearchParamsProcedure[spProcedure_location]);
  for i := 0 to resource.performerList.Count - 1 do
    index(context, 'Procedure', key, 0, resource.performerList[i].actor, CODES_TSearchParamsProcedure[spProcedure_performer]);
  index(context, 'Procedure', key, 0, resource.encounter, CODES_TSearchParamsProcedure[spProcedure_encounter]);
  index('Procedure', key, 0, resource.identifierList, CODES_TSearchParamsProcedure[spProcedure_identifier]);
end;

procedure TFhirIndexManager2.buildIndexValuesSpecimen(key: integer; id : String; context : TFhirResource; resource: TFhirSpecimen);
var
  i, j : integer;
begin
  index(context, 'Specimen', key, 0, resource.subject, CODES_TSearchParamsSpecimen[spSpecimen_subject]);
  patientCompartment(key, resource.subject);

  index(context, 'Specimen', key, 0, resource.subject, CODES_TSearchParamsSpecimen[spSpecimen_patient], 'Patient');
  index('Specimen', key, 0, resource.accessionIdentifier, CODES_TSearchParamsSpecimen[spSpecimen_accession]);
  index('Specimen', key, 0, resource.type_, CODES_TSearchParamsSpecimen[spSpecimen_type]);
  index('Specimen', key, 0, resource.identifierList, CODES_TSearchParamsSpecimen[spSpecimen_identifier]);
  index(context, 'Specimen', key, 0, resource.parentList, CODES_TSearchParamsSpecimen[spSpecimen_parent]);
  if (resource.collection <> nil) then
  begin
    if resource.collection.collected is TFhirPeriod then
      index('Specimen', key, 0, TFhirPeriod(resource.collection.collected), CODES_TSearchParamsSpecimen[spSpecimen_collected])
    else
      index('Specimen', key, 0, TFhirDateTime(resource.collection.collected), CODES_TSearchParamsSpecimen[spSpecimen_collected]);
    index(context, 'Specimen', key, 0, resource.collection.collector, CODES_TSearchParamsSpecimen[spSpecimen_collector]);
    index('Specimen', key, 0, resource.collection.bodySite, CODES_TSearchParamsSpecimen[spSpecimen_bodysite]);
  end;
  for i := 0 to resource.containerList.Count - 1 do
  begin
    index('Specimen', key, 0, resource.containerList[i].type_, CODES_TSearchParamsSpecimen[spSpecimen_container]);
    index('Specimen', key, 0, resource.containerList[i].identifierList, CODES_TSearchParamsSpecimen[spSpecimen_containerid]);
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesImmunizationRecommendation(key: integer; id : String; context : TFhirResource; resource: TFhirImmunizationRecommendation);
var
  i,j  : integer;
begin
  index(context, 'ImmunizationRecommendation', key, 0, resource.patient, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_patient]);
  patientCompartment(key, resource.patient);


  for i := 0 to resource.identifierList.count - 1 do
    index('ImmunizationRecommendation', key, 0, resource.identifierList[i], CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_identifier]);

  for i := 0 to resource.recommendationList.count - 1 do
  begin
    index('ImmunizationRecommendation', key, 0, resource.recommendationList[i].dateElement, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_date]);
    index('ImmunizationRecommendation', key, 0, resource.recommendationList[i].vaccineCode, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_vaccinetype]);
    index('ImmunizationRecommendation', key, 0, resource.recommendationList[i].doseNumberElement, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_dosenumber]);
    index('ImmunizationRecommendation', key, 0, resource.recommendationList[i].forecastStatus, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_status]);
    if resource.recommendationList[i].protocol <> nil then
      index('ImmunizationRecommendation', key, 0, resource.recommendationList[i].protocol.doseSequenceElement, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_dosesequence]);
    for j := 0 to resource.recommendationList[i].supportingPatientInformationList.Count - 1 do
      index(context, 'ImmunizationRecommendation', key, 0, resource.recommendationList[i].supportingPatientInformationList[j], CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_information]);
    for j := 0 to resource.recommendationList[i].supportingImmunizationList.Count - 1 do
      index(context, 'ImmunizationRecommendation', key, 0, resource.recommendationList[i].supportingImmunizationList[j], CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_support]);
  end;
end;

procedure TFhirIndexManager2.buildIndexValuesQuestionnaire(key: integer; id : String; context : TFhirResource; resource: TFhirQuestionnaire);
  procedure IndexGroup(group : TFhirQuestionnaireGroup);
  var
    i : integer;
  begin
    index('Questionnaire', key, 0, group.conceptList, CODES_TSearchParamsQuestionnaire[spQuestionnaire_code]);
    for I := 0 to group.groupList.Count - 1 do
      indexGroup(group.groupList[i]);
  end;
begin
  index('Questionnaire', key, 0, resource.publisherElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_publisher]);
  index('Questionnaire', key, 0, resource.statusElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_status]);
  index('Questionnaire', key, 0, resource.identifierList, CODES_TSearchParamsQuestionnaire[spQuestionnaire_identifier]);
  index('Questionnaire', key, 0, resource.dateElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_date]);
  index('Questionnaire', key, 0, resource.versionElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_version]);
  index('Questionnaire', key, 0, resource.group.titleElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_title]);
  IndexGroup(resource.group);
end;

procedure TFhirIndexManager2.buildIndexValuesQuestionnaireResponse(key: integer; id : String; context : TFhirResource; resource: TFhirQuestionnaireResponse);
begin
  index(context, 'QuestionnaireResponse', key, 0, resource.author, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_author]);
  index(context, 'QuestionnaireResponse', key, 0, resource.encounter, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_encounter]);
  index(context, 'QuestionnaireResponse', key, 0, resource.questionnaire, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_questionnaire]);
  index(context, 'QuestionnaireResponse', key, 0, resource.subject, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_subject]);
  index(context, 'QuestionnaireResponse', key, 0, resource.subject, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_patient], 'Patient');
  index(context, 'QuestionnaireResponse', key, 0, resource.source, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_source]);
  index('QuestionnaireResponse', key, 0, resource.statusElement, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_status]);
  index('QuestionnaireResponse', key, 0, resource.authoredElement, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_authored]);
  patientCompartment(key, resource.subject);
  patientCompartment(key, resource.author);
end;



procedure TFhirIndexManager2.buildIndexValuesSlot(key: integer; id : String; context : TFhirResource; resource: TFhirSlot);
begin
  index(context, 'Slot', key, 0, resource.schedule, CODES_TSearchParamsSlot[spSlot_schedule]);
  index('Slot', key, 0, resource.freeBusyTypeElement, CODES_TSearchParamsSlot[spSlot_fbtype]);
  index('Slot', key, 0, resource.type_, CODES_TSearchParamsSlot[spSlot_slottype]);
  index('Slot', key, 0, resource.identifierList, CODES_TSearchParamsSlot[spSlot_identifier]);
  index('Slot', key, 0, resource.startElement, CODES_TSearchParamsSlot[spSlot_start]);
end;

procedure TFhirIndexManager2.buildIndexValuesAppointment(key: integer; id : String; context : TFhirResource; resource: TFhirAppointment);
var
  i : integer;
begin
  index('Appointment', key, 0, resource.startElement, CODES_TSearchParamsAppointment[spAppointment_date]);
  index('Appointment', key, 0, resource.identifierList, CODES_TSearchParamsAppointment[spAppointment_identifier]);
  index('Appointment', key, 0, resource.statusElement, CODES_TSearchParamsAppointment[spAppointment_status]);
  for i := 0 to resource.participantList.Count - 1 do
  begin
    index('Appointment', key, 0, resource.participantList[i].statusElement, CODES_TSearchParamsAppointment[spAppointment_Partstatus]);
    index(context, 'Appointment', key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_actor]);
    index(context, 'Appointment', key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_patient], 'Patient');
    index(context, 'Appointment', key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_location], 'Location');
    index(context, 'Appointment', key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_practitioner], 'Practitioner');
    patientCompartment(key, resource.participantList[i].actor);
  end;
end;


procedure TFhirIndexManager2.buildIndexValuesSchedule(key: integer; id : String; context : TFhirResource; resource: TFhirSchedule);
var
  i : integer;
begin
  index('Schedule', key, 0, resource.planningHorizon, CODES_TSearchParamsSchedule[spSchedule_date]);
  index('Schedule', key, 0, resource.identifierList, CODES_TSearchParamsSchedule[spSchedule_identifier]);
  index(context, 'Schedule', key, 0, resource.actor, CODES_TSearchParamsSchedule[spSchedule_actor]);
  patientCompartment(key, resource.actor);
end;

procedure TFhirIndexManager2.buildIndexValuesAppointmentResponse(key: integer; id : String; context : TFhirResource; resource: TFhirAppointmentResponse);
var
  i : integer;
begin
  index('AppointmentResponse', key, 0, resource.participantStatusElement, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_partstatus]);
  index(context, 'AppointmentResponse', key, 0, resource.appointment, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_appointment]);
  index('AppointmentResponse', key, 0, resource.identifierList, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_identifier]);
  index(context, 'AppointmentResponse', key, 0, resource.actor, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_actor]);
  index(context, 'AppointmentResponse', key, 0, resource.actor, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_patient], 'Patient');
  index(context, 'AppointmentResponse', key, 0, resource.actor, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_practitioner], 'Practitioner');
  index(context, 'AppointmentResponse', key, 0, resource.actor, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_location], 'Location');
  patientCompartment(key, resource.actor);
end;


procedure TFhirIndexManager2.buildIndexValuesHealthcareService(key: integer; id : String; context : TFhirResource; resource: TFhirHealthcareService);
var
  i : integer;
begin
  index('HealthcareService', key, 0, resource.serviceNameElement, CODES_TSearchParamsHealthcareService[spHealthcareService_name]);
  index('HealthcareService', key, 0, resource.identifierList, CODES_TSearchParamsHealthcareService[spHealthcareService_identifier]);
  index('HealthcareService', key, 0, resource.characteristicList, CODES_TSearchParamsHealthcareService[spHealthcareService_characteristic]);
  for i := 0 to resource.programNameList.Count - 1 do
    index('HealthcareService', key, 0, resource.programNameList[i], CODES_TSearchParamsHealthcareService[spHealthcareService_programname]);
  index(context, 'HealthcareService', key, 0, resource.location, CODES_TSearchParamsHealthcareService[spHealthcareService_location]);
  index(context, 'HealthcareService', key, 0, resource.providedBy, CODES_TSearchParamsHealthcareService[spHealthcareService_organization]);
  index('HealthcareService', key, 0, resource.serviceCategoryElement, CODES_TSearchParamsHealthcareService[spHealthcareService_servicecategory]);
end;

procedure TFhirIndexManager2.buildIndexValuesDataElement(key: integer; id : String; context : TFhirResource; resource: TFhirDataElement);
var
  i : integer;
begin

  index('DataElement', key, 0, resource.dateElement, CODES_TSearchParamsDataElement[spDataElement_date]);
  index('DataElement', key, 0, resource.urlElement, CODES_TSearchParamsDataElement[spDataElement_url]);
  index('DataElement', key, 0, resource.identifierList, CODES_TSearchParamsDataElement[spDataElement_identifier]);
  index('DataElement', key, 0, resource.useContextList, CODES_TSearchParamsDataElement[spDataElement_context]);
  index('DataElement', key, 0, resource.nameElement, CODES_TSearchParamsDataElement[spDataElement_name]);
  index('DataElement', key, 0, resource.publisherElement, CODES_TSearchParamsDataElement[spDataElement_publisher]);
  index('DataElement', key, 0, resource.statusElement, CODES_TSearchParamsDataElement[spDataElement_status]);
  index('DataElement', key, 0, resource.versionElement, CODES_TSearchParamsDataElement[spDataElement_version]);
  index('DataElement', key, 0, resource.stringencyElement, CODES_TSearchParamsDataElement[spDataElement_stringency]);
  for i := 0 to resource.elementList.Count - 1 do
  begin
    if i = 0 then
      index('DataElement', key, 0, resource.elementList[i].definitionElement, CODES_TSearchParamsDataElement[spDataElement_description]);
    index('DataElement', key, 0, resource.elementList[i].codeList, CODES_TSearchParamsDataElement[spDataElement_code]);
  end;
end;

procedure TFhirIndexManager2.buildIndexValuesTestScript(key: integer; id : String; context : TFhirResource; resource: TFhirTestScript);
var
  i, j : integer;
  c : TFhirTestScriptMetadataCapability;
  t : TFhirTestScriptTest;
begin
  index('TestScript', key, 0, resource.nameElement, CODES_TSearchParamsTestScript[spTestScript_name]);
  index('TestScript', key, 0, resource.descriptionElement, CODES_TSearchParamsTestScript[spTestScript_description]);
  index('TestScript', key, 0, resource.identifier, CODES_TSearchParamsTestScript[spTestScript_identifier]);
  if (resource.metadata <> nil) then
    for c in resource.metadata.capabilityList do
      index('TestScript', key, 0, c.descriptionElement, CODES_TSearchParamsTestScript[spTestScript_testscriptcapability]);

  if (resource.setup <> nil) and (resource.setup.metadata <> nil) then
    for c in resource.setup.metadata.capabilityList do
      index('TestScript', key, 0, c.descriptionElement, CODES_TSearchParamsTestScript[spTestScript_testscriptsetupcapability]);

  for t in resource.testList do
    if (t.metadata <> nil) then
      for c in t.metadata.capabilityList do
        index('TestScript', key, 0, c.descriptionElement, CODES_TSearchParamsTestScript[spTestScript_testscripttestcapability]);

  index('TestScript', key, 0, resource.url, CODES_TSearchParamsTestScript[spTestScript_url]);
end;


procedure TFhirIndexManager2.buildIndexValuesNamingSystem(key: integer; id : String; context : TFhirResource; resource: TFhirNamingSystem);
var
  i, j : integer;
begin
  index('NamingSystem', key, 0, resource.kindElement, CODES_TSearchParamsNamingSystem[spNamingSystem_kind]);
  for i  := 0 to resource.contactList.Count - 1 do
  begin
    index('NamingSystem', key, 0, resource.contactList[i].name, CODES_TSearchParamsNamingSystem[spNamingSystem_contact]);
    for j := 0 to resource.contactList[i].telecomList.Count - 1 do
      index('NamingSystem', key, 0, resource.contactList[i].telecomList[j], CODES_TSearchParamsNamingSystem[spNamingSystem_telecom]);
  end;
  index('NamingSystem', key, 0, resource.useContextList, CODES_TSearchParamsNamingSystem[spNamingSystem_context]);
  index('NamingSystem', key, 0, resource.dateElement, CODES_TSearchParamsNamingSystem[spNamingSystem_date]);
  index('NamingSystem', key, 0, resource.type_, CODES_TSearchParamsNamingSystem[spNamingSystem_type]);
  index('NamingSystem', key, 0, resource.nameElement, CODES_TSearchParamsNamingSystem[spNamingSystem_name]);
  for i := 0 to resource.uniqueIdList.Count - 1 do
  begin
    index('NamingSystem', key, 0, resource.uniqueIdList[i].period, CODES_TSearchParamsNamingSystem[spNamingSystem_period]);
    index('NamingSystem', key, 0, resource.uniqueIdList[i].type_Element, CODES_TSearchParamsNamingSystem[spNamingSystem_idtype]);
    index('NamingSystem', key, 0, resource.uniqueIdList[i].valueElement, CODES_TSearchParamsNamingSystem[spNamingSystem_value]);
  end;
  index('NamingSystem', key, 0, resource.publisherElement, CODES_TSearchParamsNamingSystem[spNamingSystem_publisher]);
  index(context, 'NamingSystem', key, 0, resource.replacedBy, CODES_TSearchParamsNamingSystem[spNamingSystem_replacedby]);
  index('NamingSystem', key, 0, resource.responsibleElement, CODES_TSearchParamsNamingSystem[spNamingSystem_responsible]);
  index('NamingSystem', key, 0, resource.statusElement, CODES_TSearchParamsNamingSystem[spNamingSystem_status]);
end;

procedure TFhirIndexManager2.buildIndexValuesSubscription(key: integer; id : String; context : TFhirResource; resource: TFhirSubscription);
var
  i : integer;
begin
  for i := 0 to resource.contactList.Count - 1 do
    index('Subscription', key, 0, resource.contactList[i], CODES_TSearchParamsSubscription[spSubscription_contact]);
  index('Subscription', key, 0, resource.criteriaElement, CODES_TSearchParamsSubscription[spSubscription_criteria]);
  index('Subscription', key, 0, resource.statusElement, CODES_TSearchParamsSubscription[spSubscription_status]);
  for i := 0 to resource.tagList.Count - 1 do
    index('Subscription', key, 0, resource.tagList[i], CODES_TSearchParamsSubscription[spSubscription_tag]);
  index('Subscription', key, 0, resource.channel.type_Element, CODES_TSearchParamsSubscription[spSubscription_type]);
  index('Subscription', key, 0, resource.channel.payloadElement, CODES_TSearchParamsSubscription[spSubscription_payload]);
  index('Subscription', key, 0, resource.channel.endpoint, CODES_TSearchParamsSubscription[spSubscription_url]);
end;

procedure TFhirIndexManager2.buildIndexValuesDetectedIssue(key: integer; id : String; context : TFhirResource; resource: TFhirDetectedIssue);
var
  i : integer;
begin
  index('DetectedIssue', key, 0, resource.category, CODES_TSearchParamsDetectedIssue[spDetectedIssue_category]);
  index('DetectedIssue', key, 0, resource.dateElement, CODES_TSearchParamsDetectedIssue[spDetectedIssue_date]);
  index('DetectedIssue', key, 0, resource.identifier, CODES_TSearchParamsDetectedIssue[spDetectedIssue_identifier]);
  for i := 0 to resource.implicatedList.Count - 1 do
    index(context, 'DetectedIssue', key, 0, resource.patient, CODES_TSearchParamsDetectedIssue[spDetectedIssue_implicated]);
  index(context, 'DetectedIssue', key, 0, resource.patient, CODES_TSearchParamsDetectedIssue[spDetectedIssue_patient]);
  patientCompartment(key, resource.patient);
  index(context, 'DetectedIssue', key, 0, resource.author, CODES_TSearchParamsDetectedIssue[spDetectedIssue_author]);
end;

procedure TFhirIndexManager2.buildIndexValuesRiskAssessment(key: integer; id : String; context : TFhirResource; resource: TFhirRiskAssessment);
var
  i : integer;
begin
  index('RiskAssessment', key, 0, resource.dateElement, CODES_TSearchParamsRiskAssessment[spRiskAssessment_date]);
  index('RiskAssessment', key, 0, resource.identifier, CODES_TSearchParamsRiskAssessment[spRiskAssessment_identifier]);

  index('RiskAssessment', key, 0, resource.method, CODES_TSearchParamsRiskAssessment[spRiskAssessment_method]);
  index(context, 'RiskAssessment', key, 0, resource.subject, CODES_TSearchParamsRiskAssessment[spRiskAssessment_subject]);
  index(context, 'RiskAssessment', key, 0, resource.subject, CODES_TSearchParamsRiskAssessment[spRiskAssessment_patient]);
  index(context, 'RiskAssessment', key, 0, resource.condition, CODES_TSearchParamsRiskAssessment[spRiskAssessment_condition]);
  index(context, 'RiskAssessment', key, 0, resource.performer, CODES_TSearchParamsRiskAssessment[spRiskAssessment_performer]);
  index(context, 'RiskAssessment', key, 0, resource.encounter, CODES_TSearchParamsRiskAssessment[spRiskAssessment_encounter]);
end;

procedure TFhirIndexManager2.buildIndexValuesOperationDefinition(key : integer; id : String; context : TFhirResource; resource: TFhirOperationDefinition);
var
  i : integer;
begin
  index('OperationDefinition', key, 0, resource.urlElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_url]);
  index('OperationDefinition', key, 0, resource.statusElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_status]);
  index('OperationDefinition', key, 0, resource.versionElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_version]);
  index('OperationDefinition', key, 0, resource.publisherElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_publisher]);
  index('OperationDefinition', key, 0, resource.nameElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_name]);
  index('OperationDefinition', key, 0, resource.codeElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_code]);
  index(context, 'OperationDefinition', key, 0, resource.base, CODES_TSearchParamsOperationDefinition[spOperationDefinition_base]);
  index('OperationDefinition', key, 0, resource.dateElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_date]);
  index('OperationDefinition', key, 0, resource.kindElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_kind]);
  index('OperationDefinition', key, 0, resource.system, CODES_TSearchParamsOperationDefinition[spOperationDefinition_system]);
  for i := 0 to resource.type_List.count - 1 Do
    index('OperationDefinition', key, 0, resource.type_List[i], CODES_TSearchParamsOperationDefinition[spOperationDefinition_type]);
  index('OperationDefinition', key, 0, resource.instance, CODES_TSearchParamsOperationDefinition[spOperationDefinition_instance]);
  for i := 0 to resource.parameterList.count - 1 Do
    index(context, 'OperationDefinition', key, 0, resource.parameterList[i].profile, CODES_TSearchParamsOperationDefinition[spOperationDefinition_profile]);
end;

procedure TFhirIndexManager2.buildIndexValuesReferralRequest(key : integer; id : String; context : TFhirResource; resource: TFhirReferralRequest);
var
  i : integer;
begin
  patientCompartment(key, resource.patient);
  index(context, 'ReferralRequest', key, 0, resource.patient, CODES_TSearchParamsReferralRequest[spReferralRequest_patient]);
  index('ReferralRequest', key, 0, resource.statusElement, CODES_TSearchParamsReferralRequest[spReferralRequest_status]);
  index('ReferralRequest', key, 0, resource.priority, CODES_TSearchParamsReferralRequest[spReferralRequest_priority]);
  for i := 0 to resource.recipientList.Count - 1 do
    index(context, 'ReferralRequest', key, 0, resource.recipientList[i], CODES_TSearchParamsReferralRequest[spReferralRequest_recipient]);
  index(context, 'ReferralRequest', key, 0, resource.requester, CODES_TSearchParamsReferralRequest[spReferralRequest_requester]);
  index('ReferralRequest', key, 0, resource.specialty, CODES_TSearchParamsReferralRequest[spReferralRequest_specialty]);
  index('ReferralRequest', key, 0, resource.type_, CODES_TSearchParamsReferralRequest[spReferralRequest_type]);

  index('ReferralRequest', key, 0, resource.dateElement, CODES_TSearchParamsReferralRequest[spReferralRequest_date]);
end;

procedure TFhirIndexManager2.buildIndexValuesNutritionOrder(key : integer; id : String; context : TFhirResource; resource: TFhirNutritionOrder);
var
  item : TFhirNutritionOrderSupplement;
begin
  patientCompartment(key, resource.patient);
  index(context, 'NutritionOrder', key, 0, resource.patient, CODES_TSearchParamsNutritionOrder[spNutritionOrder_patient]);
  index(context, 'NutritionOrder', key, 0, resource.orderer, CODES_TSearchParamsNutritionOrder[spNutritionOrder_provider]);
  index('NutritionOrder', key, 0, resource.statusElement, CODES_TSearchParamsNutritionOrder[spNutritionOrder_status]);
  index(context, 'NutritionOrder', key, 0, resource.encounter, CODES_TSearchParamsNutritionOrder[spNutritionOrder_encounter]);
  index('NutritionOrder', key, 0, resource.identifierList, CODES_TSearchParamsNutritionOrder[spNutritionOrder_identifier]);
  index('NutritionOrder', key, 0, resource.dateTimeElement, CODES_TSearchParamsNutritionOrder[spNutritionOrder_datetime]);

  if (resource.enteralFormula <> nil) then
  begin
    index('NutritionOrder', key, 0, resource.enteralFormula.additiveTypeElement, CODES_TSearchParamsNutritionOrder[spNutritionOrder_additive]);
    index('NutritionOrder', key, 0, resource.enteralFormula.baseFormulaType, CODES_TSearchParamsNutritionOrder[spNutritionOrder_formula]);

  end;
  if (resource.oralDiet <> nil) then
    index('NutritionOrder', key, 0, resource.oralDiet.type_List, CODES_TSearchParamsNutritionOrder[spNutritionOrder_oraldiet]);
  for item in resource.supplementList do
    index('NutritionOrder', key, 0, item.type_, CODES_TSearchParamsNutritionOrder[spNutritionOrder_supplement]);
end;


procedure TFhirIndexManager2.buildIndexValuesBodySite(key: integer; id : String; context : TFhirResource; resource: TFhirBodySite);
begin
  index('BodySite', key, 0, resource.codeElement, CODES_TSearchParamsBodySite[spBodySite_code]);
  index('BodySite', key, 0, resource.identifierList, CODES_TSearchParamsBodySite[spBodySite_identifier]);
  index(context, 'BodySite', key, 0, resource.patient, CODES_TSearchParamsBodySite[spBodySite_patient]);
end;

procedure TFhirIndexManager2.buildIndexValuesClinicalImpression(key: integer; id : String; context : TFhirResource; resource: TFhirClinicalImpression);
var
  i, j : integer;
begin
  index(context, 'ClinicalImpression', key, 0, resource.patient, CODES_TSearchParamsClinicalImpression[spClinicalImpression_patient]);
  patientCompartment(key, resource.patient);
  if (resource.trigger is TFhirCodeableConcept) then
    index('ClinicalImpression', key, 0, resource.trigger as TFhirCodeableConcept, CODES_TSearchParamsClinicalImpression[spClinicalImpression_triggercode])
  else
    index(context, 'ClinicalImpression', key, 0, resource.trigger as TFhirReference, CODES_TSearchParamsClinicalImpression[spClinicalImpression_trigger]);

  index(context, 'ClinicalImpression', key, 0, resource.previous, CODES_TSearchParamsClinicalImpression[spClinicalImpression_previous]);
  index('ClinicalImpression', key, 0, resource.dateElement, CODES_TSearchParamsClinicalImpression[spClinicalImpression_date]);
  index('ClinicalImpression', key, 0, resource.statusElement, CODES_TSearchParamsClinicalImpression[spClinicalImpression_status]);
  for i := 0 to resource.problemList.Count - 1 do
    index(context, 'ClinicalImpression', key, 0, resource.problemList[i], CODES_TSearchParamsClinicalImpression[spClinicalImpression_problem]);
  for i := 0 to resource.investigationsList.Count - 1 do
    for j := 0 to resource.investigationsList[i].itemList.Count - 1 do
      index(context, 'ClinicalImpression', key, 0, resource.investigationsList[i].itemList[j], CODES_TSearchParamsClinicalImpression[spClinicalImpression_investigation]);
  for i := 0 to resource.findingList.Count - 1 do
    index('ClinicalImpression', key, 0, resource.findingList[i].item, CODES_TSearchParamsClinicalImpression[spClinicalImpression_finding]);
  index(context, 'ClinicalImpression', key, 0, resource.assessor, CODES_TSearchParamsClinicalImpression[spClinicalImpression_assessor]);
  for i := 0 to resource.actionList.Count - 1 do
    index(context, 'ClinicalImpression', key, 0, resource.actionList[i], CODES_TSearchParamsClinicalImpression[spClinicalImpression_action]);
  index('ClinicalImpression', key, 0, resource.resolvedList, CODES_TSearchParamsClinicalImpression[spClinicalImpression_resolved]);
  for i := 0 to resource.ruledOutList.Count - 1 do
    index('ClinicalImpression', key, 0, resource.ruledOutList[i].item, CODES_TSearchParamsClinicalImpression[spClinicalImpression_ruledout]);
  if (resource.triggerElement is TFhirCodeableConcept) then
    index('ClinicalImpression', key, 0, resource.triggerElement as TFhirCodeableConcept, CODES_TSearchParamsClinicalImpression[spClinicalImpression_triggercode])
  else
    index(context, 'ClinicalImpression', key, 0, resource.triggerElement as TFhirReference, CODES_TSearchParamsClinicalImpression[spClinicalImpression_trigger]);
end;

procedure TFhirIndexManager2.buildIndexValuesCommunication(key: integer; id : String; context : TFhirResource; resource: TFhirCommunication);
var
  i, j : integer;
begin
  index(context, 'Communication', key, 0, resource.subject, CODES_TSearchParamsCommunication[spCommunication_patient]);
  index(context, 'Communication', key, 0, resource.subject, CODES_TSearchParamsCommunication[spCommunication_subject]);
  patientCompartment(key, resource.subject);
  index('Communication', key, 0, resource.category, CODES_TSearchParamsCommunication[spCommunication_category]);
  index(context, 'Communication', key, 0, resource.encounter, CODES_TSearchParamsCommunication[spCommunication_encounter]);
  index('Communication', key, 0, resource.identifierList, CODES_TSearchParamsCommunication[spCommunication_identifier]);
  index('Communication', key, 0, resource.mediumList, CODES_TSearchParamsCommunication[spCommunication_medium]);
  index('Communication', key, 0, resource.receivedElement, CODES_TSearchParamsCommunication[spCommunication_received]);
  index('Communication', key, 0, resource.sentElement, CODES_TSearchParamsCommunication[spCommunication_sent]);
  for i := 0 to resource.recipientList.count - 1 do
  begin
    index(context, 'Communication', key, 0, resource.recipientList[i], CODES_TSearchParamsCommunication[spCommunication_recipient]);
    patientCompartment(key, resource.recipientList[i]);
  end;
  index(context, 'Communication', key, 0, resource.sender, CODES_TSearchParamsCommunication[spCommunication_sender]);
  index(context, 'Communication', key, 0, resource.requestDetail, CODES_TSearchParamsCommunication[spCommunication_request]);
  patientCompartment(key, resource.sender);
  index('Communication', key, 0, resource.statusElement, CODES_TSearchParamsCommunication[spCommunication_status]);
end;


procedure TFhirIndexManager2.buildIndexValuesCommunicationRequest(key: integer; id : String; context : TFhirResource; resource: TFhirCommunicationRequest);
var
  i, j : integer;
begin
  index(context, 'CommunicationRequest', key, 0, resource.subject, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_patient]);
  index(context, 'CommunicationRequest', key, 0, resource.subject, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_subject]);
  patientCompartment(key, resource.subject);
  index('CommunicationRequest', key, 0, resource.category, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_category]);
  index(context, 'CommunicationRequest', key, 0, resource.encounter, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_encounter]);
  index('CommunicationRequest', key, 0, resource.identifierList, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_identifier]);
  index('CommunicationRequest', key, 0, resource.mediumList, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_medium]);
  index('CommunicationRequest', key, 0, resource.requestedOnElement, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_requested]);
  for i := 0 to resource.recipientList.count - 1 do
  begin
    index(context, 'CommunicationRequest', key, 0, resource.recipientList[i], CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_recipient]);
    patientCompartment(key, resource.recipientList[i]);
  end;
  index(context, 'CommunicationRequest', key, 0, resource.sender, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_sender]);
  index(context, 'CommunicationRequest', key, 0, resource.requester, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_requester]);
  index('CommunicationRequest', key, 0, resource.priority, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_priority]);
  index('CommunicationRequest', key, 0, resource.statusElement, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_status]);
  patientCompartment(key, resource.sender);
end;

procedure TFhirIndexManager2.buildIndexValuesDeviceComponent(key: integer; id : String; context : TFhirResource; resource: TFhirDeviceComponent);
var
  i, j : integer;
begin
  index(context, 'DeviceComponent', key, 0, resource.parent, CODES_TSearchParamsDeviceComponent[spDeviceComponent_parent]);
  index(context, 'DeviceComponent', key, 0, resource.source, CODES_TSearchParamsDeviceComponent[spDeviceComponent_source]);
  index('DeviceComponent', key, 0, resource.type_, CODES_TSearchParamsDeviceComponent[spDeviceComponent_type]);
end;

procedure TFhirIndexManager2.buildIndexValuesDeviceMetric(key: integer; id : String; context : TFhirResource; resource: TFhirDeviceMetric);
var
  i, j : integer;
begin
  index(context, 'DeviceMetric', key, 0, resource.parent, CODES_TSearchParamsDeviceMetric[spDeviceMetric_parent]);
  index(context, 'DeviceMetric', key, 0, resource.source, CODES_TSearchParamsDeviceMetric[spDeviceMetric_source]);
  index('DeviceMetric', key, 0, resource.type_, CODES_TSearchParamsDeviceMetric[spDeviceMetric_type]);
  index('DeviceMetric', key, 0, resource.identifierElement, CODES_TSearchParamsDeviceMetric[spDeviceMetric_identifier]);
  index('DeviceMetric', key, 0, resource.categoryElement, CODES_TSearchParamsDeviceMetric[spDeviceMetric_category]);
end;

procedure TFhirIndexManager2.buildIndexValuesDeviceUseStatement(key: integer; id : String; context : TFhirResource; resource: TFhirDeviceUseStatement);
var
  i, j : integer;
begin
  index(context, 'DeviceUseStatement', key, 0, resource.subject, CODES_TSearchParamsDeviceUseStatement[spDeviceUseStatement_subject]);
  index(context, 'DeviceUseStatement', key, 0, resource.subject, CODES_TSearchParamsDeviceUseStatement[spDeviceUseStatement_patient]);
  patientCompartment(key, resource.subject);
  index(context, 'DeviceUseStatement', key, 0, resource.device, CODES_TSearchParamsDeviceUseStatement[spDeviceUseStatement_device]);
end;

procedure TFhirIndexManager2.buildIndexValuesDeviceUseRequest(key: integer; id : String; context : TFhirResource; resource: TFhirDeviceUseRequest);
var
  i, j : integer;
begin
  index(context, 'DeviceUseRequest', key, 0, resource.subject, CODES_TSearchParamsDeviceUseRequest[spDeviceUseRequest_subject]);
  index(context, 'DeviceUseRequest', key, 0, resource.subject, CODES_TSearchParamsDeviceUseRequest[spDeviceUseRequest_patient]);
  patientCompartment(key, resource.subject);
  index(context, 'DeviceUseRequest', key, 0, resource.device, CODES_TSearchParamsDeviceUseRequest[spDeviceUseRequest_device]);
end;

procedure TFhirIndexManager2.buildIndexValuesEpisodeOfCare(key: integer; id : String; context : TFhirResource; resource: TFhirEpisodeOfCare);
var
  i : integer;
begin
  index('EpisodeOfCare', key, 0, resource.identifierList, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_identifier]);
  index(context, 'EpisodeOfCare', key, 0, resource.managingOrganization, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_organization]);
  index(context, 'EpisodeOfCare', key, 0, resource.patient, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_patient]);
  index(context, 'EpisodeOfCare', key, 0, resource.referralRequestList, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_incomingreferral]);
  index(context, 'EpisodeOfCare', key, 0, resource.careManager, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_caremanager]);
  patientCompartment(key, resource.patient);
  index('EpisodeOfCare', key, 0, resource.period, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_date]);
  index('EpisodeOfCare', key, 0, resource.type_List, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_type]);
  index('EpisodeOfCare', key, 0, resource.statusElement, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_status]);
  for i := 0 to resource.conditionList.Count - 1 do
    index(context, 'EpisodeOfCare', key, 0, resource.conditionList[i], CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_condition]);
  for i := 0 to resource.careTeamList.Count - 1 do
    index(context, 'EpisodeOfCare', key, 0, resource.careTeamList[i].member, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_teammember]);
end;

procedure TFhirIndexManager2.buildIndexValuesGoal(key: integer; id : String; context : TFhirResource; resource: TFhirGoal);
begin
  index('Goal', key, 0, resource.identifierList, CODES_TSearchParamsGoal[spGoal_identifier]);
  index('Goal', key, 0, resource.statusElement, CODES_TSearchParamsGoal[spGoal_status]);
  index(context, 'Goal', key, 0, resource.subject, CODES_TSearchParamsGoal[spGoal_subject]);
  index(context, 'Goal', key, 0, resource.subject, CODES_TSearchParamsGoal[spGoal_patient], 'Patient');
  patientCompartment(key, resource.subject);
end;

procedure TFhirIndexManager2.buildIndexValuesImagingObjectSelection(key: integer; id : String; context : TFhirResource; resource: TFhirImagingObjectSelection);
var
  i : integer;
begin
  index(context, 'ImagingObjectSelection', key, 0, resource.patient, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_patient]);
  patientCompartment(key, resource.patient);
  index(context, 'ImagingObjectSelection', key, 0, resource.author, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_author]);
  index('ImagingObjectSelection', key, 0, resource.authoringTimeElement, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_authoringtime]);
  index('ImagingObjectSelection', key, 0, resource.uidElement, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_identifier]);
  index('ImagingObjectSelection', key, 0, resource.title, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_title]);
  for i := 0 to resource.studyList.Count - 1 do
    index('ImagingObjectSelection', key, 0, resource.studyList[i].uid, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_selectedstudy]);
end;


procedure TFhirIndexManager2.buildIndexValuesPerson(key: integer; id : String; context : TFhirResource; resource: TFhirPerson);
var
  i, j : integer;
begin
  for i := 0 to resource.addressList.Count - 1 do
    index('Person', key, 0, resource.addressList[i], CODES_TSearchParamsPerson[spPerson_address]);
  index('Person', key, 0, resource.identifierList, CODES_TSearchParamsPerson[spPerson_identifier]);
  index('Person', key, 0, resource.birthDateElement, CODES_TSearchParamsPerson[spPerson_birthdate]);
  index('Person', key, 0, resource.genderElement, CODES_TSearchParamsPerson[spPerson_gender]);
  for i := 0 to resource.nameList.count - 1 do
  begin
    index('Person', key, 0, resource.nameList[i], 'name', CODES_TSearchParamsPerson[spPerson_phonetic]);
//    for j := 0 to resource.nameList[i].givenList.count - 1 do
//      index('Person', key, 0, resource.nameList[i].givenList[j], CODES_TSearchParamsPerson[spPerson_given]);
//    for j := 0 to resource.nameList[i].familyList.count - 1 do
//      index('Person', key, 0, resource.nameList[i].familyList[j], CODES_TSearchParamsPerson[spPerson_family]);
  end;
  index(context, 'Person', key, 0, resource.managingOrganization, CODES_TSearchParamsPerson[spPerson_organization]);
  for i := 0 to resource.telecomList.Count - 1 do
  begin
    index('Person', key, 0, resource.telecomList[i], CODES_TSearchParamsPerson[spPerson_telecom]);
    if (resource.telecomList[i].system = ContactPointSystemPhone) then
      index('Person', key, 0, resource.telecomList[i].valueElement, CODES_TSearchParamsPerson[spPerson_phone]);
    if (resource.telecomList[i].system = ContactPointSystemEmail) then
      index('Person', key, 0, resource.telecomList[i].valueElement, CODES_TSearchParamsPerson[spPerson_email]);
  end;
  for i := 0 to resource.link_List.Count - 1 do
  begin
    index(context, 'Person', key, 0, resource.link_List[i].target, CODES_TSearchParamsPerson[spPerson_link]);
    index(context, 'Person', key, 0, resource.link_List[i].target, CODES_TSearchParamsPerson[spPerson_patient], 'Patient');
    index(context, 'Person', key, 0, resource.link_List[i].target, CODES_TSearchParamsPerson[spPerson_practitioner], 'Practitioner');
    index(context, 'Person', key, 0, resource.link_List[i].target, CODES_TSearchParamsPerson[spPerson_relatedperson], 'RelatedPerson');
  end;
end;

procedure TFhirIndexManager2.buildIndexValuesProcedureRequest(key: integer; id : String; context : TFhirResource; resource: TFhirProcedureRequest);
var
  i : integer;
begin
  index(context, 'ProcedureRequest', key, 0, resource.subject, CODES_TSearchParamsProcedureRequest[spProcedureRequest_subject]);
  index(context, 'ProcedureRequest', key, 0, resource.subject, CODES_TSearchParamsProcedureRequest[spProcedureRequest_patient]);
  patientCompartment(key, resource.subject);
  index(context, 'ProcedureRequest', key, 0, resource.encounter, CODES_TSearchParamsProcedureRequest[spProcedureRequest_encounter]);
  index(context, 'ProcedureRequest', key, 0, resource.orderer, CODES_TSearchParamsProcedureRequest[spProcedureRequest_orderer]);
  index(context, 'ProcedureRequest', key, 0, resource.performer, CODES_TSearchParamsProcedureRequest[spProcedureRequest_performer]);
  index('ProcedureRequest', key, 0, resource.identifierList, CODES_TSearchParamsProcedureRequest[spProcedureRequest_identifier]);
end;


procedure TFhirIndexManager2.buildIndexValuesSearchParameter(key: integer; id : String; context : TFhirResource; resource: TFhirSearchParameter);
var
  i : integer;
begin
  index('SearchParameter', key, 0, resource.baseElement, CODES_TSearchParamsSearchParameter[spSearchParameter_base]);
  index('SearchParameter', key, 0, resource.description, CODES_TSearchParamsSearchParameter[spSearchParameter_description]);
  index('SearchParameter', key, 0, resource.name, CODES_TSearchParamsSearchParameter[spSearchParameter_name]);
  index('SearchParameter', key, 0, resource.code, CODES_TSearchParamsSearchParameter[spSearchParameter_code]);
  for i := 0 to resource.targetList.count - 1  do
    index('SearchParameter', key, 0, resource.targetList[i], CODES_TSearchParamsSearchParameter[spSearchParameter_target]);
  index('SearchParameter', key, 0, resource.type_Element, CODES_TSearchParamsSearchParameter[spSearchParameter_type]);
  index('SearchParameter', key, 0, resource.url, CODES_TSearchParamsSearchParameter[spSearchParameter_url]);
end;


procedure TFhirIndexManager2.buildIndexValuesVisionPrescription(key: integer; id : String; context : TFhirResource; resource: TFhirVisionPrescription);
var
  i : integer;
begin
  index('VisionPrescription', key, 0, resource.identifierList, CODES_TSearchParamsVisionPrescription[spVisionPrescription_identifier]);
  index(context, 'VisionPrescription', key, 0, resource.patient, CODES_TSearchParamsVisionPrescription[spVisionPrescription_patient]);
  patientCompartment(key, resource.patient);
  index('VisionPrescription', key, 0, resource.dateWrittenElement, CODES_TSearchParamsVisionPrescription[spVisionPrescription_dateWritten]);
  index(context, 'VisionPrescription', key, 0, resource.encounter, CODES_TSearchParamsVisionPrescription[spVisionPrescription_encounter]);
  index(context, 'VisionPrescription', key, 0, resource.prescriber, CODES_TSearchParamsVisionPrescription[spVisionPrescription_prescriber]);
end;

procedure TFhirIndexManager2.buildIndexValuesProcessRequest(key: integer; id : String; context : TFhirResource; resource: TFhirProcessRequest);
var
  i : integer;
begin
  index('ProcessRequest', key, 0, resource.identifierList, CODES_TSearchParamsProcessRequest[spProcessRequest_identifier]);
  index('ProcessRequest', key, 0, resource.actionElement, CODES_TSearchParamsProcessRequest[spProcessRequest_action]);
  index(context, 'ProcessRequest', key, 0, resource.organization, CODES_TSearchParamsProcessRequest[spProcessRequest_Organization]);
  index(context, 'ProcessRequest', key, 0, resource.provider, CODES_TSearchParamsProcessRequest[spProcessRequest_Provider]);
end;

procedure TFhirIndexManager2.buildIndexValuesProcessResponse(key: integer; id : String; context : TFhirResource; resource: TFhirProcessResponse);
var
  i : integer;
begin
  index('ProcessResponse', key, 0, resource.identifierList, CODES_TSearchParamsProcessResponse[spProcessResponse_identifier]);

  index(context, 'ProcessResponse', key, 0, resource.request, CODES_TSearchParamsProcessResponse[spProcessResponse_Request]);
  index(context, 'ProcessResponse', key, 0, resource.organization, CODES_TSearchParamsProcessResponse[spProcessResponse_Organization]);
  index(context, 'ProcessResponse', key, 0, resource.requestOrganization, CODES_TSearchParamsProcessResponse[spProcessResponse_Requestorganization]);
  index(context, 'ProcessResponse', key, 0, resource.requestProvider, CODES_TSearchParamsProcessResponse[spProcessResponse_Requestprovider]);
end;


procedure TFhirIndexManager2.buildIndexValuesAccount(key: integer; id : String; context : TFhirResource; resource: TFhirAccount);
var
  i : integer;
begin
  index('Account', key, 0, resource.identifierList, CODES_TSearchParamsAccount[spAccount_identifier]);
  index('Account', key, 0, resource.balance, CODES_TSearchParamsAccount[spAccount_balance]);
  index('Account', key, 0, resource.name, CODES_TSearchParamsAccount[spAccount_name]);
  index(context, 'Account', key, 0, resource.owner, CODES_TSearchParamsAccount[spAccount_owner]);
  index(context, 'Account', key, 0, resource.owner, CODES_TSearchParamsAccount[spAccount_subject]);
  index(context, 'Account', key, 0, resource.owner, CODES_TSearchParamsAccount[spAccount_patient], 'Patient');
  index('Account', key, 0, resource.type_, CODES_TSearchParamsAccount[spAccount_type]);
  index('Account', key, 0, resource.activePeriod, CODES_TSearchParamsAccount[spAccount_period]);
  index('Account', key, 0, resource.statusElement, CODES_TSearchParamsAccount[spAccount_status]);
end;


procedure TFhirIndexManager2.buildIndexValuesImplementationGuide(key: integer; id : String; context : TFhirResource; resource: TFhirImplementationGuide);
var
  p : TFhirImplementationGuidePackage;
  r : TFhirImplementationGuidePackageResource;
  i : integer;
begin
  index('ImplementationGuide', key, 0, resource.useContextList, CODES_TSearchParamsImplementationGuide[spImplementationGuide_context]);
  index('ImplementationGuide', key, 0, resource.dateElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_date]);
  for i := 0 to resource.dependencyList.Count - 1 do
    index('ImplementationGuide', key, 0, resource.dependencyList[i].uriElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_dependency]);
  index('ImplementationGuide', key, 0, resource.descriptionElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_description]);
  index('ImplementationGuide', key, 0, resource.experimentalElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_experimental]);
  index('ImplementationGuide', key, 0, resource.nameElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_name]);
  index('ImplementationGuide', key, 0, resource.publisherElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_publisher]);
  index('ImplementationGuide', key, 0, resource.statusElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_status]);
  index('ImplementationGuide', key, 0, resource.urlElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_url]);
  index('ImplementationGuide', key, 0, resource.versionElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_version]);
end;

end.



