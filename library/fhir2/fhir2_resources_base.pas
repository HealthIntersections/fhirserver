unit fhir2_resources_base;

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

{$I fhir.inc}
{$I fhir2.inc}

interface

// FHIR v1.0.2 generated 2015-10-24T07:41:03+11:00

uses
  SysUtils, Classes, 
  fsl_base, fsl_utilities, fsl_stream, 
  fhir_objects, fhir_utilities,  
  fhir2_base, fhir2_types;

Type
  TFhirResourceType = (
    frtNull, // Resource type not known / not Specified 
    {$IFDEF FHIR_ACCOUNT}frtAccount, {$ENDIF}
    {$IFDEF FHIR_ALLERGYINTOLERANCE}frtAllergyIntolerance, {$ENDIF}
    {$IFDEF FHIR_APPOINTMENT}frtAppointment, {$ENDIF}
    {$IFDEF FHIR_APPOINTMENTRESPONSE}frtAppointmentResponse, {$ENDIF}
    {$IFDEF FHIR_AUDITEVENT}frtAuditEvent, {$ENDIF}
    {$IFDEF FHIR_BASIC}frtBasic, {$ENDIF}
    {$IFDEF FHIR_BINARY}frtBinary, {$ENDIF}
    {$IFDEF FHIR_BODYSITE}frtBodySite, {$ENDIF}
    {$IFDEF FHIR_BUNDLE}frtBundle, {$ENDIF}
    {$IFDEF FHIR_CAREPLAN}frtCarePlan, {$ENDIF}
    {$IFDEF FHIR_CLAIM}frtClaim, {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE}frtClaimResponse, {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION}frtClinicalImpression, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION}frtCommunication, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST}frtCommunicationRequest, {$ENDIF}
    {$IFDEF FHIR_COMPOSITION}frtComposition, {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP}frtConceptMap, {$ENDIF}
    {$IFDEF FHIR_CONDITION}frtCondition, {$ENDIF}
    {$IFDEF FHIR_CONFORMANCE}frtConformance, {$ENDIF}
    {$IFDEF FHIR_CONTRACT}frtContract, {$ENDIF}
    {$IFDEF FHIR_COVERAGE}frtCoverage, {$ENDIF}
    {$IFDEF FHIR_DATAELEMENT}frtDataElement, {$ENDIF}
    {$IFDEF FHIR_DETECTEDISSUE}frtDetectedIssue, {$ENDIF}
    {$IFDEF FHIR_DEVICE}frtDevice, {$ENDIF}
    {$IFDEF FHIR_DEVICECOMPONENT}frtDeviceComponent, {$ENDIF}
    {$IFDEF FHIR_DEVICEMETRIC}frtDeviceMetric, {$ENDIF}
    {$IFDEF FHIR_DEVICEUSEREQUEST}frtDeviceUseRequest, {$ENDIF}
    {$IFDEF FHIR_DEVICEUSESTATEMENT}frtDeviceUseStatement, {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICORDER}frtDiagnosticOrder, {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICREPORT}frtDiagnosticReport, {$ENDIF}
    {$IFDEF FHIR_DOCUMENTMANIFEST}frtDocumentManifest, {$ENDIF}
    {$IFDEF FHIR_DOCUMENTREFERENCE}frtDocumentReference, {$ENDIF}
    {$IFDEF FHIR_ELIGIBILITYREQUEST}frtEligibilityRequest, {$ENDIF}
    {$IFDEF FHIR_ELIGIBILITYRESPONSE}frtEligibilityResponse, {$ENDIF}
    {$IFDEF FHIR_ENCOUNTER}frtEncounter, {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTREQUEST}frtEnrollmentRequest, {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTRESPONSE}frtEnrollmentResponse, {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE}frtEpisodeOfCare, {$ENDIF}
    {$IFDEF FHIR_EXPLANATIONOFBENEFIT}frtExplanationOfBenefit, {$ENDIF}
    {$IFDEF FHIR_FAMILYMEMBERHISTORY}frtFamilyMemberHistory, {$ENDIF}
    {$IFDEF FHIR_FLAG}frtFlag, {$ENDIF}
    {$IFDEF FHIR_GOAL}frtGoal, {$ENDIF}
    {$IFDEF FHIR_GROUP}frtGroup, {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE}frtHealthcareService, {$ENDIF}
    {$IFDEF FHIR_IMAGINGOBJECTSELECTION}frtImagingObjectSelection, {$ENDIF}
    {$IFDEF FHIR_IMAGINGSTUDY}frtImagingStudy, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATION}frtImmunization, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}frtImmunizationRecommendation, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE}frtImplementationGuide, {$ENDIF}
    {$IFDEF FHIR_LIST}frtList, {$ENDIF}
    {$IFDEF FHIR_LOCATION}frtLocation, {$ENDIF}
    {$IFDEF FHIR_MEDIA}frtMedia, {$ENDIF}
    {$IFDEF FHIR_MEDICATION}frtMedication, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION}frtMedicationAdministration, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE}frtMedicationDispense, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONORDER}frtMedicationOrder, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT}frtMedicationStatement, {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER}frtMessageHeader, {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM}frtNamingSystem, {$ENDIF}
    {$IFDEF FHIR_NUTRITIONORDER}frtNutritionOrder, {$ENDIF}
    {$IFDEF FHIR_OBSERVATION}frtObservation, {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION}frtOperationDefinition, {$ENDIF}
    {$IFDEF FHIR_OPERATIONOUTCOME}frtOperationOutcome, {$ENDIF}
    {$IFDEF FHIR_ORDER}frtOrder, {$ENDIF}
    {$IFDEF FHIR_ORDERRESPONSE}frtOrderResponse, {$ENDIF}
    {$IFDEF FHIR_ORGANIZATION}frtOrganization, {$ENDIF}
    {$IFDEF FHIR_PARAMETERS}frtParameters, {$ENDIF}
    {$IFDEF FHIR_PATIENT}frtPatient, {$ENDIF}
    {$IFDEF FHIR_PAYMENTNOTICE}frtPaymentNotice, {$ENDIF}
    {$IFDEF FHIR_PAYMENTRECONCILIATION}frtPaymentReconciliation, {$ENDIF}
    {$IFDEF FHIR_PERSON}frtPerson, {$ENDIF}
    {$IFDEF FHIR_PRACTITIONER}frtPractitioner, {$ENDIF}
    {$IFDEF FHIR_PROCEDURE}frtProcedure, {$ENDIF}
    {$IFDEF FHIR_PROCEDUREREQUEST}frtProcedureRequest, {$ENDIF}
    {$IFDEF FHIR_PROCESSREQUEST}frtProcessRequest, {$ENDIF}
    {$IFDEF FHIR_PROCESSRESPONSE}frtProcessResponse, {$ENDIF}
    {$IFDEF FHIR_PROVENANCE}frtProvenance, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE}frtQuestionnaire, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRERESPONSE}frtQuestionnaireResponse, {$ENDIF}
    {$IFDEF FHIR_REFERRALREQUEST}frtReferralRequest, {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON}frtRelatedPerson, {$ENDIF}
    {$IFDEF FHIR_RISKASSESSMENT}frtRiskAssessment, {$ENDIF}
    {$IFDEF FHIR_SCHEDULE}frtSchedule, {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER}frtSearchParameter, {$ENDIF}
    {$IFDEF FHIR_SLOT}frtSlot, {$ENDIF}
    {$IFDEF FHIR_SPECIMEN}frtSpecimen, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION}frtStructureDefinition, {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTION}frtSubscription, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE}frtSubstance, {$ENDIF}
    {$IFDEF FHIR_SUPPLYDELIVERY}frtSupplyDelivery, {$ENDIF}
    {$IFDEF FHIR_SUPPLYREQUEST}frtSupplyRequest, {$ENDIF}
    {$IFDEF FHIR_TESTSCRIPT}frtTestScript, {$ENDIF}
    {$IFDEF FHIR_VALUESET}frtValueSet, {$ENDIF}
    {$IFDEF FHIR_VISIONPRESCRIPTION}frtVisionPrescription, {$ENDIF}
    frtCustom);
  TFhirResourceTypeSet = set of TFhirResourceType;
Type
  TFhirResource = class;
  TFhirResourceList = class;
  TFhirDomainResource = class;
  TFhirDomainResourceList = class;

  // This is the base resource type for everything.
  TFhirResource = class abstract (TFHIRResource2)
  protected
    FId : TFhirId;
    FMeta : TFhirMeta;
    FImplicitRules : TFhirUri;
    FLanguage : TFhirCode;
    Procedure SetId(value : TFhirId);
    Function GetIdST : String;
    Procedure SetIdST(value : String);
    Procedure SetMeta(value : TFhirMeta);
    Procedure SetImplicitRules(value : TFhirUri);
    Function GetImplicitRulesST : String;
    Procedure SetImplicitRulesST(value : String);
    Procedure SetLanguage(value : TFhirCode);
    Function GetLanguageST : String;
    Procedure SetLanguageST(value : String);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function GetResourceType : TFhirResourceType; virtual; abstract;
    function GetProfileVersion : TFHIRVersion; override;
    procedure SetProfileVersion(v : TFHIRVersion); override;
    Procedure listResourceFieldsInOrder(fields: TStringList);
    procedure listFieldsInOrder(fields : TStringList); override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirResource; overload;
    function Clone : TFhirResource; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function hasExtensions : boolean; override;
    function fhirType : string; override;
    function getId : string; override;
    procedure setIdValue(id : String); override;
    procedure checkNoImplicitRules(place, role : String); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    Property ResourceType : TFhirResourceType read GetResourceType;

    // Typed access to The logical id of the resource, as used in the URL for the resource. Once assigned, this value never changes.
    property id : String read GetIdST write SetIdST;
    // The logical id of the resource, as used in the URL for the resource. Once assigned, this value never changes.
    property idElement : TFhirId read FId write SetId;

    // Typed access to The metadata about the resource. This is content that is maintained by the infrastructure. Changes to the content may not always be associated with version changes to the resource. (defined for API consistency)
    property meta : TFhirMeta read FMeta write SetMeta;
    // The metadata about the resource. This is content that is maintained by the infrastructure. Changes to the content may not always be associated with version changes to the resource.
    property metaElement : TFhirMeta read FMeta write SetMeta;

    // Typed access to A reference to a set of rules that were followed when the resource was constructed, and which must be understood when processing the content.
    property implicitRules : String read GetImplicitRulesST write SetImplicitRulesST;
    // A reference to a set of rules that were followed when the resource was constructed, and which must be understood when processing the content.
    property implicitRulesElement : TFhirUri read FImplicitRules write SetImplicitRules;

    // Typed access to The base language in which the resource is written.
    property language : String read GetLanguageST write SetLanguageST;
    // The base language in which the resource is written.
    property languageElement : TFhirCode read FLanguage write SetLanguage;

  end;

  TFhirResourceClass = class of TFhirResource;

  TFhirResourceListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirResourceList;
    function GetCurrent : TFhirResource;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirResourceList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirResource read GetCurrent;
  end;

  TFhirResourceList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirResource;
    procedure SetItemN(index : Integer; value : TFhirResource);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirResourceList; Overload;
    function Clone : TFhirResourceList; Overload;
    function GetEnumerator : TFhirResourceListEnumerator;

    // Add an already existing FhirResource to the end of the list.
    procedure AddItem(value : TFhirResource); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirResource) : Integer;

    // Insert an existing FhirResource before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirResource);

    // Get the iIndexth FhirResource. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirResource);

    // The number of items in the collection
    function Item(index : Integer) : TFhirResource;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirResources[index : Integer] : TFhirResource read GetItemN write SetItemN; default;
  End;

  // A resource that includes narrative, extensions, and contained resources.
  TFhirDomainResource = class abstract (TFhirResource)
  protected
    FText : TFhirNarrative;
    FcontainedList : TFhirResourceList;
    FextensionList : TFhirExtensionList;
    FmodifierExtensionList : TFhirExtensionList;
    Procedure SetText(value : TFhirNarrative);
    function GetContainedList : TFhirResourceList;
    function GetHasContainedList : Boolean;
    function GetExtensionList : TFhirExtensionList;
    function GetHasExtensionList : Boolean;
    function GetModifierExtensionList : TFhirExtensionList;
    function GetHasModifierExtensionList : Boolean;

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    Procedure listDomainResourceFieldsInOrder(fields: TStringList);
    procedure listFieldsInOrder(fields : TStringList); override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirDomainResource; overload;
    function Clone : TFhirDomainResource; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function hasExtensions : boolean; override;
    function fhirType : string; override;
    function isDomainResource : boolean; override;
    function hasExtension(url : string) : boolean; override;
    function getExtensionString(url : String) : String; override;
    function extensionCount(url : String) : integer; override;
    function extensions(url : String) : TFslList<TFHIRObject>; override;
    procedure addExtension(url : String; value : TFHIRObject); override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to A human-readable narrative that contains a summary of the resource, and may be used to represent the content of the resource to a human. The narrative need not encode all the structured data, but is required to contain sufficient detail to make it "clinically safe" for a human to just read the narrative. Resource definitions may define what content should be represented in the narrative to ensure clinical safety. (defined for API consistency)
    property text : TFhirNarrative read FText write SetText;
    // A human-readable narrative that contains a summary of the resource, and may be used to represent the content of the resource to a human. The narrative need not encode all the structured data, but is required to contain sufficient detail to make it "clinically safe" for a human to just read the narrative. Resource definitions may define what content should be represented in the narrative to ensure clinical safety.
    property textElement : TFhirNarrative read FText write SetText;

    // These resources do not have an independent existence apart from the resource that contains them - they cannot be identified independently, and nor can they have their own independent transaction scope.
    property containedList : TFhirResourceList read GetContainedList;
    property hasContainedList : boolean read GetHasContainedList;

    // May be used to represent additional information that is not part of the basic definition of the resource. In order to make the use of extensions safe and manageable, there is a strict set of governance  applied to the definition and use of extensions. Though any implementer is allowed to define an extension, there is a set of requirements that SHALL be met as part of the definition of the extension.
    property extensionList : TFhirExtensionList read GetExtensionList;
    property hasExtensionList : boolean read GetHasExtensionList;

    // May be used to represent additional information that is not part of the basic definition of the resource, and that modifies the understanding of the element that contains it. Usually modifier elements provide negation or qualification. In order to make the use of extensions safe and manageable, there is a strict set of governance applied to the definition and use of extensions. Though any implementer is allowed to define an extension, there is a set of requirements that SHALL be met as part of the definition of the extension. Applications processing a resource are required to check for modifier extensions.
    property modifierExtensionList : TFhirExtensionList read GetModifierExtensionList;
    property hasModifierExtensionList : boolean read GetHasModifierExtensionList;
  end;

  TFhirDomainResourceListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDomainResourceList;
    function GetCurrent : TFhirDomainResource;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirDomainResourceList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDomainResource read GetCurrent;
  end;

  TFhirDomainResourceList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDomainResource;
    procedure SetItemN(index : Integer; value : TFhirDomainResource);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDomainResourceList; Overload;
    function Clone : TFhirDomainResourceList; Overload;
    function GetEnumerator : TFhirDomainResourceListEnumerator;

    // Add an already existing FhirDomainResource to the end of the list.
    procedure AddItem(value : TFhirDomainResource); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDomainResource) : Integer;

    // Insert an existing FhirDomainResource before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDomainResource);

    // Get the iIndexth FhirDomainResource. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDomainResource);

    // The number of items in the collection
    function Item(index : Integer) : TFhirDomainResource;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirDomainResources[index : Integer] : TFhirDomainResource read GetItemN write SetItemN; default;
  End;

implementation

uses
  fhir2_utilities;

{ TFhirResource }

constructor TFhirResource.Create;
begin
  inherited;
end;

destructor TFhirResource.Destroy;
begin
  FId.free;
  FMeta.free;
  FImplicitRules.free;
  FLanguage.free;
  inherited;
end;

procedure TFhirResource.Assign(oSource : TFslObject);
begin
  inherited;
  idElement := TFhirResource(oSource).idElement.Clone;
  meta := TFhirResource(oSource).meta.Clone;
  implicitRulesElement := TFhirResource(oSource).implicitRulesElement.Clone;
  languageElement := TFhirResource(oSource).languageElement.Clone;
end;

procedure TFhirResource.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'id') Then
     list.add(self.link, 'id', FId.Link);
  if (child_name = 'meta') Then
     list.add(self.link, 'meta', FMeta.Link);
  if (child_name = 'implicitRules') Then
     list.add(self.link, 'implicitRules', FImplicitRules.Link);
  if (child_name = 'language') Then
     list.add(self.link, 'language', FLanguage.Link);
end;

function TFhirResource.getId: string;
begin
  result := GetIdST;
end;

procedure TFhirResource.setIdValue(id: String);
begin
  SetIdSt(id);
end;

procedure TFhirResource.SetProfileVersion(v : TFHIRVersion);
var
   i : integer;
begin
  if Meta = nil then
    Meta := TFhirMeta.Create;
  for i := Meta.profileList.Count - 1 downto 0 do
    if isVersionUrl(Meta.profileList[i].value, fhirType) then
      Meta.profileList.DeleteByIndex(i);
  Meta.profileList.Append.value := 'http://hl7.org/fhir/'+PF_CONST[v]+'/StructureDefinition/'+fhirType;
end;

function TFhirResource.GetProfileVersion : TFHIRVersion;
var
   p : TFHIRUri;
begin
  result := fhirVersionUnknown;
  if Meta <> nil then
    for p in Meta.profileList do
    begin
      if (p.value = 'http://hl7.org/fhir/1.0/StructureDefinition/'+fhirType) then
        exit(fhirVersionRelease2);
      if (p.value = 'http://hl7.org/fhir/3.0/StructureDefinition/'+fhirType) then
        exit(fhirVersionRelease3);
      if (p.value = 'http://hl7.org/fhir/3.4/StructureDefinition/'+fhirType) then
        exit(fhirVersionRelease4);
    end;
end;

procedure TFhirResource.checkNoImplicitRules(place, role: String);
begin
  if implicitRules <> '' then
    raise EUnsafeOperation.Create('The resource '+role+' has an unknown implicitRules tag at '+place);
end;

procedure TFhirResource.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'id', 'id', false, TFhirId, FId.Link));{2}
  oList.add(TFHIRProperty.create(self, 'meta', 'Meta', false, TFhirMeta, FMeta.Link));{2}
  oList.add(TFHIRProperty.create(self, 'implicitRules', 'uri', false, TFhirUri, FImplicitRules.Link));{2}
  oList.add(TFHIRProperty.create(self, 'language', 'code', false, TFhirCode, FLanguage.Link));{2}
end;

procedure TFhirResource.listResourceFieldsInOrder(fields: TStringList);
begin
  fields.add('id');
  fields.add('meta');
  fields.add('implicitRules');
  fields.add('language');
end;

function TFhirResource.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'id') then
  begin
    IdElement := asId(propValue){5a};
    result := propValue;
  end
  else if (propName = 'meta') then
  begin
    Meta := propValue as TFhirMeta{4b};
    result := propValue;
  end
  else if (propName = 'implicitRules') then
  begin
    ImplicitRulesElement := asUri(propValue){5a};
    result := propValue;
  end
  else if (propName = 'language') then
  begin
    LanguageElement := asCode(propValue);
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirResource.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirResource.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'id') then result := TFhirId.create() {5b}
  else if (propName = 'meta') then result := TFhirMeta.create(){4b}
  else if (propName = 'implicitRules') then result := TFhirUri.create() {5b}
  else if (propName = 'language') then result := TFhirCode.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirResource.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'id') then result := 'id'
  else if (propName = 'meta') then result := 'Meta'
  else if (propName = 'implicitRules') then result := 'uri'
  else if (propName = 'language') then result := 'code'
  else result := inherited getTypesForProperty(propName);
end;

function TFhirResource.hasExtensions: boolean;
begin
  result := false;
end;

procedure TFhirResource.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'id') then IdElement := nil
  else if (propName = 'meta') then MetaElement := nil
  else if (propName = 'implicitRules') then ImplicitRulesElement := nil
  else if (propName = 'language') then LanguageElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirResource.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'id') then IdElement := asId(new){5b}
  else if (propName = 'meta') then MetaElement := new as TFhirMeta{4}
  else if (propName = 'implicitRules') then ImplicitRulesElement := asUri(new){5b}
  else if (propName = 'language') then LanguageElement := asCode(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirResource.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirResource.fhirType : string;
begin
  result := 'Resource';
end;

function TFhirResource.equals(other : TObject) : boolean; 
var
  o : TFhirResource;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirResource)) then
    result := false
  else
  begin
    o := TFhirResource(other);
    result := compareDeep(idElement, o.idElement, true) and compareDeep(metaElement, o.metaElement, true) and 
      compareDeep(implicitRulesElement, o.implicitRulesElement, true) and compareDeep(languageElement, o.languageElement, true);
  end;
end;

function TFhirResource.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FId) and isEmptyProp(FMeta) and isEmptyProp(FImplicitRules) and isEmptyProp(FLanguage);
end;

function TFhirResource.Link : TFhirResource;
begin
  result := TFhirResource(inherited Link);
end;

function TFhirResource.Clone : TFhirResource;
begin
  result := TFhirResource(inherited Clone);
end;

procedure TFhirResource.listFieldsInOrder(fields : TStringList);
begin
  fields.add('id');
  fields.add('meta');
  fields.add('implicitRules');
  fields.add('language');
end;

{ TFhirResource }

Procedure TFhirResource.SetId(value : TFhirId);
begin
  FId.free;
  FId := value;
end;

Function TFhirResource.GetIdST : String;
begin
  if FId = nil then
    result := ''
  else
    result := FId.value;
end;

Procedure TFhirResource.SetIdST(value : String);
begin
  if value <> '' then
  begin
    if FId = nil then
      FId := TFhirId.create;
    FId.value := value
  end
  else if FId <> nil then
    FId.value := '';
end;

Procedure TFhirResource.SetMeta(value : TFhirMeta);
begin
  FMeta.free;
  FMeta := value;
end;

Procedure TFhirResource.SetImplicitRules(value : TFhirUri);
begin
  FImplicitRules.free;
  FImplicitRules := value;
end;

Function TFhirResource.GetImplicitRulesST : String;
begin
  if FImplicitRules = nil then
    result := ''
  else
    result := FImplicitRules.value;
end;

Procedure TFhirResource.SetImplicitRulesST(value : String);
begin
  if value <> '' then
  begin
    if FImplicitRules = nil then
      FImplicitRules := TFhirUri.create;
    FImplicitRules.value := value
  end
  else if FImplicitRules <> nil then
    FImplicitRules.value := '';
end;

Procedure TFhirResource.SetLanguage(value : TFhirCode);
begin
  FLanguage.free;
  FLanguage := value;
end;

Function TFhirResource.GetLanguageST : String;
begin
  if FLanguage = nil then
    result := ''
  else
    result := FLanguage.value;
end;

Procedure TFhirResource.SetLanguageST(value : String);
begin
  if value <> '' then
  begin
    if FLanguage = nil then
      FLanguage := TFhirCode.create;
    FLanguage.value := value
  end
  else if FLanguage <> nil then
    FLanguage.value := '';
end;

{ TFhirResourceListEnumerator }

Constructor TFhirResourceListEnumerator.Create(list : TFhirResourceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirResourceListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirResourceListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirResourceListEnumerator.GetCurrent : TFhirResource;
begin
  Result := FList[FIndex];
end;

function TFhirResourceListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirResourceList }
procedure TFhirResourceList.AddItem(value: TFhirResource);
begin
  assert(value.ClassName = 'TFhirResource', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirResource');
  add(value);
end;

procedure TFhirResourceList.ClearItems;
begin
  Clear;
end;

function TFhirResourceList.GetEnumerator : TFhirResourceListEnumerator;
begin
  result := TFhirResourceListEnumerator.Create(self.link);
end;

function TFhirResourceList.Clone: TFhirResourceList;
begin
  result := TFhirResourceList(inherited Clone);
end;

function TFhirResourceList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirResourceList.GetItemN(index: Integer): TFhirResource;
begin
  result := TFhirResource(ObjectByIndex[index]);
end;

function TFhirResourceList.ItemClass: TFslObjectClass;
begin
  result := TFhirResource;
end;
function TFhirResourceList.IndexOf(value: TFhirResource): Integer;
begin
  result := IndexByReference(value);
end;

procedure TFhirResourceList.InsertItem(index: Integer; value: TFhirResource);
begin
  assert(value is TFhirResource);
  Inherited Insert(index, value);
end;

function TFhirResourceList.Item(index: Integer): TFhirResource;
begin
  result := TFhirResource(ObjectByIndex[index]);
end;

function TFhirResourceList.Link: TFhirResourceList;
begin
  result := TFhirResourceList(inherited Link);
end;

procedure TFhirResourceList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirResourceList.SetItemByIndex(index: Integer; value: TFhirResource);
begin
  assert(value is TFhirResource);
  FhirResources[index] := value;
end;

procedure TFhirResourceList.SetItemN(index: Integer; value: TFhirResource);
begin
  assert(value is TFhirResource);
  ObjectByIndex[index] := value;
end;

{ TFhirDomainResource }

constructor TFhirDomainResource.Create;
begin
  inherited;
end;

destructor TFhirDomainResource.Destroy;
begin
  FText.free;
  FContainedList.Free;
  FExtensionList.Free;
  FModifierExtensionList.Free;
  inherited;
end;

procedure TFhirDomainResource.Assign(oSource : TFslObject);
begin
  inherited;
  text := TFhirDomainResource(oSource).text.Clone;
  if (TFhirDomainResource(oSource).FContainedList = nil) then
  begin
    FContainedList.free;
    FContainedList := nil;
  end
  else
  begin
    if FContainedList = nil then
      FContainedList := TFhirResourceList.Create;
    FContainedList.Assign(TFhirDomainResource(oSource).FContainedList);
  end;
  if (TFhirDomainResource(oSource).FExtensionList = nil) then
  begin
    FExtensionList.free;
    FExtensionList := nil;
  end
  else
  begin
    if FExtensionList = nil then
      FExtensionList := TFhirExtensionList.Create;
    FExtensionList.Assign(TFhirDomainResource(oSource).FExtensionList);
  end;
  if (TFhirDomainResource(oSource).FModifierExtensionList = nil) then
  begin
    FModifierExtensionList.free;
    FModifierExtensionList := nil;
  end
  else
  begin
    if FModifierExtensionList = nil then
      FModifierExtensionList := TFhirExtensionList.Create;
    FModifierExtensionList.Assign(TFhirDomainResource(oSource).FModifierExtensionList);
  end;
end;

procedure TFhirDomainResource.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'text') Then
     list.add(self.link, 'text', FText.Link);
  if (child_name = 'contained') Then
    list.addAll(self, 'contained', FContainedList);
  if (child_name = 'extension') Then
    list.addAll(self, 'extension', FExtensionList);
  if (child_name = 'modifierExtension') Then
    list.addAll(self, 'modifierExtension', FModifierExtensionList);
end;

procedure TFhirDomainResource.addExtension(url: String; value: TFHIRObject);
var
  ex : TFhirExtension;
begin
  ex := extensionList.Append;
  ex.url := url;
  ex.value := value as TFhirType;
end;

function TFhirDomainResource.extensionCount(url: String): integer;
var
  ex : TFhirExtension;
begin
  result := 0;
  for ex in ExtensionList do
    if ex.url = url then
      inc(result);
end;

function TFhirDomainResource.extensions(url: String): TFslList<TFHIRObject>;
var
  ex : TFhirExtension;
begin
  result := TFslList<TFHIRObject>.create;
  try
    for ex in ExtensionList do
      if ex.url = url then
        result.Add(ex.Link);
    result.link;
  finally
    result.Free;
  end;
end;

function TFhirDomainResource.isDomainResource: boolean;
begin
  result := true;
end;

function TFhirDomainResource.getExtensionString(url: String): String;
var
  ex : TFhirExtension;
begin
  result := '';
  for ex in ExtensionList do
  begin
    if ex.url = url then
    begin
      if not ex.value.isPrimitive then
        raise EFHIRException.create('Complex extension '+url)
      else if result <> '' then
        raise EFHIRException.create('Duplicate extension '+url)
      else
        result := ex.value.primitiveValue;
    end;
  end;
end;

function TFhirDomainResource.hasExtension(url: string): boolean;
var
  ex : TFhirExtension;
begin
  result := false;
  for ex in ExtensionList do
    if ex.url = url then
      exit(true);
end;

function TFhirDomainResource.hasExtensions: boolean;
begin
  result := (FExtensionList.Count = 0) or (FModifierExtensionList.Count = 0);
end;

procedure TFhirDomainResource.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'text', 'Narrative', false, TFhirNarrative, FText.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contained', 'Resource', true, TFhirResource, FContainedList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'extension', 'Extension', true, TFhirExtension, FExtensionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'modifierExtension', 'Extension', true, TFhirExtension, FModifierExtensionList.Link)){3};
end;

function TFhirDomainResource.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'text') then
  begin
    Text := propValue as TFhirNarrative{4b};
    result := propValue;
  end
  else if (propName = 'contained') then
  begin
    ContainedList.add(propValue as TFhirResource){2a};
    result := propValue;
  end
  else if (propName = 'extension') then
  begin
    ExtensionList.add(propValue as TFhirExtension){2a};
    result := propValue;
  end
  else if (propName = 'modifierExtension') then
  begin
    ModifierExtensionList.add(propValue as TFhirExtension){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirDomainResource.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'contained') then ContainedList.insertItem(index, propValue as TFhirResource){2a}
  else if (propName = 'extension') then ExtensionList.insertItem(index, propValue as TFhirExtension){2a}
  else if (propName = 'modifierExtension') then ModifierExtensionList.insertItem(index, propValue as TFhirExtension){2a}
  else inherited;
end;

function TFhirDomainResource.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'text') then result := TFhirNarrative.create(){4b}
  else if (propName = 'extension') then result := ExtensionList.new(){2}
  else if (propName = 'modifierExtension') then result := ModifierExtensionList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirDomainResource.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'text') then result := 'Narrative'
  else if (propName = 'contained') then result := 'Resource'
  else if (propName = 'extension') then result := 'Extension'
  else if (propName = 'modifierExtension') then result := 'Extension'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirDomainResource.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'text') then TextElement := nil
  else if (propName = 'contained') then deletePropertyValue('contained', ContainedList, value) {2}
  else if (propName = 'extension') then deletePropertyValue('extension', ExtensionList, value) {2}
  else if (propName = 'modifierExtension') then deletePropertyValue('modifierExtension', ModifierExtensionList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirDomainResource.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'text') then TextElement := new as TFhirNarrative{4}
  else if (propName = 'contained') then replacePropertyValue('contained', ContainedList, existing, new) {2}
  else if (propName = 'extension') then replacePropertyValue('extension', ExtensionList, existing, new) {2}
  else if (propName = 'modifierExtension') then replacePropertyValue('modifierExtension', ModifierExtensionList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirDomainResource.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'contained') then ContainedList.move(source, destination){2a}
  else if (propName = 'extension') then ExtensionList.move(source, destination){2a}
  else if (propName = 'modifierExtension') then ModifierExtensionList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirDomainResource.fhirType : string;
begin
  result := 'DomainResource';
end;

function TFhirDomainResource.equals(other : TObject) : boolean;
var
  o : TFhirDomainResource;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDomainResource)) then
    result := false
  else
  begin
    o := TFhirDomainResource(other);
    result := compareDeep(textElement, o.textElement, true) and compareDeep(containedList, o.containedList, true) and
      compareDeep(extensionList, o.extensionList, true) and compareDeep(modifierExtensionList, o.modifierExtensionList, true);
  end;
end;

function TFhirDomainResource.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FText) and isEmptyProp(FcontainedList) and isEmptyProp(FextensionList) and isEmptyProp(FmodifierExtensionList);
end;

function TFhirDomainResource.Link : TFhirDomainResource;
begin
  result := TFhirDomainResource(inherited Link);
end;

function TFhirDomainResource.Clone : TFhirDomainResource;
begin
  result := TFhirDomainResource(inherited Clone);
end;

procedure TFhirDomainResource.listDomainResourceFieldsInOrder(fields: TStringList);
begin
  listResourceFieldsInOrder(fields);
  fields.add('text');
  fields.add('contained');
  fields.add('extension');
  fields.add('modifierExtension');
end;

procedure TFhirDomainResource.listFieldsInOrder(fields : TStringList);
begin
  listResourceFieldsInOrder(fields);
  fields.add('text');
  fields.add('contained');
  fields.add('extension');
  fields.add('modifierExtension');
end;

{ TFhirDomainResource }

Procedure TFhirDomainResource.SetText(value : TFhirNarrative);
begin
  FText.free;
  FText := value;
end;

Function TFhirDomainResource.GetContainedList : TFhirResourceList;
begin
  if FContainedList = nil then
    FContainedList := TFhirResourceList.Create;
  result := FContainedList;
end;

Function TFhirDomainResource.GetHasContainedList : boolean;
begin
  result := (FContainedList <> nil) and (FContainedList.count > 0);
end;

Function TFhirDomainResource.GetExtensionList : TFhirExtensionList;
begin
  if FExtensionList = nil then
    FExtensionList := TFhirExtensionList.Create;
  result := FExtensionList;
end;

Function TFhirDomainResource.GetHasExtensionList : boolean;
begin
  result := (FExtensionList <> nil) and (FExtensionList.count > 0);
end;

Function TFhirDomainResource.GetModifierExtensionList : TFhirExtensionList;
begin
  if FModifierExtensionList = nil then
    FModifierExtensionList := TFhirExtensionList.Create;
  result := FModifierExtensionList;
end;

Function TFhirDomainResource.GetHasModifierExtensionList : boolean;
begin
  result := (FModifierExtensionList <> nil) and (FModifierExtensionList.count > 0);
end;

{ TFhirDomainResourceListEnumerator }

Constructor TFhirDomainResourceListEnumerator.Create(list : TFhirDomainResourceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDomainResourceListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDomainResourceListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDomainResourceListEnumerator.GetCurrent : TFhirDomainResource;
begin
  Result := FList[FIndex];
end;

function TFhirDomainResourceListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirDomainResourceList }
procedure TFhirDomainResourceList.AddItem(value: TFhirDomainResource);
begin
  assert(value.ClassName = 'TFhirDomainResource', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDomainResource');
  add(value);
end;

procedure TFhirDomainResourceList.ClearItems;
begin
  Clear;
end;

function TFhirDomainResourceList.GetEnumerator : TFhirDomainResourceListEnumerator;
begin
  result := TFhirDomainResourceListEnumerator.Create(self.link);
end;

function TFhirDomainResourceList.Clone: TFhirDomainResourceList;
begin
  result := TFhirDomainResourceList(inherited Clone);
end;

function TFhirDomainResourceList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDomainResourceList.GetItemN(index: Integer): TFhirDomainResource;
begin
  result := TFhirDomainResource(ObjectByIndex[index]);
end;

function TFhirDomainResourceList.ItemClass: TFslObjectClass;
begin
  result := TFhirDomainResource;
end;
function TFhirDomainResourceList.IndexOf(value: TFhirDomainResource): Integer;
begin
  result := IndexByReference(value);
end;

procedure TFhirDomainResourceList.InsertItem(index: Integer; value: TFhirDomainResource);
begin
  assert(value is TFhirDomainResource);
  Inherited Insert(index, value);
end;

function TFhirDomainResourceList.Item(index: Integer): TFhirDomainResource;
begin
  result := TFhirDomainResource(ObjectByIndex[index]);
end;

function TFhirDomainResourceList.Link: TFhirDomainResourceList;
begin
  result := TFhirDomainResourceList(inherited Link);
end;

procedure TFhirDomainResourceList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDomainResourceList.SetItemByIndex(index: Integer; value: TFhirDomainResource);
begin
  assert(value is TFhirDomainResource);
  FhirDomainResources[index] := value;
end;

procedure TFhirDomainResourceList.SetItemN(index: Integer; value: TFhirDomainResource);
begin
  assert(value is TFhirDomainResource);
  ObjectByIndex[index] := value;
end;

end.

