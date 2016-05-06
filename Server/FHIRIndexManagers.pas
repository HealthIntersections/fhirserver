unit FHIRIndexManagers;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Classes, Generics.Collections,
  AdvObjects, AdvObjectLists, AdvNames, AdvXmlBuilders, AdvGenerics,
  EncodeSupport, DecimalSupport, HL7v2dateSupport, StringSupport, GuidSupport,
  KDBManager,
  FHIRBase, FHIRContext, FhirSupport, FHIRResources, FHIRConstants, FHIRTypes, FHIRTags, FHIRUtilities, FHIRParser, FHIRPath, FHIRProfileUtilities, FHIRXhtml,
  TerminologyServer, ServerUtilities,
  UcumServices;

Const
  INDEX_ENTRY_LENGTH = 210;
  NARRATIVE_INDEX_NAME = '_text';

Type
  TKeyType = (ktResource, ktEntries, ktCompartment);

  TFHIRGetNextKey = function (keytype : TKeyType; aType : String; var id : string) : Integer of Object;

  TFhirIndex = class (TAdvObject)
  private
    FResourceType : String;
    FKey: Integer;
    FName: String;
    FDescription : String;
    FSearchType: TFhirSearchParamTypeEnum;
    FTargetTypes : TArray<String>;
    FURI: String;
    FPath : String;
    FUsage : TFhirSearchXpathUsageEnum;
    FMapping : String;
  public
    function Link : TFhirIndex; Overload;
    function Clone : TFhirIndex; Overload;
    procedure Assign(source : TAdvObject); Override;

    property ResourceType : String read FResourceType write FResourceType;
    property Name : String read FName write FName;
    Property Description : String read FDescription write FDescription;
    Property Key : Integer read FKey write FKey;
    Property SearchType : TFhirSearchParamTypeEnum read FSearchType write FSearchType;
    Property TargetTypes : TArray<String> read FTargetTypes write FTargetTypes;
    Property URI : String read FURI write FURI;
    Property Path : String read FPath;
    Property Usage : TFhirSearchXpathUsageEnum read FUsage;
    Property Mapping : String read FMapping write FMapping;

    function specifiedTarget : String;
  end;

  TFhirIndexList = class (TAdvObjectList)
  private
    function GetItemN(iIndex: integer): TFhirIndex;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    function Link : TFhirIndexList; Overload;

    function getByName(atype : String; name : String): TFhirIndex;
    function add(aResourceType : String; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsageEnum): TFhirIndex; overload;
    function add(aResourceType : String; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsageEnum; url : String): TFhirIndex; overload;
    function add(resourceType : String; sp : TFhirSearchParameter): TFhirIndex; overload;
    Property Item[iIndex : integer] : TFhirIndex read GetItemN; default;
  end;

  TFhirComposite = class (TAdvObject)
  private
    FResourceType : String;
    FKey: Integer;
    FName: String;
    FComponents : TDictionary<String, String>;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Link : TFhirComposite; Overload;
    function Clone : TFhirComposite; Overload;
    procedure Assign(source : TAdvObject); Override;

    property ResourceType : String read FResourceType write FResourceType;
    property Name : String read FName write FName;
    Property Key : Integer read FKey write FKey;
    Property Components : TDictionary<String, String> read FComponents;
  end;

  TFhirCompositeList = class (TAdvObjectList)
  private
    function GetItemN(iIndex: integer): TFhirComposite;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    function Link : TFhirCompositeList; Overload;

    function getByName(aType : String; name : String): TFhirComposite;
    procedure add(aResourceType : String; name : String; components : array of String); overload;
    Property Item[iIndex : integer] : TFhirComposite read GetItemN; default;
  end;

  TFhirIndexEntry = class (TAdvObject)
  private
    FKey: integer;
    FEntryKey : integer;
    FIndexKey : integer;
    FValue1: String;
    FValue2: String;
    FRefType: integer;
    FTarget: integer;
    FConcept : integer;
    FType: TFhirSearchParamTypeEnum;
    FParent: Integer;
    FFlag: boolean;
    FName : String;
    FTargetType : TFhirResourceType;
  public
    function Link : TFhirIndexEntry; overload;

    Property EntryKey : Integer Read FEntryKey write FEntryKey;
    Property IndexKey : Integer Read FIndexKey write FIndexKey;
    property Key : integer read FKey write FKey;
    Property Parent : Integer read FParent write FParent;
    property Value1 : String read FValue1 write FValue1;
    property Value2 : String read FValue2 write FValue2;
    property RefType : integer read FRefType write FRefType;
    Property target : integer read FTarget write FTarget;
    Property concept : integer read FConcept write FConcept;
    Property type_ : TFhirSearchParamTypeEnum read FType write FType;
    Property flag : boolean read FFlag write FFlag;
    Property TargetType : TFhirResourceType read FTargetType write FTargetType;
  end;

  TFhirIndexEntryList = class (TAdvList<TFhirIndexEntry>)
  private
    FKeyEvent : TFHIRGetNextKey;
    procedure filter(indexes : TFhirIndexList; name : String; list : TAdvList<TFhirIndexEntry>);
  public
    function add(key, parent : integer; index : TFhirIndex; ref : integer; value1, value2 : String; target : integer; ttype : TFHIRResourceType; type_ : TFhirSearchParamTypeEnum; flag : boolean = false; concept : integer = 0) : integer; overload;
    function add(key, parent : integer; index : TFhirComposite) : integer; overload;
    property KeyEvent : TFHIRGetNextKey read FKeyEvent write FKeyEvent;
  end;

  TFhirCompartmentEntry = class (TAdvObject)
  private
    FCKey: integer;
    FKey: integer;
    FId: string;
    FTKey: integer;
  public
    function link : TFhirCompartmentEntry; overload;
    property Key : integer read FKey write FKey; // id of resource that is in a compartment
    property TKey : integer read FTKey write FTKey; // resource type key for the compartment type
    property Id : string read FId write FId; // field two of composite id for compartment - compartment id
    property CKey : integer read FCKey write FCKey; // key for the resource that creates this compartment
  end;

  TFhirCompartmentEntryList = class (TAdvList<TFhirCompartmentEntry>)
  public
    procedure add(key, tkey, ckey : integer; id : string);
    procedure removeById(id : String);
  end;

  TFhirIndexSpaces = class (TAdvObject)
  private
    FDB : TKDBConnection;
    FSpaces : TStringList;
  public
    constructor Create(db : TKDBConnection);
    destructor Destroy; override;
    function ResolveSpace(space : String):integer;
  end;

  TFHIRIndexInformation = class (TAdvObject)
  private
    FIndexes : TFhirIndexList;
    FComposites : TFhirCompositeList;
    FCompartments : TFHIRCompartmentList;
    FNarrativeIndex : Integer;
    procedure buildIndexes;
  public
    constructor Create; Override;
    destructor Destroy; override;
    function Link : TFHIRIndexInformation; overload;
    procedure ReconcileIndexes(connection : TKDBConnection);

    Function GetTargetsByName(types : TArray<String>; name : String) : TArray<String>;
    Function GetKeyByName(name : String) : integer;
    Function GetTypeByName(types : TArray<String>; name : String) : TFhirSearchParamTypeEnum;
    Function GetComposite(types : TArray<String>; name : String; var otypes : TArray<String>) : TFhirComposite;

    property Indexes : TFhirIndexList read FIndexes;
    property Composites : TFhirCompositeList read FComposites;
    Property NarrativeIndex : integer read FNarrativeIndex;
    property Compartments : TFHIRCompartmentList read FCompartments;
  end;

  {$HINTS OFF}
  TFhirIndexManager = class (TAdvObject)
  private
    FInfo : TFHIRIndexInformation;
    FKeyEvent : TFHIRGetNextKey;
    FSpaces : TFhirIndexSpaces;
    FCompartments : TFhirCompartmentEntryList;
    FEntries : TFhirIndexEntryList;
    FMasterKey : Integer;
    FBases : TStringList;
    FValidationInfo : TWorkerContext;
    FTerminologyServer : TTerminologyServer;
    FResConfig: TAdvMap<TFHIRResourceConfig>;

    procedure GetBoundaries(value : String; comparator: TFhirQuantityComparatorEnum; var low, high : String);

    function EncodeXhtml(r : TFhirDomainResource) : TBytes;

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
    {$IFDEF FHIR2}
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
    procedure BuildIndexValuesDeviceUseRequest(key : integer; id : string; context : TFhirResource; resource : TFhirDeviceUseRequest);
    procedure BuildIndexValuesDeviceUseStatement(key : integer; id : string; context : TFhirResource; resource : TFhirDeviceUseStatement);
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
    {$ENDIF}

    procedure processCompartmentTags(key : integer; id: String; tags : TFHIRTagList);
    procedure processUnCompartmentTags(key : integer; id: String; tags : TFHIRTagList);
    procedure SetTerminologyServer(const Value: TTerminologyServer);

    procedure checkTags(resource : TFhirResource; tags : TFHIRTagList);
    procedure evaluateByFHIRPath(key : integer; context, resource : TFhirResource);
    function transform(base : TFHIRBase; uri : String) : TFHIRBase;
  public
    constructor Create(aSpaces : TFhirIndexSpaces; aInfo : TFHIRIndexInformation; ValidationInfo : TWorkerContext; ResConfig: TAdvMap<TFHIRResourceConfig>);
    destructor Destroy; override;
    function Link : TFHIRIndexManager; overload;
    property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;
    property Bases : TStringList read FBases write FBases;
    function execute(key : integer; id: String; resource : TFhirResource; tags : TFHIRTagList) : String;
    property KeyEvent : TFHIRGetNextKey read FKeyEvent write FKeyEvent;
    property Definitions : TFHIRIndexInformation read FInfo;
  end;

function normaliseDecimal(v : String): String;

implementation

uses
  FHIRIndexInformation;

Function EncodeNYSIISValue(value : TFhirString) : String; overload;
begin
  if value = nil then
    result := ''
  else
  result := EncodeNYSIIS(value.value);
end;


{ TFhirIndex }

procedure TFhirIndex.assign(source: TAdvObject);
begin
  inherited;
  FKey := TFhirIndex(source).FKey;
  FName := TFhirIndex(source).FName;
  FSearchType := TFhirIndex(source).FSearchType;
  FResourceType := TFhirIndex(source).FResourceType;
  TargetTypes := TFhirIndex(source).TargetTypes;
end;

function TFhirIndex.Clone: TFhirIndex;
begin
  result := TFhirIndex(Inherited Clone);
end;

function TFhirIndex.Link: TFhirIndex;
begin
  result := TFhirIndex(Inherited Link);
end;

function TFhirIndex.specifiedTarget: String;
var
  a : String;
  s : String;
begin
  result := '';
  for a in ALL_RESOURCE_TYPE_NAMES do
    for s in FTargetTypes do
      if s = a then
        if result = '' then
          result := a
        else
          exit('');
end;

{ TFhirIndexList }

function TFhirIndexList.add(aResourceType : String; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsageEnum) : TFHIRIndex;
begin
  result := add(aResourceType, name, description, aType, aTargetTypes, path, usage, 'http://hl7.org/fhir/SearchParameter/'+aResourceType+'-'+name.Replace('[', '').Replace(']', ''));
end;


function TFhirIndexList.add(aResourceType : String; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : Array of String; path : String; usage : TFhirSearchXpathUsageEnum; url: String) : TFHIRIndex;
var
  ndx : TFhirIndex;
  i : integer;
begin
  ndx := TFhirIndex.Create;
  try
    ndx.ResourceType := aResourceType;
    ndx.name := name;
    ndx.SearchType := aType;
    SetLength(ndx.FTargetTypes, length(aTargetTypes));
    for i := 0 to length(ndx.TargetTypes)-1 do
      ndx.FTargetTypes[i] := aTargetTypes[i];
    ndx.URI := url;
    ndx.description := description;
    ndx.FPath := path;
    ndx.FUsage := usage;
    inherited add(ndx.Link);
    result := ndx;
  finally
    ndx.free;
  end;
end;

function TFhirIndexList.add(resourceType : String; sp: TFhirSearchParameter) : TFhirIndex;
var
  targets : TArray<String>;
  i : integer;
begin
  SetLength(targets, sp.targetList.Count);
  for i := 0 to sp.targetList.Count - 1 do
    targets[i] := sp.targetList[i].value;

  result := add(resourceType, sp.name, sp.description, sp.type_, targets, sp.expression, sp.xpathUsage);
  if (sp.hasExtension('http://www.healthintersections.com.au/fhir/StructureDefinition/index-transform')) then
    result.Mapping := sp.getExtensionString('http://www.healthintersections.com.au/fhir/StructureDefinition/index-transform');
end;

function TFhirIndexList.getByName(atype, name: String): TFhirIndex;
var
  i : integer;
begin
  i := 0;
  result := nil;
  while (result = nil) and (i < Count) do
  begin
    if SameText(item[i].name, name) and (item[i].FResourceType = atype) then
      result := item[i];
    inc(i);
  end;
end;

function TFhirIndexList.GetItemN(iIndex: integer): TFhirIndex;
begin
  result := TFhirIndex(ObjectByIndex[iIndex]);
end;

function TFhirIndexList.ItemClass: TAdvObjectClass;
begin
  result := TFhirIndex;
end;

function TFhirIndexList.Link: TFhirIndexList;
begin
  result := TFhirIndexList(Inherited Link);
end;

function findPrefix(var value : String; subst : String) : boolean;
begin
  result := value.StartsWith(subst);
  if result then
    value := value.Substring(subst.Length);
end;

function normaliseDecimal(v : String): String;
var
  neg : boolean;
begin
  neg := findPrefix(v, '-');
  if not v.Contains('.') then
    result := StringPadRight(StringPadLeft(v, '0', 40)+'.', '0', 91)
  else if (v.IndexOf('.') > 40) or (v.IndexOf('.') < v.Length-50) then
    raise Exception.Create('Cannot normalise '+v)
  else
    result := StringPadRight(StringPadLeft('', '0', 40-v.IndexOf('.'))+v, '0', 91);
  if neg then
    result := '-' + result;
end;

{ TFhirIndexEntryList }

function TFhirIndexEntryList.add(key, parent : integer; index: TFhirIndex; ref: integer; value1, value2: String; target : Integer; ttype : TFHIRResourceType; type_ : TFhirSearchParamTypeEnum; flag : boolean = false; concept : integer = 0) : integer;
var
  entry : TFhirIndexEntry;
  dummy : string;
  I: Integer;
begin
  if (Index.Key = 0) then
    raise Exception.create('unknown index '+index.Name);

  case type_ of
    SearchParamTypeNumber, SearchParamTypeQuantity :
      begin
        value1 := normaliseDecimal(value1);
        value2 := normaliseDecimal(value2);
      end;
    SearchParamTypeString :
      begin
        value1 := removeCaseAndAccents(value1);
        value2 := removeCaseAndAccents(value2);
      end;
    SearchParamTypeDate : ; // nothing

    SearchParamTypeUri : ; // nothing

    SearchParamTypeToken :
      begin
      value2 := removeCaseAndAccents(value2);
      end;
    SearchParamTypeReference : ; // nothing
  else
    // null, Composite
    raise exception.create('Unhandled type generating index');
  end;

  for i := 0 to count - 1 do
  begin
    entry := Items[i];
    if (entry.Value1 = lowercase(value1)) and (entry.Value2 = lowercase(value2)) and (entry.FIndexKey = index.key) and (entry.target = target) and (entry.type_ = type_) and (entry.flag = flag) and (entry.RefType = ref) then
      exit;
  end;


  entry := TFhirIndexEntry.create;
  try
    entry.FName := index.Name;
    entry.EntryKey := KeyEvent(ktEntries, '', dummy);
    result := entry.EntryKey;
    entry.IndexKey := index.Key;
    entry.key := key;
    entry.parent := parent;
    entry.Value1 := lowercase(value1);
    entry.Value2 := lowercase(value2);
    entry.RefType := ref;
    entry.type_ := type_;
    entry.target := target;
    entry.concept := concept;
    entry.flag := flag;
    entry.targetType := ttype;
    Inherited Add(entry.Link);
  finally
    entry.free;
  end;
end;


function TFhirIndexEntryList.add(key, parent: integer; index: TFhirComposite): integer;
var
  entry : TFhirIndexEntry;
  dummy : string;
begin
  if (Index.Key = 0) then
    raise Exception.create('unknown index '+index.Name);

  entry := TFhirIndexEntry.create;
  try
    entry.EntryKey := KeyEvent(ktEntries, '', dummy);
    result := entry.EntryKey;
    entry.IndexKey := index.Key;
    entry.key := key;
    entry.parent := parent;
    Inherited Add(entry.Link);
  finally
    entry.free;
  end;
end;

procedure TFhirIndexEntryList.filter(indexes : TFhirIndexList;  name: String; list: TAdvList<TFhirIndexEntry>);
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].FName = name then
      list.Add(Items[i].Link as TFhirIndexEntry);
end;


{ TFhirIndexManager }

constructor TFhirIndexManager.Create(aSpaces : TFhirIndexSpaces; aInfo : TFHIRIndexInformation; ValidationInfo : TWorkerContext; ResConfig: TAdvMap<TFHIRResourceConfig>);
begin
  inherited Create;
  FCompartments := TFhirCompartmentEntryList.create;
  FValidationInfo := ValidationInfo;
  FSpaces := aSpaces;
  FInfo := aInfo;
  FEntries := TFhirIndexEntryList.Create;
  FResConfig := ResConfig;
end;

destructor TFhirIndexManager.Destroy;
begin
  FValidationInfo.Free;
  FTerminologyServer.free;
  FCompartments.Free;
  FSpaces.Free;
  FEntries.Free;
  FInfo.Free;
  FResConfig.Free;
  inherited;
end;



function TFhirIndexManager.EncodeXhtml(r: TFhirDomainResource): TBytes;
var
  x, body : TFhirXHtmlNode;
  xc : TAdvXmlBuilder;
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


procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirString; name: String);
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirUri; name: String);
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirCodeableConcept; name: String);
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

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value, name: String);
var
  ndx : TFhirIndex;
  types : TFhirSearchParamTypeEnumList;

begin
  if (value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+aType);

  if StringIsInteger32(value) then
    types := [SearchParamTypeString, SearchParamTypeToken, SearchParamTypeDate, SearchParamTypeReference, SearchParamTypeNumber, SearchParamTypeUri]
  else
    types := [SearchParamTypeString, SearchParamTypeToken, SearchParamTypeDate, SearchParamTypeReference, SearchParamTypeUri];
  if not (ndx.SearchType in types) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing string');
  if ndx.SearchType = SearchParamTypeString then
    value := lowercase(RemoveAccents(copy(value, 1, INDEX_ENTRY_LENGTH)))
  else if (length(value) > INDEX_ENTRY_LENGTH) then
     raise exception.create('string too long for indexing: '+value+ ' ('+inttostr(length(value))+' chars)');
  FEntries.add(key, parent, ndx, 0, value, '', 0, frtNull, ndx.SearchType);
end;

procedure TFhirIndexManager.index2(aType : String; key, parent : integer; value, name: String);
var
  ndx : TFhirIndex;
begin
  if (value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeReference]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing string');
  value := lowercase(RemoveAccents(copy(value, 1, INDEX_ENTRY_LENGTH)));
  FEntries.add(key, parent, ndx, 0, '', value, 0, frtNull, SearchParamTypeString);
end;

function TFhirIndexManager.Link: TFHIRIndexManager;
begin
  result := TFHIRIndexManager (inherited Link);
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value1, value2, name: String);
var
  ndx : TFhirIndex;
begin
  if (value1 = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [SearchParamTypeString, SearchParamTypeToken, SearchParamTypeDate]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing string');

  if ndx.SearchType = SearchParamTypeString then
    value1 := lowercase(RemoveAccents(copy(value1, 1, INDEX_ENTRY_LENGTH)))
  else  if (length(value1) > INDEX_ENTRY_LENGTH) then
    raise exception.create('string too long for indexing: '+value1+ ' ('+inttostr(length(value1))+' chars)');

  if ndx.SearchType = SearchParamTypeString then
    value2 := lowercase(RemoveAccents(copy(value2, 1, INDEX_ENTRY_LENGTH)))
  else if (length(value2) > INDEX_ENTRY_LENGTH) then
    raise exception.create('string too long for indexing: '+value2+ ' ('+inttostr(length(value2))+' chars)');

  FEntries.add(key, parent, ndx, 0, value1, value2, 0, frtNull, ndx.SearchType);
end;


procedure TFhirIndexManager.index(aType: String; key, parent: integer; value: Boolean; name: String);
var
  ndx : TFhirIndex;
  concept : integer;
begin
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [SearchParamTypeToken]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' of type '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing enumeration on '+aType);
  concept := TerminologyServer.enterIntoClosure(FSpaces.FDB, aType+'.'+name, 'http://hl7.org/fhir/special-values', BooleanToString(value));
  assert(concept <> 0);
  FEntries.add(key, parent, ndx, 0, BooleanToString(value), '', 0, frtNull, ndx.SearchType, false, concept);
end;


procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirEnum; name: String);
var
  ndx : TFhirIndex;
  concept : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;

  if (value.system = '') then
    exit;
//    raise Exception.Create('no system provided');

  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+aType);
  if not (ndx.SearchType in [SearchParamTypeToken]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' of type '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing enumeration on '+aType);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
     raise exception.create('string too long for indexing: '+value.value+ ' ('+inttostr(length(value.value))+' chars)');
  if value.system <> '' then
  begin
    concept := TerminologyServer.enterIntoClosure(FSpaces.FDB, aType+'.'+name, value.system, value.value);
    assert(concept <> 0);
  end
  else
    concept := 0;

  FEntries.add(key, parent, ndx, 0, value.value, '', 0, frtNull, ndx.SearchType, false, concept);
end;


procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirInstant; name: String);
begin
  if (value <> nil) and (value.value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

function TFhirIndexManager.transform(base: TFHIRBase; uri: String): TFHIRBase;
begin
  raise Exception.Create('not done yet');
end;

procedure TFhirIndexManager.evaluateByFHIRPath(key : integer; context, resource: TFhirResource);
var
  path : TFHIRExpressionEngine;
  i : integer;
  ndx : TFhirIndex;
  matches : TFHIRBaseList;
  match, work : TFHIRBase;
  a : TFhirResourceType;
  s : string;
  ie : TFhirIndexEntry;
begin
  path := TFHIRExpressionEngine.Create(FValidationInfo.link);
  try
    for i := 0 to FInfo.Indexes.Count - 1 do
    begin
      ndx := FInfo.Indexes[i];
      if (ndx.Path <> '') and (ndx.ResourceType = resource.fhirType) then
      begin
        matches := path.evaluate(nil, resource, ndx.Path);
        for match in matches do
        begin
          // custom resource support : do we need to do a transform?
          if ndx.mapping = '' then
            work := match.Link
          else
            work := transform(match, ndx.mapping);
          try
            case ndx.Usage of
              SearchXpathUsageNull: raise Exception.create('Path is not defined properly');
              SearchXpathUsageNormal:
                begin
                if match is TFhirString then
                  index(resource.fhirType, key, 0, TFhirString(match), ndx.Name)
                else if match is TFhirUri then
                  index(resource.fhirType, key, 0, TFhirUri(match), ndx.Name)
                else if match is TFhirEnum then
                  index(resource.fhirType, key, 0, TFhirEnum(match), ndx.Name)
                else if match is TFhirInteger  then
                  index(resource.fhirType, key, 0, TFhirInteger(match), ndx.Name)
                else if match is TFhirBoolean  then
                  index(resource.fhirType, key, 0, TFhirBoolean(match), ndx.Name)
                else if match is TFhirInstant  then
                  index(resource.fhirType, key, 0, TFhirInstant(match), ndx.Name)
                else if match is TFhirDateTime  then
                  index(resource.fhirType, key, 0, TFhirDateTime(match), ndx.Name)
                else if match is TFhirDate  then
                  index(resource.fhirType, key, 0, TFhirDate(match), ndx.Name)
                else if match is TFhirPeriod  then
                  index(resource.fhirType, key, 0, TFhirPeriod(match), ndx.Name)
                else if match is TFhirTiming  then
                  index(resource.fhirType, key, 0, TFhirTiming(match), ndx.Name)
                else if match is TFhirRatio  then
                  index(resource.fhirType, key, 0, TFhirRatio(match), ndx.Name)
                else if match is TFhirQuantity  then
                  index(resource.fhirType, key, 0, TFhirQuantity(match), ndx.Name)
                else if match is TFhirRange  then
                  index(resource.fhirType, key, 0, TFhirRange(match), ndx.Name)
                else if match is TFhirSampledData  then
                  index(resource.fhirType, key, 0, TFhirSampledData(match), ndx.Name)
                else if match is TFhirCoding  then
                  index(resource.fhirType, key, 0, TFhirCoding(match), ndx.Name)
                else if match is TFhirCodeableConcept  then
                  index(resource.fhirType, key, 0, TFhirCodeableConcept(match), ndx.Name)
                else if match is TFhirIdentifier  then
                  index(resource.fhirType, key, 0, TFhirIdentifier(match), ndx.Name)
                else if match is TFhirHumanName  then
                  index(resource.fhirType, key, 0, TFhirHumanName(match), ndx.Name, '')
                else if match is TFhirAddress  then
                  index(resource.fhirType, key, 0, TFhirAddress(match), ndx.Name)
                else if match is TFhirContactPoint  then
                  index(resource.fhirType, key, 0, TFhirContactPoint(match), ndx.Name)
                else if match is TFhirReference then
                    index(context, resource.fhirType, key, 0, TFhirReference(match), ndx.Name, ndx.specifiedTarget)
                else if match is TFhirReference then
                  index(context, resource.fhirType, key, 0, TFhirReference(match), ndx.Name, ndx.specifiedTarget)
                else
                  raise Exception.Create('The type '+match.FhirType+' is not supported in FIndexManager');
                end;
              SearchXpathUsagePhonetic:
                begin
                if match is TFhirString then
                  index(resource.fhirType, key, 0, EncodeNYSIIS(TFhirString(match).value), ndx.Name)
                else if match is TFhirHumanName then
                  index(resource.fhirType, key, 0, TFhirHumanName(match), '', ndx.Name)
                else
                  raise Exception.Create('The type '+match.FhirType+' is not supported in FIndexManager');
                end;
              SearchXpathUsageNearby:
                begin
                  // todo when a chance arises
                end;
              SearchXpathUsageDistance:
                begin
                  // todo when a chance arises
                end;
              SearchXpathUsageOther:
                begin
                  // todo when a chance arises
                end;
            end;
          finally
            work.Free;
          end;
        end;
      end;
    end;
  finally
    path.Free;
  end;

  // ok, now compartment information
  for a in [frtPatient, frtPractitioner, frtRelatedPerson, frtEncounter, frtDevice] do
  begin
    // a resource is automatically in it's own compartment
    if (a = resource.ResourceType) then
      FCompartments.add(key, FResConfig[CODES_TFHIRResourceType[a]].key, key, resource.id);
    if FInfo.Compartments.existsInCompartment(a, resource.fhirType) then
      for s in FInfo.Compartments.getIndexNames(a, resource.fhirType) do
      begin
        if (s <> '{def}') and not s.Contains('.') then // we already did this one above, so we just ignore this here and compartments with '.' in them are in error
        begin
          ndx := FInfo.FIndexes.getByName(resource.fhirType, s);
          if (ndx = nil) then
            raise Exception.Create('Unknown index '+s+' on '+CODES_TFhirResourceType[resource.ResourceType]);
          for ie in FEntries do
          begin
            if (ie.FKey = key) and (ie.IndexKey = ndx.Key) and (ie.TargetType = a) then
              FCompartments.add(key, FResConfig[CODES_TFHIRResourceType[a]].key, ie.FTarget, ie.Value1);
          end;
        end;
      end;
  end;
end;

function TFhirIndexManager.execute(key : integer; id : String; resource : TFhirResource; tags : TFHIRTagList) : String;
var
  i : integer;
  entry : TFhirIndexEntry;
  dummy : string;
begin
  checkTags(resource, tags);
  FEntries.clear;
  FEntries.FKeyEvent := FKeyEvent;

  FMasterKey := key;
  FSpaces.FDB.ExecSQL('delete from Compartments where ResourceKey in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('update IndexEntries set Flag = 2 where ResourceKey in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('update IndexEntries set Flag = 2 where Target in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('delete from SearchEntries where ResourceKey in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('update Ids set deleted = 1 where MasterResourceKey = '+inttostr(key));
  FCompartments.Clear;

  processCompartmentTags(key, id, tags);
  {$IFDEF FHIR3}
  evaluateByFHIRPath(key, resource, resource);
  {$ELSE}
  // base indexes
  index(resource.ResourceType, key, 0, id, '_id');
//  if (resource.languageElement <> nil) then
//    index(resource.ResourceType, key, 0, resource.language, '_language');

  index(resource.ResourceType, key, 0, resource.implicitRulesElement, '_rules');
  if resource.meta <> nil then
  begin
//    index(resource.ResourceType, key, 0, resource.meta.versionId, '_versionId');
    index(resource.ResourceType, key, 0, resource.meta.lastUpdatedElement, '_lastUpdated');
    for i := 0 to resource.meta.profileList.Count - 1 do
      index(resource.ResourceType, key, 0, resource.meta.profileList[i], '_profile');
    for i := 0 to resource.meta.tagList.Count - 1 do
      index(resource.ResourceType, key, 0, resource.meta.tagList[i], '_tag');
    for i := 0 to resource.meta.securityList.Count - 1 do
      index(resource.ResourceType, key, 0, resource.meta.securityList[i], '_security');
  end;

  buildIndexValues(key, id, resource, resource);
  {$ENDIF}
  processUnCompartmentTags(key, id, tags);

  if resource is TFhirDomainResource then
  begin
    FSpaces.FDB.SQL := 'insert into indexEntries (EntryKey, IndexKey, ResourceKey, Flag, Extension, Xhtml) values (:k, :i, :r, 1, ''html'', :xb)';
    FSpaces.FDB.prepare;
    FSpaces.FDB.BindInteger('k', FKeyEvent(ktEntries, '', dummy));
    FSpaces.FDB.BindInteger('i', FInfo.FNarrativeIndex);
    FSpaces.FDB.BindInteger('r', key);
    FSpaces.FDB.BindBlobFromBytes('xb', EncodeXhtml(TFhirDomainResource(resource)));
    FSpaces.FDB.execute;
    FSpaces.FDB.terminate;
  end;

  FSpaces.FDB.SQL := 'insert into indexEntries (EntryKey, IndexKey, ResourceKey, Parent, MasterResourceKey, SpaceKey, Value, Value2, Flag, target, concept) values (:k, :i, :r, :p, :m, :s, :v, :v2, :f, :t, :c)';
  FSpaces.FDB.prepare;
  for i := 0 to FEntries.Count - 1 Do
  begin
    entry := FEntries[i];
    FSpaces.FDB.BindInteger('k', FEntries[i].EntryKey);
    FSpaces.FDB.BindInteger('i', entry.IndexKey);
    FSpaces.FDB.BindInteger('r', entry.key);
    if entry.parent = 0 then
      FSpaces.FDB.BindNull('p')
    else
      FSpaces.FDB.BindInteger('p', entry.parent);
    if entry.key <> key then
      FSpaces.FDB.BindInteger('m', key)
    else
      FSpaces.FDB.BindNull('m');
    if entry.Flag then
      FSpaces.FDB.BindInteger('f', 1)
    else
      FSpaces.FDB.BindInteger('f', 0);
    if entry.concept = 0 then
      FSpaces.FDB.BindNull('c')
    else
      FSpaces.FDB.BindInteger('c', entry.concept);

    if entry.FRefType = 0 then
      FSpaces.FDB.BindNull('s')
    else
      FSpaces.FDB.BindInteger('s', entry.FRefType);
    FSpaces.FDB.BindString('v', entry.FValue1);
    FSpaces.FDB.BindString('v2', entry.FValue2);
    if (entry.Target = 0) or (entry.Target = FMasterKey) then
      FSpaces.FDB.BindNull('t')
    else
      FSpaces.FDB.BindInteger('t', entry.target);
    try
      FSpaces.FDB.execute;
    except
      on e:exception do
        raise Exception.Create('Exception storing values "'+entry.FValue1+'" and "'+entry.FValue2+'": '+e.message);

    end;
  end;
  FSpaces.FDB.terminate;

  result := '';
  if FCompartments.Count > 0 then
  begin
    FSpaces.FDB.SQL := 'insert into Compartments (ResourceCompartmentKey, ResourceKey, TypeKey, CompartmentKey, Id) values (:pk, :r, :ct, :ck, :id)';
    FSpaces.FDB.prepare;
    for i := 0 to FCompartments.Count - 1 Do
    begin
      if i > 0 then
        result := result + ', ';
      result := result + ''''+FCompartments[i].id+'''';

      FSpaces.FDB.BindInteger('pk', FKeyEvent(ktCompartment, '', dummy));
      FSpaces.FDB.BindInteger('r', FCompartments[i].key);
      FSpaces.FDB.BindInteger('ct', FCompartments[i].tkey);
      FSpaces.FDB.BindString('id', FCompartments[i].id);
      if FCompartments[i].ckey > 0 then
        FSpaces.FDB.BindInteger('ck', FCompartments[i].ckey)
      else
        FSpaces.FDB.BindNull('ck');
      FSpaces.FDB.execute;
    end;
    FSpaces.FDB.terminate;
  end;
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirCoding; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
  concept : integer;
begin
  if (value = nil) or (value.code = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if ndx.SearchType <> SearchParamTypeToken then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing Coding');
  if (value.system <> '') then
  begin
    ref := FSpaces.ResolveSpace(value.system);
    concept := TerminologyServer.enterIntoClosure(FSpaces.FDB, aType+'.'+name, value.system, value.code);
  end
  else
  begin
    ref := 0;
    concept := 0;
  end;

  if (length(value.code) > INDEX_ENTRY_LENGTH) then
    raise exception.create('code too long for indexing: '+value.code);
  if value.display <> '' then
    FEntries.add(key, parent, ndx, ref, value.code, lowercase(RemoveAccents(copy(value.display, 1, INDEX_ENTRY_LENGTH))), 0, frtNull, ndx.SearchType, false, concept)
  else
    FEntries.add(key, parent, ndx, ref, value.code, '', 0, frtNull, ndx.SearchType, false, concept);
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

procedure TFhirIndexManager.GetBoundaries(value : String; comparator: TFhirQuantityComparatorEnum; var low, high : String);
var
  dec : TSmartDecimal;
begin
  dec := TSmartDecimal.ValueOf(value);
  case comparator of
    QuantityComparatorNull :
      begin
      low := dec.lowerBound.AsDecimal;
      high := dec.upperBound.AsDecimal;
      end;
    QuantityComparatorLessThan :
      begin
      low := '-9999999999999999999999999999999999999999';
      high := dec.upperBound.AsDecimal;
      end;
    QuantityComparatorLessOrEquals :
      begin
      low := '-9999999999999999999999999999999999999999';
      high := dec.immediateLowerBound.AsDecimal;
      end;
    QuantityComparatorGreaterOrEquals :
      begin
      low := dec.lowerBound.AsDecimal;
      high := '9999999999999999999999999999999999999999';
      end;
    QuantityComparatorGreaterThan :
      begin
      low := dec.immediateUpperBound.AsDecimal;
      high := '9999999999999999999999999999999999999999';
      end;
  end;
end;


procedure TFhirIndexManager.index(aType : String; key, parent : integer; value : TFhirRange; name : String);
var
  ndx : TFhirIndex;
  v1, v2, crap : String;
  ref : integer;
  specified, canonical : TUcumPair;
begin
  if value = nil then
    exit;

  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join: "'+name+'"');
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeNumber, SearchParamTypeQuantity]) then
    raise Exception.create('Unsuitable index "'+name+'" '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing range');

  GetBoundaries(value.low.value, QuantityComparatorNull, v1, crap);
  GetBoundaries(value.high.value, QuantityComparatorNull, crap, v2);

  if (length(v1) > INDEX_ENTRY_LENGTH) then
      raise exception.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if (length(v2) > INDEX_ENTRY_LENGTH) then
      raise exception.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  ref := FSpaces.ResolveSpace(value.low.unit_);
  FEntries.add(key, parent, ndx, ref, v1, v2, 0, frtNull, ndx.SearchType);
  if value.low.system <> '' then
  begin
    ref := FSpaces.ResolveSpace(value.low.system+'#'+value.low.code);
    FEntries.add(key, parent, ndx, ref, v1, v2, 0, frtNull, ndx.SearchType);
  end;

  // ok, if there's a ucum code:
  if (value.low.code <> '') and (value.low.system = 'http://unitsofmeasure.org') then
  begin
    specified := TUcumPair.create;
    try
      specified.Value := TSmartDecimal.ValueOf(value.low.value);
      specified.UnitCode := value.low.code;
      canonical := FTerminologyServer.Ucum.getCanonicalForm(specified);
      try
        GetBoundaries(canonical.Value.AsString, QuantityComparatorNull, v1, v2);
        if (length(v1) > INDEX_ENTRY_LENGTH) then
          raise exception.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if (length(v2) > INDEX_ENTRY_LENGTH) then
          raise exception.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        ref := FSpaces.ResolveSpace('urn:ucum-canonical#'+canonical.UnitCode);
        FEntries.add(key, parent, ndx, ref, v1, v2, 0, frtNull, ndx.SearchType, true);
      finally
        canonical.free;
      end;
    finally
      specified.free;
    end;
  end;
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value : TFhirQuantity; name : String; units : string = '');
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

  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join: "'+name+'"');
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeNumber, SearchParamTypeQuantity]) then
    raise Exception.create('Unsuitable index "'+name+'" '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing quantity');

  GetBoundaries(value.value, value.comparator, v1, v2);

  if (length(v1) > INDEX_ENTRY_LENGTH) then
      raise exception.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if (length(v2) > INDEX_ENTRY_LENGTH) then
      raise exception.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  ref := FSpaces.ResolveSpace(value.unit_);
  FEntries.add(key, parent, ndx, ref, v1, v2, 0, frtNull, ndx.SearchType);
  if value.system <> '' then
  begin
    ref := FSpaces.ResolveSpace(value.system+'#'+value.code);
    FEntries.add(key, parent, ndx, ref, v1, v2, 0, frtNull, ndx.SearchType);
  end;

  // ok, if there's a ucum code:
  if (value.code <> '') and (value.system = 'http://unitsofmeasure.org') then
  begin
    specified := TUcumPair.create;
    try
      specified.Value := TSmartDecimal.ValueOf(value.value);
      specified.UnitCode := value.code;
      canonical := FTerminologyServer.Ucum.getCanonicalForm(specified);
      try
        GetBoundaries(canonical.Value.AsString, value.comparator, v1, v2);
        if (length(v1) > INDEX_ENTRY_LENGTH) then
          raise exception.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if (length(v2) > INDEX_ENTRY_LENGTH) then
          raise exception.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        ref := FSpaces.ResolveSpace('urn:ucum-canonical#'+canonical.UnitCode);
        FEntries.add(key, parent, ndx, ref, v1, v2, 0, frtNull, ndx.SearchType, true);
      finally
        canonical.free;
      end;
    finally
      specified.free;
    end;
  end;
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value : TFhirPeriod; name : String);
begin
  if (value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value : TFhirTiming; name : String);
begin
  if (value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirDateTime; name: String);
begin
  if (value <> nil) and (value.value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; min, max : TDateTime; name: String);
var
  ndx : TFhirIndex;
begin
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType = SearchParamTypeDate) then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing date');
  FEntries.add(key, parent, ndx, 0, HL7DateToString(min, 'yyyymmddhhnnss', false), HL7DateToString(max, 'yyyymmddhhnnss', false), 0, frtNull, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirIdentifier; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeToken]) then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing Identifier');
  ref := 0;
  if (value.system <> '') then
    ref := FSpaces.ResolveSpace(value.system);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
    raise exception.create('id too long for indexing: '+value.value);
  FEntries.add(key, parent, ndx, ref, value.value, '', 0, frtNull, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirAddress; name: String);
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

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirContactPoint; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeString]) then
    raise Exception.create('Unsuitable index '+name+':'+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing Contact on '+aType);
  ref := 0;
  if (value.systemElement <> nil) and (value.systemElement.value <> '') then
    ref := FSpaces.ResolveSpace(value.systemElement.value);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
    raise exception.create('contact value too long for indexing: '+value.value);
  FEntries.add(key, parent, ndx, ref, value.value, '', 0, frtNull, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType: String; key, parent: integer; value: TFhirIdentifierList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager.index(aType: String; key, parent: integer; value: TFhirCodingList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager.index(aType: String; key, parent: integer; value: TFhirCodeableConceptList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager.index(aType: String; key, parent: integer; value: TFhirSampledData; name: String);
begin
 // todo
end;

procedure TFhirIndexManager.index(aType: String; key, parent: integer; value: TFhirRatio; name: String);
begin
  // don't have a clue what to do here
end;

procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirHumanName; name, phoneticName: String);
var
  i : integer;
begin
  if (value = nil) then
    exit;
  if (name <> '') then
  begin
    index(aType, key, parent, value.text, name);
    for i := 0 to value.familyList.count - 1 do
      index(aType, key, parent, value.familyList[i], name);
    for i := 0 to value.givenList.count - 1 do
      index(aType, key, parent, value.givenList[i], name);
    for i := 0 to value.prefixList.count - 1 do
      index(aType, key, parent, value.prefixList[i], name);
    for i := 0 to value.suffixList.count - 1 do
      index(aType, key, parent, value.suffixList[i], name);
  end;

  if phoneticName <> '' then
  begin
    for i := 0 to value.familyList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.familyList[i].value), phoneticName);
    for i := 0 to value.givenList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.givenList[i].value), phoneticName);
    for i := 0 to value.prefixList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.prefixList[i].value), phoneticName);
    for i := 0 to value.suffixList.count - 1 do
      index(aType, key, parent, EncodeNYSIIS(value.suffixList[i].value), phoneticName);
  end;
end;

{
procedure TFhirIndexManager.index(aType : String; key, parent : integer; value: TFhirDecimal; name: String);
var
  ndx : TFhirIndex;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeString, SearchParamTypeToken]) then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing decimal');
  FEntries.add(key, ndx, 0, value.value, '', 0, ndx.SearchType);
end;
}

// todo: this doesn't yet handle version references
function isLocalTypeReference(url : String; var type_, id : String) : boolean;
var
  i : TFhirResourceType;
begin
  result := false;
  for i := Low(CODES_TFHIRResourceType) to High(CODES_TFHIRResourceType) do
    if url.StartsWith(CODES_TFHIRResourceType[i]+'/') and IsId(url.Substring(url.IndexOf('/')+1)) then
      result := true;
  if result then
    StringSplit(url, '/', type_, id);
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

procedure TFhirIndexManager.index(context : TFhirResource; aType : String; key, parent : integer; value: TFhirReference; name: String; specificType : String = '');
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

  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) and (name = 'patient') then
    ndx := FInfo.FIndexes.getByName(aType, 'subject');
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
// ggtodo - until target types are sorted out....
//  if (ndx.TargetTypes = []) then
//    raise Exception.create('Attempt to index a resource join in an index ('+aType+'/'+name+') that is a not a join (has no target types)');
  if ndx.SearchType <> SearchParamTypeReference then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing reference on a '+aType);

  if (length(value.reference) > INDEX_ENTRY_LENGTH) then
    raise exception.create('resource url too long for indexing: '+value.reference);

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
      raise exception.create('Reference to contained resource found in a resource that does not have contained resources"');
    if contained = nil then
      raise exception.create('No contained resource found in resource for "'+value.reference+'", list from '+CODES_TFHIRResourceType[context.ResourceType]+' = "'+sumContainedResources(TFhirDomainResource(context))+'"');
    if (specificType = '') or (contained.fhirType = specificType) then
    begin
      ttype := contained.ResourceType;
      ref := FSpaces.ResolveSpace(CODES_TFHIRResourceType[contained.ResourceType]);
      target := FKeyEvent(ktResource, contained.fhirType, id);
      FSpaces.FDB.execSql('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(ref)+' and LastId < '+id);
      FSpaces.FDB.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, MasterResourceKey) values (:k, :r, :i, null, '+inttostr(FMasterKey)+')';
      FSpaces.FDB.Prepare;
      FSpaces.FDB.BindInteger('k', target);
      FSpaces.FDB.BindInteger('r', ref);
      FSpaces.FDB.BindString('i', id);
      FSpaces.FDB.Execute;
      FSpaces.FDB.Terminate;
      {$IFDEF FHIR3}
      evaluateByFHIRPath(target, context, contained);
      {$ELSE}
      buildIndexValues(target, '', context, contained);
      {$ENDIF}
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
        ref := FSpaces.ResolveSpace(type_);
        FSpaces.FDB.sql := 'Select ResourceKey from Ids as i, Types as t where i.ResourceTypeKey = t.ResourceTypeKey and ResourceName = :t and Id = :id';
        FSpaces.FDB.Prepare;
        FSpaces.FDB.BindString('t', type_);
        FSpaces.FDB.BindString('id', id);
        FSpaces.FDB.Execute;
        if FSpaces.FDB.FetchNext then
          target := FSpaces.FDB.ColIntegerByName['ResourceKey']; // otherwise we try and link it up if we ever see the resource that this refers to
        FSpaces.FDB.Terminate;
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
    FEntries.add(key, parent, ndx, ref, id, '', target, ttype, ndx.SearchType);
end;


procedure TFhirIndexManager.index(aType: String; key, parent: integer; value: TFhirInteger; name: String);
var
  ndx : TFhirIndex;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (length(ndx.TargetTypes) > 0) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeString, SearchParamTypeNumber, SearchParamTypeToken]) then
    raise Exception.create('Unsuitable index '+name+' : '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing integer');
  FEntries.add(key, parent, ndx, 0, value.value, '', 0, frtNull, ndx.SearchType);
end;




procedure TFhirIndexManager.index(aType: String; key, parent: integer; value: TFhirDate; name: String);
begin
  if (value <> nil) and (value.value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType: String; key, parent: integer; value: TFhirBoolean; name: String);
var
  ndx : TFhirIndex;
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager.patientCompartment(key : integer; reference: TFhirReference);
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


procedure TFhirIndexManager.patientCompartment(key : integer; type_, id : String);
begin
  FSpaces.FDB.sql := 'Select i.ResourceTypeKey, ResourceKey from Ids as i, Types as t where i.ResourceTypeKey = t.ResourceTypeKey and ResourceName = :t and Id = :id';
  FSpaces.FDB.Prepare;
  FSpaces.FDB.BindString('t', type_);
  FSpaces.FDB.BindString('id', id);
  FSpaces.FDB.Execute;
  if FSpaces.FDB.FetchNext then
    FCompartments.add(key, FSpaces.FDB.ColIntegerByName['ResourceTypeKey'], FSpaces.FDB.ColIntegerByName['ResourceKey'], id)
  else
    FCompartments.add(key, 0, 0, id);
  FSpaces.FDB.Terminate;
end;

procedure TFhirIndexManager.patientCompartmentNot(key : integer; type_, id : String);
begin
  FCompartments.removeById(id);
end;

procedure TFhirIndexManager.processCompartmentTags(key: integer; id: String; tags: TFHIRTagList);
var
  i : integer;
begin
  for i := 0 to tags.Count - 1 do
    if (tags[i].system = TAG_FHIR_SYSTEM) and (tags[i].code = TAG_COMPARTMENT_IN) then
      patientCompartment(key, 'Patient', tags[i].display);

end;

procedure TFhirIndexManager.processUnCompartmentTags(key: integer; id: String; tags: TFHIRTagList);
var
  i : integer;
begin
  for i := 0 to tags.Count - 1 do
    if (tags[i].system = TAG_FHIR_SYSTEM) and (tags[i].code = TAG_COMPARTMENT_OUT) then
      patientCompartmentNot(key, 'Patient', tags[i].display);

end;

function TFhirIndexManager.index(aType: String; key, parent: integer; name: String): Integer;
var
  ndx : TFhirComposite;
begin
  ndx := FInfo.FComposites.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown composite index '+name+' on type '+aType);
  if (ndx.Key = 0) then
    raise Exception.create('unknown composite index '+ndx.Name);
  result := FEntries.add(key, parent, ndx);
end;

procedure TFhirIndexManager.index(context: TFhirResource; aType: String; key, parent: integer; value: TFhirReferenceList; name: String; specificType : String = '');
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(context, atype, key, parent, value[i], name, specificType);
end;

{ TFhirIndexSpaces }

constructor TFhirIndexSpaces.Create(db: TKDBConnection);
var
  i : integer;
begin
  inherited create;
  FSpaces := TStringList.Create;
  FSpaces.Sorted := true;

  FDB := db;
  FDB.SQL := 'select * from Spaces';
  FDb.prepare;
  FDb.execute;
  i := 0;
  while FDb.FetchNext do
  begin
    inc(i);
    try
      FSpaces.addObject(FDb.ColStringByName['Space'], TObject(FDb.ColIntegerByName['SpaceKey']));
    except
      raise Exception.Create('itereation '+inttostr(i));
    end;
  end;
  FDb.terminate;
end;


destructor TFhirIndexSpaces.destroy;
begin
  FSpaces.free;
  inherited;
end;

function TFhirIndexSpaces.ResolveSpace(space: String): integer;
var
  i : integer;
begin
  if space.trim <> space then
    raise Exception.Create('Illegal System Value "'+space+'" - cannot have leading or trailing whitespace');
  if FSpaces.Find(space, i) then
    result := integer(FSpaces.objects[i])
  else
  begin
    result := FDB.countSQL('select max(SpaceKey) from Spaces')+1;
    FDB.SQL := 'insert into Spaces (SpaceKey, Space) values ('+inttostr(result)+', :s)';
    FDb.prepare;
    FDB.BindString('s', space);
    FDb.execute;
    FDb.terminate;
    FSpaces.addObject(space, TObject(result));
  end;
end;




{ TFhirCompartmentEntryList }

procedure TFhirCompartmentEntryList.add(key, tkey, ckey: integer; id: string);
var
  item : TFhirCompartmentEntry;
begin
  item := TFhirCompartmentEntry.create;
  try
    item.key := key;
    item.tkey := tkey;
    item.ckey := ckey;
    item.id := id;
    inherited add(item.Link);
  finally
    item.free;
  end;
end;

procedure TFhirCompartmentEntryList.removeById(id: String);
var
  i : integer;
begin
  for i := count - 1 downto 0 do
    if items[i].Id = id then
      Delete(i);
end;

{ TFhirComposite }

procedure TFhirComposite.Assign(source: TAdvObject);
var
  s : String;
begin
  inherited;
  FResourceType := TFhirComposite(source).FResourceType;
  FKey := TFhirComposite(source).FKey;
  FName := TFhirComposite(source).FName;
  for s in TFhirComposite(source).FComponents.Keys do
    FComponents.Add(s, TFhirComposite(source).FComponents[s]);
end;

function TFhirComposite.Clone: TFhirComposite;
begin
  result := TFhirComposite(inherited Clone);
end;

constructor TFhirComposite.Create;
begin
  inherited;
  FComponents := TDictionary<String,String>.create;
end;

destructor TFhirComposite.Destroy;
begin
  FComponents.Free;
  inherited;
end;

function TFhirComposite.Link: TFhirComposite;
begin
  result := TFhirComposite(inherited Link);
end;

{ TFhirCompositeList }

procedure TFhirCompositeList.add(aResourceType: string; name: String; components: array of String);
var
  ndx : TFhirComposite;
  i : integer;
begin
  ndx := TFhirComposite.Create;
  try
    ndx.ResourceType := aResourceType;
    ndx.name := name;
    i := 0;
    while (i < length(components)) do
    begin
      ndx.Components.Add(components[i], components[i+1]);
      inc(i, 2);
    end;
    inherited add(ndx.Link);
  finally
    ndx.free;
  end;

end;

function TFhirCompositeList.getByName(aType: String; name: String): TFhirComposite;
var
  i : integer;
begin
  i := 0;
  result := nil;
  while (result = nil) and (i < Count) do
  begin
    if SameText(item[i].name, name) and (item[i].FResourceType = atype) then
      result := item[i];
    inc(i);
  end;
end;

function TFhirCompositeList.GetItemN(iIndex: integer): TFhirComposite;
begin
  result := TFhirComposite(ObjectByIndex[iIndex]
  );
end;

function TFhirCompositeList.ItemClass: TAdvObjectClass;
begin
  result := TFhirComposite;
end;

function TFhirCompositeList.Link: TFhirCompositeList;
begin
  result := TFhirCompositeList(inherited Link);
end;


{ TFHIRIndexInformation }

procedure TFHIRIndexInformation.buildIndexes;
var
  builder : TFHIRIndexBuilder;
begin
  builder := TFHIRIndexBuilder.create;
  try
     builder.registerIndexes(FIndexes, FCompartments);
  finally
    builder.Free;
  end;

  // manual additions:
  composites.add('DocumentReference', 'relatesTo', ['code', 'relation', 'target', 'relatesTo']);
  composites.add('Group', 'characteristic', ['value', 'value', 'code', 'characteristic']);
  composites.add('Observation', 'related', ['target', 'related-target', 'type', 'related-type']);
  composites.add('DiagnosticOrder', 'event', ['status', 'event-status', 'date', 'event-date']);
  composites.add('DiagnosticOrder', 'item', ['status', 'item-status', 'code', 'item-code', 'site', 'bodysite', 'event', 'item-event']);
  composites.add('DiagnosticOrder', 'item-event', ['status', 'item-past-status', 'date', 'item-date', 'actor', 'actor']);
  composites.add('Patient', 'name', ['given', 'given', 'family', 'family']);

  // DAF
  indexes.add('Condition', 'identifier', 'identifier', SearchParamTypeToken, [], '', SearchXpathUsageNull);
  indexes.add('Patient', 'mothersMaidenName', 'Search based on Patient mother''s Maiden Name', SearchParamTypeString, [], '', SearchXpathUsageNull, 'http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-mothersMaidenName');
  indexes.add('Patient', 'birthOrderBoolean', 'Search based on Patient''s birth order (boolean or integer)', SearchParamTypeString, [], '', SearchXpathUsageNull, 'http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-mothersMaidenName');
  indexes.add('Patient', 'age', 'Search based on Patient''s age', SearchParamTypeNumber, [], '', SearchXpathUsageNull, 'http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-age');
  indexes.add('FamilyMemberHistory', 'familymemberhistorycondition', 'Search for a history of a particular condition within a patient''s family', SearchParamTypeToken, [], '', SearchXpathUsageNull);
  {$IFDEF FHIR2}
  indexes.add(frtList, 'identifier', 'identifier', SearchParamTypeToken, [], '', SearchXpathUsageNull);

  // custom
  indexes.add(frtClaimResponse, 'request', 'request claim link', SearchParamTypeReference, [frtClaim], '', SearchXpathUsageNull);
  {$ENDIF}
end;

constructor TFHIRIndexInformation.Create;
begin
  inherited Create;
  FIndexes := TFhirIndexList.create;
  FComposites := TFhirCompositeList.create;
  FCompartments := TFHIRCompartmentList.Create;
  buildIndexes;
end;

destructor TFHIRIndexInformation.Destroy;
begin
  FIndexes.Free;
  FComposites.Free;
  FCompartments.Free;
  inherited;
end;

function TFHIRIndexInformation.Link: TFHIRIndexInformation;
begin
  result := TFHIRIndexInformation(inherited Link);
end;

procedure TFHIRIndexInformation.ReconcileIndexes(connection: TKDBConnection);
var
  i : integer;
begin
  connection.SQL := 'select * from Indexes';
  connection.prepare;
  connection.execute;
  while connection.FetchNext do
  begin
    for i := 0 to FIndexes.Count - 1 Do
      if SameText(FIndexes[i].Name, connection.ColStringByName['Name']) then
        FIndexes[i].key := connection.ColIntegerByName['IndexKey'];

    for i := 0 to FComposites.Count - 1 Do
      if SameText(FComposites[i].Name, connection.ColStringByName['Name']) then
        FComposites[i].key := connection.ColIntegerByName['IndexKey'];

    if connection.ColStringByName['Name'] = NARRATIVE_INDEX_NAME then
      FNarrativeIndex := connection.ColIntegerByName['IndexKey'];
  end;
  connection.terminate;
end;

function TFhirIndexInformation.GetTargetsByName(types: TArray<String>; name: String): TArray<String>;
var
  i : integer;
  s : String;
  ok : boolean;
begin
  result := [];
  for i := 0 to FIndexes.Count - 1 Do
    if SameText(FIndexes[i].Name, name) then
    begin
      ok := false;
      for s in types do
        ok := ok or (s = FIndexes[i].ResourceType);
      if ok then
        result := result + FIndexes[i].TargetTypes;
    end;
end;

function TFhirIndexInformation.GetKeyByName(name: String): integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to FIndexes.Count - 1 Do
    if FIndexes[i].Name = name then
    begin
      result := FIndexes[i].Key;
      exit;
    end;
end;

function TFhirIndexInformation.GetTypeByName(types: TArray<String>; name: String): TFhirSearchParamTypeEnum;
var
  i : integer;
  s : String;
  ok : boolean;
begin
  result := SearchParamTypeNull;
  for i := 0 to FIndexes.Count - 1 Do
    if SameText(FIndexes[i].Name, name) then
    begin
      ok := length(types) = 0;
      for s in types do
        ok := ok or (s = FIndexes[i].ResourceType);
      if ok then
        if (result <> SearchParamTypeNull) and (result <> FIndexes[i].FSearchType) And ((FIndexes[i].FSearchType in [SearchParamTypeDate, SearchParamTypeToken]) or (result in [SearchParamTypeDate, SearchParamTypeToken])) then
          raise Exception.create('Chained Parameters cross resource joins that create disparate index handling requirements')
        else
          result := FIndexes[i].FSearchType;
    end;
end;

function TFhirIndexInformation.GetComposite(types: TArray<String>; name: String; var otypes: TArray<String>): TFhirComposite;
var
  i : integer;
  s : String;
  ok : boolean;
begin
  oTypes := types;

  i := 0;
  result := nil;
  while (i < FComposites.Count) do
  begin
    if SameText(FComposites.item[i].name, name) then
    begin
      ok := length(types) = 0;
      for s in types do
        ok := ok or (s = FComposites.item[i].FResourceType);
      if ok then
        if result = nil then
        begin
          result := FComposites.item[i];
          oTypes := [FComposites.item[i].FResourceType];
        end
        else
          raise Exception.Create('Ambiguous composite reference "'+name+'"');
    end;
    inc(i);
  end;
end;

procedure TFhirIndexManager.checkTags(resource: TFhirResource; tags: TFHIRTagList);
var
  c : integer;
begin
  c := 0;
  if (resource.meta <> nil) then
    c := resource.meta.tagList.Count + resource.meta.securityList.Count + resource.meta.profileList.Count;
  if c <> tags.Count then
    raise Exception.Create('Tags out of sync');
end;


{$IFDEF FHIR2}
procedure TFhirIndexManager.buildIndexValues(key : integer; id : string; context, resource: TFhirResource);
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
    frtDeviceUseRequest : buildIndexValuesDeviceUseRequest(key, id, context, TFhirDeviceUseRequest(resource));
    frtDeviceUseStatement : buildIndexValuesDeviceUseStatement(key, id, context, TFhirDeviceUseStatement(resource));
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
    raise Exception.create('resource type indexing not implemented yet for '+CODES_TFHIRResourceType[resource.ResourceType]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesClaim(key: integer; id : String; context : TFhirResource; resource: TFhirClaim);
begin
  index(frtClaim, key, 0, resource.identifierList, CODES_TSearchParamsClaim[spClaim_identifier]);
  index(frtClaim, key, 0, resource.priorityElement, CODES_TSearchParamsClaim[spClaim_priority]);
  index(frtClaim, key, 0, resource.useElement, CODES_TSearchParamsClaim[spClaim_use]);
  index(context, frtClaim, key, 0, resource.patientElement, CODES_TSearchParamsClaim[spClaim_patient]);
  index(context, frtClaim, key, 0, resource.provider, CODES_TSearchParamsClaim[spClaim_provider]);
  patientCompartment(key, resource.patient);
end;


procedure TFhirIndexManager.buildIndexValuesCoverage(key: integer; id : String; context : TFhirResource; resource: TFhirCoverage);
begin
  index(frtCoverage, key, 0, resource.dependentElement, CODES_TSearchParamsCoverage[spCoverage_dependent]);
  index(frtCoverage, key, 0, resource.groupElement, CODES_TSearchParamsCoverage[spCoverage_group]);
  index(frtCoverage, key, 0, resource.identifierList, CODES_TSearchParamsCoverage[spCoverage_identifier]);
  index(frtCoverage, key, 0, resource.planElement, CODES_TSearchParamsCoverage[spCoverage_plan]);
  index(frtCoverage, key, 0, resource.sequenceElement, CODES_TSearchParamsCoverage[spCoverage_sequence]);
  index(frtCoverage, key, 0, resource.subplanElement, CODES_TSearchParamsCoverage[spCoverage_subplan]);
  index(frtCoverage, key, 0, resource.type_Element, CODES_TSearchParamsCoverage[spCoverage_type]);
//  index(context, frtCoverage, key, 0, resource.subjectList, CODES_TSearchParamsCoverage[spCoverage_subject]);
  index(context, frtCoverage, key, 0, resource.issuerElement, CODES_TSearchParamsCoverage[spCoverage_issuer]);
end;

procedure TFhirIndexManager.buildIndexValuesClaimResponse(key: integer; id : String; context : TFhirResource; resource: TFhirClaimResponse);
begin
  index(frtClaimResponse, key, 0, resource.identifierList, CODES_TSearchParamsClaimResponse[spClaimResponse_identifier]);
//  index(context, frtClaimResponse, key, 0, resource.request, CODES_TSearchParamsClaimResponse[spClaimResponse_request]);
end;

procedure TFhirIndexManager.buildIndexValuesEligibilityRequest(key: integer; id : String; context : TFhirResource; resource: TFhirEligibilityRequest);
begin
  index(frtEligibilityRequest, key, 0, resource.identifierList, CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_identifier]);
end;


procedure TFhirIndexManager.buildIndexValuesEligibilityResponse(key: integer; id : String; context : TFhirResource; resource: TFhirEligibilityResponse);
begin
  index(frtEligibilityResponse, key, 0, resource.identifierList, CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_identifier]);
end;

procedure TFhirIndexManager.buildIndexValuesEnrollmentRequest(key: integer; id : String; context : TFhirResource; resource: TFhirEnrollmentRequest);
begin
  index(frtEnrollmentRequest, key, 0, resource.identifierList, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_identifier]);
  index(context, frtEnrollmentRequest, key, 0, resource.subject, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_subject]);
  index(context, frtEnrollmentRequest, key, 0, resource.subject, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_patient]);
  patientCompartment(key, resource.subject);
end;


procedure TFhirIndexManager.buildIndexValuesEnrollmentResponse(key: integer; id : String; context : TFhirResource; resource: TFhirEnrollmentResponse);
begin
  index(frtEnrollmentResponse, key, 0, resource.identifierList, CODES_TSearchParamsEnrollmentResponse[spEnrollmentResponse_identifier]);
end;


procedure TFhirIndexManager.buildIndexValuesExplanationOfBenefit(key: integer; id : String; context : TFhirResource; resource: TFhirExplanationOfBenefit);
var
  i : integer;
begin
  index(frtExplanationOfBenefit, key, 0, resource.identifierList, CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_identifier]);
end;


procedure TFhirIndexManager.buildIndexValuesPaymentNotice(key: integer; id : String; context : TFhirResource; resource: TFhirPaymentNotice);
var
  i : integer;
begin
  index(frtPaymentNotice, key, 0, resource.identifierList, CODES_TSearchParamsPaymentNotice[spPaymentNotice_identifier]);
end;



procedure TFhirIndexManager.buildIndexValuesPaymentReconciliation(key: integer; id : String; context : TFhirResource; resource: TFhirPaymentReconciliation);
var
  i : integer;
begin
  index(frtPaymentReconciliation, key, 0, resource.identifierList, CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_identifier]);
end;


procedure TFhirIndexManager.buildIndexValuesBasic(key: integer; id : String; context : TFhirResource; resource: TFhirBasic);
begin
  index(frtBasic, key, 0, resource.createdElement, CODES_TSearchParamsBasic[spBasic_created]);
  index(frtBasic, key, 0, resource.code, CODES_TSearchParamsBasic[spBasic_code]);
  index(context, frtBasic, key, 0, resource.subject, CODES_TSearchParamsBasic[spBasic_subject]);
  index(context, frtBasic, key, 0, resource.subject, CODES_TSearchParamsBasic[spBasic_patient]);
  index(context, frtBasic, key, 0, resource.author, CODES_TSearchParamsBasic[spBasic_author]);
  index(frtBasic, key, 0, resource.identifierList, CODES_TSearchParamsBasic[spBasic_identifier]);
  patientCompartment(key, resource.subject);
  relatedPersonCompartment(key, resource.author);
  practitionerCompartment(key, resource.author);
end;

procedure TFhirIndexManager.buildIndexValuesContract(key: integer; id : String; context : TFhirResource; resource: TFhirContract);
var
  i  : integer;
begin
  for i := 0 to resource.subjectList.Count - 1 do
  begin
    index(context, frtContract, key, 0, resource.subjectList[i], CODES_TSearchParamsContract[spContract_subject]);
    patientCompartment(key, resource.subjectList[i]);
    index(context, frtContract, key, 0, resource.subjectList[i], CODES_TSearchParamsContract[spContract_patient]);
  end;
  for i := 0 to resource.actorList.Count - 1 do
  begin
    index(context, frtContract, key, 0, resource.actorList[i].entity, CODES_TSearchParamsContract[spContract_actor]);
    practitionerCompartment(key, resource.actorList[i].entity);
    relatedPersonCompartment(key, resource.actorList[i].entity);
    deviceCompartment(key, resource.actorList[i].entity);
  end;
  for i := 0 to resource.signerList.Count - 1 do
  begin
    index(context, frtContract, key, 0, resource.signerList[i].party, CODES_TSearchParamsContract[spContract_signer]);
    practitionerCompartment(key, resource.signerList[i].party);
    relatedPersonCompartment(key, resource.signerList[i].party);
    patientCompartment(key, resource.signerList[i].party);
  end;
  index(frtContract, key, 0, resource.identifier, CODES_TSearchParamsContract[spContract_identifier]);
end;


procedure TFhirIndexManager.buildIndexValuesSupplyDelivery(key: integer; id : String; context : TFhirResource; resource: TFhirSupplyDelivery);
var
  i : integer;
begin
  index(frtSupplyDelivery, key, 0, resource.identifier, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_identifier]);
  index(context, frtSupplyDelivery, key, 0, resource.patient, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_patient]);
  patientCompartment(key, resource.patient);
  index(context, frtSupplyDelivery, key, 0, resource.receiverList, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_receiver]);
  index(frtSupplyDelivery, key, 0, resource.statusElement, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_status]);
  index(context, frtSupplyDelivery, key, 0, resource.supplier, CODES_TSearchParamsSupplyDelivery[spSupplyDelivery_supplier]);
end;

procedure TFhirIndexManager.buildIndexValuesSupplyRequest(key: integer; id : String; context : TFhirResource; resource: TFhirSupplyRequest);
var
  i : integer;
begin
  index(frtSupplyRequest, key, 0, resource.identifier, CODES_TSearchParamsSupplyRequest[spSupplyRequest_identifier]);
  index(frtSupplyRequest, key, 0, resource.kind, CODES_TSearchParamsSupplyRequest[spSupplyRequest_kind]);
  index(frtSupplyRequest, key, 0, resource.dateElement, CODES_TSearchParamsSupplyRequest[spSupplyRequest_date]);
  index(context, frtSupplyRequest, key, 0, resource.source, CODES_TSearchParamsSupplyRequest[spSupplyRequest_source]);
  index(context, frtSupplyRequest, key, 0, resource.supplierList, CODES_TSearchParamsSupplyRequest[spSupplyRequest_supplier]);
  index(frtSupplyRequest, key, 0, resource.statusElement, CODES_TSearchParamsSupplyRequest[spSupplyRequest_status]);
  index(context, frtSupplyRequest, key, 0, resource.patient, CODES_TSearchParamsSupplyRequest[spSupplyRequest_patient]);
  patientCompartment(key, resource.patient);
end;


procedure TFhirIndexManager.buildIndexValuesRelatedPerson(key: integer; id : String; context : TFhirResource; resource: TFhirRelatedPerson);
var
  i : integer;
begin
  for i := 0 to resource.addressList.count - 1 do
    index(frtRelatedPerson, key, 0, resource.addressList[i], CODES_TSearchParamsRelatedPerson[spRelatedPerson_address]);
  index(frtRelatedPerson, key, 0, resource.genderElement, CODES_TSearchParamsRelatedPerson[spRelatedPerson_gender]);

  for i := 0 to resource.identifierList.count - 1 do
    index(frtRelatedPerson, key, 0, resource.identifierList[i], CODES_TSearchParamsRelatedPerson[spRelatedPerson_identifier]);
  index(frtRelatedPerson, key, 0, resource.name, 'name', CODES_TSearchParamsRelatedPerson[spRelatedPerson_phonetic]);
  index(frtRelatedPerson, key, 0, resource.birthDateElement, CODES_TSearchParamsRelatedPerson[spRelatedPerson_birthdate]);
  index(context, frtRelatedPerson, key, 0, resource.patient, CODES_TSearchParamsRelatedPerson[spRelatedPerson_patient]);
  patientCompartment(key, resource.patient);
  for i := 0 to resource.telecomList.count - 1 do
  begin
    index(frtRelatedPerson, key, 0, resource.telecomList[i], CODES_TSearchParamsRelatedPerson[spRelatedPerson_telecom]);
    if (resource.telecomList[i].system = ContactPointSystemEmail) then
      index(frtRelatedPerson, key, 0, resource.telecomList[i].value, CODES_TSearchParamsRelatedPerson[spRelatedPerson_email]);
    if (resource.telecomList[i].system = ContactPointSystemPhone) then
      index(frtRelatedPerson, key, 0, resource.telecomList[i].value, CODES_TSearchParamsRelatedPerson[spRelatedPerson_phone]);

  end;
end;

// --------- actual indexes -----------------------------------------------------------------------------------------------

procedure TFhirIndexManager.buildIndexValuesConformance(key : integer; id : String; context : TFhirResource; resource: TFhirConformance);
var
  i : integer;
  j : integer;
begin
  index(frtConformance, key, 0, resource.dateElement, CODES_TSearchParamsConformance[spConformance_date]);
  index(frtConformance, key, 0, resource.nameElement, CODES_TSearchParamsConformance[spConformance_name]);
  index(frtConformance, key, 0, resource.statusElement, CODES_TSearchParamsConformance[spConformance_status]);
  index(frtConformance, key, 0, resource.descriptionElement, CODES_TSearchParamsConformance[spConformance_description]);
  index(frtConformance, key, 0, resource.publisherElement, CODES_TSearchParamsConformance[spConformance_publisher]);
  if resource.software <> nil then
    index(frtConformance, key, 0, resource.software.nameElement, CODES_TSearchParamsConformance[spConformance_software]);
  index(frtConformance, key, 0, resource.versionElement, CODES_TSearchParamsConformance[spConformance_version]);
  index(frtConformance, key, 0, resource.fhirversionElement, CODES_TSearchParamsConformance[spConformance_fhirversion]);
  index(frtConformance, key, 0, resource.urlElement, CODES_TSearchParamsConformance[spConformance_url]);


  for j := 0 to resource.formatList.Count - 1 do
    index(frtConformance, key, 0, resource.formatList[j], CODES_TSearchParamsConformance[spConformance_format]);

  for j := 0 to resource.restList.Count - 1 do
  begin
    if resource.restList[j].security <> nil then
    begin
      for i := 0 to resource.restList[j].security.serviceList.count - 1 do
        index(frtConformance, key, 0, resource.restList[j].security.serviceList[i], CODES_TSearchParamsConformance[{$IFDEF FHIR3}spConformance_securityservice{$ELSE}spConformance_security{$ENDIF}]);
    end;
  end;


  for j := 0 to resource.restList.Count - 1 do
  begin
    for i := 0 to resource.restList[j].resourceList.count - 1 do
    begin
      index(context, frtConformance, key, 0, resource.restList[j].resourceList[i].profile, CODES_TSearchParamsConformance[{$IFDEF FHIR3} spConformance_resourceprofile{$ELSE} spConformance_profile {$ENDIF}]);
      index(frtConformance, key, 0, resource.restList[j].resourceList[i].type_Element, CODES_TSearchParamsConformance[spConformance_resource]);
    end;
    index(frtConformance, key, 0, resource.restList[j].modeElement, CODES_TSearchParamsConformance[spConformance_mode]);
  end;

  for j := 0 to resource.messagingList.Count - 1 Do
  begin
    for i := 0 to resource.messagingList[j].EventList.count - 1 do
    begin
      index(frtConformance, key, 0, resource.messagingList[j].EventList[i].focusElement, CODES_TSearchParamsConformance[spConformance_resource]);
      {$IFDEF FHIR2}
      index(context, frtConformance, key, 0, resource.messagingList[j].EventList[i].request, CODES_TSearchParamsConformance[spConformance_profile]);
      index(context, frtConformance, key, 0, resource.messagingList[j].EventList[i].response, CODES_TSearchParamsConformance[spConformance_profile]);
      {$ENDIF}
      index(frtConformance, key, 0, resource.messagingList[j].EventList[i].modeElement, CODES_TSearchParamsConformance[spConformance_mode]);
      index(frtConformance, key, 0, resource.messagingList[j].EventList[i].code, CODES_TSearchParamsConformance[spConformance_event]);
    end;
  end;

  for i := 0 to resource.DocumentList.count - 1 do
    index(context, frtConformance, key, 0, resource.DocumentList[i].profile, CODES_TSearchParamsConformance[{$IFDEF FHIR3} spConformance_resourceprofile {$ELSE} spConformance_profile {$ENDIF}]);
  for i := 0 to resource.profileList.count - 1 do
    index(context, frtConformance, key, 0, resource.ProfileList[i], CODES_TSearchParamsConformance[{$IFDEF FHIR3} spConformance_supportedprofile {$ELSE} spConformance_profile {$ENDIF}]);

end;

procedure TFhirIndexManager.buildIndexValuesEncounter(key: integer; id : String; context : TFhirResource; resource: TFhirEncounter);
var
  i : integer;
begin
  index(context, frtEncounter, key, 0, resource.patient, CODES_TSearchParamsEncounter[spEncounter_patient]);
  index(frtEncounter, key, 0, resource.type_List, CODES_TSearchParamsEncounter[spEncounter_type]);
  for i := 0 to resource.participantList.count - 1 do
  begin
    index(context, frtEncounter, key, 0, resource.participantList[i].individual, CODES_TSearchParamsEncounter[spEncounter_participant]);
    index(context, frtEncounter, key, 0, resource.participantList[i].individual, CODES_TSearchParamsEncounter[spEncounter_practitioner], frtPractitioner);
    index(frtEncounter, key, 0, resource.participantList[i].type_List, CODES_TSearchParamsEncounter[spEncounter_participanttype]);
  end;
  index(context, frtEncounter, key, 0, resource.partOf, CODES_TSearchParamsEncounter[spEncounter_partof]);
  index(frtEncounter, key, 0, resource.reasonList, CODES_TSearchParamsEncounter[spEncounter_reason]);
  index(context, frtEncounter, key, 0, resource.appointment, CODES_TSearchParamsEncounter[spEncounter_appointment]);
  index(context, frtEncounter, key, 0, resource.incomingReferralList, CODES_TSearchParamsEncounter[spEncounter_incomingreferral]);
  index(context, frtEncounter, key, 0, resource.episodeOfCareList, CODES_TSearchParamsEncounter[spEncounter_episodeofcare]);
  index(context, frtEncounter, key, 0, resource.indicationList, CODES_TSearchParamsEncounter[spEncounter_indication]);
  index(context, frtEncounter, key, 0, resource.indicationList, CODES_TSearchParamsEncounter[spEncounter_condition], frtCondition);
  index(context, frtEncounter, key, 0, resource.indicationList, CODES_TSearchParamsEncounter[spEncounter_procedure], frtProcedure);

  if resource.hospitalization <> nil then
    index(frtEncounter, key, 0, resource.hospitalization.specialArrangementList, CODES_TSearchParamsEncounter[spEncounter_specialarrangement]);
  patientCompartment(key, resource.patient);
  for i := 0 to resource.indicationList.count - 1 do
    index(context, frtEncounter, key, 0, resource.indicationList[i], CODES_TSearchParamsEncounter[spEncounter_indication]);
  index(frtEncounter, key, 0, resource.statusElement, CODES_TSearchParamsEncounter[spEncounter_status]);
  index(frtEncounter, key, 0, resource.periodElement, CODES_TSearchParamsEncounter[spEncounter_date]);
  index(frtEncounter, key, 0, resource.lengthElement, CODES_TSearchParamsEncounter[spEncounter_length]);
  for i := 0 to resource.identifierList.count - 1 do
    index(frtEncounter, key, 0, resource.identifierList[i], CODES_TSearchParamsEncounter[spEncounter_identifier]);
  for i := 0 to resource.locationList.count - 1 do
  begin
    index(context, frtEncounter, key, 0, resource.locationList[i].locationElement, CODES_TSearchParamsEncounter[spEncounter_location]);
    index(frtEncounter, key, 0, resource.locationList[i].periodElement, CODES_TSearchParamsEncounter[spEncounter_locationperiod]);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesLocation(key: integer; id : String; context : TFhirResource; resource: TFhirLocation);

var
  i : integer;

begin
  index(frtLocation, key, 0, resource.addressElement, CODES_TSearchParamsLocation[spLocation_address]);
  index(frtLocation, key, 0, resource.NameElement, CODES_TSearchParamsLocation[spLocation_name]);
  index(frtLocation, key, 0, resource.statusElement, CODES_TSearchParamsLocation[spLocation_status]);
  index(frtLocation, key, 0, resource.type_Element, CODES_TSearchParamsLocation[spLocation_type]);
  for i := 0 to resource.identifierList.Count - 1 do
    index(frtLocation, key, 0, resource.identifierList, CODES_TSearchParamsLocation[spLocation_identifier]);
  index(context, frtLocation, key, 0, resource.managingOrganizationElement, CODES_TSearchParamsLocation[spLocation_organization]);
  index(context, frtLocation, key, 0, resource.partOf, CODES_TSearchParamsLocation[spLocation_partof]);
  if resource.position <> nil then
  begin
    if (resource.position.longitude <> '') and (resource.position.latitude <> '') then
      index(frtLocation, key, 0, resource.position.longitude, resource.position.latitude, CODES_TSearchParamsLocation[spLocation_near]);
  end
//    spLocation_Near_distance, {@enum.value spLocation_Near_distance A distance quantity to limit the near search to locations within a specific distance }
end;

procedure TFhirIndexManager.BuildIndexValuesDocumentReference(key: integer;id : String; context : TFhirResource; resource: TFhirDocumentReference);
var
  i, p : integer;
begin
  index(context, frtDocumentReference, key, 0, resource.authenticator, CODES_TSearchParamsDocumentReference[spDocumentReference_authenticator]);
  for i := 0 to resource.authorList.count - 1 do
    index(context, frtDocumentReference, key, 0, resource.authorList[i], CODES_TSearchParamsDocumentReference[spDocumentReference_author]);
  index(frtDocumentReference, key, 0, resource.securityLabelList, CODES_TSearchParamsDocumentReference[spDocumentReference_securitylabel]);
  index(frtDocumentReference, key, 0, resource.createdElement, CODES_TSearchParamsDocumentReference[spDocumentReference_created]);
  index(context, frtDocumentReference, key, 0, resource.custodian, CODES_TSearchParamsDocumentReference[spDocumentReference_custodian]);
  index(frtDocumentReference, key, 0, resource.descriptionElement, CODES_TSearchParamsDocumentReference[spDocumentReference_description]);
  if resource.context <> nil then
  begin
    for i := 0 to resource.context.eventList.count - 1 do
      index(frtDocumentReference, key, 0, resource.context.eventList[i], CODES_TSearchParamsDocumentReference[spDocumentReference_event]);
    index(frtDocumentReference, key, 0, resource.context.facilityType, CODES_TSearchParamsDocumentReference[spDocumentReference_facility]);
    index(frtDocumentReference, key, 0, resource.context.period, CODES_TSearchParamsDocumentReference[spDocumentReference_period]);

    index(frtDocumentReference, key, 0, resource.context.practiceSetting, CODES_TSearchParamsDocumentReference[spDocumentReference_setting]);
    for i := 0 to resource.context.relatedList.Count - 1 do
    begin
      index(frtDocumentReference, key, 0, resource.context.relatedList[i].identifier, CODES_TSearchParamsDocumentReference[spDocumentReference_relatedid]);
      index(context, frtDocumentReference, key, 0, resource.context.relatedList[i].ref, CODES_TSearchParamsDocumentReference[spDocumentReference_relatedref]);
    end;
  index(context, frtDocumentReference, key, 0, resource.context.encounter, CODES_TSearchParamsDocumentReference[spDocumentReference_encounter]);
  end;
  for i := 0 to resource.contentList.count - 1 do
    for p := 0 to resource.contentList[i].formatList.count - 1 do
      index(frtDocumentReference, key, 0, resource.contentList[i].formatList[p], CODES_TSearchParamsDocumentReference[spDocumentReference_format]);
  index(frtDocumentReference, key, 0, resource.masterIdentifier, CODES_TSearchParamsDocumentReference[spDocumentReference_identifier]);
  for i := 0 to resource.identifierList.count - 1 do
    index(frtDocumentReference, key, 0, resource.identifierList[i], CODES_TSearchParamsDocumentReference[spDocumentReference_identifier]);
  index(frtDocumentReference, key, 0, resource.indexedElement, CODES_TSearchParamsDocumentReference[spDocumentReference_indexed]);
  index(frtDocumentReference, key, 0, resource.statusElement, CODES_TSearchParamsDocumentReference[spDocumentReference_status]);
  index(context, frtDocumentReference, key, 0, resource.subject, CODES_TSearchParamsDocumentReference[spDocumentReference_subject]);
  index(context, frtDocumentReference, key, 0, resource.subject, CODES_TSearchParamsDocumentReference[spDocumentReference_patient]);
  patientCompartment(key, resource.subject);
  for i := 0 to resource.relatesToList.Count - 1 do
  begin
    p := index(frtDocumentReference, key, 0, CODES_TSearchParamsDocumentReference[spDocumentReference_relatesTo]);
    index(context, frtDocumentReference, key, p, resource.relatesToList[i].target, CODES_TSearchParamsDocumentReference[spDocumentReference_relatesTo]);
    index(frtDocumentReference, key, p, resource.relatesToList[i].codeElement, CODES_TSearchParamsDocumentReference[spDocumentReference_relation]);
  end;
  index(frtDocumentReference, key, 0, resource.type_, CODES_TSearchParamsDocumentReference[spDocumentReference_type]);
  index(frtDocumentReference, key, 0, resource.class_, CODES_TSearchParamsDocumentReference[spDocumentReference_class]);
  for i := 0 to resource.contentList.Count - 1 do
  begin
    if (resource.contentList[i].attachment <> nil) then
    begin
      index(frtDocumentReference, key, 0, resource.contentList[i].attachment.languageElement, CODES_TSearchParamsDocumentReference[spDocumentReference_language]);
      index(frtDocumentReference, key, 0, resource.contentList[i].attachment.urlElement, CODES_TSearchParamsDocumentReference[spDocumentReference_location]);
    end;
  end;
end;


procedure TFhirIndexManager.BuildIndexValuesDocumentManifest(key: integer;id : String; context : TFhirResource; resource: TFhirDocumentManifest);
var
  i : integer;
begin
  for i := 0 to resource.authorList.count - 1 do
    index(context, frtDocumentManifest, key, 0, resource.authorList[i], CODES_TSearchParamsDocumentManifest[spDocumentManifest_author]);

  index(frtDocumentManifest, key, 0, resource.createdElement, CODES_TSearchParamsDocumentManifest[spDocumentManifest_created]);
  index(frtDocumentManifest, key, 0, resource.descriptionElement, CODES_TSearchParamsDocumentManifest[spDocumentManifest_description]);
  index(frtDocumentManifest, key, 0, resource.masterIdentifier, CODES_TSearchParamsDocumentManifest[spDocumentManifest_identifier]);
  for i := 0 to resource.identifierList.count - 1 do
    index(frtDocumentManifest, key, 0, resource.identifierList[i], CODES_TSearchParamsDocumentManifest[spDocumentManifest_identifier]);
  index(frtDocumentManifest, key, 0, resource.statusElement, CODES_TSearchParamsDocumentManifest[spDocumentManifest_status]);
  index(context, frtDocumentManifest, key, 0, resource.subject, CODES_TSearchParamsDocumentManifest[spDocumentManifest_subject]);
  index(context, frtDocumentManifest, key, 0, resource.subject, CODES_TSearchParamsDocumentManifest[spDocumentManifest_patient]);
  index(frtDocumentManifest, key, 0, resource.sourceElement, CODES_TSearchParamsDocumentManifest[spDocumentManifest_source]);
  for i := 0 to resource.contentList.count - 1 do
    if resource.contentList[i].p is TFhirReference then
      index(context, frtDocumentManifest, key, 0, resource.contentList[i].p as TFhirReference, CODES_TSearchParamsDocumentManifest[spDocumentManifest_contentref]);

  index(frtDocumentManifest, key, 0, resource.type_, CODES_TSearchParamsDocumentManifest[spDocumentManifest_type]);
  for i := 0 to resource.recipientList.count - 1 do
    index(context, frtDocumentManifest, key, 0, resource.recipientList[i], CODES_TSearchParamsDocumentManifest[spDocumentManifest_recipient]);


  for i := 0 to resource.relatedList.Count - 1 do
  begin
    index(frtDocumentManifest, key, 0, resource.relatedList[i].identifier, CODES_TSearchParamsDocumentManifest[spDocumentManifest_relatedid]);
    index(context, frtDocumentManifest, key, 0, resource.relatedList[i].ref, CODES_TSearchParamsDocumentManifest[spDocumentManifest_relatedref]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesBundle(key: integer; id : String; context : TFhirResource; resource: TFhirBundle);
var
  inner : TFhirResource;
  ref, target : integer;
  name : String;
  ndx : TFhirIndex;
begin
  index(frtBundle, key, 0, resource.type_Element, CODES_TSearchParamsBundle[spBundle_type]);
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
    ndx := FInfo.FIndexes.getByName(frtBundle, name);
    if (ndx = nil) then
      raise Exception.create('Unknown index Bundle.'+name);
    if (ndx.TargetTypes = []) then
      raise Exception.create('Attempt to index a resource join in an index (Bundle.'+name+') that is a not a join (has no target types)');
    if ndx.SearchType <> SearchParamTypeReference then
      raise Exception.create('Unsuitable index Bundle.'+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing inner');

    ref := FSpaces.ResolveSpace(CODES_TFHIRResourceType[inner.ResourceType]);
    // ignore the existing id because this is a virtual entry; we don't want the real id to appear twice if the resource also really exists
    target := FKeyEvent(ktResource, inner.ResourceType, id); //FSpaces.FDB.CountSQL('select Max(ResourceKey) from Ids') + 1;
    FSpaces.FDB.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, MasterResourceKey) values (:k, :r, :i, null, '+inttostr(FMasterKey)+')';
    FSpaces.FDB.Prepare;
    FSpaces.FDB.BindInteger('k', target);
    FSpaces.FDB.BindInteger('r', ref);
    FSpaces.FDB.BindString('i', id);
    FSpaces.FDB.Execute;
    FSpaces.FDB.Terminate;
    buildIndexValues(target, '', context, inner);
    FEntries.add(key, 0, ndx, ref, id, '', target, ndx.SearchType);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesFlag(key: integer; id : String; context : TFhirResource; resource: TFhirFlag);
begin
  index(context, frtFlag, key, 0, resource.subject, CODES_TSearchParamsFlag[spFlag_subject]);
  index(context, frtFlag, key, 0, resource.subject, CODES_TSearchParamsFlag[spFlag_patient], frtPatient);
  index(context, frtFlag, key, 0, resource.author, CODES_TSearchParamsFlag[spFlag_author]);
  index(frtFlag, key, 0, resource.period, CODES_TSearchParamsFlag[spFlag_date]);
  patientCompartment(key, resource.subject);
  practitionerCompartment(key, resource.author);
  deviceCompartment(key, resource.subject);
end;



procedure TFhirIndexManager.buildIndexValuesAllergyIntolerance(key: integer; id : String; context : TFhirResource; resource: TFhirAllergyIntolerance);
var
  i : integer;
begin
  index(frtAllergyIntolerance, key, 0, resource.categoryElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_category]);
  index(frtAllergyIntolerance, key, 0, resource.criticalityElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_criticality]);
  index(frtAllergyIntolerance, key, 0, resource.recordedDateElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_date]);
  index(frtAllergyIntolerance, key, 0, resource.identifierList, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_identifier]);
  index(context, frtAllergyIntolerance, key, 0, resource.recorderElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_recorder]);
  index(context, frtAllergyIntolerance, key, 0, resource.reporterElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_reporter]);
  index(frtAllergyIntolerance, key, 0, resource.lastOccurenceElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_lastdate]);
  index(frtAllergyIntolerance, key, 0, resource.statusElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_status]);
  index(frtAllergyIntolerance, key, 0, resource.type_Element, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_type]);
  index(context, frtAllergyIntolerance, key, 0, resource.patient, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_patient]);
  index(frtAllergyIntolerance, key, 0, resource.substanceElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_substance]);

  for i := 0 to resource.reactionList.Count - 1 do
  begin
    index(frtAllergyIntolerance, key, 0, resource.reactionList[i].substanceElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_substance]);
    index(frtAllergyIntolerance, key, 0, resource.reactionList[i].onsetElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_onset]);
    index(frtAllergyIntolerance, key, 0, resource.reactionList[i].exposureRouteElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_route]);
    index(frtAllergyIntolerance, key, 0, resource.reactionList[i].manifestationList, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_manifestation]);
    index(frtAllergyIntolerance, key, 0, resource.reactionList[i].severityElement, CODES_TSearchParamsAllergyIntolerance[spAllergyIntolerance_severity]);
  end;
  patientCompartment(key, resource.patient);
  practitionerCompartment(key, resource.recorder);
end;


procedure TFhirIndexManager.buildIndexValuesSubstance(key: integer; id : String; context : TFhirResource; resource: TFhirSubstance);
var
  i : integer;
begin
  index(frtSubstance, key, 0, resource.categoryList, CODES_TSearchParamsSubstance[spSubstance_category]);
  index(frtSubstance, key, 0, resource.code, CODES_TSearchParamsSubstance[spSubstance_code]);
  index(frtSubstance, key, 0, resource.identifierList, CODES_TSearchParamsSubstance[spSubstance_identifier]);
  for i := 0 to resource.instanceList.Count - 1 do
    index(frtSubstance, key, 0, resource.instanceList[i].expiryElement, CODES_TSearchParamsSubstance[spSubstance_expiry]);
  for i := 0 to resource.ingredientList.count - 1 do
  begin
    index(frtSubstance, key, 0, resource.ingredientList[i].quantity, CODES_TSearchParamsSubstance[spSubstance_quantity]);
    index(context, frtSubstance, key, 0, resource.ingredientList[i].substance, CODES_TSearchParamsSubstance[spSubstance_substance]);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesComposition(key : integer; id : String; context : TFhirResource; resource: TFhirComposition);
  procedure indexSection(section : TFhirCompositionSection);
  var
    i : integer;
  begin

    index(frtComposition, key, 0, section.code, CODES_TSearchParamsComposition[spComposition_section]);
    index(context, frtComposition, key, 0, section.entryList, CODES_TSearchParamsComposition[spComposition_entry]);
    for i := 0 to section.SectionList.count - 1 do
      indexSection(section.SectionList[i]);
  end;
var
  i, j : integer;
begin
  index(frtComposition, key, 0, resource.dateElement, CODES_TSearchParamsComposition[spComposition_date]);
  index(frtComposition, key, 0, resource.identifier, CODES_TSearchParamsComposition[spComposition_identifier]);
  index(context, frtComposition, key, 0, resource.subject, CODES_TSearchParamsComposition[spComposition_subject]);
  index(context, frtComposition, key, 0, resource.subject, CODES_TSearchParamsComposition[spComposition_patient], frtPatient);
  index(context, frtComposition, key, 0, resource.encounter, CODES_TSearchParamsComposition[spComposition_encounter]);
  patientCompartment(key, resource.subject);
  practitionerCompartment(key, resource.subject);
  deviceCompartment(key, resource.subject);
  index(frtComposition, key, 0, resource.titleElement, CODES_TSearchParamsComposition[spComposition_title]);
  index(frtComposition, key, 0, resource.type_Element, CODES_TSearchParamsComposition[spComposition_type]);
  index(frtComposition, key, 0, resource.class_Element, CODES_TSearchParamsComposition[spComposition_class]);
  index(frtComposition, key, 0, resource.confidentialityElement, CODES_TSearchParamsComposition[spComposition_confidentiality]);
  index(frtComposition, key, 0, resource.statusElement, CODES_TSearchParamsComposition[spComposition_status]);
  for j := 0 to resource.eventList.Count - 1 do
    for i := 0 to resource.eventList[j].codeList.Count - 1 do
    begin
      index(frtComposition, key, 0, resource.eventList[j].period, CODES_TSearchParamsComposition[spComposition_period]);
      index(frtComposition, key, 0, resource.eventList[j].codeList[i], CODES_TSearchParamsComposition[spComposition_context]);
    end;

  for i := 0 to resource.authorList.count - 1 do
  begin
    index(context, frtComposition, key, 0, resource.authorList[i], CODES_TSearchParamsComposition[spComposition_author]);
    relatedPersonCompartment(key, resource.authorList[i]);
    practitionerCompartment(key, resource.authorList[i]);
    deviceCompartment(key, resource.authorList[i]);
    patientCompartment(key, resource.authorList[i]);
  end;
  for i := 0 to resource.attesterList.count - 1 do
    index(context, frtComposition, key, 0, resource.attesterList[i].party, CODES_TSearchParamsComposition[spComposition_attester]);
  for i := 0 to resource.SectionList.count - 1 do
    indexSection(resource.SectionList[i]);
end;


procedure TFhirIndexManager.buildIndexValuesMessageHeader(key : integer; id : String; context : TFhirResource; resource: TFhirMessageHeader);
var
  i : integer;
begin

  if (resource.response <> nil) then
  begin
    index(frtMessageHeader, key, 0, resource.response.codeElement, CODES_TSearchParamsMessageHeader[spMessageHeader_code]);
    index(frtMessageHeader, key, 0, resource.response.id, CODES_TSearchParamsMessageHeader[spMessageHeader_responseid]);
  end;
  for i := 0 to resource.dataList.Count - 1 do
    index(context, frtMessageHeader, key, 0, resource.dataList[i], CODES_TSearchParamsMessageHeader[spMessageHeader_data]);
  index(context, frtMessageHeader, key, 0, resource.receiver, CODES_TSearchParamsMessageHeader[spMessageHeader_receiver]);
  index(context, frtMessageHeader, key, 0, resource.author, CODES_TSearchParamsMessageHeader[spMessageHeader_author]);
  index(context, frtMessageHeader, key, 0, resource.enterer, CODES_TSearchParamsMessageHeader[spMessageHeader_enterer]);
  index(context, frtMessageHeader, key, 0, resource.responsible, CODES_TSearchParamsMessageHeader[spMessageHeader_responsible]);
  index(frtMessageHeader, key, 0, resource.timestampElement, CODES_TSearchParamsMessageHeader[spMessageHeader_timestamp]);
  for i := 0 to resource.destinationList.Count - 1 do
  begin
    index(frtMessageHeader, key, 0, resource.destinationList[i].nameElement, CODES_TSearchParamsMessageHeader[spMessageHeader_destination]);
    index(frtMessageHeader, key, 0, resource.destinationList[i].endpointElement, CODES_TSearchParamsMessageHeader[spMessageHeader_destinationuri]);
    index(context, frtMessageHeader, key, 0, resource.destinationList[i].target, CODES_TSearchParamsMessageHeader[spMessageHeader_target]);
  end;
  if resource.source <> nil then
  begin
    index(frtMessageHeader, key, 0, resource.source.nameElement, CODES_TSearchParamsMessageHeader[spMessageHeader_source]);
    index(frtMessageHeader, key, 0, resource.source.endpointElement, CODES_TSearchParamsMessageHeader[spMessageHeader_sourceuri]);
  end;
  index(frtMessageHeader, key, 0, resource.event, CODES_TSearchParamsMessageHeader[spMessageHeader_event]);
end;

procedure TFhirIndexManager.buildIndexValuesPractitioner(key : integer; id : String; context : TFhirResource; resource: TFhirPractitioner);
var
  i, j : integer;
begin
  for i := 0 to resource.identifierList.count - 1 do
    index(frtPractitioner, key, 0, resource.identifierList[i], CODES_TSearchParamsPractitioner[spPractitioner_identifier]);
  if (resource.name <> nil) then
  begin
    index(frtPractitioner, key, 0, resource.name, 'name', CODES_TSearchParamsPractitioner[spPractitioner_phonetic]);
    for j := 0 to resource.name.givenList.count - 1 do
      index(frtPractitioner, key, 0, resource.name.givenList[j], CODES_TSearchParamsPractitioner[spPractitioner_given]);
    for j := 0 to resource.name.familyList.count - 1 do
      index(frtPractitioner, key, 0, resource.name.familyList[j], CODES_TSearchParamsPractitioner[spPractitioner_family]);
  end;
  for i := 0 to resource.telecomList.count - 1 do
  begin
    index(frtPractitioner, key, 0, resource.telecomList[i], CODES_TSearchParamsPractitioner[spPractitioner_telecom]);
    if (resource.telecomList[i].system = ContactPointSystemPhone) then
      index(frtPractitioner, key, 0, resource.telecomList[i].value, CODES_TSearchParamsPractitioner[spPractitioner_phone]);
    if (resource.telecomList[i].system = ContactPointSystemEmail) then
      index(frtPractitioner, key, 0, resource.telecomList[i].value, CODES_TSearchParamsPractitioner[spPractitioner_email]);

  end;
  index(frtPractitioner, key, 0, resource.genderElement, CODES_TSearchParamsPractitioner[spPractitioner_gender]);
  for i := 0 to resource.addressList.Count - 1 do
    index(frtPractitioner, key, 0, resource.addressList[i], CODES_TSearchParamsPractitioner[spPractitioner_address]);
  index(frtPractitioner, key, 0, resource.communicationList, CODES_TSearchParamsPractitioner[spPractitioner_communication]);
  for j := 0 to resource.practitionerRoleList.Count -1 do
  begin
    for i := 0 to resource.practitionerRoleList[j].locationList.count - 1 do
      index(context, frtPractitioner, key, 0, resource.practitionerRoleList[j].locationList[i], CODES_TSearchParamsPractitioner[spPractitioner_location]);
    index(context, frtPractitioner, key, 0, resource.practitionerRoleList[j].{$IFDEF FHIR2}managingOrganization{$ELSE}Organization{$ENDIF}, CODES_TSearchParamsPractitioner[spPractitioner_organization]);
    index(frtPractitioner, key, 0, resource.practitionerRoleList[j].roleElement, CODES_TSearchParamsPractitioner[spPractitioner_role]);
    for i := 0 to resource.practitionerRoleList[j].specialtyList.count - 1 do
      index(frtPractitioner, key, 0, resource.practitionerRoleList[j].specialtyList[i], CODES_TSearchParamsPractitioner[spPractitioner_specialty]);
  end;
end;



procedure TFhirIndexManager.buildIndexValuesOrganization(key : integer;  id : String; context : TFhirResource; resource: TFhirOrganization);
var
  i : integer;
begin
  index(frtOrganization, key, 0, resource.active, CODES_TSearchParamsOrganization[spOrganization_active]);
  index(frtOrganization, key, 0, resource.NameElement, CODES_TSearchParamsOrganization[spOrganization_name]);
  index(frtOrganization, key, 0, EncodeNYSIISValue(resource.nameElement), CODES_TSearchParamsOrganization[spOrganization_phonetic]);
  index(frtOrganization, key, 0, resource.type_, CODES_TSearchParamsOrganization[spOrganization_type]);
  for i := 0 to resource.addressList.Count - 1 Do
    index(frtOrganization, key, 0, resource.addressList[i], CODES_TSearchParamsOrganization[spOrganization_address]);
  for i := 0 to resource.IdentifierList.Count - 1 Do
    if resource.IdentifierList[i] <> nil then
      index(frtOrganization, key, 0, resource.IdentifierList[i], CODES_TSearchParamsOrganization[spOrganization_identifier]);
  index(context, frtOrganization, key, 0, resource.partOf, CODES_TSearchParamsOrganization[spOrganization_partOf]);

end;

procedure TFhirIndexManager.buildIndexValuesGroup(key : integer;  id : String; context : TFhirResource; resource: TFhirGroup);
var
  i, p : integer;
begin
  index(frtGroup, key, 0, resource.actual, CODES_TSearchParamsGroup[spGroup_actual]);
  index(frtGroup, key, 0, resource.code, CODES_TSearchParamsGroup[spGroup_code]);
  index(frtGroup, key, 0, resource.type_Element, CODES_TSearchParamsGroup[spGroup_type]);
  index(frtGroup, key, 0, resource.identifierList, CODES_TSearchParamsGroup[spGroup_identifier]);

  for i := 0 to resource.memberList.Count - 1 Do
    index(context, frtGroup, key, 0, resource.memberList[i].entity, CODES_TSearchParamsGroup[spGroup_member]);

  for i := 0 to resource.characteristicList.Count - 1 Do
  begin
    p := index(frtGroup, key, 0, CODES_TSearchParamsGroup[spGroup_characteristic]);
    index(frtGroup, key, p, resource.characteristicList[i].code, CODES_TSearchParamsGroup[spGroup_characteristic]);
    index(frtGroup, key, 0, resource.characteristicList[i].exclude, CODES_TSearchParamsGroup[spGroup_exclude]);
    if resource.characteristicList[i].value is TFhirBoolean then
      index(frtGroup, key, p, TFhirBoolean(resource.characteristicList[i].value).value, CODES_TSearchParamsGroup[spGroup_value])
    else if resource.characteristicList[i].value is TFhirString then
      index(frtGroup, key, p, TFhirString(resource.characteristicList[i].value), CODES_TSearchParamsGroup[spGroup_value])
    else if resource.characteristicList[i].value is TFhirCodeableConcept then
      index(frtGroup, key, p, TFhirCodeableConcept(resource.characteristicList[i].value), CODES_TSearchParamsGroup[spGroup_value])
  end;
end;


procedure TFhirIndexManager.buildIndexValuesObservation(key : integer;  id : String; context : TFhirResource; resource: TFhirObservation);
var
  i, p : integer;
  procedure indexValue(value : TFhirType; prefix : string);
  begin
    if value is TFhirQuantity then
      index(frtObservation, key, 0, TFhirQuantity(value), prefix+CODES_TSearchParamsObservation[spObservation_Valuequantity])
    else if value is TFhirSampledData then
      index(frtObservation, key, 0, TFhirSampledData(value), prefix+CODES_TSearchParamsObservation[spObservation_valuequantity])
    else if value is TFhirRatio then
      index(frtObservation, key, 0, TFhirRatio(value), prefix+CODES_TSearchParamsObservation[spObservation_valuequantity])
    else if value is TFhirCodeableConcept then
      index(frtObservation, key, 0, TFhirCodeableConcept(value), prefix+CODES_TSearchParamsObservation[spObservation_valueconcept])
    else if value is TFhirPeriod then
      index(frtObservation, key, 0, TFhirPeriod(value), prefix+CODES_TSearchParamsObservation[spObservation_valuedate])
    else if value is TFhirString then
      index(frtObservation, key, 0, TFhirString(value), prefix+CODES_TSearchParamsObservation[spObservation_valuestring]);
  end;
begin
  index(frtObservation, key, 0, resource.code, CODES_TSearchParamsObservation[spObservation_code]);
  index(frtObservation, key, 0, resource.category, CODES_TSearchParamsObservation[spObservation_category]);


  index(context, frtObservation, key, 0, resource.subject, CODES_TSearchParamsObservation[spObservation_subject]);
  index(context, frtObservation, key, 0, resource.subject, CODES_TSearchParamsObservation[spObservation_patient], frtPatient);
  patientCompartment(key, resource.subject);
  if resource.effective is TFhirDateTime then
    index(frtObservation, key, 0, TFhirDateTime(resource.effective), CODES_TSearchParamsObservation[spObservation_date])
  else if resource.effective is TFhirPeriod then
    index(frtObservation, key, 0, TFhirPeriod(resource.effective), CODES_TSearchParamsObservation[spObservation_date]);
  index(frtObservation, key, 0, resource.statusElement, CODES_TSearchParamsObservation[spObservation_status]);
  for i := 0 to resource.performerList.Count - 1 Do
    index(context, frtObservation, key, 0, resource.performerList[i], CODES_TSearchParamsObservation[spObservation_performer]);
  index(context, frtObservation, key, 0, resource.specimen, CODES_TSearchParamsObservation[spObservation_specimen]);

  indexValue(resource.value, '');
  index(context, frtObservation, key, 0, resource.encounter, CODES_TSearchParamsObservation[spObservation_encounter]);
  index(frtObservation, key, 0, resource.identifierList, CODES_TSearchParamsObservation[spObservation_identifier]);
  index(frtObservation, key, 0, resource.dataAbsentReasonElement, CODES_TSearchParamsObservation[spObservation_dataabsentreason]);
  index(context, frtObservation, key, 0, resource.deviceElement, CODES_TSearchParamsObservation[spObservation_device]);


  for i := 0 to resource.relatedList.Count - 1 Do
  begin
    p := index(frtObservation, key, 0, CODES_TSearchParamsObservation[spObservation_related]);
    index(frtObservation, key, p, resource.relatedList[i].type_Element, CODES_TSearchParamsObservation[spObservation_relatedtype]);
    index(context, frtObservation, key, p, resource.relatedList[i].target, CODES_TSearchParamsObservation[spObservation_relatedtarget]);
  end;
  for i := 0 to resource.componentList.Count - 1 Do
  begin
    p := 0; //todo index(frtObservation, key, 0, CODES_TSearchParamsObservation[spObservation_component]);
    index(frtObservation, key, p, resource.componentList[i].code, CODES_TSearchParamsObservation[spObservation_code]);
    index(frtObservation, key, 0, resource.componentList[i].dataAbsentReasonElement, CODES_TSearchParamsObservation[spObservation_componentdataabsentreason]);
    indexValue(resource.componentList[i].value, 'component-');
  end;

//  TODO:    spObservation_Code_value_x, {@enum.value "code-value-[x]" spObservation_Code_value_x Both code and one of the value parameters }
//  TODO:    spObservation_CMPOnnet_Code_value_x, {@enum.value "code-value-[x]" spObservation_Code_value_x Both code and one of the value parameters }

end;


procedure TFhirIndexManager.buildIndexValuesStructureDefinition(key : integer; id : String; context : TFhirResource; resource: TFHirStructureDefinition);
var
  i, j : integer;
  procedure indexElement(element : TFhirElementDefinition);
  begin
    index(frtStructureDefinition, key, 0, element.path, CODES_TSearchParamsStructureDefinition[spStructureDefinition_path]);
    if (element.base <> nil) then
      index(frtStructureDefinition, key, 0, element.base.path, CODES_TSearchParamsStructureDefinition[spStructureDefinition_basepath]);
    if (element.binding <> nil) then
      if element.binding.valueSet is TFhirUri then
        index(frtStructureDefinition, key, 0, TFhirUri(element.binding.valueset), CODES_TSearchParamsStructureDefinition[spStructureDefinition_Valueset])
      else
        index(context, frtStructureDefinition, key, 0, TFhirReference(element.binding.valueset), CODES_TSearchParamsStructureDefinition[spStructureDefinition_valueset]);
  end;
begin
  index(frtStructureDefinition, key, 0, resource.identifierList, CODES_TSearchParamsStructureDefinition[spStructureDefinition_identifier]);
  index(frtStructureDefinition, key, 0, resource.urlElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_url]);
  index(frtStructureDefinition, key, 0, resource.baseElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_base]);
  index(frtStructureDefinition, key, 0, resource.constrainedTypeElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_type]);
  index(frtStructureDefinition, key, 0, resource.nameElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_name]);
  index(frtStructureDefinition, key, 0, resource.useContextList, CODES_TSearchParamsStructureDefinition[spStructureDefinition_context]);
  index(frtStructureDefinition, key, 0, resource.contextTypeElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_contexttype]);
  for i := 0 to resource.contextList.Count - 1 do
    index(frtStructureDefinition, key, 0, resource.contextList[i], CODES_TSearchParamsStructureDefinition[spStructureDefinition_extcontext]);

  index(frtStructureDefinition, key, 0, resource.dateElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_date]);
  index(frtStructureDefinition, key, 0, resource.abstractElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_abstract]);
  index(frtStructureDefinition, key, 0, resource.descriptionElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_description]);
  index(frtStructureDefinition, key, 0, resource.experimentalElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_experimental]);
  index(frtStructureDefinition, key, 0, resource.displayElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_display]);
  index(frtStructureDefinition, key, 0, resource.statusElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_status]);
  index(frtStructureDefinition, key, 0, resource.versionElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_version]);
  index(frtStructureDefinition, key, 0, resource.publisherElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_publisher]);
  for i := 0 to resource.CodeList.count - 1 Do
    index(frtStructureDefinition, key, 0, resource.CodeList[i], CODES_TSearchParamsStructureDefinition[spStructureDefinition_code]);
  index(frtStructureDefinition, key, 0, resource.kindElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_kind]);
  if resource.snapshot <> nil then
    for j := 0 to resource.snapshot.elementList.Count - 1 do
      indexElement(resource.snapshot.elementList[j]);
  if resource.differential <> nil then
    for j := 0 to resource.differential.elementList.Count - 1 do
      indexElement(resource.differential.elementList[j]);
end;


procedure TFhirIndexManager.buildIndexValuesPatient(key : integer; id : String; context : TFhirResource; resource: TFhirPatient);
var
  i, j : integer;
  ex : TFhirExtension;
begin
  for i := 0 to resource.IdentifierList.Count - 1 Do
    if resource.IdentifierList[i] <> nil then
      index(frtPatient, key, 0, resource.IdentifierList[i], CODES_TSearchParamsPatient[spPatient_identifier]);
  for i := 0 to resource.nameList.count - 1 do
  begin
    index(frtPatient, key, 0, resource.nameList[i], 'name', CODES_TSearchParamsPatient[spPatient_phonetic]);
    for j := 0 to resource.nameList[i].givenList.count - 1 do
      index(frtPatient, key, 0, resource.nameList[i].givenList[j], CODES_TSearchParamsPatient[spPatient_given]);
    for j := 0 to resource.nameList[i].familyList.count - 1 do
      index(frtPatient, key, 0, resource.nameList[i].familyList[j], CODES_TSearchParamsPatient[spPatient_family]);
  end;

  for i := 0 to resource.telecomList.Count - 1 do
  begin
    index(frtPatient, key, 0, resource.telecomList[i], CODES_TSearchParamsPatient[spPatient_telecom]);
    if resource.telecomList[i].system = ContactPointSystemPhone then
      index(frtPatient, key, 0, resource.telecomList[i].valueElement, CODES_TSearchParamsPatient[spPatient_phone]);
    if resource.telecomList[i].system = ContactPointSystemEmail then
      index(frtPatient, key, 0, resource.telecomList[i].valueElement, CODES_TSearchParamsPatient[spPatient_email]);

  end;
  for i := 0 to resource.AddressList.Count - 1 Do
    index(frtPatient, key, 0, resource.AddressList[i], CODES_TSearchParamsPatient[spPatient_address]);
  index(frtPatient, key, 0, resource.genderElement, CODES_TSearchParamsPatient[spPatient_gender]);
  if (resource.deceased is TFhirBoolean) then
    index(frtPatient, key, 0, resource.deceased as TFhirBoolean, CODES_TSearchParamsPatient[spPatient_deceased])
  else if (resource.deceased is TFhirDateTime) then
    index(frtPatient, key, 0, resource.deceased as TFhirDateTime, CODES_TSearchParamsPatient[spPatient_deathdate]);
  for i := 0 to resource.communicationList.Count - 1 Do
    index(frtPatient, key, 0, resource.communicationList[i].language, CODES_TSearchParamsPatient[spPatient_language]);

  index(frtPatient, key, 0, resource.birthDateElement, CODES_TSearchParamsPatient[spPatient_birthdate]);

  index(context, frtPatient, key, 0, resource.managingOrganization, CODES_TSearchParamsPatient[spPatient_organization]);
  for i := 0 to resource.careProviderList.Count - 1 Do
    index(context, frtPatient, key, 0, resource.careProviderList[i], CODES_TSearchParamsPatient[spPatient_careprovider]);


  for i := 0 to resource.link_List.count - 1 do
    index(context, frtPatient, key, 0, resource.link_List[i].other, CODES_TSearchParamsPatient[spPatient_link]);

  index(frtPatient, key, 0, resource.activeElement, CODES_TSearchParamsPatient[spPatient_active]);

  if (resource.animal <> nil) then
  begin
    index(frtPatient, key, 0, resource.animal.species, CODES_TSearchParamsPatient[spPatient_animalspecies]);
    index(frtPatient, key, 0, resource.animal.breed, CODES_TSearchParamsPatient[spPatient_animalbreed]);
  end;
  patientCompartment(key, 'patient', id);

  // DAF / HL7 extensions:
  if resource.multipleBirth is TFhirBoolean then
    index(frtPatient, key, 0, BoolToStr((resource.multipleBirth as TFhirBoolean).value, true).ToLower, 'birthOrderBoolean')
  else if resource.multipleBirth is TFhirInteger then
    index(frtPatient, key, 0, (resource.multipleBirth as TFhirInteger).value, 'birthOrderBoolean');
  for ex in resource.extensionList do
  begin
    if ex.url = 'http://hl7.org/fhir/StructureDefinition/patient-mothersMaidenName' then
      index(frtPatient, key, 0, ex.value as TFhirString, 'mothersMaidenName');
    if ex.url = 'http://hl7.org/fhir/StructureDefinition/us-core-race' then
      if ex.value is TFhirCodeableConcept then
        index(frtPatient, key, 0, ex.value as TFhirCodeableConcept, CODES_TSearchParamsPatient[spPatient_race]);
    if ex.url = 'http://hl7.org/fhir/StructureDefinition/us-core-ethnicity' then
      index(frtPatient, key, 0, ex.value as TFhirCodeableConcept, CODES_TSearchParamsPatient[spPatient_ethnicity]);
  end;

end;


procedure TFhirIndexManager.buildIndexValuesDiagnosticReport(key : integer; id : String; context : TFhirResource; resource: TFhirDiagnosticReport);
var
  i, j, k : integer;
begin
  index(frtDiagnosticReport, key, 0, resource.statusElement, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_status]);
  index(frtDiagnosticReport, key, 0, resource.identifierList, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_identifier]);

  index(context, frtDiagnosticReport, key, 0, resource.requestList, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_request]);

  index(frtDiagnosticReport, key, 0, resource.code, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_code]);
  for j := 0 to resource.resultList.count - 1 do
  begin
    index(context, frtDiagnosticReport, key, 0, resource.resultList[j], CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_result]);
  end;

  index(context, frtDiagnosticReport, key, 0, resource.subject, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_subject]);
  index(context, frtDiagnosticReport, key, 0, resource.subject, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_patient], frtPatient);
  patientCompartment(key, resource.subject);
  index(context, frtDiagnosticReport, key, 0, resource.performer, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_performer]);

  index(context, frtDiagnosticReport, key, 0, resource.encounter, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_encounter]);
  encounterCompartment(key, resource.encounter);

  index(frtDiagnosticReport, key, 0, resource.issuedElement, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_issued]);
  index(frtDiagnosticReport, key, 0, resource.category, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_category]);
  if resource.effective is TFhirPeriod then
    index(frtDiagnosticReport, key, 0, TFhirPeriod(resource.effective), CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_date])
  else
    index(frtDiagnosticReport, key, 0, TFhirDateTime(resource.effective), CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_date]);

  for i := 0 to resource.specimenList.Count - 1 Do
    index(context, frtDiagnosticReport, key, 0, resource.specimenList[i], CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_specimen]);

  for i := 0 to resource.imageList.Count - 1 Do
    index(context, frtDiagnosticReport, key, 0, resource.imageList[i].link_, CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_image]);
  for i := 0 to resource.codedDiagnosisList.Count - 1 Do
    index(frtDiagnosticReport, key, 0, resource.codedDiagnosisList[i], CODES_TSearchParamsDiagnosticReport[spDiagnosticReport_diagnosis]);
end;

procedure TFhirIndexManager.buildIndexValuesDiagnosticOrder(key : integer; id : String; context : TFhirResource; resource: TFhirDiagnosticOrder);
var
  i, j, k, p, p1 : integer;
begin
  index(context, frtDiagnosticOrder, key, 0, resource.subject, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_subject]);
  index(context, frtDiagnosticOrder, key, 0, resource.subject, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_patient]);
  patientCompartment(key, resource.subject);
  deviceCompartment(key, resource.subject);
  index(context, frtDiagnosticOrder, key, 0, resource.orderer, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_orderer]);
  practitionerCompartment(key, resource.orderer);
  index(context, frtDiagnosticOrder, key, 0, resource.Encounter, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_encounter]);
  encounterCompartment(key, resource.encounter);
  for i := 0 to resource.specimenList.Count - 1 do
    index(context, frtDiagnosticOrder, key, 0, resource.specimenList[i], CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_specimen]);
  index(frtDiagnosticOrder, key, 0, resource.statusElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_status]);
  for i := 0 to resource.identifierList.Count - 1 do
    index(frtDiagnosticOrder, key, 0, resource.identifierList[i], CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_identifier]);

  for j := 0 to resource.eventList.count - 1 do
  begin
    p := index(frtDiagnosticOrder, key, 0, 'event');
    index(context, frtDiagnosticOrder, key, p, resource.eventList[j].actor, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_actor]);
    practitionerCompartment(key, resource.eventList[j].actor);
    deviceCompartment(key, resource.eventList[j].actor);
    index(frtDiagnosticOrder, key, p, resource.eventList[j].statusElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_eventstatus]);
    index(frtDiagnosticOrder, key, p, resource.eventList[j].dateTimeElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_eventdate]);
  end;

  for k := 0 to resource.itemList.count - 1 do
  begin
    p := index(frtDiagnosticOrder, key, 0, 'item');
    index(frtDiagnosticOrder, key, p, resource.itemList[k].code, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_code]);
    for i := 0 to resource.itemList[k].specimenList.Count - 1 do
      index(context, frtDiagnosticOrder, key, 0, resource.itemList[k].specimenList[i], CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_specimen]);

    index(frtDiagnosticOrder, key, p, resource.itemList[k].statusElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_itemstatus]);
    for j := 0 to resource.itemList[k].eventList.count - 1 do
    begin
      p1 := index(frtDiagnosticOrder, key, p, 'item-event');
      index(context, frtDiagnosticOrder, key, p1, resource.itemList[k].eventList[j].actor, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_actor]);
      index(frtDiagnosticOrder, key, p1, resource.itemList[k].eventList[j].statusElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_itempaststatus]);
      index(frtDiagnosticOrder, key, p1, resource.itemList[k].eventList[j].dateTimeElement, CODES_TSearchParamsDiagnosticOrder[spDiagnosticOrder_itemdate]);
    end;
  end;
end;


procedure TFhirIndexManager.buildIndexValuesValueset(key : integer; id : String; context : TFhirResource; resource: TFhirValueset);
  procedure indexConcepts(list : TFhirValueSetCodeSystemConceptList);
  var
    i : integer;
  begin
    for i := 0 to list.Count - 1 do
    begin
      index(frtValueSet, key, 0, list[i].codeElement, CODES_TSearchParamsValueSet[spValueSet_code]);
      indexConcepts(list[i].conceptList);
    end;
  end;
var
  i : integer;
begin
  if (resource.codeSystem <> nil) then
  begin
    index(frtValueSet, key, 0, resource.codeSystem.systemElement, CODES_TSearchParamsValueSet[spValueSet_system]);
    indexConcepts(resource.codeSystem.conceptList);
  end;

  index(frtValueSet, key, 0, resource.identifierElement, CODES_TSearchParamsValueSet[spValueSet_identifier]);
  index(frtValueSet, key, 0, resource.versionElement, CODES_TSearchParamsValueSet[spValueSet_version]);
  index(frtValueSet, key, 0, resource.nameElement, CODES_TSearchParamsValueSet[spValueSet_name]);

  index(frtValueSet, key, 0, resource.statusElement, CODES_TSearchParamsValueSet[spValueSet_status]);
  index(frtValueSet, key, 0, resource.urlElement, CODES_TSearchParamsValueSet[spValueSet_url]);
  index(frtValueSet, key, 0, resource.useContextList, CODES_TSearchParamsValueSet[spValueSet_context]);

  index(frtValueSet, key, 0, resource.dateElement, CODES_TSearchParamsValueSet[spValueSet_date]);
  index(frtValueSet, key, 0, resource.publisherElement, CODES_TSearchParamsValueSet[spValueSet_publisher]);
  index(frtValueSet, key, 0, resource.descriptionElement, CODES_TSearchParamsValueSet[spValueSet_description]);
  if resource.compose <> nil then
  begin
    for i := 0 to resource.compose.importList.Count - 1 do
      index(frtValueSet, key, 0, resource.compose.importList[i], CODES_TSearchParamsValueSet[spValueSet_reference]);
    for i := 0 to resource.compose.includeList.Count - 1 do
      index(frtValueSet, key, 0, resource.compose.includeList[i].systemElement, CODES_TSearchParamsValueSet[spValueSet_reference]);
    for i := 0 to resource.compose.excludeList.Count - 1 do
      index(frtValueSet, key, 0, resource.compose.excludeList[i].systemElement, CODES_TSearchParamsValueSet[spValueSet_reference]);
  end;
  if (resource.expansion <> nil) then
    index(frtValueSet, key, 0, resource.expansion.identifier, CODES_TSearchParamsValueSet[spValueSet_expansion]);
end;

procedure TFhirIndexManager.buildIndexValuesConceptMap(key : integer; id : String; context : TFhirResource; resource: TFhirConceptMap);
var
  i, j, k, l : integer;
  list : TFhirConceptMapElementList;
begin
  index(frtConceptMap, key, 0, resource.identifierElement, CODES_TSearchParamsConceptMap[spConceptMap_identifier]);
  index(frtConceptMap, key, 0, resource.versionElement, CODES_TSearchParamsConceptMap[spConceptMap_version]);
  index(frtConceptMap, key, 0, resource.nameElement, CODES_TSearchParamsConceptMap[spConceptMap_name]);
  index(frtConceptMap, key, 0, resource.statusElement, CODES_TSearchParamsConceptMap[spConceptMap_status]);
  index(frtConceptMap, key, 0, resource.dateElement, CODES_TSearchParamsConceptMap[spConceptMap_date]);
  index(frtConceptMap, key, 0, resource.publisherElement, CODES_TSearchParamsConceptMap[spConceptMap_publisher]);
  index(frtConceptMap, key, 0, resource.descriptionElement, CODES_TSearchParamsConceptMap[spConceptMap_description]);

  index(frtConceptMap, key, 0, resource.useContextList, CODES_TSearchParamsConceptMap[spConceptMap_context]);
  if resource.source is TFhirReference then
    index(context, frtConceptMap, key, 0, TFhirReference(resource.source), CODES_TSearchParamsConceptMap[spConceptMap_sourceuri])
  else
    index(frtConceptMap, key, 0, TFhirURI(resource.source), CODES_TSearchParamsConceptMap[spConceptMap_source]);
  if resource.target is TFhirReference then
    index(context, frtConceptMap, key, 0, TFhirReference(resource.target), CODES_TSearchParamsConceptMap[spConceptMap_target])
  else
    index(frtConceptMap, key, 0, TFhirURI(resource.target), CODES_TSearchParamsConceptMap[spConceptMap_target]);
  list := resource.elementList;
  index(frtConceptMap, key, 0, resource.urlElement, CODES_TSearchParamsConceptMap[spConceptMap_url]);


  for i := 0 to list.count - 1 do
  begin
    index(frtConceptMap, key, 0, list[i].systemElement, CODES_TSearchParamsConceptMap[spConceptMap_sourcesystem]);
    index(frtConceptMap, key, 0, list[i].code, CODES_TSearchParamsConceptMap[spConceptMap_sourcecode]);
    k := 0; // todo index(frtConceptMap, key, 0, CODES_TSearchParamsConceptMap[spConceptMap_target]);
    for j := 0 to list[i].targetList.Count - 1 do
    begin
      for l := 0 to  list[i].targetList[j].dependsOnList.Count - 1 do
        index(frtConceptMap, key, k, list[i].targetList[j].dependsOnList[l].codeElement, CODES_TSearchParamsConceptMap[spConceptMap_dependson]);
      index(frtConceptMap, key, k, list[i].targetList[j].systemElement, CODES_TSearchParamsConceptMap[spConceptMap_targetsystem]);
      index(frtConceptMap, key, k, list[i].targetList[j].codeElement, CODES_TSearchParamsConceptMap[spConceptMap_targetcode]);
      for l := 0 to  list[i].targetList[j].productList.Count - 1 do
        index(frtConceptMap, key, k, list[i].targetList[j].productList[l].codeElement, CODES_TSearchParamsConceptMap[spConceptMap_product]);
    end;
  end;
end;




procedure TFhirIndexManager.buildIndexValuesDevice(key : integer; id : String; context : TFhirResource; resource: TFhirDevice);
var
  i : integer;
begin
  for i  := 0 to resource.identifierList.count - 1 do
    index(frtDevice, key, 0, resource.identifierList[i], CODES_TSearchParamsDevice[spDevice_identifier]);
  index(context, frtDevice, key, 0, resource.location, CODES_TSearchParamsDevice[spDevice_location]);
  index(frtDevice, key, 0, resource.manufacturerElement, CODES_TSearchParamsDevice[spDevice_manufacturer]);
  index(frtDevice, key, 0, resource.modelElement, CODES_TSearchParamsDevice[spDevice_model]);
  index(context, frtDevice, key, 0, resource.owner, CODES_TSearchParamsDevice[spDevice_organization]);
  index(context, frtDevice, key, 0, resource.patient, CODES_TSearchParamsDevice[spDevice_patient]);
  index(frtDevice, key, 0, resource.udiElement, CODES_TSearchParamsDevice[spDevice_udi]);
  index(frtDevice, key, 0, resource.urlElement, CODES_TSearchParamsDevice[spDevice_url]);
  index(frtDevice, key, 0, resource.type_, CODES_TSearchParamsDevice[spDevice_type]);
  patientCompartment(key, resource.patient);
end;



procedure TFhirIndexManager.buildIndexValuesAuditEvent(key : integer; id : String; context : TFhirResource; resource: TFhirAuditEvent);
var
  i, j : integer;
begin
  index(frtAuditEvent, key, 0, resource.event.type_, CODES_TSearchParamsAuditEvent[spAuditEvent_type]);
  index(frtAuditEvent, key, 0, resource.event.actionElement, CODES_TSearchParamsAuditEvent[spAuditEvent_action]);
  index(frtAuditEvent, key, 0, resource.event.dateTimeElement, CODES_TSearchParamsAuditEvent[spAuditEvent_date]);
  for i := 0 to resource.event.subTypeList.count - 1 do
    index(frtAuditEvent, key, 0, resource.event.subtypeList[i], CODES_TSearchParamsAuditEvent[spAuditEvent_subtype]);

  for i := 0 to resource.participantList.count - 1 do
  begin
    index(context, frtAuditEvent, key, 0, resource.participantList[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_participant]);
    deviceCompartment(key, resource.participantList[i].reference);
    practitionerCompartment(key, resource.participantList[i].reference);
    index(context, frtAuditEvent, key, 0, resource.participantList[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_patient], frtPatient);
    index(frtAuditEvent, key, 0, resource.participantList[i].userIdElement, CODES_TSearchParamsAuditEvent[spAuditEvent_user]);
    index(frtAuditEvent, key, 0, resource.participantList[i].altIdElement, CODES_TSearchParamsAuditEvent[spAuditEvent_altid]);
    index(frtAuditEvent, key, 0, resource.participantList[i].nameElement, CODES_TSearchParamsAuditEvent[spAuditEvent_name]);
    for j := 0 to resource.participantList[i].policyList.Count - 1 do
      index(frtAuditEvent, key, 0, resource.participantList[i].policyList[j], CODES_TSearchParamsAuditEvent[spAuditEvent_policy]);
    if resource.participantList[i].network <> nil then
      index(frtAuditEvent, key, 0, resource.participantList[i].network.addressElement, CODES_TSearchParamsAuditEvent[spAuditEvent_address]);
  end;

  if resource.source <> nil Then
  begin
    index(frtAuditEvent, key, 0, resource.source.identifierElement, CODES_TSearchParamsAuditEvent[spAuditEvent_source]);
    index(frtAuditEvent, key, 0, resource.source.siteElement, CODES_TSearchParamsAuditEvent[spAuditEvent_site]);
  end;

  for i := 0 to resource.object_List.count - 1 do
  begin
    index(frtAuditEvent, key, 0, resource.object_List[i].type_, CODES_TSearchParamsAuditEvent[spAuditEvent_objecttype]);
    index(frtAuditEvent, key, 0, resource.object_List[i].identifier, CODES_TSearchParamsAuditEvent[spAuditEvent_identity]);
    index(context, frtAuditEvent, key, 0, resource.object_List[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_reference]);
    patientCompartment(key, resource.object_List[i].reference);
    index(frtAuditEvent, key, 0, resource.object_List[i].nameElement, CODES_TSearchParamsAuditEvent[spAuditEvent_desc]);
  end;
end;




procedure TFhirIndexManager.buildIndexValuesCondition(key : integer; id : String; context : TFhirResource; resource: TFhirCondition);
var
  i : integer;
begin
  index(frtCondition, key, 0, resource.code, CODES_TSearchParamsCondition[spCondition_code]);
  index(frtCondition, key, 0, resource.clinicalStatusElement, CODES_TSearchParamsCondition[spCondition_clinicalstatus]);
  index(frtCondition, key, 0, resource.severity, CODES_TSearchParamsCondition[spCondition_severity]);
  index(frtCondition, key, 0, resource.category, CODES_TSearchParamsCondition[spCondition_category]);
  index(frtCondition, key, 0, resource.identifierList, CODES_TSearchParamsCondition[spCondition_identifier]);
  index(context, frtCondition, key, 0, resource.patient, CODES_TSearchParamsCondition[spCondition_patient]);
  patientCompartment(key, resource.patient);
  encounterCompartment(key, resource.encounter);
  practitionerCompartment(key, resource.asserter);

  index(context, frtCondition, key, 0, resource.Encounter, CODES_TSearchParamsCondition[spCondition_encounter]);
  index(context, frtCondition, key, 0, resource.asserter, CODES_TSearchParamsCondition[spCondition_asserter]);
  if (resource.onsetElement is TFHIRDateTime) then
    index(frtCondition, key, 0, resource.onsetElement as TFHIRDateTime, CODES_TSearchParamsCondition[spCondition_onset])
  else if (resource.onsetElement is TFHIRPeriod) then
    index(frtCondition, key, 0, resource.onsetElement as TFHIRPeriod, CODES_TSearchParamsCondition[spCondition_onset])
  else if (resource.onsetElement is TFhirQuantity) then
    index(frtCondition, key, 0, resource.onsetElement as TFhirQuantity, CODES_TSearchParamsCondition[spCondition_age])
  else if (resource.onsetElement is TFhirRange) then
    index(frtCondition, key, 0, resource.onsetElement as TFhirRange, CODES_TSearchParamsCondition[spCondition_onsetinfo])
  else if (resource.onsetElement is TFhirString) then
    index(frtCondition, key, 0, resource.onsetElement as TFhirString, CODES_TSearchParamsCondition[spCondition_onsetinfo]);


  index(frtCondition, key, 0, resource.dateRecordedElement, CODES_TSearchParamsCondition[spCondition_daterecorded]);
// todo  index(frtCondition, key, 0, resource.onset, CODES_TSearchParamsCondition[spCondition_onset]);
  for i := 0 to resource.evidenceList.count - 1 do
    index(frtCondition, key, 0, resource.evidenceList[i].code, CODES_TSearchParamsCondition[spCondition_evidence]);
  if resource.stage <> nil then
    index(frtCondition, key, 0, resource.stage.summary, CODES_TSearchParamsCondition[spCondition_stage]);

  index(frtCondition, key, 0, resource.bodySiteList, CODES_TSearchParamsCondition[spCondition_bodysite]);

    // DAF:
    index(frtCondition, key, 0, resource.identifierList, CODES_TSearchParamsCondition[spCondition_identifier]);
end;


procedure TFhirIndexManager.buildIndexValuesOperationOutcome(key : integer; id : String; context : TFhirResource; resource: TFhirOperationOutcome);
begin
end;



procedure TFhirIndexManager.buildIndexValuesBinary(key : integer; id : String; context : TFhirResource; resource: TFhirBinary);
begin

  index(frtBinary, key, 0, resource.contentType, CODES_TSearchParamsBinary[spBinary_contentType]);

end;

procedure TFhirIndexManager.buildIndexValuesProvenance(key : integer; id : String; context : TFhirResource; resource: TFhirProvenance);
var
  i : integer;
begin
  for i := 0 to resource.targetList.Count - 1 do
  begin
    index(context, frtProvenance, key, 0, resource.targetList[i], CODES_TSearchParamsProvenance[spProvenance_target]);
    index(context, frtProvenance, key, 0, resource.targetList[i], CODES_TSearchParamsProvenance[spProvenance_patient], frtPatient);
  end;
  if (resource.period <> nil) then
  begin
    index(frtProvenance, key, 0, resource.period.startElement, CODES_TSearchParamsProvenance[spProvenance_start]);
    index(frtProvenance, key, 0, resource.period.end_Element, CODES_TSearchParamsProvenance[spProvenance_end]);
  end;
  index(context, frtProvenance, key, 0, resource.location, CODES_TSearchParamsProvenance[spProvenance_location]);

  for i := 0 to resource.signatureList.Count - 1 do
    index(frtProvenance, key, 0, resource.signatureList[i].type_List, CODES_TSearchParamsProvenance[spProvenance_sigtype]);

  for i := 0 to resource.agentList.Count - 1 do
  begin
    index(context, frtProvenance, key, 0, resource.agentList[i].actor, CODES_TSearchParamsProvenance[spProvenance_agent]);
    index(frtProvenance, key, 0, resource.agentList[i].userId, CODES_TSearchParamsProvenance[spProvenance_userid]);
  end;

  for i := 0 to resource.entityList.Count - 1 do
  begin
    index(frtProvenance, key, 0, resource.entityList[i].referenceElement, CODES_TSearchParamsProvenance[spProvenance_entity]);
    index(frtProvenance, key, 0, resource.entityList[i].type_Element, CODES_TSearchParamsProvenance[spProvenance_Entitytype]);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesMedication(key : integer; id : String; context : TFhirResource; resource: TFhirMedication);
var
  i : integer;
begin
  index(frtMedication, key, 0, resource.code, CODES_TSearchParamsMedication[spMedication_code]);
  index(context, frtMedication, key, 0, resource.manufacturer, CODES_TSearchParamsMedication[spMedication_manufacturer]);
  if (resource.package <> nil) then
  begin
    index(frtMedication, key, 0, resource.package.container, CODES_TSearchParamsMedication[spMedication_container]);
    for i := 0 to resource.package.contentList.count - 1 do
      index(context, frtMedication, key, 0, resource.package.contentList[i].item, CODES_TSearchParamsMedication[spMedication_content]);
  end;
  if (resource.product <> nil) then
  begin
    index(frtMedication, key, 0, resource.product.form, CODES_TSearchParamsMedication[spMedication_form]);
    for i := 0 to resource.product.ingredientList.count - 1 do
      index(frtMedication, key, 0, TFhirCodeableConcept(resource.product.ingredientList[i].item), CODES_TSearchParamsMedication[spMedication_Ingredient])
  end;
end;


procedure TFhirIndexManager.buildIndexValuesMedicationAdministration(key : integer; id : String; context : TFhirResource; resource: TFhirMedicationAdministration);
var
  i : integer;
begin
  index(context, frtMedicationAdministration, key, 0, resource.patient, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_patient]);
  patientCompartment(key, resource.patient);
  index(context, frtMedicationAdministration, key, 0, resource.Encounter, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_encounter]);
  index(context, frtMedicationAdministration, key, 0, resource.prescription, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_prescription]);
  index(context, frtMedicationAdministration, key, 0, resource.practitioner, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_practitioner]);
  index(frtMedicationAdministration, key, 0, resource.wasNotGiven, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_Notgiven]);
  if resource.effectiveTime is TFhirPeriod then
    index(frtMedicationAdministration, key, 0, TFhirPeriod(resource.effectiveTime), CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_effectivetime])
  else
    index(frtMedicationAdministration, key, 0, TFhirDateTime(resource.effectiveTime), CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_effectivetime]);


  index(frtMedicationAdministration, key, 0, resource.statusElement, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_status]);
  if resource.medication is TFhirReference then
    index(context, frtMedicationAdministration, key, 0, resource.medication as TFhirReference, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_medication])
  else
    index(frtMedicationAdministration, key, 0, resource.medication as TFhirCodeableConcept, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_code]);

  for i := 0 to resource.identifierList.Count - 1 do
    index(frtMedicationAdministration, key, 0, resource.identifierList[i], CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_identifier]);
  if resource.Encounter <> nil then
    index(context, frtMedicationAdministration, key, 0, resource.Encounter, CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_encounter]);
  for i := 0 to resource.deviceList.Count - 1 do
    index(context, frtMedicationAdministration, key, 0, resource.deviceList[i], CODES_TSearchParamsMedicationAdministration[spMedicationAdministration_device]);
end;

procedure TFhirIndexManager.buildIndexValuesMedicationOrder(key : integer; id : String; context : TFhirResource; resource: TFhirMedicationOrder);
var
  i : integer;
begin
  index(frtMedicationOrder, key, 0, resource.statusElement, CODES_TSearchParamsMedicationOrder[spMedicationOrder_status]);
  index(context, frtMedicationOrder, key, 0, resource.patient, CODES_TSearchParamsMedicationOrder[spMedicationOrder_patient]);
  index(context, frtMedicationOrder, key, 0, resource.prescriber, CODES_TSearchParamsMedicationOrder[spMedicationOrder_prescriber]);
  patientCompartment(key, resource.patient);
  index(context, frtMedicationOrder, key, 0, resource.Encounter, CODES_TSearchParamsMedicationOrder[spMedicationOrder_encounter]);
  if resource.medication is TFhirReference then
    index(context, frtMedicationOrder, key, 0, resource.medication as TFhirReference, CODES_TSearchParamsMedicationOrder[spMedicationOrder_medication])
  else
    index(frtMedicationOrder, key, 0, resource.medication as TFhirCodeableConcept, CODES_TSearchParamsMedicationOrder[spMedicationOrder_code]);

  for i := 0 to resource.identifierList.Count - 1 do
    index(frtMedicationOrder, key, 0, resource.identifierList[i], CODES_TSearchParamsMedicationOrder[spMedicationOrder_identifier]);
  index(frtMedicationOrder, key, 0, resource.dateWrittenElement, CODES_TSearchParamsMedicationOrder[spMedicationOrder_datewritten]);
end;


procedure TFhirIndexManager.buildIndexValuesMedicationDispense(key : integer; id : String; context : TFhirResource; resource: TFhirMedicationDispense);
var
  i, j : integer;
begin
  index(frtMedicationDispense, key, 0, resource.statusElement, CODES_TSearchParamsMedicationDispense[spMedicationDispense_status]);
  index(context, frtMedicationDispense, key, 0, resource.patient, CODES_TSearchParamsMedicationDispense[spMedicationDispense_patient]);
  patientCompartment(key, resource.patient);
  index(context, frtMedicationDispense, key, 0, resource.dispenser, CODES_TSearchParamsMedicationDispense[spMedicationDispense_dispenser]);
  index(frtMedicationDispense, key, 0, resource.identifier, CODES_TSearchParamsMedicationDispense[spMedicationDispense_identifier]);
  for i := 0 to resource.authorizingPrescriptionList.Count - 1 do
    index(context, frtMedicationDispense, key, 0, resource.authorizingPrescriptionList[i], CODES_TSearchParamsMedicationDispense[spMedicationDispense_prescription]);
  index(frtMedicationDispense, key, 0, resource.identifier, CODES_TSearchParamsMedicationDispense[spMedicationDispense_identifier]);
  index(context, frtMedicationDispense, key, 0, resource.destination, CODES_TSearchParamsMedicationDispense[spMedicationDispense_destination]);
  if resource.medication is TFhirReference then
    index(context, frtMedicationDispense, key, 0, resource.medication as TFhirReference, CODES_TSearchParamsMedicationDispense[spMedicationDispense_medication])
  else
    index(frtMedicationDispense, key, 0, resource.medication as TFhirCodeableConcept, CODES_TSearchParamsMedicationDispense[spMedicationDispense_code]);
  index(context, frtMedicationDispense, key, 0, resource.receiverList, CODES_TSearchParamsMedicationDispense[spMedicationDispense_receiver]);
  index(frtMedicationDispense, key, 0, resource.type_, CODES_TSearchParamsMedicationDispense[spMedicationDispense_type]);
  index(frtMedicationDispense, key, 0, resource.whenPreparedElement, CODES_TSearchParamsMedicationDispense[spMedicationDispense_whenprepared]);
  index(frtMedicationDispense, key, 0, resource.whenHandedOverElement, CODES_TSearchParamsMedicationDispense[spMedicationDispense_whenhandedover]);

  if resource.substitution <> nil then
  begin
    for i := 0 to resource.substitution.responsiblePartyList.count - 1 do
      index(context, frtMedicationDispense, key, 0, resource.substitution.responsiblePartyList[i], CODES_TSearchParamsMedicationDispense[spMedicationDispense_responsibleparty]);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesMedicationStatement(key : integer; id : String; context : TFhirResource; resource: TFhirMedicationStatement);
var
  i : integer;
begin
  for i := 0 to resource.identifierList.Count - 1 do
    index(frtMedicationDispense, key, 0, resource.identifierList[i], CODES_TSearchParamsMedicationStatement[spMedicationStatement_identifier]);
  if resource.medication is TFhirReference then
    index(context, frtMedicationStatement, key, 0, resource.medication as TFhirReference, CODES_TSearchParamsMedicationStatement[spMedicationStatement_medication])
  else
    index(frtMedicationStatement, key, 0, resource.medication as TFhirCodeableConcept, CODES_TSearchParamsMedicationStatement[spMedicationStatement_code]);
  index(context, frtMedicationStatement, key, 0, resource.patient, CODES_TSearchParamsMedicationStatement[spMedicationStatement_patient]);

  index(context, frtMedicationStatement, key, 0, resource.informationSource, CODES_TSearchParamsMedicationStatement[spMedicationStatement_source]);
  index(frtMedicationStatement, key, 0, resource.statusElement, CODES_TSearchParamsMedicationStatement[spMedicationStatement_status]);

  patientCompartment(key, resource.patient);
  if resource.effectiveElement is TFhirPeriod then
    index(frtMedicationStatement, key, 0, TFhirPeriod(resource.effectiveElement), CODES_TSearchParamsMedicationStatement[spMedicationStatement_Effectivedate])
  else
    index(frtMedicationStatement, key, 0, TFhirDateTime(resource.effectiveElement), CODES_TSearchParamsMedicationStatement[spMedicationStatement_Effectivedate]);

end;

procedure TFhirIndexManager.buildIndexValuesList(key : integer; id : String; context : TFhirResource; resource: TFhirList);
var
  i : integer;
begin
  index(context, frtList, key, 0, resource.source, CODES_TSearchParamsList[spList_source]);
  for i := 0 to resource.entryList.count - 1 do
    index(context, frtList, key, 0, resource.entryList[i].item, CODES_TSearchParamsList[spList_item]);
  index(frtList, key, 0, resource.emptyReason, CODES_TSearchParamsList[spList_emptyreason]);
  index(frtList, key, 0, resource.dateElement, CODES_TSearchParamsList[spList_date]);
  index(frtList, key, 0, resource.codeElement, CODES_TSearchParamsList[spList_code]);
  index(context, frtList, key, 0, resource.subject, CODES_TSearchParamsList[spList_subject]);
  index(context, frtList, key, 0, resource.subject, CODES_TSearchParamsList[spList_patient], frtPatient);
  patientCompartment(key, resource.subject);
  deviceCompartment(key, resource.subject);
  index(context, frtList, key, 0, resource.encounter, CODES_TSearchParamsList[spList_encounter]);
  index(frtList, key, 0, resource.noteElement, CODES_TSearchParamsList[spList_notes]);
  index(frtList, key, 0, resource.titleElement, CODES_TSearchParamsList[spList_title]);
  index(frtList, key, 0, resource.statusElement, CODES_TSearchParamsList[spList_status]);
end;


procedure TFhirIndexManager.buildIndexValuesCarePlan(key: integer; id : String; context : TFhirResource; resource: TFhirCarePlan);
var
  i, j, k : integer;
begin
  index(context, frtCareplan, key, 0, resource.subject, CODES_TSearchParamsCareplan[spCareplan_patient]);
  index(context, frtCareplan, key, 0, resource.subject, CODES_TSearchParamsCareplan[spCareplan_subject]);
  patientCompartment(key, resource.subject);
  index(context, frtCareplan, key, 0, resource.addressesList, CODES_TSearchParamsCareplan[spCareplan_condition]);
  index(context, frtCareplan, key, 0, resource.goalList, CODES_TSearchParamsCareplan[spCareplan_goal]);
  index(frtCareplan, key, 0, resource.period, CODES_TSearchParamsCareplan[spCareplan_date]);
  for i := 0 to resource.participantList.Count - 1 do
  begin
    index(context, frtCareplan, key, 0, resource.participantList[i].member, CODES_TSearchParamsCareplan[spCareplan_participant]);
    practitionerCompartment(key, resource.participantList[i].member);
    relatedPersonCompartment(key, resource.participantList[i].member);
  end;
  for i := 0 to resource.activityList.Count - 1 do
  begin
    k := 0; //ci index(frtCareplan, key, 0, CODES_TSearchParamsCareplan[spCareplan_activity]);
    index(context, frtCareplan, key, 0, resource.activityList[i].reference, CODES_TSearchParamsCareplan[spCareplan_activityreference]);
    if resource.activityList[i].detail <> nil then
    begin
      index(frtCareplan, key, 0, resource.activityList[i].detail.code, CODES_TSearchParamsCareplan[spCareplan_activitycode]);
      index(context, frtCareplan, key, 0, resource.activityList[i].detail.performerList, CODES_TSearchParamsCareplan[spCareplan_performer]);
      for j := 0 to resource.activityList[i].detail.performerList.Count - 1 do
      begin
        relatedPersonCompartment(0, resource.activityList[i].detail.performerList[j]);
        practitionerCompartment(key, resource.activityList[i].detail.performerList[j]);
      end;
      if (resource.activityList[i].detail.scheduled is TFhirTiming) then
        index(frtCareplan, key, 0, TFhirTiming(resource.activityList[i].detail.scheduled), CODES_TSearchParamsCareplan[spCareplan_activitydate])
      else if (resource.activityList[i].detail.scheduled is TFhirPeriod) then
        index(frtCareplan, key, 0, TFhirPeriod(resource.activityList[i].detail.scheduled), CODES_TSearchParamsCareplan[spCareplan_activitydate]);
    end;
  end;
  for i := 0 to resource.relatedPlanList.Count - 1 do
  begin
    k := index(frtCareplan, key, 0, CODES_TSearchParamsCareplan[spCareplan_related]);
    index(frtCareplan, key, 0, resource.relatedPlanList[i].codeElement, CODES_TSearchParamsCareplan[spCareplan_relatedcode]);
    index(context, frtCareplan, key, 0, resource.relatedPlanList[i].planElement, CODES_TSearchParamsCareplan[spCareplan_relatedplan]);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesImagingStudy(key: integer; id : String; context : TFhirResource; resource: TFhirImagingStudy);
var
  i, j : integer;
  series : TFhirImagingStudySeries;
  image : TFhirImagingStudySeriesInstance;
begin
  index(frtImagingStudy, key, 0, resource.accession, CODES_TSearchParamsImagingStudy[spImagingStudy_accession]);
  index(context, frtImagingStudy, key, 0, resource.patient, CODES_TSearchParamsImagingStudy[spImagingStudy_patient]);
  patientCompartment(key, resource.patient);

  index(context, frtImagingStudy, key, 0, resource.orderList, CODES_TSearchParamsImagingStudy[spImagingStudy_order]);
  index(frtImagingStudy, key, 0, resource.startedElement, CODES_TSearchParamsImagingStudy[spImagingStudy_started]);

  index(frtImagingStudy, key, 0, resource.uidElement, CODES_TSearchParamsImagingStudy[spImagingStudy_study]);
  for i := 0 to resource.seriesList.count -1 do
  begin
    series := resource.seriesList[i];
    index(frtImagingStudy, key, 0, series.bodySite, CODES_TSearchParamsImagingStudy[spImagingStudy_bodySite]);

    index(frtImagingStudy, key, 0, series.uidElement, CODES_TSearchParamsImagingStudy[spImagingStudy_series]);
    index(frtImagingStudy, key, 0, series.ModalityElement, CODES_TSearchParamsImagingStudy[spImagingStudy_modality]);
//    index(frtImagingStudy, key, 0, resource., CODES_TSearchParamsImagingStudy[spImagingStudy_size]);
    for j := 0 to series.instanceList.count - 1 do
    begin
      image := series.instanceList[j];
      index(frtImagingStudy, key, 0, image.uidElement, CODES_TSearchParamsImagingStudy[spImagingStudy_uid]);
      index(frtImagingStudy, key, 0, image.sopclassElement, CODES_TSearchParamsImagingStudy[spImagingStudy_dicomclass]);
    end;
  end;
end;


procedure TFhirIndexManager.buildIndexValuesImmunization(key: integer; id : String; context : TFhirResource; resource: TFhirImmunization);
var
  i : integer;
begin
  index(frtImmunization, key, 0, resource.vaccineCode, CODES_TSearchParamsImmunization[spImmunization_vaccinecode]);
  index(frtImmunization, key, 0, resource.dateElement, CODES_TSearchParamsImmunization[spImmunization_date]);
  if resource.explanation <> nil then
  begin
    for i := 0 to resource.explanation.reasonNotGivenList.count - 1 do
      index(frtImmunization, key, 0, resource.explanation.reasonNotGivenList[i], CODES_TSearchParamsImmunization[spImmunization_reasonnotgiven]);

    for i := 0 to resource.explanation.reasonList.count - 1 do
      index(frtImmunization, key, 0, resource.explanation.reasonList[i], CODES_TSearchParamsImmunization[spImmunization_reason]);
  end;
  for i := 0 to resource.identifierList.count - 1 do
      index(frtImmunization, key, 0, resource.identifierList[i], CODES_TSearchParamsImmunization[spImmunization_identifier]);
  index(frtImmunization, key, 0, resource.lotNumberElement, CODES_TSearchParamsImmunization[spImmunization_lotnumber]);
  index(frtImmunization, key, 0, resource.wasNotGivenElement, CODES_TSearchParamsImmunization[spImmunization_notgiven]);
  index(context, frtImmunization, key, 0, resource.patient, CODES_TSearchParamsImmunization[spImmunization_patient]);
  index(frtImmunization, key, 0, resource.statusElement, CODES_TSearchParamsImmunization[spImmunization_status]);
  patientCompartment(key, resource.patient);

  index(context, frtImmunization, key, 0, resource.manufacturer, CODES_TSearchParamsImmunization[spImmunization_manufacturer]);
  index(context, frtImmunization, key, 0, resource.location, CODES_TSearchParamsImmunization[spImmunization_location]);
  index(context, frtImmunization, key, 0, resource.performer, CODES_TSearchParamsImmunization[spImmunization_performer]);
  index(context, frtImmunization, key, 0, resource.requester, CODES_TSearchParamsImmunization[spImmunization_requester]);
  for i := 0 to resource.reactionList.count - 1 do
  begin
    index(context, frtImmunization, key, 0, resource.reactionList[i].detail, CODES_TSearchParamsImmunization[spImmunization_reaction]);
    index(frtImmunization, key, 0, resource.reactionList[i].dateElement, CODES_TSearchParamsImmunization[spImmunization_reactiondate]);
  end;
  for i := 0 to resource.vaccinationProtocolList.count - 1 do
    index(frtImmunization, key, 0, resource.vaccinationProtocolList[i].doseSequenceElement, CODES_TSearchParamsImmunization[spImmunization_dosesequence]);
end;

procedure TFhirIndexManager.buildIndexValuesOrder(key: integer; id : String; context : TFhirResource; resource: TFhirOrder);
var
  i : integer;
begin
  index(frtOrder, key, 0, resource.dateElement, CODES_TSearchParamsOrder[spOrder_date]);
  index(context, frtOrder, key, 0, resource.subject, CODES_TSearchParamsOrder[spOrder_subject]);
  index(context, frtOrder, key, 0, resource.subject, CODES_TSearchParamsOrder[spOrder_patient], frtPatient);
  index(frtOrder, key, 0, resource.identifierList, CODES_TSearchParamsOrder[spOrder_identifier]);
  patientCompartment(key, resource.subject);
  index(context, frtOrder, key, 0, resource.source, CODES_TSearchParamsOrder[spOrder_source]);
  index(context, frtOrder, key, 0, resource.target, CODES_TSearchParamsOrder[spOrder_target]);
  if resource.when <> nil then
  begin
    index(frtOrder, key, 0, resource.when.code, CODES_TSearchParamsOrder[spOrder_when_code]);
    index(frtOrder, key, 0, resource.when.schedule, CODES_TSearchParamsOrder[spOrder_when]);
  end;
  for i := 0 to resource.detailList.count - 1 do
    index(context, frtOrder, key, 0, resource.detailList[i], CODES_TSearchParamsOrder[spOrder_detail]);
end;


procedure TFhirIndexManager.buildIndexValuesOrderResponse(key: integer; id : String; context : TFhirResource; resource: TFhirOrderResponse);
var
  i : integer;
begin
  index(context, frtOrderResponse, key, 0, resource.request, CODES_TSearchParamsOrderResponse[spOrderResponse_request]);
  index(frtOrderResponse, key, 0, resource.identifierList, CODES_TSearchParamsOrderResponse[spOrderResponse_identifier]);
  index(frtOrderResponse, key, 0, resource.dateElement, CODES_TSearchParamsOrderResponse[spOrderResponse_date]);
  index(context, frtOrderResponse, key, 0, resource.who, CODES_TSearchParamsOrderResponse[spOrderResponse_who]);
  index(frtOrderResponse, key, 0, resource.orderStatusElement, CODES_TSearchParamsOrderResponse[spOrderResponse_code]);

  for i := 0 to resource.fulfillmentList.count - 1 do
    index(context, frtOrderResponse, key, 0, resource.fulfillmentList[i], CODES_TSearchParamsOrderResponse[spOrderResponse_fulfillment]);
end;

procedure TFhirIndexManager.buildIndexValuesMedia(key: integer; id : String; context : TFhirResource; resource: TFhirMedia);
var
  i : integer;
begin
  index(context, frtMedia, key, 0, resource.subject, CODES_TSearchParamsMedia[spMedia_subject]);
  patientCompartment(key, resource.subject);
  for i := 0 to resource.identifierList.count - 1 do
    index(frtMedia, key, 0, resource.identifierList[i], CODES_TSearchParamsMedia[spMedia_identifier]);
  index(context, frtMedia, key, 0, resource.operator, CODES_TSearchParamsMedia[spMedia_operator]);
  index(frtMedia, key, 0, resource.type_Element, CODES_TSearchParamsMedia[spMedia_type]);
  index(frtMedia, key, 0, resource.subtype, CODES_TSearchParamsMedia[spMedia_subtype]);
  if resource.content <> nil then
    index(frtMedia, key, 0, resource.content.creationElement, CODES_TSearchParamsMedia[spMedia_created]);

//  index(frtMedia, key, 0, resource.size, CODES_TSearchParamsMedia[spMedia_size]);
  index(frtMedia, key, 0, resource.view, CODES_TSearchParamsMedia[spMedia_view]);
end;



procedure TFhirIndexManager.buildIndexValuesFamilyMemberHistory(key: integer; id : String; context : TFhirResource; resource: TFhirFamilyMemberHistory);
var
  cond : TFhirFamilyMemberHistoryCondition;
begin
  index(frtFamilyMemberHistory, key, 0, resource.dateElement, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_date]);
  index(frtFamilyMemberHistory, key, 0, resource.genderElement, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_gender]);
  index(context, frtFamilyMemberHistory, key, 0, resource.patient, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_patient]);
  patientCompartment(key, resource.patient);
  index(frtFamilyMemberHistory, key, 0, resource.identifierList, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_identifier]);

  for cond in resource.conditionList do
  begin
    index(frtFamilyMemberHistory, key, 0, cond.code, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_code]);
    index(frtFamilyMemberHistory, key, 0, cond.code, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_condition]);

  // DAF:
    index(frtFamilyMemberHistory, key, 0, cond.code, 'familymemberhistorycondition');
  end;
  index(frtFamilyMemberHistory, key, 0, resource.relationship, CODES_TSearchParamsFamilyMemberHistory[spFamilyMemberHistory_relationship]);
end;




procedure TFhirIndexManager.buildIndexValuesProcedure(key: integer; id : String; context : TFhirResource; resource: TFhirProcedure);
var
  i : integer;
begin
  index(frtProcedure, key, 0, resource.code, CODES_TSearchParamsProcedure[spProcedure_code]);
  if resource.performed is TFhirDateTime then
    index(frtProcedure, key, 0, resource.performed as TFhirDateTime, CODES_TSearchParamsProcedure[spProcedure_date])
  else
    index(frtProcedure, key, 0, resource.performed as TFhirPeriod, CODES_TSearchParamsProcedure[spProcedure_date]);

  index(context, frtProcedure, key, 0, resource.subject, CODES_TSearchParamsProcedure[spProcedure_subject]);
  index(context, frtProcedure, key, 0, resource.subject, CODES_TSearchParamsProcedure[spProcedure_patient], frtPatient);
  patientCompartment(key, resource.subject);

  index(context, frtProcedure, key, 0, resource.location, CODES_TSearchParamsProcedure[spProcedure_location]);
  for i := 0 to resource.performerList.Count - 1 do
    index(context, frtProcedure, key, 0, resource.performerList[i].actor, CODES_TSearchParamsProcedure[spProcedure_performer]);
  index(context, frtProcedure, key, 0, resource.encounter, CODES_TSearchParamsProcedure[spProcedure_encounter]);
  index(frtProcedure, key, 0, resource.identifierList, CODES_TSearchParamsProcedure[spProcedure_identifier]);
end;

procedure TFhirIndexManager.buildIndexValuesSpecimen(key: integer; id : String; context : TFhirResource; resource: TFhirSpecimen);
var
  i, j : integer;
begin
  index(context, frtSpecimen, key, 0, resource.subject, CODES_TSearchParamsSpecimen[spSpecimen_subject]);
  patientCompartment(key, resource.subject);

  index(context, frtSpecimen, key, 0, resource.subject, CODES_TSearchParamsSpecimen[spSpecimen_patient], frtPatient);
  index(frtSpecimen, key, 0, resource.accessionIdentifier, CODES_TSearchParamsSpecimen[spSpecimen_accession]);
  index(frtSpecimen, key, 0, resource.type_, CODES_TSearchParamsSpecimen[spSpecimen_type]);
  index(frtSpecimen, key, 0, resource.identifierList, CODES_TSearchParamsSpecimen[spSpecimen_identifier]);
  index(context, frtSpecimen, key, 0, resource.parentList, CODES_TSearchParamsSpecimen[spSpecimen_parent]);
  if (resource.collection <> nil) then
  begin
    if resource.collection.collected is TFhirPeriod then
      index(frtSpecimen, key, 0, TFhirPeriod(resource.collection.collected), CODES_TSearchParamsSpecimen[spSpecimen_collected])
    else
      index(frtSpecimen, key, 0, TFhirDateTime(resource.collection.collected), CODES_TSearchParamsSpecimen[spSpecimen_collected]);
    index(context, frtSpecimen, key, 0, resource.collection.collector, CODES_TSearchParamsSpecimen[spSpecimen_collector]);
    index(frtSpecimen, key, 0, resource.collection.bodySite, CODES_TSearchParamsSpecimen[spSpecimen_bodysite]);
  end;
  for i := 0 to resource.containerList.Count - 1 do
  begin
    index(frtSpecimen, key, 0, resource.containerList[i].type_, CODES_TSearchParamsSpecimen[spSpecimen_container]);
    index(frtSpecimen, key, 0, resource.containerList[i].identifierList, CODES_TSearchParamsSpecimen[spSpecimen_containerid]);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesImmunizationRecommendation(key: integer; id : String; context : TFhirResource; resource: TFhirImmunizationRecommendation);
var
  i,j  : integer;
begin
  index(context, frtImmunizationRecommendation, key, 0, resource.patient, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_patient]);
  patientCompartment(key, resource.patient);


  for i := 0 to resource.identifierList.count - 1 do
    index(frtImmunizationRecommendation, key, 0, resource.identifierList[i], CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_identifier]);

  for i := 0 to resource.recommendationList.count - 1 do
  begin
    index(frtImmunizationRecommendation, key, 0, resource.recommendationList[i].dateElement, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_date]);
    index(frtImmunizationRecommendation, key, 0, resource.recommendationList[i].vaccineCode, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_vaccinetype]);
    index(frtImmunizationRecommendation, key, 0, resource.recommendationList[i].doseNumberElement, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_dosenumber]);
    index(frtImmunizationRecommendation, key, 0, resource.recommendationList[i].forecastStatus, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_status]);
    if resource.recommendationList[i].protocol <> nil then
      index(frtImmunizationRecommendation, key, 0, resource.recommendationList[i].protocol.doseSequenceElement, CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_dosesequence]);
    for j := 0 to resource.recommendationList[i].supportingPatientInformationList.Count - 1 do
      index(context, frtImmunizationRecommendation, key, 0, resource.recommendationList[i].supportingPatientInformationList[j], CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_information]);
    for j := 0 to resource.recommendationList[i].supportingImmunizationList.Count - 1 do
      index(context, frtImmunizationRecommendation, key, 0, resource.recommendationList[i].supportingImmunizationList[j], CODES_TSearchParamsImmunizationRecommendation[spImmunizationRecommendation_support]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesQuestionnaire(key: integer; id : String; context : TFhirResource; resource: TFhirQuestionnaire);
  procedure IndexGroup(group : TFhirQuestionnaireGroup);
  var
    i : integer;
  begin
    index(frtQuestionnaire, key, 0, group.conceptList, CODES_TSearchParamsQuestionnaire[spQuestionnaire_code]);
    for I := 0 to group.groupList.Count - 1 do
      indexGroup(group.groupList[i]);
  end;
begin
  index(frtQuestionnaire, key, 0, resource.publisherElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_publisher]);
  index(frtQuestionnaire, key, 0, resource.statusElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_status]);
  index(frtQuestionnaire, key, 0, resource.identifierList, CODES_TSearchParamsQuestionnaire[spQuestionnaire_identifier]);
  index(frtQuestionnaire, key, 0, resource.dateElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_date]);
  index(frtQuestionnaire, key, 0, resource.versionElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_version]);
  index(frtQuestionnaire, key, 0, resource.group.titleElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_title]);
  IndexGroup(resource.group);
end;

procedure TFhirIndexManager.buildIndexValuesQuestionnaireResponse(key: integer; id : String; context : TFhirResource; resource: TFhirQuestionnaireResponse);
begin
  index(context, frtQuestionnaireResponse, key, 0, resource.author, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_author]);
  index(context, frtQuestionnaireResponse, key, 0, resource.encounter, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_encounter]);
  index(context, frtQuestionnaireResponse, key, 0, resource.questionnaire, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_questionnaire]);
  index(context, frtQuestionnaireResponse, key, 0, resource.subject, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_subject]);
  index(context, frtQuestionnaireResponse, key, 0, resource.subject, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_patient], frtPatient);
  index(context, frtQuestionnaireResponse, key, 0, resource.source, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_source]);
  index(frtQuestionnaireResponse, key, 0, resource.statusElement, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_status]);
  index(frtQuestionnaireResponse, key, 0, resource.authoredElement, CODES_TSearchParamsQuestionnaireResponse[spQuestionnaireResponse_authored]);
  patientCompartment(key, resource.subject);
  patientCompartment(key, resource.author);
end;



procedure TFhirIndexManager.buildIndexValuesSlot(key: integer; id : String; context : TFhirResource; resource: TFhirSlot);
begin
  index(context, frtSlot, key, 0, resource.schedule, CODES_TSearchParamsSlot[spSlot_schedule]);
  index(frtSlot, key, 0, resource.freeBusyTypeElement, CODES_TSearchParamsSlot[spSlot_fbtype]);
  index(frtSlot, key, 0, resource.type_, CODES_TSearchParamsSlot[spSlot_slottype]);
  index(frtSlot, key, 0, resource.identifierList, CODES_TSearchParamsSlot[spSlot_identifier]);
  index(frtSlot, key, 0, resource.startElement, CODES_TSearchParamsSlot[spSlot_start]);
end;

procedure TFhirIndexManager.buildIndexValuesAppointment(key: integer; id : String; context : TFhirResource; resource: TFhirAppointment);
var
  i : integer;
begin
  index(frtAppointment, key, 0, resource.startElement, CODES_TSearchParamsAppointment[spAppointment_date]);
  index(frtAppointment, key, 0, resource.identifierList, CODES_TSearchParamsAppointment[spAppointment_identifier]);
  index(frtAppointment, key, 0, resource.statusElement, CODES_TSearchParamsAppointment[spAppointment_status]);
  for i := 0 to resource.participantList.Count - 1 do
  begin
    index(frtAppointment, key, 0, resource.participantList[i].statusElement, CODES_TSearchParamsAppointment[spAppointment_Partstatus]);
    index(context, frtAppointment, key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_actor]);
    index(context, frtAppointment, key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_patient], frtPatient);
    index(context, frtAppointment, key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_location], frtLocation);
    index(context, frtAppointment, key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_practitioner], frtPractitioner);
    patientCompartment(key, resource.participantList[i].actor);
    practitionerCompartment(key, resource.participantList[i].actor);
    deviceCompartment(key, resource.participantList[i].actor);
    relatedPersonCompartment(key, resource.participantList[i].actor);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesSchedule(key: integer; id : String; context : TFhirResource; resource: TFhirSchedule);
var
  i : integer;
begin
  index(frtSchedule, key, 0, resource.planningHorizon, CODES_TSearchParamsSchedule[spSchedule_date]);
  index(frtSchedule, key, 0, resource.identifierList, CODES_TSearchParamsSchedule[spSchedule_identifier]);
  index(context, frtSchedule, key, 0, resource.actor, CODES_TSearchParamsSchedule[spSchedule_actor]);
  patientCompartment(key, resource.actor);
end;

procedure TFhirIndexManager.buildIndexValuesAppointmentResponse(key: integer; id : String; context : TFhirResource; resource: TFhirAppointmentResponse);
var
  i : integer;
begin
  index(frtAppointmentResponse, key, 0, resource.participantStatusElement, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_partstatus]);
  index(context, frtAppointmentResponse, key, 0, resource.appointment, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_appointment]);
  index(frtAppointmentResponse, key, 0, resource.identifierList, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_identifier]);
  index(context, frtAppointmentResponse, key, 0, resource.actor, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_actor]);
  index(context, frtAppointmentResponse, key, 0, resource.actor, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_patient], frtPatient);
  index(context, frtAppointmentResponse, key, 0, resource.actor, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_practitioner], frtPractitioner);
  index(context, frtAppointmentResponse, key, 0, resource.actor, CODES_TSearchParamsAppointmentResponse[spAppointmentResponse_location], frtLocation);
  patientCompartment(key, resource.actor);
  deviceCompartment(key, resource.actor);
  practitionerCompartment(key, resource.actor);
  relatedPersonCompartment(key, resource.actor);
end;


procedure TFhirIndexManager.buildIndexValuesHealthcareService(key: integer; id : String; context : TFhirResource; resource: TFhirHealthcareService);
var
  i : integer;
begin
  index(frtHealthcareService, key, 0, resource.serviceNameElement, CODES_TSearchParamsHealthcareService[spHealthcareService_name]);
  index(frtHealthcareService, key, 0, resource.identifierList, CODES_TSearchParamsHealthcareService[spHealthcareService_identifier]);
  index(frtHealthcareService, key, 0, resource.characteristicList, CODES_TSearchParamsHealthcareService[spHealthcareService_characteristic]);
  for i := 0 to resource.programNameList.Count - 1 do
    index(frtHealthcareService, key, 0, resource.programNameList[i], CODES_TSearchParamsHealthcareService[spHealthcareService_programname]);
  index(context, frtHealthcareService, key, 0, resource.location, CODES_TSearchParamsHealthcareService[spHealthcareService_location]);
  index(context, frtHealthcareService, key, 0, resource.providedBy, CODES_TSearchParamsHealthcareService[spHealthcareService_organization]);
  index(frtHealthcareService, key, 0, resource.serviceCategoryElement, CODES_TSearchParamsHealthcareService[spHealthcareService_servicecategory]);
end;

procedure TFhirIndexManager.buildIndexValuesDataElement(key: integer; id : String; context : TFhirResource; resource: TFhirDataElement);
var
  i : integer;
begin

  index(frtDataElement, key, 0, resource.dateElement, CODES_TSearchParamsDataElement[spDataElement_date]);
  index(frtDataElement, key, 0, resource.urlElement, CODES_TSearchParamsDataElement[spDataElement_url]);
  index(frtDataElement, key, 0, resource.identifierList, CODES_TSearchParamsDataElement[spDataElement_identifier]);
  index(frtDataElement, key, 0, resource.useContextList, CODES_TSearchParamsDataElement[spDataElement_context]);
  index(frtDataElement, key, 0, resource.nameElement, CODES_TSearchParamsDataElement[spDataElement_name]);
  index(frtDataElement, key, 0, resource.publisherElement, CODES_TSearchParamsDataElement[spDataElement_publisher]);
  index(frtDataElement, key, 0, resource.statusElement, CODES_TSearchParamsDataElement[spDataElement_status]);
  index(frtDataElement, key, 0, resource.versionElement, CODES_TSearchParamsDataElement[spDataElement_version]);
  index(frtDataElement, key, 0, resource.stringencyElement, CODES_TSearchParamsDataElement[spDataElement_stringency]);
  for i := 0 to resource.elementList.Count - 1 do
  begin
    if i = 0 then
      index(frtDataElement, key, 0, resource.elementList[i].definitionElement, CODES_TSearchParamsDataElement[spDataElement_description]);
    index(frtDataElement, key, 0, resource.elementList[i].codeList, CODES_TSearchParamsDataElement[spDataElement_code]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesTestScript(key: integer; id : String; context : TFhirResource; resource: TFhirTestScript);
var
  i, j : integer;
  c : TFhirTestScriptMetadataCapability;
  t : TFhirTestScriptTest;
begin
  index(frtTestScript, key, 0, resource.nameElement, CODES_TSearchParamsTestScript[spTestScript_name]);
  index(frtTestScript, key, 0, resource.descriptionElement, CODES_TSearchParamsTestScript[spTestScript_description]);
  index(frtTestScript, key, 0, resource.identifier, CODES_TSearchParamsTestScript[spTestScript_identifier]);
  if (resource.metadata <> nil) then
    for c in resource.metadata.capabilityList do
      index(frtTestScript, key, 0, c.descriptionElement, CODES_TSearchParamsTestScript[spTestScript_testscriptcapability]);

  if (resource.setup <> nil) and (resource.setup.metadata <> nil) then
    for c in resource.setup.metadata.capabilityList do
      index(frtTestScript, key, 0, c.descriptionElement, CODES_TSearchParamsTestScript[spTestScript_testscriptsetupcapability]);

  for t in resource.testList do
    if (t.metadata <> nil) then
      for c in t.metadata.capabilityList do
        index(frtTestScript, key, 0, c.descriptionElement, CODES_TSearchParamsTestScript[spTestScript_testscripttestcapability]);

  index(frtTestScript, key, 0, resource.url, CODES_TSearchParamsTestScript[spTestScript_url]);
end;


procedure TFhirIndexManager.buildIndexValuesNamingSystem(key: integer; id : String; context : TFhirResource; resource: TFhirNamingSystem);
var
  i, j : integer;
begin
  index(frtNamingSystem, key, 0, resource.kindElement, CODES_TSearchParamsNamingSystem[spNamingSystem_kind]);
  for i  := 0 to resource.contactList.Count - 1 do
  begin
    index(frtNamingSystem, key, 0, resource.contactList[i].name, CODES_TSearchParamsNamingSystem[spNamingSystem_contact]);
    for j := 0 to resource.contactList[i].telecomList.Count - 1 do
      index(frtNamingSystem, key, 0, resource.contactList[i].telecomList[j], CODES_TSearchParamsNamingSystem[spNamingSystem_telecom]);
  end;
  index(frtNamingSystem, key, 0, resource.useContextList, CODES_TSearchParamsNamingSystem[spNamingSystem_context]);
  index(frtNamingSystem, key, 0, resource.dateElement, CODES_TSearchParamsNamingSystem[spNamingSystem_date]);
  index(frtNamingSystem, key, 0, resource.type_, CODES_TSearchParamsNamingSystem[spNamingSystem_type]);
  index(frtNamingSystem, key, 0, resource.nameElement, CODES_TSearchParamsNamingSystem[spNamingSystem_name]);
  for i := 0 to resource.uniqueIdList.Count - 1 do
  begin
    index(frtNamingSystem, key, 0, resource.uniqueIdList[i].period, CODES_TSearchParamsNamingSystem[spNamingSystem_period]);
    index(frtNamingSystem, key, 0, resource.uniqueIdList[i].type_Element, CODES_TSearchParamsNamingSystem[spNamingSystem_idtype]);
    index(frtNamingSystem, key, 0, resource.uniqueIdList[i].valueElement, CODES_TSearchParamsNamingSystem[spNamingSystem_value]);
  end;
  index(frtNamingSystem, key, 0, resource.publisherElement, CODES_TSearchParamsNamingSystem[spNamingSystem_publisher]);
  index(context, frtNamingSystem, key, 0, resource.replacedBy, CODES_TSearchParamsNamingSystem[spNamingSystem_replacedby]);
  index(frtNamingSystem, key, 0, resource.responsibleElement, CODES_TSearchParamsNamingSystem[spNamingSystem_responsible]);
  index(frtNamingSystem, key, 0, resource.statusElement, CODES_TSearchParamsNamingSystem[spNamingSystem_status]);
end;

procedure TFhirIndexManager.buildIndexValuesSubscription(key: integer; id : String; context : TFhirResource; resource: TFhirSubscription);
var
  i : integer;
begin
  for i := 0 to resource.contactList.Count - 1 do
    index(frtSubscription, key, 0, resource.contactList[i], CODES_TSearchParamsSubscription[spSubscription_contact]);
  index(frtSubscription, key, 0, resource.criteriaElement, CODES_TSearchParamsSubscription[spSubscription_criteria]);
  index(frtSubscription, key, 0, resource.statusElement, CODES_TSearchParamsSubscription[spSubscription_status]);
  for i := 0 to resource.tagList.Count - 1 do
    index(frtSubscription, key, 0, resource.tagList[i], CODES_TSearchParamsSubscription[spSubscription_tag]);
  index(frtSubscription, key, 0, resource.channel.type_Element, CODES_TSearchParamsSubscription[spSubscription_type]);
  index(frtSubscription, key, 0, resource.channel.payloadElement, CODES_TSearchParamsSubscription[spSubscription_payload]);
  index(frtSubscription, key, 0, resource.channel.endpoint, CODES_TSearchParamsSubscription[spSubscription_url]);
end;

procedure TFhirIndexManager.buildIndexValuesDetectedIssue(key: integer; id : String; context : TFhirResource; resource: TFhirDetectedIssue);
var
  i : integer;
begin
  index(frtDetectedIssue, key, 0, resource.category, CODES_TSearchParamsDetectedIssue[spDetectedIssue_category]);
  index(frtDetectedIssue, key, 0, resource.dateElement, CODES_TSearchParamsDetectedIssue[spDetectedIssue_date]);
  index(frtDetectedIssue, key, 0, resource.identifier, CODES_TSearchParamsDetectedIssue[spDetectedIssue_identifier]);
  for i := 0 to resource.implicatedList.Count - 1 do
    index(context, frtDetectedIssue, key, 0, resource.patient, CODES_TSearchParamsDetectedIssue[spDetectedIssue_implicated]);
  index(context, frtDetectedIssue, key, 0, resource.patient, CODES_TSearchParamsDetectedIssue[spDetectedIssue_patient]);
  patientCompartment(key, resource.patient);
  index(context, frtDetectedIssue, key, 0, resource.author, CODES_TSearchParamsDetectedIssue[spDetectedIssue_author]);
  deviceCompartment(key, resource.author);
  practitionerCompartment(key, resource.author);
end;

procedure TFhirIndexManager.buildIndexValuesRiskAssessment(key: integer; id : String; context : TFhirResource; resource: TFhirRiskAssessment);
var
  i : integer;
begin
  index(frtRiskAssessment, key, 0, resource.dateElement, CODES_TSearchParamsRiskAssessment[spRiskAssessment_date]);
  index(frtRiskAssessment, key, 0, resource.identifier, CODES_TSearchParamsRiskAssessment[spRiskAssessment_identifier]);

  index(frtRiskAssessment, key, 0, resource.method, CODES_TSearchParamsRiskAssessment[spRiskAssessment_method]);
  index(context, frtRiskAssessment, key, 0, resource.subject, CODES_TSearchParamsRiskAssessment[spRiskAssessment_subject]);
  index(context, frtRiskAssessment, key, 0, resource.subject, CODES_TSearchParamsRiskAssessment[spRiskAssessment_patient]);
  index(context, frtRiskAssessment, key, 0, resource.condition, CODES_TSearchParamsRiskAssessment[spRiskAssessment_condition]);
  index(context, frtRiskAssessment, key, 0, resource.performer, CODES_TSearchParamsRiskAssessment[spRiskAssessment_performer]);
  index(context, frtRiskAssessment, key, 0, resource.encounter, CODES_TSearchParamsRiskAssessment[spRiskAssessment_encounter]);
end;

procedure TFhirIndexManager.buildIndexValuesOperationDefinition(key : integer; id : String; context : TFhirResource; resource: TFhirOperationDefinition);
var
  i : integer;
begin
  index(frtOperationDefinition, key, 0, resource.urlElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_url]);
  index(frtOperationDefinition, key, 0, resource.statusElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_status]);
  index(frtOperationDefinition, key, 0, resource.versionElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_version]);
  index(frtOperationDefinition, key, 0, resource.publisherElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_publisher]);
  index(frtOperationDefinition, key, 0, resource.nameElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_name]);
  index(frtOperationDefinition, key, 0, resource.codeElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_code]);
  index(context, frtOperationDefinition, key, 0, resource.base, CODES_TSearchParamsOperationDefinition[spOperationDefinition_base]);
  index(frtOperationDefinition, key, 0, resource.dateElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_date]);
  index(frtOperationDefinition, key, 0, resource.kindElement, CODES_TSearchParamsOperationDefinition[spOperationDefinition_kind]);
  index(frtOperationDefinition, key, 0, resource.system, CODES_TSearchParamsOperationDefinition[spOperationDefinition_system]);
  for i := 0 to resource.type_List.count - 1 Do
    index(frtOperationDefinition, key, 0, resource.type_List[i], CODES_TSearchParamsOperationDefinition[spOperationDefinition_type]);
  index(frtOperationDefinition, key, 0, resource.instance, CODES_TSearchParamsOperationDefinition[spOperationDefinition_instance]);
  for i := 0 to resource.parameterList.count - 1 Do
    index(context, frtOperationDefinition, key, 0, resource.parameterList[i].profile, CODES_TSearchParamsOperationDefinition[{$IFDEF FHIR2}spOperationDefinition_profile{$ELSE}spOperationDefinition_Paramprofile{$ENDIF}]);
end;

procedure TFhirIndexManager.buildIndexValuesReferralRequest(key : integer; id : String; context : TFhirResource; resource: TFhirReferralRequest);
var
  i : integer;
begin
  patientCompartment(key, resource.patient);
  index(context, frtReferralRequest, key, 0, resource.patient, CODES_TSearchParamsReferralRequest[spReferralRequest_patient]);
  index(frtReferralRequest, key, 0, resource.statusElement, CODES_TSearchParamsReferralRequest[spReferralRequest_status]);
  index(frtReferralRequest, key, 0, resource.priority, CODES_TSearchParamsReferralRequest[spReferralRequest_priority]);
  for i := 0 to resource.recipientList.Count - 1 do
    index(context, frtReferralRequest, key, 0, resource.recipientList[i], CODES_TSearchParamsReferralRequest[spReferralRequest_recipient]);
  index(context, frtReferralRequest, key, 0, resource.requester, CODES_TSearchParamsReferralRequest[spReferralRequest_requester]);
  index(frtReferralRequest, key, 0, resource.specialty, CODES_TSearchParamsReferralRequest[spReferralRequest_specialty]);
  index(frtReferralRequest, key, 0, resource.type_, CODES_TSearchParamsReferralRequest[spReferralRequest_type]);

  index(frtReferralRequest, key, 0, resource.dateElement, CODES_TSearchParamsReferralRequest[spReferralRequest_date]);
end;

procedure TFhirIndexManager.buildIndexValuesNutritionOrder(key : integer; id : String; context : TFhirResource; resource: TFhirNutritionOrder);
var
  item : TFhirNutritionOrderSupplement;
begin
  patientCompartment(key, resource.patient);
  index(context, frtNutritionOrder, key, 0, resource.patient, CODES_TSearchParamsNutritionOrder[spNutritionOrder_patient]);
  index(context, frtNutritionOrder, key, 0, resource.orderer, CODES_TSearchParamsNutritionOrder[spNutritionOrder_provider]);
  index(frtNutritionOrder, key, 0, resource.statusElement, CODES_TSearchParamsNutritionOrder[spNutritionOrder_status]);
  index(context, frtNutritionOrder, key, 0, resource.encounter, CODES_TSearchParamsNutritionOrder[spNutritionOrder_encounter]);
  index(frtNutritionOrder, key, 0, resource.identifierList, CODES_TSearchParamsNutritionOrder[spNutritionOrder_identifier]);
  index(frtNutritionOrder, key, 0, resource.dateTimeElement, CODES_TSearchParamsNutritionOrder[spNutritionOrder_datetime]);

  if (resource.enteralFormula <> nil) then
  begin
    index(frtNutritionOrder, key, 0, resource.enteralFormula.additiveTypeElement, CODES_TSearchParamsNutritionOrder[spNutritionOrder_additive]);
    index(frtNutritionOrder, key, 0, resource.enteralFormula.baseFormulaType, CODES_TSearchParamsNutritionOrder[spNutritionOrder_formula]);

  end;
  if (resource.oralDiet <> nil) then
    index(frtNutritionOrder, key, 0, resource.oralDiet.type_List, CODES_TSearchParamsNutritionOrder[spNutritionOrder_oraldiet]);
  for item in resource.supplementList do
    index(frtNutritionOrder, key, 0, item.type_, CODES_TSearchParamsNutritionOrder[spNutritionOrder_supplement]);
end;


procedure TFhirIndexManager.buildIndexValuesBodySite(key: integer; id : String; context : TFhirResource; resource: TFhirBodySite);
begin
  index(frtBodySite, key, 0, resource.codeElement, CODES_TSearchParamsBodySite[spBodySite_code]);
  index(frtBodySite, key, 0, resource.identifierList, CODES_TSearchParamsBodySite[spBodySite_identifier]);
  index(context, frtBodySite, key, 0, resource.patient, CODES_TSearchParamsBodySite[spBodySite_patient]);
end;

procedure TFhirIndexManager.buildIndexValuesClinicalImpression(key: integer; id : String; context : TFhirResource; resource: TFhirClinicalImpression);
var
  i, j : integer;
begin
  index(context, frtClinicalImpression, key, 0, resource.patient, CODES_TSearchParamsClinicalImpression[spClinicalImpression_patient]);
  patientCompartment(key, resource.patient);
  practitionerCompartment(key, resource.assessor);
  if (resource.trigger is TFhirCodeableConcept) then
    index(frtClinicalImpression, key, 0, resource.trigger as TFhirCodeableConcept, CODES_TSearchParamsClinicalImpression[spClinicalImpression_triggercode])
  else
    index(context, frtClinicalImpression, key, 0, resource.trigger as TFhirReference, CODES_TSearchParamsClinicalImpression[spClinicalImpression_trigger]);

  index(context, frtClinicalImpression, key, 0, resource.previous, CODES_TSearchParamsClinicalImpression[spClinicalImpression_previous]);
  index(frtClinicalImpression, key, 0, resource.dateElement, CODES_TSearchParamsClinicalImpression[spClinicalImpression_date]);
  index(frtClinicalImpression, key, 0, resource.statusElement, CODES_TSearchParamsClinicalImpression[spClinicalImpression_status]);
  for i := 0 to resource.problemList.Count - 1 do
    index(context, frtClinicalImpression, key, 0, resource.problemList[i], CODES_TSearchParamsClinicalImpression[spClinicalImpression_problem]);
  for i := 0 to resource.investigationsList.Count - 1 do
    for j := 0 to resource.investigationsList[i].itemList.Count - 1 do
      index(context, frtClinicalImpression, key, 0, resource.investigationsList[i].itemList[j], CODES_TSearchParamsClinicalImpression[spClinicalImpression_investigation]);
  for i := 0 to resource.findingList.Count - 1 do
    index(frtClinicalImpression, key, 0, resource.findingList[i].item, CODES_TSearchParamsClinicalImpression[spClinicalImpression_finding]);
  index(context, frtClinicalImpression, key, 0, resource.assessor, CODES_TSearchParamsClinicalImpression[spClinicalImpression_assessor]);
  for i := 0 to resource.actionList.Count - 1 do
    index(context, frtClinicalImpression, key, 0, resource.actionList[i], CODES_TSearchParamsClinicalImpression[spClinicalImpression_action]);
  index(frtClinicalImpression, key, 0, resource.resolvedList, CODES_TSearchParamsClinicalImpression[spClinicalImpression_resolved]);
  for i := 0 to resource.ruledOutList.Count - 1 do
    index(frtClinicalImpression, key, 0, resource.ruledOutList[i].item, CODES_TSearchParamsClinicalImpression[spClinicalImpression_ruledout]);
  if (resource.triggerElement is TFhirCodeableConcept) then
    index(frtClinicalImpression, key, 0, resource.triggerElement as TFhirCodeableConcept, CODES_TSearchParamsClinicalImpression[spClinicalImpression_triggercode])
  else
    index(context, frtClinicalImpression, key, 0, resource.triggerElement as TFhirReference, CODES_TSearchParamsClinicalImpression[spClinicalImpression_trigger]);
end;

procedure TFhirIndexManager.buildIndexValuesCommunication(key: integer; id : String; context : TFhirResource; resource: TFhirCommunication);
var
  i, j : integer;
begin
  index(context, frtCommunication, key, 0, resource.subject, CODES_TSearchParamsCommunication[spCommunication_patient]);
  index(context, frtCommunication, key, 0, resource.subject, CODES_TSearchParamsCommunication[spCommunication_subject]);
  patientCompartment(key, resource.subject);
  encounterCompartment(key, resource.encounter);
  index(frtCommunication, key, 0, resource.category, CODES_TSearchParamsCommunication[spCommunication_category]);
  index(context, frtCommunication, key, 0, resource.encounter, CODES_TSearchParamsCommunication[spCommunication_encounter]);
  index(frtCommunication, key, 0, resource.identifierList, CODES_TSearchParamsCommunication[spCommunication_identifier]);
  index(frtCommunication, key, 0, resource.mediumList, CODES_TSearchParamsCommunication[spCommunication_medium]);
  index(frtCommunication, key, 0, resource.receivedElement, CODES_TSearchParamsCommunication[spCommunication_received]);
  index(frtCommunication, key, 0, resource.sentElement, CODES_TSearchParamsCommunication[spCommunication_sent]);
  for i := 0 to resource.recipientList.count - 1 do
  begin
    index(context, frtCommunication, key, 0, resource.recipientList[i], CODES_TSearchParamsCommunication[spCommunication_recipient]);
    practitionerCompartment(key, resource.recipientList[i]);
    relatedPersonCompartment(key, resource.recipientList[i]);
    deviceCompartment(key, resource.recipientList[i]);
    patientCompartment(key, resource.recipientList[i]);
  end;
  index(context, frtCommunication, key, 0, resource.sender, CODES_TSearchParamsCommunication[spCommunication_sender]);
  index(context, frtCommunication, key, 0, resource.requestDetail, CODES_TSearchParamsCommunication[spCommunication_request]);
  practitionerCompartment(key, resource.sender);
  relatedPersonCompartment(key, resource.sender);
  deviceCompartment(key, resource.sender);
  patientCompartment(key, resource.sender);
  index(frtCommunication, key, 0, resource.statusElement, CODES_TSearchParamsCommunication[spCommunication_status]);
end;


procedure TFhirIndexManager.buildIndexValuesCommunicationRequest(key: integer; id : String; context : TFhirResource; resource: TFhirCommunicationRequest);
var
  i, j : integer;
begin
  index(context, frtCommunicationRequest, key, 0, resource.subject, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_patient]);
  index(context, frtCommunicationRequest, key, 0, resource.subject, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_subject]);
  patientCompartment(key, resource.subject);
  index(frtCommunicationRequest, key, 0, resource.category, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_category]);
  index(context, frtCommunicationRequest, key, 0, resource.encounter, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_encounter]);
  index(frtCommunicationRequest, key, 0, resource.identifierList, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_identifier]);
  index(frtCommunicationRequest, key, 0, resource.mediumList, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_medium]);
  index(frtCommunicationRequest, key, 0, resource.requestedOnElement, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_requested]);
  for i := 0 to resource.recipientList.count - 1 do
  begin
    index(context, frtCommunicationRequest, key, 0, resource.recipientList[i], CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_recipient]);
    practitionerCompartment(key, resource.recipientList[i]);
    relatedPersonCompartment(key, resource.recipientList[i]);
    deviceCompartment(key, resource.recipientList[i]);
    patientCompartment(key, resource.recipientList[i]);
  end;
  index(context, frtCommunicationRequest, key, 0, resource.sender, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_sender]);
  index(context, frtCommunicationRequest, key, 0, resource.requester, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_requester]);
  index(frtCommunicationRequest, key, 0, resource.priority, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_priority]);
  index(frtCommunicationRequest, key, 0, resource.statusElement, CODES_TSearchParamsCommunicationRequest[spCommunicationRequest_status]);
  practitionerCompartment(key, resource.sender);
  relatedPersonCompartment(key, resource.sender);
  deviceCompartment(key, resource.sender);
  patientCompartment(key, resource.sender);
end;

procedure TFhirIndexManager.buildIndexValuesDeviceComponent(key: integer; id : String; context : TFhirResource; resource: TFhirDeviceComponent);
var
  i, j : integer;
begin
  index(context, frtDeviceComponent, key, 0, resource.parent, CODES_TSearchParamsDeviceComponent[spDeviceComponent_parent]);
  index(context, frtDeviceComponent, key, 0, resource.source, CODES_TSearchParamsDeviceComponent[spDeviceComponent_source]);
  index(frtDeviceComponent, key, 0, resource.type_, CODES_TSearchParamsDeviceComponent[spDeviceComponent_type]);
  deviceCompartment(key, resource.source);
end;

procedure TFhirIndexManager.buildIndexValuesDeviceMetric(key: integer; id : String; context : TFhirResource; resource: TFhirDeviceMetric);
var
  i, j : integer;
begin
  index(context, frtDeviceMetric, key, 0, resource.parent, CODES_TSearchParamsDeviceMetric[spDeviceMetric_parent]);
  index(context, frtDeviceMetric, key, 0, resource.source, CODES_TSearchParamsDeviceMetric[spDeviceMetric_source]);
  deviceCompartment(key, resource.source);
  index(frtDeviceMetric, key, 0, resource.type_, CODES_TSearchParamsDeviceMetric[spDeviceMetric_type]);
  index(frtDeviceMetric, key, 0, resource.identifierElement, CODES_TSearchParamsDeviceMetric[spDeviceMetric_identifier]);
  index(frtDeviceMetric, key, 0, resource.categoryElement, CODES_TSearchParamsDeviceMetric[spDeviceMetric_category]);
end;

procedure TFhirIndexManager.buildIndexValuesDeviceUseRequest(key: integer; id : String; context : TFhirResource; resource: TFhirDeviceUseRequest);
var
  i, j : integer;
begin
  index(context, frtDeviceUseRequest, key, 0, resource.subject, CODES_TSearchParamsDeviceUseRequest[spDeviceUseRequest_subject]);
  index(context, frtDeviceUseRequest, key, 0, resource.subject, CODES_TSearchParamsDeviceUseRequest[spDeviceUseRequest_patient]);
  patientCompartment(key, resource.subject);
  index(context, frtDeviceUseRequest, key, 0, resource.device, CODES_TSearchParamsDeviceUseRequest[spDeviceUseRequest_device]);
  deviceCompartment(key, resource.device);
end;

procedure TFhirIndexManager.buildIndexValuesDeviceUseStatement(key: integer; id : String; context : TFhirResource; resource: TFhirDeviceUseStatement);
var
  i, j : integer;
begin
  index(context, frtDeviceUseStatement, key, 0, resource.subject, CODES_TSearchParamsDeviceUseStatement[spDeviceUseStatement_subject]);
  index(context, frtDeviceUseStatement, key, 0, resource.subject, CODES_TSearchParamsDeviceUseStatement[spDeviceUseStatement_patient]);
  patientCompartment(key, resource.subject);
  index(context, frtDeviceUseStatement, key, 0, resource.device, CODES_TSearchParamsDeviceUseStatement[spDeviceUseStatement_device]);
  deviceCompartment(key, resource.device);
end;

procedure TFhirIndexManager.buildIndexValuesEpisodeOfCare(key: integer; id : String; context : TFhirResource; resource: TFhirEpisodeOfCare);
var
  i : integer;
begin
  index(frtEpisodeOfCare, key, 0, resource.identifierList, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_identifier]);
  index(context, frtEpisodeOfCare, key, 0, resource.managingOrganization, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_organization]);
  index(context, frtEpisodeOfCare, key, 0, resource.patient, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_patient]);
  index(context, frtEpisodeOfCare, key, 0, resource.referralRequestList, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_incomingreferral]);
  index(context, frtEpisodeOfCare, key, 0, resource.careManager, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_caremanager]);
  patientCompartment(key, resource.patient);
  index(frtEpisodeOfCare, key, 0, resource.period, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_date]);
  index(frtEpisodeOfCare, key, 0, resource.type_List, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_type]);
  index(frtEpisodeOfCare, key, 0, resource.statusElement, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_status]);
  for i := 0 to resource.conditionList.Count - 1 do
    index(context, frtEpisodeOfCare, key, 0, resource.conditionList[i], CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_condition]);
  for i := 0 to resource.careTeamList.Count - 1 do
    index(context, frtEpisodeOfCare, key, 0, resource.careTeamList[i].member, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_teammember]);
end;

procedure TFhirIndexManager.buildIndexValuesGoal(key: integer; id : String; context : TFhirResource; resource: TFhirGoal);
begin
  index(frtGoal, key, 0, resource.identifierList, CODES_TSearchParamsGoal[spGoal_identifier]);
  index(frtGoal, key, 0, resource.statusElement, CODES_TSearchParamsGoal[spGoal_status]);
  index(context, frtGoal, key, 0, resource.subject, CODES_TSearchParamsGoal[spGoal_subject]);
  index(context, frtGoal, key, 0, resource.subject, CODES_TSearchParamsGoal[spGoal_patient], frtPatient);
  patientCompartment(key, resource.subject);
end;

procedure TFhirIndexManager.buildIndexValuesImagingObjectSelection(key: integer; id : String; context : TFhirResource; resource: TFhirImagingObjectSelection);
var
  i : integer;
begin
  index(context, frtImagingObjectSelection, key, 0, resource.patient, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_patient]);
  patientCompartment(key, resource.patient);
  index(context, frtImagingObjectSelection, key, 0, resource.author, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_author]);
  index(frtImagingObjectSelection, key, 0, resource.authoringTimeElement, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_authoringtime]);
  index(frtImagingObjectSelection, key, 0, resource.uidElement, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_identifier]);
  index(frtImagingObjectSelection, key, 0, resource.title, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_title]);
  for i := 0 to resource.studyList.Count - 1 do
    index(frtImagingObjectSelection, key, 0, resource.studyList[i].uid, CODES_TSearchParamsImagingObjectSelection[spImagingObjectSelection_selectedstudy]);
end;


procedure TFhirIndexManager.buildIndexValuesPerson(key: integer; id : String; context : TFhirResource; resource: TFhirPerson);
var
  i, j : integer;
begin
  for i := 0 to resource.addressList.Count - 1 do
    index(frtPerson, key, 0, resource.addressList[i], CODES_TSearchParamsPerson[spPerson_address]);
  index(frtPerson, key, 0, resource.identifierList, CODES_TSearchParamsPerson[spPerson_identifier]);
  index(frtPerson, key, 0, resource.birthDateElement, CODES_TSearchParamsPerson[spPerson_birthdate]);
  index(frtPerson, key, 0, resource.genderElement, CODES_TSearchParamsPerson[spPerson_gender]);
  for i := 0 to resource.nameList.count - 1 do
  begin
    index(frtPerson, key, 0, resource.nameList[i], 'name', CODES_TSearchParamsPerson[spPerson_phonetic]);
//    for j := 0 to resource.nameList[i].givenList.count - 1 do
//      index(frtPerson, key, 0, resource.nameList[i].givenList[j], CODES_TSearchParamsPerson[spPerson_given]);
//    for j := 0 to resource.nameList[i].familyList.count - 1 do
//      index(frtPerson, key, 0, resource.nameList[i].familyList[j], CODES_TSearchParamsPerson[spPerson_family]);
  end;
  index(context, frtPerson, key, 0, resource.managingOrganization, CODES_TSearchParamsPerson[spPerson_organization]);
  for i := 0 to resource.telecomList.Count - 1 do
  begin
    index(frtPerson, key, 0, resource.telecomList[i], CODES_TSearchParamsPerson[spPerson_telecom]);
    if (resource.telecomList[i].system = ContactPointSystemPhone) then
      index(frtPerson, key, 0, resource.telecomList[i].valueElement, CODES_TSearchParamsPerson[spPerson_phone]);
    if (resource.telecomList[i].system = ContactPointSystemEmail) then
      index(frtPerson, key, 0, resource.telecomList[i].valueElement, CODES_TSearchParamsPerson[spPerson_email]);
  end;
  for i := 0 to resource.link_List.Count - 1 do
  begin
    index(context, frtPerson, key, 0, resource.link_List[i].target, CODES_TSearchParamsPerson[spPerson_link]);
    index(context, frtPerson, key, 0, resource.link_List[i].target, CODES_TSearchParamsPerson[spPerson_patient], frtPatient);
    index(context, frtPerson, key, 0, resource.link_List[i].target, CODES_TSearchParamsPerson[spPerson_practitioner], frtPractitioner);
    index(context, frtPerson, key, 0, resource.link_List[i].target, CODES_TSearchParamsPerson[spPerson_relatedperson], frtRelatedPerson);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesProcedureRequest(key: integer; id : String; context : TFhirResource; resource: TFhirProcedureRequest);
var
  i : integer;
begin
  index(context, frtProcedureRequest, key, 0, resource.subject, CODES_TSearchParamsProcedureRequest[spProcedureRequest_subject]);
  index(context, frtProcedureRequest, key, 0, resource.subject, CODES_TSearchParamsProcedureRequest[spProcedureRequest_patient]);
  patientCompartment(key, resource.subject);
  index(context, frtProcedureRequest, key, 0, resource.encounter, CODES_TSearchParamsProcedureRequest[spProcedureRequest_encounter]);
  index(context, frtProcedureRequest, key, 0, resource.orderer, CODES_TSearchParamsProcedureRequest[spProcedureRequest_orderer]);
  index(context, frtProcedureRequest, key, 0, resource.performer, CODES_TSearchParamsProcedureRequest[spProcedureRequest_performer]);
  index(frtProcedureRequest, key, 0, resource.identifierList, CODES_TSearchParamsProcedureRequest[spProcedureRequest_identifier]);
end;


procedure TFhirIndexManager.buildIndexValuesSearchParameter(key: integer; id : String; context : TFhirResource; resource: TFhirSearchParameter);
var
  i : integer;
begin
  index(frtSearchParameter, key, 0, resource.baseElement, CODES_TSearchParamsSearchParameter[spSearchParameter_base]);
  index(frtSearchParameter, key, 0, resource.description, CODES_TSearchParamsSearchParameter[spSearchParameter_description]);
  index(frtSearchParameter, key, 0, resource.name, CODES_TSearchParamsSearchParameter[spSearchParameter_name]);
  index(frtSearchParameter, key, 0, resource.code, CODES_TSearchParamsSearchParameter[spSearchParameter_code]);
  for i := 0 to resource.targetList.count - 1  do
    index(frtSearchParameter, key, 0, resource.targetList[i], CODES_TSearchParamsSearchParameter[spSearchParameter_target]);
  index(frtSearchParameter, key, 0, resource.type_Element, CODES_TSearchParamsSearchParameter[spSearchParameter_type]);
  index(frtSearchParameter, key, 0, resource.url, CODES_TSearchParamsSearchParameter[spSearchParameter_url]);
end;


procedure TFhirIndexManager.buildIndexValuesVisionPrescription(key: integer; id : String; context : TFhirResource; resource: TFhirVisionPrescription);
var
  i : integer;
begin
  index(frtVisionPrescription, key, 0, resource.identifierList, CODES_TSearchParamsVisionPrescription[spVisionPrescription_identifier]);
  index(context, frtVisionPrescription, key, 0, resource.patient, CODES_TSearchParamsVisionPrescription[spVisionPrescription_patient]);
  patientCompartment(key, resource.patient);
  index(frtVisionPrescription, key, 0, resource.dateWrittenElement, CODES_TSearchParamsVisionPrescription[spVisionPrescription_dateWritten]);
  index(context, frtVisionPrescription, key, 0, resource.encounter, CODES_TSearchParamsVisionPrescription[spVisionPrescription_encounter]);
  index(context, frtVisionPrescription, key, 0, resource.prescriber, CODES_TSearchParamsVisionPrescription[spVisionPrescription_prescriber]);
end;

procedure TFhirIndexManager.buildIndexValuesProcessRequest(key: integer; id : String; context : TFhirResource; resource: TFhirProcessRequest);
var
  i : integer;
begin
  index(frtProcessRequest, key, 0, resource.identifierList, CODES_TSearchParamsProcessRequest[spProcessRequest_identifier]);
  index(frtProcessRequest, key, 0, resource.actionElement, CODES_TSearchParamsProcessRequest[spProcessRequest_action]);
  index(context, frtProcessRequest, key, 0, resource.organization, CODES_TSearchParamsProcessRequest[spProcessRequest_Organization]);
  index(context, frtProcessRequest, key, 0, resource.provider, CODES_TSearchParamsProcessRequest[spProcessRequest_Provider]);
end;

procedure TFhirIndexManager.buildIndexValuesProcessResponse(key: integer; id : String; context : TFhirResource; resource: TFhirProcessResponse);
var
  i : integer;
begin
  index(frtProcessResponse, key, 0, resource.identifierList, CODES_TSearchParamsProcessResponse[spProcessResponse_identifier]);

  index(context, frtProcessResponse, key, 0, resource.request, CODES_TSearchParamsProcessResponse[spProcessResponse_Request]);
  index(context, frtProcessResponse, key, 0, resource.organization, CODES_TSearchParamsProcessResponse[spProcessResponse_Organization]);
  index(context, frtProcessResponse, key, 0, resource.requestOrganization, CODES_TSearchParamsProcessResponse[spProcessResponse_Requestorganization]);
  index(context, frtProcessResponse, key, 0, resource.requestProvider, CODES_TSearchParamsProcessResponse[spProcessResponse_Requestprovider]);
end;


procedure TFhirIndexManager.buildIndexValuesAccount(key: integer; id : String; context : TFhirResource; resource: TFhirAccount);
var
  i : integer;
begin
  index(frtAccount, key, 0, resource.identifierList, CODES_TSearchParamsAccount[spAccount_identifier]);
  index(frtAccount, key, 0, resource.balance, CODES_TSearchParamsAccount[spAccount_balance]);
  index(frtAccount, key, 0, resource.name, CODES_TSearchParamsAccount[spAccount_name]);
  index(context, frtAccount, key, 0, resource.owner, CODES_TSearchParamsAccount[spAccount_owner]);
  index(context, frtAccount, key, 0, resource.owner, CODES_TSearchParamsAccount[spAccount_subject]);
  index(context, frtAccount, key, 0, resource.owner, CODES_TSearchParamsAccount[spAccount_patient], frtPatient);
  index(frtAccount, key, 0, resource.type_, CODES_TSearchParamsAccount[spAccount_type]);
  index(frtAccount, key, 0, resource.activePeriod, CODES_TSearchParamsAccount[spAccount_period]);
  index(frtAccount, key, 0, resource.statusElement, CODES_TSearchParamsAccount[spAccount_status]);
end;


procedure TFhirIndexManager.buildIndexValuesImplementationGuide(key: integer; id : String; context : TFhirResource; resource: TFhirImplementationGuide);
var
  p : TFhirImplementationGuidePackage;
  r : TFhirImplementationGuidePackageResource;
  i : integer;
begin
  index(frtImplementationGuide, key, 0, resource.useContextList, CODES_TSearchParamsImplementationGuide[spImplementationGuide_context]);
  index(frtImplementationGuide, key, 0, resource.dateElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_date]);
  for i := 0 to resource.dependencyList.Count - 1 do
    index(frtImplementationGuide, key, 0, resource.dependencyList[i].uriElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_dependency]);
  index(frtImplementationGuide, key, 0, resource.descriptionElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_description]);
  index(frtImplementationGuide, key, 0, resource.experimentalElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_experimental]);
  index(frtImplementationGuide, key, 0, resource.nameElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_name]);
  index(frtImplementationGuide, key, 0, resource.publisherElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_publisher]);
  index(frtImplementationGuide, key, 0, resource.statusElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_status]);
  index(frtImplementationGuide, key, 0, resource.urlElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_url]);
  index(frtImplementationGuide, key, 0, resource.versionElement, CODES_TSearchParamsImplementationGuide[spImplementationGuide_version]);
end;


{$ENDIF}

{ TFhirIndexEntry }

function TFhirIndexEntry.Link: TFhirIndexEntry;
begin
  result := TFhirIndexEntry(inherited Link);
end;

{ TFhirCompartmentEntry }

function TFhirCompartmentEntry.link: TFhirCompartmentEntry;
begin
  result := TFhirCompartmentEntry(inherited Link);
end;

initialization
  TFHIRIndexInformation.Create.free;
end.



