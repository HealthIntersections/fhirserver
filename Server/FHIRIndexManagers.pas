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


}
uses
  SysUtils, Classes, Generics.Collections,
  AdvObjects, AdvObjectLists, AdvNames, AdvXmlBuilders, AdvGenerics,
  EncodeSupport, DecimalSupport, HL7v2dateSupport, StringSupport, GuidSupport,
  KDBManager,
  FHIRBase, FhirSupport, FHIRResources, FHIRIndexConstants, FHIRConstants, FHIRTypes, FHIRTags, FHIRUtilities, FHIRParser, FHIRPath, FHIRProfileUtilities,
  TerminologyServer,
  UcumServices;

Const
  INDEX_ENTRY_LENGTH = 128;
  NARRATIVE_INDEX_NAME = '_text';

Type
  TKeyType = (ktResource, ktEntries, ktCompartment);

  TFHIRGetNextKey = function (keytype : TKeyType; aType : TFhirResourceType; var id : string) : Integer of Object;

  TFhirIndex = class (TAdvObject)
  private
    FResourceType : TFhirResourceType;
    FKey: Integer;
    FName: String;
    FDescription : String;
    FSearchType: TFhirSearchParamTypeEnum;
    FTargetTypes : TFhirResourceTypeSet;
    FURI: String;
    FPath : String;
    FUsage : TFhirSearchXpathUsageEnum;
  public
    function Link : TFhirIndex; Overload;
    function Clone : TFhirIndex; Overload;
    procedure Assign(source : TAdvObject); Override;

    property ResourceType : TFhirResourceType read FResourceType write FResourceType;
    property Name : String read FName write FName;
    Property Description : String read FDescription write FDescription;
    Property Key : Integer read FKey write FKey;
    Property SearchType : TFhirSearchParamTypeEnum read FSearchType write FSearchType;
    Property TargetTypes : TFhirResourceTypeSet read FTargetTypes write FTargetTypes;
    Property URI : String read FURI write FURI;
    Property Path : String read FPath;
    Property Usage : TFhirSearchXpathUsageEnum read FUsage;
  end;

  TFhirIndexList = class (TAdvObjectList)
  private
    function GetItemN(iIndex: integer): TFhirIndex;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    function Link : TFhirIndexList; Overload;

    function getByName(atype : TFhirResourceType; name : String): TFhirIndex;
    procedure add(aResourceType : TFhirResourceType; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : TFhirResourceTypeSet; path : String; usage : TFhirSearchXpathUsageEnum); overload;
    procedure add(aResourceType : TFhirResourceType; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : TFhirResourceTypeSet; path : String; usage : TFhirSearchXpathUsageEnum; url : String); overload;
    Property Item[iIndex : integer] : TFhirIndex read GetItemN; default;
  end;

  TFhirComposite = class (TAdvObject)
  private
    FResourceType : TFhirResourceType;
    FKey: Integer;
    FName: String;
    FComponents : TDictionary<String, String>;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Link : TFhirComposite; Overload;
    function Clone : TFhirComposite; Overload;
    procedure Assign(source : TAdvObject); Override;

    property ResourceType : TFhirResourceType read FResourceType write FResourceType;
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

    function getByName(atype : TFhirResourceType; name : String): TFhirComposite;
    procedure add(aResourceType : TFhirResourceType; name : String; components : array of String); overload;
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
  public
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
  end;

  TFhirIndexEntryList = class (TAdvObjectList)
  private
    FKeyEvent : TFHIRGetNextKey;
    function GetItemN(iIndex: integer): TFhirIndexEntry;
    procedure filter(indexes : TFhirIndexList; name : String; list : TAdvList<TFhirIndexEntry>);
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    function add(key, parent : integer; index : TFhirIndex; ref : integer; value1, value2 : String; target : integer; type_ : TFhirSearchParamTypeEnum; flag : boolean = false; concept : integer = 0) : integer; overload;
    function add(key, parent : integer; index : TFhirComposite) : integer; overload;
    Property Item[iIndexEntry : integer] : TFhirIndexEntry read GetItemN; default;
    property KeyEvent : TFHIRGetNextKey read FKeyEvent write FKeyEvent;
  end;

  TFhirCompartmentEntry = class (TAdvObject)
  private
    FCKey: integer;
    FKey: integer;
    FId: string;
  public
    property Key : integer read FKey write FKey;
    property CKey : integer read FCKey write FCKey;
    property Id : string read FId write FId;
  end;

  TFhirCompartmentEntryList = class (TAdvObjectList)
  private
    function GetItemN(iIndex: integer): TFhirCompartmentEntry;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    procedure add(key, ckey : integer; id : string);
    procedure removeById(id : String);
    Property Item[iCompartmentEntry : integer] : TFhirCompartmentEntry read GetItemN; default;
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
    FNarrativeIndex : Integer;
    procedure buildIndexes;
    procedure buildIndexesBundle;
    procedure buildIndexesAllergyIntolerance;
    procedure buildIndexesCarePlan;
    procedure buildIndexesConformance;
    procedure buildIndexesDevice;
    procedure buildIndexesDiagnosticReport;
    procedure buildIndexesDiagnosticOrder;
    procedure buildIndexesComposition;
    procedure buildIndexesDocumentReference;
    procedure buildIndexesDocumentManifest;
    procedure buildIndexesEncounter;
    procedure buildIndexesGroup;
    procedure buildIndexesImagingStudy;
    procedure buildIndexesImmunization;
    procedure buildIndexesImmunizationRecommendation;
    procedure buildIndexesOperationOutcome;
    procedure buildIndexesList;
    procedure buildIndexesLocation;
    procedure buildIndexesMedication;
    procedure buildIndexesMedicationAdministration;
    procedure buildIndexesMedicationOrder;
    procedure buildIndexesMedicationDispense;
    procedure buildIndexesMedicationStatement;
    procedure buildIndexesMessageHeader;
    procedure buildIndexesObservation;
    procedure buildIndexesOrder;
    procedure buildIndexesOrderResponse;
    procedure buildIndexesOrganization;
    procedure buildIndexesPatient;
    procedure buildIndexesMedia;
    procedure buildIndexesProcedure;
    procedure buildIndexesCondition;
    procedure buildIndexesProvenance;
    procedure buildIndexesPractitioner;
    procedure buildIndexesQuestionnaire;
    procedure buildIndexesValueSet;
    procedure BuildIndexesConceptMap;
    procedure buildIndexesSpecimen;
    procedure buildIndexesSubstance;
    procedure buildIndexesBinary;
    procedure BuildIndexesRelatedPerson;
    procedure BuildIndexesSupplyDelivery;
    procedure BuildIndexesSupplyRequest;
    procedure buildIndexesAuditEvent;
    procedure buildIndexesStructureDefinition;
    procedure buildIndexesFlag;
    procedure buildIndexesFamilyMemberHistory;
    procedure BuildIndexesCoverage;
    procedure BuildIndexesClaimResponse;
    procedure BuildIndexesContract;
    procedure BuildIndexesClaim;
    procedure BuildIndexesBasic;
    procedure buildIndexesQuestionnaireResponse;
    procedure BuildIndexesSlot;
    procedure BuildIndexesAppointment;
    procedure BuildIndexesSchedule;
    procedure BuildIndexesAppointmentResponse;
    procedure BuildIndexesHealthcareService;
    procedure BuildIndexesDataElement;
    procedure BuildIndexesTestScript;
    procedure BuildIndexesNamingSystem;
    procedure BuildIndexesSubscription;
    procedure BuildIndexesDetectedIssue;
    procedure BuildIndexesRiskAssessment;
    procedure BuildIndexesOperationDefinition;
    procedure BuildIndexesReferralRequest;
    procedure BuildIndexesNutritionOrder;
    procedure buildIndexesBodySite;
    procedure buildIndexesClinicalImpression;
    procedure buildIndexesCommunication;
    procedure buildIndexesCommunicationRequest;
    procedure buildIndexesDeviceComponent;
    procedure buildIndexesDeviceMetric;
    procedure buildIndexesDeviceUseRequest;
    procedure buildIndexesDeviceUseStatement;
    procedure buildIndexesEligibilityRequest;
    procedure buildIndexesEligibilityResponse;
    procedure buildIndexesEnrollmentRequest;
    procedure buildIndexesEnrollmentResponse;
    procedure buildIndexesEpisodeOfCare;
    procedure buildIndexesExplanationOfBenefit;
    procedure buildIndexesGoal;
    procedure buildIndexesImagingObjectSelection;
    procedure buildIndexesPaymentNotice;
    procedure buildIndexesPerson;
    procedure buildIndexesProcedureRequest;
    procedure buildIndexesSearchParameter;
    procedure buildIndexesVisionPrescription;
    procedure buildIndexesProcessRequest;
    procedure buildIndexesProcessResponse;
    procedure buildIndexesPaymentReconciliation;
    procedure buildIndexesAccount;
    procedure buildIndexesImplementationGuide;
    {$IFDEF FHIR_DSTU3}
    procedure buildIndexesCodeSystem;
    procedure buildIndexesLinkage;
    procedure BuildIndexesDecisionSupportRule;
    procedure BuildIndexesDecisionSupportServiceModule;
    procedure BuildIndexesExpansionProfile;
    procedure BuildIndexesGuidanceResponse;
    procedure BuildIndexesLibrary;
    procedure BuildIndexesMeasure;
    procedure BuildIndexesModuleDefinition;
    procedure BuildIndexesOrderSet;
    procedure BuildIndexesProtocol;
    procedure BuildIndexesSequence;
    procedure BuildIndexesCompartmentDefinition;
    procedure BuildIndexesMeasureReport;
    procedure BuildIndexesCareTeam;
    procedure BuildIndexesTask;
    procedure BuildIndexesStructureMap;
    procedure BuildIndexesPractitionerRole;
    procedure BuildIndexesImagingExcerpt;
    {$ENDIF}

  public
    constructor Create; Override;
    destructor Destroy; override;
    function Link : TFHIRIndexInformation; overload;
    procedure ReconcileIndexes(connection : TKDBConnection);

    Function GetTargetsByName(types : TFhirResourceTypeSet; name : String) : TFhirResourceTypeSet;
    Function GetKeyByName(name : String) : integer;
    Function GetTypeByName(types : TFhirResourceTypeSet; name : String) : TFhirSearchParamTypeEnum;
    Function GetComposite(types : TFhirResourceTypeSet; name : String; var otypes : TFhirResourceTypeSet) : TFhirComposite;

    property Indexes : TFhirIndexList read FIndexes;
    property Composites : TFhirCompositeList read FComposites;
    Property NarrativeIndex : integer read FNarrativeIndex;
  end;

  {$HINTS OFF}
  TFhirIndexManager = class (TAdvObject)
  private
    FInfo : TFHIRIndexInformation;
    FKeyEvent : TFHIRGetNextKey;
    FSpaces : TFhirIndexSpaces;
    FPatientCompartments : TFhirCompartmentEntryList;
    FEntries : TFhirIndexEntryList;
    FManualEntries : TFhirIndexEntryList;
    FPathEntries : TFhirIndexEntryList;
    FMasterKey : Integer;
    FBases : TStringList;
    FValidationInfo : TValidatorServiceProvider;
    FTerminologyServer : TTerminologyServer;

    procedure GetBoundaries(value : String; comparator: TFhirQuantityComparatorEnum; var low, high : String);

    function EncodeXhtml(r : TFhirDomainResource) : TBytes;

    procedure buildIndexValues(key : integer; id : String; context, resource : TFhirResource);

    procedure patientCompartment(key : integer; reference : TFhirReference); overload;
    procedure patientCompartmentNot(key : integer; type_, id : String); overload;
    procedure patientCompartment(key : integer; type_, id : String); overload;

    procedure practitionerCompartment(key : integer; reference : TFhirReference); overload;
    procedure practitionerCompartmentNot(key : integer; type_, id : String); overload;
    procedure practitionerCompartment(key : integer; type_, id : String); overload;

    procedure deviceCompartment(key : integer; reference : TFhirReference); overload;
    procedure deviceCompartmentNot(key : integer; type_, id : String); overload;
    procedure deviceCompartment(key : integer; type_, id : String); overload;

    procedure relatedPersonCompartment(key : integer; reference : TFhirReference); overload;
    procedure relatedPersonCompartmentNot(key : integer; type_, id : String); overload;
    procedure relatedPersonCompartment(key : integer; type_, id : String); overload;

    procedure encounterCompartment(key : integer; reference : TFhirReference); overload;
    procedure encounterCompartmentNot(key : integer; type_, id : String); overload;
    procedure encounterCompartment(key : integer; type_, id : String); overload;

    // very primitives
    procedure index(aType : TFhirResourceType; key, parent : integer; value1, value2, name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value, name : String); overload;
    procedure index2(aType : TFhirResourceType; key, parent : integer; value, name : String); overload;

    // primitives
    procedure index(aType : TFhirResourceType; key, parent : integer; value : Boolean; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirString; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirUri; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirEnum; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirInteger; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirBoolean; name : String); overload;

    // intervals of time
    procedure index(aType : TFhirResourceType; key, parent : integer; min, max : TDateTime; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirInstant; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirDateTime; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirDate; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirPeriod; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirTiming; name : String); overload;

    // complexes
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirRatio; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirQuantity; name : String; units : string = ''); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirRange; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirSampledData; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirCoding; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirCodingList; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirCodeableConcept; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirCodeableConceptList; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirIdentifier; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirIdentifierList; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirHumanName; name, phoneticName : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirAddress; name : String); overload;
    procedure index(aType : TFhirResourceType; key, parent : integer; value : TFhirContactPoint; name : String); overload;
    procedure index(context : TFhirResource; aType : TFhirResourceType; key, parent : integer; value : TFhirReference; name : String; specificType : TFhirResourceType = frtNull); overload;
    procedure index(context : TFhirResource; aType : TFhirResourceType; key, parent : integer; value : TFhirReferenceList; name : String; specificType : TFhirResourceType = frtNull); overload;

    // structure holder
    function index(aType : TFhirResourceType; key, parent : integer; name : String) : Integer; overload;

    { resource functionality }
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
    {$IFDEF FHIR_DSTU3}
    procedure BuildIndexValuesCodeSystem(key : integer; id : string; context : TFhirResource; resource : TFhirCodeSystem);
    procedure BuildIndexValuesLinkage(key : integer; id : string; context : TFhirResource; resource : TFhirLinkage);
    procedure buildIndexValuesDecisionSupportRule(key : integer; id : string; context : TFhirResource; resource : TFhirDecisionSupportRule);
    procedure BuildIndexValuesDecisionSupportServiceModule(key : integer; id : string; context : TFhirResource; resource : TFhirDecisionSupportServiceModule);
    procedure BuildIndexValuesExpansionProfile(key : integer; id : string; context : TFhirResource; resource : TFhirExpansionProfile);
    procedure BuildIndexValuesGuidanceResponse(key : integer; id : string; context : TFhirResource; resource : TFhirGuidanceResponse);
    procedure BuildIndexValuesLibrary(key : integer; id : string; context : TFhirResource; resource : TFhirLibrary);
    procedure BuildIndexValuesMeasure(key : integer; id : string; context : TFhirResource; resource : TFhirMeasure);
    procedure BuildIndexValuesModuleDefinition(key : integer; id : string; context : TFhirResource; resource : TFhirModuleDefinition);
    procedure BuildIndexValuesOrderSet(key : integer; id : string; context : TFhirResource; resource : TFhirOrderSet);
    procedure BuildIndexValuesProtocol(key : integer; id : string; context : TFhirResource; resource : TFhirProtocol);
    procedure BuildIndexValuesSequence(key : integer; id : string; context : TFhirResource; resource : TFhirSequence);
    procedure BuildIndexValuesCompartmentDefinition(key : integer; id : string; context : TFhirResource; resource : TFhirCompartmentDefinition);
    procedure BuildIndexValuesStructureMap(key : integer; id : string; context : TFhirResource; resource : TFhirStructureMap);
    procedure BuildIndexValuesMeasureReport(key : integer; id : string; context : TFhirResource; resource : TFhirMeasureReport);
    procedure BuildIndexValuesCareTeam(key : integer; id : string; context : TFhirResource; resource : TFhirCareTeam);
    procedure BuildIndexValuesTask(key : integer; id : string; context : TFhirResource; resource : TFhirTask);
    procedure BuildIndexValuesPractitionerRole(key : integer; id : string; context : TFhirResource; resource : TFhirPractitionerRole);
    procedure BuildIndexValuesImagingExcerpt(key : integer; id : string; context : TFhirResource; resource : TFhirImagingExcerpt);
    {$ENDIF}

    procedure BuildIndexValuesImplementationGuide(key : integer; id : string; context : TFhirResource; resource : TFhirImplementationGuide);
    procedure processCompartmentTags(key : integer; id: String; tags : TFHIRTagList);
    procedure processUnCompartmentTags(key : integer; id: String; tags : TFHIRTagList);
    procedure SetTerminologyServer(const Value: TTerminologyServer);

    procedure checkTags(resource : TFhirResource; tags : TFHIRTagList);
    procedure compareIndexEntries(rt, n, expr : string);
    procedure evaluateByFHIRPath(resource : TFhirResource);
  public
    constructor Create(aSpaces : TFhirIndexSpaces; aInfo : TFHIRIndexInformation; ValidationInfo : TValidatorServiceProvider);
    destructor Destroy; override;
    function Link : TFHIRIndexManager; overload;
    property TerminologyServer : TTerminologyServer read FTerminologyServer write SetTerminologyServer;
    property Bases : TStringList read FBases write FBases;
    function execute(key : integer; id: String; resource : TFhirResource; tags : TFHIRTagList) : String;
    property KeyEvent : TFHIRGetNextKey read FKeyEvent write FKeyEvent;
  end;

function normaliseDecimal(v : String): String;
  
implementation

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

{ TFhirIndexList }

procedure TFhirIndexList.add(aResourceType : TFhirResourceType; name, description : String; aType : TFhirSearchParamTypeEnum; aTargetTypes : TFhirResourceTypeSet; path : String; usage : TFhirSearchXpathUsageEnum);
begin
  add(aResourceType, name, description, aType, aTargetTypes, path, usage, 'http://hl7.org/fhir/SearchParameter/'+CODES_TFHIRResourceType[aResourceType]+'-'+name.Replace('[', '').Replace(']', ''));
end;


procedure TFhirIndexList.add(aResourceType: TFhirResourceType; name, description: String; aType: TFhirSearchParamTypeEnum; aTargetTypes: TFhirResourceTypeSet; path : String; usage : TFhirSearchXpathUsageEnum; url: String);
var
  ndx : TFhirIndex;
begin
  ndx := TFhirIndex.Create;
  try
    ndx.ResourceType := aResourceType;
    ndx.name := name;
    ndx.SearchType := aType;
    ndx.TargetTypes := aTargetTypes;
    ndx.URI := url;
    ndx.description := description;
    ndx.FPath := path;
    ndx.FUsage := usage;
    inherited add(ndx.Link);
  finally
    ndx.free;
  end;
end;

function TFhirIndexList.getByName(atype : TFhirResourceType; name: String): TFhirIndex;
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

function TFhirIndexEntryList.add(key, parent : integer; index: TFhirIndex; ref: integer; value1, value2: String; target : Integer; type_ : TFhirSearchParamTypeEnum; flag : boolean = false; concept : integer = 0) : integer;
var
  entry : TFhirIndexEntry;
  dummy : string;
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

  entry := TFhirIndexEntry.create;
  try
    entry.FName := index.Name;
    entry.EntryKey := KeyEvent(ktEntries, frtNull, dummy);
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
    entry.EntryKey := KeyEvent(ktEntries, frtNull, dummy);
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
    if Item[i].FName = name then
      list.Add(Item[i].Link as TFhirIndexEntry);
end;

function TFhirIndexEntryList.GetItemN(iIndex: integer): TFhirIndexEntry;
begin
  result := TFhirIndexEntry(objectByindex[iIndex]);
end;

function TFhirIndexEntryList.ItemClass: TAdvObjectClass;
begin
  result := TFhirIndexEntry;
end;

{ TFhirIndexManager }

constructor TFhirIndexManager.Create(aSpaces : TFhirIndexSpaces; aInfo : TFHIRIndexInformation; ValidationInfo : TValidatorServiceProvider);
begin
  inherited Create;
  FPatientCompartments := TFhirCompartmentEntryList.create;
  FValidationInfo := ValidationInfo;
  FSpaces := aSpaces;
  FInfo := aInfo;
  FManualEntries := TFhirIndexEntryList.Create;
  FPathEntries := TFhirIndexEntryList.Create;
end;

destructor TFhirIndexManager.Destroy;
begin
  FValidationInfo.Free;
  FTerminologyServer.free;
  FPatientCompartments.Free;
  FSpaces.Free;
  FManualEntries.Free;
  FPathEntries.Free;
  FInfo.Free;
  inherited;
end;

procedure TFhirIndexManager.deviceCompartment(key: integer;
  reference: TFhirReference);
begin

end;

procedure TFhirIndexManager.deviceCompartment(key: integer; type_, id: String);
begin

end;

procedure TFhirIndexManager.deviceCompartmentNot(key: integer; type_,
  id: String);
begin

end;

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
    {$IFDEF FHIR_DSTU3}
    frtCodeSystem : buildIndexValuesCodeSystem(key, id, context, TFhirCodeSystem(resource));
    frtLinkage : buildIndexValuesLinkage(key, id, context, TFhirLinkage(resource));
    frtDecisionSupportRule : buildIndexValuesDecisionSupportRule(key, id, context, TFhirDecisionSupportRule(resource));
    frtDecisionSupportServiceModule : buildIndexValuesDecisionSupportServiceModule(key, id, context, TFHIRDecisionSupportServiceModule(resource));
    frtExpansionProfile : buildIndexValuesExpansionProfile(key, id, context, TFHIRExpansionProfile(resource));
    frtGuidanceResponse : buildIndexValuesGuidanceResponse(key, id, context, TFHIRGuidanceResponse(resource));
    frtLibrary : buildIndexValuesLibrary(key, id, context, TFHIRLibrary(resource));
    frtMeasure : buildIndexValuesMeasure(key, id, context, TFHIRMeasure(resource));
    frtModuleDefinition : buildIndexValuesModuleDefinition(key, id, context, TFHIRModuleDefinition(resource));
    frtOrderSet : buildIndexValuesOrderSet(key, id, context, TFHIROrderSet(resource));
    frtProtocol : buildIndexValuesProtocol(key, id, context, TFHIRProtocol(resource));
    frtSequence : buildIndexValuesSequence(key, id, context, TFHIRSequence(resource));
    frtCompartmentDefinition : buildIndexValuesCompartmentDefinition(key, id, context, TFHIRCompartmentDefinition(resource));
    frtMeasureReport : buildIndexValuesMeasureReport(key, id, context, TFHIRMeasureReport(resource));
    frtCareTeam : buildIndexValuesCareTeam(key, id, context, TFHIRCareTeam(resource));
    frtTask : buildIndexValuesTask(key, id, context, TFHIRTask(resource));
    frtStructureMap : buildIndexValuesStructureMap(key, id, context, TFHIRStructureMap(resource));
    frtPractitionerRole : buildIndexValuesPractitionerRole(key, id, context, TFHIRPractitionerRole(resource));
    frtImagingExcerpt : buildIndexValuesImagingExcerpt(key, id, context, TFHIRImagingExcerpt(resource));
    {$ENDIF}

  else
    raise Exception.create('resource type indexing not implemented yet for '+CODES_TFHIRResourceType[resource.ResourceType]);
  end;
end;


function TFhirIndexManager.EncodeXhtml(r: TFhirDomainResource): TBytes;
var
  b : TBytesStream;
  x, body : TFhirXHtmlNode;
  xc : TAdvXmlBuilder;
  fc : TFHIRXmlComposer;
begin
  b :=  TBytesStream.Create;
  try
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
        xc := TAdvXmlBuilder.Create;
        try
          xc.Start;
          fc := TFHIRXmlComposer.Create('en');
          try
            fc.ComposeXHtmlNode(xc, 'html', x);
          finally
            fc.Free;
          end;
          xc.Finish;
          xc.Build(b);
        finally
          xc.Free;
        end;
      finally
        x.Free;
      end;
    end;
    result := copy(b.Bytes, 0, b.size); // don't compress, sql server has to read it.
  finally
    b.free;
  end;
end;


procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirString; name: String);
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirUri; name: String);
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirCodeableConcept; name: String);
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

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value, name: String);
var
  ndx : TFhirIndex;
  types : TFhirSearchParamTypeEnumList;

begin
  if (value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+CODES_TFHIRResourceType[aType]);

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
  FEntries.add(key, parent, ndx, 0, value, '', 0, ndx.SearchType);
end;

procedure TFhirIndexManager.index2(aType : TFhirResourceType; key, parent : integer; value, name: String);
var
  ndx : TFhirIndex;
begin
  if (value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+CODES_TFHIRResourceType[aType]);
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeReference]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing string');
  value := lowercase(RemoveAccents(copy(value, 1, INDEX_ENTRY_LENGTH)));
  FEntries.add(key, parent, ndx, 0, '', value, 0, SearchParamTypeString);
end;

function TFhirIndexManager.Link: TFHIRIndexManager;
begin
  result := TFHIRIndexManager (inherited Link);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value1, value2, name: String);
var
  ndx : TFhirIndex;
begin
  if (value1 = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+CODES_TFHIRResourceType[aType]);
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

  FEntries.add(key, parent, ndx, 0, value1, value2, 0, ndx.SearchType);
end;


procedure TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; value: Boolean; name: String);
var
  ndx : TFhirIndex;
  concept : integer;
begin
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+CODES_TFHIRResourceType[aType]);
  if not (ndx.SearchType in [SearchParamTypeToken]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' of type '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing enumeration on '+CODES_TFHIRResourceType[aType]);
  concept := TerminologyServer.enterIntoClosure(FSpaces.FDB, CODES_TFHIRResourceType[aType]+'.'+name, 'http://hl7.org/fhir/special-values', BooleanToString(value));
  assert(concept <> 0);
  FEntries.add(key, parent, ndx, 0, BooleanToString(value), '', 0, ndx.SearchType, false, concept);
end;


procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirEnum; name: String);
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
    raise Exception.create('Unknown index '+name+' on type '+CODES_TFHIRResourceType[aType]);
  if not (ndx.SearchType in [SearchParamTypeToken]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' of type '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing enumeration on '+CODES_TFHIRResourceType[aType]);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
     raise exception.create('string too long for indexing: '+value.value+ ' ('+inttostr(length(value.value))+' chars)');
  if value.system <> '' then
  begin
    concept := TerminologyServer.enterIntoClosure(FSpaces.FDB, CODES_TFHIRResourceType[aType]+'.'+name, value.system, value.value);
    assert(concept <> 0);
  end
  else
    concept := 0;

  FEntries.add(key, parent, ndx, 0, value.value, '', 0, ndx.SearchType, false, concept);
end;


procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirInstant; name: String);
begin
  if (value <> nil) and (value.value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.relatedPersonCompartment(key: integer; type_,
  id: String);
begin

end;

procedure TFhirIndexManager.relatedPersonCompartment(key: integer;
  reference: TFhirReference);
begin

end;

procedure TFhirIndexManager.relatedPersonCompartmentNot(key: integer; type_,
  id: String);
begin

end;

procedure TFhirIndexManager.SetTerminologyServer(const Value: TTerminologyServer);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

procedure TFhirIndexManager.encounterCompartment(key: integer;
  reference: TFhirReference);
begin

end;

procedure TFhirIndexManager.encounterCompartment(key: integer; type_,
  id: String);
begin

end;

procedure TFhirIndexManager.encounterCompartmentNot(key: integer; type_,
  id: String);
begin

end;

procedure TFhirIndexManager.evaluateByFHIRPath(resource: TFhirResource);
var
  path : TFHIRExpressionEngine;
  i : integer;
  ndx : TFhirIndex;
  matches : TFHIRBaseList;
  match : TFHIRBase;
begin
  path := TFHIRExpressionEngine.Create(FValidationInfo.link);
  try
    for i := 0 to FInfo.Indexes.Count - 1 do
    begin
      ndx := FInfo.Indexes[i];
      if (ndx.Path <> '') and (ndx.ResourceType = resource.ResourceType) then
      begin
        matches := path.evaluate(nil, resource, ndx.Path);
        for match in matches do
        begin
          case ndx.Usage of
            SearchXpathUsageNull: raise Exception.create('Path is not defined properly');
            SearchXpathUsageNormal:
              begin
              if match is TFhirString then
                index(resource.ResourceType, ndx.Key, 0, TFhirString(match), ndx.Name)
              else if match is TFhirUri then
                index(resource.ResourceType, ndx.key, 0, TFhirUri(match), ndx.Name)
              else if match is TFhirEnum then
                index(resource.ResourceType, ndx.key, 0, TFhirEnum(match), ndx.Name)
              else if match is TFhirInteger  then
                index(resource.ResourceType, ndx.key, 0, TFhirInteger(match), ndx.Name)
              else if match is TFhirBoolean  then
                index(resource.ResourceType, ndx.key, 0, TFhirBoolean(match), ndx.Name)
              else if match is TFhirInstant  then
                index(resource.ResourceType, ndx.key, 0, TFhirInstant(match), ndx.Name)
              else if match is TFhirDateTime  then
                index(resource.ResourceType, ndx.key, 0, TFhirDateTime(match), ndx.Name)
              else if match is TFhirDate  then
                index(resource.ResourceType, ndx.key, 0, TFhirDate(match), ndx.Name)
              else if match is TFhirPeriod  then
                index(resource.ResourceType, ndx.key, 0, TFhirPeriod(match), ndx.Name)
              else if match is TFhirTiming  then
                index(resource.ResourceType, ndx.key, 0, TFhirTiming(match), ndx.Name)
              else if match is TFhirRatio  then
                index(resource.ResourceType, ndx.key, 0, TFhirRatio(match), ndx.Name)
              else if match is TFhirQuantity  then
                index(resource.ResourceType, ndx.key, 0, TFhirQuantity(match), ndx.Name)
              else if match is TFhirRange  then
                index(resource.ResourceType, ndx.key, 0, TFhirRange(match), ndx.Name)
              else if match is TFhirSampledData  then
                index(resource.ResourceType, ndx.key, 0, TFhirSampledData(match), ndx.Name)
              else if match is TFhirCoding  then
                index(resource.ResourceType, ndx.key, 0, TFhirCoding(match), ndx.Name)
              else if match is TFhirCodeableConcept  then
                index(resource.ResourceType, ndx.key, 0, TFhirCodeableConcept(match), ndx.Name)
              else if match is TFhirIdentifier  then
                index(resource.ResourceType, ndx.key, 0, TFhirIdentifier(match), ndx.Name)
              else if match is TFhirHumanName  then
                index(resource.ResourceType, ndx.key, 0, TFhirHumanName(match), ndx.Name, '')
              else if match is TFhirAddress  then
                index(resource.ResourceType, ndx.key, 0, TFhirAddress(match), ndx.Name)
              else if match is TFhirContactPoint  then
                index(resource.ResourceType, ndx.key, 0, TFhirContactPoint(match), ndx.Name)
              else if match is TFhirReference then
                index(resource, resource.ResourceType, ndx.key, 0, TFhirReference(match), ndx.Name)
              else
                raise Exception.Create('The type '+match.FhirType+' is not supported in FIndexManager');
              end;
            SearchXpathUsagePhonetic: raise Exception.create('not supported');
            SearchXpathUsageNearby:  raise Exception.create('not supported');
            SearchXpathUsageDistance: raise Exception.create('not supported');
            SearchXpathUsageOther: raise Exception.create('not supported');
          end;
        end;
        compareIndexEntries(CODES_TFHIRResourceType[resource.ResourceType], ndx.Name, ndx.Path);
      end;
    end;
  finally
    path.Free;
  end;
end;

function TFhirIndexManager.execute(key : integer; id : String; resource : TFhirResource; tags : TFHIRTagList) : String;
var
  i : integer;
  entry : TFhirIndexEntry;
  dummy : string;
begin
  checkTags(resource, tags);
  FManualEntries.clear;
  FPathEntries.clear;
  FManualEntries.FKeyEvent := FKeyEvent;
  FPathEntries.FKeyEvent := FKeyEvent;

  FEntries := FManualEntries;

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


  FMasterKey := key;
  FSpaces.FDB.ExecSQL('delete from Compartments where ResourceKey in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('update IndexEntries set Flag = 2 where ResourceKey in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('update IndexEntries set Flag = 2 where Target in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('delete from SearchEntries where ResourceKey in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('update Ids set deleted = 1 where MasterResourceKey = '+inttostr(key));
  FPatientCompartments.Clear;

  processCompartmentTags(key, id, tags);
  buildIndexValues(key, id, resource, resource);
  processUnCompartmentTags(key, id, tags);

  if resource is TFhirDomainResource then
  begin
    FSpaces.FDB.SQL := 'insert into indexEntries (EntryKey, IndexKey, ResourceKey, Flag, Extension, Xhtml) values (:k, :i, :r, 1, ''html'', :xb)';
    FSpaces.FDB.prepare;
    FSpaces.FDB.BindInteger('k', FKeyEvent(ktEntries, frtNull, dummy));
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
  if FPatientCompartments.Count > 0 then
  begin
    FSpaces.FDB.SQL := 'insert into Compartments (ResourceCompartmentKey, ResourceKey, CompartmentType, CompartmentKey, Id) values (:pk, :r, :ct, :ck, :id)';
    FSpaces.FDB.prepare;
    for i := 0 to FPatientCompartments.Count - 1 Do
    begin
      if i > 0 then
        result := result + ', ';
      result := result + ''''+FPatientCompartments[i].id+'''';

      FSpaces.FDB.BindInteger('pk', FKeyEvent(ktCompartment, frtNull, dummy));
      FSpaces.FDB.BindInteger('r', FPatientCompartments[i].key);
      FSpaces.FDB.BindInteger('ct', 1);
      FSpaces.FDB.BindString('id', FPatientCompartments[i].id);
      if FPatientCompartments[i].ckey > 0 then
        FSpaces.FDB.BindInteger('ck', FPatientCompartments[i].ckey)
      else
        FSpaces.FDB.BindNull('ck');
      FSpaces.FDB.execute;
    end;
    FSpaces.FDB.terminate;
  end;

  FEntries := FPathEntries;
  {$IFDEF FHIR_DSTU3}
  evaluateByFHIRPath(resource);
  {$ENDIF}
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirCoding; name: String);
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
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if ndx.SearchType <> SearchParamTypeToken then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing Coding');
  if (value.system <> '') then
  begin
    ref := FSpaces.ResolveSpace(value.system);
    concept := TerminologyServer.enterIntoClosure(FSpaces.FDB, CODES_TFHIRResourceType[aType]+'.'+name, value.system, value.code);
  end
  else
  begin
    ref := 0;
    concept := 0;
  end;

  if (length(value.code) > INDEX_ENTRY_LENGTH) then
    raise exception.create('code too long for indexing: '+value.code);
  if value.display <> '' then
    FEntries.add(key, parent, ndx, ref, value.code, lowercase(RemoveAccents(copy(value.display, 1, INDEX_ENTRY_LENGTH))), 0, ndx.SearchType, false, concept)
  else
    FEntries.add(key, parent, ndx, ref, value.code, '', 0, ndx.SearchType, false, concept);
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
  dec := FTerminologyServer.Ucum.Model.Context.Value(value);
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


procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value : TFhirRange; name : String);
var
  ndx : TFhirIndex;
  v1, v2, crap : String;
  ref : integer;
  specified, canonical : TUcumPair;
  context : TSmartDecimalContext;
begin
  if value = nil then
    exit;

  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
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
  FEntries.add(key, parent, ndx, ref, v1, v2, 0, ndx.SearchType);
  if value.low.system <> '' then
  begin
    ref := FSpaces.ResolveSpace(value.low.system+'#'+value.low.code);
    FEntries.add(key, parent, ndx, ref, v1, v2, 0, ndx.SearchType);
  end;

  // ok, if there's a ucum code:
  if (value.low.code <> '') and (value.low.system = 'http://unitsofmeasure.org') then
  begin
    context := TSmartDecimalContext.Create;
    specified := TUcumPair.create;
    try
      specified.Value := context.Value(value.low.value).Link;
      specified.UnitCode := value.low.code;
      canonical := FTerminologyServer.Ucum.getCanonicalForm(specified);
      try
        GetBoundaries(canonical.Value.AsString, QuantityComparatorNull, v1, v2);
        if (length(v1) > INDEX_ENTRY_LENGTH) then
          raise exception.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if (length(v2) > INDEX_ENTRY_LENGTH) then
          raise exception.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        ref := FSpaces.ResolveSpace('urn:ucum-canonical#'+canonical.UnitCode);
        FEntries.add(key, parent, ndx, ref, v1, v2, 0, ndx.SearchType, true);
      finally
        canonical.free;
        context.Free;
      end;
    finally
      specified.free;
    end;
  end;
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value : TFhirQuantity; name : String; units : string = '');
var
  ndx : TFhirIndex;
  v1, v2 : String;
  ref : integer;
  specified, canonical : TUcumPair;
  context : TSmartDecimalContext;
begin
  if value = nil then
    exit;

  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join: "'+name+'"');
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeNumber, SearchParamTypeQuantity]) then
    raise Exception.create('Unsuitable index "'+name+'" '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing quantity');

  GetBoundaries(value.value, value.comparator, v1, v2);

  if (length(v1) > INDEX_ENTRY_LENGTH) then
      raise exception.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  if (length(v2) > INDEX_ENTRY_LENGTH) then
      raise exception.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  ref := FSpaces.ResolveSpace(value.unit_);
  FEntries.add(key, parent, ndx, ref, v1, v2, 0, ndx.SearchType);
  if value.system <> '' then
  begin
    ref := FSpaces.ResolveSpace(value.system+'#'+value.code);
    FEntries.add(key, parent, ndx, ref, v1, v2, 0, ndx.SearchType);
  end;

  // ok, if there's a ucum code:
  if (value.code <> '') and (value.system = 'http://unitsofmeasure.org') then
  begin
    context := TSmartDecimalContext.Create;
    specified := TUcumPair.create;
    try
      specified.Value := context.Value(value.value).Link;
      specified.UnitCode := value.code;
      canonical := FTerminologyServer.Ucum.getCanonicalForm(specified);
      try
        GetBoundaries(canonical.Value.AsString, value.comparator, v1, v2);
        if (length(v1) > INDEX_ENTRY_LENGTH) then
          raise exception.create('quantity.value too long for indexing: "'+v1+ '" ('+inttostr(length(v1))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        if (length(v2) > INDEX_ENTRY_LENGTH) then
          raise exception.create('quantity.value too long for indexing: "'+v2+ '" ('+inttostr(length(v2))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        ref := FSpaces.ResolveSpace('urn:ucum-canonical#'+canonical.UnitCode);
        FEntries.add(key, parent, ndx, ref, v1, v2, 0, ndx.SearchType, true);
      finally
        canonical.free;
        context.Free;
      end;
    finally
      specified.free;
    end;
  end;
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value : TFhirPeriod; name : String);
begin
  if (value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value : TFhirTiming; name : String);
begin
  if (value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirDateTime; name: String);
begin
  if (value <> nil) and (value.value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; min, max : TDateTime; name: String);
var
  ndx : TFhirIndex;
begin
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType = SearchParamTypeDate) then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing date');
  FEntries.add(key, parent, ndx, 0, HL7DateToString(min, 'yyyymmddhhnnss', false), HL7DateToString(max, 'yyyymmddhhnnss', false), 0, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirIdentifier; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeToken]) then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing Identifier');
  ref := 0;
  if (value.system <> '') then
    ref := FSpaces.ResolveSpace(value.system);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
    raise exception.create('id too long for indexing: '+value.value);
  FEntries.add(key, parent, ndx, ref, value.value, '', 0, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirAddress; name: String);
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

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirContactPoint; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeString]) then
    raise Exception.create('Unsuitable index '+name+':'+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing Contact on '+CODES_TFHIRResourceType[aType]);
  ref := 0;
  if (value.systemElement <> nil) and (value.systemElement.value <> '') then
    ref := FSpaces.ResolveSpace(value.systemElement.value);
  if (length(value.value) > INDEX_ENTRY_LENGTH) then
    raise exception.create('contact value too long for indexing: '+value.value);
  FEntries.add(key, parent, ndx, ref, value.value, '', 0, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; value: TFhirIdentifierList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; value: TFhirCodingList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; value: TFhirCodeableConceptList; name: String);
var
  i : integer;
begin
  if (value <> nil) then
    for i := 0 to value.Count - 1 do
      index(atype, key, parent, value[i], name);
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; value: TFhirSampledData; name: String);
begin
 // todo
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; value: TFhirRatio; name: String);
begin
  // don't have a clue what to do here
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirHumanName; name, phoneticName: String);
var
  i : integer;
begin
  if (value = nil) then
    exit;
  index(aType, key, parent, value.text, name);
  for i := 0 to value.familyList.count - 1 do
    index(aType, key, parent, value.familyList[i], name);
  for i := 0 to value.givenList.count - 1 do
    index(aType, key, parent, value.givenList[i], name);
  for i := 0 to value.prefixList.count - 1 do
    index(aType, key, parent, value.prefixList[i], name);
  for i := 0 to value.suffixList.count - 1 do
    index(aType, key, parent, value.suffixList[i], name);
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
procedure TFhirIndexManager.index(aType : TFhirResourceType; key, parent : integer; value: TFhirDecimal; name: String);
var
  ndx : TFhirIndex;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
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

procedure TFhirIndexManager.index(context : TFhirResource; aType : TFhirResourceType; key, parent : integer; value: TFhirReference; name: String; specificType : TFhirResourceType = frtNull);
var
  ndx : TFhirIndex;
  ref, i : integer;
  target : integer;
  type_, id : String;
  contained : TFhirResource;
  url : String;
  ok : boolean;
begin
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
//    raise Exception.create('Attempt to index a resource join in an index ('+CODES_TFHIRResourceType[aType]+'/'+name+') that is a not a join (has no target types)');
  if ndx.SearchType <> SearchParamTypeReference then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing reference on a '+CODES_TFHIRResourceType[aType]);

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
    if (specificType = frtNull) or (contained.ResourceType = specificType) then
    begin
      ref := FSpaces.ResolveSpace(CODES_TFHIRResourceType[contained.ResourceType]);
      target := FKeyEvent(ktResource, contained.ResourceType, id);
      FSpaces.FDB.execSql('update Types set LastId = '+id+' where ResourceTypeKey = '+inttostr(ref)+' and LastId < '+id);
      FSpaces.FDB.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, MasterResourceKey) values (:k, :r, :i, null, '+inttostr(FMasterKey)+')';
      FSpaces.FDB.Prepare;
      FSpaces.FDB.BindInteger('k', target);
      FSpaces.FDB.BindInteger('r', ref);
      FSpaces.FDB.BindString('i', id);
      FSpaces.FDB.Execute;
      FSpaces.FDB.Terminate;
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
      if (specificType = frtNull) or (type_ = CODES_TFHIRResourceType[specificType]) then
      begin
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
    FEntries.add(key, parent, ndx, ref, id, '', target, ndx.SearchType);
end;

Const
  CHECK_TSearchParamsEncounter : Array[TSearchParamsEncounter] of TSearchParamsEncounter = (spEncounter__content, spEncounter__id, spEncounter__lastUpdated, spEncounter__profile, spEncounter__query, spEncounter__security, spEncounter__tag, spEncounter__text,
    spEncounter_Appointment, spEncounter_Condition, spEncounter_Date, spEncounter_Episodeofcare, spEncounter_Identifier, spEncounter_Incomingreferral, spEncounter_Indication, spEncounter_Length, spEncounter_Location, spEncounter_Locationperiod,
    spEncounter_Partof, spEncounter_Participant, spEncounter_Participanttype, spEncounter_Patient, spEncounter_Practitioner, spEncounter_Procedure, spEncounter_Reason, spEncounter_Specialarrangement, spEncounter_Status, spEncounter_Type);

procedure TFhirIndexInformation.buildIndexesEncounter;
var
  a : TSearchParamsEncounter;
begin
  for a := low(TSearchParamsEncounter) to high(TSearchParamsEncounter) do
  begin
    assert(CHECK_TSearchParamsEncounter[a] = a);
    indexes.add(frtEncounter, CODES_TSearchParamsEncounter[a], DESC_TSearchParamsEncounter[a], TYPES_TSearchParamsEncounter[a], TARGETS_TSearchParamsEncounter[a], PATHS_TSearchParamsEncounter[a], USES_TSearchParamsEncounter[a]);
  end;
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

Const

  CHECK_TSearchParamsLocation : Array[TSearchParamsLocation] of TSearchParamsLocation = (spLocation__content, spLocation__id, spLocation__lastUpdated, spLocation__profile, spLocation__query, spLocation__security, spLocation__tag, spLocation__text, spLocation_Address, spLocation_Addresscity, spLocation_Addresscountry, spLocation_Addresspostalcode, spLocation_Addressstate, spLocation_Addressuse, spLocation_Identifier, spLocation_Name, spLocation_Near, spLocation_Neardistance, spLocation_Organization, spLocation_Partof, spLocation_Status, spLocation_Type);



procedure TFhirIndexInformation.buildIndexesLocation;
var
  a : TSearchParamsLocation;
begin
  for a := low(TSearchParamsLocation) to high(TSearchParamsLocation) do
  begin
    assert(CHECK_TSearchParamsLocation[a] = a);
    indexes.add(frtLocation, CODES_TSearchParamsLocation[a], DESC_TSearchParamsLocation[a], TYPES_TSearchParamsLocation[a], TARGETS_TSearchParamsLocation[a], PATHS_TSearchParamsLocation[a], USES_TSearchParamsLocation[a]);
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



Const
  CHECK_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of TSearchParamsDocumentReference = (
    spDocumentReference__content, spDocumentReference__id, spDocumentReference__lastUpdated, spDocumentReference__profile, spDocumentReference__query, spDocumentReference__security, spDocumentReference__tag,
    spDocumentReference__text, spDocumentReference_Authenticator, spDocumentReference_Author, spDocumentReference_Class, spDocumentReference_Created, spDocumentReference_Custodian, spDocumentReference_Description, spDocumentReference_Encounter,
    spDocumentReference_Event, spDocumentReference_Facility, spDocumentReference_Format, spDocumentReference_Identifier, spDocumentReference_Indexed, spDocumentReference_Language, spDocumentReference_Location, spDocumentReference_Patient,
    spDocumentReference_Period, spDocumentReference_Relatedid, spDocumentReference_Relatedref, spDocumentReference_Relatesto, spDocumentReference_Relation, spDocumentReference_Relationship, spDocumentReference_Securitylabel,
    spDocumentReference_Setting, spDocumentReference_Status, spDocumentReference_Subject, spDocumentReference_Type);



procedure TFhirIndexInformation.buildIndexesDocumentReference;
var
  a : TSearchParamsDocumentReference;
begin
  for a := low(TSearchParamsDocumentReference) to high(TSearchParamsDocumentReference) do
  begin
    assert(CHECK_TSearchParamsDocumentReference[a] = a);
    indexes.add(frtDocumentReference, CODES_TSearchParamsDocumentReference[a], DESC_TSearchParamsDocumentReference[a], TYPES_TSearchParamsDocumentReference[a], TARGETS_TSearchParamsDocumentReference[a], PATHS_TSearchParamsDocumentReference[a], USES_TSearchParamsDocumentReference[a]);
  end;
  composites.add(frtDocumentReference, 'relatesTo', ['code', 'relation', 'target', 'relatesTo']);
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

Const
  CHECK_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of TSearchParamsDocumentManifest = (spDocumentManifest__content, spDocumentManifest__id, spDocumentManifest__lastUpdated, spDocumentManifest__profile, spDocumentManifest__query, spDocumentManifest__security, spDocumentManifest__tag, spDocumentManifest__text,
  spDocumentManifest_Author, spDocumentManifest_Contentref, spDocumentManifest_Created, spDocumentManifest_Description, spDocumentManifest_Identifier, spDocumentManifest_Patient, spDocumentManifest_Recipient, spDocumentManifest_Relatedid, spDocumentManifest_Relatedref, spDocumentManifest_Source, spDocumentManifest_Status, spDocumentManifest_Subject, spDocumentManifest_Type);

procedure TFhirIndexInformation.buildIndexesDocumentManifest;
var
  a : TSearchParamsDocumentManifest;
begin
  for a := low(TSearchParamsDocumentManifest) to high(TSearchParamsDocumentManifest) do
  begin
    assert(CHECK_TSearchParamsDocumentManifest[a] = a);
    indexes.add(frtDocumentManifest, CODES_TSearchParamsDocumentManifest[a], DESC_TSearchParamsDocumentManifest[a], TYPES_TSearchParamsDocumentManifest[a], TARGETS_TSearchParamsDocumentManifest[a], PATHS_TSearchParamsDocumentManifest[a], USES_TSearchParamsDocumentManifest[a]);
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

{
procedure TFhirIndexManager.index(aType: TFhirResourceType; key: integer; value: TFhirEnumList; name: String);
var
  i : integer;
begin
  for i := 0 to value.count - 1 do
    index(aType, key, value[i], name);
end;
}

procedure TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; value: TFhirInteger; name: String);
var
  ndx : TFhirIndex;
begin
  if (value = nil) or (value.value = '') then
    exit;
  ndx := FInfo.FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeString, SearchParamTypeNumber, SearchParamTypeToken]) then
    raise Exception.create('Unsuitable index '+name+' : '+CODES_TFhirSearchParamTypeEnum[ndx.SearchType]+' indexing integer');
  FEntries.add(key, parent, ndx, 0, value.value, '', 0, ndx.SearchType);
end;


Const
  CHECK_TSearchParamsBundle : Array[TSearchParamsBundle] of TSearchParamsBundle = (spBundle__content, spBundle__id, spBundle__lastUpdated, spBundle__profile, spBundle__query, spBundle__security, spBundle__tag, spBundle__text, spBundle_Composition, spBundle_Message, spBundle_Type);

procedure TFhirIndexInformation.buildIndexesBundle;
var
  a : TSearchParamsBundle;
begin
  for a := low(TSearchParamsBundle) to high(TSearchParamsBundle) do
  begin
    assert(CHECK_TSearchParamsBundle[a] = a);
    indexes.add(frtBundle, CODES_TSearchParamsBundle[a], DESC_TSearchParamsBundle[a], TYPES_TSearchParamsBundle[a], TARGETS_TSearchParamsBundle[a], PATHS_TSearchParamsBundle[a], USES_TSearchParamsBundle[a]);
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



Const
  CHECK_TSearchParamsFlag : Array[TSearchParamsFlag] of TSearchParamsFlag = (spFlag__content, spFlag__id, spFlag__lastUpdated, spFlag__profile, spFlag__query, spFlag__security, spFlag__tag, spFlag__text,
    spFlag_Author, spFlag_Date, spFlag_Encounter, spFlag_Patient, spFlag_Subject);

procedure TFhirIndexInformation.buildIndexesFlag;
var
  a : TSearchParamsFlag;
begin
  for a := low(TSearchParamsFlag) to high(TSearchParamsFlag) do
  begin
    assert(CHECK_TSearchParamsFlag[a] = a);
    indexes.add(frtFlag, CODES_TSearchParamsFlag[a], DESC_TSearchParamsFlag[a], TYPES_TSearchParamsFlag[a], TARGETS_TSearchParamsFlag[a], PATHS_TSearchParamsFlag[a], USES_TSearchParamsFlag[a]);
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


Const
  CHECK_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of TSearchParamsAllergyIntolerance = ( spAllergyIntolerance__content, spAllergyIntolerance__id, spAllergyIntolerance__lastUpdated, spAllergyIntolerance__profile, spAllergyIntolerance__query, spAllergyIntolerance__security, spAllergyIntolerance__tag, spAllergyIntolerance__text,
    spAllergyIntolerance_Category, spAllergyIntolerance_Criticality, spAllergyIntolerance_Date, spAllergyIntolerance_Identifier, spAllergyIntolerance_Lastdate, spAllergyIntolerance_Manifestation, spAllergyIntolerance_Onset, spAllergyIntolerance_Patient,
    spAllergyIntolerance_Recorder, spAllergyIntolerance_Reporter, spAllergyIntolerance_Route, spAllergyIntolerance_Severity, spAllergyIntolerance_Status, spAllergyIntolerance_Substance, spAllergyIntolerance_Type);

procedure TFhirIndexInformation.buildIndexesAllergyIntolerance;
var
  a : TSearchParamsAllergyIntolerance;
begin
  for a := low(TSearchParamsAllergyIntolerance) to high(TSearchParamsAllergyIntolerance) do
  begin
    assert(CHECK_TSearchParamsAllergyIntolerance[a] = a);
    indexes.add(frtAllergyIntolerance, CODES_TSearchParamsAllergyIntolerance[a], DESC_TSearchParamsAllergyIntolerance[a], TYPES_TSearchParamsAllergyIntolerance[a], TARGETS_TSearchParamsAllergyIntolerance[a], PATHS_TSearchParamsAllergyIntolerance[a], USES_TSearchParamsAllergyIntolerance[a]);
  end;
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




Const
  CHECK_TSearchParamsSubstance : Array[TSearchParamsSubstance] of TSearchParamsSubstance = (
    spSubstance__content, spSubstance__id, spSubstance__lastUpdated, spSubstance__profile, spSubstance__query, spSubstance__security, spSubstance__tag, spSubstance__text,
    spSubstance_Category, spSubstance_Code, spSubstance_Containeridentifier, spSubstance_Expiry, spSubstance_Identifier, spSubstance_Quantity, spSubstance_Substance);


procedure TFhirIndexInformation.buildIndexesSubstance;
var
  a : TSearchParamsSubstance;
begin
  for a := low(TSearchParamsSubstance) to high(TSearchParamsSubstance) do
  begin
    assert(CHECK_TSearchParamsSubstance[a] = a);
    indexes.add(frtSubstance, CODES_TSearchParamsSubstance[a], DESC_TSearchParamsSubstance[a], TYPES_TSearchParamsSubstance[a], TARGETS_TSearchParamsSubstance[a], PATHS_TSearchParamsSubstance[a], USES_TSearchParamsSubstance[a]);
  end;
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

procedure TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; value: TFhirDate; name: String);
begin
  if (value <> nil) and (value.value <> nil) then
    index(aType, key, parent, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; value: TFhirBoolean; name: String);
var
  ndx : TFhirIndex;
begin
  if (value <> nil) then
    index(aType, key, parent, value.value, name);
end;



Const
  {$IFDEF FHIR_DSTU2}
  CHECK_TSearchParamsBasic : Array[TSearchParamsBasic] of TSearchParamsBasic = ( spBasic__content, spBasic__id, spBasic__lastUpdated, spBasic__profile, spBasic__query, spBasic__security, spBasic__tag, spBasic__text,
    spBasic_Author, spBasic_Code, spBasic_Created, spBasic_Description, spBasic_Identifier, spBasic_Keyword, spBasic_MinScore, spBasic_Patient, spBasic_Status, spBasic_Subject, spBasic_Title, spBasic_Topic, spBasic_Version);
  {$ELSE}
  CHECK_TSearchParamsBasic : Array[TSearchParamsBasic] of TSearchParamsBasic = ( spBasic__content, spBasic__id, spBasic__lastUpdated, spBasic__profile, spBasic__query, spBasic__security, spBasic__tag, spBasic__text,
    spBasic_Author, spBasic_Code, spBasic_Created, spBasic_Identifier, spBasic_Patient, spBasic_Subject);
  {$ENDIF}

procedure TFhirIndexInformation.buildIndexesBasic;
var
  a : TSearchParamsBasic;
begin
  for a := low(TSearchParamsBasic) to high(TSearchParamsBasic) do
  begin
    assert(CHECK_TSearchParamsBasic[a] = a);
    indexes.add(frtBasic, CODES_TSearchParamsBasic[a], DESC_TSearchParamsBasic[a], TYPES_TSearchParamsBasic[a], TARGETS_TSearchParamsBasic[a], PATHS_TSearchParamsBasic[a], USES_TSearchParamsBasic[a]);
  end;
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


{$IFDEF FHIR_DSTU2}
Const
  CHECK_TSearchParamsClaim : Array[TSearchParamsClaim] of TSearchParamsClaim = ( spClaim__content, spClaim__id, spClaim__lastUpdated, spClaim__profile, spClaim__query, spClaim__security, spClaim__tag, spClaim__text,
    spClaim_Identifier, spClaim_Patient, spClaim_Priority, spClaim_Provider, spClaim_Use);

procedure TFhirIndexInformation.buildIndexesClaim;
var
  a : TSearchParamsClaim;
begin
  for a := low(TSearchParamsClaim) to high(TSearchParamsClaim) do
  begin
    assert(CHECK_TSearchParamsClaim[a] = a);
    indexes.add(frtClaim, CODES_TSearchParamsClaim[a], DESC_TSearchParamsClaim[a], TYPES_TSearchParamsClaim[a], TARGETS_TSearchParamsClaim[a], PATHS_TSearchParamsClaim[a], USES_TSearchParamsClaim[a]);
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


Const
  CHECK_TSearchParamsCoverage : Array[TSearchParamsCoverage] of TSearchParamsCoverage = ( spCoverage__content, spCoverage__id, spCoverage__lastUpdated, spCoverage__profile, spCoverage__query, spCoverage__security, spCoverage__tag, spCoverage__text,
    spCoverage_Dependent, spCoverage_Group, spCoverage_Identifier, spCoverage_Issuer, spCoverage_Plan, spCoverage_Sequence, spCoverage_Subplan, spCoverage_Type);

procedure TFhirIndexInformation.buildIndexesCoverage;
var
  a : TSearchParamsCoverage;
begin
  for a := low(TSearchParamsCoverage) to high(TSearchParamsCoverage) do
  begin
    assert(CHECK_TSearchParamsCoverage[a] = a);
    indexes.add(frtCoverage, CODES_TSearchParamsCoverage[a], DESC_TSearchParamsCoverage[a], TYPES_TSearchParamsCoverage[a], TARGETS_TSearchParamsCoverage[a], PATHS_TSearchParamsCoverage[a], USES_TSearchParamsCoverage[a]);
  end;
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
Const
  CHECK_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of TSearchParamsClaimResponse = ( spClaimResponse__content, spClaimResponse__id, spClaimResponse__lastUpdated, spClaimResponse__profile, spClaimResponse__query, spClaimResponse__security, spClaimResponse__tag, spClaimResponse__text,
     spClaimResponse_Identifier);

procedure TFhirIndexInformation.buildIndexesClaimResponse;
var
  a : TSearchParamsClaimResponse;
begin
  for a := low(TSearchParamsClaimResponse) to high(TSearchParamsClaimResponse) do
  begin
    assert(CHECK_TSearchParamsClaimResponse[a] = a);
    indexes.add(frtClaimResponse, CODES_TSearchParamsClaimResponse[a], DESC_TSearchParamsClaimResponse[a], TYPES_TSearchParamsClaimResponse[a], TARGETS_TSearchParamsClaimResponse[a], PATHS_TSearchParamsClaimResponse[a], USES_TSearchParamsClaimResponse[a]);
  end;
  indexes.add(frtClaimResponse, 'request', 'request claim link', SearchParamTypeReference, [frtClaim], '', SearchXpathUsageNull);
end;

procedure TFhirIndexManager.buildIndexValuesClaimResponse(key: integer; id : String; context : TFhirResource; resource: TFhirClaimResponse);
begin
  index(frtClaimResponse, key, 0, resource.identifierList, CODES_TSearchParamsClaimResponse[spClaimResponse_identifier]);
//  index(context, frtClaimResponse, key, 0, resource.request, CODES_TSearchParamsClaimResponse[spClaimResponse_request]);
end;

const
  CHECK_TSearchParamsEligibilityRequest : Array[TSearchParamsEligibilityRequest] of TSearchParamsEligibilityRequest = ( spEligibilityRequest__content, spEligibilityRequest__id, spEligibilityRequest__lastUpdated, spEligibilityRequest__profile, spEligibilityRequest__query, spEligibilityRequest__security, spEligibilityRequest__tag, spEligibilityRequest__text,
    spEligibilityRequest_Identifier);

procedure TFhirIndexInformation.buildIndexesEligibilityRequest;
var
  a : TSearchParamsEligibilityRequest;
begin
  for a := low(TSearchParamsEligibilityRequest) to high(TSearchParamsEligibilityRequest) do
  begin
    assert(CHECK_TSearchParamsEligibilityRequest[a] = a);
    indexes.add(frtEligibilityRequest, CODES_TSearchParamsEligibilityRequest[a], DESC_TSearchParamsEligibilityRequest[a], TYPES_TSearchParamsEligibilityRequest[a], TARGETS_TSearchParamsEligibilityRequest[a], PATHS_TSearchParamsEligibilityRequest[a], USES_TSearchParamsEligibilityRequest[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesEligibilityRequest(key: integer; id : String; context : TFhirResource; resource: TFhirEligibilityRequest);
begin
  index(frtEligibilityRequest, key, 0, resource.identifierList, CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_identifier]);
end;

const
  CHECK_TSearchParamsEligibilityResponse : Array[TSearchParamsEligibilityResponse] of TSearchParamsEligibilityResponse = ( spEligibilityResponse__content, spEligibilityResponse__id, spEligibilityResponse__lastUpdated, spEligibilityResponse__profile, spEligibilityResponse__query, spEligibilityResponse__security, spEligibilityResponse__tag, spEligibilityResponse__text,
    spEligibilityResponse_Identifier);

procedure TFhirIndexInformation.buildIndexesEligibilityResponse;
var
  a : TSearchParamsEligibilityResponse;
begin
  for a := low(TSearchParamsEligibilityResponse) to high(TSearchParamsEligibilityResponse) do
  begin
    assert(CHECK_TSearchParamsEligibilityResponse[a] = a);
    indexes.add(frtEligibilityResponse, CODES_TSearchParamsEligibilityResponse[a], DESC_TSearchParamsEligibilityResponse[a], TYPES_TSearchParamsEligibilityResponse[a], TARGETS_TSearchParamsEligibilityResponse[a], PATHS_TSearchParamsEligibilityResponse[a], USES_TSearchParamsEligibilityResponse[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesEligibilityResponse(key: integer; id : String; context : TFhirResource; resource: TFhirEligibilityResponse);
begin
  index(frtEligibilityResponse, key, 0, resource.identifierList, CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_identifier]);
end;

const
  CHECK_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of TSearchParamsEnrollmentRequest = ( spEnrollmentRequest__content, spEnrollmentRequest__id, spEnrollmentRequest__lastUpdated, spEnrollmentRequest__profile, spEnrollmentRequest__query, spEnrollmentRequest__security, spEnrollmentRequest__tag, spEnrollmentRequest__text,
    spEnrollmentRequest_Identifier, spEnrollmentRequest_Patient, spEnrollmentRequest_Subject);

procedure TFhirIndexInformation.buildIndexesEnrollmentRequest;
var
  a : TSearchParamsEnrollmentRequest;
begin
  for a := low(TSearchParamsEnrollmentRequest) to high(TSearchParamsEnrollmentRequest) do
  begin
    assert(CHECK_TSearchParamsEnrollmentRequest[a] = a);
    indexes.add(frtEnrollmentRequest, CODES_TSearchParamsEnrollmentRequest[a], DESC_TSearchParamsEnrollmentRequest[a], TYPES_TSearchParamsEnrollmentRequest[a], TARGETS_TSearchParamsEnrollmentRequest[a], PATHS_TSearchParamsEnrollmentRequest[a], USES_TSearchParamsEnrollmentRequest[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesEnrollmentRequest(key: integer; id : String; context : TFhirResource; resource: TFhirEnrollmentRequest);
begin
  index(frtEnrollmentRequest, key, 0, resource.identifierList, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_identifier]);
  index(context, frtEnrollmentRequest, key, 0, resource.subject, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_subject]);
  index(context, frtEnrollmentRequest, key, 0, resource.subject, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_patient]);
  patientCompartment(key, resource.subject);
end;

const
  CHECK_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of TSearchParamsEnrollmentResponse = ( spEnrollmentResponse__content, spEnrollmentResponse__id, spEnrollmentResponse__lastUpdated, spEnrollmentResponse__profile, spEnrollmentResponse__query, spEnrollmentResponse__security, spEnrollmentResponse__tag, spEnrollmentResponse__text,
    spEnrollmentResponse_Identifier);

procedure TFhirIndexInformation.buildIndexesEnrollmentResponse;
var
  a : TSearchParamsEnrollmentResponse;
begin
  for a := low(TSearchParamsEnrollmentResponse) to high(TSearchParamsEnrollmentResponse) do
  begin
    assert(CHECK_TSearchParamsEnrollmentResponse[a] = a);
    indexes.add(frtEnrollmentResponse, CODES_TSearchParamsEnrollmentResponse[a], DESC_TSearchParamsEnrollmentResponse[a], TYPES_TSearchParamsEnrollmentResponse[a], TARGETS_TSearchParamsEnrollmentResponse[a], PATHS_TSearchParamsEnrollmentResponse[a], USES_TSearchParamsEnrollmentResponse[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesEnrollmentResponse(key: integer; id : String; context : TFhirResource; resource: TFhirEnrollmentResponse);
begin
  index(frtEnrollmentResponse, key, 0, resource.identifierList, CODES_TSearchParamsEnrollmentResponse[spEnrollmentResponse_identifier]);
end;


const
  CHECK_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of TSearchParamsExplanationOfBenefit = (
    spExplanationOfBenefit__content, spExplanationOfBenefit__id, spExplanationOfBenefit__lastUpdated, spExplanationOfBenefit__profile, spExplanationOfBenefit__query, spExplanationOfBenefit__security, spExplanationOfBenefit__tag, spExplanationOfBenefit__text,
    spExplanationOfBenefit_Identifier);

procedure TFhirIndexInformation.buildIndexesExplanationOfBenefit;
var
  a : TSearchParamsExplanationOfBenefit;
begin
  for a := low(TSearchParamsExplanationOfBenefit) to high(TSearchParamsExplanationOfBenefit) do
  begin
    assert(CHECK_TSearchParamsExplanationOfBenefit[a] = a);
    indexes.add(frtExplanationOfBenefit, CODES_TSearchParamsExplanationOfBenefit[a], DESC_TSearchParamsExplanationOfBenefit[a], TYPES_TSearchParamsExplanationOfBenefit[a], TARGETS_TSearchParamsExplanationOfBenefit[a], PATHS_TSearchParamsExplanationOfBenefit[a], USES_TSearchParamsExplanationOfBenefit[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesExplanationOfBenefit(key: integer; id : String; context : TFhirResource; resource: TFhirExplanationOfBenefit);
var
  i : integer;
begin
  index(frtExplanationOfBenefit, key, 0, resource.identifierList, CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_identifier]);
end;

const
  CHECK_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of TSearchParamsPaymentNotice = ( spPaymentNotice__content, spPaymentNotice__id, spPaymentNotice__lastUpdated, spPaymentNotice__profile, spPaymentNotice__query, spPaymentNotice__security, spPaymentNotice__tag, spPaymentNotice__text,
    spPaymentNotice_Identifier);

procedure TFhirIndexInformation.buildIndexesPaymentNotice;
var
  a : TSearchParamsPaymentNotice;
begin
  for a := low(TSearchParamsPaymentNotice) to high(TSearchParamsPaymentNotice) do
  begin
    assert(CHECK_TSearchParamsPaymentNotice[a] = a);
    indexes.add(frtPaymentNotice, CODES_TSearchParamsPaymentNotice[a], DESC_TSearchParamsPaymentNotice[a], TYPES_TSearchParamsPaymentNotice[a], TARGETS_TSearchParamsPaymentNotice[a], PATHS_TSearchParamsPaymentNotice[a], USES_TSearchParamsPaymentNotice[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesPaymentNotice(key: integer; id : String; context : TFhirResource; resource: TFhirPaymentNotice);
var
  i : integer;
begin
  index(frtPaymentNotice, key, 0, resource.identifierList, CODES_TSearchParamsPaymentNotice[spPaymentNotice_identifier]);
end;


const
  CHECK_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of TSearchParamsPaymentReconciliation = (
    spPaymentReconciliation__content, spPaymentReconciliation__id, spPaymentReconciliation__lastUpdated, spPaymentReconciliation__profile, spPaymentReconciliation__query, spPaymentReconciliation__security, spPaymentReconciliation__tag, spPaymentReconciliation__text,
    spPaymentReconciliation_Identifier);

procedure TFhirIndexInformation.buildIndexesPaymentReconciliation;
var
  a : TSearchParamsPaymentReconciliation;
begin
  for a := low(TSearchParamsPaymentReconciliation) to high(TSearchParamsPaymentReconciliation) do
  begin
    assert(CHECK_TSearchParamsPaymentReconciliation[a] = a);
    indexes.add(frtPaymentReconciliation, CODES_TSearchParamsPaymentReconciliation[a], DESC_TSearchParamsPaymentReconciliation[a], TYPES_TSearchParamsPaymentReconciliation[a], TARGETS_TSearchParamsPaymentReconciliation[a], PATHS_TSearchParamsPaymentReconciliation[a], USES_TSearchParamsPaymentReconciliation[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesPaymentReconciliation(key: integer; id : String; context : TFhirResource; resource: TFhirPaymentReconciliation);
var
  i : integer;
begin
  index(frtPaymentReconciliation, key, 0, resource.identifierList, CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_identifier]);
end;


{$ELSE}
Const
  CHECK_TSearchParamsCoverage : Array[TSearchParamsCoverage] of TSearchParamsCoverage = ( spCoverage__content, spCoverage__id, spCoverage__lastUpdated, spCoverage__profile, spCoverage__query, spCoverage__security, spCoverage__tag, spCoverage__text,
    spCoverage_Beneficiaryidentifier, spCoverage_Beneficiaryreference, spCoverage_Dependent, spCoverage_Group, spCoverage_Identifier, spCoverage_Issueridentifier, spCoverage_Issuerreference, spCoverage_Plan, spCoverage_Planholderidentifier, spCoverage_Planholderreference, spCoverage_Sequence, spCoverage_Subplan, spCoverage_Type);

procedure TFhirIndexInformation.buildIndexesCoverage;
var
  a : TSearchParamsCoverage;
begin
  for a := low(TSearchParamsCoverage) to high(TSearchParamsCoverage) do
  begin
    assert(CHECK_TSearchParamsCoverage[a] = a);
    indexes.add(frtCoverage, CODES_TSearchParamsCoverage[a], DESC_TSearchParamsCoverage[a], TYPES_TSearchParamsCoverage[a], TARGETS_TSearchParamsCoverage[a], PATHS_TSearchParamsCoverage[a], USES_TSearchParamsCoverage[a]);
  end;
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
  if resource.issuer is TFhirIdentifier then
    index(frtCoverage, key, 0, resource.issuer as TFhirIdentifier, CODES_TSearchParamsCoverage[spCoverage_issueridentifier])
  else if resource.issuer is TFhirReference then
    index(context, frtCoverage, key, 0, resource.issuer as TFhirReference, CODES_TSearchParamsCoverage[spCoverage_issuerreference]);

  if resource.beneficiary is TFhirIdentifier then
    index(frtCoverage, key, 0, resource.beneficiary as TFhirIdentifier, CODES_TSearchParamsCoverage[spCoverage_beneficiaryidentifier])
  else if resource.beneficiary is TFhirReference then
    index(context, frtCoverage, key, 0, resource.beneficiary as TFhirReference, CODES_TSearchParamsCoverage[spCoverage_beneficiaryreference]);

  if resource.planholder is TFhirIdentifier then
    index(frtCoverage, key, 0, resource.planholder as TFhirIdentifier, CODES_TSearchParamsCoverage[spCoverage_planholderidentifier])
  else if resource.planholder is TFhirReference then
    index(context, frtCoverage, key, 0, resource.planholder as TFhirReference, CODES_TSearchParamsCoverage[spCoverage_planholderreference]);
end;


Const
  CHECK_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of TSearchParamsClaimResponse = ( spClaimResponse__content, spClaimResponse__id, spClaimResponse__lastUpdated, spClaimResponse__profile, spClaimResponse__query, spClaimResponse__security, spClaimResponse__tag, spClaimResponse__text,
    spClaimResponse_Created, spClaimResponse_Disposition, spClaimResponse_Identifier, spClaimResponse_Organizationidentifier, spClaimResponse_Organizationreference, spClaimResponse_Outcome, spClaimResponse_Paymentdate, spClaimResponse_Requestidentifier, spClaimResponse_Requestreference);

procedure TFhirIndexInformation.buildIndexesClaimResponse;
var
  a : TSearchParamsClaimResponse;
begin
  for a := low(TSearchParamsClaimResponse) to high(TSearchParamsClaimResponse) do
  begin
    assert(CHECK_TSearchParamsClaimResponse[a] = a);
    indexes.add(frtClaimResponse, CODES_TSearchParamsClaimResponse[a], DESC_TSearchParamsClaimResponse[a], TYPES_TSearchParamsClaimResponse[a], TARGETS_TSearchParamsClaimResponse[a], PATHS_TSearchParamsClaimResponse[a], USES_TSearchParamsClaimResponse[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesClaimResponse(key: integer; id : String; context : TFhirResource; resource: TFhirClaimResponse);
begin
  index(frtClaimResponse, key, 0, resource.identifierList, CODES_TSearchParamsClaimResponse[spClaimResponse_identifier]);
  index(frtClaimResponse, key, 0, resource.createdElement, CODES_TSearchParamsClaimResponse[spClaimResponse_created]);
  index(frtClaimResponse, key, 0, resource.dispositionElement, CODES_TSearchParamsClaimResponse[spClaimResponse_disposition]);
  if resource.organization is TFhirReference then
    index(context, frtClaimResponse, key, 0, TFhirReference(resource.organization), CODES_TSearchParamsClaimResponse[spClaimResponse_Organizationreference])
  else if resource.organization is TFhirIdentifier then
    index(frtClaimResponse, key, 0, TFhirIdentifier(resource.organization), CODES_TSearchParamsClaimResponse[spClaimResponse_Organizationidentifier]);
  index(frtClaimResponse, key, 0, resource.outcomeElement, CODES_TSearchParamsClaimResponse[spClaimResponse_outcome]);
  index(frtClaimResponse, key, 0, resource.paymentDateElement, CODES_TSearchParamsClaimResponse[spClaimResponse_paymentdate]);
  if resource.request is TFhirReference then
    index(context, frtClaimResponse, key, 0, TFhirReference(resource.request), CODES_TSearchParamsClaimResponse[spClaimResponse_Requestreference])
  else if resource.request is TFhirIdentifier then
    index(frtClaimResponse, key, 0, TFhirIdentifier(resource.request), CODES_TSearchParamsClaimResponse[spClaimResponse_Requestidentifier]);
end;

Const
  CHECK_TSearchParamsClaim : Array[TSearchParamsClaim] of TSearchParamsClaim = ( spClaim__content, spClaim__id, spClaim__lastUpdated, spClaim__profile, spClaim__query, spClaim__security, spClaim__tag, spClaim__text,
    spClaim_Created, spClaim_Facilityidentifier, spClaim_Facilityreference, spClaim_Identifier, spClaim_Organizationidentifier, spClaim_Organizationreference, spClaim_Patientidentifier, spClaim_Patientreference, spClaim_Priority, spClaim_Provideridentifier, spClaim_Providerreference, spClaim_Targetidentifier, spClaim_Targetreference, spClaim_Use);


procedure TFhirIndexInformation.buildIndexesClaim;
var
  a : TSearchParamsClaim;
begin
  for a := low(TSearchParamsClaim) to high(TSearchParamsClaim) do
  begin
    assert(CHECK_TSearchParamsClaim[a] = a);
    indexes.add(frtClaim, CODES_TSearchParamsClaim[a], DESC_TSearchParamsClaim[a], TYPES_TSearchParamsClaim[a], TARGETS_TSearchParamsClaim[a], PATHS_TSearchParamsClaim[a], USES_TSearchParamsClaim[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesClaim(key: integer; id : String; context : TFhirResource; resource: TFhirClaim);
begin
  index(frtClaim, key, 0, resource.createdElement, CODES_TSearchParamsClaim[spClaim_created]);
  if resource.facility is TFhirIdentifier then
    index(frtClaim, key, 0, resource.facility as TFhirIdentifier, CODES_TSearchParamsClaim[spClaim_facilityidentifier])
  else if resource.facility is TFhirReference then
    index(context, frtClaim, key, 0, resource.facility as TFhirReference, CODES_TSearchParamsClaim[spClaim_facilityreference]);
  index(frtClaim, key, 0, resource.identifierList, CODES_TSearchParamsClaim[spClaim_identifier]);
  if resource.organization is TFhirIdentifier then
    index(frtClaim, key, 0, resource.organization as TFhirIdentifier, CODES_TSearchParamsClaim[spClaim_organizationidentifier])
  else if resource.organization is TFhirReference then
    index(context, frtClaim, key, 0, resource.organization as TFhirReference, CODES_TSearchParamsClaim[spClaim_organizationreference]);
  if resource.patient is TFhirIdentifier then
    index(frtClaim, key, 0, resource.patient as TFhirIdentifier, CODES_TSearchParamsClaim[spClaim_patientidentifier])
  else if resource.patient is TFhirReference then
    index(context, frtClaim, key, 0, resource.patient as TFhirReference, CODES_TSearchParamsClaim[spClaim_patientreference]);
  index(frtClaim, key, 0, resource.priorityElement, CODES_TSearchParamsClaim[spClaim_priority]);
  if resource.provider is TFhirIdentifier then
    index(frtClaim, key, 0, resource.provider as TFhirIdentifier, CODES_TSearchParamsClaim[spClaim_provideridentifier])
  else if resource.provider is TFhirReference then
    index(context, frtClaim, key, 0, resource.provider as TFhirReference, CODES_TSearchParamsClaim[spClaim_providerreference]);
  if resource.target is TFhirIdentifier then
    index(frtClaim, key, 0, resource.target as TFhirIdentifier, CODES_TSearchParamsClaim[spClaim_targetidentifier])
  else if resource.target is TFhirReference then
    index(context, frtClaim, key, 0, resource.target as TFhirReference, CODES_TSearchParamsClaim[spClaim_targetreference]);
  index(frtClaim, key, 0, resource.useElement, CODES_TSearchParamsClaim[spClaim_use]);
end;

const
  CHECK_TSearchParamsEligibilityRequest : Array[TSearchParamsEligibilityRequest] of TSearchParamsEligibilityRequest = ( spEligibilityRequest__content, spEligibilityRequest__id, spEligibilityRequest__lastUpdated, spEligibilityRequest__profile, spEligibilityRequest__query, spEligibilityRequest__security, spEligibilityRequest__tag, spEligibilityRequest__text,
    spEligibilityRequest_Created, spEligibilityRequest_Facilityidentifier, spEligibilityRequest_Facilityreference, spEligibilityRequest_Identifier, spEligibilityRequest_Organizationidentifier, spEligibilityRequest_Organizationreference, spEligibilityRequest_Patientidentifier, spEligibilityRequest_Patientreference, spEligibilityRequest_Provideridentifier, spEligibilityRequest_Providerreference);

procedure TFhirIndexInformation.buildIndexesEligibilityRequest;
var
  a : TSearchParamsEligibilityRequest;
begin
  for a := low(TSearchParamsEligibilityRequest) to high(TSearchParamsEligibilityRequest) do
  begin
    assert(CHECK_TSearchParamsEligibilityRequest[a] = a);
    indexes.add(frtEligibilityRequest, CODES_TSearchParamsEligibilityRequest[a], DESC_TSearchParamsEligibilityRequest[a], TYPES_TSearchParamsEligibilityRequest[a], TARGETS_TSearchParamsEligibilityRequest[a], PATHS_TSearchParamsEligibilityRequest[a], USES_TSearchParamsEligibilityRequest[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesEligibilityRequest(key: integer; id : String; context : TFhirResource; resource: TFhirEligibilityRequest);
begin
  index(frtEligibilityRequest, key, 0, resource.identifierList, CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_identifier]);
  index(frtEligibilityRequest, key, 0, resource.createdElement, CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_created]);
  if resource.facility is TFhirIdentifier then
    index(frtEligibilityRequest, key, 0, TFhirIdentifier(resource.facility), CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_Facilityidentifier])
  else if resource.facility is TFhirReference then
    index(context, frtEligibilityRequest, key, 0, TFhirReference(resource.facility), CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_Facilityreference]);
  if resource.organization is TFhirIdentifier then
    index(frtEligibilityRequest, key, 0, TFhirIdentifier(resource.organization), CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_Organizationidentifier])
  else if resource.organization is TFhirReference then
    index(context, frtEligibilityRequest, key, 0, TFhirReference(resource.organization), CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_Organizationreference]);
  if resource.patientElement is TFhirIdentifier then
    index(frtEligibilityRequest, key, 0, TFhirIdentifier(resource.patientElement), CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_Patientidentifier])
  else if resource.patientElement is TFhirReference then
    index(context, frtEligibilityRequest, key, 0, TFhirReference(resource.patientElement), CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_Patientreference]);
  if resource.provider is TFhirIdentifier then
    index(frtEligibilityRequest, key, 0, TFhirIdentifier(resource.provider), CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_Provideridentifier])
  else if resource.provider is TFhirReference then
    index(context, frtEligibilityRequest, key, 0, TFhirReference(resource.provider), CODES_TSearchParamsEligibilityRequest[spEligibilityRequest_Providerreference]);
end;

const
  CHECK_TSearchParamsEligibilityResponse : Array[TSearchParamsEligibilityResponse] of TSearchParamsEligibilityResponse = ( spEligibilityResponse__content, spEligibilityResponse__id, spEligibilityResponse__lastUpdated, spEligibilityResponse__profile, spEligibilityResponse__query, spEligibilityResponse__security, spEligibilityResponse__tag, spEligibilityResponse__text,
    spEligibilityResponse_Created, spEligibilityResponse_Disposition, spEligibilityResponse_Identifier, spEligibilityResponse_Organizationidentifier, spEligibilityResponse_Organizationreference, spEligibilityResponse_Outcome, spEligibilityResponse_Requestidentifier, spEligibilityResponse_Requestorganizationidentifier, spEligibilityResponse_Requestorganizationreference, spEligibilityResponse_Requestprovideridentifier, spEligibilityResponse_Requestproviderreference, spEligibilityResponse_Requestreference);

procedure TFhirIndexInformation.buildIndexesEligibilityResponse;
var
  a : TSearchParamsEligibilityResponse;
begin
  for a := low(TSearchParamsEligibilityResponse) to high(TSearchParamsEligibilityResponse) do
  begin
    assert(CHECK_TSearchParamsEligibilityResponse[a] = a);
    indexes.add(frtEligibilityResponse, CODES_TSearchParamsEligibilityResponse[a], DESC_TSearchParamsEligibilityResponse[a], TYPES_TSearchParamsEligibilityResponse[a], TARGETS_TSearchParamsEligibilityResponse[a], PATHS_TSearchParamsEligibilityResponse[a], USES_TSearchParamsEligibilityResponse[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesEligibilityResponse(key: integer; id : String; context : TFhirResource; resource: TFhirEligibilityResponse);
begin
  index(frtEligibilityResponse, key, 0, resource.createdElement, CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_created]);
  index(frtEligibilityResponse, key, 0, resource.dispositionElement, CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_disposition]);
  index(frtEligibilityResponse, key, 0, resource.identifierList, CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_identifier]);
  index(frtEligibilityResponse, key, 0, resource.outcomeElement, CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_outcome]);

  if resource.request is TFhirIdentifier then
    index(frtEligibilityResponse, key, 0, TFhirIdentifier(resource.request), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Requestidentifier])
  else if resource.request is TFhirReference then
    index(context, frtEligibilityResponse, key, 0, TFhirReference(resource.request), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Requestreference]);
  if resource.organization is TFhirIdentifier then
    index(frtEligibilityResponse, key, 0, TFhirIdentifier(resource.organization), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Organizationidentifier])
  else if resource.organization is TFhirReference then
    index(context, frtEligibilityResponse, key, 0, TFhirReference(resource.organization), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Organizationreference]);
  if resource.requestOrganization is TFhirIdentifier then
    index(frtEligibilityResponse, key, 0, TFhirIdentifier(resource.requestOrganization), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Requestorganizationidentifier])
  else if resource.requestOrganization is TFhirReference then
    index(context, frtEligibilityResponse, key, 0, TFhirReference(resource.requestOrganization), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Requestorganizationreference]);
  if resource.requestProvider is TFhirIdentifier then
    index(frtEligibilityResponse, key, 0, TFhirIdentifier(resource.requestProvider), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Requestorganizationidentifier])
  else if resource.requestProvider is TFhirReference then
    index(context, frtEligibilityResponse, key, 0, TFhirReference(resource.requestProvider), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Requestorganizationreference]);
end;

const
  CHECK_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of TSearchParamsEnrollmentRequest = ( spEnrollmentRequest__content, spEnrollmentRequest__id, spEnrollmentRequest__lastUpdated, spEnrollmentRequest__profile, spEnrollmentRequest__query, spEnrollmentRequest__security, spEnrollmentRequest__tag, spEnrollmentRequest__text,
    spEnrollmentRequest_Identifier, spEnrollmentRequest_Patient, spEnrollmentRequest_Subject);

procedure TFhirIndexInformation.buildIndexesEnrollmentRequest;
var
  a : TSearchParamsEnrollmentRequest;
begin
  for a := low(TSearchParamsEnrollmentRequest) to high(TSearchParamsEnrollmentRequest) do
  begin
    assert(CHECK_TSearchParamsEnrollmentRequest[a] = a);
    indexes.add(frtEnrollmentRequest, CODES_TSearchParamsEnrollmentRequest[a], DESC_TSearchParamsEnrollmentRequest[a], TYPES_TSearchParamsEnrollmentRequest[a], TARGETS_TSearchParamsEnrollmentRequest[a], PATHS_TSearchParamsEnrollmentRequest[a], USES_TSearchParamsEnrollmentRequest[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesEnrollmentRequest(key: integer; id : String; context : TFhirResource; resource: TFhirEnrollmentRequest);
begin
  index(frtEnrollmentRequest, key, 0, resource.identifierList, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_identifier]);
  index(context, frtEnrollmentRequest, key, 0, resource.subject, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_subject]);
  index(context, frtEnrollmentRequest, key, 0, resource.subject, CODES_TSearchParamsEnrollmentRequest[spEnrollmentRequest_patient]);
  patientCompartment(key, resource.subject);
end;

const
  CHECK_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of TSearchParamsEnrollmentResponse = ( spEnrollmentResponse__content, spEnrollmentResponse__id, spEnrollmentResponse__lastUpdated, spEnrollmentResponse__profile, spEnrollmentResponse__query, spEnrollmentResponse__security, spEnrollmentResponse__tag, spEnrollmentResponse__text,
    spEnrollmentResponse_Identifier);

procedure TFhirIndexInformation.buildIndexesEnrollmentResponse;
var
  a : TSearchParamsEnrollmentResponse;
begin
  for a := low(TSearchParamsEnrollmentResponse) to high(TSearchParamsEnrollmentResponse) do
  begin
    assert(CHECK_TSearchParamsEnrollmentResponse[a] = a);
    indexes.add(frtEnrollmentResponse, CODES_TSearchParamsEnrollmentResponse[a], DESC_TSearchParamsEnrollmentResponse[a], TYPES_TSearchParamsEnrollmentResponse[a], TARGETS_TSearchParamsEnrollmentResponse[a], PATHS_TSearchParamsEnrollmentResponse[a], USES_TSearchParamsEnrollmentResponse[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesEnrollmentResponse(key: integer; id : String; context : TFhirResource; resource: TFhirEnrollmentResponse);
begin
  index(frtEnrollmentResponse, key, 0, resource.identifierList, CODES_TSearchParamsEnrollmentResponse[spEnrollmentResponse_identifier]);
end;

const
  CHECK_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of TSearchParamsExplanationOfBenefit = (
    spExplanationOfBenefit__content, spExplanationOfBenefit__id, spExplanationOfBenefit__lastUpdated, spExplanationOfBenefit__profile, spExplanationOfBenefit__query, spExplanationOfBenefit__security, spExplanationOfBenefit__tag, spExplanationOfBenefit__text,
    spExplanationOfBenefit_Claimindentifier, spExplanationOfBenefit_Claimreference, spExplanationOfBenefit_Created, spExplanationOfBenefit_Disposition, spExplanationOfBenefit_Facilityidentifier, spExplanationOfBenefit_Facilityreference, spExplanationOfBenefit_Identifier, spExplanationOfBenefit_Organizationidentifier, spExplanationOfBenefit_Organizationreference, spExplanationOfBenefit_Patientidentifier, spExplanationOfBenefit_Patientreference, spExplanationOfBenefit_Provideridentifier, spExplanationOfBenefit_Providerreference);

procedure TFhirIndexInformation.buildIndexesExplanationOfBenefit;
var
  a : TSearchParamsExplanationOfBenefit;
begin
  for a := low(TSearchParamsExplanationOfBenefit) to high(TSearchParamsExplanationOfBenefit) do
  begin
    assert(CHECK_TSearchParamsExplanationOfBenefit[a] = a);
    indexes.add(frtExplanationOfBenefit, CODES_TSearchParamsExplanationOfBenefit[a], DESC_TSearchParamsExplanationOfBenefit[a], TYPES_TSearchParamsExplanationOfBenefit[a], TARGETS_TSearchParamsExplanationOfBenefit[a], PATHS_TSearchParamsExplanationOfBenefit[a], USES_TSearchParamsExplanationOfBenefit[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesExplanationOfBenefit(key: integer; id : String; context : TFhirResource; resource: TFhirExplanationOfBenefit);
var
  i : integer;
begin
  index(frtExplanationOfBenefit, key, 0, resource.createdElement, CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_created]);
  index(frtExplanationOfBenefit, key, 0, resource.dispositionElement, CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_disposition]);
  index(frtExplanationOfBenefit, key, 0, resource.identifierList, CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_identifier]);
  if resource.claim is TFhirIdentifier then
    index(frtExplanationOfBenefit, key, 0, TFhirIdentifier(resource.claim), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Claimindentifier])
  else if resource.claim is TFhirReference then
    index(context, frtExplanationOfBenefit, key, 0, TFhirReference(resource.claim), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Claimreference]);
  if resource.facility is TFhirIdentifier then
    index(frtExplanationOfBenefit, key, 0, TFhirIdentifier(resource.facility), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Facilityidentifier])
  else if resource.facility is TFhirReference then
    index(context, frtExplanationOfBenefit, key, 0, TFhirReference(resource.facility), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Facilityreference]);
  if resource.organization is TFhirIdentifier then
    index(frtExplanationOfBenefit, key, 0, TFhirIdentifier(resource.organization), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Organizationidentifier])
  else if resource.organization is TFhirReference then
    index(context, frtExplanationOfBenefit, key, 0, TFhirReference(resource.organization), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Organizationreference]);
  if resource.patient is TFhirIdentifier then
    index(frtExplanationOfBenefit, key, 0, TFhirIdentifier(resource.patient), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Patientidentifier])
  else if resource.patient is TFhirReference then
    index(context, frtExplanationOfBenefit, key, 0, TFhirReference(resource.patient), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Patientreference]);
  if resource.provider is TFhirIdentifier then
    index(frtExplanationOfBenefit, key, 0, TFhirIdentifier(resource.provider), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Provideridentifier])
  else if resource.provider is TFhirReference then
    index(context, frtExplanationOfBenefit, key, 0, TFhirReference(resource.provider), CODES_TSearchParamsExplanationOfBenefit[spExplanationOfBenefit_Providerreference]);
end;

const
  CHECK_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of TSearchParamsPaymentNotice = ( spPaymentNotice__content, spPaymentNotice__id, spPaymentNotice__lastUpdated, spPaymentNotice__profile, spPaymentNotice__query, spPaymentNotice__security, spPaymentNotice__tag, spPaymentNotice__text,
    spPaymentNotice_Created, spPaymentNotice_Identifier, spPaymentNotice_Organizationidentifier, spPaymentNotice_Organizationreference, spPaymentNotice_Paymentstatus, spPaymentNotice_Provideridentifier, spPaymentNotice_Providerreference, spPaymentNotice_Requestidentifier, spPaymentNotice_Requestreference, spPaymentNotice_Responseidentifier, spPaymentNotice_Responsereference, spPaymentNotice_Statusdate);

procedure TFhirIndexInformation.buildIndexesPaymentNotice;
var
  a : TSearchParamsPaymentNotice;
begin
  for a := low(TSearchParamsPaymentNotice) to high(TSearchParamsPaymentNotice) do
  begin
    assert(CHECK_TSearchParamsPaymentNotice[a] = a);
    indexes.add(frtPaymentNotice, CODES_TSearchParamsPaymentNotice[a], DESC_TSearchParamsPaymentNotice[a], TYPES_TSearchParamsPaymentNotice[a], TARGETS_TSearchParamsPaymentNotice[a], PATHS_TSearchParamsPaymentNotice[a], USES_TSearchParamsPaymentNotice[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesPaymentNotice(key: integer; id : String; context : TFhirResource; resource: TFhirPaymentNotice);
var
  i : integer;
begin
  index(frtPaymentNotice, key, 0, resource.createdElement, CODES_TSearchParamsPaymentNotice[spPaymentNotice_created]);
  index(frtPaymentNotice, key, 0, resource.identifierList, CODES_TSearchParamsPaymentNotice[spPaymentNotice_identifier]);
  index(frtPaymentNotice, key, 0, resource.statusDateElement, CODES_TSearchParamsPaymentNotice[spPaymentNotice_statusdate]);
  index(frtPaymentNotice, key, 0, resource.paymentStatusElement, CODES_TSearchParamsPaymentNotice[spPaymentNotice_paymentstatus]);

{
  if resource.requestProvider is TFhirIdentifier then
    index(frtEligibilityResponse, key, 0, TFhirIdentifier(resource.requestProvider), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Requestorganizationidentifier])
  else if resource.requestProvider is TFhirReference then
    index(context, frtEligibilityResponse, key, 0, TFhirReference(resource.requestProvider), CODES_TSearchParamsEligibilityResponse[spEligibilityResponse_Requestorganizationreference]);
}
  if resource.organization is TFhirIdentifier then
    index(frtPaymentNotice, key, 0, TFhirIdentifier(resource.organization), CODES_TSearchParamsPaymentNotice[spPaymentNotice_Organizationidentifier])
  else if resource.organization is TFhirReference then
    index(context, frtPaymentNotice, key, 0, TFhirReference(resource.organization), CODES_TSearchParamsPaymentNotice[spPaymentNotice_Organizationreference]);
  if resource.provider is TFhirIdentifier then
    index(frtPaymentNotice, key, 0, TFhirIdentifier(resource.provider), CODES_TSearchParamsPaymentNotice[spPaymentNotice_Provideridentifier])
  else if resource.provider is TFhirReference then
    index(context, frtPaymentNotice, key, 0, TFhirReference(resource.provider), CODES_TSearchParamsPaymentNotice[spPaymentNotice_Providerreference]);
  if resource.request is TFhirIdentifier then
    index(frtPaymentNotice, key, 0, TFhirIdentifier(resource.request), CODES_TSearchParamsPaymentNotice[spPaymentNotice_Requestidentifier])
  else if resource.request is TFhirReference then
    index(context, frtPaymentNotice, key, 0, TFhirReference(resource.request), CODES_TSearchParamsPaymentNotice[spPaymentNotice_Requestreference]);
  if resource.response is TFhirIdentifier then
    index(frtPaymentNotice, key, 0, TFhirIdentifier(resource.response), CODES_TSearchParamsPaymentNotice[spPaymentNotice_Responseidentifier])
  else if resource.response is TFhirReference then
    index(context, frtPaymentNotice, key, 0, TFhirReference(resource.response), CODES_TSearchParamsPaymentNotice[spPaymentNotice_Responsereference]);
end;

const
  CHECK_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of TSearchParamsPaymentReconciliation = (
    spPaymentReconciliation__content, spPaymentReconciliation__id, spPaymentReconciliation__lastUpdated, spPaymentReconciliation__profile, spPaymentReconciliation__query, spPaymentReconciliation__security, spPaymentReconciliation__tag, spPaymentReconciliation__text,
    spPaymentReconciliation_Created, spPaymentReconciliation_Disposition, spPaymentReconciliation_Identifier, spPaymentReconciliation_Organizationidentifier, spPaymentReconciliation_Organizationreference, spPaymentReconciliation_Outcome, spPaymentReconciliation_Requestidentifier, spPaymentReconciliation_Requestorganizationidentifier, spPaymentReconciliation_Requestorganizationreference, spPaymentReconciliation_Requestprovideridentifier, spPaymentReconciliation_Requestproviderreference, spPaymentReconciliation_Requestreference);

procedure TFhirIndexInformation.buildIndexesPaymentReconciliation;
var
  a : TSearchParamsPaymentReconciliation;
begin
  for a := low(TSearchParamsPaymentReconciliation) to high(TSearchParamsPaymentReconciliation) do
  begin
    assert(CHECK_TSearchParamsPaymentReconciliation[a] = a);
    indexes.add(frtPaymentReconciliation, CODES_TSearchParamsPaymentReconciliation[a], DESC_TSearchParamsPaymentReconciliation[a], TYPES_TSearchParamsPaymentReconciliation[a], TARGETS_TSearchParamsPaymentReconciliation[a], PATHS_TSearchParamsPaymentReconciliation[a], USES_TSearchParamsPaymentReconciliation[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesPaymentReconciliation(key: integer; id : String; context : TFhirResource; resource: TFhirPaymentReconciliation);
var
  i : integer;
begin
  index(frtPaymentReconciliation, key, 0, resource.identifierList, CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_identifier]);
  index(frtPaymentReconciliation, key, 0, resource.createdElement, CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_created]);
  index(frtPaymentReconciliation, key, 0, resource.dispositionElement, CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_disposition]);
  index(frtPaymentReconciliation, key, 0, resource.outcomeElement, CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_outcome]);

  if resource.organization is TFhirIdentifier then
    index(frtPaymentReconciliation, key, 0, TFhirIdentifier(resource.organization), CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_Organizationidentifier])
  else if resource.organization is TFhirReference then
    index(context, frtPaymentReconciliation, key, 0, TFhirReference(resource.organization), CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_Organizationreference]);
  if resource.request is TFhirIdentifier then
    index(frtPaymentReconciliation, key, 0, TFhirIdentifier(resource.request), CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_Requestidentifier])
  else if resource.request is TFhirReference then
    index(context, frtPaymentReconciliation, key, 0, TFhirReference(resource.request), CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_Requestreference]);
  if resource.requestOrganization is TFhirIdentifier then
    index(frtPaymentReconciliation, key, 0, TFhirIdentifier(resource.requestOrganization), CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_Requestorganizationidentifier])
  else if resource.requestOrganization is TFhirReference then
    index(context, frtPaymentReconciliation, key, 0, TFhirReference(resource.requestOrganization), CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_Requestorganizationreference]);
  if resource.requestProvider is TFhirIdentifier then
    index(frtPaymentReconciliation, key, 0, TFhirIdentifier(resource.requestProvider), CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_Requestprovideridentifier])
  else if resource.requestProvider is TFhirReference then
    index(context, frtPaymentReconciliation, key, 0, TFhirReference(resource.requestProvider), CODES_TSearchParamsPaymentReconciliation[spPaymentReconciliation_Requestproviderreference]);
end;


const
  CHECK_TSearchParamsDecisionSupportRule : Array[TSearchParamsDecisionSupportRule] of TSearchParamsDecisionSupportRule = (
    spDecisionSupportRule__content, spDecisionSupportRule__id, spDecisionSupportRule__lastUpdated, spDecisionSupportRule__profile, spDecisionSupportRule__query, spDecisionSupportRule__security, spDecisionSupportRule__tag, spDecisionSupportRule__text,
    spDecisionSupportRule_Description, spDecisionSupportRule_Identifier, spDecisionSupportRule_Status, spDecisionSupportRule_Title, spDecisionSupportRule_Topic, spDecisionSupportRule_Version);

procedure TFhirIndexInformation.BuildIndexesDecisionSupportRule;
var
  a : TSearchParamsDecisionSupportRule;
begin
  for a := low(TSearchParamsDecisionSupportRule) to high(TSearchParamsDecisionSupportRule) do
  begin
    assert(CHECK_TSearchParamsDecisionSupportRule[a] = a);
    indexes.add(frtDecisionSupportRule, CODES_TSearchParamsDecisionSupportRule[a], DESC_TSearchParamsDecisionSupportRule[a], TYPES_TSearchParamsDecisionSupportRule[a], TARGETS_TSearchParamsDecisionSupportRule[a], PATHS_TSearchParamsDecisionSupportRule[a], USES_TSearchParamsDecisionSupportRule[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesDecisionSupportRule(key: integer; id : String; context : TFhirResource; resource: TFhirDecisionSupportRule);
begin
  index(frtDecisionSupportRule, key, 0, resource.moduleMetadata.DescriptionElement, CODES_TSearchParamsDecisionSupportRule[spDecisionSupportRule_Description]);
  index(frtDecisionSupportRule, key, 0, resource.moduleMetadata.identifierList, CODES_TSearchParamsDecisionSupportRule[spDecisionSupportRule_Identifier]);
  index(frtDecisionSupportRule, key, 0, resource.moduleMetadata.StatusElement, CODES_TSearchParamsDecisionSupportRule[spDecisionSupportRule_Status]);
  index(frtDecisionSupportRule, key, 0, resource.moduleMetadata.TitleElement, CODES_TSearchParamsDecisionSupportRule[spDecisionSupportRule_Title]);
  index(frtDecisionSupportRule, key, 0, resource.moduleMetadata.topicList, CODES_TSearchParamsDecisionSupportRule[spDecisionSupportRule_Topic]);
  index(frtDecisionSupportRule, key, 0, resource.moduleMetadata.VersionElement, CODES_TSearchParamsDecisionSupportRule[spDecisionSupportRule_Version]);
end;


const
  CHECK_TSearchParamsDecisionSupportServiceModule : Array[TSearchParamsDecisionSupportServiceModule] of TSearchParamsDecisionSupportServiceModule = (
    spDecisionSupportServiceModule__content, spDecisionSupportServiceModule__id, spDecisionSupportServiceModule__lastUpdated, spDecisionSupportServiceModule__profile, spDecisionSupportServiceModule__query, spDecisionSupportServiceModule__security, spDecisionSupportServiceModule__tag, spDecisionSupportServiceModule__text,
    spDecisionSupportServiceModule_Description, spDecisionSupportServiceModule_Identifier, spDecisionSupportServiceModule_Status, spDecisionSupportServiceModule_Title, spDecisionSupportServiceModule_Topic, spDecisionSupportServiceModule_Version);

procedure TFhirIndexInformation.BuildIndexesDecisionSupportServiceModule;
var
  a : TSearchParamsDecisionSupportServiceModule;
begin
  for a := low(TSearchParamsDecisionSupportServiceModule) to high(TSearchParamsDecisionSupportServiceModule) do
  begin
    assert(CHECK_TSearchParamsDecisionSupportServiceModule[a] = a);
    indexes.add(frtDecisionSupportServiceModule, CODES_TSearchParamsDecisionSupportServiceModule[a], DESC_TSearchParamsDecisionSupportServiceModule[a], TYPES_TSearchParamsDecisionSupportServiceModule[a], TARGETS_TSearchParamsDecisionSupportServiceModule[a], PATHS_TSearchParamsDecisionSupportServiceModule[a], USES_TSearchParamsDecisionSupportServiceModule[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesDecisionSupportServiceModule(key: integer; id : String; context : TFhirResource; resource: TFhirDecisionSupportServiceModule);
begin
  index(frtDecisionSupportServiceModule, key, 0, resource.moduleMetadata.DescriptionElement, CODES_TSearchParamsDecisionSupportServiceModule[spDecisionSupportServiceModule_Description]);
  index(frtDecisionSupportServiceModule, key, 0, resource.moduleMetadata.identifierList, CODES_TSearchParamsDecisionSupportServiceModule[spDecisionSupportServiceModule_Identifier]);
  index(frtDecisionSupportServiceModule, key, 0, resource.moduleMetadata.StatusElement, CODES_TSearchParamsDecisionSupportServiceModule[spDecisionSupportServiceModule_Status]);
  index(frtDecisionSupportServiceModule, key, 0, resource.moduleMetadata.TitleElement, CODES_TSearchParamsDecisionSupportServiceModule[spDecisionSupportServiceModule_Title]);
  index(frtDecisionSupportServiceModule, key, 0, resource.moduleMetadata.topicList, CODES_TSearchParamsDecisionSupportServiceModule[spDecisionSupportServiceModule_Topic]);
  index(frtDecisionSupportServiceModule, key, 0, resource.moduleMetadata.VersionElement, CODES_TSearchParamsDecisionSupportServiceModule[spDecisionSupportServiceModule_Version]);
end;


const
  CHECK_TSearchParamsExpansionProfile : Array[TSearchParamsExpansionProfile] of TSearchParamsExpansionProfile = (
    spExpansionProfile__content, spExpansionProfile__id, spExpansionProfile__lastUpdated, spExpansionProfile__profile, spExpansionProfile__query, spExpansionProfile__security, spExpansionProfile__tag, spExpansionProfile__text,
    spExpansionProfile_Date, spExpansionProfile_Description, spExpansionProfile_Identifier, spExpansionProfile_Name, spExpansionProfile_Publisher, spExpansionProfile_Status, spExpansionProfile_Url, spExpansionProfile_Version);

procedure TFhirIndexInformation.BuildIndexesExpansionProfile;
var
  a : TSearchParamsExpansionProfile;
begin
  for a := low(TSearchParamsExpansionProfile) to high(TSearchParamsExpansionProfile) do
  begin
    assert(CHECK_TSearchParamsExpansionProfile[a] = a);
    indexes.add(frtExpansionProfile, CODES_TSearchParamsExpansionProfile[a], DESC_TSearchParamsExpansionProfile[a], TYPES_TSearchParamsExpansionProfile[a], TARGETS_TSearchParamsExpansionProfile[a], PATHS_TSearchParamsExpansionProfile[a], USES_TSearchParamsExpansionProfile[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesExpansionProfile(key: integer; id : String; context : TFhirResource; resource: TFhirExpansionProfile);
begin
  index(frtExpansionProfile, key, 0, resource.dateElement, CODES_TSearchParamsExpansionProfile[spExpansionProfile_date]);
  index(frtExpansionProfile, key, 0, resource.descriptionElement, CODES_TSearchParamsExpansionProfile[spExpansionProfile_description]);
  index(frtExpansionProfile, key, 0, resource.identifierElement, CODES_TSearchParamsExpansionProfile[spExpansionProfile_identifier]);
  index(frtExpansionProfile, key, 0, resource.nameElement, CODES_TSearchParamsExpansionProfile[spExpansionProfile_name]);
  index(frtExpansionProfile, key, 0, resource.publisherElement, CODES_TSearchParamsExpansionProfile[spExpansionProfile_publisher]);
  index(frtExpansionProfile, key, 0, resource.urlElement, CODES_TSearchParamsExpansionProfile[spExpansionProfile_url]);
  index(frtExpansionProfile, key, 0, resource.versionElement, CODES_TSearchParamsExpansionProfile[spExpansionProfile_version]);
  index(frtExpansionProfile, key, 0, resource.statusElement, CODES_TSearchParamsExpansionProfile[spExpansionProfile_status]);
end;


const
  CHECK_TSearchParamsGuidanceResponse : Array[TSearchParamsGuidanceResponse] of TSearchParamsGuidanceResponse = (
    spGuidanceResponse__content, spGuidanceResponse__id, spGuidanceResponse__lastUpdated, spGuidanceResponse__profile, spGuidanceResponse__query, spGuidanceResponse__security, spGuidanceResponse__tag, spGuidanceResponse__text);

procedure TFhirIndexInformation.BuildIndexesGuidanceResponse;
var
  a : TSearchParamsGuidanceResponse;
begin
  for a := low(TSearchParamsGuidanceResponse) to high(TSearchParamsGuidanceResponse) do
  begin
    assert(CHECK_TSearchParamsGuidanceResponse[a] = a);
    indexes.add(frtGuidanceResponse, CODES_TSearchParamsGuidanceResponse[a], DESC_TSearchParamsGuidanceResponse[a], TYPES_TSearchParamsGuidanceResponse[a], TARGETS_TSearchParamsGuidanceResponse[a], PATHS_TSearchParamsGuidanceResponse[a], USES_TSearchParamsGuidanceResponse[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesGuidanceResponse(key: integer; id : String; context : TFhirResource; resource: TFhirGuidanceResponse);
begin
end;


const
  CHECK_TSearchParamsLibrary : Array[TSearchParamsLibrary] of TSearchParamsLibrary = (
    spLibrary__content, spLibrary__id, spLibrary__lastUpdated, spLibrary__profile, spLibrary__query, spLibrary__security, spLibrary__tag, spLibrary__text,
    spLibrary_Description, spLibrary_Identifier, spLibrary_Status, spLibrary_Title, spLibrary_Topic, spLibrary_Version);

procedure TFhirIndexInformation.BuildIndexesLibrary;
var
  a : TSearchParamsLibrary;
begin
  for a := low(TSearchParamsLibrary) to high(TSearchParamsLibrary) do
  begin
    assert(CHECK_TSearchParamsLibrary[a] = a);
    indexes.add(frtLibrary, CODES_TSearchParamsLibrary[a], DESC_TSearchParamsLibrary[a], TYPES_TSearchParamsLibrary[a], TARGETS_TSearchParamsLibrary[a], PATHS_TSearchParamsLibrary[a], USES_TSearchParamsLibrary[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesLibrary(key: integer; id : String; context : TFhirResource; resource: TFhirLibrary);
begin
  index(frtLibrary, key, 0, resource.moduleMetadata.DescriptionElement, CODES_TSearchParamsLibrary[spLibrary_Description]);
  index(frtLibrary, key, 0, resource.moduleMetadata.identifierList, CODES_TSearchParamsLibrary[spLibrary_Identifier]);
  index(frtLibrary, key, 0, resource.moduleMetadata.StatusElement, CODES_TSearchParamsLibrary[spLibrary_Status]);
  index(frtLibrary, key, 0, resource.moduleMetadata.TitleElement, CODES_TSearchParamsLibrary[spLibrary_Title]);
  index(frtLibrary, key, 0, resource.moduleMetadata.topicList, CODES_TSearchParamsLibrary[spLibrary_Topic]);
  index(frtLibrary, key, 0, resource.moduleMetadata.VersionElement, CODES_TSearchParamsLibrary[spLibrary_Version]);
end;


const
  CHECK_TSearchParamsMeasure : Array[TSearchParamsMeasure] of TSearchParamsMeasure = (
    spMeasure__content, spMeasure__id, spMeasure__lastUpdated, spMeasure__profile, spMeasure__query, spMeasure__security, spMeasure__tag, spMeasure__text,
    spMeasure_Description, spMeasure_Identifier, spMeasure_Status, spMeasure_Title, spMeasure_Topic, spMeasure_Version);

procedure TFhirIndexInformation.BuildIndexesMeasure;
var
  a : TSearchParamsMeasure;
begin
  for a := low(TSearchParamsMeasure) to high(TSearchParamsMeasure) do
  begin
    assert(CHECK_TSearchParamsMeasure[a] = a);
    indexes.add(frtMeasure, CODES_TSearchParamsMeasure[a], DESC_TSearchParamsMeasure[a], TYPES_TSearchParamsMeasure[a], TARGETS_TSearchParamsMeasure[a], PATHS_TSearchParamsMeasure[a], USES_TSearchParamsMeasure[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesMeasure(key: integer; id : String; context : TFhirResource; resource: TFhirMeasure);
begin
  index(frtMeasure, key, 0, resource.moduleMetadata.DescriptionElement, CODES_TSearchParamsMeasure[spMeasure_Description]);
  index(frtMeasure, key, 0, resource.moduleMetadata.identifierList, CODES_TSearchParamsMeasure[spMeasure_Identifier]);
  index(frtMeasure, key, 0, resource.moduleMetadata.StatusElement, CODES_TSearchParamsMeasure[spMeasure_Status]);
  index(frtMeasure, key, 0, resource.moduleMetadata.TitleElement, CODES_TSearchParamsMeasure[spMeasure_Title]);
  index(frtMeasure, key, 0, resource.moduleMetadata.topicList, CODES_TSearchParamsMeasure[spMeasure_Topic]);
  index(frtMeasure, key, 0, resource.moduleMetadata.VersionElement, CODES_TSearchParamsMeasure[spMeasure_Version]);
end;


const
  CHECK_TSearchParamsModuleDefinition : Array[TSearchParamsModuleDefinition] of TSearchParamsModuleDefinition = (
    spModuleDefinition__content, spModuleDefinition__id, spModuleDefinition__lastUpdated, spModuleDefinition__profile, spModuleDefinition__query, spModuleDefinition__security, spModuleDefinition__tag, spModuleDefinition__text);

procedure TFhirIndexInformation.BuildIndexesModuleDefinition;
var
  a : TSearchParamsModuleDefinition;
begin
  for a := low(TSearchParamsModuleDefinition) to high(TSearchParamsModuleDefinition) do
  begin
    assert(CHECK_TSearchParamsModuleDefinition[a] = a);
    indexes.add(frtModuleDefinition, CODES_TSearchParamsModuleDefinition[a], DESC_TSearchParamsModuleDefinition[a], TYPES_TSearchParamsModuleDefinition[a], TARGETS_TSearchParamsModuleDefinition[a], PATHS_TSearchParamsModuleDefinition[a], USES_TSearchParamsModuleDefinition[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesModuleDefinition(key: integer; id : String; context : TFhirResource; resource: TFhirModuleDefinition);
begin
end;


const
  CHECK_TSearchParamsOrderSet : Array[TSearchParamsOrderSet] of TSearchParamsOrderSet = (
    spOrderSet__content, spOrderSet__id, spOrderSet__lastUpdated, spOrderSet__profile, spOrderSet__query, spOrderSet__security, spOrderSet__tag, spOrderSet__text,
    spOrderSet_Description, spOrderSet_Identifier, spOrderSet_Status, spOrderSet_Title, spOrderSet_Topic, spOrderSet_Version);

procedure TFhirIndexInformation.BuildIndexesOrderSet;
var
  a : TSearchParamsOrderSet;
begin
  for a := low(TSearchParamsOrderSet) to high(TSearchParamsOrderSet) do
  begin
    assert(CHECK_TSearchParamsOrderSet[a] = a);
    indexes.add(frtOrderSet, CODES_TSearchParamsOrderSet[a], DESC_TSearchParamsOrderSet[a], TYPES_TSearchParamsOrderSet[a], TARGETS_TSearchParamsOrderSet[a], PATHS_TSearchParamsOrderSet[a], USES_TSearchParamsOrderSet[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesOrderSet(key: integer; id : String; context : TFhirResource; resource: TFhirOrderSet);
begin
  index(frtOrderSet, key, 0, resource.moduleMetadata.DescriptionElement, CODES_TSearchParamsOrderSet[spOrderSet_Description]);
  index(frtOrderSet, key, 0, resource.moduleMetadata.identifierList, CODES_TSearchParamsOrderSet[spOrderSet_Identifier]);
  index(frtOrderSet, key, 0, resource.moduleMetadata.StatusElement, CODES_TSearchParamsOrderSet[spOrderSet_Status]);
  index(frtOrderSet, key, 0, resource.moduleMetadata.TitleElement, CODES_TSearchParamsOrderSet[spOrderSet_Title]);
  index(frtOrderSet, key, 0, resource.moduleMetadata.topicList, CODES_TSearchParamsOrderSet[spOrderSet_Topic]);
  index(frtOrderSet, key, 0, resource.moduleMetadata.VersionElement, CODES_TSearchParamsOrderSet[spOrderSet_Version]);
end;

const
  CHECK_TSearchParamsProtocol : Array[TSearchParamsProtocol] of TSearchParamsProtocol = (
    spProtocol__content, spProtocol__id, spProtocol__lastUpdated, spProtocol__profile, spProtocol__query, spProtocol__security, spProtocol__tag, spProtocol__text,
    spProtocol_Identifier, spProtocol_Subject);

procedure TFhirIndexInformation.BuildIndexesProtocol;
var
  a : TSearchParamsProtocol;
begin
  for a := low(TSearchParamsProtocol) to high(TSearchParamsProtocol) do
  begin
    assert(CHECK_TSearchParamsProtocol[a] = a);
    indexes.add(frtProtocol, CODES_TSearchParamsProtocol[a], DESC_TSearchParamsProtocol[a], TYPES_TSearchParamsProtocol[a], TARGETS_TSearchParamsProtocol[a], PATHS_TSearchParamsProtocol[a], USES_TSearchParamsProtocol[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesProtocol(key: integer; id : String; context : TFhirResource; resource: TFhirProtocol);
begin
end;


const
  CHECK_TSearchParamsSequence : Array[TSearchParamsSequence] of TSearchParamsSequence = (
    spSequence__content, spSequence__id, spSequence__lastUpdated, spSequence__profile, spSequence__query, spSequence__security, spSequence__tag, spSequence__text,
    spSequence_Chromosome, spSequence_Coordinate, spSequence_End, spSequence_Patient, spSequence_Species, spSequence_Start, spSequence_Type);

procedure TFhirIndexInformation.BuildIndexesSequence;
var
  a : TSearchParamsSequence;
begin
  for a := low(TSearchParamsSequence) to high(TSearchParamsSequence) do
  begin
    assert(CHECK_TSearchParamsSequence[a] = a);
    indexes.add(frtSequence, CODES_TSearchParamsSequence[a], DESC_TSearchParamsSequence[a], TYPES_TSearchParamsSequence[a], TARGETS_TSearchParamsSequence[a], PATHS_TSearchParamsSequence[a], USES_TSearchParamsSequence[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesSequence(key: integer; id : String; context : TFhirResource; resource: TFhirSequence);
var
  c : TFhirSequenceReferenceSeq;
begin
  index(context, frtSequence, key, 0, resource.patient, CODES_TSearchParamsSequence[spSequence_patient]);
  index(frtSequence, key, 0, resource.species, CODES_TSearchParamsSequence[spSequence_species]);
  index(frtSequence, key, 0, resource.type_Element, CODES_TSearchParamsSequence[spSequence_type]);
  for c in resource.referenceSeqList do
    index(frtSequence, key, 0, c.chromosome, CODES_TSearchParamsSequence[spSequence_chromosome]);
  if resource.variation <> nil then
  begin
    index(frtSequence, key, 0, resource.variation.startElement, CODES_TSearchParamsSequence[spSequence_start]);
    index(frtSequence, key, 0, resource.variation.end_Element, CODES_TSearchParamsSequence[spSequence_end]);
// spSequence_Coordinate, {@enum.value "coordinate" spSequence_Coordinate Genomic coordinate of the sequence. For example, a search for sequence in region 1:123-345 can be represented as _coordinate=1$lt345$gt123_ }
  end;
end;


{$ENDIF}



{$IFDEF FHIR_DSTU3}
Const
  CHECK_TSearchParamsContract : Array[TSearchParamsContract] of TSearchParamsContract = ( spContract__content, spContract__id, spContract__lastUpdated, spContract__profile, spContract__query, spContract__security, spContract__tag, spContract__text,
    spContract_Agent, spContract_Authority, spContract_Domain, spContract_Identifier, spContract_Issued, spContract_Patient, spContract_Signer, spContract_Subject, spContract_Topic, spContract_Ttopic);

{$ELSE}
Const
  CHECK_TSearchParamsContract : Array[TSearchParamsContract] of TSearchParamsContract = ( spContract__content, spContract__id, spContract__lastUpdated, spContract__profile, spContract__query, spContract__security, spContract__tag, spContract__text,
    spContract_Actor, spContract_Identifier, spContract_Patient, spContract_Signer, spContract_Subject);
{$ENDIF}

procedure TFhirIndexInformation.buildIndexesContract;
var
  a : TSearchParamsContract;
begin
  for a := low(TSearchParamsContract) to high(TSearchParamsContract) do
  begin
    assert(CHECK_TSearchParamsContract[a] = a);
    indexes.add(frtContract, CODES_TSearchParamsContract[a], DESC_TSearchParamsContract[a], TYPES_TSearchParamsContract[a], TARGETS_TSearchParamsContract[a], PATHS_TSearchParamsContract[a], USES_TSearchParamsContract[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesContract(key: integer; id : String; context : TFhirResource; resource: TFhirContract);
var
  i  : integer;
begin
  {$IFDEF FHIR_DSTU3}
  for i := 0 to resource.topicList.Count - 1 do
  begin
    index(context, frtContract, key, 0, resource.topicList[i], CODES_TSearchParamsContract[spContract_topic]);
    index(context, frtContract, key, 0, resource.topicList[i], CODES_TSearchParamsContract[spContract_patient], frtPatient);
    patientCompartment(key, resource.topicList[i]);
  end;
  for i := 0 to resource.agentList.Count - 1 do
  begin
    index(context, frtContract, key, 0, resource.agentList[i].actor, CODES_TSearchParamsContract[spContract_agent]);
    practitionerCompartment(key, resource.agentList[i].actor);
    relatedPersonCompartment(key, resource.agentList[i].actor);
    deviceCompartment(key, resource.agentList[i].actor);
  end;
  {$ELSE}
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
  {$ENDIF}
  for i := 0 to resource.signerList.Count - 1 do
  begin
    index(context, frtContract, key, 0, resource.signerList[i].party, CODES_TSearchParamsContract[spContract_signer]);
    practitionerCompartment(key, resource.signerList[i].party);
    relatedPersonCompartment(key, resource.signerList[i].party);
    patientCompartment(key, resource.signerList[i].party);
  end;
  index(frtContract, key, 0, resource.identifier, CODES_TSearchParamsContract[spContract_identifier]);
  {$IFDEF FHIR_DSTU3}
  index(frtContract, key, 0, resource.issuedElement, CODES_TSearchParamsContract[spContract_issued]);
  index(context, frtContract, key, 0, resource.authorityList, CODES_TSearchParamsContract[spContract_authority]);
  index(context, frtContract, key, 0, resource.domainList, CODES_TSearchParamsContract[spContract_domain]);
  {$ENDIF}
end;

Const
  CHECK_TSearchParamsSupplyDelivery : Array[TSearchParamsSupplyDelivery] of TSearchParamsSupplyDelivery = ( spSupplyDelivery__content, spSupplyDelivery__id, spSupplyDelivery__lastUpdated, spSupplyDelivery__profile, spSupplyDelivery__query, spSupplyDelivery__security, spSupplyDelivery__tag, spSupplyDelivery__text,
    spSupplyDelivery_Identifier, spSupplyDelivery_Patient, spSupplyDelivery_Receiver, spSupplyDelivery_Status, spSupplyDelivery_Supplier);

procedure TFhirIndexInformation.buildIndexesSupplyDelivery;
var
  a : TSearchParamsSupplyDelivery;
begin
  for a := low(TSearchParamsSupplyDelivery) to high(TSearchParamsSupplyDelivery) do
  begin
    assert(CHECK_TSearchParamsSupplyDelivery[a] = a);
    indexes.add(frtSupplyDelivery, CODES_TSearchParamsSupplyDelivery[a], DESC_TSearchParamsSupplyDelivery[a], TYPES_TSearchParamsSupplyDelivery[a], TARGETS_TSearchParamsSupplyDelivery[a], PATHS_TSearchParamsSupplyDelivery[a], USES_TSearchParamsSupplyDelivery[a]);
  end;
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

Const
  CHECK_TSearchParamsSupplyRequest : Array[TSearchParamsSupplyRequest] of TSearchParamsSupplyRequest = ( spSupplyRequest__content, spSupplyRequest__id, spSupplyRequest__lastUpdated, spSupplyRequest__profile, spSupplyRequest__query, spSupplyRequest__security, spSupplyRequest__tag, spSupplyRequest__text,
    spSupplyRequest_Date, spSupplyRequest_Identifier, spSupplyRequest_Kind, spSupplyRequest_Patient, spSupplyRequest_Source, spSupplyRequest_Status, spSupplyRequest_Supplier);

procedure TFhirIndexInformation.buildIndexesSupplyRequest;
var
  a : TSearchParamsSupplyRequest;
begin
  for a := low(TSearchParamsSupplyRequest) to high(TSearchParamsSupplyRequest) do
  begin
    assert(CHECK_TSearchParamsSupplyRequest[a] = a);
    indexes.add(frtSupplyRequest, CODES_TSearchParamsSupplyRequest[a], DESC_TSearchParamsSupplyRequest[a], TYPES_TSearchParamsSupplyRequest[a], TARGETS_TSearchParamsSupplyRequest[a], PATHS_TSearchParamsSupplyRequest[a], USES_TSearchParamsSupplyRequest[a]);
  end;
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

Const
  CHECK_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of TSearchParamsRelatedPerson = (spRelatedPerson__content, spRelatedPerson__id, spRelatedPerson__lastUpdated, spRelatedPerson__profile, spRelatedPerson__query, spRelatedPerson__security, spRelatedPerson__tag, spRelatedPerson__text,
    spRelatedPerson_Address, spRelatedPerson_Addresscity, spRelatedPerson_Addresscountry, spRelatedPerson_Addresspostalcode, spRelatedPerson_Addressstate, spRelatedPerson_Addressuse,
    spRelatedPerson_Birthdate, spRelatedPerson_Email, spRelatedPerson_Gender, spRelatedPerson_Identifier, spRelatedPerson_Name, spRelatedPerson_Patient, spRelatedPerson_Phone, spRelatedPerson_Phonetic, spRelatedPerson_Telecom);

procedure TFhirIndexInformation.buildIndexesRelatedPerson;
var
  a : TSearchParamsRelatedPerson;
begin
  for a := low(TSearchParamsRelatedPerson) to high(TSearchParamsRelatedPerson) do
  begin
    assert(CHECK_TSearchParamsRelatedPerson[a] = a);
    indexes.add(frtRelatedPerson, CODES_TSearchParamsRelatedPerson[a], DESC_TSearchParamsRelatedPerson[a], TYPES_TSearchParamsRelatedPerson[a], TARGETS_TSearchParamsRelatedPerson[a], PATHS_TSearchParamsRelatedPerson[a], USES_TSearchParamsRelatedPerson[a]);
  end;
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
  FSpaces.FDB.sql := 'Select ResourceKey from Ids as i, Types as t where i.ResourceTypeKey = t.ResourceTypeKey and ResourceName = :t and Id = :id';
  FSpaces.FDB.Prepare;
  FSpaces.FDB.BindString('t', type_);
  FSpaces.FDB.BindString('id', id);
  FSpaces.FDB.Execute;
  if FSpaces.FDB.FetchNext then
    FPatientCompartments.add(key, FSpaces.FDB.ColIntegerByName['ResourceKey'], id)
  else
    FPatientCompartments.add(key, 0, id);
  FSpaces.FDB.Terminate;
end;

procedure TFhirIndexManager.patientCompartmentNot(key : integer; type_, id : String);
begin
  FPatientCompartments.removeById(id);
end;

procedure TFhirIndexManager.practitionerCompartment(key: integer; type_,
  id: String);
begin

end;

procedure TFhirIndexManager.practitionerCompartment(key: integer;
  reference: TFhirReference);
begin

end;

procedure TFhirIndexManager.practitionerCompartmentNot(key: integer; type_,
  id: String);
begin

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

function TFhirIndexManager.index(aType: TFhirResourceType; key, parent: integer; name: String): Integer;
var
  ndx : TFhirComposite;
begin
  ndx := FInfo.FComposites.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown composite index '+name+' on type '+CODES_TFHIRResourceType[aType]);
  if (ndx.Key = 0) then
    raise Exception.create('unknown composite index '+ndx.Name);
  result := FEntries.add(key, parent, ndx);
end;

procedure TFhirIndexManager.index(context: TFhirResource; aType: TFhirResourceType; key, parent: integer; value: TFhirReferenceList; name: String; specificType : TFhirResourceType = frtNull);
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


// --------- actual indexes -----------------------------------------------------------------------------------------------

Const
  {$IFDEF FHIR_DSTU3}
  CHECK_TSearchParamsConformance : Array[TSearchParamsConformance] of TSearchParamsConformance = (
    spConformance__content, spConformance__id, spConformance__lastUpdated, spConformance__profile, spConformance__query, spConformance__security, spConformance__tag,  spConformance__text,
    spConformance_Context, spConformance_Date, spConformance_Description, spConformance_Event, spConformance_Fhirversion, spConformance_Format, spConformance_Mode, spConformance_Name, spConformance_Publisher, spConformance_Resource, spConformance_Resourceprofile, spConformance_Securityservice, spConformance_Software, spConformance_Status, spConformance_Supportedprofile, spConformance_Url, spConformance_Version);
  {$ELSE}
  CHECK_TSearchParamsConformance : Array[TSearchParamsConformance] of TSearchParamsConformance = (
    spConformance__content, spConformance__id, spConformance__lastUpdated, spConformance__profile, spConformance__query, spConformance__security, spConformance__tag,  spConformance__text,spConformance_Date, spConformance_Description, spConformance_Event, spConformance_Fhirversion,
    spConformance_Format, spConformance_Mode, spConformance_Name, spConformance_Profile, spConformance_Publisher, spConformance_Resource, spConformance_Security, spConformance_Software, spConformance_Status, spConformance_Supportedprofile,
    spConformance_Url, spConformance_Version);
  {$ENDIF}

procedure TFhirIndexInformation.buildIndexesConformance();
var
  a : TSearchParamsConformance;
begin
  for a := low(TSearchParamsConformance) to high(TSearchParamsConformance) do
  begin
    assert(CHECK_TSearchParamsConformance[a] = a);
    indexes.add(frtConformance, CODES_TSearchParamsConformance[a], DESC_TSearchParamsConformance[a], TYPES_TSearchParamsConformance[a], TARGETS_TSearchParamsConformance[a], PATHS_TSearchParamsConformance[a], USES_TSearchParamsConformance[a]);
  end;
end;

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
  {$IFDEF FHIR_DSTU3}
  index(frtConformance, key, 0, resource.useContextList, CODES_TSearchParamsConformance[spConformance_Context]);
  {$ENDIF}


  for j := 0 to resource.formatList.Count - 1 do
    index(frtConformance, key, 0, resource.formatList[j], CODES_TSearchParamsConformance[spConformance_format]);

  for j := 0 to resource.restList.Count - 1 do
  begin
    if resource.restList[j].security <> nil then
    begin
      for i := 0 to resource.restList[j].security.serviceList.count - 1 do
        index(frtConformance, key, 0, resource.restList[j].security.serviceList[i], CODES_TSearchParamsConformance[{$IFDEF FHIR_DSTU3}spConformance_securityservice{$ELSE}spConformance_security{$ENDIF}]);
    end;
  end;


  for j := 0 to resource.restList.Count - 1 do
  begin
    for i := 0 to resource.restList[j].resourceList.count - 1 do
    begin
      index(context, frtConformance, key, 0, resource.restList[j].resourceList[i].profile, CODES_TSearchParamsConformance[{$IFDEF FHIR_DSTU3} spConformance_resourceprofile{$ELSE} spConformance_profile {$ENDIF}]);
      index(frtConformance, key, 0, resource.restList[j].resourceList[i].type_Element, CODES_TSearchParamsConformance[spConformance_resource]);
    end;
    index(frtConformance, key, 0, resource.restList[j].modeElement, CODES_TSearchParamsConformance[spConformance_mode]);
  end;

  for j := 0 to resource.messagingList.Count - 1 Do
  begin
    for i := 0 to resource.messagingList[j].EventList.count - 1 do
    begin
      index(frtConformance, key, 0, resource.messagingList[j].EventList[i].focusElement, CODES_TSearchParamsConformance[spConformance_resource]);
      {$IFDEF FHIR_DSTU2}
      index(context, frtConformance, key, 0, resource.messagingList[j].EventList[i].request, CODES_TSearchParamsConformance[spConformance_profile]);
      index(context, frtConformance, key, 0, resource.messagingList[j].EventList[i].response, CODES_TSearchParamsConformance[spConformance_profile]);
      {$ENDIF}
      index(frtConformance, key, 0, resource.messagingList[j].EventList[i].modeElement, CODES_TSearchParamsConformance[spConformance_mode]);
      index(frtConformance, key, 0, resource.messagingList[j].EventList[i].code, CODES_TSearchParamsConformance[spConformance_event]);
    end;
  end;

  for i := 0 to resource.DocumentList.count - 1 do
    index(context, frtConformance, key, 0, resource.DocumentList[i].profile, CODES_TSearchParamsConformance[{$IFDEF FHIR_DSTU3} spConformance_resourceprofile {$ELSE} spConformance_profile {$ENDIF}]);
  for i := 0 to resource.profileList.count - 1 do
    index(context, frtConformance, key, 0, resource.ProfileList[i], CODES_TSearchParamsConformance[{$IFDEF FHIR_DSTU3} spConformance_supportedprofile {$ELSE} spConformance_profile {$ENDIF}]);

end;

{ TFhirCompositionIndexManager }

Const
  CHECK_TSearchParamsComposition : Array[TSearchParamsComposition] of TSearchParamsComposition = ( spComposition__content, spComposition__id, spComposition__lastUpdated, spComposition__profile, spComposition__query, spComposition__security, spComposition__tag, spComposition__text,
    spComposition_Attester, spComposition_Author, spComposition_Class, spComposition_Confidentiality, spComposition_Context, spComposition_Date, spComposition_Encounter, spComposition_Entry, spComposition_Identifier, spComposition_Patient, spComposition_Period, spComposition_Section, spComposition_Status, spComposition_Subject, spComposition_Title, spComposition_Type);


procedure TFhirIndexInformation.buildIndexesComposition;
var
  a : TSearchParamsComposition;
begin
  for a := low(TSearchParamsComposition) to high(TSearchParamsComposition) do
  begin
    assert(CHECK_TSearchParamsComposition[a] = a);
    indexes.add(frtComposition, CODES_TSearchParamsComposition[a], DESC_TSearchParamsComposition[a], TYPES_TSearchParamsComposition[a], TARGETS_TSearchParamsComposition[a], PATHS_TSearchParamsComposition[a], USES_TSearchParamsComposition[a]);
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


Const
  CHECK_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of TSearchParamsMessageHeader = ( spMessageHeader__content, spMessageHeader__id, spMessageHeader__lastUpdated, spMessageHeader__profile, spMessageHeader__query, spMessageHeader__security, spMessageHeader__tag, spMessageHeader__text,
    spMessageHeader_Author, spMessageHeader_Code, spMessageHeader_Data, spMessageHeader_Destination, spMessageHeader_Destinationuri, spMessageHeader_Enterer, spMessageHeader_Event,
    spMessageHeader_Receiver, spMessageHeader_Responseid, spMessageHeader_Responsible, spMessageHeader_Source, spMessageHeader_Sourceuri, spMessageHeader_Target, spMessageHeader_Timestamp);


procedure TFhirIndexInformation.buildIndexesMessageHeader;
var
  a : TSearchParamsMessageHeader;
begin
  for a := low(TSearchParamsMessageHeader) to high(TSearchParamsMessageHeader) do
  begin
    assert(CHECK_TSearchParamsMessageHeader[a] = a);
    indexes.add(frtMessageHeader, CODES_TSearchParamsMessageHeader[a], DESC_TSearchParamsMessageHeader[a], TYPES_TSearchParamsMessageHeader[a], TARGETS_TSearchParamsMessageHeader[a], PATHS_TSearchParamsMessageHeader[a], USES_TSearchParamsMessageHeader[a]);
  end;
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


Const
  CHECK_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of TSearchParamsPractitioner = (spPractitioner__content, spPractitioner__id, spPractitioner__lastUpdated, spPractitioner__profile, spPractitioner__query, spPractitioner__security, spPractitioner__tag, spPractitioner__text,
    spPractitioner_Address, spPractitioner_Addresscity, spPractitioner_Addresscountry, spPractitioner_Addresspostalcode, spPractitioner_Addressstate, spPractitioner_Addressuse, spPractitioner_Communication, spPractitioner_Email, spPractitioner_Family, spPractitioner_Gender, spPractitioner_Given, spPractitioner_Identifier, spPractitioner_Location, spPractitioner_Name, spPractitioner_Organization, spPractitioner_Phone, spPractitioner_Phonetic, spPractitioner_Role, spPractitioner_Specialty, spPractitioner_Telecom);


procedure TFhirIndexInformation.buildIndexesPractitioner;
var
  a : TSearchParamsPractitioner;
begin
  for a := low(TSearchParamsPractitioner) to high(TSearchParamsPractitioner) do
  begin
    assert(CHECK_TSearchParamsPractitioner[a] = a);
    indexes.add(frtPractitioner, CODES_TSearchParamsPractitioner[a], DESC_TSearchParamsPractitioner[a], TYPES_TSearchParamsPractitioner[a], TARGETS_TSearchParamsPractitioner[a], PATHS_TSearchParamsPractitioner[a], USES_TSearchParamsPractitioner[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesPractitioner(key : integer; id : String; context : TFhirResource; resource: TFhirPractitioner);
var
  i, j : integer;
begin
  for i := 0 to resource.identifierList.count - 1 do
    index(frtPractitioner, key, 0, resource.identifierList[i], CODES_TSearchParamsPractitioner[spPractitioner_identifier]);
  {$IFDEF FHIR_DSTU3}
  for i := 0 to resource.nameList.count - 1 do
  begin
    index(frtPractitioner, key, 0, resource.nameList[i], 'name', CODES_TSearchParamsPractitioner[spPractitioner_phonetic]);
    for j := 0 to resource.nameList[i].givenList.count - 1 do
      index(frtPractitioner, key, 0, resource.nameList[i].givenList[j], CODES_TSearchParamsPractitioner[spPractitioner_given]);
    for j := 0 to resource.nameList[i].familyList.count - 1 do
      index(frtPractitioner, key, 0, resource.nameList[i].familyList[j], CODES_TSearchParamsPractitioner[spPractitioner_family]);
  end;
  {$ELSE}
  if (resource.name <> nil) then
  begin
    index(frtPractitioner, key, 0, resource.name, 'name', CODES_TSearchParamsPractitioner[spPractitioner_phonetic]);
    for j := 0 to resource.name.givenList.count - 1 do
      index(frtPractitioner, key, 0, resource.name.givenList[j], CODES_TSearchParamsPractitioner[spPractitioner_given]);
    for j := 0 to resource.name.familyList.count - 1 do
      index(frtPractitioner, key, 0, resource.name.familyList[j], CODES_TSearchParamsPractitioner[spPractitioner_family]);
  end;
  {$ENDIF}
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
    index(context, frtPractitioner, key, 0, resource.practitionerRoleList[j].{$IFDEF FHIR_DSTU2}managingOrganization{$ELSE}Organization{$ENDIF}, CODES_TSearchParamsPractitioner[spPractitioner_organization]);
    index(frtPractitioner, key, 0, resource.practitionerRoleList[j].roleElement, CODES_TSearchParamsPractitioner[spPractitioner_role]);
    for i := 0 to resource.practitionerRoleList[j].specialtyList.count - 1 do
      index(frtPractitioner, key, 0, resource.practitionerRoleList[j].specialtyList[i], CODES_TSearchParamsPractitioner[spPractitioner_specialty]);
  end;
end;


Const
  CHECK_TSearchParamsOrganization : Array[TSearchParamsOrganization] of TSearchParamsOrganization = ( spOrganization__content, spOrganization__id, spOrganization__lastUpdated, spOrganization__profile, spOrganization__query, spOrganization__security, spOrganization__tag, spOrganization__text,
    spOrganization_Active, spOrganization_Address, spOrganization_Addresscity, spOrganization_Addresscountry, spOrganization_Addresspostalcode, spOrganization_Addressstate, spOrganization_Addressuse, spOrganization_Identifier, spOrganization_Name, spOrganization_Partof, spOrganization_Phonetic, spOrganization_Type);



procedure TFhirIndexInformation.buildIndexesOrganization;
var
  a : TSearchParamsOrganization;
begin
  for a := low(TSearchParamsOrganization) to high(TSearchParamsOrganization) do
  begin
    assert(CHECK_TSearchParamsOrganization[a] = a);
    indexes.add(frtOrganization, CODES_TSearchParamsOrganization[a], DESC_TSearchParamsOrganization[a], TYPES_TSearchParamsOrganization[a], TARGETS_TSearchParamsOrganization[a], PATHS_TSearchParamsOrganization[a], USES_TSearchParamsOrganization[a]);
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


Const
  CHECK_TSearchParamsGroup : Array[TSearchParamsGroup] of TSearchParamsGroup = ( spGroup__content, spGroup__id, spGroup__lastUpdated, spGroup__profile, spGroup__query, spGroup__security, spGroup__tag, spGroup__text,
     spGroup_Actual, spGroup_Characteristic, spGroup_Characteristicvalue, spGroup_Code, spGroup_Exclude, spGroup_Identifier, spGroup_Member, spGroup_Type, spGroup_Value);


procedure TFhirIndexInformation.buildIndexesGroup;
var
  a : TSearchParamsGroup;
begin
  for a := low(TSearchParamsGroup) to high(TSearchParamsGroup) do
  begin
    assert(CHECK_TSearchParamsGroup[a] = a);
    indexes.add(frtGroup, CODES_TSearchParamsGroup[a], DESC_TSearchParamsGroup[a], TYPES_TSearchParamsGroup[a], TARGETS_TSearchParamsGroup[a], PATHS_TSearchParamsGroup[a], USES_TSearchParamsGroup[a]);
  end;
  composites.add(frtGroup, 'characteristic', ['value', 'value', 'code', 'characteristic']);
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

Const
  CHECK_TSearchParamsObservation : Array[TSearchParamsObservation] of TSearchParamsObservation = (
    spObservation__content, spObservation__id, spObservation__lastUpdated, spObservation__profile, spObservation__query, spObservation__security, spObservation__tag,  spObservation__text,
    spObservation_Category, spObservation_Code, spObservation_Codevaluex, spObservation_Componentcode, spObservation_Componentcodevaluex, spObservation_Componentdataabsentreason, spObservation_Componentvalueconcept, spObservation_Componentvaluequantity, spObservation_Componentvaluestring, spObservation_Dataabsentreason, spObservation_Date,
    spObservation_Device, spObservation_Encounter, spObservation_Identifier, spObservation_Patient, spObservation_Performer, spObservation_Related, spObservation_Relatedtarget, spObservation_Relatedtype, spObservation_Specimen, spObservation_Status, spObservation_Subject, spObservation_Valueconcept, spObservation_Valuedate, spObservation_Valuequantity, spObservation_Valuestring);


procedure TFhirIndexInformation.buildIndexesObservation;
var
  a : TSearchParamsObservation;
begin
  for a := low(TSearchParamsObservation) to high(TSearchParamsObservation) do
  begin
    assert(CHECK_TSearchParamsObservation[a] = a);
    indexes.add(frtObservation, CODES_TSearchParamsObservation[a], DESC_TSearchParamsObservation[a], TYPES_TSearchParamsObservation[a], TARGETS_TSearchParamsObservation[a], PATHS_TSearchParamsObservation[a], USES_TSearchParamsObservation[a]);
  end;
  composites.add(frtObservation, 'related', ['target', 'related-target', 'type', 'related-type']);
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

Const
  CHECK_TSearchParamsStructureDefinition : Array[TSearchParamsStructureDefinition] of TSearchParamsStructureDefinition = (
    spStructureDefinition__content, spStructureDefinition__id, spStructureDefinition__lastUpdated, spStructureDefinition__profile, spStructureDefinition__query, spStructureDefinition__security, spStructureDefinition__tag, spStructureDefinition__text,
    spStructureDefinition_Abstract, spStructureDefinition_Base, spStructureDefinition_Basepath, spStructureDefinition_Code, spStructureDefinition_Context, spStructureDefinition_Contexttype, spStructureDefinition_Date, spStructureDefinition_Derivation, spStructureDefinition_Description, spStructureDefinition_Display, spStructureDefinition_Experimental, spStructureDefinition_Extcontext,
    spStructureDefinition_Identifier, spStructureDefinition_Kind, spStructureDefinition_Name, spStructureDefinition_Path, spStructureDefinition_Publisher, spStructureDefinition_Status, spStructureDefinition_Type, spStructureDefinition_Url, spStructureDefinition_Valueset, spStructureDefinition_Version);

procedure TFhirIndexInformation.buildIndexesStructureDefinition;
var
  a : TSearchParamsStructureDefinition;
begin
  for a := low(TSearchParamsStructureDefinition) to high(TSearchParamsStructureDefinition) do
  begin
    assert(CHECK_TSearchParamsStructureDefinition[a] = a);
    indexes.add(frtStructureDefinition, CODES_TSearchParamsStructureDefinition[a], DESC_TSearchParamsStructureDefinition[a], TYPES_TSearchParamsStructureDefinition[a], TARGETS_TSearchParamsStructureDefinition[a], PATHS_TSearchParamsStructureDefinition[a], USES_TSearchParamsStructureDefinition[a]);
  end;
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
  {$IFDEF FHIR_DSTU2}
  index(frtStructureDefinition, key, 0, resource.baseElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_base]);
  index(frtStructureDefinition, key, 0, resource.constrainedTypeElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_type]);
  {$ELSE}
  index(frtStructureDefinition, key, 0, resource.baseDefinitionElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_base]);
  index(frtStructureDefinition, key, 0, resource.baseTypeElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_type]);
  index(frtStructureDefinition, key, 0, resource.derivationElement, CODES_TSearchParamsStructureDefinition[spStructureDefinition_Derivation]);
  {$ENDIF}
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
    begin
      index(frtStructureDefinition, key, 0, resource.differential.elementList[j].path, CODES_TSearchParamsStructureDefinition[spStructureDefinition_path]);
      indexElement(resource.differential.elementList[j]);
    end;
end;



Const
  CHECK_TSearchParamsPatient : Array[TSearchParamsPatient] of TSearchParamsPatient = (
    spPatient__content, spPatient__id, spPatient__lastUpdated, spPatient__profile, spPatient__query, spPatient__security, spPatient__tag, spPatient__text,
    spPatient_Active, spPatient_Address, spPatient_Addresscity, spPatient_Addresscountry, spPatient_Addresspostalcode, spPatient_Addressstate, spPatient_Addressuse, spPatient_Animalbreed, spPatient_Animalspecies, spPatient_Birthdate, spPatient_Careprovider,
    spPatient_Deathdate, spPatient_Deceased, spPatient_Email, spPatient_Ethnicity, spPatient_Family, spPatient_Gender, spPatient_Given, spPatient_Identifier, spPatient_Language, spPatient_Link, spPatient_Name, spPatient_Organization, spPatient_Phone, spPatient_Phonetic, spPatient_Race, spPatient_Telecom);

procedure TFhirIndexInformation.buildIndexesPatient;
var
  a : TSearchParamsPatient;
begin
  for a := low(TSearchParamsPatient) to high(TSearchParamsPatient) do
  begin
    assert(CHECK_TSearchParamsPatient[a] = a);
    indexes.add(frtPatient, CODES_TSearchParamsPatient[a], DESC_TSearchParamsPatient[a], TYPES_TSearchParamsPatient[a], TARGETS_TSearchParamsPatient[a], PATHS_TSearchParamsPatient[a], USES_TSearchParamsPatient[a]);
  end;
  composites.add(frtPatient, 'name', ['given', 'given', 'family', 'family']);
  // DAF:
  indexes.add(frtPatient, 'mothersMaidenName', 'Search based on Patient mother''s Maiden Name', SearchParamTypeString, [], '', SearchXpathUsageNull, 'http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-mothersMaidenName');
  indexes.add(frtPatient, 'birthOrderBoolean', 'Search based on Patient''s birth order (boolean or integer)', SearchParamTypeString, [], '', SearchXpathUsageNull, 'http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-mothersMaidenName');
  indexes.add(frtPatient, 'age', 'Search based on Patient''s age', SearchParamTypeNumber, [], '', SearchXpathUsageNull, 'http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-age');
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

Const
  CHECK_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of TSearchParamsDiagnosticReport = (
    spDiagnosticReport__content, spDiagnosticReport__id, spDiagnosticReport__lastUpdated, spDiagnosticReport__profile, spDiagnosticReport__query, spDiagnosticReport__security, spDiagnosticReport__tag, spDiagnosticReport__text,
    spDiagnosticReport_Category, spDiagnosticReport_Code, spDiagnosticReport_Date, spDiagnosticReport_Diagnosis, spDiagnosticReport_Encounter, spDiagnosticReport_Identifier, spDiagnosticReport_Image, spDiagnosticReport_Issued, spDiagnosticReport_Patient, spDiagnosticReport_Performer, spDiagnosticReport_Request, spDiagnosticReport_Result, spDiagnosticReport_Specimen, spDiagnosticReport_Status, spDiagnosticReport_Subject);

procedure TFhirIndexInformation.buildIndexesDiagnosticReport;
var
  a : TSearchParamsDiagnosticReport;
begin
  for a := low(TSearchParamsDiagnosticReport) to high(TSearchParamsDiagnosticReport) do
  begin
    assert(CHECK_TSearchParamsDiagnosticReport[a] = a);
    indexes.add(frtDiagnosticReport, CODES_TSearchParamsDiagnosticReport[a], DESC_TSearchParamsDiagnosticReport[a], TYPES_TSearchParamsDiagnosticReport[a], TARGETS_TSearchParamsDiagnosticReport[a], PATHS_TSearchParamsDiagnosticReport[a], USES_TSearchParamsDiagnosticReport[a]);
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


Const
  CHECK_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of TSearchParamsDiagnosticOrder = ( spDiagnosticOrder__content, spDiagnosticOrder__id, spDiagnosticOrder__lastUpdated, spDiagnosticOrder__profile, spDiagnosticOrder__query, spDiagnosticOrder__security, spDiagnosticOrder__tag, spDiagnosticOrder__text,
    spDiagnosticOrder_Actor, spDiagnosticOrder_Bodysite, spDiagnosticOrder_Code, spDiagnosticOrder_Encounter, spDiagnosticOrder_Eventdate, spDiagnosticOrder_Eventstatus, spDiagnosticOrder_Eventstatusdate, spDiagnosticOrder_Identifier, spDiagnosticOrder_Itemdate, spDiagnosticOrder_Itempaststatus, spDiagnosticOrder_Itemstatus, spDiagnosticOrder_Itemstatusdate, spDiagnosticOrder_Orderer, spDiagnosticOrder_Patient, spDiagnosticOrder_Specimen, spDiagnosticOrder_Status, spDiagnosticOrder_Subject);



procedure TFhirIndexInformation.buildIndexesDiagnosticOrder;
var
  a : TSearchParamsDiagnosticOrder;
begin
  for a := low(TSearchParamsDiagnosticOrder) to high(TSearchParamsDiagnosticOrder) do
  begin
    assert(CHECK_TSearchParamsDiagnosticOrder[a] = a);
    indexes.add(frtDiagnosticOrder, CODES_TSearchParamsDiagnosticOrder[a], DESC_TSearchParamsDiagnosticOrder[a], TYPES_TSearchParamsDiagnosticOrder[a], TARGETS_TSearchParamsDiagnosticOrder[a], PATHS_TSearchParamsDiagnosticOrder[a], USES_TSearchParamsDiagnosticOrder[a]);
  end;
  composites.add(frtDiagnosticOrder, 'event', ['status', 'event-status', 'date', 'event-date']);
  composites.add(frtDiagnosticOrder, 'item', ['status', 'item-status', 'code', 'item-code', 'site', 'bodysite', 'event', 'item-event']);
  composites.add(frtDiagnosticOrder, 'item-event', ['status', 'item-past-status', 'date', 'item-date', 'actor', 'actor']);
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


const
  CHECK_TSearchParamsValueSet : Array[TSearchParamsValueSet] of TSearchParamsValueSet = (
    spValueSet__content, spValueSet__id, spValueSet__lastUpdated, spValueSet__profile, spValueSet__query, spValueSet__security, spValueSet__tag, spValueSet__text,
    {$IFDEF FHIR_DSTU2}
    spValueSet_Code, spValueSet_Context, spValueSet_Date, spValueSet_Description, spValueSet_Expansion, spValueSet_Identifier, spValueSet_Name, spValueSet_Publisher, spValueSet_Reference, spValueSet_Status, spValueSet_System, spValueSet_Url, spValueSet_Version);
    {$ELSE}
    spValueSet_Context, spValueSet_Date, spValueSet_Description, spValueSet_Expansion, spValueSet_Identifier, spValueSet_Name, spValueSet_Publisher, spValueSet_Reference, spValueSet_Status, spValueSet_Url, spValueSet_Version);
    {$ENDIF}


procedure TFhirIndexInformation.buildIndexesValueset;
var
  a : TSearchParamsValueset;
begin
  for a := low(TSearchParamsValueset) to high(TSearchParamsValueset) do
  begin
    assert(CHECK_TSearchParamsValueSet[a] = a);
    indexes.add(frtValueset, CODES_TSearchParamsValueset[a], DESC_TSearchParamsValueset[a], TYPES_TSearchParamsValueset[a], TARGETS_TSearchParamsValueset[a], PATHS_TSearchParamsValueset[a], USES_TSearchParamsValueset[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesValueset(key : integer; id : String; context : TFhirResource; resource: TFhirValueset);
  {$IFDEF FHIR_DSTU2}
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
  {$ENDIF}
var
  i : integer;
begin
  {$IFDEF FHIR_DSTU2}
  if (resource.codeSystem <> nil) then
  begin
    index(frtValueSet, key, 0, resource.codeSystem.systemElement, CODES_TSearchParamsValueSet[spValueSet_system]);
    indexConcepts(resource.codeSystem.conceptList);
  end;
  {$ENDIF}

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

{$IFDEF FHIR_DSTU3}
const
  CHECK_TSearchParamsCodeSystem : Array[TSearchParamsCodeSystem] of TSearchParamsCodeSystem = (
    spCodeSystem__content, spCodeSystem__id, spCodeSystem__lastUpdated, spCodeSystem__profile, spCodeSystem__query, spCodeSystem__security, spCodeSystem__tag, spCodeSystem__text,
    spCodeSystem_Code, spCodeSystem_Context, spCodeSystem_Date, spCodeSystem_Description, spCodeSystem_Identifier, spCodeSystem_Language, spCodeSystem_Name, spCodeSystem_Publisher, spCodeSystem_Status, spCodeSystem_System, spCodeSystem_Url, spCodeSystem_Version);

procedure TFhirIndexInformation.buildIndexesCodeSystem;
var
  a : TSearchParamsCodeSystem;
begin
  for a := low(TSearchParamsCodeSystem) to high(TSearchParamsCodeSystem) do
  begin
    assert(CHECK_TSearchParamsCodeSystem[a] = a);
    indexes.add(frtCodeSystem, CODES_TSearchParamsCodeSystem[a], DESC_TSearchParamsCodeSystem[a], TYPES_TSearchParamsCodeSystem[a], TARGETS_TSearchParamsCodeSystem[a], PATHS_TSearchParamsCodeSystem[a], USES_TSearchParamsCodeSystem[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesCodeSystem(key : integer; id : String; context : TFhirResource; resource: TFhirCodeSystem);
  procedure indexConcepts(list : TFhirCodeSystemConceptList);
  var
    i : integer;
  begin
    for i := 0 to list.Count - 1 do
    begin
      index(frtCodeSystem, key, 0, list[i].codeElement, CODES_TSearchParamsCodeSystem[spCodeSystem_code]);
      indexConcepts(list[i].conceptList);
    end;
  end;
var
  i : integer;
begin
  index(frtCodeSystem, key, 0, resource.identifierElement, CODES_TSearchParamsCodeSystem[spCodeSystem_identifier]);
  index(frtCodeSystem, key, 0, resource.versionElement, CODES_TSearchParamsCodeSystem[spCodeSystem_version]);
  index(frtCodeSystem, key, 0, resource.nameElement, CODES_TSearchParamsCodeSystem[spCodeSystem_name]);

  index(frtCodeSystem, key, 0, resource.statusElement, CODES_TSearchParamsCodeSystem[spCodeSystem_status]);
  index(frtCodeSystem, key, 0, resource.urlElement, CODES_TSearchParamsCodeSystem[spCodeSystem_url]);
  index(frtCodeSystem, key, 0, resource.useContextList, CODES_TSearchParamsCodeSystem[spCodeSystem_context]);

  index(frtCodeSystem, key, 0, resource.dateElement, CODES_TSearchParamsCodeSystem[spCodeSystem_date]);
  index(frtCodeSystem, key, 0, resource.publisherElement, CODES_TSearchParamsCodeSystem[spCodeSystem_publisher]);
  index(frtCodeSystem, key, 0, resource.descriptionElement, CODES_TSearchParamsCodeSystem[spCodeSystem_description]);
  indexConcepts(resource.conceptList);
end;

const
  CHECK_TSearchParamsLinkage : Array[TSearchParamsLinkage] of TSearchParamsLinkage = (
    spLinkage__content, spLinkage__id, spLinkage__lastUpdated, spLinkage__profile, spLinkage__query, spLinkage__security, spLinkage__tag, spLinkage__text,
    spLinkage_Author, spLinkage_Item, spLinkage_Source);


procedure TFhirIndexInformation.buildIndexesLinkage;
var
  a : TSearchParamsLinkage;
begin
  for a := low(TSearchParamsLinkage) to high(TSearchParamsLinkage) do
  begin
    assert(CHECK_TSearchParamsLinkage[a] = a);
    indexes.add(frtLinkage, CODES_TSearchParamsLinkage[a], DESC_TSearchParamsLinkage[a], TYPES_TSearchParamsLinkage[a], TARGETS_TSearchParamsLinkage[a], PATHS_TSearchParamsLinkage[a], USES_TSearchParamsLinkage[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesLinkage(key : integer; id : String; context : TFhirResource; resource: TFhirLinkage);
var
  i : TFhirLinkageItem;
begin
  index(context, frtLinkage, key, 0, resource.author, CODES_TSearchParamsLinkage[spLinkage_author]);
  for i in resource.itemList do
  begin
    index(context, frtLinkage, key, 0, i.resource, CODES_TSearchParamsLinkage[spLinkage_item]);
    if i.type_ = LinkageTypeSource then
      index(context, frtLinkage, key, 0, i.resource, CODES_TSearchParamsLinkage[spLinkage_source]);
  end;
end;
{$ENDIF}

const
  CHECK_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of TSearchParamsConceptMap = (
    spConceptMap__content, spConceptMap__id, spConceptMap__lastUpdated, spConceptMap__profile, spConceptMap__query, spConceptMap__security, spConceptMap__tag, spConceptMap__text,
    spConceptMap_Context, spConceptMap_Date, spConceptMap_Dependson, spConceptMap_Description, spConceptMap_Identifier, spConceptMap_Name, spConceptMap_Product, spConceptMap_Publisher, spConceptMap_Source,
    spConceptMap_Sourcecode, spConceptMap_Sourcesystem, spConceptMap_Sourceuri, spConceptMap_Status, spConceptMap_Target, spConceptMap_Targetcode, spConceptMap_Targetsystem, spConceptMap_Url, spConceptMap_Version);

procedure TFhirIndexInformation.buildIndexesConceptMap;
var
  a : TSearchParamsConceptMap;
begin
  for a := low(TSearchParamsConceptMap) to high(TSearchParamsConceptMap) do
  begin
    assert(CHECK_TSearchParamsConceptMap[a] = a);
    indexes.add(frtConceptMap, CODES_TSearchParamsConceptMap[a], DESC_TSearchParamsConceptMap[a], TYPES_TSearchParamsConceptMap[a], TARGETS_TSearchParamsConceptMap[a], PATHS_TSearchParamsConceptMap[a], USES_TSearchParamsConceptMap[a]);
  end;
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



const
  CHECK_TSearchParamsDevice : Array[TSearchParamsDevice] of TSearchParamsDevice = ( spDevice__content, spDevice__id, spDevice__lastUpdated, spDevice__profile, spDevice__query, spDevice__security, spDevice__tag, spDevice__text,
    spDevice_Identifier, spDevice_Location, spDevice_Manufacturer, spDevice_Model, spDevice_Organization, spDevice_Patient, spDevice_Type, {$IFDEF FHIR_DSTU2}spDevice_Udi{$ELSE}spDevice_Udicarrier{$ENDIF}, spDevice_Url);


procedure TFhirIndexInformation.buildIndexesDevice;
var
  a : TSearchParamsDevice;
begin
  for a := low(TSearchParamsDevice) to high(TSearchParamsDevice) do
  begin
    assert(CHECK_TSearchParamsDevice[a] = a);
    indexes.add(frtDevice, CODES_TSearchParamsDevice[a], DESC_TSearchParamsDevice[a], TYPES_TSearchParamsDevice[a], TARGETS_TSearchParamsDevice[a], PATHS_TSearchParamsDevice[a], USES_TSearchParamsDevice[a]);
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
  {$IFDEF FHIR_DSTU2}
  index(frtDevice, key, 0, resource.udiElement, CODES_TSearchParamsDevice[spDevice_udi]);
  {$ELSE}
  index(frtDevice, key, 0, resource.udiCarrierElement, CODES_TSearchParamsDevice[spDevice_udicarrier]);
  {$ENDIF}
  index(frtDevice, key, 0, resource.urlElement, CODES_TSearchParamsDevice[spDevice_url]);
  index(frtDevice, key, 0, resource.type_, CODES_TSearchParamsDevice[spDevice_type]);
  patientCompartment(key, resource.patient);
end;

const
  CHECK_TSearchParamsAuditEvent : Array[TSearchParamsAuditEvent] of TSearchParamsAuditEvent = (
    spAuditEvent__content, spAuditEvent__id, spAuditEvent__lastUpdated, spAuditEvent__profile, spAuditEvent__query, spAuditEvent__security, spAuditEvent__tag, spAuditEvent__text,
  {$IFDEF FHIR_DSTU2}
    spAuditEvent_Action, spAuditEvent_Address, spAuditEvent_Altid, spAuditEvent_Date, spAuditEvent_Desc, spAuditEvent_Identity, spAuditEvent_Name, spAuditEvent_Objecttype, spAuditEvent_Participant, spAuditEvent_Patient, spAuditEvent_Policy, spAuditEvent_Reference, spAuditEvent_Site, spAuditEvent_Source, spAuditEvent_Subtype, spAuditEvent_Type, spAuditEvent_User);
  {$ELSE}
    spAuditEvent_Action, spAuditEvent_Address, spAuditEvent_Agent, spAuditEvent_Agentname, spAuditEvent_Altid, spAuditEvent_Date, spAuditEvent_Entity, spAuditEvent_Entityid, spAuditEvent_Entityname, spAuditEvent_Entitytype, spAuditEvent_Patient, spAuditEvent_Policy, spAuditEvent_Site, spAuditEvent_Source, spAuditEvent_Subtype, spAuditEvent_Type, spAuditEvent_User);
  {$ENDIF}

procedure TFhirIndexInformation.buildIndexesAuditEvent;
var
  a : TSearchParamsAuditEvent;
begin
  for a := low(TSearchParamsAuditEvent) to high(TSearchParamsAuditEvent) do
  begin
    assert(CHECK_TSearchParamsAuditEvent[a] = a);
    indexes.add(frtAuditEvent, CODES_TSearchParamsAuditEvent[a], DESC_TSearchParamsAuditEvent[a], TYPES_TSearchParamsAuditEvent[a], TARGETS_TSearchParamsAuditEvent[a], PATHS_TSearchParamsAuditEvent[a], USES_TSearchParamsAuditEvent[a]);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesAuditEvent(key : integer; id : String; context : TFhirResource; resource: TFhirAuditEvent);
var
  i, j : integer;
begin
  {$IFDEF FHIR_DSTU3}
  index(frtAuditEvent, key, 0, resource.actionElement, CODES_TSearchParamsAuditEvent[spAuditEvent_action]);
  index(frtAuditEvent, key, 0, resource.recordedElement, CODES_TSearchParamsAuditEvent[spAuditEvent_date]);

  index(frtAuditEvent, key, 0, resource.type_, CODES_TSearchParamsAuditEvent[spAuditEvent_type]);
  for i := 0 to resource.subTypeList.count - 1 do
    index(frtAuditEvent, key, 0, resource.subtypeList[i], CODES_TSearchParamsAuditEvent[spAuditEvent_subtype]);

  for i := 0 to resource.agentList.count - 1 do
  begin
    if resource.agentList[i].network <> nil then
      index(frtAuditEvent, key, 0, resource.agentList[i].network.addressElement, CODES_TSearchParamsAuditEvent[spAuditEvent_address]);
    index(context, frtAuditEvent, key, 0, resource.agentList[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_agent]);
    index(frtAuditEvent, key, 0, resource.agentList[i].nameElement, CODES_TSearchParamsAuditEvent[spAuditEvent_agentname]);
    index(frtAuditEvent, key, 0, resource.agentList[i].altIdElement, CODES_TSearchParamsAuditEvent[spAuditEvent_altid]);
    index(context, frtAuditEvent, key, 0, resource.agentList[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_patient], frtPatient);
    for j := 0 to resource.agentList[i].policyList.Count - 1 do
      index(frtAuditEvent, key, 0, resource.agentList[i].policyList[j], CODES_TSearchParamsAuditEvent[spAuditEvent_policy]);
    index(frtAuditEvent, key, 0, resource.agentList[i].userIdElement, CODES_TSearchParamsAuditEvent[spAuditEvent_user]);

    patientCompartment(key, resource.agentList[i].reference);
    deviceCompartment(key, resource.agentList[i].reference);
    practitionerCompartment(key, resource.agentList[i].reference);
  end;

  if resource.source <> nil Then
  begin
    index(frtAuditEvent, key, 0, resource.source.siteElement, CODES_TSearchParamsAuditEvent[spAuditEvent_site]);
    index(frtAuditEvent, key, 0, resource.source.identifierElement, CODES_TSearchParamsAuditEvent[spAuditEvent_source]);
  end;

  for i := 0 to resource.entityList.count - 1 do
  begin
    index(context, frtAuditEvent, key, 0, resource.entityList[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_entity]);
    index(context, frtAuditEvent, key, 0, resource.entityList[i].reference, CODES_TSearchParamsAuditEvent[spAuditEvent_patient], frtPatient);
    index(frtAuditEvent, key, 0, resource.entityList[i].identifier, CODES_TSearchParamsAuditEvent[spAuditEvent_entityid]);
    index(frtAuditEvent, key, 0, resource.entityList[i].nameElement, CODES_TSearchParamsAuditEvent[spAuditEvent_entityname]);
    index(frtAuditEvent, key, 0, resource.entityList[i].type_, CODES_TSearchParamsAuditEvent[spAuditEvent_entitytype]);
    patientCompartment(key, resource.entityList[i].reference);
  end;

{$ELSE}
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
  {$ENDIF}
end;



const
  CHECK_TSearchParamsCondition : Array[TSearchParamsCondition] of TSearchParamsCondition = (
    spCondition__content, spCondition__id, spCondition__lastUpdated, spCondition__profile, spCondition__query, spCondition__security, spCondition__tag, spCondition__text,
    spCondition_Age, spCondition_Asserter, spCondition_Bodysite, spCondition_Category, spCondition_Clinicalstatus, spCondition_Code, spCondition_Daterecorded, spCondition_Encounter, spCondition_Evidence, spCondition_Identifier, spCondition_Onset, spCondition_Onsetinfo, spCondition_Patient, spCondition_Severity, spCondition_Stage);

procedure TFhirIndexInformation.buildIndexesCondition;
var
  a : TSearchParamsCondition;
begin
  for a := low(TSearchParamsCondition) to high(TSearchParamsCondition) do
  begin
    assert(CHECK_TSearchParamsCondition[a] = a);
    indexes.add(frtCondition, CODES_TSearchParamsCondition[a], DESC_TSearchParamsCondition[a], TYPES_TSearchParamsCondition[a], TARGETS_TSearchParamsCondition[a], PATHS_TSearchParamsCondition[a], USES_TSearchParamsCondition[a]);
  end;
  // DAF
  indexes.add(frtCondition, 'identifier', 'identifier', SearchParamTypeToken, [], '', SearchXpathUsageNull);
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

const
  CHECK_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of TSearchParamsOperationOutcome = (spOperationOutcome__content, spOperationOutcome__id, spOperationOutcome__lastUpdated, spOperationOutcome__profile, spOperationOutcome__query, spOperationOutcome__security, spOperationOutcome__tag, spOperationOutcome__text);

procedure TFhirIndexInformation.buildIndexesOperationOutcome;
var
  a : TSearchParamsOperationOutcome;
begin
  for a := low(TSearchParamsOperationOutcome) to high(TSearchParamsOperationOutcome) do
  begin
    assert(CHECK_TSearchParamsOperationOutcome[a] = a);
    indexes.add(frtOperationOutcome, CODES_TSearchParamsOperationOutcome[a], DESC_TSearchParamsOperationOutcome[a], TYPES_TSearchParamsOperationOutcome[a], TARGETS_TSearchParamsOperationOutcome[a], PATHS_TSearchParamsOperationOutcome[a], USES_TSearchParamsOperationOutcome[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesOperationOutcome(key : integer; id : String; context : TFhirResource; resource: TFhirOperationOutcome);
begin
end;



const
  CHECK_TSearchParamsBinary : Array[TSearchParamsBinary] of TSearchParamsBinary = (spBinary__content, spBinary__id, spBinary__lastUpdated, spBinary__profile, spBinary__query, spBinary__security, spBinary__tag,  spBinary__text,spBinary_Contenttype);


procedure TFhirIndexInformation.buildIndexesBinary;
var
  a : TSearchParamsBinary;
begin
  for a := low(TSearchParamsBinary) to high(TSearchParamsBinary) do
  begin
    assert(CHECK_TSearchParamsBinary[a] = a);
    indexes.add(frtBinary, CODES_TSearchParamsBinary[a], DESC_TSearchParamsBinary[a], TYPES_TSearchParamsBinary[a], TARGETS_TSearchParamsBinary[a], PATHS_TSearchParamsBinary[a], USES_TSearchParamsBinary[a]);
  end;

end;

procedure TFhirIndexManager.buildIndexValuesBinary(key : integer; id : String; context : TFhirResource; resource: TFhirBinary);
begin

  index(frtBinary, key, 0, resource.contentType, CODES_TSearchParamsBinary[spBinary_contentType]);

end;

const
  CHECK_TSearchParamsProvenance : Array[TSearchParamsProvenance] of TSearchParamsProvenance = (spProvenance__content, spProvenance__id, spProvenance__lastUpdated, spProvenance__profile, spProvenance__query, spProvenance__security, spProvenance__tag, spProvenance__text,
    {$IFDEF FHIR_DSTU2}
    spProvenance_Agent, spProvenance_End, spProvenance_Entity, spProvenance_Entitytype, spProvenance_Location, spProvenance_Patient, spProvenance_Sigtype, spProvenance_Start, spProvenance_Target, spProvenance_Userid);
    {$ELSE}
    spProvenance_Agent, spProvenance_End, spProvenance_Entity, spProvenance_Entitytype, spProvenance_Location, spProvenance_Patient, spProvenance_Sig, spProvenance_Start, spProvenance_Target, spProvenance_Userid);
    {$ENDIF}

procedure TFhirIndexInformation.buildIndexesProvenance;
var
  a : TSearchParamsProvenance;
begin
  for a := low(TSearchParamsProvenance) to high(TSearchParamsProvenance) do
  begin
    assert(CHECK_TSearchParamsProvenance[a] = a);
    indexes.add(frtProvenance, CODES_TSearchParamsProvenance[a], DESC_TSearchParamsProvenance[a], TYPES_TSearchParamsProvenance[a], TARGETS_TSearchParamsProvenance[a], PATHS_TSearchParamsProvenance[a], USES_TSearchParamsProvenance[a]);
  end;
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
    index(frtProvenance, key, 0, resource.signatureList[i].type_List, CODES_TSearchParamsProvenance[{$IFDEF FHIR_DSTU2}spProvenance_sigtype{$ELSE}spProvenance_sig{$ENDIF}]);

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


const
  {$IFDEF FHIR_DSTU3}
  CHECK_TSearchParamsMedication : Array[TSearchParamsMedication] of TSearchParamsMedication = ( spMedication__content, spMedication__id, spMedication__lastUpdated, spMedication__profile, spMedication__query, spMedication__security, spMedication__tag, spMedication__text,
    spMedication_Code, spMedication_Container, spMedication_Form, spMedication_Ingredient, spMedication_Ingredientcode, spMedication_Manufacturer, spMedication_Packageitem, spMedication_Packageitemcode);
  {$ELSE}
  CHECK_TSearchParamsMedication : Array[TSearchParamsMedication] of TSearchParamsMedication = ( spMedication__content, spMedication__id, spMedication__lastUpdated, spMedication__profile, spMedication__query, spMedication__security, spMedication__tag, spMedication__text,
     spMedication_Code, spMedication_Container, spMedication_Content, spMedication_Form, spMedication_Ingredient, spMedication_Manufacturer);
  {$ENDIF}


procedure TFhirIndexInformation.buildIndexesMedication;
var
  a : TSearchParamsMedication;
begin
  for a := low(TSearchParamsMedication) to high(TSearchParamsMedication) do
  begin
    assert(CHECK_TSearchParamsMedication[a] = a);
    indexes.add(frtMedication, CODES_TSearchParamsMedication[a], DESC_TSearchParamsMedication[a], TYPES_TSearchParamsMedication[a], TARGETS_TSearchParamsMedication[a], PATHS_TSearchParamsMedication[a], USES_TSearchParamsMedication[a]);
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
      {$IFDEF FHIR_DSTU3}
      if resource.package.contentList[i].item is TFhirReference then
        index(context, frtMedication, key, 0, TFhirReference(resource.package.contentList[i].item), CODES_TSearchParamsMedication[spMedication_packageitem])
      else if resource.package.contentList[i].item is TFhirCodeableConcept then
        index(frtMedication, key, 0, TFhirCodeableConcept(resource.package.contentList[i].item), CODES_TSearchParamsMedication[spMedication_Packageitemcode])
      {$ELSE}
      index(context, frtMedication, key, 0, resource.package.contentList[i].item, CODES_TSearchParamsMedication[spMedication_content]);
      {$ENDIF}
  end;
  if (resource.product <> nil) then
  begin
    index(frtMedication, key, 0, resource.product.form, CODES_TSearchParamsMedication[spMedication_form]);
    for i := 0 to resource.product.ingredientList.count - 1 do
      if resource.product.ingredientList[i].item is TFhirReference then
        index(context, frtMedication, key, 0, TFhirReference(resource.product.ingredientList[i].item), CODES_TSearchParamsMedication[spMedication_Ingredient])
      else if resource.product.ingredientList[i].item is TFhirCodeableConcept then
        index(frtMedication, key, 0, TFhirCodeableConcept(resource.product.ingredientList[i].item), CODES_TSearchParamsMedication[spMedication_Ingredientcode])
  end;
end;


const
  {$IFDEF FHIR_DSTU3}
  CHECK_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of TSearchParamsMedicationAdministration = ( spMedicationAdministration__content, spMedicationAdministration__id, spMedicationAdministration__lastUpdated, spMedicationAdministration__profile, spMedicationAdministration__query, spMedicationAdministration__security, spMedicationAdministration__tag, spMedicationAdministration__text,
     spMedicationAdministration_Code, spMedicationAdministration_Device, spMedicationAdministration_Effectivetime, spMedicationAdministration_Encounter, spMedicationAdministration_Identifier, spMedicationAdministration_Medication, spMedicationAdministration_Patient, spMedicationAdministration_Practitioner, spMedicationAdministration_Prescription, spMedicationAdministration_Status, spMedicationAdministration_Wasnotgiven);
  {$ELSE}
  CHECK_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of TSearchParamsMedicationAdministration = ( spMedicationAdministration__content, spMedicationAdministration__id, spMedicationAdministration__lastUpdated, spMedicationAdministration__profile, spMedicationAdministration__query, spMedicationAdministration__security, spMedicationAdministration__tag, spMedicationAdministration__text,
     spMedicationAdministration_Code, spMedicationAdministration_Device, spMedicationAdministration_Effectivetime, spMedicationAdministration_Encounter, spMedicationAdministration_Identifier, spMedicationAdministration_Medication, spMedicationAdministration_Notgiven, spMedicationAdministration_Patient, spMedicationAdministration_Practitioner, spMedicationAdministration_Prescription, spMedicationAdministration_Status);
  {$ENDIF}





procedure TFhirIndexInformation.buildIndexesMedicationAdministration;
var
  a : TSearchParamsMedicationAdministration;
begin
  for a := low(TSearchParamsMedicationAdministration) to high(TSearchParamsMedicationAdministration) do
  begin
    assert(CHECK_TSearchParamsMedicationAdministration[a] = a);
    indexes.add(frtMedicationAdministration, CODES_TSearchParamsMedicationAdministration[a], DESC_TSearchParamsMedicationAdministration[a], TYPES_TSearchParamsMedicationAdministration[a], TARGETS_TSearchParamsMedicationAdministration[a], PATHS_TSearchParamsMedicationAdministration[a], USES_TSearchParamsMedicationAdministration[a]);
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
  index(frtMedicationAdministration, key, 0, resource.wasNotGiven, CODES_TSearchParamsMedicationAdministration[{$IFDEF FHIR_DSTU3} spMedicationAdministration_wasnotgiven {$ELSE} spMedicationAdministration_notgiven {$ENDIF}]);
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

const
  CHECK_TSearchParamsMedicationOrder : Array[TSearchParamsMedicationOrder] of TSearchParamsMedicationOrder = ( spMedicationOrder__content, spMedicationOrder__id, spMedicationOrder__lastUpdated, spMedicationOrder__profile, spMedicationOrder__query, spMedicationOrder__security, spMedicationOrder__tag, spMedicationOrder__text,
    spMedicationOrder_Code, spMedicationOrder_Datewritten, spMedicationOrder_Encounter, spMedicationOrder_Identifier, spMedicationOrder_Medication, spMedicationOrder_Patient, spMedicationOrder_Prescriber, spMedicationOrder_Status);


procedure TFhirIndexInformation.buildIndexesMedicationOrder;
var
  a : TSearchParamsMedicationOrder;
begin
  for a := low(TSearchParamsMedicationOrder) to high(TSearchParamsMedicationOrder) do
  begin
    assert(CHECK_TSearchParamsMedicationOrder[a] = a);
    indexes.add(frtMedicationOrder, CODES_TSearchParamsMedicationOrder[a], DESC_TSearchParamsMedicationOrder[a], TYPES_TSearchParamsMedicationOrder[a], TARGETS_TSearchParamsMedicationOrder[a], PATHS_TSearchParamsMedicationOrder[a], USES_TSearchParamsMedicationOrder[a]);
  end;
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

const
  CHECK_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of TSearchParamsMedicationDispense = ( spMedicationDispense__content, spMedicationDispense__id, spMedicationDispense__lastUpdated, spMedicationDispense__profile, spMedicationDispense__query, spMedicationDispense__security, spMedicationDispense__tag, spMedicationDispense__text,
    spMedicationDispense_Code, spMedicationDispense_Destination, spMedicationDispense_Dispenser, spMedicationDispense_Identifier, spMedicationDispense_Medication, spMedicationDispense_Patient, spMedicationDispense_Prescription, spMedicationDispense_Receiver,
    spMedicationDispense_Responsibleparty, spMedicationDispense_Status, spMedicationDispense_Type, spMedicationDispense_Whenhandedover, spMedicationDispense_Whenprepared);


procedure TFhirIndexInformation.buildIndexesMedicationDispense;
var
  a : TSearchParamsMedicationDispense;
begin
  for a := low(TSearchParamsMedicationDispense) to high(TSearchParamsMedicationDispense) do
  begin
    assert(CHECK_TSearchParamsMedicationDispense[a] = a);
    indexes.add(frtMedicationDispense, CODES_TSearchParamsMedicationDispense[a], DESC_TSearchParamsMedicationDispense[a], TYPES_TSearchParamsMedicationDispense[a], TARGETS_TSearchParamsMedicationDispense[a], PATHS_TSearchParamsMedicationDispense[a], USES_TSearchParamsMedicationDispense[a]);
  end;
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

const
  CHECK_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of TSearchParamsMedicationStatement = (
    spMedicationStatement__content, spMedicationStatement__id, spMedicationStatement__lastUpdated, spMedicationStatement__profile, spMedicationStatement__query, spMedicationStatement__security, spMedicationStatement__tag, spMedicationStatement__text,
    spMedicationStatement_Code,  {$IFDEF FHIR_DSTU3}spMedicationStatement_Effective {$ELSE} spMedicationStatement_Effectivedate{$ENDIF},  spMedicationStatement_Identifier,  spMedicationStatement_Medication,  spMedicationStatement_Patient,  spMedicationStatement_Source,  spMedicationStatement_Status);


procedure TFhirIndexInformation.buildIndexesMedicationStatement;
var
  a : TSearchParamsMedicationStatement;
begin
  for a := low(TSearchParamsMedicationStatement) to high(TSearchParamsMedicationStatement) do
  begin
    assert(CHECK_TSearchParamsMedicationStatement[a] = a);
    indexes.add(frtMedicationStatement, CODES_TSearchParamsMedicationStatement[a], DESC_TSearchParamsMedicationStatement[a], TYPES_TSearchParamsMedicationStatement[a], TARGETS_TSearchParamsMedicationStatement[a], PATHS_TSearchParamsMedicationStatement[a], USES_TSearchParamsMedicationStatement[a]);
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
    index(frtMedicationStatement, key, 0, TFhirPeriod(resource.effectiveElement), CODES_TSearchParamsMedicationStatement[{$IFDEF FHIR_DSTU3}spMedicationStatement_Effective {$ELSE} spMedicationStatement_Effectivedate{$ENDIF}])
  else
    index(frtMedicationStatement, key, 0, TFhirDateTime(resource.effectiveElement), CODES_TSearchParamsMedicationStatement[{$IFDEF FHIR_DSTU3}spMedicationStatement_Effective {$ELSE} spMedicationStatement_Effectivedate{$ENDIF}]);

end;


const
  {$IFDEF FHIR_DSTU3}
  CHECK_TSearchParamsList : Array[TSearchParamsList] of TSearchParamsList = ( spList__content, spList__id, spList__lastUpdated, spList__profile, spList__query, spList__security, spList__tag, spList__text,
    spList_Code, spList_Date, spList_Emptyreason, spList_Encounter, spList_Identifier, spList_Item, spList_Notes, spList_Patient, spList_Source, spList_Status, spList_Subject, spList_Title);
  {$ELSE}
  CHECK_TSearchParamsList : Array[TSearchParamsList] of TSearchParamsList = ( spList__content, spList__id, spList__lastUpdated, spList__profile, spList__query, spList__security, spList__tag, spList__text,
    spList_Code, spList_Date, spList_Emptyreason, spList_Encounter, spList_Item, spList_Notes, spList_Patient, spList_Source, spList_Status, spList_Subject, spList_Title);
  {$ENDIF}


procedure TFhirIndexInformation.buildIndexesList;
var
  a : TSearchParamsList;
begin
  for a := low(TSearchParamsList) to high(TSearchParamsList) do
  begin
    assert(CHECK_TSearchParamsList[a] = a);
    indexes.add(frtList, CODES_TSearchParamsList[a], DESC_TSearchParamsList[a], TYPES_TSearchParamsList[a], TARGETS_TSearchParamsList[a], PATHS_TSearchParamsList[a], USES_TSearchParamsList[a]);
  end;
  // DAF:
  {$IFNDEF FHIR_DSTU3}
  indexes.add(frtList, 'identifier', 'identifier', SearchParamTypeToken, [], '', SearchXpathUsageNull);
  {$ENDIF}
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
  {$IFDEF FHIR_DSTU3}
  for i := 0 to resource.noteList.Count - 1 do
    index(frtList, key, 0, resource.noteList[i].text, CODES_TSearchParamsList[spList_notes]);
  index(frtList, key, 0, resource.identifierList, CODES_TSearchParamsList[spList_identifier]);
  {$ELSE}
  index(frtList, key, 0, resource.noteElement, CODES_TSearchParamsList[spList_notes]);
  {$ENDIF}
  index(frtList, key, 0, resource.titleElement, CODES_TSearchParamsList[spList_title]);
  index(frtList, key, 0, resource.statusElement, CODES_TSearchParamsList[spList_status]);
end;


const
  CHECK_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of TSearchParamsCarePlan = ( spCarePlan__content, spCarePlan__id, spCarePlan__lastUpdated, spCarePlan__profile, spCarePlan__query, spCarePlan__security, spCarePlan__tag, spCarePlan__text,
    spCarePlan_Activitycode, spCarePlan_Activitydate, spCarePlan_Activityreference, spCarePlan_Condition, spCarePlan_Date, spCarePlan_Goal, spCarePlan_Participant,
    spCarePlan_Patient, spCarePlan_Performer, spCarePlan_Related, spCarePlan_Relatedcode, spCarePlan_Relatedplan, spCarePlan_Subject);


procedure TFhirIndexInformation.buildIndexesCarePlan;
var
  a : TSearchParamsCarePlan;
begin
  for a := low(TSearchParamsCarePlan) to high(TSearchParamsCarePlan) do
  begin
    assert(CHECK_TSearchParamsCarePlan[a] = a);
    indexes.add(frtCarePlan, CODES_TSearchParamsCarePlan[a], DESC_TSearchParamsCarePlan[a], TYPES_TSearchParamsCarePlan[a], TARGETS_TSearchParamsCarePlan[a], PATHS_TSearchParamsCarePlan[a], USES_TSearchParamsCarePlan[a]);
  end;
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


const
  CHECK_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of TSearchParamsImagingStudy = ( spImagingStudy__content, spImagingStudy__id, spImagingStudy__lastUpdated, spImagingStudy__profile, spImagingStudy__query, spImagingStudy__security, spImagingStudy__tag, spImagingStudy__text,
    {$IFDEF FHIR_DSTU2}
    spImagingStudy_Accession, spImagingStudy_Bodysite, spImagingStudy_Dicomclass, spImagingStudy_Modality, spImagingStudy_Order, spImagingStudy_Patient, spImagingStudy_Series, spImagingStudy_Started, spImagingStudy_Study, spImagingStudy_Uid);
    {$ELSE}
    spImagingStudy_Accession, spImagingStudy_Bodysite, spImagingStudy_Dicomclass, spImagingStudy_Identifier, spImagingStudy_Modality, spImagingStudy_Order, spImagingStudy_Patient, spImagingStudy_Series, spImagingStudy_Started, spImagingStudy_Study, spImagingStudy_Uid);
    {$ENDIF}


procedure TFhirIndexInformation.buildIndexesImagingStudy;
var
  a : TSearchParamsImagingStudy;
begin
  for a := low(TSearchParamsImagingStudy) to high(TSearchParamsImagingStudy) do
  begin
    assert(CHECK_TSearchParamsImagingStudy[a] = a);
    indexes.add(frtImagingStudy, CODES_TSearchParamsImagingStudy[a], DESC_TSearchParamsImagingStudy[a], TYPES_TSearchParamsImagingStudy[a], TARGETS_TSearchParamsImagingStudy[a], PATHS_TSearchParamsImagingStudy[a], USES_TSearchParamsImagingStudy[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesImagingStudy(key: integer; id : String; context : TFhirResource; resource: TFhirImagingStudy);
var
  i, j : integer;
  series : TFhirImagingStudySeries;
  image : TFhirImagingStudySeriesInstance;
begin
  index(frtImagingStudy, key, 0, resource.accession, CODES_TSearchParamsImagingStudy[spImagingStudy_accession]);
  {$IFDEF FHIR_DSTU3}
  index(frtImagingStudy, key, 0, resource.identifierList, CODES_TSearchParamsImagingStudy[spImagingStudy_identifier]);
  {$ENDIF}
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


const
  CHECK_TSearchParamsImmunization : Array[TSearchParamsImmunization] of TSearchParamsImmunization = (
    spImmunization__content, spImmunization__id, spImmunization__lastUpdated, spImmunization__profile, spImmunization__query, spImmunization__security, spImmunization__tag,  spImmunization__text,
    spImmunization_Date, spImmunization_Dosesequence, spImmunization_Identifier, spImmunization_Location, spImmunization_Lotnumber, spImmunization_Manufacturer, spImmunization_Notgiven, spImmunization_Patient,
    spImmunization_Performer, spImmunization_Reaction, spImmunization_Reactiondate, spImmunization_Reason, spImmunization_Reasonnotgiven, spImmunization_Requester, spImmunization_Status, spImmunization_Vaccinecode);

procedure TFhirIndexInformation.buildIndexesImmunization;
var
  a : TSearchParamsImmunization;
begin
  for a := low(TSearchParamsImmunization) to high(TSearchParamsImmunization) do
  begin
    assert(CHECK_TSearchParamsImmunization[a] = a);
    indexes.add(frtImmunization, CODES_TSearchParamsImmunization[a], DESC_TSearchParamsImmunization[a], TYPES_TSearchParamsImmunization[a], TARGETS_TSearchParamsImmunization[a], PATHS_TSearchParamsImmunization[a], USES_TSearchParamsImmunization[a]);
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

const
  CHECK_TSearchParamsOrder : Array[TSearchParamsOrder] of TSearchParamsOrder = (spOrder__content, spOrder__id, spOrder__lastUpdated, spOrder__profile, spOrder__query, spOrder__security, spOrder__tag, spOrder__text,
    spOrder_Date, spOrder_Detail, spOrder_Identifier, spOrder_Patient, spOrder_Source, spOrder_Subject, spOrder_Target, spOrder_When, spOrder_When_code);

procedure TFhirIndexInformation.buildIndexesOrder;
var
  a : TSearchParamsOrder;
begin
  for a := low(TSearchParamsOrder) to high(TSearchParamsOrder) do
  begin
    assert(CHECK_TSearchParamsOrder[a] = a);
    indexes.add(frtOrder, CODES_TSearchParamsOrder[a], DESC_TSearchParamsOrder[a], TYPES_TSearchParamsOrder[a], TARGETS_TSearchParamsOrder[a], PATHS_TSearchParamsOrder[a], USES_TSearchParamsOrder[a]);
  end;
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

const
  CHECK_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of TSearchParamsOrderResponse = ( spOrderResponse__content, spOrderResponse__id, spOrderResponse__lastUpdated, spOrderResponse__profile, spOrderResponse__query, spOrderResponse__security, spOrderResponse__tag, spOrderResponse__text,
     spOrderResponse_Code, spOrderResponse_Date, spOrderResponse_Fulfillment, spOrderResponse_Identifier, spOrderResponse_Request, spOrderResponse_Who);

procedure TFhirIndexInformation.buildIndexesOrderResponse;
var
  a : TSearchParamsOrderResponse;
begin
  for a := low(TSearchParamsOrderResponse) to high(TSearchParamsOrderResponse) do
  begin
    assert(CHECK_TSearchParamsOrderResponse[a] = a);
    indexes.add(frtOrderResponse, CODES_TSearchParamsOrderResponse[a], DESC_TSearchParamsOrderResponse[a], TYPES_TSearchParamsOrderResponse[a], TARGETS_TSearchParamsOrderResponse[a], PATHS_TSearchParamsOrderResponse[a], USES_TSearchParamsOrderResponse[a]);
  end;
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

const
  CHECK_TSearchParamsMedia : Array[TSearchParamsMedia] of TSearchParamsMedia = ( spMedia__content, spMedia__id, spMedia__lastUpdated, spMedia__profile, spMedia__query, spMedia__security, spMedia__tag, spMedia__text,
    spMedia_Created, spMedia_Identifier, spMedia_Operator, spMedia_Patient, spMedia_Subject, spMedia_Subtype, spMedia_Type, spMedia_View);



procedure TFhirIndexInformation.buildIndexesMedia;
var
  a : TSearchParamsMedia;
begin
  for a := low(TSearchParamsMedia) to high(TSearchParamsMedia) do
  begin
    assert(CHECK_TSearchParamsMedia[a] = a);
    indexes.add(frtMedia, CODES_TSearchParamsMedia[a], DESC_TSearchParamsMedia[a], TYPES_TSearchParamsMedia[a], TARGETS_TSearchParamsMedia[a], PATHS_TSearchParamsMedia[a], USES_TSearchParamsMedia[a]);
  end;
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


const
  CHECK_TSearchParamsFamilyMemberHistory : Array[TSearchParamsFamilyMemberHistory] of TSearchParamsFamilyMemberHistory = ( spFamilyMemberHistory__content, spFamilyMemberHistory__id, spFamilyMemberHistory__lastUpdated, spFamilyMemberHistory__profile, spFamilyMemberHistory__query, spFamilyMemberHistory__security, spFamilyMemberHistory__tag, spFamilyMemberHistory__text,
    spFamilyMemberHistory_Code, spFamilyMemberHistory_Condition, spFamilyMemberHistory_Date, spFamilyMemberHistory_Gender, spFamilyMemberHistory_Identifier, spFamilyMemberHistory_Patient, spFamilyMemberHistory_Relationship);

procedure TFhirIndexInformation.buildIndexesFamilyMemberHistory;
var
  a : TSearchParamsFamilyMemberHistory;
begin
  for a := low(TSearchParamsFamilyMemberHistory) to high(TSearchParamsFamilyMemberHistory) do
  begin
    assert(CHECK_TSearchParamsFamilyMemberHistory[a] = a);
    indexes.add(frtFamilyMemberHistory, CODES_TSearchParamsFamilyMemberHistory[a], DESC_TSearchParamsFamilyMemberHistory[a], TYPES_TSearchParamsFamilyMemberHistory[a], TARGETS_TSearchParamsFamilyMemberHistory[a], PATHS_TSearchParamsFamilyMemberHistory[a], USES_TSearchParamsFamilyMemberHistory[a]);
  end;

  // DAF:
  indexes.add(frtFamilyMemberHistory, 'familymemberhistorycondition', 'Search for a history of a particular condition within a patient''s family', SearchParamTypeToken, [], '', SearchXpathUsageNull);
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



const
  CHECK_TSearchParamsProcedure : Array[TSearchParamsProcedure] of TSearchParamsProcedure = (
    spProcedure__content, spProcedure__id, spProcedure__lastUpdated, spProcedure__profile, spProcedure__query, spProcedure__security, spProcedure__tag, spProcedure__text,
    spProcedure_Code, spProcedure_Date, spProcedure_Encounter, spProcedure_Identifier, spProcedure_Location, spProcedure_Patient, spProcedure_Performer, spProcedure_Subject);


procedure TFhirIndexInformation.buildIndexesProcedure;
var
  a : TSearchParamsProcedure;
begin
  for a := low(TSearchParamsProcedure) to high(TSearchParamsProcedure) do
  begin
    assert(CHECK_TSearchParamsProcedure[a] = a);
    indexes.add(frtProcedure, CODES_TSearchParamsProcedure[a], DESC_TSearchParamsProcedure[a], TYPES_TSearchParamsProcedure[a], TARGETS_TSearchParamsProcedure[a], PATHS_TSearchParamsProcedure[a], USES_TSearchParamsProcedure[a]);
  end;
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

const
  CHECK_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of TSearchParamsSpecimen = (
    spSpecimen__content, spSpecimen__id, spSpecimen__lastUpdated, spSpecimen__profile, spSpecimen__query, spSpecimen__security, spSpecimen__tag, spSpecimen__text,
    spSpecimen_Accession, spSpecimen_Bodysite, spSpecimen_Collected, spSpecimen_Collector, spSpecimen_Container, spSpecimen_Containerid, spSpecimen_Identifier, spSpecimen_Parent, spSpecimen_Patient, spSpecimen_Subject, spSpecimen_Type);

procedure TFhirIndexInformation.buildIndexesSpecimen;
var
  a : TSearchParamsSpecimen;
begin
  for a := low(TSearchParamsSpecimen) to high(TSearchParamsSpecimen) do
  begin
    assert(CHECK_TSearchParamsSpecimen[a] = a);
    indexes.add(frtSpecimen, CODES_TSearchParamsSpecimen[a], DESC_TSearchParamsSpecimen[a], TYPES_TSearchParamsSpecimen[a], TARGETS_TSearchParamsSpecimen[a], PATHS_TSearchParamsSpecimen[a], USES_TSearchParamsSpecimen[a]);
  end;
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

const
  CHECK_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of TSearchParamsImmunizationRecommendation = (
    spImmunizationRecommendation__content, spImmunizationRecommendation__id, spImmunizationRecommendation__lastUpdated, spImmunizationRecommendation__profile, spImmunizationRecommendation__query, spImmunizationRecommendation__security, spImmunizationRecommendation__tag, spImmunizationRecommendation__text,
    spImmunizationRecommendation_Date, spImmunizationRecommendation_Dosenumber, spImmunizationRecommendation_Dosesequence, spImmunizationRecommendation_Identifier, spImmunizationRecommendation_Information, spImmunizationRecommendation_Patient, spImmunizationRecommendation_Status, spImmunizationRecommendation_Support, spImmunizationRecommendation_Vaccinetype);


procedure TFhirIndexInformation.buildIndexesImmunizationRecommendation;
var
  a : TSearchParamsImmunizationRecommendation;
begin
  for a := low(TSearchParamsImmunizationRecommendation) to high(TSearchParamsImmunizationRecommendation) do
  begin
    assert(CHECK_TSearchParamsImmunizationRecommendation[a] = a);
    indexes.add(frtImmunizationRecommendation, CODES_TSearchParamsImmunizationRecommendation[a], DESC_TSearchParamsImmunizationRecommendation[a], TYPES_TSearchParamsImmunizationRecommendation[a], TARGETS_TSearchParamsImmunizationRecommendation[a], PATHS_TSearchParamsImmunizationRecommendation[a], USES_TSearchParamsImmunizationRecommendation[a]);
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

{$IFDEF FHIR_DSTU2}
Const
  CHECK_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of TSearchParamsQuestionnaire = (spQuestionnaire__content, spQuestionnaire__id, spQuestionnaire__lastUpdated, spQuestionnaire__profile, spQuestionnaire__query, spQuestionnaire__security, spQuestionnaire__tag, spQuestionnaire__text,
    spQuestionnaire_Code, spQuestionnaire_Date, spQuestionnaire_Identifier, spQuestionnaire_Publisher, spQuestionnaire_Status, spQuestionnaire_Title, spQuestionnaire_Version);

procedure TFhirIndexInformation.buildIndexesQuestionnaire;
var
  a : TSearchParamsQuestionnaire;
begin
  for a := low(TSearchParamsQuestionnaire) to high(TSearchParamsQuestionnaire) do
  begin
    assert(CHECK_TSearchParamsQuestionnaire[a] = a);
    indexes.add(frtQuestionnaire, CODES_TSearchParamsQuestionnaire[a], DESC_TSearchParamsQuestionnaire[a], TYPES_TSearchParamsQuestionnaire[a], TARGETS_TSearchParamsQuestionnaire[a], PATHS_TSearchParamsQuestionnaire[a], USES_TSearchParamsQuestionnaire[a]);
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
{$ELSE}
Const
  CHECK_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of TSearchParamsQuestionnaire = (spQuestionnaire__content, spQuestionnaire__id, spQuestionnaire__lastUpdated, spQuestionnaire__profile, spQuestionnaire__query, spQuestionnaire__security, spQuestionnaire__tag, spQuestionnaire__text,
    spQuestionnaire_Code, spQuestionnaire_Context, spQuestionnaire_Date, spQuestionnaire_Identifier, spQuestionnaire_Publisher, spQuestionnaire_Status, spQuestionnaire_Title, spQuestionnaire_Version);

procedure TFhirIndexInformation.buildIndexesQuestionnaire;
var
  a : TSearchParamsQuestionnaire;
begin
  for a := low(TSearchParamsQuestionnaire) to high(TSearchParamsQuestionnaire) do
  begin
    assert(CHECK_TSearchParamsQuestionnaire[a] = a);
    indexes.add(frtQuestionnaire, CODES_TSearchParamsQuestionnaire[a], DESC_TSearchParamsQuestionnaire[a], TYPES_TSearchParamsQuestionnaire[a], TARGETS_TSearchParamsQuestionnaire[a], PATHS_TSearchParamsQuestionnaire[a], USES_TSearchParamsQuestionnaire[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesQuestionnaire(key: integer; id : String; context : TFhirResource; resource: TFhirQuestionnaire);
  procedure IndexGroup(group : TFhirQuestionnaireItem);
  var
    i : integer;
  begin
    index(frtQuestionnaire, key, 0, group.conceptList, CODES_TSearchParamsQuestionnaire[spQuestionnaire_code]);
    for I := 0 to group.itemList.Count - 1 do
      indexGroup(group.itemList[i]);
  end;
var
  i : integer;
begin
  index(frtQuestionnaire, key, 0, resource.publisherElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_publisher]);
  index(frtQuestionnaire, key, 0, resource.statusElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_status]);
  index(frtQuestionnaire, key, 0, resource.useContextList, CODES_TSearchParamsQuestionnaire[spQuestionnaire_Context]);
  index(frtQuestionnaire, key, 0, resource.identifierList, CODES_TSearchParamsQuestionnaire[spQuestionnaire_identifier]);
  index(frtQuestionnaire, key, 0, resource.dateElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_date]);
  index(frtQuestionnaire, key, 0, resource.versionElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_version]);
  index(frtQuestionnaire, key, 0, resource.titleElement, CODES_TSearchParamsQuestionnaire[spQuestionnaire_title]);
  for I := 0 to resource.itemList.Count - 1 do
    indexGroup(resource.itemList[i]);
end;
{$ENDIF}

Const
  CHECK_TSearchParamsQuestionnaireResponse : Array[TSearchParamsQuestionnaireResponse] of TSearchParamsQuestionnaireResponse = (
    spQuestionnaireResponse__content, spQuestionnaireResponse__id, spQuestionnaireResponse__lastUpdated, spQuestionnaireResponse__profile, spQuestionnaireResponse__query, spQuestionnaireResponse__security, spQuestionnaireResponse__tag, spQuestionnaireResponse__text,
    spQuestionnaireResponse_Author, spQuestionnaireResponse_Authored, spQuestionnaireResponse_Encounter, spQuestionnaireResponse_Patient, spQuestionnaireResponse_Questionnaire, spQuestionnaireResponse_Source, spQuestionnaireResponse_Status, spQuestionnaireResponse_Subject);

procedure TFhirIndexInformation.buildIndexesQuestionnaireResponse;
var
  a : TSearchParamsQuestionnaireResponse;
begin
  for a := low(TSearchParamsQuestionnaireResponse) to high(TSearchParamsQuestionnaireResponse) do
  begin
    assert(CHECK_TSearchParamsQuestionnaireResponse[a] = a);
    indexes.add(frtQuestionnaireResponse, CODES_TSearchParamsQuestionnaireResponse[a], DESC_TSearchParamsQuestionnaireResponse[a], TYPES_TSearchParamsQuestionnaireResponse[a], TARGETS_TSearchParamsQuestionnaireResponse[a], PATHS_TSearchParamsQuestionnaireResponse[a], USES_TSearchParamsQuestionnaireResponse[a]);
  end;
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



Const
  CHECK_TSearchParamsSlot : Array[TSearchParamsSlot] of TSearchParamsSlot = (
    spSlot__content, spSlot__id, spSlot__lastUpdated, spSlot__profile, spSlot__query, spSlot__security, spSlot__tag, spSlot__text,
    {$IFDEF FHIR_DSTU2}spSlot_Fbtype, {$ENDIF} spSlot_Identifier, spSlot_Schedule, spSlot_Slottype, spSlot_Start{$IFDEF FHIR_DSTU3}, spSlot_Status{$ENDIF});


procedure TFhirIndexInformation.buildIndexesSlot;
var
  a : TSearchParamsSlot;
begin
  for a := low(TSearchParamsSlot) to high(TSearchParamsSlot) do
  begin
    assert(CHECK_TSearchParamsSlot[a] = a);
    indexes.add(frtSlot, CODES_TSearchParamsSlot[a], DESC_TSearchParamsSlot[a], TYPES_TSearchParamsSlot[a], TARGETS_TSearchParamsSlot[a], PATHS_TSearchParamsSlot[a], USES_TSearchParamsSlot[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesSlot(key: integer; id : String; context : TFhirResource; resource: TFhirSlot);
begin
  index(context, frtSlot, key, 0, resource.schedule, CODES_TSearchParamsSlot[spSlot_schedule]);
  {$IFDEF FHIR_DSTU2}
  index(frtSlot, key, 0, resource.freeBusyTypeElement, CODES_TSearchParamsSlot[spSlot_fbtype]);
  index(frtSlot, key, 0, resource.type_, CODES_TSearchParamsSlot[spSlot_slottype]);
  {$ELSE}
  index(frtSlot, key, 0, resource.statusElement, CODES_TSearchParamsSlot[spSlot_status]);
  index(frtSlot, key, 0, resource.serviceTypeList, CODES_TSearchParamsSlot[spSlot_slottype]);
  {$ENDIF}
  index(frtSlot, key, 0, resource.identifierList, CODES_TSearchParamsSlot[spSlot_identifier]);
  index(frtSlot, key, 0, resource.startElement, CODES_TSearchParamsSlot[spSlot_start]);
end;

Const
  CHECK_TSearchParamsAppointment : Array[TSearchParamsAppointment] of TSearchParamsAppointment = (
    spAppointment__content, spAppointment__id, spAppointment__lastUpdated, spAppointment__profile, spAppointment__query, spAppointment__security, spAppointment__tag, spAppointment__text,
    {$IFDEF FHIR_DSTU2}
    spAppointment_Actor,                                spAppointment_Date, spAppointment_Identifier, spAppointment_Location, spAppointment_Partstatus, spAppointment_Patient, spAppointment_Practitioner,                            spAppointment_Status);
    {$ELSE}
    spAppointment_Actor, spAppointment_Appointmenttype, spAppointment_Date, spAppointment_Identifier, spAppointment_Location, spAppointment_Partstatus, spAppointment_Patient, spAppointment_Practitioner, spAppointment_Servicetype, spAppointment_Status);
    {$ENDIF}

procedure TFhirIndexInformation.buildIndexesAppointment;
var
  a : TSearchParamsAppointment;
begin
  for a := low(TSearchParamsAppointment) to high(TSearchParamsAppointment) do
  begin
    assert(CHECK_TSearchParamsAppointment[a] = a);
    indexes.add(frtAppointment, CODES_TSearchParamsAppointment[a], DESC_TSearchParamsAppointment[a], TYPES_TSearchParamsAppointment[a], TARGETS_TSearchParamsAppointment[a], PATHS_TSearchParamsAppointment[a], USES_TSearchParamsAppointment[a]);
  end;
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
    index(frtAppointment, key, 0, resource.participantList[i].statusElement, CODES_TSearchParamsAppointment[spAppointment_status]);
    index(context, frtAppointment, key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_actor]);
    index(context, frtAppointment, key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_patient], frtPatient);
    index(context, frtAppointment, key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_location], frtLocation);
    index(context, frtAppointment, key, 0, resource.participantList[i].actor, CODES_TSearchParamsAppointment[spAppointment_practitioner], frtPractitioner);
    patientCompartment(key, resource.participantList[i].actor);
    practitionerCompartment(key, resource.participantList[i].actor);
    deviceCompartment(key, resource.participantList[i].actor);
    relatedPersonCompartment(key, resource.participantList[i].actor);
  end;
  {$IFDEF FHIR_DSTU3}
  index(frtAppointment, key, 0, resource.serviceTypeList, CODES_TSearchParamsAppointment[spAppointment_Servicetype]);
  index(frtAppointment, key, 0, resource.appointmentType, CODES_TSearchParamsAppointment[spAppointment_Appointmenttype]);
  {$ENDIF}
end;

Const
  CHECK_TSearchParamsSchedule : Array[TSearchParamsSchedule] of TSearchParamsSchedule = (
    spSchedule__content, spSchedule__id, spSchedule__lastUpdated, spSchedule__profile, spSchedule__query, spSchedule__security, spSchedule__tag, spSchedule__text,
    spSchedule_Actor, spSchedule_Date, spSchedule_Identifier, spSchedule_Type);

procedure TFhirIndexInformation.buildIndexesSchedule;
var
  a : TSearchParamsSchedule;
begin
  for a := low(TSearchParamsSchedule) to high(TSearchParamsSchedule) do
  begin
    assert(CHECK_TSearchParamsSchedule[a] = a);
    indexes.add(frtSchedule, CODES_TSearchParamsSchedule[a], DESC_TSearchParamsSchedule[a], TYPES_TSearchParamsSchedule[a], TARGETS_TSearchParamsSchedule[a], PATHS_TSearchParamsSchedule[a], USES_TSearchParamsSchedule[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesSchedule(key: integer; id : String; context : TFhirResource; resource: TFhirSchedule);
var
  i : integer;
begin
  index(frtSchedule, key, 0, resource.planningHorizon, CODES_TSearchParamsSchedule[spSchedule_date]);
  index(frtSchedule, key, 0, resource.identifierList, CODES_TSearchParamsSchedule[spSchedule_identifier]);
  index(context, frtSchedule, key, 0, resource.actor, CODES_TSearchParamsSchedule[spSchedule_actor]);
  for i := 0 to resource.serviceTypeList.Count - 1 do
    index(frtSchedule, key, 0, resource.serviceTypeList[i], CODES_TSearchParamsSchedule[spSchedule_type]);
  patientCompartment(key, resource.actor);
end;

Const
  CHECK_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of TSearchParamsAppointmentResponse = (
    spAppointmentResponse__content, spAppointmentResponse__id, spAppointmentResponse__lastUpdated, spAppointmentResponse__profile, spAppointmentResponse__query, spAppointmentResponse__security, spAppointmentResponse__tag, spAppointmentResponse__text,
    spAppointmentResponse_Actor, spAppointmentResponse_Appointment, spAppointmentResponse_Identifier, spAppointmentResponse_Location, spAppointmentResponse_Partstatus, spAppointmentResponse_Patient, spAppointmentResponse_Practitioner);

procedure TFhirIndexInformation.buildIndexesAppointmentResponse;
var
  a : TSearchParamsAppointmentResponse;
begin
  for a := low(TSearchParamsAppointmentResponse) to high(TSearchParamsAppointmentResponse) do
  begin
    assert(CHECK_TSearchParamsAppointmentResponse[a] = a);
    indexes.add(frtAppointmentResponse, CODES_TSearchParamsAppointmentResponse[a], DESC_TSearchParamsAppointmentResponse[a], TYPES_TSearchParamsAppointmentResponse[a], TARGETS_TSearchParamsAppointmentResponse[a], PATHS_TSearchParamsAppointmentResponse[a], USES_TSearchParamsAppointmentResponse[a]);
  end;
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

Const
  CHECK_TSearchParamsHealthcareService : Array[TSearchParamsHealthcareService] of TSearchParamsHealthcareService = (
    spHealthcareService__content, spHealthcareService__id, spHealthcareService__lastUpdated, spHealthcareService__profile, spHealthcareService__query, spHealthcareService__security, spHealthcareService__tag, spHealthcareService__text,
    spHealthcareService_Characteristic, spHealthcareService_Identifier, spHealthcareService_Location, spHealthcareService_Name, spHealthcareService_Organization, spHealthcareService_Programname, spHealthcareService_Servicecategory, spHealthcareService_Servicetype);

procedure TFhirIndexInformation.buildIndexesHealthcareService;
var
  a : TSearchParamsHealthcareService;
begin
  for a := low(TSearchParamsHealthcareService) to high(TSearchParamsHealthcareService) do
  begin
    assert(CHECK_TSearchParamsHealthcareService[a] = a);
    indexes.add(frtHealthcareService, CODES_TSearchParamsHealthcareService[a], DESC_TSearchParamsHealthcareService[a], TYPES_TSearchParamsHealthcareService[a], TARGETS_TSearchParamsHealthcareService[a], PATHS_TSearchParamsHealthcareService[a], USES_TSearchParamsHealthcareService[a]);
  end;
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
  {$IFDEF FHIR_DSTU2}
  index(context, frtHealthcareService, key, 0, resource.location, CODES_TSearchParamsHealthcareService[spHealthcareService_location]);
  {$ELSE}
  index(context, frtHealthcareService, key, 0, resource.locationList, CODES_TSearchParamsHealthcareService[spHealthcareService_location]);
  {$ENDIF}
  index(context, frtHealthcareService, key, 0, resource.providedBy, CODES_TSearchParamsHealthcareService[spHealthcareService_organization]);
  index(frtHealthcareService, key, 0, resource.serviceCategoryElement, CODES_TSearchParamsHealthcareService[spHealthcareService_servicecategory]);
  index(frtHealthcareService, key, 0, resource.serviceTypeList[i], CODES_TSearchParamsHealthcareService[spHealthcareService_servicetype]);
end;

Const
  CHECK_TSearchParamsDataElement : Array[TSearchParamsDataElement] of TSearchParamsDataElement = (
    spDataElement__content, spDataElement__id, spDataElement__lastUpdated, spDataElement__profile, spDataElement__query, spDataElement__security, spDataElement__tag, spDataElement__text,
    spDataElement_Code, spDataElement_Context, spDataElement_Date, spDataElement_Description, spDataElement_Identifier, spDataElement_Name, spDataElement_ObjectClass, spDataElement_ObjectClassProperty, spDataElement_Publisher, spDataElement_Status, spDataElement_Stringency, spDataElement_Url, spDataElement_Version);

procedure TFhirIndexInformation.buildIndexesDataElement;
var
  a : TSearchParamsDataElement;
begin
  for a := low(TSearchParamsDataElement) to high(TSearchParamsDataElement) do
  begin
    assert(CHECK_TSearchParamsDataElement[a] = a);
    indexes.add(frtDataElement, CODES_TSearchParamsDataElement[a], DESC_TSearchParamsDataElement[a], TYPES_TSearchParamsDataElement[a], TARGETS_TSearchParamsDataElement[a], PATHS_TSearchParamsDataElement[a], USES_TSearchParamsDataElement[a]);
  end;
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

Const
  CHECK_TSearchParamsTestScript : Array[TSearchParamsTestScript] of TSearchParamsTestScript = (
    spTestScript__content, spTestScript__id, spTestScript__lastUpdated, spTestScript__profile, spTestScript__query, spTestScript__security, spTestScript__tag, spTestScript__text,
    spTestScript_Description, spTestScript_Identifier, spTestScript_Name, spTestScript_Testscriptcapability, spTestScript_Testscriptsetupcapability, spTestScript_Testscripttestcapability, spTestScript_Url);

procedure TFhirIndexInformation.buildIndexesTestScript;
var
  a : TSearchParamsTestScript;
begin
  for a := low(TSearchParamsTestScript) to high(TSearchParamsTestScript) do
  begin
    assert(CHECK_TSearchParamsTestScript[a] = a);
    indexes.add(frtTestScript, CODES_TSearchParamsTestScript[a], DESC_TSearchParamsTestScript[a], TYPES_TSearchParamsTestScript[a], TARGETS_TSearchParamsTestScript[a], PATHS_TSearchParamsTestScript[a], USES_TSearchParamsTestScript[a]);
  end;
  // todo
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

Const
  CHECK_TSearchParamsNamingSystem : Array[TSearchParamsNamingSystem] of TSearchParamsNamingSystem = (spNamingSystem__content, spNamingSystem__id, spNamingSystem__lastUpdated, spNamingSystem__profile, spNamingSystem__query, spNamingSystem__security, spNamingSystem__tag, spNamingSystem__text,
    spNamingSystem_Contact, spNamingSystem_Context, spNamingSystem_Date, spNamingSystem_Idtype, spNamingSystem_Kind, spNamingSystem_Name, spNamingSystem_Period, spNamingSystem_Publisher,
    spNamingSystem_Replacedby, spNamingSystem_Responsible, spNamingSystem_Status, spNamingSystem_Telecom, spNamingSystem_Type, spNamingSystem_Value);

procedure TFhirIndexInformation.buildIndexesNamingSystem;
var
  a : TSearchParamsNamingSystem;
begin
  for a := low(TSearchParamsNamingSystem) to high(TSearchParamsNamingSystem) do
  begin
    assert(CHECK_TSearchParamsNamingSystem[a] = a);
    indexes.add(frtNamingSystem, CODES_TSearchParamsNamingSystem[a], DESC_TSearchParamsNamingSystem[a], TYPES_TSearchParamsNamingSystem[a], TARGETS_TSearchParamsNamingSystem[a], PATHS_TSearchParamsNamingSystem[a], USES_TSearchParamsNamingSystem[a]);
  end;
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

Const
  CHECK_TSearchParamsSubscription : Array[TSearchParamsSubscription] of TSearchParamsSubscription = (
    spSubscription__content, spSubscription__id, spSubscription__lastUpdated, spSubscription__profile, spSubscription__query, spSubscription__security, spSubscription__tag, spSubscription__text,
     spSubscription_Contact, spSubscription_Criteria, spSubscription_Payload, spSubscription_Status, spSubscription_Tag, spSubscription_Type, spSubscription_Url);

procedure TFhirIndexInformation.buildIndexesSubscription;
var
  a : TSearchParamsSubscription;
begin
  for a := low(TSearchParamsSubscription) to high(TSearchParamsSubscription) do
  begin
    assert(CHECK_TSearchParamsSubscription[a] = a);
    indexes.add(frtSubscription, CODES_TSearchParamsSubscription[a], DESC_TSearchParamsSubscription[a], TYPES_TSearchParamsSubscription[a], TARGETS_TSearchParamsSubscription[a], PATHS_TSearchParamsSubscription[a], USES_TSearchParamsSubscription[a]);
  end;
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

Const
  CHECK_TSearchParamsDetectedIssue : Array[TSearchParamsDetectedIssue] of TSearchParamsDetectedIssue = (
    spDetectedIssue__content, spDetectedIssue__id, spDetectedIssue__lastUpdated, spDetectedIssue__profile, spDetectedIssue__query, spDetectedIssue__security, spDetectedIssue__tag, spDetectedIssue__text,
    spDetectedIssue_Author, spDetectedIssue_Category, spDetectedIssue_Date, spDetectedIssue_Identifier, spDetectedIssue_Implicated, spDetectedIssue_Patient);

procedure TFhirIndexInformation.buildIndexesDetectedIssue;
var
  a : TSearchParamsDetectedIssue;
begin
  for a := low(TSearchParamsDetectedIssue) to high(TSearchParamsDetectedIssue) do
  begin
    assert(CHECK_TSearchParamsDetectedIssue[a] = a);
    indexes.add(frtDetectedIssue, CODES_TSearchParamsDetectedIssue[a], DESC_TSearchParamsDetectedIssue[a], TYPES_TSearchParamsDetectedIssue[a], TARGETS_TSearchParamsDetectedIssue[a], PATHS_TSearchParamsDetectedIssue[a], USES_TSearchParamsDetectedIssue[a]);
  end;
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

Const
  CHECK_TSearchParamsRiskAssessment : Array[TSearchParamsRiskAssessment] of TSearchParamsRiskAssessment = (spRiskAssessment__content, spRiskAssessment__id, spRiskAssessment__lastUpdated, spRiskAssessment__profile, spRiskAssessment__query, spRiskAssessment__security, spRiskAssessment__tag, spRiskAssessment__text,
     spRiskAssessment_Condition, spRiskAssessment_Date, spRiskAssessment_Encounter, spRiskAssessment_Identifier, spRiskAssessment_Method, spRiskAssessment_Patient, spRiskAssessment_Performer, spRiskAssessment_Subject);

procedure TFhirIndexInformation.buildIndexesRiskAssessment;
var
  a : TSearchParamsRiskAssessment;
begin
  for a := low(TSearchParamsRiskAssessment) to high(TSearchParamsRiskAssessment) do
  begin
    assert(CHECK_TSearchParamsRiskAssessment[a] = a);
    indexes.add(frtRiskAssessment, CODES_TSearchParamsRiskAssessment[a], DESC_TSearchParamsRiskAssessment[a], TYPES_TSearchParamsRiskAssessment[a], TARGETS_TSearchParamsRiskAssessment[a], PATHS_TSearchParamsRiskAssessment[a], USES_TSearchParamsRiskAssessment[a]);
  end;
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

const
  CHECK_TSearchParamsOperationDefinition : Array[TSearchParamsOperationDefinition] of TSearchParamsOperationDefinition = (
    spOperationDefinition__content, spOperationDefinition__id, spOperationDefinition__lastUpdated, spOperationDefinition__profile, spOperationDefinition__query, spOperationDefinition__security, spOperationDefinition__tag,  spOperationDefinition__text,
    spOperationDefinition_Base, spOperationDefinition_Code, spOperationDefinition_Context, spOperationDefinition_Date, spOperationDefinition_Instance, spOperationDefinition_Kind, spOperationDefinition_Name, spOperationDefinition_Paramprofile, spOperationDefinition_Publisher, spOperationDefinition_Status, spOperationDefinition_System, spOperationDefinition_Type, spOperationDefinition_Url, spOperationDefinition_Version);

procedure TFhirIndexInformation.buildIndexesOperationDefinition;
var
  a : TSearchParamsOperationDefinition;
begin
  for a := low(TSearchParamsOperationDefinition) to high(TSearchParamsOperationDefinition) do
  begin
    assert(CHECK_TSearchParamsOperationDefinition[a] = a);
    indexes.add(frtOperationDefinition, CODES_TSearchParamsOperationDefinition[a], DESC_TSearchParamsOperationDefinition[a], TYPES_TSearchParamsOperationDefinition[a], TARGETS_TSearchParamsOperationDefinition[a], PATHS_TSearchParamsOperationDefinition[a], USES_TSearchParamsOperationDefinition[a]);
  end;
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
  index(frtOperationDefinition, key, 0, resource.useContextList, CODES_TSearchParamsOperationDefinition[spOperationDefinition_Context]);
  index(frtOperationDefinition, key, 0, resource.system, CODES_TSearchParamsOperationDefinition[spOperationDefinition_system]);
  for i := 0 to resource.type_List.count - 1 Do
    index(frtOperationDefinition, key, 0, resource.type_List[i], CODES_TSearchParamsOperationDefinition[spOperationDefinition_type]);
  index(frtOperationDefinition, key, 0, resource.instance, CODES_TSearchParamsOperationDefinition[spOperationDefinition_instance]);
  for i := 0 to resource.parameterList.count - 1 Do
    index(context, frtOperationDefinition, key, 0, resource.parameterList[i].profile, CODES_TSearchParamsOperationDefinition[spOperationDefinition_Paramprofile]);
end;

const
  CHECK_TSearchParamsReferralRequest : Array[TSearchParamsReferralRequest] of TSearchParamsReferralRequest = ( spReferralRequest__content, spReferralRequest__id, spReferralRequest__lastUpdated, spReferralRequest__profile, spReferralRequest__query, spReferralRequest__security, spReferralRequest__tag, spReferralRequest__text,
    {$IFDEF FHIR_DSTU2}
    spReferralRequest_Date, spReferralRequest_Patient, spReferralRequest_Priority, spReferralRequest_Recipient, spReferralRequest_Requester, spReferralRequest_Specialty, spReferralRequest_Status, spReferralRequest_Type);
    {$ELSE}
    spReferralRequest_Basedon, spReferralRequest_Category, spReferralRequest_Context, spReferralRequest_Date, spReferralRequest_Parent, spReferralRequest_Patient, spReferralRequest_Priority, spReferralRequest_Recipient, spReferralRequest_Requester, spReferralRequest_Specialty, spReferralRequest_Status, spReferralRequest_Type);
    {$ENDIF}
procedure TFhirIndexInformation.buildIndexesReferralRequest;
var
  a : TSearchParamsReferralRequest;
begin
  for a := low(TSearchParamsReferralRequest) to high(TSearchParamsReferralRequest) do
  begin
    assert(CHECK_TSearchParamsReferralRequest[a] = a);
    indexes.add(frtReferralRequest, CODES_TSearchParamsReferralRequest[a], DESC_TSearchParamsReferralRequest[a], TYPES_TSearchParamsReferralRequest[a], TARGETS_TSearchParamsReferralRequest[a], PATHS_TSearchParamsReferralRequest[a], USES_TSearchParamsReferralRequest[a]);
  end;
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

  {$IFDEF FHIR_DSTU2}
  index(frtReferralRequest, key, 0, resource.dateElement, CODES_TSearchParamsReferralRequest[spReferralRequest_date]);
  {$ELSE}
  index(frtReferralRequest, key, 0, resource.authoredElement, CODES_TSearchParamsReferralRequest[spReferralRequest_date]);
  index(frtReferralRequest, key, 0, resource.parentElement, CODES_TSearchParamsReferralRequest[spReferralRequest_Parent]);
  index(frtReferralRequest, key, 0, resource.categoryElement, CODES_TSearchParamsReferralRequest[spReferralRequest_category]);
  index(context, frtReferralRequest, key, 0, resource.context, CODES_TSearchParamsReferralRequest[spReferralRequest_Context]);
  index(context, frtReferralRequest, key, 0, resource.basedOnList, CODES_TSearchParamsReferralRequest[spReferralRequest_Basedon]);
  {$ENDIF}
end;

const
  CHECK_TSearchParamsNutritionOrder : Array[TSearchParamsNutritionOrder] of TSearchParamsNutritionOrder = (
    spNutritionOrder__content, spNutritionOrder__id, spNutritionOrder__lastUpdated, spNutritionOrder__profile, spNutritionOrder__query, spNutritionOrder__security, spNutritionOrder__tag, spNutritionOrder__text,
    spNutritionOrder_Additive, spNutritionOrder_Datetime, spNutritionOrder_Encounter, spNutritionOrder_Formula, spNutritionOrder_Identifier, spNutritionOrder_Oraldiet, spNutritionOrder_Patient, spNutritionOrder_Provider, spNutritionOrder_Status, spNutritionOrder_Supplement);

procedure TFhirIndexInformation.buildIndexesNutritionOrder;
var
  a : TSearchParamsNutritionOrder;
begin
  for a := low(TSearchParamsNutritionOrder) to high(TSearchParamsNutritionOrder) do
  begin
    assert(CHECK_TSearchParamsNutritionOrder[a] = a);
    indexes.add(frtNutritionOrder, CODES_TSearchParamsNutritionOrder[a], DESC_TSearchParamsNutritionOrder[a], TYPES_TSearchParamsNutritionOrder[a], TARGETS_TSearchParamsNutritionOrder[a], PATHS_TSearchParamsNutritionOrder[a], USES_TSearchParamsNutritionOrder[a]);
  end;
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

const
  CHECK_TSearchParamsBodySite : Array[TSearchParamsBodySite] of TSearchParamsBodySite = (spBodySite__content, spBodySite__id, spBodySite__lastUpdated, spBodySite__profile, spBodySite__query, spBodySite__security, spBodySite__tag, spBodySite__text,
  spBodySite_Code, spBodySite_Identifier, spBodySite_Patient);

procedure TFhirIndexInformation.buildIndexesBodySite;
var
  a : TSearchParamsBodySite;
begin
  for a := low(TSearchParamsBodySite) to high(TSearchParamsBodySite) do
  begin
    assert(CHECK_TSearchParamsBodySite[a] = a);
    indexes.add(frtBodySite, CODES_TSearchParamsBodySite[a], DESC_TSearchParamsBodySite[a], TYPES_TSearchParamsBodySite[a], TARGETS_TSearchParamsBodySite[a], PATHS_TSearchParamsBodySite[a], USES_TSearchParamsBodySite[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesBodySite(key: integer; id : String; context : TFhirResource; resource: TFhirBodySite);
begin
  index(frtBodySite, key, 0, resource.codeElement, CODES_TSearchParamsBodySite[spBodySite_code]);
  index(frtBodySite, key, 0, resource.identifierList, CODES_TSearchParamsBodySite[spBodySite_identifier]);
  index(context, frtBodySite, key, 0, resource.patient, CODES_TSearchParamsBodySite[spBodySite_patient]);
end;

const
  CHECK_TSearchParamsClinicalImpression : Array[TSearchParamsClinicalImpression] of TSearchParamsClinicalImpression = (
    spClinicalImpression__content, spClinicalImpression__id, spClinicalImpression__lastUpdated, spClinicalImpression__profile, spClinicalImpression__query, spClinicalImpression__security, spClinicalImpression__tag, spClinicalImpression__text,
    spClinicalImpression_Action, spClinicalImpression_Assessor, spClinicalImpression_Date, spClinicalImpression_Finding, spClinicalImpression_Investigation, spClinicalImpression_Patient, spClinicalImpression_Plan, spClinicalImpression_Previous,
    spClinicalImpression_Problem, spClinicalImpression_Resolved, spClinicalImpression_Ruledout, spClinicalImpression_Status, spClinicalImpression_Trigger, spClinicalImpression_Triggercode);

procedure TFhirIndexInformation.buildIndexesClinicalImpression;
var
  a : TSearchParamsClinicalImpression;
begin
  for a := low(TSearchParamsClinicalImpression) to high(TSearchParamsClinicalImpression) do
  begin
    assert(CHECK_TSearchParamsClinicalImpression[a] = a);
    indexes.add(frtClinicalImpression, CODES_TSearchParamsClinicalImpression[a], DESC_TSearchParamsClinicalImpression[a], TYPES_TSearchParamsClinicalImpression[a], TARGETS_TSearchParamsClinicalImpression[a], PATHS_TSearchParamsClinicalImpression[a], USES_TSearchParamsClinicalImpression[a]);
  end;
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

const
  CHECK_TSearchParamsCommunication : Array[TSearchParamsCommunication] of TSearchParamsCommunication = (
    spCommunication__content, spCommunication__id, spCommunication__lastUpdated, spCommunication__profile, spCommunication__query, spCommunication__security, spCommunication__tag, spCommunication__text,
    spCommunication_Category, spCommunication_Encounter, spCommunication_Identifier, spCommunication_Medium, spCommunication_Patient, spCommunication_Received,
    spCommunication_Recipient, spCommunication_Request, spCommunication_Sender, spCommunication_Sent, spCommunication_Status, spCommunication_Subject);

procedure TFhirIndexInformation.buildIndexesCommunication;
var
  a : TSearchParamsCommunication;
begin
  for a := low(TSearchParamsCommunication) to high(TSearchParamsCommunication) do
  begin
    assert(CHECK_TSearchParamsCommunication[a] = a);
    indexes.add(frtCommunication, CODES_TSearchParamsCommunication[a], DESC_TSearchParamsCommunication[a], TYPES_TSearchParamsCommunication[a], TARGETS_TSearchParamsCommunication[a], PATHS_TSearchParamsCommunication[a], USES_TSearchParamsCommunication[a]);
  end;
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

const
  CHECK_TSearchParamsCommunicationRequest : Array[TSearchParamsCommunicationRequest] of TSearchParamsCommunicationRequest = (
    spCommunicationRequest__content, spCommunicationRequest__id, spCommunicationRequest__lastUpdated, spCommunicationRequest__profile, spCommunicationRequest__query, spCommunicationRequest__security, spCommunicationRequest__tag, spCommunicationRequest__text,
    spCommunicationRequest_Category, spCommunicationRequest_Encounter, spCommunicationRequest_Identifier, spCommunicationRequest_Medium, spCommunicationRequest_Patient, spCommunicationRequest_Priority, spCommunicationRequest_Recipient,
    spCommunicationRequest_Requested, spCommunicationRequest_Requester, spCommunicationRequest_Sender, spCommunicationRequest_Status, spCommunicationRequest_Subject, spCommunicationRequest_Time);

procedure TFhirIndexInformation.buildIndexesCommunicationRequest;
var
  a : TSearchParamsCommunicationRequest;
begin
  for a := low(TSearchParamsCommunicationRequest) to high(TSearchParamsCommunicationRequest) do
  begin
    assert(CHECK_TSearchParamsCommunicationRequest[a] = a);
    indexes.add(frtCommunicationRequest, CODES_TSearchParamsCommunicationRequest[a], DESC_TSearchParamsCommunicationRequest[a], TYPES_TSearchParamsCommunicationRequest[a], TARGETS_TSearchParamsCommunicationRequest[a], PATHS_TSearchParamsCommunicationRequest[a], USES_TSearchParamsCommunicationRequest[a]);
  end;
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

const
  CHECK_TSearchParamsDeviceComponent : Array[TSearchParamsDeviceComponent] of TSearchParamsDeviceComponent = (
    spDeviceComponent__content, spDeviceComponent__id, spDeviceComponent__lastUpdated, spDeviceComponent__profile, spDeviceComponent__query, spDeviceComponent__security, spDeviceComponent__tag, spDeviceComponent__text,
    spDeviceComponent_Parent, spDeviceComponent_Source, spDeviceComponent_Type);

procedure TFhirIndexInformation.buildIndexesDeviceComponent;
var
  a : TSearchParamsDeviceComponent;
begin
  for a := low(TSearchParamsDeviceComponent) to high(TSearchParamsDeviceComponent) do
  begin
    assert(CHECK_TSearchParamsDeviceComponent[a] = a);
    indexes.add(frtDeviceComponent, CODES_TSearchParamsDeviceComponent[a], DESC_TSearchParamsDeviceComponent[a], TYPES_TSearchParamsDeviceComponent[a], TARGETS_TSearchParamsDeviceComponent[a], PATHS_TSearchParamsDeviceComponent[a], USES_TSearchParamsDeviceComponent[a]);
  end;
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

const
  CHECK_TSearchParamsDeviceMetric : Array[TSearchParamsDeviceMetric] of TSearchParamsDeviceMetric = ( spDeviceMetric__content, spDeviceMetric__id, spDeviceMetric__lastUpdated, spDeviceMetric__profile, spDeviceMetric__query, spDeviceMetric__security, spDeviceMetric__tag, spDeviceMetric__text,
    spDeviceMetric_Category, spDeviceMetric_Identifier, spDeviceMetric_Parent, spDeviceMetric_Source, spDeviceMetric_Type);

procedure TFhirIndexInformation.buildIndexesDeviceMetric;
var
  a : TSearchParamsDeviceMetric;
begin
  for a := low(TSearchParamsDeviceMetric) to high(TSearchParamsDeviceMetric) do
  begin
    assert(CHECK_TSearchParamsDeviceMetric[a] = a);
    indexes.add(frtDeviceMetric, CODES_TSearchParamsDeviceMetric[a], DESC_TSearchParamsDeviceMetric[a], TYPES_TSearchParamsDeviceMetric[a], TARGETS_TSearchParamsDeviceMetric[a], PATHS_TSearchParamsDeviceMetric[a], USES_TSearchParamsDeviceMetric[a]);
  end;
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

const
  CHECK_TSearchParamsDeviceUseRequest : Array[TSearchParamsDeviceUseRequest] of TSearchParamsDeviceUseRequest = ( spDeviceUseRequest__content, spDeviceUseRequest__id, spDeviceUseRequest__lastUpdated, spDeviceUseRequest__profile, spDeviceUseRequest__query, spDeviceUseRequest__security, spDeviceUseRequest__tag, spDeviceUseRequest__text,
    spDeviceUseRequest_Device, spDeviceUseRequest_Patient, spDeviceUseRequest_Subject);

procedure TFhirIndexInformation.buildIndexesDeviceUseRequest;
var
  a : TSearchParamsDeviceUseRequest;
begin
  for a := low(TSearchParamsDeviceUseRequest) to high(TSearchParamsDeviceUseRequest) do
  begin
    assert(CHECK_TSearchParamsDeviceUseRequest[a] = a);
    indexes.add(frtDeviceUseRequest, CODES_TSearchParamsDeviceUseRequest[a], DESC_TSearchParamsDeviceUseRequest[a], TYPES_TSearchParamsDeviceUseRequest[a], TARGETS_TSearchParamsDeviceUseRequest[a], PATHS_TSearchParamsDeviceUseRequest[a], USES_TSearchParamsDeviceUseRequest[a]);
  end;
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

const
  CHECK_TSearchParamsDeviceUseStatement : Array[TSearchParamsDeviceUseStatement] of TSearchParamsDeviceUseStatement = ( spDeviceUseStatement__content, spDeviceUseStatement__id, spDeviceUseStatement__lastUpdated, spDeviceUseStatement__profile, spDeviceUseStatement__query, spDeviceUseStatement__security, spDeviceUseStatement__tag, spDeviceUseStatement__text,
    spDeviceUseStatement_Device, spDeviceUseStatement_Patient, spDeviceUseStatement_Subject);

procedure TFhirIndexInformation.buildIndexesDeviceUseStatement;
var
  a : TSearchParamsDeviceUseStatement;
begin
  for a := low(TSearchParamsDeviceUseStatement) to high(TSearchParamsDeviceUseStatement) do
  begin
    assert(CHECK_TSearchParamsDeviceUseStatement[a] = a);
    indexes.add(frtDeviceUseStatement, CODES_TSearchParamsDeviceUseStatement[a], DESC_TSearchParamsDeviceUseStatement[a], TYPES_TSearchParamsDeviceUseStatement[a], TARGETS_TSearchParamsDeviceUseStatement[a], PATHS_TSearchParamsDeviceUseStatement[a], USES_TSearchParamsDeviceUseStatement[a]);
  end;
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

const
  CHECK_TSearchParamsEpisodeOfCare : Array[TSearchParamsEpisodeOfCare] of TSearchParamsEpisodeOfCare = ( spEpisodeOfCare__content, spEpisodeOfCare__id, spEpisodeOfCare__lastUpdated, spEpisodeOfCare__profile, spEpisodeOfCare__query, spEpisodeOfCare__security, spEpisodeOfCare__tag, spEpisodeOfCare__text,
    {$IFDEF FHIR_DSTU2}
    spEpisodeOfCare_Caremanager, spEpisodeOfCare_Condition, spEpisodeOfCare_Date, spEpisodeOfCare_Identifier, spEpisodeOfCare_Incomingreferral, spEpisodeOfCare_Organization, spEpisodeOfCare_Patient, spEpisodeOfCare_Status, spEpisodeOfCare_Teammember, spEpisodeOfCare_Type);
    {$ELSE}
    spEpisodeOfCare_Caremanager, spEpisodeOfCare_Condition, spEpisodeOfCare_Date, spEpisodeOfCare_Identifier, spEpisodeOfCare_Incomingreferral, spEpisodeOfCare_Organization, spEpisodeOfCare_Patient, spEpisodeOfCare_Status, spEpisodeOfCare_Type);
    {$ENDIF}

procedure TFhirIndexInformation.buildIndexesEpisodeOfCare;
var
  a : TSearchParamsEpisodeOfCare;
begin
  for a := low(TSearchParamsEpisodeOfCare) to high(TSearchParamsEpisodeOfCare) do
  begin
    assert(CHECK_TSearchParamsEpisodeOfCare[a] = a);
    indexes.add(frtEpisodeOfCare, CODES_TSearchParamsEpisodeOfCare[a], DESC_TSearchParamsEpisodeOfCare[a], TYPES_TSearchParamsEpisodeOfCare[a], TARGETS_TSearchParamsEpisodeOfCare[a], PATHS_TSearchParamsEpisodeOfCare[a], USES_TSearchParamsEpisodeOfCare[a]);
  end;
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
  {$IFDEF FHIR_DSTU2}
  for i := 0 to resource.careTeamList.Count - 1 do
    index(context, frtEpisodeOfCare, key, 0, resource.careTeamList[i].member, CODES_TSearchParamsEpisodeOfCare[spEpisodeOfCare_teammember]);
  {$ENDIF}
end;

const
  CHECK_TSearchParamsGoal : Array[TSearchParamsGoal] of TSearchParamsGoal = (
    spGoal__content, spGoal__id, spGoal__lastUpdated, spGoal__profile, spGoal__query, spGoal__security, spGoal__tag, spGoal__text,
    spGoal_Category, spGoal_Identifier, spGoal_Patient, spGoal_Status, spGoal_Subject, spGoal_Targetdate);

procedure TFhirIndexInformation.buildIndexesGoal;
var
  a : TSearchParamsGoal;
begin
  for a := low(TSearchParamsGoal) to high(TSearchParamsGoal) do
  begin
    assert(CHECK_TSearchParamsGoal[a] = a);
    indexes.add(frtGoal, CODES_TSearchParamsGoal[a], DESC_TSearchParamsGoal[a], TYPES_TSearchParamsGoal[a], TARGETS_TSearchParamsGoal[a], PATHS_TSearchParamsGoal[a], USES_TSearchParamsGoal[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesGoal(key: integer; id : String; context : TFhirResource; resource: TFhirGoal);
begin
  index(frtGoal, key, 0, resource.identifierList, CODES_TSearchParamsGoal[spGoal_identifier]);
  index(frtGoal, key, 0, resource.statusElement, CODES_TSearchParamsGoal[spGoal_status]);
  index(context, frtGoal, key, 0, resource.subject, CODES_TSearchParamsGoal[spGoal_subject]);
  index(context, frtGoal, key, 0, resource.subject, CODES_TSearchParamsGoal[spGoal_patient], frtPatient);
  patientCompartment(key, resource.subject);
end;

const
  CHECK_TSearchParamsImagingObjectSelection : Array[TSearchParamsImagingObjectSelection] of TSearchParamsImagingObjectSelection = (
    spImagingObjectSelection__content, spImagingObjectSelection__id, spImagingObjectSelection__lastUpdated, spImagingObjectSelection__profile, spImagingObjectSelection__query, spImagingObjectSelection__security, spImagingObjectSelection__tag, spImagingObjectSelection__text,
    spImagingObjectSelection_Author, spImagingObjectSelection_Authoringtime, spImagingObjectSelection_Identifier, spImagingObjectSelection_Patient, spImagingObjectSelection_Selectedstudy, spImagingObjectSelection_Title);

procedure TFhirIndexInformation.buildIndexesImagingObjectSelection;
var
  a : TSearchParamsImagingObjectSelection;
begin
  for a := low(TSearchParamsImagingObjectSelection) to high(TSearchParamsImagingObjectSelection) do
  begin
    assert(CHECK_TSearchParamsImagingObjectSelection[a] = a);
    indexes.add(frtImagingObjectSelection, CODES_TSearchParamsImagingObjectSelection[a], DESC_TSearchParamsImagingObjectSelection[a], TYPES_TSearchParamsImagingObjectSelection[a], TARGETS_TSearchParamsImagingObjectSelection[a], PATHS_TSearchParamsImagingObjectSelection[a], USES_TSearchParamsImagingObjectSelection[a]);
  end;
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


const
  CHECK_TSearchParamsPerson : Array[TSearchParamsPerson] of TSearchParamsPerson = (
    spPerson__content, spPerson__id, spPerson__lastUpdated, spPerson__profile, spPerson__query, spPerson__security, spPerson__tag, spPerson__text,
    spPerson_Address, spPerson_Addresscity, spPerson_Addresscountry, spPerson_Addresspostalcode, spPerson_Addressstate, spPerson_Addressuse, spPerson_Birthdate, spPerson_Email, spPerson_Gender, spPerson_Identifier, spPerson_Link,
    spPerson_Name, spPerson_Organization, spPerson_Patient, spPerson_Phone, spPerson_Phonetic, spPerson_Practitioner, spPerson_Relatedperson, spPerson_Telecom);

procedure TFhirIndexInformation.buildIndexesPerson;
var
  a : TSearchParamsPerson;
begin
  for a := low(TSearchParamsPerson) to high(TSearchParamsPerson) do
  begin
    assert(CHECK_TSearchParamsPerson[a] = a);
    indexes.add(frtPerson, CODES_TSearchParamsPerson[a], DESC_TSearchParamsPerson[a], TYPES_TSearchParamsPerson[a], TARGETS_TSearchParamsPerson[a], PATHS_TSearchParamsPerson[a], USES_TSearchParamsPerson[a]);
  end;
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

const
  CHECK_TSearchParamsProcedureRequest : Array[TSearchParamsProcedureRequest] of TSearchParamsProcedureRequest = ( spProcedureRequest__content, spProcedureRequest__id, spProcedureRequest__lastUpdated, spProcedureRequest__profile, spProcedureRequest__query, spProcedureRequest__security, spProcedureRequest__tag, spProcedureRequest__text,
    spProcedureRequest_Encounter, spProcedureRequest_Identifier, spProcedureRequest_Orderer, spProcedureRequest_Patient, spProcedureRequest_Performer, spProcedureRequest_Subject);

procedure TFhirIndexInformation.buildIndexesProcedureRequest;
var
  a : TSearchParamsProcedureRequest;
begin
  for a := low(TSearchParamsProcedureRequest) to high(TSearchParamsProcedureRequest) do
  begin
    assert(CHECK_TSearchParamsProcedureRequest[a] = a);
    indexes.add(frtProcedureRequest, CODES_TSearchParamsProcedureRequest[a], DESC_TSearchParamsProcedureRequest[a], TYPES_TSearchParamsProcedureRequest[a], TARGETS_TSearchParamsProcedureRequest[a], PATHS_TSearchParamsProcedureRequest[a], USES_TSearchParamsProcedureRequest[a]);
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


const
  CHECK_TSearchParamsSearchParameter : Array[TSearchParamsSearchParameter] of TSearchParamsSearchParameter = (
    spSearchParameter__content, spSearchParameter__id, spSearchParameter__lastUpdated, spSearchParameter__profile, spSearchParameter__query, spSearchParameter__security, spSearchParameter__tag, spSearchParameter__text,
    spSearchParameter_Base, spSearchParameter_Code, spSearchParameter_Context, spSearchParameter_Description, spSearchParameter_Name, spSearchParameter_Target, spSearchParameter_Type, spSearchParameter_Url);

procedure TFhirIndexInformation.buildIndexesSearchParameter;
var
  a : TSearchParamsSearchParameter;
begin
  for a := low(TSearchParamsSearchParameter) to high(TSearchParamsSearchParameter) do
  begin
    assert(CHECK_TSearchParamsSearchParameter[a] = a);
    indexes.add(frtSearchParameter, CODES_TSearchParamsSearchParameter[a], DESC_TSearchParamsSearchParameter[a], TYPES_TSearchParamsSearchParameter[a], TARGETS_TSearchParamsSearchParameter[a], PATHS_TSearchParamsSearchParameter[a], USES_TSearchParamsSearchParameter[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesSearchParameter(key: integer; id : String; context : TFhirResource; resource: TFhirSearchParameter);
var
  i : integer;
begin
  index(frtSearchParameter, key, 0, resource.baseElement, CODES_TSearchParamsSearchParameter[spSearchParameter_base]);
  index(frtSearchParameter, key, 0, resource.description, CODES_TSearchParamsSearchParameter[spSearchParameter_description]);
  index(frtSearchParameter, key, 0, resource.name, CODES_TSearchParamsSearchParameter[spSearchParameter_name]);
  index(frtSearchParameter, key, 0, resource.code, CODES_TSearchParamsSearchParameter[spSearchParameter_code]);
  index(frtSearchParameter, key, 0, resource.useContextList, CODES_TSearchParamsSearchParameter[spSearchParameter_Context]);
  for i := 0 to resource.targetList.count - 1  do
    index(frtSearchParameter, key, 0, resource.targetList[i], CODES_TSearchParamsSearchParameter[spSearchParameter_target]);
  index(frtSearchParameter, key, 0, resource.type_Element, CODES_TSearchParamsSearchParameter[spSearchParameter_type]);
  index(frtSearchParameter, key, 0, resource.url, CODES_TSearchParamsSearchParameter[spSearchParameter_url]);
end;


const
  CHECK_TSearchParamsVisionPrescription : Array[TSearchParamsVisionPrescription] of TSearchParamsVisionPrescription = ( spVisionPrescription__content, spVisionPrescription__id, spVisionPrescription__lastUpdated, spVisionPrescription__profile, spVisionPrescription__query, spVisionPrescription__security, spVisionPrescription__tag, spVisionPrescription__text,
    spVisionPrescription_Datewritten, spVisionPrescription_Encounter, spVisionPrescription_Identifier, spVisionPrescription_Patient, spVisionPrescription_Prescriber);

procedure TFhirIndexInformation.buildIndexesVisionPrescription;
var
  a : TSearchParamsVisionPrescription;
begin
  for a := low(TSearchParamsVisionPrescription) to high(TSearchParamsVisionPrescription) do
  begin
    assert(CHECK_TSearchParamsVisionPrescription[a] = a);
    indexes.add(frtVisionPrescription, CODES_TSearchParamsVisionPrescription[a], DESC_TSearchParamsVisionPrescription[a], TYPES_TSearchParamsVisionPrescription[a], TARGETS_TSearchParamsVisionPrescription[a], PATHS_TSearchParamsVisionPrescription[a], USES_TSearchParamsVisionPrescription[a]);
  end;
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

procedure TFhirIndexManager.compareIndexEntries(rt, n, expr: string);
var
  manual, auto : TAdvList<TFhirIndexEntry>;
  m, a : TFhirIndexEntry;
  ok : boolean;
begin
  manual := TAdvList<TFhirIndexEntry>.create;
  auto := TAdvList<TFhirIndexEntry>.create;
  try
    FManualEntries.filter(FInfo.Indexes, n, manual);
    FPathEntries.filter(FInfo.Indexes, n, auto);
    if manual.Count <> auto.Count then
      raise Exception.Create('manual and auto counts differ for '+rt+':'+n+' for expression: '+expr);
    for m in manual do
    begin
      ok := false;
      for a in auto do
        if (a.FValue1 = m.FValue1) and (a.FValue2 = m.FValue2) and (a.FType = m.FType) then
          ok := true;
      if not ok then
        raise Exception.Create('manual and auto counts differ for '+rt+':'+n);
    end;
  finally
    manual.free;
    auto.free;
  end;
end;

const
  CHECK_TSearchParamsProcessRequest : Array[TSearchParamsProcessRequest] of TSearchParamsProcessRequest = ( spProcessRequest__content, spProcessRequest__id, spProcessRequest__lastUpdated, spProcessRequest__profile, spProcessRequest__query, spProcessRequest__security, spProcessRequest__tag, spProcessRequest__text,
    spProcessRequest_Action, spProcessRequest_Identifier, spProcessRequest_Organizationidentifier, spProcessRequest_Organizationreference, spProcessRequest_Provideridentifier, spProcessRequest_Providerreference);

procedure TFhirIndexInformation.buildIndexesProcessRequest;
var
  a : TSearchParamsProcessRequest;
begin
  for a := low(TSearchParamsProcessRequest) to high(TSearchParamsProcessRequest) do
  begin
    assert(CHECK_TSearchParamsProcessRequest[a] = a);
    indexes.add(frtProcessRequest, CODES_TSearchParamsProcessRequest[a], DESC_TSearchParamsProcessRequest[a], TYPES_TSearchParamsProcessRequest[a], TARGETS_TSearchParamsProcessRequest[a], PATHS_TSearchParamsProcessRequest[a], USES_TSearchParamsProcessRequest[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesProcessRequest(key: integer; id : String; context : TFhirResource; resource: TFhirProcessRequest);
var
  i : integer;
begin
  index(frtProcessRequest, key, 0, resource.identifierList, CODES_TSearchParamsProcessRequest[spProcessRequest_identifier]);
  index(frtProcessRequest, key, 0, resource.actionElement, CODES_TSearchParamsProcessRequest[spProcessRequest_action]);
  if resource.organization is TFhirReference then
    index(context, frtProcessRequest, key, 0, TFhirReference(resource.organization), CODES_TSearchParamsProcessRequest[spProcessRequest_Organizationreference])
  else if resource.organization is TFhirIdentifier then
    index(frtProcessRequest, key, 0, TFhirIdentifier(resource.organization), CODES_TSearchParamsProcessRequest[spProcessRequest_Organizationidentifier]);
  if resource.provider is TFhirReference then
    index(context, frtProcessRequest, key, 0, TFhirReference(resource.provider), CODES_TSearchParamsProcessRequest[spProcessRequest_Providerreference])
  else if resource.provider is TFhirIdentifier then
    index(frtProcessRequest, key, 0, TFhirIdentifier(resource.provider), CODES_TSearchParamsProcessRequest[spProcessRequest_Provideridentifier]);
end;

const
  CHECK_TSearchParamsProcessResponse : Array[TSearchParamsProcessResponse] of TSearchParamsProcessResponse = ( spProcessResponse__content, spProcessResponse__id, spProcessResponse__lastUpdated, spProcessResponse__profile, spProcessResponse__query, spProcessResponse__security, spProcessResponse__tag, spProcessResponse__text,
    spProcessResponse_Identifier, spProcessResponse_Organizationidentifier, spProcessResponse_Organizationreference, spProcessResponse_Requestidentifier, spProcessResponse_Requestorganizationidentifier, spProcessResponse_Requestorganizationreference, spProcessResponse_Requestprovideridentifier, spProcessResponse_Requestproviderreference, spProcessResponse_Requestreference);

procedure TFhirIndexInformation.buildIndexesProcessResponse;
var
  a : TSearchParamsProcessResponse;
begin
  for a := low(TSearchParamsProcessResponse) to high(TSearchParamsProcessResponse) do
  begin
    assert(CHECK_TSearchParamsProcessResponse[a] = a);
    indexes.add(frtProcessResponse, CODES_TSearchParamsProcessResponse[a], DESC_TSearchParamsProcessResponse[a], TYPES_TSearchParamsProcessResponse[a], TARGETS_TSearchParamsProcessResponse[a], PATHS_TSearchParamsProcessResponse[a], USES_TSearchParamsProcessResponse[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesProcessResponse(key: integer; id : String; context : TFhirResource; resource: TFhirProcessResponse);
var
  i : integer;
begin
  index(frtProcessResponse, key, 0, resource.identifierList, CODES_TSearchParamsProcessResponse[spProcessResponse_identifier]);

  if resource.request is TFhirReference then
    index(context, frtProcessResponse, key, 0, TFhirReference(resource.request), CODES_TSearchParamsProcessResponse[spProcessResponse_Requestreference])
  else if resource.request is TFhirIdentifier then
    index(frtProcessResponse, key, 0, TFhirIdentifier(resource.request), CODES_TSearchParamsProcessResponse[spProcessResponse_Requestidentifier]);
  if resource.organization is TFhirReference then
    index(context, frtProcessResponse, key, 0, TFhirReference(resource.organization), CODES_TSearchParamsProcessResponse[spProcessResponse_Organizationreference])
  else if resource.organization is TFhirIdentifier then
    index(frtProcessResponse, key, 0, TFhirIdentifier(resource.organization), CODES_TSearchParamsProcessResponse[spProcessResponse_Organizationidentifier]);
  if resource.requestOrganization is TFhirReference then
    index(context, frtProcessResponse, key, 0, TFhirReference(resource.requestOrganization), CODES_TSearchParamsProcessResponse[spProcessResponse_Requestorganizationreference])
  else if resource.requestOrganization is TFhirIdentifier then
    index(frtProcessResponse, key, 0, TFhirIdentifier(resource.requestOrganization), CODES_TSearchParamsProcessResponse[spProcessResponse_Requestorganizationidentifier]);
  if resource.requestProvider is TFhirReference then
    index(context, frtProcessResponse, key, 0, TFhirReference(resource.requestProvider), CODES_TSearchParamsProcessResponse[spProcessResponse_Requestproviderreference])
  else if resource.requestProvider is TFhirIdentifier then
    index(frtProcessResponse, key, 0, TFhirIdentifier(resource.requestProvider), CODES_TSearchParamsProcessResponse[spProcessResponse_Requestprovideridentifier]);
end;


const
  CHECK_TSearchParamsAccount : Array[TSearchParamsAccount] of TSearchParamsAccount = (
    spAccount__content, spAccount__id, spAccount__lastUpdated, spAccount__profile, spAccount__query, spAccount__security, spAccount__tag, spAccount__text,
    spAccount_Balance, spAccount_Identifier, spAccount_Name, spAccount_Owner, spAccount_Patient, spAccount_Period, spAccount_Status, spAccount_Subject, spAccount_Type);

procedure TFhirIndexInformation.buildIndexesAccount;
var
  a : TSearchParamsAccount;
begin
  for a := low(TSearchParamsAccount) to high(TSearchParamsAccount) do
  begin
    assert(CHECK_TSearchParamsAccount[a] = a);
    indexes.add(frtAccount, CODES_TSearchParamsAccount[a], DESC_TSearchParamsAccount[a], TYPES_TSearchParamsAccount[a], TARGETS_TSearchParamsAccount[a], PATHS_TSearchParamsAccount[a], USES_TSearchParamsAccount[a]);
  end;
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


const
  CHECK_TSearchParamsImplementationGuide : Array[TSearchParamsImplementationGuide] of TSearchParamsImplementationGuide = (
    spImplementationGuide__content, spImplementationGuide__id, spImplementationGuide__lastUpdated, spImplementationGuide__profile, spImplementationGuide__query, spImplementationGuide__security, spImplementationGuide__tag, spImplementationGuide__text,
    {$IFDEF FHIR_DSTU2}
    spImplementationGuide_Context, spImplementationGuide_Date, spImplementationGuide_Dependency, spImplementationGuide_Description, spImplementationGuide_Experimental, spImplementationGuide_Name, spImplementationGuide_Publisher, spImplementationGuide_Status, spImplementationGuide_Url, spImplementationGuide_Version);
    {$ELSE}
    spImplementationGuide_Context, spImplementationGuide_Date, spImplementationGuide_Dependency, spImplementationGuide_Description, spImplementationGuide_Experimental, spImplementationGuide_Name, spImplementationGuide_Publisher, spImplementationGuide_Resource, spImplementationGuide_Status, spImplementationGuide_Url, spImplementationGuide_Version);
    {$ENDIF}

procedure TFhirIndexInformation.buildIndexesImplementationGuide;
var
  a : TSearchParamsImplementationGuide;
begin
  for a := low(TSearchParamsImplementationGuide) to high(TSearchParamsImplementationGuide) do
  begin
    assert(CHECK_TSearchParamsImplementationGuide[a] = a);
    indexes.add(frtImplementationGuide, CODES_TSearchParamsImplementationGuide[a], DESC_TSearchParamsImplementationGuide[a], TYPES_TSearchParamsImplementationGuide[a], TARGETS_TSearchParamsImplementationGuide[a], PATHS_TSearchParamsImplementationGuide[a], USES_TSearchParamsImplementationGuide[a]);
  end;
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
  for p in resource.packageList do
    for r in p.resourceList do
      if r.sourceElement is TFhirReference then
        index(context, frtImplementationGuide, key, 0, TFhirReference(r.sourceElement), CODES_TSearchParamsImplementationGuide[spImplementationGuide_Resource]);
end;

{$IFDEF FHIR_DSTU3}
const
  CHECK_TSearchParamsCompartmentDefinition : Array[TSearchParamsCompartmentDefinition] of TSearchParamsCompartmentDefinition = (
    spCompartmentDefinition__content, spCompartmentDefinition__id, spCompartmentDefinition__lastUpdated, spCompartmentDefinition__profile, spCompartmentDefinition__query, spCompartmentDefinition__security, spCompartmentDefinition__tag, spCompartmentDefinition__text,
    spCompartmentDefinition_Code, spCompartmentDefinition_Date, spCompartmentDefinition_Name, spCompartmentDefinition_Resource, spCompartmentDefinition_Status, spCompartmentDefinition_Url);

procedure TFhirIndexInformation.buildIndexesCompartmentDefinition;
var
  a : TSearchParamsCompartmentDefinition;
begin
  for a := low(TSearchParamsCompartmentDefinition) to high(TSearchParamsCompartmentDefinition) do
  begin
    assert(CHECK_TSearchParamsCompartmentDefinition[a] = a);
    indexes.add(frtCompartmentDefinition, CODES_TSearchParamsCompartmentDefinition[a], DESC_TSearchParamsCompartmentDefinition[a], TYPES_TSearchParamsCompartmentDefinition[a], TARGETS_TSearchParamsCompartmentDefinition[a], PATHS_TSearchParamsCompartmentDefinition[a], USES_TSearchParamsCompartmentDefinition[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesCompartmentDefinition(key: integer; id : String; context : TFhirResource; resource: TFhirCompartmentDefinition);
var
  i : integer;
begin
  index(frtCompartmentDefinition, key, 0, resource.codeElement, CODES_TSearchParamsCompartmentDefinition[spCompartmentDefinition_code]);
  index(frtCompartmentDefinition, key, 0, resource.dateElement, CODES_TSearchParamsCompartmentDefinition[spCompartmentDefinition_date]);
  index(frtCompartmentDefinition, key, 0, resource.nameElement, CODES_TSearchParamsCompartmentDefinition[spCompartmentDefinition_name]);
  for i := 0 to resource.resourceList.Count - 1 do
    index(frtCompartmentDefinition, key, 0, resource.resourceList[i].codeElement, CODES_TSearchParamsCompartmentDefinition[spCompartmentDefinition_resource]);
  index(frtCompartmentDefinition, key, 0, resource.statusElement, CODES_TSearchParamsCompartmentDefinition[spCompartmentDefinition_status]);
  index(frtCompartmentDefinition, key, 0, resource.urlElement, CODES_TSearchParamsCompartmentDefinition[spCompartmentDefinition_url]);
end;

const
  CHECK_TSearchParamsStructureMap : Array[TSearchParamsStructureMap] of TSearchParamsStructureMap = (
    spStructureMap__content, spStructureMap__id, spStructureMap__lastUpdated, spStructureMap__profile, spStructureMap__query, spStructureMap__security, spStructureMap__tag, spStructureMap__text,
    spStructureMap_Context, spStructureMap_Date, spStructureMap_Description, spStructureMap_Experimental, spStructureMap_Identifier, spStructureMap_Name, spStructureMap_Publisher, spStructureMap_Status, spStructureMap_Url, spStructureMap_Version);

procedure TFhirIndexInformation.buildIndexesStructureMap;
var
  a : TSearchParamsStructureMap;
begin
  for a := low(TSearchParamsStructureMap) to high(TSearchParamsStructureMap) do
  begin
    assert(CHECK_TSearchParamsStructureMap[a] = a);
    indexes.add(frtStructureMap, CODES_TSearchParamsStructureMap[a], DESC_TSearchParamsStructureMap[a], TYPES_TSearchParamsStructureMap[a], TARGETS_TSearchParamsStructureMap[a], PATHS_TSearchParamsStructureMap[a], USES_TSearchParamsStructureMap[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesStructureMap(key: integer; id : String; context : TFhirResource; resource: TFhirStructureMap);
var
  i : integer;
begin
  index(frtStructureMap, key, 0, resource.useContextList, CODES_TSearchParamsStructureMap[spStructureMap_context]);
  index(frtStructureMap, key, 0, resource.dateElement, CODES_TSearchParamsStructureMap[spStructureMap_date]);
  index(frtStructureMap, key, 0, resource.descriptionElement, CODES_TSearchParamsStructureMap[spStructureMap_description]);
  index(frtStructureMap, key, 0, resource.experimentalElement, CODES_TSearchParamsStructureMap[spStructureMap_experimental]);
  index(frtStructureMap, key, 0, resource.identifierList, CODES_TSearchParamsStructureMap[spStructureMap_identifier]);
  index(frtStructureMap, key, 0, resource.nameElement, CODES_TSearchParamsStructureMap[spStructureMap_name]);
  index(frtStructureMap, key, 0, resource.publisherElement, CODES_TSearchParamsStructureMap[spStructureMap_publisher]);
  index(frtStructureMap, key, 0, resource.statusElement, CODES_TSearchParamsStructureMap[spStructureMap_status]);
  index(frtStructureMap, key, 0, resource.urlElement, CODES_TSearchParamsStructureMap[spStructureMap_url]);
  index(frtStructureMap, key, 0, resource.versionElement, CODES_TSearchParamsStructureMap[spStructureMap_version]);
end;

const
  CHECK_TSearchParamsMeasureReport : Array[TSearchParamsMeasureReport] of TSearchParamsMeasureReport = (
    spMeasureReport__content, spMeasureReport__id, spMeasureReport__lastUpdated, spMeasureReport__profile, spMeasureReport__query, spMeasureReport__security, spMeasureReport__tag, spMeasureReport__text,
    spMeasureReport_Patient);

procedure TFhirIndexInformation.buildIndexesMeasureReport;
var
  a : TSearchParamsMeasureReport;
begin
  for a := low(TSearchParamsMeasureReport) to high(TSearchParamsMeasureReport) do
  begin
    assert(CHECK_TSearchParamsMeasureReport[a] = a);
    indexes.add(frtMeasureReport, CODES_TSearchParamsMeasureReport[a], DESC_TSearchParamsMeasureReport[a], TYPES_TSearchParamsMeasureReport[a], TARGETS_TSearchParamsMeasureReport[a], PATHS_TSearchParamsMeasureReport[a], USES_TSearchParamsMeasureReport[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesMeasureReport(key: integer; id : String; context : TFhirResource; resource: TFhirMeasureReport);
var
  i : integer;
begin
  index(context, frtMeasureReport, key, 0, resource.patient, CODES_TSearchParamsMeasureReport[spMeasureReport_patient]);
end;

const
  CHECK_TSearchParamsCareTeam : Array[TSearchParamsCareTeam] of TSearchParamsCareTeam = (
    spCareTeam__content, spCareTeam__id, spCareTeam__lastUpdated, spCareTeam__profile, spCareTeam__query, spCareTeam__security, spCareTeam__tag, spCareTeam__text,
    spCareTeam_Date, spCareTeam_Identifier, spCareTeam_Participant, spCareTeam_Patient, spCareTeam_Status, spCareTeam_Subject, spCareTeam_Type);

procedure TFhirIndexInformation.buildIndexesCareTeam;
var
  a : TSearchParamsCareTeam;
begin
  for a := low(TSearchParamsCareTeam) to high(TSearchParamsCareTeam) do
  begin
    assert(CHECK_TSearchParamsCareTeam[a] = a);
    indexes.add(frtCareTeam, CODES_TSearchParamsCareTeam[a], DESC_TSearchParamsCareTeam[a], TYPES_TSearchParamsCareTeam[a], TARGETS_TSearchParamsCareTeam[a], PATHS_TSearchParamsCareTeam[a], USES_TSearchParamsCareTeam[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesCareTeam(key: integer; id : String; context : TFhirResource; resource: TFhirCareTeam);
var
  p : TFhirCareTeamParticipant;
begin
  index(frtCareTeam, key, 0, resource.period, CODES_TSearchParamsCareTeam[spCareTeam_date]);
  index(frtCareTeam, key, 0, resource.identifierList, CODES_TSearchParamsCareTeam[spCareTeam_Identifier]);
  index(frtCareTeam, key, 0, resource.statusElement, CODES_TSearchParamsCareTeam[spCareTeam_Status]);
  index(frtCareTeam, key, 0, resource.type_List, CODES_TSearchParamsCareTeam[spCareTeam_Type]);
  index(context, frtCareTeam, key, 0, resource.subject, CODES_TSearchParamsCareTeam[spCareTeam_Subject]);
  index(context, frtCareTeam, key, 0, resource.subject, CODES_TSearchParamsCareTeam[spCareTeam_Patient], frtPatient);
  for p in resource.participantList do
  begin
    index(context, frtCareTeam, key, 0, p.member, CODES_TSearchParamsCareTeam[spCareTeam_patient], frtPatient);
    index(context, frtCareTeam, key, 0, p.member, CODES_TSearchParamsCareTeam[spCareTeam_Participant]);
  end;
end;

const
  CHECK_TSearchParamsTask : Array[TSearchParamsTask] of TSearchParamsTask = (
    spTask__content, spTask__id, spTask__lastUpdated, spTask__profile, spTask__query, spTask__security, spTask__tag, spTask__text,
    spTask_Created, spTask_Creator, spTask_Definition, spTask_Failure, spTask_Identifier, spTask_Modified, spTask_Owner, spTask_Parent, spTask_Performer,
    spTask_Priority, spTask_Status, spTask_Subject, spTask_Type);

procedure TFhirIndexInformation.buildIndexesTask;
var
  a : TSearchParamsTask;
begin
  for a := low(TSearchParamsTask) to high(TSearchParamsTask) do
  begin
    assert(CHECK_TSearchParamsTask[a] = a);
    indexes.add(frtTask, CODES_TSearchParamsTask[a], DESC_TSearchParamsTask[a], TYPES_TSearchParamsTask[a], TARGETS_TSearchParamsTask[a], PATHS_TSearchParamsTask[a], USES_TSearchParamsTask[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesTask(key: integer; id : String; context : TFhirResource; resource: TFhirTask);
var
  c : TFhirCoding;
begin
  index(frtTask, key, 0, resource.createdElement, CODES_TSearchParamsTask[spTask_created]);
  index(context, frtTask, key, 0, resource.creatorElement, CODES_TSearchParamsTask[spTask_creator]);
  index(frtTask, key, 0, resource.definitionElement, CODES_TSearchParamsTask[spTask_definition]);
  index(frtTask, key, 0, resource.failureReasonElement, CODES_TSearchParamsTask[spTask_failure]);
  index(frtTask, key, 0, resource.identifierElement, CODES_TSearchParamsTask[spTask_identifier]);
  index(frtTask, key, 0, resource.lastModifiedElement, CODES_TSearchParamsTask[spTask_modified]);
  index(context, frtTask, key, 0, resource.owner, CODES_TSearchParamsTask[spTask_owner]);
  for c in resource.performerTypeList do
    index(frtTask, key, 0, c, CODES_TSearchParamsTask[spTask_performer]);
  index(context, frtTask, key, 0, resource.parentElement, CODES_TSearchParamsTask[spTask_Parent]);
  index(frtTask, key, 0, resource.performerTypeList, CODES_TSearchParamsTask[spTask_Performer]);
  index(frtTask, key, 0, resource.priorityElement, CODES_TSearchParamsTask[spTask_priority]);
  index(frtTask, key, 0, resource.statusElement, CODES_TSearchParamsTask[spTask_status]);
  index(context, frtTask, key, 0, resource.subject, CODES_TSearchParamsTask[spTask_subject]);
  index(frtTask, key, 0, resource.type_, CODES_TSearchParamsTask[spTask_type]);
end;

const
  CHECK_TSearchParamsPractitionerRole : Array[TSearchParamsPractitionerRole] of TSearchParamsPractitionerRole = (
    spPractitionerRole__content, spPractitionerRole__id, spPractitionerRole__lastUpdated, spPractitionerRole__profile, spPractitionerRole__query, spPractitionerRole__security, spPractitionerRole__tag, spPractitionerRole__text,
    spPractitionerRole_Email, spPractitionerRole_Identifier, spPractitionerRole_Location, spPractitionerRole_Organization, spPractitionerRole_Phone, spPractitionerRole_Practitioner, spPractitionerRole_Role, spPractitionerRole_Specialty, spPractitionerRole_Telecom);

procedure TFhirIndexInformation.buildIndexesPractitionerRole;
var
  a : TSearchParamsPractitionerRole;
begin
  for a := low(TSearchParamsPractitionerRole) to high(TSearchParamsPractitionerRole) do
  begin
    assert(CHECK_TSearchParamsPractitionerRole[a] = a);
    indexes.add(frtPractitionerRole, CODES_TSearchParamsPractitionerRole[a], DESC_TSearchParamsPractitionerRole[a], TYPES_TSearchParamsPractitionerRole[a], TARGETS_TSearchParamsPractitionerRole[a], PATHS_TSearchParamsPractitionerRole[a], USES_TSearchParamsPractitionerRole[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesPractitionerRole(key: integer; id : String; context : TFhirResource; resource: TFhirPractitionerRole);
var
  i : integer;
begin
  for i := 0 to resource.telecomList.Count - 1 do
  begin
    if (resource.telecomList[i].system = ContactPointSystemEmail) then
      index(frtPractitionerRole, key, 0, resource.telecomList[i], CODES_TSearchParamsPractitionerRole[spPractitionerRole_email]);
    if (resource.telecomList[i].system = ContactPointSystemPhone) then
      index(frtPractitionerRole, key, 0, resource.telecomList[i], CODES_TSearchParamsPractitionerRole[spPractitionerRole_phone]);
    index(frtPractitionerRole, key, 0, resource.telecomList[i], CODES_TSearchParamsPractitionerRole[spPractitionerRole_telecom]);
  end;
  index(frtPractitionerRole, key, 0, resource.identifierList, CODES_TSearchParamsPractitionerRole[spPractitionerRole_identifier]);
  index(context, frtPractitionerRole, key, 0, resource.locationList, CODES_TSearchParamsPractitionerRole[spPractitionerRole_location]);
  index(context, frtPractitionerRole, key, 0, resource.organizationElement, CODES_TSearchParamsPractitionerRole[spPractitionerRole_organization]);
  index(context, frtPractitionerRole, key, 0, resource.practitionerElement, CODES_TSearchParamsPractitionerRole[spPractitionerRole_practitioner]);
  index(frtPractitionerRole, key, 0, resource.roleList, CODES_TSearchParamsPractitionerRole[spPractitionerRole_role]);
  index(frtPractitionerRole, key, 0, resource.specialtyList, CODES_TSearchParamsPractitionerRole[spPractitionerRole_specialty]);
end;

const
  CHECK_TSearchParamsImagingExcerpt : Array[TSearchParamsImagingExcerpt] of TSearchParamsImagingExcerpt = (
    spImagingExcerpt__content, spImagingExcerpt__id, spImagingExcerpt__lastUpdated, spImagingExcerpt__profile, spImagingExcerpt__query, spImagingExcerpt__security, spImagingExcerpt__tag, spImagingExcerpt__text,
    spImagingExcerpt_Author, spImagingExcerpt_Authoringtime, spImagingExcerpt_Identifier, spImagingExcerpt_Patient, spImagingExcerpt_Selectedstudy, spImagingExcerpt_Title);

procedure TFhirIndexInformation.buildIndexesImagingExcerpt;
var
  a : TSearchParamsImagingExcerpt;
begin
  for a := low(TSearchParamsImagingExcerpt) to high(TSearchParamsImagingExcerpt) do
  begin
    assert(CHECK_TSearchParamsImagingExcerpt[a] = a);
    indexes.add(frtImagingExcerpt, CODES_TSearchParamsImagingExcerpt[a], DESC_TSearchParamsImagingExcerpt[a], TYPES_TSearchParamsImagingExcerpt[a], TARGETS_TSearchParamsImagingExcerpt[a], PATHS_TSearchParamsImagingExcerpt[a], USES_TSearchParamsImagingExcerpt[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesImagingExcerpt(key: integer; id : String; context : TFhirResource; resource: TFhirImagingExcerpt);
var
  s : TFhirImagingExcerptStudy;
begin
  index(context, frtImagingExcerpt, key, 0, resource.authorElement, CODES_TSearchParamsImagingExcerpt[spImagingExcerpt_author]);
  index(frtImagingExcerpt, key, 0, resource.authoringTimeElement, CODES_TSearchParamsImagingExcerpt[spImagingExcerpt_authoringtime]);
  index(frtImagingExcerpt, key, 0, resource.uidElement, CODES_TSearchParamsImagingExcerpt[spImagingExcerpt_identifier]);
  index(context, frtImagingExcerpt, key, 0, resource.patient, CODES_TSearchParamsImagingExcerpt[spImagingExcerpt_patient]);
  for s in resource.studyList do
    index(frtImagingExcerpt, key, 0, s.uidElement, CODES_TSearchParamsImagingExcerpt[spImagingExcerpt_Selectedstudy]);
  index(frtImagingExcerpt, key, 0, resource.titleElement, CODES_TSearchParamsImagingExcerpt[spImagingExcerpt_title]);
end;

{$ENDIF}

{ TFhirCompartmentEntryList }

procedure TFhirCompartmentEntryList.add(key, ckey: integer; id: string);
var
  item : TFhirCompartmentEntry;
begin
  item := TFhirCompartmentEntry.create;
  try
    item.key := key;
    item.ckey := ckey;
    item.id := id;
    inherited add(item.Link);
  finally
    item.free;
  end;
end;

function TFhirCompartmentEntryList.GetItemN(iIndex: integer): TFhirCompartmentEntry;
begin
  result := TFhirCompartmentEntry(ObjectByIndex[iIndex]);
end;

function TFhirCompartmentEntryList.ItemClass: TAdvObjectClass;
begin
  result := TFhirCompartmentEntry;
end;

procedure TFhirCompartmentEntryList.removeById(id: String);
var
  i : integer;
begin
  for i := count - 1 downto 0 do
    if GetItemN(i).Id = id then
      DeleteByIndex(i);
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

procedure TFhirCompositeList.add(aResourceType: TFhirResourceType; name: String; components: array of String);
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

function TFhirCompositeList.getByName(atype: TFhirResourceType; name: String): TFhirComposite;
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

constructor TFHIRIndexInformation.Create;
begin
  inherited Create;
  FIndexes := TFhirIndexList.create;
  FComposites := TFhirCompositeList.create;
  buildIndexes;
end;

destructor TFHIRIndexInformation.Destroy;
begin
  FIndexes.Free;
  FComposites.Free;
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

procedure TFHIRIndexInformation.buildIndexes;
var
  i : TFhirResourceType;
  s : String;
begin
  // the order of these matters when building search forms
  buildIndexesPractitioner;
  buildIndexesPatient;
  buildIndexesOrganization;

  buildIndexesAllergyIntolerance;
  buildIndexesCarePlan;
  buildIndexesConformance;
  buildIndexesDevice;
  buildIndexesDiagnosticReport;
  buildIndexesDiagnosticOrder;
  buildIndexesComposition;
  buildIndexesDocumentReference;
  buildIndexesDocumentManifest;
  buildIndexesGroup;
  buildIndexesImagingStudy;
  buildIndexesImmunization;
  buildIndexesImmunizationRecommendation;
  buildIndexesOperationOutcome;
  buildIndexesList;
  buildIndexesLocation;
  buildIndexesMedicationAdministration;
  buildIndexesMedication;
  buildIndexesMedicationOrder;
  buildIndexesMedicationDispense;
  buildIndexesMedicationStatement;
  buildIndexesMessageHeader;
  buildIndexesObservation;
  buildIndexesOrder;
  buildIndexesOrderResponse;
  buildIndexesMedia;
  buildIndexesCondition;
  buildIndexesProcedure;
  buildIndexesProvenance;
  buildIndexesQuestionnaire;
  buildIndexesSpecimen;
  buildIndexesSubstance;
  buildIndexesValueSet;
  buildIndexesConceptMap;
  buildIndexesEncounter;
  BuildIndexesRelatedPerson;
  BuildIndexesSupplyRequest;
  BuildIndexesSupplyDelivery;
  buildIndexesAuditEvent;
  buildIndexesStructureDefinition;
  buildIndexesFamilyMemberHistory;
  buildIndexesFlag;
  buildIndexesBundle;
  BuildIndexesCoverage;
  BuildIndexesClaimResponse;
  BuildIndexesContract;
  BuildIndexesClaim;
  BuildIndexesBasic;
  buildIndexesQuestionnaireResponse;
  BuildIndexesSlot;
  BuildIndexesAppointment;
  BuildIndexesSchedule;
  BuildIndexesAppointmentResponse;
  BuildIndexesHealthcareService;
  BuildIndexesDataElement;
  BuildIndexesTestScript;
  BuildIndexesNamingSystem;
  BuildIndexesSubscription;
  BuildIndexesDetectedIssue;
  BuildIndexesRiskAssessment;
  BuildIndexesOperationDefinition;
  BuildIndexesReferralRequest;
  BuildIndexesNutritionOrder;
  buildIndexesBodySite;
  buildIndexesClinicalImpression;
  buildIndexesCommunication;
  buildIndexesCommunicationRequest;
  buildIndexesDeviceComponent;
  buildIndexesDeviceMetric;
  buildIndexesDeviceUseRequest;
  buildIndexesDeviceUseStatement;
  buildIndexesEligibilityRequest;
  buildIndexesEligibilityResponse;
  buildIndexesEnrollmentRequest;
  buildIndexesEnrollmentResponse;
  buildIndexesEpisodeOfCare;
  buildIndexesExplanationOfBenefit;
  buildIndexesGoal;
  buildIndexesImagingObjectSelection;
  buildIndexesPaymentNotice;
  buildIndexesPerson;
  buildIndexesProcedureRequest;
  buildIndexesSearchParameter;
  buildIndexesVisionPrescription;
  buildIndexesProcessRequest;
  buildIndexesProcessResponse;
  buildIndexesPaymentReconciliation;
  buildIndexesBinary;
  buildIndexesAccount;
  buildIndexesImplementationGuide;
  {$IFDEF FHIR_DSTU3}
  buildIndexesCodeSystem;
  buildIndexesLinkage;
  BuildIndexesDecisionSupportRule;
  BuildIndexesDecisionSupportServiceModule;
  BuildIndexesExpansionProfile;
  BuildIndexesGuidanceResponse;
  BuildIndexesLibrary;
  BuildIndexesMeasure;
  BuildIndexesModuleDefinition;
  BuildIndexesOrderSet;
  BuildIndexesProtocol;
  BuildIndexesSequence;
  BuildIndexesCompartmentDefinition;
  BuildIndexesMeasureReport;
  BuildIndexesStructureMap;
  BuildIndexesCareTeam;
  BuildIndexesTask;
  BuildIndexesPractitionerRole;
  BuildIndexesImagingExcerpt;
  {$ENDIF}



  s := '';
  for I := TFhirResourceType(1) to High(TFhirResourceType) do
    if not (i in [frtParameters]) and (FIndexes.getByName(i, '_id') = nil) then
      if s = '' then
        s := CODES_TFHIRResourceType[i]
      else
        s := s +', '+CODES_TFHIRResourceType[i];

  if s <> '' then
    begin
    writeln('No registration for '+s);
    raise Exception.Create('No registration for '+s);
    end;
end;

function TFhirIndexInformation.GetTargetsByName(types: TFhirResourceTypeSet; name: String): TFhirResourceTypeSet;
var
  i : integer;
begin
  result := [];
  for i := 0 to FIndexes.Count - 1 Do
    if SameText(FIndexes[i].Name, name) and (FIndexes[i].ResourceType in types) then
      result := result + FIndexes[i].TargetTypes;
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

function TFhirIndexInformation.GetTypeByName(types: TFhirResourceTypeSet; name: String): TFhirSearchParamTypeEnum;
var
  i : integer;
begin
  result := SearchParamTypeNull;
  for i := 0 to FIndexes.Count - 1 Do
    if SameText(FIndexes[i].Name, name) and ((FIndexes[i].ResourceType in types) or (types = [frtNull])) then
      if (result <> SearchParamTypeNull) and (result <> FIndexes[i].FSearchType) And ((FIndexes[i].FSearchType in [SearchParamTypeDate, SearchParamTypeToken]) or (result in [SearchParamTypeDate, SearchParamTypeToken])) then
        raise Exception.create('Chained Parameters cross resource joins that create disparate index handling requirements')
      else
        result := FIndexes[i].FSearchType;
end;

function TFhirIndexInformation.GetComposite(types: TFhirResourceTypeSet; name: String; var otypes: TFhirResourceTypeSet): TFhirComposite;
var
  i : integer;
begin
  oTypes := types;

  i := 0;
  result := nil;
  while (i < FComposites.Count) do
  begin
    if SameText(FComposites.item[i].name, name) and (FComposites.item[i].FResourceType in types) then
      if result = nil then
      begin
        result := FComposites.item[i];
        oTypes := [FComposites.item[i].FResourceType];
      end
      else
        raise Exception.Create('Ambiguous composite reference "'+name+'"');
    inc(i);
  end;
end;


initialization
  TFHIRIndexInformation.Create.free;
end.

  if (resource.codeSystem <> nil) then
  begin
    index(frtValueSet, key, 0, resource.codeSystem.systemElement, CODES_TSearchParamsValueSet[spValueSet_system]);
    indexConcepts(resource.codeSystem.conceptList);
  end;
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

