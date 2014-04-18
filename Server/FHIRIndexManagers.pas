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

}
uses
  SysUtils, Classes,
  AdvObjects, AdvObjectLists, AdvNames, AdvXmlBuilders,
  EncodeSupport, DecimalSupport, HL7v2dateSupport, StringSupport, GuidSupport,
  KDBManager,
  FHIRBase, FhirSupport, FHIRResources, FHIRComponents, FHIRConstants, FHIRAtomFeed, FHIRTypes, FHIRTags, FHIRUtilities, FHIRParser,
  UcumServices;

Const
  INDEX_ENTRY_LENGTH = 128;
  NARRATIVE_INDEX_NAME = '_text';

Type
  TKeyType = (ktResource, ktEntries, ktCompartment);

  TFHIRGetNextKey = function (keytype : TKeyType) : Integer of Object;

  TFhirIndex = class (TAdvObject)
  private
    FResourceType : TFhirResourceType;
    FKey: Integer;
    FName: String;
    FDescription : String;
    FSearchType: TFhirSearchParamType;
    FTargetTypes : TFhirResourceTypeSet;
  public
    function Link : TFhirIndex; Overload;
    function Clone : TFhirIndex; Overload;
    procedure Assign(source : TAdvObject); Override;

    property ResourceType : TFhirResourceType read FResourceType write FResourceType;
    property Name : String read FName write FName;
    Property Description : String read FDescription write FDescription;
    Property Key : Integer read FKey write FKey;
    Property SearchType : TFhirSearchParamType read FSearchType write FSearchType;
    Property TargetTypes : TFhirResourceTypeSet read FTargetTypes write FTargetTypes;
  end;

  TFhirIndexList = class (TAdvObjectList)
  private
    function GetItemN(iIndex: integer): TFhirIndex;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    function Link : TFhirIndexList; Overload;

    function getByName(atype : TFhirResourceType; name : String): TFhirIndex;
    procedure add(aResourceType : TFhirResourceType; name, description : String; aType : TFhirSearchParamType; aTargetTypes : TFhirResourceTypeSet); overload;
    Property Item[iIndex : integer] : TFhirIndex read GetItemN; default;
  end;

  TFhirIndexEntry = class (TAdvObject)
  private
    FIndex : TFhirIndex;
    FValue1: String;
    FValue2: String;
    FRefType: integer;
    FKey: integer;
    FTarget: integer;
    FType: TFhirSearchParamType;
    procedure SetIndex(const Value: TFhirIndex);
  public
    destructor Destroy; override;
    property Key : integer read FKey write FKey;
    property Index : TFhirIndex read FIndex write SetIndex;
    property Value1 : String read FValue1 write FValue1;
    property Value2 : String read FValue2 write FValue2;
    property RefType : integer read FRefType write FRefType;
    Property target : integer read FTarget write FTarget;
    Property type_ : TFhirSearchParamType read FType write FType;
  end;

  TFhirIndexEntryList = class (TAdvObjectList)
  private
    function GetItemN(iIndex: integer): TFhirIndexEntry;
  protected
    function ItemClass : TAdvObjectClass; override;
  public
    procedure add(key : integer; index : TFhirIndex; ref : integer; value1, value2 : String; target : integer; type_ : TFhirSearchParamType);
    Property Item[iIndexEntry : integer] : TFhirIndexEntry read GetItemN; default;
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

  TFhirIndexManager = class (TAdvObject)
  private
    FUcum :  TUcumServices;
    FKeyEvent : TFHIRGetNextKey;
    FSpaces : TFhirIndexSpaces;
    FIndexes : TFhirIndexList;
    FPatientCompartments : TFhirCompartmentEntryList;
    FEntries : TFhirIndexEntryList;
    FMasterKey : Integer;
    FNarrativeIndex : Integer;
    FBases : TStringList;

    procedure ReconcileIndexes;

    function EncodeXhtml(r : TFhirResource) : TBytes;

    procedure buildIndexes;
    procedure buildIndexValues(key : integer; id : String; resource : TFhirResource);

    procedure patientCompartment(key : integer; reference : TFhirResourceReference); overload;
    procedure patientCompartmentNot(key : integer; type_, id : String); overload;
    procedure patientCompartment(key : integer; type_, id : String); overload;

    // primitives
    procedure index(aType : TFhirResourceType; key : integer; value1, value2, name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value, name : String); overload;
    procedure index2(aType : TFhirResourceType; key : integer; value, name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirString; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirUri; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirEnum; name : String); overload;
//    procedure index(aType : TFhirResourceType; key : integer; value : TFhirEnumList; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirInteger; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirBoolean; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : Boolean; name : String); overload;

    // intervals of time
    procedure index(aType : TFhirResourceType; key : integer; min, max : TDateTime; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirInstant; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirDateTime; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirDate; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirPeriod; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirSchedule; name : String); overload;

    // complexes
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirRatio; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirQuantity; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirSampledData; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirCoding; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirCodeableConcept; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirIdentifier; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; resource : TFhirResource; value : TFhirResourceReference; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirHumanName; name, phoneticName : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirAddress; name : String); overload;
    procedure index(aType : TFhirResourceType; key : integer; value : TFhirContact; name : String); overload;

    { resource functionality }
    procedure buildIndexValuesAdverseReaction(key : integer; id : string; resource : TFhirAdverseReaction);
    procedure buildIndexValuesAlert(key : integer; id : string; resource : TFhirAlert);
    procedure buildIndexValuesAllergyIntolerance(key : integer; id : string; resource : TFhirAllergyIntolerance);
    procedure buildIndexValuesBinary(key : integer; id : string; resource : TFhirBinary);
    procedure BuildIndexValuesCarePlan(key : integer; id : string; resource : TFhirCarePlan);
    procedure BuildIndexValuesCondition(key : integer; id : string; resource : TFhirCondition);
    procedure BuildIndexValuesConformance(key : integer; id : string; resource : TFhirConformance);
    procedure BuildIndexValuesDevice(key : integer; id : string; resource : TFhirDevice);
    procedure BuildIndexValuesDeviceObservationReport(key : integer; id : string; resource : TFhirDeviceObservationReport);
    procedure BuildIndexValuesDiagnosticOrder(key : integer; id : string; resource : TFhirDiagnosticOrder);
    procedure BuildIndexValuesDiagnosticReport(key : integer; id : string; resource : TFhirDiagnosticReport);
    procedure BuildIndexValuesComposition(key : integer; id : string; resource : TFhirComposition);
    procedure BuildIndexValuesDocumentReference(key : integer; id : string; resource : TFhirDocumentReference);
    procedure BuildIndexValuesDocumentManifest(key : integer; id : string; resource : TFhirDocumentManifest);
    procedure BuildIndexValuesEncounter(key : integer; id : string; resource : TFhirEncounter);
    procedure buildIndexValuesFamilyHistory(key : integer; id : string; resource : TFhirFamilyHistory);
    procedure BuildIndexValuesGroup(key : integer; id : string; resource : TFhirGroup);
    procedure BuildIndexValuesImagingStudy(key : integer; id : string; resource : TFhirImagingStudy);
    procedure BuildIndexValuesImmunization(key : integer; id : string; resource : TFhirImmunization);
    procedure buildIndexValuesImmunizationRecommendation(key : integer; id : string; resource : TFhirImmunizationRecommendation);
    procedure BuildIndexValuesList(key : integer; id : string; resource : TFhirList);
    procedure BuildIndexValuesLocation(key : integer; id : string; resource : TFhirLocation);
    procedure BuildIndexValuesMedia(key : integer; id : string; resource : TFhirMedia);
    procedure BuildIndexValuesMedication(key : integer; id : string; resource : TFhirMedication);
    procedure BuildIndexValuesMedicationAdministration(key : integer; id : string; resource : TFhirMedicationAdministration);
    procedure BuildIndexValuesMedicationDispense(key : integer; id : string; resource : TFhirMedicationDispense);
    procedure BuildIndexValuesMedicationPrescription(key : integer; id : string; resource : TFhirMedicationPrescription);
    procedure BuildIndexValuesMedicationStatement(key : integer; id : string; resource : TFhirMedicationStatement);
    procedure BuildIndexValuesMessageHeader(key : integer; id : string; resource : TFhirMessageHeader);
    procedure BuildIndexValuesObservation(key : integer; id : string; resource : TFhirObservation);
    procedure BuildIndexValuesOperationOutcome(key : integer; id : string; resource : TFhirOperationOutcome);
    procedure BuildIndexValuesOrder(key : integer; id : string; resource : TFhirOrder);
    procedure BuildIndexValuesOrderResponse(key : integer; id : string; resource : TFhirOrderResponse);
    procedure BuildIndexValuesOrganization(key : integer; id : string; resource : TFhirOrganization);
    procedure BuildIndexValuesPatient(key : integer; id : string; resource : TFhirPatient);
    procedure BuildIndexValuesPractitioner(key : integer; id : string; resource : TFhirPractitioner);
    procedure buildIndexValuesProcedure(key : integer; id : string; resource : TFhirProcedure);
    procedure BuildIndexValuesProfile(key : integer; id : string; resource : TFhirProfile);
    procedure BuildIndexValuesProvenance(key : integer; id : string; resource : TFhirProvenance);
    procedure BuildIndexValuesQuery(key : integer; id : string; resource : TFhirQuery);
    procedure BuildIndexValuesQuestionnaire(key : integer; id : string; resource : TFhirQuestionnaire);
    procedure BuildIndexValuesSecurityEvent(key : integer; id : string; resource : TFhirSecurityEvent);
    procedure buildIndexValuesSpecimen(key : integer; id : string; resource : TFhirSpecimen);
    procedure buildIndexValuesSubstance(key : integer; id : string; resource : TFhirSubstance);
    procedure BuildIndexValuesValueSet(key : integer; id : string; resource : TFhirValueSet);
    procedure BuildIndexValuesConceptMap(key : integer; id : string; resource : TFhirConceptMap);
    procedure BuildIndexValuesRelatedPerson(key : integer; id : string; resource : TFhirRelatedPerson);
    procedure BuildIndexValuesSupply(key : integer; id : string; resource : TFhirSupply);
    procedure BuildIndexValuesOther(key : integer; id : string; resource : TFhirOther);

    procedure buildIndexesAdverseReaction;
    procedure buildIndexesAlert;
    procedure buildIndexesAllergyIntolerance;
    procedure buildIndexesCarePlan;
    procedure buildIndexesConformance;
    procedure buildIndexesDevice;
    procedure buildIndexesDeviceObservationReport;
    procedure buildIndexesDiagnosticReport;
    procedure buildIndexesDiagnosticOrder;
    procedure buildIndexesComposition;
    procedure buildIndexesDocumentReference;
    procedure buildIndexesDocumentManifest;
    procedure buildIndexesFamilyHistory;
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
    procedure buildIndexesMedicationPrescription;
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
    procedure buildIndexesProfile;
    procedure buildIndexesProvenance;
    procedure buildIndexesPractitioner;
    procedure buildIndexesQuery;
    procedure buildIndexesQuestionnaire;
    procedure buildIndexesSecurityEvent;
    procedure buildIndexesValueSet;
    procedure BuildIndexesConceptMap;
    procedure buildIndexesSpecimen;
    procedure buildIndexesSubstance;
    procedure buildIndexesBinary();
    procedure BuildIndexesRelatedPerson;
    procedure BuildIndexesSupply;
    procedure BuildIndexesOther;

    procedure processCompartmentTags(key : integer; id: String; tags : TFHIRAtomCategoryList);
    procedure processUnCompartmentTags(key : integer; id: String; tags : TFHIRAtomCategoryList);
    procedure SetUcum(const Value: TUcumServices);

  public
    constructor Create(aSpaces : TFhirIndexSpaces);
    destructor Destroy; override;
    property Indexes : TFhirIndexList read FIndexes;
    property Ucum : TUcumServices read FUcum write SetUcum;
    property Bases : TStringList read FBases write FBases;
    function execute(key : integer; id: String; resource : TFhirResource; tags : TFHIRAtomCategoryList) : String;
    Function GetKeyByName(types : TFhirResourceTypeSet; name : String) : integer;
    Function GetTypeByName(types : TFhirResourceTypeSet; name : String) : TFhirSearchParamType;
    Function GetTargetsByName(types : TFhirResourceTypeSet; name : String) : TFhirResourceTypeSet;
    property KeyEvent : TFHIRGetNextKey read FKeyEvent write FKeyEvent;
    Property NarrativeIndex : integer read FNarrativeIndex;
  end;

implementation

Function EncodeNYSIISValue(value : TFhirString) : String; overload;
begin
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

procedure TFhirIndexList.add(aResourceType : TFhirResourceType; name, description : String; aType : TFhirSearchParamType; aTargetTypes : TFhirResourceTypeSet);
var
  ndx : TFhirIndex;
begin
  ndx := TFhirIndex.Create;
  try
    ndx.ResourceType := aResourceType;
    ndx.name := name;
    ndx.SearchType := aType;
    ndx.TargetTypes := aTargetTypes;
    ndx.description := description;
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

{ TFhirIndexEntry }

destructor TFhirIndexEntry.destroy;
begin
  FIndex.Free;
  inherited;
end;

procedure TFhirIndexEntry.SetIndex(const Value: TFhirIndex);
begin
  FIndex.Free;
  FIndex := Value;
end;

{ TFhirIndexEntryList }

procedure TFhirIndexEntryList.add(key : integer; index: TFhirIndex; ref: integer; value1, value2: String; target : Integer; type_ : TFhirSearchParamType);
var
  entry : TFhirIndexEntry;
begin
  case type_ of
    SearchParamTypeNumber : ; // nothing
    SearchParamTypeQuantity : ; // nothing
    SearchParamTypeString :
      begin
        value1 := removeCaseAndAccents(value1);
        value2 := removeCaseAndAccents(value2);
      end;
    SearchParamTypeDate : ; // nothing
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
    entry.key := key;
    entry.Index := index.link;
    entry.Value1 := lowercase(value1);
    entry.Value2 := lowercase(value2);
    entry.RefType := ref;
    entry.type_ := type_;
    entry.target := target;
    Inherited Add(entry.Link);
  finally
    entry.free;
  end;
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

constructor TFhirIndexManager.Create(aSpaces : TFhirIndexSpaces);
begin
  inherited Create;
  FPatientCompartments := TFhirCompartmentEntryList.create;
  FSpaces := TFhirIndexSpaces(aSpaces.Link);
  FIndexes := TFhirIndexList.create;
  buildIndexes;

  FEntries := TFhirIndexEntryList.Create;
  if FSpaces <> nil then
    ReconcileIndexes;
end;

destructor TFhirIndexManager.Destroy;
begin
  FUcum.free;
  FPatientCompartments.Free;
  FSpaces.Free;
  FEntries.Free;
  FIndexes.Free;
  inherited;
end;

procedure TFhirIndexManager.buildIndexes;
begin
  // the order of these matters when building search forms
  buildIndexesPractitioner;
  buildIndexesPatient;
  buildIndexesOrganization;

  buildIndexesAdverseReaction;
  buildIndexesAlert;
  buildIndexesAllergyIntolerance;
  buildIndexesCarePlan;
  buildIndexesConformance;
  buildIndexesDevice;
  buildIndexesDeviceObservationReport;
  buildIndexesDiagnosticReport;
  buildIndexesDiagnosticOrder;
  buildIndexesComposition;
  buildIndexesDocumentReference;
  buildIndexesDocumentManifest;
  buildIndexesFamilyHistory;
  buildIndexesGroup;
  buildIndexesImagingStudy;
  buildIndexesImmunization;
  buildIndexesImmunizationRecommendation;
  buildIndexesOperationOutcome;
  buildIndexesList;
  buildIndexesLocation;
  buildIndexesMedicationAdministration;
  buildIndexesMedication;
  buildIndexesMedicationPrescription;
  buildIndexesMedicationDispense;
  buildIndexesMedicationStatement;
  buildIndexesMessageHeader;
  buildIndexesObservation;
  buildIndexesOrder;
  buildIndexesOrderResponse;
  buildIndexesOrganization;
  buildIndexesPatient;
  buildIndexesMedia;
  buildIndexesPractitioner;
  buildIndexesCondition;
  buildIndexesProcedure;
  buildIndexesProfile;
  buildIndexesProvenance;
  buildIndexesQuestionnaire;
  buildIndexesQuery;
  buildIndexesSecurityEvent;
  buildIndexesSpecimen;
  buildIndexesSubstance;
  buildIndexesValueSet;
  buildIndexesConceptMap;
  buildIndexesEncounter;
  BuildIndexesRelatedPerson;
  BuildIndexesSupply;
  BuildIndexesOther;

  buildIndexesBinary;
end;

procedure TFhirIndexManager.buildIndexValues(key : integer; id : string; resource: TFhirResource);
begin
  case resource.ResourceType of
    frtBinary : buildIndexValuesBinary(key, id, TFhirBinary(resource));
    frtAdverseReaction : buildIndexValuesAdverseReaction(key, id, TFhirAdverseReaction(resource));
    frtAlert : buildIndexValuesAlert(key, id, TFhirAlert(resource));
    frtAllergyIntolerance : buildIndexValuesAllergyIntolerance(key, id, TFhirAllergyIntolerance(resource));
    frtCarePlan : buildIndexValuesCarePlan(key, id, TFhirCarePlan(resource));
    frtConformance : buildIndexValuesConformance(key, id, TFhirConformance(resource));
    frtDevice : buildIndexValuesDevice(key, id, TFhirDevice(resource));
    frtDeviceObservationReport : buildIndexValuesDeviceObservationReport(key, id, TFhirDeviceObservationReport(resource));
    frtDiagnosticReport : buildIndexValuesDiagnosticReport(key, id, TFhirDiagnosticReport(resource));
    frtDiagnosticOrder : buildIndexValuesDiagnosticOrder(key, id, TFhirDiagnosticOrder(resource));
    frtComposition : buildIndexValuesComposition(key, id, TFhirComposition(resource));
    frtDocumentReference : buildIndexValuesDocumentReference(key, id, TFhirDocumentReference(resource));
    frtDocumentManifest : buildIndexValuesDocumentManifest(key, id, TFhirDocumentManifest(resource));
    frtFamilyHistory : buildIndexValuesFamilyHistory(key, id, TFhirFamilyHistory(resource));
    frtGroup : buildIndexValuesGroup(key, id, TFhirGroup(resource));
    frtImagingStudy : buildIndexValuesImagingStudy(key, id, TFhirImagingStudy(resource));
    frtImmunization : buildIndexValuesImmunization(key, id, TFhirImmunization(resource));
    frtImmunizationRecommendation : buildIndexValuesImmunizationRecommendation(key, id, TFhirImmunizationRecommendation(resource));
    frtOperationOutcome : buildIndexValuesOperationOutcome(key, id, TFhirOperationOutcome(resource));
    frtList : buildIndexValuesList(key, id, TFhirList(resource));
    frtLocation : buildIndexValuesLocation(key, id, TFhirLocation(resource));
    frtMedication : buildIndexValuesMedication(key, id, TFhirMedication(resource));
    frtMedicationAdministration : buildIndexValuesMedicationAdministration(key, id, TFhirMedicationAdministration(resource));
    frtMedicationPrescription : buildIndexValuesMedicationPrescription(key, id, TFhirMedicationPrescription(resource));
    frtMedicationDispense : buildIndexValuesMedicationDispense(key, id, TFhirMedicationDispense(resource));
    frtMedicationStatement : buildIndexValuesMedicationStatement(key, id, TFhirMedicationStatement(resource));
    frtMessageHeader : buildIndexValuesMessageHeader(key, id, TFhirMessageHeader(resource));
    frtObservation : buildIndexValuesObservation(key, id, TFhirObservation(resource));
    frtOrder : buildIndexValuesOrder(key, id, TFhirOrder(resource));
    frtOrderResponse : buildIndexValuesOrderResponse(key, id, TFhirOrderResponse(resource));
    frtOrganization : buildIndexValuesOrganization(key, id, TFhirOrganization(resource));
    frtPatient : buildIndexValuesPatient(key, id, TFhirPatient(resource));
    frtMedia : buildIndexValuesMedia(key, id, TFhirMedia(resource));
    frtPractitioner : buildIndexValuesPractitioner(key, id, TFhirPractitioner(resource));
    frtCondition : buildIndexValuesCondition(key, id, TFhirCondition(resource));
    frtProcedure : buildIndexValuesProcedure(key, id, TFhirProcedure(resource));
    frtProfile : buildIndexValuesProfile(key, id, TFhirProfile(resource));
    frtProvenance : buildIndexValuesProvenance(key, id, TFhirProvenance(resource));
    frtQuery : buildIndexValuesQuery(key, id, TFhirQuery(resource));
    frtQuestionnaire : buildIndexValuesQuestionnaire(key, id, TFhirQuestionnaire(resource));
    frtSecurityEvent : buildIndexValuesSecurityEvent(key, id, TFhirSecurityEvent(resource));
    frtSpecimen : buildIndexValuesSpecimen(key, id, TFhirSpecimen(resource));
    frtSubstance : buildIndexValuesSubstance(key, id, TFhirSubstance(resource));
    frtValueSet : buildIndexValuesValueSet(key, id, TFhirValueSet(resource));
    frtConceptMap : buildIndexValuesConceptMap(key, id, TFhirConceptMap(resource));
    frtEncounter : buildIndexValuesEncounter(key, id, TFhirEncounter(resource));
    frtRelatedPerson : buildIndexValuesRelatedPerson(key, id, TFhirRelatedPerson(resource));
    frtSupply : buildIndexValuesSupply(key, id, TFhirSupply(resource));
    frtOther : buildIndexValuesOther(key, id, TFhirOther(resource));
  else
    raise Exception.create('resource type indexing not implemented yet for '+CODES_TFhirResourceType[resource.ResourceType]);
  end;
end;


function TFhirIndexManager.EncodeXhtml(r: TFhirResource): TBytes;
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
        x.AddChild('head').AddChild('title').AddText(CODES_TFhirResourceType[r.ResourceType]);
        body := x.AddChild('body');
        if (r.language = nil) then
          body.SetAttribute('lang', 'en')
        else
          body.SetAttribute('lang', r.language.value);
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


procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirString; name: String);
begin
  if (value <> nil) then
    index(aType, key, value.value, name);
end;


procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirEnum; name: String);
begin
  if (value <> nil) then
    index(aType, key, value.value, name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirUri; name: String);
begin
  if (value <> nil) then
    index(aType, key, value.value, name);
end;


procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirInstant; name: String);
begin
  if (value <> nil) and (value.value <> nil) then
    index(aType, key, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.ReconcileIndexes;
var
  i : integer;
begin
  FSpaces.FDB.SQL := 'select * from Indexes';
  FSpaces.FDb.prepare;
  FSpaces.FDb.execute;
  while FSpaces.FDb.FetchNext do
  begin
    for i := 0 to FIndexes.Count - 1 Do
      if SameText(FIndexes[i].Name, FSpaces.FDb.ColStringByName['Name']) then
        FIndexes[i].key := FSpaces.FDb.ColIntegerByName['IndexKey'];
    if FSpaces.FDb.ColStringByName['Name'] = NARRATIVE_INDEX_NAME then
      FNarrativeIndex := FSpaces.FDb.ColIntegerByName['IndexKey'];
  end;
  FSpaces.FDb.terminate;
end;

procedure TFhirIndexManager.SetUcum(const Value: TUcumServices);
begin
  FUcum.Free;
  FUcum := Value;
end;

function TFhirIndexManager.execute(key : integer; id : String; resource : TFhirResource; tags : TFHIRAtomCategoryList) : String;
var
  i : integer;
  entry : TFhirIndexEntry;
begin
  if (resource.ResourceType in [frtBinary]) then
    exit;

  FEntries.clear;
  index(resource.ResourceType, key, id, '_id');
//  if (resource.language <> nil) then
    index(resource.ResourceType, key, resource.language, 'language');
  FMasterKey := key;
  FSpaces.FDB.ExecSQL('delete from Compartments where ResourceKey in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('delete from IndexEntries where ResourceKey in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('delete from IndexEntries where Target in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('delete from SearchEntries where ResourceKey in (select ResourceKey from Ids where MasterResourceKey = '+inttostr(key)+')');
  FSpaces.FDB.ExecSQL('delete from Ids where MasterResourceKey = '+inttostr(key));
  FPatientCompartments.Clear;

  processCompartmentTags(key, id, tags);
  buildIndexValues(key, id, resource);
  processUnCompartmentTags(key, id, tags);

  FSpaces.FDB.SQL := 'insert into indexEntries (EntryKey, IndexKey, ResourceKey, Extension, Xhtml) values (:k, :i, :r, ''html'', :xb)';
  FSpaces.FDB.prepare;
  FSpaces.FDB.BindInteger('k', FKeyEvent(ktEntries));
  FSpaces.FDB.BindInteger('i', FNarrativeIndex);
  FSpaces.FDB.BindInteger('r', key);
  FSpaces.FDB.BindBlobFromBytes('xb', EncodeXhtml(resource));
  FSpaces.FDB.execute;
  FSpaces.FDB.terminate;


  FSpaces.FDB.SQL := 'insert into indexEntries (EntryKey, IndexKey, ResourceKey, MasterResourceKey, SpaceKey, Value, Value2, target) values (:k, :i, :r, :m, :s, :v, :v2, :t)';
  FSpaces.FDB.prepare;
  for i := 0 to FEntries.Count - 1 Do
  begin
    FSpaces.FDB.BindInteger('k', FKeyEvent(ktEntries));
    entry := FEntries[i];
    if entry.Index.Key = 0 then
      raise Exception.create('unknown index '+entry.index.Name);
    FSpaces.FDB.BindInteger('i', entry.Index.Key);
    FSpaces.FDB.BindInteger('r', entry.key);
    if entry.key <> key then
      FSpaces.FDB.BindInteger('m', key)
    else
      FSpaces.FDB.BindNull('m');
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
    FSpaces.FDB.execute;
  end;
  FSpaces.FDB.terminate;

  result := '';
  if FPatientCompartments.Count > 0 then
  begin
    FSpaces.FDB.SQL := 'insert into Compartments (ResourceCompartmentKey, ResourceKey, MasterResourceKey, CompartmentType, CompartmentKey, Id) values (:pk, :r, :mr, :ct, :ck, :id)';
    FSpaces.FDB.prepare;
    for i := 0 to FPatientCompartments.Count - 1 Do
    begin
      if i > 0 then
        result := result + ', ';
      result := result + ''''+FPatientCompartments[i].id+'''';

      FSpaces.FDB.BindInteger('pk', FKeyEvent(ktCompartment));
      FSpaces.FDB.BindInteger('r', FPatientCompartments[i].key);
      FSpaces.FDB.BindInteger('mr', key);
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
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirCoding; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.code = nil) or (value.code.value = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if ndx.SearchType <> SearchParamTypeToken then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Coding');
  ref := 0;
  if (value.system <> nil) and (value.system.value <> '') then
    ref := FSpaces.ResolveSpace(value.system.value);
  if (length(value.code.value) > INDEX_ENTRY_LENGTH) then
    raise exception.create('code too long for indexing: '+value.code.value);
  if value.display <> nil then
    FEntries.add(key, ndx, ref, value.code.value, value.display.value, 0, ndx.SearchType)
  else
    FEntries.add(key, ndx, ref, value.code.value, '', 0, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirCodeableConcept; name: String);
var
  i : integer;
begin
  if value <> nil then
  begin
    for i := 0 to value.codingList.count - 1 do
      index(aType, key, value.codingList[i], name);
    if value.text <> nil then
      index2(aType, key, value.text.value, name);
  End;
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value, name: String);
var
  ndx : TFhirIndex;                    
begin
  if (value = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+CODES_TFhirResourceType[aType]);
  if not (ndx.SearchType in [SearchParamTypeString, SearchParamTypeToken, SearchParamTypeDate, SearchParamTypeReference]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing string');
  if (length(value) > INDEX_ENTRY_LENGTH) then
    if ndx.SearchType = SearchParamTypeString then
      value := copy(value, 1, 64)
    else
      raise exception.create('string too long for indexing: '+value+ ' ('+inttostr(length(value))+' chars)');
  FEntries.add(key, ndx, 0, value, '', 0, ndx.SearchType);
end;

procedure TFhirIndexManager.index2(aType : TFhirResourceType; key : integer; value, name: String);
var
  ndx : TFhirIndex;
begin
  if (value = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+CODES_TFhirResourceType[aType]);
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeReference]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing string');
  if ndx.SearchType = SearchParamTypeString then
    value := copy(value, 1, 64);
  FEntries.add(key, ndx, 0, '', value, 0, SearchParamTypeString);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value1, value2, name: String);
var
  ndx : TFhirIndex;
begin
  if (value1 = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name+' on type '+CODES_TFhirResourceType[aType]);
  if not (ndx.SearchType in [SearchParamTypeString, SearchParamTypeToken, SearchParamTypeDate]) then //todo: fix up text
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing string');
  if (length(value1) > INDEX_ENTRY_LENGTH) then
    if ndx.SearchType = SearchParamTypeString then
      value1 := copy(value1, 1, 64)
    else
      raise exception.create('string too long for indexing: '+value1+ ' ('+inttostr(length(value1))+' chars)');
  if (length(value2) > INDEX_ENTRY_LENGTH) then
    if ndx.SearchType = SearchParamTypeString then
      value2 := copy(value2, 1, 64)
    else
      raise exception.create('string too long for indexing: '+value2+ ' ('+inttostr(length(value2))+' chars)');
  FEntries.add(key, ndx, 0, value1, value2, 0, ndx.SearchType);
end;

Function ComparatorPrefix(v : String; c : TFhirQuantityComparator) : String;
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

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value : TFhirQuantity; name : String);
var
  ndx : TFhirIndex;
  v : String;
  ref : integer;
  specified, canonical : TUcumPair;
begin
  if value = nil then
    exit;

  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join: "'+name+'"');
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeNumber, SearchParamTypeQuantity]) then
    raise Exception.create('Unsuitable index "'+name+'" '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing quantity');

  v := ComparatorPrefix(value.valueST, value.comparatorST);

  if (length(v) > INDEX_ENTRY_LENGTH) then
      raise exception.create('quantity.value too long for indexing: "'+v+ '" ('+inttostr(length(v))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
  ref := FSpaces.ResolveSpace(value.unitsST);
  FEntries.add(key, ndx, ref, v, '', 0, ndx.SearchType);
  // ok, if there's a ucum code:
  if (value.codeST <> '') and (value.systemST = 'http://loinc.org') then
  begin
    specified := TUcumPair.create;
    try
      specified.Value := TSmartDecimal.create(value.valueST);
      specified.UnitCode := value.codeST;
      canonical := Ucum.getCanonicalForm(specified);
      try
        v := ComparatorPrefix(canonical.Value.AsString, value.comparatorST);
        if (length(v) > INDEX_ENTRY_LENGTH) then
          raise exception.create('quantity.value too long for indexing: "'+v+ '" ('+inttostr(length(v))+' chars, limit '+inttostr(INDEX_ENTRY_LENGTH)+')');
        ref := FSpaces.ResolveSpace('urn:ucum-canonical:'+canonical.UnitCode);
        FEntries.add(key, ndx, ref, v, '', 0, ndx.SearchType);
      finally
        canonical.free;
      end;
    finally
      specified.free;
    end;
  end;
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value : TFhirPeriod; name : String);
begin
  if (value <> nil) then
    index(aType, key, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value : TFhirSchedule; name : String);
begin
  if (value <> nil) then
    index(aType, key, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirDateTime; name: String);
begin
  if (value <> nil) and (value.value <> nil) then
    index(aType, key, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; min, max : TDateTime; name: String);
var
  ndx : TFhirIndex;
begin
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType = SearchParamTypeDate) then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing date');
  FEntries.add(key, ndx, 0, HL7DateToString(min, 'yyyymmddhhnnss', false), HL7DateToString(max, 'yyyymmddhhnnss', false), 0, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirIdentifier; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = nil) or (value.value.value = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeToken]) then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Identifier');
  ref := 0;
  if (value.system <> nil) and (value.system.value <> '') then
    ref := FSpaces.ResolveSpace(value.system.value);
  if (length(value.value.value) > INDEX_ENTRY_LENGTH) then
    raise exception.create('id too long for indexing: '+value.value.value);
  FEntries.add(key, ndx, ref, value.value.value, '', 0, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirAddress; name: String);
var
  i : integer;
begin
  if (value = nil) then
    exit;
  for i := 0 to value.lineList.count - 1 do
    index(aType, key, value.lineList[i].value, name);
  index(aType, key, value.city, name);
  index(aType, key, value.state, name);
  index(aType, key, value.country, name);
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirContact; name: String);
var
  ndx : TFhirIndex;
  ref : integer;
begin
  if (value = nil) or (value.value = nil) or (value.value.value = '') then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeToken, SearchParamTypeString]) then
    raise Exception.create('Unsuitable index '+name+':'+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Contact on '+CODES_TFhirResourceType[aType]);
  ref := 0;
  if (value.system <> nil) and (value.system.value <> '') then
    ref := FSpaces.ResolveSpace(value.system.value);
  if (length(value.value.value) > INDEX_ENTRY_LENGTH) then
    raise exception.create('contact value too long for indexing: '+value.value.value);
  FEntries.add(key, ndx, ref, value.value.value, '', 0, ndx.SearchType);
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key: integer; value: TFhirSampledData; name: String);
begin
 // todo
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key: integer; value: TFhirRatio; name: String);
begin
  // don't have a clue what to do here
end;

procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirHumanName; name, phoneticName: String);
var
  i : integer;
begin
  if (value = nil) then
    exit;
  for i := 0 to value.familyList.count - 1 do
    index(aType, key, value.familyList[i].value, name);
  for i := 0 to value.givenList.count - 1 do
    index(aType, key, value.givenList[i].value, name);
  for i := 0 to value.prefixList.count - 1 do
    index(aType, key, value.prefixList[i].value, name);
  for i := 0 to value.suffixList.count - 1 do
    index(aType, key, value.suffixList[i].value, name);
  if phoneticName <> '' then
  begin
    for i := 0 to value.familyList.count - 1 do
      index(aType, key, EncodeNYSIIS(value.familyList[i].value), phoneticName);
    for i := 0 to value.givenList.count - 1 do
      index(aType, key, EncodeNYSIIS(value.givenList[i].value), phoneticName);
    for i := 0 to value.prefixList.count - 1 do
      index(aType, key, EncodeNYSIIS(value.prefixList[i].value), phoneticName);
    for i := 0 to value.suffixList.count - 1 do
      index(aType, key, EncodeNYSIIS(value.suffixList[i].value), phoneticName);
  end;
end;

{
procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; value: TFhirDecimal; name: String);
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
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing decimal');
  FEntries.add(key, ndx, 0, value.value, '', 0, ndx.SearchType);
end;
}

// todo: this doesn't yet handle version references
function isLocalTypeReference(url : String; var type_, id : String) : boolean;
var
  i : TFhirResourceType;
begin
  result := false;
  for i := Low(CODES_TFhirResourceType) to High(CODES_TFhirResourceType) do
    if url.StartsWith(CODES_TFhirResourceType[i]+'/') and IsId(url.Substring(url.IndexOf('/')+1)) then
      result := true;
  if result then
    StringSplit(url, '/', type_, id);
end;


procedure TFhirIndexManager.index(aType : TFhirResourceType; key : integer; resource : TFhirResource; value: TFhirResourceReference; name: String);
var
  ndx : TFhirIndex;
  ref, i : integer;
  target : integer;
  type_, id : String;
  contained : TFhirResource;
  url : String;
begin
  if (value = nil) then
    exit;
  if (value.referenceST = '') and (value.displayST <> '') then
  begin
    index(aType, key, value.displayST, name);
    exit;
  end;
  if (value.referenceST = '') then
    exit;

  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes = []) then
    raise Exception.create('Attempt to index a resource join in an index that is a not a join (has no target types)');
  if ndx.SearchType <> SearchParamTypeReference then
    raise Exception.create('Unsuitable index '+name+' '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing Contact');

  if (length(value.referenceST) > INDEX_ENTRY_LENGTH) then
    raise exception.create('resource url too long for indexing: '+value.referenceST);

 {
  ! if the value has a value, then we need to index the value, even though we don't actually have it as a resource
  ! what we do is construct it with a fictional GUID id and index that
  }

  target := 0;
  ref := 0;

  if StringStartsWith(value.referenceST, '#') then
  begin
    contained := FindContainedResource(resource, value);
    ref := FSpaces.ResolveSpace(CODES_TFhirResourceType[contained.ResourceType]);
    if (contained = nil) then
      raise exception.create('Unable to find internal reference to contained resource: "'+value.referenceST+'"');
    id := FHIRGuidToString(CreateGuid);
    target := FKeyEvent(ktResource); //FSpaces.FDB.CountSQL('select Max(ResourceKey) from Ids') + 1;
    FSpaces.FDB.SQL := 'insert into Ids (ResourceKey, ResourceTypeKey, Id, MostRecent, MasterResourceKey) values (:k, :r, :i, null, '+inttostr(FMasterKey)+')';
    FSpaces.FDB.Prepare;
    FSpaces.FDB.BindInteger('k', target);
    FSpaces.FDB.BindInteger('r', ref);
    FSpaces.FDB.BindString('i', id);
    FSpaces.FDB.Execute;
    FSpaces.FDB.Terminate;
    buildIndexValues(target, '', contained);
  end
  else
  begin
    url := value.referenceST;
    for i := 0 to FBases.Count -1 do
    begin
      if StringStartsWith(url, FBases[i]+'/') then
        url := copy(Url, length(FBases[i])+2, $FFFF);
    end;
    if isLocalTypeReference(url, type_, id) then
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
    end;
  end;

  FEntries.add(key, ndx, ref, id, '', target, ndx.SearchType);
end;

function TFhirIndexManager.GetKeyByName(types: TFhirResourceTypeSet; name: String): integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to FIndexes.Count - 1 Do
    if SameText(FIndexes[i].Name, name) then
    begin
      result := FIndexes[i].Key;
      exit;
    end;
end;

function TFhirIndexManager.GetTargetsByName(types: TFhirResourceTypeSet; name: String): TFhirResourceTypeSet;
var
  i : integer;
begin
  result := [];
  for i := 0 to FIndexes.Count - 1 Do
    if SameText(FIndexes[i].Name, name) and (FIndexes[i].ResourceType in types) then
      result := result + FIndexes[i].TargetTypes;
end;

function TFhirIndexManager.GetTypeByName(types: TFhirResourceTypeSet; name: String): TFhirSearchParamType;
var
  i : integer;
begin
  result := SearchParamTypeNull;
  for i := 0 to FIndexes.Count - 1 Do
    if SameText(FIndexes[i].Name, name) and (FIndexes[i].ResourceType in types) then
      if (result <> SearchParamTypeNull) and (result <> FIndexes[i].FSearchType) And ((FIndexes[i].FSearchType in [SearchParamTypeDate, SearchParamTypeToken]) or (result in [SearchParamTypeDate, SearchParamTypeToken])) then
        raise Exception.create('Chained Parameters cross resource joins that create disparate index handling requirements')
      else
        result := FIndexes[i].FSearchType;
end;

Const
  CHECK_TSearchParamsEncounter : Array[TSearchParamsEncounter] of TSearchParamsEncounter = ( spEncounter__id, spEncounter__Language, spEncounter_Date, spEncounter_Identifier, spEncounter_Indication, spEncounter_Length, spEncounter_Location, spEncounter_Location_period, spEncounter_Status, spEncounter_Subject);


procedure TFhirIndexManager.buildIndexesEncounter;
var
  a : TSearchParamsEncounter;
begin
  for a := low(TSearchParamsEncounter) to high(TSearchParamsEncounter) do
  begin
    assert(CHECK_TSearchParamsEncounter[a] = a);
    indexes.add(frtEncounter, CODES_TSearchParamsEncounter[a], DESC_TSearchParamsEncounter[a], TYPES_TSearchParamsEncounter[a], TARGETS_TSearchParamsEncounter[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesEncounter(key: integer; id : String; resource: TFhirEncounter);
var
  i : integer;
begin
  index(frtEncounter, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtEncounter, key, resource.status, 'status');
  index(frtEncounter, key, resource.period, 'date');
  index(frtEncounter, key, resource.length, 'length');
  index(frtEncounter, key, resource, resource.indication, 'indication');
  for i := 0 to resource.identifierList.count - 1 do
    index(frtEncounter, key, resource.identifierList[i], 'identifier');
  for i := 0 to resource.locationList.count - 1 do
  begin
    index(frtEncounter, key, resource, resource.locationList[i].location, 'location');
    index(frtEncounter, key, resource.locationList[i].period, 'location-period');
  end;
end;

Const
  CHECK_TSearchParamsLocation : Array[TSearchParamsLocation] of TSearchParamsLocation = ( spLocation__id, spLocation__Language, spLocation_Address, spLocation_Identifier, spLocation_Name, spLocation_Near, spLocation_Near_distance, spLocation_Partof, spLocation_Status, spLocation_Type);


procedure TFhirIndexManager.buildIndexesLocation;
var
  a : TSearchParamsLocation;
begin
  for a := low(TSearchParamsLocation) to high(TSearchParamsLocation) do
  begin
    assert(CHECK_TSearchParamsLocation[a] = a);
    indexes.add(frtLocation, CODES_TSearchParamsLocation[a], DESC_TSearchParamsLocation[a], TYPES_TSearchParamsLocation[a], TARGETS_TSearchParamsLocation[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesLocation(key: integer; id : String; resource: TFhirLocation);
begin
  index(frtLocation, key, resource.address, 'address');
  index(frtLocation, key, resource.Name, 'name');
  index(frtLocation, key, resource.status, 'status');
  index(frtLocation, key, resource.type_, 'type');
  index(frtLocation, key, resource.identifier, 'identifier');
  index(frtLocation, key, resource, resource.partOf, 'partof');
  if resource.position <> nil then
  begin
    if (resource.position.longitude <> nil) and (resource.position.latitude <> nil) then
      index(frtLocation, key, resource.position.longitudeST, resource.position.latitudeST, 'near');
  end
//    spLocation_Near_distance, {@enum.value spLocation_Near_distance A distance quantity to limit the near search to locations within a specific distance }
end;

Const
  CHECK_TSearchParamsQuery : Array[TSearchParamsQuery] of TSearchParamsQuery = ( spQuery__id, spQuery__Language, spQuery_Identifier, spQuery_Response);

procedure TFhirIndexManager.buildIndexesQuery;
var
  a : TSearchParamsQuery;
begin
  for a := low(TSearchParamsQuery) to high(TSearchParamsQuery) do
  begin
    assert(CHECK_TSearchParamsQuery[a] = a);
    indexes.add(frtQuery, CODES_TSearchParamsQuery[a], DESC_TSearchParamsQuery[a], TYPES_TSearchParamsQuery[a], TARGETS_TSearchParamsQuery[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesQuery(key: integer; id : String; resource: TFhirQuery);
begin
  index(frtQuery, key, resource.identifier, 'identifier');
  if resource.response <> nil then
    index(frtQuery, key, resource.response.identifier, 'response');
end;

Const
  CHECK_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of TSearchParamsQuestionnaire = ( spQuestionnaire__id,  spQuestionnaire__Language,  spQuestionnaire_Author, spQuestionnaire_Authored, spQuestionnaire_Encounter, spQuestionnaire_Identifier, spQuestionnaire_Name, spQuestionnaire_Status, spQuestionnaire_Subject);

procedure TFhirIndexManager.buildIndexesQuestionnaire;
var
  a : TSearchParamsQuestionnaire;
begin
  for a := low(TSearchParamsQuestionnaire) to high(TSearchParamsQuestionnaire) do
  begin
    assert(CHECK_TSearchParamsQuestionnaire[a] = a);
    indexes.add(frtQuestionnaire, CODES_TSearchParamsQuestionnaire[a], DESC_TSearchParamsQuestionnaire[a], TYPES_TSearchParamsQuestionnaire[a], TARGETS_TSearchParamsQuestionnaire[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesQuestionnaire(key: integer; id : String; resource: TFhirQuestionnaire);
var
  i : integer;
begin
  index(frtQuestionnaire, key, resource, resource.author, 'author');
  index(frtQuestionnaire, key, resource.authored, 'authored');
  index(frtQuestionnaire, key, resource, resource.Encounter, 'Encounter');
  for i := 0 to resource.identifierList.Count - 1 do
    index(frtQuestionnaire, key, resource.identifierList[i], 'identifier');
  index(frtQuestionnaire, key, resource.name, 'name');
  index(frtQuestionnaire, key, resource.status, 'status');
  index(frtQuestionnaire, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
end;


Const
  CHECK_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of TSearchParamsDocumentReference = ( spDocumentReference__id, spDocumentReference__Language, spDocumentReference_Authenticator, spDocumentReference_Author, spDocumentReference_Class, spDocumentReference_Confidentiality, spDocumentReference_Created, spDocumentReference_Custodian, spDocumentReference_Description, spDocumentReference_Event, spDocumentReference_Facility, spDocumentReference_Format, spDocumentReference_Identifier, spDocumentReference_Indexed, spDocumentReference_Language, spDocumentReference_Location, spDocumentReference_Period, spDocumentReference_Relatesto, spDocumentReference_Relation, spDocumentReference_Relationship, spDocumentReference_Size, spDocumentReference_Status, spDocumentReference_Subject, spDocumentReference_Type);

procedure TFhirIndexManager.buildIndexesDocumentReference;
var
  a : TSearchParamsDocumentReference;
begin
  for a := low(TSearchParamsDocumentReference) to high(TSearchParamsDocumentReference) do
  begin
    assert(CHECK_TSearchParamsDocumentReference[a] = a);
    indexes.add(frtDocumentReference, CODES_TSearchParamsDocumentReference[a], DESC_TSearchParamsDocumentReference[a], TYPES_TSearchParamsDocumentReference[a], TARGETS_TSearchParamsDocumentReference[a]);
  end;
end;

procedure TFhirIndexManager.BuildIndexValuesDocumentReference(key: integer;id : String; resource: TFhirDocumentReference);
var
  i : integer;
begin
  index(frtDocumentReference, key, resource, resource.authenticator, 'authenticator');
  for i := 0 to resource.authorList.count - 1 do
    index(frtDocumentReference, key, resource, resource.authorList[i], 'author');
  for i := 0 to resource.confidentialityList.count - 1 do
    index(frtDocumentReference, key, resource.confidentialityList[i], 'confidentiality');
  index(frtDocumentReference, key, resource.created, 'created');
  index(frtDocumentReference, key, resource, resource.custodian, 'custodian');
  index(frtDocumentReference, key, resource.description, 'description');
  if resource.context <> nil then
  begin
    for i := 0 to resource.context.eventList.count - 1 do
      index(frtDocumentReference, key, resource.context.eventList[i], 'event');
    index(frtDocumentReference, key, resource.context.facilityType, 'facility');
    index(frtDocumentReference, key, resource.context.period, 'period');
  end;
  for i := 0 to resource.formatList.count - 1 do
    index(frtDocumentReference, key, resource.formatList[i], 'format');
  index(frtDocumentReference, key, resource.masterIdentifier, 'identifier');
  for i := 0 to resource.identifierList.count - 1 do
    index(frtDocumentReference, key, resource.identifierList[i], 'identifier');
  index(frtDocumentReference, key, resource.indexed, 'indexed');
  index(frtDocumentReference, key, resource.primaryLanguage, 'language');
  index(frtDocumentReference, key, resource.location, 'location');
  index(frtDocumentReference, key, resource.size, 'size');
  index(frtDocumentReference, key, resource.status, 'status');
  index(frtDocumentReference, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  for i := 0 to resource.relatesToList.Count - 1 do
  begin
    index(frtDocumentReference, key, resource, resource.relatesToList[i].target, 'relatesTo');
    index(frtDocumentReference, key, resource.relatesToList[i].code, 'relation');
  end;
  index(frtDocumentReference, key, resource.type_, 'type');
  index(frtDocumentReference, key, resource.class_, 'class');
end;

Const
  CHECK_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of TSearchParamsDocumentManifest = ( spDocumentManifest__id, spDocumentManifest__Language, spDocumentManifest_Author, spDocumentManifest_Confidentiality, spDocumentManifest_Content, spDocumentManifest_Created, spDocumentManifest_Description, spDocumentManifest_Identifier, spDocumentManifest_Recipient, spDocumentManifest_Status, spDocumentManifest_Subject, spDocumentManifest_Supersedes, spDocumentManifest_Type);

procedure TFhirIndexManager.buildIndexesDocumentManifest;
var
  a : TSearchParamsDocumentManifest;
begin
  for a := low(TSearchParamsDocumentManifest) to high(TSearchParamsDocumentManifest) do
  begin
    assert(CHECK_TSearchParamsDocumentManifest[a] = a);
    indexes.add(frtDocumentManifest, CODES_TSearchParamsDocumentManifest[a], DESC_TSearchParamsDocumentManifest[a], TYPES_TSearchParamsDocumentManifest[a], TARGETS_TSearchParamsDocumentManifest[a]);
  end;
end;

procedure TFhirIndexManager.BuildIndexValuesDocumentManifest(key: integer;id : String; resource: TFhirDocumentManifest);
var
  i : integer;
begin
  for i := 0 to resource.authorList.count - 1 do
    index(frtDocumentManifest, key, resource, resource.authorList[i], 'author');
  index(frtDocumentManifest, key, resource.confidentiality, 'confidentiality');
  index(frtDocumentManifest, key, resource.created, 'created');
  index(frtDocumentManifest, key, resource.description, 'description');
  index(frtDocumentManifest, key, resource.masterIdentifier, 'identifier');
  for i := 0 to resource.identifierList.count - 1 do
    index(frtDocumentManifest, key, resource.identifierList[i], 'identifier');
  index(frtDocumentManifest, key, resource.status, 'status');
  for i := 0 to resource.subjectList.count - 1 do
    index(frtDocumentManifest, key, resource, resource.subjectList[i], 'subject');
  index(frtDocumentManifest, key, resource, resource.supercedes, 'supercedes');
  index(frtDocumentManifest, key, resource.type_, 'type');
  for i := 0 to resource.recipientList.count - 1 do
    index(frtDocumentManifest, key, resource, resource.recipientList[i], 'recipient');
  for i := 0 to resource.contentList.count - 1 do
    index(frtDocumentManifest, key, resource, resource.contentList[i], 'content');
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

procedure TFhirIndexManager.index(aType: TFhirResourceType; key: integer; value: TFhirInteger; name: String);
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
  if not (ndx.SearchType in [SearchParamTypeString, SearchParamTypeNumber, SearchParamTypeToken]) then
    raise Exception.create('Unsuitable index '+name+' : '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing integer');
  FEntries.add(key, ndx, 0, value.value, '', 0, ndx.SearchType);
end;

Const
  CHECK_TSearchParamsAdverseReaction : Array[TSearchParamsAdverseReaction] of TSearchParamsAdverseReaction = ( spAdverseReaction__id, spAdverseReaction__Language, spAdverseReaction_Date, spAdverseReaction_Subject, spAdverseReaction_Substance, spAdverseReaction_Symptom);

procedure TFhirIndexManager.buildIndexesAdverseReaction;
var
  a : TSearchParamsAdverseReaction;
begin
  for a := low(TSearchParamsAdverseReaction) to high(TSearchParamsAdverseReaction) do
  begin
    assert(CHECK_TSearchParamsAdverseReaction[a] = a);
    indexes.add(frtAdverseReaction, CODES_TSearchParamsAdverseReaction[a], DESC_TSearchParamsAdverseReaction[a], TYPES_TSearchParamsAdverseReaction[a], TARGETS_TSearchParamsAdverseReaction[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesAdverseReaction(key: integer; id : String; resource: TFhirAdverseReaction);
var
  i : integer;
begin
  index(frtAdverseReaction, key, resource.date, 'date');
  index(frtAdverseReaction, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
//  index(frtAdverseReaction, key, resource, resource.substance, 'substance');
  for i := 0 to resource.symptomList.count - 1 do
    index(frtAdverseReaction, key, resource.symptomList[i].code, 'symptom');
end;

Const
  CHECK_TSearchParamsAlert : Array[TSearchParamsAlert] of TSearchParamsAlert = ( spAlert__id, spAlert__Language, spAlert_Subject);

procedure TFhirIndexManager.buildIndexesAlert;
var
  a : TSearchParamsAlert;
begin
  for a := low(TSearchParamsAlert) to high(TSearchParamsAlert) do
  begin
    assert(CHECK_TSearchParamsAlert[a] = a);
    indexes.add(frtAlert, CODES_TSearchParamsAlert[a], DESC_TSearchParamsAlert[a], TYPES_TSearchParamsAlert[a], TARGETS_TSearchParamsAlert[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesAlert(key: integer; id : String; resource: TFhirAlert);
begin
  index(frtAlert, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
end;

Const
  CHECK_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of TSearchParamsAllergyIntolerance = ( spAllergyIntolerance__id, spAllergyIntolerance__Language, spAllergyIntolerance_Date, spAllergyIntolerance_Recorder, spAllergyIntolerance_Status, spAllergyIntolerance_Subject, spAllergyIntolerance_Substance, spAllergyIntolerance_Type);

procedure TFhirIndexManager.buildIndexesAllergyIntolerance;
var
  a : TSearchParamsAllergyIntolerance;
begin
  for a := low(TSearchParamsAllergyIntolerance) to high(TSearchParamsAllergyIntolerance) do
  begin
    assert(CHECK_TSearchParamsAllergyIntolerance[a] = a);
    indexes.add(frtAllergyIntolerance, CODES_TSearchParamsAllergyIntolerance[a], DESC_TSearchParamsAllergyIntolerance[a], TYPES_TSearchParamsAllergyIntolerance[a], TARGETS_TSearchParamsAllergyIntolerance[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesAllergyIntolerance(key: integer; id : String; resource: TFhirAllergyIntolerance);
begin
  index(frtAllergyIntolerance, key, resource.recordedDate, 'date');
  index(frtAllergyIntolerance, key, resource.status, 'status');
  index(frtAllergyIntolerance, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtAllergyIntolerance, key, resource, resource.recorder, 'recorder');
  index(frtAllergyIntolerance, key, resource, resource.substance, 'substance');
  index(frtAllergyIntolerance, key, resource.sensitivityType, 'type');
end;

Const
  CHECK_TSearchParamsSubstance : Array[TSearchParamsSubstance] of TSearchParamsSubstance = ( spSubstance__id, spSubstance__Language, spSubstance_Expiry, spSubstance_Identifier, spSubstance_Quantity, spSubstance_Substance, spSubstance_Type);


procedure TFhirIndexManager.buildIndexesSubstance;
var
  a : TSearchParamsSubstance;
begin
  for a := low(TSearchParamsSubstance) to high(TSearchParamsSubstance) do
  begin
    assert(CHECK_TSearchParamsSubstance[a] = a);
    indexes.add(frtSubstance, CODES_TSearchParamsSubstance[a], DESC_TSearchParamsSubstance[a], TYPES_TSearchParamsSubstance[a], TARGETS_TSearchParamsSubstance[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesSubstance(key: integer; id : String; resource: TFhirSubstance);
var
  i : integer;
begin
  index(frtSubstance, key, resource.type_, 'type');
  if resource.instance <> nil then
  begin
    index(frtSubstance, key, resource.instance.identifier, 'identifier');
    index(frtSubstance, key, resource.instance.expiry, 'expiry');
  end;
  for i := 0 to resource.ingredientList.count - 1 do
  begin
    index(frtSubstance, key, resource.ingredientList[i].quantity, 'quantity');
    index(frtSubstance, key, resource, resource.ingredientList[i].substance, 'substance');
  end;
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key: integer; value: TFhirDate; name: String);
begin
  if (value <> nil) and (value.value <> nil) then
    index(aType, key, asUTCMin(value), asUTCMax(value), name);
end;

procedure TFhirIndexManager.index(aType: TFhirResourceType; key: integer; value: TFhirBoolean; name: String);
var
  ndx : TFhirIndex;
begin
  if (value = nil) then
    exit;
  ndx := FIndexes.getByName(aType, name);
  if (ndx = nil) then
    raise Exception.create('Unknown index '+name);
  if (ndx.TargetTypes <> []) then
    raise Exception.create('Attempt to index a simple type in an index that is a resource join');
  if not (ndx.SearchType in [SearchParamTypeString, SearchParamTypeNumber, SearchParamTypeToken]) then
    raise Exception.create('Unsuitable index '+name+' : '+CODES_TFhirSearchParamType[ndx.SearchType]+' indexing boolean');
  FEntries.add(key, ndx, 0, BooleanToString(value.value), '', 0, ndx.SearchType);
end;

Const
  CHECK_TSearchParamsOther : Array[TSearchParamsOther] of TSearchParamsOther = ( spOther__id, spOther__Language, spOther_Code, spOther_Created, spOther_Subject);

procedure TFhirIndexManager.buildIndexesOther;
var
  a : TSearchParamsOther;
begin
  for a := low(TSearchParamsOther) to high(TSearchParamsOther) do
  begin
    assert(CHECK_TSearchParamsOther[a] = a);
    indexes.add(frtOther, CODES_TSearchParamsOther[a], DESC_TSearchParamsOther[a], TYPES_TSearchParamsOther[a], TARGETS_TSearchParamsOther[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesOther(key: integer; id : String; resource: TFhirOther);
begin
  index(frtOther, key, resource.created, 'created');
  index(frtOther, key, resource.code, 'code');
  index(frtOther, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
end;

Const
  CHECK_TSearchParamsSupply : Array[TSearchParamsSupply] of TSearchParamsSupply = ( spSupply__id, spSupply__Language, spSupply_Dispenseid, spSupply_Dispensestatus, spSupply_Identifier, spSupply_Kind, spSupply_Patient, spSupply_Status, spSupply_Supplier);

procedure TFhirIndexManager.buildIndexesSupply;
var
  a : TSearchParamsSupply;
begin
  for a := low(TSearchParamsSupply) to high(TSearchParamsSupply) do
  begin
    assert(CHECK_TSearchParamsSupply[a] = a);
    indexes.add(frtSupply, CODES_TSearchParamsSupply[a], DESC_TSearchParamsSupply[a], TYPES_TSearchParamsSupply[a], TARGETS_TSearchParamsSupply[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesSupply(key: integer; id : String; resource: TFhirSupply);
var
  i : integer;
begin
  index(frtSupply, key, resource.identifier, 'identifier');
  index(frtSupply, key, resource.kind, 'kind');
  index(frtSupply, key, resource.status, 'status');
  index(frtSupply, key, resource, resource.patient, 'patient');
  patientCompartment(key, resource.patient);
  for i := 0 to resource.dispenseList.count - 1 do
  begin
    index(frtSupply, key, resource.dispenseList[i].identifier, 'dispenseid');
    index(frtSupply, key, resource.dispenseList[i].status, 'dispensestatus');
    index(frtSupply, key, resource, resource.dispenseList[i].supplier, 'supplier');
  end;
end;

Const
  CHECK_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of TSearchParamsRelatedPerson = (spRelatedPerson__id, spRelatedPerson__Language, spRelatedPerson_Address, spRelatedPerson_Gender, spRelatedPerson_Identifier, spRelatedPerson_Name, spRelatedPerson_Patient, spRelatedPerson_Phonetic, spRelatedPerson_Telecom);

procedure TFhirIndexManager.buildIndexesRelatedPerson;
var
  a : TSearchParamsRelatedPerson;
begin
  for a := low(TSearchParamsRelatedPerson) to high(TSearchParamsRelatedPerson) do
  begin
    assert(CHECK_TSearchParamsRelatedPerson[a] = a);
    indexes.add(frtRelatedPerson, CODES_TSearchParamsRelatedPerson[a], DESC_TSearchParamsRelatedPerson[a], TYPES_TSearchParamsRelatedPerson[a], TARGETS_TSearchParamsRelatedPerson[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesRelatedPerson(key: integer; id : String; resource: TFhirRelatedPerson);
var
  i : integer;
begin
  index(frtRelatedPerson, key, resource.address, 'address');
  index(frtRelatedPerson, key, resource.gender, 'gender');
  for i := 0 to resource.identifierList.count - 1 do
    index(frtRelatedPerson, key, resource.identifierList[i], 'identifier');
  index(frtRelatedPerson, key, resource.name, 'name', 'phonetic');
  index(frtRelatedPerson, key, resource, resource.patient, 'patient');
  patientCompartment(key, resource.patient);
  for i := 0 to resource.telecomList.count - 1 do
    index(frtRelatedPerson, key, resource.telecomList[i], 'telecom');
end;

procedure TFhirIndexManager.patientCompartment(key : integer; reference: TFhirResourceReference);
var
  sid : string;
begin
  if reference = nil then
    exit;
  if reference.referenceST = '' then
    exit;
  if StringStartsWith(reference.referenceST, '#') then
    exit; // what to do in this case?
  if not StringStartsWith(reference.referenceST, 'Patient/') then
    exit; // what to do in this case?
  sid := copy(reference.referenceST, 9, $FF);
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

procedure TFhirIndexManager.index(aType: TFhirResourceType; key: integer; value: Boolean; name: String);
begin
  index(aType, key, BooleanToString(value), name);
end;

procedure TFhirIndexManager.processCompartmentTags(key: integer; id: String; tags: TFHIRAtomCategoryList);
var
  i : integer;
begin
  for i := 0 to tags.Count - 1 do
    if StringStartsWith(tags[i].term, TAG_COMPARTMENT_IN) then
      patientCompartment(key, 'Patient', Copy(tags[i].term, length(TAG_COMPARTMENT_IN), $FF));
end;

procedure TFhirIndexManager.processUnCompartmentTags(key: integer; id: String; tags: TFHIRAtomCategoryList);
var
  i : integer;
begin
  for i := 0 to tags.Count - 1 do
    if StringStartsWith(tags[i].term, TAG_COMPARTMENT_OUT) then
      patientCompartmentNot(key, 'Patient', Copy(tags[i].term, length(TAG_COMPARTMENT_OUT), $FF));
end;

{ TFhirIndexSpaces }

constructor TFhirIndexSpaces.Create(db: TKDBConnection);
begin
  inherited create;
  FSpaces := TStringList.Create;
  FSpaces.Sorted := true;

  FDB := db;
  FDB.SQL := 'select * from Spaces';
  FDb.prepare;
  FDb.execute;
  while FDb.FetchNext do
    FSpaces.addObject(FDb.ColStringByName['Space'], TObject(FDb.ColIntegerByName['SpaceKey']));
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
  CHECK_TSearchParamsConformance : Array[TSearchParamsConformance] of TSearchParamsConformance = ( spConformance__id, spConformance__Language, spConformance_Date, spConformance_Description, spConformance_Event, spConformance_Fhirversion, spConformance_Format, spConformance_Identifier, spConformance_Mode, spConformance_Name, spConformance_Profile, spConformance_Publisher, spConformance_Resource, spConformance_Security, spConformance_Software, spConformance_Status, spConformance_Supported_profile, spConformance_Version);

procedure TFhirIndexManager.buildIndexesConformance();
var
  a : TSearchParamsConformance;
begin
  for a := low(TSearchParamsConformance) to high(TSearchParamsConformance) do
  begin
    assert(CHECK_TSearchParamsConformance[a] = a);
    indexes.add(frtConformance, CODES_TSearchParamsConformance[a], DESC_TSearchParamsConformance[a], TYPES_TSearchParamsConformance[a], TARGETS_TSearchParamsConformance[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesConformance(key : integer; id : String; resource: TFhirConformance);
var
  i : integer;
  j : integer;
begin
  index(frtConformance, key, resource.date, 'date');
  index(frtConformance, key, resource.name, 'name');
  index(frtConformance, key, resource.status, 'status');
  index(frtConformance, key, resource.description, 'description');
  index(frtConformance, key, resource.publisher, 'publisher');
  if resource.software <> nil then
    index(frtConformance, key, resource.software.name, 'software');
  index(frtConformance, key, resource.version, 'version');
  index(frtConformance, key, resource.fhirversion, 'fhirversion');
  index(frtConformance, key, resource.identifier, 'identifier');

  for j := 0 to resource.formatList.Count - 1 do
    index(frtConformance, key, resource.formatList[j], 'format');

  for j := 0 to resource.restList.Count - 1 do
  begin
    if resource.restList[j].security <> nil then
    begin
      for i := 0 to resource.restList[j].security.serviceList.count - 1 do
        index(frtConformance, key, resource.restList[j].security.serviceList[i], 'security');
    end;
  end;


  for j := 0 to resource.restList.Count - 1 do
  begin
    for i := 0 to resource.restList[j].resourceList.count - 1 do
    begin
      index(frtConformance, key, resource, resource.restList[j].resourceList[i].profile, 'profile');
      index(frtConformance, key, resource.restList[j].resourceList[i].type_, 'resource');
    end;
    index(frtConformance, key, resource.restList[j].mode, 'mode');
  end;

  for j := 0 to resource.messagingList.Count - 1 Do
  begin
    for i := 0 to resource.messagingList[j].EventList.count - 1 do
    begin
      index(frtConformance, key, resource.messagingList[j].EventList[i].focus, 'resource');
      index(frtConformance, key, resource, resource.messagingList[j].EventList[i].request, 'profile');
      index(frtConformance, key, resource, resource.messagingList[j].EventList[i].response, 'profile');
      index(frtConformance, key, resource.messagingList[j].EventList[i].mode, 'mode');
      index(frtConformance, key, resource.messagingList[j].EventList[i].code, 'event');
    end;
  end;

  for i := 0 to resource.DocumentList.count - 1 do
    index(frtConformance, key, resource, resource.DocumentList[i].profile, 'profile');
  for i := 0 to resource.profileList.count - 1 do
    index(frtConformance, key, resource, resource.ProfileList[i], 'supported-profile');
end;

{ TFhirCompositionIndexManager }

Const
  CHECK_TSearchParamsComposition : Array[TSearchParamsComposition] of TSearchParamsComposition = ( spComposition__id, spComposition__Language, spComposition_Attester, spComposition_Author, spComposition_Class, spComposition_Context, spComposition_Date, spComposition_Identifier, spComposition_Section_content, spComposition_Section_type, spComposition_Subject, spComposition_Type);

procedure TFhirIndexManager.buildIndexesComposition;
var
  a : TSearchParamsComposition;
begin
  for a := low(TSearchParamsComposition) to high(TSearchParamsComposition) do
  begin
    assert(CHECK_TSearchParamsComposition[a] = a);
    indexes.add(frtComposition, CODES_TSearchParamsComposition[a], DESC_TSearchParamsComposition[a], TYPES_TSearchParamsComposition[a], TARGETS_TSearchParamsComposition[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesComposition(key : integer; id : String; resource: TFhirComposition);
  procedure indexSection(section : TFhirCompositionSection);
  var
    i : integer;
  begin
    index(frtComposition, key, section.code, 'section-type');
    index(frtComposition, key, resource, section.content, 'section-content');
    for i := 0 to section.SectionList.count - 1 do
      indexSection(section.SectionList[i]);
  end;
var
  i : integer;
begin
  index(frtComposition, key, resource.date, 'date');
  index(frtComposition, key, resource.identifier, 'identifier');
  index(frtComposition, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtComposition, key, resource.type_, 'type');
  index(frtComposition, key, resource.class_, 'class');
  if resource.event <> nil then
    for i := 0 to resource.event.codeList.Count - 1 do
      index(frtComposition, key, resource.event.codeList[i], 'context');
  for i := 0 to resource.authorList.count - 1 do
    index(frtComposition, key, resource, resource.authorList[i], 'author');
  for i := 0 to resource.attesterList.count - 1 do
    index(frtComposition, key, resource, resource.attesterList[i].party, 'attester');
  for i := 0 to resource.SectionList.count - 1 do
    indexSection(resource.SectionList[i]);
end;


Const
  CHECK_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of TSearchParamsMessageHeader = ( spMessageHeader__id, spMessageHeader__Language);

procedure TFhirIndexManager.buildIndexesMessageHeader;
var
  a : TSearchParamsMessageHeader;
begin
  for a := low(TSearchParamsMessageHeader) to high(TSearchParamsMessageHeader) do
  begin
    assert(CHECK_TSearchParamsMessageHeader[a] = a);
    indexes.add(frtMessageHeader, CODES_TSearchParamsMessageHeader[a], DESC_TSearchParamsMessageHeader[a], TYPES_TSearchParamsMessageHeader[a], TARGETS_TSearchParamsMessageHeader[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesMessageHeader(key : integer; id : String; resource: TFhirMessageHeader);
begin
  //raise exception.create('should not call this method (TFhirMessageHeaderIndexManager.buildIndexValues)');
end;


Const
  CHECK_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of TSearchParamsPractitioner = ( spPractitioner__id,  spPractitioner__Language,  spPractitioner_Address, spPractitioner_Family, spPractitioner_Gender, spPractitioner_Given, spPractitioner_Identifier, spPractitioner_Name, spPractitioner_Organization, spPractitioner_Phonetic, spPractitioner_Telecom);

procedure TFhirIndexManager.buildIndexesPractitioner;
var
  a : TSearchParamsPractitioner;
begin
  for a := low(TSearchParamsPractitioner) to high(TSearchParamsPractitioner) do
  begin
    assert(CHECK_TSearchParamsPractitioner[a] = a);
    indexes.add(frtPractitioner, CODES_TSearchParamsPractitioner[a], DESC_TSearchParamsPractitioner[a], TYPES_TSearchParamsPractitioner[a], TARGETS_TSearchParamsPractitioner[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesPractitioner(key : integer; id : String; resource: TFhirPractitioner);
var
  i, j : integer;
begin
  for i := 0 to resource.identifierList.count - 1 do
    index(frtPractitioner, key, resource.identifierList[0], 'identifier');
  if resource.name <> nil then
  begin
    index(frtPractitioner, key, resource.name, 'name', 'phonetic');
    for j := 0 to resource.name.givenList.count - 1 do
      index(frtPractitioner, key, resource.name.givenList[j], 'given');
    for j := 0 to resource.name.familyList.count - 1 do
      index(frtPractitioner, key, resource.name.familyList[j], 'family');
  end;
  for i := 0 to resource.telecomList.count - 1 do
    index(frtPractitioner, key, resource.telecomList[0].value, 'telecom');
  index(frtPractitioner, key, resource.address, 'address');
  index(frtPractitioner, key, resource.gender, 'gender');
  index(frtPractitioner, key, resource, resource.organization, 'organization');
end;


Const
  CHECK_TSearchParamsOrganization : Array[TSearchParamsOrganization] of TSearchParamsOrganization = ( spOrganization__id, spOrganization__Language, spOrganization_Active, spOrganization_Identifier, spOrganization_Name, spOrganization_Partof, spOrganization_Phonetic, spOrganization_Type);



procedure TFhirIndexManager.buildIndexesOrganization;
var
  a : TSearchParamsOrganization;
begin
  for a := low(TSearchParamsOrganization) to high(TSearchParamsOrganization) do
  begin
    assert(CHECK_TSearchParamsOrganization[a] = a);
    indexes.add(frtOrganization, CODES_TSearchParamsOrganization[a], DESC_TSearchParamsOrganization[a], TYPES_TSearchParamsOrganization[a], TARGETS_TSearchParamsOrganization[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesOrganization(key : integer;  id : String; resource: TFhirOrganization);
var
  i : integer;
begin
  index(frtOrganization, key, resource.active, 'active');
  index(frtOrganization, key, resource.Name, 'name');
  index(frtOrganization, key, EncodeNYSIISValue(resource.Name), 'phonetic');
  index(frtOrganization, key, resource.type_, 'type');
  for i := 0 to resource.IdentifierList.Count - 1 Do
    if resource.IdentifierList[i] <> nil then
      index(frtOrganization, key, resource.IdentifierList[i], 'identifier');
//  for i := 0 to resource.telecomList.Count - 1 Do
//    index(frtOrganization, key, resource.telecomList[i].value, 'telecom');
//  for i := 0 to resource.addressList.Count - 1 Do
//    index(frtOrganization, key, resource.addressList[i], 'address');

//  for j := 0 to resource.contactEntityList.Count - 1 Do
//  begin
//    contact := resource.contactEntityList[j];
//    index(frtOrganization, key, contact.name, 'cname', '');
//   index(frtOrganization, key, contact.address, 'caddress');
//    for i := 0 to contact.telecomList.Count - 1 Do
//      index(frtOrganization, key, contact.telecomList[i].value, 'ctelecom');
//  end;
  index(frtOrganization, key, resource, resource.partOf, 'partOf');
end;


Const
  CHECK_TSearchParamsGroup : Array[TSearchParamsGroup] of TSearchParamsGroup = ( spGroup__id, spGroup__Language, spGroup_Actual, spGroup_Characteristic, spGroup_Characteristic_value, spGroup_Code, spGroup_Exclude, spGroup_Identifier, spGroup_Member, spGroup_Type, spGroup_Value);

procedure TFhirIndexManager.buildIndexesGroup;
var
  a : TSearchParamsGroup;
begin
  for a := low(TSearchParamsGroup) to high(TSearchParamsGroup) do
  begin
    assert(CHECK_TSearchParamsGroup[a] = a);
    indexes.add(frtGroup, CODES_TSearchParamsGroup[a], DESC_TSearchParamsGroup[a], TYPES_TSearchParamsGroup[a], TARGETS_TSearchParamsGroup[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesGroup(key : integer;  id : String; resource: TFhirGroup);
var
  i : integer;
begin
  index(frtGroup, key, resource.actual, 'actual');
  index(frtGroup, key, resource.code, 'code');
  index(frtGroup, key, resource.type_, 'type');
  index(frtGroup, key, resource.identifier, 'identifier');

  for i := 0 to resource.memberList.Count - 1 Do
    index(frtGroup, key, resource, resource.memberList[i], 'member');

  for i := 0 to resource.characteristicList.Count - 1 Do
  begin
    index(frtGroup, key, resource.characteristicList[i].code, 'characteristic');
    index(frtGroup, key, resource.characteristicList[i].exclude, 'exclude');
    if resource.characteristicList[i].value is TFhirBoolean then
      index(frtGroup, key, TFhirBoolean(resource.characteristicList[i].value).value, 'value')
    else if resource.characteristicList[i].value is TFhirString then
      index(frtGroup, key, TFhirString(resource.characteristicList[i].value), 'value')
    else if resource.characteristicList[i].value is TFhirCodeableConcept then
      index(frtGroup, key, TFhirCodeableConcept(resource.characteristicList[i].value), 'value')
  end;
end;

Const
  CHECK_TSearchParamsObservation : Array[TSearchParamsObservation] of TSearchParamsObservation = ( spObservation__id, spObservation__Language, spObservation_Date, spObservation_Name,   spObservation_Name_value_x, spObservation_Performer, spObservation_Related, spObservation_Related_target, spObservation_Related_type, spObservation_Reliability, spObservation_Specimen, spObservation_Status, spObservation_Subject, spObservation_Value_concept, spObservation_Value_date, spObservation_Value_quantity, spObservation_Value_string);

procedure TFhirIndexManager.buildIndexesObservation;
var
  a : TSearchParamsObservation;
begin
  for a := low(TSearchParamsObservation) to high(TSearchParamsObservation) do
  begin
    assert(CHECK_TSearchParamsObservation[a] = a);
    indexes.add(frtObservation, CODES_TSearchParamsObservation[a], DESC_TSearchParamsObservation[a], TYPES_TSearchParamsObservation[a], TARGETS_TSearchParamsObservation[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesObservation(key : integer;  id : String; resource: TFhirObservation);
var
  i : integer;
begin
  index(frtObservation, key, resource.name, 'name');
  index(frtObservation, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  if resource.applies is TFhirDateTime then
    index(frtObservation, key, TFhirDateTime(resource.applies), 'date');
  index(frtObservation, key, resource.status, 'status');
  index(frtObservation, key, resource.reliability, 'reliability');
  for i := 0 to resource.performerList.Count - 1 Do
    index(frtObservation, key, resource, resource.performerList[i], 'performer');
  index(frtObservation, key, resource, resource.specimen, 'specimen');

  if resource.value is TFhirQuantity then
    index(frtObservation, key, TFhirQuantity(resource.value), 'value-quantity')
  else if resource.value is TFhirSampledData then
    index(frtObservation, key, TFhirSampledData(resource.value), 'value-quantity')
  else if resource.value is TFhirRatio then
    index(frtObservation, key, TFhirRatio(resource.value), 'value-quantity')
  else if resource.value is TFhirCodeableConcept then
    index(frtObservation, key, TFhirCodeableConcept(resource.value), 'value-concept')
  else if resource.value is TFhirPeriod then
    index(frtObservation, key, TFhirPeriod(resource.value), 'value-date')
  else if resource.value is TFhirString then
    index(frtObservation, key, TFhirString(resource.value).value, 'value-string');

  for i := 0 to resource.relatedList.Count - 1 Do
  begin
    index(frtObservation, key, resource.relatedList[i].type_, 'related-type');
    index(frtObservation, key, resource, resource.relatedList[i].target, 'related-target');
  end;
end;

Const
  CHECK_TSearchParamsProfile : Array[TSearchParamsProfile] of TSearchParamsProfile = ( spProfile__id,  spProfile__Language,  spProfile_Code, spProfile_Date, spProfile_Description, spProfile_Extension, spProfile_Identifier, spProfile_Name, spProfile_Publisher, spProfile_Status, spProfile_Type, spProfile_Valueset, spProfile_Version);

procedure TFhirIndexManager.buildIndexesProfile;
var
  a : TSearchParamsProfile;
begin
  for a := low(TSearchParamsProfile) to high(TSearchParamsProfile) do
  begin
    assert(CHECK_TSearchParamsProfile[a] = a);
    indexes.add(frtProfile, CODES_TSearchParamsProfile[a], DESC_TSearchParamsProfile[a], TYPES_TSearchParamsProfile[a], TARGETS_TSearchParamsProfile[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesProfile(key : integer; id : String; resource: TFhirProfile);
var
  i, j : integer;
begin
  index(frtProfile, key, resource.name, 'name');
  index(frtProfile, key, resource.date, 'date');
  index(frtProfile, key, resource.description, 'description');
  index(frtProfile, key, resource.status, 'status');
  index(frtProfile, key, resource.identifier, 'identifier');
  index(frtProfile, key, resource.version, 'version');
  index(frtProfile, key, resource.publisher, 'publisher');
  for i := 0 to resource.CodeList.count - 1 Do
    index(frtProfile, key, resource.CodeList[i], 'code');
  for i := 0 to resource.StructureList.count - 1 do
  begin
    index(frtProfile, key, resource.StructureList[i].type_, 'type');
    for j := 0 to resource.structureList[i].elementList.Count - 1 do
      if (resource.structureList[i].elementList[j].definition <> nil) and
        (resource.structureList[i].elementList[j].definition.binding <> nil) then
        if resource.structureList[i].elementList[j].definition.binding.reference is TFhirUri then
          index(frtProfile, key, TFhirUri(resource.structureList[i].elementList[j].definition.binding.reference), 'valueset')
        else
          index(frtProfile, key, resource, TFhirResourceReference(resource.structureList[i].elementList[j].definition.binding.reference), 'valueset');
  end;
  for i := 0 to resource.ExtensionDefnList.count - 1 do
    index(frtProfile, key, resource.ExtensionDefnList[i].code, 'extension');
end;

Const
  CHECK_TSearchParamsPatient : Array[TSearchParamsPatient] of TSearchParamsPatient = ( spPatient__id,  spPatient__Language,  spPatient_Active, spPatient_Address, spPatient_Animal_breed, spPatient_Animal_species, spPatient_Birthdate, spPatient_Family, spPatient_Gender, spPatient_Given, spPatient_Identifier, spPatient_Language, spPatient_Link, spPatient_Name, spPatient_Phonetic, spPatient_Provider, spPatient_Telecom);

procedure TFhirIndexManager.buildIndexesPatient;
var
  a : TSearchParamsPatient;
begin
  for a := low(TSearchParamsPatient) to high(TSearchParamsPatient) do
  begin
    assert(CHECK_TSearchParamsPatient[a] = a);
    indexes.add(frtPatient, CODES_TSearchParamsPatient[a], DESC_TSearchParamsPatient[a], TYPES_TSearchParamsPatient[a], TARGETS_TSearchParamsPatient[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesPatient(key : integer; id : String; resource: TFhirPatient);
var
  i, j : integer;
begin
  for i := 0 to resource.IdentifierList.Count - 1 Do
    if resource.IdentifierList[i] <> nil then
      index(frtPatient, key, resource.IdentifierList[i], 'identifier');
    for i := 0 to resource.nameList.count - 1 do
    begin
      index(frtPractitioner, key, resource.nameList[i], 'name', 'phonetic');
      for j := 0 to resource.nameList[i].givenList.count - 1 do
        index(frtPractitioner, key, resource.nameList[i].givenList[j], 'given');
      for j := 0 to resource.nameList[i].familyList.count - 1 do
        index(frtPractitioner, key, resource.nameList[i].familyList[j], 'family');
    end;

    for i := 0 to resource.telecomList.Count - 1 do
      index(frtPatient, key, resource.telecomList[i].value, 'telecom');
    for i := 0 to resource.AddressList.Count - 1 Do
      index(frtPatient, key, resource.AddressList[i], 'address');
    index(frtPatient, key, resource.gender, 'gender');
    for i := 0 to resource.communicationList.Count - 1 Do
      index(frtPatient, key, resource.communicationList[i], 'language');
    index(frtPatient, key, resource.birthDate, 'birthdate');

  index(frtPatient, key, resource, resource.managingOrganization, 'provider');

  for i := 0 to resource.link_List.count - 1 do
    index(frtPatient, key, resource, resource.link_List[i].other, 'link');

  index(frtPatient, key, resource.activeST, 'active');

  if (resource.animal <> nil) then
  begin
    index(frtPatient, key, resource.animal.species, 'animal-species');
    index(frtPatient, key, resource.animal.breed, 'animal-breed');
  end;
  patientCompartment(key, 'patient', id);
end;

Const
  CHECK_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of TSearchParamsDiagnosticReport = ( spDiagnosticReport__id, spDiagnosticReport__Language, spDiagnosticReport_Date, spDiagnosticReport_Diagnosis, spDiagnosticReport_Identifier, spDiagnosticReport_Image, spDiagnosticReport_Issued, spDiagnosticReport_Name, spDiagnosticReport_Performer, spDiagnosticReport_Request, spDiagnosticReport_Result, spDiagnosticReport_Service, spDiagnosticReport_Specimen, spDiagnosticReport_Status, spDiagnosticReport_Subject);

procedure TFhirIndexManager.buildIndexesDiagnosticReport;
var
  a : TSearchParamsDiagnosticReport;
begin
  for a := low(TSearchParamsDiagnosticReport) to high(TSearchParamsDiagnosticReport) do
  begin
    assert(CHECK_TSearchParamsDiagnosticReport[a] = a);
    indexes.add(frtDiagnosticReport, CODES_TSearchParamsDiagnosticReport[a], DESC_TSearchParamsDiagnosticReport[a], TYPES_TSearchParamsDiagnosticReport[a], TARGETS_TSearchParamsDiagnosticReport[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesDiagnosticReport(key : integer; id : String; resource: TFhirDiagnosticReport);
{  procedure IndexGroup(group : TFhirDiagnosticReportResults);
  var
    i : integer;
  begin
    index(frtDiagnosticReport, key, group.name, 'group');
    index(frtDiagnosticReport, key, resource, group.specimen, 'specimen');
    for i := 0 to group.resultList.count - 1 do
      index(frtDiagnosticReport, key, resource, group.resultList[i], 'result');
    for i := 0 to group.groupList.count - 1 do
      IndexGroup(group.groupList[i]);
  end;}
var
  i, j, k : integer;
begin
  index(frtDiagnosticReport, key, resource.status, 'status');
  index(frtDiagnosticReport, key, resource.identifier, 'identifier');
  for k := 0 to resource.RequestDetailList.count - 1 do
    index(frtDiagnosticReport, key, resource, resource.requestDetailList[k], 'request');

  index(frtDiagnosticReport, key, resource.name, 'name');
  for j := 0 to resource.resultList.count - 1 do
    index(frtDiagnosticReport, key, resource, resource.resultList[j], 'result');

  index(frtDiagnosticReport, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtDiagnosticReport, key, resource, resource.performer, 'performer');
  index(frtDiagnosticReport, key, resource.issued, 'issued');
  index(frtDiagnosticReport, key, resource.identifier, 'identifier');
  index(frtDiagnosticReport, key, resource.serviceCategory, 'service');
  if resource.diagnostic is TFhirPeriod then
    index(frtDiagnosticReport, key, TFhirPeriod(resource.diagnostic), 'date')
  else
    index(frtDiagnosticReport, key, TFhirDateTime(resource.diagnostic), 'date');

  for i := 0 to resource.specimenList.Count - 1 Do
    index(frtDiagnosticReport, key, resource, resource.specimenList[i], 'specimen');

  for i := 0 to resource.imageList.Count - 1 Do
    index(frtDiagnosticReport, key, resource, resource.imageList[i].link_, 'image');
  for i := 0 to resource.codedDiagnosisList.Count - 1 Do
    index(frtDiagnosticReport, key, resource.codedDiagnosisList[i], 'diagnosis');
end;

Const
  CHECK_TSearchParamsDeviceObservationReport : Array[TSearchParamsDeviceObservationReport] of TSearchParamsDeviceObservationReport = ( spDeviceObservationReport__id,  spDeviceObservationReport__Language,  spDeviceObservationReport_Channel, spDeviceObservationReport_Code, spDeviceObservationReport_Observation, spDeviceObservationReport_Source, spDeviceObservationReport_Subject);

procedure TFhirIndexManager.buildIndexesDeviceObservationReport;
var
  a : TSearchParamsDeviceObservationReport;
begin
  for a := low(TSearchParamsDeviceObservationReport) to high(TSearchParamsDeviceObservationReport) do
  begin
    assert(CHECK_TSearchParamsDeviceObservationReport[a] = a);
    indexes.add(frtDeviceObservationReport, CODES_TSearchParamsDeviceObservationReport[a], DESC_TSearchParamsDeviceObservationReport[a], TYPES_TSearchParamsDeviceObservationReport[a], TARGETS_TSearchParamsDeviceObservationReport[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesDeviceObservationReport(key : integer; id : String; resource: TFhirDeviceObservationReport);
var
  i, j, k : integer;
  vmd : TFhirDeviceObservationReportVirtualDevice;
  chan : TFhirDeviceObservationReportVirtualDeviceChannel;
begin
  index(frtDeviceObservationReport, key, resource, resource.source, 'source');
  index(frtDeviceObservationReport, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);

  for i := 0 to resource.virtualDeviceList.Count - 1 do
  begin
    vmd := resource.virtualDeviceList[i];
    index(frtDeviceObservationReport, key, vmd.code, 'code');
    for j := 0 to vmd.channelList.Count - 1 do
    begin
      chan := vmd.channelList[j];
      index(frtDeviceObservationReport, key, chan.code, 'channel');
      for k := 0 to chan.metricList.Count - 1 do
        index(frtDeviceObservationReport, key, resource, chan.metricList[k].observation, 'observation');
    end;
  end;
end;

Const
  CHECK_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of TSearchParamsDiagnosticOrder = ( spDiagnosticOrder__id, spDiagnosticOrder__Language, spDiagnosticOrder_Actor, spDiagnosticOrder_Bodysite, spDiagnosticOrder_Code, spDiagnosticOrder_Encounter, spDiagnosticOrder_Event_date, spDiagnosticOrder_Event_status, spDiagnosticOrder_Event_status_date, spDiagnosticOrder_Identifier, spDiagnosticOrder_Item_date, spDiagnosticOrder_Item_past_status, spDiagnosticOrder_Item_status, spDiagnosticOrder_Item_status_date, spDiagnosticOrder_Orderer, spDiagnosticOrder_Specimen, spDiagnosticOrder_Status, spDiagnosticOrder_Subject);


procedure TFhirIndexManager.buildIndexesDiagnosticOrder;
var
  a : TSearchParamsDiagnosticOrder;
begin
  for a := low(TSearchParamsDiagnosticOrder) to high(TSearchParamsDiagnosticOrder) do
  begin
    assert(CHECK_TSearchParamsDiagnosticOrder[a] = a);
    indexes.add(frtDiagnosticOrder, CODES_TSearchParamsDiagnosticOrder[a], DESC_TSearchParamsDiagnosticOrder[a], TYPES_TSearchParamsDiagnosticOrder[a], TARGETS_TSearchParamsDiagnosticOrder[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesDiagnosticOrder(key : integer; id : String; resource: TFhirDiagnosticOrder);
var
  i, j, k : integer;
begin
  index(frtDiagnosticOrder, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtDiagnosticOrder, key, resource, resource.orderer, 'orderer');
  index(frtDiagnosticOrder, key, resource, resource.Encounter, 'encounter');
  for i := 0 to resource.specimenList.Count - 1 do
    index(frtDiagnosticOrder, key, resource, resource.specimenList[i], 'specimen');
  index(frtDiagnosticOrder, key, resource.status, 'status');
  for i := 0 to resource.identifierList.Count - 1 do
    index(frtDiagnosticOrder, key, resource.identifierList[i], 'identifier');

  for j := 0 to resource.eventList.count - 1 do
  begin
    index(frtDiagnosticOrder, key, resource, resource.eventList[j].actor, 'actor');
    index(frtDiagnosticOrder, key, resource.eventList[j].status, 'event-status');
    index(frtDiagnosticOrder, key, resource.eventList[j].dateTime, 'event-date');
  end;

  for k := 0 to resource.itemList.count - 1 do
  begin
    index(frtDiagnosticOrder, key, resource.itemList[k].code, 'code');
    for i := 0 to resource.itemList[k].specimenList.Count - 1 do
      index(frtDiagnosticOrder, key, resource, resource.itemList[k].specimenList[i], 'specimen');
    index(frtDiagnosticOrder, key, resource.itemList[k].bodySite, 'bodysite');
    index(frtDiagnosticOrder, key, resource.itemList[k].status, 'item-status');
    for j := 0 to resource.itemList[k].eventList.count - 1 do
    begin
      index(frtDiagnosticOrder, key, resource, resource.itemList[k].eventList[j].actor, 'actor');
      index(frtDiagnosticOrder, key, resource.itemList[k].eventList[j].status, 'item-past-status');
      index(frtDiagnosticOrder, key, resource.itemList[k].eventList[j].dateTime, 'item-date');
    end;
  end;
end;

const
  CHECK_TSearchParamsValueSet : Array[TSearchParamsValueSet] of TSearchParamsValueSet = ( spValueSet__id, spValueSet__Language, spValueSet_Code, spValueSet_Date, spValueSet_Description, spValueSet_Identifier, spValueSet_Name, spValueSet_Publisher, spValueSet_Reference, spValueSet_Status, spValueSet_System, spValueSet_Version);

procedure TFhirIndexManager.buildIndexesValueset;
var
  a : TSearchParamsValueset;
begin
  for a := low(TSearchParamsValueset) to high(TSearchParamsValueset) do
  begin
    assert(CHECK_TSearchParamsValueSet[a] = a);
    indexes.add(frtValueset, CODES_TSearchParamsValueset[a], DESC_TSearchParamsValueset[a], TYPES_TSearchParamsValueset[a], TARGETS_TSearchParamsValueset[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesValueset(key : integer; id : String; resource: TFhirValueset);
  procedure indexConcepts(list : TFhirValueSetDefineConceptList);
  var
    i : integer;
  begin
    for i := 0 to list.Count - 1 do
    begin
      index(frtValueSet, key, list[i].code, 'code');
      indexConcepts(list[i].conceptList);
    end;
  end;
var
  i : integer;
begin
  index(frtValueSet, key, resource.identifier, 'identifier');
  index(frtValueSet, key, resource.version, 'version');
  index(frtValueSet, key, resource.name, 'name');
  index(frtValueSet, key, resource.status, 'status');
  index(frtValueSet, key, resource.date, 'date');
  index(frtValueSet, key, resource.publisher, 'publisher');
  index(frtValueSet, key, resource.description, 'description');
  if (resource.define <> nil) then
  begin
    index(frtValueSet, key, resource.define.system, 'system');
    indexConcepts(resource.define.conceptList);
  end;
  if resource.compose <> nil then
  begin
    for i := 0 to resource.compose.importList.Count - 1 do
      index(frtValueSet, key, resource.compose.importList[i], 'reference');
    for i := 0 to resource.compose.includeList.Count - 1 do
      index(frtValueSet, key, resource.compose.includeList[i].system, 'reference');
    for i := 0 to resource.compose.excludeList.Count - 1 do
      index(frtValueSet, key, resource.compose.excludeList[i].system, 'reference');
  end;
end;

const
  CHECK_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of TSearchParamsConceptMap = ( spConceptMap__id, spConceptMap__Language, spConceptMap_Date, spConceptMap_Dependson, spConceptMap_Description, spConceptMap_Identifier, spConceptMap_Name, spConceptMap_Product, spConceptMap_Publisher, spConceptMap_Source, spConceptMap_Status, spConceptMap_System, spConceptMap_Target, spConceptMap_Version);

procedure TFhirIndexManager.buildIndexesConceptMap;
var
  a : TSearchParamsConceptMap;
begin
  for a := low(TSearchParamsConceptMap) to high(TSearchParamsConceptMap) do
  begin
    assert(CHECK_TSearchParamsConceptMap[a] = a);
    indexes.add(frtConceptMap, CODES_TSearchParamsConceptMap[a], DESC_TSearchParamsConceptMap[a], TYPES_TSearchParamsConceptMap[a], TARGETS_TSearchParamsConceptMap[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesConceptMap(key : integer; id : String; resource: TFhirConceptMap);
var
  i, j, k : integer;
begin
  index(frtConceptMap, key, resource.identifier, 'identifier');
  index(frtConceptMap, key, resource.version, 'version');
  index(frtConceptMap, key, resource.name, 'name');
  index(frtConceptMap, key, resource.status, 'status');
  index(frtConceptMap, key, resource.date, 'date');
  index(frtConceptMap, key, resource.publisher, 'publisher');
  index(frtConceptMap, key, resource.description, 'description');
  index(frtConceptMap, key, resource, resource.source, 'source');
  index(frtConceptMap, key, resource, resource.target, 'target');
  for i := 0 to resource.conceptList.count - 1 do
  begin
    index(frtConceptMap, key, resource.conceptList[i].system, 'system');
    for j := 0 to resource.conceptList[i].dependsOnList.Count - 1 do
      index(frtConceptMap, key, resource.conceptList[i].dependsOnList[j].concept, 'dependson');
    for j := 0 to resource.conceptList[i].mapList.Count - 1 do
    begin
      index(frtConceptMap, key, resource.conceptList[i].mapList[j].system, 'system');
      for k := 0 to resource.conceptList[i].mapList[j].productList.Count - 1 do
        index(frtConceptMap, key, resource.conceptList[i].mapList[j].productList[k].concept, 'dependson');
    end;
  end;
end;

const
  CHECK_TSearchParamsDevice : Array[TSearchParamsDevice] of TSearchParamsDevice = ( spDevice__id, spDevice__Language, spDevice_Identifier, spDevice_Location, spDevice_Manufacturer, spDevice_Model, spDevice_Organization, spDevice_Patient, spDevice_Type, spDevice_Udi);

procedure TFhirIndexManager.buildIndexesDevice;
var
  a : TSearchParamsDevice;
begin
  for a := low(TSearchParamsDevice) to high(TSearchParamsDevice) do
  begin
    assert(CHECK_TSearchParamsDevice[a] = a);
    indexes.add(frtDevice, CODES_TSearchParamsDevice[a], DESC_TSearchParamsDevice[a], TYPES_TSearchParamsDevice[a], TARGETS_TSearchParamsDevice[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesDevice(key : integer; id : String; resource: TFhirDevice);
var
  i : integer;
begin
  for i  := 0 to resource.identifierList.count - 1 do
    index(frtDevice, key, resource.identifierList[i], 'identifier');
  index(frtDevice, key, resource.udi, 'udi');
  index(frtDevice, key, resource, resource.location, 'location');
  index(frtDevice, key, resource.manufacturer, 'manufacturer');
  index(frtDevice, key, resource.model, 'model');
  index(frtDevice, key, resource, resource.owner, 'organization');
  index(frtDevice, key, resource, resource.patient, 'patient');
  index(frtDevice, key, resource.type_, 'type');
  patientCompartment(key, resource.patient);
end;

const
  CHECK_TSearchParamsSecurityEvent : Array[TSearchParamsSecurityEvent] of TSearchParamsSecurityEvent = ( spSecurityEvent__id, spSecurityEvent__Language, spSecurityEvent_Action, spSecurityEvent_Address, spSecurityEvent_Altid, spSecurityEvent_Date, spSecurityEvent_Desc, spSecurityEvent_Identity, spSecurityEvent_Name, spSecurityEvent_Object_type, spSecurityEvent_Patientid, spSecurityEvent_Reference, spSecurityEvent_Site, spSecurityEvent_Source, spSecurityEvent_Subtype, spSecurityEvent_Type, spSecurityEvent_User);

procedure TFhirIndexManager.buildIndexesSecurityEvent;
var
  a : TSearchParamsSecurityEvent;
begin
  for a := low(TSearchParamsSecurityEvent) to high(TSearchParamsSecurityEvent) do
  begin
    assert(CHECK_TSearchParamsSecurityEvent[a] = a);
    indexes.add(frtSecurityEvent, CODES_TSearchParamsSecurityEvent[a], DESC_TSearchParamsSecurityEvent[a], TYPES_TSearchParamsSecurityEvent[a], TARGETS_TSearchParamsSecurityEvent[a]);
  end;
end;


procedure TFhirIndexManager.buildIndexValuesSecurityEvent(key : integer; id : String; resource: TFhirSecurityEvent);
var
  i : integer;
begin
  index(frtSecurityEvent, key, resource.event.type_, 'type');
  index(frtSecurityEvent, key, resource.event.action, 'action');
  index(frtSecurityEvent, key, resource.event.dateTime, 'date');
  for i := 0 to resource.event.subTypeList.count - 1 do
    index(frtSecurityEvent, key, resource.event.subtypeList[i], 'subtype');

  for i := 0 to resource.participantList.count - 1 do
  begin
    index(frtSecurityEvent, key, resource.participantList[i].userId, 'user');
    index(frtSecurityEvent, key, resource.participantList[i].altId, 'altid');
    index(frtSecurityEvent, key, resource.participantList[i].name, 'name');
    if resource.participantList[i].network <> nil then
      index(frtSecurityEvent, key, resource.participantList[i].network.identifier, 'address');
  end;

  if resource.source <> nil Then
  begin
    index(frtSecurityEvent, key, resource.source.identifier, 'source');
    index(frtSecurityEvent, key, resource.source.site, 'site');
  end;

  for i := 0 to resource.object_List.count - 1 do
  begin
    index(frtSecurityEvent, key, resource.object_List[i].type_, 'object-type');
    index(frtSecurityEvent, key, resource.object_List[i].identifier, 'identity');
    index(frtSecurityEvent, key, resource, resource.object_List[i].reference, 'reference');
    patientCompartment(key, resource.object_List[i].reference);
    index(frtSecurityEvent, key, resource.object_List[i].name, 'desc');
  end;
  // todo: spAudit_Patientid
end;

const
  CHECK_TSearchParamsCondition : Array[TSearchParamsCondition] of TSearchParamsCondition = ( spCondition__id, spCondition__Language, spCondition_Asserter, spCondition_Category, spCondition_Code, spCondition_Date_asserted, spCondition_Encounter, spCondition_Evidence, spCondition_Location, spCondition_Onset, spCondition_Related_code, spCondition_Related_item, spCondition_Severity, spCondition_Stage, spCondition_Status, spCondition_Subject);

procedure TFhirIndexManager.buildIndexesCondition;
var
  a : TSearchParamsCondition;
begin
  for a := low(TSearchParamsCondition) to high(TSearchParamsCondition) do
  begin
    assert(CHECK_TSearchParamsCondition[a] = a);        
    indexes.add(frtCondition, CODES_TSearchParamsCondition[a], DESC_TSearchParamsCondition[a], TYPES_TSearchParamsCondition[a], TARGETS_TSearchParamsCondition[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesCondition(key : integer; id : String; resource: TFhirCondition);
var
  i : integer;
begin
  index(frtCondition, key, resource.code, 'code');
  index(frtCondition, key, resource.status, 'status');
  index(frtCondition, key, resource.severity, 'severity');
  index(frtCondition, key, resource.category, 'category');
  index(frtCondition, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtCondition, key, resource, resource.Encounter, 'encounter');
  index(frtCondition, key, resource, resource.asserter, 'asserter');
  for i := 0 to resource.relatedItemList.count - 1 do
  begin
    index(frtCondition, key, resource.relatedItemList[i].code, 'related-code');
    index(frtCondition, key, resource, resource.relatedItemList[i].target, 'related-item');
  end;
  index(frtCondition, key, resource.dateAsserted, 'date-asserted');
// todo  index(frtCondition, key, resource.onset, 'onset');
  for i := 0 to resource.evidenceList.count - 1 do
    index(frtCondition, key, resource.evidenceList[i].code, 'evidence');
  for i := 0 to resource.locationList.count - 1 do
    index(frtCondition, key, resource.locationList[i].code, 'location');
  if resource.stage <> nil then
    index(frtCondition, key, resource.stage.summary, 'stage');
end;

const
  CHECK_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of TSearchParamsOperationOutcome = ( spOperationOutcome__id, spOperationOutcome__Language);

procedure TFhirIndexManager.buildIndexesOperationOutcome;
var
  a : TSearchParamsOperationOutcome;
begin
  for a := low(TSearchParamsOperationOutcome) to high(TSearchParamsOperationOutcome) do
  begin
    assert(CHECK_TSearchParamsOperationOutcome[a] = a);
    indexes.add(frtOperationOutcome, CODES_TSearchParamsOperationOutcome[a], DESC_TSearchParamsOperationOutcome[a], TYPES_TSearchParamsOperationOutcome[a], TARGETS_TSearchParamsOperationOutcome[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesOperationOutcome(key : integer; id : String; resource: TFhirOperationOutcome);
begin
end;


procedure TFhirIndexManager.buildIndexesBinary;
begin
  indexes.add(frtBinary, 'id', 'id', SearchParamTypeToken, []);
end;

procedure TFhirIndexManager.buildIndexValuesBinary(key : integer; id : String; resource: TFhirBinary);
begin
end;

const
  CHECK_TSearchParamsProvenance : Array[TSearchParamsProvenance] of TSearchParamsProvenance = ( spProvenance__id,  spProvenance__Language,  spProvenance_End, spProvenance_Location, spProvenance_Party, spProvenance_Partytype, spProvenance_Start, spProvenance_Target);

procedure TFhirIndexManager.buildIndexesProvenance;
var
  a : TSearchParamsProvenance;
begin
  for a := low(TSearchParamsProvenance) to high(TSearchParamsProvenance) do
  begin
    assert(CHECK_TSearchParamsProvenance[a] = a);
    indexes.add(frtProvenance, CODES_TSearchParamsProvenance[a], DESC_TSearchParamsProvenance[a], TYPES_TSearchParamsProvenance[a], TARGETS_TSearchParamsProvenance[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesProvenance(key : integer; id : String; resource: TFhirProvenance);
var
  i : integer;
begin
  for i := 0 to resource.targetList.Count - 1 do
  index(frtProvenance, key, resource, resource.targetList[i], 'target');
  if (resource.period <> nil) then
  begin
    index(frtProvenance, key, resource.period.start, 'start');
    index(frtProvenance, key, resource.period.end_, 'end');
  end;
  index(frtProvenance, key, resource, resource.location, 'location');

  for i := 0 to resource.entityList.Count - 1 do
  begin
    index(frtProvenance, key, resource.entityList[i].reference, 'party');
    index(frtProvenance, key, resource.entityList[i].type_, 'partytype');
  end;
end;


const
  CHECK_TSearchParamsMedication : Array[TSearchParamsMedication] of TSearchParamsMedication = ( spMedication__id, spMedication__Language, spMedication_Code, spMedication_Container, spMedication_Content, spMedication_Form, spMedication_Ingredient, spMedication_Manufacturer, spMedication_Name);

procedure TFhirIndexManager.buildIndexesMedication;
var
  a : TSearchParamsMedication;
begin
  for a := low(TSearchParamsMedication) to high(TSearchParamsMedication) do
  begin
    assert(CHECK_TSearchParamsMedication[a] = a);
    indexes.add(frtMedication, CODES_TSearchParamsMedication[a], DESC_TSearchParamsMedication[a], TYPES_TSearchParamsMedication[a], TARGETS_TSearchParamsMedication[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesMedication(key : integer; id : String; resource: TFhirMedication);
var
  i : integer;
begin
  index(frtMedication, key, resource.code, 'code');
  index(frtMedication, key, resource.name, 'name');
  index(frtMedication, key, resource, resource.manufacturer, 'manufacturer');
  if (resource.package <> nil) then
  begin
    index(frtMedication, key, resource.package.container, 'container');
    for i := 0 to resource.package.contentList.count - 1 do
      index(frtMedication, key, resource, resource.package.contentList[i].item, 'content');
  end;
  if (resource.product <> nil) then
  begin
    index(frtMedication, key, resource.product.form, 'form');
    for i := 0 to resource.product.ingredientList.count - 1 do
      index(frtMedication, key, resource, resource.product.ingredientList[i].item, 'ingredient');
  end;
end;


const
  CHECK_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of TSearchParamsMedicationAdministration = ( spMedicationAdministration__id, spMedicationAdministration__Language, spMedicationAdministration_Device, spMedicationAdministration_Encounter, spMedicationAdministration_Identifier, spMedicationAdministration_Medication, spMedicationAdministration_Notgiven, spMedicationAdministration_Patient, spMedicationAdministration_Prescription, spMedicationAdministration_Status, spMedicationAdministration_Whengiven);


procedure TFhirIndexManager.buildIndexesMedicationAdministration;
var
  a : TSearchParamsMedicationAdministration;
begin
  for a := low(TSearchParamsMedicationAdministration) to high(TSearchParamsMedicationAdministration) do
  begin
    assert(CHECK_TSearchParamsMedicationAdministration[a] = a);
    indexes.add(frtMedicationAdministration, CODES_TSearchParamsMedicationAdministration[a], DESC_TSearchParamsMedicationAdministration[a], TYPES_TSearchParamsMedicationAdministration[a], TARGETS_TSearchParamsMedicationAdministration[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesMedicationAdministration(key : integer; id : String; resource: TFhirMedicationAdministration);
var
  i : integer;
begin
  index(frtMedicationAdministration, key, resource, resource.patient, 'patient');
  patientCompartment(key, resource.patient);
  index(frtMedicationAdministration, key, resource, resource.Encounter, 'encounter');
  index(frtMedicationAdministration, key, resource, resource.prescription, 'prescription');
  index(frtMedicationAdministration, key, resource.wasNotGiven, 'notgiven');
  index(frtMedicationAdministration, key, resource.whenGiven, 'whengiven');
  index(frtMedicationAdministration, key, resource.status, 'status');
  index(frtMedicationAdministration, key, resource, resource.medication, 'medication');
  for i := 0 to resource.identifierList.Count - 1 do
    index(frtMedicationAdministration, key, resource.identifierList[i], 'identifier');
  if resource.Encounter <> nil then
    index(frtMedicationAdministration, key, resource, resource.Encounter, 'Encounter');
  for i := 0 to resource.deviceList.Count - 1 do
    index(frtMedicationAdministration, key, resource, resource.deviceList[i], 'device');
end;

const
  CHECK_TSearchParamsMedicationPrescription : Array[TSearchParamsMedicationPrescription] of TSearchParamsMedicationPrescription = ( spMedicationPrescription__id, spMedicationPrescription__Language, spMedicationPrescription_Datewritten, spMedicationPrescription_Encounter, spMedicationPrescription_Identifier, spMedicationPrescription_Medication, spMedicationPrescription_Patient, spMedicationPrescription_Status);

procedure TFhirIndexManager.buildIndexesMedicationPrescription;
var
  a : TSearchParamsMedicationPrescription;
begin
  for a := low(TSearchParamsMedicationPrescription) to high(TSearchParamsMedicationPrescription) do
  begin
    assert(CHECK_TSearchParamsMedicationPrescription[a] = a);
    indexes.add(frtMedicationPrescription, CODES_TSearchParamsMedicationPrescription[a], DESC_TSearchParamsMedicationPrescription[a], TYPES_TSearchParamsMedicationPrescription[a], TARGETS_TSearchParamsMedicationPrescription[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesMedicationPrescription(key : integer; id : String; resource: TFhirMedicationPrescription);
var
  i : integer;
begin
  index(frtMedicationPrescription, key, resource.status, 'status');
  index(frtMedicationPrescription, key, resource, resource.patient, 'patient');
  patientCompartment(key, resource.patient);
  index(frtMedicationPrescription, key, resource, resource.Encounter, 'encounter');
  index(frtMedicationPrescription, key, resource, resource.medication, 'medication');
  for i := 0 to resource.identifierList.Count - 1 do
    index(frtMedicationPrescription, key, resource.identifierList[i], 'identifier');
  index(frtMedicationPrescription, key, resource.dateWritten, 'datewritten');
end;

const
  CHECK_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of TSearchParamsMedicationDispense = ( spMedicationDispense__id, spMedicationDispense__Language, spMedicationDispense_Destination, spMedicationDispense_Dispenser, spMedicationDispense_Identifier, spMedicationDispense_Medication, spMedicationDispense_Patient, spMedicationDispense_Prescription, spMedicationDispense_ResponsibleParty, spMedicationDispense_Status, spMedicationDispense_Type, spMedicationDispense_WhenHandedOver, spMedicationDispense_WhenPrepared);

procedure TFhirIndexManager.buildIndexesMedicationDispense;
var
  a : TSearchParamsMedicationDispense;
begin
  for a := low(TSearchParamsMedicationDispense) to high(TSearchParamsMedicationDispense) do
  begin
    assert(CHECK_TSearchParamsMedicationDispense[a] = a);
    indexes.add(frtMedicationDispense, CODES_TSearchParamsMedicationDispense[a], DESC_TSearchParamsMedicationDispense[a], TYPES_TSearchParamsMedicationDispense[a], TARGETS_TSearchParamsMedicationDispense[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesMedicationDispense(key : integer; id : String; resource: TFhirMedicationDispense);
var
  i, j : integer;
begin
  index(frtMedicationDispense, key, resource.status, 'status');
  index(frtMedicationDispense, key, resource, resource.patient, 'patient');
  patientCompartment(key, resource.patient);
  index(frtMedicationDispense, key, resource, resource.dispenser, 'dispenser');
  index(frtMedicationDispense, key, resource.identifier, 'identifier');
  for i := 0 to resource.authorizingPrescriptionList.Count - 1 do
    index(frtMedicationDispense, key, resource, resource.authorizingPrescriptionList[i], 'prescription');
  for j := 0 to resource.dispenseList.count - 1 do
  begin
    index(frtMedicationDispense, key, resource.dispenseList[j].identifier, 'identifier');
    index(frtMedicationDispense, key, resource, resource.dispenseList[j].destination, 'destination');
    index(frtMedicationDispense, key, resource, resource.dispenseList[j].medication, 'medication');
    index(frtMedicationDispense, key, resource.dispenseList[j].type_, 'type');
    index(frtMedicationDispense, key, resource.dispenseList[j].whenPrepared, 'whenPrepared');
    index(frtMedicationDispense, key, resource.dispenseList[j].whenHandedOver, 'whenHandedOver');
  end;
  if resource.substitution <> nil then
  begin
    for i := 0 to resource.substitution.responsiblePartyList.count - 1 do
      index(frtMedicationDispense, key, resource, resource.substitution.responsiblePartyList[i], 'dispenser');
  end;
end;

const
  CHECK_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of TSearchParamsMedicationStatement = ( spMedicationStatement__id, spMedicationStatement__Language, spMedicationStatement_Device, spMedicationStatement_Identifier, spMedicationStatement_Medication, spMedicationStatement_Patient, spMedicationStatement_When_given);

procedure TFhirIndexManager.buildIndexesMedicationStatement;
var
  a : TSearchParamsMedicationStatement;
begin
  for a := low(TSearchParamsMedicationStatement) to high(TSearchParamsMedicationStatement) do
  begin
    assert(CHECK_TSearchParamsMedicationStatement[a] = a);
    indexes.add(frtMedicationStatement, CODES_TSearchParamsMedicationStatement[a], DESC_TSearchParamsMedicationStatement[a], TYPES_TSearchParamsMedicationStatement[a], TARGETS_TSearchParamsMedicationStatement[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesMedicationStatement(key : integer; id : String; resource: TFhirMedicationStatement);
var
  i : integer;
begin
  for i := 0 to resource.identifierList.Count - 1 do
    index(frtMedicationStatement, key, resource.identifierList[i], 'identifier');
  index(frtMedicationStatement, key, resource, resource.medication, 'medication');
  index(frtMedicationStatement, key, resource, resource.patient, 'patient');
  patientCompartment(key, resource.patient);
  for i := 0 to resource.deviceList.Count - 1 do
    index(frtMedicationStatement, key, resource, resource.deviceList[i], 'device');
  index(frtMedicationStatement, key, resource.whenGiven, 'when-given');
end;


const
  CHECK_TSearchParamsList : Array[TSearchParamsList] of TSearchParamsList = ( spList__id, spList__Language, spList_Code, spList_Date, spList_Empty_reason, spList_Item, spList_Source, spList_Subject);

procedure TFhirIndexManager.buildIndexesList;
var
  a : TSearchParamsList;
begin
  for a := low(TSearchParamsList) to high(TSearchParamsList) do
  begin
    assert(CHECK_TSearchParamsList[a] = a);
    indexes.add(frtList, CODES_TSearchParamsList[a], DESC_TSearchParamsList[a], TYPES_TSearchParamsList[a], TARGETS_TSearchParamsList[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesList(key : integer; id : String; resource: TFhirList);
var
  i : integer;
begin
  index(frtList, key, resource, resource.source, 'source');
  for i := 0 to resource.entryList.count - 1 do
    index(frtList, key, resource, resource.entryList[i].item, 'item');
  index(frtList, key, resource.emptyReason, 'empty-reason');
  index(frtList, key, resource.date, 'date');
  index(frtList, key, resource.code, 'code');
  index(frtList, key, resource, resource.subject, 'subject');
end;


const
  CHECK_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of TSearchParamsCarePlan = ( spCarePlan__id,  spCarePlan__Language,  spCarePlan_Activitycode, spCarePlan_Activitydate, spCarePlan_Activitydetail, spCarePlan_Condition, spCarePlan_Date, spCarePlan_Participant, spCarePlan_Patient);

procedure TFhirIndexManager.buildIndexesCarePlan;
var
  a : TSearchParamsCarePlan;
begin
  for a := low(TSearchParamsCarePlan) to high(TSearchParamsCarePlan) do
  begin
    assert(CHECK_TSearchParamsCarePlan[a] = a);
    indexes.add(frtCarePlan, CODES_TSearchParamsCarePlan[a], DESC_TSearchParamsCarePlan[a], TYPES_TSearchParamsCarePlan[a], TARGETS_TSearchParamsCarePlan[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesCarePlan(key: integer; id : String; resource: TFhirCarePlan);
var
  i : integer;
begin
  index(frtCareplan, key, resource, resource.patient, 'patient');
  patientCompartment(key, resource.patient);
  for i := 0 to resource.concernList.Count - 1 do
    index(frtCareplan, key, resource, resource.concernList[i], 'condition');
  index(frtCareplan, key, resource.period, 'date');
  for i := 0 to resource.participantList.Count - 1 do
    index(frtCareplan, key, resource, resource.participantList[i].member, 'participant');
  for i := 0 to resource.activityList.Count - 1 do
    if resource.activityList[i].simple <> nil then
    begin
      index(frtCareplan, key, resource.activityList[i].simple.code, 'activitycode');
      index(frtCareplan, key, resource, resource.activityList[i].detail, 'activitydetail');
      if (resource.activityList[i].simple.timing is TFhirSchedule) then
        index(frtCareplan, key, TFhirSchedule(resource.activityList[i].simple.timing), 'activitydate')
      else if (resource.activityList[i].simple.timing is TFhirPeriod) then
        index(frtCareplan, key, TFhirPeriod(resource.activityList[i].simple.timing), 'activitydate');
    end;
end;


const
  CHECK_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of TSearchParamsImagingStudy = ( spImagingStudy__id,  spImagingStudy__Language,  spImagingStudy_Accession,  spImagingStudy_Bodysite,  spImagingStudy_Date,  spImagingStudy_Dicom_Class,  spImagingStudy_Modality,  spImagingStudy_Series,  spImagingStudy_Size,  spImagingStudy_Study,  spImagingStudy_Subject,  spImagingStudy_Uid);

procedure TFhirIndexManager.buildIndexesImagingStudy;
var
  a : TSearchParamsImagingStudy;
begin
  for a := low(TSearchParamsImagingStudy) to high(TSearchParamsImagingStudy) do
  begin
    assert(CHECK_TSearchParamsImagingStudy[a] = a);
    indexes.add(frtImagingStudy, CODES_TSearchParamsImagingStudy[a], DESC_TSearchParamsImagingStudy[a], TYPES_TSearchParamsImagingStudy[a], TARGETS_TSearchParamsImagingStudy[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesImagingStudy(key: integer; id : String; resource: TFhirImagingStudy);
var
  i, j : integer;
  series : TFhirImagingStudySeries;
  image : TFhirImagingStudySeriesInstance;
begin
  index(frtImagingStudy, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtImagingStudy, key, resource.dateTime, 'date');
  index(frtImagingStudy, key, resource.accessionNo, 'accession');
  index(frtImagingStudy, key, resource.uid, 'study');
  for i := 0 to resource.seriesList.count -1 do
  begin
    series := resource.seriesList[i];
    index(frtImagingStudy, key, series.uid, 'series');
    index(frtImagingStudy, key, series.Modality, 'modality');
//  index(frtImagingStudy, key, resource.size, 'size');
    index(frtImagingStudy, key, series.bodySite, 'bodySite');
    for j := 0 to series.instanceList.count - 1 do
    begin
      image := series.instanceList[j];
      index(frtImagingStudy, key, image.uid, 'uid');
      index(frtImagingStudy, key, image.sopClass, 'dicom-class');
    end;
  end;
end;

const
  CHECK_TSearchParamsImmunization : Array[TSearchParamsImmunization] of TSearchParamsImmunization = ( spImmunization__id, spImmunization__Language, spImmunization_Date, spImmunization_Dose_sequence, spImmunization_Identifier, spImmunization_Location, spImmunization_Lot_number, spImmunization_Manufacturer, spImmunization_Performer, spImmunization_Reaction, spImmunization_Reaction_date, spImmunization_Reason, spImmunization_Refusal_reason, spImmunization_Refused, spImmunization_Requester, spImmunization_Subject, spImmunization_Vaccine_type);


procedure TFhirIndexManager.buildIndexesImmunization;
var
  a : TSearchParamsImmunization;
begin
  for a := low(TSearchParamsImmunization) to high(TSearchParamsImmunization) do
  begin
    assert(CHECK_TSearchParamsImmunization[a] = a);
    indexes.add(frtImmunization, CODES_TSearchParamsImmunization[a], DESC_TSearchParamsImmunization[a], TYPES_TSearchParamsImmunization[a], TARGETS_TSearchParamsImmunization[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesImmunization(key: integer; id : String; resource: TFhirImmunization);
var
  i : integer;
begin
  index(frtImmunization, key, resource.vaccineType, 'vaccine-type');
  index(frtImmunization, key, resource.date, 'date');
  if resource.explanation <> nil then
  begin
    for i := 0 to resource.explanation.refusalReasonList.count - 1 do
      index(frtImmunization, key, resource.explanation.refusalReasonList[i], 'refusal-reason');
    for i := 0 to resource.explanation.reasonList.count - 1 do
      index(frtImmunization, key, resource.explanation.reasonList[i], 'reason');
  end;
  for i := 0 to resource.identifierList.count - 1 do
      index(frtImmunization, key, resource.identifierList[i], 'identifier');
  index(frtImmunization, key, resource.lotNumber, 'lot-number');
  index(frtImmunization, key, resource.refusedIndicator, 'refused');
  index(frtImmunization, key, resource, resource.manufacturer, 'manufacturer');
  index(frtImmunization, key, resource, resource.location, 'location');
  index(frtImmunization, key, resource, resource.performer, 'performer');
  index(frtImmunization, key, resource, resource.requester, 'requester');
  index(frtImmunization, key, resource, resource.subject, 'subject');
  for i := 0 to resource.reactionList.count - 1 do
  begin
    index(frtImmunization, key, resource, resource.reactionList[i].detail, 'reaction');
    index(frtImmunization, key, resource.reactionList[i].date, 'reaction-date');
  end;
  for i := 0 to resource.vaccinationProtocolList.count - 1 do
    index(frtImmunization, key, resource.vaccinationProtocolList[i].doseSequence, 'dose-sequence');
  patientCompartment(key, resource.subject);
end;

const
  CHECK_TSearchParamsOrder : Array[TSearchParamsOrder] of TSearchParamsOrder = ( spOrder__id,  spOrder__Language,  spOrder_Authority,  spOrder_Date,  spOrder_Detail,  spOrder_Source,  spOrder_Subject,  spOrder_Target,  spOrder_When,  spOrder_When_code);


procedure TFhirIndexManager.buildIndexesOrder;
var
  a : TSearchParamsOrder;
begin
  for a := low(TSearchParamsOrder) to high(TSearchParamsOrder) do
  begin
    assert(CHECK_TSearchParamsOrder[a] = a);
    indexes.add(frtOrder, CODES_TSearchParamsOrder[a], DESC_TSearchParamsOrder[a], TYPES_TSearchParamsOrder[a], TARGETS_TSearchParamsOrder[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesOrder(key: integer; id : String; resource: TFhirOrder);
var
  i : integer;
begin
  index(frtOrder, key, resource.date, 'date');
  index(frtOrder, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtOrder, key, resource, resource.source, 'source');
  index(frtOrder, key, resource, resource.target, 'target');
  index(frtOrder, key, resource, resource.authority, 'authority');
  if resource.when <> nil then
  begin
    index(frtOrder, key, resource.when.code, 'when_code');
    index(frtOrder, key, resource.when.schedule, 'when');
  end;
  for i := 0 to resource.detailList.count - 1 do
    index(frtOrder, key, resource, resource.detailList[i], 'detail');
end;

const
  CHECK_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of TSearchParamsOrderResponse = ( spOrderResponse__id, spOrderResponse__Language, spOrderResponse_Code, spOrderResponse_Date, spOrderResponse_Fulfillment, spOrderResponse_Request, spOrderResponse_Who);


procedure TFhirIndexManager.buildIndexesOrderResponse;
var
  a : TSearchParamsOrderResponse;
begin
  for a := low(TSearchParamsOrderResponse) to high(TSearchParamsOrderResponse) do
  begin
    assert(CHECK_TSearchParamsOrderResponse[a] = a);
    indexes.add(frtOrderResponse, CODES_TSearchParamsOrderResponse[a], DESC_TSearchParamsOrderResponse[a], TYPES_TSearchParamsOrderResponse[a], TARGETS_TSearchParamsOrderResponse[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesOrderResponse(key: integer; id : String; resource: TFhirOrderResponse);
var
  i : integer;
begin
  index(frtOrderResponse, key, resource, resource.request, 'request');
  index(frtOrderResponse, key, resource.date, 'date');
  index(frtOrderResponse, key, resource, resource.who, 'who');
  index(frtOrderResponse, key, resource.code, 'code');
  for i := 0 to resource.fulfillmentList.count - 1 do
    index(frtOrderResponse, key, resource, resource.fulfillmentList[i], 'fulfillment');
end;

const
  CHECK_TSearchParamsMedia : Array[TSearchParamsMedia] of TSearchParamsMedia = ( spMedia__id,  spMedia__Language,  spMedia_Date, spMedia_Identifier, spMedia_Operator, spMedia_Subject, spMedia_Subtype, spMedia_Type, spMedia_View);


procedure TFhirIndexManager.buildIndexesMedia;
var
  a : TSearchParamsMedia;
begin
  for a := low(TSearchParamsMedia) to high(TSearchParamsMedia) do
  begin
    assert(CHECK_TSearchParamsMedia[a] = a);
    indexes.add(frtMedia, CODES_TSearchParamsMedia[a], DESC_TSearchParamsMedia[a], TYPES_TSearchParamsMedia[a], TARGETS_TSearchParamsMedia[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesMedia(key: integer; id : String; resource: TFhirMedia);
var
  i : integer;
begin
  index(frtMedia, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtMedia, key, resource.dateTime, 'date');
  for i := 0 to resource.identifierList.count - 1 do
    index(frtMedia, key, resource.identifierList[i], 'identifier');
  index(frtMedia, key, resource, resource.operator, 'operator');
  index(frtMedia, key, resource.type_, 'type');
  index(frtMedia, key, resource.subtype, 'subtype');
//  index(frtMedia, key, resource.size, 'size');
  index(frtMedia, key, resource.view, 'view');
end;

const
  CHECK_TSearchParamsFamilyHistory : Array[TSearchParamsFamilyHistory] of TSearchParamsFamilyHistory = ( spFamilyHistory__id,  spFamilyHistory__Language,  spFamilyHistory_Subject);

procedure TFhirIndexManager.buildIndexesFamilyHistory;
var
  a : TSearchParamsFamilyHistory;
begin
  for a := low(TSearchParamsFamilyHistory) to high(TSearchParamsFamilyHistory) do
  begin
    assert(CHECK_TSearchParamsFamilyHistory[a] = a);
    indexes.add(frtFamilyHistory, CODES_TSearchParamsFamilyHistory[a], DESC_TSearchParamsFamilyHistory[a], TYPES_TSearchParamsFamilyHistory[a], TARGETS_TSearchParamsFamilyHistory[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesFamilyHistory(key: integer; id : String; resource: TFhirFamilyHistory);
begin
  index(frtFamilyHistory, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
end;
                                  
const
  CHECK_TSearchParamsProcedure : Array[TSearchParamsProcedure] of TSearchParamsProcedure = ( spProcedure__id,  spProcedure__Language,  spProcedure_Date, spProcedure_Subject, spProcedure_Type);

procedure TFhirIndexManager.buildIndexesProcedure;
var
  a : TSearchParamsProcedure;
begin
  for a := low(TSearchParamsProcedure) to high(TSearchParamsProcedure) do
  begin
    assert(CHECK_TSearchParamsProcedure[a] = a);
    indexes.add(frtProcedure, CODES_TSearchParamsProcedure[a], DESC_TSearchParamsProcedure[a], TYPES_TSearchParamsProcedure[a], TARGETS_TSearchParamsProcedure[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesProcedure(key: integer; id : String; resource: TFhirProcedure);
begin
  index(frtProcedure, key, resource.date, 'date');
  index(frtProcedure, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
  index(frtProcedure, key, resource.type_, 'type');
end;

const
  CHECK_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of TSearchParamsSpecimen = ( spSpecimen__id, spSpecimen__Language, spSpecimen_Subject);

procedure TFhirIndexManager.buildIndexesSpecimen;
var
  a : TSearchParamsSpecimen;
begin
  for a := low(TSearchParamsSpecimen) to high(TSearchParamsSpecimen) do
  begin
    assert(CHECK_TSearchParamsSpecimen[a] = a);
    indexes.add(frtSpecimen, CODES_TSearchParamsSpecimen[a], DESC_TSearchParamsSpecimen[a], TYPES_TSearchParamsSpecimen[a], TARGETS_TSearchParamsSpecimen[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesSpecimen(key: integer; id : String; resource: TFhirSpecimen);
begin
  index(frtSpecimen, key, resource, resource.subject, 'subject');
  patientCompartment(key, resource.subject);
end;

const
  CHECK_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of TSearchParamsImmunizationRecommendation = ( spImmunizationRecommendation__id, spImmunizationRecommendation__Language, spImmunizationRecommendation_Date, spImmunizationRecommendation_Dose_number, spImmunizationRecommendation_Dose_sequence, spImmunizationRecommendation_Identifier, spImmunizationRecommendation_Information, spImmunizationRecommendation_Status, spImmunizationRecommendation_Subject, spImmunizationRecommendation_Support, spImmunizationRecommendation_Vaccine_type);

procedure TFhirIndexManager.buildIndexesImmunizationRecommendation;
var
  a : TSearchParamsImmunizationRecommendation;
begin
  for a := low(TSearchParamsImmunizationRecommendation) to high(TSearchParamsImmunizationRecommendation) do
  begin
    assert(CHECK_TSearchParamsImmunizationRecommendation[a] = a);
    indexes.add(frtImmunizationRecommendation, CODES_TSearchParamsImmunizationRecommendation[a], DESC_TSearchParamsImmunizationRecommendation[a], TYPES_TSearchParamsImmunizationRecommendation[a], TARGETS_TSearchParamsImmunizationRecommendation[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesImmunizationRecommendation(key: integer; id : String; resource: TFhirImmunizationRecommendation);
var
  i,j  : integer;
begin
  patientCompartment(key, resource.subject);

  index(frtImmunizationRecommendation, key, resource, resource.subject, 'subject');
  for i := 0 to resource.identifierList.count - 1 do
    index(frtImmunizationRecommendation, key, resource.identifierList[i], 'identifier');

  for i := 0 to resource.recommendationList.count - 1 do
  begin
    index(frtImmunizationRecommendation, key, resource.recommendationList[i].date, 'date');
    index(frtImmunizationRecommendation, key, resource.recommendationList[i].vaccineType, 'vaccine-type');
    index(frtImmunizationRecommendation, key, resource.recommendationList[i].doseNumber, 'dose-number');
    index(frtImmunizationRecommendation, key, resource.recommendationList[i].forecastStatus, 'status');
    if resource.recommendationList[i].protocol <> nil then
      index(frtImmunizationRecommendation, key, resource.recommendationList[i].protocol.doseSequence, 'dose-sequence');
    for j := 0 to resource.recommendationList[i].supportingPatientInformationList.Count - 1 do
      index(frtImmunizationRecommendation, key, resource, resource.recommendationList[i].supportingPatientInformationList[j], 'information');
    for j := 0 to resource.recommendationList[i].supportingImmunizationList.Count - 1 do
      index(frtImmunizationRecommendation, key, resource, resource.recommendationList[i].supportingImmunizationList[j], 'support');
  end;
end;

(*
Const
  CHECK_TSearchParamsAppointment : Array[TSearchParamsAppointment] of TSearchParamsAppointment = ( spAppointment__id, spAppointment__Language, spAppointment_Date, spAppointment_Partstatus, spAppointment_Status, spAppointment_Subject);

procedure TFhirIndexManager.buildIndexesAppointment;
var
  a : TSearchParamsAppointment;
begin
  for a := low(TSearchParamsAppointment) to high(TSearchParamsAppointment) do
  begin
    assert(CHECK_TSearchParamsAppointment[a] = a);
    indexes.add(frtAppointment, CODES_TSearchParamsAppointment[a], DESC_TSearchParamsAppointment[a], TYPES_TSearchParamsAppointment[a], TARGETS_TSearchParamsAppointment[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesAppointment(key: integer; id : String; resource: TFhirAppointment);
var
  i, j : integer;
begin
  index(frtAppointment, key, resource.start, 'date');
  index(frtAppointment, key, resource.status, 'status');
  for i := 0 to resource.participantList.Count - 1 do
  begin
    index(frtAppointment, key, resource.participantList[i].status, 'partstatus');
    for j := 0 to resource.participantList[i].individualList.Count - 1 do
    begin
      index(frtAppointment, key, resource, resource.participantList[i].individualList[j], 'subject');
//      if resource.participantList[i].individualList[j] is patient... then
//        patientCompartment(key, resource.subject);
    end;
  end;
end;


Const
  CHECK_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of TSearchParamsAppointmentResponse = ( spAppointmentResponse__id, spAppointmentResponse__Language, spAppointmentResponse_Appointment, spAppointmentResponse_Partstatus, spAppointmentResponse_Subject);


procedure TFhirIndexManager.buildIndexesAppointmentResponse;
var
  a : TSearchParamsAppointmentResponse;
begin
  for a := low(TSearchParamsAppointmentResponse) to high(TSearchParamsAppointmentResponse) do
  begin
    assert(CHECK_TSearchParamsAppointmentResponse[a] = a);
    indexes.add(frtAppointmentResponse, CODES_TSearchParamsAppointmentResponse[a], DESC_TSearchParamsAppointmentResponse[a], TYPES_TSearchParamsAppointmentResponse[a], TARGETS_TSearchParamsAppointmentResponse[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesAppointmentResponse(key: integer; id : String; resource: TFhirAppointmentResponse);
var
  i : integer;
begin
  index(frtAppointmentResponse, key, resource, resource.appointment, 'appointment');
  index(frtAppointmentResponse, key, resource.participantStatus, 'partstatus');
  for i := 0 to resource.individualList.Count - 1 do
  begin
    index(frtAppointmentResponse, key, resource, resource.individualList[i], 'subject');
    patientCompartment(key, resource.individualList[i]);
  end;
end;

Const
  CHECK_TSearchParamsSlot : Array[TSearchParamsSlot] of TSearchParamsSlot = ( spSlot__id, spSlot__Language, spSlot_Availability, spSlot_Fbtype, spSlot_Slottype, spSlot_Start);

procedure TFhirIndexManager.buildIndexesSlot;
var
  a : TSearchParamsSlot;
begin
  for a := low(TSearchParamsSlot) to high(TSearchParamsSlot) do
  begin
    assert(CHECK_TSearchParamsSlot[a] = a);
    indexes.add(frtSlot, CODES_TSearchParamsSlot[a], DESC_TSearchParamsSlot[a], TYPES_TSearchParamsSlot[a], TARGETS_TSearchParamsSlot[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesSlot(key: integer; id : String; resource: TFhirSlot);
begin
  index(frtSlot, key, resource, resource.availability, 'availability');
  index(frtSlot, key, resource.freeBusyType, 'fbtype');
  index(frtSlot, key, resource.type_, 'slottype');
  index(frtSlot, key, resource.start, 'start');
end;


Const
  CHECK_TSearchParamsAvailability : Array[TSearchParamsAvailability] of TSearchParamsAvailability = ( spAvailability__id, spAvailability__Language, spAvailability_Individual, spAvailability_Slottype);

procedure TFhirIndexManager.buildIndexesAvailability;
var
  a : TSearchParamsAvailability;
begin
  for a := low(TSearchParamsAvailability) to high(TSearchParamsAvailability) do
  begin
    assert(CHECK_TSearchParamsAvailability[a] = a);
    indexes.add(frtAvailability, CODES_TSearchParamsAvailability[a], DESC_TSearchParamsAvailability[a], TYPES_TSearchParamsAvailability[a], TARGETS_TSearchParamsAvailability[a]);
  end;
end;

procedure TFhirIndexManager.buildIndexValuesAvailability(key: integer; id : String; resource: TFhirAvailability);
begin
  index(frtAvailability, key, resource, resource.individual, 'individual');
  index(frtAvailability, key, resource.type_, 'slottype');
end;
*)


{ TFhirCompartmentEntryList }

procedure TFhirCompartmentEntryList.add(key, ckey: integer; id: string);
var
  item : TFhirCompartmentEntry;
begin
  item := TFhirCompartmentEntry.create;
  try
    item.key := key;
    item.ckey := key;
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

initialization
  TFhirIndexManager.Create(nil).free;
end.


