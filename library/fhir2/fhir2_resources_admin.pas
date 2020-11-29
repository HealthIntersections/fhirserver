
unit fhir2_resources_admin;

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
  fhir2_base, fhir2_types, fhir2_resources_base;

Type
{$IFDEF FHIR_DEVICE}
  TFhirDevice = class;
  TFhirDeviceList = class;
{$ENDIF FHIR_DEVICE}
{$IFDEF FHIR_DEVICECOMPONENT}
  TFhirDeviceComponentProductionSpecification = class;
  TFhirDeviceComponentProductionSpecificationList = class;
  TFhirDeviceComponent = class;
  TFhirDeviceComponentList = class;
{$ENDIF FHIR_DEVICECOMPONENT}
{$IFDEF FHIR_DEVICEMETRIC}
  TFhirDeviceMetricCalibration = class;
  TFhirDeviceMetricCalibrationList = class;
  TFhirDeviceMetric = class;
  TFhirDeviceMetricList = class;
{$ENDIF FHIR_DEVICEMETRIC}
{$IFDEF FHIR_ENCOUNTER}
  TFhirEncounterStatusHistory = class;
  TFhirEncounterStatusHistoryList = class;
  TFhirEncounterParticipant = class;
  TFhirEncounterParticipantList = class;
  TFhirEncounterHospitalization = class;
  TFhirEncounterHospitalizationList = class;
  TFhirEncounterLocation = class;
  TFhirEncounterLocationList = class;
  TFhirEncounter = class;
  TFhirEncounterList = class;
{$ENDIF FHIR_ENCOUNTER}
{$IFDEF FHIR_EPISODEOFCARE}
  TFhirEpisodeOfCareStatusHistory = class;
  TFhirEpisodeOfCareStatusHistoryList = class;
  TFhirEpisodeOfCareCareTeam = class;
  TFhirEpisodeOfCareCareTeamList = class;
  TFhirEpisodeOfCare = class;
  TFhirEpisodeOfCareList = class;
{$ENDIF FHIR_EPISODEOFCARE}
{$IFDEF FHIR_GROUP}
  TFhirGroupCharacteristic = class;
  TFhirGroupCharacteristicList = class;
  TFhirGroupMember = class;
  TFhirGroupMemberList = class;
  TFhirGroup = class;
  TFhirGroupList = class;
{$ENDIF FHIR_GROUP}
{$IFDEF FHIR_HEALTHCARESERVICE}
  TFhirHealthcareServiceServiceType = class;
  TFhirHealthcareServiceServiceTypeList = class;
  TFhirHealthcareServiceAvailableTime = class;
  TFhirHealthcareServiceAvailableTimeList = class;
  TFhirHealthcareServiceNotAvailable = class;
  TFhirHealthcareServiceNotAvailableList = class;
  TFhirHealthcareService = class;
  TFhirHealthcareServiceList = class;
{$ENDIF FHIR_HEALTHCARESERVICE}
{$IFDEF FHIR_LOCATION}
  TFhirLocationPosition = class;
  TFhirLocationPositionList = class;
  TFhirLocation = class;
  TFhirLocationList = class;
{$ENDIF FHIR_LOCATION}
{$IFDEF FHIR_ORGANIZATION}
  TFhirOrganizationContact = class;
  TFhirOrganizationContactList = class;
  TFhirOrganization = class;
  TFhirOrganizationList = class;
{$ENDIF FHIR_ORGANIZATION}
{$IFDEF FHIR_PATIENT}
  TFhirPatientContact = class;
  TFhirPatientContactList = class;
  TFhirPatientAnimal = class;
  TFhirPatientAnimalList = class;
  TFhirPatientCommunication = class;
  TFhirPatientCommunicationList = class;
  TFhirPatientLink = class;
  TFhirPatientLinkList = class;
  TFhirPatient = class;
  TFhirPatientList = class;
{$ENDIF FHIR_PATIENT}
{$IFDEF FHIR_PERSON}
  TFhirPersonLink = class;
  TFhirPersonLinkList = class;
  TFhirPerson = class;
  TFhirPersonList = class;
{$ENDIF FHIR_PERSON}
{$IFDEF FHIR_PRACTITIONER}
  TFhirPractitionerPractitionerRole = class;
  TFhirPractitionerPractitionerRoleList = class;
  TFhirPractitionerQualification = class;
  TFhirPractitionerQualificationList = class;
  TFhirPractitioner = class;
  TFhirPractitionerList = class;
{$ENDIF FHIR_PRACTITIONER}
{$IFDEF FHIR_RELATEDPERSON}
  TFhirRelatedPerson = class;
  TFhirRelatedPersonList = class;
{$ENDIF FHIR_RELATEDPERSON}
{$IFDEF FHIR_SCHEDULE}
  TFhirSchedule = class;
  TFhirScheduleList = class;
{$ENDIF FHIR_SCHEDULE}
{$IFDEF FHIR_SLOT}
  TFhirSlot = class;
  TFhirSlotList = class;
{$ENDIF FHIR_SLOT}
{$IFDEF FHIR_SUBSTANCE}
  TFhirSubstanceInstance = class;
  TFhirSubstanceInstanceList = class;
  TFhirSubstanceIngredient = class;
  TFhirSubstanceIngredientList = class;
  TFhirSubstance = class;
  TFhirSubstanceList = class;
{$ENDIF FHIR_SUBSTANCE}

{$IFDEF FHIR_DEVICE}

  // This resource identifies an instance of a manufactured item that is used in the provision of healthcare without being substantially changed through that activity. The device may be a medical or non-medical device.  Medical devices includes durable (reusable) medical equipment, implantable devices, as well as disposable equipment used for diagnostic, treatment, and research for healthcare and public health.  Non-medical devices may include items such as a machine, cellphone, computer, application, etc.
  TFhirDevice = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FType_ : TFhirCodeableConcept;
    FnoteList : TFhirAnnotationList;
    FStatus : TFhirEnum;
    FManufacturer : TFhirString;
    FModel : TFhirString;
    FVersion : TFhirString;
    FManufactureDate : TFhirDateTime;
    FExpiry : TFhirDateTime;
    FUdi : TFhirString;
    FLotNumber : TFhirString;
    FOwner : TFhirReference{TFhirOrganization};
    FLocation : TFhirReference{TFhirLocation};
    FPatient : TFhirReference{TFhirPatient};
    FcontactList : TFhirContactPointList;
    FUrl : TFhirUri;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetType_(value : TFhirCodeableConcept);
    function GetNoteList : TFhirAnnotationList;
    function GetHasNoteList : Boolean;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirDevicestatusEnum;
    Procedure SetStatusST(value : TFhirDevicestatusEnum);
    Procedure SetManufacturer(value : TFhirString);
    Function GetManufacturerST : String;
    Procedure SetManufacturerST(value : String);
    Procedure SetModel(value : TFhirString);
    Function GetModelST : String;
    Procedure SetModelST(value : String);
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetManufactureDate(value : TFhirDateTime);
    Function GetManufactureDateST : TFslDateTime;
    Procedure SetManufactureDateST(value : TFslDateTime);
    Procedure SetExpiry(value : TFhirDateTime);
    Function GetExpiryST : TFslDateTime;
    Procedure SetExpiryST(value : TFslDateTime);
    Procedure SetUdi(value : TFhirString);
    Function GetUdiST : String;
    Procedure SetUdiST(value : String);
    Procedure SetLotNumber(value : TFhirString);
    Function GetLotNumberST : String;
    Procedure SetLotNumberST(value : String);
    Procedure SetOwner(value : TFhirReference{TFhirOrganization});
    Procedure SetLocation(value : TFhirReference{TFhirLocation});
    Procedure SetPatient(value : TFhirReference{TFhirPatient});
    function GetContactList : TFhirContactPointList;
    function GetHasContactList : Boolean;
    Procedure SetUrl(value : TFhirUri);
    Function GetUrlST : String;
    Procedure SetUrlST(value : String);

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirDevice; overload;
    function Clone : TFhirDevice; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Unique instance identifiers assigned to a device by organizations like manufacturers or owners. If the identifier identifies the type of device, Device.type should be used.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // Typed access to Code or identifier to identify a kind of device. (defined for API consistency)
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    // Code or identifier to identify a kind of device.
    property type_Element : TFhirCodeableConcept read FType_ write SetType_;

    // Descriptive information, usage information or implantation information that is not captured in an existing element.
    property noteList : TFhirAnnotationList read GetNoteList;
    property hasNoteList : boolean read GetHasNoteList;

    // Status of the Device availability.
    property status : TFhirDevicestatusEnum read GetStatusST write SetStatusST;
    property statusElement : TFhirEnum read FStatus write SetStatus;

    // Typed access to A name of the manufacturer.
    property manufacturer : String read GetManufacturerST write SetManufacturerST;
    // A name of the manufacturer.
    property manufacturerElement : TFhirString read FManufacturer write SetManufacturer;

    // Typed access to The "model" is an identifier assigned by the manufacturer to identify the product by its type. This number is shared by the all devices sold as the same type.
    property model : String read GetModelST write SetModelST;
    // The "model" is an identifier assigned by the manufacturer to identify the product by its type. This number is shared by the all devices sold as the same type.
    property modelElement : TFhirString read FModel write SetModel;

    // Typed access to The version of the device, if the device has multiple releases under the same model, or if the device is software or carries firmware.
    property version : String read GetVersionST write SetVersionST;
    // The version of the device, if the device has multiple releases under the same model, or if the device is software or carries firmware.
    property versionElement : TFhirString read FVersion write SetVersion;

    // Typed access to The date and time when the device was manufactured.
    property manufactureDate : TFslDateTime read GetManufactureDateST write SetManufactureDateST;
    // The date and time when the device was manufactured.
    property manufactureDateElement : TFhirDateTime read FManufactureDate write SetManufactureDate;

    // Typed access to The date and time beyond which this device is no longer valid or should not be used (if applicable).
    property expiry : TFslDateTime read GetExpiryST write SetExpiryST;
    // The date and time beyond which this device is no longer valid or should not be used (if applicable).
    property expiryElement : TFhirDateTime read FExpiry write SetExpiry;

    // Typed access to United States Food and Drug Administration mandated Unique Device Identifier (UDI). Use the human readable information (the content that the user sees, which is sometimes different to the exact syntax represented in the barcode)  - see http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/UniqueDeviceIdentification/default.htm.
    property udi : String read GetUdiST write SetUdiST;
    // United States Food and Drug Administration mandated Unique Device Identifier (UDI). Use the human readable information (the content that the user sees, which is sometimes different to the exact syntax represented in the barcode)  - see http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/UniqueDeviceIdentification/default.htm.
    property udiElement : TFhirString read FUdi write SetUdi;

    // Typed access to Lot number assigned by the manufacturer.
    property lotNumber : String read GetLotNumberST write SetLotNumberST;
    // Lot number assigned by the manufacturer.
    property lotNumberElement : TFhirString read FLotNumber write SetLotNumber;

    // Typed access to An organization that is responsible for the provision and ongoing maintenance of the device. (defined for API consistency)
    property owner : TFhirReference{TFhirOrganization} read FOwner write SetOwner;
    // An organization that is responsible for the provision and ongoing maintenance of the device.
    property ownerElement : TFhirReference{TFhirOrganization} read FOwner write SetOwner;

    // Typed access to The place where the device can be found. (defined for API consistency)
    property location : TFhirReference{TFhirLocation} read FLocation write SetLocation;
    // The place where the device can be found.
    property locationElement : TFhirReference{TFhirLocation} read FLocation write SetLocation;

    // Typed access to Patient information, if the resource is affixed to a person. (defined for API consistency)
    property patient : TFhirReference{TFhirPatient} read FPatient write SetPatient;
    // Patient information, if the resource is affixed to a person.
    property patientElement : TFhirReference{TFhirPatient} read FPatient write SetPatient;

    // Contact details for an organization or a particular human that is responsible for the device.
    property contactList : TFhirContactPointList read GetContactList;
    property hasContactList : boolean read GetHasContactList;

    // Typed access to A network address on which the device may be contacted directly.
    property url : String read GetUrlST write SetUrlST;
    // A network address on which the device may be contacted directly.
    property urlElement : TFhirUri read FUrl write SetUrl;

  end;

  TFhirDeviceListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDeviceList;
    function GetCurrent : TFhirDevice;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirDeviceList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDevice read GetCurrent;
  end;

  TFhirDeviceList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDevice;
    procedure SetItemN(index : Integer; value : TFhirDevice);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDeviceList; Overload;
    function Clone : TFhirDeviceList; Overload;
    function GetEnumerator : TFhirDeviceListEnumerator;

    //  Add a FhirDevice to the end of the list.
    function Append : TFhirDevice;

    // Add an already existing FhirDevice to the end of the list.
    procedure AddItem(value : TFhirDevice); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDevice) : Integer;

    // Insert FhirDevice before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDevice;

    // Insert an existing FhirDevice before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDevice);

    // Get the iIndexth FhirDevice. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDevice);

    // The number of items in the collection
    function Item(index : Integer) : TFhirDevice;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirDevices[index : Integer] : TFhirDevice read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_DEVICE}

{$IFDEF FHIR_DEVICECOMPONENT}

  // Describes the production specification such as component revision, serial number, etc.
  TFhirDeviceComponentProductionSpecification = class (TFhirBackboneElement)
  protected
    FSpecType : TFhirCodeableConcept;
    FComponentId : TFhirIdentifier;
    FProductionSpec : TFhirString;
    Procedure SetSpecType(value : TFhirCodeableConcept);
    Procedure SetComponentId(value : TFhirIdentifier);
    Procedure SetProductionSpec(value : TFhirString);
    Function GetProductionSpecST : String;
    Procedure SetProductionSpecST(value : String);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirDeviceComponentProductionSpecification; overload;
    function Clone : TFhirDeviceComponentProductionSpecification; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to Describes the specification type, such as, serial number, part number, hardware revision, software revision, etc. (defined for API consistency)
    property specType : TFhirCodeableConcept read FSpecType write SetSpecType;
    // Describes the specification type, such as, serial number, part number, hardware revision, software revision, etc.
    property specTypeElement : TFhirCodeableConcept read FSpecType write SetSpecType;

    // Typed access to Describes the internal component unique identification. This is a provision for manufacture specific standard components using a private OID. 11073-10101 has a partition for private OID semantic that the manufacture can make use of. (defined for API consistency)
    property componentId : TFhirIdentifier read FComponentId write SetComponentId;
    // Describes the internal component unique identification. This is a provision for manufacture specific standard components using a private OID. 11073-10101 has a partition for private OID semantic that the manufacture can make use of.
    property componentIdElement : TFhirIdentifier read FComponentId write SetComponentId;

    // Typed access to Describes the printable string defining the component.
    property productionSpec : String read GetProductionSpecST write SetProductionSpecST;
    // Describes the printable string defining the component.
    property productionSpecElement : TFhirString read FProductionSpec write SetProductionSpec;

  end;

  TFhirDeviceComponentProductionSpecificationListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDeviceComponentProductionSpecificationList;
    function GetCurrent : TFhirDeviceComponentProductionSpecification;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirDeviceComponentProductionSpecificationList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDeviceComponentProductionSpecification read GetCurrent;
  end;

  TFhirDeviceComponentProductionSpecificationList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDeviceComponentProductionSpecification;
    procedure SetItemN(index : Integer; value : TFhirDeviceComponentProductionSpecification);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDeviceComponentProductionSpecificationList; Overload;
    function Clone : TFhirDeviceComponentProductionSpecificationList; Overload;
    function GetEnumerator : TFhirDeviceComponentProductionSpecificationListEnumerator;

    //  Add a FhirDeviceComponentProductionSpecification to the end of the list.
    function Append : TFhirDeviceComponentProductionSpecification;

    // Add an already existing FhirDeviceComponentProductionSpecification to the end of the list.
    procedure AddItem(value : TFhirDeviceComponentProductionSpecification); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDeviceComponentProductionSpecification) : Integer;

    // Insert FhirDeviceComponentProductionSpecification before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDeviceComponentProductionSpecification;

    // Insert an existing FhirDeviceComponentProductionSpecification before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDeviceComponentProductionSpecification);

    // Get the iIndexth FhirDeviceComponentProductionSpecification. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDeviceComponentProductionSpecification);

    // The number of items in the collection
    function Item(index : Integer) : TFhirDeviceComponentProductionSpecification;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirDeviceComponentProductionSpecifications[index : Integer] : TFhirDeviceComponentProductionSpecification read GetItemN write SetItemN; default;
  End;

  // Describes the characteristics, operational status and capabilities of a medical-related component of a medical device.
  TFhirDeviceComponent = class (TFhirDomainResource)
  protected
    FType_ : TFhirCodeableConcept;
    FIdentifier : TFhirIdentifier;
    FLastSystemChange : TFhirInstant;
    FSource : TFhirReference{TFhirDevice};
    FParent : TFhirReference{TFhirDeviceComponent};
    FoperationalStatusList : TFhirCodeableConceptList;
    FParameterGroup : TFhirCodeableConcept;
    FMeasurementPrinciple : TFhirEnum;
    FproductionSpecificationList : TFhirDeviceComponentProductionSpecificationList;
    FLanguageCode : TFhirCodeableConcept;
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetLastSystemChange(value : TFhirInstant);
    Function GetLastSystemChangeST : TFslDateTime;
    Procedure SetLastSystemChangeST(value : TFslDateTime);
    Procedure SetSource(value : TFhirReference{TFhirDevice});
    Procedure SetParent(value : TFhirReference{TFhirDeviceComponent});
    function GetOperationalStatusList : TFhirCodeableConceptList;
    function GetHasOperationalStatusList : Boolean;
    Procedure SetParameterGroup(value : TFhirCodeableConcept);
    Procedure SetMeasurementPrinciple(value : TFhirEnum);
    Function GetMeasurementPrincipleST : TFhirMeasurementPrincipleEnum;
    Procedure SetMeasurementPrincipleST(value : TFhirMeasurementPrincipleEnum);
    function GetProductionSpecificationList : TFhirDeviceComponentProductionSpecificationList;
    function GetHasProductionSpecificationList : Boolean;
    Procedure SetLanguageCode(value : TFhirCodeableConcept);

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirDeviceComponent; overload;
    function Clone : TFhirDeviceComponent; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to Describes the specific component type as defined in the object-oriented or metric nomenclature partition. (defined for API consistency)
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    // Describes the specific component type as defined in the object-oriented or metric nomenclature partition.
    property type_Element : TFhirCodeableConcept read FType_ write SetType_;

    // Typed access to Describes the local assigned unique identification by the software. For example: handle ID. (defined for API consistency)
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    // Describes the local assigned unique identification by the software. For example: handle ID.
    property identifierElement : TFhirIdentifier read FIdentifier write SetIdentifier;

    // Typed access to Describes the timestamp for the most recent system change which includes device configuration or setting change.
    property lastSystemChange : TFslDateTime read GetLastSystemChangeST write SetLastSystemChangeST;
    // Describes the timestamp for the most recent system change which includes device configuration or setting change.
    property lastSystemChangeElement : TFhirInstant read FLastSystemChange write SetLastSystemChange;

    // Typed access to Describes the link to the source Device that contains administrative device information such as manufacture, serial number, etc. (defined for API consistency)
    property source : TFhirReference{TFhirDevice} read FSource write SetSource;
    // Describes the link to the source Device that contains administrative device information such as manufacture, serial number, etc.
    property sourceElement : TFhirReference{TFhirDevice} read FSource write SetSource;

    // Typed access to Describes the link to the parent resource. For example: Channel is linked to its VMD parent. (defined for API consistency)
    property parent : TFhirReference{TFhirDeviceComponent} read FParent write SetParent;
    // Describes the link to the parent resource. For example: Channel is linked to its VMD parent.
    property parentElement : TFhirReference{TFhirDeviceComponent} read FParent write SetParent;

    // Indicates current operational status of the device. For example: On, Off, Standby, etc.
    property operationalStatusList : TFhirCodeableConceptList read GetOperationalStatusList;
    property hasOperationalStatusList : boolean read GetHasOperationalStatusList;

    // Typed access to Describes the parameter group supported by the current device component that is based on some nomenclature, e.g. cardiovascular. (defined for API consistency)
    property parameterGroup : TFhirCodeableConcept read FParameterGroup write SetParameterGroup;
    // Describes the parameter group supported by the current device component that is based on some nomenclature, e.g. cardiovascular.
    property parameterGroupElement : TFhirCodeableConcept read FParameterGroup write SetParameterGroup;

    // Describes the physical principle of the measurement. For example: thermal, chemical, acoustical, etc.
    property measurementPrinciple : TFhirMeasurementPrincipleEnum read GetMeasurementPrincipleST write SetMeasurementPrincipleST;
    property measurementPrincipleElement : TFhirEnum read FMeasurementPrinciple write SetMeasurementPrinciple;

    // Describes the production specification such as component revision, serial number, etc.
    property productionSpecificationList : TFhirDeviceComponentProductionSpecificationList read GetProductionSpecificationList;
    property hasProductionSpecificationList : boolean read GetHasProductionSpecificationList;

    // Typed access to Describes the language code for the human-readable text string produced by the device. This language code will follow the IETF language tag. Example: en-US. (defined for API consistency)
    property languageCode : TFhirCodeableConcept read FLanguageCode write SetLanguageCode;
    // Describes the language code for the human-readable text string produced by the device. This language code will follow the IETF language tag. Example: en-US.
    property languageCodeElement : TFhirCodeableConcept read FLanguageCode write SetLanguageCode;

  end;

  TFhirDeviceComponentListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDeviceComponentList;
    function GetCurrent : TFhirDeviceComponent;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirDeviceComponentList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDeviceComponent read GetCurrent;
  end;

  TFhirDeviceComponentList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDeviceComponent;
    procedure SetItemN(index : Integer; value : TFhirDeviceComponent);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDeviceComponentList; Overload;
    function Clone : TFhirDeviceComponentList; Overload;
    function GetEnumerator : TFhirDeviceComponentListEnumerator;

    //  Add a FhirDeviceComponent to the end of the list.
    function Append : TFhirDeviceComponent;

    // Add an already existing FhirDeviceComponent to the end of the list.
    procedure AddItem(value : TFhirDeviceComponent); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDeviceComponent) : Integer;

    // Insert FhirDeviceComponent before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDeviceComponent;

    // Insert an existing FhirDeviceComponent before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDeviceComponent);

    // Get the iIndexth FhirDeviceComponent. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDeviceComponent);

    // The number of items in the collection
    function Item(index : Integer) : TFhirDeviceComponent;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirDeviceComponents[index : Integer] : TFhirDeviceComponent read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_DEVICECOMPONENT}

{$IFDEF FHIR_DEVICEMETRIC}

  // Describes the calibrations that have been performed or that are required to be performed.
  TFhirDeviceMetricCalibration = class (TFhirBackboneElement)
  protected
    FType_ : TFhirEnum;
    FState : TFhirEnum;
    FTime : TFhirInstant;
    Procedure SetType_(value : TFhirEnum);
    Function GetType_ST : TFhirMetricCalibrationTypeEnum;
    Procedure SetType_ST(value : TFhirMetricCalibrationTypeEnum);
    Procedure SetState(value : TFhirEnum);
    Function GetStateST : TFhirMetricCalibrationStateEnum;
    Procedure SetStateST(value : TFhirMetricCalibrationStateEnum);
    Procedure SetTime(value : TFhirInstant);
    Function GetTimeST : TFslDateTime;
    Procedure SetTimeST(value : TFslDateTime);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirDeviceMetricCalibration; overload;
    function Clone : TFhirDeviceMetricCalibration; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Describes the type of the calibration method.
    property type_ : TFhirMetricCalibrationTypeEnum read GetType_ST write SetType_ST;
    property type_Element : TFhirEnum read FType_ write SetType_;

    // Describes the state of the calibration.
    property state : TFhirMetricCalibrationStateEnum read GetStateST write SetStateST;
    property stateElement : TFhirEnum read FState write SetState;

    // Typed access to Describes the time last calibration has been performed.
    property time : TFslDateTime read GetTimeST write SetTimeST;
    // Describes the time last calibration has been performed.
    property timeElement : TFhirInstant read FTime write SetTime;

  end;

  TFhirDeviceMetricCalibrationListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDeviceMetricCalibrationList;
    function GetCurrent : TFhirDeviceMetricCalibration;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirDeviceMetricCalibrationList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDeviceMetricCalibration read GetCurrent;
  end;

  TFhirDeviceMetricCalibrationList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDeviceMetricCalibration;
    procedure SetItemN(index : Integer; value : TFhirDeviceMetricCalibration);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDeviceMetricCalibrationList; Overload;
    function Clone : TFhirDeviceMetricCalibrationList; Overload;
    function GetEnumerator : TFhirDeviceMetricCalibrationListEnumerator;

    //  Add a FhirDeviceMetricCalibration to the end of the list.
    function Append : TFhirDeviceMetricCalibration;

    // Add an already existing FhirDeviceMetricCalibration to the end of the list.
    procedure AddItem(value : TFhirDeviceMetricCalibration); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDeviceMetricCalibration) : Integer;

    // Insert FhirDeviceMetricCalibration before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDeviceMetricCalibration;

    // Insert an existing FhirDeviceMetricCalibration before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDeviceMetricCalibration);

    // Get the iIndexth FhirDeviceMetricCalibration. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDeviceMetricCalibration);

    // The number of items in the collection
    function Item(index : Integer) : TFhirDeviceMetricCalibration;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirDeviceMetricCalibrations[index : Integer] : TFhirDeviceMetricCalibration read GetItemN write SetItemN; default;
  End;

  // Describes a measurement, calculation or setting capability of a medical device.
  TFhirDeviceMetric = class (TFhirDomainResource)
  protected
    FType_ : TFhirCodeableConcept;
    FIdentifier : TFhirIdentifier;
    FUnit_ : TFhirCodeableConcept;
    FSource : TFhirReference{TFhirDevice};
    FParent : TFhirReference{TFhirDeviceComponent};
    FOperationalStatus : TFhirEnum;
    FColor : TFhirEnum;
    FCategory : TFhirEnum;
    FMeasurementPeriod : TFhirTiming;
    FcalibrationList : TFhirDeviceMetricCalibrationList;
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetUnit_(value : TFhirCodeableConcept);
    Procedure SetSource(value : TFhirReference{TFhirDevice});
    Procedure SetParent(value : TFhirReference{TFhirDeviceComponent});
    Procedure SetOperationalStatus(value : TFhirEnum);
    Function GetOperationalStatusST : TFhirMetricOperationalStatusEnum;
    Procedure SetOperationalStatusST(value : TFhirMetricOperationalStatusEnum);
    Procedure SetColor(value : TFhirEnum);
    Function GetColorST : TFhirMetricColorEnum;
    Procedure SetColorST(value : TFhirMetricColorEnum);
    Procedure SetCategory(value : TFhirEnum);
    Function GetCategoryST : TFhirMetricCategoryEnum;
    Procedure SetCategoryST(value : TFhirMetricCategoryEnum);
    Procedure SetMeasurementPeriod(value : TFhirTiming);
    function GetCalibrationList : TFhirDeviceMetricCalibrationList;
    function GetHasCalibrationList : Boolean;

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirDeviceMetric; overload;
    function Clone : TFhirDeviceMetric; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to Describes the type of the metric. For example: Heart Rate, PEEP Setting, etc. (defined for API consistency)
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    // Describes the type of the metric. For example: Heart Rate, PEEP Setting, etc.
    property type_Element : TFhirCodeableConcept read FType_ write SetType_;

    // Typed access to Describes the unique identification of this metric that has been assigned by the device or gateway software. For example: handle ID.  It should be noted that in order to make the identifier unique, the system element of the identifier should be set to the unique identifier of the device. (defined for API consistency)
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    // Describes the unique identification of this metric that has been assigned by the device or gateway software. For example: handle ID.  It should be noted that in order to make the identifier unique, the system element of the identifier should be set to the unique identifier of the device.
    property identifierElement : TFhirIdentifier read FIdentifier write SetIdentifier;

    // Typed access to Describes the unit that an observed value determined for this metric will have. For example: Percent, Seconds, etc. (defined for API consistency)
    property unit_ : TFhirCodeableConcept read FUnit_ write SetUnit_;
    // Describes the unit that an observed value determined for this metric will have. For example: Percent, Seconds, etc.
    property unit_Element : TFhirCodeableConcept read FUnit_ write SetUnit_;

    // Typed access to Describes the link to the  Device that this DeviceMetric belongs to and that contains administrative device information such as manufacture, serial number, etc. (defined for API consistency)
    property source : TFhirReference{TFhirDevice} read FSource write SetSource;
    // Describes the link to the  Device that this DeviceMetric belongs to and that contains administrative device information such as manufacture, serial number, etc.
    property sourceElement : TFhirReference{TFhirDevice} read FSource write SetSource;

    // Typed access to Describes the link to the  DeviceComponent that this DeviceMetric belongs to and that provide information about the location of this DeviceMetric in the containment structure of the parent Device. An example would be a DeviceComponent that represents a Channel. This reference can be used by a client application to distinguish DeviceMetrics that have the same type, but should be interpreted based on their containment location. (defined for API consistency)
    property parent : TFhirReference{TFhirDeviceComponent} read FParent write SetParent;
    // Describes the link to the  DeviceComponent that this DeviceMetric belongs to and that provide information about the location of this DeviceMetric in the containment structure of the parent Device. An example would be a DeviceComponent that represents a Channel. This reference can be used by a client application to distinguish DeviceMetrics that have the same type, but should be interpreted based on their containment location.
    property parentElement : TFhirReference{TFhirDeviceComponent} read FParent write SetParent;

    // Indicates current operational state of the device. For example: On, Off, Standby, etc.
    property operationalStatus : TFhirMetricOperationalStatusEnum read GetOperationalStatusST write SetOperationalStatusST;
    property operationalStatusElement : TFhirEnum read FOperationalStatus write SetOperationalStatus;

    // Describes the color representation for the metric. This is often used to aid clinicians to track and identify parameter types by color. In practice, consider a Patient Monitor that has ECG/HR and Pleth for example; the parameters are displayed in different characteristic colors, such as HR-blue, BP-green, and PR and SpO2- magenta.
    property color : TFhirMetricColorEnum read GetColorST write SetColorST;
    property colorElement : TFhirEnum read FColor write SetColor;

    // Indicates the category of the observation generation process. A DeviceMetric can be for example a setting, measurement, or calculation.
    property category : TFhirMetricCategoryEnum read GetCategoryST write SetCategoryST;
    property categoryElement : TFhirEnum read FCategory write SetCategory;

    // Typed access to Describes the measurement repetition time. This is not necessarily the same as the update period. The measurement repetition time can range from milliseconds up to hours. An example for a measurement repetition time in the range of milliseconds is the sampling rate of an ECG. An example for a measurement repetition time in the range of hours is a NIBP that is triggered automatically every hour. The update period may be different than the measurement repetition time, if the device does not update the {$IFNDEF FPC}published{$ENDIF} observed value with the same frequency as it was measured. (defined for API consistency)
    property measurementPeriod : TFhirTiming read FMeasurementPeriod write SetMeasurementPeriod;
    // Describes the measurement repetition time. This is not necessarily the same as the update period. The measurement repetition time can range from milliseconds up to hours. An example for a measurement repetition time in the range of milliseconds is the sampling rate of an ECG. An example for a measurement repetition time in the range of hours is a NIBP that is triggered automatically every hour. The update period may be different than the measurement repetition time, if the device does not update the {$IFNDEF FPC}published{$ENDIF} observed value with the same frequency as it was measured.
    property measurementPeriodElement : TFhirTiming read FMeasurementPeriod write SetMeasurementPeriod;

    // Describes the calibrations that have been performed or that are required to be performed.
    property calibrationList : TFhirDeviceMetricCalibrationList read GetCalibrationList;
    property hasCalibrationList : boolean read GetHasCalibrationList;

  end;

  TFhirDeviceMetricListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirDeviceMetricList;
    function GetCurrent : TFhirDeviceMetric;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirDeviceMetricList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirDeviceMetric read GetCurrent;
  end;

  TFhirDeviceMetricList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirDeviceMetric;
    procedure SetItemN(index : Integer; value : TFhirDeviceMetric);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirDeviceMetricList; Overload;
    function Clone : TFhirDeviceMetricList; Overload;
    function GetEnumerator : TFhirDeviceMetricListEnumerator;

    //  Add a FhirDeviceMetric to the end of the list.
    function Append : TFhirDeviceMetric;

    // Add an already existing FhirDeviceMetric to the end of the list.
    procedure AddItem(value : TFhirDeviceMetric); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirDeviceMetric) : Integer;

    // Insert FhirDeviceMetric before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirDeviceMetric;

    // Insert an existing FhirDeviceMetric before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirDeviceMetric);

    // Get the iIndexth FhirDeviceMetric. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirDeviceMetric);

    // The number of items in the collection
    function Item(index : Integer) : TFhirDeviceMetric;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirDeviceMetrics[index : Integer] : TFhirDeviceMetric read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_DEVICEMETRIC}

{$IFDEF FHIR_ENCOUNTER}

  // The status history permits the encounter resource to contain the status history without needing to read through the historical versions of the resource, or even have the server store them.
  TFhirEncounterStatusHistory = class (TFhirBackboneElement)
  protected
    FStatus : TFhirEnum;
    FPeriod : TFhirPeriod;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirEncounterStateEnum;
    Procedure SetStatusST(value : TFhirEncounterStateEnum);
    Procedure SetPeriod(value : TFhirPeriod);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirEncounterStatusHistory; overload;
    function Clone : TFhirEncounterStatusHistory; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // planned | arrived | in-progress | onleave | finished | cancelled.
    property status : TFhirEncounterStateEnum read GetStatusST write SetStatusST;
    property statusElement : TFhirEnum read FStatus write SetStatus;

    // Typed access to The time that the episode was in the specified status. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The time that the episode was in the specified status.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;

  TFhirEncounterStatusHistoryListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEncounterStatusHistoryList;
    function GetCurrent : TFhirEncounterStatusHistory;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirEncounterStatusHistoryList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEncounterStatusHistory read GetCurrent;
  end;

  TFhirEncounterStatusHistoryList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirEncounterStatusHistory;
    procedure SetItemN(index : Integer; value : TFhirEncounterStatusHistory);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirEncounterStatusHistoryList; Overload;
    function Clone : TFhirEncounterStatusHistoryList; Overload;
    function GetEnumerator : TFhirEncounterStatusHistoryListEnumerator;

    //  Add a FhirEncounterStatusHistory to the end of the list.
    function Append : TFhirEncounterStatusHistory;

    // Add an already existing FhirEncounterStatusHistory to the end of the list.
    procedure AddItem(value : TFhirEncounterStatusHistory); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEncounterStatusHistory) : Integer;

    // Insert FhirEncounterStatusHistory before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEncounterStatusHistory;

    // Insert an existing FhirEncounterStatusHistory before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEncounterStatusHistory);

    // Get the iIndexth FhirEncounterStatusHistory. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEncounterStatusHistory);

    // The number of items in the collection
    function Item(index : Integer) : TFhirEncounterStatusHistory;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirEncounterStatusHistories[index : Integer] : TFhirEncounterStatusHistory read GetItemN write SetItemN; default;
  End;

  // The?list of?people?responsible for providing the service.
  TFhirEncounterParticipant = class (TFhirBackboneElement)
  protected
    Ftype_List : TFhirCodeableConceptList;
    FPeriod : TFhirPeriod;
    FIndividual : TFhirReference{Resource};
    function GetType_List : TFhirCodeableConceptList;
    function GetHasType_List : Boolean;
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetIndividual(value : TFhirReference{Resource});

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirEncounterParticipant; overload;
    function Clone : TFhirEncounterParticipant; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Role of participant in encounter.
    property type_List : TFhirCodeableConceptList read GetType_List;
    property hasType_List : boolean read GetHasType_List;

    // Typed access to The period of time that the specified participant was present during the encounter. These can overlap or be sub-sets of the overall encounters period. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The period of time that the specified participant was present during the encounter. These can overlap or be sub-sets of the overall encounters period.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

    // Typed access to Persons involved in the encounter other than the patient. (defined for API consistency)
    property individual : TFhirReference{Resource} read FIndividual write SetIndividual;
    // Persons involved in the encounter other than the patient.
    property individualElement : TFhirReference{Resource} read FIndividual write SetIndividual;

  end;

  TFhirEncounterParticipantListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEncounterParticipantList;
    function GetCurrent : TFhirEncounterParticipant;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirEncounterParticipantList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEncounterParticipant read GetCurrent;
  end;

  TFhirEncounterParticipantList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirEncounterParticipant;
    procedure SetItemN(index : Integer; value : TFhirEncounterParticipant);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirEncounterParticipantList; Overload;
    function Clone : TFhirEncounterParticipantList; Overload;
    function GetEnumerator : TFhirEncounterParticipantListEnumerator;

    //  Add a FhirEncounterParticipant to the end of the list.
    function Append : TFhirEncounterParticipant;

    // Add an already existing FhirEncounterParticipant to the end of the list.
    procedure AddItem(value : TFhirEncounterParticipant); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEncounterParticipant) : Integer;

    // Insert FhirEncounterParticipant before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEncounterParticipant;

    // Insert an existing FhirEncounterParticipant before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEncounterParticipant);

    // Get the iIndexth FhirEncounterParticipant. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEncounterParticipant);

    // The number of items in the collection
    function Item(index : Integer) : TFhirEncounterParticipant;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirEncounterParticipants[index : Integer] : TFhirEncounterParticipant read GetItemN write SetItemN; default;
  End;

  // Details about the admission to a healthcare service.
  TFhirEncounterHospitalization = class (TFhirBackboneElement)
  protected
    FPreAdmissionIdentifier : TFhirIdentifier;
    FOrigin : TFhirReference{TFhirLocation};
    FAdmitSource : TFhirCodeableConcept;
    FadmittingDiagnosisList : TFhirReferenceList{TFhirCondition};
    FReAdmission : TFhirCodeableConcept;
    FdietPreferenceList : TFhirCodeableConceptList;
    FspecialCourtesyList : TFhirCodeableConceptList;
    FspecialArrangementList : TFhirCodeableConceptList;
    FDestination : TFhirReference{TFhirLocation};
    FDischargeDisposition : TFhirCodeableConcept;
    FdischargeDiagnosisList : TFhirReferenceList{TFhirCondition};
    Procedure SetPreAdmissionIdentifier(value : TFhirIdentifier);
    Procedure SetOrigin(value : TFhirReference{TFhirLocation});
    Procedure SetAdmitSource(value : TFhirCodeableConcept);
    function GetAdmittingDiagnosisList : TFhirReferenceList{TFhirCondition};
    function GetHasAdmittingDiagnosisList : Boolean;
    Procedure SetReAdmission(value : TFhirCodeableConcept);
    function GetDietPreferenceList : TFhirCodeableConceptList;
    function GetHasDietPreferenceList : Boolean;
    function GetSpecialCourtesyList : TFhirCodeableConceptList;
    function GetHasSpecialCourtesyList : Boolean;
    function GetSpecialArrangementList : TFhirCodeableConceptList;
    function GetHasSpecialArrangementList : Boolean;
    Procedure SetDestination(value : TFhirReference{TFhirLocation});
    Procedure SetDischargeDisposition(value : TFhirCodeableConcept);
    function GetDischargeDiagnosisList : TFhirReferenceList{TFhirCondition};
    function GetHasDischargeDiagnosisList : Boolean;

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirEncounterHospitalization; overload;
    function Clone : TFhirEncounterHospitalization; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to Pre-admission identifier. (defined for API consistency)
    property preAdmissionIdentifier : TFhirIdentifier read FPreAdmissionIdentifier write SetPreAdmissionIdentifier;
    // Pre-admission identifier.
    property preAdmissionIdentifierElement : TFhirIdentifier read FPreAdmissionIdentifier write SetPreAdmissionIdentifier;

    // Typed access to The location from which the patient came before admission. (defined for API consistency)
    property origin : TFhirReference{TFhirLocation} read FOrigin write SetOrigin;
    // The location from which the patient came before admission.
    property originElement : TFhirReference{TFhirLocation} read FOrigin write SetOrigin;

    // Typed access to From where patient was admitted (physician referral, transfer). (defined for API consistency)
    property admitSource : TFhirCodeableConcept read FAdmitSource write SetAdmitSource;
    // From where patient was admitted (physician referral, transfer).
    property admitSourceElement : TFhirCodeableConcept read FAdmitSource write SetAdmitSource;

    // The admitting diagnosis field is used to record the diagnosis codes as reported by admitting practitioner. This could be different or in addition to the conditions reported as reason-condition(s) for the encounter.
    property admittingDiagnosisList : TFhirReferenceList{TFhirCondition} read GetAdmittingDiagnosisList;
    property hasAdmittingDiagnosisList : boolean read GetHasAdmittingDiagnosisList;

    // Typed access to Whether this hospitalization is a readmission and why if known. (defined for API consistency)
    property reAdmission : TFhirCodeableConcept read FReAdmission write SetReAdmission;
    // Whether this hospitalization is a readmission and why if known.
    property reAdmissionElement : TFhirCodeableConcept read FReAdmission write SetReAdmission;

    // Diet preferences reported by the patient.
    property dietPreferenceList : TFhirCodeableConceptList read GetDietPreferenceList;
    property hasDietPreferenceList : boolean read GetHasDietPreferenceList;

    // Special courtesies (VIP, board member).
    property specialCourtesyList : TFhirCodeableConceptList read GetSpecialCourtesyList;
    property hasSpecialCourtesyList : boolean read GetHasSpecialCourtesyList;

    // Wheelchair, translator, stretcher, etc.
    property specialArrangementList : TFhirCodeableConceptList read GetSpecialArrangementList;
    property hasSpecialArrangementList : boolean read GetHasSpecialArrangementList;

    // Typed access to Location to which the patient is discharged. (defined for API consistency)
    property destination : TFhirReference{TFhirLocation} read FDestination write SetDestination;
    // Location to which the patient is discharged.
    property destinationElement : TFhirReference{TFhirLocation} read FDestination write SetDestination;

    // Typed access to Category or kind of location after discharge. (defined for API consistency)
    property dischargeDisposition : TFhirCodeableConcept read FDischargeDisposition write SetDischargeDisposition;
    // Category or kind of location after discharge.
    property dischargeDispositionElement : TFhirCodeableConcept read FDischargeDisposition write SetDischargeDisposition;

    // The final diagnosis given a patient before release from the hospital after all testing, surgery, and workup are complete.
    property dischargeDiagnosisList : TFhirReferenceList{TFhirCondition} read GetDischargeDiagnosisList;
    property hasDischargeDiagnosisList : boolean read GetHasDischargeDiagnosisList;

  end;

  TFhirEncounterHospitalizationListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEncounterHospitalizationList;
    function GetCurrent : TFhirEncounterHospitalization;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirEncounterHospitalizationList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEncounterHospitalization read GetCurrent;
  end;

  TFhirEncounterHospitalizationList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirEncounterHospitalization;
    procedure SetItemN(index : Integer; value : TFhirEncounterHospitalization);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirEncounterHospitalizationList; Overload;
    function Clone : TFhirEncounterHospitalizationList; Overload;
    function GetEnumerator : TFhirEncounterHospitalizationListEnumerator;

    //  Add a FhirEncounterHospitalization to the end of the list.
    function Append : TFhirEncounterHospitalization;

    // Add an already existing FhirEncounterHospitalization to the end of the list.
    procedure AddItem(value : TFhirEncounterHospitalization); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEncounterHospitalization) : Integer;

    // Insert FhirEncounterHospitalization before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEncounterHospitalization;

    // Insert an existing FhirEncounterHospitalization before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEncounterHospitalization);

    // Get the iIndexth FhirEncounterHospitalization. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEncounterHospitalization);

    // The number of items in the collection
    function Item(index : Integer) : TFhirEncounterHospitalization;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirEncounterHospitalizations[index : Integer] : TFhirEncounterHospitalization read GetItemN write SetItemN; default;
  End;

  // List of locations where  the patient has been during this encounter.
  TFhirEncounterLocation = class (TFhirBackboneElement)
  protected
    FLocation : TFhirReference{TFhirLocation};
    FStatus : TFhirEnum;
    FPeriod : TFhirPeriod;
    Procedure SetLocation(value : TFhirReference{TFhirLocation});
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirEncounterLocationStatusEnum;
    Procedure SetStatusST(value : TFhirEncounterLocationStatusEnum);
    Procedure SetPeriod(value : TFhirPeriod);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirEncounterLocation; overload;
    function Clone : TFhirEncounterLocation; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to The location where the encounter takes place. (defined for API consistency)
    property location : TFhirReference{TFhirLocation} read FLocation write SetLocation;
    // The location where the encounter takes place.
    property locationElement : TFhirReference{TFhirLocation} read FLocation write SetLocation;

    // The status of the participants' presence at the specified location during the period specified. If the participant is is no longer at the location, then the period will have an end date/time.
    property status : TFhirEncounterLocationStatusEnum read GetStatusST write SetStatusST;
    property statusElement : TFhirEnum read FStatus write SetStatus;

    // Typed access to Time period during which the patient was present at the location. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // Time period during which the patient was present at the location.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;

  TFhirEncounterLocationListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEncounterLocationList;
    function GetCurrent : TFhirEncounterLocation;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirEncounterLocationList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEncounterLocation read GetCurrent;
  end;

  TFhirEncounterLocationList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirEncounterLocation;
    procedure SetItemN(index : Integer; value : TFhirEncounterLocation);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirEncounterLocationList; Overload;
    function Clone : TFhirEncounterLocationList; Overload;
    function GetEnumerator : TFhirEncounterLocationListEnumerator;

    //  Add a FhirEncounterLocation to the end of the list.
    function Append : TFhirEncounterLocation;

    // Add an already existing FhirEncounterLocation to the end of the list.
    procedure AddItem(value : TFhirEncounterLocation); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEncounterLocation) : Integer;

    // Insert FhirEncounterLocation before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEncounterLocation;

    // Insert an existing FhirEncounterLocation before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEncounterLocation);

    // Get the iIndexth FhirEncounterLocation. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEncounterLocation);

    // The number of items in the collection
    function Item(index : Integer) : TFhirEncounterLocation;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirEncounterLocations[index : Integer] : TFhirEncounterLocation read GetItemN write SetItemN; default;
  End;

  // An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient.
  TFhirEncounter = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FStatus : TFhirEnum;
    FstatusHistoryList : TFhirEncounterStatusHistoryList;
    FClass_ : TFhirEnum;
    Ftype_List : TFhirCodeableConceptList;
    FPriority : TFhirCodeableConcept;
    FPatient : TFhirReference{TFhirPatient};
    FepisodeOfCareList : TFhirReferenceList{TFhirEpisodeOfCare};
    FincomingReferralList : TFhirReferenceList{TFhirReferralRequest};
    FparticipantList : TFhirEncounterParticipantList;
    FAppointment : TFhirReference{TFhirAppointment};
    FPeriod : TFhirPeriod;
    FLength : TFhirQuantity;
    FreasonList : TFhirCodeableConceptList;
    FindicationList : TFhirReferenceList{Resource};
    FHospitalization : TFhirEncounterHospitalization;
    FlocationList : TFhirEncounterLocationList;
    FServiceProvider : TFhirReference{TFhirOrganization};
    FPartOf : TFhirReference{TFhirEncounter};
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirEncounterStateEnum;
    Procedure SetStatusST(value : TFhirEncounterStateEnum);
    function GetStatusHistoryList : TFhirEncounterStatusHistoryList;
    function GetHasStatusHistoryList : Boolean;
    Procedure SetClass_(value : TFhirEnum);
    Function GetClass_ST : TFhirEncounterClassEnum;
    Procedure SetClass_ST(value : TFhirEncounterClassEnum);
    function GetType_List : TFhirCodeableConceptList;
    function GetHasType_List : Boolean;
    Procedure SetPriority(value : TFhirCodeableConcept);
    Procedure SetPatient(value : TFhirReference{TFhirPatient});
    function GetEpisodeOfCareList : TFhirReferenceList{TFhirEpisodeOfCare};
    function GetHasEpisodeOfCareList : Boolean;
    function GetIncomingReferralList : TFhirReferenceList{TFhirReferralRequest};
    function GetHasIncomingReferralList : Boolean;
    function GetParticipantList : TFhirEncounterParticipantList;
    function GetHasParticipantList : Boolean;
    Procedure SetAppointment(value : TFhirReference{TFhirAppointment});
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetLength(value : TFhirQuantity);
    function GetReasonList : TFhirCodeableConceptList;
    function GetHasReasonList : Boolean;
    function GetIndicationList : TFhirReferenceList{Resource};
    function GetHasIndicationList : Boolean;
    Procedure SetHospitalization(value : TFhirEncounterHospitalization);
    function GetLocationList : TFhirEncounterLocationList;
    function GetHasLocationList : Boolean;
    Procedure SetServiceProvider(value : TFhirReference{TFhirOrganization});
    Procedure SetPartOf(value : TFhirReference{TFhirEncounter});

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirEncounter; overload;
    function Clone : TFhirEncounter; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Identifier(s) by which this encounter is known.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // planned | arrived | in-progress | onleave | finished | cancelled.
    property status : TFhirEncounterStateEnum read GetStatusST write SetStatusST;
    property statusElement : TFhirEnum read FStatus write SetStatus;

    // The status history permits the encounter resource to contain the status history without needing to read through the historical versions of the resource, or even have the server store them.
    property statusHistoryList : TFhirEncounterStatusHistoryList read GetStatusHistoryList;
    property hasStatusHistoryList : boolean read GetHasStatusHistoryList;

    // inpatient | outpatient | ambulatory | emergency +.
    property class_ : TFhirEncounterClassEnum read GetClass_ST write SetClass_ST;
    property class_Element : TFhirEnum read FClass_ write SetClass_;

    // Specific type of encounter (e.g. e-mail consultation, surgical day-care, skilled nursing, rehabilitation).
    property type_List : TFhirCodeableConceptList read GetType_List;
    property hasType_List : boolean read GetHasType_List;

    // Typed access to Indicates the urgency of the encounter. (defined for API consistency)
    property priority : TFhirCodeableConcept read FPriority write SetPriority;
    // Indicates the urgency of the encounter.
    property priorityElement : TFhirCodeableConcept read FPriority write SetPriority;

    // Typed access to The patient present at the encounter. (defined for API consistency)
    property patient : TFhirReference{TFhirPatient} read FPatient write SetPatient;
    // The patient present at the encounter.
    property patientElement : TFhirReference{TFhirPatient} read FPatient write SetPatient;

    // Where a specific encounter should be classified as a part of a specific episode(s) of care this field should be used. This association can facilitate grouping of related encounters together for a specific purpose, such as government reporting, issue tracking, association via a common problem.  The association is recorded on the encounter as these are typically created after the episode of care, and grouped on entry rather than editing the episode of care to append another encounter to it (the episode of care could span years).
    property episodeOfCareList : TFhirReferenceList{TFhirEpisodeOfCare} read GetEpisodeOfCareList;
    property hasEpisodeOfCareList : boolean read GetHasEpisodeOfCareList;

    // The referral request this encounter satisfies (incoming referral).
    property incomingReferralList : TFhirReferenceList{TFhirReferralRequest} read GetIncomingReferralList;
    property hasIncomingReferralList : boolean read GetHasIncomingReferralList;

    // The?list of?people?responsible for providing the service.
    property participantList : TFhirEncounterParticipantList read GetParticipantList;
    property hasParticipantList : boolean read GetHasParticipantList;

    // Typed access to The appointment that scheduled this encounter. (defined for API consistency)
    property appointment : TFhirReference{TFhirAppointment} read FAppointment write SetAppointment;
    // The appointment that scheduled this encounter.
    property appointmentElement : TFhirReference{TFhirAppointment} read FAppointment write SetAppointment;

    // Typed access to The start and end time of the encounter. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The start and end time of the encounter.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

    // Typed access to Quantity of time the encounter lasted. This excludes the time during leaves of absence. (defined for API consistency)
    property length : TFhirQuantity read FLength write SetLength;
    // Quantity of time the encounter lasted. This excludes the time during leaves of absence.
    property lengthElement : TFhirQuantity read FLength write SetLength;

    // Reason the encounter takes place, expressed as a code. For admissions, this can be used for a coded admission diagnosis.
    property reasonList : TFhirCodeableConceptList read GetReasonList;
    property hasReasonList : boolean read GetHasReasonList;

    // Reason the encounter takes place, as specified using information from another resource. For admissions, this is the admission diagnosis. The indication will typically be a Condition (with other resources referenced in the evidence.detail), or a Procedure.
    property indicationList : TFhirReferenceList{Resource} read GetIndicationList;
    property hasIndicationList : boolean read GetHasIndicationList;

    // Typed access to Details about the admission to a healthcare service. (defined for API consistency)
    property hospitalization : TFhirEncounterHospitalization read FHospitalization write SetHospitalization;
    // Details about the admission to a healthcare service.
    property hospitalizationElement : TFhirEncounterHospitalization read FHospitalization write SetHospitalization;

    // List of locations where  the patient has been during this encounter.
    property locationList : TFhirEncounterLocationList read GetLocationList;
    property hasLocationList : boolean read GetHasLocationList;

    // Typed access to An organization that is in charge of maintaining the information of this Encounter (e.g. who maintains the report or the master service catalog item, etc.). This MAY be the same as the organization on the Patient record, however it could be different. This MAY not be not the Service Delivery Location's Organization. (defined for API consistency)
    property serviceProvider : TFhirReference{TFhirOrganization} read FServiceProvider write SetServiceProvider;
    // An organization that is in charge of maintaining the information of this Encounter (e.g. who maintains the report or the master service catalog item, etc.). This MAY be the same as the organization on the Patient record, however it could be different. This MAY not be not the Service Delivery Location's Organization.
    property serviceProviderElement : TFhirReference{TFhirOrganization} read FServiceProvider write SetServiceProvider;

    // Typed access to Another Encounter of which this encounter is a part of (administratively or in time). (defined for API consistency)
    property partOf : TFhirReference{TFhirEncounter} read FPartOf write SetPartOf;
    // Another Encounter of which this encounter is a part of (administratively or in time).
    property partOfElement : TFhirReference{TFhirEncounter} read FPartOf write SetPartOf;

  end;

  TFhirEncounterListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEncounterList;
    function GetCurrent : TFhirEncounter;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirEncounterList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEncounter read GetCurrent;
  end;

  TFhirEncounterList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirEncounter;
    procedure SetItemN(index : Integer; value : TFhirEncounter);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirEncounterList; Overload;
    function Clone : TFhirEncounterList; Overload;
    function GetEnumerator : TFhirEncounterListEnumerator;

    //  Add a FhirEncounter to the end of the list.
    function Append : TFhirEncounter;

    // Add an already existing FhirEncounter to the end of the list.
    procedure AddItem(value : TFhirEncounter); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEncounter) : Integer;

    // Insert FhirEncounter before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEncounter;

    // Insert an existing FhirEncounter before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEncounter);

    // Get the iIndexth FhirEncounter. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEncounter);

    // The number of items in the collection
    function Item(index : Integer) : TFhirEncounter;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirEncounters[index : Integer] : TFhirEncounter read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_ENCOUNTER}

{$IFDEF FHIR_EPISODEOFCARE}

  // The history of statuses that the EpisodeOfCare has been through (without requiring processing the history of the resource).
  TFhirEpisodeOfCareStatusHistory = class (TFhirBackboneElement)
  protected
    FStatus : TFhirEnum;
    FPeriod : TFhirPeriod;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirEpisodeOfCareStatusEnum;
    Procedure SetStatusST(value : TFhirEpisodeOfCareStatusEnum);
    Procedure SetPeriod(value : TFhirPeriod);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirEpisodeOfCareStatusHistory; overload;
    function Clone : TFhirEpisodeOfCareStatusHistory; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // planned | waitlist | active | onhold | finished | cancelled.
    property status : TFhirEpisodeOfCareStatusEnum read GetStatusST write SetStatusST;
    property statusElement : TFhirEnum read FStatus write SetStatus;

    // Typed access to The period during this EpisodeOfCare that the specific status applied. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The period during this EpisodeOfCare that the specific status applied.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;

  TFhirEpisodeOfCareStatusHistoryListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEpisodeOfCareStatusHistoryList;
    function GetCurrent : TFhirEpisodeOfCareStatusHistory;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirEpisodeOfCareStatusHistoryList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEpisodeOfCareStatusHistory read GetCurrent;
  end;

  TFhirEpisodeOfCareStatusHistoryList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirEpisodeOfCareStatusHistory;
    procedure SetItemN(index : Integer; value : TFhirEpisodeOfCareStatusHistory);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirEpisodeOfCareStatusHistoryList; Overload;
    function Clone : TFhirEpisodeOfCareStatusHistoryList; Overload;
    function GetEnumerator : TFhirEpisodeOfCareStatusHistoryListEnumerator;

    //  Add a FhirEpisodeOfCareStatusHistory to the end of the list.
    function Append : TFhirEpisodeOfCareStatusHistory;

    // Add an already existing FhirEpisodeOfCareStatusHistory to the end of the list.
    procedure AddItem(value : TFhirEpisodeOfCareStatusHistory); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEpisodeOfCareStatusHistory) : Integer;

    // Insert FhirEpisodeOfCareStatusHistory before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEpisodeOfCareStatusHistory;

    // Insert an existing FhirEpisodeOfCareStatusHistory before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEpisodeOfCareStatusHistory);

    // Get the iIndexth FhirEpisodeOfCareStatusHistory. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEpisodeOfCareStatusHistory);

    // The number of items in the collection
    function Item(index : Integer) : TFhirEpisodeOfCareStatusHistory;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirEpisodeOfCareStatusHistories[index : Integer] : TFhirEpisodeOfCareStatusHistory read GetItemN write SetItemN; default;
  End;

  // The list of practitioners that may be facilitating this episode of care for specific purposes.
  TFhirEpisodeOfCareCareTeam = class (TFhirBackboneElement)
  protected
    FroleList : TFhirCodeableConceptList;
    FPeriod : TFhirPeriod;
    FMember : TFhirReference{Resource};
    function GetRoleList : TFhirCodeableConceptList;
    function GetHasRoleList : Boolean;
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetMember(value : TFhirReference{Resource});

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirEpisodeOfCareCareTeam; overload;
    function Clone : TFhirEpisodeOfCareCareTeam; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // The role this team member is taking within this episode of care.
    property roleList : TFhirCodeableConceptList read GetRoleList;
    property hasRoleList : boolean read GetHasRoleList;

    // Typed access to The period of time this practitioner is performing some role within the episode of care. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The period of time this practitioner is performing some role within the episode of care.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

    // Typed access to The practitioner (or Organization) within the team. (defined for API consistency)
    property member : TFhirReference{Resource} read FMember write SetMember;
    // The practitioner (or Organization) within the team.
    property memberElement : TFhirReference{Resource} read FMember write SetMember;

  end;

  TFhirEpisodeOfCareCareTeamListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEpisodeOfCareCareTeamList;
    function GetCurrent : TFhirEpisodeOfCareCareTeam;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirEpisodeOfCareCareTeamList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEpisodeOfCareCareTeam read GetCurrent;
  end;

  TFhirEpisodeOfCareCareTeamList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirEpisodeOfCareCareTeam;
    procedure SetItemN(index : Integer; value : TFhirEpisodeOfCareCareTeam);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirEpisodeOfCareCareTeamList; Overload;
    function Clone : TFhirEpisodeOfCareCareTeamList; Overload;
    function GetEnumerator : TFhirEpisodeOfCareCareTeamListEnumerator;

    //  Add a FhirEpisodeOfCareCareTeam to the end of the list.
    function Append : TFhirEpisodeOfCareCareTeam;

    // Add an already existing FhirEpisodeOfCareCareTeam to the end of the list.
    procedure AddItem(value : TFhirEpisodeOfCareCareTeam); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEpisodeOfCareCareTeam) : Integer;

    // Insert FhirEpisodeOfCareCareTeam before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEpisodeOfCareCareTeam;

    // Insert an existing FhirEpisodeOfCareCareTeam before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEpisodeOfCareCareTeam);

    // Get the iIndexth FhirEpisodeOfCareCareTeam. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEpisodeOfCareCareTeam);

    // The number of items in the collection
    function Item(index : Integer) : TFhirEpisodeOfCareCareTeam;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirEpisodeOfCareCareTeams[index : Integer] : TFhirEpisodeOfCareCareTeam read GetItemN write SetItemN; default;
  End;

  // An association between a patient and an organization / healthcare provider(s) during which time encounters may occur. The managing organization assumes a level of responsibility for the patient during this time.
  TFhirEpisodeOfCare = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FStatus : TFhirEnum;
    FstatusHistoryList : TFhirEpisodeOfCareStatusHistoryList;
    Ftype_List : TFhirCodeableConceptList;
    FconditionList : TFhirReferenceList{TFhirCondition};
    FPatient : TFhirReference{TFhirPatient};
    FManagingOrganization : TFhirReference{TFhirOrganization};
    FPeriod : TFhirPeriod;
    FreferralRequestList : TFhirReferenceList{TFhirReferralRequest};
    FCareManager : TFhirReference{TFhirPractitioner};
    FcareTeamList : TFhirEpisodeOfCareCareTeamList;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirEpisodeOfCareStatusEnum;
    Procedure SetStatusST(value : TFhirEpisodeOfCareStatusEnum);
    function GetStatusHistoryList : TFhirEpisodeOfCareStatusHistoryList;
    function GetHasStatusHistoryList : Boolean;
    function GetType_List : TFhirCodeableConceptList;
    function GetHasType_List : Boolean;
    function GetConditionList : TFhirReferenceList{TFhirCondition};
    function GetHasConditionList : Boolean;
    Procedure SetPatient(value : TFhirReference{TFhirPatient});
    Procedure SetManagingOrganization(value : TFhirReference{TFhirOrganization});
    Procedure SetPeriod(value : TFhirPeriod);
    function GetReferralRequestList : TFhirReferenceList{TFhirReferralRequest};
    function GetHasReferralRequestList : Boolean;
    Procedure SetCareManager(value : TFhirReference{TFhirPractitioner});
    function GetCareTeamList : TFhirEpisodeOfCareCareTeamList;
    function GetHasCareTeamList : Boolean;

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirEpisodeOfCare; overload;
    function Clone : TFhirEpisodeOfCare; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Identifier(s) by which this EpisodeOfCare is known.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // planned | waitlist | active | onhold | finished | cancelled.
    property status : TFhirEpisodeOfCareStatusEnum read GetStatusST write SetStatusST;
    property statusElement : TFhirEnum read FStatus write SetStatus;

    // The history of statuses that the EpisodeOfCare has been through (without requiring processing the history of the resource).
    property statusHistoryList : TFhirEpisodeOfCareStatusHistoryList read GetStatusHistoryList;
    property hasStatusHistoryList : boolean read GetHasStatusHistoryList;

    // A classification of the type of encounter; e.g. specialist referral, disease management, type of funded care.
    property type_List : TFhirCodeableConceptList read GetType_List;
    property hasType_List : boolean read GetHasType_List;

    // A list of conditions/problems/diagnoses that this episode of care is intended to be providing care for.
    property conditionList : TFhirReferenceList{TFhirCondition} read GetConditionList;
    property hasConditionList : boolean read GetHasConditionList;

    // Typed access to The patient that this EpisodeOfCare applies to. (defined for API consistency)
    property patient : TFhirReference{TFhirPatient} read FPatient write SetPatient;
    // The patient that this EpisodeOfCare applies to.
    property patientElement : TFhirReference{TFhirPatient} read FPatient write SetPatient;

    // Typed access to The organization that has assumed the specific responsibilities for the specified duration. (defined for API consistency)
    property managingOrganization : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;
    // The organization that has assumed the specific responsibilities for the specified duration.
    property managingOrganizationElement : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;

    // Typed access to The interval during which the managing organization assumes the defined responsibility. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The interval during which the managing organization assumes the defined responsibility.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

    // Referral Request(s) that are fulfilled by this EpisodeOfCare, incoming referrals.
    property referralRequestList : TFhirReferenceList{TFhirReferralRequest} read GetReferralRequestList;
    property hasReferralRequestList : boolean read GetHasReferralRequestList;

    // Typed access to The practitioner that is the care manager/care co-ordinator for this patient. (defined for API consistency)
    property careManager : TFhirReference{TFhirPractitioner} read FCareManager write SetCareManager;
    // The practitioner that is the care manager/care co-ordinator for this patient.
    property careManagerElement : TFhirReference{TFhirPractitioner} read FCareManager write SetCareManager;

    // The list of practitioners that may be facilitating this episode of care for specific purposes.
    property careTeamList : TFhirEpisodeOfCareCareTeamList read GetCareTeamList;
    property hasCareTeamList : boolean read GetHasCareTeamList;

  end;

  TFhirEpisodeOfCareListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirEpisodeOfCareList;
    function GetCurrent : TFhirEpisodeOfCare;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirEpisodeOfCareList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirEpisodeOfCare read GetCurrent;
  end;

  TFhirEpisodeOfCareList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirEpisodeOfCare;
    procedure SetItemN(index : Integer; value : TFhirEpisodeOfCare);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirEpisodeOfCareList; Overload;
    function Clone : TFhirEpisodeOfCareList; Overload;
    function GetEnumerator : TFhirEpisodeOfCareListEnumerator;

    //  Add a FhirEpisodeOfCare to the end of the list.
    function Append : TFhirEpisodeOfCare;

    // Add an already existing FhirEpisodeOfCare to the end of the list.
    procedure AddItem(value : TFhirEpisodeOfCare); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirEpisodeOfCare) : Integer;

    // Insert FhirEpisodeOfCare before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirEpisodeOfCare;

    // Insert an existing FhirEpisodeOfCare before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirEpisodeOfCare);

    // Get the iIndexth FhirEpisodeOfCare. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirEpisodeOfCare);

    // The number of items in the collection
    function Item(index : Integer) : TFhirEpisodeOfCare;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirEpisodeOfCares[index : Integer] : TFhirEpisodeOfCare read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_EPISODEOFCARE}

{$IFDEF FHIR_GROUP}

  // Identifies the traits shared by members of the group.
  TFhirGroupCharacteristic = class (TFhirBackboneElement)
  protected
    FCode : TFhirCodeableConcept;
    FValue : TFhirType;
    FExclude : TFhirBoolean;
    FPeriod : TFhirPeriod;
    Procedure SetCode(value : TFhirCodeableConcept);
    Procedure SetValue(value : TFhirType);
    Procedure SetExclude(value : TFhirBoolean);
    Function GetExcludeST : Boolean;
    Procedure SetExcludeST(value : Boolean);
    Procedure SetPeriod(value : TFhirPeriod);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirGroupCharacteristic; overload;
    function Clone : TFhirGroupCharacteristic; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to A code that identifies the kind of trait being asserted. (defined for API consistency)
    property code : TFhirCodeableConcept read FCode write SetCode;
    // A code that identifies the kind of trait being asserted.
    property codeElement : TFhirCodeableConcept read FCode write SetCode;

    // Typed access to The value of the trait that holds (or does not hold - see 'exclude') for members of the group. (defined for API consistency)
    property value : TFhirType read FValue write SetValue;
    // The value of the trait that holds (or does not hold - see 'exclude') for members of the group.
    property valueElement : TFhirType read FValue write SetValue;

    // Typed access to If true, indicates the characteristic is one that is NOT held by members of the group.
    property exclude : Boolean read GetExcludeST write SetExcludeST;
    // If true, indicates the characteristic is one that is NOT held by members of the group.
    property excludeElement : TFhirBoolean read FExclude write SetExclude;

    // Typed access to The period over which the characteristic is tested; e.g. the patient had an operation during the month of June. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The period over which the characteristic is tested; e.g. the patient had an operation during the month of June.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;

  TFhirGroupCharacteristicListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirGroupCharacteristicList;
    function GetCurrent : TFhirGroupCharacteristic;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirGroupCharacteristicList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirGroupCharacteristic read GetCurrent;
  end;

  TFhirGroupCharacteristicList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirGroupCharacteristic;
    procedure SetItemN(index : Integer; value : TFhirGroupCharacteristic);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirGroupCharacteristicList; Overload;
    function Clone : TFhirGroupCharacteristicList; Overload;
    function GetEnumerator : TFhirGroupCharacteristicListEnumerator;

    //  Add a FhirGroupCharacteristic to the end of the list.
    function Append : TFhirGroupCharacteristic;

    // Add an already existing FhirGroupCharacteristic to the end of the list.
    procedure AddItem(value : TFhirGroupCharacteristic); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirGroupCharacteristic) : Integer;

    // Insert FhirGroupCharacteristic before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirGroupCharacteristic;

    // Insert an existing FhirGroupCharacteristic before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirGroupCharacteristic);

    // Get the iIndexth FhirGroupCharacteristic. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirGroupCharacteristic);

    // The number of items in the collection
    function Item(index : Integer) : TFhirGroupCharacteristic;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirGroupCharacteristics[index : Integer] : TFhirGroupCharacteristic read GetItemN write SetItemN; default;
  End;

  // Identifies the resource instances that are members of the group.
  TFhirGroupMember = class (TFhirBackboneElement)
  protected
    FEntity : TFhirReference{Resource};
    FPeriod : TFhirPeriod;
    FInactive : TFhirBoolean;
    Procedure SetEntity(value : TFhirReference{Resource});
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetInactive(value : TFhirBoolean);
    Function GetInactiveST : Boolean;
    Procedure SetInactiveST(value : Boolean);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirGroupMember; overload;
    function Clone : TFhirGroupMember; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to A reference to the entity that is a member of the group. Must be consistent with Group.type. (defined for API consistency)
    property entity : TFhirReference{Resource} read FEntity write SetEntity;
    // A reference to the entity that is a member of the group. Must be consistent with Group.type.
    property entityElement : TFhirReference{Resource} read FEntity write SetEntity;

    // Typed access to The period that the member was in the group, if known. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The period that the member was in the group, if known.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

    // Typed access to A flag to indicate that the member is no longer in the group, but previously may have been a member.
    property inactive : Boolean read GetInactiveST write SetInactiveST;
    // A flag to indicate that the member is no longer in the group, but previously may have been a member.
    property inactiveElement : TFhirBoolean read FInactive write SetInactive;

  end;

  TFhirGroupMemberListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirGroupMemberList;
    function GetCurrent : TFhirGroupMember;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirGroupMemberList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirGroupMember read GetCurrent;
  end;

  TFhirGroupMemberList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirGroupMember;
    procedure SetItemN(index : Integer; value : TFhirGroupMember);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirGroupMemberList; Overload;
    function Clone : TFhirGroupMemberList; Overload;
    function GetEnumerator : TFhirGroupMemberListEnumerator;

    //  Add a FhirGroupMember to the end of the list.
    function Append : TFhirGroupMember;

    // Add an already existing FhirGroupMember to the end of the list.
    procedure AddItem(value : TFhirGroupMember); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirGroupMember) : Integer;

    // Insert FhirGroupMember before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirGroupMember;

    // Insert an existing FhirGroupMember before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirGroupMember);

    // Get the iIndexth FhirGroupMember. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirGroupMember);

    // The number of items in the collection
    function Item(index : Integer) : TFhirGroupMember;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirGroupMembers[index : Integer] : TFhirGroupMember read GetItemN write SetItemN; default;
  End;

  // Represents a defined collection of entities that may be discussed or acted upon collectively but which are not expected to act collectively and are not formally or legally recognized; i.e. a collection of entities that isn't an Organization.
  TFhirGroup = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FType_ : TFhirEnum;
    FActual : TFhirBoolean;
    FCode : TFhirCodeableConcept;
    FName : TFhirString;
    FQuantity : TFhirUnsignedInt;
    FcharacteristicList : TFhirGroupCharacteristicList;
    FmemberList : TFhirGroupMemberList;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetType_(value : TFhirEnum);
    Function GetType_ST : TFhirGroupTypeEnum;
    Procedure SetType_ST(value : TFhirGroupTypeEnum);
    Procedure SetActual(value : TFhirBoolean);
    Function GetActualST : Boolean;
    Procedure SetActualST(value : Boolean);
    Procedure SetCode(value : TFhirCodeableConcept);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetQuantity(value : TFhirUnsignedInt);
    Function GetQuantityST : String;
    Procedure SetQuantityST(value : String);
    function GetCharacteristicList : TFhirGroupCharacteristicList;
    function GetHasCharacteristicList : Boolean;
    function GetMemberList : TFhirGroupMemberList;
    function GetHasMemberList : Boolean;

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirGroup; overload;
    function Clone : TFhirGroup; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // A unique business identifier for this group.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // Identifies the broad classification of the kind of resources the group includes.
    property type_ : TFhirGroupTypeEnum read GetType_ST write SetType_ST;
    property type_Element : TFhirEnum read FType_ write SetType_;

    // Typed access to If true, indicates that the resource refers to a specific group of real individuals.  If false, the group defines a set of intended individuals.
    property actual : Boolean read GetActualST write SetActualST;
    // If true, indicates that the resource refers to a specific group of real individuals.  If false, the group defines a set of intended individuals.
    property actualElement : TFhirBoolean read FActual write SetActual;

    // Typed access to Provides a specific type of resource the group includes; e.g. "cow", "syringe", etc. (defined for API consistency)
    property code : TFhirCodeableConcept read FCode write SetCode;
    // Provides a specific type of resource the group includes; e.g. "cow", "syringe", etc.
    property codeElement : TFhirCodeableConcept read FCode write SetCode;

    // Typed access to A label assigned to the group for human identification and communication.
    property name : String read GetNameST write SetNameST;
    // A label assigned to the group for human identification and communication.
    property nameElement : TFhirString read FName write SetName;

    // Typed access to A count of the number of resource instances that are part of the group.
    property quantity : String read GetQuantityST write SetQuantityST;
    // A count of the number of resource instances that are part of the group.
    property quantityElement : TFhirUnsignedInt read FQuantity write SetQuantity;

    // Identifies the traits shared by members of the group.
    property characteristicList : TFhirGroupCharacteristicList read GetCharacteristicList;
    property hasCharacteristicList : boolean read GetHasCharacteristicList;

    // Identifies the resource instances that are members of the group.
    property memberList : TFhirGroupMemberList read GetMemberList;
    property hasMemberList : boolean read GetHasMemberList;

  end;

  TFhirGroupListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirGroupList;
    function GetCurrent : TFhirGroup;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirGroupList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirGroup read GetCurrent;
  end;

  TFhirGroupList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirGroup;
    procedure SetItemN(index : Integer; value : TFhirGroup);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirGroupList; Overload;
    function Clone : TFhirGroupList; Overload;
    function GetEnumerator : TFhirGroupListEnumerator;

    //  Add a FhirGroup to the end of the list.
    function Append : TFhirGroup;

    // Add an already existing FhirGroup to the end of the list.
    procedure AddItem(value : TFhirGroup); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirGroup) : Integer;

    // Insert FhirGroup before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirGroup;

    // Insert an existing FhirGroup before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirGroup);

    // Get the iIndexth FhirGroup. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirGroup);

    // The number of items in the collection
    function Item(index : Integer) : TFhirGroup;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirGroups[index : Integer] : TFhirGroup read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_GROUP}

{$IFDEF FHIR_HEALTHCARESERVICE}

  // A specific type of service that may be delivered or performed.
  TFhirHealthcareServiceServiceType = class (TFhirBackboneElement)
  protected
    FType_ : TFhirCodeableConcept;
    FspecialtyList : TFhirCodeableConceptList;
    Procedure SetType_(value : TFhirCodeableConcept);
    function GetSpecialtyList : TFhirCodeableConceptList;
    function GetHasSpecialtyList : Boolean;

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirHealthcareServiceServiceType; overload;
    function Clone : TFhirHealthcareServiceServiceType; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to The specific type of service being delivered or performed. (defined for API consistency)
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    // The specific type of service being delivered or performed.
    property type_Element : TFhirCodeableConcept read FType_ write SetType_;

    // Collection of specialties handled by the service site. This is more of a medical term.
    property specialtyList : TFhirCodeableConceptList read GetSpecialtyList;
    property hasSpecialtyList : boolean read GetHasSpecialtyList;

  end;

  TFhirHealthcareServiceServiceTypeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirHealthcareServiceServiceTypeList;
    function GetCurrent : TFhirHealthcareServiceServiceType;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirHealthcareServiceServiceTypeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirHealthcareServiceServiceType read GetCurrent;
  end;

  TFhirHealthcareServiceServiceTypeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirHealthcareServiceServiceType;
    procedure SetItemN(index : Integer; value : TFhirHealthcareServiceServiceType);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirHealthcareServiceServiceTypeList; Overload;
    function Clone : TFhirHealthcareServiceServiceTypeList; Overload;
    function GetEnumerator : TFhirHealthcareServiceServiceTypeListEnumerator;

    //  Add a FhirHealthcareServiceServiceType to the end of the list.
    function Append : TFhirHealthcareServiceServiceType;

    // Add an already existing FhirHealthcareServiceServiceType to the end of the list.
    procedure AddItem(value : TFhirHealthcareServiceServiceType); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirHealthcareServiceServiceType) : Integer;

    // Insert FhirHealthcareServiceServiceType before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirHealthcareServiceServiceType;

    // Insert an existing FhirHealthcareServiceServiceType before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirHealthcareServiceServiceType);

    // Get the iIndexth FhirHealthcareServiceServiceType. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirHealthcareServiceServiceType);

    // The number of items in the collection
    function Item(index : Integer) : TFhirHealthcareServiceServiceType;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirHealthcareServiceServiceTypes[index : Integer] : TFhirHealthcareServiceServiceType read GetItemN write SetItemN; default;
  End;

  // A collection of times that the Service Site is available.
  TFhirHealthcareServiceAvailableTime = class (TFhirBackboneElement)
  protected
    FDaysOfWeek : TFhirEnumList;
    FAllDay : TFhirBoolean;
    FAvailableStartTime : TFhirTime;
    FAvailableEndTime : TFhirTime;
    function GetDaysOfWeek : TFhirEnumList;
    function GetHasDaysOfWeek : Boolean;
    Function GetDaysOfWeekST : TFhirDaysOfWeekEnumList;
    Procedure SetDaysOfWeekST(value : TFhirDaysOfWeekEnumList);
    Procedure SetAllDay(value : TFhirBoolean);
    Function GetAllDayST : Boolean;
    Procedure SetAllDayST(value : Boolean);
    Procedure SetAvailableStartTime(value : TFhirTime);
    Function GetAvailableStartTimeST : String;
    Procedure SetAvailableStartTimeST(value : String);
    Procedure SetAvailableEndTime(value : TFhirTime);
    Function GetAvailableEndTimeST : String;
    Procedure SetAvailableEndTimeST(value : String);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirHealthcareServiceAvailableTime; overload;
    function Clone : TFhirHealthcareServiceAvailableTime; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Indicates which days of the week are available between the start and end Times.
    property daysOfWeek : TFhirDaysOfWeekEnumList read GetDaysOfWeekST write SetDaysOfWeekST;
    property daysOfWeekList : TFhirEnumList read GetDaysOfWeek;
    property hasDaysOfWeek : boolean read GetHasDaysOfWeek;
    // Typed access to Is this always available? (hence times are irrelevant) e.g. 24 hour service.
    property allDay : Boolean read GetAllDayST write SetAllDayST;
    // Is this always available? (hence times are irrelevant) e.g. 24 hour service.
    property allDayElement : TFhirBoolean read FAllDay write SetAllDay;

    // Typed access to The opening time of day. Note: If the AllDay flag is set, then this time is ignored.
    property availableStartTime : String read GetAvailableStartTimeST write SetAvailableStartTimeST;
    // The opening time of day. Note: If the AllDay flag is set, then this time is ignored.
    property availableStartTimeElement : TFhirTime read FAvailableStartTime write SetAvailableStartTime;

    // Typed access to The closing time of day. Note: If the AllDay flag is set, then this time is ignored.
    property availableEndTime : String read GetAvailableEndTimeST write SetAvailableEndTimeST;
    // The closing time of day. Note: If the AllDay flag is set, then this time is ignored.
    property availableEndTimeElement : TFhirTime read FAvailableEndTime write SetAvailableEndTime;

  end;

  TFhirHealthcareServiceAvailableTimeListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirHealthcareServiceAvailableTimeList;
    function GetCurrent : TFhirHealthcareServiceAvailableTime;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirHealthcareServiceAvailableTimeList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirHealthcareServiceAvailableTime read GetCurrent;
  end;

  TFhirHealthcareServiceAvailableTimeList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirHealthcareServiceAvailableTime;
    procedure SetItemN(index : Integer; value : TFhirHealthcareServiceAvailableTime);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirHealthcareServiceAvailableTimeList; Overload;
    function Clone : TFhirHealthcareServiceAvailableTimeList; Overload;
    function GetEnumerator : TFhirHealthcareServiceAvailableTimeListEnumerator;

    //  Add a FhirHealthcareServiceAvailableTime to the end of the list.
    function Append : TFhirHealthcareServiceAvailableTime;

    // Add an already existing FhirHealthcareServiceAvailableTime to the end of the list.
    procedure AddItem(value : TFhirHealthcareServiceAvailableTime); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirHealthcareServiceAvailableTime) : Integer;

    // Insert FhirHealthcareServiceAvailableTime before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirHealthcareServiceAvailableTime;

    // Insert an existing FhirHealthcareServiceAvailableTime before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirHealthcareServiceAvailableTime);

    // Get the iIndexth FhirHealthcareServiceAvailableTime. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirHealthcareServiceAvailableTime);

    // The number of items in the collection
    function Item(index : Integer) : TFhirHealthcareServiceAvailableTime;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirHealthcareServiceAvailableTimes[index : Integer] : TFhirHealthcareServiceAvailableTime read GetItemN write SetItemN; default;
  End;

  // The HealthcareService is not available during this period of time due to the provided reason.
  TFhirHealthcareServiceNotAvailable = class (TFhirBackboneElement)
  protected
    FDescription : TFhirString;
    FDuring : TFhirPeriod;
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetDuring(value : TFhirPeriod);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirHealthcareServiceNotAvailable; overload;
    function Clone : TFhirHealthcareServiceNotAvailable; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to The reason that can be presented to the user as to why this time is not available.
    property description : String read GetDescriptionST write SetDescriptionST;
    // The reason that can be presented to the user as to why this time is not available.
    property descriptionElement : TFhirString read FDescription write SetDescription;

    // Typed access to Service is not available (seasonally or for a public holiday) from this date. (defined for API consistency)
    property during : TFhirPeriod read FDuring write SetDuring;
    // Service is not available (seasonally or for a public holiday) from this date.
    property duringElement : TFhirPeriod read FDuring write SetDuring;

  end;

  TFhirHealthcareServiceNotAvailableListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirHealthcareServiceNotAvailableList;
    function GetCurrent : TFhirHealthcareServiceNotAvailable;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirHealthcareServiceNotAvailableList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirHealthcareServiceNotAvailable read GetCurrent;
  end;

  TFhirHealthcareServiceNotAvailableList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirHealthcareServiceNotAvailable;
    procedure SetItemN(index : Integer; value : TFhirHealthcareServiceNotAvailable);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirHealthcareServiceNotAvailableList; Overload;
    function Clone : TFhirHealthcareServiceNotAvailableList; Overload;
    function GetEnumerator : TFhirHealthcareServiceNotAvailableListEnumerator;

    //  Add a FhirHealthcareServiceNotAvailable to the end of the list.
    function Append : TFhirHealthcareServiceNotAvailable;

    // Add an already existing FhirHealthcareServiceNotAvailable to the end of the list.
    procedure AddItem(value : TFhirHealthcareServiceNotAvailable); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirHealthcareServiceNotAvailable) : Integer;

    // Insert FhirHealthcareServiceNotAvailable before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirHealthcareServiceNotAvailable;

    // Insert an existing FhirHealthcareServiceNotAvailable before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirHealthcareServiceNotAvailable);

    // Get the iIndexth FhirHealthcareServiceNotAvailable. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirHealthcareServiceNotAvailable);

    // The number of items in the collection
    function Item(index : Integer) : TFhirHealthcareServiceNotAvailable;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirHealthcareServiceNotAvailables[index : Integer] : TFhirHealthcareServiceNotAvailable read GetItemN write SetItemN; default;
  End;

  // The details of a healthcare service available at a location.
  TFhirHealthcareService = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FProvidedBy : TFhirReference{TFhirOrganization};
    FServiceCategory : TFhirCodeableConcept;
    FserviceTypeList : TFhirHealthcareServiceServiceTypeList;
    FLocation : TFhirReference{TFhirLocation};
    FServiceName : TFhirString;
    FComment : TFhirString;
    FExtraDetails : TFhirString;
    FPhoto : TFhirAttachment;
    FtelecomList : TFhirContactPointList;
    FcoverageAreaList : TFhirReferenceList{TFhirLocation};
    FserviceProvisionCodeList : TFhirCodeableConceptList;
    FEligibility : TFhirCodeableConcept;
    FEligibilityNote : TFhirString;
    FprogramNameList : TFhirStringList;
    FcharacteristicList : TFhirCodeableConceptList;
    FreferralMethodList : TFhirCodeableConceptList;
    FPublicKey : TFhirString;
    FAppointmentRequired : TFhirBoolean;
    FavailableTimeList : TFhirHealthcareServiceAvailableTimeList;
    FnotAvailableList : TFhirHealthcareServiceNotAvailableList;
    FAvailabilityExceptions : TFhirString;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetProvidedBy(value : TFhirReference{TFhirOrganization});
    Procedure SetServiceCategory(value : TFhirCodeableConcept);
    function GetServiceTypeList : TFhirHealthcareServiceServiceTypeList;
    function GetHasServiceTypeList : Boolean;
    Procedure SetLocation(value : TFhirReference{TFhirLocation});
    Procedure SetServiceName(value : TFhirString);
    Function GetServiceNameST : String;
    Procedure SetServiceNameST(value : String);
    Procedure SetComment(value : TFhirString);
    Function GetCommentST : String;
    Procedure SetCommentST(value : String);
    Procedure SetExtraDetails(value : TFhirString);
    Function GetExtraDetailsST : String;
    Procedure SetExtraDetailsST(value : String);
    Procedure SetPhoto(value : TFhirAttachment);
    function GetTelecomList : TFhirContactPointList;
    function GetHasTelecomList : Boolean;
    function GetCoverageAreaList : TFhirReferenceList{TFhirLocation};
    function GetHasCoverageAreaList : Boolean;
    function GetServiceProvisionCodeList : TFhirCodeableConceptList;
    function GetHasServiceProvisionCodeList : Boolean;
    Procedure SetEligibility(value : TFhirCodeableConcept);
    Procedure SetEligibilityNote(value : TFhirString);
    Function GetEligibilityNoteST : String;
    Procedure SetEligibilityNoteST(value : String);
    function GetProgramNameList : TFhirStringList;
    function GetHasProgramNameList : Boolean;
    function GetCharacteristicList : TFhirCodeableConceptList;
    function GetHasCharacteristicList : Boolean;
    function GetReferralMethodList : TFhirCodeableConceptList;
    function GetHasReferralMethodList : Boolean;
    Procedure SetPublicKey(value : TFhirString);
    Function GetPublicKeyST : String;
    Procedure SetPublicKeyST(value : String);
    Procedure SetAppointmentRequired(value : TFhirBoolean);
    Function GetAppointmentRequiredST : Boolean;
    Procedure SetAppointmentRequiredST(value : Boolean);
    function GetAvailableTimeList : TFhirHealthcareServiceAvailableTimeList;
    function GetHasAvailableTimeList : Boolean;
    function GetNotAvailableList : TFhirHealthcareServiceNotAvailableList;
    function GetHasNotAvailableList : Boolean;
    Procedure SetAvailabilityExceptions(value : TFhirString);
    Function GetAvailabilityExceptionsST : String;
    Procedure SetAvailabilityExceptionsST(value : String);

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirHealthcareService; overload;
    function Clone : TFhirHealthcareService; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // External identifiers for this item.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // Typed access to The organization that provides this healthcare service. (defined for API consistency)
    property providedBy : TFhirReference{TFhirOrganization} read FProvidedBy write SetProvidedBy;
    // The organization that provides this healthcare service.
    property providedByElement : TFhirReference{TFhirOrganization} read FProvidedBy write SetProvidedBy;

    // Typed access to Identifies the broad category of service being performed or delivered. (defined for API consistency)
    property serviceCategory : TFhirCodeableConcept read FServiceCategory write SetServiceCategory;
    // Identifies the broad category of service being performed or delivered.
    property serviceCategoryElement : TFhirCodeableConcept read FServiceCategory write SetServiceCategory;

    // A specific type of service that may be delivered or performed.
    property serviceTypeList : TFhirHealthcareServiceServiceTypeList read GetServiceTypeList;
    property hasServiceTypeList : boolean read GetHasServiceTypeList;

    // Typed access to The location where this healthcare service may be provided. (defined for API consistency)
    property location : TFhirReference{TFhirLocation} read FLocation write SetLocation;
    // The location where this healthcare service may be provided.
    property locationElement : TFhirReference{TFhirLocation} read FLocation write SetLocation;

    // Typed access to Further description of the service as it would be presented to a consumer while searching.
    property serviceName : String read GetServiceNameST write SetServiceNameST;
    // Further description of the service as it would be presented to a consumer while searching.
    property serviceNameElement : TFhirString read FServiceName write SetServiceName;

    // Typed access to Any additional description of the service and/or any specific issues not covered by the other attributes, which can be displayed as further detail under the serviceName.
    property comment : String read GetCommentST write SetCommentST;
    // Any additional description of the service and/or any specific issues not covered by the other attributes, which can be displayed as further detail under the serviceName.
    property commentElement : TFhirString read FComment write SetComment;

    // Typed access to Extra details about the service that can't be placed in the other fields.
    property extraDetails : String read GetExtraDetailsST write SetExtraDetailsST;
    // Extra details about the service that can't be placed in the other fields.
    property extraDetailsElement : TFhirString read FExtraDetails write SetExtraDetails;

    // Typed access to If there is a photo/symbol associated with this HealthcareService, it may be included here to facilitate quick identification of the service in a list. (defined for API consistency)
    property photo : TFhirAttachment read FPhoto write SetPhoto;
    // If there is a photo/symbol associated with this HealthcareService, it may be included here to facilitate quick identification of the service in a list.
    property photoElement : TFhirAttachment read FPhoto write SetPhoto;

    // List of contacts related to this specific healthcare service.
    property telecomList : TFhirContactPointList read GetTelecomList;
    property hasTelecomList : boolean read GetHasTelecomList;

    // The location(s) that this service is available to (not where the service is provided).
    property coverageAreaList : TFhirReferenceList{TFhirLocation} read GetCoverageAreaList;
    property hasCoverageAreaList : boolean read GetHasCoverageAreaList;

    // The code(s) that detail the conditions under which the healthcare service is available/offered.
    property serviceProvisionCodeList : TFhirCodeableConceptList read GetServiceProvisionCodeList;
    property hasServiceProvisionCodeList : boolean read GetHasServiceProvisionCodeList;

    // Typed access to Does this service have specific eligibility requirements that need to be met in order to use the service? (defined for API consistency)
    property eligibility : TFhirCodeableConcept read FEligibility write SetEligibility;
    // Does this service have specific eligibility requirements that need to be met in order to use the service?
    property eligibilityElement : TFhirCodeableConcept read FEligibility write SetEligibility;

    // Typed access to Describes the eligibility conditions for the service.
    property eligibilityNote : String read GetEligibilityNoteST write SetEligibilityNoteST;
    // Describes the eligibility conditions for the service.
    property eligibilityNoteElement : TFhirString read FEligibilityNote write SetEligibilityNote;

    // Program Names that can be used to categorize the service.
    property programNameList : TFhirStringList read GetProgramNameList;
    property hasProgramNameList : boolean read GetHasProgramNameList;

    // Collection of characteristics (attributes).
    property characteristicList : TFhirCodeableConceptList read GetCharacteristicList;
    property hasCharacteristicList : boolean read GetHasCharacteristicList;

    // Ways that the service accepts referrals, if this is not provided then it is implied that no referral is required.
    property referralMethodList : TFhirCodeableConceptList read GetReferralMethodList;
    property hasReferralMethodList : boolean read GetHasReferralMethodList;

    // Typed access to The public part of the 'keys' allocated to an Organization by an accredited body to support secure exchange of data over the internet. To be provided by the Organization, where available.
    property publicKey : String read GetPublicKeyST write SetPublicKeyST;
    // The public part of the 'keys' allocated to an Organization by an accredited body to support secure exchange of data over the internet. To be provided by the Organization, where available.
    property publicKeyElement : TFhirString read FPublicKey write SetPublicKey;

    // Typed access to Indicates whether or not a prospective consumer will require an appointment for a particular service at a site to be provided by the Organization. Indicates if an appointment is required for access to this service.
    property appointmentRequired : Boolean read GetAppointmentRequiredST write SetAppointmentRequiredST;
    // Indicates whether or not a prospective consumer will require an appointment for a particular service at a site to be provided by the Organization. Indicates if an appointment is required for access to this service.
    property appointmentRequiredElement : TFhirBoolean read FAppointmentRequired write SetAppointmentRequired;

    // A collection of times that the Service Site is available.
    property availableTimeList : TFhirHealthcareServiceAvailableTimeList read GetAvailableTimeList;
    property hasAvailableTimeList : boolean read GetHasAvailableTimeList;

    // The HealthcareService is not available during this period of time due to the provided reason.
    property notAvailableList : TFhirHealthcareServiceNotAvailableList read GetNotAvailableList;
    property hasNotAvailableList : boolean read GetHasNotAvailableList;

    // Typed access to A description of site availability exceptions, e.g. public holiday availability. Succinctly describing all possible exceptions to normal site availability as details in the available Times and not available Times.
    property availabilityExceptions : String read GetAvailabilityExceptionsST write SetAvailabilityExceptionsST;
    // A description of site availability exceptions, e.g. public holiday availability. Succinctly describing all possible exceptions to normal site availability as details in the available Times and not available Times.
    property availabilityExceptionsElement : TFhirString read FAvailabilityExceptions write SetAvailabilityExceptions;

  end;

  TFhirHealthcareServiceListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirHealthcareServiceList;
    function GetCurrent : TFhirHealthcareService;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirHealthcareServiceList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirHealthcareService read GetCurrent;
  end;

  TFhirHealthcareServiceList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirHealthcareService;
    procedure SetItemN(index : Integer; value : TFhirHealthcareService);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirHealthcareServiceList; Overload;
    function Clone : TFhirHealthcareServiceList; Overload;
    function GetEnumerator : TFhirHealthcareServiceListEnumerator;

    //  Add a FhirHealthcareService to the end of the list.
    function Append : TFhirHealthcareService;

    // Add an already existing FhirHealthcareService to the end of the list.
    procedure AddItem(value : TFhirHealthcareService); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirHealthcareService) : Integer;

    // Insert FhirHealthcareService before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirHealthcareService;

    // Insert an existing FhirHealthcareService before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirHealthcareService);

    // Get the iIndexth FhirHealthcareService. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirHealthcareService);

    // The number of items in the collection
    function Item(index : Integer) : TFhirHealthcareService;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirHealthcareServices[index : Integer] : TFhirHealthcareService read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_HEALTHCARESERVICE}

{$IFDEF FHIR_LOCATION}

  // The absolute geographic location of the Location, expressed using the WGS84 datum (This is the same co-ordinate system used in KML).
  TFhirLocationPosition = class (TFhirBackboneElement)
  protected
    FLongitude : TFhirDecimal;
    FLatitude : TFhirDecimal;
    FAltitude : TFhirDecimal;
    Procedure SetLongitude(value : TFhirDecimal);
    Function GetLongitudeST : String;
    Procedure SetLongitudeST(value : String);
    Procedure SetLatitude(value : TFhirDecimal);
    Function GetLatitudeST : String;
    Procedure SetLatitudeST(value : String);
    Procedure SetAltitude(value : TFhirDecimal);
    Function GetAltitudeST : String;
    Procedure SetAltitudeST(value : String);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirLocationPosition; overload;
    function Clone : TFhirLocationPosition; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to Longitude. The value domain and the interpretation are the same as for the text of the longitude element in KML (see notes below).
    property longitude : String read GetLongitudeST write SetLongitudeST;
    // Longitude. The value domain and the interpretation are the same as for the text of the longitude element in KML (see notes below).
    property longitudeElement : TFhirDecimal read FLongitude write SetLongitude;

    // Typed access to Latitude. The value domain and the interpretation are the same as for the text of the latitude element in KML (see notes below).
    property latitude : String read GetLatitudeST write SetLatitudeST;
    // Latitude. The value domain and the interpretation are the same as for the text of the latitude element in KML (see notes below).
    property latitudeElement : TFhirDecimal read FLatitude write SetLatitude;

    // Typed access to Altitude. The value domain and the interpretation are the same as for the text of the altitude element in KML (see notes below).
    property altitude : String read GetAltitudeST write SetAltitudeST;
    // Altitude. The value domain and the interpretation are the same as for the text of the altitude element in KML (see notes below).
    property altitudeElement : TFhirDecimal read FAltitude write SetAltitude;

  end;

  TFhirLocationPositionListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirLocationPositionList;
    function GetCurrent : TFhirLocationPosition;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirLocationPositionList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirLocationPosition read GetCurrent;
  end;

  TFhirLocationPositionList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirLocationPosition;
    procedure SetItemN(index : Integer; value : TFhirLocationPosition);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirLocationPositionList; Overload;
    function Clone : TFhirLocationPositionList; Overload;
    function GetEnumerator : TFhirLocationPositionListEnumerator;

    //  Add a FhirLocationPosition to the end of the list.
    function Append : TFhirLocationPosition;

    // Add an already existing FhirLocationPosition to the end of the list.
    procedure AddItem(value : TFhirLocationPosition); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirLocationPosition) : Integer;

    // Insert FhirLocationPosition before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirLocationPosition;

    // Insert an existing FhirLocationPosition before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirLocationPosition);

    // Get the iIndexth FhirLocationPosition. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirLocationPosition);

    // The number of items in the collection
    function Item(index : Integer) : TFhirLocationPosition;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirLocationPositions[index : Integer] : TFhirLocationPosition read GetItemN write SetItemN; default;
  End;

  // Details and position information for a physical place where services are provided  and resources and participants may be stored, found, contained or accommodated.
  TFhirLocation = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FStatus : TFhirEnum;
    FName : TFhirString;
    FDescription : TFhirString;
    FMode : TFhirEnum;
    FType_ : TFhirCodeableConcept;
    FtelecomList : TFhirContactPointList;
    FAddress : TFhirAddress;
    FPhysicalType : TFhirCodeableConcept;
    FPosition : TFhirLocationPosition;
    FManagingOrganization : TFhirReference{TFhirOrganization};
    FPartOf : TFhirReference{TFhirLocation};
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirLocationStatusEnum;
    Procedure SetStatusST(value : TFhirLocationStatusEnum);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetMode(value : TFhirEnum);
    Function GetModeST : TFhirLocationModeEnum;
    Procedure SetModeST(value : TFhirLocationModeEnum);
    Procedure SetType_(value : TFhirCodeableConcept);
    function GetTelecomList : TFhirContactPointList;
    function GetHasTelecomList : Boolean;
    Procedure SetAddress(value : TFhirAddress);
    Procedure SetPhysicalType(value : TFhirCodeableConcept);
    Procedure SetPosition(value : TFhirLocationPosition);
    Procedure SetManagingOrganization(value : TFhirReference{TFhirOrganization});
    Procedure SetPartOf(value : TFhirReference{TFhirLocation});

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirLocation; overload;
    function Clone : TFhirLocation; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Unique code or number identifying the location to its users.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // active | suspended | inactive.
    property status : TFhirLocationStatusEnum read GetStatusST write SetStatusST;
    property statusElement : TFhirEnum read FStatus write SetStatus;

    // Typed access to Name of the location as used by humans. Does not need to be unique.
    property name : String read GetNameST write SetNameST;
    // Name of the location as used by humans. Does not need to be unique.
    property nameElement : TFhirString read FName write SetName;

    // Typed access to Description of the Location, which helps in finding or referencing the place.
    property description : String read GetDescriptionST write SetDescriptionST;
    // Description of the Location, which helps in finding or referencing the place.
    property descriptionElement : TFhirString read FDescription write SetDescription;

    // Indicates whether a resource instance represents a specific location or a class of locations.
    property mode : TFhirLocationModeEnum read GetModeST write SetModeST;
    property modeElement : TFhirEnum read FMode write SetMode;

    // Typed access to Indicates the type of function performed at the location. (defined for API consistency)
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    // Indicates the type of function performed at the location.
    property type_Element : TFhirCodeableConcept read FType_ write SetType_;

    // The contact details of communication devices available at the location. This can include phone numbers, fax numbers, mobile numbers, email addresses and web sites.
    property telecomList : TFhirContactPointList read GetTelecomList;
    property hasTelecomList : boolean read GetHasTelecomList;

    // Typed access to Physical location. (defined for API consistency)
    property address : TFhirAddress read FAddress write SetAddress;
    // Physical location.
    property addressElement : TFhirAddress read FAddress write SetAddress;

    // Typed access to Physical form of the location, e.g. building, room, vehicle, road. (defined for API consistency)
    property physicalType : TFhirCodeableConcept read FPhysicalType write SetPhysicalType;
    // Physical form of the location, e.g. building, room, vehicle, road.
    property physicalTypeElement : TFhirCodeableConcept read FPhysicalType write SetPhysicalType;

    // Typed access to The absolute geographic location of the Location, expressed using the WGS84 datum (This is the same co-ordinate system used in KML). (defined for API consistency)
    property position : TFhirLocationPosition read FPosition write SetPosition;
    // The absolute geographic location of the Location, expressed using the WGS84 datum (This is the same co-ordinate system used in KML).
    property positionElement : TFhirLocationPosition read FPosition write SetPosition;

    // Typed access to The organization responsible for the provisioning and upkeep of the location. (defined for API consistency)
    property managingOrganization : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;
    // The organization responsible for the provisioning and upkeep of the location.
    property managingOrganizationElement : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;

    // Typed access to Another Location which this Location is physically part of. (defined for API consistency)
    property partOf : TFhirReference{TFhirLocation} read FPartOf write SetPartOf;
    // Another Location which this Location is physically part of.
    property partOfElement : TFhirReference{TFhirLocation} read FPartOf write SetPartOf;

  end;

  TFhirLocationListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirLocationList;
    function GetCurrent : TFhirLocation;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirLocationList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirLocation read GetCurrent;
  end;

  TFhirLocationList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirLocation;
    procedure SetItemN(index : Integer; value : TFhirLocation);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirLocationList; Overload;
    function Clone : TFhirLocationList; Overload;
    function GetEnumerator : TFhirLocationListEnumerator;

    //  Add a FhirLocation to the end of the list.
    function Append : TFhirLocation;

    // Add an already existing FhirLocation to the end of the list.
    procedure AddItem(value : TFhirLocation); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirLocation) : Integer;

    // Insert FhirLocation before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirLocation;

    // Insert an existing FhirLocation before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirLocation);

    // Get the iIndexth FhirLocation. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirLocation);

    // The number of items in the collection
    function Item(index : Integer) : TFhirLocation;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirLocations[index : Integer] : TFhirLocation read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_LOCATION}

{$IFDEF FHIR_ORGANIZATION}

  // Contact for the organization for a certain purpose.
  TFhirOrganizationContact = class (TFhirBackboneElement)
  protected
    FPurpose : TFhirCodeableConcept;
    FName : TFhirHumanName;
    FtelecomList : TFhirContactPointList;
    FAddress : TFhirAddress;
    Procedure SetPurpose(value : TFhirCodeableConcept);
    Procedure SetName(value : TFhirHumanName);
    function GetTelecomList : TFhirContactPointList;
    function GetHasTelecomList : Boolean;
    Procedure SetAddress(value : TFhirAddress);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirOrganizationContact; overload;
    function Clone : TFhirOrganizationContact; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to Indicates a purpose for which the contact can be reached. (defined for API consistency)
    property purpose : TFhirCodeableConcept read FPurpose write SetPurpose;
    // Indicates a purpose for which the contact can be reached.
    property purposeElement : TFhirCodeableConcept read FPurpose write SetPurpose;

    // Typed access to A name associated with the contact. (defined for API consistency)
    property name : TFhirHumanName read FName write SetName;
    // A name associated with the contact.
    property nameElement : TFhirHumanName read FName write SetName;

    // A contact detail (e.g. a telephone number or an email address) by which the party may be contacted.
    property telecomList : TFhirContactPointList read GetTelecomList;
    property hasTelecomList : boolean read GetHasTelecomList;

    // Typed access to Visiting or postal addresses for the contact. (defined for API consistency)
    property address : TFhirAddress read FAddress write SetAddress;
    // Visiting or postal addresses for the contact.
    property addressElement : TFhirAddress read FAddress write SetAddress;

  end;

  TFhirOrganizationContactListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirOrganizationContactList;
    function GetCurrent : TFhirOrganizationContact;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirOrganizationContactList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirOrganizationContact read GetCurrent;
  end;

  TFhirOrganizationContactList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirOrganizationContact;
    procedure SetItemN(index : Integer; value : TFhirOrganizationContact);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirOrganizationContactList; Overload;
    function Clone : TFhirOrganizationContactList; Overload;
    function GetEnumerator : TFhirOrganizationContactListEnumerator;

    //  Add a FhirOrganizationContact to the end of the list.
    function Append : TFhirOrganizationContact;

    // Add an already existing FhirOrganizationContact to the end of the list.
    procedure AddItem(value : TFhirOrganizationContact); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirOrganizationContact) : Integer;

    // Insert FhirOrganizationContact before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirOrganizationContact;

    // Insert an existing FhirOrganizationContact before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirOrganizationContact);

    // Get the iIndexth FhirOrganizationContact. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirOrganizationContact);

    // The number of items in the collection
    function Item(index : Integer) : TFhirOrganizationContact;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirOrganizationContacts[index : Integer] : TFhirOrganizationContact read GetItemN write SetItemN; default;
  End;

  // A formally or informally recognized grouping of people or organizations formed for the purpose of achieving some form of collective action.  Includes companies, institutions, corporations, departments, community groups, healthcare practice groups, etc.
  TFhirOrganization = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FActive : TFhirBoolean;
    FType_ : TFhirCodeableConcept;
    FName : TFhirString;
    FtelecomList : TFhirContactPointList;
    FaddressList : TFhirAddressList;
    FPartOf : TFhirReference{TFhirOrganization};
    FcontactList : TFhirOrganizationContactList;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetActive(value : TFhirBoolean);
    Function GetActiveST : Boolean;
    Procedure SetActiveST(value : Boolean);
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    function GetTelecomList : TFhirContactPointList;
    function GetHasTelecomList : Boolean;
    function GetAddressList : TFhirAddressList;
    function GetHasAddressList : Boolean;
    Procedure SetPartOf(value : TFhirReference{TFhirOrganization});
    function GetContactList : TFhirOrganizationContactList;
    function GetHasContactList : Boolean;

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirOrganization; overload;
    function Clone : TFhirOrganization; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Identifier for the organization that is used to identify the organization across multiple disparate systems.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // Typed access to Whether the organization's record is still in active use.
    property active : Boolean read GetActiveST write SetActiveST;
    // Whether the organization's record is still in active use.
    property activeElement : TFhirBoolean read FActive write SetActive;

    // Typed access to The kind of organization that this is. (defined for API consistency)
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    // The kind of organization that this is.
    property type_Element : TFhirCodeableConcept read FType_ write SetType_;

    // Typed access to A name associated with the organization.
    property name : String read GetNameST write SetNameST;
    // A name associated with the organization.
    property nameElement : TFhirString read FName write SetName;

    // A contact detail for the organization.
    property telecomList : TFhirContactPointList read GetTelecomList;
    property hasTelecomList : boolean read GetHasTelecomList;

    // An address for the organization.
    property addressList : TFhirAddressList read GetAddressList;
    property hasAddressList : boolean read GetHasAddressList;

    // Typed access to The organization of which this organization forms a part. (defined for API consistency)
    property partOf : TFhirReference{TFhirOrganization} read FPartOf write SetPartOf;
    // The organization of which this organization forms a part.
    property partOfElement : TFhirReference{TFhirOrganization} read FPartOf write SetPartOf;

    // Contact for the organization for a certain purpose.
    property contactList : TFhirOrganizationContactList read GetContactList;
    property hasContactList : boolean read GetHasContactList;

  end;

  TFhirOrganizationListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirOrganizationList;
    function GetCurrent : TFhirOrganization;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirOrganizationList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirOrganization read GetCurrent;
  end;

  TFhirOrganizationList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirOrganization;
    procedure SetItemN(index : Integer; value : TFhirOrganization);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirOrganizationList; Overload;
    function Clone : TFhirOrganizationList; Overload;
    function GetEnumerator : TFhirOrganizationListEnumerator;

    //  Add a FhirOrganization to the end of the list.
    function Append : TFhirOrganization;

    // Add an already existing FhirOrganization to the end of the list.
    procedure AddItem(value : TFhirOrganization); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirOrganization) : Integer;

    // Insert FhirOrganization before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirOrganization;

    // Insert an existing FhirOrganization before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirOrganization);

    // Get the iIndexth FhirOrganization. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirOrganization);

    // The number of items in the collection
    function Item(index : Integer) : TFhirOrganization;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirOrganizations[index : Integer] : TFhirOrganization read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_ORGANIZATION}

{$IFDEF FHIR_PATIENT}

  // A contact party (e.g. guardian, partner, friend) for the patient.
  TFhirPatientContact = class (TFhirBackboneElement)
  protected
    FrelationshipList : TFhirCodeableConceptList;
    FName : TFhirHumanName;
    FtelecomList : TFhirContactPointList;
    FAddress : TFhirAddress;
    FGender : TFhirEnum;
    FOrganization : TFhirReference{TFhirOrganization};
    FPeriod : TFhirPeriod;
    function GetRelationshipList : TFhirCodeableConceptList;
    function GetHasRelationshipList : Boolean;
    Procedure SetName(value : TFhirHumanName);
    function GetTelecomList : TFhirContactPointList;
    function GetHasTelecomList : Boolean;
    Procedure SetAddress(value : TFhirAddress);
    Procedure SetGender(value : TFhirEnum);
    Function GetGenderST : TFhirAdministrativeGenderEnum;
    Procedure SetGenderST(value : TFhirAdministrativeGenderEnum);
    Procedure SetOrganization(value : TFhirReference{TFhirOrganization});
    Procedure SetPeriod(value : TFhirPeriod);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPatientContact; overload;
    function Clone : TFhirPatientContact; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // The nature of the relationship between the patient and the contact person.
    property relationshipList : TFhirCodeableConceptList read GetRelationshipList;
    property hasRelationshipList : boolean read GetHasRelationshipList;

    // Typed access to A name associated with the contact person. (defined for API consistency)
    property name : TFhirHumanName read FName write SetName;
    // A name associated with the contact person.
    property nameElement : TFhirHumanName read FName write SetName;

    // A contact detail for the person, e.g. a telephone number or an email address.
    property telecomList : TFhirContactPointList read GetTelecomList;
    property hasTelecomList : boolean read GetHasTelecomList;

    // Typed access to Address for the contact person. (defined for API consistency)
    property address : TFhirAddress read FAddress write SetAddress;
    // Address for the contact person.
    property addressElement : TFhirAddress read FAddress write SetAddress;

    // Administrative Gender - the gender that the contact person is considered to have for administration and record keeping purposes.
    property gender : TFhirAdministrativeGenderEnum read GetGenderST write SetGenderST;
    property genderElement : TFhirEnum read FGender write SetGender;

    // Typed access to Organization on behalf of which the contact is acting or for which the contact is working. (defined for API consistency)
    property organization : TFhirReference{TFhirOrganization} read FOrganization write SetOrganization;
    // Organization on behalf of which the contact is acting or for which the contact is working.
    property organizationElement : TFhirReference{TFhirOrganization} read FOrganization write SetOrganization;

    // Typed access to The period during which this contact person or organization is valid to be contacted relating to this patient. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The period during which this contact person or organization is valid to be contacted relating to this patient.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;

  TFhirPatientContactListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPatientContactList;
    function GetCurrent : TFhirPatientContact;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPatientContactList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPatientContact read GetCurrent;
  end;

  TFhirPatientContactList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPatientContact;
    procedure SetItemN(index : Integer; value : TFhirPatientContact);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPatientContactList; Overload;
    function Clone : TFhirPatientContactList; Overload;
    function GetEnumerator : TFhirPatientContactListEnumerator;

    //  Add a FhirPatientContact to the end of the list.
    function Append : TFhirPatientContact;

    // Add an already existing FhirPatientContact to the end of the list.
    procedure AddItem(value : TFhirPatientContact); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPatientContact) : Integer;

    // Insert FhirPatientContact before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPatientContact;

    // Insert an existing FhirPatientContact before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPatientContact);

    // Get the iIndexth FhirPatientContact. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPatientContact);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPatientContact;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPatientContacts[index : Integer] : TFhirPatientContact read GetItemN write SetItemN; default;
  End;

  // This patient is known to be an animal.
  TFhirPatientAnimal = class (TFhirBackboneElement)
  protected
    FSpecies : TFhirCodeableConcept;
    FBreed : TFhirCodeableConcept;
    FGenderStatus : TFhirCodeableConcept;
    Procedure SetSpecies(value : TFhirCodeableConcept);
    Procedure SetBreed(value : TFhirCodeableConcept);
    Procedure SetGenderStatus(value : TFhirCodeableConcept);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPatientAnimal; overload;
    function Clone : TFhirPatientAnimal; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to Identifies the high level taxonomic categorization of the kind of animal. (defined for API consistency)
    property species : TFhirCodeableConcept read FSpecies write SetSpecies;
    // Identifies the high level taxonomic categorization of the kind of animal.
    property speciesElement : TFhirCodeableConcept read FSpecies write SetSpecies;

    // Typed access to Identifies the detailed categorization of the kind of animal. (defined for API consistency)
    property breed : TFhirCodeableConcept read FBreed write SetBreed;
    // Identifies the detailed categorization of the kind of animal.
    property breedElement : TFhirCodeableConcept read FBreed write SetBreed;

    // Typed access to Indicates the current state of the animal's reproductive organs. (defined for API consistency)
    property genderStatus : TFhirCodeableConcept read FGenderStatus write SetGenderStatus;
    // Indicates the current state of the animal's reproductive organs.
    property genderStatusElement : TFhirCodeableConcept read FGenderStatus write SetGenderStatus;

  end;

  TFhirPatientAnimalListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPatientAnimalList;
    function GetCurrent : TFhirPatientAnimal;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPatientAnimalList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPatientAnimal read GetCurrent;
  end;

  TFhirPatientAnimalList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPatientAnimal;
    procedure SetItemN(index : Integer; value : TFhirPatientAnimal);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPatientAnimalList; Overload;
    function Clone : TFhirPatientAnimalList; Overload;
    function GetEnumerator : TFhirPatientAnimalListEnumerator;

    //  Add a FhirPatientAnimal to the end of the list.
    function Append : TFhirPatientAnimal;

    // Add an already existing FhirPatientAnimal to the end of the list.
    procedure AddItem(value : TFhirPatientAnimal); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPatientAnimal) : Integer;

    // Insert FhirPatientAnimal before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPatientAnimal;

    // Insert an existing FhirPatientAnimal before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPatientAnimal);

    // Get the iIndexth FhirPatientAnimal. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPatientAnimal);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPatientAnimal;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPatientAnimals[index : Integer] : TFhirPatientAnimal read GetItemN write SetItemN; default;
  End;

  // Languages which may be used to communicate with the patient about his or her health.
  TFhirPatientCommunication = class (TFhirBackboneElement)
  protected
    FLanguage : TFhirCodeableConcept;
    FPreferred : TFhirBoolean;
    Procedure SetLanguage(value : TFhirCodeableConcept);
    Procedure SetPreferred(value : TFhirBoolean);
    Function GetPreferredST : Boolean;
    Procedure SetPreferredST(value : Boolean);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPatientCommunication; overload;
    function Clone : TFhirPatientCommunication; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to The ISO-639-1 alpha 2 code in lower case for the language, optionally followed by a hyphen and the ISO-3166-1 alpha 2 code for the region in upper case; e.g. "en" for English, or "en-US" for American English versus "en-EN" for England English. (defined for API consistency)
    property language : TFhirCodeableConcept read FLanguage write SetLanguage;
    // The ISO-639-1 alpha 2 code in lower case for the language, optionally followed by a hyphen and the ISO-3166-1 alpha 2 code for the region in upper case; e.g. "en" for English, or "en-US" for American English versus "en-EN" for England English.
    property languageElement : TFhirCodeableConcept read FLanguage write SetLanguage;

    // Typed access to Indicates whether or not the patient prefers this language (over other languages he masters up a certain level).
    property preferred : Boolean read GetPreferredST write SetPreferredST;
    // Indicates whether or not the patient prefers this language (over other languages he masters up a certain level).
    property preferredElement : TFhirBoolean read FPreferred write SetPreferred;

  end;

  TFhirPatientCommunicationListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPatientCommunicationList;
    function GetCurrent : TFhirPatientCommunication;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPatientCommunicationList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPatientCommunication read GetCurrent;
  end;

  TFhirPatientCommunicationList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPatientCommunication;
    procedure SetItemN(index : Integer; value : TFhirPatientCommunication);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPatientCommunicationList; Overload;
    function Clone : TFhirPatientCommunicationList; Overload;
    function GetEnumerator : TFhirPatientCommunicationListEnumerator;

    //  Add a FhirPatientCommunication to the end of the list.
    function Append : TFhirPatientCommunication;

    // Add an already existing FhirPatientCommunication to the end of the list.
    procedure AddItem(value : TFhirPatientCommunication); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPatientCommunication) : Integer;

    // Insert FhirPatientCommunication before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPatientCommunication;

    // Insert an existing FhirPatientCommunication before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPatientCommunication);

    // Get the iIndexth FhirPatientCommunication. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPatientCommunication);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPatientCommunication;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPatientCommunications[index : Integer] : TFhirPatientCommunication read GetItemN write SetItemN; default;
  End;

  // Link to another patient resource that concerns the same actual patient.
  TFhirPatientLink = class (TFhirBackboneElement)
  protected
    FOther : TFhirReference{TFhirPatient};
    FType_ : TFhirEnum;
    Procedure SetOther(value : TFhirReference{TFhirPatient});
    Procedure SetType_(value : TFhirEnum);
    Function GetType_ST : TFhirLinkTypeEnum;
    Procedure SetType_ST(value : TFhirLinkTypeEnum);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPatientLink; overload;
    function Clone : TFhirPatientLink; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to The other patient resource that the link refers to. (defined for API consistency)
    property other : TFhirReference{TFhirPatient} read FOther write SetOther;
    // The other patient resource that the link refers to.
    property otherElement : TFhirReference{TFhirPatient} read FOther write SetOther;

    // The type of link between this patient resource and another patient resource.
    property type_ : TFhirLinkTypeEnum read GetType_ST write SetType_ST;
    property type_Element : TFhirEnum read FType_ write SetType_;

  end;

  TFhirPatientLinkListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPatientLinkList;
    function GetCurrent : TFhirPatientLink;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPatientLinkList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPatientLink read GetCurrent;
  end;

  TFhirPatientLinkList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPatientLink;
    procedure SetItemN(index : Integer; value : TFhirPatientLink);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPatientLinkList; Overload;
    function Clone : TFhirPatientLinkList; Overload;
    function GetEnumerator : TFhirPatientLinkListEnumerator;

    //  Add a FhirPatientLink to the end of the list.
    function Append : TFhirPatientLink;

    // Add an already existing FhirPatientLink to the end of the list.
    procedure AddItem(value : TFhirPatientLink); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPatientLink) : Integer;

    // Insert FhirPatientLink before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPatientLink;

    // Insert an existing FhirPatientLink before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPatientLink);

    // Get the iIndexth FhirPatientLink. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPatientLink);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPatientLink;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPatientLinks[index : Integer] : TFhirPatientLink read GetItemN write SetItemN; default;
  End;

  // Demographics and other administrative information about an individual or animal receiving care or other health-related services.
  TFhirPatient = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FActive : TFhirBoolean;
    FnameList : TFhirHumanNameList;
    FtelecomList : TFhirContactPointList;
    FGender : TFhirEnum;
    FBirthDate : TFhirDate;
    FDeceased : TFhirType;
    FaddressList : TFhirAddressList;
    FMaritalStatus : TFhirCodeableConcept;
    FMultipleBirth : TFhirType;
    FphotoList : TFhirAttachmentList;
    FcontactList : TFhirPatientContactList;
    FAnimal : TFhirPatientAnimal;
    FcommunicationList : TFhirPatientCommunicationList;
    FcareProviderList : TFhirReferenceList{Resource};
    FManagingOrganization : TFhirReference{TFhirOrganization};
    Flink_List : TFhirPatientLinkList;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetActive(value : TFhirBoolean);
    Function GetActiveST : Boolean;
    Procedure SetActiveST(value : Boolean);
    function GetNameList : TFhirHumanNameList;
    function GetHasNameList : Boolean;
    function GetTelecomList : TFhirContactPointList;
    function GetHasTelecomList : Boolean;
    Procedure SetGender(value : TFhirEnum);
    Function GetGenderST : TFhirAdministrativeGenderEnum;
    Procedure SetGenderST(value : TFhirAdministrativeGenderEnum);
    Procedure SetBirthDate(value : TFhirDate);
    Function GetBirthDateST : TFslDateTime;
    Procedure SetBirthDateST(value : TFslDateTime);
    Procedure SetDeceased(value : TFhirType);
    function GetAddressList : TFhirAddressList;
    function GetHasAddressList : Boolean;
    Procedure SetMaritalStatus(value : TFhirCodeableConcept);
    Procedure SetMultipleBirth(value : TFhirType);
    function GetPhotoList : TFhirAttachmentList;
    function GetHasPhotoList : Boolean;
    function GetContactList : TFhirPatientContactList;
    function GetHasContactList : Boolean;
    Procedure SetAnimal(value : TFhirPatientAnimal);
    function GetCommunicationList : TFhirPatientCommunicationList;
    function GetHasCommunicationList : Boolean;
    function GetCareProviderList : TFhirReferenceList{Resource};
    function GetHasCareProviderList : Boolean;
    Procedure SetManagingOrganization(value : TFhirReference{TFhirOrganization});
    function GetLink_List : TFhirPatientLinkList;
    function GetHasLink_List : Boolean;

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPatient; overload;
    function Clone : TFhirPatient; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // An identifier for this patient.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // Typed access to Whether this patient record is in active use.
    property active : Boolean read GetActiveST write SetActiveST;
    // Whether this patient record is in active use.
    property activeElement : TFhirBoolean read FActive write SetActive;

    // A name associated with the individual.
    property nameList : TFhirHumanNameList read GetNameList;
    property hasNameList : boolean read GetHasNameList;

    // A contact detail (e.g. a telephone number or an email address) by which the individual may be contacted.
    property telecomList : TFhirContactPointList read GetTelecomList;
    property hasTelecomList : boolean read GetHasTelecomList;

    // Administrative Gender - the gender that the patient is considered to have for administration and record keeping purposes.
    property gender : TFhirAdministrativeGenderEnum read GetGenderST write SetGenderST;
    property genderElement : TFhirEnum read FGender write SetGender;

    // Typed access to The date of birth for the individual.
    property birthDate : TFslDateTime read GetBirthDateST write SetBirthDateST;
    // The date of birth for the individual.
    property birthDateElement : TFhirDate read FBirthDate write SetBirthDate;

    // Typed access to Indicates if the individual is deceased or not. (defined for API consistency)
    property deceased : TFhirType read FDeceased write SetDeceased;
    // Indicates if the individual is deceased or not.
    property deceasedElement : TFhirType read FDeceased write SetDeceased;

    // Addresses for the individual.
    property addressList : TFhirAddressList read GetAddressList;
    property hasAddressList : boolean read GetHasAddressList;

    // Typed access to This field contains a patient's most recent marital (civil) status. (defined for API consistency)
    property maritalStatus : TFhirCodeableConcept read FMaritalStatus write SetMaritalStatus;
    // This field contains a patient's most recent marital (civil) status.
    property maritalStatusElement : TFhirCodeableConcept read FMaritalStatus write SetMaritalStatus;

    // Typed access to Indicates whether the patient is part of a multiple or indicates the actual birth order. (defined for API consistency)
    property multipleBirth : TFhirType read FMultipleBirth write SetMultipleBirth;
    // Indicates whether the patient is part of a multiple or indicates the actual birth order.
    property multipleBirthElement : TFhirType read FMultipleBirth write SetMultipleBirth;

    // Image of the patient.
    property photoList : TFhirAttachmentList read GetPhotoList;
    property hasPhotoList : boolean read GetHasPhotoList;

    // A contact party (e.g. guardian, partner, friend) for the patient.
    property contactList : TFhirPatientContactList read GetContactList;
    property hasContactList : boolean read GetHasContactList;

    // Typed access to This patient is known to be an animal. (defined for API consistency)
    property animal : TFhirPatientAnimal read FAnimal write SetAnimal;
    // This patient is known to be an animal.
    property animalElement : TFhirPatientAnimal read FAnimal write SetAnimal;

    // Languages which may be used to communicate with the patient about his or her health.
    property communicationList : TFhirPatientCommunicationList read GetCommunicationList;
    property hasCommunicationList : boolean read GetHasCommunicationList;

    // Patient's nominated care provider.
    property careProviderList : TFhirReferenceList{Resource} read GetCareProviderList;
    property hasCareProviderList : boolean read GetHasCareProviderList;

    // Typed access to Organization that is the custodian of the patient record. (defined for API consistency)
    property managingOrganization : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;
    // Organization that is the custodian of the patient record.
    property managingOrganizationElement : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;

    // Link to another patient resource that concerns the same actual patient.
    property link_List : TFhirPatientLinkList read GetLink_List;
    property hasLink_List : boolean read GetHasLink_List;

  end;

  TFhirPatientListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPatientList;
    function GetCurrent : TFhirPatient;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPatientList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPatient read GetCurrent;
  end;

  TFhirPatientList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPatient;
    procedure SetItemN(index : Integer; value : TFhirPatient);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPatientList; Overload;
    function Clone : TFhirPatientList; Overload;
    function GetEnumerator : TFhirPatientListEnumerator;

    //  Add a FhirPatient to the end of the list.
    function Append : TFhirPatient;

    // Add an already existing FhirPatient to the end of the list.
    procedure AddItem(value : TFhirPatient); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPatient) : Integer;

    // Insert FhirPatient before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPatient;

    // Insert an existing FhirPatient before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPatient);

    // Get the iIndexth FhirPatient. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPatient);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPatient;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPatients[index : Integer] : TFhirPatient read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_PATIENT}

{$IFDEF FHIR_PERSON}

  // Link to a resource that concerns the same actual person.
  TFhirPersonLink = class (TFhirBackboneElement)
  protected
    FTarget : TFhirReference{Resource};
    FAssurance : TFhirEnum;
    Procedure SetTarget(value : TFhirReference{Resource});
    Procedure SetAssurance(value : TFhirEnum);
    Function GetAssuranceST : TFhirIdentityAssuranceLevelEnum;
    Procedure SetAssuranceST(value : TFhirIdentityAssuranceLevelEnum);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPersonLink; overload;
    function Clone : TFhirPersonLink; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to The resource to which this actual person is associated. (defined for API consistency)
    property target : TFhirReference{Resource} read FTarget write SetTarget;
    // The resource to which this actual person is associated.
    property targetElement : TFhirReference{Resource} read FTarget write SetTarget;

    // Level of assurance that this link is actually associated with the target resource.
    property assurance : TFhirIdentityAssuranceLevelEnum read GetAssuranceST write SetAssuranceST;
    property assuranceElement : TFhirEnum read FAssurance write SetAssurance;

  end;

  TFhirPersonLinkListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPersonLinkList;
    function GetCurrent : TFhirPersonLink;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPersonLinkList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPersonLink read GetCurrent;
  end;

  TFhirPersonLinkList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPersonLink;
    procedure SetItemN(index : Integer; value : TFhirPersonLink);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPersonLinkList; Overload;
    function Clone : TFhirPersonLinkList; Overload;
    function GetEnumerator : TFhirPersonLinkListEnumerator;

    //  Add a FhirPersonLink to the end of the list.
    function Append : TFhirPersonLink;

    // Add an already existing FhirPersonLink to the end of the list.
    procedure AddItem(value : TFhirPersonLink); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPersonLink) : Integer;

    // Insert FhirPersonLink before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPersonLink;

    // Insert an existing FhirPersonLink before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPersonLink);

    // Get the iIndexth FhirPersonLink. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPersonLink);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPersonLink;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPersonLinks[index : Integer] : TFhirPersonLink read GetItemN write SetItemN; default;
  End;

  // Demographics and administrative information about a person independent of a specific health-related context.
  TFhirPerson = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FnameList : TFhirHumanNameList;
    FtelecomList : TFhirContactPointList;
    FGender : TFhirEnum;
    FBirthDate : TFhirDate;
    FaddressList : TFhirAddressList;
    FPhoto : TFhirAttachment;
    FManagingOrganization : TFhirReference{TFhirOrganization};
    FActive : TFhirBoolean;
    Flink_List : TFhirPersonLinkList;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    function GetNameList : TFhirHumanNameList;
    function GetHasNameList : Boolean;
    function GetTelecomList : TFhirContactPointList;
    function GetHasTelecomList : Boolean;
    Procedure SetGender(value : TFhirEnum);
    Function GetGenderST : TFhirAdministrativeGenderEnum;
    Procedure SetGenderST(value : TFhirAdministrativeGenderEnum);
    Procedure SetBirthDate(value : TFhirDate);
    Function GetBirthDateST : TFslDateTime;
    Procedure SetBirthDateST(value : TFslDateTime);
    function GetAddressList : TFhirAddressList;
    function GetHasAddressList : Boolean;
    Procedure SetPhoto(value : TFhirAttachment);
    Procedure SetManagingOrganization(value : TFhirReference{TFhirOrganization});
    Procedure SetActive(value : TFhirBoolean);
    Function GetActiveST : Boolean;
    Procedure SetActiveST(value : Boolean);
    function GetLink_List : TFhirPersonLinkList;
    function GetHasLink_List : Boolean;

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPerson; overload;
    function Clone : TFhirPerson; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Identifier for a person within a particular scope.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // A name associated with the person.
    property nameList : TFhirHumanNameList read GetNameList;
    property hasNameList : boolean read GetHasNameList;

    // A contact detail for the person, e.g. a telephone number or an email address.
    property telecomList : TFhirContactPointList read GetTelecomList;
    property hasTelecomList : boolean read GetHasTelecomList;

    // Administrative Gender.
    property gender : TFhirAdministrativeGenderEnum read GetGenderST write SetGenderST;
    property genderElement : TFhirEnum read FGender write SetGender;

    // Typed access to The birth date for the person.
    property birthDate : TFslDateTime read GetBirthDateST write SetBirthDateST;
    // The birth date for the person.
    property birthDateElement : TFhirDate read FBirthDate write SetBirthDate;

    // One or more addresses for the person.
    property addressList : TFhirAddressList read GetAddressList;
    property hasAddressList : boolean read GetHasAddressList;

    // Typed access to An image that can be displayed as a thumbnail of the person to enhance the identification of the individual. (defined for API consistency)
    property photo : TFhirAttachment read FPhoto write SetPhoto;
    // An image that can be displayed as a thumbnail of the person to enhance the identification of the individual.
    property photoElement : TFhirAttachment read FPhoto write SetPhoto;

    // Typed access to The organization that is the custodian of the person record. (defined for API consistency)
    property managingOrganization : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;
    // The organization that is the custodian of the person record.
    property managingOrganizationElement : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;

    // Typed access to Whether this person's record is in active use.
    property active : Boolean read GetActiveST write SetActiveST;
    // Whether this person's record is in active use.
    property activeElement : TFhirBoolean read FActive write SetActive;

    // Link to a resource that concerns the same actual person.
    property link_List : TFhirPersonLinkList read GetLink_List;
    property hasLink_List : boolean read GetHasLink_List;

  end;

  TFhirPersonListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPersonList;
    function GetCurrent : TFhirPerson;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPersonList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPerson read GetCurrent;
  end;

  TFhirPersonList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPerson;
    procedure SetItemN(index : Integer; value : TFhirPerson);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPersonList; Overload;
    function Clone : TFhirPersonList; Overload;
    function GetEnumerator : TFhirPersonListEnumerator;

    //  Add a FhirPerson to the end of the list.
    function Append : TFhirPerson;

    // Add an already existing FhirPerson to the end of the list.
    procedure AddItem(value : TFhirPerson); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPerson) : Integer;

    // Insert FhirPerson before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPerson;

    // Insert an existing FhirPerson before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPerson);

    // Get the iIndexth FhirPerson. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPerson);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPerson;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPeople[index : Integer] : TFhirPerson read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_PERSON}

{$IFDEF FHIR_PRACTITIONER}

  // The list of roles/organizations that the practitioner is associated with.
  TFhirPractitionerPractitionerRole = class (TFhirBackboneElement)
  protected
    FManagingOrganization : TFhirReference{TFhirOrganization};
    FRole : TFhirCodeableConcept;
    FspecialtyList : TFhirCodeableConceptList;
    FPeriod : TFhirPeriod;
    FlocationList : TFhirReferenceList{TFhirLocation};
    FhealthcareServiceList : TFhirReferenceList{TFhirHealthcareService};
    Procedure SetManagingOrganization(value : TFhirReference{TFhirOrganization});
    Procedure SetRole(value : TFhirCodeableConcept);
    function GetSpecialtyList : TFhirCodeableConceptList;
    function GetHasSpecialtyList : Boolean;
    Procedure SetPeriod(value : TFhirPeriod);
    function GetLocationList : TFhirReferenceList{TFhirLocation};
    function GetHasLocationList : Boolean;
    function GetHealthcareServiceList : TFhirReferenceList{TFhirHealthcareService};
    function GetHasHealthcareServiceList : Boolean;

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPractitionerPractitionerRole; overload;
    function Clone : TFhirPractitionerPractitionerRole; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to The organization where the Practitioner performs the roles associated. (defined for API consistency)
    property managingOrganization : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;
    // The organization where the Practitioner performs the roles associated.
    property managingOrganizationElement : TFhirReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;

    // Typed access to Roles which this practitioner is authorized to perform for the organization. (defined for API consistency)
    property role : TFhirCodeableConcept read FRole write SetRole;
    // Roles which this practitioner is authorized to perform for the organization.
    property roleElement : TFhirCodeableConcept read FRole write SetRole;

    // Specific specialty of the practitioner.
    property specialtyList : TFhirCodeableConceptList read GetSpecialtyList;
    property hasSpecialtyList : boolean read GetHasSpecialtyList;

    // Typed access to The period during which the person is authorized to act as a practitioner in these role(s) for the organization. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The period during which the person is authorized to act as a practitioner in these role(s) for the organization.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

    // The location(s) at which this practitioner provides care.
    property locationList : TFhirReferenceList{TFhirLocation} read GetLocationList;
    property hasLocationList : boolean read GetHasLocationList;

    // The list of healthcare services that this worker provides for this role's Organization/Location(s).
    property healthcareServiceList : TFhirReferenceList{TFhirHealthcareService} read GetHealthcareServiceList;
    property hasHealthcareServiceList : boolean read GetHasHealthcareServiceList;

  end;

  TFhirPractitionerPractitionerRoleListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPractitionerPractitionerRoleList;
    function GetCurrent : TFhirPractitionerPractitionerRole;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPractitionerPractitionerRoleList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPractitionerPractitionerRole read GetCurrent;
  end;

  TFhirPractitionerPractitionerRoleList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPractitionerPractitionerRole;
    procedure SetItemN(index : Integer; value : TFhirPractitionerPractitionerRole);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPractitionerPractitionerRoleList; Overload;
    function Clone : TFhirPractitionerPractitionerRoleList; Overload;
    function GetEnumerator : TFhirPractitionerPractitionerRoleListEnumerator;

    //  Add a FhirPractitionerPractitionerRole to the end of the list.
    function Append : TFhirPractitionerPractitionerRole;

    // Add an already existing FhirPractitionerPractitionerRole to the end of the list.
    procedure AddItem(value : TFhirPractitionerPractitionerRole); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPractitionerPractitionerRole) : Integer;

    // Insert FhirPractitionerPractitionerRole before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPractitionerPractitionerRole;

    // Insert an existing FhirPractitionerPractitionerRole before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPractitionerPractitionerRole);

    // Get the iIndexth FhirPractitionerPractitionerRole. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPractitionerPractitionerRole);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPractitionerPractitionerRole;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPractitionerPractitionerRoles[index : Integer] : TFhirPractitionerPractitionerRole read GetItemN write SetItemN; default;
  End;

  // Qualifications obtained by training and certification.
  TFhirPractitionerQualification = class (TFhirBackboneElement)
  protected
    FidentifierList : TFhirIdentifierList;
    FCode : TFhirCodeableConcept;
    FPeriod : TFhirPeriod;
    FIssuer : TFhirReference{TFhirOrganization};
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetCode(value : TFhirCodeableConcept);
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetIssuer(value : TFhirReference{TFhirOrganization});

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPractitionerQualification; overload;
    function Clone : TFhirPractitionerQualification; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // An identifier that applies to this person's qualification in this role.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // Typed access to Coded representation of the qualification. (defined for API consistency)
    property code : TFhirCodeableConcept read FCode write SetCode;
    // Coded representation of the qualification.
    property codeElement : TFhirCodeableConcept read FCode write SetCode;

    // Typed access to Period during which the qualification is valid. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // Period during which the qualification is valid.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

    // Typed access to Organization that regulates and issues the qualification. (defined for API consistency)
    property issuer : TFhirReference{TFhirOrganization} read FIssuer write SetIssuer;
    // Organization that regulates and issues the qualification.
    property issuerElement : TFhirReference{TFhirOrganization} read FIssuer write SetIssuer;

  end;

  TFhirPractitionerQualificationListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPractitionerQualificationList;
    function GetCurrent : TFhirPractitionerQualification;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPractitionerQualificationList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPractitionerQualification read GetCurrent;
  end;

  TFhirPractitionerQualificationList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPractitionerQualification;
    procedure SetItemN(index : Integer; value : TFhirPractitionerQualification);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPractitionerQualificationList; Overload;
    function Clone : TFhirPractitionerQualificationList; Overload;
    function GetEnumerator : TFhirPractitionerQualificationListEnumerator;

    //  Add a FhirPractitionerQualification to the end of the list.
    function Append : TFhirPractitionerQualification;

    // Add an already existing FhirPractitionerQualification to the end of the list.
    procedure AddItem(value : TFhirPractitionerQualification); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPractitionerQualification) : Integer;

    // Insert FhirPractitionerQualification before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPractitionerQualification;

    // Insert an existing FhirPractitionerQualification before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPractitionerQualification);

    // Get the iIndexth FhirPractitionerQualification. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPractitionerQualification);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPractitionerQualification;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPractitionerQualifications[index : Integer] : TFhirPractitionerQualification read GetItemN write SetItemN; default;
  End;

  // A person who is directly or indirectly involved in the provisioning of healthcare.
  TFhirPractitioner = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FActive : TFhirBoolean;
    FName : TFhirHumanName;
    FtelecomList : TFhirContactPointList;
    FaddressList : TFhirAddressList;
    FGender : TFhirEnum;
    FBirthDate : TFhirDate;
    FphotoList : TFhirAttachmentList;
    FpractitionerRoleList : TFhirPractitionerPractitionerRoleList;
    FqualificationList : TFhirPractitionerQualificationList;
    FcommunicationList : TFhirCodeableConceptList;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetActive(value : TFhirBoolean);
    Function GetActiveST : Boolean;
    Procedure SetActiveST(value : Boolean);
    Procedure SetName(value : TFhirHumanName);
    function GetTelecomList : TFhirContactPointList;
    function GetHasTelecomList : Boolean;
    function GetAddressList : TFhirAddressList;
    function GetHasAddressList : Boolean;
    Procedure SetGender(value : TFhirEnum);
    Function GetGenderST : TFhirAdministrativeGenderEnum;
    Procedure SetGenderST(value : TFhirAdministrativeGenderEnum);
    Procedure SetBirthDate(value : TFhirDate);
    Function GetBirthDateST : TFslDateTime;
    Procedure SetBirthDateST(value : TFslDateTime);
    function GetPhotoList : TFhirAttachmentList;
    function GetHasPhotoList : Boolean;
    function GetPractitionerRoleList : TFhirPractitionerPractitionerRoleList;
    function GetHasPractitionerRoleList : Boolean;
    function GetQualificationList : TFhirPractitionerQualificationList;
    function GetHasQualificationList : Boolean;
    function GetCommunicationList : TFhirCodeableConceptList;
    function GetHasCommunicationList : Boolean;

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirPractitioner; overload;
    function Clone : TFhirPractitioner; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // An identifier that applies to this person in this role.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // Typed access to Whether this practitioner's record is in active use.
    property active : Boolean read GetActiveST write SetActiveST;
    // Whether this practitioner's record is in active use.
    property activeElement : TFhirBoolean read FActive write SetActive;

    // Typed access to A name associated with the person. (defined for API consistency)
    property name : TFhirHumanName read FName write SetName;
    // A name associated with the person.
    property nameElement : TFhirHumanName read FName write SetName;

    // A contact detail for the practitioner, e.g. a telephone number or an email address.
    property telecomList : TFhirContactPointList read GetTelecomList;
    property hasTelecomList : boolean read GetHasTelecomList;

    // The postal address where the practitioner can be found or visited or to which mail can be delivered.
    property addressList : TFhirAddressList read GetAddressList;
    property hasAddressList : boolean read GetHasAddressList;

    // Administrative Gender - the gender that the person is considered to have for administration and record keeping purposes.
    property gender : TFhirAdministrativeGenderEnum read GetGenderST write SetGenderST;
    property genderElement : TFhirEnum read FGender write SetGender;

    // Typed access to The date of birth for the practitioner.
    property birthDate : TFslDateTime read GetBirthDateST write SetBirthDateST;
    // The date of birth for the practitioner.
    property birthDateElement : TFhirDate read FBirthDate write SetBirthDate;

    // Image of the person.
    property photoList : TFhirAttachmentList read GetPhotoList;
    property hasPhotoList : boolean read GetHasPhotoList;

    // The list of roles/organizations that the practitioner is associated with.
    property practitionerRoleList : TFhirPractitionerPractitionerRoleList read GetPractitionerRoleList;
    property hasPractitionerRoleList : boolean read GetHasPractitionerRoleList;

    // Qualifications obtained by training and certification.
    property qualificationList : TFhirPractitionerQualificationList read GetQualificationList;
    property hasQualificationList : boolean read GetHasQualificationList;

    // A language the practitioner is able to use in patient communication.
    property communicationList : TFhirCodeableConceptList read GetCommunicationList;
    property hasCommunicationList : boolean read GetHasCommunicationList;

  end;

  TFhirPractitionerListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirPractitionerList;
    function GetCurrent : TFhirPractitioner;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirPractitionerList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirPractitioner read GetCurrent;
  end;

  TFhirPractitionerList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirPractitioner;
    procedure SetItemN(index : Integer; value : TFhirPractitioner);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirPractitionerList; Overload;
    function Clone : TFhirPractitionerList; Overload;
    function GetEnumerator : TFhirPractitionerListEnumerator;

    //  Add a FhirPractitioner to the end of the list.
    function Append : TFhirPractitioner;

    // Add an already existing FhirPractitioner to the end of the list.
    procedure AddItem(value : TFhirPractitioner); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirPractitioner) : Integer;

    // Insert FhirPractitioner before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirPractitioner;

    // Insert an existing FhirPractitioner before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirPractitioner);

    // Get the iIndexth FhirPractitioner. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirPractitioner);

    // The number of items in the collection
    function Item(index : Integer) : TFhirPractitioner;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirPractitioners[index : Integer] : TFhirPractitioner read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_PRACTITIONER}

{$IFDEF FHIR_RELATEDPERSON}

  // Information about a person that is involved in the care for a patient, but who is not the target of healthcare, nor has a formal responsibility in the care process.
  TFhirRelatedPerson = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FPatient : TFhirReference{TFhirPatient};
    FRelationship : TFhirCodeableConcept;
    FName : TFhirHumanName;
    FtelecomList : TFhirContactPointList;
    FGender : TFhirEnum;
    FBirthDate : TFhirDate;
    FaddressList : TFhirAddressList;
    FphotoList : TFhirAttachmentList;
    FPeriod : TFhirPeriod;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetPatient(value : TFhirReference{TFhirPatient});
    Procedure SetRelationship(value : TFhirCodeableConcept);
    Procedure SetName(value : TFhirHumanName);
    function GetTelecomList : TFhirContactPointList;
    function GetHasTelecomList : Boolean;
    Procedure SetGender(value : TFhirEnum);
    Function GetGenderST : TFhirAdministrativeGenderEnum;
    Procedure SetGenderST(value : TFhirAdministrativeGenderEnum);
    Procedure SetBirthDate(value : TFhirDate);
    Function GetBirthDateST : TFslDateTime;
    Procedure SetBirthDateST(value : TFslDateTime);
    function GetAddressList : TFhirAddressList;
    function GetHasAddressList : Boolean;
    function GetPhotoList : TFhirAttachmentList;
    function GetHasPhotoList : Boolean;
    Procedure SetPeriod(value : TFhirPeriod);

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirRelatedPerson; overload;
    function Clone : TFhirRelatedPerson; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Identifier for a person within a particular scope.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // Typed access to The patient this person is related to. (defined for API consistency)
    property patient : TFhirReference{TFhirPatient} read FPatient write SetPatient;
    // The patient this person is related to.
    property patientElement : TFhirReference{TFhirPatient} read FPatient write SetPatient;

    // Typed access to The nature of the relationship between a patient and the related person. (defined for API consistency)
    property relationship : TFhirCodeableConcept read FRelationship write SetRelationship;
    // The nature of the relationship between a patient and the related person.
    property relationshipElement : TFhirCodeableConcept read FRelationship write SetRelationship;

    // Typed access to A name associated with the person. (defined for API consistency)
    property name : TFhirHumanName read FName write SetName;
    // A name associated with the person.
    property nameElement : TFhirHumanName read FName write SetName;

    // A contact detail for the person, e.g. a telephone number or an email address.
    property telecomList : TFhirContactPointList read GetTelecomList;
    property hasTelecomList : boolean read GetHasTelecomList;

    // Administrative Gender - the gender that the person is considered to have for administration and record keeping purposes.
    property gender : TFhirAdministrativeGenderEnum read GetGenderST write SetGenderST;
    property genderElement : TFhirEnum read FGender write SetGender;

    // Typed access to The date on which the related person was born.
    property birthDate : TFslDateTime read GetBirthDateST write SetBirthDateST;
    // The date on which the related person was born.
    property birthDateElement : TFhirDate read FBirthDate write SetBirthDate;

    // Address where the related person can be contacted or visited.
    property addressList : TFhirAddressList read GetAddressList;
    property hasAddressList : boolean read GetHasAddressList;

    // Image of the person.
    property photoList : TFhirAttachmentList read GetPhotoList;
    property hasPhotoList : boolean read GetHasPhotoList;

    // Typed access to The period of time that this relationship is considered to be valid. If there are no dates defined, then the interval is unknown. (defined for API consistency)
    property period : TFhirPeriod read FPeriod write SetPeriod;
    // The period of time that this relationship is considered to be valid. If there are no dates defined, then the interval is unknown.
    property periodElement : TFhirPeriod read FPeriod write SetPeriod;

  end;

  TFhirRelatedPersonListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirRelatedPersonList;
    function GetCurrent : TFhirRelatedPerson;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirRelatedPersonList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirRelatedPerson read GetCurrent;
  end;

  TFhirRelatedPersonList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirRelatedPerson;
    procedure SetItemN(index : Integer; value : TFhirRelatedPerson);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirRelatedPersonList; Overload;
    function Clone : TFhirRelatedPersonList; Overload;
    function GetEnumerator : TFhirRelatedPersonListEnumerator;

    //  Add a FhirRelatedPerson to the end of the list.
    function Append : TFhirRelatedPerson;

    // Add an already existing FhirRelatedPerson to the end of the list.
    procedure AddItem(value : TFhirRelatedPerson); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirRelatedPerson) : Integer;

    // Insert FhirRelatedPerson before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirRelatedPerson;

    // Insert an existing FhirRelatedPerson before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirRelatedPerson);

    // Get the iIndexth FhirRelatedPerson. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirRelatedPerson);

    // The number of items in the collection
    function Item(index : Integer) : TFhirRelatedPerson;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirRelatedPeople[index : Integer] : TFhirRelatedPerson read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_RELATEDPERSON}

{$IFDEF FHIR_SCHEDULE}

  // A container for slot(s) of time that may be available for booking appointments.
  TFhirSchedule = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    Ftype_List : TFhirCodeableConceptList;
    FActor : TFhirReference{Resource};
    FPlanningHorizon : TFhirPeriod;
    FComment : TFhirString;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    function GetType_List : TFhirCodeableConceptList;
    function GetHasType_List : Boolean;
    Procedure SetActor(value : TFhirReference{Resource});
    Procedure SetPlanningHorizon(value : TFhirPeriod);
    Procedure SetComment(value : TFhirString);
    Function GetCommentST : String;
    Procedure SetCommentST(value : String);

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirSchedule; overload;
    function Clone : TFhirSchedule; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // External Ids for this item.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // The schedule type can be used for the categorization of healthcare services or other appointment types.
    property type_List : TFhirCodeableConceptList read GetType_List;
    property hasType_List : boolean read GetHasType_List;

    // Typed access to The resource this Schedule resource is providing availability information for. These are expected to usually be one of HealthcareService, Location, Practitioner, Device, Patient or RelatedPerson. (defined for API consistency)
    property actor : TFhirReference{Resource} read FActor write SetActor;
    // The resource this Schedule resource is providing availability information for. These are expected to usually be one of HealthcareService, Location, Practitioner, Device, Patient or RelatedPerson.
    property actorElement : TFhirReference{Resource} read FActor write SetActor;

    // Typed access to The period of time that the slots that are attached to this Schedule resource cover (even if none exist). These  cover the amount of time that an organization's planning horizon; the interval for which they are currently accepting appointments. This does not define a "template" for planning outside these dates. (defined for API consistency)
    property planningHorizon : TFhirPeriod read FPlanningHorizon write SetPlanningHorizon;
    // The period of time that the slots that are attached to this Schedule resource cover (even if none exist). These  cover the amount of time that an organization's planning horizon; the interval for which they are currently accepting appointments. This does not define a "template" for planning outside these dates.
    property planningHorizonElement : TFhirPeriod read FPlanningHorizon write SetPlanningHorizon;

    // Typed access to Comments on the availability to describe any extended information. Such as custom constraints on the slot(s) that may be associated.
    property comment : String read GetCommentST write SetCommentST;
    // Comments on the availability to describe any extended information. Such as custom constraints on the slot(s) that may be associated.
    property commentElement : TFhirString read FComment write SetComment;

  end;

  TFhirScheduleListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirScheduleList;
    function GetCurrent : TFhirSchedule;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirScheduleList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSchedule read GetCurrent;
  end;

  TFhirScheduleList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirSchedule;
    procedure SetItemN(index : Integer; value : TFhirSchedule);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirScheduleList; Overload;
    function Clone : TFhirScheduleList; Overload;
    function GetEnumerator : TFhirScheduleListEnumerator;

    //  Add a FhirSchedule to the end of the list.
    function Append : TFhirSchedule;

    // Add an already existing FhirSchedule to the end of the list.
    procedure AddItem(value : TFhirSchedule); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirSchedule) : Integer;

    // Insert FhirSchedule before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirSchedule;

    // Insert an existing FhirSchedule before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirSchedule);

    // Get the iIndexth FhirSchedule. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirSchedule);

    // The number of items in the collection
    function Item(index : Integer) : TFhirSchedule;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirSchedules[index : Integer] : TFhirSchedule read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_SCHEDULE}

{$IFDEF FHIR_SLOT}

  // A slot of time on a schedule that may be available for booking appointments.
  TFhirSlot = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FType_ : TFhirCodeableConcept;
    FSchedule : TFhirReference{TFhirSchedule};
    FFreeBusyType : TFhirEnum;
    FStart : TFhirInstant;
    FEnd_ : TFhirInstant;
    FOverbooked : TFhirBoolean;
    FComment : TFhirString;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetSchedule(value : TFhirReference{TFhirSchedule});
    Procedure SetFreeBusyType(value : TFhirEnum);
    Function GetFreeBusyTypeST : TFhirSlotstatusEnum;
    Procedure SetFreeBusyTypeST(value : TFhirSlotstatusEnum);
    Procedure SetStart(value : TFhirInstant);
    Function GetStartST : TFslDateTime;
    Procedure SetStartST(value : TFslDateTime);
    Procedure SetEnd_(value : TFhirInstant);
    Function GetEnd_ST : TFslDateTime;
    Procedure SetEnd_ST(value : TFslDateTime);
    Procedure SetOverbooked(value : TFhirBoolean);
    Function GetOverbookedST : Boolean;
    Procedure SetOverbookedST(value : Boolean);
    Procedure SetComment(value : TFhirString);
    Function GetCommentST : String;
    Procedure SetCommentST(value : String);

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirSlot; overload;
    function Clone : TFhirSlot; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // External Ids for this item.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // Typed access to The type of appointments that can be booked into this slot (ideally this would be an identifiable service - which is at a location, rather than the location itself). If provided then this overrides the value provided on the availability resource. (defined for API consistency)
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    // The type of appointments that can be booked into this slot (ideally this would be an identifiable service - which is at a location, rather than the location itself). If provided then this overrides the value provided on the availability resource.
    property type_Element : TFhirCodeableConcept read FType_ write SetType_;

    // Typed access to The schedule resource that this slot defines an interval of status information. (defined for API consistency)
    property schedule : TFhirReference{TFhirSchedule} read FSchedule write SetSchedule;
    // The schedule resource that this slot defines an interval of status information.
    property scheduleElement : TFhirReference{TFhirSchedule} read FSchedule write SetSchedule;

    // busy | free | busy-unavailable | busy-tentative.
    property freeBusyType : TFhirSlotstatusEnum read GetFreeBusyTypeST write SetFreeBusyTypeST;
    property freeBusyTypeElement : TFhirEnum read FFreeBusyType write SetFreeBusyType;

    // Typed access to Date/Time that the slot is to begin.
    property start : TFslDateTime read GetStartST write SetStartST;
    // Date/Time that the slot is to begin.
    property startElement : TFhirInstant read FStart write SetStart;

    // Typed access to Date/Time that the slot is to conclude.
    property end_ : TFslDateTime read GetEnd_ST write SetEnd_ST;
    // Date/Time that the slot is to conclude.
    property end_Element : TFhirInstant read FEnd_ write SetEnd_;

    // Typed access to This slot has already been overbooked, appointments are unlikely to be accepted for this time.
    property overbooked : Boolean read GetOverbookedST write SetOverbookedST;
    // This slot has already been overbooked, appointments are unlikely to be accepted for this time.
    property overbookedElement : TFhirBoolean read FOverbooked write SetOverbooked;

    // Typed access to Comments on the slot to describe any extended information. Such as custom constraints on the slot.
    property comment : String read GetCommentST write SetCommentST;
    // Comments on the slot to describe any extended information. Such as custom constraints on the slot.
    property commentElement : TFhirString read FComment write SetComment;

  end;

  TFhirSlotListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirSlotList;
    function GetCurrent : TFhirSlot;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirSlotList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSlot read GetCurrent;
  end;

  TFhirSlotList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirSlot;
    procedure SetItemN(index : Integer; value : TFhirSlot);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirSlotList; Overload;
    function Clone : TFhirSlotList; Overload;
    function GetEnumerator : TFhirSlotListEnumerator;

    //  Add a FhirSlot to the end of the list.
    function Append : TFhirSlot;

    // Add an already existing FhirSlot to the end of the list.
    procedure AddItem(value : TFhirSlot); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirSlot) : Integer;

    // Insert FhirSlot before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirSlot;

    // Insert an existing FhirSlot before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirSlot);

    // Get the iIndexth FhirSlot. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirSlot);

    // The number of items in the collection
    function Item(index : Integer) : TFhirSlot;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirSlots[index : Integer] : TFhirSlot read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_SLOT}

{$IFDEF FHIR_SUBSTANCE}

  // Substance may be used to describe a kind of substance, or a specific package/container of the substance: an instance.
  TFhirSubstanceInstance = class (TFhirBackboneElement)
  protected
    FIdentifier : TFhirIdentifier;
    FExpiry : TFhirDateTime;
    FQuantity : TFhirQuantity;
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetExpiry(value : TFhirDateTime);
    Function GetExpiryST : TFslDateTime;
    Procedure SetExpiryST(value : TFslDateTime);
    Procedure SetQuantity(value : TFhirQuantity);

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirSubstanceInstance; overload;
    function Clone : TFhirSubstanceInstance; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to Identifier associated with the package/container (usually a label affixed directly). (defined for API consistency)
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    // Identifier associated with the package/container (usually a label affixed directly).
    property identifierElement : TFhirIdentifier read FIdentifier write SetIdentifier;

    // Typed access to When the substance is no longer valid to use. For some substances, a single arbitrary date is used for expiry.
    property expiry : TFslDateTime read GetExpiryST write SetExpiryST;
    // When the substance is no longer valid to use. For some substances, a single arbitrary date is used for expiry.
    property expiryElement : TFhirDateTime read FExpiry write SetExpiry;

    // Typed access to The amount of the substance. (defined for API consistency)
    property quantity : TFhirQuantity read FQuantity write SetQuantity;
    // The amount of the substance.
    property quantityElement : TFhirQuantity read FQuantity write SetQuantity;

  end;

  TFhirSubstanceInstanceListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirSubstanceInstanceList;
    function GetCurrent : TFhirSubstanceInstance;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirSubstanceInstanceList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSubstanceInstance read GetCurrent;
  end;

  TFhirSubstanceInstanceList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirSubstanceInstance;
    procedure SetItemN(index : Integer; value : TFhirSubstanceInstance);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirSubstanceInstanceList; Overload;
    function Clone : TFhirSubstanceInstanceList; Overload;
    function GetEnumerator : TFhirSubstanceInstanceListEnumerator;

    //  Add a FhirSubstanceInstance to the end of the list.
    function Append : TFhirSubstanceInstance;

    // Add an already existing FhirSubstanceInstance to the end of the list.
    procedure AddItem(value : TFhirSubstanceInstance); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirSubstanceInstance) : Integer;

    // Insert FhirSubstanceInstance before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirSubstanceInstance;

    // Insert an existing FhirSubstanceInstance before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirSubstanceInstance);

    // Get the iIndexth FhirSubstanceInstance. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirSubstanceInstance);

    // The number of items in the collection
    function Item(index : Integer) : TFhirSubstanceInstance;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirSubstanceInstances[index : Integer] : TFhirSubstanceInstance read GetItemN write SetItemN; default;
  End;

  // A substance can be composed of other substances.
  TFhirSubstanceIngredient = class (TFhirBackboneElement)
  protected
    FQuantity : TFhirRatio;
    FSubstance : TFhirReference{TFhirSubstance};
    Procedure SetQuantity(value : TFhirRatio);
    Procedure SetSubstance(value : TFhirReference{TFhirSubstance});

    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirSubstanceIngredient; overload;
    function Clone : TFhirSubstanceIngredient; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string) : TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Typed access to The amount of the ingredient in the substance - a concentration ratio. (defined for API consistency)
    property quantity : TFhirRatio read FQuantity write SetQuantity;
    // The amount of the ingredient in the substance - a concentration ratio.
    property quantityElement : TFhirRatio read FQuantity write SetQuantity;

    // Typed access to Another substance that is a component of this substance. (defined for API consistency)
    property substance : TFhirReference{TFhirSubstance} read FSubstance write SetSubstance;
    // Another substance that is a component of this substance.
    property substanceElement : TFhirReference{TFhirSubstance} read FSubstance write SetSubstance;

  end;

  TFhirSubstanceIngredientListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirSubstanceIngredientList;
    function GetCurrent : TFhirSubstanceIngredient;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirSubstanceIngredientList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSubstanceIngredient read GetCurrent;
  end;

  TFhirSubstanceIngredientList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirSubstanceIngredient;
    procedure SetItemN(index : Integer; value : TFhirSubstanceIngredient);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirSubstanceIngredientList; Overload;
    function Clone : TFhirSubstanceIngredientList; Overload;
    function GetEnumerator : TFhirSubstanceIngredientListEnumerator;

    //  Add a FhirSubstanceIngredient to the end of the list.
    function Append : TFhirSubstanceIngredient;

    // Add an already existing FhirSubstanceIngredient to the end of the list.
    procedure AddItem(value : TFhirSubstanceIngredient); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirSubstanceIngredient) : Integer;

    // Insert FhirSubstanceIngredient before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirSubstanceIngredient;

    // Insert an existing FhirSubstanceIngredient before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirSubstanceIngredient);

    // Get the iIndexth FhirSubstanceIngredient. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirSubstanceIngredient);

    // The number of items in the collection
    function Item(index : Integer) : TFhirSubstanceIngredient;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirSubstanceIngredients[index : Integer] : TFhirSubstanceIngredient read GetItemN write SetItemN; default;
  End;

  // A homogeneous material with a definite composition.
  TFhirSubstance = class (TFhirDomainResource)
  protected
    FidentifierList : TFhirIdentifierList;
    FcategoryList : TFhirCodeableConceptList;
    FCode : TFhirCodeableConcept;
    FDescription : TFhirString;
    FinstanceList : TFhirSubstanceInstanceList;
    FingredientList : TFhirSubstanceIngredientList;
    function GetIdentifierList : TFhirIdentifierList;
    function GetHasIdentifierList : Boolean;
    function GetCategoryList : TFhirCodeableConceptList;
    function GetHasCategoryList : Boolean;
    Procedure SetCode(value : TFhirCodeableConcept);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    function GetInstanceList : TFhirSubstanceInstanceList;
    function GetHasInstanceList : Boolean;
    function GetIngredientList : TFhirSubstanceIngredientList;
    function GetHasIngredientList : Boolean;

    function GetResourceType : TFhirResourceType; override;
    Procedure GetChildrenByName(child_name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    procedure listFieldsInOrder(fields : TStringList); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    procedure Assign(oSource : TFslObject); override;
    function Link : TFhirSubstance; overload;
    function Clone : TFhirSubstance; overload;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    procedure deleteProperty(propName : string; value : TFHIRObject); override;
    procedure replaceProperty(propName : string; existing, new : TFHIRObject); override;
    procedure reorderProperty(propName : string; source, destination : integer); override;
    function fhirType : string; override;
    function Equals(other : TObject) : boolean; override;
    function isEmpty : boolean; override;
  {$IFNDEF FPC}published{$ENDIF}
    // Unique identifier for the substance.
    property identifierList : TFhirIdentifierList read GetIdentifierList;
    property hasIdentifierList : boolean read GetHasIdentifierList;

    // A code that classifies the general type of substance.  This is used  for searching, sorting and display purposes.
    property categoryList : TFhirCodeableConceptList read GetCategoryList;
    property hasCategoryList : boolean read GetHasCategoryList;

    // Typed access to A code (or set of codes) that identify this substance. (defined for API consistency)
    property code : TFhirCodeableConcept read FCode write SetCode;
    // A code (or set of codes) that identify this substance.
    property codeElement : TFhirCodeableConcept read FCode write SetCode;

    // Typed access to A description of the substance - its appearance, handling requirements, and other usage notes.
    property description : String read GetDescriptionST write SetDescriptionST;
    // A description of the substance - its appearance, handling requirements, and other usage notes.
    property descriptionElement : TFhirString read FDescription write SetDescription;

    // Substance may be used to describe a kind of substance, or a specific package/container of the substance: an instance.
    property instanceList : TFhirSubstanceInstanceList read GetInstanceList;
    property hasInstanceList : boolean read GetHasInstanceList;

    // A substance can be composed of other substances.
    property ingredientList : TFhirSubstanceIngredientList read GetIngredientList;
    property hasIngredientList : boolean read GetHasIngredientList;

  end;

  TFhirSubstanceListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFhirSubstanceList;
    function GetCurrent : TFhirSubstance;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(list : TFhirSubstanceList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirSubstance read GetCurrent;
  end;

  TFhirSubstanceList = class (TFHIRObjectList)
  private

    function GetItemN(index : Integer) : TFhirSubstance;
    procedure SetItemN(index : Integer; value : TFhirSubstance);
  protected
    function ItemClass : TFslObjectClass; override;
  public

    function Link : TFhirSubstanceList; Overload;
    function Clone : TFhirSubstanceList; Overload;
    function GetEnumerator : TFhirSubstanceListEnumerator;

    //  Add a FhirSubstance to the end of the list.
    function Append : TFhirSubstance;

    // Add an already existing FhirSubstance to the end of the list.
    procedure AddItem(value : TFhirSubstance); overload;

    // See if an item is already in the list. returns -1 if not in the list
    function IndexOf(value : TFhirSubstance) : Integer;

    // Insert FhirSubstance before the designated index (0 = first item)
    function Insert(index : Integer) : TFhirSubstance;

    // Insert an existing FhirSubstance before the designated index (0 = first item)
    procedure InsertItem(index : Integer; value : TFhirSubstance);

    // Get the iIndexth FhirSubstance. (0 = first item)
    procedure SetItemByIndex(index : Integer; value : TFhirSubstance);

    // The number of items in the collection
    function Item(index : Integer) : TFhirSubstance;

    // The number of items in the collection
    function Count : Integer; Overload;

    // Remove the indexth item. The first item is index 0.
    procedure Remove(index : Integer);

    // Remove All Items from the list
    procedure ClearItems;

    Property FhirSubstances[index : Integer] : TFhirSubstance read GetItemN write SetItemN; default;
  End;

{$ENDIF FHIR_SUBSTANCE}

implementation

uses
  fhir2_utilities;

{$IFDEF FHIR_DEVICE}

{ TFhirDevice }

constructor TFhirDevice.Create;
begin
  inherited;
end;

destructor TFhirDevice.Destroy;
begin
  FIdentifierList.Free;
  FType_.free;
  FNoteList.Free;
  FStatus.free;
  FManufacturer.free;
  FModel.free;
  FVersion.free;
  FManufactureDate.free;
  FExpiry.free;
  FUdi.free;
  FLotNumber.free;
  FOwner.free;
  FLocation.free;
  FPatient.free;
  FContactList.Free;
  FUrl.free;
  inherited;
end;

function TFhirDevice.GetResourceType : TFhirResourceType;
begin
  result := frtDevice;
end;

procedure TFhirDevice.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirDevice(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirDevice(oSource).FIdentifierList);
  end;
  type_ := TFhirDevice(oSource).type_.Clone;
  if (TFhirDevice(oSource).FNoteList = nil) then
  begin
    FNoteList.free;
    FNoteList := nil;
  end
  else
  begin
    if FNoteList = nil then
      FNoteList := TFhirAnnotationList.Create;
    FNoteList.Assign(TFhirDevice(oSource).FNoteList);
  end;
  FStatus := TFhirDevice(oSource).FStatus.Link;
  manufacturerElement := TFhirDevice(oSource).manufacturerElement.Clone;
  modelElement := TFhirDevice(oSource).modelElement.Clone;
  versionElement := TFhirDevice(oSource).versionElement.Clone;
  manufactureDateElement := TFhirDevice(oSource).manufactureDateElement.Clone;
  expiryElement := TFhirDevice(oSource).expiryElement.Clone;
  udiElement := TFhirDevice(oSource).udiElement.Clone;
  lotNumberElement := TFhirDevice(oSource).lotNumberElement.Clone;
  owner := TFhirDevice(oSource).owner.Clone;
  location := TFhirDevice(oSource).location.Clone;
  patient := TFhirDevice(oSource).patient.Clone;
  if (TFhirDevice(oSource).FContactList = nil) then
  begin
    FContactList.free;
    FContactList := nil;
  end
  else
  begin
    if FContactList = nil then
      FContactList := TFhirContactPointList.Create;
    FContactList.Assign(TFhirDevice(oSource).FContactList);
  end;
  urlElement := TFhirDevice(oSource).urlElement.Clone;
end;

procedure TFhirDevice.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'note') Then
    list.addAll(self, 'note', FNoteList);
  if (child_name = 'status') Then
     list.add(self.link, 'status', FStatus.Link);
  if (child_name = 'manufacturer') Then
     list.add(self.link, 'manufacturer', FManufacturer.Link);
  if (child_name = 'model') Then
     list.add(self.link, 'model', FModel.Link);
  if (child_name = 'version') Then
     list.add(self.link, 'version', FVersion.Link);
  if (child_name = 'manufactureDate') Then
     list.add(self.link, 'manufactureDate', FManufactureDate.Link);
  if (child_name = 'expiry') Then
     list.add(self.link, 'expiry', FExpiry.Link);
  if (child_name = 'udi') Then
     list.add(self.link, 'udi', FUdi.Link);
  if (child_name = 'lotNumber') Then
     list.add(self.link, 'lotNumber', FLotNumber.Link);
  if (child_name = 'owner') Then
     list.add(self.link, 'owner', FOwner.Link);
  if (child_name = 'location') Then
     list.add(self.link, 'location', FLocation.Link);
  if (child_name = 'patient') Then
     list.add(self.link, 'patient', FPatient.Link);
  if (child_name = 'contact') Then
    list.addAll(self, 'contact', FContactList);
  if (child_name = 'url') Then
     list.add(self.link, 'url', FUrl.Link);
end;

procedure TFhirDevice.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', false, TFhirCodeableConcept, FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'note', 'Annotation', true, TFhirAnnotation, FNoteList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', false, TFHIREnum, FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'manufacturer', 'string', false, TFhirString, FManufacturer.Link));{2}
  oList.add(TFHIRProperty.create(self, 'model', 'string', false, TFhirString, FModel.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', false, TFhirString, FVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'manufactureDate', 'dateTime', false, TFhirDateTime, FManufactureDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'expiry', 'dateTime', false, TFhirDateTime, FExpiry.Link));{2}
  oList.add(TFHIRProperty.create(self, 'udi', 'string', false, TFhirString, FUdi.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lotNumber', 'string', false, TFhirString, FLotNumber.Link));{2}
  oList.add(TFHIRProperty.create(self, 'owner', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FOwner.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Reference(Location)', false, TFhirReference{TFhirLocation}, FLocation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'patient', 'Reference(Patient)', false, TFhirReference{TFhirPatient}, FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contact', 'ContactPoint', true, TFhirContactPoint, FContactList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'url', 'uri', false, TFhirUri, FUrl.Link));{2}
end;

function TFhirDevice.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'type') then
  begin
    Type_ := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'note') then
  begin
    NoteList.add(propValue as TFhirAnnotation){2a};
    result := propValue;
  end
  else if (propName = 'status') then
  begin
    StatusElement := asEnum(SYSTEMS_TFhirDevicestatusEnum, CODES_TFhirDevicestatusEnum, propValue);
    result := propValue
  end
  else if (propName = 'manufacturer') then
  begin
    ManufacturerElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'model') then
  begin
    ModelElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'version') then
  begin
    VersionElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'manufactureDate') then
  begin
    ManufactureDateElement := asDateTime(propValue){5a};
    result := propValue;
  end
  else if (propName = 'expiry') then
  begin
    ExpiryElement := asDateTime(propValue){5a};
    result := propValue;
  end
  else if (propName = 'udi') then
  begin
    UdiElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'lotNumber') then
  begin
    LotNumberElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'owner') then
  begin
    Owner := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'location') then
  begin
    Location := propValue as TFhirReference{TFhirLocation}{4b};
    result := propValue;
  end
  else if (propName = 'patient') then
  begin
    Patient := propValue as TFhirReference{TFhirPatient}{4b};
    result := propValue;
  end
  else if (propName = 'contact') then
  begin
    ContactList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'url') then
  begin
    UrlElement := asUri(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirDevice.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'note') then NoteList.insertItem(index, propValue as TFhirAnnotation){2a}
  else if (propName = 'contact') then ContactList.insertItem(index, propValue as TFhirContactPoint){2a}
  else inherited;
end;

function TFhirDevice.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'type') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'note') then result := NoteList.new(){2}
  else if (propName = 'manufacturer') then result := TFhirString.create() {5b}
  else if (propName = 'model') then result := TFhirString.create() {5b}
  else if (propName = 'version') then result := TFhirString.create() {5b}
  else if (propName = 'manufactureDate') then result := TFhirDateTime.create() {5b}
  else if (propName = 'expiry') then result := TFhirDateTime.create() {5b}
  else if (propName = 'udi') then result := TFhirString.create() {5b}
  else if (propName = 'lotNumber') then result := TFhirString.create() {5b}
  else if (propName = 'owner') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'location') then result := TFhirReference{TFhirLocation}.create(){4b}
  else if (propName = 'patient') then result := TFhirReference{TFhirPatient}.create(){4b}
  else if (propName = 'contact') then result := ContactList.new(){2}
  else if (propName = 'url') then result := TFhirUri.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirDevice.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'note') then result := 'Annotation'
  else if (propName = 'status') then result := 'code'
  else if (propName = 'manufacturer') then result := 'string'
  else if (propName = 'model') then result := 'string'
  else if (propName = 'version') then result := 'string'
  else if (propName = 'manufactureDate') then result := 'dateTime'
  else if (propName = 'expiry') then result := 'dateTime'
  else if (propName = 'udi') then result := 'string'
  else if (propName = 'lotNumber') then result := 'string'
  else if (propName = 'owner') then result := 'Reference'
  else if (propName = 'location') then result := 'Reference'
  else if (propName = 'patient') then result := 'Reference'
  else if (propName = 'contact') then result := 'ContactPoint'
  else if (propName = 'url') then result := 'uri'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirDevice.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'type') then Type_Element := nil
  else if (propName = 'note') then deletePropertyValue('note', NoteList, value) {2}
  else if (propName = 'status') then StatusElement := nil
  else if (propName = 'manufacturer') then ManufacturerElement := nil
  else if (propName = 'model') then ModelElement := nil
  else if (propName = 'version') then VersionElement := nil
  else if (propName = 'manufactureDate') then ManufactureDateElement := nil
  else if (propName = 'expiry') then ExpiryElement := nil
  else if (propName = 'udi') then UdiElement := nil
  else if (propName = 'lotNumber') then LotNumberElement := nil
  else if (propName = 'owner') then OwnerElement := nil
  else if (propName = 'location') then LocationElement := nil
  else if (propName = 'patient') then PatientElement := nil
  else if (propName = 'contact') then deletePropertyValue('contact', ContactList, value) {2}
  else if (propName = 'url') then UrlElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirDevice.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'type') then Type_Element := new as TFhirCodeableConcept{4}
  else if (propName = 'note') then replacePropertyValue('note', NoteList, existing, new) {2}
  else if (propName = 'status') then StatusElement := asEnum(SYSTEMS_TFhirDevicestatusEnum, CODES_TFhirDevicestatusEnum, new){4}
  else if (propName = 'manufacturer') then ManufacturerElement := asString(new){5b}
  else if (propName = 'model') then ModelElement := asString(new){5b}
  else if (propName = 'version') then VersionElement := asString(new){5b}
  else if (propName = 'manufactureDate') then ManufactureDateElement := asDateTime(new){5b}
  else if (propName = 'expiry') then ExpiryElement := asDateTime(new){5b}
  else if (propName = 'udi') then UdiElement := asString(new){5b}
  else if (propName = 'lotNumber') then LotNumberElement := asString(new){5b}
  else if (propName = 'owner') then OwnerElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'location') then LocationElement := new as TFhirReference{TFhirLocation}{4}
  else if (propName = 'patient') then PatientElement := new as TFhirReference{TFhirPatient}{4}
  else if (propName = 'contact') then replacePropertyValue('contact', ContactList, existing, new) {2}
  else if (propName = 'url') then UrlElement := asUri(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirDevice.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'note') then NoteList.move(source, destination){2a}
  else if (propName = 'contact') then ContactList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirDevice.fhirType : string;
begin
  result := 'Device';
end;

function TFhirDevice.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FType_) and isEmptyProp(FnoteList) and isEmptyProp(FStatus) and isEmptyProp(FManufacturer) and isEmptyProp(FModel) and isEmptyProp(FVersion) and isEmptyProp(FManufactureDate) and isEmptyProp(FExpiry) and isEmptyProp(FUdi) and isEmptyProp(FLotNumber) and isEmptyProp(FOwner) and isEmptyProp(FLocation) and isEmptyProp(FPatient) and isEmptyProp(FcontactList) and isEmptyProp(FUrl);
end;

function TFhirDevice.equals(other : TObject) : boolean;
var
  o : TFhirDevice;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDevice)) then
    result := false
  else
  begin
    o := TFhirDevice(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(type_Element, o.type_Element, true) and
      compareDeep(noteList, o.noteList, true) and compareDeep(statusElement, o.statusElement, true) and
      compareDeep(manufacturerElement, o.manufacturerElement, true) and compareDeep(modelElement, o.modelElement, true) and
      compareDeep(versionElement, o.versionElement, true) and compareDeep(manufactureDateElement, o.manufactureDateElement, true) and
      compareDeep(expiryElement, o.expiryElement, true) and compareDeep(udiElement, o.udiElement, true) and
      compareDeep(lotNumberElement, o.lotNumberElement, true) and compareDeep(ownerElement, o.ownerElement, true) and
      compareDeep(locationElement, o.locationElement, true) and compareDeep(patientElement, o.patientElement, true) and
      compareDeep(contactList, o.contactList, true) and compareDeep(urlElement, o.urlElement, true);
  end;
end;

function TFhirDevice.Link : TFhirDevice;
begin
  result := TFhirDevice(inherited Link);
end;

function TFhirDevice.Clone : TFhirDevice;
begin
  result := TFhirDevice(inherited Clone);
end;

procedure TFhirDevice.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('type');
  fields.add('note');
  fields.add('status');
  fields.add('manufacturer');
  fields.add('model');
  fields.add('version');
  fields.add('manufactureDate');
  fields.add('expiry');
  fields.add('udi');
  fields.add('lotNumber');
  fields.add('owner');
  fields.add('location');
  fields.add('patient');
  fields.add('contact');
  fields.add('url');
end;

{ TFhirDevice }

Function TFhirDevice.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirDevice.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirDevice.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirDevice.GetNoteList : TFhirAnnotationList;
begin
  if FNoteList = nil then
    FNoteList := TFhirAnnotationList.Create;
  result := FNoteList;
end;

Function TFhirDevice.GetHasNoteList : boolean;
begin
  result := (FNoteList <> nil) and (FNoteList.count > 0);
end;

Procedure TFhirDevice.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirDevice.GetStatusST : TFhirDevicestatusEnum;
begin
  if FStatus = nil then
    result := TFhirDevicestatusEnum(0)
  else
    result := TFhirDevicestatusEnum(StringArrayIndexOfSensitive(CODES_TFhirDevicestatusEnum, FStatus.value));
end;

Procedure TFhirDevice.SetStatusST(value : TFhirDevicestatusEnum);
begin
  if ord(value) = 0 then
    StatusElement := nil
  else
    StatusElement := TFhirEnum.create(SYSTEMS_TFhirDevicestatusEnum[value], CODES_TFhirDevicestatusEnum[value]);
end;

Procedure TFhirDevice.SetManufacturer(value : TFhirString);
begin
  FManufacturer.free;
  FManufacturer := value;
end;

Function TFhirDevice.GetManufacturerST : String;
begin
  if FManufacturer = nil then
    result := ''
  else
    result := FManufacturer.value;
end;

Procedure TFhirDevice.SetManufacturerST(value : String);
begin
  if value <> '' then
  begin
    if FManufacturer = nil then
      FManufacturer := TFhirString.create;
    FManufacturer.value := value
  end
  else if FManufacturer <> nil then
    FManufacturer.value := '';
end;

Procedure TFhirDevice.SetModel(value : TFhirString);
begin
  FModel.free;
  FModel := value;
end;

Function TFhirDevice.GetModelST : String;
begin
  if FModel = nil then
    result := ''
  else
    result := FModel.value;
end;

Procedure TFhirDevice.SetModelST(value : String);
begin
  if value <> '' then
  begin
    if FModel = nil then
      FModel := TFhirString.create;
    FModel.value := value
  end
  else if FModel <> nil then
    FModel.value := '';
end;

Procedure TFhirDevice.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirDevice.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := FVersion.value;
end;

Procedure TFhirDevice.SetVersionST(value : String);
begin
  if value <> '' then
  begin
    if FVersion = nil then
      FVersion := TFhirString.create;
    FVersion.value := value
  end
  else if FVersion <> nil then
    FVersion.value := '';
end;

Procedure TFhirDevice.SetManufactureDate(value : TFhirDateTime);
begin
  FManufactureDate.free;
  FManufactureDate := value;
end;

Function TFhirDevice.GetManufactureDateST : TFslDateTime;
begin
  if FManufactureDate = nil then
    result := TFslDateTime.makeNull
  else
    result := FManufactureDate.value;
end;

Procedure TFhirDevice.SetManufactureDateST(value : TFslDateTime);
begin
  if FManufactureDate = nil then
    FManufactureDate := TFhirDateTime.create;
  FManufactureDate.value := value
end;

Procedure TFhirDevice.SetExpiry(value : TFhirDateTime);
begin
  FExpiry.free;
  FExpiry := value;
end;

Function TFhirDevice.GetExpiryST : TFslDateTime;
begin
  if FExpiry = nil then
    result := TFslDateTime.makeNull
  else
    result := FExpiry.value;
end;

Procedure TFhirDevice.SetExpiryST(value : TFslDateTime);
begin
  if FExpiry = nil then
    FExpiry := TFhirDateTime.create;
  FExpiry.value := value
end;

Procedure TFhirDevice.SetUdi(value : TFhirString);
begin
  FUdi.free;
  FUdi := value;
end;

Function TFhirDevice.GetUdiST : String;
begin
  if FUdi = nil then
    result := ''
  else
    result := FUdi.value;
end;

Procedure TFhirDevice.SetUdiST(value : String);
begin
  if value <> '' then
  begin
    if FUdi = nil then
      FUdi := TFhirString.create;
    FUdi.value := value
  end
  else if FUdi <> nil then
    FUdi.value := '';
end;

Procedure TFhirDevice.SetLotNumber(value : TFhirString);
begin
  FLotNumber.free;
  FLotNumber := value;
end;

Function TFhirDevice.GetLotNumberST : String;
begin
  if FLotNumber = nil then
    result := ''
  else
    result := FLotNumber.value;
end;

Procedure TFhirDevice.SetLotNumberST(value : String);
begin
  if value <> '' then
  begin
    if FLotNumber = nil then
      FLotNumber := TFhirString.create;
    FLotNumber.value := value
  end
  else if FLotNumber <> nil then
    FLotNumber.value := '';
end;

Procedure TFhirDevice.SetOwner(value : TFhirReference{TFhirOrganization});
begin
  FOwner.free;
  FOwner := value;
end;

Procedure TFhirDevice.SetLocation(value : TFhirReference{TFhirLocation});
begin
  FLocation.free;
  FLocation := value;
end;

Procedure TFhirDevice.SetPatient(value : TFhirReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Function TFhirDevice.GetContactList : TFhirContactPointList;
begin
  if FContactList = nil then
    FContactList := TFhirContactPointList.Create;
  result := FContactList;
end;

Function TFhirDevice.GetHasContactList : boolean;
begin
  result := (FContactList <> nil) and (FContactList.count > 0);
end;

Procedure TFhirDevice.SetUrl(value : TFhirUri);
begin
  FUrl.free;
  FUrl := value;
end;

Function TFhirDevice.GetUrlST : String;
begin
  if FUrl = nil then
    result := ''
  else
    result := FUrl.value;
end;

Procedure TFhirDevice.SetUrlST(value : String);
begin
  if value <> '' then
  begin
    if FUrl = nil then
      FUrl := TFhirUri.create;
    FUrl.value := value
  end
  else if FUrl <> nil then
    FUrl.value := '';
end;

function TFhirDevice.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FType_.sizeInBytes);
  inc(result, FnoteList.sizeInBytes);
  inc(result, FStatus.sizeInBytes);
  inc(result, FManufacturer.sizeInBytes);
  inc(result, FModel.sizeInBytes);
  inc(result, FVersion.sizeInBytes);
  inc(result, FManufactureDate.sizeInBytes);
  inc(result, FExpiry.sizeInBytes);
  inc(result, FUdi.sizeInBytes);
  inc(result, FLotNumber.sizeInBytes);
  inc(result, FOwner.sizeInBytes);
  inc(result, FLocation.sizeInBytes);
  inc(result, FPatient.sizeInBytes);
  inc(result, FcontactList.sizeInBytes);
  inc(result, FUrl.sizeInBytes);
end;

{ TFhirDeviceListEnumerator }

Constructor TFhirDeviceListEnumerator.Create(list : TFhirDeviceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDeviceListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDeviceListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDeviceListEnumerator.GetCurrent : TFhirDevice;
begin
  Result := FList[FIndex];
end;

function TFhirDeviceListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirDeviceList }
procedure TFhirDeviceList.AddItem(value: TFhirDevice);
begin
  assert(value.ClassName = 'TFhirDevice', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDevice');
  add(value);
end;

function TFhirDeviceList.Append: TFhirDevice;
begin
  result := TFhirDevice.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceList.ClearItems;
begin
  Clear;
end;

function TFhirDeviceList.GetEnumerator : TFhirDeviceListEnumerator;
begin
  result := TFhirDeviceListEnumerator.Create(self.link);
end;

function TFhirDeviceList.Clone: TFhirDeviceList;
begin
  result := TFhirDeviceList(inherited Clone);
end;

function TFhirDeviceList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDeviceList.GetItemN(index: Integer): TFhirDevice;
begin
  result := TFhirDevice(ObjectByIndex[index]);
end;

function TFhirDeviceList.ItemClass: TFslObjectClass;
begin
  result := TFhirDevice;
end;
function TFhirDeviceList.IndexOf(value: TFhirDevice): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirDeviceList.Insert(index: Integer): TFhirDevice;
begin
  result := TFhirDevice.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceList.InsertItem(index: Integer; value: TFhirDevice);
begin
  assert(value is TFhirDevice);
  Inherited Insert(index, value);
end;

function TFhirDeviceList.Item(index: Integer): TFhirDevice;
begin
  result := TFhirDevice(ObjectByIndex[index]);
end;

function TFhirDeviceList.Link: TFhirDeviceList;
begin
  result := TFhirDeviceList(inherited Link);
end;

procedure TFhirDeviceList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDeviceList.SetItemByIndex(index: Integer; value: TFhirDevice);
begin
  assert(value is TFhirDevice);
  FhirDevices[index] := value;
end;

procedure TFhirDeviceList.SetItemN(index: Integer; value: TFhirDevice);
begin
  assert(value is TFhirDevice);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_DEVICE}

{$IFDEF FHIR_DEVICECOMPONENT}

{ TFhirDeviceComponentProductionSpecification }

constructor TFhirDeviceComponentProductionSpecification.Create;
begin
  inherited;
end;

destructor TFhirDeviceComponentProductionSpecification.Destroy;
begin
  FSpecType.free;
  FComponentId.free;
  FProductionSpec.free;
  inherited;
end;

procedure TFhirDeviceComponentProductionSpecification.Assign(oSource : TFslObject);
begin
  inherited;
  specType := TFhirDeviceComponentProductionSpecification(oSource).specType.Clone;
  componentId := TFhirDeviceComponentProductionSpecification(oSource).componentId.Clone;
  productionSpecElement := TFhirDeviceComponentProductionSpecification(oSource).productionSpecElement.Clone;
end;

procedure TFhirDeviceComponentProductionSpecification.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'specType') Then
     list.add(self.link, 'specType', FSpecType.Link);
  if (child_name = 'componentId') Then
     list.add(self.link, 'componentId', FComponentId.Link);
  if (child_name = 'productionSpec') Then
     list.add(self.link, 'productionSpec', FProductionSpec.Link);
end;

procedure TFhirDeviceComponentProductionSpecification.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'specType', 'CodeableConcept', false, TFhirCodeableConcept, FSpecType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'componentId', 'Identifier', false, TFhirIdentifier, FComponentId.Link));{2}
  oList.add(TFHIRProperty.create(self, 'productionSpec', 'string', false, TFhirString, FProductionSpec.Link));{2}
end;

function TFhirDeviceComponentProductionSpecification.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'specType') then
  begin
    SpecType := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'componentId') then
  begin
    ComponentId := propValue as TFhirIdentifier{4b};
    result := propValue;
  end
  else if (propName = 'productionSpec') then
  begin
    ProductionSpecElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirDeviceComponentProductionSpecification.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirDeviceComponentProductionSpecification.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'specType') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'componentId') then result := TFhirIdentifier.create(){4b}
  else if (propName = 'productionSpec') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirDeviceComponentProductionSpecification.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'specType') then result := 'CodeableConcept'
  else if (propName = 'componentId') then result := 'Identifier'
  else if (propName = 'productionSpec') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirDeviceComponentProductionSpecification.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'specType') then SpecTypeElement := nil
  else if (propName = 'componentId') then ComponentIdElement := nil
  else if (propName = 'productionSpec') then ProductionSpecElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirDeviceComponentProductionSpecification.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'specType') then SpecTypeElement := new as TFhirCodeableConcept{4}
  else if (propName = 'componentId') then ComponentIdElement := new as TFhirIdentifier{4}
  else if (propName = 'productionSpec') then ProductionSpecElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirDeviceComponentProductionSpecification.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirDeviceComponentProductionSpecification.fhirType : string;
begin
  result := 'productionSpecification';
end;

function TFhirDeviceComponentProductionSpecification.Link : TFhirDeviceComponentProductionSpecification;
begin
  result := TFhirDeviceComponentProductionSpecification(inherited Link);
end;

function TFhirDeviceComponentProductionSpecification.Clone : TFhirDeviceComponentProductionSpecification;
begin
  result := TFhirDeviceComponentProductionSpecification(inherited Clone);
end;

function TFhirDeviceComponentProductionSpecification.equals(other : TObject) : boolean;
var
  o : TFhirDeviceComponentProductionSpecification;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDeviceComponentProductionSpecification)) then
    result := false
  else
  begin
    o := TFhirDeviceComponentProductionSpecification(other);
    result := compareDeep(specTypeElement, o.specTypeElement, true) and compareDeep(componentIdElement, o.componentIdElement, true) and
      compareDeep(productionSpecElement, o.productionSpecElement, true);
  end;
end;

function TFhirDeviceComponentProductionSpecification.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FSpecType) and isEmptyProp(FComponentId) and isEmptyProp(FProductionSpec);
end;

procedure TFhirDeviceComponentProductionSpecification.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('specType');
  fields.add('componentId');
  fields.add('productionSpec');
end;

{ TFhirDeviceComponentProductionSpecification }

Procedure TFhirDeviceComponentProductionSpecification.SetSpecType(value : TFhirCodeableConcept);
begin
  FSpecType.free;
  FSpecType := value;
end;

Procedure TFhirDeviceComponentProductionSpecification.SetComponentId(value : TFhirIdentifier);
begin
  FComponentId.free;
  FComponentId := value;
end;

Procedure TFhirDeviceComponentProductionSpecification.SetProductionSpec(value : TFhirString);
begin
  FProductionSpec.free;
  FProductionSpec := value;
end;

Function TFhirDeviceComponentProductionSpecification.GetProductionSpecST : String;
begin
  if FProductionSpec = nil then
    result := ''
  else
    result := FProductionSpec.value;
end;

Procedure TFhirDeviceComponentProductionSpecification.SetProductionSpecST(value : String);
begin
  if value <> '' then
  begin
    if FProductionSpec = nil then
      FProductionSpec := TFhirString.create;
    FProductionSpec.value := value
  end
  else if FProductionSpec <> nil then
    FProductionSpec.value := '';
end;

function TFhirDeviceComponentProductionSpecification.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSpecType.sizeInBytes);
  inc(result, FComponentId.sizeInBytes);
  inc(result, FProductionSpec.sizeInBytes);
end;

{ TFhirDeviceComponentProductionSpecificationListEnumerator }

Constructor TFhirDeviceComponentProductionSpecificationListEnumerator.Create(list : TFhirDeviceComponentProductionSpecificationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDeviceComponentProductionSpecificationListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDeviceComponentProductionSpecificationListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDeviceComponentProductionSpecificationListEnumerator.GetCurrent : TFhirDeviceComponentProductionSpecification;
begin
  Result := FList[FIndex];
end;

function TFhirDeviceComponentProductionSpecificationListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirDeviceComponentProductionSpecificationList }
procedure TFhirDeviceComponentProductionSpecificationList.AddItem(value: TFhirDeviceComponentProductionSpecification);
begin
  assert(value.ClassName = 'TFhirDeviceComponentProductionSpecification', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDeviceComponentProductionSpecification');
  add(value);
end;

function TFhirDeviceComponentProductionSpecificationList.Append: TFhirDeviceComponentProductionSpecification;
begin
  result := TFhirDeviceComponentProductionSpecification.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceComponentProductionSpecificationList.ClearItems;
begin
  Clear;
end;

function TFhirDeviceComponentProductionSpecificationList.GetEnumerator : TFhirDeviceComponentProductionSpecificationListEnumerator;
begin
  result := TFhirDeviceComponentProductionSpecificationListEnumerator.Create(self.link);
end;

function TFhirDeviceComponentProductionSpecificationList.Clone: TFhirDeviceComponentProductionSpecificationList;
begin
  result := TFhirDeviceComponentProductionSpecificationList(inherited Clone);
end;

function TFhirDeviceComponentProductionSpecificationList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDeviceComponentProductionSpecificationList.GetItemN(index: Integer): TFhirDeviceComponentProductionSpecification;
begin
  result := TFhirDeviceComponentProductionSpecification(ObjectByIndex[index]);
end;

function TFhirDeviceComponentProductionSpecificationList.ItemClass: TFslObjectClass;
begin
  result := TFhirDeviceComponentProductionSpecification;
end;
function TFhirDeviceComponentProductionSpecificationList.IndexOf(value: TFhirDeviceComponentProductionSpecification): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirDeviceComponentProductionSpecificationList.Insert(index: Integer): TFhirDeviceComponentProductionSpecification;
begin
  result := TFhirDeviceComponentProductionSpecification.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceComponentProductionSpecificationList.InsertItem(index: Integer; value: TFhirDeviceComponentProductionSpecification);
begin
  assert(value is TFhirDeviceComponentProductionSpecification);
  Inherited Insert(index, value);
end;

function TFhirDeviceComponentProductionSpecificationList.Item(index: Integer): TFhirDeviceComponentProductionSpecification;
begin
  result := TFhirDeviceComponentProductionSpecification(ObjectByIndex[index]);
end;

function TFhirDeviceComponentProductionSpecificationList.Link: TFhirDeviceComponentProductionSpecificationList;
begin
  result := TFhirDeviceComponentProductionSpecificationList(inherited Link);
end;

procedure TFhirDeviceComponentProductionSpecificationList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDeviceComponentProductionSpecificationList.SetItemByIndex(index: Integer; value: TFhirDeviceComponentProductionSpecification);
begin
  assert(value is TFhirDeviceComponentProductionSpecification);
  FhirDeviceComponentProductionSpecifications[index] := value;
end;

procedure TFhirDeviceComponentProductionSpecificationList.SetItemN(index: Integer; value: TFhirDeviceComponentProductionSpecification);
begin
  assert(value is TFhirDeviceComponentProductionSpecification);
  ObjectByIndex[index] := value;
end;

{ TFhirDeviceComponent }

constructor TFhirDeviceComponent.Create;
begin
  inherited;
end;

destructor TFhirDeviceComponent.Destroy;
begin
  FType_.free;
  FIdentifier.free;
  FLastSystemChange.free;
  FSource.free;
  FParent.free;
  FOperationalStatusList.Free;
  FParameterGroup.free;
  FMeasurementPrinciple.free;
  FProductionSpecificationList.Free;
  FLanguageCode.free;
  inherited;
end;

function TFhirDeviceComponent.GetResourceType : TFhirResourceType;
begin
  result := frtDeviceComponent;
end;

procedure TFhirDeviceComponent.Assign(oSource : TFslObject);
begin
  inherited;
  type_ := TFhirDeviceComponent(oSource).type_.Clone;
  identifier := TFhirDeviceComponent(oSource).identifier.Clone;
  lastSystemChangeElement := TFhirDeviceComponent(oSource).lastSystemChangeElement.Clone;
  source := TFhirDeviceComponent(oSource).source.Clone;
  parent := TFhirDeviceComponent(oSource).parent.Clone;
  if (TFhirDeviceComponent(oSource).FOperationalStatusList = nil) then
  begin
    FOperationalStatusList.free;
    FOperationalStatusList := nil;
  end
  else
  begin
    if FOperationalStatusList = nil then
      FOperationalStatusList := TFhirCodeableConceptList.Create;
    FOperationalStatusList.Assign(TFhirDeviceComponent(oSource).FOperationalStatusList);
  end;
  parameterGroup := TFhirDeviceComponent(oSource).parameterGroup.Clone;
  FMeasurementPrinciple := TFhirDeviceComponent(oSource).FMeasurementPrinciple.Link;
  if (TFhirDeviceComponent(oSource).FProductionSpecificationList = nil) then
  begin
    FProductionSpecificationList.free;
    FProductionSpecificationList := nil;
  end
  else
  begin
    if FProductionSpecificationList = nil then
      FProductionSpecificationList := TFhirDeviceComponentProductionSpecificationList.Create;
    FProductionSpecificationList.Assign(TFhirDeviceComponent(oSource).FProductionSpecificationList);
  end;
  languageCode := TFhirDeviceComponent(oSource).languageCode.Clone;
end;

procedure TFhirDeviceComponent.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'identifier') Then
     list.add(self.link, 'identifier', FIdentifier.Link);
  if (child_name = 'lastSystemChange') Then
     list.add(self.link, 'lastSystemChange', FLastSystemChange.Link);
  if (child_name = 'source') Then
     list.add(self.link, 'source', FSource.Link);
  if (child_name = 'parent') Then
     list.add(self.link, 'parent', FParent.Link);
  if (child_name = 'operationalStatus') Then
    list.addAll(self, 'operationalStatus', FOperationalStatusList);
  if (child_name = 'parameterGroup') Then
     list.add(self.link, 'parameterGroup', FParameterGroup.Link);
  if (child_name = 'measurementPrinciple') Then
     list.add(self.link, 'measurementPrinciple', FMeasurementPrinciple.Link);
  if (child_name = 'productionSpecification') Then
    list.addAll(self, 'productionSpecification', FProductionSpecificationList);
  if (child_name = 'languageCode') Then
     list.add(self.link, 'languageCode', FLanguageCode.Link);
end;

procedure TFhirDeviceComponent.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', false, TFhirCodeableConcept, FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', false, TFhirIdentifier, FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lastSystemChange', 'instant', false, TFhirInstant, FLastSystemChange.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Reference(Device)', false, TFhirReference{TFhirDevice}, FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'parent', 'Reference(DeviceComponent)', false, TFhirReference{TFhirDeviceComponent}, FParent.Link));{2}
  oList.add(TFHIRProperty.create(self, 'operationalStatus', 'CodeableConcept', true, TFhirCodeableConcept, FOperationalStatusList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'parameterGroup', 'CodeableConcept', false, TFhirCodeableConcept, FParameterGroup.Link));{2}
  oList.add(TFHIRProperty.create(self, 'measurementPrinciple', 'code', false, TFHIREnum, FMeasurementPrinciple.Link));{1}
  oList.add(TFHIRProperty.create(self, 'productionSpecification', '', true, TFhirDeviceComponentProductionSpecification, FProductionSpecificationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'languageCode', 'CodeableConcept', false, TFhirCodeableConcept, FLanguageCode.Link));{2}
end;

function TFhirDeviceComponent.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'type') then
  begin
    Type_ := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'identifier') then
  begin
    Identifier := propValue as TFhirIdentifier{4b};
    result := propValue;
  end
  else if (propName = 'lastSystemChange') then
  begin
    LastSystemChangeElement := asInstant(propValue){5a};
    result := propValue;
  end
  else if (propName = 'source') then
  begin
    Source := propValue as TFhirReference{TFhirDevice}{4b};
    result := propValue;
  end
  else if (propName = 'parent') then
  begin
    Parent := propValue as TFhirReference{TFhirDeviceComponent}{4b};
    result := propValue;
  end
  else if (propName = 'operationalStatus') then
  begin
    OperationalStatusList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'parameterGroup') then
  begin
    ParameterGroup := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'measurementPrinciple') then
  begin
    MeasurementPrincipleElement := asEnum(SYSTEMS_TFhirMeasurementPrincipleEnum, CODES_TFhirMeasurementPrincipleEnum, propValue);
    result := propValue
  end
  else if (propName = 'productionSpecification') then
  begin
    ProductionSpecificationList.add(propValue as TFhirDeviceComponentProductionSpecification){2a};
    result := propValue;
  end
  else if (propName = 'languageCode') then
  begin
    LanguageCode := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirDeviceComponent.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'operationalStatus') then OperationalStatusList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'productionSpecification') then ProductionSpecificationList.insertItem(index, propValue as TFhirDeviceComponentProductionSpecification){2a}
  else inherited;
end;

function TFhirDeviceComponent.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'type') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'identifier') then result := TFhirIdentifier.create(){4b}
  else if (propName = 'lastSystemChange') then result := TFhirInstant.create() {5b}
  else if (propName = 'source') then result := TFhirReference{TFhirDevice}.create(){4b}
  else if (propName = 'parent') then result := TFhirReference{TFhirDeviceComponent}.create(){4b}
  else if (propName = 'operationalStatus') then result := OperationalStatusList.new(){2}
  else if (propName = 'parameterGroup') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'productionSpecification') then result := ProductionSpecificationList.new(){2}
  else if (propName = 'languageCode') then result := TFhirCodeableConcept.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirDeviceComponent.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'lastSystemChange') then result := 'instant'
  else if (propName = 'source') then result := 'Reference'
  else if (propName = 'parent') then result := 'Reference'
  else if (propName = 'operationalStatus') then result := 'CodeableConcept'
  else if (propName = 'parameterGroup') then result := 'CodeableConcept'
  else if (propName = 'measurementPrinciple') then result := 'code'
  else if (propName = 'productionSpecification') then result := ''
  else if (propName = 'languageCode') then result := 'CodeableConcept'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirDeviceComponent.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'type') then Type_Element := nil
  else if (propName = 'identifier') then IdentifierElement := nil
  else if (propName = 'lastSystemChange') then LastSystemChangeElement := nil
  else if (propName = 'source') then SourceElement := nil
  else if (propName = 'parent') then ParentElement := nil
  else if (propName = 'operationalStatus') then deletePropertyValue('operationalStatus', OperationalStatusList, value) {2}
  else if (propName = 'parameterGroup') then ParameterGroupElement := nil
  else if (propName = 'measurementPrinciple') then MeasurementPrincipleElement := nil
  else if (propName = 'productionSpecification') then deletePropertyValue('productionSpecification', ProductionSpecificationList, value) {2}
  else if (propName = 'languageCode') then LanguageCodeElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirDeviceComponent.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'type') then Type_Element := new as TFhirCodeableConcept{4}
  else if (propName = 'identifier') then IdentifierElement := new as TFhirIdentifier{4}
  else if (propName = 'lastSystemChange') then LastSystemChangeElement := asInstant(new){5b}
  else if (propName = 'source') then SourceElement := new as TFhirReference{TFhirDevice}{4}
  else if (propName = 'parent') then ParentElement := new as TFhirReference{TFhirDeviceComponent}{4}
  else if (propName = 'operationalStatus') then replacePropertyValue('operationalStatus', OperationalStatusList, existing, new) {2}
  else if (propName = 'parameterGroup') then ParameterGroupElement := new as TFhirCodeableConcept{4}
  else if (propName = 'measurementPrinciple') then MeasurementPrincipleElement := asEnum(SYSTEMS_TFhirMeasurementPrincipleEnum, CODES_TFhirMeasurementPrincipleEnum, new){4}
  else if (propName = 'productionSpecification') then replacePropertyValue('productionSpecification', ProductionSpecificationList, existing, new) {2}
  else if (propName = 'languageCode') then LanguageCodeElement := new as TFhirCodeableConcept{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirDeviceComponent.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'operationalStatus') then OperationalStatusList.move(source, destination){2a}
  else if (propName = 'productionSpecification') then ProductionSpecificationList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirDeviceComponent.fhirType : string;
begin
  result := 'DeviceComponent';
end;

function TFhirDeviceComponent.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FType_) and isEmptyProp(FIdentifier) and isEmptyProp(FLastSystemChange) and isEmptyProp(FSource) and isEmptyProp(FParent) and isEmptyProp(FoperationalStatusList) and isEmptyProp(FParameterGroup) and isEmptyProp(FMeasurementPrinciple) and isEmptyProp(FproductionSpecificationList) and isEmptyProp(FLanguageCode);
end;

function TFhirDeviceComponent.equals(other : TObject) : boolean;
var
  o : TFhirDeviceComponent;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDeviceComponent)) then
    result := false
  else
  begin
    o := TFhirDeviceComponent(other);
    result := compareDeep(type_Element, o.type_Element, true) and compareDeep(identifierElement, o.identifierElement, true) and
      compareDeep(lastSystemChangeElement, o.lastSystemChangeElement, true) and compareDeep(sourceElement, o.sourceElement, true) and
      compareDeep(parentElement, o.parentElement, true) and compareDeep(operationalStatusList, o.operationalStatusList, true) and
      compareDeep(parameterGroupElement, o.parameterGroupElement, true) and compareDeep(measurementPrincipleElement, o.measurementPrincipleElement, true) and
      compareDeep(productionSpecificationList, o.productionSpecificationList, true) and
      compareDeep(languageCodeElement, o.languageCodeElement, true);
  end;
end;

function TFhirDeviceComponent.Link : TFhirDeviceComponent;
begin
  result := TFhirDeviceComponent(inherited Link);
end;

function TFhirDeviceComponent.Clone : TFhirDeviceComponent;
begin
  result := TFhirDeviceComponent(inherited Clone);
end;

procedure TFhirDeviceComponent.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('type');
  fields.add('identifier');
  fields.add('lastSystemChange');
  fields.add('source');
  fields.add('parent');
  fields.add('operationalStatus');
  fields.add('parameterGroup');
  fields.add('measurementPrinciple');
  fields.add('productionSpecification');
  fields.add('languageCode');
end;

{ TFhirDeviceComponent }

Procedure TFhirDeviceComponent.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirDeviceComponent.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirDeviceComponent.SetLastSystemChange(value : TFhirInstant);
begin
  FLastSystemChange.free;
  FLastSystemChange := value;
end;

Function TFhirDeviceComponent.GetLastSystemChangeST : TFslDateTime;
begin
  if FLastSystemChange = nil then
    result := TFslDateTime.makeNull
  else
    result := FLastSystemChange.value;
end;

Procedure TFhirDeviceComponent.SetLastSystemChangeST(value : TFslDateTime);
begin
  if FLastSystemChange = nil then
    FLastSystemChange := TFhirInstant.create;
  FLastSystemChange.value := value
end;

Procedure TFhirDeviceComponent.SetSource(value : TFhirReference{TFhirDevice});
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirDeviceComponent.SetParent(value : TFhirReference{TFhirDeviceComponent});
begin
  FParent.free;
  FParent := value;
end;

Function TFhirDeviceComponent.GetOperationalStatusList : TFhirCodeableConceptList;
begin
  if FOperationalStatusList = nil then
    FOperationalStatusList := TFhirCodeableConceptList.Create;
  result := FOperationalStatusList;
end;

Function TFhirDeviceComponent.GetHasOperationalStatusList : boolean;
begin
  result := (FOperationalStatusList <> nil) and (FOperationalStatusList.count > 0);
end;

Procedure TFhirDeviceComponent.SetParameterGroup(value : TFhirCodeableConcept);
begin
  FParameterGroup.free;
  FParameterGroup := value;
end;

Procedure TFhirDeviceComponent.SetMeasurementPrinciple(value : TFhirEnum);
begin
  FMeasurementPrinciple.free;
  FMeasurementPrinciple := value;
end;

Function TFhirDeviceComponent.GetMeasurementPrincipleST : TFhirMeasurementPrincipleEnum;
begin
  if FMeasurementPrinciple = nil then
    result := TFhirMeasurementPrincipleEnum(0)
  else
    result := TFhirMeasurementPrincipleEnum(StringArrayIndexOfSensitive(CODES_TFhirMeasurementPrincipleEnum, FMeasurementPrinciple.value));
end;

Procedure TFhirDeviceComponent.SetMeasurementPrincipleST(value : TFhirMeasurementPrincipleEnum);
begin
  if ord(value) = 0 then
    MeasurementPrincipleElement := nil
  else
    MeasurementPrincipleElement := TFhirEnum.create(SYSTEMS_TFhirMeasurementPrincipleEnum[value], CODES_TFhirMeasurementPrincipleEnum[value]);
end;

Function TFhirDeviceComponent.GetProductionSpecificationList : TFhirDeviceComponentProductionSpecificationList;
begin
  if FProductionSpecificationList = nil then
    FProductionSpecificationList := TFhirDeviceComponentProductionSpecificationList.Create;
  result := FProductionSpecificationList;
end;

Function TFhirDeviceComponent.GetHasProductionSpecificationList : boolean;
begin
  result := (FProductionSpecificationList <> nil) and (FProductionSpecificationList.count > 0);
end;

Procedure TFhirDeviceComponent.SetLanguageCode(value : TFhirCodeableConcept);
begin
  FLanguageCode.free;
  FLanguageCode := value;
end;

function TFhirDeviceComponent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FType_.sizeInBytes);
  inc(result, FIdentifier.sizeInBytes);
  inc(result, FLastSystemChange.sizeInBytes);
  inc(result, FSource.sizeInBytes);
  inc(result, FoperationalStatusList.sizeInBytes);
  inc(result, FParameterGroup.sizeInBytes);
  inc(result, FMeasurementPrinciple.sizeInBytes);
  inc(result, FproductionSpecificationList.sizeInBytes);
  inc(result, FLanguageCode.sizeInBytes);
end;

{ TFhirDeviceComponentListEnumerator }

Constructor TFhirDeviceComponentListEnumerator.Create(list : TFhirDeviceComponentList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDeviceComponentListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDeviceComponentListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDeviceComponentListEnumerator.GetCurrent : TFhirDeviceComponent;
begin
  Result := FList[FIndex];
end;

function TFhirDeviceComponentListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirDeviceComponentList }
procedure TFhirDeviceComponentList.AddItem(value: TFhirDeviceComponent);
begin
  assert(value.ClassName = 'TFhirDeviceComponent', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDeviceComponent');
  add(value);
end;

function TFhirDeviceComponentList.Append: TFhirDeviceComponent;
begin
  result := TFhirDeviceComponent.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceComponentList.ClearItems;
begin
  Clear;
end;

function TFhirDeviceComponentList.GetEnumerator : TFhirDeviceComponentListEnumerator;
begin
  result := TFhirDeviceComponentListEnumerator.Create(self.link);
end;

function TFhirDeviceComponentList.Clone: TFhirDeviceComponentList;
begin
  result := TFhirDeviceComponentList(inherited Clone);
end;

function TFhirDeviceComponentList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDeviceComponentList.GetItemN(index: Integer): TFhirDeviceComponent;
begin
  result := TFhirDeviceComponent(ObjectByIndex[index]);
end;

function TFhirDeviceComponentList.ItemClass: TFslObjectClass;
begin
  result := TFhirDeviceComponent;
end;
function TFhirDeviceComponentList.IndexOf(value: TFhirDeviceComponent): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirDeviceComponentList.Insert(index: Integer): TFhirDeviceComponent;
begin
  result := TFhirDeviceComponent.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceComponentList.InsertItem(index: Integer; value: TFhirDeviceComponent);
begin
  assert(value is TFhirDeviceComponent);
  Inherited Insert(index, value);
end;

function TFhirDeviceComponentList.Item(index: Integer): TFhirDeviceComponent;
begin
  result := TFhirDeviceComponent(ObjectByIndex[index]);
end;

function TFhirDeviceComponentList.Link: TFhirDeviceComponentList;
begin
  result := TFhirDeviceComponentList(inherited Link);
end;

procedure TFhirDeviceComponentList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDeviceComponentList.SetItemByIndex(index: Integer; value: TFhirDeviceComponent);
begin
  assert(value is TFhirDeviceComponent);
  FhirDeviceComponents[index] := value;
end;

procedure TFhirDeviceComponentList.SetItemN(index: Integer; value: TFhirDeviceComponent);
begin
  assert(value is TFhirDeviceComponent);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_DEVICECOMPONENT}

{$IFDEF FHIR_DEVICEMETRIC}

{ TFhirDeviceMetricCalibration }

constructor TFhirDeviceMetricCalibration.Create;
begin
  inherited;
end;

destructor TFhirDeviceMetricCalibration.Destroy;
begin
  FType_.free;
  FState.free;
  FTime.free;
  inherited;
end;

procedure TFhirDeviceMetricCalibration.Assign(oSource : TFslObject);
begin
  inherited;
  FType_ := TFhirDeviceMetricCalibration(oSource).FType_.Link;
  FState := TFhirDeviceMetricCalibration(oSource).FState.Link;
  timeElement := TFhirDeviceMetricCalibration(oSource).timeElement.Clone;
end;

procedure TFhirDeviceMetricCalibration.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'state') Then
     list.add(self.link, 'state', FState.Link);
  if (child_name = 'time') Then
     list.add(self.link, 'time', FTime.Link);
end;

procedure TFhirDeviceMetricCalibration.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'code', false, TFHIREnum, FType_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'state', 'code', false, TFHIREnum, FState.Link));{1}
  oList.add(TFHIRProperty.create(self, 'time', 'instant', false, TFhirInstant, FTime.Link));{2}
end;

function TFhirDeviceMetricCalibration.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'type') then
  begin
    Type_Element := asEnum(SYSTEMS_TFhirMetricCalibrationTypeEnum, CODES_TFhirMetricCalibrationTypeEnum, propValue);
    result := propValue
  end
  else if (propName = 'state') then
  begin
    StateElement := asEnum(SYSTEMS_TFhirMetricCalibrationStateEnum, CODES_TFhirMetricCalibrationStateEnum, propValue);
    result := propValue
  end
  else if (propName = 'time') then
  begin
    TimeElement := asInstant(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirDeviceMetricCalibration.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirDeviceMetricCalibration.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'time') then result := TFhirInstant.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirDeviceMetricCalibration.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'type') then result := 'code'
  else if (propName = 'state') then result := 'code'
  else if (propName = 'time') then result := 'instant'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirDeviceMetricCalibration.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'type') then Type_Element := nil
  else if (propName = 'state') then StateElement := nil
  else if (propName = 'time') then TimeElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirDeviceMetricCalibration.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'type') then Type_Element := asEnum(SYSTEMS_TFhirMetricCalibrationTypeEnum, CODES_TFhirMetricCalibrationTypeEnum, new){4}
  else if (propName = 'state') then StateElement := asEnum(SYSTEMS_TFhirMetricCalibrationStateEnum, CODES_TFhirMetricCalibrationStateEnum, new){4}
  else if (propName = 'time') then TimeElement := asInstant(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirDeviceMetricCalibration.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirDeviceMetricCalibration.fhirType : string;
begin
  result := 'calibration';
end;

function TFhirDeviceMetricCalibration.Link : TFhirDeviceMetricCalibration;
begin
  result := TFhirDeviceMetricCalibration(inherited Link);
end;

function TFhirDeviceMetricCalibration.Clone : TFhirDeviceMetricCalibration;
begin
  result := TFhirDeviceMetricCalibration(inherited Clone);
end;

function TFhirDeviceMetricCalibration.equals(other : TObject) : boolean;
var
  o : TFhirDeviceMetricCalibration;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDeviceMetricCalibration)) then
    result := false
  else
  begin
    o := TFhirDeviceMetricCalibration(other);
    result := compareDeep(type_Element, o.type_Element, true) and compareDeep(stateElement, o.stateElement, true) and
      compareDeep(timeElement, o.timeElement, true);
  end;
end;

function TFhirDeviceMetricCalibration.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FType_) and isEmptyProp(FState) and isEmptyProp(FTime);
end;

procedure TFhirDeviceMetricCalibration.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('type');
  fields.add('state');
  fields.add('time');
end;

{ TFhirDeviceMetricCalibration }

Procedure TFhirDeviceMetricCalibration.SetType_(value : TFhirEnum);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirDeviceMetricCalibration.GetType_ST : TFhirMetricCalibrationTypeEnum;
begin
  if FType_ = nil then
    result := TFhirMetricCalibrationTypeEnum(0)
  else
    result := TFhirMetricCalibrationTypeEnum(StringArrayIndexOfSensitive(CODES_TFhirMetricCalibrationTypeEnum, FType_.value));
end;

Procedure TFhirDeviceMetricCalibration.SetType_ST(value : TFhirMetricCalibrationTypeEnum);
begin
  if ord(value) = 0 then
    Type_Element := nil
  else
    Type_Element := TFhirEnum.create(SYSTEMS_TFhirMetricCalibrationTypeEnum[value], CODES_TFhirMetricCalibrationTypeEnum[value]);
end;

Procedure TFhirDeviceMetricCalibration.SetState(value : TFhirEnum);
begin
  FState.free;
  FState := value;
end;

Function TFhirDeviceMetricCalibration.GetStateST : TFhirMetricCalibrationStateEnum;
begin
  if FState = nil then
    result := TFhirMetricCalibrationStateEnum(0)
  else
    result := TFhirMetricCalibrationStateEnum(StringArrayIndexOfSensitive(CODES_TFhirMetricCalibrationStateEnum, FState.value));
end;

Procedure TFhirDeviceMetricCalibration.SetStateST(value : TFhirMetricCalibrationStateEnum);
begin
  if ord(value) = 0 then
    StateElement := nil
  else
    StateElement := TFhirEnum.create(SYSTEMS_TFhirMetricCalibrationStateEnum[value], CODES_TFhirMetricCalibrationStateEnum[value]);
end;

Procedure TFhirDeviceMetricCalibration.SetTime(value : TFhirInstant);
begin
  FTime.free;
  FTime := value;
end;

Function TFhirDeviceMetricCalibration.GetTimeST : TFslDateTime;
begin
  if FTime = nil then
    result := TFslDateTime.makeNull
  else
    result := FTime.value;
end;

Procedure TFhirDeviceMetricCalibration.SetTimeST(value : TFslDateTime);
begin
  if FTime = nil then
    FTime := TFhirInstant.create;
  FTime.value := value
end;

function TFhirDeviceMetricCalibration.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FType_.sizeInBytes);
  inc(result, FState.sizeInBytes);
  inc(result, FTime.sizeInBytes);
end;

{ TFhirDeviceMetricCalibrationListEnumerator }

Constructor TFhirDeviceMetricCalibrationListEnumerator.Create(list : TFhirDeviceMetricCalibrationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDeviceMetricCalibrationListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDeviceMetricCalibrationListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDeviceMetricCalibrationListEnumerator.GetCurrent : TFhirDeviceMetricCalibration;
begin
  Result := FList[FIndex];
end;

function TFhirDeviceMetricCalibrationListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirDeviceMetricCalibrationList }
procedure TFhirDeviceMetricCalibrationList.AddItem(value: TFhirDeviceMetricCalibration);
begin
  assert(value.ClassName = 'TFhirDeviceMetricCalibration', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDeviceMetricCalibration');
  add(value);
end;

function TFhirDeviceMetricCalibrationList.Append: TFhirDeviceMetricCalibration;
begin
  result := TFhirDeviceMetricCalibration.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceMetricCalibrationList.ClearItems;
begin
  Clear;
end;

function TFhirDeviceMetricCalibrationList.GetEnumerator : TFhirDeviceMetricCalibrationListEnumerator;
begin
  result := TFhirDeviceMetricCalibrationListEnumerator.Create(self.link);
end;

function TFhirDeviceMetricCalibrationList.Clone: TFhirDeviceMetricCalibrationList;
begin
  result := TFhirDeviceMetricCalibrationList(inherited Clone);
end;

function TFhirDeviceMetricCalibrationList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDeviceMetricCalibrationList.GetItemN(index: Integer): TFhirDeviceMetricCalibration;
begin
  result := TFhirDeviceMetricCalibration(ObjectByIndex[index]);
end;

function TFhirDeviceMetricCalibrationList.ItemClass: TFslObjectClass;
begin
  result := TFhirDeviceMetricCalibration;
end;
function TFhirDeviceMetricCalibrationList.IndexOf(value: TFhirDeviceMetricCalibration): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirDeviceMetricCalibrationList.Insert(index: Integer): TFhirDeviceMetricCalibration;
begin
  result := TFhirDeviceMetricCalibration.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceMetricCalibrationList.InsertItem(index: Integer; value: TFhirDeviceMetricCalibration);
begin
  assert(value is TFhirDeviceMetricCalibration);
  Inherited Insert(index, value);
end;

function TFhirDeviceMetricCalibrationList.Item(index: Integer): TFhirDeviceMetricCalibration;
begin
  result := TFhirDeviceMetricCalibration(ObjectByIndex[index]);
end;

function TFhirDeviceMetricCalibrationList.Link: TFhirDeviceMetricCalibrationList;
begin
  result := TFhirDeviceMetricCalibrationList(inherited Link);
end;

procedure TFhirDeviceMetricCalibrationList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDeviceMetricCalibrationList.SetItemByIndex(index: Integer; value: TFhirDeviceMetricCalibration);
begin
  assert(value is TFhirDeviceMetricCalibration);
  FhirDeviceMetricCalibrations[index] := value;
end;

procedure TFhirDeviceMetricCalibrationList.SetItemN(index: Integer; value: TFhirDeviceMetricCalibration);
begin
  assert(value is TFhirDeviceMetricCalibration);
  ObjectByIndex[index] := value;
end;

{ TFhirDeviceMetric }

constructor TFhirDeviceMetric.Create;
begin
  inherited;
end;

destructor TFhirDeviceMetric.Destroy;
begin
  FType_.free;
  FIdentifier.free;
  FUnit_.free;
  FSource.free;
  FParent.free;
  FOperationalStatus.free;
  FColor.free;
  FCategory.free;
  FMeasurementPeriod.free;
  FCalibrationList.Free;
  inherited;
end;

function TFhirDeviceMetric.GetResourceType : TFhirResourceType;
begin
  result := frtDeviceMetric;
end;

procedure TFhirDeviceMetric.Assign(oSource : TFslObject);
begin
  inherited;
  type_ := TFhirDeviceMetric(oSource).type_.Clone;
  identifier := TFhirDeviceMetric(oSource).identifier.Clone;
  unit_ := TFhirDeviceMetric(oSource).unit_.Clone;
  source := TFhirDeviceMetric(oSource).source.Clone;
  parent := TFhirDeviceMetric(oSource).parent.Clone;
  FOperationalStatus := TFhirDeviceMetric(oSource).FOperationalStatus.Link;
  FColor := TFhirDeviceMetric(oSource).FColor.Link;
  FCategory := TFhirDeviceMetric(oSource).FCategory.Link;
  measurementPeriod := TFhirDeviceMetric(oSource).measurementPeriod.Clone;
  if (TFhirDeviceMetric(oSource).FCalibrationList = nil) then
  begin
    FCalibrationList.free;
    FCalibrationList := nil;
  end
  else
  begin
    if FCalibrationList = nil then
      FCalibrationList := TFhirDeviceMetricCalibrationList.Create;
    FCalibrationList.Assign(TFhirDeviceMetric(oSource).FCalibrationList);
  end;
end;

procedure TFhirDeviceMetric.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'identifier') Then
     list.add(self.link, 'identifier', FIdentifier.Link);
  if (child_name = 'unit') Then
     list.add(self.link, 'unit', FUnit_.Link);
  if (child_name = 'source') Then
     list.add(self.link, 'source', FSource.Link);
  if (child_name = 'parent') Then
     list.add(self.link, 'parent', FParent.Link);
  if (child_name = 'operationalStatus') Then
     list.add(self.link, 'operationalStatus', FOperationalStatus.Link);
  if (child_name = 'color') Then
     list.add(self.link, 'color', FColor.Link);
  if (child_name = 'category') Then
     list.add(self.link, 'category', FCategory.Link);
  if (child_name = 'measurementPeriod') Then
     list.add(self.link, 'measurementPeriod', FMeasurementPeriod.Link);
  if (child_name = 'calibration') Then
    list.addAll(self, 'calibration', FCalibrationList);
end;

procedure TFhirDeviceMetric.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', false, TFhirCodeableConcept, FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', false, TFhirIdentifier, FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'unit', 'CodeableConcept', false, TFhirCodeableConcept, FUnit_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Reference(Device)', false, TFhirReference{TFhirDevice}, FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'parent', 'Reference(DeviceComponent)', false, TFhirReference{TFhirDeviceComponent}, FParent.Link));{2}
  oList.add(TFHIRProperty.create(self, 'operationalStatus', 'code', false, TFHIREnum, FOperationalStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'color', 'code', false, TFHIREnum, FColor.Link));{1}
  oList.add(TFHIRProperty.create(self, 'category', 'code', false, TFHIREnum, FCategory.Link));{1}
  oList.add(TFHIRProperty.create(self, 'measurementPeriod', 'Timing', false, TFhirTiming, FMeasurementPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'calibration', '', true, TFhirDeviceMetricCalibration, FCalibrationList.Link)){3};
end;

function TFhirDeviceMetric.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'type') then
  begin
    Type_ := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'identifier') then
  begin
    Identifier := propValue as TFhirIdentifier{4b};
    result := propValue;
  end
  else if (propName = 'unit') then
  begin
    Unit_ := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'source') then
  begin
    Source := propValue as TFhirReference{TFhirDevice}{4b};
    result := propValue;
  end
  else if (propName = 'parent') then
  begin
    Parent := propValue as TFhirReference{TFhirDeviceComponent}{4b};
    result := propValue;
  end
  else if (propName = 'operationalStatus') then
  begin
    OperationalStatusElement := asEnum(SYSTEMS_TFhirMetricOperationalStatusEnum, CODES_TFhirMetricOperationalStatusEnum, propValue);
    result := propValue
  end
  else if (propName = 'color') then
  begin
    ColorElement := asEnum(SYSTEMS_TFhirMetricColorEnum, CODES_TFhirMetricColorEnum, propValue);
    result := propValue
  end
  else if (propName = 'category') then
  begin
    CategoryElement := asEnum(SYSTEMS_TFhirMetricCategoryEnum, CODES_TFhirMetricCategoryEnum, propValue);
    result := propValue
  end
  else if (propName = 'measurementPeriod') then
  begin
    MeasurementPeriod := propValue as TFhirTiming{4b};
    result := propValue;
  end
  else if (propName = 'calibration') then
  begin
    CalibrationList.add(propValue as TFhirDeviceMetricCalibration){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirDeviceMetric.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'calibration') then CalibrationList.insertItem(index, propValue as TFhirDeviceMetricCalibration){2a}
  else inherited;
end;

function TFhirDeviceMetric.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'type') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'identifier') then result := TFhirIdentifier.create(){4b}
  else if (propName = 'unit') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'source') then result := TFhirReference{TFhirDevice}.create(){4b}
  else if (propName = 'parent') then result := TFhirReference{TFhirDeviceComponent}.create(){4b}
  else if (propName = 'measurementPeriod') then result := TFhirTiming.create(){4b}
  else if (propName = 'calibration') then result := CalibrationList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirDeviceMetric.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'unit') then result := 'CodeableConcept'
  else if (propName = 'source') then result := 'Reference'
  else if (propName = 'parent') then result := 'Reference'
  else if (propName = 'operationalStatus') then result := 'code'
  else if (propName = 'color') then result := 'code'
  else if (propName = 'category') then result := 'code'
  else if (propName = 'measurementPeriod') then result := 'Timing'
  else if (propName = 'calibration') then result := ''
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirDeviceMetric.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'type') then Type_Element := nil
  else if (propName = 'identifier') then IdentifierElement := nil
  else if (propName = 'unit') then Unit_Element := nil
  else if (propName = 'source') then SourceElement := nil
  else if (propName = 'parent') then ParentElement := nil
  else if (propName = 'operationalStatus') then OperationalStatusElement := nil
  else if (propName = 'color') then ColorElement := nil
  else if (propName = 'category') then CategoryElement := nil
  else if (propName = 'measurementPeriod') then MeasurementPeriodElement := nil
  else if (propName = 'calibration') then deletePropertyValue('calibration', CalibrationList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirDeviceMetric.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'type') then Type_Element := new as TFhirCodeableConcept{4}
  else if (propName = 'identifier') then IdentifierElement := new as TFhirIdentifier{4}
  else if (propName = 'unit') then Unit_Element := new as TFhirCodeableConcept{4}
  else if (propName = 'source') then SourceElement := new as TFhirReference{TFhirDevice}{4}
  else if (propName = 'parent') then ParentElement := new as TFhirReference{TFhirDeviceComponent}{4}
  else if (propName = 'operationalStatus') then OperationalStatusElement := asEnum(SYSTEMS_TFhirMetricOperationalStatusEnum, CODES_TFhirMetricOperationalStatusEnum, new){4}
  else if (propName = 'color') then ColorElement := asEnum(SYSTEMS_TFhirMetricColorEnum, CODES_TFhirMetricColorEnum, new){4}
  else if (propName = 'category') then CategoryElement := asEnum(SYSTEMS_TFhirMetricCategoryEnum, CODES_TFhirMetricCategoryEnum, new){4}
  else if (propName = 'measurementPeriod') then MeasurementPeriodElement := new as TFhirTiming{4}
  else if (propName = 'calibration') then replacePropertyValue('calibration', CalibrationList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirDeviceMetric.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'calibration') then CalibrationList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirDeviceMetric.fhirType : string;
begin
  result := 'DeviceMetric';
end;

function TFhirDeviceMetric.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FType_) and isEmptyProp(FIdentifier) and isEmptyProp(FUnit_) and isEmptyProp(FSource) and isEmptyProp(FParent) and isEmptyProp(FOperationalStatus) and isEmptyProp(FColor) and isEmptyProp(FCategory) and isEmptyProp(FMeasurementPeriod) and isEmptyProp(FcalibrationList);
end;

function TFhirDeviceMetric.equals(other : TObject) : boolean;
var
  o : TFhirDeviceMetric;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirDeviceMetric)) then
    result := false
  else
  begin
    o := TFhirDeviceMetric(other);
    result := compareDeep(type_Element, o.type_Element, true) and compareDeep(identifierElement, o.identifierElement, true) and
      compareDeep(unit_Element, o.unit_Element, true) and compareDeep(sourceElement, o.sourceElement, true) and
      compareDeep(parentElement, o.parentElement, true) and compareDeep(operationalStatusElement, o.operationalStatusElement, true) and
      compareDeep(colorElement, o.colorElement, true) and compareDeep(categoryElement, o.categoryElement, true) and
      compareDeep(measurementPeriodElement, o.measurementPeriodElement, true) and compareDeep(calibrationList, o.calibrationList, true);
  end;
end;

function TFhirDeviceMetric.Link : TFhirDeviceMetric;
begin
  result := TFhirDeviceMetric(inherited Link);
end;

function TFhirDeviceMetric.Clone : TFhirDeviceMetric;
begin
  result := TFhirDeviceMetric(inherited Clone);
end;

procedure TFhirDeviceMetric.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('type');
  fields.add('identifier');
  fields.add('unit');
  fields.add('source');
  fields.add('parent');
  fields.add('operationalStatus');
  fields.add('color');
  fields.add('category');
  fields.add('measurementPeriod');
  fields.add('calibration');
end;

{ TFhirDeviceMetric }

Procedure TFhirDeviceMetric.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirDeviceMetric.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirDeviceMetric.SetUnit_(value : TFhirCodeableConcept);
begin
  FUnit_.free;
  FUnit_ := value;
end;

Procedure TFhirDeviceMetric.SetSource(value : TFhirReference{TFhirDevice});
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirDeviceMetric.SetParent(value : TFhirReference{TFhirDeviceComponent});
begin
  FParent.free;
  FParent := value;
end;

Procedure TFhirDeviceMetric.SetOperationalStatus(value : TFhirEnum);
begin
  FOperationalStatus.free;
  FOperationalStatus := value;
end;

Function TFhirDeviceMetric.GetOperationalStatusST : TFhirMetricOperationalStatusEnum;
begin
  if FOperationalStatus = nil then
    result := TFhirMetricOperationalStatusEnum(0)
  else
    result := TFhirMetricOperationalStatusEnum(StringArrayIndexOfSensitive(CODES_TFhirMetricOperationalStatusEnum, FOperationalStatus.value));
end;

Procedure TFhirDeviceMetric.SetOperationalStatusST(value : TFhirMetricOperationalStatusEnum);
begin
  if ord(value) = 0 then
    OperationalStatusElement := nil
  else
    OperationalStatusElement := TFhirEnum.create(SYSTEMS_TFhirMetricOperationalStatusEnum[value], CODES_TFhirMetricOperationalStatusEnum[value]);
end;

Procedure TFhirDeviceMetric.SetColor(value : TFhirEnum);
begin
  FColor.free;
  FColor := value;
end;

Function TFhirDeviceMetric.GetColorST : TFhirMetricColorEnum;
begin
  if FColor = nil then
    result := TFhirMetricColorEnum(0)
  else
    result := TFhirMetricColorEnum(StringArrayIndexOfSensitive(CODES_TFhirMetricColorEnum, FColor.value));
end;

Procedure TFhirDeviceMetric.SetColorST(value : TFhirMetricColorEnum);
begin
  if ord(value) = 0 then
    ColorElement := nil
  else
    ColorElement := TFhirEnum.create(SYSTEMS_TFhirMetricColorEnum[value], CODES_TFhirMetricColorEnum[value]);
end;

Procedure TFhirDeviceMetric.SetCategory(value : TFhirEnum);
begin
  FCategory.free;
  FCategory := value;
end;

Function TFhirDeviceMetric.GetCategoryST : TFhirMetricCategoryEnum;
begin
  if FCategory = nil then
    result := TFhirMetricCategoryEnum(0)
  else
    result := TFhirMetricCategoryEnum(StringArrayIndexOfSensitive(CODES_TFhirMetricCategoryEnum, FCategory.value));
end;

Procedure TFhirDeviceMetric.SetCategoryST(value : TFhirMetricCategoryEnum);
begin
  if ord(value) = 0 then
    CategoryElement := nil
  else
    CategoryElement := TFhirEnum.create(SYSTEMS_TFhirMetricCategoryEnum[value], CODES_TFhirMetricCategoryEnum[value]);
end;

Procedure TFhirDeviceMetric.SetMeasurementPeriod(value : TFhirTiming);
begin
  FMeasurementPeriod.free;
  FMeasurementPeriod := value;
end;

Function TFhirDeviceMetric.GetCalibrationList : TFhirDeviceMetricCalibrationList;
begin
  if FCalibrationList = nil then
    FCalibrationList := TFhirDeviceMetricCalibrationList.Create;
  result := FCalibrationList;
end;

Function TFhirDeviceMetric.GetHasCalibrationList : boolean;
begin
  result := (FCalibrationList <> nil) and (FCalibrationList.count > 0);
end;

function TFhirDeviceMetric.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FType_.sizeInBytes);
  inc(result, FIdentifier.sizeInBytes);
  inc(result, FUnit_.sizeInBytes);
  inc(result, FSource.sizeInBytes);
  inc(result, FOperationalStatus.sizeInBytes);
  inc(result, FColor.sizeInBytes);
  inc(result, FCategory.sizeInBytes);
  inc(result, FMeasurementPeriod.sizeInBytes);
  inc(result, FcalibrationList.sizeInBytes);
end;

{ TFhirDeviceMetricListEnumerator }

Constructor TFhirDeviceMetricListEnumerator.Create(list : TFhirDeviceMetricList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirDeviceMetricListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirDeviceMetricListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirDeviceMetricListEnumerator.GetCurrent : TFhirDeviceMetric;
begin
  Result := FList[FIndex];
end;

function TFhirDeviceMetricListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirDeviceMetricList }
procedure TFhirDeviceMetricList.AddItem(value: TFhirDeviceMetric);
begin
  assert(value.ClassName = 'TFhirDeviceMetric', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirDeviceMetric');
  add(value);
end;

function TFhirDeviceMetricList.Append: TFhirDeviceMetric;
begin
  result := TFhirDeviceMetric.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceMetricList.ClearItems;
begin
  Clear;
end;

function TFhirDeviceMetricList.GetEnumerator : TFhirDeviceMetricListEnumerator;
begin
  result := TFhirDeviceMetricListEnumerator.Create(self.link);
end;

function TFhirDeviceMetricList.Clone: TFhirDeviceMetricList;
begin
  result := TFhirDeviceMetricList(inherited Clone);
end;

function TFhirDeviceMetricList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirDeviceMetricList.GetItemN(index: Integer): TFhirDeviceMetric;
begin
  result := TFhirDeviceMetric(ObjectByIndex[index]);
end;

function TFhirDeviceMetricList.ItemClass: TFslObjectClass;
begin
  result := TFhirDeviceMetric;
end;
function TFhirDeviceMetricList.IndexOf(value: TFhirDeviceMetric): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirDeviceMetricList.Insert(index: Integer): TFhirDeviceMetric;
begin
  result := TFhirDeviceMetric.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirDeviceMetricList.InsertItem(index: Integer; value: TFhirDeviceMetric);
begin
  assert(value is TFhirDeviceMetric);
  Inherited Insert(index, value);
end;

function TFhirDeviceMetricList.Item(index: Integer): TFhirDeviceMetric;
begin
  result := TFhirDeviceMetric(ObjectByIndex[index]);
end;

function TFhirDeviceMetricList.Link: TFhirDeviceMetricList;
begin
  result := TFhirDeviceMetricList(inherited Link);
end;

procedure TFhirDeviceMetricList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirDeviceMetricList.SetItemByIndex(index: Integer; value: TFhirDeviceMetric);
begin
  assert(value is TFhirDeviceMetric);
  FhirDeviceMetrics[index] := value;
end;

procedure TFhirDeviceMetricList.SetItemN(index: Integer; value: TFhirDeviceMetric);
begin
  assert(value is TFhirDeviceMetric);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_DEVICEMETRIC}

{$IFDEF FHIR_ENCOUNTER}

{ TFhirEncounterStatusHistory }

constructor TFhirEncounterStatusHistory.Create;
begin
  inherited;
end;

destructor TFhirEncounterStatusHistory.Destroy;
begin
  FStatus.free;
  FPeriod.free;
  inherited;
end;

procedure TFhirEncounterStatusHistory.Assign(oSource : TFslObject);
begin
  inherited;
  FStatus := TFhirEncounterStatusHistory(oSource).FStatus.Link;
  period := TFhirEncounterStatusHistory(oSource).period.Clone;
end;

procedure TFhirEncounterStatusHistory.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'status') Then
     list.add(self.link, 'status', FStatus.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
end;

procedure TFhirEncounterStatusHistory.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'status', 'code', false, TFHIREnum, FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
end;

function TFhirEncounterStatusHistory.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'status') then
  begin
    StatusElement := asEnum(SYSTEMS_TFhirEncounterStateEnum, CODES_TFhirEncounterStateEnum, propValue);
    result := propValue
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirEncounterStatusHistory.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirEncounterStatusHistory.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirEncounterStatusHistory.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'status') then result := 'code'
  else if (propName = 'period') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirEncounterStatusHistory.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'status') then StatusElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirEncounterStatusHistory.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'status') then StatusElement := asEnum(SYSTEMS_TFhirEncounterStateEnum, CODES_TFhirEncounterStateEnum, new){4}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirEncounterStatusHistory.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirEncounterStatusHistory.fhirType : string;
begin
  result := 'statusHistory';
end;

function TFhirEncounterStatusHistory.Link : TFhirEncounterStatusHistory;
begin
  result := TFhirEncounterStatusHistory(inherited Link);
end;

function TFhirEncounterStatusHistory.Clone : TFhirEncounterStatusHistory;
begin
  result := TFhirEncounterStatusHistory(inherited Clone);
end;

function TFhirEncounterStatusHistory.equals(other : TObject) : boolean;
var
  o : TFhirEncounterStatusHistory;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEncounterStatusHistory)) then
    result := false
  else
  begin
    o := TFhirEncounterStatusHistory(other);
    result := compareDeep(statusElement, o.statusElement, true) and compareDeep(periodElement, o.periodElement, true);
  end;
end;

function TFhirEncounterStatusHistory.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FStatus) and isEmptyProp(FPeriod);
end;

procedure TFhirEncounterStatusHistory.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('status');
  fields.add('period');
end;

{ TFhirEncounterStatusHistory }

Procedure TFhirEncounterStatusHistory.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirEncounterStatusHistory.GetStatusST : TFhirEncounterStateEnum;
begin
  if FStatus = nil then
    result := TFhirEncounterStateEnum(0)
  else
    result := TFhirEncounterStateEnum(StringArrayIndexOfSensitive(CODES_TFhirEncounterStateEnum, FStatus.value));
end;

Procedure TFhirEncounterStatusHistory.SetStatusST(value : TFhirEncounterStateEnum);
begin
  if ord(value) = 0 then
    StatusElement := nil
  else
    StatusElement := TFhirEnum.create(SYSTEMS_TFhirEncounterStateEnum[value], CODES_TFhirEncounterStateEnum[value]);
end;

Procedure TFhirEncounterStatusHistory.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

function TFhirEncounterStatusHistory.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStatus.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
end;

{ TFhirEncounterStatusHistoryListEnumerator }

Constructor TFhirEncounterStatusHistoryListEnumerator.Create(list : TFhirEncounterStatusHistoryList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEncounterStatusHistoryListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEncounterStatusHistoryListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEncounterStatusHistoryListEnumerator.GetCurrent : TFhirEncounterStatusHistory;
begin
  Result := FList[FIndex];
end;

function TFhirEncounterStatusHistoryListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirEncounterStatusHistoryList }
procedure TFhirEncounterStatusHistoryList.AddItem(value: TFhirEncounterStatusHistory);
begin
  assert(value.ClassName = 'TFhirEncounterStatusHistory', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEncounterStatusHistory');
  add(value);
end;

function TFhirEncounterStatusHistoryList.Append: TFhirEncounterStatusHistory;
begin
  result := TFhirEncounterStatusHistory.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterStatusHistoryList.ClearItems;
begin
  Clear;
end;

function TFhirEncounterStatusHistoryList.GetEnumerator : TFhirEncounterStatusHistoryListEnumerator;
begin
  result := TFhirEncounterStatusHistoryListEnumerator.Create(self.link);
end;

function TFhirEncounterStatusHistoryList.Clone: TFhirEncounterStatusHistoryList;
begin
  result := TFhirEncounterStatusHistoryList(inherited Clone);
end;

function TFhirEncounterStatusHistoryList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEncounterStatusHistoryList.GetItemN(index: Integer): TFhirEncounterStatusHistory;
begin
  result := TFhirEncounterStatusHistory(ObjectByIndex[index]);
end;

function TFhirEncounterStatusHistoryList.ItemClass: TFslObjectClass;
begin
  result := TFhirEncounterStatusHistory;
end;
function TFhirEncounterStatusHistoryList.IndexOf(value: TFhirEncounterStatusHistory): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirEncounterStatusHistoryList.Insert(index: Integer): TFhirEncounterStatusHistory;
begin
  result := TFhirEncounterStatusHistory.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterStatusHistoryList.InsertItem(index: Integer; value: TFhirEncounterStatusHistory);
begin
  assert(value is TFhirEncounterStatusHistory);
  Inherited Insert(index, value);
end;

function TFhirEncounterStatusHistoryList.Item(index: Integer): TFhirEncounterStatusHistory;
begin
  result := TFhirEncounterStatusHistory(ObjectByIndex[index]);
end;

function TFhirEncounterStatusHistoryList.Link: TFhirEncounterStatusHistoryList;
begin
  result := TFhirEncounterStatusHistoryList(inherited Link);
end;

procedure TFhirEncounterStatusHistoryList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEncounterStatusHistoryList.SetItemByIndex(index: Integer; value: TFhirEncounterStatusHistory);
begin
  assert(value is TFhirEncounterStatusHistory);
  FhirEncounterStatusHistories[index] := value;
end;

procedure TFhirEncounterStatusHistoryList.SetItemN(index: Integer; value: TFhirEncounterStatusHistory);
begin
  assert(value is TFhirEncounterStatusHistory);
  ObjectByIndex[index] := value;
end;

{ TFhirEncounterParticipant }

constructor TFhirEncounterParticipant.Create;
begin
  inherited;
end;

destructor TFhirEncounterParticipant.Destroy;
begin
  FType_List.Free;
  FPeriod.free;
  FIndividual.free;
  inherited;
end;

procedure TFhirEncounterParticipant.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirEncounterParticipant(oSource).FType_List = nil) then
  begin
    FType_List.free;
    FType_List := nil;
  end
  else
  begin
    if FType_List = nil then
      FType_List := TFhirCodeableConceptList.Create;
    FType_List.Assign(TFhirEncounterParticipant(oSource).FType_List);
  end;
  period := TFhirEncounterParticipant(oSource).period.Clone;
  individual := TFhirEncounterParticipant(oSource).individual.Clone;
end;

procedure TFhirEncounterParticipant.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'type') Then
    list.addAll(self, 'type', FType_List);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'individual') Then
     list.add(self.link, 'individual', FIndividual.Link);
end;

procedure TFhirEncounterParticipant.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', true, TFhirCodeableConcept, FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'individual', 'Reference(Practitioner|RelatedPerson)', false, TFhirReference{Resource}, FIndividual.Link));{2}
end;

function TFhirEncounterParticipant.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'type') then
  begin
    Type_List.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else if (propName = 'individual') then
  begin
    Individual := propValue as TFhirReference{Resource}{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirEncounterParticipant.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'type') then Type_List.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else inherited;
end;

function TFhirEncounterParticipant.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'type') then result := Type_List.new(){2}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else if (propName = 'individual') then result := TFhirReference{Resource}.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirEncounterParticipant.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'period') then result := 'Period'
  else if (propName = 'individual') then result := 'Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirEncounterParticipant.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'type') then deletePropertyValue('type', Type_List, value) {2}
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'individual') then IndividualElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirEncounterParticipant.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'type') then replacePropertyValue('type', Type_List, existing, new) {2}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else if (propName = 'individual') then IndividualElement := new as TFhirReference{Resource}{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirEncounterParticipant.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'type') then Type_List.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirEncounterParticipant.fhirType : string;
begin
  result := 'participant';
end;

function TFhirEncounterParticipant.Link : TFhirEncounterParticipant;
begin
  result := TFhirEncounterParticipant(inherited Link);
end;

function TFhirEncounterParticipant.Clone : TFhirEncounterParticipant;
begin
  result := TFhirEncounterParticipant(inherited Clone);
end;

function TFhirEncounterParticipant.equals(other : TObject) : boolean;
var
  o : TFhirEncounterParticipant;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEncounterParticipant)) then
    result := false
  else
  begin
    o := TFhirEncounterParticipant(other);
    result := compareDeep(type_List, o.type_List, true) and compareDeep(periodElement, o.periodElement, true) and
      compareDeep(individualElement, o.individualElement, true);
  end;
end;

function TFhirEncounterParticipant.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(Ftype_List) and isEmptyProp(FPeriod) and isEmptyProp(FIndividual);
end;

procedure TFhirEncounterParticipant.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('type');
  fields.add('period');
  fields.add('individual');
end;

{ TFhirEncounterParticipant }

Function TFhirEncounterParticipant.GetType_List : TFhirCodeableConceptList;
begin
  if FType_List = nil then
    FType_List := TFhirCodeableConceptList.Create;
  result := FType_List;
end;

Function TFhirEncounterParticipant.GetHasType_List : boolean;
begin
  result := (FType_List <> nil) and (FType_List.count > 0);
end;

Procedure TFhirEncounterParticipant.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Procedure TFhirEncounterParticipant.SetIndividual(value : TFhirReference{Resource});
begin
  FIndividual.free;
  FIndividual := value;
end;

function TFhirEncounterParticipant.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Ftype_List.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FIndividual.sizeInBytes);
end;

{ TFhirEncounterParticipantListEnumerator }

Constructor TFhirEncounterParticipantListEnumerator.Create(list : TFhirEncounterParticipantList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEncounterParticipantListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEncounterParticipantListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEncounterParticipantListEnumerator.GetCurrent : TFhirEncounterParticipant;
begin
  Result := FList[FIndex];
end;

function TFhirEncounterParticipantListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirEncounterParticipantList }
procedure TFhirEncounterParticipantList.AddItem(value: TFhirEncounterParticipant);
begin
  assert(value.ClassName = 'TFhirEncounterParticipant', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEncounterParticipant');
  add(value);
end;

function TFhirEncounterParticipantList.Append: TFhirEncounterParticipant;
begin
  result := TFhirEncounterParticipant.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterParticipantList.ClearItems;
begin
  Clear;
end;

function TFhirEncounterParticipantList.GetEnumerator : TFhirEncounterParticipantListEnumerator;
begin
  result := TFhirEncounterParticipantListEnumerator.Create(self.link);
end;

function TFhirEncounterParticipantList.Clone: TFhirEncounterParticipantList;
begin
  result := TFhirEncounterParticipantList(inherited Clone);
end;

function TFhirEncounterParticipantList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEncounterParticipantList.GetItemN(index: Integer): TFhirEncounterParticipant;
begin
  result := TFhirEncounterParticipant(ObjectByIndex[index]);
end;

function TFhirEncounterParticipantList.ItemClass: TFslObjectClass;
begin
  result := TFhirEncounterParticipant;
end;
function TFhirEncounterParticipantList.IndexOf(value: TFhirEncounterParticipant): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirEncounterParticipantList.Insert(index: Integer): TFhirEncounterParticipant;
begin
  result := TFhirEncounterParticipant.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterParticipantList.InsertItem(index: Integer; value: TFhirEncounterParticipant);
begin
  assert(value is TFhirEncounterParticipant);
  Inherited Insert(index, value);
end;

function TFhirEncounterParticipantList.Item(index: Integer): TFhirEncounterParticipant;
begin
  result := TFhirEncounterParticipant(ObjectByIndex[index]);
end;

function TFhirEncounterParticipantList.Link: TFhirEncounterParticipantList;
begin
  result := TFhirEncounterParticipantList(inherited Link);
end;

procedure TFhirEncounterParticipantList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEncounterParticipantList.SetItemByIndex(index: Integer; value: TFhirEncounterParticipant);
begin
  assert(value is TFhirEncounterParticipant);
  FhirEncounterParticipants[index] := value;
end;

procedure TFhirEncounterParticipantList.SetItemN(index: Integer; value: TFhirEncounterParticipant);
begin
  assert(value is TFhirEncounterParticipant);
  ObjectByIndex[index] := value;
end;

{ TFhirEncounterHospitalization }

constructor TFhirEncounterHospitalization.Create;
begin
  inherited;
end;

destructor TFhirEncounterHospitalization.Destroy;
begin
  FPreAdmissionIdentifier.free;
  FOrigin.free;
  FAdmitSource.free;
  FAdmittingDiagnosisList.Free;
  FReAdmission.free;
  FDietPreferenceList.Free;
  FSpecialCourtesyList.Free;
  FSpecialArrangementList.Free;
  FDestination.free;
  FDischargeDisposition.free;
  FDischargeDiagnosisList.Free;
  inherited;
end;

procedure TFhirEncounterHospitalization.Assign(oSource : TFslObject);
begin
  inherited;
  preAdmissionIdentifier := TFhirEncounterHospitalization(oSource).preAdmissionIdentifier.Clone;
  origin := TFhirEncounterHospitalization(oSource).origin.Clone;
  admitSource := TFhirEncounterHospitalization(oSource).admitSource.Clone;
  if (TFhirEncounterHospitalization(oSource).FAdmittingDiagnosisList = nil) then
  begin
    FAdmittingDiagnosisList.free;
    FAdmittingDiagnosisList := nil;
  end
  else
  begin
    if FAdmittingDiagnosisList = nil then
      FAdmittingDiagnosisList := TFhirReferenceList{TFhirCondition}.Create;
    FAdmittingDiagnosisList.Assign(TFhirEncounterHospitalization(oSource).FAdmittingDiagnosisList);
  end;
  reAdmission := TFhirEncounterHospitalization(oSource).reAdmission.Clone;
  if (TFhirEncounterHospitalization(oSource).FDietPreferenceList = nil) then
  begin
    FDietPreferenceList.free;
    FDietPreferenceList := nil;
  end
  else
  begin
    if FDietPreferenceList = nil then
      FDietPreferenceList := TFhirCodeableConceptList.Create;
    FDietPreferenceList.Assign(TFhirEncounterHospitalization(oSource).FDietPreferenceList);
  end;
  if (TFhirEncounterHospitalization(oSource).FSpecialCourtesyList = nil) then
  begin
    FSpecialCourtesyList.free;
    FSpecialCourtesyList := nil;
  end
  else
  begin
    if FSpecialCourtesyList = nil then
      FSpecialCourtesyList := TFhirCodeableConceptList.Create;
    FSpecialCourtesyList.Assign(TFhirEncounterHospitalization(oSource).FSpecialCourtesyList);
  end;
  if (TFhirEncounterHospitalization(oSource).FSpecialArrangementList = nil) then
  begin
    FSpecialArrangementList.free;
    FSpecialArrangementList := nil;
  end
  else
  begin
    if FSpecialArrangementList = nil then
      FSpecialArrangementList := TFhirCodeableConceptList.Create;
    FSpecialArrangementList.Assign(TFhirEncounterHospitalization(oSource).FSpecialArrangementList);
  end;
  destination := TFhirEncounterHospitalization(oSource).destination.Clone;
  dischargeDisposition := TFhirEncounterHospitalization(oSource).dischargeDisposition.Clone;
  if (TFhirEncounterHospitalization(oSource).FDischargeDiagnosisList = nil) then
  begin
    FDischargeDiagnosisList.free;
    FDischargeDiagnosisList := nil;
  end
  else
  begin
    if FDischargeDiagnosisList = nil then
      FDischargeDiagnosisList := TFhirReferenceList{TFhirCondition}.Create;
    FDischargeDiagnosisList.Assign(TFhirEncounterHospitalization(oSource).FDischargeDiagnosisList);
  end;
end;

procedure TFhirEncounterHospitalization.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'preAdmissionIdentifier') Then
     list.add(self.link, 'preAdmissionIdentifier', FPreAdmissionIdentifier.Link);
  if (child_name = 'origin') Then
     list.add(self.link, 'origin', FOrigin.Link);
  if (child_name = 'admitSource') Then
     list.add(self.link, 'admitSource', FAdmitSource.Link);
  if (child_name = 'admittingDiagnosis') Then
    list.addAll(self, 'admittingDiagnosis', FAdmittingDiagnosisList);
  if (child_name = 'reAdmission') Then
     list.add(self.link, 'reAdmission', FReAdmission.Link);
  if (child_name = 'dietPreference') Then
    list.addAll(self, 'dietPreference', FDietPreferenceList);
  if (child_name = 'specialCourtesy') Then
    list.addAll(self, 'specialCourtesy', FSpecialCourtesyList);
  if (child_name = 'specialArrangement') Then
    list.addAll(self, 'specialArrangement', FSpecialArrangementList);
  if (child_name = 'destination') Then
     list.add(self.link, 'destination', FDestination.Link);
  if (child_name = 'dischargeDisposition') Then
     list.add(self.link, 'dischargeDisposition', FDischargeDisposition.Link);
  if (child_name = 'dischargeDiagnosis') Then
    list.addAll(self, 'dischargeDiagnosis', FDischargeDiagnosisList);
end;

procedure TFhirEncounterHospitalization.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'preAdmissionIdentifier', 'Identifier', false, TFhirIdentifier, FPreAdmissionIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'origin', 'Reference(Location)', false, TFhirReference{TFhirLocation}, FOrigin.Link));{2}
  oList.add(TFHIRProperty.create(self, 'admitSource', 'CodeableConcept', false, TFhirCodeableConcept, FAdmitSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'admittingDiagnosis', 'Reference(Condition)', true, TFhirReference{TFhirCondition}, FAdmittingDiagnosisList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'reAdmission', 'CodeableConcept', false, TFhirCodeableConcept, FReAdmission.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dietPreference', 'CodeableConcept', true, TFhirCodeableConcept, FDietPreferenceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'specialCourtesy', 'CodeableConcept', true, TFhirCodeableConcept, FSpecialCourtesyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'specialArrangement', 'CodeableConcept', true, TFhirCodeableConcept, FSpecialArrangementList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'destination', 'Reference(Location)', false, TFhirReference{TFhirLocation}, FDestination.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dischargeDisposition', 'CodeableConcept', false, TFhirCodeableConcept, FDischargeDisposition.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dischargeDiagnosis', 'Reference(Condition)', true, TFhirReference{TFhirCondition}, FDischargeDiagnosisList.Link)){3};
end;

function TFhirEncounterHospitalization.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'preAdmissionIdentifier') then
  begin
    PreAdmissionIdentifier := propValue as TFhirIdentifier{4b};
    result := propValue;
  end
  else if (propName = 'origin') then
  begin
    Origin := propValue as TFhirReference{TFhirLocation}{4b};
    result := propValue;
  end
  else if (propName = 'admitSource') then
  begin
    AdmitSource := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'admittingDiagnosis') then
  begin
    AdmittingDiagnosisList.add(propValue as TFhirReference{TFhirCondition}){2a};
    result := propValue;
  end
  else if (propName = 'reAdmission') then
  begin
    ReAdmission := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'dietPreference') then
  begin
    DietPreferenceList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'specialCourtesy') then
  begin
    SpecialCourtesyList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'specialArrangement') then
  begin
    SpecialArrangementList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'destination') then
  begin
    Destination := propValue as TFhirReference{TFhirLocation}{4b};
    result := propValue;
  end
  else if (propName = 'dischargeDisposition') then
  begin
    DischargeDisposition := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'dischargeDiagnosis') then
  begin
    DischargeDiagnosisList.add(propValue as TFhirReference{TFhirCondition}){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirEncounterHospitalization.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'admittingDiagnosis') then AdmittingDiagnosisList.insertItem(index, propValue as TFhirReference{TFhirCondition}){2a}
  else if (propName = 'dietPreference') then DietPreferenceList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'specialCourtesy') then SpecialCourtesyList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'specialArrangement') then SpecialArrangementList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'dischargeDiagnosis') then DischargeDiagnosisList.insertItem(index, propValue as TFhirReference{TFhirCondition}){2a}
  else inherited;
end;

function TFhirEncounterHospitalization.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'preAdmissionIdentifier') then result := TFhirIdentifier.create(){4b}
  else if (propName = 'origin') then result := TFhirReference{TFhirLocation}.create(){4b}
  else if (propName = 'admitSource') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'admittingDiagnosis') then result := AdmittingDiagnosisList.new(){2}
  else if (propName = 'reAdmission') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'dietPreference') then result := DietPreferenceList.new(){2}
  else if (propName = 'specialCourtesy') then result := SpecialCourtesyList.new(){2}
  else if (propName = 'specialArrangement') then result := SpecialArrangementList.new(){2}
  else if (propName = 'destination') then result := TFhirReference{TFhirLocation}.create(){4b}
  else if (propName = 'dischargeDisposition') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'dischargeDiagnosis') then result := DischargeDiagnosisList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirEncounterHospitalization.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'preAdmissionIdentifier') then result := 'Identifier'
  else if (propName = 'origin') then result := 'Reference'
  else if (propName = 'admitSource') then result := 'CodeableConcept'
  else if (propName = 'admittingDiagnosis') then result := 'Reference'
  else if (propName = 'reAdmission') then result := 'CodeableConcept'
  else if (propName = 'dietPreference') then result := 'CodeableConcept'
  else if (propName = 'specialCourtesy') then result := 'CodeableConcept'
  else if (propName = 'specialArrangement') then result := 'CodeableConcept'
  else if (propName = 'destination') then result := 'Reference'
  else if (propName = 'dischargeDisposition') then result := 'CodeableConcept'
  else if (propName = 'dischargeDiagnosis') then result := 'Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirEncounterHospitalization.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'preAdmissionIdentifier') then PreAdmissionIdentifierElement := nil
  else if (propName = 'origin') then OriginElement := nil
  else if (propName = 'admitSource') then AdmitSourceElement := nil
  else if (propName = 'admittingDiagnosis') then deletePropertyValue('admittingDiagnosis', AdmittingDiagnosisList, value) {2}
  else if (propName = 'reAdmission') then ReAdmissionElement := nil
  else if (propName = 'dietPreference') then deletePropertyValue('dietPreference', DietPreferenceList, value) {2}
  else if (propName = 'specialCourtesy') then deletePropertyValue('specialCourtesy', SpecialCourtesyList, value) {2}
  else if (propName = 'specialArrangement') then deletePropertyValue('specialArrangement', SpecialArrangementList, value) {2}
  else if (propName = 'destination') then DestinationElement := nil
  else if (propName = 'dischargeDisposition') then DischargeDispositionElement := nil
  else if (propName = 'dischargeDiagnosis') then deletePropertyValue('dischargeDiagnosis', DischargeDiagnosisList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirEncounterHospitalization.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'preAdmissionIdentifier') then PreAdmissionIdentifierElement := new as TFhirIdentifier{4}
  else if (propName = 'origin') then OriginElement := new as TFhirReference{TFhirLocation}{4}
  else if (propName = 'admitSource') then AdmitSourceElement := new as TFhirCodeableConcept{4}
  else if (propName = 'admittingDiagnosis') then replacePropertyValue('admittingDiagnosis', AdmittingDiagnosisList, existing, new) {2}
  else if (propName = 'reAdmission') then ReAdmissionElement := new as TFhirCodeableConcept{4}
  else if (propName = 'dietPreference') then replacePropertyValue('dietPreference', DietPreferenceList, existing, new) {2}
  else if (propName = 'specialCourtesy') then replacePropertyValue('specialCourtesy', SpecialCourtesyList, existing, new) {2}
  else if (propName = 'specialArrangement') then replacePropertyValue('specialArrangement', SpecialArrangementList, existing, new) {2}
  else if (propName = 'destination') then DestinationElement := new as TFhirReference{TFhirLocation}{4}
  else if (propName = 'dischargeDisposition') then DischargeDispositionElement := new as TFhirCodeableConcept{4}
  else if (propName = 'dischargeDiagnosis') then replacePropertyValue('dischargeDiagnosis', DischargeDiagnosisList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirEncounterHospitalization.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'admittingDiagnosis') then AdmittingDiagnosisList.move(source, destination){2a}
  else if (propName = 'dietPreference') then DietPreferenceList.move(source, destination){2a}
  else if (propName = 'specialCourtesy') then SpecialCourtesyList.move(source, destination){2a}
  else if (propName = 'specialArrangement') then SpecialArrangementList.move(source, destination){2a}
  else if (propName = 'dischargeDiagnosis') then DischargeDiagnosisList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirEncounterHospitalization.fhirType : string;
begin
  result := 'hospitalization';
end;

function TFhirEncounterHospitalization.Link : TFhirEncounterHospitalization;
begin
  result := TFhirEncounterHospitalization(inherited Link);
end;

function TFhirEncounterHospitalization.Clone : TFhirEncounterHospitalization;
begin
  result := TFhirEncounterHospitalization(inherited Clone);
end;

function TFhirEncounterHospitalization.equals(other : TObject) : boolean;
var
  o : TFhirEncounterHospitalization;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEncounterHospitalization)) then
    result := false
  else
  begin
    o := TFhirEncounterHospitalization(other);
    result := compareDeep(preAdmissionIdentifierElement, o.preAdmissionIdentifierElement, true) and
      compareDeep(originElement, o.originElement, true) and compareDeep(admitSourceElement, o.admitSourceElement, true) and
      compareDeep(admittingDiagnosisList, o.admittingDiagnosisList, true) and compareDeep(reAdmissionElement, o.reAdmissionElement, true) and
      compareDeep(dietPreferenceList, o.dietPreferenceList, true) and compareDeep(specialCourtesyList, o.specialCourtesyList, true) and
      compareDeep(specialArrangementList, o.specialArrangementList, true) and compareDeep(destinationElement, o.destinationElement, true) and
      compareDeep(dischargeDispositionElement, o.dischargeDispositionElement, true) and
      compareDeep(dischargeDiagnosisList, o.dischargeDiagnosisList, true);
  end;
end;

function TFhirEncounterHospitalization.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FPreAdmissionIdentifier) and isEmptyProp(FOrigin) and isEmptyProp(FAdmitSource) and isEmptyProp(FadmittingDiagnosisList) and isEmptyProp(FReAdmission) and isEmptyProp(FdietPreferenceList) and isEmptyProp(FspecialCourtesyList) and isEmptyProp(FspecialArrangementList) and isEmptyProp(FDestination) and isEmptyProp(FDischargeDisposition) and isEmptyProp(FdischargeDiagnosisList);
end;

procedure TFhirEncounterHospitalization.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('preAdmissionIdentifier');
  fields.add('origin');
  fields.add('admitSource');
  fields.add('admittingDiagnosis');
  fields.add('reAdmission');
  fields.add('dietPreference');
  fields.add('specialCourtesy');
  fields.add('specialArrangement');
  fields.add('destination');
  fields.add('dischargeDisposition');
  fields.add('dischargeDiagnosis');
end;

{ TFhirEncounterHospitalization }

Procedure TFhirEncounterHospitalization.SetPreAdmissionIdentifier(value : TFhirIdentifier);
begin
  FPreAdmissionIdentifier.free;
  FPreAdmissionIdentifier := value;
end;

Procedure TFhirEncounterHospitalization.SetOrigin(value : TFhirReference{TFhirLocation});
begin
  FOrigin.free;
  FOrigin := value;
end;

Procedure TFhirEncounterHospitalization.SetAdmitSource(value : TFhirCodeableConcept);
begin
  FAdmitSource.free;
  FAdmitSource := value;
end;

Function TFhirEncounterHospitalization.GetAdmittingDiagnosisList : TFhirReferenceList{TFhirCondition};
begin
  if FAdmittingDiagnosisList = nil then
    FAdmittingDiagnosisList := TFhirReferenceList{TFhirCondition}.Create;
  result := FAdmittingDiagnosisList;
end;

Function TFhirEncounterHospitalization.GetHasAdmittingDiagnosisList : boolean;
begin
  result := (FAdmittingDiagnosisList <> nil) and (FAdmittingDiagnosisList.count > 0);
end;

Procedure TFhirEncounterHospitalization.SetReAdmission(value : TFhirCodeableConcept);
begin
  FReAdmission.free;
  FReAdmission := value;
end;

Function TFhirEncounterHospitalization.GetDietPreferenceList : TFhirCodeableConceptList;
begin
  if FDietPreferenceList = nil then
    FDietPreferenceList := TFhirCodeableConceptList.Create;
  result := FDietPreferenceList;
end;

Function TFhirEncounterHospitalization.GetHasDietPreferenceList : boolean;
begin
  result := (FDietPreferenceList <> nil) and (FDietPreferenceList.count > 0);
end;

Function TFhirEncounterHospitalization.GetSpecialCourtesyList : TFhirCodeableConceptList;
begin
  if FSpecialCourtesyList = nil then
    FSpecialCourtesyList := TFhirCodeableConceptList.Create;
  result := FSpecialCourtesyList;
end;

Function TFhirEncounterHospitalization.GetHasSpecialCourtesyList : boolean;
begin
  result := (FSpecialCourtesyList <> nil) and (FSpecialCourtesyList.count > 0);
end;

Function TFhirEncounterHospitalization.GetSpecialArrangementList : TFhirCodeableConceptList;
begin
  if FSpecialArrangementList = nil then
    FSpecialArrangementList := TFhirCodeableConceptList.Create;
  result := FSpecialArrangementList;
end;

Function TFhirEncounterHospitalization.GetHasSpecialArrangementList : boolean;
begin
  result := (FSpecialArrangementList <> nil) and (FSpecialArrangementList.count > 0);
end;

Procedure TFhirEncounterHospitalization.SetDestination(value : TFhirReference{TFhirLocation});
begin
  FDestination.free;
  FDestination := value;
end;

Procedure TFhirEncounterHospitalization.SetDischargeDisposition(value : TFhirCodeableConcept);
begin
  FDischargeDisposition.free;
  FDischargeDisposition := value;
end;

Function TFhirEncounterHospitalization.GetDischargeDiagnosisList : TFhirReferenceList{TFhirCondition};
begin
  if FDischargeDiagnosisList = nil then
    FDischargeDiagnosisList := TFhirReferenceList{TFhirCondition}.Create;
  result := FDischargeDiagnosisList;
end;

Function TFhirEncounterHospitalization.GetHasDischargeDiagnosisList : boolean;
begin
  result := (FDischargeDiagnosisList <> nil) and (FDischargeDiagnosisList.count > 0);
end;

function TFhirEncounterHospitalization.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPreAdmissionIdentifier.sizeInBytes);
  inc(result, FOrigin.sizeInBytes);
  inc(result, FAdmitSource.sizeInBytes);
  inc(result, FadmittingDiagnosisList.sizeInBytes);
  inc(result, FReAdmission.sizeInBytes);
  inc(result, FdietPreferenceList.sizeInBytes);
  inc(result, FspecialCourtesyList.sizeInBytes);
  inc(result, FspecialArrangementList.sizeInBytes);
  inc(result, FDestination.sizeInBytes);
  inc(result, FDischargeDisposition.sizeInBytes);
  inc(result, FdischargeDiagnosisList.sizeInBytes);
end;

{ TFhirEncounterHospitalizationListEnumerator }

Constructor TFhirEncounterHospitalizationListEnumerator.Create(list : TFhirEncounterHospitalizationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEncounterHospitalizationListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEncounterHospitalizationListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEncounterHospitalizationListEnumerator.GetCurrent : TFhirEncounterHospitalization;
begin
  Result := FList[FIndex];
end;

function TFhirEncounterHospitalizationListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirEncounterHospitalizationList }
procedure TFhirEncounterHospitalizationList.AddItem(value: TFhirEncounterHospitalization);
begin
  assert(value.ClassName = 'TFhirEncounterHospitalization', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEncounterHospitalization');
  add(value);
end;

function TFhirEncounterHospitalizationList.Append: TFhirEncounterHospitalization;
begin
  result := TFhirEncounterHospitalization.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterHospitalizationList.ClearItems;
begin
  Clear;
end;

function TFhirEncounterHospitalizationList.GetEnumerator : TFhirEncounterHospitalizationListEnumerator;
begin
  result := TFhirEncounterHospitalizationListEnumerator.Create(self.link);
end;

function TFhirEncounterHospitalizationList.Clone: TFhirEncounterHospitalizationList;
begin
  result := TFhirEncounterHospitalizationList(inherited Clone);
end;

function TFhirEncounterHospitalizationList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEncounterHospitalizationList.GetItemN(index: Integer): TFhirEncounterHospitalization;
begin
  result := TFhirEncounterHospitalization(ObjectByIndex[index]);
end;

function TFhirEncounterHospitalizationList.ItemClass: TFslObjectClass;
begin
  result := TFhirEncounterHospitalization;
end;
function TFhirEncounterHospitalizationList.IndexOf(value: TFhirEncounterHospitalization): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirEncounterHospitalizationList.Insert(index: Integer): TFhirEncounterHospitalization;
begin
  result := TFhirEncounterHospitalization.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterHospitalizationList.InsertItem(index: Integer; value: TFhirEncounterHospitalization);
begin
  assert(value is TFhirEncounterHospitalization);
  Inherited Insert(index, value);
end;

function TFhirEncounterHospitalizationList.Item(index: Integer): TFhirEncounterHospitalization;
begin
  result := TFhirEncounterHospitalization(ObjectByIndex[index]);
end;

function TFhirEncounterHospitalizationList.Link: TFhirEncounterHospitalizationList;
begin
  result := TFhirEncounterHospitalizationList(inherited Link);
end;

procedure TFhirEncounterHospitalizationList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEncounterHospitalizationList.SetItemByIndex(index: Integer; value: TFhirEncounterHospitalization);
begin
  assert(value is TFhirEncounterHospitalization);
  FhirEncounterHospitalizations[index] := value;
end;

procedure TFhirEncounterHospitalizationList.SetItemN(index: Integer; value: TFhirEncounterHospitalization);
begin
  assert(value is TFhirEncounterHospitalization);
  ObjectByIndex[index] := value;
end;

{ TFhirEncounterLocation }

constructor TFhirEncounterLocation.Create;
begin
  inherited;
end;

destructor TFhirEncounterLocation.Destroy;
begin
  FLocation.free;
  FStatus.free;
  FPeriod.free;
  inherited;
end;

procedure TFhirEncounterLocation.Assign(oSource : TFslObject);
begin
  inherited;
  location := TFhirEncounterLocation(oSource).location.Clone;
  FStatus := TFhirEncounterLocation(oSource).FStatus.Link;
  period := TFhirEncounterLocation(oSource).period.Clone;
end;

procedure TFhirEncounterLocation.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'location') Then
     list.add(self.link, 'location', FLocation.Link);
  if (child_name = 'status') Then
     list.add(self.link, 'status', FStatus.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
end;

procedure TFhirEncounterLocation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'location', 'Reference(Location)', false, TFhirReference{TFhirLocation}, FLocation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', false, TFHIREnum, FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
end;

function TFhirEncounterLocation.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'location') then
  begin
    Location := propValue as TFhirReference{TFhirLocation}{4b};
    result := propValue;
  end
  else if (propName = 'status') then
  begin
    StatusElement := asEnum(SYSTEMS_TFhirEncounterLocationStatusEnum, CODES_TFhirEncounterLocationStatusEnum, propValue);
    result := propValue
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirEncounterLocation.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirEncounterLocation.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'location') then result := TFhirReference{TFhirLocation}.create(){4b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirEncounterLocation.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'location') then result := 'Reference'
  else if (propName = 'status') then result := 'code'
  else if (propName = 'period') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirEncounterLocation.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'location') then LocationElement := nil
  else if (propName = 'status') then StatusElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirEncounterLocation.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'location') then LocationElement := new as TFhirReference{TFhirLocation}{4}
  else if (propName = 'status') then StatusElement := asEnum(SYSTEMS_TFhirEncounterLocationStatusEnum, CODES_TFhirEncounterLocationStatusEnum, new){4}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirEncounterLocation.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirEncounterLocation.fhirType : string;
begin
  result := 'location';
end;

function TFhirEncounterLocation.Link : TFhirEncounterLocation;
begin
  result := TFhirEncounterLocation(inherited Link);
end;

function TFhirEncounterLocation.Clone : TFhirEncounterLocation;
begin
  result := TFhirEncounterLocation(inherited Clone);
end;

function TFhirEncounterLocation.equals(other : TObject) : boolean;
var
  o : TFhirEncounterLocation;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEncounterLocation)) then
    result := false
  else
  begin
    o := TFhirEncounterLocation(other);
    result := compareDeep(locationElement, o.locationElement, true) and compareDeep(statusElement, o.statusElement, true) and
      compareDeep(periodElement, o.periodElement, true);
  end;
end;

function TFhirEncounterLocation.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FLocation) and isEmptyProp(FStatus) and isEmptyProp(FPeriod);
end;

procedure TFhirEncounterLocation.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('location');
  fields.add('status');
  fields.add('period');
end;

{ TFhirEncounterLocation }

Procedure TFhirEncounterLocation.SetLocation(value : TFhirReference{TFhirLocation});
begin
  FLocation.free;
  FLocation := value;
end;

Procedure TFhirEncounterLocation.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirEncounterLocation.GetStatusST : TFhirEncounterLocationStatusEnum;
begin
  if FStatus = nil then
    result := TFhirEncounterLocationStatusEnum(0)
  else
    result := TFhirEncounterLocationStatusEnum(StringArrayIndexOfSensitive(CODES_TFhirEncounterLocationStatusEnum, FStatus.value));
end;

Procedure TFhirEncounterLocation.SetStatusST(value : TFhirEncounterLocationStatusEnum);
begin
  if ord(value) = 0 then
    StatusElement := nil
  else
    StatusElement := TFhirEnum.create(SYSTEMS_TFhirEncounterLocationStatusEnum[value], CODES_TFhirEncounterLocationStatusEnum[value]);
end;

Procedure TFhirEncounterLocation.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

function TFhirEncounterLocation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLocation.sizeInBytes);
  inc(result, FStatus.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
end;

{ TFhirEncounterLocationListEnumerator }

Constructor TFhirEncounterLocationListEnumerator.Create(list : TFhirEncounterLocationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEncounterLocationListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEncounterLocationListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEncounterLocationListEnumerator.GetCurrent : TFhirEncounterLocation;
begin
  Result := FList[FIndex];
end;

function TFhirEncounterLocationListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirEncounterLocationList }
procedure TFhirEncounterLocationList.AddItem(value: TFhirEncounterLocation);
begin
  assert(value.ClassName = 'TFhirEncounterLocation', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEncounterLocation');
  add(value);
end;

function TFhirEncounterLocationList.Append: TFhirEncounterLocation;
begin
  result := TFhirEncounterLocation.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterLocationList.ClearItems;
begin
  Clear;
end;

function TFhirEncounterLocationList.GetEnumerator : TFhirEncounterLocationListEnumerator;
begin
  result := TFhirEncounterLocationListEnumerator.Create(self.link);
end;

function TFhirEncounterLocationList.Clone: TFhirEncounterLocationList;
begin
  result := TFhirEncounterLocationList(inherited Clone);
end;

function TFhirEncounterLocationList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEncounterLocationList.GetItemN(index: Integer): TFhirEncounterLocation;
begin
  result := TFhirEncounterLocation(ObjectByIndex[index]);
end;

function TFhirEncounterLocationList.ItemClass: TFslObjectClass;
begin
  result := TFhirEncounterLocation;
end;
function TFhirEncounterLocationList.IndexOf(value: TFhirEncounterLocation): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirEncounterLocationList.Insert(index: Integer): TFhirEncounterLocation;
begin
  result := TFhirEncounterLocation.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterLocationList.InsertItem(index: Integer; value: TFhirEncounterLocation);
begin
  assert(value is TFhirEncounterLocation);
  Inherited Insert(index, value);
end;

function TFhirEncounterLocationList.Item(index: Integer): TFhirEncounterLocation;
begin
  result := TFhirEncounterLocation(ObjectByIndex[index]);
end;

function TFhirEncounterLocationList.Link: TFhirEncounterLocationList;
begin
  result := TFhirEncounterLocationList(inherited Link);
end;

procedure TFhirEncounterLocationList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEncounterLocationList.SetItemByIndex(index: Integer; value: TFhirEncounterLocation);
begin
  assert(value is TFhirEncounterLocation);
  FhirEncounterLocations[index] := value;
end;

procedure TFhirEncounterLocationList.SetItemN(index: Integer; value: TFhirEncounterLocation);
begin
  assert(value is TFhirEncounterLocation);
  ObjectByIndex[index] := value;
end;

{ TFhirEncounter }

constructor TFhirEncounter.Create;
begin
  inherited;
end;

destructor TFhirEncounter.Destroy;
begin
  FIdentifierList.Free;
  FStatus.free;
  FStatusHistoryList.Free;
  FClass_.free;
  FType_List.Free;
  FPriority.free;
  FPatient.free;
  FEpisodeOfCareList.Free;
  FIncomingReferralList.Free;
  FParticipantList.Free;
  FAppointment.free;
  FPeriod.free;
  FLength.free;
  FReasonList.Free;
  FIndicationList.Free;
  FHospitalization.free;
  FLocationList.Free;
  FServiceProvider.free;
  FPartOf.free;
  inherited;
end;

function TFhirEncounter.GetResourceType : TFhirResourceType;
begin
  result := frtEncounter;
end;

procedure TFhirEncounter.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirEncounter(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirEncounter(oSource).FIdentifierList);
  end;
  FStatus := TFhirEncounter(oSource).FStatus.Link;
  if (TFhirEncounter(oSource).FStatusHistoryList = nil) then
  begin
    FStatusHistoryList.free;
    FStatusHistoryList := nil;
  end
  else
  begin
    if FStatusHistoryList = nil then
      FStatusHistoryList := TFhirEncounterStatusHistoryList.Create;
    FStatusHistoryList.Assign(TFhirEncounter(oSource).FStatusHistoryList);
  end;
  FClass_ := TFhirEncounter(oSource).FClass_.Link;
  if (TFhirEncounter(oSource).FType_List = nil) then
  begin
    FType_List.free;
    FType_List := nil;
  end
  else
  begin
    if FType_List = nil then
      FType_List := TFhirCodeableConceptList.Create;
    FType_List.Assign(TFhirEncounter(oSource).FType_List);
  end;
  priority := TFhirEncounter(oSource).priority.Clone;
  patient := TFhirEncounter(oSource).patient.Clone;
  if (TFhirEncounter(oSource).FEpisodeOfCareList = nil) then
  begin
    FEpisodeOfCareList.free;
    FEpisodeOfCareList := nil;
  end
  else
  begin
    if FEpisodeOfCareList = nil then
      FEpisodeOfCareList := TFhirReferenceList{TFhirEpisodeOfCare}.Create;
    FEpisodeOfCareList.Assign(TFhirEncounter(oSource).FEpisodeOfCareList);
  end;
  if (TFhirEncounter(oSource).FIncomingReferralList = nil) then
  begin
    FIncomingReferralList.free;
    FIncomingReferralList := nil;
  end
  else
  begin
    if FIncomingReferralList = nil then
      FIncomingReferralList := TFhirReferenceList{TFhirReferralRequest}.Create;
    FIncomingReferralList.Assign(TFhirEncounter(oSource).FIncomingReferralList);
  end;
  if (TFhirEncounter(oSource).FParticipantList = nil) then
  begin
    FParticipantList.free;
    FParticipantList := nil;
  end
  else
  begin
    if FParticipantList = nil then
      FParticipantList := TFhirEncounterParticipantList.Create;
    FParticipantList.Assign(TFhirEncounter(oSource).FParticipantList);
  end;
  appointment := TFhirEncounter(oSource).appointment.Clone;
  period := TFhirEncounter(oSource).period.Clone;
  length := TFhirEncounter(oSource).length.Clone;
  if (TFhirEncounter(oSource).FReasonList = nil) then
  begin
    FReasonList.free;
    FReasonList := nil;
  end
  else
  begin
    if FReasonList = nil then
      FReasonList := TFhirCodeableConceptList.Create;
    FReasonList.Assign(TFhirEncounter(oSource).FReasonList);
  end;
  if (TFhirEncounter(oSource).FIndicationList = nil) then
  begin
    FIndicationList.free;
    FIndicationList := nil;
  end
  else
  begin
    if FIndicationList = nil then
      FIndicationList := TFhirReferenceList{Resource}.Create;
    FIndicationList.Assign(TFhirEncounter(oSource).FIndicationList);
  end;
  hospitalization := TFhirEncounter(oSource).hospitalization.Clone;
  if (TFhirEncounter(oSource).FLocationList = nil) then
  begin
    FLocationList.free;
    FLocationList := nil;
  end
  else
  begin
    if FLocationList = nil then
      FLocationList := TFhirEncounterLocationList.Create;
    FLocationList.Assign(TFhirEncounter(oSource).FLocationList);
  end;
  serviceProvider := TFhirEncounter(oSource).serviceProvider.Clone;
  partOf := TFhirEncounter(oSource).partOf.Clone;
end;

procedure TFhirEncounter.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'status') Then
     list.add(self.link, 'status', FStatus.Link);
  if (child_name = 'statusHistory') Then
    list.addAll(self, 'statusHistory', FStatusHistoryList);
  if (child_name = 'class') Then
     list.add(self.link, 'class', FClass_.Link);
  if (child_name = 'type') Then
    list.addAll(self, 'type', FType_List);
  if (child_name = 'priority') Then
     list.add(self.link, 'priority', FPriority.Link);
  if (child_name = 'patient') Then
     list.add(self.link, 'patient', FPatient.Link);
  if (child_name = 'episodeOfCare') Then
    list.addAll(self, 'episodeOfCare', FEpisodeOfCareList);
  if (child_name = 'incomingReferral') Then
    list.addAll(self, 'incomingReferral', FIncomingReferralList);
  if (child_name = 'participant') Then
    list.addAll(self, 'participant', FParticipantList);
  if (child_name = 'appointment') Then
     list.add(self.link, 'appointment', FAppointment.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'length') Then
     list.add(self.link, 'length', FLength.Link);
  if (child_name = 'reason') Then
    list.addAll(self, 'reason', FReasonList);
  if (child_name = 'indication') Then
    list.addAll(self, 'indication', FIndicationList);
  if (child_name = 'hospitalization') Then
     list.add(self.link, 'hospitalization', FHospitalization.Link);
  if (child_name = 'location') Then
    list.addAll(self, 'location', FLocationList);
  if (child_name = 'serviceProvider') Then
     list.add(self.link, 'serviceProvider', FServiceProvider.Link);
  if (child_name = 'partOf') Then
     list.add(self.link, 'partOf', FPartOf.Link);
end;

procedure TFhirEncounter.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', false, TFHIREnum, FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'statusHistory', '', true, TFhirEncounterStatusHistory, FStatusHistoryList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'class', 'code', false, TFHIREnum, FClass_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', true, TFhirCodeableConcept, FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'priority', 'CodeableConcept', false, TFhirCodeableConcept, FPriority.Link));{2}
  oList.add(TFHIRProperty.create(self, 'patient', 'Reference(Patient)', false, TFhirReference{TFhirPatient}, FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'episodeOfCare', 'Reference(EpisodeOfCare)', true, TFhirReference{TFhirEpisodeOfCare}, FEpisodeOfCareList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'incomingReferral', 'Reference(ReferralRequest)', true, TFhirReference{TFhirReferralRequest}, FIncomingReferralList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'participant', '', true, TFhirEncounterParticipant, FParticipantList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'appointment', 'Reference(Appointment)', false, TFhirReference{TFhirAppointment}, FAppointment.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'length', 'Quantity', false, TFhirQuantity, FLength.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason', 'CodeableConcept', true, TFhirCodeableConcept, FReasonList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'indication', 'Reference(Condition|Procedure)', true, TFhirReference{Resource}, FIndicationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'hospitalization', '', false, TFhirEncounterHospitalization, FHospitalization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', '', true, TFhirEncounterLocation, FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'serviceProvider', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FServiceProvider.Link));{2}
  oList.add(TFHIRProperty.create(self, 'partOf', 'Reference(Encounter)', false, TFhirReference{TFhirEncounter}, FPartOf.Link));{2}
end;

function TFhirEncounter.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'status') then
  begin
    StatusElement := asEnum(SYSTEMS_TFhirEncounterStateEnum, CODES_TFhirEncounterStateEnum, propValue);
    result := propValue
  end
  else if (propName = 'statusHistory') then
  begin
    StatusHistoryList.add(propValue as TFhirEncounterStatusHistory){2a};
    result := propValue;
  end
  else if (propName = 'class') then
  begin
    Class_Element := asEnum(SYSTEMS_TFhirEncounterClassEnum, CODES_TFhirEncounterClassEnum, propValue);
    result := propValue
  end
  else if (propName = 'type') then
  begin
    Type_List.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'priority') then
  begin
    Priority := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'patient') then
  begin
    Patient := propValue as TFhirReference{TFhirPatient}{4b};
    result := propValue;
  end
  else if (propName = 'episodeOfCare') then
  begin
    EpisodeOfCareList.add(propValue as TFhirReference{TFhirEpisodeOfCare}){2a};
    result := propValue;
  end
  else if (propName = 'incomingReferral') then
  begin
    IncomingReferralList.add(propValue as TFhirReference{TFhirReferralRequest}){2a};
    result := propValue;
  end
  else if (propName = 'participant') then
  begin
    ParticipantList.add(propValue as TFhirEncounterParticipant){2a};
    result := propValue;
  end
  else if (propName = 'appointment') then
  begin
    Appointment := propValue as TFhirReference{TFhirAppointment}{4b};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else if (propName = 'length') then
  begin
    Length := propValue as TFhirQuantity{4b};
    result := propValue;
  end
  else if (propName = 'reason') then
  begin
    ReasonList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'indication') then
  begin
    IndicationList.add(propValue as TFhirReference{Resource}){2a};
    result := propValue;
  end
  else if (propName = 'hospitalization') then
  begin
    Hospitalization := propValue as TFhirEncounterHospitalization{4b};
    result := propValue;
  end
  else if (propName = 'location') then
  begin
    LocationList.add(propValue as TFhirEncounterLocation){2a};
    result := propValue;
  end
  else if (propName = 'serviceProvider') then
  begin
    ServiceProvider := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'partOf') then
  begin
    PartOf := propValue as TFhirReference{TFhirEncounter}{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirEncounter.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'statusHistory') then StatusHistoryList.insertItem(index, propValue as TFhirEncounterStatusHistory){2a}
  else if (propName = 'type') then Type_List.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'episodeOfCare') then EpisodeOfCareList.insertItem(index, propValue as TFhirReference{TFhirEpisodeOfCare}){2a}
  else if (propName = 'incomingReferral') then IncomingReferralList.insertItem(index, propValue as TFhirReference{TFhirReferralRequest}){2a}
  else if (propName = 'participant') then ParticipantList.insertItem(index, propValue as TFhirEncounterParticipant){2a}
  else if (propName = 'reason') then ReasonList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'indication') then IndicationList.insertItem(index, propValue as TFhirReference{Resource}){2a}
  else if (propName = 'location') then LocationList.insertItem(index, propValue as TFhirEncounterLocation){2a}
  else inherited;
end;

function TFhirEncounter.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'statusHistory') then result := StatusHistoryList.new(){2}
  else if (propName = 'type') then result := Type_List.new(){2}
  else if (propName = 'priority') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'patient') then result := TFhirReference{TFhirPatient}.create(){4b}
  else if (propName = 'episodeOfCare') then result := EpisodeOfCareList.new(){2}
  else if (propName = 'incomingReferral') then result := IncomingReferralList.new(){2}
  else if (propName = 'participant') then result := ParticipantList.new(){2}
  else if (propName = 'appointment') then result := TFhirReference{TFhirAppointment}.create(){4b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else if (propName = 'length') then result := TFhirQuantity.create(){4b}
  else if (propName = 'reason') then result := ReasonList.new(){2}
  else if (propName = 'indication') then result := IndicationList.new(){2}
  else if (propName = 'hospitalization') then result := TFhirEncounterHospitalization.create(){4b}
  else if (propName = 'location') then result := LocationList.new(){2}
  else if (propName = 'serviceProvider') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'partOf') then result := TFhirReference{TFhirEncounter}.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirEncounter.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'status') then result := 'code'
  else if (propName = 'statusHistory') then result := ''
  else if (propName = 'class') then result := 'code'
  else if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'priority') then result := 'CodeableConcept'
  else if (propName = 'patient') then result := 'Reference'
  else if (propName = 'episodeOfCare') then result := 'Reference'
  else if (propName = 'incomingReferral') then result := 'Reference'
  else if (propName = 'participant') then result := ''
  else if (propName = 'appointment') then result := 'Reference'
  else if (propName = 'period') then result := 'Period'
  else if (propName = 'length') then result := 'Quantity'
  else if (propName = 'reason') then result := 'CodeableConcept'
  else if (propName = 'indication') then result := 'Reference'
  else if (propName = 'hospitalization') then result := ''
  else if (propName = 'location') then result := ''
  else if (propName = 'serviceProvider') then result := 'Reference'
  else if (propName = 'partOf') then result := 'Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirEncounter.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'status') then StatusElement := nil
  else if (propName = 'statusHistory') then deletePropertyValue('statusHistory', StatusHistoryList, value) {2}
  else if (propName = 'class') then Class_Element := nil
  else if (propName = 'type') then deletePropertyValue('type', Type_List, value) {2}
  else if (propName = 'priority') then PriorityElement := nil
  else if (propName = 'patient') then PatientElement := nil
  else if (propName = 'episodeOfCare') then deletePropertyValue('episodeOfCare', EpisodeOfCareList, value) {2}
  else if (propName = 'incomingReferral') then deletePropertyValue('incomingReferral', IncomingReferralList, value) {2}
  else if (propName = 'participant') then deletePropertyValue('participant', ParticipantList, value) {2}
  else if (propName = 'appointment') then AppointmentElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'length') then LengthElement := nil
  else if (propName = 'reason') then deletePropertyValue('reason', ReasonList, value) {2}
  else if (propName = 'indication') then deletePropertyValue('indication', IndicationList, value) {2}
  else if (propName = 'hospitalization') then HospitalizationElement := nil
  else if (propName = 'location') then deletePropertyValue('location', LocationList, value) {2}
  else if (propName = 'serviceProvider') then ServiceProviderElement := nil
  else if (propName = 'partOf') then PartOfElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirEncounter.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'status') then StatusElement := asEnum(SYSTEMS_TFhirEncounterStateEnum, CODES_TFhirEncounterStateEnum, new){4}
  else if (propName = 'statusHistory') then replacePropertyValue('statusHistory', StatusHistoryList, existing, new) {2}
  else if (propName = 'class') then Class_Element := asEnum(SYSTEMS_TFhirEncounterClassEnum, CODES_TFhirEncounterClassEnum, new){4}
  else if (propName = 'type') then replacePropertyValue('type', Type_List, existing, new) {2}
  else if (propName = 'priority') then PriorityElement := new as TFhirCodeableConcept{4}
  else if (propName = 'patient') then PatientElement := new as TFhirReference{TFhirPatient}{4}
  else if (propName = 'episodeOfCare') then replacePropertyValue('episodeOfCare', EpisodeOfCareList, existing, new) {2}
  else if (propName = 'incomingReferral') then replacePropertyValue('incomingReferral', IncomingReferralList, existing, new) {2}
  else if (propName = 'participant') then replacePropertyValue('participant', ParticipantList, existing, new) {2}
  else if (propName = 'appointment') then AppointmentElement := new as TFhirReference{TFhirAppointment}{4}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else if (propName = 'length') then LengthElement := new as TFhirQuantity{4}
  else if (propName = 'reason') then replacePropertyValue('reason', ReasonList, existing, new) {2}
  else if (propName = 'indication') then replacePropertyValue('indication', IndicationList, existing, new) {2}
  else if (propName = 'hospitalization') then HospitalizationElement := new as TFhirEncounterHospitalization{4}
  else if (propName = 'location') then replacePropertyValue('location', LocationList, existing, new) {2}
  else if (propName = 'serviceProvider') then ServiceProviderElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'partOf') then PartOfElement := new as TFhirReference{TFhirEncounter}{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirEncounter.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'statusHistory') then StatusHistoryList.move(source, destination){2a}
  else if (propName = 'type') then Type_List.move(source, destination){2a}
  else if (propName = 'episodeOfCare') then EpisodeOfCareList.move(source, destination){2a}
  else if (propName = 'incomingReferral') then IncomingReferralList.move(source, destination){2a}
  else if (propName = 'participant') then ParticipantList.move(source, destination){2a}
  else if (propName = 'reason') then ReasonList.move(source, destination){2a}
  else if (propName = 'indication') then IndicationList.move(source, destination){2a}
  else if (propName = 'location') then LocationList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirEncounter.fhirType : string;
begin
  result := 'Encounter';
end;

function TFhirEncounter.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FStatus) and isEmptyProp(FstatusHistoryList) and isEmptyProp(FClass_) and isEmptyProp(Ftype_List) and isEmptyProp(FPriority) and isEmptyProp(FPatient) and isEmptyProp(FepisodeOfCareList) and isEmptyProp(FincomingReferralList) and isEmptyProp(FparticipantList) and isEmptyProp(FAppointment) and isEmptyProp(FPeriod) and isEmptyProp(FLength) and isEmptyProp(FreasonList) and isEmptyProp(FindicationList) and isEmptyProp(FHospitalization) and isEmptyProp(FlocationList) and isEmptyProp(FServiceProvider) and isEmptyProp(FPartOf);
end;

function TFhirEncounter.equals(other : TObject) : boolean;
var
  o : TFhirEncounter;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEncounter)) then
    result := false
  else
  begin
    o := TFhirEncounter(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(statusElement, o.statusElement, true) and
      compareDeep(statusHistoryList, o.statusHistoryList, true) and compareDeep(class_Element, o.class_Element, true) and
      compareDeep(type_List, o.type_List, true) and compareDeep(priorityElement, o.priorityElement, true) and
      compareDeep(patientElement, o.patientElement, true) and compareDeep(episodeOfCareList, o.episodeOfCareList, true) and
      compareDeep(incomingReferralList, o.incomingReferralList, true) and compareDeep(participantList, o.participantList, true) and
      compareDeep(appointmentElement, o.appointmentElement, true) and compareDeep(periodElement, o.periodElement, true) and
      compareDeep(lengthElement, o.lengthElement, true) and compareDeep(reasonList, o.reasonList, true) and
      compareDeep(indicationList, o.indicationList, true) and compareDeep(hospitalizationElement, o.hospitalizationElement, true) and
      compareDeep(locationList, o.locationList, true) and compareDeep(serviceProviderElement, o.serviceProviderElement, true) and
      compareDeep(partOfElement, o.partOfElement, true);
  end;
end;

function TFhirEncounter.Link : TFhirEncounter;
begin
  result := TFhirEncounter(inherited Link);
end;

function TFhirEncounter.Clone : TFhirEncounter;
begin
  result := TFhirEncounter(inherited Clone);
end;

procedure TFhirEncounter.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('status');
  fields.add('statusHistory');
  fields.add('class');
  fields.add('type');
  fields.add('priority');
  fields.add('patient');
  fields.add('episodeOfCare');
  fields.add('incomingReferral');
  fields.add('participant');
  fields.add('appointment');
  fields.add('period');
  fields.add('length');
  fields.add('reason');
  fields.add('indication');
  fields.add('hospitalization');
  fields.add('location');
  fields.add('serviceProvider');
  fields.add('partOf');
end;

{ TFhirEncounter }

Function TFhirEncounter.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirEncounter.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirEncounter.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirEncounter.GetStatusST : TFhirEncounterStateEnum;
begin
  if FStatus = nil then
    result := TFhirEncounterStateEnum(0)
  else
    result := TFhirEncounterStateEnum(StringArrayIndexOfSensitive(CODES_TFhirEncounterStateEnum, FStatus.value));
end;

Procedure TFhirEncounter.SetStatusST(value : TFhirEncounterStateEnum);
begin
  if ord(value) = 0 then
    StatusElement := nil
  else
    StatusElement := TFhirEnum.create(SYSTEMS_TFhirEncounterStateEnum[value], CODES_TFhirEncounterStateEnum[value]);
end;

Function TFhirEncounter.GetStatusHistoryList : TFhirEncounterStatusHistoryList;
begin
  if FStatusHistoryList = nil then
    FStatusHistoryList := TFhirEncounterStatusHistoryList.Create;
  result := FStatusHistoryList;
end;

Function TFhirEncounter.GetHasStatusHistoryList : boolean;
begin
  result := (FStatusHistoryList <> nil) and (FStatusHistoryList.count > 0);
end;

Procedure TFhirEncounter.SetClass_(value : TFhirEnum);
begin
  FClass_.free;
  FClass_ := value;
end;

Function TFhirEncounter.GetClass_ST : TFhirEncounterClassEnum;
begin
  if FClass_ = nil then
    result := TFhirEncounterClassEnum(0)
  else
    result := TFhirEncounterClassEnum(StringArrayIndexOfSensitive(CODES_TFhirEncounterClassEnum, FClass_.value));
end;

Procedure TFhirEncounter.SetClass_ST(value : TFhirEncounterClassEnum);
begin
  if ord(value) = 0 then
    Class_Element := nil
  else
    Class_Element := TFhirEnum.create(SYSTEMS_TFhirEncounterClassEnum[value], CODES_TFhirEncounterClassEnum[value]);
end;

Function TFhirEncounter.GetType_List : TFhirCodeableConceptList;
begin
  if FType_List = nil then
    FType_List := TFhirCodeableConceptList.Create;
  result := FType_List;
end;

Function TFhirEncounter.GetHasType_List : boolean;
begin
  result := (FType_List <> nil) and (FType_List.count > 0);
end;

Procedure TFhirEncounter.SetPriority(value : TFhirCodeableConcept);
begin
  FPriority.free;
  FPriority := value;
end;

Procedure TFhirEncounter.SetPatient(value : TFhirReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Function TFhirEncounter.GetEpisodeOfCareList : TFhirReferenceList{TFhirEpisodeOfCare};
begin
  if FEpisodeOfCareList = nil then
    FEpisodeOfCareList := TFhirReferenceList{TFhirEpisodeOfCare}.Create;
  result := FEpisodeOfCareList;
end;

Function TFhirEncounter.GetHasEpisodeOfCareList : boolean;
begin
  result := (FEpisodeOfCareList <> nil) and (FEpisodeOfCareList.count > 0);
end;

Function TFhirEncounter.GetIncomingReferralList : TFhirReferenceList{TFhirReferralRequest};
begin
  if FIncomingReferralList = nil then
    FIncomingReferralList := TFhirReferenceList{TFhirReferralRequest}.Create;
  result := FIncomingReferralList;
end;

Function TFhirEncounter.GetHasIncomingReferralList : boolean;
begin
  result := (FIncomingReferralList <> nil) and (FIncomingReferralList.count > 0);
end;

Function TFhirEncounter.GetParticipantList : TFhirEncounterParticipantList;
begin
  if FParticipantList = nil then
    FParticipantList := TFhirEncounterParticipantList.Create;
  result := FParticipantList;
end;

Function TFhirEncounter.GetHasParticipantList : boolean;
begin
  result := (FParticipantList <> nil) and (FParticipantList.count > 0);
end;

Procedure TFhirEncounter.SetAppointment(value : TFhirReference{TFhirAppointment});
begin
  FAppointment.free;
  FAppointment := value;
end;

Procedure TFhirEncounter.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Procedure TFhirEncounter.SetLength(value : TFhirQuantity);
begin
  FLength.free;
  FLength := value;
end;

Function TFhirEncounter.GetReasonList : TFhirCodeableConceptList;
begin
  if FReasonList = nil then
    FReasonList := TFhirCodeableConceptList.Create;
  result := FReasonList;
end;

Function TFhirEncounter.GetHasReasonList : boolean;
begin
  result := (FReasonList <> nil) and (FReasonList.count > 0);
end;

Function TFhirEncounter.GetIndicationList : TFhirReferenceList{Resource};
begin
  if FIndicationList = nil then
    FIndicationList := TFhirReferenceList{Resource}.Create;
  result := FIndicationList;
end;

Function TFhirEncounter.GetHasIndicationList : boolean;
begin
  result := (FIndicationList <> nil) and (FIndicationList.count > 0);
end;

Procedure TFhirEncounter.SetHospitalization(value : TFhirEncounterHospitalization);
begin
  FHospitalization.free;
  FHospitalization := value;
end;

Function TFhirEncounter.GetLocationList : TFhirEncounterLocationList;
begin
  if FLocationList = nil then
    FLocationList := TFhirEncounterLocationList.Create;
  result := FLocationList;
end;

Function TFhirEncounter.GetHasLocationList : boolean;
begin
  result := (FLocationList <> nil) and (FLocationList.count > 0);
end;

Procedure TFhirEncounter.SetServiceProvider(value : TFhirReference{TFhirOrganization});
begin
  FServiceProvider.free;
  FServiceProvider := value;
end;

Procedure TFhirEncounter.SetPartOf(value : TFhirReference{TFhirEncounter});
begin
  FPartOf.free;
  FPartOf := value;
end;

function TFhirEncounter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FStatus.sizeInBytes);
  inc(result, FstatusHistoryList.sizeInBytes);
  inc(result, FClass_.sizeInBytes);
  inc(result, Ftype_List.sizeInBytes);
  inc(result, FPriority.sizeInBytes);
  inc(result, FPatient.sizeInBytes);
  inc(result, FepisodeOfCareList.sizeInBytes);
  inc(result, FincomingReferralList.sizeInBytes);
  inc(result, FparticipantList.sizeInBytes);
  inc(result, FAppointment.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FLength.sizeInBytes);
  inc(result, FreasonList.sizeInBytes);
  inc(result, FindicationList.sizeInBytes);
  inc(result, FHospitalization.sizeInBytes);
  inc(result, FlocationList.sizeInBytes);
  inc(result, FServiceProvider.sizeInBytes);
  inc(result, FPartOf.sizeInBytes);
end;

{ TFhirEncounterListEnumerator }

Constructor TFhirEncounterListEnumerator.Create(list : TFhirEncounterList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEncounterListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEncounterListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEncounterListEnumerator.GetCurrent : TFhirEncounter;
begin
  Result := FList[FIndex];
end;

function TFhirEncounterListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirEncounterList }
procedure TFhirEncounterList.AddItem(value: TFhirEncounter);
begin
  assert(value.ClassName = 'TFhirEncounter', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEncounter');
  add(value);
end;

function TFhirEncounterList.Append: TFhirEncounter;
begin
  result := TFhirEncounter.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterList.ClearItems;
begin
  Clear;
end;

function TFhirEncounterList.GetEnumerator : TFhirEncounterListEnumerator;
begin
  result := TFhirEncounterListEnumerator.Create(self.link);
end;

function TFhirEncounterList.Clone: TFhirEncounterList;
begin
  result := TFhirEncounterList(inherited Clone);
end;

function TFhirEncounterList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEncounterList.GetItemN(index: Integer): TFhirEncounter;
begin
  result := TFhirEncounter(ObjectByIndex[index]);
end;

function TFhirEncounterList.ItemClass: TFslObjectClass;
begin
  result := TFhirEncounter;
end;
function TFhirEncounterList.IndexOf(value: TFhirEncounter): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirEncounterList.Insert(index: Integer): TFhirEncounter;
begin
  result := TFhirEncounter.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEncounterList.InsertItem(index: Integer; value: TFhirEncounter);
begin
  assert(value is TFhirEncounter);
  Inherited Insert(index, value);
end;

function TFhirEncounterList.Item(index: Integer): TFhirEncounter;
begin
  result := TFhirEncounter(ObjectByIndex[index]);
end;

function TFhirEncounterList.Link: TFhirEncounterList;
begin
  result := TFhirEncounterList(inherited Link);
end;

procedure TFhirEncounterList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEncounterList.SetItemByIndex(index: Integer; value: TFhirEncounter);
begin
  assert(value is TFhirEncounter);
  FhirEncounters[index] := value;
end;

procedure TFhirEncounterList.SetItemN(index: Integer; value: TFhirEncounter);
begin
  assert(value is TFhirEncounter);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_ENCOUNTER}

{$IFDEF FHIR_EPISODEOFCARE}

{ TFhirEpisodeOfCareStatusHistory }

constructor TFhirEpisodeOfCareStatusHistory.Create;
begin
  inherited;
end;

destructor TFhirEpisodeOfCareStatusHistory.Destroy;
begin
  FStatus.free;
  FPeriod.free;
  inherited;
end;

procedure TFhirEpisodeOfCareStatusHistory.Assign(oSource : TFslObject);
begin
  inherited;
  FStatus := TFhirEpisodeOfCareStatusHistory(oSource).FStatus.Link;
  period := TFhirEpisodeOfCareStatusHistory(oSource).period.Clone;
end;

procedure TFhirEpisodeOfCareStatusHistory.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'status') Then
     list.add(self.link, 'status', FStatus.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
end;

procedure TFhirEpisodeOfCareStatusHistory.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'status', 'code', false, TFHIREnum, FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
end;

function TFhirEpisodeOfCareStatusHistory.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'status') then
  begin
    StatusElement := asEnum(SYSTEMS_TFhirEpisodeOfCareStatusEnum, CODES_TFhirEpisodeOfCareStatusEnum, propValue);
    result := propValue
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirEpisodeOfCareStatusHistory.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirEpisodeOfCareStatusHistory.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirEpisodeOfCareStatusHistory.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'status') then result := 'code'
  else if (propName = 'period') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirEpisodeOfCareStatusHistory.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'status') then StatusElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirEpisodeOfCareStatusHistory.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'status') then StatusElement := asEnum(SYSTEMS_TFhirEpisodeOfCareStatusEnum, CODES_TFhirEpisodeOfCareStatusEnum, new){4}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirEpisodeOfCareStatusHistory.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirEpisodeOfCareStatusHistory.fhirType : string;
begin
  result := 'statusHistory';
end;

function TFhirEpisodeOfCareStatusHistory.Link : TFhirEpisodeOfCareStatusHistory;
begin
  result := TFhirEpisodeOfCareStatusHistory(inherited Link);
end;

function TFhirEpisodeOfCareStatusHistory.Clone : TFhirEpisodeOfCareStatusHistory;
begin
  result := TFhirEpisodeOfCareStatusHistory(inherited Clone);
end;

function TFhirEpisodeOfCareStatusHistory.equals(other : TObject) : boolean;
var
  o : TFhirEpisodeOfCareStatusHistory;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEpisodeOfCareStatusHistory)) then
    result := false
  else
  begin
    o := TFhirEpisodeOfCareStatusHistory(other);
    result := compareDeep(statusElement, o.statusElement, true) and compareDeep(periodElement, o.periodElement, true);
  end;
end;

function TFhirEpisodeOfCareStatusHistory.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FStatus) and isEmptyProp(FPeriod);
end;

procedure TFhirEpisodeOfCareStatusHistory.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('status');
  fields.add('period');
end;

{ TFhirEpisodeOfCareStatusHistory }

Procedure TFhirEpisodeOfCareStatusHistory.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirEpisodeOfCareStatusHistory.GetStatusST : TFhirEpisodeOfCareStatusEnum;
begin
  if FStatus = nil then
    result := TFhirEpisodeOfCareStatusEnum(0)
  else
    result := TFhirEpisodeOfCareStatusEnum(StringArrayIndexOfSensitive(CODES_TFhirEpisodeOfCareStatusEnum, FStatus.value));
end;

Procedure TFhirEpisodeOfCareStatusHistory.SetStatusST(value : TFhirEpisodeOfCareStatusEnum);
begin
  if ord(value) = 0 then
    StatusElement := nil
  else
    StatusElement := TFhirEnum.create(SYSTEMS_TFhirEpisodeOfCareStatusEnum[value], CODES_TFhirEpisodeOfCareStatusEnum[value]);
end;

Procedure TFhirEpisodeOfCareStatusHistory.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

function TFhirEpisodeOfCareStatusHistory.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStatus.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
end;

{ TFhirEpisodeOfCareStatusHistoryListEnumerator }

Constructor TFhirEpisodeOfCareStatusHistoryListEnumerator.Create(list : TFhirEpisodeOfCareStatusHistoryList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEpisodeOfCareStatusHistoryListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEpisodeOfCareStatusHistoryListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEpisodeOfCareStatusHistoryListEnumerator.GetCurrent : TFhirEpisodeOfCareStatusHistory;
begin
  Result := FList[FIndex];
end;

function TFhirEpisodeOfCareStatusHistoryListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirEpisodeOfCareStatusHistoryList }
procedure TFhirEpisodeOfCareStatusHistoryList.AddItem(value: TFhirEpisodeOfCareStatusHistory);
begin
  assert(value.ClassName = 'TFhirEpisodeOfCareStatusHistory', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEpisodeOfCareStatusHistory');
  add(value);
end;

function TFhirEpisodeOfCareStatusHistoryList.Append: TFhirEpisodeOfCareStatusHistory;
begin
  result := TFhirEpisodeOfCareStatusHistory.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEpisodeOfCareStatusHistoryList.ClearItems;
begin
  Clear;
end;

function TFhirEpisodeOfCareStatusHistoryList.GetEnumerator : TFhirEpisodeOfCareStatusHistoryListEnumerator;
begin
  result := TFhirEpisodeOfCareStatusHistoryListEnumerator.Create(self.link);
end;

function TFhirEpisodeOfCareStatusHistoryList.Clone: TFhirEpisodeOfCareStatusHistoryList;
begin
  result := TFhirEpisodeOfCareStatusHistoryList(inherited Clone);
end;

function TFhirEpisodeOfCareStatusHistoryList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEpisodeOfCareStatusHistoryList.GetItemN(index: Integer): TFhirEpisodeOfCareStatusHistory;
begin
  result := TFhirEpisodeOfCareStatusHistory(ObjectByIndex[index]);
end;

function TFhirEpisodeOfCareStatusHistoryList.ItemClass: TFslObjectClass;
begin
  result := TFhirEpisodeOfCareStatusHistory;
end;
function TFhirEpisodeOfCareStatusHistoryList.IndexOf(value: TFhirEpisodeOfCareStatusHistory): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirEpisodeOfCareStatusHistoryList.Insert(index: Integer): TFhirEpisodeOfCareStatusHistory;
begin
  result := TFhirEpisodeOfCareStatusHistory.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEpisodeOfCareStatusHistoryList.InsertItem(index: Integer; value: TFhirEpisodeOfCareStatusHistory);
begin
  assert(value is TFhirEpisodeOfCareStatusHistory);
  Inherited Insert(index, value);
end;

function TFhirEpisodeOfCareStatusHistoryList.Item(index: Integer): TFhirEpisodeOfCareStatusHistory;
begin
  result := TFhirEpisodeOfCareStatusHistory(ObjectByIndex[index]);
end;

function TFhirEpisodeOfCareStatusHistoryList.Link: TFhirEpisodeOfCareStatusHistoryList;
begin
  result := TFhirEpisodeOfCareStatusHistoryList(inherited Link);
end;

procedure TFhirEpisodeOfCareStatusHistoryList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEpisodeOfCareStatusHistoryList.SetItemByIndex(index: Integer; value: TFhirEpisodeOfCareStatusHistory);
begin
  assert(value is TFhirEpisodeOfCareStatusHistory);
  FhirEpisodeOfCareStatusHistories[index] := value;
end;

procedure TFhirEpisodeOfCareStatusHistoryList.SetItemN(index: Integer; value: TFhirEpisodeOfCareStatusHistory);
begin
  assert(value is TFhirEpisodeOfCareStatusHistory);
  ObjectByIndex[index] := value;
end;

{ TFhirEpisodeOfCareCareTeam }

constructor TFhirEpisodeOfCareCareTeam.Create;
begin
  inherited;
end;

destructor TFhirEpisodeOfCareCareTeam.Destroy;
begin
  FRoleList.Free;
  FPeriod.free;
  FMember.free;
  inherited;
end;

procedure TFhirEpisodeOfCareCareTeam.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirEpisodeOfCareCareTeam(oSource).FRoleList = nil) then
  begin
    FRoleList.free;
    FRoleList := nil;
  end
  else
  begin
    if FRoleList = nil then
      FRoleList := TFhirCodeableConceptList.Create;
    FRoleList.Assign(TFhirEpisodeOfCareCareTeam(oSource).FRoleList);
  end;
  period := TFhirEpisodeOfCareCareTeam(oSource).period.Clone;
  member := TFhirEpisodeOfCareCareTeam(oSource).member.Clone;
end;

procedure TFhirEpisodeOfCareCareTeam.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'role') Then
    list.addAll(self, 'role', FRoleList);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'member') Then
     list.add(self.link, 'member', FMember.Link);
end;

procedure TFhirEpisodeOfCareCareTeam.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'role', 'CodeableConcept', true, TFhirCodeableConcept, FRoleList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'member', 'Reference(Practitioner|Organization)', false, TFhirReference{Resource}, FMember.Link));{2}
end;

function TFhirEpisodeOfCareCareTeam.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'role') then
  begin
    RoleList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else if (propName = 'member') then
  begin
    Member := propValue as TFhirReference{Resource}{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirEpisodeOfCareCareTeam.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'role') then RoleList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else inherited;
end;

function TFhirEpisodeOfCareCareTeam.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'role') then result := RoleList.new(){2}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else if (propName = 'member') then result := TFhirReference{Resource}.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirEpisodeOfCareCareTeam.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'role') then result := 'CodeableConcept'
  else if (propName = 'period') then result := 'Period'
  else if (propName = 'member') then result := 'Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirEpisodeOfCareCareTeam.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'role') then deletePropertyValue('role', RoleList, value) {2}
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'member') then MemberElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirEpisodeOfCareCareTeam.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'role') then replacePropertyValue('role', RoleList, existing, new) {2}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else if (propName = 'member') then MemberElement := new as TFhirReference{Resource}{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirEpisodeOfCareCareTeam.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'role') then RoleList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirEpisodeOfCareCareTeam.fhirType : string;
begin
  result := 'careTeam';
end;

function TFhirEpisodeOfCareCareTeam.Link : TFhirEpisodeOfCareCareTeam;
begin
  result := TFhirEpisodeOfCareCareTeam(inherited Link);
end;

function TFhirEpisodeOfCareCareTeam.Clone : TFhirEpisodeOfCareCareTeam;
begin
  result := TFhirEpisodeOfCareCareTeam(inherited Clone);
end;

function TFhirEpisodeOfCareCareTeam.equals(other : TObject) : boolean;
var
  o : TFhirEpisodeOfCareCareTeam;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEpisodeOfCareCareTeam)) then
    result := false
  else
  begin
    o := TFhirEpisodeOfCareCareTeam(other);
    result := compareDeep(roleList, o.roleList, true) and compareDeep(periodElement, o.periodElement, true) and
      compareDeep(memberElement, o.memberElement, true);
  end;
end;

function TFhirEpisodeOfCareCareTeam.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FroleList) and isEmptyProp(FPeriod) and isEmptyProp(FMember);
end;

procedure TFhirEpisodeOfCareCareTeam.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('role');
  fields.add('period');
  fields.add('member');
end;

{ TFhirEpisodeOfCareCareTeam }

Function TFhirEpisodeOfCareCareTeam.GetRoleList : TFhirCodeableConceptList;
begin
  if FRoleList = nil then
    FRoleList := TFhirCodeableConceptList.Create;
  result := FRoleList;
end;

Function TFhirEpisodeOfCareCareTeam.GetHasRoleList : boolean;
begin
  result := (FRoleList <> nil) and (FRoleList.count > 0);
end;

Procedure TFhirEpisodeOfCareCareTeam.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Procedure TFhirEpisodeOfCareCareTeam.SetMember(value : TFhirReference{Resource});
begin
  FMember.free;
  FMember := value;
end;

function TFhirEpisodeOfCareCareTeam.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FroleList.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FMember.sizeInBytes);
end;

{ TFhirEpisodeOfCareCareTeamListEnumerator }

Constructor TFhirEpisodeOfCareCareTeamListEnumerator.Create(list : TFhirEpisodeOfCareCareTeamList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEpisodeOfCareCareTeamListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEpisodeOfCareCareTeamListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEpisodeOfCareCareTeamListEnumerator.GetCurrent : TFhirEpisodeOfCareCareTeam;
begin
  Result := FList[FIndex];
end;

function TFhirEpisodeOfCareCareTeamListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirEpisodeOfCareCareTeamList }
procedure TFhirEpisodeOfCareCareTeamList.AddItem(value: TFhirEpisodeOfCareCareTeam);
begin
  assert(value.ClassName = 'TFhirEpisodeOfCareCareTeam', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEpisodeOfCareCareTeam');
  add(value);
end;

function TFhirEpisodeOfCareCareTeamList.Append: TFhirEpisodeOfCareCareTeam;
begin
  result := TFhirEpisodeOfCareCareTeam.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEpisodeOfCareCareTeamList.ClearItems;
begin
  Clear;
end;

function TFhirEpisodeOfCareCareTeamList.GetEnumerator : TFhirEpisodeOfCareCareTeamListEnumerator;
begin
  result := TFhirEpisodeOfCareCareTeamListEnumerator.Create(self.link);
end;

function TFhirEpisodeOfCareCareTeamList.Clone: TFhirEpisodeOfCareCareTeamList;
begin
  result := TFhirEpisodeOfCareCareTeamList(inherited Clone);
end;

function TFhirEpisodeOfCareCareTeamList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEpisodeOfCareCareTeamList.GetItemN(index: Integer): TFhirEpisodeOfCareCareTeam;
begin
  result := TFhirEpisodeOfCareCareTeam(ObjectByIndex[index]);
end;

function TFhirEpisodeOfCareCareTeamList.ItemClass: TFslObjectClass;
begin
  result := TFhirEpisodeOfCareCareTeam;
end;
function TFhirEpisodeOfCareCareTeamList.IndexOf(value: TFhirEpisodeOfCareCareTeam): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirEpisodeOfCareCareTeamList.Insert(index: Integer): TFhirEpisodeOfCareCareTeam;
begin
  result := TFhirEpisodeOfCareCareTeam.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEpisodeOfCareCareTeamList.InsertItem(index: Integer; value: TFhirEpisodeOfCareCareTeam);
begin
  assert(value is TFhirEpisodeOfCareCareTeam);
  Inherited Insert(index, value);
end;

function TFhirEpisodeOfCareCareTeamList.Item(index: Integer): TFhirEpisodeOfCareCareTeam;
begin
  result := TFhirEpisodeOfCareCareTeam(ObjectByIndex[index]);
end;

function TFhirEpisodeOfCareCareTeamList.Link: TFhirEpisodeOfCareCareTeamList;
begin
  result := TFhirEpisodeOfCareCareTeamList(inherited Link);
end;

procedure TFhirEpisodeOfCareCareTeamList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEpisodeOfCareCareTeamList.SetItemByIndex(index: Integer; value: TFhirEpisodeOfCareCareTeam);
begin
  assert(value is TFhirEpisodeOfCareCareTeam);
  FhirEpisodeOfCareCareTeams[index] := value;
end;

procedure TFhirEpisodeOfCareCareTeamList.SetItemN(index: Integer; value: TFhirEpisodeOfCareCareTeam);
begin
  assert(value is TFhirEpisodeOfCareCareTeam);
  ObjectByIndex[index] := value;
end;

{ TFhirEpisodeOfCare }

constructor TFhirEpisodeOfCare.Create;
begin
  inherited;
end;

destructor TFhirEpisodeOfCare.Destroy;
begin
  FIdentifierList.Free;
  FStatus.free;
  FStatusHistoryList.Free;
  FType_List.Free;
  FConditionList.Free;
  FPatient.free;
  FManagingOrganization.free;
  FPeriod.free;
  FReferralRequestList.Free;
  FCareManager.free;
  FCareTeamList.Free;
  inherited;
end;

function TFhirEpisodeOfCare.GetResourceType : TFhirResourceType;
begin
  result := frtEpisodeOfCare;
end;

procedure TFhirEpisodeOfCare.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirEpisodeOfCare(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirEpisodeOfCare(oSource).FIdentifierList);
  end;
  FStatus := TFhirEpisodeOfCare(oSource).FStatus.Link;
  if (TFhirEpisodeOfCare(oSource).FStatusHistoryList = nil) then
  begin
    FStatusHistoryList.free;
    FStatusHistoryList := nil;
  end
  else
  begin
    if FStatusHistoryList = nil then
      FStatusHistoryList := TFhirEpisodeOfCareStatusHistoryList.Create;
    FStatusHistoryList.Assign(TFhirEpisodeOfCare(oSource).FStatusHistoryList);
  end;
  if (TFhirEpisodeOfCare(oSource).FType_List = nil) then
  begin
    FType_List.free;
    FType_List := nil;
  end
  else
  begin
    if FType_List = nil then
      FType_List := TFhirCodeableConceptList.Create;
    FType_List.Assign(TFhirEpisodeOfCare(oSource).FType_List);
  end;
  if (TFhirEpisodeOfCare(oSource).FConditionList = nil) then
  begin
    FConditionList.free;
    FConditionList := nil;
  end
  else
  begin
    if FConditionList = nil then
      FConditionList := TFhirReferenceList{TFhirCondition}.Create;
    FConditionList.Assign(TFhirEpisodeOfCare(oSource).FConditionList);
  end;
  patient := TFhirEpisodeOfCare(oSource).patient.Clone;
  managingOrganization := TFhirEpisodeOfCare(oSource).managingOrganization.Clone;
  period := TFhirEpisodeOfCare(oSource).period.Clone;
  if (TFhirEpisodeOfCare(oSource).FReferralRequestList = nil) then
  begin
    FReferralRequestList.free;
    FReferralRequestList := nil;
  end
  else
  begin
    if FReferralRequestList = nil then
      FReferralRequestList := TFhirReferenceList{TFhirReferralRequest}.Create;
    FReferralRequestList.Assign(TFhirEpisodeOfCare(oSource).FReferralRequestList);
  end;
  careManager := TFhirEpisodeOfCare(oSource).careManager.Clone;
  if (TFhirEpisodeOfCare(oSource).FCareTeamList = nil) then
  begin
    FCareTeamList.free;
    FCareTeamList := nil;
  end
  else
  begin
    if FCareTeamList = nil then
      FCareTeamList := TFhirEpisodeOfCareCareTeamList.Create;
    FCareTeamList.Assign(TFhirEpisodeOfCare(oSource).FCareTeamList);
  end;
end;

procedure TFhirEpisodeOfCare.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'status') Then
     list.add(self.link, 'status', FStatus.Link);
  if (child_name = 'statusHistory') Then
    list.addAll(self, 'statusHistory', FStatusHistoryList);
  if (child_name = 'type') Then
    list.addAll(self, 'type', FType_List);
  if (child_name = 'condition') Then
    list.addAll(self, 'condition', FConditionList);
  if (child_name = 'patient') Then
     list.add(self.link, 'patient', FPatient.Link);
  if (child_name = 'managingOrganization') Then
     list.add(self.link, 'managingOrganization', FManagingOrganization.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'referralRequest') Then
    list.addAll(self, 'referralRequest', FReferralRequestList);
  if (child_name = 'careManager') Then
     list.add(self.link, 'careManager', FCareManager.Link);
  if (child_name = 'careTeam') Then
    list.addAll(self, 'careTeam', FCareTeamList);
end;

procedure TFhirEpisodeOfCare.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', false, TFHIREnum, FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'statusHistory', '', true, TFhirEpisodeOfCareStatusHistory, FStatusHistoryList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', true, TFhirCodeableConcept, FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'condition', 'Reference(Condition)', true, TFhirReference{TFhirCondition}, FConditionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'patient', 'Reference(Patient)', false, TFhirReference{TFhirPatient}, FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'managingOrganization', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FManagingOrganization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'referralRequest', 'Reference(ReferralRequest)', true, TFhirReference{TFhirReferralRequest}, FReferralRequestList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'careManager', 'Reference(Practitioner)', false, TFhirReference{TFhirPractitioner}, FCareManager.Link));{2}
  oList.add(TFHIRProperty.create(self, 'careTeam', '', true, TFhirEpisodeOfCareCareTeam, FCareTeamList.Link)){3};
end;

function TFhirEpisodeOfCare.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'status') then
  begin
    StatusElement := asEnum(SYSTEMS_TFhirEpisodeOfCareStatusEnum, CODES_TFhirEpisodeOfCareStatusEnum, propValue);
    result := propValue
  end
  else if (propName = 'statusHistory') then
  begin
    StatusHistoryList.add(propValue as TFhirEpisodeOfCareStatusHistory){2a};
    result := propValue;
  end
  else if (propName = 'type') then
  begin
    Type_List.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'condition') then
  begin
    ConditionList.add(propValue as TFhirReference{TFhirCondition}){2a};
    result := propValue;
  end
  else if (propName = 'patient') then
  begin
    Patient := propValue as TFhirReference{TFhirPatient}{4b};
    result := propValue;
  end
  else if (propName = 'managingOrganization') then
  begin
    ManagingOrganization := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else if (propName = 'referralRequest') then
  begin
    ReferralRequestList.add(propValue as TFhirReference{TFhirReferralRequest}){2a};
    result := propValue;
  end
  else if (propName = 'careManager') then
  begin
    CareManager := propValue as TFhirReference{TFhirPractitioner}{4b};
    result := propValue;
  end
  else if (propName = 'careTeam') then
  begin
    CareTeamList.add(propValue as TFhirEpisodeOfCareCareTeam){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirEpisodeOfCare.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'statusHistory') then StatusHistoryList.insertItem(index, propValue as TFhirEpisodeOfCareStatusHistory){2a}
  else if (propName = 'type') then Type_List.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'condition') then ConditionList.insertItem(index, propValue as TFhirReference{TFhirCondition}){2a}
  else if (propName = 'referralRequest') then ReferralRequestList.insertItem(index, propValue as TFhirReference{TFhirReferralRequest}){2a}
  else if (propName = 'careTeam') then CareTeamList.insertItem(index, propValue as TFhirEpisodeOfCareCareTeam){2a}
  else inherited;
end;

function TFhirEpisodeOfCare.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'statusHistory') then result := StatusHistoryList.new(){2}
  else if (propName = 'type') then result := Type_List.new(){2}
  else if (propName = 'condition') then result := ConditionList.new(){2}
  else if (propName = 'patient') then result := TFhirReference{TFhirPatient}.create(){4b}
  else if (propName = 'managingOrganization') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else if (propName = 'referralRequest') then result := ReferralRequestList.new(){2}
  else if (propName = 'careManager') then result := TFhirReference{TFhirPractitioner}.create(){4b}
  else if (propName = 'careTeam') then result := CareTeamList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirEpisodeOfCare.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'status') then result := 'code'
  else if (propName = 'statusHistory') then result := ''
  else if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'condition') then result := 'Reference'
  else if (propName = 'patient') then result := 'Reference'
  else if (propName = 'managingOrganization') then result := 'Reference'
  else if (propName = 'period') then result := 'Period'
  else if (propName = 'referralRequest') then result := 'Reference'
  else if (propName = 'careManager') then result := 'Reference'
  else if (propName = 'careTeam') then result := ''
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirEpisodeOfCare.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'status') then StatusElement := nil
  else if (propName = 'statusHistory') then deletePropertyValue('statusHistory', StatusHistoryList, value) {2}
  else if (propName = 'type') then deletePropertyValue('type', Type_List, value) {2}
  else if (propName = 'condition') then deletePropertyValue('condition', ConditionList, value) {2}
  else if (propName = 'patient') then PatientElement := nil
  else if (propName = 'managingOrganization') then ManagingOrganizationElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'referralRequest') then deletePropertyValue('referralRequest', ReferralRequestList, value) {2}
  else if (propName = 'careManager') then CareManagerElement := nil
  else if (propName = 'careTeam') then deletePropertyValue('careTeam', CareTeamList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirEpisodeOfCare.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'status') then StatusElement := asEnum(SYSTEMS_TFhirEpisodeOfCareStatusEnum, CODES_TFhirEpisodeOfCareStatusEnum, new){4}
  else if (propName = 'statusHistory') then replacePropertyValue('statusHistory', StatusHistoryList, existing, new) {2}
  else if (propName = 'type') then replacePropertyValue('type', Type_List, existing, new) {2}
  else if (propName = 'condition') then replacePropertyValue('condition', ConditionList, existing, new) {2}
  else if (propName = 'patient') then PatientElement := new as TFhirReference{TFhirPatient}{4}
  else if (propName = 'managingOrganization') then ManagingOrganizationElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else if (propName = 'referralRequest') then replacePropertyValue('referralRequest', ReferralRequestList, existing, new) {2}
  else if (propName = 'careManager') then CareManagerElement := new as TFhirReference{TFhirPractitioner}{4}
  else if (propName = 'careTeam') then replacePropertyValue('careTeam', CareTeamList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirEpisodeOfCare.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'statusHistory') then StatusHistoryList.move(source, destination){2a}
  else if (propName = 'type') then Type_List.move(source, destination){2a}
  else if (propName = 'condition') then ConditionList.move(source, destination){2a}
  else if (propName = 'referralRequest') then ReferralRequestList.move(source, destination){2a}
  else if (propName = 'careTeam') then CareTeamList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirEpisodeOfCare.fhirType : string;
begin
  result := 'EpisodeOfCare';
end;

function TFhirEpisodeOfCare.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FStatus) and isEmptyProp(FstatusHistoryList) and isEmptyProp(Ftype_List) and isEmptyProp(FconditionList) and isEmptyProp(FPatient) and isEmptyProp(FManagingOrganization) and isEmptyProp(FPeriod) and isEmptyProp(FreferralRequestList) and isEmptyProp(FCareManager) and isEmptyProp(FcareTeamList);
end;

function TFhirEpisodeOfCare.equals(other : TObject) : boolean;
var
  o : TFhirEpisodeOfCare;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirEpisodeOfCare)) then
    result := false
  else
  begin
    o := TFhirEpisodeOfCare(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(statusElement, o.statusElement, true) and
      compareDeep(statusHistoryList, o.statusHistoryList, true) and compareDeep(type_List, o.type_List, true) and
      compareDeep(conditionList, o.conditionList, true) and compareDeep(patientElement, o.patientElement, true) and
      compareDeep(managingOrganizationElement, o.managingOrganizationElement, true) and
      compareDeep(periodElement, o.periodElement, true) and compareDeep(referralRequestList, o.referralRequestList, true) and
      compareDeep(careManagerElement, o.careManagerElement, true) and compareDeep(careTeamList, o.careTeamList, true);
  end;
end;

function TFhirEpisodeOfCare.Link : TFhirEpisodeOfCare;
begin
  result := TFhirEpisodeOfCare(inherited Link);
end;

function TFhirEpisodeOfCare.Clone : TFhirEpisodeOfCare;
begin
  result := TFhirEpisodeOfCare(inherited Clone);
end;

procedure TFhirEpisodeOfCare.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('status');
  fields.add('statusHistory');
  fields.add('type');
  fields.add('condition');
  fields.add('patient');
  fields.add('managingOrganization');
  fields.add('period');
  fields.add('referralRequest');
  fields.add('careManager');
  fields.add('careTeam');
end;

{ TFhirEpisodeOfCare }

Function TFhirEpisodeOfCare.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirEpisodeOfCare.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirEpisodeOfCare.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirEpisodeOfCare.GetStatusST : TFhirEpisodeOfCareStatusEnum;
begin
  if FStatus = nil then
    result := TFhirEpisodeOfCareStatusEnum(0)
  else
    result := TFhirEpisodeOfCareStatusEnum(StringArrayIndexOfSensitive(CODES_TFhirEpisodeOfCareStatusEnum, FStatus.value));
end;

Procedure TFhirEpisodeOfCare.SetStatusST(value : TFhirEpisodeOfCareStatusEnum);
begin
  if ord(value) = 0 then
    StatusElement := nil
  else
    StatusElement := TFhirEnum.create(SYSTEMS_TFhirEpisodeOfCareStatusEnum[value], CODES_TFhirEpisodeOfCareStatusEnum[value]);
end;

Function TFhirEpisodeOfCare.GetStatusHistoryList : TFhirEpisodeOfCareStatusHistoryList;
begin
  if FStatusHistoryList = nil then
    FStatusHistoryList := TFhirEpisodeOfCareStatusHistoryList.Create;
  result := FStatusHistoryList;
end;

Function TFhirEpisodeOfCare.GetHasStatusHistoryList : boolean;
begin
  result := (FStatusHistoryList <> nil) and (FStatusHistoryList.count > 0);
end;

Function TFhirEpisodeOfCare.GetType_List : TFhirCodeableConceptList;
begin
  if FType_List = nil then
    FType_List := TFhirCodeableConceptList.Create;
  result := FType_List;
end;

Function TFhirEpisodeOfCare.GetHasType_List : boolean;
begin
  result := (FType_List <> nil) and (FType_List.count > 0);
end;

Function TFhirEpisodeOfCare.GetConditionList : TFhirReferenceList{TFhirCondition};
begin
  if FConditionList = nil then
    FConditionList := TFhirReferenceList{TFhirCondition}.Create;
  result := FConditionList;
end;

Function TFhirEpisodeOfCare.GetHasConditionList : boolean;
begin
  result := (FConditionList <> nil) and (FConditionList.count > 0);
end;

Procedure TFhirEpisodeOfCare.SetPatient(value : TFhirReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirEpisodeOfCare.SetManagingOrganization(value : TFhirReference{TFhirOrganization});
begin
  FManagingOrganization.free;
  FManagingOrganization := value;
end;

Procedure TFhirEpisodeOfCare.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Function TFhirEpisodeOfCare.GetReferralRequestList : TFhirReferenceList{TFhirReferralRequest};
begin
  if FReferralRequestList = nil then
    FReferralRequestList := TFhirReferenceList{TFhirReferralRequest}.Create;
  result := FReferralRequestList;
end;

Function TFhirEpisodeOfCare.GetHasReferralRequestList : boolean;
begin
  result := (FReferralRequestList <> nil) and (FReferralRequestList.count > 0);
end;

Procedure TFhirEpisodeOfCare.SetCareManager(value : TFhirReference{TFhirPractitioner});
begin
  FCareManager.free;
  FCareManager := value;
end;

Function TFhirEpisodeOfCare.GetCareTeamList : TFhirEpisodeOfCareCareTeamList;
begin
  if FCareTeamList = nil then
    FCareTeamList := TFhirEpisodeOfCareCareTeamList.Create;
  result := FCareTeamList;
end;

Function TFhirEpisodeOfCare.GetHasCareTeamList : boolean;
begin
  result := (FCareTeamList <> nil) and (FCareTeamList.count > 0);
end;

function TFhirEpisodeOfCare.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FStatus.sizeInBytes);
  inc(result, FstatusHistoryList.sizeInBytes);
  inc(result, Ftype_List.sizeInBytes);
  inc(result, FconditionList.sizeInBytes);
  inc(result, FPatient.sizeInBytes);
  inc(result, FManagingOrganization.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FreferralRequestList.sizeInBytes);
  inc(result, FCareManager.sizeInBytes);
  inc(result, FcareTeamList.sizeInBytes);
end;

{ TFhirEpisodeOfCareListEnumerator }

Constructor TFhirEpisodeOfCareListEnumerator.Create(list : TFhirEpisodeOfCareList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirEpisodeOfCareListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirEpisodeOfCareListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirEpisodeOfCareListEnumerator.GetCurrent : TFhirEpisodeOfCare;
begin
  Result := FList[FIndex];
end;

function TFhirEpisodeOfCareListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirEpisodeOfCareList }
procedure TFhirEpisodeOfCareList.AddItem(value: TFhirEpisodeOfCare);
begin
  assert(value.ClassName = 'TFhirEpisodeOfCare', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirEpisodeOfCare');
  add(value);
end;

function TFhirEpisodeOfCareList.Append: TFhirEpisodeOfCare;
begin
  result := TFhirEpisodeOfCare.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEpisodeOfCareList.ClearItems;
begin
  Clear;
end;

function TFhirEpisodeOfCareList.GetEnumerator : TFhirEpisodeOfCareListEnumerator;
begin
  result := TFhirEpisodeOfCareListEnumerator.Create(self.link);
end;

function TFhirEpisodeOfCareList.Clone: TFhirEpisodeOfCareList;
begin
  result := TFhirEpisodeOfCareList(inherited Clone);
end;

function TFhirEpisodeOfCareList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirEpisodeOfCareList.GetItemN(index: Integer): TFhirEpisodeOfCare;
begin
  result := TFhirEpisodeOfCare(ObjectByIndex[index]);
end;

function TFhirEpisodeOfCareList.ItemClass: TFslObjectClass;
begin
  result := TFhirEpisodeOfCare;
end;
function TFhirEpisodeOfCareList.IndexOf(value: TFhirEpisodeOfCare): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirEpisodeOfCareList.Insert(index: Integer): TFhirEpisodeOfCare;
begin
  result := TFhirEpisodeOfCare.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirEpisodeOfCareList.InsertItem(index: Integer; value: TFhirEpisodeOfCare);
begin
  assert(value is TFhirEpisodeOfCare);
  Inherited Insert(index, value);
end;

function TFhirEpisodeOfCareList.Item(index: Integer): TFhirEpisodeOfCare;
begin
  result := TFhirEpisodeOfCare(ObjectByIndex[index]);
end;

function TFhirEpisodeOfCareList.Link: TFhirEpisodeOfCareList;
begin
  result := TFhirEpisodeOfCareList(inherited Link);
end;

procedure TFhirEpisodeOfCareList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirEpisodeOfCareList.SetItemByIndex(index: Integer; value: TFhirEpisodeOfCare);
begin
  assert(value is TFhirEpisodeOfCare);
  FhirEpisodeOfCares[index] := value;
end;

procedure TFhirEpisodeOfCareList.SetItemN(index: Integer; value: TFhirEpisodeOfCare);
begin
  assert(value is TFhirEpisodeOfCare);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_EPISODEOFCARE}

{$IFDEF FHIR_GROUP}

{ TFhirGroupCharacteristic }

constructor TFhirGroupCharacteristic.Create;
begin
  inherited;
end;

destructor TFhirGroupCharacteristic.Destroy;
begin
  FCode.free;
  FValue.free;
  FExclude.free;
  FPeriod.free;
  inherited;
end;

procedure TFhirGroupCharacteristic.Assign(oSource : TFslObject);
begin
  inherited;
  code := TFhirGroupCharacteristic(oSource).code.Clone;
  value := TFhirGroupCharacteristic(oSource).value.Clone;
  excludeElement := TFhirGroupCharacteristic(oSource).excludeElement.Clone;
  period := TFhirGroupCharacteristic(oSource).period.Clone;
end;

procedure TFhirGroupCharacteristic.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'code') Then
     list.add(self.link, 'code', FCode.Link);
  if (child_name = 'value[x]') or (child_name = 'value') Then
     list.add(self.link, 'value[x]', FValue.Link);
  if (child_name = 'exclude') Then
     list.add(self.link, 'exclude', FExclude.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
end;

procedure TFhirGroupCharacteristic.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', false, TFhirCodeableConcept, FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'value[x]', 'CodeableConcept|boolean|Quantity|Range', false, TFhirType, FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'exclude', 'boolean', false, TFhirBoolean, FExclude.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
end;

function TFhirGroupCharacteristic.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'code') then
  begin
    Code := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (isMatchingName(propName, 'value', ['CodeableConcept', 'Boolean', 'Quantity', 'Range'])) then
  begin
    Value := propValue as TFhirType{4};
    result := propValue;
  end
  else if (propName = 'exclude') then
  begin
    ExcludeElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirGroupCharacteristic.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirGroupCharacteristic.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'code') then result := TFhirCodeableConcept.create(){4b}
  else if (isMatchingName(propName, 'value', ['CodeableConcept', 'Boolean', 'Quantity', 'Range'])) then raise EFHIRException.create('Cannot make property Value'){4x}
  else if (propName = 'exclude') then result := TFhirBoolean.create() {5b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirGroupCharacteristic.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'code') then result := 'CodeableConcept'
  else if (propName = 'value[x]') then result := 'CodeableConcept|boolean|Quantity|Range'
  else if (propName = 'exclude') then result := 'boolean'
  else if (propName = 'period') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirGroupCharacteristic.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'code') then CodeElement := nil
  else if (isMatchingName(propName, 'value', ['CodeableConcept', 'Boolean', 'Quantity', 'Range'])) then ValueElement := nil{4x}
  else if (propName = 'exclude') then ExcludeElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirGroupCharacteristic.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'code') then CodeElement := new as TFhirCodeableConcept{4}
  else if (isMatchingName(propName, 'value', ['CodeableConcept', 'Boolean', 'Quantity', 'Range'])) then ValueElement := new as TFhirType{4x}
  else if (propName = 'exclude') then ExcludeElement := asBoolean(new){5b}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirGroupCharacteristic.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirGroupCharacteristic.fhirType : string;
begin
  result := 'characteristic';
end;

function TFhirGroupCharacteristic.Link : TFhirGroupCharacteristic;
begin
  result := TFhirGroupCharacteristic(inherited Link);
end;

function TFhirGroupCharacteristic.Clone : TFhirGroupCharacteristic;
begin
  result := TFhirGroupCharacteristic(inherited Clone);
end;

function TFhirGroupCharacteristic.equals(other : TObject) : boolean;
var
  o : TFhirGroupCharacteristic;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirGroupCharacteristic)) then
    result := false
  else
  begin
    o := TFhirGroupCharacteristic(other);
    result := compareDeep(codeElement, o.codeElement, true) and compareDeep(valueElement, o.valueElement, true) and
      compareDeep(excludeElement, o.excludeElement, true) and compareDeep(periodElement, o.periodElement, true);
  end;
end;

function TFhirGroupCharacteristic.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FCode) and isEmptyProp(FValue) and isEmptyProp(FExclude) and isEmptyProp(FPeriod);
end;

procedure TFhirGroupCharacteristic.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('code');
  fields.add('value[x]');
  fields.add('exclude');
  fields.add('period');
end;

{ TFhirGroupCharacteristic }

Procedure TFhirGroupCharacteristic.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

Procedure TFhirGroupCharacteristic.SetValue(value : TFhirType);
begin
  FValue.free;
  FValue := value;
end;

Procedure TFhirGroupCharacteristic.SetExclude(value : TFhirBoolean);
begin
  FExclude.free;
  FExclude := value;
end;

Function TFhirGroupCharacteristic.GetExcludeST : Boolean;
begin
  if FExclude = nil then
    result := false
  else
    result := FExclude.value;
end;

Procedure TFhirGroupCharacteristic.SetExcludeST(value : Boolean);
begin
  if FExclude = nil then
    FExclude := TFhirBoolean.create;
  FExclude.value := value
end;

Procedure TFhirGroupCharacteristic.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

function TFhirGroupCharacteristic.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCode.sizeInBytes);
  inc(result, FValue.sizeInBytes);
  inc(result, FExclude.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
end;

{ TFhirGroupCharacteristicListEnumerator }

Constructor TFhirGroupCharacteristicListEnumerator.Create(list : TFhirGroupCharacteristicList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirGroupCharacteristicListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirGroupCharacteristicListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirGroupCharacteristicListEnumerator.GetCurrent : TFhirGroupCharacteristic;
begin
  Result := FList[FIndex];
end;

function TFhirGroupCharacteristicListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirGroupCharacteristicList }
procedure TFhirGroupCharacteristicList.AddItem(value: TFhirGroupCharacteristic);
begin
  assert(value.ClassName = 'TFhirGroupCharacteristic', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirGroupCharacteristic');
  add(value);
end;

function TFhirGroupCharacteristicList.Append: TFhirGroupCharacteristic;
begin
  result := TFhirGroupCharacteristic.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirGroupCharacteristicList.ClearItems;
begin
  Clear;
end;

function TFhirGroupCharacteristicList.GetEnumerator : TFhirGroupCharacteristicListEnumerator;
begin
  result := TFhirGroupCharacteristicListEnumerator.Create(self.link);
end;

function TFhirGroupCharacteristicList.Clone: TFhirGroupCharacteristicList;
begin
  result := TFhirGroupCharacteristicList(inherited Clone);
end;

function TFhirGroupCharacteristicList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirGroupCharacteristicList.GetItemN(index: Integer): TFhirGroupCharacteristic;
begin
  result := TFhirGroupCharacteristic(ObjectByIndex[index]);
end;

function TFhirGroupCharacteristicList.ItemClass: TFslObjectClass;
begin
  result := TFhirGroupCharacteristic;
end;
function TFhirGroupCharacteristicList.IndexOf(value: TFhirGroupCharacteristic): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirGroupCharacteristicList.Insert(index: Integer): TFhirGroupCharacteristic;
begin
  result := TFhirGroupCharacteristic.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirGroupCharacteristicList.InsertItem(index: Integer; value: TFhirGroupCharacteristic);
begin
  assert(value is TFhirGroupCharacteristic);
  Inherited Insert(index, value);
end;

function TFhirGroupCharacteristicList.Item(index: Integer): TFhirGroupCharacteristic;
begin
  result := TFhirGroupCharacteristic(ObjectByIndex[index]);
end;

function TFhirGroupCharacteristicList.Link: TFhirGroupCharacteristicList;
begin
  result := TFhirGroupCharacteristicList(inherited Link);
end;

procedure TFhirGroupCharacteristicList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirGroupCharacteristicList.SetItemByIndex(index: Integer; value: TFhirGroupCharacteristic);
begin
  assert(value is TFhirGroupCharacteristic);
  FhirGroupCharacteristics[index] := value;
end;

procedure TFhirGroupCharacteristicList.SetItemN(index: Integer; value: TFhirGroupCharacteristic);
begin
  assert(value is TFhirGroupCharacteristic);
  ObjectByIndex[index] := value;
end;

{ TFhirGroupMember }

constructor TFhirGroupMember.Create;
begin
  inherited;
end;

destructor TFhirGroupMember.Destroy;
begin
  FEntity.free;
  FPeriod.free;
  FInactive.free;
  inherited;
end;

procedure TFhirGroupMember.Assign(oSource : TFslObject);
begin
  inherited;
  entity := TFhirGroupMember(oSource).entity.Clone;
  period := TFhirGroupMember(oSource).period.Clone;
  inactiveElement := TFhirGroupMember(oSource).inactiveElement.Clone;
end;

procedure TFhirGroupMember.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'entity') Then
     list.add(self.link, 'entity', FEntity.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'inactive') Then
     list.add(self.link, 'inactive', FInactive.Link);
end;

procedure TFhirGroupMember.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'entity', 'Reference(Patient|Practitioner|Device|Medication|Substance)', false, TFhirReference{Resource}, FEntity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'inactive', 'boolean', false, TFhirBoolean, FInactive.Link));{2}
end;

function TFhirGroupMember.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'entity') then
  begin
    Entity := propValue as TFhirReference{Resource}{4b};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else if (propName = 'inactive') then
  begin
    InactiveElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirGroupMember.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirGroupMember.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'entity') then result := TFhirReference{Resource}.create(){4b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else if (propName = 'inactive') then result := TFhirBoolean.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirGroupMember.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'entity') then result := 'Reference'
  else if (propName = 'period') then result := 'Period'
  else if (propName = 'inactive') then result := 'boolean'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirGroupMember.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'entity') then EntityElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'inactive') then InactiveElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirGroupMember.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'entity') then EntityElement := new as TFhirReference{Resource}{4}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else if (propName = 'inactive') then InactiveElement := asBoolean(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirGroupMember.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirGroupMember.fhirType : string;
begin
  result := 'member';
end;

function TFhirGroupMember.Link : TFhirGroupMember;
begin
  result := TFhirGroupMember(inherited Link);
end;

function TFhirGroupMember.Clone : TFhirGroupMember;
begin
  result := TFhirGroupMember(inherited Clone);
end;

function TFhirGroupMember.equals(other : TObject) : boolean;
var
  o : TFhirGroupMember;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirGroupMember)) then
    result := false
  else
  begin
    o := TFhirGroupMember(other);
    result := compareDeep(entityElement, o.entityElement, true) and compareDeep(periodElement, o.periodElement, true) and
      compareDeep(inactiveElement, o.inactiveElement, true);
  end;
end;

function TFhirGroupMember.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FEntity) and isEmptyProp(FPeriod) and isEmptyProp(FInactive);
end;

procedure TFhirGroupMember.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('entity');
  fields.add('period');
  fields.add('inactive');
end;

{ TFhirGroupMember }

Procedure TFhirGroupMember.SetEntity(value : TFhirReference{Resource});
begin
  FEntity.free;
  FEntity := value;
end;

Procedure TFhirGroupMember.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Procedure TFhirGroupMember.SetInactive(value : TFhirBoolean);
begin
  FInactive.free;
  FInactive := value;
end;

Function TFhirGroupMember.GetInactiveST : Boolean;
begin
  if FInactive = nil then
    result := false
  else
    result := FInactive.value;
end;

Procedure TFhirGroupMember.SetInactiveST(value : Boolean);
begin
  if FInactive = nil then
    FInactive := TFhirBoolean.create;
  FInactive.value := value
end;

function TFhirGroupMember.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FEntity.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FInactive.sizeInBytes);
end;

{ TFhirGroupMemberListEnumerator }

Constructor TFhirGroupMemberListEnumerator.Create(list : TFhirGroupMemberList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirGroupMemberListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirGroupMemberListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirGroupMemberListEnumerator.GetCurrent : TFhirGroupMember;
begin
  Result := FList[FIndex];
end;

function TFhirGroupMemberListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirGroupMemberList }
procedure TFhirGroupMemberList.AddItem(value: TFhirGroupMember);
begin
  assert(value.ClassName = 'TFhirGroupMember', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirGroupMember');
  add(value);
end;

function TFhirGroupMemberList.Append: TFhirGroupMember;
begin
  result := TFhirGroupMember.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirGroupMemberList.ClearItems;
begin
  Clear;
end;

function TFhirGroupMemberList.GetEnumerator : TFhirGroupMemberListEnumerator;
begin
  result := TFhirGroupMemberListEnumerator.Create(self.link);
end;

function TFhirGroupMemberList.Clone: TFhirGroupMemberList;
begin
  result := TFhirGroupMemberList(inherited Clone);
end;

function TFhirGroupMemberList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirGroupMemberList.GetItemN(index: Integer): TFhirGroupMember;
begin
  result := TFhirGroupMember(ObjectByIndex[index]);
end;

function TFhirGroupMemberList.ItemClass: TFslObjectClass;
begin
  result := TFhirGroupMember;
end;
function TFhirGroupMemberList.IndexOf(value: TFhirGroupMember): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirGroupMemberList.Insert(index: Integer): TFhirGroupMember;
begin
  result := TFhirGroupMember.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirGroupMemberList.InsertItem(index: Integer; value: TFhirGroupMember);
begin
  assert(value is TFhirGroupMember);
  Inherited Insert(index, value);
end;

function TFhirGroupMemberList.Item(index: Integer): TFhirGroupMember;
begin
  result := TFhirGroupMember(ObjectByIndex[index]);
end;

function TFhirGroupMemberList.Link: TFhirGroupMemberList;
begin
  result := TFhirGroupMemberList(inherited Link);
end;

procedure TFhirGroupMemberList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirGroupMemberList.SetItemByIndex(index: Integer; value: TFhirGroupMember);
begin
  assert(value is TFhirGroupMember);
  FhirGroupMembers[index] := value;
end;

procedure TFhirGroupMemberList.SetItemN(index: Integer; value: TFhirGroupMember);
begin
  assert(value is TFhirGroupMember);
  ObjectByIndex[index] := value;
end;

{ TFhirGroup }

constructor TFhirGroup.Create;
begin
  inherited;
end;

destructor TFhirGroup.Destroy;
begin
  FIdentifierList.Free;
  FType_.free;
  FActual.free;
  FCode.free;
  FName.free;
  FQuantity.free;
  FCharacteristicList.Free;
  FMemberList.Free;
  inherited;
end;

function TFhirGroup.GetResourceType : TFhirResourceType;
begin
  result := frtGroup;
end;

procedure TFhirGroup.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirGroup(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirGroup(oSource).FIdentifierList);
  end;
  FType_ := TFhirGroup(oSource).FType_.Link;
  actualElement := TFhirGroup(oSource).actualElement.Clone;
  code := TFhirGroup(oSource).code.Clone;
  nameElement := TFhirGroup(oSource).nameElement.Clone;
  quantityElement := TFhirGroup(oSource).quantityElement.Clone;
  if (TFhirGroup(oSource).FCharacteristicList = nil) then
  begin
    FCharacteristicList.free;
    FCharacteristicList := nil;
  end
  else
  begin
    if FCharacteristicList = nil then
      FCharacteristicList := TFhirGroupCharacteristicList.Create;
    FCharacteristicList.Assign(TFhirGroup(oSource).FCharacteristicList);
  end;
  if (TFhirGroup(oSource).FMemberList = nil) then
  begin
    FMemberList.free;
    FMemberList := nil;
  end
  else
  begin
    if FMemberList = nil then
      FMemberList := TFhirGroupMemberList.Create;
    FMemberList.Assign(TFhirGroup(oSource).FMemberList);
  end;
end;

procedure TFhirGroup.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'actual') Then
     list.add(self.link, 'actual', FActual.Link);
  if (child_name = 'code') Then
     list.add(self.link, 'code', FCode.Link);
  if (child_name = 'name') Then
     list.add(self.link, 'name', FName.Link);
  if (child_name = 'quantity') Then
     list.add(self.link, 'quantity', FQuantity.Link);
  if (child_name = 'characteristic') Then
    list.addAll(self, 'characteristic', FCharacteristicList);
  if (child_name = 'member') Then
    list.addAll(self, 'member', FMemberList);
end;

procedure TFhirGroup.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'code', false, TFHIREnum, FType_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'actual', 'boolean', false, TFhirBoolean, FActual.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', false, TFhirCodeableConcept, FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', false, TFhirString, FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'quantity', 'unsignedInt', false, TFhirUnsignedInt, FQuantity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'characteristic', '', true, TFhirGroupCharacteristic, FCharacteristicList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'member', '', true, TFhirGroupMember, FMemberList.Link)){3};
end;

function TFhirGroup.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'type') then
  begin
    Type_Element := asEnum(SYSTEMS_TFhirGroupTypeEnum, CODES_TFhirGroupTypeEnum, propValue);
    result := propValue
  end
  else if (propName = 'actual') then
  begin
    ActualElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'code') then
  begin
    Code := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'name') then
  begin
    NameElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'quantity') then
  begin
    QuantityElement := asUnsignedInt(propValue){5a};
    result := propValue;
  end
  else if (propName = 'characteristic') then
  begin
    CharacteristicList.add(propValue as TFhirGroupCharacteristic){2a};
    result := propValue;
  end
  else if (propName = 'member') then
  begin
    MemberList.add(propValue as TFhirGroupMember){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirGroup.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'characteristic') then CharacteristicList.insertItem(index, propValue as TFhirGroupCharacteristic){2a}
  else if (propName = 'member') then MemberList.insertItem(index, propValue as TFhirGroupMember){2a}
  else inherited;
end;

function TFhirGroup.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'actual') then result := TFhirBoolean.create() {5b}
  else if (propName = 'code') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'name') then result := TFhirString.create() {5b}
  else if (propName = 'quantity') then result := TFhirUnsignedInt.create() {5b}
  else if (propName = 'characteristic') then result := CharacteristicList.new(){2}
  else if (propName = 'member') then result := MemberList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirGroup.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'type') then result := 'code'
  else if (propName = 'actual') then result := 'boolean'
  else if (propName = 'code') then result := 'CodeableConcept'
  else if (propName = 'name') then result := 'string'
  else if (propName = 'quantity') then result := 'unsignedInt'
  else if (propName = 'characteristic') then result := ''
  else if (propName = 'member') then result := ''
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirGroup.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'type') then Type_Element := nil
  else if (propName = 'actual') then ActualElement := nil
  else if (propName = 'code') then CodeElement := nil
  else if (propName = 'name') then NameElement := nil
  else if (propName = 'quantity') then QuantityElement := nil
  else if (propName = 'characteristic') then deletePropertyValue('characteristic', CharacteristicList, value) {2}
  else if (propName = 'member') then deletePropertyValue('member', MemberList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirGroup.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'type') then Type_Element := asEnum(SYSTEMS_TFhirGroupTypeEnum, CODES_TFhirGroupTypeEnum, new){4}
  else if (propName = 'actual') then ActualElement := asBoolean(new){5b}
  else if (propName = 'code') then CodeElement := new as TFhirCodeableConcept{4}
  else if (propName = 'name') then NameElement := asString(new){5b}
  else if (propName = 'quantity') then QuantityElement := asUnsignedInt(new){5b}
  else if (propName = 'characteristic') then replacePropertyValue('characteristic', CharacteristicList, existing, new) {2}
  else if (propName = 'member') then replacePropertyValue('member', MemberList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirGroup.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'characteristic') then CharacteristicList.move(source, destination){2a}
  else if (propName = 'member') then MemberList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirGroup.fhirType : string;
begin
  result := 'Group';
end;

function TFhirGroup.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FType_) and isEmptyProp(FActual) and isEmptyProp(FCode) and isEmptyProp(FName) and isEmptyProp(FQuantity) and isEmptyProp(FcharacteristicList) and isEmptyProp(FmemberList);
end;

function TFhirGroup.equals(other : TObject) : boolean;
var
  o : TFhirGroup;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirGroup)) then
    result := false
  else
  begin
    o := TFhirGroup(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(type_Element, o.type_Element, true) and
      compareDeep(actualElement, o.actualElement, true) and compareDeep(codeElement, o.codeElement, true) and
      compareDeep(nameElement, o.nameElement, true) and compareDeep(quantityElement, o.quantityElement, true) and
      compareDeep(characteristicList, o.characteristicList, true) and compareDeep(memberList, o.memberList, true);
  end;
end;

function TFhirGroup.Link : TFhirGroup;
begin
  result := TFhirGroup(inherited Link);
end;

function TFhirGroup.Clone : TFhirGroup;
begin
  result := TFhirGroup(inherited Clone);
end;

procedure TFhirGroup.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('type');
  fields.add('actual');
  fields.add('code');
  fields.add('name');
  fields.add('quantity');
  fields.add('characteristic');
  fields.add('member');
end;

{ TFhirGroup }

Function TFhirGroup.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirGroup.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirGroup.SetType_(value : TFhirEnum);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirGroup.GetType_ST : TFhirGroupTypeEnum;
begin
  if FType_ = nil then
    result := TFhirGroupTypeEnum(0)
  else
    result := TFhirGroupTypeEnum(StringArrayIndexOfSensitive(CODES_TFhirGroupTypeEnum, FType_.value));
end;

Procedure TFhirGroup.SetType_ST(value : TFhirGroupTypeEnum);
begin
  if ord(value) = 0 then
    Type_Element := nil
  else
    Type_Element := TFhirEnum.create(SYSTEMS_TFhirGroupTypeEnum[value], CODES_TFhirGroupTypeEnum[value]);
end;

Procedure TFhirGroup.SetActual(value : TFhirBoolean);
begin
  FActual.free;
  FActual := value;
end;

Function TFhirGroup.GetActualST : Boolean;
begin
  if FActual = nil then
    result := false
  else
    result := FActual.value;
end;

Procedure TFhirGroup.SetActualST(value : Boolean);
begin
  if FActual = nil then
    FActual := TFhirBoolean.create;
  FActual.value := value
end;

Procedure TFhirGroup.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

Procedure TFhirGroup.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirGroup.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := FName.value;
end;

Procedure TFhirGroup.SetNameST(value : String);
begin
  if value <> '' then
  begin
    if FName = nil then
      FName := TFhirString.create;
    FName.value := value
  end
  else if FName <> nil then
    FName.value := '';
end;

Procedure TFhirGroup.SetQuantity(value : TFhirUnsignedInt);
begin
  FQuantity.free;
  FQuantity := value;
end;

Function TFhirGroup.GetQuantityST : String;
begin
  if FQuantity = nil then
    result := ''
  else
    result := FQuantity.value;
end;

Procedure TFhirGroup.SetQuantityST(value : String);
begin
  if value <> '' then
  begin
    if FQuantity = nil then
      FQuantity := TFhirUnsignedInt.create;
    FQuantity.value := value
  end
  else if FQuantity <> nil then
    FQuantity.value := '';
end;

Function TFhirGroup.GetCharacteristicList : TFhirGroupCharacteristicList;
begin
  if FCharacteristicList = nil then
    FCharacteristicList := TFhirGroupCharacteristicList.Create;
  result := FCharacteristicList;
end;

Function TFhirGroup.GetHasCharacteristicList : boolean;
begin
  result := (FCharacteristicList <> nil) and (FCharacteristicList.count > 0);
end;

Function TFhirGroup.GetMemberList : TFhirGroupMemberList;
begin
  if FMemberList = nil then
    FMemberList := TFhirGroupMemberList.Create;
  result := FMemberList;
end;

Function TFhirGroup.GetHasMemberList : boolean;
begin
  result := (FMemberList <> nil) and (FMemberList.count > 0);
end;

function TFhirGroup.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FType_.sizeInBytes);
  inc(result, FActual.sizeInBytes);
  inc(result, FCode.sizeInBytes);
  inc(result, FName.sizeInBytes);
  inc(result, FQuantity.sizeInBytes);
  inc(result, FcharacteristicList.sizeInBytes);
  inc(result, FmemberList.sizeInBytes);
end;

{ TFhirGroupListEnumerator }

Constructor TFhirGroupListEnumerator.Create(list : TFhirGroupList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirGroupListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirGroupListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirGroupListEnumerator.GetCurrent : TFhirGroup;
begin
  Result := FList[FIndex];
end;

function TFhirGroupListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirGroupList }
procedure TFhirGroupList.AddItem(value: TFhirGroup);
begin
  assert(value.ClassName = 'TFhirGroup', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirGroup');
  add(value);
end;

function TFhirGroupList.Append: TFhirGroup;
begin
  result := TFhirGroup.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirGroupList.ClearItems;
begin
  Clear;
end;

function TFhirGroupList.GetEnumerator : TFhirGroupListEnumerator;
begin
  result := TFhirGroupListEnumerator.Create(self.link);
end;

function TFhirGroupList.Clone: TFhirGroupList;
begin
  result := TFhirGroupList(inherited Clone);
end;

function TFhirGroupList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirGroupList.GetItemN(index: Integer): TFhirGroup;
begin
  result := TFhirGroup(ObjectByIndex[index]);
end;

function TFhirGroupList.ItemClass: TFslObjectClass;
begin
  result := TFhirGroup;
end;
function TFhirGroupList.IndexOf(value: TFhirGroup): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirGroupList.Insert(index: Integer): TFhirGroup;
begin
  result := TFhirGroup.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirGroupList.InsertItem(index: Integer; value: TFhirGroup);
begin
  assert(value is TFhirGroup);
  Inherited Insert(index, value);
end;

function TFhirGroupList.Item(index: Integer): TFhirGroup;
begin
  result := TFhirGroup(ObjectByIndex[index]);
end;

function TFhirGroupList.Link: TFhirGroupList;
begin
  result := TFhirGroupList(inherited Link);
end;

procedure TFhirGroupList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirGroupList.SetItemByIndex(index: Integer; value: TFhirGroup);
begin
  assert(value is TFhirGroup);
  FhirGroups[index] := value;
end;

procedure TFhirGroupList.SetItemN(index: Integer; value: TFhirGroup);
begin
  assert(value is TFhirGroup);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_GROUP}

{$IFDEF FHIR_HEALTHCARESERVICE}

{ TFhirHealthcareServiceServiceType }

constructor TFhirHealthcareServiceServiceType.Create;
begin
  inherited;
end;

destructor TFhirHealthcareServiceServiceType.Destroy;
begin
  FType_.free;
  FSpecialtyList.Free;
  inherited;
end;

procedure TFhirHealthcareServiceServiceType.Assign(oSource : TFslObject);
begin
  inherited;
  type_ := TFhirHealthcareServiceServiceType(oSource).type_.Clone;
  if (TFhirHealthcareServiceServiceType(oSource).FSpecialtyList = nil) then
  begin
    FSpecialtyList.free;
    FSpecialtyList := nil;
  end
  else
  begin
    if FSpecialtyList = nil then
      FSpecialtyList := TFhirCodeableConceptList.Create;
    FSpecialtyList.Assign(TFhirHealthcareServiceServiceType(oSource).FSpecialtyList);
  end;
end;

procedure TFhirHealthcareServiceServiceType.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'specialty') Then
    list.addAll(self, 'specialty', FSpecialtyList);
end;

procedure TFhirHealthcareServiceServiceType.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', false, TFhirCodeableConcept, FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'specialty', 'CodeableConcept', true, TFhirCodeableConcept, FSpecialtyList.Link)){3};
end;

function TFhirHealthcareServiceServiceType.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'type') then
  begin
    Type_ := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'specialty') then
  begin
    SpecialtyList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirHealthcareServiceServiceType.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'specialty') then SpecialtyList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else inherited;
end;

function TFhirHealthcareServiceServiceType.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'type') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'specialty') then result := SpecialtyList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirHealthcareServiceServiceType.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'specialty') then result := 'CodeableConcept'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirHealthcareServiceServiceType.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'type') then Type_Element := nil
  else if (propName = 'specialty') then deletePropertyValue('specialty', SpecialtyList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirHealthcareServiceServiceType.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'type') then Type_Element := new as TFhirCodeableConcept{4}
  else if (propName = 'specialty') then replacePropertyValue('specialty', SpecialtyList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirHealthcareServiceServiceType.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'specialty') then SpecialtyList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirHealthcareServiceServiceType.fhirType : string;
begin
  result := 'serviceType';
end;

function TFhirHealthcareServiceServiceType.Link : TFhirHealthcareServiceServiceType;
begin
  result := TFhirHealthcareServiceServiceType(inherited Link);
end;

function TFhirHealthcareServiceServiceType.Clone : TFhirHealthcareServiceServiceType;
begin
  result := TFhirHealthcareServiceServiceType(inherited Clone);
end;

function TFhirHealthcareServiceServiceType.equals(other : TObject) : boolean;
var
  o : TFhirHealthcareServiceServiceType;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirHealthcareServiceServiceType)) then
    result := false
  else
  begin
    o := TFhirHealthcareServiceServiceType(other);
    result := compareDeep(type_Element, o.type_Element, true) and compareDeep(specialtyList, o.specialtyList, true);
  end;
end;

function TFhirHealthcareServiceServiceType.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FType_) and isEmptyProp(FspecialtyList);
end;

procedure TFhirHealthcareServiceServiceType.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('type');
  fields.add('specialty');
end;

{ TFhirHealthcareServiceServiceType }

Procedure TFhirHealthcareServiceServiceType.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirHealthcareServiceServiceType.GetSpecialtyList : TFhirCodeableConceptList;
begin
  if FSpecialtyList = nil then
    FSpecialtyList := TFhirCodeableConceptList.Create;
  result := FSpecialtyList;
end;

Function TFhirHealthcareServiceServiceType.GetHasSpecialtyList : boolean;
begin
  result := (FSpecialtyList <> nil) and (FSpecialtyList.count > 0);
end;

function TFhirHealthcareServiceServiceType.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FType_.sizeInBytes);
  inc(result, FspecialtyList.sizeInBytes);
end;

{ TFhirHealthcareServiceServiceTypeListEnumerator }

Constructor TFhirHealthcareServiceServiceTypeListEnumerator.Create(list : TFhirHealthcareServiceServiceTypeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirHealthcareServiceServiceTypeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirHealthcareServiceServiceTypeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirHealthcareServiceServiceTypeListEnumerator.GetCurrent : TFhirHealthcareServiceServiceType;
begin
  Result := FList[FIndex];
end;

function TFhirHealthcareServiceServiceTypeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirHealthcareServiceServiceTypeList }
procedure TFhirHealthcareServiceServiceTypeList.AddItem(value: TFhirHealthcareServiceServiceType);
begin
  assert(value.ClassName = 'TFhirHealthcareServiceServiceType', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirHealthcareServiceServiceType');
  add(value);
end;

function TFhirHealthcareServiceServiceTypeList.Append: TFhirHealthcareServiceServiceType;
begin
  result := TFhirHealthcareServiceServiceType.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHealthcareServiceServiceTypeList.ClearItems;
begin
  Clear;
end;

function TFhirHealthcareServiceServiceTypeList.GetEnumerator : TFhirHealthcareServiceServiceTypeListEnumerator;
begin
  result := TFhirHealthcareServiceServiceTypeListEnumerator.Create(self.link);
end;

function TFhirHealthcareServiceServiceTypeList.Clone: TFhirHealthcareServiceServiceTypeList;
begin
  result := TFhirHealthcareServiceServiceTypeList(inherited Clone);
end;

function TFhirHealthcareServiceServiceTypeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirHealthcareServiceServiceTypeList.GetItemN(index: Integer): TFhirHealthcareServiceServiceType;
begin
  result := TFhirHealthcareServiceServiceType(ObjectByIndex[index]);
end;

function TFhirHealthcareServiceServiceTypeList.ItemClass: TFslObjectClass;
begin
  result := TFhirHealthcareServiceServiceType;
end;
function TFhirHealthcareServiceServiceTypeList.IndexOf(value: TFhirHealthcareServiceServiceType): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirHealthcareServiceServiceTypeList.Insert(index: Integer): TFhirHealthcareServiceServiceType;
begin
  result := TFhirHealthcareServiceServiceType.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHealthcareServiceServiceTypeList.InsertItem(index: Integer; value: TFhirHealthcareServiceServiceType);
begin
  assert(value is TFhirHealthcareServiceServiceType);
  Inherited Insert(index, value);
end;

function TFhirHealthcareServiceServiceTypeList.Item(index: Integer): TFhirHealthcareServiceServiceType;
begin
  result := TFhirHealthcareServiceServiceType(ObjectByIndex[index]);
end;

function TFhirHealthcareServiceServiceTypeList.Link: TFhirHealthcareServiceServiceTypeList;
begin
  result := TFhirHealthcareServiceServiceTypeList(inherited Link);
end;

procedure TFhirHealthcareServiceServiceTypeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirHealthcareServiceServiceTypeList.SetItemByIndex(index: Integer; value: TFhirHealthcareServiceServiceType);
begin
  assert(value is TFhirHealthcareServiceServiceType);
  FhirHealthcareServiceServiceTypes[index] := value;
end;

procedure TFhirHealthcareServiceServiceTypeList.SetItemN(index: Integer; value: TFhirHealthcareServiceServiceType);
begin
  assert(value is TFhirHealthcareServiceServiceType);
  ObjectByIndex[index] := value;
end;

{ TFhirHealthcareServiceAvailableTime }

constructor TFhirHealthcareServiceAvailableTime.Create;
begin
  inherited;
end;

destructor TFhirHealthcareServiceAvailableTime.Destroy;
begin
  FDaysOfWeek.Free;
  FAllDay.free;
  FAvailableStartTime.free;
  FAvailableEndTime.free;
  inherited;
end;

procedure TFhirHealthcareServiceAvailableTime.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirHealthcareServiceAvailableTime(oSource).FDaysOfWeek = nil) then
  begin
    FDaysOfWeek.free;
    FDaysOfWeek := nil;
  end
  else
  begin
    FDaysOfWeek := TFHIREnumList.Create(SYSTEMS_TFhirDaysOfWeekEnum, CODES_TFhirDaysOfWeekEnum);
    FDaysOfWeek.Assign(TFhirHealthcareServiceAvailableTime(oSource).FDaysOfWeek);
  end;
  allDayElement := TFhirHealthcareServiceAvailableTime(oSource).allDayElement.Clone;
  availableStartTimeElement := TFhirHealthcareServiceAvailableTime(oSource).availableStartTimeElement.Clone;
  availableEndTimeElement := TFhirHealthcareServiceAvailableTime(oSource).availableEndTimeElement.Clone;
end;

procedure TFhirHealthcareServiceAvailableTime.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'daysOfWeek') Then
     list.addAll(self, 'daysOfWeek', FDaysOfWeek);
  if (child_name = 'allDay') Then
     list.add(self.link, 'allDay', FAllDay.Link);
  if (child_name = 'availableStartTime') Then
     list.add(self.link, 'availableStartTime', FAvailableStartTime.Link);
  if (child_name = 'availableEndTime') Then
     list.add(self.link, 'availableEndTime', FAvailableEndTime.Link);
end;

procedure TFhirHealthcareServiceAvailableTime.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'daysOfWeek', 'code', true, TFHIREnum, FDaysOfWeek.Link)){3};
  oList.add(TFHIRProperty.create(self, 'allDay', 'boolean', false, TFhirBoolean, FAllDay.Link));{2}
  oList.add(TFHIRProperty.create(self, 'availableStartTime', 'time', false, TFhirTime, FAvailableStartTime.Link));{2}
  oList.add(TFHIRProperty.create(self, 'availableEndTime', 'time', false, TFhirTime, FAvailableEndTime.Link));{2}
end;

function TFhirHealthcareServiceAvailableTime.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'daysOfWeek') then
  begin
    DaysOfWeekList.add(asEnum(SYSTEMS_TFhirDaysOfWeekEnum, CODES_TFhirDaysOfWeekEnum, propValue)); {1}
    result := propValue;
  end
  else if (propName = 'allDay') then
  begin
    AllDayElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'availableStartTime') then
  begin
    AvailableStartTimeElement := asTime(propValue){5a};
    result := propValue;
  end
  else if (propName = 'availableEndTime') then
  begin
    AvailableEndTimeElement := asTime(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirHealthcareServiceAvailableTime.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'daysOfWeek') then FDaysOfWeek.insertItem(index, asEnum(SYSTEMS_TFhirDaysOfWeekEnum, CODES_TFhirDaysOfWeekEnum, propValue)) {1}
  else inherited;
end;

function TFhirHealthcareServiceAvailableTime.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'allDay') then result := TFhirBoolean.create() {5b}
  else if (propName = 'availableStartTime') then result := TFhirTime.create() {5b}
  else if (propName = 'availableEndTime') then result := TFhirTime.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirHealthcareServiceAvailableTime.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'daysOfWeek') then result := 'code'
  else if (propName = 'allDay') then result := 'boolean'
  else if (propName = 'availableStartTime') then result := 'time'
  else if (propName = 'availableEndTime') then result := 'time'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirHealthcareServiceAvailableTime.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'allDay') then AllDayElement := nil
  else if (propName = 'availableStartTime') then AvailableStartTimeElement := nil
  else if (propName = 'availableEndTime') then AvailableEndTimeElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirHealthcareServiceAvailableTime.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'allDay') then AllDayElement := asBoolean(new){5b}
  else if (propName = 'availableStartTime') then AvailableStartTimeElement := asTime(new){5b}
  else if (propName = 'availableEndTime') then AvailableEndTimeElement := asTime(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirHealthcareServiceAvailableTime.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'daysOfWeek') then FDaysOfWeek.move(source, destination) {1}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirHealthcareServiceAvailableTime.fhirType : string;
begin
  result := 'availableTime';
end;

function TFhirHealthcareServiceAvailableTime.Link : TFhirHealthcareServiceAvailableTime;
begin
  result := TFhirHealthcareServiceAvailableTime(inherited Link);
end;

function TFhirHealthcareServiceAvailableTime.Clone : TFhirHealthcareServiceAvailableTime;
begin
  result := TFhirHealthcareServiceAvailableTime(inherited Clone);
end;

function TFhirHealthcareServiceAvailableTime.equals(other : TObject) : boolean;
var
  o : TFhirHealthcareServiceAvailableTime;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirHealthcareServiceAvailableTime)) then
    result := false
  else
  begin
    o := TFhirHealthcareServiceAvailableTime(other);
    result := compareDeep(daysOfWeekList, o.daysOfWeekList, true) and compareDeep(allDayElement, o.allDayElement, true) and
      compareDeep(availableStartTimeElement, o.availableStartTimeElement, true) and
      compareDeep(availableEndTimeElement, o.availableEndTimeElement, true);
  end;
end;

function TFhirHealthcareServiceAvailableTime.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FDaysOfWeek) and isEmptyProp(FAllDay) and isEmptyProp(FAvailableStartTime) and isEmptyProp(FAvailableEndTime);
end;

procedure TFhirHealthcareServiceAvailableTime.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('daysOfWeek');
  fields.add('allDay');
  fields.add('availableStartTime');
  fields.add('availableEndTime');
end;

{ TFhirHealthcareServiceAvailableTime }

Function TFhirHealthcareServiceAvailableTime.GetDaysOfWeek : TFhirEnumList;
begin
  if FDaysOfWeek = nil then
    FDaysOfWeek := TFHIREnumList.Create(SYSTEMS_TFhirDaysOfWeekEnum, CODES_TFhirDaysOfWeekEnum);
  result := FDaysOfWeek;
end;

Function TFhirHealthcareServiceAvailableTime.GetHasDaysOfWeek : boolean;
begin
  result := (FDaysOfWeek <> nil) and (FDaysOfWeek.count > 0);
end;

Function TFhirHealthcareServiceAvailableTime.GetDaysOfWeekST : TFhirDaysOfWeekEnumList;
  var i : integer;
begin
  result := [];
  if FdaysOfWeek <> nil then
    for i := 0 to FdaysOfWeek.count - 1 do
      result := result + [TFhirDaysOfWeekEnum(StringArrayIndexOfSensitive(CODES_TFhirDaysOfWeekEnum, FdaysOfWeek[i].value))];
end;

Procedure TFhirHealthcareServiceAvailableTime.SetDaysOfWeekST(value : TFhirDaysOfWeekEnumList);
var a : TFhirDaysOfWeekEnum;
begin
  if FdaysOfWeek = nil then
    FdaysOfWeek := TFhirEnumList.create(SYSTEMS_TFhirDaysOfWeekEnum, CODES_TFhirDaysOfWeekEnum);
  FdaysOfWeek.clear;
  for a := low(TFhirDaysOfWeekEnum) to high(TFhirDaysOfWeekEnum) do
    if a in value then
      begin
         if FdaysOfWeek = nil then
           FdaysOfWeek := TFhirEnumList.create(SYSTEMS_TFhirDaysOfWeekEnum, CODES_TFhirDaysOfWeekEnum);
         FdaysOfWeek.add(TFhirEnum.create(SYSTEMS_TFhirDaysOfWeekEnum[a], CODES_TFhirDaysOfWeekEnum[a]));
      end;
end;

Procedure TFhirHealthcareServiceAvailableTime.SetAllDay(value : TFhirBoolean);
begin
  FAllDay.free;
  FAllDay := value;
end;

Function TFhirHealthcareServiceAvailableTime.GetAllDayST : Boolean;
begin
  if FAllDay = nil then
    result := false
  else
    result := FAllDay.value;
end;

Procedure TFhirHealthcareServiceAvailableTime.SetAllDayST(value : Boolean);
begin
  if FAllDay = nil then
    FAllDay := TFhirBoolean.create;
  FAllDay.value := value
end;

Procedure TFhirHealthcareServiceAvailableTime.SetAvailableStartTime(value : TFhirTime);
begin
  FAvailableStartTime.free;
  FAvailableStartTime := value;
end;

Function TFhirHealthcareServiceAvailableTime.GetAvailableStartTimeST : String;
begin
  if FAvailableStartTime = nil then
    result := ''
  else
    result := FAvailableStartTime.value;
end;

Procedure TFhirHealthcareServiceAvailableTime.SetAvailableStartTimeST(value : String);
begin
  if value <> '' then
  begin
    if FAvailableStartTime = nil then
      FAvailableStartTime := TFhirTime.create;
    FAvailableStartTime.value := value
  end
  else if FAvailableStartTime <> nil then
    FAvailableStartTime.value := '';
end;

Procedure TFhirHealthcareServiceAvailableTime.SetAvailableEndTime(value : TFhirTime);
begin
  FAvailableEndTime.free;
  FAvailableEndTime := value;
end;

Function TFhirHealthcareServiceAvailableTime.GetAvailableEndTimeST : String;
begin
  if FAvailableEndTime = nil then
    result := ''
  else
    result := FAvailableEndTime.value;
end;

Procedure TFhirHealthcareServiceAvailableTime.SetAvailableEndTimeST(value : String);
begin
  if value <> '' then
  begin
    if FAvailableEndTime = nil then
      FAvailableEndTime := TFhirTime.create;
    FAvailableEndTime.value := value
  end
  else if FAvailableEndTime <> nil then
    FAvailableEndTime.value := '';
end;

function TFhirHealthcareServiceAvailableTime.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDaysOfWeek.sizeInBytes);
  inc(result, FAllDay.sizeInBytes);
  inc(result, FAvailableStartTime.sizeInBytes);
  inc(result, FAvailableEndTime.sizeInBytes);
end;

{ TFhirHealthcareServiceAvailableTimeListEnumerator }

Constructor TFhirHealthcareServiceAvailableTimeListEnumerator.Create(list : TFhirHealthcareServiceAvailableTimeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirHealthcareServiceAvailableTimeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirHealthcareServiceAvailableTimeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirHealthcareServiceAvailableTimeListEnumerator.GetCurrent : TFhirHealthcareServiceAvailableTime;
begin
  Result := FList[FIndex];
end;

function TFhirHealthcareServiceAvailableTimeListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirHealthcareServiceAvailableTimeList }
procedure TFhirHealthcareServiceAvailableTimeList.AddItem(value: TFhirHealthcareServiceAvailableTime);
begin
  assert(value.ClassName = 'TFhirHealthcareServiceAvailableTime', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirHealthcareServiceAvailableTime');
  add(value);
end;

function TFhirHealthcareServiceAvailableTimeList.Append: TFhirHealthcareServiceAvailableTime;
begin
  result := TFhirHealthcareServiceAvailableTime.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHealthcareServiceAvailableTimeList.ClearItems;
begin
  Clear;
end;

function TFhirHealthcareServiceAvailableTimeList.GetEnumerator : TFhirHealthcareServiceAvailableTimeListEnumerator;
begin
  result := TFhirHealthcareServiceAvailableTimeListEnumerator.Create(self.link);
end;

function TFhirHealthcareServiceAvailableTimeList.Clone: TFhirHealthcareServiceAvailableTimeList;
begin
  result := TFhirHealthcareServiceAvailableTimeList(inherited Clone);
end;

function TFhirHealthcareServiceAvailableTimeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirHealthcareServiceAvailableTimeList.GetItemN(index: Integer): TFhirHealthcareServiceAvailableTime;
begin
  result := TFhirHealthcareServiceAvailableTime(ObjectByIndex[index]);
end;

function TFhirHealthcareServiceAvailableTimeList.ItemClass: TFslObjectClass;
begin
  result := TFhirHealthcareServiceAvailableTime;
end;
function TFhirHealthcareServiceAvailableTimeList.IndexOf(value: TFhirHealthcareServiceAvailableTime): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirHealthcareServiceAvailableTimeList.Insert(index: Integer): TFhirHealthcareServiceAvailableTime;
begin
  result := TFhirHealthcareServiceAvailableTime.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHealthcareServiceAvailableTimeList.InsertItem(index: Integer; value: TFhirHealthcareServiceAvailableTime);
begin
  assert(value is TFhirHealthcareServiceAvailableTime);
  Inherited Insert(index, value);
end;

function TFhirHealthcareServiceAvailableTimeList.Item(index: Integer): TFhirHealthcareServiceAvailableTime;
begin
  result := TFhirHealthcareServiceAvailableTime(ObjectByIndex[index]);
end;

function TFhirHealthcareServiceAvailableTimeList.Link: TFhirHealthcareServiceAvailableTimeList;
begin
  result := TFhirHealthcareServiceAvailableTimeList(inherited Link);
end;

procedure TFhirHealthcareServiceAvailableTimeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirHealthcareServiceAvailableTimeList.SetItemByIndex(index: Integer; value: TFhirHealthcareServiceAvailableTime);
begin
  assert(value is TFhirHealthcareServiceAvailableTime);
  FhirHealthcareServiceAvailableTimes[index] := value;
end;

procedure TFhirHealthcareServiceAvailableTimeList.SetItemN(index: Integer; value: TFhirHealthcareServiceAvailableTime);
begin
  assert(value is TFhirHealthcareServiceAvailableTime);
  ObjectByIndex[index] := value;
end;

{ TFhirHealthcareServiceNotAvailable }

constructor TFhirHealthcareServiceNotAvailable.Create;
begin
  inherited;
end;

destructor TFhirHealthcareServiceNotAvailable.Destroy;
begin
  FDescription.free;
  FDuring.free;
  inherited;
end;

procedure TFhirHealthcareServiceNotAvailable.Assign(oSource : TFslObject);
begin
  inherited;
  descriptionElement := TFhirHealthcareServiceNotAvailable(oSource).descriptionElement.Clone;
  during := TFhirHealthcareServiceNotAvailable(oSource).during.Clone;
end;

procedure TFhirHealthcareServiceNotAvailable.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'description') Then
     list.add(self.link, 'description', FDescription.Link);
  if (child_name = 'during') Then
     list.add(self.link, 'during', FDuring.Link);
end;

procedure TFhirHealthcareServiceNotAvailable.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'description', 'string', false, TFhirString, FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'during', 'Period', false, TFhirPeriod, FDuring.Link));{2}
end;

function TFhirHealthcareServiceNotAvailable.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'description') then
  begin
    DescriptionElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'during') then
  begin
    During := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirHealthcareServiceNotAvailable.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirHealthcareServiceNotAvailable.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'description') then result := TFhirString.create() {5b}
  else if (propName = 'during') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirHealthcareServiceNotAvailable.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'description') then result := 'string'
  else if (propName = 'during') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirHealthcareServiceNotAvailable.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'description') then DescriptionElement := nil
  else if (propName = 'during') then DuringElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirHealthcareServiceNotAvailable.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'description') then DescriptionElement := asString(new){5b}
  else if (propName = 'during') then DuringElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirHealthcareServiceNotAvailable.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirHealthcareServiceNotAvailable.fhirType : string;
begin
  result := 'notAvailable';
end;

function TFhirHealthcareServiceNotAvailable.Link : TFhirHealthcareServiceNotAvailable;
begin
  result := TFhirHealthcareServiceNotAvailable(inherited Link);
end;

function TFhirHealthcareServiceNotAvailable.Clone : TFhirHealthcareServiceNotAvailable;
begin
  result := TFhirHealthcareServiceNotAvailable(inherited Clone);
end;

function TFhirHealthcareServiceNotAvailable.equals(other : TObject) : boolean;
var
  o : TFhirHealthcareServiceNotAvailable;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirHealthcareServiceNotAvailable)) then
    result := false
  else
  begin
    o := TFhirHealthcareServiceNotAvailable(other);
    result := compareDeep(descriptionElement, o.descriptionElement, true) and compareDeep(duringElement, o.duringElement, true);
  end;
end;

function TFhirHealthcareServiceNotAvailable.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FDescription) and isEmptyProp(FDuring);
end;

procedure TFhirHealthcareServiceNotAvailable.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('description');
  fields.add('during');
end;

{ TFhirHealthcareServiceNotAvailable }

Procedure TFhirHealthcareServiceNotAvailable.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirHealthcareServiceNotAvailable.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := FDescription.value;
end;

Procedure TFhirHealthcareServiceNotAvailable.SetDescriptionST(value : String);
begin
  if value <> '' then
  begin
    if FDescription = nil then
      FDescription := TFhirString.create;
    FDescription.value := value
  end
  else if FDescription <> nil then
    FDescription.value := '';
end;

Procedure TFhirHealthcareServiceNotAvailable.SetDuring(value : TFhirPeriod);
begin
  FDuring.free;
  FDuring := value;
end;

function TFhirHealthcareServiceNotAvailable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDescription.sizeInBytes);
  inc(result, FDuring.sizeInBytes);
end;

{ TFhirHealthcareServiceNotAvailableListEnumerator }

Constructor TFhirHealthcareServiceNotAvailableListEnumerator.Create(list : TFhirHealthcareServiceNotAvailableList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirHealthcareServiceNotAvailableListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirHealthcareServiceNotAvailableListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirHealthcareServiceNotAvailableListEnumerator.GetCurrent : TFhirHealthcareServiceNotAvailable;
begin
  Result := FList[FIndex];
end;

function TFhirHealthcareServiceNotAvailableListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirHealthcareServiceNotAvailableList }
procedure TFhirHealthcareServiceNotAvailableList.AddItem(value: TFhirHealthcareServiceNotAvailable);
begin
  assert(value.ClassName = 'TFhirHealthcareServiceNotAvailable', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirHealthcareServiceNotAvailable');
  add(value);
end;

function TFhirHealthcareServiceNotAvailableList.Append: TFhirHealthcareServiceNotAvailable;
begin
  result := TFhirHealthcareServiceNotAvailable.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHealthcareServiceNotAvailableList.ClearItems;
begin
  Clear;
end;

function TFhirHealthcareServiceNotAvailableList.GetEnumerator : TFhirHealthcareServiceNotAvailableListEnumerator;
begin
  result := TFhirHealthcareServiceNotAvailableListEnumerator.Create(self.link);
end;

function TFhirHealthcareServiceNotAvailableList.Clone: TFhirHealthcareServiceNotAvailableList;
begin
  result := TFhirHealthcareServiceNotAvailableList(inherited Clone);
end;

function TFhirHealthcareServiceNotAvailableList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirHealthcareServiceNotAvailableList.GetItemN(index: Integer): TFhirHealthcareServiceNotAvailable;
begin
  result := TFhirHealthcareServiceNotAvailable(ObjectByIndex[index]);
end;

function TFhirHealthcareServiceNotAvailableList.ItemClass: TFslObjectClass;
begin
  result := TFhirHealthcareServiceNotAvailable;
end;
function TFhirHealthcareServiceNotAvailableList.IndexOf(value: TFhirHealthcareServiceNotAvailable): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirHealthcareServiceNotAvailableList.Insert(index: Integer): TFhirHealthcareServiceNotAvailable;
begin
  result := TFhirHealthcareServiceNotAvailable.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHealthcareServiceNotAvailableList.InsertItem(index: Integer; value: TFhirHealthcareServiceNotAvailable);
begin
  assert(value is TFhirHealthcareServiceNotAvailable);
  Inherited Insert(index, value);
end;

function TFhirHealthcareServiceNotAvailableList.Item(index: Integer): TFhirHealthcareServiceNotAvailable;
begin
  result := TFhirHealthcareServiceNotAvailable(ObjectByIndex[index]);
end;

function TFhirHealthcareServiceNotAvailableList.Link: TFhirHealthcareServiceNotAvailableList;
begin
  result := TFhirHealthcareServiceNotAvailableList(inherited Link);
end;

procedure TFhirHealthcareServiceNotAvailableList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirHealthcareServiceNotAvailableList.SetItemByIndex(index: Integer; value: TFhirHealthcareServiceNotAvailable);
begin
  assert(value is TFhirHealthcareServiceNotAvailable);
  FhirHealthcareServiceNotAvailables[index] := value;
end;

procedure TFhirHealthcareServiceNotAvailableList.SetItemN(index: Integer; value: TFhirHealthcareServiceNotAvailable);
begin
  assert(value is TFhirHealthcareServiceNotAvailable);
  ObjectByIndex[index] := value;
end;

{ TFhirHealthcareService }

constructor TFhirHealthcareService.Create;
begin
  inherited;
end;

destructor TFhirHealthcareService.Destroy;
begin
  FIdentifierList.Free;
  FProvidedBy.free;
  FServiceCategory.free;
  FServiceTypeList.Free;
  FLocation.free;
  FServiceName.free;
  FComment.free;
  FExtraDetails.free;
  FPhoto.free;
  FTelecomList.Free;
  FCoverageAreaList.Free;
  FServiceProvisionCodeList.Free;
  FEligibility.free;
  FEligibilityNote.free;
  FProgramNameList.Free;
  FCharacteristicList.Free;
  FReferralMethodList.Free;
  FPublicKey.free;
  FAppointmentRequired.free;
  FAvailableTimeList.Free;
  FNotAvailableList.Free;
  FAvailabilityExceptions.free;
  inherited;
end;

function TFhirHealthcareService.GetResourceType : TFhirResourceType;
begin
  result := frtHealthcareService;
end;

procedure TFhirHealthcareService.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirHealthcareService(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirHealthcareService(oSource).FIdentifierList);
  end;
  providedBy := TFhirHealthcareService(oSource).providedBy.Clone;
  serviceCategory := TFhirHealthcareService(oSource).serviceCategory.Clone;
  if (TFhirHealthcareService(oSource).FServiceTypeList = nil) then
  begin
    FServiceTypeList.free;
    FServiceTypeList := nil;
  end
  else
  begin
    if FServiceTypeList = nil then
      FServiceTypeList := TFhirHealthcareServiceServiceTypeList.Create;
    FServiceTypeList.Assign(TFhirHealthcareService(oSource).FServiceTypeList);
  end;
  location := TFhirHealthcareService(oSource).location.Clone;
  serviceNameElement := TFhirHealthcareService(oSource).serviceNameElement.Clone;
  commentElement := TFhirHealthcareService(oSource).commentElement.Clone;
  extraDetailsElement := TFhirHealthcareService(oSource).extraDetailsElement.Clone;
  photo := TFhirHealthcareService(oSource).photo.Clone;
  if (TFhirHealthcareService(oSource).FTelecomList = nil) then
  begin
    FTelecomList.free;
    FTelecomList := nil;
  end
  else
  begin
    if FTelecomList = nil then
      FTelecomList := TFhirContactPointList.Create;
    FTelecomList.Assign(TFhirHealthcareService(oSource).FTelecomList);
  end;
  if (TFhirHealthcareService(oSource).FCoverageAreaList = nil) then
  begin
    FCoverageAreaList.free;
    FCoverageAreaList := nil;
  end
  else
  begin
    if FCoverageAreaList = nil then
      FCoverageAreaList := TFhirReferenceList{TFhirLocation}.Create;
    FCoverageAreaList.Assign(TFhirHealthcareService(oSource).FCoverageAreaList);
  end;
  if (TFhirHealthcareService(oSource).FServiceProvisionCodeList = nil) then
  begin
    FServiceProvisionCodeList.free;
    FServiceProvisionCodeList := nil;
  end
  else
  begin
    if FServiceProvisionCodeList = nil then
      FServiceProvisionCodeList := TFhirCodeableConceptList.Create;
    FServiceProvisionCodeList.Assign(TFhirHealthcareService(oSource).FServiceProvisionCodeList);
  end;
  eligibility := TFhirHealthcareService(oSource).eligibility.Clone;
  eligibilityNoteElement := TFhirHealthcareService(oSource).eligibilityNoteElement.Clone;
  if (TFhirHealthcareService(oSource).FProgramNameList = nil) then
  begin
    FProgramNameList.free;
    FProgramNameList := nil;
  end
  else
  begin
    if FProgramNameList = nil then
      FProgramNameList := TFhirStringList.Create;
    FProgramNameList.Assign(TFhirHealthcareService(oSource).FProgramNameList);
  end;
  if (TFhirHealthcareService(oSource).FCharacteristicList = nil) then
  begin
    FCharacteristicList.free;
    FCharacteristicList := nil;
  end
  else
  begin
    if FCharacteristicList = nil then
      FCharacteristicList := TFhirCodeableConceptList.Create;
    FCharacteristicList.Assign(TFhirHealthcareService(oSource).FCharacteristicList);
  end;
  if (TFhirHealthcareService(oSource).FReferralMethodList = nil) then
  begin
    FReferralMethodList.free;
    FReferralMethodList := nil;
  end
  else
  begin
    if FReferralMethodList = nil then
      FReferralMethodList := TFhirCodeableConceptList.Create;
    FReferralMethodList.Assign(TFhirHealthcareService(oSource).FReferralMethodList);
  end;
  publicKeyElement := TFhirHealthcareService(oSource).publicKeyElement.Clone;
  appointmentRequiredElement := TFhirHealthcareService(oSource).appointmentRequiredElement.Clone;
  if (TFhirHealthcareService(oSource).FAvailableTimeList = nil) then
  begin
    FAvailableTimeList.free;
    FAvailableTimeList := nil;
  end
  else
  begin
    if FAvailableTimeList = nil then
      FAvailableTimeList := TFhirHealthcareServiceAvailableTimeList.Create;
    FAvailableTimeList.Assign(TFhirHealthcareService(oSource).FAvailableTimeList);
  end;
  if (TFhirHealthcareService(oSource).FNotAvailableList = nil) then
  begin
    FNotAvailableList.free;
    FNotAvailableList := nil;
  end
  else
  begin
    if FNotAvailableList = nil then
      FNotAvailableList := TFhirHealthcareServiceNotAvailableList.Create;
    FNotAvailableList.Assign(TFhirHealthcareService(oSource).FNotAvailableList);
  end;
  availabilityExceptionsElement := TFhirHealthcareService(oSource).availabilityExceptionsElement.Clone;
end;

procedure TFhirHealthcareService.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'providedBy') Then
     list.add(self.link, 'providedBy', FProvidedBy.Link);
  if (child_name = 'serviceCategory') Then
     list.add(self.link, 'serviceCategory', FServiceCategory.Link);
  if (child_name = 'serviceType') Then
    list.addAll(self, 'serviceType', FServiceTypeList);
  if (child_name = 'location') Then
     list.add(self.link, 'location', FLocation.Link);
  if (child_name = 'serviceName') Then
     list.add(self.link, 'serviceName', FServiceName.Link);
  if (child_name = 'comment') Then
     list.add(self.link, 'comment', FComment.Link);
  if (child_name = 'extraDetails') Then
     list.add(self.link, 'extraDetails', FExtraDetails.Link);
  if (child_name = 'photo') Then
     list.add(self.link, 'photo', FPhoto.Link);
  if (child_name = 'telecom') Then
    list.addAll(self, 'telecom', FTelecomList);
  if (child_name = 'coverageArea') Then
    list.addAll(self, 'coverageArea', FCoverageAreaList);
  if (child_name = 'serviceProvisionCode') Then
    list.addAll(self, 'serviceProvisionCode', FServiceProvisionCodeList);
  if (child_name = 'eligibility') Then
     list.add(self.link, 'eligibility', FEligibility.Link);
  if (child_name = 'eligibilityNote') Then
     list.add(self.link, 'eligibilityNote', FEligibilityNote.Link);
  if (child_name = 'programName') Then
    list.addAll(self, 'programName', FProgramNameList);
  if (child_name = 'characteristic') Then
    list.addAll(self, 'characteristic', FCharacteristicList);
  if (child_name = 'referralMethod') Then
    list.addAll(self, 'referralMethod', FReferralMethodList);
  if (child_name = 'publicKey') Then
     list.add(self.link, 'publicKey', FPublicKey.Link);
  if (child_name = 'appointmentRequired') Then
     list.add(self.link, 'appointmentRequired', FAppointmentRequired.Link);
  if (child_name = 'availableTime') Then
    list.addAll(self, 'availableTime', FAvailableTimeList);
  if (child_name = 'notAvailable') Then
    list.addAll(self, 'notAvailable', FNotAvailableList);
  if (child_name = 'availabilityExceptions') Then
     list.add(self.link, 'availabilityExceptions', FAvailabilityExceptions.Link);
end;

procedure TFhirHealthcareService.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'providedBy', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FProvidedBy.Link));{2}
  oList.add(TFHIRProperty.create(self, 'serviceCategory', 'CodeableConcept', false, TFhirCodeableConcept, FServiceCategory.Link));{2}
  oList.add(TFHIRProperty.create(self, 'serviceType', '', true, TFhirHealthcareServiceServiceType, FServiceTypeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'location', 'Reference(Location)', false, TFhirReference{TFhirLocation}, FLocation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'serviceName', 'string', false, TFhirString, FServiceName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comment', 'string', false, TFhirString, FComment.Link));{2}
  oList.add(TFHIRProperty.create(self, 'extraDetails', 'string', false, TFhirString, FExtraDetails.Link));{2}
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', false, TFhirAttachment, FPhoto.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'ContactPoint', true, TFhirContactPoint, FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'coverageArea', 'Reference(Location)', true, TFhirReference{TFhirLocation}, FCoverageAreaList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'serviceProvisionCode', 'CodeableConcept', true, TFhirCodeableConcept, FServiceProvisionCodeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'eligibility', 'CodeableConcept', false, TFhirCodeableConcept, FEligibility.Link));{2}
  oList.add(TFHIRProperty.create(self, 'eligibilityNote', 'string', false, TFhirString, FEligibilityNote.Link));{2}
  oList.add(TFHIRProperty.create(self, 'programName', 'string', true, TFhirString, FProgramNameList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'characteristic', 'CodeableConcept', true, TFhirCodeableConcept, FCharacteristicList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'referralMethod', 'CodeableConcept', true, TFhirCodeableConcept, FReferralMethodList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'publicKey', 'string', false, TFhirString, FPublicKey.Link));{2}
  oList.add(TFHIRProperty.create(self, 'appointmentRequired', 'boolean', false, TFhirBoolean, FAppointmentRequired.Link));{2}
  oList.add(TFHIRProperty.create(self, 'availableTime', '', true, TFhirHealthcareServiceAvailableTime, FAvailableTimeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'notAvailable', '', true, TFhirHealthcareServiceNotAvailable, FNotAvailableList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'availabilityExceptions', 'string', false, TFhirString, FAvailabilityExceptions.Link));{2}
end;

function TFhirHealthcareService.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'providedBy') then
  begin
    ProvidedBy := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'serviceCategory') then
  begin
    ServiceCategory := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'serviceType') then
  begin
    ServiceTypeList.add(propValue as TFhirHealthcareServiceServiceType){2a};
    result := propValue;
  end
  else if (propName = 'location') then
  begin
    Location := propValue as TFhirReference{TFhirLocation}{4b};
    result := propValue;
  end
  else if (propName = 'serviceName') then
  begin
    ServiceNameElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'comment') then
  begin
    CommentElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'extraDetails') then
  begin
    ExtraDetailsElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'photo') then
  begin
    Photo := propValue as TFhirAttachment{4b};
    result := propValue;
  end
  else if (propName = 'telecom') then
  begin
    TelecomList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'coverageArea') then
  begin
    CoverageAreaList.add(propValue as TFhirReference{TFhirLocation}){2a};
    result := propValue;
  end
  else if (propName = 'serviceProvisionCode') then
  begin
    ServiceProvisionCodeList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'eligibility') then
  begin
    Eligibility := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'eligibilityNote') then
  begin
    EligibilityNoteElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'programName') then
  begin
    ProgramNameList.add(asString(propValue)){2};     result := propValue;

  end
  else if (propName = 'characteristic') then
  begin
    CharacteristicList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'referralMethod') then
  begin
    ReferralMethodList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'publicKey') then
  begin
    PublicKeyElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'appointmentRequired') then
  begin
    AppointmentRequiredElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'availableTime') then
  begin
    AvailableTimeList.add(propValue as TFhirHealthcareServiceAvailableTime){2a};
    result := propValue;
  end
  else if (propName = 'notAvailable') then
  begin
    NotAvailableList.add(propValue as TFhirHealthcareServiceNotAvailable){2a};
    result := propValue;
  end
  else if (propName = 'availabilityExceptions') then
  begin
    AvailabilityExceptionsElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirHealthcareService.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'serviceType') then ServiceTypeList.insertItem(index, propValue as TFhirHealthcareServiceServiceType){2a}
  else if (propName = 'telecom') then TelecomList.insertItem(index, propValue as TFhirContactPoint){2a}
  else if (propName = 'coverageArea') then CoverageAreaList.insertItem(index, propValue as TFhirReference{TFhirLocation}){2a}
  else if (propName = 'serviceProvisionCode') then ServiceProvisionCodeList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'programName') then ProgramNameList.insertItem(index, asString(propValue)){2}
  else if (propName = 'characteristic') then CharacteristicList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'referralMethod') then ReferralMethodList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'availableTime') then AvailableTimeList.insertItem(index, propValue as TFhirHealthcareServiceAvailableTime){2a}
  else if (propName = 'notAvailable') then NotAvailableList.insertItem(index, propValue as TFhirHealthcareServiceNotAvailable){2a}
  else inherited;
end;

function TFhirHealthcareService.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'providedBy') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'serviceCategory') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'serviceType') then result := ServiceTypeList.new(){2}
  else if (propName = 'location') then result := TFhirReference{TFhirLocation}.create(){4b}
  else if (propName = 'serviceName') then result := TFhirString.create() {5b}
  else if (propName = 'comment') then result := TFhirString.create() {5b}
  else if (propName = 'extraDetails') then result := TFhirString.create() {5b}
  else if (propName = 'photo') then result := TFhirAttachment.create(){4b}
  else if (propName = 'telecom') then result := TelecomList.new(){2}
  else if (propName = 'coverageArea') then result := CoverageAreaList.new(){2}
  else if (propName = 'serviceProvisionCode') then result := ServiceProvisionCodeList.new(){2}
  else if (propName = 'eligibility') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'eligibilityNote') then result := TFhirString.create() {5b}
  else if (propName = 'programName') then result := ProgramNameList.new(){2}
  else if (propName = 'characteristic') then result := CharacteristicList.new(){2}
  else if (propName = 'referralMethod') then result := ReferralMethodList.new(){2}
  else if (propName = 'publicKey') then result := TFhirString.create() {5b}
  else if (propName = 'appointmentRequired') then result := TFhirBoolean.create() {5b}
  else if (propName = 'availableTime') then result := AvailableTimeList.new(){2}
  else if (propName = 'notAvailable') then result := NotAvailableList.new(){2}
  else if (propName = 'availabilityExceptions') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirHealthcareService.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'providedBy') then result := 'Reference'
  else if (propName = 'serviceCategory') then result := 'CodeableConcept'
  else if (propName = 'serviceType') then result := ''
  else if (propName = 'location') then result := 'Reference'
  else if (propName = 'serviceName') then result := 'string'
  else if (propName = 'comment') then result := 'string'
  else if (propName = 'extraDetails') then result := 'string'
  else if (propName = 'photo') then result := 'Attachment'
  else if (propName = 'telecom') then result := 'ContactPoint'
  else if (propName = 'coverageArea') then result := 'Reference'
  else if (propName = 'serviceProvisionCode') then result := 'CodeableConcept'
  else if (propName = 'eligibility') then result := 'CodeableConcept'
  else if (propName = 'eligibilityNote') then result := 'string'
  else if (propName = 'programName') then result := 'string'
  else if (propName = 'characteristic') then result := 'CodeableConcept'
  else if (propName = 'referralMethod') then result := 'CodeableConcept'
  else if (propName = 'publicKey') then result := 'string'
  else if (propName = 'appointmentRequired') then result := 'boolean'
  else if (propName = 'availableTime') then result := ''
  else if (propName = 'notAvailable') then result := ''
  else if (propName = 'availabilityExceptions') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirHealthcareService.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'providedBy') then ProvidedByElement := nil
  else if (propName = 'serviceCategory') then ServiceCategoryElement := nil
  else if (propName = 'serviceType') then deletePropertyValue('serviceType', ServiceTypeList, value) {2}
  else if (propName = 'location') then LocationElement := nil
  else if (propName = 'serviceName') then ServiceNameElement := nil
  else if (propName = 'comment') then CommentElement := nil
  else if (propName = 'extraDetails') then ExtraDetailsElement := nil
  else if (propName = 'photo') then PhotoElement := nil
  else if (propName = 'telecom') then deletePropertyValue('telecom', TelecomList, value) {2}
  else if (propName = 'coverageArea') then deletePropertyValue('coverageArea', CoverageAreaList, value) {2}
  else if (propName = 'serviceProvisionCode') then deletePropertyValue('serviceProvisionCode', ServiceProvisionCodeList, value) {2}
  else if (propName = 'eligibility') then EligibilityElement := nil
  else if (propName = 'eligibilityNote') then EligibilityNoteElement := nil
  else if (propName = 'programName') then deletePropertyValue('programName', ProgramNameList, value) {2}
  else if (propName = 'characteristic') then deletePropertyValue('characteristic', CharacteristicList, value) {2}
  else if (propName = 'referralMethod') then deletePropertyValue('referralMethod', ReferralMethodList, value) {2}
  else if (propName = 'publicKey') then PublicKeyElement := nil
  else if (propName = 'appointmentRequired') then AppointmentRequiredElement := nil
  else if (propName = 'availableTime') then deletePropertyValue('availableTime', AvailableTimeList, value) {2}
  else if (propName = 'notAvailable') then deletePropertyValue('notAvailable', NotAvailableList, value) {2}
  else if (propName = 'availabilityExceptions') then AvailabilityExceptionsElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirHealthcareService.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'providedBy') then ProvidedByElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'serviceCategory') then ServiceCategoryElement := new as TFhirCodeableConcept{4}
  else if (propName = 'serviceType') then replacePropertyValue('serviceType', ServiceTypeList, existing, new) {2}
  else if (propName = 'location') then LocationElement := new as TFhirReference{TFhirLocation}{4}
  else if (propName = 'serviceName') then ServiceNameElement := asString(new){5b}
  else if (propName = 'comment') then CommentElement := asString(new){5b}
  else if (propName = 'extraDetails') then ExtraDetailsElement := asString(new){5b}
  else if (propName = 'photo') then PhotoElement := new as TFhirAttachment{4}
  else if (propName = 'telecom') then replacePropertyValue('telecom', TelecomList, existing, new) {2}
  else if (propName = 'coverageArea') then replacePropertyValue('coverageArea', CoverageAreaList, existing, new) {2}
  else if (propName = 'serviceProvisionCode') then replacePropertyValue('serviceProvisionCode', ServiceProvisionCodeList, existing, new) {2}
  else if (propName = 'eligibility') then EligibilityElement := new as TFhirCodeableConcept{4}
  else if (propName = 'eligibilityNote') then EligibilityNoteElement := asString(new){5b}
  else if (propName = 'programName') then replacePropertyValue('programName', ProgramNameList, existing, new) {2}
  else if (propName = 'characteristic') then replacePropertyValue('characteristic', CharacteristicList, existing, new) {2}
  else if (propName = 'referralMethod') then replacePropertyValue('referralMethod', ReferralMethodList, existing, new) {2}
  else if (propName = 'publicKey') then PublicKeyElement := asString(new){5b}
  else if (propName = 'appointmentRequired') then AppointmentRequiredElement := asBoolean(new){5b}
  else if (propName = 'availableTime') then replacePropertyValue('availableTime', AvailableTimeList, existing, new) {2}
  else if (propName = 'notAvailable') then replacePropertyValue('notAvailable', NotAvailableList, existing, new) {2}
  else if (propName = 'availabilityExceptions') then AvailabilityExceptionsElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirHealthcareService.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'serviceType') then ServiceTypeList.move(source, destination){2a}
  else if (propName = 'telecom') then TelecomList.move(source, destination){2a}
  else if (propName = 'coverageArea') then CoverageAreaList.move(source, destination){2a}
  else if (propName = 'serviceProvisionCode') then ServiceProvisionCodeList.move(source, destination){2a}
  else if (propName = 'programName') then ProgramNameList.move(source, destination){2}
  else if (propName = 'characteristic') then CharacteristicList.move(source, destination){2a}
  else if (propName = 'referralMethod') then ReferralMethodList.move(source, destination){2a}
  else if (propName = 'availableTime') then AvailableTimeList.move(source, destination){2a}
  else if (propName = 'notAvailable') then NotAvailableList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirHealthcareService.fhirType : string;
begin
  result := 'HealthcareService';
end;

function TFhirHealthcareService.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FProvidedBy) and isEmptyProp(FServiceCategory) and isEmptyProp(FserviceTypeList) and isEmptyProp(FLocation) and isEmptyProp(FServiceName) and isEmptyProp(FComment) and isEmptyProp(FExtraDetails) and isEmptyProp(FPhoto) and isEmptyProp(FtelecomList) and isEmptyProp(FcoverageAreaList) and isEmptyProp(FserviceProvisionCodeList) and isEmptyProp(FEligibility) and isEmptyProp(FEligibilityNote) and isEmptyProp(FprogramNameList) and isEmptyProp(FcharacteristicList) and isEmptyProp(FreferralMethodList) and isEmptyProp(FPublicKey) and isEmptyProp(FAppointmentRequired) and isEmptyProp(FavailableTimeList) and isEmptyProp(FnotAvailableList) and isEmptyProp(FAvailabilityExceptions);
end;

function TFhirHealthcareService.equals(other : TObject) : boolean;
var
  o : TFhirHealthcareService;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirHealthcareService)) then
    result := false
  else
  begin
    o := TFhirHealthcareService(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(providedByElement, o.providedByElement, true) and
      compareDeep(serviceCategoryElement, o.serviceCategoryElement, true) and compareDeep(serviceTypeList, o.serviceTypeList, true) and
      compareDeep(locationElement, o.locationElement, true) and compareDeep(serviceNameElement, o.serviceNameElement, true) and
      compareDeep(commentElement, o.commentElement, true) and compareDeep(extraDetailsElement, o.extraDetailsElement, true) and
      compareDeep(photoElement, o.photoElement, true) and compareDeep(telecomList, o.telecomList, true) and
      compareDeep(coverageAreaList, o.coverageAreaList, true) and compareDeep(serviceProvisionCodeList, o.serviceProvisionCodeList, true) and
      compareDeep(eligibilityElement, o.eligibilityElement, true) and compareDeep(eligibilityNoteElement, o.eligibilityNoteElement, true) and
      compareDeep(programNameList, o.programNameList, true) and compareDeep(characteristicList, o.characteristicList, true) and
      compareDeep(referralMethodList, o.referralMethodList, true) and compareDeep(publicKeyElement, o.publicKeyElement, true) and
      compareDeep(appointmentRequiredElement, o.appointmentRequiredElement, true) and
      compareDeep(availableTimeList, o.availableTimeList, true) and compareDeep(notAvailableList, o.notAvailableList, true) and
      compareDeep(availabilityExceptionsElement, o.availabilityExceptionsElement, true);
  end;
end;

function TFhirHealthcareService.Link : TFhirHealthcareService;
begin
  result := TFhirHealthcareService(inherited Link);
end;

function TFhirHealthcareService.Clone : TFhirHealthcareService;
begin
  result := TFhirHealthcareService(inherited Clone);
end;

procedure TFhirHealthcareService.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('providedBy');
  fields.add('serviceCategory');
  fields.add('serviceType');
  fields.add('location');
  fields.add('serviceName');
  fields.add('comment');
  fields.add('extraDetails');
  fields.add('photo');
  fields.add('telecom');
  fields.add('coverageArea');
  fields.add('serviceProvisionCode');
  fields.add('eligibility');
  fields.add('eligibilityNote');
  fields.add('programName');
  fields.add('characteristic');
  fields.add('referralMethod');
  fields.add('publicKey');
  fields.add('appointmentRequired');
  fields.add('availableTime');
  fields.add('notAvailable');
  fields.add('availabilityExceptions');
end;

{ TFhirHealthcareService }

Function TFhirHealthcareService.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirHealthcareService.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirHealthcareService.SetProvidedBy(value : TFhirReference{TFhirOrganization});
begin
  FProvidedBy.free;
  FProvidedBy := value;
end;

Procedure TFhirHealthcareService.SetServiceCategory(value : TFhirCodeableConcept);
begin
  FServiceCategory.free;
  FServiceCategory := value;
end;

Function TFhirHealthcareService.GetServiceTypeList : TFhirHealthcareServiceServiceTypeList;
begin
  if FServiceTypeList = nil then
    FServiceTypeList := TFhirHealthcareServiceServiceTypeList.Create;
  result := FServiceTypeList;
end;

Function TFhirHealthcareService.GetHasServiceTypeList : boolean;
begin
  result := (FServiceTypeList <> nil) and (FServiceTypeList.count > 0);
end;

Procedure TFhirHealthcareService.SetLocation(value : TFhirReference{TFhirLocation});
begin
  FLocation.free;
  FLocation := value;
end;

Procedure TFhirHealthcareService.SetServiceName(value : TFhirString);
begin
  FServiceName.free;
  FServiceName := value;
end;

Function TFhirHealthcareService.GetServiceNameST : String;
begin
  if FServiceName = nil then
    result := ''
  else
    result := FServiceName.value;
end;

Procedure TFhirHealthcareService.SetServiceNameST(value : String);
begin
  if value <> '' then
  begin
    if FServiceName = nil then
      FServiceName := TFhirString.create;
    FServiceName.value := value
  end
  else if FServiceName <> nil then
    FServiceName.value := '';
end;

Procedure TFhirHealthcareService.SetComment(value : TFhirString);
begin
  FComment.free;
  FComment := value;
end;

Function TFhirHealthcareService.GetCommentST : String;
begin
  if FComment = nil then
    result := ''
  else
    result := FComment.value;
end;

Procedure TFhirHealthcareService.SetCommentST(value : String);
begin
  if value <> '' then
  begin
    if FComment = nil then
      FComment := TFhirString.create;
    FComment.value := value
  end
  else if FComment <> nil then
    FComment.value := '';
end;

Procedure TFhirHealthcareService.SetExtraDetails(value : TFhirString);
begin
  FExtraDetails.free;
  FExtraDetails := value;
end;

Function TFhirHealthcareService.GetExtraDetailsST : String;
begin
  if FExtraDetails = nil then
    result := ''
  else
    result := FExtraDetails.value;
end;

Procedure TFhirHealthcareService.SetExtraDetailsST(value : String);
begin
  if value <> '' then
  begin
    if FExtraDetails = nil then
      FExtraDetails := TFhirString.create;
    FExtraDetails.value := value
  end
  else if FExtraDetails <> nil then
    FExtraDetails.value := '';
end;

Procedure TFhirHealthcareService.SetPhoto(value : TFhirAttachment);
begin
  FPhoto.free;
  FPhoto := value;
end;

Function TFhirHealthcareService.GetTelecomList : TFhirContactPointList;
begin
  if FTelecomList = nil then
    FTelecomList := TFhirContactPointList.Create;
  result := FTelecomList;
end;

Function TFhirHealthcareService.GetHasTelecomList : boolean;
begin
  result := (FTelecomList <> nil) and (FTelecomList.count > 0);
end;

Function TFhirHealthcareService.GetCoverageAreaList : TFhirReferenceList{TFhirLocation};
begin
  if FCoverageAreaList = nil then
    FCoverageAreaList := TFhirReferenceList{TFhirLocation}.Create;
  result := FCoverageAreaList;
end;

Function TFhirHealthcareService.GetHasCoverageAreaList : boolean;
begin
  result := (FCoverageAreaList <> nil) and (FCoverageAreaList.count > 0);
end;

Function TFhirHealthcareService.GetServiceProvisionCodeList : TFhirCodeableConceptList;
begin
  if FServiceProvisionCodeList = nil then
    FServiceProvisionCodeList := TFhirCodeableConceptList.Create;
  result := FServiceProvisionCodeList;
end;

Function TFhirHealthcareService.GetHasServiceProvisionCodeList : boolean;
begin
  result := (FServiceProvisionCodeList <> nil) and (FServiceProvisionCodeList.count > 0);
end;

Procedure TFhirHealthcareService.SetEligibility(value : TFhirCodeableConcept);
begin
  FEligibility.free;
  FEligibility := value;
end;

Procedure TFhirHealthcareService.SetEligibilityNote(value : TFhirString);
begin
  FEligibilityNote.free;
  FEligibilityNote := value;
end;

Function TFhirHealthcareService.GetEligibilityNoteST : String;
begin
  if FEligibilityNote = nil then
    result := ''
  else
    result := FEligibilityNote.value;
end;

Procedure TFhirHealthcareService.SetEligibilityNoteST(value : String);
begin
  if value <> '' then
  begin
    if FEligibilityNote = nil then
      FEligibilityNote := TFhirString.create;
    FEligibilityNote.value := value
  end
  else if FEligibilityNote <> nil then
    FEligibilityNote.value := '';
end;

Function TFhirHealthcareService.GetProgramNameList : TFhirStringList;
begin
  if FProgramNameList = nil then
    FProgramNameList := TFhirStringList.Create;
  result := FProgramNameList;
end;

Function TFhirHealthcareService.GetHasProgramNameList : boolean;
begin
  result := (FProgramNameList <> nil) and (FProgramNameList.count > 0);
end;

Function TFhirHealthcareService.GetCharacteristicList : TFhirCodeableConceptList;
begin
  if FCharacteristicList = nil then
    FCharacteristicList := TFhirCodeableConceptList.Create;
  result := FCharacteristicList;
end;

Function TFhirHealthcareService.GetHasCharacteristicList : boolean;
begin
  result := (FCharacteristicList <> nil) and (FCharacteristicList.count > 0);
end;

Function TFhirHealthcareService.GetReferralMethodList : TFhirCodeableConceptList;
begin
  if FReferralMethodList = nil then
    FReferralMethodList := TFhirCodeableConceptList.Create;
  result := FReferralMethodList;
end;

Function TFhirHealthcareService.GetHasReferralMethodList : boolean;
begin
  result := (FReferralMethodList <> nil) and (FReferralMethodList.count > 0);
end;

Procedure TFhirHealthcareService.SetPublicKey(value : TFhirString);
begin
  FPublicKey.free;
  FPublicKey := value;
end;

Function TFhirHealthcareService.GetPublicKeyST : String;
begin
  if FPublicKey = nil then
    result := ''
  else
    result := FPublicKey.value;
end;

Procedure TFhirHealthcareService.SetPublicKeyST(value : String);
begin
  if value <> '' then
  begin
    if FPublicKey = nil then
      FPublicKey := TFhirString.create;
    FPublicKey.value := value
  end
  else if FPublicKey <> nil then
    FPublicKey.value := '';
end;

Procedure TFhirHealthcareService.SetAppointmentRequired(value : TFhirBoolean);
begin
  FAppointmentRequired.free;
  FAppointmentRequired := value;
end;

Function TFhirHealthcareService.GetAppointmentRequiredST : Boolean;
begin
  if FAppointmentRequired = nil then
    result := false
  else
    result := FAppointmentRequired.value;
end;

Procedure TFhirHealthcareService.SetAppointmentRequiredST(value : Boolean);
begin
  if FAppointmentRequired = nil then
    FAppointmentRequired := TFhirBoolean.create;
  FAppointmentRequired.value := value
end;

Function TFhirHealthcareService.GetAvailableTimeList : TFhirHealthcareServiceAvailableTimeList;
begin
  if FAvailableTimeList = nil then
    FAvailableTimeList := TFhirHealthcareServiceAvailableTimeList.Create;
  result := FAvailableTimeList;
end;

Function TFhirHealthcareService.GetHasAvailableTimeList : boolean;
begin
  result := (FAvailableTimeList <> nil) and (FAvailableTimeList.count > 0);
end;

Function TFhirHealthcareService.GetNotAvailableList : TFhirHealthcareServiceNotAvailableList;
begin
  if FNotAvailableList = nil then
    FNotAvailableList := TFhirHealthcareServiceNotAvailableList.Create;
  result := FNotAvailableList;
end;

Function TFhirHealthcareService.GetHasNotAvailableList : boolean;
begin
  result := (FNotAvailableList <> nil) and (FNotAvailableList.count > 0);
end;

Procedure TFhirHealthcareService.SetAvailabilityExceptions(value : TFhirString);
begin
  FAvailabilityExceptions.free;
  FAvailabilityExceptions := value;
end;

Function TFhirHealthcareService.GetAvailabilityExceptionsST : String;
begin
  if FAvailabilityExceptions = nil then
    result := ''
  else
    result := FAvailabilityExceptions.value;
end;

Procedure TFhirHealthcareService.SetAvailabilityExceptionsST(value : String);
begin
  if value <> '' then
  begin
    if FAvailabilityExceptions = nil then
      FAvailabilityExceptions := TFhirString.create;
    FAvailabilityExceptions.value := value
  end
  else if FAvailabilityExceptions <> nil then
    FAvailabilityExceptions.value := '';
end;

function TFhirHealthcareService.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FProvidedBy.sizeInBytes);
  inc(result, FServiceCategory.sizeInBytes);
  inc(result, FserviceTypeList.sizeInBytes);
  inc(result, FLocation.sizeInBytes);
  inc(result, FServiceName.sizeInBytes);
  inc(result, FComment.sizeInBytes);
  inc(result, FExtraDetails.sizeInBytes);
  inc(result, FPhoto.sizeInBytes);
  inc(result, FtelecomList.sizeInBytes);
  inc(result, FcoverageAreaList.sizeInBytes);
  inc(result, FserviceProvisionCodeList.sizeInBytes);
  inc(result, FEligibility.sizeInBytes);
  inc(result, FEligibilityNote.sizeInBytes);
  inc(result, FprogramNameList.sizeInBytes);
  inc(result, FcharacteristicList.sizeInBytes);
  inc(result, FreferralMethodList.sizeInBytes);
  inc(result, FPublicKey.sizeInBytes);
  inc(result, FAppointmentRequired.sizeInBytes);
  inc(result, FavailableTimeList.sizeInBytes);
  inc(result, FnotAvailableList.sizeInBytes);
  inc(result, FAvailabilityExceptions.sizeInBytes);
end;

{ TFhirHealthcareServiceListEnumerator }

Constructor TFhirHealthcareServiceListEnumerator.Create(list : TFhirHealthcareServiceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirHealthcareServiceListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirHealthcareServiceListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirHealthcareServiceListEnumerator.GetCurrent : TFhirHealthcareService;
begin
  Result := FList[FIndex];
end;

function TFhirHealthcareServiceListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirHealthcareServiceList }
procedure TFhirHealthcareServiceList.AddItem(value: TFhirHealthcareService);
begin
  assert(value.ClassName = 'TFhirHealthcareService', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirHealthcareService');
  add(value);
end;

function TFhirHealthcareServiceList.Append: TFhirHealthcareService;
begin
  result := TFhirHealthcareService.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHealthcareServiceList.ClearItems;
begin
  Clear;
end;

function TFhirHealthcareServiceList.GetEnumerator : TFhirHealthcareServiceListEnumerator;
begin
  result := TFhirHealthcareServiceListEnumerator.Create(self.link);
end;

function TFhirHealthcareServiceList.Clone: TFhirHealthcareServiceList;
begin
  result := TFhirHealthcareServiceList(inherited Clone);
end;

function TFhirHealthcareServiceList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirHealthcareServiceList.GetItemN(index: Integer): TFhirHealthcareService;
begin
  result := TFhirHealthcareService(ObjectByIndex[index]);
end;

function TFhirHealthcareServiceList.ItemClass: TFslObjectClass;
begin
  result := TFhirHealthcareService;
end;
function TFhirHealthcareServiceList.IndexOf(value: TFhirHealthcareService): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirHealthcareServiceList.Insert(index: Integer): TFhirHealthcareService;
begin
  result := TFhirHealthcareService.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirHealthcareServiceList.InsertItem(index: Integer; value: TFhirHealthcareService);
begin
  assert(value is TFhirHealthcareService);
  Inherited Insert(index, value);
end;

function TFhirHealthcareServiceList.Item(index: Integer): TFhirHealthcareService;
begin
  result := TFhirHealthcareService(ObjectByIndex[index]);
end;

function TFhirHealthcareServiceList.Link: TFhirHealthcareServiceList;
begin
  result := TFhirHealthcareServiceList(inherited Link);
end;

procedure TFhirHealthcareServiceList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirHealthcareServiceList.SetItemByIndex(index: Integer; value: TFhirHealthcareService);
begin
  assert(value is TFhirHealthcareService);
  FhirHealthcareServices[index] := value;
end;

procedure TFhirHealthcareServiceList.SetItemN(index: Integer; value: TFhirHealthcareService);
begin
  assert(value is TFhirHealthcareService);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_HEALTHCARESERVICE}

{$IFDEF FHIR_LOCATION}

{ TFhirLocationPosition }

constructor TFhirLocationPosition.Create;
begin
  inherited;
end;

destructor TFhirLocationPosition.Destroy;
begin
  FLongitude.free;
  FLatitude.free;
  FAltitude.free;
  inherited;
end;

procedure TFhirLocationPosition.Assign(oSource : TFslObject);
begin
  inherited;
  longitudeElement := TFhirLocationPosition(oSource).longitudeElement.Clone;
  latitudeElement := TFhirLocationPosition(oSource).latitudeElement.Clone;
  altitudeElement := TFhirLocationPosition(oSource).altitudeElement.Clone;
end;

procedure TFhirLocationPosition.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'longitude') Then
     list.add(self.link, 'longitude', FLongitude.Link);
  if (child_name = 'latitude') Then
     list.add(self.link, 'latitude', FLatitude.Link);
  if (child_name = 'altitude') Then
     list.add(self.link, 'altitude', FAltitude.Link);
end;

procedure TFhirLocationPosition.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'longitude', 'decimal', false, TFhirDecimal, FLongitude.Link));{2}
  oList.add(TFHIRProperty.create(self, 'latitude', 'decimal', false, TFhirDecimal, FLatitude.Link));{2}
  oList.add(TFHIRProperty.create(self, 'altitude', 'decimal', false, TFhirDecimal, FAltitude.Link));{2}
end;

function TFhirLocationPosition.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'longitude') then
  begin
    LongitudeElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'latitude') then
  begin
    LatitudeElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else if (propName = 'altitude') then
  begin
    AltitudeElement := asDecimal(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirLocationPosition.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirLocationPosition.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'longitude') then result := TFhirDecimal.create() {5b}
  else if (propName = 'latitude') then result := TFhirDecimal.create() {5b}
  else if (propName = 'altitude') then result := TFhirDecimal.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirLocationPosition.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'longitude') then result := 'decimal'
  else if (propName = 'latitude') then result := 'decimal'
  else if (propName = 'altitude') then result := 'decimal'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirLocationPosition.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'longitude') then LongitudeElement := nil
  else if (propName = 'latitude') then LatitudeElement := nil
  else if (propName = 'altitude') then AltitudeElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirLocationPosition.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'longitude') then LongitudeElement := asDecimal(new){5b}
  else if (propName = 'latitude') then LatitudeElement := asDecimal(new){5b}
  else if (propName = 'altitude') then AltitudeElement := asDecimal(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirLocationPosition.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirLocationPosition.fhirType : string;
begin
  result := 'position';
end;

function TFhirLocationPosition.Link : TFhirLocationPosition;
begin
  result := TFhirLocationPosition(inherited Link);
end;

function TFhirLocationPosition.Clone : TFhirLocationPosition;
begin
  result := TFhirLocationPosition(inherited Clone);
end;

function TFhirLocationPosition.equals(other : TObject) : boolean;
var
  o : TFhirLocationPosition;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirLocationPosition)) then
    result := false
  else
  begin
    o := TFhirLocationPosition(other);
    result := compareDeep(longitudeElement, o.longitudeElement, true) and compareDeep(latitudeElement, o.latitudeElement, true) and
      compareDeep(altitudeElement, o.altitudeElement, true);
  end;
end;

function TFhirLocationPosition.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FLongitude) and isEmptyProp(FLatitude) and isEmptyProp(FAltitude);
end;

procedure TFhirLocationPosition.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('longitude');
  fields.add('latitude');
  fields.add('altitude');
end;

{ TFhirLocationPosition }

Procedure TFhirLocationPosition.SetLongitude(value : TFhirDecimal);
begin
  FLongitude.free;
  FLongitude := value;
end;

Function TFhirLocationPosition.GetLongitudeST : String;
begin
  if FLongitude = nil then
    result := ''
  else
    result := FLongitude.value;
end;

Procedure TFhirLocationPosition.SetLongitudeST(value : String);
begin
  if value <> '' then
  begin
    if FLongitude = nil then
      FLongitude := TFhirDecimal.create;
    FLongitude.value := value
  end
  else if FLongitude <> nil then
    FLongitude.value := '';
end;

Procedure TFhirLocationPosition.SetLatitude(value : TFhirDecimal);
begin
  FLatitude.free;
  FLatitude := value;
end;

Function TFhirLocationPosition.GetLatitudeST : String;
begin
  if FLatitude = nil then
    result := ''
  else
    result := FLatitude.value;
end;

Procedure TFhirLocationPosition.SetLatitudeST(value : String);
begin
  if value <> '' then
  begin
    if FLatitude = nil then
      FLatitude := TFhirDecimal.create;
    FLatitude.value := value
  end
  else if FLatitude <> nil then
    FLatitude.value := '';
end;

Procedure TFhirLocationPosition.SetAltitude(value : TFhirDecimal);
begin
  FAltitude.free;
  FAltitude := value;
end;

Function TFhirLocationPosition.GetAltitudeST : String;
begin
  if FAltitude = nil then
    result := ''
  else
    result := FAltitude.value;
end;

Procedure TFhirLocationPosition.SetAltitudeST(value : String);
begin
  if value <> '' then
  begin
    if FAltitude = nil then
      FAltitude := TFhirDecimal.create;
    FAltitude.value := value
  end
  else if FAltitude <> nil then
    FAltitude.value := '';
end;

function TFhirLocationPosition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLongitude.sizeInBytes);
  inc(result, FLatitude.sizeInBytes);
  inc(result, FAltitude.sizeInBytes);
end;

{ TFhirLocationPositionListEnumerator }

Constructor TFhirLocationPositionListEnumerator.Create(list : TFhirLocationPositionList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirLocationPositionListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirLocationPositionListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirLocationPositionListEnumerator.GetCurrent : TFhirLocationPosition;
begin
  Result := FList[FIndex];
end;

function TFhirLocationPositionListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirLocationPositionList }
procedure TFhirLocationPositionList.AddItem(value: TFhirLocationPosition);
begin
  assert(value.ClassName = 'TFhirLocationPosition', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirLocationPosition');
  add(value);
end;

function TFhirLocationPositionList.Append: TFhirLocationPosition;
begin
  result := TFhirLocationPosition.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirLocationPositionList.ClearItems;
begin
  Clear;
end;

function TFhirLocationPositionList.GetEnumerator : TFhirLocationPositionListEnumerator;
begin
  result := TFhirLocationPositionListEnumerator.Create(self.link);
end;

function TFhirLocationPositionList.Clone: TFhirLocationPositionList;
begin
  result := TFhirLocationPositionList(inherited Clone);
end;

function TFhirLocationPositionList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirLocationPositionList.GetItemN(index: Integer): TFhirLocationPosition;
begin
  result := TFhirLocationPosition(ObjectByIndex[index]);
end;

function TFhirLocationPositionList.ItemClass: TFslObjectClass;
begin
  result := TFhirLocationPosition;
end;
function TFhirLocationPositionList.IndexOf(value: TFhirLocationPosition): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirLocationPositionList.Insert(index: Integer): TFhirLocationPosition;
begin
  result := TFhirLocationPosition.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirLocationPositionList.InsertItem(index: Integer; value: TFhirLocationPosition);
begin
  assert(value is TFhirLocationPosition);
  Inherited Insert(index, value);
end;

function TFhirLocationPositionList.Item(index: Integer): TFhirLocationPosition;
begin
  result := TFhirLocationPosition(ObjectByIndex[index]);
end;

function TFhirLocationPositionList.Link: TFhirLocationPositionList;
begin
  result := TFhirLocationPositionList(inherited Link);
end;

procedure TFhirLocationPositionList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirLocationPositionList.SetItemByIndex(index: Integer; value: TFhirLocationPosition);
begin
  assert(value is TFhirLocationPosition);
  FhirLocationPositions[index] := value;
end;

procedure TFhirLocationPositionList.SetItemN(index: Integer; value: TFhirLocationPosition);
begin
  assert(value is TFhirLocationPosition);
  ObjectByIndex[index] := value;
end;

{ TFhirLocation }

constructor TFhirLocation.Create;
begin
  inherited;
end;

destructor TFhirLocation.Destroy;
begin
  FIdentifierList.Free;
  FStatus.free;
  FName.free;
  FDescription.free;
  FMode.free;
  FType_.free;
  FTelecomList.Free;
  FAddress.free;
  FPhysicalType.free;
  FPosition.free;
  FManagingOrganization.free;
  FPartOf.free;
  inherited;
end;

function TFhirLocation.GetResourceType : TFhirResourceType;
begin
  result := frtLocation;
end;

procedure TFhirLocation.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirLocation(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirLocation(oSource).FIdentifierList);
  end;
  FStatus := TFhirLocation(oSource).FStatus.Link;
  nameElement := TFhirLocation(oSource).nameElement.Clone;
  descriptionElement := TFhirLocation(oSource).descriptionElement.Clone;
  FMode := TFhirLocation(oSource).FMode.Link;
  type_ := TFhirLocation(oSource).type_.Clone;
  if (TFhirLocation(oSource).FTelecomList = nil) then
  begin
    FTelecomList.free;
    FTelecomList := nil;
  end
  else
  begin
    if FTelecomList = nil then
      FTelecomList := TFhirContactPointList.Create;
    FTelecomList.Assign(TFhirLocation(oSource).FTelecomList);
  end;
  address := TFhirLocation(oSource).address.Clone;
  physicalType := TFhirLocation(oSource).physicalType.Clone;
  position := TFhirLocation(oSource).position.Clone;
  managingOrganization := TFhirLocation(oSource).managingOrganization.Clone;
  partOf := TFhirLocation(oSource).partOf.Clone;
end;

procedure TFhirLocation.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'status') Then
     list.add(self.link, 'status', FStatus.Link);
  if (child_name = 'name') Then
     list.add(self.link, 'name', FName.Link);
  if (child_name = 'description') Then
     list.add(self.link, 'description', FDescription.Link);
  if (child_name = 'mode') Then
     list.add(self.link, 'mode', FMode.Link);
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'telecom') Then
    list.addAll(self, 'telecom', FTelecomList);
  if (child_name = 'address') Then
     list.add(self.link, 'address', FAddress.Link);
  if (child_name = 'physicalType') Then
     list.add(self.link, 'physicalType', FPhysicalType.Link);
  if (child_name = 'position') Then
     list.add(self.link, 'position', FPosition.Link);
  if (child_name = 'managingOrganization') Then
     list.add(self.link, 'managingOrganization', FManagingOrganization.Link);
  if (child_name = 'partOf') Then
     list.add(self.link, 'partOf', FPartOf.Link);
end;

procedure TFhirLocation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', false, TFHIREnum, FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'name', 'string', false, TFhirString, FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', false, TFhirString, FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mode', 'code', false, TFHIREnum, FMode.Link));{1}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', false, TFhirCodeableConcept, FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'ContactPoint', true, TFhirContactPoint, FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', false, TFhirAddress, FAddress.Link));{2}
  oList.add(TFHIRProperty.create(self, 'physicalType', 'CodeableConcept', false, TFhirCodeableConcept, FPhysicalType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'position', '', false, TFhirLocationPosition, FPosition.Link));{2}
  oList.add(TFHIRProperty.create(self, 'managingOrganization', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FManagingOrganization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'partOf', 'Reference(Location)', false, TFhirReference{TFhirLocation}, FPartOf.Link));{2}
end;

function TFhirLocation.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'status') then
  begin
    StatusElement := asEnum(SYSTEMS_TFhirLocationStatusEnum, CODES_TFhirLocationStatusEnum, propValue);
    result := propValue
  end
  else if (propName = 'name') then
  begin
    NameElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'description') then
  begin
    DescriptionElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'mode') then
  begin
    ModeElement := asEnum(SYSTEMS_TFhirLocationModeEnum, CODES_TFhirLocationModeEnum, propValue);
    result := propValue
  end
  else if (propName = 'type') then
  begin
    Type_ := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'telecom') then
  begin
    TelecomList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'address') then
  begin
    Address := propValue as TFhirAddress{4b};
    result := propValue;
  end
  else if (propName = 'physicalType') then
  begin
    PhysicalType := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'position') then
  begin
    Position := propValue as TFhirLocationPosition{4b};
    result := propValue;
  end
  else if (propName = 'managingOrganization') then
  begin
    ManagingOrganization := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'partOf') then
  begin
    PartOf := propValue as TFhirReference{TFhirLocation}{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirLocation.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'telecom') then TelecomList.insertItem(index, propValue as TFhirContactPoint){2a}
  else inherited;
end;

function TFhirLocation.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'name') then result := TFhirString.create() {5b}
  else if (propName = 'description') then result := TFhirString.create() {5b}
  else if (propName = 'type') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'telecom') then result := TelecomList.new(){2}
  else if (propName = 'address') then result := TFhirAddress.create(){4b}
  else if (propName = 'physicalType') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'position') then result := TFhirLocationPosition.create(){4b}
  else if (propName = 'managingOrganization') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'partOf') then result := TFhirReference{TFhirLocation}.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirLocation.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'status') then result := 'code'
  else if (propName = 'name') then result := 'string'
  else if (propName = 'description') then result := 'string'
  else if (propName = 'mode') then result := 'code'
  else if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'telecom') then result := 'ContactPoint'
  else if (propName = 'address') then result := 'Address'
  else if (propName = 'physicalType') then result := 'CodeableConcept'
  else if (propName = 'position') then result := ''
  else if (propName = 'managingOrganization') then result := 'Reference'
  else if (propName = 'partOf') then result := 'Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirLocation.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'status') then StatusElement := nil
  else if (propName = 'name') then NameElement := nil
  else if (propName = 'description') then DescriptionElement := nil
  else if (propName = 'mode') then ModeElement := nil
  else if (propName = 'type') then Type_Element := nil
  else if (propName = 'telecom') then deletePropertyValue('telecom', TelecomList, value) {2}
  else if (propName = 'address') then AddressElement := nil
  else if (propName = 'physicalType') then PhysicalTypeElement := nil
  else if (propName = 'position') then PositionElement := nil
  else if (propName = 'managingOrganization') then ManagingOrganizationElement := nil
  else if (propName = 'partOf') then PartOfElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirLocation.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'status') then StatusElement := asEnum(SYSTEMS_TFhirLocationStatusEnum, CODES_TFhirLocationStatusEnum, new){4}
  else if (propName = 'name') then NameElement := asString(new){5b}
  else if (propName = 'description') then DescriptionElement := asString(new){5b}
  else if (propName = 'mode') then ModeElement := asEnum(SYSTEMS_TFhirLocationModeEnum, CODES_TFhirLocationModeEnum, new){4}
  else if (propName = 'type') then Type_Element := new as TFhirCodeableConcept{4}
  else if (propName = 'telecom') then replacePropertyValue('telecom', TelecomList, existing, new) {2}
  else if (propName = 'address') then AddressElement := new as TFhirAddress{4}
  else if (propName = 'physicalType') then PhysicalTypeElement := new as TFhirCodeableConcept{4}
  else if (propName = 'position') then PositionElement := new as TFhirLocationPosition{4}
  else if (propName = 'managingOrganization') then ManagingOrganizationElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'partOf') then PartOfElement := new as TFhirReference{TFhirLocation}{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirLocation.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'telecom') then TelecomList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirLocation.fhirType : string;
begin
  result := 'Location';
end;

function TFhirLocation.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FStatus) and isEmptyProp(FName) and isEmptyProp(FDescription) and isEmptyProp(FMode) and isEmptyProp(FType_) and isEmptyProp(FtelecomList) and isEmptyProp(FAddress) and isEmptyProp(FPhysicalType) and isEmptyProp(FPosition) and isEmptyProp(FManagingOrganization) and isEmptyProp(FPartOf);
end;

function TFhirLocation.equals(other : TObject) : boolean;
var
  o : TFhirLocation;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirLocation)) then
    result := false
  else
  begin
    o := TFhirLocation(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(statusElement, o.statusElement, true) and
      compareDeep(nameElement, o.nameElement, true) and compareDeep(descriptionElement, o.descriptionElement, true) and
      compareDeep(modeElement, o.modeElement, true) and compareDeep(type_Element, o.type_Element, true) and
      compareDeep(telecomList, o.telecomList, true) and compareDeep(addressElement, o.addressElement, true) and
      compareDeep(physicalTypeElement, o.physicalTypeElement, true) and compareDeep(positionElement, o.positionElement, true) and
      compareDeep(managingOrganizationElement, o.managingOrganizationElement, true) and
      compareDeep(partOfElement, o.partOfElement, true);
  end;
end;

function TFhirLocation.Link : TFhirLocation;
begin
  result := TFhirLocation(inherited Link);
end;

function TFhirLocation.Clone : TFhirLocation;
begin
  result := TFhirLocation(inherited Clone);
end;

procedure TFhirLocation.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('status');
  fields.add('name');
  fields.add('description');
  fields.add('mode');
  fields.add('type');
  fields.add('telecom');
  fields.add('address');
  fields.add('physicalType');
  fields.add('position');
  fields.add('managingOrganization');
  fields.add('partOf');
end;

{ TFhirLocation }

Function TFhirLocation.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirLocation.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirLocation.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirLocation.GetStatusST : TFhirLocationStatusEnum;
begin
  if FStatus = nil then
    result := TFhirLocationStatusEnum(0)
  else
    result := TFhirLocationStatusEnum(StringArrayIndexOfSensitive(CODES_TFhirLocationStatusEnum, FStatus.value));
end;

Procedure TFhirLocation.SetStatusST(value : TFhirLocationStatusEnum);
begin
  if ord(value) = 0 then
    StatusElement := nil
  else
    StatusElement := TFhirEnum.create(SYSTEMS_TFhirLocationStatusEnum[value], CODES_TFhirLocationStatusEnum[value]);
end;

Procedure TFhirLocation.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirLocation.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := FName.value;
end;

Procedure TFhirLocation.SetNameST(value : String);
begin
  if value <> '' then
  begin
    if FName = nil then
      FName := TFhirString.create;
    FName.value := value
  end
  else if FName <> nil then
    FName.value := '';
end;

Procedure TFhirLocation.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirLocation.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := FDescription.value;
end;

Procedure TFhirLocation.SetDescriptionST(value : String);
begin
  if value <> '' then
  begin
    if FDescription = nil then
      FDescription := TFhirString.create;
    FDescription.value := value
  end
  else if FDescription <> nil then
    FDescription.value := '';
end;

Procedure TFhirLocation.SetMode(value : TFhirEnum);
begin
  FMode.free;
  FMode := value;
end;

Function TFhirLocation.GetModeST : TFhirLocationModeEnum;
begin
  if FMode = nil then
    result := TFhirLocationModeEnum(0)
  else
    result := TFhirLocationModeEnum(StringArrayIndexOfSensitive(CODES_TFhirLocationModeEnum, FMode.value));
end;

Procedure TFhirLocation.SetModeST(value : TFhirLocationModeEnum);
begin
  if ord(value) = 0 then
    ModeElement := nil
  else
    ModeElement := TFhirEnum.create(SYSTEMS_TFhirLocationModeEnum[value], CODES_TFhirLocationModeEnum[value]);
end;

Procedure TFhirLocation.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirLocation.GetTelecomList : TFhirContactPointList;
begin
  if FTelecomList = nil then
    FTelecomList := TFhirContactPointList.Create;
  result := FTelecomList;
end;

Function TFhirLocation.GetHasTelecomList : boolean;
begin
  result := (FTelecomList <> nil) and (FTelecomList.count > 0);
end;

Procedure TFhirLocation.SetAddress(value : TFhirAddress);
begin
  FAddress.free;
  FAddress := value;
end;

Procedure TFhirLocation.SetPhysicalType(value : TFhirCodeableConcept);
begin
  FPhysicalType.free;
  FPhysicalType := value;
end;

Procedure TFhirLocation.SetPosition(value : TFhirLocationPosition);
begin
  FPosition.free;
  FPosition := value;
end;

Procedure TFhirLocation.SetManagingOrganization(value : TFhirReference{TFhirOrganization});
begin
  FManagingOrganization.free;
  FManagingOrganization := value;
end;

Procedure TFhirLocation.SetPartOf(value : TFhirReference{TFhirLocation});
begin
  FPartOf.free;
  FPartOf := value;
end;

function TFhirLocation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FStatus.sizeInBytes);
  inc(result, FName.sizeInBytes);
  inc(result, FDescription.sizeInBytes);
  inc(result, FMode.sizeInBytes);
  inc(result, FType_.sizeInBytes);
  inc(result, FtelecomList.sizeInBytes);
  inc(result, FAddress.sizeInBytes);
  inc(result, FPhysicalType.sizeInBytes);
  inc(result, FPosition.sizeInBytes);
  inc(result, FManagingOrganization.sizeInBytes);
  inc(result, FPartOf.sizeInBytes);
end;

{ TFhirLocationListEnumerator }

Constructor TFhirLocationListEnumerator.Create(list : TFhirLocationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirLocationListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirLocationListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirLocationListEnumerator.GetCurrent : TFhirLocation;
begin
  Result := FList[FIndex];
end;

function TFhirLocationListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirLocationList }
procedure TFhirLocationList.AddItem(value: TFhirLocation);
begin
  assert(value.ClassName = 'TFhirLocation', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirLocation');
  add(value);
end;

function TFhirLocationList.Append: TFhirLocation;
begin
  result := TFhirLocation.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirLocationList.ClearItems;
begin
  Clear;
end;

function TFhirLocationList.GetEnumerator : TFhirLocationListEnumerator;
begin
  result := TFhirLocationListEnumerator.Create(self.link);
end;

function TFhirLocationList.Clone: TFhirLocationList;
begin
  result := TFhirLocationList(inherited Clone);
end;

function TFhirLocationList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirLocationList.GetItemN(index: Integer): TFhirLocation;
begin
  result := TFhirLocation(ObjectByIndex[index]);
end;

function TFhirLocationList.ItemClass: TFslObjectClass;
begin
  result := TFhirLocation;
end;
function TFhirLocationList.IndexOf(value: TFhirLocation): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirLocationList.Insert(index: Integer): TFhirLocation;
begin
  result := TFhirLocation.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirLocationList.InsertItem(index: Integer; value: TFhirLocation);
begin
  assert(value is TFhirLocation);
  Inherited Insert(index, value);
end;

function TFhirLocationList.Item(index: Integer): TFhirLocation;
begin
  result := TFhirLocation(ObjectByIndex[index]);
end;

function TFhirLocationList.Link: TFhirLocationList;
begin
  result := TFhirLocationList(inherited Link);
end;

procedure TFhirLocationList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirLocationList.SetItemByIndex(index: Integer; value: TFhirLocation);
begin
  assert(value is TFhirLocation);
  FhirLocations[index] := value;
end;

procedure TFhirLocationList.SetItemN(index: Integer; value: TFhirLocation);
begin
  assert(value is TFhirLocation);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_LOCATION}

{$IFDEF FHIR_ORGANIZATION}

{ TFhirOrganizationContact }

constructor TFhirOrganizationContact.Create;
begin
  inherited;
end;

destructor TFhirOrganizationContact.Destroy;
begin
  FPurpose.free;
  FName.free;
  FTelecomList.Free;
  FAddress.free;
  inherited;
end;

procedure TFhirOrganizationContact.Assign(oSource : TFslObject);
begin
  inherited;
  purpose := TFhirOrganizationContact(oSource).purpose.Clone;
  name := TFhirOrganizationContact(oSource).name.Clone;
  if (TFhirOrganizationContact(oSource).FTelecomList = nil) then
  begin
    FTelecomList.free;
    FTelecomList := nil;
  end
  else
  begin
    if FTelecomList = nil then
      FTelecomList := TFhirContactPointList.Create;
    FTelecomList.Assign(TFhirOrganizationContact(oSource).FTelecomList);
  end;
  address := TFhirOrganizationContact(oSource).address.Clone;
end;

procedure TFhirOrganizationContact.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'purpose') Then
     list.add(self.link, 'purpose', FPurpose.Link);
  if (child_name = 'name') Then
     list.add(self.link, 'name', FName.Link);
  if (child_name = 'telecom') Then
    list.addAll(self, 'telecom', FTelecomList);
  if (child_name = 'address') Then
     list.add(self.link, 'address', FAddress.Link);
end;

procedure TFhirOrganizationContact.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'purpose', 'CodeableConcept', false, TFhirCodeableConcept, FPurpose.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', false, TFhirHumanName, FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'ContactPoint', true, TFhirContactPoint, FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', false, TFhirAddress, FAddress.Link));{2}
end;

function TFhirOrganizationContact.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'purpose') then
  begin
    Purpose := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'name') then
  begin
    Name := propValue as TFhirHumanName{4b};
    result := propValue;
  end
  else if (propName = 'telecom') then
  begin
    TelecomList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'address') then
  begin
    Address := propValue as TFhirAddress{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirOrganizationContact.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'telecom') then TelecomList.insertItem(index, propValue as TFhirContactPoint){2a}
  else inherited;
end;

function TFhirOrganizationContact.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'purpose') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'name') then result := TFhirHumanName.create(){4b}
  else if (propName = 'telecom') then result := TelecomList.new(){2}
  else if (propName = 'address') then result := TFhirAddress.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirOrganizationContact.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'purpose') then result := 'CodeableConcept'
  else if (propName = 'name') then result := 'HumanName'
  else if (propName = 'telecom') then result := 'ContactPoint'
  else if (propName = 'address') then result := 'Address'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirOrganizationContact.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'purpose') then PurposeElement := nil
  else if (propName = 'name') then NameElement := nil
  else if (propName = 'telecom') then deletePropertyValue('telecom', TelecomList, value) {2}
  else if (propName = 'address') then AddressElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirOrganizationContact.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'purpose') then PurposeElement := new as TFhirCodeableConcept{4}
  else if (propName = 'name') then NameElement := new as TFhirHumanName{4}
  else if (propName = 'telecom') then replacePropertyValue('telecom', TelecomList, existing, new) {2}
  else if (propName = 'address') then AddressElement := new as TFhirAddress{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirOrganizationContact.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'telecom') then TelecomList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirOrganizationContact.fhirType : string;
begin
  result := 'contact';
end;

function TFhirOrganizationContact.Link : TFhirOrganizationContact;
begin
  result := TFhirOrganizationContact(inherited Link);
end;

function TFhirOrganizationContact.Clone : TFhirOrganizationContact;
begin
  result := TFhirOrganizationContact(inherited Clone);
end;

function TFhirOrganizationContact.equals(other : TObject) : boolean;
var
  o : TFhirOrganizationContact;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirOrganizationContact)) then
    result := false
  else
  begin
    o := TFhirOrganizationContact(other);
    result := compareDeep(purposeElement, o.purposeElement, true) and compareDeep(nameElement, o.nameElement, true) and
      compareDeep(telecomList, o.telecomList, true) and compareDeep(addressElement, o.addressElement, true);
  end;
end;

function TFhirOrganizationContact.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FPurpose) and isEmptyProp(FName) and isEmptyProp(FtelecomList) and isEmptyProp(FAddress);
end;

procedure TFhirOrganizationContact.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('purpose');
  fields.add('name');
  fields.add('telecom');
  fields.add('address');
end;

{ TFhirOrganizationContact }

Procedure TFhirOrganizationContact.SetPurpose(value : TFhirCodeableConcept);
begin
  FPurpose.free;
  FPurpose := value;
end;

Procedure TFhirOrganizationContact.SetName(value : TFhirHumanName);
begin
  FName.free;
  FName := value;
end;

Function TFhirOrganizationContact.GetTelecomList : TFhirContactPointList;
begin
  if FTelecomList = nil then
    FTelecomList := TFhirContactPointList.Create;
  result := FTelecomList;
end;

Function TFhirOrganizationContact.GetHasTelecomList : boolean;
begin
  result := (FTelecomList <> nil) and (FTelecomList.count > 0);
end;

Procedure TFhirOrganizationContact.SetAddress(value : TFhirAddress);
begin
  FAddress.free;
  FAddress := value;
end;

function TFhirOrganizationContact.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPurpose.sizeInBytes);
  inc(result, FName.sizeInBytes);
  inc(result, FtelecomList.sizeInBytes);
  inc(result, FAddress.sizeInBytes);
end;

{ TFhirOrganizationContactListEnumerator }

Constructor TFhirOrganizationContactListEnumerator.Create(list : TFhirOrganizationContactList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirOrganizationContactListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirOrganizationContactListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirOrganizationContactListEnumerator.GetCurrent : TFhirOrganizationContact;
begin
  Result := FList[FIndex];
end;

function TFhirOrganizationContactListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirOrganizationContactList }
procedure TFhirOrganizationContactList.AddItem(value: TFhirOrganizationContact);
begin
  assert(value.ClassName = 'TFhirOrganizationContact', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirOrganizationContact');
  add(value);
end;

function TFhirOrganizationContactList.Append: TFhirOrganizationContact;
begin
  result := TFhirOrganizationContact.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirOrganizationContactList.ClearItems;
begin
  Clear;
end;

function TFhirOrganizationContactList.GetEnumerator : TFhirOrganizationContactListEnumerator;
begin
  result := TFhirOrganizationContactListEnumerator.Create(self.link);
end;

function TFhirOrganizationContactList.Clone: TFhirOrganizationContactList;
begin
  result := TFhirOrganizationContactList(inherited Clone);
end;

function TFhirOrganizationContactList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirOrganizationContactList.GetItemN(index: Integer): TFhirOrganizationContact;
begin
  result := TFhirOrganizationContact(ObjectByIndex[index]);
end;

function TFhirOrganizationContactList.ItemClass: TFslObjectClass;
begin
  result := TFhirOrganizationContact;
end;
function TFhirOrganizationContactList.IndexOf(value: TFhirOrganizationContact): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirOrganizationContactList.Insert(index: Integer): TFhirOrganizationContact;
begin
  result := TFhirOrganizationContact.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirOrganizationContactList.InsertItem(index: Integer; value: TFhirOrganizationContact);
begin
  assert(value is TFhirOrganizationContact);
  Inherited Insert(index, value);
end;

function TFhirOrganizationContactList.Item(index: Integer): TFhirOrganizationContact;
begin
  result := TFhirOrganizationContact(ObjectByIndex[index]);
end;

function TFhirOrganizationContactList.Link: TFhirOrganizationContactList;
begin
  result := TFhirOrganizationContactList(inherited Link);
end;

procedure TFhirOrganizationContactList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirOrganizationContactList.SetItemByIndex(index: Integer; value: TFhirOrganizationContact);
begin
  assert(value is TFhirOrganizationContact);
  FhirOrganizationContacts[index] := value;
end;

procedure TFhirOrganizationContactList.SetItemN(index: Integer; value: TFhirOrganizationContact);
begin
  assert(value is TFhirOrganizationContact);
  ObjectByIndex[index] := value;
end;

{ TFhirOrganization }

constructor TFhirOrganization.Create;
begin
  inherited;
end;

destructor TFhirOrganization.Destroy;
begin
  FIdentifierList.Free;
  FActive.free;
  FType_.free;
  FName.free;
  FTelecomList.Free;
  FAddressList.Free;
  FPartOf.free;
  FContactList.Free;
  inherited;
end;

function TFhirOrganization.GetResourceType : TFhirResourceType;
begin
  result := frtOrganization;
end;

procedure TFhirOrganization.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirOrganization(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirOrganization(oSource).FIdentifierList);
  end;
  activeElement := TFhirOrganization(oSource).activeElement.Clone;
  type_ := TFhirOrganization(oSource).type_.Clone;
  nameElement := TFhirOrganization(oSource).nameElement.Clone;
  if (TFhirOrganization(oSource).FTelecomList = nil) then
  begin
    FTelecomList.free;
    FTelecomList := nil;
  end
  else
  begin
    if FTelecomList = nil then
      FTelecomList := TFhirContactPointList.Create;
    FTelecomList.Assign(TFhirOrganization(oSource).FTelecomList);
  end;
  if (TFhirOrganization(oSource).FAddressList = nil) then
  begin
    FAddressList.free;
    FAddressList := nil;
  end
  else
  begin
    if FAddressList = nil then
      FAddressList := TFhirAddressList.Create;
    FAddressList.Assign(TFhirOrganization(oSource).FAddressList);
  end;
  partOf := TFhirOrganization(oSource).partOf.Clone;
  if (TFhirOrganization(oSource).FContactList = nil) then
  begin
    FContactList.free;
    FContactList := nil;
  end
  else
  begin
    if FContactList = nil then
      FContactList := TFhirOrganizationContactList.Create;
    FContactList.Assign(TFhirOrganization(oSource).FContactList);
  end;
end;

procedure TFhirOrganization.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'active') Then
     list.add(self.link, 'active', FActive.Link);
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'name') Then
     list.add(self.link, 'name', FName.Link);
  if (child_name = 'telecom') Then
    list.addAll(self, 'telecom', FTelecomList);
  if (child_name = 'address') Then
    list.addAll(self, 'address', FAddressList);
  if (child_name = 'partOf') Then
     list.add(self.link, 'partOf', FPartOf.Link);
  if (child_name = 'contact') Then
    list.addAll(self, 'contact', FContactList);
end;

procedure TFhirOrganization.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'active', 'boolean', false, TFhirBoolean, FActive.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', false, TFhirCodeableConcept, FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', false, TFhirString, FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'ContactPoint', true, TFhirContactPoint, FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', true, TFhirAddress, FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'partOf', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FPartOf.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contact', '', true, TFhirOrganizationContact, FContactList.Link)){3};
end;

function TFhirOrganization.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'active') then
  begin
    ActiveElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'type') then
  begin
    Type_ := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'name') then
  begin
    NameElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'telecom') then
  begin
    TelecomList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'address') then
  begin
    AddressList.add(propValue as TFhirAddress){2a};
    result := propValue;
  end
  else if (propName = 'partOf') then
  begin
    PartOf := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'contact') then
  begin
    ContactList.add(propValue as TFhirOrganizationContact){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirOrganization.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'telecom') then TelecomList.insertItem(index, propValue as TFhirContactPoint){2a}
  else if (propName = 'address') then AddressList.insertItem(index, propValue as TFhirAddress){2a}
  else if (propName = 'contact') then ContactList.insertItem(index, propValue as TFhirOrganizationContact){2a}
  else inherited;
end;

function TFhirOrganization.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'active') then result := TFhirBoolean.create() {5b}
  else if (propName = 'type') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'name') then result := TFhirString.create() {5b}
  else if (propName = 'telecom') then result := TelecomList.new(){2}
  else if (propName = 'address') then result := AddressList.new(){2}
  else if (propName = 'partOf') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'contact') then result := ContactList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirOrganization.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'active') then result := 'boolean'
  else if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'name') then result := 'string'
  else if (propName = 'telecom') then result := 'ContactPoint'
  else if (propName = 'address') then result := 'Address'
  else if (propName = 'partOf') then result := 'Reference'
  else if (propName = 'contact') then result := ''
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirOrganization.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'active') then ActiveElement := nil
  else if (propName = 'type') then Type_Element := nil
  else if (propName = 'name') then NameElement := nil
  else if (propName = 'telecom') then deletePropertyValue('telecom', TelecomList, value) {2}
  else if (propName = 'address') then deletePropertyValue('address', AddressList, value) {2}
  else if (propName = 'partOf') then PartOfElement := nil
  else if (propName = 'contact') then deletePropertyValue('contact', ContactList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirOrganization.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'active') then ActiveElement := asBoolean(new){5b}
  else if (propName = 'type') then Type_Element := new as TFhirCodeableConcept{4}
  else if (propName = 'name') then NameElement := asString(new){5b}
  else if (propName = 'telecom') then replacePropertyValue('telecom', TelecomList, existing, new) {2}
  else if (propName = 'address') then replacePropertyValue('address', AddressList, existing, new) {2}
  else if (propName = 'partOf') then PartOfElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'contact') then replacePropertyValue('contact', ContactList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirOrganization.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'telecom') then TelecomList.move(source, destination){2a}
  else if (propName = 'address') then AddressList.move(source, destination){2a}
  else if (propName = 'contact') then ContactList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirOrganization.fhirType : string;
begin
  result := 'Organization';
end;

function TFhirOrganization.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FActive) and isEmptyProp(FType_) and isEmptyProp(FName) and isEmptyProp(FtelecomList) and isEmptyProp(FaddressList) and isEmptyProp(FPartOf) and isEmptyProp(FcontactList);
end;

function TFhirOrganization.equals(other : TObject) : boolean;
var
  o : TFhirOrganization;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirOrganization)) then
    result := false
  else
  begin
    o := TFhirOrganization(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(activeElement, o.activeElement, true) and
      compareDeep(type_Element, o.type_Element, true) and compareDeep(nameElement, o.nameElement, true) and
      compareDeep(telecomList, o.telecomList, true) and compareDeep(addressList, o.addressList, true) and
      compareDeep(partOfElement, o.partOfElement, true) and compareDeep(contactList, o.contactList, true);
  end;
end;

function TFhirOrganization.Link : TFhirOrganization;
begin
  result := TFhirOrganization(inherited Link);
end;

function TFhirOrganization.Clone : TFhirOrganization;
begin
  result := TFhirOrganization(inherited Clone);
end;

procedure TFhirOrganization.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('active');
  fields.add('type');
  fields.add('name');
  fields.add('telecom');
  fields.add('address');
  fields.add('partOf');
  fields.add('contact');
end;

{ TFhirOrganization }

Function TFhirOrganization.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirOrganization.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirOrganization.SetActive(value : TFhirBoolean);
begin
  FActive.free;
  FActive := value;
end;

Function TFhirOrganization.GetActiveST : Boolean;
begin
  if FActive = nil then
    result := false
  else
    result := FActive.value;
end;

Procedure TFhirOrganization.SetActiveST(value : Boolean);
begin
  if FActive = nil then
    FActive := TFhirBoolean.create;
  FActive.value := value
end;

Procedure TFhirOrganization.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirOrganization.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirOrganization.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := FName.value;
end;

Procedure TFhirOrganization.SetNameST(value : String);
begin
  if value <> '' then
  begin
    if FName = nil then
      FName := TFhirString.create;
    FName.value := value
  end
  else if FName <> nil then
    FName.value := '';
end;

Function TFhirOrganization.GetTelecomList : TFhirContactPointList;
begin
  if FTelecomList = nil then
    FTelecomList := TFhirContactPointList.Create;
  result := FTelecomList;
end;

Function TFhirOrganization.GetHasTelecomList : boolean;
begin
  result := (FTelecomList <> nil) and (FTelecomList.count > 0);
end;

Function TFhirOrganization.GetAddressList : TFhirAddressList;
begin
  if FAddressList = nil then
    FAddressList := TFhirAddressList.Create;
  result := FAddressList;
end;

Function TFhirOrganization.GetHasAddressList : boolean;
begin
  result := (FAddressList <> nil) and (FAddressList.count > 0);
end;

Procedure TFhirOrganization.SetPartOf(value : TFhirReference{TFhirOrganization});
begin
  FPartOf.free;
  FPartOf := value;
end;

Function TFhirOrganization.GetContactList : TFhirOrganizationContactList;
begin
  if FContactList = nil then
    FContactList := TFhirOrganizationContactList.Create;
  result := FContactList;
end;

Function TFhirOrganization.GetHasContactList : boolean;
begin
  result := (FContactList <> nil) and (FContactList.count > 0);
end;

function TFhirOrganization.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FActive.sizeInBytes);
  inc(result, FType_.sizeInBytes);
  inc(result, FName.sizeInBytes);
  inc(result, FtelecomList.sizeInBytes);
  inc(result, FaddressList.sizeInBytes);
  inc(result, FPartOf.sizeInBytes);
  inc(result, FcontactList.sizeInBytes);
end;

{ TFhirOrganizationListEnumerator }

Constructor TFhirOrganizationListEnumerator.Create(list : TFhirOrganizationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirOrganizationListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirOrganizationListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirOrganizationListEnumerator.GetCurrent : TFhirOrganization;
begin
  Result := FList[FIndex];
end;

function TFhirOrganizationListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirOrganizationList }
procedure TFhirOrganizationList.AddItem(value: TFhirOrganization);
begin
  assert(value.ClassName = 'TFhirOrganization', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirOrganization');
  add(value);
end;

function TFhirOrganizationList.Append: TFhirOrganization;
begin
  result := TFhirOrganization.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirOrganizationList.ClearItems;
begin
  Clear;
end;

function TFhirOrganizationList.GetEnumerator : TFhirOrganizationListEnumerator;
begin
  result := TFhirOrganizationListEnumerator.Create(self.link);
end;

function TFhirOrganizationList.Clone: TFhirOrganizationList;
begin
  result := TFhirOrganizationList(inherited Clone);
end;

function TFhirOrganizationList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirOrganizationList.GetItemN(index: Integer): TFhirOrganization;
begin
  result := TFhirOrganization(ObjectByIndex[index]);
end;

function TFhirOrganizationList.ItemClass: TFslObjectClass;
begin
  result := TFhirOrganization;
end;
function TFhirOrganizationList.IndexOf(value: TFhirOrganization): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirOrganizationList.Insert(index: Integer): TFhirOrganization;
begin
  result := TFhirOrganization.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirOrganizationList.InsertItem(index: Integer; value: TFhirOrganization);
begin
  assert(value is TFhirOrganization);
  Inherited Insert(index, value);
end;

function TFhirOrganizationList.Item(index: Integer): TFhirOrganization;
begin
  result := TFhirOrganization(ObjectByIndex[index]);
end;

function TFhirOrganizationList.Link: TFhirOrganizationList;
begin
  result := TFhirOrganizationList(inherited Link);
end;

procedure TFhirOrganizationList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirOrganizationList.SetItemByIndex(index: Integer; value: TFhirOrganization);
begin
  assert(value is TFhirOrganization);
  FhirOrganizations[index] := value;
end;

procedure TFhirOrganizationList.SetItemN(index: Integer; value: TFhirOrganization);
begin
  assert(value is TFhirOrganization);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_ORGANIZATION}

{$IFDEF FHIR_PATIENT}

{ TFhirPatientContact }

constructor TFhirPatientContact.Create;
begin
  inherited;
end;

destructor TFhirPatientContact.Destroy;
begin
  FRelationshipList.Free;
  FName.free;
  FTelecomList.Free;
  FAddress.free;
  FGender.free;
  FOrganization.free;
  FPeriod.free;
  inherited;
end;

procedure TFhirPatientContact.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirPatientContact(oSource).FRelationshipList = nil) then
  begin
    FRelationshipList.free;
    FRelationshipList := nil;
  end
  else
  begin
    if FRelationshipList = nil then
      FRelationshipList := TFhirCodeableConceptList.Create;
    FRelationshipList.Assign(TFhirPatientContact(oSource).FRelationshipList);
  end;
  name := TFhirPatientContact(oSource).name.Clone;
  if (TFhirPatientContact(oSource).FTelecomList = nil) then
  begin
    FTelecomList.free;
    FTelecomList := nil;
  end
  else
  begin
    if FTelecomList = nil then
      FTelecomList := TFhirContactPointList.Create;
    FTelecomList.Assign(TFhirPatientContact(oSource).FTelecomList);
  end;
  address := TFhirPatientContact(oSource).address.Clone;
  FGender := TFhirPatientContact(oSource).FGender.Link;
  organization := TFhirPatientContact(oSource).organization.Clone;
  period := TFhirPatientContact(oSource).period.Clone;
end;

procedure TFhirPatientContact.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'relationship') Then
    list.addAll(self, 'relationship', FRelationshipList);
  if (child_name = 'name') Then
     list.add(self.link, 'name', FName.Link);
  if (child_name = 'telecom') Then
    list.addAll(self, 'telecom', FTelecomList);
  if (child_name = 'address') Then
     list.add(self.link, 'address', FAddress.Link);
  if (child_name = 'gender') Then
     list.add(self.link, 'gender', FGender.Link);
  if (child_name = 'organization') Then
     list.add(self.link, 'organization', FOrganization.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
end;

procedure TFhirPatientContact.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'relationship', 'CodeableConcept', true, TFhirCodeableConcept, FRelationshipList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', false, TFhirHumanName, FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'ContactPoint', true, TFhirContactPoint, FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', false, TFhirAddress, FAddress.Link));{2}
  oList.add(TFHIRProperty.create(self, 'gender', 'code', false, TFHIREnum, FGender.Link));{1}
  oList.add(TFHIRProperty.create(self, 'organization', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FOrganization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
end;

function TFhirPatientContact.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'relationship') then
  begin
    RelationshipList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'name') then
  begin
    Name := propValue as TFhirHumanName{4b};
    result := propValue;
  end
  else if (propName = 'telecom') then
  begin
    TelecomList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'address') then
  begin
    Address := propValue as TFhirAddress{4b};
    result := propValue;
  end
  else if (propName = 'gender') then
  begin
    GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, propValue);
    result := propValue
  end
  else if (propName = 'organization') then
  begin
    Organization := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirPatientContact.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'relationship') then RelationshipList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'telecom') then TelecomList.insertItem(index, propValue as TFhirContactPoint){2a}
  else inherited;
end;

function TFhirPatientContact.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'relationship') then result := RelationshipList.new(){2}
  else if (propName = 'name') then result := TFhirHumanName.create(){4b}
  else if (propName = 'telecom') then result := TelecomList.new(){2}
  else if (propName = 'address') then result := TFhirAddress.create(){4b}
  else if (propName = 'organization') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPatientContact.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'relationship') then result := 'CodeableConcept'
  else if (propName = 'name') then result := 'HumanName'
  else if (propName = 'telecom') then result := 'ContactPoint'
  else if (propName = 'address') then result := 'Address'
  else if (propName = 'gender') then result := 'code'
  else if (propName = 'organization') then result := 'Reference'
  else if (propName = 'period') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPatientContact.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'relationship') then deletePropertyValue('relationship', RelationshipList, value) {2}
  else if (propName = 'name') then NameElement := nil
  else if (propName = 'telecom') then deletePropertyValue('telecom', TelecomList, value) {2}
  else if (propName = 'address') then AddressElement := nil
  else if (propName = 'gender') then GenderElement := nil
  else if (propName = 'organization') then OrganizationElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPatientContact.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'relationship') then replacePropertyValue('relationship', RelationshipList, existing, new) {2}
  else if (propName = 'name') then NameElement := new as TFhirHumanName{4}
  else if (propName = 'telecom') then replacePropertyValue('telecom', TelecomList, existing, new) {2}
  else if (propName = 'address') then AddressElement := new as TFhirAddress{4}
  else if (propName = 'gender') then GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, new){4}
  else if (propName = 'organization') then OrganizationElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPatientContact.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'relationship') then RelationshipList.move(source, destination){2a}
  else if (propName = 'telecom') then TelecomList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirPatientContact.fhirType : string;
begin
  result := 'contact';
end;

function TFhirPatientContact.Link : TFhirPatientContact;
begin
  result := TFhirPatientContact(inherited Link);
end;

function TFhirPatientContact.Clone : TFhirPatientContact;
begin
  result := TFhirPatientContact(inherited Clone);
end;

function TFhirPatientContact.equals(other : TObject) : boolean;
var
  o : TFhirPatientContact;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPatientContact)) then
    result := false
  else
  begin
    o := TFhirPatientContact(other);
    result := compareDeep(relationshipList, o.relationshipList, true) and compareDeep(nameElement, o.nameElement, true) and
      compareDeep(telecomList, o.telecomList, true) and compareDeep(addressElement, o.addressElement, true) and
      compareDeep(genderElement, o.genderElement, true) and compareDeep(organizationElement, o.organizationElement, true) and
      compareDeep(periodElement, o.periodElement, true);
  end;
end;

function TFhirPatientContact.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FrelationshipList) and isEmptyProp(FName) and isEmptyProp(FtelecomList) and isEmptyProp(FAddress) and isEmptyProp(FGender) and isEmptyProp(FOrganization) and isEmptyProp(FPeriod);
end;

procedure TFhirPatientContact.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('relationship');
  fields.add('name');
  fields.add('telecom');
  fields.add('address');
  fields.add('gender');
  fields.add('organization');
  fields.add('period');
end;

{ TFhirPatientContact }

Function TFhirPatientContact.GetRelationshipList : TFhirCodeableConceptList;
begin
  if FRelationshipList = nil then
    FRelationshipList := TFhirCodeableConceptList.Create;
  result := FRelationshipList;
end;

Function TFhirPatientContact.GetHasRelationshipList : boolean;
begin
  result := (FRelationshipList <> nil) and (FRelationshipList.count > 0);
end;

Procedure TFhirPatientContact.SetName(value : TFhirHumanName);
begin
  FName.free;
  FName := value;
end;

Function TFhirPatientContact.GetTelecomList : TFhirContactPointList;
begin
  if FTelecomList = nil then
    FTelecomList := TFhirContactPointList.Create;
  result := FTelecomList;
end;

Function TFhirPatientContact.GetHasTelecomList : boolean;
begin
  result := (FTelecomList <> nil) and (FTelecomList.count > 0);
end;

Procedure TFhirPatientContact.SetAddress(value : TFhirAddress);
begin
  FAddress.free;
  FAddress := value;
end;

Procedure TFhirPatientContact.SetGender(value : TFhirEnum);
begin
  FGender.free;
  FGender := value;
end;

Function TFhirPatientContact.GetGenderST : TFhirAdministrativeGenderEnum;
begin
  if FGender = nil then
    result := TFhirAdministrativeGenderEnum(0)
  else
    result := TFhirAdministrativeGenderEnum(StringArrayIndexOfSensitive(CODES_TFhirAdministrativeGenderEnum, FGender.value));
end;

Procedure TFhirPatientContact.SetGenderST(value : TFhirAdministrativeGenderEnum);
begin
  if ord(value) = 0 then
    GenderElement := nil
  else
    GenderElement := TFhirEnum.create(SYSTEMS_TFhirAdministrativeGenderEnum[value], CODES_TFhirAdministrativeGenderEnum[value]);
end;

Procedure TFhirPatientContact.SetOrganization(value : TFhirReference{TFhirOrganization});
begin
  FOrganization.free;
  FOrganization := value;
end;

Procedure TFhirPatientContact.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

function TFhirPatientContact.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FrelationshipList.sizeInBytes);
  inc(result, FName.sizeInBytes);
  inc(result, FtelecomList.sizeInBytes);
  inc(result, FAddress.sizeInBytes);
  inc(result, FGender.sizeInBytes);
  inc(result, FOrganization.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
end;

{ TFhirPatientContactListEnumerator }

Constructor TFhirPatientContactListEnumerator.Create(list : TFhirPatientContactList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPatientContactListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPatientContactListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPatientContactListEnumerator.GetCurrent : TFhirPatientContact;
begin
  Result := FList[FIndex];
end;

function TFhirPatientContactListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPatientContactList }
procedure TFhirPatientContactList.AddItem(value: TFhirPatientContact);
begin
  assert(value.ClassName = 'TFhirPatientContact', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPatientContact');
  add(value);
end;

function TFhirPatientContactList.Append: TFhirPatientContact;
begin
  result := TFhirPatientContact.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientContactList.ClearItems;
begin
  Clear;
end;

function TFhirPatientContactList.GetEnumerator : TFhirPatientContactListEnumerator;
begin
  result := TFhirPatientContactListEnumerator.Create(self.link);
end;

function TFhirPatientContactList.Clone: TFhirPatientContactList;
begin
  result := TFhirPatientContactList(inherited Clone);
end;

function TFhirPatientContactList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPatientContactList.GetItemN(index: Integer): TFhirPatientContact;
begin
  result := TFhirPatientContact(ObjectByIndex[index]);
end;

function TFhirPatientContactList.ItemClass: TFslObjectClass;
begin
  result := TFhirPatientContact;
end;
function TFhirPatientContactList.IndexOf(value: TFhirPatientContact): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPatientContactList.Insert(index: Integer): TFhirPatientContact;
begin
  result := TFhirPatientContact.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientContactList.InsertItem(index: Integer; value: TFhirPatientContact);
begin
  assert(value is TFhirPatientContact);
  Inherited Insert(index, value);
end;

function TFhirPatientContactList.Item(index: Integer): TFhirPatientContact;
begin
  result := TFhirPatientContact(ObjectByIndex[index]);
end;

function TFhirPatientContactList.Link: TFhirPatientContactList;
begin
  result := TFhirPatientContactList(inherited Link);
end;

procedure TFhirPatientContactList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPatientContactList.SetItemByIndex(index: Integer; value: TFhirPatientContact);
begin
  assert(value is TFhirPatientContact);
  FhirPatientContacts[index] := value;
end;

procedure TFhirPatientContactList.SetItemN(index: Integer; value: TFhirPatientContact);
begin
  assert(value is TFhirPatientContact);
  ObjectByIndex[index] := value;
end;

{ TFhirPatientAnimal }

constructor TFhirPatientAnimal.Create;
begin
  inherited;
end;

destructor TFhirPatientAnimal.Destroy;
begin
  FSpecies.free;
  FBreed.free;
  FGenderStatus.free;
  inherited;
end;

procedure TFhirPatientAnimal.Assign(oSource : TFslObject);
begin
  inherited;
  species := TFhirPatientAnimal(oSource).species.Clone;
  breed := TFhirPatientAnimal(oSource).breed.Clone;
  genderStatus := TFhirPatientAnimal(oSource).genderStatus.Clone;
end;

procedure TFhirPatientAnimal.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'species') Then
     list.add(self.link, 'species', FSpecies.Link);
  if (child_name = 'breed') Then
     list.add(self.link, 'breed', FBreed.Link);
  if (child_name = 'genderStatus') Then
     list.add(self.link, 'genderStatus', FGenderStatus.Link);
end;

procedure TFhirPatientAnimal.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'species', 'CodeableConcept', false, TFhirCodeableConcept, FSpecies.Link));{2}
  oList.add(TFHIRProperty.create(self, 'breed', 'CodeableConcept', false, TFhirCodeableConcept, FBreed.Link));{2}
  oList.add(TFHIRProperty.create(self, 'genderStatus', 'CodeableConcept', false, TFhirCodeableConcept, FGenderStatus.Link));{2}
end;

function TFhirPatientAnimal.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'species') then
  begin
    Species := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'breed') then
  begin
    Breed := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'genderStatus') then
  begin
    GenderStatus := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirPatientAnimal.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirPatientAnimal.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'species') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'breed') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'genderStatus') then result := TFhirCodeableConcept.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPatientAnimal.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'species') then result := 'CodeableConcept'
  else if (propName = 'breed') then result := 'CodeableConcept'
  else if (propName = 'genderStatus') then result := 'CodeableConcept'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPatientAnimal.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'species') then SpeciesElement := nil
  else if (propName = 'breed') then BreedElement := nil
  else if (propName = 'genderStatus') then GenderStatusElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPatientAnimal.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'species') then SpeciesElement := new as TFhirCodeableConcept{4}
  else if (propName = 'breed') then BreedElement := new as TFhirCodeableConcept{4}
  else if (propName = 'genderStatus') then GenderStatusElement := new as TFhirCodeableConcept{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPatientAnimal.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirPatientAnimal.fhirType : string;
begin
  result := 'animal';
end;

function TFhirPatientAnimal.Link : TFhirPatientAnimal;
begin
  result := TFhirPatientAnimal(inherited Link);
end;

function TFhirPatientAnimal.Clone : TFhirPatientAnimal;
begin
  result := TFhirPatientAnimal(inherited Clone);
end;

function TFhirPatientAnimal.equals(other : TObject) : boolean;
var
  o : TFhirPatientAnimal;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPatientAnimal)) then
    result := false
  else
  begin
    o := TFhirPatientAnimal(other);
    result := compareDeep(speciesElement, o.speciesElement, true) and compareDeep(breedElement, o.breedElement, true) and
      compareDeep(genderStatusElement, o.genderStatusElement, true);
  end;
end;

function TFhirPatientAnimal.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FSpecies) and isEmptyProp(FBreed) and isEmptyProp(FGenderStatus);
end;

procedure TFhirPatientAnimal.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('species');
  fields.add('breed');
  fields.add('genderStatus');
end;

{ TFhirPatientAnimal }

Procedure TFhirPatientAnimal.SetSpecies(value : TFhirCodeableConcept);
begin
  FSpecies.free;
  FSpecies := value;
end;

Procedure TFhirPatientAnimal.SetBreed(value : TFhirCodeableConcept);
begin
  FBreed.free;
  FBreed := value;
end;

Procedure TFhirPatientAnimal.SetGenderStatus(value : TFhirCodeableConcept);
begin
  FGenderStatus.free;
  FGenderStatus := value;
end;

function TFhirPatientAnimal.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSpecies.sizeInBytes);
  inc(result, FBreed.sizeInBytes);
  inc(result, FGenderStatus.sizeInBytes);
end;

{ TFhirPatientAnimalListEnumerator }

Constructor TFhirPatientAnimalListEnumerator.Create(list : TFhirPatientAnimalList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPatientAnimalListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPatientAnimalListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPatientAnimalListEnumerator.GetCurrent : TFhirPatientAnimal;
begin
  Result := FList[FIndex];
end;

function TFhirPatientAnimalListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPatientAnimalList }
procedure TFhirPatientAnimalList.AddItem(value: TFhirPatientAnimal);
begin
  assert(value.ClassName = 'TFhirPatientAnimal', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPatientAnimal');
  add(value);
end;

function TFhirPatientAnimalList.Append: TFhirPatientAnimal;
begin
  result := TFhirPatientAnimal.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientAnimalList.ClearItems;
begin
  Clear;
end;

function TFhirPatientAnimalList.GetEnumerator : TFhirPatientAnimalListEnumerator;
begin
  result := TFhirPatientAnimalListEnumerator.Create(self.link);
end;

function TFhirPatientAnimalList.Clone: TFhirPatientAnimalList;
begin
  result := TFhirPatientAnimalList(inherited Clone);
end;

function TFhirPatientAnimalList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPatientAnimalList.GetItemN(index: Integer): TFhirPatientAnimal;
begin
  result := TFhirPatientAnimal(ObjectByIndex[index]);
end;

function TFhirPatientAnimalList.ItemClass: TFslObjectClass;
begin
  result := TFhirPatientAnimal;
end;
function TFhirPatientAnimalList.IndexOf(value: TFhirPatientAnimal): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPatientAnimalList.Insert(index: Integer): TFhirPatientAnimal;
begin
  result := TFhirPatientAnimal.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientAnimalList.InsertItem(index: Integer; value: TFhirPatientAnimal);
begin
  assert(value is TFhirPatientAnimal);
  Inherited Insert(index, value);
end;

function TFhirPatientAnimalList.Item(index: Integer): TFhirPatientAnimal;
begin
  result := TFhirPatientAnimal(ObjectByIndex[index]);
end;

function TFhirPatientAnimalList.Link: TFhirPatientAnimalList;
begin
  result := TFhirPatientAnimalList(inherited Link);
end;

procedure TFhirPatientAnimalList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPatientAnimalList.SetItemByIndex(index: Integer; value: TFhirPatientAnimal);
begin
  assert(value is TFhirPatientAnimal);
  FhirPatientAnimals[index] := value;
end;

procedure TFhirPatientAnimalList.SetItemN(index: Integer; value: TFhirPatientAnimal);
begin
  assert(value is TFhirPatientAnimal);
  ObjectByIndex[index] := value;
end;

{ TFhirPatientCommunication }

constructor TFhirPatientCommunication.Create;
begin
  inherited;
end;

destructor TFhirPatientCommunication.Destroy;
begin
  FLanguage.free;
  FPreferred.free;
  inherited;
end;

procedure TFhirPatientCommunication.Assign(oSource : TFslObject);
begin
  inherited;
  language := TFhirPatientCommunication(oSource).language.Clone;
  preferredElement := TFhirPatientCommunication(oSource).preferredElement.Clone;
end;

procedure TFhirPatientCommunication.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'language') Then
     list.add(self.link, 'language', FLanguage.Link);
  if (child_name = 'preferred') Then
     list.add(self.link, 'preferred', FPreferred.Link);
end;

procedure TFhirPatientCommunication.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'language', 'CodeableConcept', false, TFhirCodeableConcept, FLanguage.Link));{2}
  oList.add(TFHIRProperty.create(self, 'preferred', 'boolean', false, TFhirBoolean, FPreferred.Link));{2}
end;

function TFhirPatientCommunication.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'language') then
  begin
    Language := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'preferred') then
  begin
    PreferredElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirPatientCommunication.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirPatientCommunication.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'language') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'preferred') then result := TFhirBoolean.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPatientCommunication.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'language') then result := 'CodeableConcept'
  else if (propName = 'preferred') then result := 'boolean'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPatientCommunication.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'language') then LanguageElement := nil
  else if (propName = 'preferred') then PreferredElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPatientCommunication.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'language') then LanguageElement := new as TFhirCodeableConcept{4}
  else if (propName = 'preferred') then PreferredElement := asBoolean(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPatientCommunication.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirPatientCommunication.fhirType : string;
begin
  result := 'communication';
end;

function TFhirPatientCommunication.Link : TFhirPatientCommunication;
begin
  result := TFhirPatientCommunication(inherited Link);
end;

function TFhirPatientCommunication.Clone : TFhirPatientCommunication;
begin
  result := TFhirPatientCommunication(inherited Clone);
end;

function TFhirPatientCommunication.equals(other : TObject) : boolean;
var
  o : TFhirPatientCommunication;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPatientCommunication)) then
    result := false
  else
  begin
    o := TFhirPatientCommunication(other);
    result := compareDeep(languageElement, o.languageElement, true) and compareDeep(preferredElement, o.preferredElement, true);
  end;
end;

function TFhirPatientCommunication.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FLanguage) and isEmptyProp(FPreferred);
end;

procedure TFhirPatientCommunication.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('language');
  fields.add('preferred');
end;

{ TFhirPatientCommunication }

Procedure TFhirPatientCommunication.SetLanguage(value : TFhirCodeableConcept);
begin
  FLanguage.free;
  FLanguage := value;
end;

Procedure TFhirPatientCommunication.SetPreferred(value : TFhirBoolean);
begin
  FPreferred.free;
  FPreferred := value;
end;

Function TFhirPatientCommunication.GetPreferredST : Boolean;
begin
  if FPreferred = nil then
    result := false
  else
    result := FPreferred.value;
end;

Procedure TFhirPatientCommunication.SetPreferredST(value : Boolean);
begin
  if FPreferred = nil then
    FPreferred := TFhirBoolean.create;
  FPreferred.value := value
end;

function TFhirPatientCommunication.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLanguage.sizeInBytes);
  inc(result, FPreferred.sizeInBytes);
end;

{ TFhirPatientCommunicationListEnumerator }

Constructor TFhirPatientCommunicationListEnumerator.Create(list : TFhirPatientCommunicationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPatientCommunicationListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPatientCommunicationListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPatientCommunicationListEnumerator.GetCurrent : TFhirPatientCommunication;
begin
  Result := FList[FIndex];
end;

function TFhirPatientCommunicationListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPatientCommunicationList }
procedure TFhirPatientCommunicationList.AddItem(value: TFhirPatientCommunication);
begin
  assert(value.ClassName = 'TFhirPatientCommunication', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPatientCommunication');
  add(value);
end;

function TFhirPatientCommunicationList.Append: TFhirPatientCommunication;
begin
  result := TFhirPatientCommunication.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientCommunicationList.ClearItems;
begin
  Clear;
end;

function TFhirPatientCommunicationList.GetEnumerator : TFhirPatientCommunicationListEnumerator;
begin
  result := TFhirPatientCommunicationListEnumerator.Create(self.link);
end;

function TFhirPatientCommunicationList.Clone: TFhirPatientCommunicationList;
begin
  result := TFhirPatientCommunicationList(inherited Clone);
end;

function TFhirPatientCommunicationList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPatientCommunicationList.GetItemN(index: Integer): TFhirPatientCommunication;
begin
  result := TFhirPatientCommunication(ObjectByIndex[index]);
end;

function TFhirPatientCommunicationList.ItemClass: TFslObjectClass;
begin
  result := TFhirPatientCommunication;
end;
function TFhirPatientCommunicationList.IndexOf(value: TFhirPatientCommunication): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPatientCommunicationList.Insert(index: Integer): TFhirPatientCommunication;
begin
  result := TFhirPatientCommunication.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientCommunicationList.InsertItem(index: Integer; value: TFhirPatientCommunication);
begin
  assert(value is TFhirPatientCommunication);
  Inherited Insert(index, value);
end;

function TFhirPatientCommunicationList.Item(index: Integer): TFhirPatientCommunication;
begin
  result := TFhirPatientCommunication(ObjectByIndex[index]);
end;

function TFhirPatientCommunicationList.Link: TFhirPatientCommunicationList;
begin
  result := TFhirPatientCommunicationList(inherited Link);
end;

procedure TFhirPatientCommunicationList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPatientCommunicationList.SetItemByIndex(index: Integer; value: TFhirPatientCommunication);
begin
  assert(value is TFhirPatientCommunication);
  FhirPatientCommunications[index] := value;
end;

procedure TFhirPatientCommunicationList.SetItemN(index: Integer; value: TFhirPatientCommunication);
begin
  assert(value is TFhirPatientCommunication);
  ObjectByIndex[index] := value;
end;

{ TFhirPatientLink }

constructor TFhirPatientLink.Create;
begin
  inherited;
end;

destructor TFhirPatientLink.Destroy;
begin
  FOther.free;
  FType_.free;
  inherited;
end;

procedure TFhirPatientLink.Assign(oSource : TFslObject);
begin
  inherited;
  other := TFhirPatientLink(oSource).other.Clone;
  FType_ := TFhirPatientLink(oSource).FType_.Link;
end;

procedure TFhirPatientLink.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'other') Then
     list.add(self.link, 'other', FOther.Link);
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
end;

procedure TFhirPatientLink.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'other', 'Reference(Patient)', false, TFhirReference{TFhirPatient}, FOther.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'code', false, TFHIREnum, FType_.Link));{1}
end;

function TFhirPatientLink.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'other') then
  begin
    Other := propValue as TFhirReference{TFhirPatient}{4b};
    result := propValue;
  end
  else if (propName = 'type') then
  begin
    Type_Element := asEnum(SYSTEMS_TFhirLinkTypeEnum, CODES_TFhirLinkTypeEnum, propValue);
    result := propValue
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirPatientLink.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirPatientLink.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'other') then result := TFhirReference{TFhirPatient}.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPatientLink.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'other') then result := 'Reference'
  else if (propName = 'type') then result := 'code'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPatientLink.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'other') then OtherElement := nil
  else if (propName = 'type') then Type_Element := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPatientLink.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'other') then OtherElement := new as TFhirReference{TFhirPatient}{4}
  else if (propName = 'type') then Type_Element := asEnum(SYSTEMS_TFhirLinkTypeEnum, CODES_TFhirLinkTypeEnum, new){4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPatientLink.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirPatientLink.fhirType : string;
begin
  result := 'link';
end;

function TFhirPatientLink.Link : TFhirPatientLink;
begin
  result := TFhirPatientLink(inherited Link);
end;

function TFhirPatientLink.Clone : TFhirPatientLink;
begin
  result := TFhirPatientLink(inherited Clone);
end;

function TFhirPatientLink.equals(other : TObject) : boolean;
var
  o : TFhirPatientLink;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPatientLink)) then
    result := false
  else
  begin
    o := TFhirPatientLink(other);
    result := compareDeep(otherElement, o.otherElement, true) and compareDeep(type_Element, o.type_Element, true);
  end;
end;

function TFhirPatientLink.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FOther) and isEmptyProp(FType_);
end;

procedure TFhirPatientLink.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('other');
  fields.add('type');
end;

{ TFhirPatientLink }

Procedure TFhirPatientLink.SetOther(value : TFhirReference{TFhirPatient});
begin
  FOther.free;
  FOther := value;
end;

Procedure TFhirPatientLink.SetType_(value : TFhirEnum);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirPatientLink.GetType_ST : TFhirLinkTypeEnum;
begin
  if FType_ = nil then
    result := TFhirLinkTypeEnum(0)
  else
    result := TFhirLinkTypeEnum(StringArrayIndexOfSensitive(CODES_TFhirLinkTypeEnum, FType_.value));
end;

Procedure TFhirPatientLink.SetType_ST(value : TFhirLinkTypeEnum);
begin
  if ord(value) = 0 then
    Type_Element := nil
  else
    Type_Element := TFhirEnum.create(SYSTEMS_TFhirLinkTypeEnum[value], CODES_TFhirLinkTypeEnum[value]);
end;

function TFhirPatientLink.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOther.sizeInBytes);
  inc(result, FType_.sizeInBytes);
end;

{ TFhirPatientLinkListEnumerator }

Constructor TFhirPatientLinkListEnumerator.Create(list : TFhirPatientLinkList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPatientLinkListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPatientLinkListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPatientLinkListEnumerator.GetCurrent : TFhirPatientLink;
begin
  Result := FList[FIndex];
end;

function TFhirPatientLinkListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPatientLinkList }
procedure TFhirPatientLinkList.AddItem(value: TFhirPatientLink);
begin
  assert(value.ClassName = 'TFhirPatientLink', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPatientLink');
  add(value);
end;

function TFhirPatientLinkList.Append: TFhirPatientLink;
begin
  result := TFhirPatientLink.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientLinkList.ClearItems;
begin
  Clear;
end;

function TFhirPatientLinkList.GetEnumerator : TFhirPatientLinkListEnumerator;
begin
  result := TFhirPatientLinkListEnumerator.Create(self.link);
end;

function TFhirPatientLinkList.Clone: TFhirPatientLinkList;
begin
  result := TFhirPatientLinkList(inherited Clone);
end;

function TFhirPatientLinkList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPatientLinkList.GetItemN(index: Integer): TFhirPatientLink;
begin
  result := TFhirPatientLink(ObjectByIndex[index]);
end;

function TFhirPatientLinkList.ItemClass: TFslObjectClass;
begin
  result := TFhirPatientLink;
end;
function TFhirPatientLinkList.IndexOf(value: TFhirPatientLink): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPatientLinkList.Insert(index: Integer): TFhirPatientLink;
begin
  result := TFhirPatientLink.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientLinkList.InsertItem(index: Integer; value: TFhirPatientLink);
begin
  assert(value is TFhirPatientLink);
  Inherited Insert(index, value);
end;

function TFhirPatientLinkList.Item(index: Integer): TFhirPatientLink;
begin
  result := TFhirPatientLink(ObjectByIndex[index]);
end;

function TFhirPatientLinkList.Link: TFhirPatientLinkList;
begin
  result := TFhirPatientLinkList(inherited Link);
end;

procedure TFhirPatientLinkList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPatientLinkList.SetItemByIndex(index: Integer; value: TFhirPatientLink);
begin
  assert(value is TFhirPatientLink);
  FhirPatientLinks[index] := value;
end;

procedure TFhirPatientLinkList.SetItemN(index: Integer; value: TFhirPatientLink);
begin
  assert(value is TFhirPatientLink);
  ObjectByIndex[index] := value;
end;

{ TFhirPatient }

constructor TFhirPatient.Create;
begin
  inherited;
end;

destructor TFhirPatient.Destroy;
begin
  FIdentifierList.Free;
  FActive.free;
  FNameList.Free;
  FTelecomList.Free;
  FGender.free;
  FBirthDate.free;
  FDeceased.free;
  FAddressList.Free;
  FMaritalStatus.free;
  FMultipleBirth.free;
  FPhotoList.Free;
  FContactList.Free;
  FAnimal.free;
  FCommunicationList.Free;
  FCareProviderList.Free;
  FManagingOrganization.free;
  FLink_List.Free;
  inherited;
end;

function TFhirPatient.GetResourceType : TFhirResourceType;
begin
  result := frtPatient;
end;

procedure TFhirPatient.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirPatient(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirPatient(oSource).FIdentifierList);
  end;
  activeElement := TFhirPatient(oSource).activeElement.Clone;
  if (TFhirPatient(oSource).FNameList = nil) then
  begin
    FNameList.free;
    FNameList := nil;
  end
  else
  begin
    if FNameList = nil then
      FNameList := TFhirHumanNameList.Create;
    FNameList.Assign(TFhirPatient(oSource).FNameList);
  end;
  if (TFhirPatient(oSource).FTelecomList = nil) then
  begin
    FTelecomList.free;
    FTelecomList := nil;
  end
  else
  begin
    if FTelecomList = nil then
      FTelecomList := TFhirContactPointList.Create;
    FTelecomList.Assign(TFhirPatient(oSource).FTelecomList);
  end;
  FGender := TFhirPatient(oSource).FGender.Link;
  birthDateElement := TFhirPatient(oSource).birthDateElement.Clone;
  deceased := TFhirPatient(oSource).deceased.Clone;
  if (TFhirPatient(oSource).FAddressList = nil) then
  begin
    FAddressList.free;
    FAddressList := nil;
  end
  else
  begin
    if FAddressList = nil then
      FAddressList := TFhirAddressList.Create;
    FAddressList.Assign(TFhirPatient(oSource).FAddressList);
  end;
  maritalStatus := TFhirPatient(oSource).maritalStatus.Clone;
  multipleBirth := TFhirPatient(oSource).multipleBirth.Clone;
  if (TFhirPatient(oSource).FPhotoList = nil) then
  begin
    FPhotoList.free;
    FPhotoList := nil;
  end
  else
  begin
    if FPhotoList = nil then
      FPhotoList := TFhirAttachmentList.Create;
    FPhotoList.Assign(TFhirPatient(oSource).FPhotoList);
  end;
  if (TFhirPatient(oSource).FContactList = nil) then
  begin
    FContactList.free;
    FContactList := nil;
  end
  else
  begin
    if FContactList = nil then
      FContactList := TFhirPatientContactList.Create;
    FContactList.Assign(TFhirPatient(oSource).FContactList);
  end;
  animal := TFhirPatient(oSource).animal.Clone;
  if (TFhirPatient(oSource).FCommunicationList = nil) then
  begin
    FCommunicationList.free;
    FCommunicationList := nil;
  end
  else
  begin
    if FCommunicationList = nil then
      FCommunicationList := TFhirPatientCommunicationList.Create;
    FCommunicationList.Assign(TFhirPatient(oSource).FCommunicationList);
  end;
  if (TFhirPatient(oSource).FCareProviderList = nil) then
  begin
    FCareProviderList.free;
    FCareProviderList := nil;
  end
  else
  begin
    if FCareProviderList = nil then
      FCareProviderList := TFhirReferenceList{Resource}.Create;
    FCareProviderList.Assign(TFhirPatient(oSource).FCareProviderList);
  end;
  managingOrganization := TFhirPatient(oSource).managingOrganization.Clone;
  if (TFhirPatient(oSource).FLink_List = nil) then
  begin
    FLink_List.free;
    FLink_List := nil;
  end
  else
  begin
    if FLink_List = nil then
      FLink_List := TFhirPatientLinkList.Create;
    FLink_List.Assign(TFhirPatient(oSource).FLink_List);
  end;
end;

procedure TFhirPatient.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'active') Then
     list.add(self.link, 'active', FActive.Link);
  if (child_name = 'name') Then
    list.addAll(self, 'name', FNameList);
  if (child_name = 'telecom') Then
    list.addAll(self, 'telecom', FTelecomList);
  if (child_name = 'gender') Then
     list.add(self.link, 'gender', FGender.Link);
  if (child_name = 'birthDate') Then
     list.add(self.link, 'birthDate', FBirthDate.Link);
  if (child_name = 'deceased[x]') or (child_name = 'deceased') Then
     list.add(self.link, 'deceased[x]', FDeceased.Link);
  if (child_name = 'address') Then
    list.addAll(self, 'address', FAddressList);
  if (child_name = 'maritalStatus') Then
     list.add(self.link, 'maritalStatus', FMaritalStatus.Link);
  if (child_name = 'multipleBirth[x]') or (child_name = 'multipleBirth') Then
     list.add(self.link, 'multipleBirth[x]', FMultipleBirth.Link);
  if (child_name = 'photo') Then
    list.addAll(self, 'photo', FPhotoList);
  if (child_name = 'contact') Then
    list.addAll(self, 'contact', FContactList);
  if (child_name = 'animal') Then
     list.add(self.link, 'animal', FAnimal.Link);
  if (child_name = 'communication') Then
    list.addAll(self, 'communication', FCommunicationList);
  if (child_name = 'careProvider') Then
    list.addAll(self, 'careProvider', FCareProviderList);
  if (child_name = 'managingOrganization') Then
     list.add(self.link, 'managingOrganization', FManagingOrganization.Link);
  if (child_name = 'link') Then
    list.addAll(self, 'link', FLink_List);
end;

procedure TFhirPatient.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'active', 'boolean', false, TFhirBoolean, FActive.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', true, TFhirHumanName, FNameList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'telecom', 'ContactPoint', true, TFhirContactPoint, FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'gender', 'code', false, TFHIREnum, FGender.Link));{1}
  oList.add(TFHIRProperty.create(self, 'birthDate', 'date', false, TFhirDate, FBirthDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'deceased[x]', 'boolean|dateTime', false, TFhirType, FDeceased.Link));{2}
  oList.add(TFHIRProperty.create(self, 'address', 'Address', true, TFhirAddress, FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'maritalStatus', 'CodeableConcept', false, TFhirCodeableConcept, FMaritalStatus.Link));{2}
  oList.add(TFHIRProperty.create(self, 'multipleBirth[x]', 'boolean|integer', false, TFhirType, FMultipleBirth.Link));{2}
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', true, TFhirAttachment, FPhotoList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'contact', '', true, TFhirPatientContact, FContactList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'animal', '', false, TFhirPatientAnimal, FAnimal.Link));{2}
  oList.add(TFHIRProperty.create(self, 'communication', '', true, TFhirPatientCommunication, FCommunicationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'careProvider', 'Reference(Organization|Practitioner)', true, TFhirReference{Resource}, FCareProviderList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'managingOrganization', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FManagingOrganization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'link', '', true, TFhirPatientLink, FLink_List.Link)){3};
end;

function TFhirPatient.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'active') then
  begin
    ActiveElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'name') then
  begin
    NameList.add(propValue as TFhirHumanName){2a};
    result := propValue;
  end
  else if (propName = 'telecom') then
  begin
    TelecomList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'gender') then
  begin
    GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, propValue);
    result := propValue
  end
  else if (propName = 'birthDate') then
  begin
    BirthDateElement := asDate(propValue){5a};
    result := propValue;
  end
  else if (isMatchingName(propName, 'deceased', ['Boolean', 'DateTime'])) then
  begin
    Deceased := propValue as TFhirType{4};
    result := propValue;
  end
  else if (propName = 'address') then
  begin
    AddressList.add(propValue as TFhirAddress){2a};
    result := propValue;
  end
  else if (propName = 'maritalStatus') then
  begin
    MaritalStatus := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (isMatchingName(propName, 'multipleBirth', ['Boolean', 'Integer'])) then
  begin
    MultipleBirth := propValue as TFhirType{4};
    result := propValue;
  end
  else if (propName = 'photo') then
  begin
    PhotoList.add(propValue as TFhirAttachment){2a};
    result := propValue;
  end
  else if (propName = 'contact') then
  begin
    ContactList.add(propValue as TFhirPatientContact){2a};
    result := propValue;
  end
  else if (propName = 'animal') then
  begin
    Animal := propValue as TFhirPatientAnimal{4b};
    result := propValue;
  end
  else if (propName = 'communication') then
  begin
    CommunicationList.add(propValue as TFhirPatientCommunication){2a};
    result := propValue;
  end
  else if (propName = 'careProvider') then
  begin
    CareProviderList.add(propValue as TFhirReference{Resource}){2a};
    result := propValue;
  end
  else if (propName = 'managingOrganization') then
  begin
    ManagingOrganization := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'link') then
  begin
    Link_List.add(propValue as TFhirPatientLink){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirPatient.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'name') then NameList.insertItem(index, propValue as TFhirHumanName){2a}
  else if (propName = 'telecom') then TelecomList.insertItem(index, propValue as TFhirContactPoint){2a}
  else if (propName = 'address') then AddressList.insertItem(index, propValue as TFhirAddress){2a}
  else if (propName = 'photo') then PhotoList.insertItem(index, propValue as TFhirAttachment){2a}
  else if (propName = 'contact') then ContactList.insertItem(index, propValue as TFhirPatientContact){2a}
  else if (propName = 'communication') then CommunicationList.insertItem(index, propValue as TFhirPatientCommunication){2a}
  else if (propName = 'careProvider') then CareProviderList.insertItem(index, propValue as TFhirReference{Resource}){2a}
  else if (propName = 'link') then Link_List.insertItem(index, propValue as TFhirPatientLink){2a}
  else inherited;
end;

function TFhirPatient.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'active') then result := TFhirBoolean.create() {5b}
  else if (propName = 'name') then result := NameList.new(){2}
  else if (propName = 'telecom') then result := TelecomList.new(){2}
  else if (propName = 'birthDate') then result := TFhirDate.create() {5b}
  else if (isMatchingName(propName, 'deceased', ['Boolean', 'DateTime'])) then raise EFHIRException.create('Cannot make property Deceased'){4x}
  else if (propName = 'address') then result := AddressList.new(){2}
  else if (propName = 'maritalStatus') then result := TFhirCodeableConcept.create(){4b}
  else if (isMatchingName(propName, 'multipleBirth', ['Boolean', 'Integer'])) then raise EFHIRException.create('Cannot make property MultipleBirth'){4x}
  else if (propName = 'photo') then result := PhotoList.new(){2}
  else if (propName = 'contact') then result := ContactList.new(){2}
  else if (propName = 'animal') then result := TFhirPatientAnimal.create(){4b}
  else if (propName = 'communication') then result := CommunicationList.new(){2}
  else if (propName = 'careProvider') then result := CareProviderList.new(){2}
  else if (propName = 'managingOrganization') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'link') then result := Link_List.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPatient.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'active') then result := 'boolean'
  else if (propName = 'name') then result := 'HumanName'
  else if (propName = 'telecom') then result := 'ContactPoint'
  else if (propName = 'gender') then result := 'code'
  else if (propName = 'birthDate') then result := 'date'
  else if (propName = 'deceased[x]') then result := 'boolean|dateTime'
  else if (propName = 'address') then result := 'Address'
  else if (propName = 'maritalStatus') then result := 'CodeableConcept'
  else if (propName = 'multipleBirth[x]') then result := 'boolean|integer'
  else if (propName = 'photo') then result := 'Attachment'
  else if (propName = 'contact') then result := ''
  else if (propName = 'animal') then result := ''
  else if (propName = 'communication') then result := ''
  else if (propName = 'careProvider') then result := 'Reference'
  else if (propName = 'managingOrganization') then result := 'Reference'
  else if (propName = 'link') then result := ''
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPatient.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'active') then ActiveElement := nil
  else if (propName = 'name') then deletePropertyValue('name', NameList, value) {2}
  else if (propName = 'telecom') then deletePropertyValue('telecom', TelecomList, value) {2}
  else if (propName = 'gender') then GenderElement := nil
  else if (propName = 'birthDate') then BirthDateElement := nil
  else if (isMatchingName(propName, 'deceased', ['Boolean', 'DateTime'])) then DeceasedElement := nil{4x}
  else if (propName = 'address') then deletePropertyValue('address', AddressList, value) {2}
  else if (propName = 'maritalStatus') then MaritalStatusElement := nil
  else if (isMatchingName(propName, 'multipleBirth', ['Boolean', 'Integer'])) then MultipleBirthElement := nil{4x}
  else if (propName = 'photo') then deletePropertyValue('photo', PhotoList, value) {2}
  else if (propName = 'contact') then deletePropertyValue('contact', ContactList, value) {2}
  else if (propName = 'animal') then AnimalElement := nil
  else if (propName = 'communication') then deletePropertyValue('communication', CommunicationList, value) {2}
  else if (propName = 'careProvider') then deletePropertyValue('careProvider', CareProviderList, value) {2}
  else if (propName = 'managingOrganization') then ManagingOrganizationElement := nil
  else if (propName = 'link') then deletePropertyValue('link', Link_List, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPatient.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'active') then ActiveElement := asBoolean(new){5b}
  else if (propName = 'name') then replacePropertyValue('name', NameList, existing, new) {2}
  else if (propName = 'telecom') then replacePropertyValue('telecom', TelecomList, existing, new) {2}
  else if (propName = 'gender') then GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, new){4}
  else if (propName = 'birthDate') then BirthDateElement := asDate(new){5b}
  else if (isMatchingName(propName, 'deceased', ['Boolean', 'DateTime'])) then DeceasedElement := new as TFhirType{4x}
  else if (propName = 'address') then replacePropertyValue('address', AddressList, existing, new) {2}
  else if (propName = 'maritalStatus') then MaritalStatusElement := new as TFhirCodeableConcept{4}
  else if (isMatchingName(propName, 'multipleBirth', ['Boolean', 'Integer'])) then MultipleBirthElement := new as TFhirType{4x}
  else if (propName = 'photo') then replacePropertyValue('photo', PhotoList, existing, new) {2}
  else if (propName = 'contact') then replacePropertyValue('contact', ContactList, existing, new) {2}
  else if (propName = 'animal') then AnimalElement := new as TFhirPatientAnimal{4}
  else if (propName = 'communication') then replacePropertyValue('communication', CommunicationList, existing, new) {2}
  else if (propName = 'careProvider') then replacePropertyValue('careProvider', CareProviderList, existing, new) {2}
  else if (propName = 'managingOrganization') then ManagingOrganizationElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'link') then replacePropertyValue('link', Link_List, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPatient.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'name') then NameList.move(source, destination){2a}
  else if (propName = 'telecom') then TelecomList.move(source, destination){2a}
  else if (propName = 'address') then AddressList.move(source, destination){2a}
  else if (propName = 'photo') then PhotoList.move(source, destination){2a}
  else if (propName = 'contact') then ContactList.move(source, destination){2a}
  else if (propName = 'communication') then CommunicationList.move(source, destination){2a}
  else if (propName = 'careProvider') then CareProviderList.move(source, destination){2a}
  else if (propName = 'link') then Link_List.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirPatient.fhirType : string;
begin
  result := 'Patient';
end;

function TFhirPatient.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FActive) and isEmptyProp(FnameList) and isEmptyProp(FtelecomList) and isEmptyProp(FGender) and isEmptyProp(FBirthDate) and isEmptyProp(FDeceased) and isEmptyProp(FaddressList) and isEmptyProp(FMaritalStatus) and isEmptyProp(FMultipleBirth) and isEmptyProp(FphotoList) and isEmptyProp(FcontactList) and isEmptyProp(FAnimal) and isEmptyProp(FcommunicationList) and isEmptyProp(FcareProviderList) and isEmptyProp(FManagingOrganization) and isEmptyProp(Flink_List);
end;

function TFhirPatient.equals(other : TObject) : boolean;
var
  o : TFhirPatient;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPatient)) then
    result := false
  else
  begin
    o := TFhirPatient(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(activeElement, o.activeElement, true) and
      compareDeep(nameList, o.nameList, true) and compareDeep(telecomList, o.telecomList, true) and
      compareDeep(genderElement, o.genderElement, true) and compareDeep(birthDateElement, o.birthDateElement, true) and
      compareDeep(deceasedElement, o.deceasedElement, true) and compareDeep(addressList, o.addressList, true) and
      compareDeep(maritalStatusElement, o.maritalStatusElement, true) and compareDeep(multipleBirthElement, o.multipleBirthElement, true) and
      compareDeep(photoList, o.photoList, true) and compareDeep(contactList, o.contactList, true) and
      compareDeep(animalElement, o.animalElement, true) and compareDeep(communicationList, o.communicationList, true) and
      compareDeep(careProviderList, o.careProviderList, true) and compareDeep(managingOrganizationElement, o.managingOrganizationElement, true) and
      compareDeep(link_List, o.link_List, true);
  end;
end;

function TFhirPatient.Link : TFhirPatient;
begin
  result := TFhirPatient(inherited Link);
end;

function TFhirPatient.Clone : TFhirPatient;
begin
  result := TFhirPatient(inherited Clone);
end;

procedure TFhirPatient.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('active');
  fields.add('name');
  fields.add('telecom');
  fields.add('gender');
  fields.add('birthDate');
  fields.add('deceased[x]');
  fields.add('address');
  fields.add('maritalStatus');
  fields.add('multipleBirth[x]');
  fields.add('photo');
  fields.add('contact');
  fields.add('animal');
  fields.add('communication');
  fields.add('careProvider');
  fields.add('managingOrganization');
  fields.add('link');
end;

{ TFhirPatient }

Function TFhirPatient.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirPatient.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirPatient.SetActive(value : TFhirBoolean);
begin
  FActive.free;
  FActive := value;
end;

Function TFhirPatient.GetActiveST : Boolean;
begin
  if FActive = nil then
    result := false
  else
    result := FActive.value;
end;

Procedure TFhirPatient.SetActiveST(value : Boolean);
begin
  if FActive = nil then
    FActive := TFhirBoolean.create;
  FActive.value := value
end;

Function TFhirPatient.GetNameList : TFhirHumanNameList;
begin
  if FNameList = nil then
    FNameList := TFhirHumanNameList.Create;
  result := FNameList;
end;

Function TFhirPatient.GetHasNameList : boolean;
begin
  result := (FNameList <> nil) and (FNameList.count > 0);
end;

Function TFhirPatient.GetTelecomList : TFhirContactPointList;
begin
  if FTelecomList = nil then
    FTelecomList := TFhirContactPointList.Create;
  result := FTelecomList;
end;

Function TFhirPatient.GetHasTelecomList : boolean;
begin
  result := (FTelecomList <> nil) and (FTelecomList.count > 0);
end;

Procedure TFhirPatient.SetGender(value : TFhirEnum);
begin
  FGender.free;
  FGender := value;
end;

Function TFhirPatient.GetGenderST : TFhirAdministrativeGenderEnum;
begin
  if FGender = nil then
    result := TFhirAdministrativeGenderEnum(0)
  else
    result := TFhirAdministrativeGenderEnum(StringArrayIndexOfSensitive(CODES_TFhirAdministrativeGenderEnum, FGender.value));
end;

Procedure TFhirPatient.SetGenderST(value : TFhirAdministrativeGenderEnum);
begin
  if ord(value) = 0 then
    GenderElement := nil
  else
    GenderElement := TFhirEnum.create(SYSTEMS_TFhirAdministrativeGenderEnum[value], CODES_TFhirAdministrativeGenderEnum[value]);
end;

Procedure TFhirPatient.SetBirthDate(value : TFhirDate);
begin
  FBirthDate.free;
  FBirthDate := value;
end;

Function TFhirPatient.GetBirthDateST : TFslDateTime;
begin
  if FBirthDate = nil then
    result := TFslDateTime.makeNull
  else
    result := FBirthDate.value;
end;

Procedure TFhirPatient.SetBirthDateST(value : TFslDateTime);
begin
  if FBirthDate = nil then
    FBirthDate := TFhirDate.create;
  FBirthDate.value := value
end;

Procedure TFhirPatient.SetDeceased(value : TFhirType);
begin
  FDeceased.free;
  FDeceased := value;
end;

Function TFhirPatient.GetAddressList : TFhirAddressList;
begin
  if FAddressList = nil then
    FAddressList := TFhirAddressList.Create;
  result := FAddressList;
end;

Function TFhirPatient.GetHasAddressList : boolean;
begin
  result := (FAddressList <> nil) and (FAddressList.count > 0);
end;

Procedure TFhirPatient.SetMaritalStatus(value : TFhirCodeableConcept);
begin
  FMaritalStatus.free;
  FMaritalStatus := value;
end;

Procedure TFhirPatient.SetMultipleBirth(value : TFhirType);
begin
  FMultipleBirth.free;
  FMultipleBirth := value;
end;

Function TFhirPatient.GetPhotoList : TFhirAttachmentList;
begin
  if FPhotoList = nil then
    FPhotoList := TFhirAttachmentList.Create;
  result := FPhotoList;
end;

Function TFhirPatient.GetHasPhotoList : boolean;
begin
  result := (FPhotoList <> nil) and (FPhotoList.count > 0);
end;

Function TFhirPatient.GetContactList : TFhirPatientContactList;
begin
  if FContactList = nil then
    FContactList := TFhirPatientContactList.Create;
  result := FContactList;
end;

Function TFhirPatient.GetHasContactList : boolean;
begin
  result := (FContactList <> nil) and (FContactList.count > 0);
end;

Procedure TFhirPatient.SetAnimal(value : TFhirPatientAnimal);
begin
  FAnimal.free;
  FAnimal := value;
end;

Function TFhirPatient.GetCommunicationList : TFhirPatientCommunicationList;
begin
  if FCommunicationList = nil then
    FCommunicationList := TFhirPatientCommunicationList.Create;
  result := FCommunicationList;
end;

Function TFhirPatient.GetHasCommunicationList : boolean;
begin
  result := (FCommunicationList <> nil) and (FCommunicationList.count > 0);
end;

Function TFhirPatient.GetCareProviderList : TFhirReferenceList{Resource};
begin
  if FCareProviderList = nil then
    FCareProviderList := TFhirReferenceList{Resource}.Create;
  result := FCareProviderList;
end;

Function TFhirPatient.GetHasCareProviderList : boolean;
begin
  result := (FCareProviderList <> nil) and (FCareProviderList.count > 0);
end;

Procedure TFhirPatient.SetManagingOrganization(value : TFhirReference{TFhirOrganization});
begin
  FManagingOrganization.free;
  FManagingOrganization := value;
end;

Function TFhirPatient.GetLink_List : TFhirPatientLinkList;
begin
  if FLink_List = nil then
    FLink_List := TFhirPatientLinkList.Create;
  result := FLink_List;
end;

Function TFhirPatient.GetHasLink_List : boolean;
begin
  result := (FLink_List <> nil) and (FLink_List.count > 0);
end;

function TFhirPatient.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FActive.sizeInBytes);
  inc(result, FnameList.sizeInBytes);
  inc(result, FtelecomList.sizeInBytes);
  inc(result, FGender.sizeInBytes);
  inc(result, FBirthDate.sizeInBytes);
  inc(result, FDeceased.sizeInBytes);
  inc(result, FaddressList.sizeInBytes);
  inc(result, FMaritalStatus.sizeInBytes);
  inc(result, FMultipleBirth.sizeInBytes);
  inc(result, FphotoList.sizeInBytes);
  inc(result, FcontactList.sizeInBytes);
  inc(result, FAnimal.sizeInBytes);
  inc(result, FcommunicationList.sizeInBytes);
  inc(result, FcareProviderList.sizeInBytes);
  inc(result, FManagingOrganization.sizeInBytes);
  inc(result, Flink_List.sizeInBytes);
end;

{ TFhirPatientListEnumerator }

Constructor TFhirPatientListEnumerator.Create(list : TFhirPatientList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPatientListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPatientListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPatientListEnumerator.GetCurrent : TFhirPatient;
begin
  Result := FList[FIndex];
end;

function TFhirPatientListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPatientList }
procedure TFhirPatientList.AddItem(value: TFhirPatient);
begin
  assert(value.ClassName = 'TFhirPatient', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPatient');
  add(value);
end;

function TFhirPatientList.Append: TFhirPatient;
begin
  result := TFhirPatient.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientList.ClearItems;
begin
  Clear;
end;

function TFhirPatientList.GetEnumerator : TFhirPatientListEnumerator;
begin
  result := TFhirPatientListEnumerator.Create(self.link);
end;

function TFhirPatientList.Clone: TFhirPatientList;
begin
  result := TFhirPatientList(inherited Clone);
end;

function TFhirPatientList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPatientList.GetItemN(index: Integer): TFhirPatient;
begin
  result := TFhirPatient(ObjectByIndex[index]);
end;

function TFhirPatientList.ItemClass: TFslObjectClass;
begin
  result := TFhirPatient;
end;
function TFhirPatientList.IndexOf(value: TFhirPatient): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPatientList.Insert(index: Integer): TFhirPatient;
begin
  result := TFhirPatient.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPatientList.InsertItem(index: Integer; value: TFhirPatient);
begin
  assert(value is TFhirPatient);
  Inherited Insert(index, value);
end;

function TFhirPatientList.Item(index: Integer): TFhirPatient;
begin
  result := TFhirPatient(ObjectByIndex[index]);
end;

function TFhirPatientList.Link: TFhirPatientList;
begin
  result := TFhirPatientList(inherited Link);
end;

procedure TFhirPatientList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPatientList.SetItemByIndex(index: Integer; value: TFhirPatient);
begin
  assert(value is TFhirPatient);
  FhirPatients[index] := value;
end;

procedure TFhirPatientList.SetItemN(index: Integer; value: TFhirPatient);
begin
  assert(value is TFhirPatient);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_PATIENT}

{$IFDEF FHIR_PERSON}

{ TFhirPersonLink }

constructor TFhirPersonLink.Create;
begin
  inherited;
end;

destructor TFhirPersonLink.Destroy;
begin
  FTarget.free;
  FAssurance.free;
  inherited;
end;

procedure TFhirPersonLink.Assign(oSource : TFslObject);
begin
  inherited;
  target := TFhirPersonLink(oSource).target.Clone;
  FAssurance := TFhirPersonLink(oSource).FAssurance.Link;
end;

procedure TFhirPersonLink.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'target') Then
     list.add(self.link, 'target', FTarget.Link);
  if (child_name = 'assurance') Then
     list.add(self.link, 'assurance', FAssurance.Link);
end;

procedure TFhirPersonLink.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'target', 'Reference(Patient|Practitioner|RelatedPerson|Person)', false, TFhirReference{Resource}, FTarget.Link));{2}
  oList.add(TFHIRProperty.create(self, 'assurance', 'code', false, TFHIREnum, FAssurance.Link));{1}
end;

function TFhirPersonLink.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'target') then
  begin
    Target := propValue as TFhirReference{Resource}{4b};
    result := propValue;
  end
  else if (propName = 'assurance') then
  begin
    AssuranceElement := asEnum(SYSTEMS_TFhirIdentityAssuranceLevelEnum, CODES_TFhirIdentityAssuranceLevelEnum, propValue);
    result := propValue
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirPersonLink.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirPersonLink.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'target') then result := TFhirReference{Resource}.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPersonLink.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'target') then result := 'Reference'
  else if (propName = 'assurance') then result := 'code'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPersonLink.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'target') then TargetElement := nil
  else if (propName = 'assurance') then AssuranceElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPersonLink.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'target') then TargetElement := new as TFhirReference{Resource}{4}
  else if (propName = 'assurance') then AssuranceElement := asEnum(SYSTEMS_TFhirIdentityAssuranceLevelEnum, CODES_TFhirIdentityAssuranceLevelEnum, new){4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPersonLink.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirPersonLink.fhirType : string;
begin
  result := 'link';
end;

function TFhirPersonLink.Link : TFhirPersonLink;
begin
  result := TFhirPersonLink(inherited Link);
end;

function TFhirPersonLink.Clone : TFhirPersonLink;
begin
  result := TFhirPersonLink(inherited Clone);
end;

function TFhirPersonLink.equals(other : TObject) : boolean;
var
  o : TFhirPersonLink;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPersonLink)) then
    result := false
  else
  begin
    o := TFhirPersonLink(other);
    result := compareDeep(targetElement, o.targetElement, true) and compareDeep(assuranceElement, o.assuranceElement, true);
  end;
end;

function TFhirPersonLink.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FTarget) and isEmptyProp(FAssurance);
end;

procedure TFhirPersonLink.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('target');
  fields.add('assurance');
end;

{ TFhirPersonLink }

Procedure TFhirPersonLink.SetTarget(value : TFhirReference{Resource});
begin
  FTarget.free;
  FTarget := value;
end;

Procedure TFhirPersonLink.SetAssurance(value : TFhirEnum);
begin
  FAssurance.free;
  FAssurance := value;
end;

Function TFhirPersonLink.GetAssuranceST : TFhirIdentityAssuranceLevelEnum;
begin
  if FAssurance = nil then
    result := TFhirIdentityAssuranceLevelEnum(0)
  else
    result := TFhirIdentityAssuranceLevelEnum(StringArrayIndexOfSensitive(CODES_TFhirIdentityAssuranceLevelEnum, FAssurance.value));
end;

Procedure TFhirPersonLink.SetAssuranceST(value : TFhirIdentityAssuranceLevelEnum);
begin
  if ord(value) = 0 then
    AssuranceElement := nil
  else
    AssuranceElement := TFhirEnum.create(SYSTEMS_TFhirIdentityAssuranceLevelEnum[value], CODES_TFhirIdentityAssuranceLevelEnum[value]);
end;

function TFhirPersonLink.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTarget.sizeInBytes);
  inc(result, FAssurance.sizeInBytes);
end;

{ TFhirPersonLinkListEnumerator }

Constructor TFhirPersonLinkListEnumerator.Create(list : TFhirPersonLinkList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPersonLinkListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPersonLinkListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPersonLinkListEnumerator.GetCurrent : TFhirPersonLink;
begin
  Result := FList[FIndex];
end;

function TFhirPersonLinkListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPersonLinkList }
procedure TFhirPersonLinkList.AddItem(value: TFhirPersonLink);
begin
  assert(value.ClassName = 'TFhirPersonLink', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPersonLink');
  add(value);
end;

function TFhirPersonLinkList.Append: TFhirPersonLink;
begin
  result := TFhirPersonLink.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPersonLinkList.ClearItems;
begin
  Clear;
end;

function TFhirPersonLinkList.GetEnumerator : TFhirPersonLinkListEnumerator;
begin
  result := TFhirPersonLinkListEnumerator.Create(self.link);
end;

function TFhirPersonLinkList.Clone: TFhirPersonLinkList;
begin
  result := TFhirPersonLinkList(inherited Clone);
end;

function TFhirPersonLinkList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPersonLinkList.GetItemN(index: Integer): TFhirPersonLink;
begin
  result := TFhirPersonLink(ObjectByIndex[index]);
end;

function TFhirPersonLinkList.ItemClass: TFslObjectClass;
begin
  result := TFhirPersonLink;
end;
function TFhirPersonLinkList.IndexOf(value: TFhirPersonLink): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPersonLinkList.Insert(index: Integer): TFhirPersonLink;
begin
  result := TFhirPersonLink.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPersonLinkList.InsertItem(index: Integer; value: TFhirPersonLink);
begin
  assert(value is TFhirPersonLink);
  Inherited Insert(index, value);
end;

function TFhirPersonLinkList.Item(index: Integer): TFhirPersonLink;
begin
  result := TFhirPersonLink(ObjectByIndex[index]);
end;

function TFhirPersonLinkList.Link: TFhirPersonLinkList;
begin
  result := TFhirPersonLinkList(inherited Link);
end;

procedure TFhirPersonLinkList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPersonLinkList.SetItemByIndex(index: Integer; value: TFhirPersonLink);
begin
  assert(value is TFhirPersonLink);
  FhirPersonLinks[index] := value;
end;

procedure TFhirPersonLinkList.SetItemN(index: Integer; value: TFhirPersonLink);
begin
  assert(value is TFhirPersonLink);
  ObjectByIndex[index] := value;
end;

{ TFhirPerson }

constructor TFhirPerson.Create;
begin
  inherited;
end;

destructor TFhirPerson.Destroy;
begin
  FIdentifierList.Free;
  FNameList.Free;
  FTelecomList.Free;
  FGender.free;
  FBirthDate.free;
  FAddressList.Free;
  FPhoto.free;
  FManagingOrganization.free;
  FActive.free;
  FLink_List.Free;
  inherited;
end;

function TFhirPerson.GetResourceType : TFhirResourceType;
begin
  result := frtPerson;
end;

procedure TFhirPerson.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirPerson(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirPerson(oSource).FIdentifierList);
  end;
  if (TFhirPerson(oSource).FNameList = nil) then
  begin
    FNameList.free;
    FNameList := nil;
  end
  else
  begin
    if FNameList = nil then
      FNameList := TFhirHumanNameList.Create;
    FNameList.Assign(TFhirPerson(oSource).FNameList);
  end;
  if (TFhirPerson(oSource).FTelecomList = nil) then
  begin
    FTelecomList.free;
    FTelecomList := nil;
  end
  else
  begin
    if FTelecomList = nil then
      FTelecomList := TFhirContactPointList.Create;
    FTelecomList.Assign(TFhirPerson(oSource).FTelecomList);
  end;
  FGender := TFhirPerson(oSource).FGender.Link;
  birthDateElement := TFhirPerson(oSource).birthDateElement.Clone;
  if (TFhirPerson(oSource).FAddressList = nil) then
  begin
    FAddressList.free;
    FAddressList := nil;
  end
  else
  begin
    if FAddressList = nil then
      FAddressList := TFhirAddressList.Create;
    FAddressList.Assign(TFhirPerson(oSource).FAddressList);
  end;
  photo := TFhirPerson(oSource).photo.Clone;
  managingOrganization := TFhirPerson(oSource).managingOrganization.Clone;
  activeElement := TFhirPerson(oSource).activeElement.Clone;
  if (TFhirPerson(oSource).FLink_List = nil) then
  begin
    FLink_List.free;
    FLink_List := nil;
  end
  else
  begin
    if FLink_List = nil then
      FLink_List := TFhirPersonLinkList.Create;
    FLink_List.Assign(TFhirPerson(oSource).FLink_List);
  end;
end;

procedure TFhirPerson.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'name') Then
    list.addAll(self, 'name', FNameList);
  if (child_name = 'telecom') Then
    list.addAll(self, 'telecom', FTelecomList);
  if (child_name = 'gender') Then
     list.add(self.link, 'gender', FGender.Link);
  if (child_name = 'birthDate') Then
     list.add(self.link, 'birthDate', FBirthDate.Link);
  if (child_name = 'address') Then
    list.addAll(self, 'address', FAddressList);
  if (child_name = 'photo') Then
     list.add(self.link, 'photo', FPhoto.Link);
  if (child_name = 'managingOrganization') Then
     list.add(self.link, 'managingOrganization', FManagingOrganization.Link);
  if (child_name = 'active') Then
     list.add(self.link, 'active', FActive.Link);
  if (child_name = 'link') Then
    list.addAll(self, 'link', FLink_List);
end;

procedure TFhirPerson.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', true, TFhirHumanName, FNameList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'telecom', 'ContactPoint', true, TFhirContactPoint, FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'gender', 'code', false, TFHIREnum, FGender.Link));{1}
  oList.add(TFHIRProperty.create(self, 'birthDate', 'date', false, TFhirDate, FBirthDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'address', 'Address', true, TFhirAddress, FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', false, TFhirAttachment, FPhoto.Link));{2}
  oList.add(TFHIRProperty.create(self, 'managingOrganization', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FManagingOrganization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'active', 'boolean', false, TFhirBoolean, FActive.Link));{2}
  oList.add(TFHIRProperty.create(self, 'link', '', true, TFhirPersonLink, FLink_List.Link)){3};
end;

function TFhirPerson.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'name') then
  begin
    NameList.add(propValue as TFhirHumanName){2a};
    result := propValue;
  end
  else if (propName = 'telecom') then
  begin
    TelecomList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'gender') then
  begin
    GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, propValue);
    result := propValue
  end
  else if (propName = 'birthDate') then
  begin
    BirthDateElement := asDate(propValue){5a};
    result := propValue;
  end
  else if (propName = 'address') then
  begin
    AddressList.add(propValue as TFhirAddress){2a};
    result := propValue;
  end
  else if (propName = 'photo') then
  begin
    Photo := propValue as TFhirAttachment{4b};
    result := propValue;
  end
  else if (propName = 'managingOrganization') then
  begin
    ManagingOrganization := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'active') then
  begin
    ActiveElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'link') then
  begin
    Link_List.add(propValue as TFhirPersonLink){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirPerson.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'name') then NameList.insertItem(index, propValue as TFhirHumanName){2a}
  else if (propName = 'telecom') then TelecomList.insertItem(index, propValue as TFhirContactPoint){2a}
  else if (propName = 'address') then AddressList.insertItem(index, propValue as TFhirAddress){2a}
  else if (propName = 'link') then Link_List.insertItem(index, propValue as TFhirPersonLink){2a}
  else inherited;
end;

function TFhirPerson.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'name') then result := NameList.new(){2}
  else if (propName = 'telecom') then result := TelecomList.new(){2}
  else if (propName = 'birthDate') then result := TFhirDate.create() {5b}
  else if (propName = 'address') then result := AddressList.new(){2}
  else if (propName = 'photo') then result := TFhirAttachment.create(){4b}
  else if (propName = 'managingOrganization') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'active') then result := TFhirBoolean.create() {5b}
  else if (propName = 'link') then result := Link_List.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPerson.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'name') then result := 'HumanName'
  else if (propName = 'telecom') then result := 'ContactPoint'
  else if (propName = 'gender') then result := 'code'
  else if (propName = 'birthDate') then result := 'date'
  else if (propName = 'address') then result := 'Address'
  else if (propName = 'photo') then result := 'Attachment'
  else if (propName = 'managingOrganization') then result := 'Reference'
  else if (propName = 'active') then result := 'boolean'
  else if (propName = 'link') then result := ''
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPerson.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'name') then deletePropertyValue('name', NameList, value) {2}
  else if (propName = 'telecom') then deletePropertyValue('telecom', TelecomList, value) {2}
  else if (propName = 'gender') then GenderElement := nil
  else if (propName = 'birthDate') then BirthDateElement := nil
  else if (propName = 'address') then deletePropertyValue('address', AddressList, value) {2}
  else if (propName = 'photo') then PhotoElement := nil
  else if (propName = 'managingOrganization') then ManagingOrganizationElement := nil
  else if (propName = 'active') then ActiveElement := nil
  else if (propName = 'link') then deletePropertyValue('link', Link_List, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPerson.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'name') then replacePropertyValue('name', NameList, existing, new) {2}
  else if (propName = 'telecom') then replacePropertyValue('telecom', TelecomList, existing, new) {2}
  else if (propName = 'gender') then GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, new){4}
  else if (propName = 'birthDate') then BirthDateElement := asDate(new){5b}
  else if (propName = 'address') then replacePropertyValue('address', AddressList, existing, new) {2}
  else if (propName = 'photo') then PhotoElement := new as TFhirAttachment{4}
  else if (propName = 'managingOrganization') then ManagingOrganizationElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'active') then ActiveElement := asBoolean(new){5b}
  else if (propName = 'link') then replacePropertyValue('link', Link_List, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPerson.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'name') then NameList.move(source, destination){2a}
  else if (propName = 'telecom') then TelecomList.move(source, destination){2a}
  else if (propName = 'address') then AddressList.move(source, destination){2a}
  else if (propName = 'link') then Link_List.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirPerson.fhirType : string;
begin
  result := 'Person';
end;

function TFhirPerson.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FnameList) and isEmptyProp(FtelecomList) and isEmptyProp(FGender) and isEmptyProp(FBirthDate) and isEmptyProp(FaddressList) and isEmptyProp(FPhoto) and isEmptyProp(FManagingOrganization) and isEmptyProp(FActive) and isEmptyProp(Flink_List);
end;

function TFhirPerson.equals(other : TObject) : boolean;
var
  o : TFhirPerson;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPerson)) then
    result := false
  else
  begin
    o := TFhirPerson(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(nameList, o.nameList, true) and
      compareDeep(telecomList, o.telecomList, true) and compareDeep(genderElement, o.genderElement, true) and
      compareDeep(birthDateElement, o.birthDateElement, true) and compareDeep(addressList, o.addressList, true) and
      compareDeep(photoElement, o.photoElement, true) and compareDeep(managingOrganizationElement, o.managingOrganizationElement, true) and
      compareDeep(activeElement, o.activeElement, true) and compareDeep(link_List, o.link_List, true);
  end;
end;

function TFhirPerson.Link : TFhirPerson;
begin
  result := TFhirPerson(inherited Link);
end;

function TFhirPerson.Clone : TFhirPerson;
begin
  result := TFhirPerson(inherited Clone);
end;

procedure TFhirPerson.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('name');
  fields.add('telecom');
  fields.add('gender');
  fields.add('birthDate');
  fields.add('address');
  fields.add('photo');
  fields.add('managingOrganization');
  fields.add('active');
  fields.add('link');
end;

{ TFhirPerson }

Function TFhirPerson.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirPerson.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Function TFhirPerson.GetNameList : TFhirHumanNameList;
begin
  if FNameList = nil then
    FNameList := TFhirHumanNameList.Create;
  result := FNameList;
end;

Function TFhirPerson.GetHasNameList : boolean;
begin
  result := (FNameList <> nil) and (FNameList.count > 0);
end;

Function TFhirPerson.GetTelecomList : TFhirContactPointList;
begin
  if FTelecomList = nil then
    FTelecomList := TFhirContactPointList.Create;
  result := FTelecomList;
end;

Function TFhirPerson.GetHasTelecomList : boolean;
begin
  result := (FTelecomList <> nil) and (FTelecomList.count > 0);
end;

Procedure TFhirPerson.SetGender(value : TFhirEnum);
begin
  FGender.free;
  FGender := value;
end;

Function TFhirPerson.GetGenderST : TFhirAdministrativeGenderEnum;
begin
  if FGender = nil then
    result := TFhirAdministrativeGenderEnum(0)
  else
    result := TFhirAdministrativeGenderEnum(StringArrayIndexOfSensitive(CODES_TFhirAdministrativeGenderEnum, FGender.value));
end;

Procedure TFhirPerson.SetGenderST(value : TFhirAdministrativeGenderEnum);
begin
  if ord(value) = 0 then
    GenderElement := nil
  else
    GenderElement := TFhirEnum.create(SYSTEMS_TFhirAdministrativeGenderEnum[value], CODES_TFhirAdministrativeGenderEnum[value]);
end;

Procedure TFhirPerson.SetBirthDate(value : TFhirDate);
begin
  FBirthDate.free;
  FBirthDate := value;
end;

Function TFhirPerson.GetBirthDateST : TFslDateTime;
begin
  if FBirthDate = nil then
    result := TFslDateTime.makeNull
  else
    result := FBirthDate.value;
end;

Procedure TFhirPerson.SetBirthDateST(value : TFslDateTime);
begin
  if FBirthDate = nil then
    FBirthDate := TFhirDate.create;
  FBirthDate.value := value
end;

Function TFhirPerson.GetAddressList : TFhirAddressList;
begin
  if FAddressList = nil then
    FAddressList := TFhirAddressList.Create;
  result := FAddressList;
end;

Function TFhirPerson.GetHasAddressList : boolean;
begin
  result := (FAddressList <> nil) and (FAddressList.count > 0);
end;

Procedure TFhirPerson.SetPhoto(value : TFhirAttachment);
begin
  FPhoto.free;
  FPhoto := value;
end;

Procedure TFhirPerson.SetManagingOrganization(value : TFhirReference{TFhirOrganization});
begin
  FManagingOrganization.free;
  FManagingOrganization := value;
end;

Procedure TFhirPerson.SetActive(value : TFhirBoolean);
begin
  FActive.free;
  FActive := value;
end;

Function TFhirPerson.GetActiveST : Boolean;
begin
  if FActive = nil then
    result := false
  else
    result := FActive.value;
end;

Procedure TFhirPerson.SetActiveST(value : Boolean);
begin
  if FActive = nil then
    FActive := TFhirBoolean.create;
  FActive.value := value
end;

Function TFhirPerson.GetLink_List : TFhirPersonLinkList;
begin
  if FLink_List = nil then
    FLink_List := TFhirPersonLinkList.Create;
  result := FLink_List;
end;

Function TFhirPerson.GetHasLink_List : boolean;
begin
  result := (FLink_List <> nil) and (FLink_List.count > 0);
end;

function TFhirPerson.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FnameList.sizeInBytes);
  inc(result, FtelecomList.sizeInBytes);
  inc(result, FGender.sizeInBytes);
  inc(result, FBirthDate.sizeInBytes);
  inc(result, FaddressList.sizeInBytes);
  inc(result, FPhoto.sizeInBytes);
  inc(result, FManagingOrganization.sizeInBytes);
  inc(result, FActive.sizeInBytes);
  inc(result, Flink_List.sizeInBytes);
end;

{ TFhirPersonListEnumerator }

Constructor TFhirPersonListEnumerator.Create(list : TFhirPersonList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPersonListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPersonListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPersonListEnumerator.GetCurrent : TFhirPerson;
begin
  Result := FList[FIndex];
end;

function TFhirPersonListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPersonList }
procedure TFhirPersonList.AddItem(value: TFhirPerson);
begin
  assert(value.ClassName = 'TFhirPerson', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPerson');
  add(value);
end;

function TFhirPersonList.Append: TFhirPerson;
begin
  result := TFhirPerson.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPersonList.ClearItems;
begin
  Clear;
end;

function TFhirPersonList.GetEnumerator : TFhirPersonListEnumerator;
begin
  result := TFhirPersonListEnumerator.Create(self.link);
end;

function TFhirPersonList.Clone: TFhirPersonList;
begin
  result := TFhirPersonList(inherited Clone);
end;

function TFhirPersonList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPersonList.GetItemN(index: Integer): TFhirPerson;
begin
  result := TFhirPerson(ObjectByIndex[index]);
end;

function TFhirPersonList.ItemClass: TFslObjectClass;
begin
  result := TFhirPerson;
end;
function TFhirPersonList.IndexOf(value: TFhirPerson): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPersonList.Insert(index: Integer): TFhirPerson;
begin
  result := TFhirPerson.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPersonList.InsertItem(index: Integer; value: TFhirPerson);
begin
  assert(value is TFhirPerson);
  Inherited Insert(index, value);
end;

function TFhirPersonList.Item(index: Integer): TFhirPerson;
begin
  result := TFhirPerson(ObjectByIndex[index]);
end;

function TFhirPersonList.Link: TFhirPersonList;
begin
  result := TFhirPersonList(inherited Link);
end;

procedure TFhirPersonList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPersonList.SetItemByIndex(index: Integer; value: TFhirPerson);
begin
  assert(value is TFhirPerson);
  FhirPeople[index] := value;
end;

procedure TFhirPersonList.SetItemN(index: Integer; value: TFhirPerson);
begin
  assert(value is TFhirPerson);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_PERSON}

{$IFDEF FHIR_PRACTITIONER}

{ TFhirPractitionerPractitionerRole }

constructor TFhirPractitionerPractitionerRole.Create;
begin
  inherited;
end;

destructor TFhirPractitionerPractitionerRole.Destroy;
begin
  FManagingOrganization.free;
  FRole.free;
  FSpecialtyList.Free;
  FPeriod.free;
  FLocationList.Free;
  FHealthcareServiceList.Free;
  inherited;
end;

procedure TFhirPractitionerPractitionerRole.Assign(oSource : TFslObject);
begin
  inherited;
  managingOrganization := TFhirPractitionerPractitionerRole(oSource).managingOrganization.Clone;
  role := TFhirPractitionerPractitionerRole(oSource).role.Clone;
  if (TFhirPractitionerPractitionerRole(oSource).FSpecialtyList = nil) then
  begin
    FSpecialtyList.free;
    FSpecialtyList := nil;
  end
  else
  begin
    if FSpecialtyList = nil then
      FSpecialtyList := TFhirCodeableConceptList.Create;
    FSpecialtyList.Assign(TFhirPractitionerPractitionerRole(oSource).FSpecialtyList);
  end;
  period := TFhirPractitionerPractitionerRole(oSource).period.Clone;
  if (TFhirPractitionerPractitionerRole(oSource).FLocationList = nil) then
  begin
    FLocationList.free;
    FLocationList := nil;
  end
  else
  begin
    if FLocationList = nil then
      FLocationList := TFhirReferenceList{TFhirLocation}.Create;
    FLocationList.Assign(TFhirPractitionerPractitionerRole(oSource).FLocationList);
  end;
  if (TFhirPractitionerPractitionerRole(oSource).FHealthcareServiceList = nil) then
  begin
    FHealthcareServiceList.free;
    FHealthcareServiceList := nil;
  end
  else
  begin
    if FHealthcareServiceList = nil then
      FHealthcareServiceList := TFhirReferenceList{TFhirHealthcareService}.Create;
    FHealthcareServiceList.Assign(TFhirPractitionerPractitionerRole(oSource).FHealthcareServiceList);
  end;
end;

procedure TFhirPractitionerPractitionerRole.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'managingOrganization') Then
     list.add(self.link, 'managingOrganization', FManagingOrganization.Link);
  if (child_name = 'role') Then
     list.add(self.link, 'role', FRole.Link);
  if (child_name = 'specialty') Then
    list.addAll(self, 'specialty', FSpecialtyList);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'location') Then
    list.addAll(self, 'location', FLocationList);
  if (child_name = 'healthcareService') Then
    list.addAll(self, 'healthcareService', FHealthcareServiceList);
end;

procedure TFhirPractitionerPractitionerRole.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'managingOrganization', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FManagingOrganization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'role', 'CodeableConcept', false, TFhirCodeableConcept, FRole.Link));{2}
  oList.add(TFHIRProperty.create(self, 'specialty', 'CodeableConcept', true, TFhirCodeableConcept, FSpecialtyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Reference(Location)', true, TFhirReference{TFhirLocation}, FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'healthcareService', 'Reference(HealthcareService)', true, TFhirReference{TFhirHealthcareService}, FHealthcareServiceList.Link)){3};
end;

function TFhirPractitionerPractitionerRole.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'managingOrganization') then
  begin
    ManagingOrganization := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else if (propName = 'role') then
  begin
    Role := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'specialty') then
  begin
    SpecialtyList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else if (propName = 'location') then
  begin
    LocationList.add(propValue as TFhirReference{TFhirLocation}){2a};
    result := propValue;
  end
  else if (propName = 'healthcareService') then
  begin
    HealthcareServiceList.add(propValue as TFhirReference{TFhirHealthcareService}){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirPractitionerPractitionerRole.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'specialty') then SpecialtyList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'location') then LocationList.insertItem(index, propValue as TFhirReference{TFhirLocation}){2a}
  else if (propName = 'healthcareService') then HealthcareServiceList.insertItem(index, propValue as TFhirReference{TFhirHealthcareService}){2a}
  else inherited;
end;

function TFhirPractitionerPractitionerRole.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'managingOrganization') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else if (propName = 'role') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'specialty') then result := SpecialtyList.new(){2}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else if (propName = 'location') then result := LocationList.new(){2}
  else if (propName = 'healthcareService') then result := HealthcareServiceList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPractitionerPractitionerRole.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'managingOrganization') then result := 'Reference'
  else if (propName = 'role') then result := 'CodeableConcept'
  else if (propName = 'specialty') then result := 'CodeableConcept'
  else if (propName = 'period') then result := 'Period'
  else if (propName = 'location') then result := 'Reference'
  else if (propName = 'healthcareService') then result := 'Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPractitionerPractitionerRole.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'managingOrganization') then ManagingOrganizationElement := nil
  else if (propName = 'role') then RoleElement := nil
  else if (propName = 'specialty') then deletePropertyValue('specialty', SpecialtyList, value) {2}
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'location') then deletePropertyValue('location', LocationList, value) {2}
  else if (propName = 'healthcareService') then deletePropertyValue('healthcareService', HealthcareServiceList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPractitionerPractitionerRole.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'managingOrganization') then ManagingOrganizationElement := new as TFhirReference{TFhirOrganization}{4}
  else if (propName = 'role') then RoleElement := new as TFhirCodeableConcept{4}
  else if (propName = 'specialty') then replacePropertyValue('specialty', SpecialtyList, existing, new) {2}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else if (propName = 'location') then replacePropertyValue('location', LocationList, existing, new) {2}
  else if (propName = 'healthcareService') then replacePropertyValue('healthcareService', HealthcareServiceList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPractitionerPractitionerRole.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'specialty') then SpecialtyList.move(source, destination){2a}
  else if (propName = 'location') then LocationList.move(source, destination){2a}
  else if (propName = 'healthcareService') then HealthcareServiceList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirPractitionerPractitionerRole.fhirType : string;
begin
  result := 'practitionerRole';
end;

function TFhirPractitionerPractitionerRole.Link : TFhirPractitionerPractitionerRole;
begin
  result := TFhirPractitionerPractitionerRole(inherited Link);
end;

function TFhirPractitionerPractitionerRole.Clone : TFhirPractitionerPractitionerRole;
begin
  result := TFhirPractitionerPractitionerRole(inherited Clone);
end;

function TFhirPractitionerPractitionerRole.equals(other : TObject) : boolean;
var
  o : TFhirPractitionerPractitionerRole;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPractitionerPractitionerRole)) then
    result := false
  else
  begin
    o := TFhirPractitionerPractitionerRole(other);
    result := compareDeep(managingOrganizationElement, o.managingOrganizationElement, true) and
      compareDeep(roleElement, o.roleElement, true) and compareDeep(specialtyList, o.specialtyList, true) and
      compareDeep(periodElement, o.periodElement, true) and compareDeep(locationList, o.locationList, true) and
      compareDeep(healthcareServiceList, o.healthcareServiceList, true);
  end;
end;

function TFhirPractitionerPractitionerRole.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FManagingOrganization) and isEmptyProp(FRole) and isEmptyProp(FspecialtyList) and isEmptyProp(FPeriod) and isEmptyProp(FlocationList) and isEmptyProp(FhealthcareServiceList);
end;

procedure TFhirPractitionerPractitionerRole.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('managingOrganization');
  fields.add('role');
  fields.add('specialty');
  fields.add('period');
  fields.add('location');
  fields.add('healthcareService');
end;

{ TFhirPractitionerPractitionerRole }

Procedure TFhirPractitionerPractitionerRole.SetManagingOrganization(value : TFhirReference{TFhirOrganization});
begin
  FManagingOrganization.free;
  FManagingOrganization := value;
end;

Procedure TFhirPractitionerPractitionerRole.SetRole(value : TFhirCodeableConcept);
begin
  FRole.free;
  FRole := value;
end;

Function TFhirPractitionerPractitionerRole.GetSpecialtyList : TFhirCodeableConceptList;
begin
  if FSpecialtyList = nil then
    FSpecialtyList := TFhirCodeableConceptList.Create;
  result := FSpecialtyList;
end;

Function TFhirPractitionerPractitionerRole.GetHasSpecialtyList : boolean;
begin
  result := (FSpecialtyList <> nil) and (FSpecialtyList.count > 0);
end;

Procedure TFhirPractitionerPractitionerRole.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Function TFhirPractitionerPractitionerRole.GetLocationList : TFhirReferenceList{TFhirLocation};
begin
  if FLocationList = nil then
    FLocationList := TFhirReferenceList{TFhirLocation}.Create;
  result := FLocationList;
end;

Function TFhirPractitionerPractitionerRole.GetHasLocationList : boolean;
begin
  result := (FLocationList <> nil) and (FLocationList.count > 0);
end;

Function TFhirPractitionerPractitionerRole.GetHealthcareServiceList : TFhirReferenceList{TFhirHealthcareService};
begin
  if FHealthcareServiceList = nil then
    FHealthcareServiceList := TFhirReferenceList{TFhirHealthcareService}.Create;
  result := FHealthcareServiceList;
end;

Function TFhirPractitionerPractitionerRole.GetHasHealthcareServiceList : boolean;
begin
  result := (FHealthcareServiceList <> nil) and (FHealthcareServiceList.count > 0);
end;

function TFhirPractitionerPractitionerRole.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FManagingOrganization.sizeInBytes);
  inc(result, FRole.sizeInBytes);
  inc(result, FspecialtyList.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FlocationList.sizeInBytes);
  inc(result, FhealthcareServiceList.sizeInBytes);
end;

{ TFhirPractitionerPractitionerRoleListEnumerator }

Constructor TFhirPractitionerPractitionerRoleListEnumerator.Create(list : TFhirPractitionerPractitionerRoleList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPractitionerPractitionerRoleListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPractitionerPractitionerRoleListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPractitionerPractitionerRoleListEnumerator.GetCurrent : TFhirPractitionerPractitionerRole;
begin
  Result := FList[FIndex];
end;

function TFhirPractitionerPractitionerRoleListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPractitionerPractitionerRoleList }
procedure TFhirPractitionerPractitionerRoleList.AddItem(value: TFhirPractitionerPractitionerRole);
begin
  assert(value.ClassName = 'TFhirPractitionerPractitionerRole', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPractitionerPractitionerRole');
  add(value);
end;

function TFhirPractitionerPractitionerRoleList.Append: TFhirPractitionerPractitionerRole;
begin
  result := TFhirPractitionerPractitionerRole.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPractitionerPractitionerRoleList.ClearItems;
begin
  Clear;
end;

function TFhirPractitionerPractitionerRoleList.GetEnumerator : TFhirPractitionerPractitionerRoleListEnumerator;
begin
  result := TFhirPractitionerPractitionerRoleListEnumerator.Create(self.link);
end;

function TFhirPractitionerPractitionerRoleList.Clone: TFhirPractitionerPractitionerRoleList;
begin
  result := TFhirPractitionerPractitionerRoleList(inherited Clone);
end;

function TFhirPractitionerPractitionerRoleList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPractitionerPractitionerRoleList.GetItemN(index: Integer): TFhirPractitionerPractitionerRole;
begin
  result := TFhirPractitionerPractitionerRole(ObjectByIndex[index]);
end;

function TFhirPractitionerPractitionerRoleList.ItemClass: TFslObjectClass;
begin
  result := TFhirPractitionerPractitionerRole;
end;
function TFhirPractitionerPractitionerRoleList.IndexOf(value: TFhirPractitionerPractitionerRole): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPractitionerPractitionerRoleList.Insert(index: Integer): TFhirPractitionerPractitionerRole;
begin
  result := TFhirPractitionerPractitionerRole.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPractitionerPractitionerRoleList.InsertItem(index: Integer; value: TFhirPractitionerPractitionerRole);
begin
  assert(value is TFhirPractitionerPractitionerRole);
  Inherited Insert(index, value);
end;

function TFhirPractitionerPractitionerRoleList.Item(index: Integer): TFhirPractitionerPractitionerRole;
begin
  result := TFhirPractitionerPractitionerRole(ObjectByIndex[index]);
end;

function TFhirPractitionerPractitionerRoleList.Link: TFhirPractitionerPractitionerRoleList;
begin
  result := TFhirPractitionerPractitionerRoleList(inherited Link);
end;

procedure TFhirPractitionerPractitionerRoleList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPractitionerPractitionerRoleList.SetItemByIndex(index: Integer; value: TFhirPractitionerPractitionerRole);
begin
  assert(value is TFhirPractitionerPractitionerRole);
  FhirPractitionerPractitionerRoles[index] := value;
end;

procedure TFhirPractitionerPractitionerRoleList.SetItemN(index: Integer; value: TFhirPractitionerPractitionerRole);
begin
  assert(value is TFhirPractitionerPractitionerRole);
  ObjectByIndex[index] := value;
end;

{ TFhirPractitionerQualification }

constructor TFhirPractitionerQualification.Create;
begin
  inherited;
end;

destructor TFhirPractitionerQualification.Destroy;
begin
  FIdentifierList.Free;
  FCode.free;
  FPeriod.free;
  FIssuer.free;
  inherited;
end;

procedure TFhirPractitionerQualification.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirPractitionerQualification(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirPractitionerQualification(oSource).FIdentifierList);
  end;
  code := TFhirPractitionerQualification(oSource).code.Clone;
  period := TFhirPractitionerQualification(oSource).period.Clone;
  issuer := TFhirPractitionerQualification(oSource).issuer.Clone;
end;

procedure TFhirPractitionerQualification.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'code') Then
     list.add(self.link, 'code', FCode.Link);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
  if (child_name = 'issuer') Then
     list.add(self.link, 'issuer', FIssuer.Link);
end;

procedure TFhirPractitionerQualification.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', false, TFhirCodeableConcept, FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'issuer', 'Reference(Organization)', false, TFhirReference{TFhirOrganization}, FIssuer.Link));{2}
end;

function TFhirPractitionerQualification.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'code') then
  begin
    Code := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else if (propName = 'issuer') then
  begin
    Issuer := propValue as TFhirReference{TFhirOrganization}{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirPractitionerQualification.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else inherited;
end;

function TFhirPractitionerQualification.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'code') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else if (propName = 'issuer') then result := TFhirReference{TFhirOrganization}.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPractitionerQualification.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'code') then result := 'CodeableConcept'
  else if (propName = 'period') then result := 'Period'
  else if (propName = 'issuer') then result := 'Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPractitionerQualification.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'code') then CodeElement := nil
  else if (propName = 'period') then PeriodElement := nil
  else if (propName = 'issuer') then IssuerElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPractitionerQualification.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'code') then CodeElement := new as TFhirCodeableConcept{4}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else if (propName = 'issuer') then IssuerElement := new as TFhirReference{TFhirOrganization}{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPractitionerQualification.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirPractitionerQualification.fhirType : string;
begin
  result := 'qualification';
end;

function TFhirPractitionerQualification.Link : TFhirPractitionerQualification;
begin
  result := TFhirPractitionerQualification(inherited Link);
end;

function TFhirPractitionerQualification.Clone : TFhirPractitionerQualification;
begin
  result := TFhirPractitionerQualification(inherited Clone);
end;

function TFhirPractitionerQualification.equals(other : TObject) : boolean;
var
  o : TFhirPractitionerQualification;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPractitionerQualification)) then
    result := false
  else
  begin
    o := TFhirPractitionerQualification(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(codeElement, o.codeElement, true) and
      compareDeep(periodElement, o.periodElement, true) and compareDeep(issuerElement, o.issuerElement, true);
  end;
end;

function TFhirPractitionerQualification.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FCode) and isEmptyProp(FPeriod) and isEmptyProp(FIssuer);
end;

procedure TFhirPractitionerQualification.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('code');
  fields.add('period');
  fields.add('issuer');
end;

{ TFhirPractitionerQualification }

Function TFhirPractitionerQualification.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirPractitionerQualification.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirPractitionerQualification.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

Procedure TFhirPractitionerQualification.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Procedure TFhirPractitionerQualification.SetIssuer(value : TFhirReference{TFhirOrganization});
begin
  FIssuer.free;
  FIssuer := value;
end;

function TFhirPractitionerQualification.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FCode.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
  inc(result, FIssuer.sizeInBytes);
end;

{ TFhirPractitionerQualificationListEnumerator }

Constructor TFhirPractitionerQualificationListEnumerator.Create(list : TFhirPractitionerQualificationList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPractitionerQualificationListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPractitionerQualificationListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPractitionerQualificationListEnumerator.GetCurrent : TFhirPractitionerQualification;
begin
  Result := FList[FIndex];
end;

function TFhirPractitionerQualificationListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPractitionerQualificationList }
procedure TFhirPractitionerQualificationList.AddItem(value: TFhirPractitionerQualification);
begin
  assert(value.ClassName = 'TFhirPractitionerQualification', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPractitionerQualification');
  add(value);
end;

function TFhirPractitionerQualificationList.Append: TFhirPractitionerQualification;
begin
  result := TFhirPractitionerQualification.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPractitionerQualificationList.ClearItems;
begin
  Clear;
end;

function TFhirPractitionerQualificationList.GetEnumerator : TFhirPractitionerQualificationListEnumerator;
begin
  result := TFhirPractitionerQualificationListEnumerator.Create(self.link);
end;

function TFhirPractitionerQualificationList.Clone: TFhirPractitionerQualificationList;
begin
  result := TFhirPractitionerQualificationList(inherited Clone);
end;

function TFhirPractitionerQualificationList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPractitionerQualificationList.GetItemN(index: Integer): TFhirPractitionerQualification;
begin
  result := TFhirPractitionerQualification(ObjectByIndex[index]);
end;

function TFhirPractitionerQualificationList.ItemClass: TFslObjectClass;
begin
  result := TFhirPractitionerQualification;
end;
function TFhirPractitionerQualificationList.IndexOf(value: TFhirPractitionerQualification): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPractitionerQualificationList.Insert(index: Integer): TFhirPractitionerQualification;
begin
  result := TFhirPractitionerQualification.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPractitionerQualificationList.InsertItem(index: Integer; value: TFhirPractitionerQualification);
begin
  assert(value is TFhirPractitionerQualification);
  Inherited Insert(index, value);
end;

function TFhirPractitionerQualificationList.Item(index: Integer): TFhirPractitionerQualification;
begin
  result := TFhirPractitionerQualification(ObjectByIndex[index]);
end;

function TFhirPractitionerQualificationList.Link: TFhirPractitionerQualificationList;
begin
  result := TFhirPractitionerQualificationList(inherited Link);
end;

procedure TFhirPractitionerQualificationList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPractitionerQualificationList.SetItemByIndex(index: Integer; value: TFhirPractitionerQualification);
begin
  assert(value is TFhirPractitionerQualification);
  FhirPractitionerQualifications[index] := value;
end;

procedure TFhirPractitionerQualificationList.SetItemN(index: Integer; value: TFhirPractitionerQualification);
begin
  assert(value is TFhirPractitionerQualification);
  ObjectByIndex[index] := value;
end;

{ TFhirPractitioner }

constructor TFhirPractitioner.Create;
begin
  inherited;
end;

destructor TFhirPractitioner.Destroy;
begin
  FIdentifierList.Free;
  FActive.free;
  FName.free;
  FTelecomList.Free;
  FAddressList.Free;
  FGender.free;
  FBirthDate.free;
  FPhotoList.Free;
  FPractitionerRoleList.Free;
  FQualificationList.Free;
  FCommunicationList.Free;
  inherited;
end;

function TFhirPractitioner.GetResourceType : TFhirResourceType;
begin
  result := frtPractitioner;
end;

procedure TFhirPractitioner.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirPractitioner(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirPractitioner(oSource).FIdentifierList);
  end;
  activeElement := TFhirPractitioner(oSource).activeElement.Clone;
  name := TFhirPractitioner(oSource).name.Clone;
  if (TFhirPractitioner(oSource).FTelecomList = nil) then
  begin
    FTelecomList.free;
    FTelecomList := nil;
  end
  else
  begin
    if FTelecomList = nil then
      FTelecomList := TFhirContactPointList.Create;
    FTelecomList.Assign(TFhirPractitioner(oSource).FTelecomList);
  end;
  if (TFhirPractitioner(oSource).FAddressList = nil) then
  begin
    FAddressList.free;
    FAddressList := nil;
  end
  else
  begin
    if FAddressList = nil then
      FAddressList := TFhirAddressList.Create;
    FAddressList.Assign(TFhirPractitioner(oSource).FAddressList);
  end;
  FGender := TFhirPractitioner(oSource).FGender.Link;
  birthDateElement := TFhirPractitioner(oSource).birthDateElement.Clone;
  if (TFhirPractitioner(oSource).FPhotoList = nil) then
  begin
    FPhotoList.free;
    FPhotoList := nil;
  end
  else
  begin
    if FPhotoList = nil then
      FPhotoList := TFhirAttachmentList.Create;
    FPhotoList.Assign(TFhirPractitioner(oSource).FPhotoList);
  end;
  if (TFhirPractitioner(oSource).FPractitionerRoleList = nil) then
  begin
    FPractitionerRoleList.free;
    FPractitionerRoleList := nil;
  end
  else
  begin
    if FPractitionerRoleList = nil then
      FPractitionerRoleList := TFhirPractitionerPractitionerRoleList.Create;
    FPractitionerRoleList.Assign(TFhirPractitioner(oSource).FPractitionerRoleList);
  end;
  if (TFhirPractitioner(oSource).FQualificationList = nil) then
  begin
    FQualificationList.free;
    FQualificationList := nil;
  end
  else
  begin
    if FQualificationList = nil then
      FQualificationList := TFhirPractitionerQualificationList.Create;
    FQualificationList.Assign(TFhirPractitioner(oSource).FQualificationList);
  end;
  if (TFhirPractitioner(oSource).FCommunicationList = nil) then
  begin
    FCommunicationList.free;
    FCommunicationList := nil;
  end
  else
  begin
    if FCommunicationList = nil then
      FCommunicationList := TFhirCodeableConceptList.Create;
    FCommunicationList.Assign(TFhirPractitioner(oSource).FCommunicationList);
  end;
end;

procedure TFhirPractitioner.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'active') Then
     list.add(self.link, 'active', FActive.Link);
  if (child_name = 'name') Then
     list.add(self.link, 'name', FName.Link);
  if (child_name = 'telecom') Then
    list.addAll(self, 'telecom', FTelecomList);
  if (child_name = 'address') Then
    list.addAll(self, 'address', FAddressList);
  if (child_name = 'gender') Then
     list.add(self.link, 'gender', FGender.Link);
  if (child_name = 'birthDate') Then
     list.add(self.link, 'birthDate', FBirthDate.Link);
  if (child_name = 'photo') Then
    list.addAll(self, 'photo', FPhotoList);
  if (child_name = 'practitionerRole') Then
    list.addAll(self, 'practitionerRole', FPractitionerRoleList);
  if (child_name = 'qualification') Then
    list.addAll(self, 'qualification', FQualificationList);
  if (child_name = 'communication') Then
    list.addAll(self, 'communication', FCommunicationList);
end;

procedure TFhirPractitioner.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'active', 'boolean', false, TFhirBoolean, FActive.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', false, TFhirHumanName, FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'ContactPoint', true, TFhirContactPoint, FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', true, TFhirAddress, FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'gender', 'code', false, TFHIREnum, FGender.Link));{1}
  oList.add(TFHIRProperty.create(self, 'birthDate', 'date', false, TFhirDate, FBirthDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', true, TFhirAttachment, FPhotoList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'practitionerRole', '', true, TFhirPractitionerPractitionerRole, FPractitionerRoleList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'qualification', '', true, TFhirPractitionerQualification, FQualificationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'communication', 'CodeableConcept', true, TFhirCodeableConcept, FCommunicationList.Link)){3};
end;

function TFhirPractitioner.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'active') then
  begin
    ActiveElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'name') then
  begin
    Name := propValue as TFhirHumanName{4b};
    result := propValue;
  end
  else if (propName = 'telecom') then
  begin
    TelecomList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'address') then
  begin
    AddressList.add(propValue as TFhirAddress){2a};
    result := propValue;
  end
  else if (propName = 'gender') then
  begin
    GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, propValue);
    result := propValue
  end
  else if (propName = 'birthDate') then
  begin
    BirthDateElement := asDate(propValue){5a};
    result := propValue;
  end
  else if (propName = 'photo') then
  begin
    PhotoList.add(propValue as TFhirAttachment){2a};
    result := propValue;
  end
  else if (propName = 'practitionerRole') then
  begin
    PractitionerRoleList.add(propValue as TFhirPractitionerPractitionerRole){2a};
    result := propValue;
  end
  else if (propName = 'qualification') then
  begin
    QualificationList.add(propValue as TFhirPractitionerQualification){2a};
    result := propValue;
  end
  else if (propName = 'communication') then
  begin
    CommunicationList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirPractitioner.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'telecom') then TelecomList.insertItem(index, propValue as TFhirContactPoint){2a}
  else if (propName = 'address') then AddressList.insertItem(index, propValue as TFhirAddress){2a}
  else if (propName = 'photo') then PhotoList.insertItem(index, propValue as TFhirAttachment){2a}
  else if (propName = 'practitionerRole') then PractitionerRoleList.insertItem(index, propValue as TFhirPractitionerPractitionerRole){2a}
  else if (propName = 'qualification') then QualificationList.insertItem(index, propValue as TFhirPractitionerQualification){2a}
  else if (propName = 'communication') then CommunicationList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else inherited;
end;

function TFhirPractitioner.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'active') then result := TFhirBoolean.create() {5b}
  else if (propName = 'name') then result := TFhirHumanName.create(){4b}
  else if (propName = 'telecom') then result := TelecomList.new(){2}
  else if (propName = 'address') then result := AddressList.new(){2}
  else if (propName = 'birthDate') then result := TFhirDate.create() {5b}
  else if (propName = 'photo') then result := PhotoList.new(){2}
  else if (propName = 'practitionerRole') then result := PractitionerRoleList.new(){2}
  else if (propName = 'qualification') then result := QualificationList.new(){2}
  else if (propName = 'communication') then result := CommunicationList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirPractitioner.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'active') then result := 'boolean'
  else if (propName = 'name') then result := 'HumanName'
  else if (propName = 'telecom') then result := 'ContactPoint'
  else if (propName = 'address') then result := 'Address'
  else if (propName = 'gender') then result := 'code'
  else if (propName = 'birthDate') then result := 'date'
  else if (propName = 'photo') then result := 'Attachment'
  else if (propName = 'practitionerRole') then result := ''
  else if (propName = 'qualification') then result := ''
  else if (propName = 'communication') then result := 'CodeableConcept'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirPractitioner.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'active') then ActiveElement := nil
  else if (propName = 'name') then NameElement := nil
  else if (propName = 'telecom') then deletePropertyValue('telecom', TelecomList, value) {2}
  else if (propName = 'address') then deletePropertyValue('address', AddressList, value) {2}
  else if (propName = 'gender') then GenderElement := nil
  else if (propName = 'birthDate') then BirthDateElement := nil
  else if (propName = 'photo') then deletePropertyValue('photo', PhotoList, value) {2}
  else if (propName = 'practitionerRole') then deletePropertyValue('practitionerRole', PractitionerRoleList, value) {2}
  else if (propName = 'qualification') then deletePropertyValue('qualification', QualificationList, value) {2}
  else if (propName = 'communication') then deletePropertyValue('communication', CommunicationList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirPractitioner.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'active') then ActiveElement := asBoolean(new){5b}
  else if (propName = 'name') then NameElement := new as TFhirHumanName{4}
  else if (propName = 'telecom') then replacePropertyValue('telecom', TelecomList, existing, new) {2}
  else if (propName = 'address') then replacePropertyValue('address', AddressList, existing, new) {2}
  else if (propName = 'gender') then GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, new){4}
  else if (propName = 'birthDate') then BirthDateElement := asDate(new){5b}
  else if (propName = 'photo') then replacePropertyValue('photo', PhotoList, existing, new) {2}
  else if (propName = 'practitionerRole') then replacePropertyValue('practitionerRole', PractitionerRoleList, existing, new) {2}
  else if (propName = 'qualification') then replacePropertyValue('qualification', QualificationList, existing, new) {2}
  else if (propName = 'communication') then replacePropertyValue('communication', CommunicationList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirPractitioner.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'telecom') then TelecomList.move(source, destination){2a}
  else if (propName = 'address') then AddressList.move(source, destination){2a}
  else if (propName = 'photo') then PhotoList.move(source, destination){2a}
  else if (propName = 'practitionerRole') then PractitionerRoleList.move(source, destination){2a}
  else if (propName = 'qualification') then QualificationList.move(source, destination){2a}
  else if (propName = 'communication') then CommunicationList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirPractitioner.fhirType : string;
begin
  result := 'Practitioner';
end;

function TFhirPractitioner.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FActive) and isEmptyProp(FName) and isEmptyProp(FtelecomList) and isEmptyProp(FaddressList) and isEmptyProp(FGender) and isEmptyProp(FBirthDate) and isEmptyProp(FphotoList) and isEmptyProp(FpractitionerRoleList) and isEmptyProp(FqualificationList) and isEmptyProp(FcommunicationList);
end;

function TFhirPractitioner.equals(other : TObject) : boolean;
var
  o : TFhirPractitioner;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirPractitioner)) then
    result := false
  else
  begin
    o := TFhirPractitioner(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(activeElement, o.activeElement, true) and
      compareDeep(nameElement, o.nameElement, true) and compareDeep(telecomList, o.telecomList, true) and
      compareDeep(addressList, o.addressList, true) and compareDeep(genderElement, o.genderElement, true) and
      compareDeep(birthDateElement, o.birthDateElement, true) and compareDeep(photoList, o.photoList, true) and
      compareDeep(practitionerRoleList, o.practitionerRoleList, true) and compareDeep(qualificationList, o.qualificationList, true) and
      compareDeep(communicationList, o.communicationList, true);
  end;
end;

function TFhirPractitioner.Link : TFhirPractitioner;
begin
  result := TFhirPractitioner(inherited Link);
end;

function TFhirPractitioner.Clone : TFhirPractitioner;
begin
  result := TFhirPractitioner(inherited Clone);
end;

procedure TFhirPractitioner.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('active');
  fields.add('name');
  fields.add('telecom');
  fields.add('address');
  fields.add('gender');
  fields.add('birthDate');
  fields.add('photo');
  fields.add('practitionerRole');
  fields.add('qualification');
  fields.add('communication');
end;

{ TFhirPractitioner }

Function TFhirPractitioner.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirPractitioner.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirPractitioner.SetActive(value : TFhirBoolean);
begin
  FActive.free;
  FActive := value;
end;

Function TFhirPractitioner.GetActiveST : Boolean;
begin
  if FActive = nil then
    result := false
  else
    result := FActive.value;
end;

Procedure TFhirPractitioner.SetActiveST(value : Boolean);
begin
  if FActive = nil then
    FActive := TFhirBoolean.create;
  FActive.value := value
end;

Procedure TFhirPractitioner.SetName(value : TFhirHumanName);
begin
  FName.free;
  FName := value;
end;

Function TFhirPractitioner.GetTelecomList : TFhirContactPointList;
begin
  if FTelecomList = nil then
    FTelecomList := TFhirContactPointList.Create;
  result := FTelecomList;
end;

Function TFhirPractitioner.GetHasTelecomList : boolean;
begin
  result := (FTelecomList <> nil) and (FTelecomList.count > 0);
end;

Function TFhirPractitioner.GetAddressList : TFhirAddressList;
begin
  if FAddressList = nil then
    FAddressList := TFhirAddressList.Create;
  result := FAddressList;
end;

Function TFhirPractitioner.GetHasAddressList : boolean;
begin
  result := (FAddressList <> nil) and (FAddressList.count > 0);
end;

Procedure TFhirPractitioner.SetGender(value : TFhirEnum);
begin
  FGender.free;
  FGender := value;
end;

Function TFhirPractitioner.GetGenderST : TFhirAdministrativeGenderEnum;
begin
  if FGender = nil then
    result := TFhirAdministrativeGenderEnum(0)
  else
    result := TFhirAdministrativeGenderEnum(StringArrayIndexOfSensitive(CODES_TFhirAdministrativeGenderEnum, FGender.value));
end;

Procedure TFhirPractitioner.SetGenderST(value : TFhirAdministrativeGenderEnum);
begin
  if ord(value) = 0 then
    GenderElement := nil
  else
    GenderElement := TFhirEnum.create(SYSTEMS_TFhirAdministrativeGenderEnum[value], CODES_TFhirAdministrativeGenderEnum[value]);
end;

Procedure TFhirPractitioner.SetBirthDate(value : TFhirDate);
begin
  FBirthDate.free;
  FBirthDate := value;
end;

Function TFhirPractitioner.GetBirthDateST : TFslDateTime;
begin
  if FBirthDate = nil then
    result := TFslDateTime.makeNull
  else
    result := FBirthDate.value;
end;

Procedure TFhirPractitioner.SetBirthDateST(value : TFslDateTime);
begin
  if FBirthDate = nil then
    FBirthDate := TFhirDate.create;
  FBirthDate.value := value
end;

Function TFhirPractitioner.GetPhotoList : TFhirAttachmentList;
begin
  if FPhotoList = nil then
    FPhotoList := TFhirAttachmentList.Create;
  result := FPhotoList;
end;

Function TFhirPractitioner.GetHasPhotoList : boolean;
begin
  result := (FPhotoList <> nil) and (FPhotoList.count > 0);
end;

Function TFhirPractitioner.GetPractitionerRoleList : TFhirPractitionerPractitionerRoleList;
begin
  if FPractitionerRoleList = nil then
    FPractitionerRoleList := TFhirPractitionerPractitionerRoleList.Create;
  result := FPractitionerRoleList;
end;

Function TFhirPractitioner.GetHasPractitionerRoleList : boolean;
begin
  result := (FPractitionerRoleList <> nil) and (FPractitionerRoleList.count > 0);
end;

Function TFhirPractitioner.GetQualificationList : TFhirPractitionerQualificationList;
begin
  if FQualificationList = nil then
    FQualificationList := TFhirPractitionerQualificationList.Create;
  result := FQualificationList;
end;

Function TFhirPractitioner.GetHasQualificationList : boolean;
begin
  result := (FQualificationList <> nil) and (FQualificationList.count > 0);
end;

Function TFhirPractitioner.GetCommunicationList : TFhirCodeableConceptList;
begin
  if FCommunicationList = nil then
    FCommunicationList := TFhirCodeableConceptList.Create;
  result := FCommunicationList;
end;

Function TFhirPractitioner.GetHasCommunicationList : boolean;
begin
  result := (FCommunicationList <> nil) and (FCommunicationList.count > 0);
end;

function TFhirPractitioner.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FActive.sizeInBytes);
  inc(result, FName.sizeInBytes);
  inc(result, FtelecomList.sizeInBytes);
  inc(result, FaddressList.sizeInBytes);
  inc(result, FGender.sizeInBytes);
  inc(result, FBirthDate.sizeInBytes);
  inc(result, FphotoList.sizeInBytes);
  inc(result, FpractitionerRoleList.sizeInBytes);
  inc(result, FqualificationList.sizeInBytes);
  inc(result, FcommunicationList.sizeInBytes);
end;

{ TFhirPractitionerListEnumerator }

Constructor TFhirPractitionerListEnumerator.Create(list : TFhirPractitionerList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirPractitionerListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPractitionerListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPractitionerListEnumerator.GetCurrent : TFhirPractitioner;
begin
  Result := FList[FIndex];
end;

function TFhirPractitionerListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirPractitionerList }
procedure TFhirPractitionerList.AddItem(value: TFhirPractitioner);
begin
  assert(value.ClassName = 'TFhirPractitioner', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirPractitioner');
  add(value);
end;

function TFhirPractitionerList.Append: TFhirPractitioner;
begin
  result := TFhirPractitioner.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPractitionerList.ClearItems;
begin
  Clear;
end;

function TFhirPractitionerList.GetEnumerator : TFhirPractitionerListEnumerator;
begin
  result := TFhirPractitionerListEnumerator.Create(self.link);
end;

function TFhirPractitionerList.Clone: TFhirPractitionerList;
begin
  result := TFhirPractitionerList(inherited Clone);
end;

function TFhirPractitionerList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirPractitionerList.GetItemN(index: Integer): TFhirPractitioner;
begin
  result := TFhirPractitioner(ObjectByIndex[index]);
end;

function TFhirPractitionerList.ItemClass: TFslObjectClass;
begin
  result := TFhirPractitioner;
end;
function TFhirPractitionerList.IndexOf(value: TFhirPractitioner): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirPractitionerList.Insert(index: Integer): TFhirPractitioner;
begin
  result := TFhirPractitioner.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirPractitionerList.InsertItem(index: Integer; value: TFhirPractitioner);
begin
  assert(value is TFhirPractitioner);
  Inherited Insert(index, value);
end;

function TFhirPractitionerList.Item(index: Integer): TFhirPractitioner;
begin
  result := TFhirPractitioner(ObjectByIndex[index]);
end;

function TFhirPractitionerList.Link: TFhirPractitionerList;
begin
  result := TFhirPractitionerList(inherited Link);
end;

procedure TFhirPractitionerList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirPractitionerList.SetItemByIndex(index: Integer; value: TFhirPractitioner);
begin
  assert(value is TFhirPractitioner);
  FhirPractitioners[index] := value;
end;

procedure TFhirPractitionerList.SetItemN(index: Integer; value: TFhirPractitioner);
begin
  assert(value is TFhirPractitioner);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_PRACTITIONER}

{$IFDEF FHIR_RELATEDPERSON}

{ TFhirRelatedPerson }

constructor TFhirRelatedPerson.Create;
begin
  inherited;
end;

destructor TFhirRelatedPerson.Destroy;
begin
  FIdentifierList.Free;
  FPatient.free;
  FRelationship.free;
  FName.free;
  FTelecomList.Free;
  FGender.free;
  FBirthDate.free;
  FAddressList.Free;
  FPhotoList.Free;
  FPeriod.free;
  inherited;
end;

function TFhirRelatedPerson.GetResourceType : TFhirResourceType;
begin
  result := frtRelatedPerson;
end;

procedure TFhirRelatedPerson.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirRelatedPerson(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirRelatedPerson(oSource).FIdentifierList);
  end;
  patient := TFhirRelatedPerson(oSource).patient.Clone;
  relationship := TFhirRelatedPerson(oSource).relationship.Clone;
  name := TFhirRelatedPerson(oSource).name.Clone;
  if (TFhirRelatedPerson(oSource).FTelecomList = nil) then
  begin
    FTelecomList.free;
    FTelecomList := nil;
  end
  else
  begin
    if FTelecomList = nil then
      FTelecomList := TFhirContactPointList.Create;
    FTelecomList.Assign(TFhirRelatedPerson(oSource).FTelecomList);
  end;
  FGender := TFhirRelatedPerson(oSource).FGender.Link;
  birthDateElement := TFhirRelatedPerson(oSource).birthDateElement.Clone;
  if (TFhirRelatedPerson(oSource).FAddressList = nil) then
  begin
    FAddressList.free;
    FAddressList := nil;
  end
  else
  begin
    if FAddressList = nil then
      FAddressList := TFhirAddressList.Create;
    FAddressList.Assign(TFhirRelatedPerson(oSource).FAddressList);
  end;
  if (TFhirRelatedPerson(oSource).FPhotoList = nil) then
  begin
    FPhotoList.free;
    FPhotoList := nil;
  end
  else
  begin
    if FPhotoList = nil then
      FPhotoList := TFhirAttachmentList.Create;
    FPhotoList.Assign(TFhirRelatedPerson(oSource).FPhotoList);
  end;
  period := TFhirRelatedPerson(oSource).period.Clone;
end;

procedure TFhirRelatedPerson.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'patient') Then
     list.add(self.link, 'patient', FPatient.Link);
  if (child_name = 'relationship') Then
     list.add(self.link, 'relationship', FRelationship.Link);
  if (child_name = 'name') Then
     list.add(self.link, 'name', FName.Link);
  if (child_name = 'telecom') Then
    list.addAll(self, 'telecom', FTelecomList);
  if (child_name = 'gender') Then
     list.add(self.link, 'gender', FGender.Link);
  if (child_name = 'birthDate') Then
     list.add(self.link, 'birthDate', FBirthDate.Link);
  if (child_name = 'address') Then
    list.addAll(self, 'address', FAddressList);
  if (child_name = 'photo') Then
    list.addAll(self, 'photo', FPhotoList);
  if (child_name = 'period') Then
     list.add(self.link, 'period', FPeriod.Link);
end;

procedure TFhirRelatedPerson.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'patient', 'Reference(Patient)', false, TFhirReference{TFhirPatient}, FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'relationship', 'CodeableConcept', false, TFhirCodeableConcept, FRelationship.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', false, TFhirHumanName, FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'ContactPoint', true, TFhirContactPoint, FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'gender', 'code', false, TFHIREnum, FGender.Link));{1}
  oList.add(TFHIRProperty.create(self, 'birthDate', 'date', false, TFhirDate, FBirthDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'address', 'Address', true, TFhirAddress, FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', true, TFhirAttachment, FPhotoList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', false, TFhirPeriod, FPeriod.Link));{2}
end;

function TFhirRelatedPerson.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'patient') then
  begin
    Patient := propValue as TFhirReference{TFhirPatient}{4b};
    result := propValue;
  end
  else if (propName = 'relationship') then
  begin
    Relationship := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'name') then
  begin
    Name := propValue as TFhirHumanName{4b};
    result := propValue;
  end
  else if (propName = 'telecom') then
  begin
    TelecomList.add(propValue as TFhirContactPoint){2a};
    result := propValue;
  end
  else if (propName = 'gender') then
  begin
    GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, propValue);
    result := propValue
  end
  else if (propName = 'birthDate') then
  begin
    BirthDateElement := asDate(propValue){5a};
    result := propValue;
  end
  else if (propName = 'address') then
  begin
    AddressList.add(propValue as TFhirAddress){2a};
    result := propValue;
  end
  else if (propName = 'photo') then
  begin
    PhotoList.add(propValue as TFhirAttachment){2a};
    result := propValue;
  end
  else if (propName = 'period') then
  begin
    Period := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirRelatedPerson.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'telecom') then TelecomList.insertItem(index, propValue as TFhirContactPoint){2a}
  else if (propName = 'address') then AddressList.insertItem(index, propValue as TFhirAddress){2a}
  else if (propName = 'photo') then PhotoList.insertItem(index, propValue as TFhirAttachment){2a}
  else inherited;
end;

function TFhirRelatedPerson.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'patient') then result := TFhirReference{TFhirPatient}.create(){4b}
  else if (propName = 'relationship') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'name') then result := TFhirHumanName.create(){4b}
  else if (propName = 'telecom') then result := TelecomList.new(){2}
  else if (propName = 'birthDate') then result := TFhirDate.create() {5b}
  else if (propName = 'address') then result := AddressList.new(){2}
  else if (propName = 'photo') then result := PhotoList.new(){2}
  else if (propName = 'period') then result := TFhirPeriod.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirRelatedPerson.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'patient') then result := 'Reference'
  else if (propName = 'relationship') then result := 'CodeableConcept'
  else if (propName = 'name') then result := 'HumanName'
  else if (propName = 'telecom') then result := 'ContactPoint'
  else if (propName = 'gender') then result := 'code'
  else if (propName = 'birthDate') then result := 'date'
  else if (propName = 'address') then result := 'Address'
  else if (propName = 'photo') then result := 'Attachment'
  else if (propName = 'period') then result := 'Period'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirRelatedPerson.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'patient') then PatientElement := nil
  else if (propName = 'relationship') then RelationshipElement := nil
  else if (propName = 'name') then NameElement := nil
  else if (propName = 'telecom') then deletePropertyValue('telecom', TelecomList, value) {2}
  else if (propName = 'gender') then GenderElement := nil
  else if (propName = 'birthDate') then BirthDateElement := nil
  else if (propName = 'address') then deletePropertyValue('address', AddressList, value) {2}
  else if (propName = 'photo') then deletePropertyValue('photo', PhotoList, value) {2}
  else if (propName = 'period') then PeriodElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirRelatedPerson.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'patient') then PatientElement := new as TFhirReference{TFhirPatient}{4}
  else if (propName = 'relationship') then RelationshipElement := new as TFhirCodeableConcept{4}
  else if (propName = 'name') then NameElement := new as TFhirHumanName{4}
  else if (propName = 'telecom') then replacePropertyValue('telecom', TelecomList, existing, new) {2}
  else if (propName = 'gender') then GenderElement := asEnum(SYSTEMS_TFhirAdministrativeGenderEnum, CODES_TFhirAdministrativeGenderEnum, new){4}
  else if (propName = 'birthDate') then BirthDateElement := asDate(new){5b}
  else if (propName = 'address') then replacePropertyValue('address', AddressList, existing, new) {2}
  else if (propName = 'photo') then replacePropertyValue('photo', PhotoList, existing, new) {2}
  else if (propName = 'period') then PeriodElement := new as TFhirPeriod{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirRelatedPerson.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'telecom') then TelecomList.move(source, destination){2a}
  else if (propName = 'address') then AddressList.move(source, destination){2a}
  else if (propName = 'photo') then PhotoList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirRelatedPerson.fhirType : string;
begin
  result := 'RelatedPerson';
end;

function TFhirRelatedPerson.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FPatient) and isEmptyProp(FRelationship) and isEmptyProp(FName) and isEmptyProp(FtelecomList) and isEmptyProp(FGender) and isEmptyProp(FBirthDate) and isEmptyProp(FaddressList) and isEmptyProp(FphotoList) and isEmptyProp(FPeriod);
end;

function TFhirRelatedPerson.equals(other : TObject) : boolean;
var
  o : TFhirRelatedPerson;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirRelatedPerson)) then
    result := false
  else
  begin
    o := TFhirRelatedPerson(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(patientElement, o.patientElement, true) and
      compareDeep(relationshipElement, o.relationshipElement, true) and compareDeep(nameElement, o.nameElement, true) and
      compareDeep(telecomList, o.telecomList, true) and compareDeep(genderElement, o.genderElement, true) and
      compareDeep(birthDateElement, o.birthDateElement, true) and compareDeep(addressList, o.addressList, true) and
      compareDeep(photoList, o.photoList, true) and compareDeep(periodElement, o.periodElement, true);
  end;
end;

function TFhirRelatedPerson.Link : TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson(inherited Link);
end;

function TFhirRelatedPerson.Clone : TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson(inherited Clone);
end;

procedure TFhirRelatedPerson.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('patient');
  fields.add('relationship');
  fields.add('name');
  fields.add('telecom');
  fields.add('gender');
  fields.add('birthDate');
  fields.add('address');
  fields.add('photo');
  fields.add('period');
end;

{ TFhirRelatedPerson }

Function TFhirRelatedPerson.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirRelatedPerson.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirRelatedPerson.SetPatient(value : TFhirReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirRelatedPerson.SetRelationship(value : TFhirCodeableConcept);
begin
  FRelationship.free;
  FRelationship := value;
end;

Procedure TFhirRelatedPerson.SetName(value : TFhirHumanName);
begin
  FName.free;
  FName := value;
end;

Function TFhirRelatedPerson.GetTelecomList : TFhirContactPointList;
begin
  if FTelecomList = nil then
    FTelecomList := TFhirContactPointList.Create;
  result := FTelecomList;
end;

Function TFhirRelatedPerson.GetHasTelecomList : boolean;
begin
  result := (FTelecomList <> nil) and (FTelecomList.count > 0);
end;

Procedure TFhirRelatedPerson.SetGender(value : TFhirEnum);
begin
  FGender.free;
  FGender := value;
end;

Function TFhirRelatedPerson.GetGenderST : TFhirAdministrativeGenderEnum;
begin
  if FGender = nil then
    result := TFhirAdministrativeGenderEnum(0)
  else
    result := TFhirAdministrativeGenderEnum(StringArrayIndexOfSensitive(CODES_TFhirAdministrativeGenderEnum, FGender.value));
end;

Procedure TFhirRelatedPerson.SetGenderST(value : TFhirAdministrativeGenderEnum);
begin
  if ord(value) = 0 then
    GenderElement := nil
  else
    GenderElement := TFhirEnum.create(SYSTEMS_TFhirAdministrativeGenderEnum[value], CODES_TFhirAdministrativeGenderEnum[value]);
end;

Procedure TFhirRelatedPerson.SetBirthDate(value : TFhirDate);
begin
  FBirthDate.free;
  FBirthDate := value;
end;

Function TFhirRelatedPerson.GetBirthDateST : TFslDateTime;
begin
  if FBirthDate = nil then
    result := TFslDateTime.makeNull
  else
    result := FBirthDate.value;
end;

Procedure TFhirRelatedPerson.SetBirthDateST(value : TFslDateTime);
begin
  if FBirthDate = nil then
    FBirthDate := TFhirDate.create;
  FBirthDate.value := value
end;

Function TFhirRelatedPerson.GetAddressList : TFhirAddressList;
begin
  if FAddressList = nil then
    FAddressList := TFhirAddressList.Create;
  result := FAddressList;
end;

Function TFhirRelatedPerson.GetHasAddressList : boolean;
begin
  result := (FAddressList <> nil) and (FAddressList.count > 0);
end;

Function TFhirRelatedPerson.GetPhotoList : TFhirAttachmentList;
begin
  if FPhotoList = nil then
    FPhotoList := TFhirAttachmentList.Create;
  result := FPhotoList;
end;

Function TFhirRelatedPerson.GetHasPhotoList : boolean;
begin
  result := (FPhotoList <> nil) and (FPhotoList.count > 0);
end;

Procedure TFhirRelatedPerson.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

function TFhirRelatedPerson.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FPatient.sizeInBytes);
  inc(result, FRelationship.sizeInBytes);
  inc(result, FName.sizeInBytes);
  inc(result, FtelecomList.sizeInBytes);
  inc(result, FGender.sizeInBytes);
  inc(result, FBirthDate.sizeInBytes);
  inc(result, FaddressList.sizeInBytes);
  inc(result, FphotoList.sizeInBytes);
  inc(result, FPeriod.sizeInBytes);
end;

{ TFhirRelatedPersonListEnumerator }

Constructor TFhirRelatedPersonListEnumerator.Create(list : TFhirRelatedPersonList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirRelatedPersonListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirRelatedPersonListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirRelatedPersonListEnumerator.GetCurrent : TFhirRelatedPerson;
begin
  Result := FList[FIndex];
end;

function TFhirRelatedPersonListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirRelatedPersonList }
procedure TFhirRelatedPersonList.AddItem(value: TFhirRelatedPerson);
begin
  assert(value.ClassName = 'TFhirRelatedPerson', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirRelatedPerson');
  add(value);
end;

function TFhirRelatedPersonList.Append: TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirRelatedPersonList.ClearItems;
begin
  Clear;
end;

function TFhirRelatedPersonList.GetEnumerator : TFhirRelatedPersonListEnumerator;
begin
  result := TFhirRelatedPersonListEnumerator.Create(self.link);
end;

function TFhirRelatedPersonList.Clone: TFhirRelatedPersonList;
begin
  result := TFhirRelatedPersonList(inherited Clone);
end;

function TFhirRelatedPersonList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirRelatedPersonList.GetItemN(index: Integer): TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson(ObjectByIndex[index]);
end;

function TFhirRelatedPersonList.ItemClass: TFslObjectClass;
begin
  result := TFhirRelatedPerson;
end;
function TFhirRelatedPersonList.IndexOf(value: TFhirRelatedPerson): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirRelatedPersonList.Insert(index: Integer): TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirRelatedPersonList.InsertItem(index: Integer; value: TFhirRelatedPerson);
begin
  assert(value is TFhirRelatedPerson);
  Inherited Insert(index, value);
end;

function TFhirRelatedPersonList.Item(index: Integer): TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson(ObjectByIndex[index]);
end;

function TFhirRelatedPersonList.Link: TFhirRelatedPersonList;
begin
  result := TFhirRelatedPersonList(inherited Link);
end;

procedure TFhirRelatedPersonList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirRelatedPersonList.SetItemByIndex(index: Integer; value: TFhirRelatedPerson);
begin
  assert(value is TFhirRelatedPerson);
  FhirRelatedPeople[index] := value;
end;

procedure TFhirRelatedPersonList.SetItemN(index: Integer; value: TFhirRelatedPerson);
begin
  assert(value is TFhirRelatedPerson);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_RELATEDPERSON}

{$IFDEF FHIR_SCHEDULE}

{ TFhirSchedule }

constructor TFhirSchedule.Create;
begin
  inherited;
end;

destructor TFhirSchedule.Destroy;
begin
  FIdentifierList.Free;
  FType_List.Free;
  FActor.free;
  FPlanningHorizon.free;
  FComment.free;
  inherited;
end;

function TFhirSchedule.GetResourceType : TFhirResourceType;
begin
  result := frtSchedule;
end;

procedure TFhirSchedule.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirSchedule(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirSchedule(oSource).FIdentifierList);
  end;
  if (TFhirSchedule(oSource).FType_List = nil) then
  begin
    FType_List.free;
    FType_List := nil;
  end
  else
  begin
    if FType_List = nil then
      FType_List := TFhirCodeableConceptList.Create;
    FType_List.Assign(TFhirSchedule(oSource).FType_List);
  end;
  actor := TFhirSchedule(oSource).actor.Clone;
  planningHorizon := TFhirSchedule(oSource).planningHorizon.Clone;
  commentElement := TFhirSchedule(oSource).commentElement.Clone;
end;

procedure TFhirSchedule.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'type') Then
    list.addAll(self, 'type', FType_List);
  if (child_name = 'actor') Then
     list.add(self.link, 'actor', FActor.Link);
  if (child_name = 'planningHorizon') Then
     list.add(self.link, 'planningHorizon', FPlanningHorizon.Link);
  if (child_name = 'comment') Then
     list.add(self.link, 'comment', FComment.Link);
end;

procedure TFhirSchedule.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', true, TFhirCodeableConcept, FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'actor', 'Reference(Patient|Practitioner|RelatedPerson|Device|HealthcareService|Location)', false, TFhirReference{Resource}, FActor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'planningHorizon', 'Period', false, TFhirPeriod, FPlanningHorizon.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comment', 'string', false, TFhirString, FComment.Link));{2}
end;

function TFhirSchedule.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'type') then
  begin
    Type_List.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'actor') then
  begin
    Actor := propValue as TFhirReference{Resource}{4b};
    result := propValue;
  end
  else if (propName = 'planningHorizon') then
  begin
    PlanningHorizon := propValue as TFhirPeriod{4b};
    result := propValue;
  end
  else if (propName = 'comment') then
  begin
    CommentElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirSchedule.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'type') then Type_List.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else inherited;
end;

function TFhirSchedule.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'type') then result := Type_List.new(){2}
  else if (propName = 'actor') then result := TFhirReference{Resource}.create(){4b}
  else if (propName = 'planningHorizon') then result := TFhirPeriod.create(){4b}
  else if (propName = 'comment') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirSchedule.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'actor') then result := 'Reference'
  else if (propName = 'planningHorizon') then result := 'Period'
  else if (propName = 'comment') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirSchedule.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'type') then deletePropertyValue('type', Type_List, value) {2}
  else if (propName = 'actor') then ActorElement := nil
  else if (propName = 'planningHorizon') then PlanningHorizonElement := nil
  else if (propName = 'comment') then CommentElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirSchedule.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'type') then replacePropertyValue('type', Type_List, existing, new) {2}
  else if (propName = 'actor') then ActorElement := new as TFhirReference{Resource}{4}
  else if (propName = 'planningHorizon') then PlanningHorizonElement := new as TFhirPeriod{4}
  else if (propName = 'comment') then CommentElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirSchedule.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'type') then Type_List.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirSchedule.fhirType : string;
begin
  result := 'Schedule';
end;

function TFhirSchedule.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(Ftype_List) and isEmptyProp(FActor) and isEmptyProp(FPlanningHorizon) and isEmptyProp(FComment);
end;

function TFhirSchedule.equals(other : TObject) : boolean;
var
  o : TFhirSchedule;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirSchedule)) then
    result := false
  else
  begin
    o := TFhirSchedule(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(type_List, o.type_List, true) and
      compareDeep(actorElement, o.actorElement, true) and compareDeep(planningHorizonElement, o.planningHorizonElement, true) and
      compareDeep(commentElement, o.commentElement, true);
  end;
end;

function TFhirSchedule.Link : TFhirSchedule;
begin
  result := TFhirSchedule(inherited Link);
end;

function TFhirSchedule.Clone : TFhirSchedule;
begin
  result := TFhirSchedule(inherited Clone);
end;

procedure TFhirSchedule.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('type');
  fields.add('actor');
  fields.add('planningHorizon');
  fields.add('comment');
end;

{ TFhirSchedule }

Function TFhirSchedule.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirSchedule.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Function TFhirSchedule.GetType_List : TFhirCodeableConceptList;
begin
  if FType_List = nil then
    FType_List := TFhirCodeableConceptList.Create;
  result := FType_List;
end;

Function TFhirSchedule.GetHasType_List : boolean;
begin
  result := (FType_List <> nil) and (FType_List.count > 0);
end;

Procedure TFhirSchedule.SetActor(value : TFhirReference{Resource});
begin
  FActor.free;
  FActor := value;
end;

Procedure TFhirSchedule.SetPlanningHorizon(value : TFhirPeriod);
begin
  FPlanningHorizon.free;
  FPlanningHorizon := value;
end;

Procedure TFhirSchedule.SetComment(value : TFhirString);
begin
  FComment.free;
  FComment := value;
end;

Function TFhirSchedule.GetCommentST : String;
begin
  if FComment = nil then
    result := ''
  else
    result := FComment.value;
end;

Procedure TFhirSchedule.SetCommentST(value : String);
begin
  if value <> '' then
  begin
    if FComment = nil then
      FComment := TFhirString.create;
    FComment.value := value
  end
  else if FComment <> nil then
    FComment.value := '';
end;

function TFhirSchedule.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, Ftype_List.sizeInBytes);
  inc(result, FActor.sizeInBytes);
  inc(result, FPlanningHorizon.sizeInBytes);
  inc(result, FComment.sizeInBytes);
end;

{ TFhirScheduleListEnumerator }

Constructor TFhirScheduleListEnumerator.Create(list : TFhirScheduleList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirScheduleListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirScheduleListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirScheduleListEnumerator.GetCurrent : TFhirSchedule;
begin
  Result := FList[FIndex];
end;

function TFhirScheduleListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirScheduleList }
procedure TFhirScheduleList.AddItem(value: TFhirSchedule);
begin
  assert(value.ClassName = 'TFhirSchedule', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirSchedule');
  add(value);
end;

function TFhirScheduleList.Append: TFhirSchedule;
begin
  result := TFhirSchedule.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirScheduleList.ClearItems;
begin
  Clear;
end;

function TFhirScheduleList.GetEnumerator : TFhirScheduleListEnumerator;
begin
  result := TFhirScheduleListEnumerator.Create(self.link);
end;

function TFhirScheduleList.Clone: TFhirScheduleList;
begin
  result := TFhirScheduleList(inherited Clone);
end;

function TFhirScheduleList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirScheduleList.GetItemN(index: Integer): TFhirSchedule;
begin
  result := TFhirSchedule(ObjectByIndex[index]);
end;

function TFhirScheduleList.ItemClass: TFslObjectClass;
begin
  result := TFhirSchedule;
end;
function TFhirScheduleList.IndexOf(value: TFhirSchedule): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirScheduleList.Insert(index: Integer): TFhirSchedule;
begin
  result := TFhirSchedule.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirScheduleList.InsertItem(index: Integer; value: TFhirSchedule);
begin
  assert(value is TFhirSchedule);
  Inherited Insert(index, value);
end;

function TFhirScheduleList.Item(index: Integer): TFhirSchedule;
begin
  result := TFhirSchedule(ObjectByIndex[index]);
end;

function TFhirScheduleList.Link: TFhirScheduleList;
begin
  result := TFhirScheduleList(inherited Link);
end;

procedure TFhirScheduleList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirScheduleList.SetItemByIndex(index: Integer; value: TFhirSchedule);
begin
  assert(value is TFhirSchedule);
  FhirSchedules[index] := value;
end;

procedure TFhirScheduleList.SetItemN(index: Integer; value: TFhirSchedule);
begin
  assert(value is TFhirSchedule);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_SCHEDULE}

{$IFDEF FHIR_SLOT}

{ TFhirSlot }

constructor TFhirSlot.Create;
begin
  inherited;
end;

destructor TFhirSlot.Destroy;
begin
  FIdentifierList.Free;
  FType_.free;
  FSchedule.free;
  FFreeBusyType.free;
  FStart.free;
  FEnd_.free;
  FOverbooked.free;
  FComment.free;
  inherited;
end;

function TFhirSlot.GetResourceType : TFhirResourceType;
begin
  result := frtSlot;
end;

procedure TFhirSlot.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirSlot(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirSlot(oSource).FIdentifierList);
  end;
  type_ := TFhirSlot(oSource).type_.Clone;
  schedule := TFhirSlot(oSource).schedule.Clone;
  FFreeBusyType := TFhirSlot(oSource).FFreeBusyType.Link;
  startElement := TFhirSlot(oSource).startElement.Clone;
  end_Element := TFhirSlot(oSource).end_Element.Clone;
  overbookedElement := TFhirSlot(oSource).overbookedElement.Clone;
  commentElement := TFhirSlot(oSource).commentElement.Clone;
end;

procedure TFhirSlot.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'type') Then
     list.add(self.link, 'type', FType_.Link);
  if (child_name = 'schedule') Then
     list.add(self.link, 'schedule', FSchedule.Link);
  if (child_name = 'freeBusyType') Then
     list.add(self.link, 'freeBusyType', FFreeBusyType.Link);
  if (child_name = 'start') Then
     list.add(self.link, 'start', FStart.Link);
  if (child_name = 'end') Then
     list.add(self.link, 'end', FEnd_.Link);
  if (child_name = 'overbooked') Then
     list.add(self.link, 'overbooked', FOverbooked.Link);
  if (child_name = 'comment') Then
     list.add(self.link, 'comment', FComment.Link);
end;

procedure TFhirSlot.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', false, TFhirCodeableConcept, FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'schedule', 'Reference(Schedule)', false, TFhirReference{TFhirSchedule}, FSchedule.Link));{2}
  oList.add(TFHIRProperty.create(self, 'freeBusyType', 'code', false, TFHIREnum, FFreeBusyType.Link));{1}
  oList.add(TFHIRProperty.create(self, 'start', 'instant', false, TFhirInstant, FStart.Link));{2}
  oList.add(TFHIRProperty.create(self, 'end', 'instant', false, TFhirInstant, FEnd_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'overbooked', 'boolean', false, TFhirBoolean, FOverbooked.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comment', 'string', false, TFhirString, FComment.Link));{2}
end;

function TFhirSlot.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'type') then
  begin
    Type_ := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'schedule') then
  begin
    Schedule := propValue as TFhirReference{TFhirSchedule}{4b};
    result := propValue;
  end
  else if (propName = 'freeBusyType') then
  begin
    FreeBusyTypeElement := asEnum(SYSTEMS_TFhirSlotstatusEnum, CODES_TFhirSlotstatusEnum, propValue);
    result := propValue
  end
  else if (propName = 'start') then
  begin
    StartElement := asInstant(propValue){5a};
    result := propValue;
  end
  else if (propName = 'end') then
  begin
    End_Element := asInstant(propValue){5a};
    result := propValue;
  end
  else if (propName = 'overbooked') then
  begin
    OverbookedElement := asBoolean(propValue){5a};
    result := propValue;
  end
  else if (propName = 'comment') then
  begin
    CommentElement := asString(propValue){5a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirSlot.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else inherited;
end;

function TFhirSlot.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'type') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'schedule') then result := TFhirReference{TFhirSchedule}.create(){4b}
  else if (propName = 'start') then result := TFhirInstant.create() {5b}
  else if (propName = 'end') then result := TFhirInstant.create() {5b}
  else if (propName = 'overbooked') then result := TFhirBoolean.create() {5b}
  else if (propName = 'comment') then result := TFhirString.create() {5b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirSlot.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'type') then result := 'CodeableConcept'
  else if (propName = 'schedule') then result := 'Reference'
  else if (propName = 'freeBusyType') then result := 'code'
  else if (propName = 'start') then result := 'instant'
  else if (propName = 'end') then result := 'instant'
  else if (propName = 'overbooked') then result := 'boolean'
  else if (propName = 'comment') then result := 'string'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirSlot.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'type') then Type_Element := nil
  else if (propName = 'schedule') then ScheduleElement := nil
  else if (propName = 'freeBusyType') then FreeBusyTypeElement := nil
  else if (propName = 'start') then StartElement := nil
  else if (propName = 'end') then End_Element := nil
  else if (propName = 'overbooked') then OverbookedElement := nil
  else if (propName = 'comment') then CommentElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirSlot.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'type') then Type_Element := new as TFhirCodeableConcept{4}
  else if (propName = 'schedule') then ScheduleElement := new as TFhirReference{TFhirSchedule}{4}
  else if (propName = 'freeBusyType') then FreeBusyTypeElement := asEnum(SYSTEMS_TFhirSlotstatusEnum, CODES_TFhirSlotstatusEnum, new){4}
  else if (propName = 'start') then StartElement := asInstant(new){5b}
  else if (propName = 'end') then End_Element := asInstant(new){5b}
  else if (propName = 'overbooked') then OverbookedElement := asBoolean(new){5b}
  else if (propName = 'comment') then CommentElement := asString(new){5b}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirSlot.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirSlot.fhirType : string;
begin
  result := 'Slot';
end;

function TFhirSlot.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FType_) and isEmptyProp(FSchedule) and isEmptyProp(FFreeBusyType) and isEmptyProp(FStart) and isEmptyProp(FEnd_) and isEmptyProp(FOverbooked) and isEmptyProp(FComment);
end;

function TFhirSlot.equals(other : TObject) : boolean;
var
  o : TFhirSlot;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirSlot)) then
    result := false
  else
  begin
    o := TFhirSlot(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(type_Element, o.type_Element, true) and
      compareDeep(scheduleElement, o.scheduleElement, true) and compareDeep(freeBusyTypeElement, o.freeBusyTypeElement, true) and
      compareDeep(startElement, o.startElement, true) and compareDeep(end_Element, o.end_Element, true) and
      compareDeep(overbookedElement, o.overbookedElement, true) and compareDeep(commentElement, o.commentElement, true);
  end;
end;

function TFhirSlot.Link : TFhirSlot;
begin
  result := TFhirSlot(inherited Link);
end;

function TFhirSlot.Clone : TFhirSlot;
begin
  result := TFhirSlot(inherited Clone);
end;

procedure TFhirSlot.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('type');
  fields.add('schedule');
  fields.add('freeBusyType');
  fields.add('start');
  fields.add('end');
  fields.add('overbooked');
  fields.add('comment');
end;

{ TFhirSlot }

Function TFhirSlot.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirSlot.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Procedure TFhirSlot.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirSlot.SetSchedule(value : TFhirReference{TFhirSchedule});
begin
  FSchedule.free;
  FSchedule := value;
end;

Procedure TFhirSlot.SetFreeBusyType(value : TFhirEnum);
begin
  FFreeBusyType.free;
  FFreeBusyType := value;
end;

Function TFhirSlot.GetFreeBusyTypeST : TFhirSlotstatusEnum;
begin
  if FFreeBusyType = nil then
    result := TFhirSlotstatusEnum(0)
  else
    result := TFhirSlotstatusEnum(StringArrayIndexOfSensitive(CODES_TFhirSlotstatusEnum, FFreeBusyType.value));
end;

Procedure TFhirSlot.SetFreeBusyTypeST(value : TFhirSlotstatusEnum);
begin
  if ord(value) = 0 then
    FreeBusyTypeElement := nil
  else
    FreeBusyTypeElement := TFhirEnum.create(SYSTEMS_TFhirSlotstatusEnum[value], CODES_TFhirSlotstatusEnum[value]);
end;

Procedure TFhirSlot.SetStart(value : TFhirInstant);
begin
  FStart.free;
  FStart := value;
end;

Function TFhirSlot.GetStartST : TFslDateTime;
begin
  if FStart = nil then
    result := TFslDateTime.makeNull
  else
    result := FStart.value;
end;

Procedure TFhirSlot.SetStartST(value : TFslDateTime);
begin
  if FStart = nil then
    FStart := TFhirInstant.create;
  FStart.value := value
end;

Procedure TFhirSlot.SetEnd_(value : TFhirInstant);
begin
  FEnd_.free;
  FEnd_ := value;
end;

Function TFhirSlot.GetEnd_ST : TFslDateTime;
begin
  if FEnd_ = nil then
    result := TFslDateTime.makeNull
  else
    result := FEnd_.value;
end;

Procedure TFhirSlot.SetEnd_ST(value : TFslDateTime);
begin
  if FEnd_ = nil then
    FEnd_ := TFhirInstant.create;
  FEnd_.value := value
end;

Procedure TFhirSlot.SetOverbooked(value : TFhirBoolean);
begin
  FOverbooked.free;
  FOverbooked := value;
end;

Function TFhirSlot.GetOverbookedST : Boolean;
begin
  if FOverbooked = nil then
    result := false
  else
    result := FOverbooked.value;
end;

Procedure TFhirSlot.SetOverbookedST(value : Boolean);
begin
  if FOverbooked = nil then
    FOverbooked := TFhirBoolean.create;
  FOverbooked.value := value
end;

Procedure TFhirSlot.SetComment(value : TFhirString);
begin
  FComment.free;
  FComment := value;
end;

Function TFhirSlot.GetCommentST : String;
begin
  if FComment = nil then
    result := ''
  else
    result := FComment.value;
end;

Procedure TFhirSlot.SetCommentST(value : String);
begin
  if value <> '' then
  begin
    if FComment = nil then
      FComment := TFhirString.create;
    FComment.value := value
  end
  else if FComment <> nil then
    FComment.value := '';
end;

function TFhirSlot.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FType_.sizeInBytes);
  inc(result, FSchedule.sizeInBytes);
  inc(result, FFreeBusyType.sizeInBytes);
  inc(result, FStart.sizeInBytes);
  inc(result, FEnd_.sizeInBytes);
  inc(result, FOverbooked.sizeInBytes);
  inc(result, FComment.sizeInBytes);
end;

{ TFhirSlotListEnumerator }

Constructor TFhirSlotListEnumerator.Create(list : TFhirSlotList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirSlotListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirSlotListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirSlotListEnumerator.GetCurrent : TFhirSlot;
begin
  Result := FList[FIndex];
end;

function TFhirSlotListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirSlotList }
procedure TFhirSlotList.AddItem(value: TFhirSlot);
begin
  assert(value.ClassName = 'TFhirSlot', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirSlot');
  add(value);
end;

function TFhirSlotList.Append: TFhirSlot;
begin
  result := TFhirSlot.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSlotList.ClearItems;
begin
  Clear;
end;

function TFhirSlotList.GetEnumerator : TFhirSlotListEnumerator;
begin
  result := TFhirSlotListEnumerator.Create(self.link);
end;

function TFhirSlotList.Clone: TFhirSlotList;
begin
  result := TFhirSlotList(inherited Clone);
end;

function TFhirSlotList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirSlotList.GetItemN(index: Integer): TFhirSlot;
begin
  result := TFhirSlot(ObjectByIndex[index]);
end;

function TFhirSlotList.ItemClass: TFslObjectClass;
begin
  result := TFhirSlot;
end;
function TFhirSlotList.IndexOf(value: TFhirSlot): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirSlotList.Insert(index: Integer): TFhirSlot;
begin
  result := TFhirSlot.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSlotList.InsertItem(index: Integer; value: TFhirSlot);
begin
  assert(value is TFhirSlot);
  Inherited Insert(index, value);
end;

function TFhirSlotList.Item(index: Integer): TFhirSlot;
begin
  result := TFhirSlot(ObjectByIndex[index]);
end;

function TFhirSlotList.Link: TFhirSlotList;
begin
  result := TFhirSlotList(inherited Link);
end;

procedure TFhirSlotList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirSlotList.SetItemByIndex(index: Integer; value: TFhirSlot);
begin
  assert(value is TFhirSlot);
  FhirSlots[index] := value;
end;

procedure TFhirSlotList.SetItemN(index: Integer; value: TFhirSlot);
begin
  assert(value is TFhirSlot);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_SLOT}

{$IFDEF FHIR_SUBSTANCE}

{ TFhirSubstanceInstance }

constructor TFhirSubstanceInstance.Create;
begin
  inherited;
end;

destructor TFhirSubstanceInstance.Destroy;
begin
  FIdentifier.free;
  FExpiry.free;
  FQuantity.free;
  inherited;
end;

procedure TFhirSubstanceInstance.Assign(oSource : TFslObject);
begin
  inherited;
  identifier := TFhirSubstanceInstance(oSource).identifier.Clone;
  expiryElement := TFhirSubstanceInstance(oSource).expiryElement.Clone;
  quantity := TFhirSubstanceInstance(oSource).quantity.Clone;
end;

procedure TFhirSubstanceInstance.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(self.link, 'identifier', FIdentifier.Link);
  if (child_name = 'expiry') Then
     list.add(self.link, 'expiry', FExpiry.Link);
  if (child_name = 'quantity') Then
     list.add(self.link, 'quantity', FQuantity.Link);
end;

procedure TFhirSubstanceInstance.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', false, TFhirIdentifier, FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'expiry', 'dateTime', false, TFhirDateTime, FExpiry.Link));{2}
  oList.add(TFHIRProperty.create(self, 'quantity', 'Quantity', false, TFhirQuantity, FQuantity.Link));{2}
end;

function TFhirSubstanceInstance.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    Identifier := propValue as TFhirIdentifier{4b};
    result := propValue;
  end
  else if (propName = 'expiry') then
  begin
    ExpiryElement := asDateTime(propValue){5a};
    result := propValue;
  end
  else if (propName = 'quantity') then
  begin
    Quantity := propValue as TFhirQuantity{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirSubstanceInstance.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirSubstanceInstance.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := TFhirIdentifier.create(){4b}
  else if (propName = 'expiry') then result := TFhirDateTime.create() {5b}
  else if (propName = 'quantity') then result := TFhirQuantity.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirSubstanceInstance.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'expiry') then result := 'dateTime'
  else if (propName = 'quantity') then result := 'Quantity'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirSubstanceInstance.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierElement := nil
  else if (propName = 'expiry') then ExpiryElement := nil
  else if (propName = 'quantity') then QuantityElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirSubstanceInstance.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierElement := new as TFhirIdentifier{4}
  else if (propName = 'expiry') then ExpiryElement := asDateTime(new){5b}
  else if (propName = 'quantity') then QuantityElement := new as TFhirQuantity{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirSubstanceInstance.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirSubstanceInstance.fhirType : string;
begin
  result := 'instance';
end;

function TFhirSubstanceInstance.Link : TFhirSubstanceInstance;
begin
  result := TFhirSubstanceInstance(inherited Link);
end;

function TFhirSubstanceInstance.Clone : TFhirSubstanceInstance;
begin
  result := TFhirSubstanceInstance(inherited Clone);
end;

function TFhirSubstanceInstance.equals(other : TObject) : boolean;
var
  o : TFhirSubstanceInstance;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirSubstanceInstance)) then
    result := false
  else
  begin
    o := TFhirSubstanceInstance(other);
    result := compareDeep(identifierElement, o.identifierElement, true) and compareDeep(expiryElement, o.expiryElement, true) and
      compareDeep(quantityElement, o.quantityElement, true);
  end;
end;

function TFhirSubstanceInstance.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FIdentifier) and isEmptyProp(FExpiry) and isEmptyProp(FQuantity);
end;

procedure TFhirSubstanceInstance.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('expiry');
  fields.add('quantity');
end;

{ TFhirSubstanceInstance }

Procedure TFhirSubstanceInstance.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirSubstanceInstance.SetExpiry(value : TFhirDateTime);
begin
  FExpiry.free;
  FExpiry := value;
end;

Function TFhirSubstanceInstance.GetExpiryST : TFslDateTime;
begin
  if FExpiry = nil then
    result := TFslDateTime.makeNull
  else
    result := FExpiry.value;
end;

Procedure TFhirSubstanceInstance.SetExpiryST(value : TFslDateTime);
begin
  if FExpiry = nil then
    FExpiry := TFhirDateTime.create;
  FExpiry.value := value
end;

Procedure TFhirSubstanceInstance.SetQuantity(value : TFhirQuantity);
begin
  FQuantity.free;
  FQuantity := value;
end;

function TFhirSubstanceInstance.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FIdentifier.sizeInBytes);
  inc(result, FExpiry.sizeInBytes);
  inc(result, FQuantity.sizeInBytes);
end;

{ TFhirSubstanceInstanceListEnumerator }

Constructor TFhirSubstanceInstanceListEnumerator.Create(list : TFhirSubstanceInstanceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirSubstanceInstanceListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirSubstanceInstanceListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirSubstanceInstanceListEnumerator.GetCurrent : TFhirSubstanceInstance;
begin
  Result := FList[FIndex];
end;

function TFhirSubstanceInstanceListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirSubstanceInstanceList }
procedure TFhirSubstanceInstanceList.AddItem(value: TFhirSubstanceInstance);
begin
  assert(value.ClassName = 'TFhirSubstanceInstance', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirSubstanceInstance');
  add(value);
end;

function TFhirSubstanceInstanceList.Append: TFhirSubstanceInstance;
begin
  result := TFhirSubstanceInstance.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSubstanceInstanceList.ClearItems;
begin
  Clear;
end;

function TFhirSubstanceInstanceList.GetEnumerator : TFhirSubstanceInstanceListEnumerator;
begin
  result := TFhirSubstanceInstanceListEnumerator.Create(self.link);
end;

function TFhirSubstanceInstanceList.Clone: TFhirSubstanceInstanceList;
begin
  result := TFhirSubstanceInstanceList(inherited Clone);
end;

function TFhirSubstanceInstanceList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirSubstanceInstanceList.GetItemN(index: Integer): TFhirSubstanceInstance;
begin
  result := TFhirSubstanceInstance(ObjectByIndex[index]);
end;

function TFhirSubstanceInstanceList.ItemClass: TFslObjectClass;
begin
  result := TFhirSubstanceInstance;
end;
function TFhirSubstanceInstanceList.IndexOf(value: TFhirSubstanceInstance): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirSubstanceInstanceList.Insert(index: Integer): TFhirSubstanceInstance;
begin
  result := TFhirSubstanceInstance.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSubstanceInstanceList.InsertItem(index: Integer; value: TFhirSubstanceInstance);
begin
  assert(value is TFhirSubstanceInstance);
  Inherited Insert(index, value);
end;

function TFhirSubstanceInstanceList.Item(index: Integer): TFhirSubstanceInstance;
begin
  result := TFhirSubstanceInstance(ObjectByIndex[index]);
end;

function TFhirSubstanceInstanceList.Link: TFhirSubstanceInstanceList;
begin
  result := TFhirSubstanceInstanceList(inherited Link);
end;

procedure TFhirSubstanceInstanceList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirSubstanceInstanceList.SetItemByIndex(index: Integer; value: TFhirSubstanceInstance);
begin
  assert(value is TFhirSubstanceInstance);
  FhirSubstanceInstances[index] := value;
end;

procedure TFhirSubstanceInstanceList.SetItemN(index: Integer; value: TFhirSubstanceInstance);
begin
  assert(value is TFhirSubstanceInstance);
  ObjectByIndex[index] := value;
end;

{ TFhirSubstanceIngredient }

constructor TFhirSubstanceIngredient.Create;
begin
  inherited;
end;

destructor TFhirSubstanceIngredient.Destroy;
begin
  FQuantity.free;
  FSubstance.free;
  inherited;
end;

procedure TFhirSubstanceIngredient.Assign(oSource : TFslObject);
begin
  inherited;
  quantity := TFhirSubstanceIngredient(oSource).quantity.Clone;
  substance := TFhirSubstanceIngredient(oSource).substance.Clone;
end;

procedure TFhirSubstanceIngredient.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'quantity') Then
     list.add(self.link, 'quantity', FQuantity.Link);
  if (child_name = 'substance') Then
     list.add(self.link, 'substance', FSubstance.Link);
end;

procedure TFhirSubstanceIngredient.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'quantity', 'Ratio', false, TFhirRatio, FQuantity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'substance', 'Reference(Substance)', false, TFhirReference{TFhirSubstance}, FSubstance.Link));{2}
end;

function TFhirSubstanceIngredient.setProperty(propName : string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'quantity') then
  begin
    Quantity := propValue as TFhirRatio{4b};
    result := propValue;
  end
  else if (propName = 'substance') then
  begin
    Substance := propValue as TFhirReference{TFhirSubstance}{4b};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);
end;

procedure TFhirSubstanceIngredient.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  inherited;
end;

function TFhirSubstanceIngredient.createPropertyValue(propName : string) : TFHIRObject;
begin
  if (propName = 'quantity') then result := TFhirRatio.create(){4b}
  else if (propName = 'substance') then result := TFhirReference{TFhirSubstance}.create(){4b}
  else result := inherited createPropertyValue(propName);
end;

function TFhirSubstanceIngredient.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'quantity') then result := 'Ratio'
  else if (propName = 'substance') then result := 'Reference'
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirSubstanceIngredient.deleteProperty(propName: string; value : TFHIRObject);
begin
  if (propName = 'quantity') then QuantityElement := nil
  else if (propName = 'substance') then SubstanceElement := nil
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirSubstanceIngredient.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'quantity') then QuantityElement := new as TFhirRatio{4}
  else if (propName = 'substance') then SubstanceElement := new as TFhirReference{TFhirSubstance}{4}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirSubstanceIngredient.reorderProperty(propName : string; source, destination : integer);
begin
  inherited reorderProperty(propName, source, destination);
end;

function TFhirSubstanceIngredient.fhirType : string;
begin
  result := 'ingredient';
end;

function TFhirSubstanceIngredient.Link : TFhirSubstanceIngredient;
begin
  result := TFhirSubstanceIngredient(inherited Link);
end;

function TFhirSubstanceIngredient.Clone : TFhirSubstanceIngredient;
begin
  result := TFhirSubstanceIngredient(inherited Clone);
end;

function TFhirSubstanceIngredient.equals(other : TObject) : boolean;
var
  o : TFhirSubstanceIngredient;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirSubstanceIngredient)) then
    result := false
  else
  begin
    o := TFhirSubstanceIngredient(other);
    result := compareDeep(quantityElement, o.quantityElement, true) and compareDeep(substanceElement, o.substanceElement, true);
  end;
end;

function TFhirSubstanceIngredient.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FQuantity) and isEmptyProp(FSubstance);
end;

procedure TFhirSubstanceIngredient.listFieldsInOrder(fields : TStringList);
begin
  listBackboneElementFieldsInOrder(fields);
  fields.add('quantity');
  fields.add('substance');
end;

{ TFhirSubstanceIngredient }

Procedure TFhirSubstanceIngredient.SetQuantity(value : TFhirRatio);
begin
  FQuantity.free;
  FQuantity := value;
end;

Procedure TFhirSubstanceIngredient.SetSubstance(value : TFhirReference{TFhirSubstance});
begin
  FSubstance.free;
  FSubstance := value;
end;

function TFhirSubstanceIngredient.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FQuantity.sizeInBytes);
  inc(result, FSubstance.sizeInBytes);
end;

{ TFhirSubstanceIngredientListEnumerator }

Constructor TFhirSubstanceIngredientListEnumerator.Create(list : TFhirSubstanceIngredientList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirSubstanceIngredientListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirSubstanceIngredientListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirSubstanceIngredientListEnumerator.GetCurrent : TFhirSubstanceIngredient;
begin
  Result := FList[FIndex];
end;

function TFhirSubstanceIngredientListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirSubstanceIngredientList }
procedure TFhirSubstanceIngredientList.AddItem(value: TFhirSubstanceIngredient);
begin
  assert(value.ClassName = 'TFhirSubstanceIngredient', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirSubstanceIngredient');
  add(value);
end;

function TFhirSubstanceIngredientList.Append: TFhirSubstanceIngredient;
begin
  result := TFhirSubstanceIngredient.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSubstanceIngredientList.ClearItems;
begin
  Clear;
end;

function TFhirSubstanceIngredientList.GetEnumerator : TFhirSubstanceIngredientListEnumerator;
begin
  result := TFhirSubstanceIngredientListEnumerator.Create(self.link);
end;

function TFhirSubstanceIngredientList.Clone: TFhirSubstanceIngredientList;
begin
  result := TFhirSubstanceIngredientList(inherited Clone);
end;

function TFhirSubstanceIngredientList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirSubstanceIngredientList.GetItemN(index: Integer): TFhirSubstanceIngredient;
begin
  result := TFhirSubstanceIngredient(ObjectByIndex[index]);
end;

function TFhirSubstanceIngredientList.ItemClass: TFslObjectClass;
begin
  result := TFhirSubstanceIngredient;
end;
function TFhirSubstanceIngredientList.IndexOf(value: TFhirSubstanceIngredient): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirSubstanceIngredientList.Insert(index: Integer): TFhirSubstanceIngredient;
begin
  result := TFhirSubstanceIngredient.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSubstanceIngredientList.InsertItem(index: Integer; value: TFhirSubstanceIngredient);
begin
  assert(value is TFhirSubstanceIngredient);
  Inherited Insert(index, value);
end;

function TFhirSubstanceIngredientList.Item(index: Integer): TFhirSubstanceIngredient;
begin
  result := TFhirSubstanceIngredient(ObjectByIndex[index]);
end;

function TFhirSubstanceIngredientList.Link: TFhirSubstanceIngredientList;
begin
  result := TFhirSubstanceIngredientList(inherited Link);
end;

procedure TFhirSubstanceIngredientList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirSubstanceIngredientList.SetItemByIndex(index: Integer; value: TFhirSubstanceIngredient);
begin
  assert(value is TFhirSubstanceIngredient);
  FhirSubstanceIngredients[index] := value;
end;

procedure TFhirSubstanceIngredientList.SetItemN(index: Integer; value: TFhirSubstanceIngredient);
begin
  assert(value is TFhirSubstanceIngredient);
  ObjectByIndex[index] := value;
end;

{ TFhirSubstance }

constructor TFhirSubstance.Create;
begin
  inherited;
end;

destructor TFhirSubstance.Destroy;
begin
  FIdentifierList.Free;
  FCategoryList.Free;
  FCode.free;
  FDescription.free;
  FInstanceList.Free;
  FIngredientList.Free;
  inherited;
end;

function TFhirSubstance.GetResourceType : TFhirResourceType;
begin
  result := frtSubstance;
end;

procedure TFhirSubstance.Assign(oSource : TFslObject);
begin
  inherited;
  if (TFhirSubstance(oSource).FIdentifierList = nil) then
  begin
    FIdentifierList.free;
    FIdentifierList := nil;
  end
  else
  begin
    if FIdentifierList = nil then
      FIdentifierList := TFhirIdentifierList.Create;
    FIdentifierList.Assign(TFhirSubstance(oSource).FIdentifierList);
  end;
  if (TFhirSubstance(oSource).FCategoryList = nil) then
  begin
    FCategoryList.free;
    FCategoryList := nil;
  end
  else
  begin
    if FCategoryList = nil then
      FCategoryList := TFhirCodeableConceptList.Create;
    FCategoryList.Assign(TFhirSubstance(oSource).FCategoryList);
  end;
  code := TFhirSubstance(oSource).code.Clone;
  descriptionElement := TFhirSubstance(oSource).descriptionElement.Clone;
  if (TFhirSubstance(oSource).FInstanceList = nil) then
  begin
    FInstanceList.free;
    FInstanceList := nil;
  end
  else
  begin
    if FInstanceList = nil then
      FInstanceList := TFhirSubstanceInstanceList.Create;
    FInstanceList.Assign(TFhirSubstance(oSource).FInstanceList);
  end;
  if (TFhirSubstance(oSource).FIngredientList = nil) then
  begin
    FIngredientList.free;
    FIngredientList := nil;
  end
  else
  begin
    if FIngredientList = nil then
      FIngredientList := TFhirSubstanceIngredientList.Create;
    FIngredientList.Assign(TFhirSubstance(oSource).FIngredientList);
  end;
end;

procedure TFhirSubstance.GetChildrenByName(child_name : string; list : TFHIRSelectionList);
begin
  inherited;
  if (child_name = 'identifier') Then
    list.addAll(self, 'identifier', FIdentifierList);
  if (child_name = 'category') Then
    list.addAll(self, 'category', FCategoryList);
  if (child_name = 'code') Then
     list.add(self.link, 'code', FCode.Link);
  if (child_name = 'description') Then
     list.add(self.link, 'description', FDescription.Link);
  if (child_name = 'instance') Then
    list.addAll(self, 'instance', FInstanceList);
  if (child_name = 'ingredient') Then
    list.addAll(self, 'ingredient', FIngredientList);
end;

procedure TFhirSubstance.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', true, TFhirIdentifier, FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'category', 'CodeableConcept', true, TFhirCodeableConcept, FCategoryList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', false, TFhirCodeableConcept, FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', false, TFhirString, FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'instance', '', true, TFhirSubstanceInstance, FInstanceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'ingredient', '', true, TFhirSubstanceIngredient, FIngredientList.Link)){3};
end;

function TFhirSubstance.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  if (propName = 'identifier') then
  begin
    IdentifierList.add(propValue as TFhirIdentifier){2a};
    result := propValue;
  end
  else if (propName = 'category') then
  begin
    CategoryList.add(propValue as TFhirCodeableConcept){2a};
    result := propValue;
  end
  else if (propName = 'code') then
  begin
    Code := propValue as TFhirCodeableConcept{4b};
    result := propValue;
  end
  else if (propName = 'description') then
  begin
    DescriptionElement := asString(propValue){5a};
    result := propValue;
  end
  else if (propName = 'instance') then
  begin
    InstanceList.add(propValue as TFhirSubstanceInstance){2a};
    result := propValue;
  end
  else if (propName = 'ingredient') then
  begin
    IngredientList.add(propValue as TFhirSubstanceIngredient){2a};
    result := propValue;
  end
  else result := inherited setProperty(propName, propValue);;
end;

procedure TFhirSubstance.insertProperty(propName: string; propValue: TFHIRObject; index : integer);
begin
  if (propName = 'identifier') then IdentifierList.insertItem(index, propValue as TFhirIdentifier){2a}
  else if (propName = 'category') then CategoryList.insertItem(index, propValue as TFhirCodeableConcept){2a}
  else if (propName = 'instance') then InstanceList.insertItem(index, propValue as TFhirSubstanceInstance){2a}
  else if (propName = 'ingredient') then IngredientList.insertItem(index, propValue as TFhirSubstanceIngredient){2a}
  else inherited;
end;

function TFhirSubstance.createPropertyValue(propName: string) : TFHIRObject;
begin
  if (propName = 'identifier') then result := IdentifierList.new(){2}
  else if (propName = 'category') then result := CategoryList.new(){2}
  else if (propName = 'code') then result := TFhirCodeableConcept.create(){4b}
  else if (propName = 'description') then result := TFhirString.create() {5b}
  else if (propName = 'instance') then result := InstanceList.new(){2}
  else if (propName = 'ingredient') then result := IngredientList.new(){2}
  else result := inherited createPropertyValue(propName);
end;

function TFhirSubstance.getTypesForProperty(propName: string) : String;
begin
  if (propName = 'identifier') then result := 'Identifier'
  else if (propName = 'category') then result := 'CodeableConcept'
  else if (propName = 'code') then result := 'CodeableConcept'
  else if (propName = 'description') then result := 'string'
  else if (propName = 'instance') then result := ''
  else if (propName = 'ingredient') then result := ''
  else result := inherited getTypesForProperty(propName);
end;

procedure TFhirSubstance.deleteProperty(propName : string; value : TFHIRObject);
begin
  if (propName = 'identifier') then deletePropertyValue('identifier', IdentifierList, value) {2}
  else if (propName = 'category') then deletePropertyValue('category', CategoryList, value) {2}
  else if (propName = 'code') then CodeElement := nil
  else if (propName = 'description') then DescriptionElement := nil
  else if (propName = 'instance') then deletePropertyValue('instance', InstanceList, value) {2}
  else if (propName = 'ingredient') then deletePropertyValue('ingredient', IngredientList, value) {2}
  else
    inherited deleteProperty(propName, value);
end;

procedure TFhirSubstance.replaceProperty(propName : string; existing, new : TFHIRObject);
begin
  if (propName = 'identifier') then replacePropertyValue('identifier', IdentifierList, existing, new) {2}
  else if (propName = 'category') then replacePropertyValue('category', CategoryList, existing, new) {2}
  else if (propName = 'code') then CodeElement := new as TFhirCodeableConcept{4}
  else if (propName = 'description') then DescriptionElement := asString(new){5b}
  else if (propName = 'instance') then replacePropertyValue('instance', InstanceList, existing, new) {2}
  else if (propName = 'ingredient') then replacePropertyValue('ingredient', IngredientList, existing, new) {2}
  else
    inherited replaceProperty(propName, existing, new);
end;

procedure TFhirSubstance.reorderProperty(propName : string; source, destination : integer);
begin
  if (propName = 'identifier') then IdentifierList.move(source, destination){2a}
  else if (propName = 'category') then CategoryList.move(source, destination){2a}
  else if (propName = 'instance') then InstanceList.move(source, destination){2a}
  else if (propName = 'ingredient') then IngredientList.move(source, destination){2a}
  else
    inherited reorderProperty(propName, source, destination);
end;

function TFhirSubstance.fhirType : string;
begin
  result := 'Substance';
end;

function TFhirSubstance.isEmpty : boolean;
begin
  result := inherited isEmpty  and isEmptyProp(FidentifierList) and isEmptyProp(FcategoryList) and isEmptyProp(FCode) and isEmptyProp(FDescription) and isEmptyProp(FinstanceList) and isEmptyProp(FingredientList);
end;

function TFhirSubstance.equals(other : TObject) : boolean;
var
  o : TFhirSubstance;
begin
  if (not inherited equals(other)) then
    result := false
  else if (not (other is TFhirSubstance)) then
    result := false
  else
  begin
    o := TFhirSubstance(other);
    result := compareDeep(identifierList, o.identifierList, true) and compareDeep(categoryList, o.categoryList, true) and
      compareDeep(codeElement, o.codeElement, true) and compareDeep(descriptionElement, o.descriptionElement, true) and
      compareDeep(instanceList, o.instanceList, true) and compareDeep(ingredientList, o.ingredientList, true);
  end;
end;

function TFhirSubstance.Link : TFhirSubstance;
begin
  result := TFhirSubstance(inherited Link);
end;

function TFhirSubstance.Clone : TFhirSubstance;
begin
  result := TFhirSubstance(inherited Clone);
end;

procedure TFhirSubstance.listFieldsInOrder(fields : TStringList);
begin
  listDomainResourceFieldsInOrder(fields);
  fields.add('identifier');
  fields.add('category');
  fields.add('code');
  fields.add('description');
  fields.add('instance');
  fields.add('ingredient');
end;

{ TFhirSubstance }

Function TFhirSubstance.GetIdentifierList : TFhirIdentifierList;
begin
  if FIdentifierList = nil then
    FIdentifierList := TFhirIdentifierList.Create;
  result := FIdentifierList;
end;

Function TFhirSubstance.GetHasIdentifierList : boolean;
begin
  result := (FIdentifierList <> nil) and (FIdentifierList.count > 0);
end;

Function TFhirSubstance.GetCategoryList : TFhirCodeableConceptList;
begin
  if FCategoryList = nil then
    FCategoryList := TFhirCodeableConceptList.Create;
  result := FCategoryList;
end;

Function TFhirSubstance.GetHasCategoryList : boolean;
begin
  result := (FCategoryList <> nil) and (FCategoryList.count > 0);
end;

Procedure TFhirSubstance.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

Procedure TFhirSubstance.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirSubstance.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := FDescription.value;
end;

Procedure TFhirSubstance.SetDescriptionST(value : String);
begin
  if value <> '' then
  begin
    if FDescription = nil then
      FDescription := TFhirString.create;
    FDescription.value := value
  end
  else if FDescription <> nil then
    FDescription.value := '';
end;

Function TFhirSubstance.GetInstanceList : TFhirSubstanceInstanceList;
begin
  if FInstanceList = nil then
    FInstanceList := TFhirSubstanceInstanceList.Create;
  result := FInstanceList;
end;

Function TFhirSubstance.GetHasInstanceList : boolean;
begin
  result := (FInstanceList <> nil) and (FInstanceList.count > 0);
end;

Function TFhirSubstance.GetIngredientList : TFhirSubstanceIngredientList;
begin
  if FIngredientList = nil then
    FIngredientList := TFhirSubstanceIngredientList.Create;
  result := FIngredientList;
end;

Function TFhirSubstance.GetHasIngredientList : boolean;
begin
  result := (FIngredientList <> nil) and (FIngredientList.count > 0);
end;

function TFhirSubstance.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FidentifierList.sizeInBytes);
  inc(result, FcategoryList.sizeInBytes);
  inc(result, FCode.sizeInBytes);
  inc(result, FDescription.sizeInBytes);
  inc(result, FinstanceList.sizeInBytes);
  inc(result, FingredientList.sizeInBytes);
end;

{ TFhirSubstanceListEnumerator }

Constructor TFhirSubstanceListEnumerator.Create(list : TFhirSubstanceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

destructor TFhirSubstanceListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirSubstanceListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirSubstanceListEnumerator.GetCurrent : TFhirSubstance;
begin
  Result := FList[FIndex];
end;

function TFhirSubstanceListEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FList.sizeInBytes);
end;

{ TFhirSubstanceList }
procedure TFhirSubstanceList.AddItem(value: TFhirSubstance);
begin
  assert(value.ClassName = 'TFhirSubstance', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirSubstance');
  add(value);
end;

function TFhirSubstanceList.Append: TFhirSubstance;
begin
  result := TFhirSubstance.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSubstanceList.ClearItems;
begin
  Clear;
end;

function TFhirSubstanceList.GetEnumerator : TFhirSubstanceListEnumerator;
begin
  result := TFhirSubstanceListEnumerator.Create(self.link);
end;

function TFhirSubstanceList.Clone: TFhirSubstanceList;
begin
  result := TFhirSubstanceList(inherited Clone);
end;

function TFhirSubstanceList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirSubstanceList.GetItemN(index: Integer): TFhirSubstance;
begin
  result := TFhirSubstance(ObjectByIndex[index]);
end;

function TFhirSubstanceList.ItemClass: TFslObjectClass;
begin
  result := TFhirSubstance;
end;
function TFhirSubstanceList.IndexOf(value: TFhirSubstance): Integer;
begin
  result := IndexByReference(value);
end;

function TFhirSubstanceList.Insert(index: Integer): TFhirSubstance;
begin
  result := TFhirSubstance.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFhirSubstanceList.InsertItem(index: Integer; value: TFhirSubstance);
begin
  assert(value is TFhirSubstance);
  Inherited Insert(index, value);
end;

function TFhirSubstanceList.Item(index: Integer): TFhirSubstance;
begin
  result := TFhirSubstance(ObjectByIndex[index]);
end;

function TFhirSubstanceList.Link: TFhirSubstanceList;
begin
  result := TFhirSubstanceList(inherited Link);
end;

procedure TFhirSubstanceList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirSubstanceList.SetItemByIndex(index: Integer; value: TFhirSubstance);
begin
  assert(value is TFhirSubstance);
  FhirSubstances[index] := value;
end;

procedure TFhirSubstanceList.SetItemN(index: Integer; value: TFhirSubstance);
begin
  assert(value is TFhirSubstance);
  ObjectByIndex[index] := value;
end;

{$ENDIF FHIR_SUBSTANCE}

end.

