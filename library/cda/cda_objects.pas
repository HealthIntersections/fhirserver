unit cda_objects;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)

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

// todo:
// assign attributes
// extensions:
//  asEntityIdentifier
//  Employment
//  asQualifications
// entitlement
// policy
// coverage
// brand substitute allowed

interface

uses
  SysUtils,
  fsl_base, fsl_collections,
  cda_base, cda_types, cda_narrative;

Type

  TcdaClinicalStatement = class;
  TcdaAct = class;
  TcdaAssignedAuthor = class;
  TcdaAssignedCustodian = class;
  TcdaAssignedEntity = class;
  TcdaAssociatedEntity = class;
  TcdaAuthenticator = class;
  TcdaAuthorChoice = class;
  TcdaAuthor = class;
  TcdaAuthoringDevice = class;
  TcdaAuthorization = class;
  TcdaBirthplace = class;
  TcdaClinicalDocument = class;
  TcdaComponent1 = class;
  TcdaComponent2 = class;
  TcdaComponentSect = class;
  TcdaComponent4 = class;
  TcdaConsent = class;
  TcdaConsumable = class;
  TcdaCriterion = class;
  TcdaCustodian = class;
  TcdaCustodianOrganization = class;
  TcdaDataEnterer = class;
  TcdaDevice = class;
  TcdaDocumentationOf = class;
  TcdaEncompassingEncounter = class;
  TcdaEncounter = class;
  TcdaEncounterParticipant = class;
  TcdaEntity = class;
  TcdaEntry = class;
  TcdaEntryRelationship = class;
  TcdaExternalActChoice = class;
  TcdaExternalAct = class;
  TcdaExternalDocument = class;
  TcdaExternalObservation = class;
  TcdaExternalProcedure = class;
  TcdaGuardian = class;
  TcdaHealthCareFacility = class;
  TcdaInformant12 = class;
  TcdaInformantChoice = class;
  TcdaInformationRecipient = class;
  TcdaInFulfillmentOf = class;
  TcdaIntendedRecipient = class;
  TcdaLabeledDrug = class;
  TcdaLanguageCommunication = class;
  TcdaEntityIdentifier = class;
  TcdaLegalAuthenticator = class;
  TcdaLocation = class;
  TcdaMaintainedEntity = class;
  TcdaManufacturedProduct = class;
  TcdaMaterial = class;
  TcdaNonXMLBody = class;
  TcdaObservation = class;
  TcdaObservationMedia = class;
  TcdaObservationRange = class;
  TcdaOrder = class;
  TcdaOrganization = class;
  TcdaOrganizationPartOf = class;
  TcdaOrganizer = class;
  TcdaParentDocument = class;
  TcdaParticipant1 = class;
  TcdaParticipant2 = class;
  TcdaParticipantRole = class;
  TcdaPatient = class;
  TcdaPatientRole = class;
  TcdaPerformer1 = class;
  TcdaPerformer2 = class;
  TcdaPerson = class;
  TcdaPlace = class;
  TcdaPlayingEntity = class;
  TcdaPrecondition = class;
  TcdaProcedure = class;
  TcdaProduct = class;
  TcdaRecordTarget = class;
  TcdaReference = class;
  TcdaReferenceRange = class;
  TcdaRegionOfInterest = class;
  TcdaRelatedDocument = class;
  TcdaRelatedEntity = class;
  TcdaRelatedSubject = class;
  TcdaResponsibleParty = class;
  TcdaSection = class;
  TcdaServiceEvent = class;
  TcdaSpecimen = class;
  TcdaSpecimenRole = class;
  TcdaStructuredBody = class;
  TcdaSubject = class;
  TcdaSubjectPerson = class;
  TcdaSubstanceAdministration = class;
  TcdaSupply = class;

  TcdaAuthorList = class;
  TcdaEntryRelationshipList = class;
  TcdaInformant12List = class;
  TcdaParticipant2List = class;
  TcdaPerformer2List = class;
  TcdaPreconditionList = class;
  TcdaReferenceList = class;
  TcdaSpecimenList = class;
  TcdaMaintainedEntityList = class;
  TcdaAuthenticatorList = class;
  TcdaAuthorizationList = class;
  TcdaDocumentationOfList = class;
  TcdaInformationRecipientList = class;
  TcdaInFulfillmentOfList = class;
  TcdaParticipant1List = class;
  TcdaRecordTargetList = class;
  TcdaRelatedDocumentList = class;
  TcdaEncounterParticipantList = class;
  TcdaReferenceRangeList = class;
  TcdaComponent4List = class;
  TcdaGuardianList = class;
  TcdaLanguageCommunicationList = class;
  TcdaEntityIdentifierList = class;
  TcdaEntryList = class;
  TcdaPerformer1List = class;
  TcdaComponentSectList = class;
  TcdaRegionOfInterest_valueList = class;

  TcdaBaseCollection = class (Tv3BaseList);

  {
    Base support for CDA structural elements
  }
  TcdaBase = class (Tv3Base)
  private
    FxmlId : String;
    FnullFlavor: Tv3NullFlavor;
    FtypeId: Tv3II;
    FrealmCode: Tv3ListCS;
    FtemplateId: Tv3ListII;
    procedure SettypeId(const Value: Tv3II);
    procedure SettemplateId(const Value: Tv3ListII);
    procedure SetrealmCode(const Value: Tv3ListCS);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaBase; Overload;
    Function Clone(parent : Tv3Base) : TcdaBase; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV : String; Override;
    Function CDAClassNameV : String; Override;
  Published
    {
      XML id associated with this element
    }
    Property xmlId : String read FxmlId write FxmlId;
    Property realmCode : Tv3ListCS read FrealmCode write SetrealmCode;
    Property typeId : Tv3II read FtypeId write SettypeId;
    Property templateId : Tv3ListII read FtemplateId write SettemplateId;
    // attributes
    Property nullFlavor : Tv3NullFlavor read FnullFlavor write FnullFlavor;
  End;

  {
     Abstract Ancestor for Act, Encounter, Observation, ObservationMedia, Organizer, Procedure_, RegionOfInterest, SubstanceAdministration or Supply
  }
  TcdaClinicalStatement = class (TcdaBase)
  private
    FclassCode: String;
    FmoodCode: String;
    Fid: Tv3ListII;
    procedure SetClassCode(const sValue : String);
    procedure SetMoodCode(const sValue : String);
    procedure Setid(const Value: Tv3ListII);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function ClassCodeIsFixed : Boolean; Virtual;
    function MoodCodeIsFixed : Boolean; Virtual;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaClinicalStatement; Overload;
    Function Clone(parent : Tv3Base) : TcdaClinicalStatement; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  Published
    Property classCode : String read FclassCode write SetclassCode;
    Property moodCode : String read FmoodCode write SetmoodCode;
    Property id : Tv3ListII read Fid write Setid;
  End;

  {
     A derivative of the RIM Act class, to be used when the other more specific classes aren't appropriate
  }
  TcdaAct = class (TcdaClinicalStatement)
  private
    Fauthor: TcdaAuthorList;
    FentryRelationship: TcdaEntryRelationshipList;
    Finformant: TcdaInformant12List;
    Fparticipant: TcdaParticipant2List;
    Fperformer: TcdaPerformer2List;
    Fprecondition: TcdaPreconditionList;
    Freference: TcdaReferenceList;
    Fspecimen: TcdaSpecimenList;
    Fsubject: TcdaSubject;
    FnegationInd: boolean;
    FHasNegationInd: boolean;
    Fcode: Tv3CD;
    FpriorityCode: Tv3CD;
    FstatusCode: Tv3CS;
    FlanguageCode: Tv3CS;
    Ftext: Tv3ED;
    FeffectiveTime: Tv3IVL;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setcode(const Value: Tv3CD);
    procedure SeteffectiveTime(const Value: Tv3IVL);
    procedure SetentryRelationship(const Value: TcdaEntryRelationshipList);
    procedure Setinformant(const Value: TcdaInformant12List);
    procedure SetlanguageCode(const Value: Tv3CS);
    procedure Setparticipant(const Value: TcdaParticipant2List);
    procedure Setperformer(const Value: TcdaPerformer2List);
    procedure Setprecondition(const Value: TcdaPreconditionList);
    procedure SetpriorityCode(const Value: Tv3CD);
    procedure Setreference(const Value: TcdaReferenceList);
    procedure Setspecimen(const Value: TcdaSpecimenList);
    procedure SetstatusCode(const Value: Tv3CS);
    procedure Setsubject(const Value: TcdaSubject);
    procedure Settext(const Value: Tv3ED);
    procedure SetnegationInd(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAct; Overload;
    Function Clone(parent : Tv3Base) : TcdaAct; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property text : Tv3ED read Ftext write Settext;
    Property statusCode : Tv3CS read FstatusCode write SetstatusCode;
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    Property priorityCode : Tv3CD read FpriorityCode write SetpriorityCode;
    Property languageCode : Tv3CS read FlanguageCode write SetlanguageCode;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimen : TcdaSpecimenList read Fspecimen write Setspecimen;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer2List read Fperformer write Setperformer;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Fauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Finformant write Setinformant;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant2List read Fparticipant write Setparticipant;
    {
      CDA has identified and modeled various link and reference scenarios. These scenarios
      enable CDA entries to be semantically linked to entries that exist within the same
      document (by traversing the entryRelationship class) or to objects external to
      it (by traversing the reference class).

      NOTE: The CDA specification permits any CDA entry to relate to any CDA entry
      using any of the following relationship types. In many cases, this would result
      in nonsensical relationships. The following table is a guideline for reasonable
      relationships between CDA entries, and is not a conformance constraint.
    }
    Property entryRelationship : TcdaEntryRelationshipList read FentryRelationship write SetentryRelationship;
    {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
    }
    Property reference : TcdaReferenceList read Freference write Setreference;
    {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
    }
    Property precondition : TcdaPreconditionList read Fprecondition write Setprecondition;
    // attributes
    {
      Act.negationInd, when set to "true", is a positive assertion that the Act as
      a whole is negated. Some properties such as Act.id, Act.moodCode, and the
      participations are not affected. These properties always have the same
      meaning: i.e., the author remains the author of the negative Act. An act
      statement with negationInd is still a statement about the specific fact
      described by the Act. For instance, a negated "finding of wheezing on
      July 1" means that the author positively denies that there was wheezing
      on July 1, and that he takes the same responsibility for such statement
      and the same requirement to have evidence for such statement than if he
      had not used negation.
    }
    Property negationInd : boolean read FnegationInd write SetnegationInd;
    {
      false if negationInd is null, true if negationInd is either true or false
    }
    Property HasNegationInd : boolean read FHasNegationInd;
  End;


  {
    A entity acting in the employ of or on behalf of an organization.
  }
  TcdaAssignedAuthor = class (TcdaBase)
  private
    FassignedAuthoringDevice: TcdaAuthoringDevice;
    FrepresentedOrganization: TcdaOrganization;
    FassignedPerson: TcdaPerson;
    Fcode: Tv3CD;
    Faddr: Tv3ListAD;
    Fid: Tv3ListII;
    Ftelecom: Tv3ListTEL;
    FclassCode: String;
    procedure Setaddr(const Value: Tv3ListAD);
    procedure SetassignedAuthoringDevice(const Value: TcdaAuthoringDevice);
    procedure SetassignedPerson(const Value: TcdaPerson);
    procedure Setcode(const Value: Tv3CD);
    procedure Setid(const Value: Tv3ListII);
    procedure SetrepresentedOrganization(const Value: TcdaOrganization);
    procedure Settelecom(const Value: Tv3ListTEL);
    function GetAuthorChoice: TcdaAuthorChoice;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAssignedAuthor; Overload;
    Function Clone(parent : Tv3Base) : TcdaAssignedAuthor; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property addr : Tv3ListAD read Faddr write Setaddr;
    Property telecom : Tv3ListTEL read Ftelecom write Settelecom;
    // choice starts:
    Property assignedPerson : TcdaPerson read FassignedPerson write SetassignedPerson;
    Property assignedAuthoringDevice : TcdaAuthoringDevice read FassignedAuthoringDevice write SetassignedAuthoringDevice;

    {
      Choice of assignedPerson or assignedAuthoringDevice
    }
    Property assignedAuthorChoice : TcdaAuthorChoice read GetAuthorChoice;
    // end choice
    Property representedOrganization : TcdaOrganization read FrepresentedOrganization write SetrepresentedOrganization;
    // attributes
    Property classCode : String read FclassCode; // RoleClassAssignedEntity fixed=ASSIGNED
  End;


  {
    a scoping organization in the role of an assigned custodian
  }
  TcdaAssignedCustodian = class (TcdaBase)
  private
    FrepresentedCustodianOrganization: TcdaCustodianOrganization;
    FclassCode: String;
    procedure SetrepresentedCustodianOrganization(const Value: TcdaCustodianOrganization);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAssignedCustodian; Overload;
    Function Clone(parent : Tv3Base) : TcdaAssignedCustodian; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property representedCustodianOrganization : TcdaCustodianOrganization read FrepresentedCustodianOrganization write SetrepresentedCustodianOrganization;
    // attributes
    Property classCode : String read FclassCode; // RoleClassAssignedEntity fixed=ASSIGNED
  End;


 {
   Abstract Ancestor for either AssignedEntity or Related Entity
  }
  TcdaInformantChoice = class (TcdaBase)
  private
    FclassCode: String;
    Fcode: Tv3CD;
    Faddr: Tv3ListAD;
    Ftelecom: Tv3ListTEL;
    procedure Setaddr(const Value: Tv3ListAD);
    procedure Setcode(const Value: Tv3CD);
    procedure Settelecom(const Value: Tv3ListTEL);
    procedure SetClassCode(const sValue : String);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function ClassCodeIsFixed : Boolean; Virtual;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaInformantChoice; Overload;
    Function Clone(parent : Tv3Base) : TcdaInformantChoice; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property addr : Tv3ListAD read Faddr write Setaddr;
    Property telecom : Tv3ListTEL read Ftelecom write Settelecom;
    // attributes
    Property classCode : String read FclassCode write SetclassCode; // RoleClassMutualRelationship
  End;


  {
    An assigned entity is a person assigned to the role by the scoping organization.
  }
  TcdaAssignedEntity = class (TcdaInformantChoice)
  private
    FrepresentedOrganization: TcdaOrganization;
    FassignedPerson: TcdaPerson;
    Fid: Tv3ListII;
    procedure SetassignedPerson(const Value: TcdaPerson);
    procedure Setid(const Value: Tv3ListII);
    procedure SetrepresentedOrganization(const Value: TcdaOrganization);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function ClassCodeIsFixed : Boolean; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAssignedEntity; Overload;
    Function Clone(parent : Tv3Base) : TcdaAssignedEntity; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property assignedPerson : TcdaPerson read FassignedPerson write SetassignedPerson;
    Property representedOrganization : TcdaOrganization read FrepresentedOrganization write SetrepresentedOrganization;
  End;


  {
    a person or organization in the role of a participating entity
  }
  TcdaAssociatedEntity = class (TcdaBase)
  private
    FscopingOrganization: TcdaOrganization;
    FassociatedPerson: TcdaPerson;
    FclassCode: String;
    Fcode: Tv3CD;
    Faddr: Tv3ListAD;
    Fid: Tv3ListII;
    Ftelecom: Tv3ListTEL;
    procedure Setaddr(const Value: Tv3ListAD);
    procedure SetassociatedPerson(const Value: TcdaPerson);
    procedure Setcode(const Value: Tv3CD);
    procedure Setid(const Value: Tv3ListII);
    procedure SetscopingOrganization(const Value: TcdaOrganization);
    procedure Settelecom(const Value: Tv3ListTEL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAssociatedEntity; Overload;
    Function Clone(parent : Tv3Base) : TcdaAssociatedEntity; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property addr : Tv3ListAD read Faddr write Setaddr;
    Property telecom : Tv3ListTEL read Ftelecom write Settelecom;
    Property associatedPerson : TcdaPerson read FassociatedPerson write SetassociatedPerson;
    Property scopingOrganization : TcdaOrganization read FscopingOrganization write SetscopingOrganization;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // RoleClassAssociative
  End;


  {
    Represents a participant who has attested to the accuracy of the document, but who
    does not have privileges to legally authenticate the document. An example would be
    a resident physician who sees a patient and dictates a note, then later signs it.
  }
  TcdaAuthenticator = class (TcdaBase)
  private
    FassignedEntity: TcdaAssignedEntity;
    FtypeCode: String;
    FsignatureCode: Tv3CS;
    Ftime: Tv3TS;
    procedure SetassignedEntity(const Value: TcdaAssignedEntity);
    procedure SetsignatureCode(const Value: Tv3CS);
    procedure Settime(const Value: Tv3TS);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAuthenticator; Overload;
    Function Clone(parent : Tv3Base) : TcdaAuthenticator; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property time : Tv3TS read Ftime write Settime;
    Property signatureCode : Tv3CS read FsignatureCode write SetsignatureCode;
    Property assignedEntity : TcdaAssignedEntity read FassignedEntity write SetassignedEntity;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType   fixed="AUTHEN"
  End;


  {
     Represents the humans and/or machines that authored the document.

     In some cases, the role or function of the author is inherent in the
     ClinicalDocument.code, such as where ClinicalDocument.code is "Medical
     Student Progress Note". The role of the author can also be recorded in
     the Author.functionCode or AssignedAuthor.code attribute. If either of
     these attributes is included, they should be equivalent to or further
     specialize the role inherent in the ClinicalDocument.code (such as where
     the ClinicalDocument.code is simply "Physician Progress Note" and the
     value of Author.functionCode is "rounding physician"), and shall not
     conflict with the role inherent in the ClinicalDocument.code, as such
     a conflict would constitute an ambiguous situation.
  }
  TcdaAuthor = class (TcdaBase)
  private
    FassignedAuthor: TcdaAssignedAuthor;
    FtypeCode: String;
    FfunctionCode: Tv3CD;
    FcontextControlCode: String;
    Ftime: Tv3TS;
    procedure SetassignedAuthor(const Value: TcdaAssignedAuthor);
    procedure SetfunctionCode(const Value: Tv3CD);
    procedure Settime(const Value: Tv3TS);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAuthor; Overload;
    Function Clone(parent : Tv3Base) : TcdaAuthor; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property functionCode : Tv3CD read FfunctionCode write SetfunctionCode;
    Property time : Tv3TS read Ftime write Settime;
    Property assignedAuthor : TcdaAssignedAuthor read FassignedAuthor write SetassignedAuthor;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType   " fixed="AUT"/>
    Property contextControlCode : String read FcontextControlCode; // ContextControl fixed="OP"
  End;

  {
    Abstract ancestor for either Person or AuthoringDevice
  }
  TcdaAuthorChoice = class (TcdaBase)
  private
    FdeterminerCode: String;
    FclassCode : String;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function GetClassCode : String; virtual;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAuthorChoice; Overload;
    Function Clone(parent : Tv3Base) : TcdaAuthorChoice; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;

    Property classCode : String read FclassCode; // EntityClassDevice   fixed by descendent
    Property determinerCode : String read FdeterminerCode; // EntityDeterminer   fixed="INSTANCE"
  End;

  {
    Used when a device (application/machine/etc) authored the document or section
  }
  TcdaAuthoringDevice = class (TcdaAuthorChoice)
  private
    FasMaintainedEntity: TcdaMaintainedEntityList;
    Fcode: Tv3CD;
    FmanufacturerModelName: Tv3SC;
    FsoftwareName: Tv3SC;
    procedure SetasMaintainedEntity(const Value: TcdaMaintainedEntityList);
    procedure Setcode(const Value: Tv3CD);
    procedure SetmanufacturerModelName(const Value: Tv3SC);
    procedure SetsoftwareName(const Value: Tv3SC);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function GetClassCode : String; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAuthoringDevice; Overload;
    Function Clone(parent : Tv3Base) : TcdaAuthoringDevice; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property manufacturerModelName : Tv3SC read FmanufacturerModelName write SetmanufacturerModelName;
    Property softwareName : Tv3SC read FsoftwareName write SetsoftwareName;
    {
      In CDA, Release One, it was possible to specify those individuals responsible
      for the device. This functionality has been deprecated in CDA, Release Two.
      The MaintainedEntity class is present for backwards compatibility, and its
      use is discouraged, except where needed to support the transformation of CDA,
      Release One documents
    }
    Property asMaintainedEntity : TcdaMaintainedEntityList read FasMaintainedEntity write SetasMaintainedEntity;
  End;


  {
    references the consents associated with this document
  }
  TcdaAuthorization = class (TcdaBase)
  private
    Fconsent: TcdaConsent;
    FtypeCode: String;
    procedure Setconsent(const Value: TcdaConsent);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaAuthorization; Overload;
    Function Clone(parent : Tv3Base) : TcdaAuthorization; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
       This class references the consents associated with this document. The
       type of consent (e.g. a consent to perform the related ServiceEvent, a
       consent for the information contained in the document to be released to
       a third party) is conveyed in Consent.code. Consents referenced in the
       CDA Header have been finalized (Consent.statusCode must equal
       "completed") and should be on file.
    }
    Property consent : TcdaConsent read Fconsent write Setconsent;
    // attributes
    Property typeCode : String read FtypeCode; // ActRelationshipType fixed="AUTH"
  End;


  {
    A Patient's birthplace is represented as a relationship between a patient and a place
  }
  TcdaBirthplace = class (TcdaBase)
  private
    Fplace: TcdaPlace;
    FclassCode: String;
    procedure Setplace(const Value: TcdaPlace);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaBirthplace; Overload;
    Function Clone(parent : Tv3Base) : TcdaBirthplace; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
      The Birthplace class is played by a place (Place class), and scoped by the patient (Patient class)
    }
    Property place : TcdaPlace read Fplace write Setplace;
    // attributes
    Property classCode : String read FclassCode; // RoleClass  fixed="BIRTHPL"
  End;


  {
    The ClinicalDocument class is the entry point into the CDA R-MIM, and corresponds
    to the <ClinicalDocument> XML element that is the root element of a CDA document.

    A CDA document is logically broken up into a CDA Header and a CDA Body. The
    CDA Header is comprised of ClinicalDocument attributes, participants, and
    act relationships. The CDA Body is the target of the ClinicalDocument
    component act relationship.
  }
  TcdaClinicalDocument = class (TcdaBase)
  private
    Fauthenticator: TcdaAuthenticatorList;
    Fauthorization: TcdaAuthorizationList;
    Fauthor: TcdaAuthorList;
    FcomponentOf: TcdaComponent1;
    Fcomponent: TcdaComponent2;
    Fcustodian: TcdaCustodian;
    FdataEnterer: TcdaDataEnterer;
    FdocumentationOf: TcdaDocumentationOfList;
    Finformant: TcdaInformant12List;
    FinformationRecipient: TcdaInformationRecipientList;
    FinFulfillmentOf: TcdaInFulfillmentOfList;
    FlegalAuthenticator: TcdaLegalAuthenticator;
    Fparticipant: TcdaParticipant1List;
    FrecordTarget: TcdaRecordTargetList;
    FrelatedDocument: TcdaRelatedDocumentList;
    FclassCode: String;
    FmoodCode: String;
    Fcode: Tv3CD;
    FconfidentialityCode: Tv3CD;
    FlanguageCode: Tv3CS;
    Fid: Tv3II;
    FsetId: Tv3II;
    FversionNumber: Tv3INT;
    Ftitle: Tv3ST;
    FcopyTime: Tv3TS;
    FeffectiveTime: Tv3TS;
    procedure Setauthenticator(const Value: TcdaAuthenticatorList);
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setauthorization(const Value: TcdaAuthorizationList);
    procedure Setcode(const Value: Tv3CD);
    procedure Setcomponent(const Value: TcdaComponent2);
    procedure SetcomponentOf(const Value: TcdaComponent1);
    procedure SetconfidentialityCode(const Value: Tv3CD);
    procedure SetcopyTime(const Value: Tv3TS);
    procedure Setcustodian(const Value: TcdaCustodian);
    procedure SetdataEnterer(const Value: TcdaDataEnterer);
    procedure SetdocumentationOf(const Value: TcdaDocumentationOfList);
    procedure SeteffectiveTime(const Value: Tv3TS);
    procedure Set_id(const Value: Tv3II);
    procedure Setinformant(const Value: TcdaInformant12List);
    procedure SetinformationRecipient(const Value: TcdaInformationRecipientList);
    procedure SetinFulfillmentOf(const Value: TcdaInFulfillmentOfList);
    procedure SetlanguageCode(const Value: Tv3CS);
    procedure SetlegalAuthenticator(const Value: TcdaLegalAuthenticator);
    procedure Setparticipant(const Value: TcdaParticipant1List);
    procedure SetrecordTarget(const Value: TcdaRecordTargetList);
    procedure SetrelatedDocument(const Value: TcdaRelatedDocumentList);
    procedure SetsetId(const Value: Tv3II);
    procedure Settitle(const Value: Tv3ST);
    procedure SetversionNumber(const Value: Tv3INT);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaClinicalDocument; Overload;
    Function Clone(parent : Tv3Base) : TcdaClinicalDocument; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;

  published
    Property id : Tv3II read Fid write Set_id;
    Property code : Tv3CD read Fcode write Setcode;
    Property title : Tv3ST read Ftitle write Settitle;
    Property effectiveTime : Tv3TS read FeffectiveTime write SeteffectiveTime;
    Property confidentialityCode : Tv3CD read FconfidentialityCode write SetconfidentialityCode;
    Property languageCode : Tv3CS read FlanguageCode write SetlanguageCode;
    Property setId : Tv3II read FsetId write SetsetId;
    Property versionNumber : Tv3INT read FversionNumber write SetversionNumber;
    Property copyTime : Tv3TS read FcopyTime write SetcopyTime;
    {
       The recordTarget represents the medical record that this document belongs to.

       A clinical document typically has exactly one recordTarget participant. In
       the uncommon case where a clinical document (such as a group encounter note)
       is placed into more than one patient chart, more than one recordTarget
       participants can be stated.

       The recordTarget(s) of a document are stated in the header and propagate
       to nested content, where they cannot be overridden
    }
    Property recordTarget : TcdaRecordTargetList read FrecordTarget write SetrecordTarget;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Fauthor write Setauthor;
    {
       Represents the participant who has transformed a dictated note into text.
    }
    Property dataEnterer : TcdaDataEnterer read FdataEnterer write SetdataEnterer;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Finformant write Setinformant;
    {
        Represents the organization that is in charge of maintaining the document.
        The custodian is the steward that is entrusted with the care of the document.
        Every CDA document has exactly one custodian or section
    }
    Property custodian : TcdaCustodian read Fcustodian write Setcustodian;
    {
       Represents a recipient who should receive a copy of the document.

       NOTE: The information recipient is an entity to whom a copy of a
       document is directed, at the time of document authorship. It is
       not the same as the cumulative set of persons to whom the document
       has subsequently been disclosed, over the life-time of the patient.
       Such a disclosure list would not be contained within the document,
       and it outside the scope of CDA.
    }
    Property informationRecipient : TcdaInformationRecipientList read FinformationRecipient write SetinformationRecipient;
    {
       Represents a participant who has legally authenticated the document.

       The CDA is a standard that specifies the structure of exchanged clinical
       documents. In the case where a local document is transformed into a CDA
       document for exchange, authentication occurs on the local document, and
       that fact is reflected in the exchanged CDA document. A CDA document can
       reflect the unauthenticated, authenticated, or legally authenticated state.
       The unauthenticated state exists when no authentication information has
       been recorded (i.e., it is the absence of being either authenticated or
       legally authenticated).

       While electronic signatures are not captured in a CDA document, both
       authentication and legal authentication require that a document has
       been signed manually or electronically by the responsible individual.
       A legalAuthenticator has a required legalAuthenticator.time indicating
       the time of authentication, and a required legalAuthenticator.signatureCode,
       indicating that a signature has been obtained and is on file.
    }
    Property legalAuthenticator : TcdaLegalAuthenticator read FlegalAuthenticator write SetlegalAuthenticator;
    {
       Represents a participant who has attested to the accuracy of the document, but
       who does not have privileges to legally authenticate the document. An example
       would be a resident physician who sees a patient and dictates a note, then later signs it
    }
    Property authenticator : TcdaAuthenticatorList read Fauthenticator write Setauthenticator;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant1List read Fparticipant write Setparticipant;
    {
      represents those orders that are fulfilled by this document. For
      instance, a provider orders an X-Ray. The X-Ray is performed. A radiologist
      reads the X-Ray and generates a report. The X-Ray order identifier is
      transmitted in the Order class, the performed X-Ray procedure is transmitted
      in the ServiceEvent class, and the ClinicalDocument.code would be valued with
      "Diagnostic Imaging Report".
    }
    Property inFulfillmentOf : TcdaInFulfillmentOfList read FinFulfillmentOf write SetinFulfillmentOf;
    {
        represents the main Act, such as a colonoscopy or an
        appendectomy, being documented.

        In some cases, the ServiceEvent is inherent in the ClinicalDocument.code,
        such as where ClinicalDocument.code is "History and Physical Report" and
        the procedure being documented is a "History and Physical" act. A ServiceEvent
        can further specialize the act inherent in the ClinicalDocument.code, such as
        where the ClinicalDocument.code is simply "Procedure Report" and the
        procedure was a "colonoscopy". If ServiceEvent is included, it must be
        equivalent to or further specialize the value inherent in the ClinicalDocument.code,
        and shall not conflict with the value inherent in the ClinicalDocument.code, as
        such a conflict would constitute an ambiguous situation.

        ServiceEvent.effectiveTime can be used to indicate the time the actual event
        (as opposed to the encounter surrounding the event) took place.
    }
    Property documentationOf : TcdaDocumentationOfList read FdocumentationOf write SetdocumentationOf;
    {
       A conformant CDA document can have a single relatedDocument with typeCode "APND";
       a single relatedDocument with typeCode "RPLC"; a single relatedDocument with
       typeCode "XFRM"; a combination of two relatedDocuments with typeCodes "XFRM"
       and "RPLC"; or a combination of two relatedDocuments with typeCodes "XFRM"
       and "APND". No other combinations are allowed.
    }
    Property relatedDocument : TcdaRelatedDocumentList read FrelatedDocument write SetrelatedDocument;
    {
       references the consents associated with this document. The
       type of consent (e.g. a consent to perform the related ServiceEvent, a
       consent for the information contained in the document to be released to
       a third party) is conveyed in Consent.code. Consents referenced in the
       CDA Header have been finalized (Consent.statusCode must equal
       "completed") and should be on file.
    }
    Property authorization : TcdaAuthorizationList read Fauthorization write Setauthorization;
    {
       represents the setting of the clinical encounter during
       which the documented act(s) or ServiceEvent occurred. Documents are not
       necessarily generated during an encounter, such as when a clinician, in
       response to an abnormal lab result, attempts to contact the patient but
       can't, and writes a Progress Note.

       In some cases, the setting of the encounter is inherent in the
       ClinicalDocument.code, such as where ClinicalDocument.code is "Diabetes
       Clinic Progress Note". The setting of an encounter can also be transmitted
       in the HealthCareFacility.code attribute. If HealthCareFacility.code is
       sent, it should be equivalent to or further specialize the value inherent
       in the ClinicalDocument.code (such as where the ClinicalDocument.code is
       simply "Clinic Progress Note" and the value of HealthCareFacility.code is
       "cardiology clinic"), and shall not conflict with the value inherent in
       the ClinicalDocument.code, as such a conflict would constitute an ambiguous
       situation.

       EncompassingEncounter.dischargeDispositionCode can be used to depict the
       disposition of the patient at the time of hospital discharge (e.g.,
       discharged to home, expired, against medical advice, etc.).
    }
    Property componentOf : TcdaComponent1 read FcomponentOf write SetcomponentOf;
    {
      The CDA body. The body can be either an unstructured blob, or can be comprised of
      structured markup. Every CDA document has exactly one body, associated
      with the ClinicalDocument class through the component relationship.
    }
    Property component : TcdaComponent2 read Fcomponent write Setcomponent;
    // attributes
    Property classCode : String read FclassCode; // ActClinicalDocument    fixed="DOCCLIN"
    Property moodCode : String read FmoodCode; // ActMood  fixed="EVN"
  End;


  {
    the setting of the clinical encounter during which the documented act(s) or ServiceEvent occurred
  }
  TcdaComponent1 = class (TcdaBase)
  private
    FencompassingEncounter: TcdaEncompassingEncounter;
    FtypeCode: String;
    procedure SetencompassingEncounter(const Value: TcdaEncompassingEncounter);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaComponent1; Overload;
    Function Clone(parent : Tv3Base) : TcdaComponent1; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
       represents the setting of the clinical encounter during
       which the documented act(s) or ServiceEvent occurred. Documents are not
       necessarily generated during an encounter, such as when a clinician, in
       response to an abnormal lab result, attempts to contact the patient but
       can't, and writes a Progress Note.

       In some cases, the setting of the encounter is inherent in the
       ClinicalDocument.code, such as where ClinicalDocument.code is "Diabetes
       Clinic Progress Note". The setting of an encounter can also be transmitted
       in the HealthCareFacility.code attribute. If HealthCareFacility.code is
       sent, it should be equivalent to or further specialize the value inherent
       in the ClinicalDocument.code (such as where the ClinicalDocument.code is
       simply "Clinic Progress Note" and the value of HealthCareFacility.code is
       "cardiology clinic"), and shall not conflict with the value inherent in
       the ClinicalDocument.code, as such a conflict would constitute an ambiguous
       situation.

       EncompassingEncounter.dischargeDispositionCode can be used to depict the
       disposition of the patient at the time of hospital discharge (e.g.,
       discharged to home, expired, against medical advice, etc.).
    }
    Property encompassingEncounter : TcdaEncompassingEncounter read FencompassingEncounter write SetencompassingEncounter;
    // attributes
    Property typeCode : String read FtypeCode; // ActRelationshipHasComponent  fixed="COMP"
  End;


  {
    The CDA body can be either an unstructured blob, or can be comprised of
    structured markup. Every CDA document has exactly one body, associated
    with the ClinicalDocument class through the component relationship.
  }
  TcdaComponent2 = class (TcdaBase)
  private
    FnonXMLBody: TcdaNonXMLBody;
    FstructuredBody: TcdaStructuredBody;
    FtypeCode: String;
    FcontextConductionInd: boolean;
    FHasContextConductionInd: boolean;
    procedure SetnonXMLBody(const Value: TcdaNonXMLBody);
    procedure SetstructuredBody(const Value: TcdaStructuredBody);
    procedure SetcontextConductionInd(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaComponent2; Overload;
    Function Clone(parent : Tv3Base) : TcdaComponent2; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    // choice starts:
    {
       represents a document body that is in some format
       other than XML. NonXMLBody.text is used to reference data that is stored
       externally to the CDA document or to encode the data directly inline.

       Rendering a referenced non-XML body requires a software tool that
       recognizes the particular MIME media type of the blob.
    }
    Property nonXMLBody : TcdaNonXMLBody read FnonXMLBody write SetnonXMLBody;
    {
       represents a CDA document body that is comprised of one or more document sections, possibly with entries
    }
    Property structuredBody : TcdaStructuredBody read FstructuredBody write SetstructuredBody;
    // end choice
    // attributes
    Property typeCode : String read FtypeCode; // ActRelationshipHasComponent fixed="COMP"
    Property contextConductionInd : boolean read FcontextConductionInd write SetcontextConductionInd;  // fixed="true"/
    {
      false if contextConductionInd is null, true if contextConductionInd is either true or false
    }
    Property HasContextConductionInd : boolean read FHasContextConductionInd;   ///
  End;


  {
     A StructuredBody or section class is associated with one or more Section classes through a component relationship
  }
  TcdaComponentSect = class (TcdaBase)
  private
    Fsection: TcdaSection;
    FtypeCode: String;
    FcontextConductionInd: boolean;
    FHasContextConductionInd: boolean;
    procedure Setsection(const Value: TcdaSection);
    procedure SetcontextConductionInd(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaComponentSect; Overload;
    Function Clone(parent : Tv3Base) : TcdaComponentSect; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
       Document sections can nest, can override context propagated from the
       header, and can contain narrative and CDA entries.

       The section has an ID attribute. This attribute serves as the target of
       a linkHtml reference in narrative. All values of attributes of type
       XML ID must be unique within the document (per the W3C XML specification).
    }
    Property section : TcdaSection read Fsection write Setsection;
    {
      ActRelationshipHasComponent - fixed value is "COMP"
    }
    Property typeCode : String read FtypeCode;
    {
      fixed value is "true"
    }
    Property contextConductionInd : boolean read FcontextConductionInd write SetcontextConductionInd;
    {
      false if contextConductionInd is null, true if contextConductionInd is either true or false
    }
    Property HasContextConductionInd : boolean read FHasContextConductionInd;      ///
  End;


  {
    The components of an organizer
  }
  TcdaComponent4 = class (TcdaBase)
  private
    Fact: TcdaAct;
    Fencounter: TcdaEncounter;
    Fobservation: TcdaObservation;
    FobservationMedia: TcdaObservationMedia;
    Forganizer: TcdaOrganizer;
    Fprocedure: TcdaProcedure;
    FregionOfInterest: TcdaRegionOfInterest;
    FsubstanceAdministration: TcdaSubstanceAdministration;
    Fsupply: TcdaSupply;
    FtypeCode: String;
    FcontextConductionInd: boolean;
    FHasContextConductionInd: boolean;
    FseperatableInd: Tv3BL;
    FsequenceNumber: Tv3INT;
    procedure Setact(const Value: TcdaAct);
    procedure Setencounter(const Value: TcdaEncounter);
    procedure Setobservation(const Value: TcdaObservation);
    procedure SetobservationMedia(const Value: TcdaObservationMedia);
    procedure Setorganizer(const Value: TcdaOrganizer);
    procedure Setprocedure(const Value: TcdaProcedure);
    procedure SetregionOfInterest(const Value: TcdaRegionOfInterest);
    procedure SetseperatableInd(const Value: Tv3BL);
    procedure SetsequenceNumber(const Value: Tv3INT);
    procedure SetsubstanceAdministration(const Value: TcdaSubstanceAdministration);
    procedure Setsupply(const Value: TcdaSupply);
    procedure SetcontextConductionInd(const Value: boolean);
    function GetClinicalStatement: TcdaClinicalStatement;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaComponent4; Overload;
    Function Clone(parent : Tv3Base) : TcdaComponent4; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property sequenceNumber : Tv3INT read FsequenceNumber write SetsequenceNumber;
    Property seperatableInd : Tv3BL read FseperatableInd write SetseperatableInd;
    // choice starts:
    {
       A derivative of the RIM Act class, to be used when the other more specific classes aren't appropriate
    }
    Property act : TcdaAct read Fact write Setact;
    {
      A derivative of the RIM PatientEncounter class, used to represent related encounters, such as follow-up visits or referenced past encounters
    }
    Property encounter : TcdaEncounter read Fencounter write Setencounter;
    {
       A derivative of the RIM Observation class, used for representing coded and other observations.
    }
    Property observation : TcdaObservation read Fobservation write Setobservation;
    {
       A derivative of the RIM Observation class that represents multimedia that
       is logically part of the current document. This class is only for multimedia
       that is logically part of the attested content of the document. Rendering
       a referenced ObservationMedia requires a software tool that recognizes
       the particular MIME media type.
    }
    Property observationMedia : TcdaObservationMedia read FobservationMedia write SetobservationMedia;
    {
      A derivative of the RIM Act class, which can be used to create arbitrary
      groupings of other CDA entries that share a common context. An Organizer
      can contain other Organizers and/or other CDA entries, by traversing the
      component relationship. An Organizer can refer to external acts by
      traversing the reference relationship. An Organizer cannot be the source
      of an entryRelationship relationship.
    }
    Property organizer : TcdaOrganizer read Forganizer write Setorganizer;
    {
      A derivative of the RIM Procedure class, used for representing procedures.
    }
    Property procedure_ : TcdaProcedure read Fprocedure write Setprocedure;
    {
       A derivative of the RIM Observation class that represents a region of interest
       on an image, using an overlay shape. RegionOfInterest is used to make reference
       to specific regions in images, e.g., to specify the site of a physical finding
       by "circling" a region in a schematic picture of a human body. The units of
       the coordinate values in RegionOfInterest.value are in pixels, expressed as
       a list of integers. The origin is in the upper left hand corner, with positive
       X values going to the right and positive Y values going down. The relationship
       between a RegionOfInterest and its referenced ObservationMedia or ExternalObservation
       is specified by traversing the entryRelationship or reference class, respectively,
       where typeCode equals "SUBJ". A RegionOfInterest must reference exactly one
       ObservationMedia or one ExternalObservation. If the RegionOfInterest is the
       target of a <renderMultimedia> reference, then it shall only reference an
       ObservationMedia and not an ExternalObservation.
    }
    Property regionOfInterest : TcdaRegionOfInterest read FregionOfInterest write SetregionOfInterest;
    {
      A derivative of the RIM SubstanceAdministration class, used for representing
      medication-related events such as medication history or planned medication
      administration orders.
    }
    Property substanceAdministration : TcdaSubstanceAdministration read FsubstanceAdministration write SetsubstanceAdministration;
    {
       A derivative of the RIM Supply class, used for representing the
       provision of a material by one entity to another.
    }
    Property supply : TcdaSupply read Fsupply write Setsupply;

    {
      Choice of act, encounter, observation, observationMedia, organizer, procedure_, regionOfInterest, substanceAdministration or supply
    }
    Property clinicalStatement : TcdaClinicalStatement read GetClinicalStatement;
    // end choice
    // attributes
    Property typeCode : String read FtypeCode; // ActRelationshipHasComponent   fixed="COMP"
    Property contextConductionInd : boolean read FcontextConductionInd write SetcontextConductionInd;  //     fixed="true"/
    {
      false if contextConductionInd is null, true if contextConductionInd is either true or false
    }
    Property HasContextConductionInd : boolean read FHasContextConductionInd;
  End;


  {
       This class references the consents associated with this document. The
       type of consent (e.g. a consent to perform the related ServiceEvent, a
       consent for the information contained in the document to be released to
       a third party) is conveyed in Consent.code. Consents referenced in the
       CDA Header have been finalized (Consent.statusCode must equal
       "completed") and should be on file.
  }
  TcdaConsent = class (TcdaBase)
  private
    FclassCode: String;
    FmoodCode: String;
    Fcode: Tv3CD;
    FstatusCode: Tv3CS;
    Fid: Tv3ListII;
    procedure Setcode(const Value: Tv3CD);
    procedure Setid(const Value: Tv3ListII);
    procedure SetstatusCode(const Value: Tv3CS);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaConsent; Overload;
    Function Clone(parent : Tv3Base) : TcdaConsent; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property statusCode : Tv3CS read FstatusCode write SetstatusCode;
    // attributes
    Property classCode : String read FclassCode; // ActClass   fixed="CONS"
    Property moodCode : String read FmoodCode; // ActMood  fixed="EVN"
  End;


  {
     used to bring in the LabeledDrug or Material entity that describes the administered substance
  }
  TcdaConsumable = class (TcdaBase)
  private
    FmanufacturedProduct: TcdaManufacturedProduct;
    FtypeCode: String;
    procedure SetmanufacturedProduct(const Value: TcdaManufacturedProduct);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaConsumable; Overload;
    Function Clone(parent : Tv3Base) : TcdaConsumable; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
       used to bring in the LabeledDrug or Material entity that describes the administered substance
    }
    Property manufacturedProduct : TcdaManufacturedProduct read FmanufacturedProduct write SetmanufacturedProduct;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType  fixed="CSM"
  End;


  {
    a condition that must hold true before some over activity occurs
  }
  TcdaCriterion = class (TcdaBase)
  private
    FclassCode: String;
    FmoodCode: String;
    Fvalue: Tv3ANY;
    Fcode: Tv3CD;
    Ftext: Tv3ED;
    procedure Setcode(const Value: Tv3CD);
    procedure Settext(const Value: Tv3ED);
    procedure Setvalue(const Value: Tv3ANY);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaCriterion; Overload;
    Function Clone(parent : Tv3Base) : TcdaCriterion; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property text : Tv3ED read Ftext write Settext;
    Property value : Tv3ANY read Fvalue write Setvalue;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // ActClassObservation  default="OBS"
    Property moodCode : String read FmoodCode; // ActMood   fixed="EVN.CRT"
  End;


  {
    Represents the organization that is in charge of maintaining the document. The custodian is the steward that is entrusted with the care of the document. Every CDA document has exactly one custodian.
  }
  TcdaCustodian = class (TcdaBase)
  private
    FassignedCustodian: TcdaAssignedCustodian;
    FtypeCode: String;
    procedure SetassignedCustodian(const Value: TcdaAssignedCustodian);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaCustodian; Overload;
    Function Clone(parent : Tv3Base) : TcdaCustodian; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
      The actual custodian
    }
    Property assignedCustodian : TcdaAssignedCustodian read FassignedCustodian write SetassignedCustodian;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType fixed="CST"
  End;


  {
    The steward organization is an entity scoping the role of AssignedCustodian, and has a required CustodianOrganization.id
  }
  TcdaCustodianOrganization = class (TcdaBase)
  private
    Faddr: Tv3AD;
    FclassCode: String;
    FdeterminerCode: String;
    Fid: Tv3ListII;
    Fname: Tv3EN;
    Ftelecom: Tv3TEL;
    procedure Setaddr(const Value: Tv3AD);
    procedure Setid(const Value: Tv3ListII);
    procedure Setname(const Value: Tv3EN);
    procedure Settelecom(const Value: Tv3TEL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaCustodianOrganization; Overload;
    Function Clone(parent : Tv3Base) : TcdaCustodianOrganization; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property name : Tv3EN read Fname write Setname;
    Property telecom : Tv3TEL read Ftelecom write Settelecom;
    Property addr : Tv3AD read Faddr write Setaddr;
    // attributes
    Property classCode : String read FclassCode; // EntityClassOrganization   fixed="ORG"
    Property determinerCode : String read FdeterminerCode; // EntityDeterminer  fixed="INSTANCE"
  End;


  {
       Represents the participant who has transformed a dictated note into text.
  }
  TcdaDataEnterer = class (TcdaBase)
  private
    FassignedEntity: TcdaAssignedEntity;
    FtypeCode: String;
    FcontextControlCode: String;
    Ftime: Tv3TS;
    procedure SetassignedEntity(const Value: TcdaAssignedEntity);
    procedure Settime(const Value: Tv3TS);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaDataEnterer; Overload;
    Function Clone(parent : Tv3Base) : TcdaDataEnterer; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property time : Tv3TS read Ftime write Settime;
    {
       Represents the actual participant who has transformed a dictated note into text.
    }
    Property assignedEntity : TcdaAssignedEntity read FassignedEntity write SetassignedEntity;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType fixed="ENT"
    Property contextControlCode : String read FcontextControlCode; // ContextControl   fixed="OP"
  End;


  {
    An entity used in an activity, without being substantially changed through that activity.
  }
  TcdaDevice = class (TcdaBase)
  private
    Fcode: Tv3CD;
    FclassCode: String;
    FdeterminerCode: String;
    FmanufacturerModelName: Tv3SC;
    FsoftwareName: Tv3SC;
    procedure Setcode(const Value: Tv3CD);
    procedure SetmanufacturerModelName(const Value: Tv3SC);
    procedure SetsoftwareName(const Value: Tv3SC);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaDevice; Overload;
    Function Clone(parent : Tv3Base) : TcdaDevice; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property manufacturerModelName : Tv3SC read FmanufacturerModelName write SetmanufacturerModelName;
    Property softwareName : Tv3SC read FsoftwareName write SetsoftwareName;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // EntityClassDevice    default="DEV"
    Property determinerCode : String read FdeterminerCode; // EntityDeterminer  fixed="INSTANCE"
  End;


  {
        represents the main Act, such as a colonoscopy or an
        appendectomy, being documented.
  }
  TcdaDocumentationOf = class (TcdaBase)
  private
    FserviceEvent: TcdaServiceEvent;
    FtypeCode: String;
    procedure SetserviceEvent(const Value: TcdaServiceEvent);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaDocumentationOf; Overload;
    Function Clone(parent : Tv3Base) : TcdaDocumentationOf; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
        represents the main Act, such as a colonoscopy or an
        appendectomy, being documented.

        In some cases, the ServiceEvent is inherent in the ClinicalDocument.code,
        such as where ClinicalDocument.code is "History and Physical Report" and
        the procedure being documented is a "History and Physical" act. A ServiceEvent
        can further specialize the act inherent in the ClinicalDocument.code, such as
        where the ClinicalDocument.code is simply "Procedure Report" and the
        procedure was a "colonoscopy". If ServiceEvent is included, it must be
        equivalent to or further specialize the value inherent in the ClinicalDocument.code,
        and shall not conflict with the value inherent in the ClinicalDocument.code, as
        such a conflict would constitute an ambiguous situation.

        ServiceEvent.effectiveTime can be used to indicate the time the actual event
        (as opposed to the encounter surrounding the event) took place.
    }
    Property serviceEvent : TcdaServiceEvent read FserviceEvent write SetserviceEvent;
    // attributes
    Property typeCode : String read FtypeCode; // ActRelationshipType fixed="DOC"
  End;


  {
       This optional class represents the setting of the clinical encounter during
       which the documented act(s) or ServiceEvent occurred. Documents are not
       necessarily generated during an encounter, such as when a clinician, in
       response to an abnormal lab result, attempts to contact the patient but
       can't, and writes a Progress Note.

       In some cases, the setting of the encounter is inherent in the
       ClinicalDocument.code, such as where ClinicalDocument.code is "Diabetes
       Clinic Progress Note". The setting of an encounter can also be transmitted
       in the HealthCareFacility.code attribute. If HealthCareFacility.code is
       sent, it should be equivalent to or further specialize the value inherent
       in the ClinicalDocument.code (such as where the ClinicalDocument.code is
       simply "Clinic Progress Note" and the value of HealthCareFacility.code is
       "cardiology clinic"), and shall not conflict with the value inherent in
       the ClinicalDocument.code, as such a conflict would constitute an ambiguous
       situation.

       EncompassingEncounter.dischargeDispositionCode can be used to depict the
       disposition of the patient at the time of hospital discharge (e.g.,
       discharged to home, expired, against medical advice, etc.).
  }
  TcdaEncompassingEncounter = class (TcdaBase)
  private
    FencounterParticipant: TcdaEncounterParticipantList;
    Flocation: TcdaLocation;
    FresponsibleParty: TcdaResponsibleParty;
    FclassCode: String;
    FmoodCode: String;
    Fcode: Tv3CD;
    FdischargeDispositionCode: Tv3CD;
    FeffectiveTime: Tv3IVL;
    Fid: Tv3ListII;
    procedure Setcode(const Value: Tv3CD);
    procedure SetdischargeDispositionCode(const Value: Tv3CD);
    procedure SeteffectiveTime(const Value: Tv3IVL);
    procedure SetencounterParticipant(const Value: TcdaEncounterParticipantList);
    procedure Setid(const Value: Tv3ListII);
    procedure Setlocation(const Value: TcdaLocation);
    procedure SetresponsibleParty(const Value: TcdaResponsibleParty);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaEncompassingEncounter; Overload;
    Function Clone(parent : Tv3Base) : TcdaEncompassingEncounter; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    Property dischargeDispositionCode : Tv3CD read FdischargeDispositionCode write SetdischargeDispositionCode;
    {
      The responsibleParty participant represents the participant having primary
      legal responsibility for the encounter. This differs from the legalAuthenticator
      participant in that the legalAuthenticator may or may not be the responsible
      party, and is serving a medical records function by signing off on the document,
      moving it into a completed state.
    }
    Property responsibleParty : TcdaResponsibleParty read FresponsibleParty write SetresponsibleParty;

    {
      The encounterParticipant participant represents clinicians directly associated
      with the encounter (e.g. by initiating, terminating, or overseeing it).
    }
    Property encounterParticipant : TcdaEncounterParticipantList read FencounterParticipant write SetencounterParticipant;
    {
       relates a healthcare facility (HealthCareFacility class)
       to the encounter to indicate where the encounter took place. The entity playing the role of
       HealthCareFacility is a place (Place class). The entity scoping the HealthCareFacility role
       is an organization (Organization class).

       The setting of an encounter (e.g. cardiology clinic, primary care clinic, rehabilitation hospital,
       skilled nursing facility) can be expressed in HealthCareFacility.code. Note that setting and
       physical location are not the same. There is a many-to-many relationship between setting and
       the physical location where care is delivered. Thus, a particular room can provide the location
       for cardiology clinic one day, and for primary care clinic another day; and cardiology clinic
       today might be held in one physical location, but in another physical location tomorrow.

       When the location is an organization, this is indicated by the presence of a scoping Organization,
       without a playing Place.
    }
    Property location : TcdaLocation read Flocation write Setlocation;
    // attributes
    Property classCode : String read FclassCode; // ActClass     fixed="ENC"
    Property moodCode : String read FmoodCode; // ActMood      fixed="EVN"
  End;


  {
      A derivative of the RIM PatientEncounter class, used to represent related encounters, such as follow-up visits or referenced past encounters
  }
  TcdaEncounter = class (TcdaClinicalStatement)
  private
    Fauthor: TcdaAuthorList;
    FentryRelationship: TcdaEntryRelationshipList;
    Finformant: TcdaInformant12List;
    Fparticipant: TcdaParticipant2List;
    Fperformer: TcdaPerformer2List;
    Fprecondition: TcdaPreconditionList;
    Freference: TcdaReferenceList;
    Fspecimen: TcdaSpecimenList;
    Fsubject: TcdaSubject;
    Fcode: Tv3CD;
    FpriorityCode: Tv3CD;
    FstatusCode: Tv3CS;
    Ftext: Tv3ED;
    FeffectiveTime: Tv3IVL;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setcode(const Value: Tv3CD);
    procedure SeteffectiveTime(const Value: Tv3IVL);
    procedure SetentryRelationship(const Value: TcdaEntryRelationshipList);
    procedure Setinformant(const Value: TcdaInformant12List);
    procedure Setparticipant(const Value: TcdaParticipant2List);
    procedure Setperformer(const Value: TcdaPerformer2List);
    procedure Setprecondition(const Value: TcdaPreconditionList);
    procedure SetpriorityCode(const Value: Tv3CD);
    procedure Setreference(const Value: TcdaReferenceList);
    procedure Setspecimen(const Value: TcdaSpecimenList);
    procedure SetstatusCode(const Value: Tv3CS);
    procedure Setsubject(const Value: TcdaSubject);
    procedure Settext(const Value: Tv3ED);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaEncounter; Overload;
    Function Clone(parent : Tv3Base) : TcdaEncounter; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property text : Tv3ED read Ftext write Settext;
    Property statusCode : Tv3CS read FstatusCode write SetstatusCode;
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    Property priorityCode : Tv3CD read FpriorityCode write SetpriorityCode;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimen : TcdaSpecimenList read Fspecimen write Setspecimen;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer2List read Fperformer write Setperformer;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Fauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Finformant write Setinformant;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant2List read Fparticipant write Setparticipant;
    {
      CDA has identified and modeled various link and reference scenarios. These scenarios
      enable CDA entries to be semantically linked to entries that exist within the same
      document (by traversing the entryRelationship class) or to objects external to
      it (by traversing the reference class).

      NOTE: The CDA specification permits any CDA entry to relate to any CDA entry
      using any of the following relationship types. In many cases, this would result
      in nonsensical relationships. The following table is a guideline for reasonable
      relationships between CDA entries, and is not a conformance constraint.
    }
    Property entryRelationship : TcdaEntryRelationshipList read FentryRelationship write SetentryRelationship;
    {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
    }
    Property reference : TcdaReferenceList read Freference write Setreference;
    {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
    }
    Property precondition : TcdaPreconditionList read Fprecondition write Setprecondition;
  End;


  {
      The encounterParticipant participant represents clinicians directly associated
      with the encounter (e.g. by initiating, terminating, or overseeing it).
  }
  TcdaEncounterParticipant = class (TcdaBase)
  private
    FassignedEntity: TcdaAssignedEntity;
    FtypeCode: String;
    Ftime: Tv3IVL;
    procedure SetassignedEntity(const Value: TcdaAssignedEntity);
    procedure Settime(const Value: Tv3IVL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaEncounterParticipant; Overload;
    Function Clone(parent : Tv3Base) : TcdaEncounterParticipant; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property time : Tv3IVL read Ftime write Settime;
    Property assignedEntity : TcdaAssignedEntity read FassignedEntity write SetassignedEntity;
    // attributes
    Property typeCode : String read FtypeCode write FtypeCode; // EncounterParticipant
  End;


  {
    A physical thing, group of physical things or an organization capable of participating in Acts while in a role.
  }
  TcdaEntity = class (TcdaBase)
  private
    Fcode: Tv3CD;
    Fdesc: Tv3ED;
    FclassCode: String;
    FdeterminerCode: String;
    Fid: Tv3ListII;
    procedure Setcode(const Value: Tv3CD);
    procedure Setdesc(const Value: Tv3ED);
    procedure Setid(const Value: Tv3ListII);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaEntity; Overload;
    Function Clone(parent : Tv3Base) : TcdaEntity; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property desc : Tv3ED read Fdesc write Setdesc;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // EntityClassRoot   default="ENT"
    Property determinerCode : String read FdeterminerCode; // EntityDeterminer  fixed="INSTANCE"
  End;


  {
    In terms of the relationship between a section and its entries, CDA defines a
    default general case, and a more specific case that can be used when applicable.

    The entry relationship is defaulted to "COMP" (component), for the general case
    where the only assertion is that the related entries are contained within the
    source section and no other semantics are implied. In this case, the narrative
    is the original authenticated content. The CDA entries are created by various
    techniques (e.g., natural language processing, a human coder, a structured
    data entry tool that outputs both entries and a text report). The method of
    entry creation may be indicated by the entry participants (e.g., by identifying
    the algorithm or person that generated them). Relationships between various
    entries (such as two Observations or an Observation and an ObservationMedia)
    are encoded using the relationship types defined in entryRelationship.
  }
  TcdaEntry = class (TcdaBase)
  private
    Fact: TcdaAct;
    Fencounter: TcdaEncounter;
    Fobservation: TcdaObservation;
    FobservationMedia: TcdaObservationMedia;
    Forganizer: TcdaOrganizer;
    Fprocedure: TcdaProcedure;
    FregionOfInterest: TcdaRegionOfInterest;
    FsubstanceAdministration: TcdaSubstanceAdministration;
    Fsupply: TcdaSupply;
    FtypeCode: String;
    FcontextConductionInd: boolean;
    FHasContextConductionInd: boolean;
    procedure Setact(const Value: TcdaAct);
    procedure Setencounter(const Value: TcdaEncounter);
    procedure Setobservation(const Value: TcdaObservation);
    procedure SetobservationMedia(const Value: TcdaObservationMedia);
    procedure Setorganizer(const Value: TcdaOrganizer);
    procedure Setprocedure(const Value: TcdaProcedure);
    procedure SetregionOfInterest(const Value: TcdaRegionOfInterest);
    procedure SetsubstanceAdministration(const Value: TcdaSubstanceAdministration);
    procedure Setsupply(const Value: TcdaSupply);
    procedure SetcontextConductionInd(const Value: boolean);
    function GetClinicalStatement: TcdaClinicalStatement;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaEntry; Overload;
    Function Clone(parent : Tv3Base) : TcdaEntry; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    // choice starts:
    {
       A derivative of the RIM Act class, to be used when the other more specific classes aren't appropriate
    }
    Property act : TcdaAct read Fact write Setact;
    {
      A derivative of the RIM PatientEncounter class, used to represent related encounters, such as follow-up visits or referenced past encounters
    }
    Property encounter : TcdaEncounter read Fencounter write Setencounter;
    {
       A derivative of the RIM Observation class, used for representing coded and other observations.
    }
    Property observation : TcdaObservation read Fobservation write Setobservation;
    {
       A derivative of the RIM Observation class that represents multimedia that
       is logically part of the current document. This class is only for multimedia
       that is logically part of the attested content of the document. Rendering
       a referenced ObservationMedia requires a software tool that recognizes
       the particular MIME media type.
    }
    Property observationMedia : TcdaObservationMedia read FobservationMedia write SetobservationMedia;
    {
      A derivative of the RIM Act class, which can be used to create arbitrary
      groupings of other CDA entries that share a common context. An Organizer
      can contain other Organizers and/or other CDA entries, by traversing the
      component relationship. An Organizer can refer to external acts by
      traversing the reference relationship. An Organizer cannot be the source
      of an entryRelationship relationship.
    }
    Property organizer : TcdaOrganizer read Forganizer write Setorganizer;
    {
      A derivative of the RIM Procedure class, used for representing procedures.
    }
    Property procedure_ : TcdaProcedure read Fprocedure write Setprocedure;
    {
       A derivative of the RIM Observation class that represents a region of interest
       on an image, using an overlay shape. RegionOfInterest is used to make reference
       to specific regions in images, e.g., to specify the site of a physical finding
       by "circling" a region in a schematic picture of a human body. The units of
       the coordinate values in RegionOfInterest.value are in pixels, expressed as
       a list of integers. The origin is in the upper left hand corner, with positive
       X values going to the right and positive Y values going down. The relationship
       between a RegionOfInterest and its referenced ObservationMedia or ExternalObservation
       is specified by traversing the entryRelationship or reference class, respectively,
       where typeCode equals "SUBJ". A RegionOfInterest must reference exactly one
       ObservationMedia or one ExternalObservation. If the RegionOfInterest is the
       target of a <renderMultimedia> reference, then it shall only reference an
       ObservationMedia and not an ExternalObservation.
    }
    Property regionOfInterest : TcdaRegionOfInterest read FregionOfInterest write SetregionOfInterest;
    {
      A derivative of the RIM SubstanceAdministration class, used for representing
      medication-related events such as medication history or planned medication
      administration orders.
    }
    Property substanceAdministration : TcdaSubstanceAdministration read FsubstanceAdministration write SetsubstanceAdministration;
    {
       A derivative of the RIM Supply class, used for representing the
       provision of a material by one entity to another.
    }
    Property supply : TcdaSupply read Fsupply write Setsupply;

    {
      Choice of act, encounter, observation, observationMedia, organizer, procedure_, regionOfInterest, substanceAdministration or supply
    }
    Property clinicalStatement : TcdaClinicalStatement read GetClinicalStatement;
    // end choice
    // attributes
    Property typeCode : String read FtypeCode write FtypeCode; // ActRelationshipEntry     default="COMP"
    Property contextConductionInd : boolean read FcontextConductionInd write SetcontextConductionInd;  // fixed="true"
    {
      false if contextConductionInd is null, true if contextConductionInd is either true or false
    }
    Property HasContextConductionInd : boolean read FHasContextConductionInd;    //"
  End;


  {
      CDA has identified and modeled various link and reference scenarios. These scenarios
      enable CDA entries to be semantically linked to entries that exist within the same
      document (by traversing the entryRelationship class) or to objects external to
      it (by traversing the reference class).

      NOTE: The CDA specification permits any CDA entry to relate to any CDA entry
      using any of the following relationship types. In many cases, this would result
      in nonsensical relationships. The following table is a guideline for reasonable
      relationships between CDA entries, and is not a conformance constraint.
  }
  TcdaEntryRelationship = class (TcdaBase)
  private
    Fact: TcdaAct;
    Fencounter: TcdaEncounter;
    Fobservation: TcdaObservation;
    FobservationMedia: TcdaObservationMedia;
    Forganizer: TcdaOrganizer;
    Fprocedure: TcdaProcedure;
    FregionOfInterest: TcdaRegionOfInterest;
    FsubstanceAdministration: TcdaSubstanceAdministration;
    Fsupply: TcdaSupply;
    FtypeCode: String;
    FcontextConductionInd: boolean;
    FHasContextConductionInd: boolean;
    FnegationInd: boolean;
    FHasNegationInd: boolean;
    FinversionInd: boolean;
    FHasInversionInd: boolean;
    FseperatableInd: Tv3BL;
    FsequenceNumber: Tv3INT;
    procedure Setact(const Value: TcdaAct);
    procedure Setencounter(const Value: TcdaEncounter);
    procedure Setobservation(const Value: TcdaObservation);
    procedure SetobservationMedia(const Value: TcdaObservationMedia);
    procedure Setorganizer(const Value: TcdaOrganizer);
    procedure Setprocedure(const Value: TcdaProcedure);
    procedure SetregionOfInterest(const Value: TcdaRegionOfInterest);
    procedure SetseperatableInd(const Value: Tv3BL);
    procedure SetsequenceNumber(const Value: Tv3INT);
    procedure SetsubstanceAdministration(const Value: TcdaSubstanceAdministration);
    procedure Setsupply(const Value: TcdaSupply);
    procedure SetcontextConductionInd(const Value: boolean);
    procedure SetinversionInd(const Value: boolean);
    procedure SetnegationInd(const Value: boolean);
    function GetClinicalStatement: TcdaClinicalStatement;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaEntryRelationship; Overload;
    Function Clone(parent : Tv3Base) : TcdaEntryRelationship; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property sequenceNumber : Tv3INT read FsequenceNumber write SetsequenceNumber;
    Property seperatableInd : Tv3BL read FseperatableInd write SetseperatableInd;
    // choice starts:
    {
       A derivative of the RIM Act class, to be used when the other more specific classes aren't appropriate
    }
    Property act : TcdaAct read Fact write Setact;
    {
      A derivative of the RIM PatientEncounter class, used to represent related encounters, such as follow-up visits or referenced past encounters
    }
    Property encounter : TcdaEncounter read Fencounter write Setencounter;
    {
       A derivative of the RIM Observation class, used for representing coded and other observations.
    }
    Property observation : TcdaObservation read Fobservation write Setobservation;
    {
       A derivative of the RIM Observation class that represents multimedia that
       is logically part of the current document. This class is only for multimedia
       that is logically part of the attested content of the document. Rendering
       a referenced ObservationMedia requires a software tool that recognizes
       the particular MIME media type.
    }
    Property observationMedia : TcdaObservationMedia read FobservationMedia write SetobservationMedia;
    {
      A derivative of the RIM Act class, which can be used to create arbitrary
      groupings of other CDA entries that share a common context. An Organizer
      can contain other Organizers and/or other CDA entries, by traversing the
      component relationship. An Organizer can refer to external acts by
      traversing the reference relationship. An Organizer cannot be the source
      of an entryRelationship relationship.
    }
    Property organizer : TcdaOrganizer read Forganizer write Setorganizer;
    {
      A derivative of the RIM Procedure class, used for representing procedures.
    }
    Property procedure_ : TcdaProcedure read Fprocedure write Setprocedure;
    {
       A derivative of the RIM Observation class that represents a region of interest
       on an image, using an overlay shape. RegionOfInterest is used to make reference
       to specific regions in images, e.g., to specify the site of a physical finding
       by "circling" a region in a schematic picture of a human body. The units of
       the coordinate values in RegionOfInterest.value are in pixels, expressed as
       a list of integers. The origin is in the upper left hand corner, with positive
       X values going to the right and positive Y values going down. The relationship
       between a RegionOfInterest and its referenced ObservationMedia or ExternalObservation
       is specified by traversing the entryRelationship or reference class, respectively,
       where typeCode equals "SUBJ". A RegionOfInterest must reference exactly one
       ObservationMedia or one ExternalObservation. If the RegionOfInterest is the
       target of a <renderMultimedia> reference, then it shall only reference an
       ObservationMedia and not an ExternalObservation.
    }
    Property regionOfInterest : TcdaRegionOfInterest read FregionOfInterest write SetregionOfInterest;
    {
      A derivative of the RIM SubstanceAdministration class, used for representing
      medication-related events such as medication history or planned medication
      administration orders.
    }
    Property substanceAdministration : TcdaSubstanceAdministration read FsubstanceAdministration write SetsubstanceAdministration;
    {
       A derivative of the RIM Supply class, used for representing the
       provision of a material by one entity to another.
    }
    Property supply : TcdaSupply read Fsupply write Setsupply;

    {
      Choice of act, encounter, observation, observationMedia, organizer, procedure_, regionOfInterest, substanceAdministration or supply
    }
    Property clinicalStatement : TcdaClinicalStatement read GetClinicalStatement;
    // end choice
    // attributes
    Property typeCode : String read FtypeCode write FtypeCode; // ActRelationshipEntryRelationship
    {
      The entryRelationship.inversionInd can be set to "true" to indicate that the
      relationship should be interpreted as if the roles of the source and target
      entries were reversed. In the example in the table above, "treadmill test"
      RSON (has reason) "chest pain". Inverted, this would have "chest pain" as
      the source and "treadmill test" as the target: "chest pain" RSON (inverted)
      "treadmill test". Inversion can be useful when the current context is
      describing the target of an act relationship that needs to be related
      back to the source.
    }
    Property inversionInd : boolean read FinversionInd write SetinversionInd;     // default="true"
    {
      false if inversionInd is null, true if inversionInd is either true or false
    }
    Property HasInversionInd : boolean read FHasInversionInd;
    {
       The entryRelationship.contextConductionInd differs from the otherwise common
       use of this attribute (see CDA Context (' 4.4 )) in that in all other cases
       where this attribute is used, the value is fixed at "true", whereas here the
       value is defaulted to "true", and can be changed to "false" when referencing
       an entry in the same document. Setting the context conduction to false when
       referencing an entry in the same document keeps clear the fact that the
       referenced object retains its original context.
    }
    Property contextConductionInd : boolean read FcontextConductionInd write SetcontextConductionInd;
    {
      false if contextConductionInd is null, true if contextConductionInd is either true or false
    }
    Property HasContextConductionInd : boolean read FHasContextConductionInd;
    Property negationInd : boolean read FnegationInd write SetnegationInd;
    {
      false if negationInd is null, true if negationInd is either true or false
    }
    Property HasNegationInd : boolean read FHasNegationInd;
  End;


  {
     ExternalDocument is a derivative of the RIM Document class, used for
     representing external documents. ExternalDocument.text is modeled as
     an ED data type - allowing for the expression of the MIME type of the
     external document
  }
  TcdaExternalActChoice = class (TcdaBase)
  private
    FclassCode: String;
    FmoodCode: String;
    Fcode: Tv3CD;
    Ftext: Tv3ED;
    Fid: Tv3ListII;
    procedure Setcode(const Value: Tv3CD);
    procedure Set_id(const Value: Tv3ListII);
    procedure Settext(const Value: Tv3ED);
    Procedure SetClassCode(sValue : String);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function ClassCodeIsFixed : Boolean; Virtual;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaExternalActChoice; Overload;
    Function Clone(parent : Tv3Base) : TcdaExternalActChoice; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Set_id;
    Property code : Tv3CD read Fcode write Setcode;
    Property text : Tv3ED read Ftext write Settext;
    // attributes
    Property classCode : String read FclassCode write SetclassCode;  // fixed by ancestor
    Property moodCode : String read FmoodCode; // ActMood        fixed="EVN"
  End;


  {
    ExternalAct is a derivative of the RIM Act class, to be used when the other more specific classes are not appropriate.
  }
  TcdaExternalAct = class (TcdaExternalActChoice)
  public
    constructor Create; Override;
    function Link : TcdaExternalAct; Overload;
    Function Clone(parent : Tv3Base) : TcdaExternalAct; Overload;
    Function CDAClassNameV : string; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  End;


  {
     ExternalDocument is a derivative of the RIM Document class, used for
     representing external documents. ExternalDocument.text is modeled as
     an ED data type - allowing for the expression of the MIME type of the
     external document
  }
  TcdaExternalDocument = class (TcdaExternalActChoice)
  private
    FsetId: Tv3II;
    FversionNumber: Tv3INT;
    procedure SetsetId(const Value: Tv3II);
    procedure SetversionNumber(const Value: Tv3INT);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaExternalDocument; Overload;
    Function Clone(parent : Tv3Base) : TcdaExternalDocument; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property setId : Tv3II read FsetId write SetsetId;
    Property versionNumber : Tv3INT read FversionNumber write SetversionNumber;
  End;


  {
    ExternalObservation is a derivative of the RIM Observation class, used for representing external coded and other observations.
  }
  TcdaExternalObservation = class (TcdaExternalActChoice)
  public
    constructor Create; Override;
    function Link : TcdaExternalObservation; Overload;
    Function Clone(parent : Tv3Base) : TcdaExternalObservation; Overload;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  End;


  {
    ExternalProcedure is a derivative of the RIM Procedure class, used for representing external procedures
  }
  TcdaExternalProcedure = class (TcdaExternalActChoice)
  Protected
    Function ClassCodeIsFixed : Boolean; Override;
  public
    constructor Create; Override;
    function Link : TcdaExternalProcedure; Overload;
    Function Clone(parent : Tv3Base) : TcdaExternalProcedure; Overload;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  End;


  {
    A patient's guardian is a person or organization in the role of guardian
  }
  TcdaGuardian = class (TcdaBase)
  private
    FguardianOrganization: TcdaOrganization;
    FguardianPerson: TcdaPerson;
    Fcode: Tv3CD;
    Faddr: Tv3ListAD;
    Fid: Tv3ListII;
    Ftelecom: Tv3ListTEL;
    FclassCode: String;
    procedure Setaddr(const Value: Tv3ListAD);
    procedure Setcode(const Value: Tv3CD);
    procedure SetguardianOrganization(const Value: TcdaOrganization);
    procedure SetguardianPerson(const Value: TcdaPerson);
    procedure Setid(const Value: Tv3ListII);
    procedure Settelecom(const Value: Tv3ListTEL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaGuardian; Overload;
    Function Clone(parent : Tv3Base) : TcdaGuardian; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property addr : Tv3ListAD read Faddr write Setaddr;
    Property telecom : Tv3ListTEL read Ftelecom write Settelecom;
    // choice starts:
    {
      person in the role of guardian
    }
    Property guardianPerson : TcdaPerson read FguardianPerson write SetguardianPerson;
    {
      organization in the role of guardian
    }
    Property guardianOrganization : TcdaOrganization read FguardianOrganization write SetguardianOrganization;
    // end choice
    // attributes
    Property classCode : String read FclassCode; // RoleClass  fixed="GUARD"
  End;


  {
    The setting of an encounter (e.g. cardiology clinic, primary care clinic,
    rehabilitation hospital, skilled nursing facility) can be expressed in
    HealthCareFacility.code. Note that setting and physical location are not
    the same. There is a many-to-many relationship between setting and the
    physical location where care is delivered. Thus, a particular room can
    provide the location for cardiology clinic one day, and for primary care
    clinic another day; and cardiology clinic today might be held in one
    physical location, but in another physical location tomorrow.
  }
  TcdaHealthCareFacility = class (TcdaBase)
  private
    FserviceProviderOrganization: TcdaOrganization;
    Flocation: TcdaPlace;
    Fcode: Tv3CD;
    Fid: Tv3ListII;
    FclassCode: String;
    procedure Setcode(const Value: Tv3CD);
    procedure Setid(const Value: Tv3ListII);
    procedure Setlocation(const Value: TcdaPlace);
    procedure SetserviceProviderOrganization(const Value: TcdaOrganization);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaHealthCareFacility; Overload;
    Function Clone(parent : Tv3Base) : TcdaHealthCareFacility; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    {
       The location participant (location class) relates a healthcare facility (HealthCareFacility class)
       to the encounter to indicate where the encounter took place. The entity playing the role of
       HealthCareFacility is a place (Place class). The entity scoping the HealthCareFacility role
       is an organization (Organization class).

       The setting of an encounter (e.g. cardiology clinic, primary care clinic, rehabilitation hospital,
       skilled nursing facility) can be expressed in HealthCareFacility.code. Note that setting and
       physical location are not the same. There is a many-to-many relationship between setting and
       the physical location where care is delivered. Thus, a particular room can provide the location
       for cardiology clinic one day, and for primary care clinic another day; and cardiology clinic
       today might be held in one physical location, but in another physical location tomorrow.

       When the location is an organization, this is indicated by the presence of a serviceProviderOrganization,
       without a playing Place.
    }
    Property location : TcdaPlace read Flocation write Setlocation;

    {
      Organisation that owns / controls the healthcare facility
    }
    Property serviceProviderOrganization : TcdaOrganization read FserviceProviderOrganization write SetserviceProviderOrganization;
    // attributes
    Property classCode : String read FclassCode write FclassCode;   // RoleClassServiceDeliveryLocation   default="SDLOC"
  End;


  {
    An informant (or source of information) is a person that provides relevant information, such as the parent of a comatose patient who describes the patient's behavior prior to the onset of coma.

    An informant can be a person in one of two roles. The RelatedEntity role is used to represent an
    informant without a role.id (e.g. a parent or guy on the street). The informant in this case bears
    some formal or personal relationship to the patient. The role is unscoped, with the assumption that
    the patient is always the implied scoper. RelatedEntity.code can be used to specify the nature of
    the relationship. The AssignedEntity role is used for an identified informant, and is scoped by
    an Organization.
  }
  TcdaInformant12 = class (TcdaBase)
  private
    FassignedEntity: TcdaAssignedEntity;
    FrelatedEntity: TcdaRelatedEntity;
    FtypeCode: String;
    FcontextControlCode: String;
    procedure SetassignedEntity(const Value: TcdaAssignedEntity);
    procedure SetrelatedEntity(const Value: TcdaRelatedEntity);
    function GetInformantChoice: TcdaInformantChoice;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaInformant12; Overload;
    Function Clone(parent : Tv3Base) : TcdaInformant12; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    // choice starts:
    {
      The AssignedEntity role is used for an identified informant, and is scoped by
      an Organization.
    }
    Property assignedEntity : TcdaAssignedEntity read FassignedEntity write SetassignedEntity;
    {
      The RelatedEntity role is used to represent an
      informant without a role.id (e.g. a parent or guy on the street). The informant in this case bears
      some formal or personal relationship to the patient. The role is unscoped, with the assumption that
      the patient is always the implied scoper.
    }
    Property relatedEntity : TcdaRelatedEntity read FrelatedEntity write SetrelatedEntity;

    {
      Choice of assignedEntity or relatedEntity
    }
    Property informantChoice : TcdaInformantChoice read GetInformantChoice;
    // end choice
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType   fixed="INF"/
    Property contextControlCode : String read FcontextControlCode; // ContextControl   fixed="OP"
  End;


  {
       Represents a recipient who should receive a copy of the document.

       NOTE: The information recipient is an entity to whom a copy of a
       document is directed, at the time of document authorship. It is
       not the same as the cumulative set of persons to whom the document
       has subsequently been disclosed, over the life-time of the patient.
       Such a disclosure list would not be contained within the document,
       and it outside the scope of CDA.
  }
  TcdaInformationRecipient = class (TcdaBase)
  private
    FintendedRecipient: TcdaIntendedRecipient;
    FtypeCode: String;
    procedure SetintendedRecipient(const Value: TcdaIntendedRecipient);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaInformationRecipient; Overload;
    Function Clone(parent : Tv3Base) : TcdaInformationRecipient; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
       the recipient who should receive a copy of the document.
    }
    Property intendedRecipient : TcdaIntendedRecipient read FintendedRecipient write SetintendedRecipient;
    // attributes
    Property typeCode : String read FtypeCode write FtypeCode; // InformationRecipient  default="PRCP"
  End;


  {
      represents an order that is fulfilled by this document. For
      instance, a provider orders an X-Ray. The X-Ray is performed. A radiologist
      reads the X-Ray and generates a report. The X-Ray order identifier is
      transmitted in the Order class, the performed X-Ray procedure is transmitted
      in the ServiceEvent class, and the ClinicalDocument.code would be valued with
      "Diagnostic Imaging Report".
  }
  TcdaInFulfillmentOf = class (TcdaBase)
  private
    Forder: TcdaOrder;
    FtypeCode: String;
    procedure Setorder(const Value: TcdaOrder);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaInFulfillmentOf; Overload;
    Function Clone(parent : Tv3Base) : TcdaInFulfillmentOf; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
       represents an order that is fulfilled by this document. For instance,
       a provider orders an X-Ray. The X-Ray is performed. A radiologist reads
       the X-Ray and generates a report. The X-Ray order identifier is transmitted
       in the Order class, the performed X-Ray procedure is transmitted in the
       ServiceEvent class, and the ClinicalDocument.code would be valued with
       "Diagnostic Imaging Report".
    }
    Property order : TcdaOrder read Forder write Setorder;
    // attributes
    Property typeCode : String read FtypeCode; // ActRelationshipFulfills   fixed="FLFS"
  End;


  {
    Where a person is the intended recipient, the playing entity is a person,
    optionally scoped by an organization (Organization class).
  }
  TcdaIntendedRecipient = class (TcdaBase)
  private
    FreceivedOrganization: TcdaOrganization;
    FinformationRecipient: TcdaPerson;
    FclassCode: String;
    Faddr: Tv3ListAD;
    Fid: Tv3ListII;
    Ftelecom: Tv3ListTEL;
    procedure Setaddr(const Value: Tv3ListAD);
    procedure Setid(const Value: Tv3ListII);
    procedure SetinformationRecipient(const Value: TcdaPerson);
    procedure SetreceivedOrganization(const Value: TcdaOrganization);
    procedure Settelecom(const Value: Tv3ListTEL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaIntendedRecipient; Overload;
    Function Clone(parent : Tv3Base) : TcdaIntendedRecipient; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property addr : Tv3ListAD read Faddr write Setaddr;
    Property telecom : Tv3ListTEL read Ftelecom write Settelecom;
    {
      Where a person is the intended recipient, the playing entity is a person,
    }
    Property informationRecipient : TcdaPerson read FinformationRecipient write SetinformationRecipient;
    {
      Where a person is the intended recipient, the playing entity is a person,
      optionally scoped by an organization (Organization class).
    }
    Property receivedOrganization : TcdaOrganization read FreceivedOrganization write SetreceivedOrganization;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // InformationRecipientRole default="ASSIGNED"
  End;


  {
    The LabeledDrug class, which is an Entity class playing the Role of Manufactured Product, identifies the drug that is consumed in the substance administration
  }
  TcdaLabeledDrug = class (TcdaBase)
  private
    Fcode: Tv3CD;
    Fname: Tv3EN;
    FclassCode: String;
    FdeterminerCode: String;
    procedure Setcode(const Value: Tv3CD);
    procedure Setname(const Value: Tv3EN);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaLabeledDrug; Overload;
    Function Clone(parent : Tv3Base) : TcdaLabeledDrug; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property name : Tv3EN read Fname write Setname;
    // attributes
    Property classCode : String read FclassCode; // EntityClassManufacturedMaterial   fixed="MMAT"
    Property determinerCode : String read FdeterminerCode;  // EntityDeterminerDetermined  fixed="KIND"
  End;


  {
    A patient's language communication skills can be expressed in the associated LanguageCommunication class
  }
  TcdaLanguageCommunication = class (TcdaBase)
  private
    FpreferenceInd: Tv3BL;
    FmodeCode: Tv3CD;
    FproficiencyLevelCode: Tv3CD;
    FlanguageCode: Tv3CS;
    procedure SetlanguageCode(const Value: Tv3CS);
    procedure SetmodeCode(const Value: Tv3CD);
    procedure SetpreferenceInd(const Value: Tv3BL);
    procedure SetproficiencyLevelCode(const Value: Tv3CD);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaLanguageCommunication; Overload;
    Function Clone(parent : Tv3Base) : TcdaLanguageCommunication; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property languageCode : Tv3CS read FlanguageCode write SetlanguageCode;
    Property modeCode : Tv3CD read FmodeCode write SetmodeCode;
    Property proficiencyLevelCode : Tv3CD read FproficiencyLevelCode write SetproficiencyLevelCode;
    Property preferenceInd : Tv3BL read FpreferenceInd write SetpreferenceInd;
  End;

  {
    A patient's language communication skills can be expressed in the associated EntityIdentifier class
  }
  TcdaEntityIdentifier = class (TcdaBase)
  private
    FId : Tv3II;
    FCode: Tv3CD;
    procedure SetId(const Value: Tv3II);
    procedure SetCode(const Value: Tv3CD);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaEntityIdentifier; Overload;
    Function Clone(parent : Tv3Base) : TcdaEntityIdentifier; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
      the actual identifier
    }
    Property id : Tv3II read FId write SetId;
    {
      a code that indicates the type of identifier
    }
    Property code : Tv3CD read FCode write SetCode;
  End;

  {
    Represents a participant who has legally authenticated the document.

    The CDA is a standard that specifies the structure of exchanged clinical documents.
    In the case where a local document is transformed into a CDA document for exchange,
    authentication occurs on the local document, and that fact is reflected in the
    exchanged CDA document. A CDA document can reflect the unauthenticated,
    authenticated, or legally authenticated state. The unauthenticated state
    exists when no authentication information has been recorded (i.e., it is the
    absence of being either authenticated or legally authenticated).

    While electronic signatures are not captured in a CDA document, both authentication
    and legal authentication require that a document has been signed manually or
    electronically by the responsible individual. A legalAuthenticator has a required
    legalAuthenticator.time indicating the time of authentication, and a required
    legalAuthenticator.signatureCode, indicating that a signature has been obtained
    and is on file.
  }
  TcdaLegalAuthenticator = class (TcdaBase)
  private
    FassignedEntity: TcdaAssignedEntity;
    FtypeCode: String;
    FcontextControlCode: String;
    FsignatureCode: Tv3CS;
    Ftime: Tv3TS;
    procedure SetassignedEntity(const Value: TcdaAssignedEntity);
    procedure SetsignatureCode(const Value: Tv3CS);
    procedure Settime(const Value: Tv3TS);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaLegalAuthenticator; Overload;
    Function Clone(parent : Tv3Base) : TcdaLegalAuthenticator; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property time : Tv3TS read Ftime write Settime;
    Property signatureCode : Tv3CS read FsignatureCode write SetsignatureCode;
    {
      the participant who has legally authenticated the document
    }
    Property assignedEntity : TcdaAssignedEntity read FassignedEntity write SetassignedEntity;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType    fixed="LA"
    Property contextControlCode : String read FcontextControlCode; // ContextControl fixed="OP"
  End;


  {
       The location participant (location class) relates a healthcare facility (HealthCareFacility class)
       to the encounter to indicate where the encounter took place. The entity playing the role of
       HealthCareFacility is a place (Place class). The entity scoping the HealthCareFacility role
       is an organization (Organization class).

       The setting of an encounter (e.g. cardiology clinic, primary care clinic, rehabilitation hospital,
       skilled nursing facility) can be expressed in HealthCareFacility.code. Note that setting and
       physical location are not the same. There is a many-to-many relationship between setting and
       the physical location where care is delivered. Thus, a particular room can provide the location
       for cardiology clinic one day, and for primary care clinic another day; and cardiology clinic
       today might be held in one physical location, but in another physical location tomorrow.

       When the location is an organization, this is indicated by the presence of a scoping Organization,
       without a playing Place.
  }
  TcdaLocation = class (TcdaBase)
  private
    FhealthCareFacility: TcdaHealthCareFacility;
    FtypeCode: String;
    procedure SethealthCareFacility(const Value: TcdaHealthCareFacility);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaLocation; Overload;
    Function Clone(parent : Tv3Base) : TcdaLocation; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property healthCareFacility : TcdaHealthCareFacility read FhealthCareFacility write SethealthCareFacility;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationTargetLocation  fixed="LOC"
  End;


  {
      In CDA, Release One, it was possible to specify those individuals responsible
      for the device. This functionality has been deprecated in CDA, Release Two.
      The MaintainedEntity class is present for backwards compatibility, and its
      use is discouraged, except where needed to support the transformation of CDA,
      Release One documents
  }
  TcdaMaintainedEntity = class (TcdaBase)
  private
    FmaintainingPerson: TcdaPerson;
    FeffectiveTime: Tv3IVL;
    FclassCode: String;
    procedure SeteffectiveTime(const Value: Tv3IVL);
    procedure SetmaintainingPerson(const Value: TcdaPerson);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaMaintainedEntity; Overload;
    Function Clone(parent : Tv3Base) : TcdaMaintainedEntity; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    Property maintainingPerson : TcdaPerson read FmaintainingPerson write SetmaintainingPerson;
    // attributes
    Property classCode : String read FclassCode; // RoleClass fixed="MNT"
  End;


  {
    used to bring in the LabeledDrug or Material entity that describes the administered substance
  }
  TcdaManufacturedProduct = class (TcdaBase)
  private
    FmanufacturedLabeledDrug: TcdaLabeledDrug;
    FmanufacturedMaterial: TcdaMaterial;
    FmanufacturerOrganization: TcdaOrganization;
    Fid: Tv3ListII;
    FclassCode: String;
    procedure Setid(const Value: Tv3ListII);
    procedure SetmanufacturedLabeledDrug(const Value: TcdaLabeledDrug);
    procedure SetmanufacturedMaterial(const Value: TcdaMaterial);
    procedure SetmanufacturerOrganization(const Value: TcdaOrganization);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaManufacturedProduct; Overload;
    Function Clone(parent : Tv3Base) : TcdaManufacturedProduct; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    // choice starts:
    {
      The LabeledDrug class, which is an Entity class playing the Role of Manufactured Product, identifies the drug that is consumed in the substance administration
    }
    Property manufacturedLabeledDrug : TcdaLabeledDrug read FmanufacturedLabeledDrug write SetmanufacturedLabeledDrug;
    {
      used to identify non-drug administered substances such as vaccines and blood products
    }
    Property manufacturedMaterial : TcdaMaterial read FmanufacturedMaterial write SetmanufacturedMaterial;
    // end choice
    Property manufacturerOrganization : TcdaOrganization read FmanufacturerOrganization write SetmanufacturerOrganization;
    // attributes
    Property classCode : String read FclassCode;  // RoleClassManufacturedProduct  fixed="MANU"
  End;


  {
    used to identify non-drug administered substances such as vaccines and blood products
  }
  TcdaMaterial = class (TcdaBase)
  private
    Fcode: Tv3CD;
    Fname: Tv3EN;
    FclassCode: String;
    FdeterminerCode: String;
    FlotNumberText: Tv3ST;
    procedure Setcode(const Value: Tv3CD);
    procedure SetlotNumberText(const Value: Tv3ST);
    procedure Setname(const Value: Tv3EN);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaMaterial; Overload;
    Function Clone(parent : Tv3Base) : TcdaMaterial; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property name : Tv3EN read Fname write Setname;
    Property lotNumberText : Tv3ST read FlotNumberText write SetlotNumberText;
    // attributes
    Property classCode : String read FclassCode; // EntityClassManufacturedMaterial  fixed="MMAT"
    Property determinerCode : String read FdeterminerCode;  // EntityDeterminerDetermined  fixed="KIND"
  End;


  {
     The NonXMLBody class represents a document body that is in some format other
     than XML. NonXMLBody.text is used to reference data that is stored externally
     to the CDA document or to encode the data directly inline.

     Rendering a referenced non-XML body requires a software tool that recognizes
     the particular MIME media type of the blob.
  }
  TcdaNonXMLBody = class (TcdaBase)
  private
    FclassCode: String;
    FmoodCode: String;
    FconfidentialityCode: Tv3CD;
    FlanguageCode: Tv3CS;
    Ftext: Tv3ED;
    procedure SetconfidentialityCode(const Value: Tv3CD);
    procedure SetlanguageCode(const Value: Tv3CS);
    procedure Settext(const Value: Tv3ED);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaNonXMLBody; Overload;
    Function Clone(parent : Tv3Base) : TcdaNonXMLBody; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property text : Tv3ED read Ftext write Settext;
    Property confidentialityCode : Tv3CD read FconfidentialityCode write SetconfidentialityCode;
    Property languageCode : Tv3CS read FlanguageCode write SetlanguageCode;
    // attributes
    Property classCode : String read FclassCode; // ActClass fixed="DOCBODY"
    Property moodCode : String read FmoodCode; // ActMood fixed="EVN"
  End;


  {
       A derivative of the RIM Observation class, used for representing coded and other observations.
  }
  TcdaObservation = class (TcdaClinicalStatement)
  private
    Fauthor: TcdaAuthorList;
    FentryRelationship: TcdaEntryRelationshipList;
    Finformant: TcdaInformant12List;
    Fparticipant: TcdaParticipant2List;
    Fperformer: TcdaPerformer2List;
    Fprecondition: TcdaPreconditionList;
    Freference: TcdaReferenceList;
    FreferenceRange: TcdaReferenceRangeList;
    Fspecimen: TcdaSpecimenList;
    Fsubject: TcdaSubject;
    FnegationInd: boolean;
    FHasNegationInd: boolean;
    FpriorityCode: Tv3CD;
    Fcode: Tv3CD;
    FstatusCode: Tv3CS;
    FlanguageCode: Tv3CS;
    Ftext: Tv3ED;
    FeffectiveTime: Tv3IVL;
    FrepeatNumber: Tv3IVL;
    Fvalue: Tv3ListANY;
    FtargetSiteCode: Tv3ListCD;
    FmethodCode: Tv3ListCD;
    FinterpretationCode: Tv3ListCD;
    FderivationExpr: Tv3ST;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setcode(const Value: Tv3CD);
    procedure SetderivationExpr(const Value: Tv3ST);
    procedure SeteffectiveTime(const Value: Tv3IVL);
    procedure SetentryRelationship(const Value: TcdaEntryRelationshipList);
    procedure Setinformant(const Value: TcdaInformant12List);
    procedure SetinterpretationCode(const Value: Tv3ListCD);
    procedure SetlanguageCode(const Value: Tv3CS);
    procedure SetmethodCode(const Value: Tv3ListCD);
    procedure Setparticipant(const Value: TcdaParticipant2List);
    procedure Setperformer(const Value: TcdaPerformer2List);
    procedure Setprecondition(const Value: TcdaPreconditionList);
    procedure SetpriorityCode(const Value: Tv3CD);
    procedure Setreference(const Value: TcdaReferenceList);
    procedure SetreferenceRange(const Value: TcdaReferenceRangeList);
    procedure SetrepeatNumber(const Value: Tv3IVL);
    procedure Setspecimen(const Value: TcdaSpecimenList);
    procedure SetstatusCode(const Value: Tv3CS);
    procedure Setsubject(const Value: TcdaSubject);
    procedure SettargetSiteCode(const Value: Tv3ListCD);
    procedure Settext(const Value: Tv3ED);
    procedure Setvalue(const Value: Tv3ListANY);
    procedure SetnegationInd(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaObservation; Overload;
    Function Clone(parent : Tv3Base) : TcdaObservation; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property derivationExpr : Tv3ST read FderivationExpr write SetderivationExpr;
    Property text : Tv3ED read Ftext write Settext;
    Property statusCode : Tv3CS read FstatusCode write SetstatusCode;
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    Property priorityCode : Tv3CD read FpriorityCode write SetpriorityCode;
    Property repeatNumber : Tv3IVL read FrepeatNumber write SetrepeatNumber;
    Property languageCode : Tv3CS read FlanguageCode write SetlanguageCode;
    Property value : Tv3ListANY read Fvalue write Setvalue;
    Property interpretationCode : Tv3ListCD read FinterpretationCode write SetinterpretationCode;
    Property methodCode : Tv3ListCD read FmethodCode write SetmethodCode;
    Property targetSiteCode : Tv3ListCD read FtargetSiteCode write SettargetSiteCode;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimen : TcdaSpecimenList read Fspecimen write Setspecimen;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer2List read Fperformer write Setperformer;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Fauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Finformant write Setinformant;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant2List read Fparticipant write Setparticipant;
    {
      CDA has identified and modeled various link and reference scenarios. These scenarios
      enable CDA entries to be semantically linked to entries that exist within the same
      document (by traversing the entryRelationship class) or to objects external to
      it (by traversing the reference class).

      NOTE: The CDA specification permits any CDA entry to relate to any CDA entry
      using any of the following relationship types. In many cases, this would result
      in nonsensical relationships. The following table is a guideline for reasonable
      relationships between CDA entries, and is not a conformance constraint.
    }
    Property entryRelationship : TcdaEntryRelationshipList read FentryRelationship write SetentryRelationship;
    {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
    }
    Property reference : TcdaReferenceList read Freference write Setreference;
    {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
    }
    Property precondition : TcdaPreconditionList read Fprecondition write Setprecondition;
    {
      An Observation can have zero to many referenceRange relationships, which relate
      an Observation to the ObservationRange class, where the expected range of values
      for a particular observation can be specified.
    }
    Property referenceRange : TcdaReferenceRangeList read FreferenceRange write SetreferenceRange;
    // attributes
    {
      Observation.negationInd, when set to "true", is a positive assertion that
      the Observation as a whole is negated. Some properties such as Observation.id,
      Observation.moodCode, and the participations are not negated. These properties
      always have the same meaning: i.e., the author remains the author of the
      negative Observation. An observation statement with negationInd is still
      a statement about the specific fact described by the Observation. For instance,
      a negated "finding of wheezing on July 1" means that the author positively denies
      that there was wheezing on July 1, and that he takes the same responsibility for
      such statement and the same requirement to have evidence for such statement than
      if he had not used negation.
    }
    Property negationInd : boolean read FnegationInd write SetnegationInd;
    {
      false if negationInd is null, true if negationInd is either true or false
    }
    Property HasNegationInd : boolean read FHasNegationInd;
  End;


  {
       A derivative of the RIM Observation class that represents multimedia that
       is logically part of the current document. This class is only for multimedia
       that is logically part of the attested content of the document. Rendering
       a referenced ObservationMedia requires a software tool that recognizes
       the particular MIME media type.
  }
  TcdaObservationMedia = class (TcdaClinicalStatement)
  private
    Fauthor: TcdaAuthorList;
    FentryRelationship: TcdaEntryRelationshipList;
    Finformant: TcdaInformant12List;
    Fparticipant: TcdaParticipant2List;
    Fperformer: TcdaPerformer2List;
    Fprecondition: TcdaPreconditionList;
    Freference: TcdaReferenceList;
    Fspecimen: TcdaSpecimenList;
    Fsubject: TcdaSubject;
    FlanguageCode: Tv3CS;
    Fvalue: Tv3ED;
    FID_: String;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure SetentryRelationship(const Value: TcdaEntryRelationshipList);
    procedure Setinformant(const Value: TcdaInformant12List);
    procedure SetlanguageCode(const Value: Tv3CS);
    procedure Setparticipant(const Value: TcdaParticipant2List);
    procedure Setperformer(const Value: TcdaPerformer2List);
    procedure Setprecondition(const Value: TcdaPreconditionList);
    procedure Setreference(const Value: TcdaReferenceList);
    procedure Setspecimen(const Value: TcdaSpecimenList);
    procedure Setsubject(const Value: TcdaSubject);
    procedure Setvalue(const Value: Tv3ED);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaObservationMedia; Overload;
    Function Clone(parent : Tv3Base) : TcdaObservationMedia; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property languageCode : Tv3CS read FlanguageCode write SetlanguageCode;
    Property value : Tv3ED read Fvalue write Setvalue;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimen : TcdaSpecimenList read Fspecimen write Setspecimen;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer2List read Fperformer write Setperformer;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Fauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Finformant write Setinformant;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant2List read Fparticipant write Setparticipant;
    {
      CDA has identified and modeled various link and reference scenarios. These scenarios
      enable CDA entries to be semantically linked to entries that exist within the same
      document (by traversing the entryRelationship class) or to objects external to
      it (by traversing the reference class).

      NOTE: The CDA specification permits any CDA entry to relate to any CDA entry
      using any of the following relationship types. In many cases, this would result
      in nonsensical relationships. The following table is a guideline for reasonable
      relationships between CDA entries, and is not a conformance constraint.
    }
    Property entryRelationship : TcdaEntryRelationshipList read FentryRelationship write SetentryRelationship;
    {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
    }
    Property reference : TcdaReferenceList read Freference write Setreference;
    {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
    }
    Property precondition : TcdaPreconditionList read Fprecondition write Setprecondition;
    // attributes
    {
      XML ID - target of an ID reference in the structured text.

      Note that in XML this is the "ID" attribute, but has been renamed for the sake
      of some non-case sensitive implementations
    }
    Property ID_ : String read FID_ write FID_;
  End;


  {
      An Observation can have zero to many referenceRange relationships, which relate
      an Observation to the ObservationRange class, where the expected range of values
      for a particular observation can be specified.
  }
  TcdaObservationRange = class (TcdaBase)
  private
    FclassCode: String;
    FmoodCode: String;
    Fvalue: Tv3ANY;
    FinterpretationCode: Tv3CD;
    Fcode: Tv3CD;
    Ftext: Tv3ED;
    procedure Setcode(const Value: Tv3CD);
    procedure SetinterpretationCode(const Value: Tv3CD);
    procedure Settext(const Value: Tv3ED);
    procedure Setvalue(const Value: Tv3ANY);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaObservationRange; Overload;
    Function Clone(parent : Tv3Base) : TcdaObservationRange; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property text : Tv3ED read Ftext write Settext;
    Property value : Tv3ANY read Fvalue write Setvalue;
    Property interpretationCode : Tv3CD read FinterpretationCode write SetinterpretationCode;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // ActClassObservation  default="OBS"
    Property moodCode : String read FmoodCode; // ActMood                 fixed="EVN.CRT"
  End;


  {
    This class represents those orders that are fulfilled by this document. For
    instance, a provider orders an X-Ray. The X-Ray is performed. A radiologist
    reads the X-Ray and generates a report. The X-Ray order identifier is
    transmitted in the Order class, the performed X-Ray procedure is transmitted
    in the ServiceEvent class, and the ClinicalDocument.code would be valued with
    "Diagnostic Imaging Report".
  }
  TcdaOrder = class (TcdaBase)
  private
    FclassCode: String;
    FmoodCode: String;
    FpriorityCode: Tv3CD;
    Fcode: Tv3CD;
    Fid: Tv3ListII;
    procedure Setcode(const Value: Tv3CD);
    procedure Setid(const Value: Tv3ListII);
    procedure SetpriorityCode(const Value: Tv3CD);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link: TcdaOrder; Overload;
    Function Clone(parent : Tv3Base) : TcdaOrder; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property priorityCode : Tv3CD read FpriorityCode write SetpriorityCode;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // ActClassRoot   default="ACT"
    Property moodCode : String read FmoodCode; // ActMood   fixed="RQO"
  End;


  {
    A social or legal structure formed by human beings.
  }
  TcdaOrganization = class (TcdaBase)
  private
    FasOrganizationPartOf: TcdaOrganizationPartOf;
    FstandardIndustryClassCode: Tv3CD;
    FclassCode: String;
    FdeterminerCode: String;
    Faddr: Tv3ListAD;
    Fid: Tv3ListII;
    Fname: Tv3ListEN;
    Ftelecom: Tv3ListTEL;
    procedure Setaddr(const Value: Tv3ListAD);
    procedure SetasOrganizationPartOf(const Value: TcdaOrganizationPartOf);
    procedure Setid(const Value: Tv3ListII);
    procedure Setname(const Value: Tv3ListEN);
    procedure SetstandardIndustryClassCode(const Value: Tv3CD);
    procedure Settelecom(const Value: Tv3ListTEL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaOrganization; Overload;
    Function Clone(parent : Tv3Base) : TcdaOrganization; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV  : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property name : Tv3ListEN read Fname write Setname;
    Property telecom : Tv3ListTEL read Ftelecom write Settelecom;
    Property addr : Tv3ListAD read Faddr write Setaddr;
    Property standardIndustryClassCode : Tv3CD read FstandardIndustryClassCode write SetstandardIndustryClassCode;
    Property asOrganizationPartOf : TcdaOrganizationPartOf read FasOrganizationPartOf write SetasOrganizationPartOf;
    // attributes
    Property classCode : String read FclassCode; // EntityClassOrganization    fixed="ORG"
    Property determinerCode : String read FdeterminerCode; // EntityDeterminer   fixed="INSTANCE"
  End;


  {
    An organization can be part of a larger organization. Where there is a
    need to include whole-part relationships, the OrganizationPartOf role can be
    used. OrganizationPartOf.statusCode indicates the state of the whole-part
    relationship (e.g. "active", "terminated"). OrganizationPartOf.effectiveTime
    is an interval of time specifying the period during which the whole-part
    relationhship is in effect, if such time limit is applicable and known.
  }
  TcdaOrganizationPartOf = class (TcdaBase)
  private
    FwholeOrganization: TcdaOrganization;
    Fcode: Tv3CD;
    FstatusCode: Tv3CS;
    FeffectiveTime: Tv3IVL;
    Fid: Tv3ListII;
    FclassCode: String;
    procedure Setcode(const Value: Tv3CD);
    procedure SeteffectiveTime(const Value: Tv3IVL);
    procedure Setid(const Value: Tv3ListII);
    procedure SetstatusCode(const Value: Tv3CS);
    procedure SetwholeOrganization(const Value: TcdaOrganization);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaOrganizationPartOf; Overload;
    Function Clone(parent : Tv3Base) : TcdaOrganizationPartOf; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property statusCode : Tv3CS read FstatusCode write SetstatusCode;
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    Property wholeOrganization : TcdaOrganization read FwholeOrganization write SetwholeOrganization;
    // attributes
    Property classCode : String read FclassCode; // RoleClass fixed="PART"
  End;


  {
      A derivative of the RIM Act class, which can be used to create arbitrary
      groupings of other CDA entries that share a common context. An Organizer
      can contain other Organizers and/or other CDA entries, by traversing the
      component relationship. An Organizer can refer to external acts by
      traversing the reference relationship. An Organizer cannot be the source
      of an entryRelationship relationship.
  }
  TcdaOrganizer = class (TcdaClinicalStatement)
  private
    Fauthor: TcdaAuthorList;
    Fcomponent: TcdaComponent4List;
    Finformant: TcdaInformant12List;
    Fparticipant: TcdaParticipant2List;
    Fperformer: TcdaPerformer2List;
    Fprecondition: TcdaPreconditionList;
    Freference: TcdaReferenceList;
    Fspecimen: TcdaSpecimenList;
    Fsubject: TcdaSubject;
    Fcode: Tv3CD;
    FstatusCode: Tv3CS;
    FeffectiveTime: Tv3IVL;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setcode(const Value: Tv3CD);
    procedure Setcomponent(const Value: TcdaComponent4List);
    procedure SeteffectiveTime(const Value: Tv3IVL);
    procedure Setinformant(const Value: TcdaInformant12List);
    procedure Setparticipant(const Value: TcdaParticipant2List);
    procedure Setperformer(const Value: TcdaPerformer2List);
    procedure Setprecondition(const Value: TcdaPreconditionList);
    procedure Setreference(const Value: TcdaReferenceList);
    procedure Setspecimen(const Value: TcdaSpecimenList);
    procedure SetstatusCode(const Value: Tv3CS);
    procedure Setsubject(const Value: TcdaSubject);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaOrganizer; Overload;
    Function Clone(parent : Tv3Base) : TcdaOrganizer; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property statusCode : Tv3CS read FstatusCode write SetstatusCode;
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimen : TcdaSpecimenList read Fspecimen write Setspecimen;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer2List read Fperformer write Setperformer;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Fauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Finformant write Setinformant;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant2List read Fparticipant write Setparticipant;
    {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
    }
    Property reference : TcdaReferenceList read Freference write Setreference;
    {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
    }
    Property precondition : TcdaPreconditionList read Fprecondition write Setprecondition;
    {
      The "component" Act Relationship is used to nest content. Context propagates to nested content
    }
    Property component : TcdaComponent4List read Fcomponent write Setcomponent;
    // attributes
  End;


  {
    The ParentDocument represents the source of a document revision, addenda, or
    transformation. ParentDocument.text is modeled as an ED data type - allowing
    for the expression of the MIME type of the parent document. It is not to be
    used to embed the related document, and thus ParentDocument.text.BIN is
    precluded from use.
  }
  TcdaParentDocument = class (TcdaBase)
  private
    FclassCode: String;
    FmoodCode: String;
    Fcode: Tv3CD;
    Ftext: Tv3ED;
    FsetId: Tv3II;
    FversionNumber: Tv3INT;
    Fid: Tv3ListII;
    procedure Setcode(const Value: Tv3CD);
    procedure Set_id(const Value: Tv3ListII);
    procedure SetsetId(const Value: Tv3II);
    procedure Settext(const Value: Tv3ED);
    procedure SetversionNumber(const Value: Tv3INT);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaParentDocument; Overload;
    Function Clone(parent : Tv3Base) : TcdaParentDocument; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Set_id;
    Property code : Tv3CD read Fcode write Setcode;
    Property text : Tv3ED read Ftext write Settext;
    Property setId : Tv3II read FsetId write SetsetId;
    Property versionNumber : Tv3INT read FversionNumber write SetversionNumber;
    // attributes
    Property classCode : String read FclassCode; // ActClinicalDocument  fixed="DOCCLIN"
    Property moodCode : String read FmoodCode; // ActMood fixed="EVN"
  End;


  {
    Can be used to represent any other participant that cannot be represented with one of the more specific participants
  }
  TcdaParticipant1 = class (TcdaBase)
  private
    FassociatedEntity: TcdaAssociatedEntity;
    FtypeCode: String;
    FfunctionCode: Tv3CD;
    FcontextControlCode: String;
    Ftime: Tv3IVL;
    procedure SetassociatedEntity(const Value: TcdaAssociatedEntity);
    procedure SetfunctionCode(const Value: Tv3CD);
    procedure Settime(const Value: Tv3IVL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaParticipant1; Overload;
    Function Clone(parent : Tv3Base) : TcdaParticipant1; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property functionCode : Tv3CD read FfunctionCode write SetfunctionCode;
    Property time : Tv3IVL read Ftime write Settime;
    Property associatedEntity : TcdaAssociatedEntity read FassociatedEntity write SetassociatedEntity;
    // attributes
    Property typeCode : String read FtypeCode write FtypeCode; // ParticipationType
    Property contextControlCode : String read FcontextControlCode; // ContextControl   fixed="OP"
  End;


  {
    Can be used to represent any other participant that cannot be represented with one of the more specific participants
  }
  TcdaParticipant2 = class (TcdaBase)
  private
    FparticipantRole: TcdaParticipantRole;
    FtypeCode: String;
    FawarenessCode: Tv3CD;
    FcontextControlCode: String;
    Ftime: Tv3IVL;
    procedure SetawarenessCode(const Value: Tv3CD);
    procedure SetparticipantRole(const Value: TcdaParticipantRole);
    procedure Settime(const Value: Tv3IVL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaParticipant2; Overload;
    Function Clone(parent : Tv3Base) : TcdaParticipant2; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property time : Tv3IVL read Ftime write Settime;
    Property awarenessCode : Tv3CD read FawarenessCode write SetawarenessCode;
    Property participantRole : TcdaParticipantRole read FparticipantRole write SetparticipantRole;
    // attributes
    Property typeCode : String read FtypeCode write FtypeCode; // ParticipationType
    Property contextControlCode : String read FcontextControlCode; // ContextControl  fixed="OP"
  End;


  {
    Describes how the entity is participating in the act.
  }
  TcdaParticipantRole = class (TcdaBase)
  private
    FplayingDevice: TcdaDevice;
    FscopingEntity: TcdaEntity;
    FplayingEntity: TcdaPlayingEntity;
    Fcode: Tv3CD;
    Faddr: Tv3ListAD;
    Fid: Tv3ListII;
    Ftelecom: Tv3ListTEL;
    FclassCode: String;
    procedure Setaddr(const Value: Tv3ListAD);
    procedure Setcode(const Value: Tv3CD);
    procedure Setid(const Value: Tv3ListII);
    procedure SetplayingDevice(const Value: TcdaDevice);
    procedure SetplayingEntity(const Value: TcdaPlayingEntity);
    procedure SetscopingEntity(const Value: TcdaEntity);
    procedure Settelecom(const Value: Tv3ListTEL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaParticipantRole; Overload;
    Function Clone(parent : Tv3Base) : TcdaParticipantRole; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property addr : Tv3ListAD read Faddr write Setaddr;
    Property telecom : Tv3ListTEL read Ftelecom write Settelecom;
    // choice starts:
    Property playingDevice : TcdaDevice read FplayingDevice write SetplayingDevice;
    Property playingEntity : TcdaPlayingEntity read FplayingEntity write SetplayingEntity;
    // end choice
    Property scopingEntity : TcdaEntity read FscopingEntity write SetscopingEntity;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // ActClassRoot default="ROL"
  End;


  {
    A person that receives health care services from a provider.
  }
  TcdaPatient = class (TcdaBase)
  private
    Fbirthplace: TcdaBirthplace;
    Fguardian: TcdaGuardianList;
    FlanguageCommunication: TcdaLanguageCommunicationList;
    FadministrativeGenderCode: Tv3CD;
    FraceCode: Tv3CD;
    FmaritalStatusCode: Tv3CD;
    FreligiousAffiliationCode: Tv3CD;
    FethnicGroupCode: Tv3CD;
    FclassCode: String;
    FdeterminerCode: String;
    Fid: Tv3II;
    Fname: Tv3ListEN;
    FbirthTime: Tv3TS;
    FEntityIdentifier : TcdaEntityIdentifierList;
    procedure SetadministrativeGenderCode(const Value: Tv3CD);
    procedure Setbirthplace(const Value: TcdaBirthplace);
    procedure SetbirthTime(const Value: Tv3TS);
    procedure SetethnicGroupCode(const Value: Tv3CD);
    Function Getguardian : TcdaGuardianList;
    procedure Setguardian(const Value: TcdaGuardianList);
    procedure Setid(const Value: Tv3II);
    Function GetlanguageCommunication : TcdaLanguageCommunicationList;
    procedure SetlanguageCommunication(const Value: TcdaLanguageCommunicationList);
    procedure SetmaritalStatusCode(const Value: Tv3CD);
    procedure Setname(const Value: Tv3ListEN);
    procedure SetraceCode(const Value: Tv3CD);
    procedure SetreligiousAffiliationCode(const Value: Tv3CD);
    Function GetEntityIdentifier : TcdaEntityIdentifierList;
    procedure SetEntityIdentifier(const Value: TcdaEntityIdentifierList);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaPatient; Overload;
    Function Clone(parent : Tv3Base) : TcdaPatient; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;

  published
    Property id : Tv3II read Fid write Setid;
    Property name : Tv3ListEN read Fname write Setname;
    Property administrativeGenderCode : Tv3CD read FadministrativeGenderCode write SetadministrativeGenderCode;
    Property birthTime : Tv3TS read FbirthTime write SetbirthTime;
    Property maritalStatusCode : Tv3CD read FmaritalStatusCode write SetmaritalStatusCode;
    Property religiousAffiliationCode : Tv3CD read FreligiousAffiliationCode write SetreligiousAffiliationCode;
    Property raceCode : Tv3CD read FraceCode write SetraceCode;
    Property ethnicGroupCode : Tv3CD read FethnicGroupCode write SetethnicGroupCode;
    Property guardian : TcdaGuardianList read Getguardian write Setguardian;
    Property birthplace : TcdaBirthplace read Fbirthplace write Setbirthplace;
    Property languageCommunication : TcdaLanguageCommunicationList read GetlanguageCommunication write SetlanguageCommunication;
    // attributes
    Property classCode : String read FclassCode; // EntityClass     fixed="PSN"
    Property determinerCode : String read FdeterminerCode; // EntityDeterminer  fixed="INSTANCE"
    Property asEntityIdentifier : TcdaEntityIdentifierList read GetEntityIdentifier write SetEntityIdentifier;
  End;


  {
    A person that is a patient (receiving healthcare from a provider)
  }
  TcdaPatientRole = class (TcdaBase)
  private
    FproviderOrganization: TcdaOrganization;
    Fpatient: TcdaPatient;
    Faddr: Tv3ListAD;
    Fid: Tv3ListII;
    Ftelecom: Tv3ListTEL;
    FclassCode: String;
    procedure Setaddr(const Value: Tv3ListAD);
    procedure Setid(const Value: Tv3ListII);
    procedure Setpatient(const Value: TcdaPatient);
    procedure SetproviderOrganization(const Value: TcdaOrganization);
    procedure Settelecom(const Value: Tv3ListTEL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaPatientRole; Overload;
    Function Clone(parent : Tv3Base) : TcdaPatientRole; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;

  published
    Property id : Tv3ListII read Fid write Setid;
    Property addr : Tv3ListAD read Faddr write Setaddr;
    Property telecom : Tv3ListTEL read Ftelecom write Settelecom;
    Property patient : TcdaPatient read Fpatient write Setpatient;
    Property providerOrganization : TcdaOrganization read FproviderOrganization write SetproviderOrganization;
    // attributes
    Property classCode : String read FclassCode; // RoleClass fixed="PAT"
  End;


  {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
  }
  TcdaPerformer1 = class (TcdaBase)
  private
    FassignedEntity: TcdaAssignedEntity;
    FtypeCode: String;
    FfunctionCode: Tv3CD;
    Ftime: Tv3IVL;
    procedure SetassignedEntity(const Value: TcdaAssignedEntity);
    procedure SetfunctionCode(const Value: Tv3CD);
    procedure Settime(const Value: Tv3IVL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaPerformer1; Overload;
    Function Clone(parent : Tv3Base) : TcdaPerformer1; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property functionCode : Tv3CD read FfunctionCode write SetfunctionCode;
    Property time : Tv3IVL read Ftime write Settime;
    Property assignedEntity : TcdaAssignedEntity read FassignedEntity write SetassignedEntity;
    // attributes
    Property typeCode : String read FtypeCode write FtypeCode; // ServiceEventPerformer
  End;


  {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
  }
  TcdaPerformer2 = class (TcdaBase)
  private
    FassignedEntity: TcdaAssignedEntity;
    FmodeCode: Tv3CD;
    Ftime: Tv3IVL;
    FtypeCode: String;
    procedure SetassignedEntity(const Value: TcdaAssignedEntity);
    procedure SetmodeCode(const Value: Tv3CD);
    procedure Settime(const Value: Tv3IVL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaPerformer2; Overload;
    Function Clone(parent : Tv3Base) : TcdaPerformer2; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property time : Tv3IVL read Ftime write Settime;
    Property modeCode : Tv3CD read FmodeCode write SetmodeCode;
    Property assignedEntity : TcdaAssignedEntity read FassignedEntity write SetassignedEntity;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationPhysicalPerformer  fixed="PRF"
  End;


  {
    An entity that is a person
  }
  TcdaPerson = class (TcdaAuthorChoice)
  private
    Fname: Tv3ListEN;
    procedure Setname(const Value: Tv3ListEN);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function GetClassCode : String; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link: TcdaPerson; Overload;
    Function Clone(parent : Tv3Base) : TcdaPerson; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;

  published
    Property name : Tv3ListEN read Fname write Setname;
    // attributes
  End;

  {
    a location - identified by a name and/or address
  }
  TcdaPlace = class (TcdaBase)
  private
    Faddr: Tv3AD;
    Fname: Tv3EN;
    FclassCode: String;
    FdeterminerCode: String;
    procedure Setaddr(const Value: Tv3AD);
    procedure Setname(const Value: Tv3EN);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaPlace; Overload;
    Function Clone(parent : Tv3Base) : TcdaPlace; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property name : Tv3EN read Fname write Setname;
    Property addr : Tv3AD read Faddr write Setaddr;
    // attributes
    Property classCode : String read FclassCode; // EntityClassPlace  fixed="PLC"
    Property determinerCode : String read FdeterminerCode; // EntityDeterminer  fixed="INSTANCE"
  End;


  {
    A physical thing, group of physical things or an organization capable of participating in Acts while in a role by playing the role
  }
  TcdaPlayingEntity = class (TcdaBase)
  private
    Fcode: Tv3CD;
    Fdesc: Tv3ED;
    FclassCode: String;
    FdeterminerCode: String;
    Fname: Tv3ListEN;
    Fquantity: Tv3ListPQ;
    procedure Setcode(const Value: Tv3CD);
    procedure Setdesc(const Value: Tv3ED);
    procedure Setname(const Value: Tv3ListEN);
    procedure Setquantity(const Value: Tv3ListPQ);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaPlayingEntity; Overload;
    Function Clone(parent : Tv3Base) : TcdaPlayingEntity; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;

  published
    Property code : Tv3CD read Fcode write Setcode;
    Property quantity : Tv3ListPQ read Fquantity write Setquantity;
    Property name : Tv3ListEN read Fname write Setname;
    Property desc : Tv3ED read Fdesc write Setdesc;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // EntityClassRoot default="ENT"
    Property determinerCode : String read FdeterminerCode; // EntityDeterminer  fixed="INSTANCE"
  End;


  {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
  }
  TcdaPrecondition = class (TcdaBase)
  private
    Fcriterion: TcdaCriterion;
    FtypeCode: String;
    procedure Setcriterion(const Value: TcdaCriterion);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaPrecondition; Overload;
    Function Clone(parent : Tv3Base) : TcdaPrecondition; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property criterion : TcdaCriterion read Fcriterion write Setcriterion;
    // attributes
    Property typeCode : String read FtypeCode; // ActRelationshipType   fixed="PRCN"
  End;


  {
      A derivative of the RIM Procedure class, used for representing procedures.
  }
  TcdaProcedure = class (TcdaClinicalStatement)
  private
    Fauthor: TcdaAuthorList;
    FentryRelationship: TcdaEntryRelationshipList;
    Finformant: TcdaInformant12List;
    Fparticipant: TcdaParticipant2List;
    Fperformer: TcdaPerformer2List;
    Fprecondition: TcdaPreconditionList;
    Freference: TcdaReferenceList;
    Fspecimen: TcdaSpecimenList;
    Fsubject: TcdaSubject;
    FnegationInd: boolean;
    FHasNegationInd: boolean;
    Fcode: Tv3CD;
    FpriorityCode: Tv3CD;
    FlanguageCode: Tv3CS;
    FstatusCode: Tv3CS;
    Ftext: Tv3ED;
    FeffectiveTime: Tv3IVL;
    FmethodCode: Tv3ListCD;
    FapproachSiteCode: Tv3ListCD;
    FtargetSiteCode: Tv3ListCD;
    procedure SetapproachSiteCode(const Value: Tv3ListCD);
    Function Getauthor : TcdaAuthorList;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setcode(const Value: Tv3CD);
    procedure SeteffectiveTime(const Value: Tv3IVL);
    Function GetentryRelationship : TcdaEntryRelationshipList;
    procedure SetentryRelationship(const Value: TcdaEntryRelationshipList);
    Function Getinformant : TcdaInformant12List;
    procedure Setinformant(const Value: TcdaInformant12List);
    procedure SetlanguageCode(const Value: Tv3CS);
    procedure SetmethodCode(const Value: Tv3ListCD);
    Function Getparticipant : TcdaParticipant2List;
    procedure Setparticipant(const Value: TcdaParticipant2List);
    Function Getperformer : TcdaPerformer2List;
    procedure Setperformer(const Value: TcdaPerformer2List);
    Function Getprecondition : TcdaPreconditionList;
    procedure Setprecondition(const Value: TcdaPreconditionList);
    procedure SetpriorityCode(const Value: Tv3CD);
    Function Getreference : TcdaReferenceList;
    procedure Setreference(const Value: TcdaReferenceList);
    Function Getspecimen : TcdaSpecimenList;
    procedure Setspecimen(const Value: TcdaSpecimenList);
    procedure SetstatusCode(const Value: Tv3CS);
    procedure Setsubject(const Value: TcdaSubject);
    procedure SettargetSiteCode(const Value: Tv3ListCD);
    procedure Settext(const Value: Tv3ED);
    procedure SetnegationInd(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaProcedure; Overload;
    Function Clone(parent : Tv3Base) : TcdaProcedure; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;

  published
    Property code : Tv3CD read Fcode write Setcode;
    Property text : Tv3ED read Ftext write Settext;
    Property statusCode : Tv3CS read FstatusCode write SetstatusCode;
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    Property priorityCode : Tv3CD read FpriorityCode write SetpriorityCode;
    Property languageCode : Tv3CS read FlanguageCode write SetlanguageCode;
    Property methodCode : Tv3ListCD read FmethodCode write SetmethodCode;
    Property approachSiteCode : Tv3ListCD read FapproachSiteCode write SetapproachSiteCode;
    Property targetSiteCode : Tv3ListCD read FtargetSiteCode write SettargetSiteCode;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimen : TcdaSpecimenList read Getspecimen write Setspecimen;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer2List read Getperformer write Setperformer;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Getauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Getinformant write Setinformant;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant2List read Getparticipant write Setparticipant;
    {
      CDA has identified and modeled various link and reference scenarios. These scenarios
      enable CDA entries to be semantically linked to entries that exist within the same
      document (by traversing the entryRelationship class) or to objects external to
      it (by traversing the reference class).

      NOTE: The CDA specification permits any CDA entry to relate to any CDA entry
      using any of the following relationship types. In many cases, this would result
      in nonsensical relationships. The following table is a guideline for reasonable
      relationships between CDA entries, and is not a conformance constraint.
    }
    Property entryRelationship : TcdaEntryRelationshipList read GetentryRelationship write SetentryRelationship;
    {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
    }
    Property reference : TcdaReferenceList read Getreference write Setreference;
    {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
    }
    Property precondition : TcdaPreconditionList read Getprecondition write Setprecondition;
    // attributes
    {
        Procedure.negationInd, when set to "true", is a positive assertion that the
        Procedure as a whole is negated. Some properties such as Procedure.id,
        Procedure.moodCode, and the participations are not affected. These properties
        always have the same meaning: i.e., the author remains the author of the negative
        Procedure. A procedure statement with negationInd is still a statement about the
        specific fact described by the Procedure. For instance, a negated "appendectomy
        performed" means that the author positively denies that there was ever an
        appendectomy performed, and that he takes the same responsibility for such
        statement and the same requirement to have evidence for such statement
        than if he had not used negation.
    }
    Property negationInd : boolean read FnegationInd write SetnegationInd;
    {
      false if negationInd is null, true if negationInd is either true or false
    }
    Property HasNegationInd : boolean read FHasNegationInd;
  End;


  {
    The dispensed product is associated with the Supply act via a product
    participant, which connects to the same ManufacturedProduct role used
    for SubstanceAdministration
  }
  TcdaProduct = class (TcdaBase)
  private
    FmanufacturedProduct: TcdaManufacturedProduct;
    FtypeCode: String;
    procedure SetmanufacturedProduct(const Value: TcdaManufacturedProduct);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaProduct; Overload;
    Function Clone(parent : Tv3Base) : TcdaProduct; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property manufacturedProduct : TcdaManufacturedProduct read FmanufacturedProduct write SetmanufacturedProduct;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType   fixed="PRD"
  End;


  {
    The recordTarget represents the medical record that this document belongs to.

    A clinical document typically has exactly one recordTarget participant. In the
    uncommon case where a clinical document (such as a group encounter note) is
    placed into more than one patient chart, more than one recordTarget
    participants can be stated.
  }
  TcdaRecordTarget = class (TcdaBase)
  private
    FpatientRole: TcdaPatientRole;
    FtypeCode: String;
    FcontextControlCode: String;
    procedure SetpatientRole(const Value: TcdaPatientRole);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaRecordTarget; Overload;
    Function Clone(parent : Tv3Base) : TcdaRecordTarget; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property patientRole : TcdaPatientRole read FpatientRole write SetpatientRole;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType    fixed="RCT"/
    Property contextControlCode : String read FcontextControlCode; // ContextControl fixed="OP"
  End;


  {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
  }
  TcdaReference = class (TcdaBase)
  private
    FexternalAct: TcdaExternalAct;
    FexternalDocument: TcdaExternalDocument;
    FexternalObservation: TcdaExternalObservation;
    FexternalProcedure: TcdaExternalProcedure;
    FtypeCode: String;
    FseperatableInd: Tv3BL;
    procedure SetexternalAct(const Value: TcdaExternalAct);
    procedure SetexternalDocument(const Value: TcdaExternalDocument);
    procedure SetexternalObservation(const Value: TcdaExternalObservation);
    procedure SetexternalProcedure(const Value: TcdaExternalProcedure);
    procedure SetseperatableInd(const Value: Tv3BL);
    function GetExternalActChoice: TcdaExternalActChoice;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaReference; Overload;
    Function Clone(parent : Tv3Base) : TcdaReference; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
       the attribute reference.seperatableInd indicates whether or not the source
       is intended to be interpreted independently of the target. The indicator
       cannot prevent an individual or application from separating the source and
       target, but indicates the author's desire and willingness to attest to the
       content of the source if separated from the target. Typically, where
       seperatableInd is "false", the exchanged package should include the
       target of the reference so that the recipient can render it.
    }
    Property seperatableInd : Tv3BL read FseperatableInd write SetseperatableInd;
    // choice starts:
    {
      ExternalAct is a derivative of the RIM Act class, to be used when the other more specific classes are not appropriate.
    }
    Property externalAct : TcdaExternalAct read FexternalAct write SetexternalAct;
    {
      ExternalObservation is a derivative of the RIM Observation class, used for representing external coded and other observations
    }
    Property externalObservation : TcdaExternalObservation read FexternalObservation write SetexternalObservation;
    {
      ExternalProcedure is a derivative of the RIM Procedure class, used for representing external procedures.
    }
    Property externalProcedure : TcdaExternalProcedure read FexternalProcedure write SetexternalProcedure;
    {
       ExternalDocument is a derivative of the RIM Document class, used for representing external
       documents. ExternalDocument.text is modeled as an ED data type - allowing for the expression
       of the MIME type of the external document.
    }
    Property externalDocument : TcdaExternalDocument read FexternalDocument write SetexternalDocument;

    {
      Choice of externalAct, externalObservation, externalProcedure, or externalDocument
    }
    Property externalActChoice : TcdaExternalActChoice read GetExternalActChoice;
    // end choice
    // attributes
    Property typeCode : String read FtypeCode write FtypeCode; // ActRelationshipExternalReference
  End;


  {
     An Observation can have zero to many referenceRange relationships, which relate
     an Observation to the ObservationRange class, where the expected range of values
     for a particular observation can be specified.
  }
  TcdaReferenceRange = class (TcdaBase)
  private
    FobservationRange: TcdaObservationRange;
    FtypeCode: String;
    procedure SetobservationRange(const Value: TcdaObservationRange);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaReferenceRange; Overload;
    Function Clone(parent : Tv3Base) : TcdaReferenceRange; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
      An Observation can have zero to many referenceRange relationships, which relate
      an Observation to the ObservationRange class, where the expected range of values
      for a particular observation can be specified.
    }
    Property observationRange : TcdaObservationRange read FobservationRange write SetobservationRange;
    // attributes
    Property typeCode : String read FtypeCode; // ActRelationshipType    fixed="REFV"
  End;

  {
    A value. The meaning of unsorted is presently unknown (!)
  }
  TcdaRegionOfInterest_value = class (Tv3INT)
  private
    Funsorted: Boolean;
    FHasUnsorted : Boolean;
    procedure Setunsorted(const Value: Boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    function Link : TcdaRegionOfInterest_value; Overload;
    Function Clone(parent : Tv3Base) : TcdaRegionOfInterest_value; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
      The meaning of unsorted is presently unknown (!)
    }
    Property unsorted : Boolean read Funsorted write Setunsorted;
    {
      false if unsorted is null, true if unsorted is either true or false
    }
    Property HasUnsorted : Boolean read FHasUnsorted;
  End;


  {
       A derivative of the RIM Observation class that represents a region of interest
       on an image, using an overlay shape. RegionOfInterest is used to make reference
       to specific regions in images, e.g., to specify the site of a physical finding
       by "circling" a region in a schematic picture of a human body. The units of
       the coordinate values in RegionOfInterest.value are in pixels, expressed as
       a list of integers. The origin is in the upper left hand corner, with positive
       X values going to the right and positive Y values going down. The relationship
       between a RegionOfInterest and its referenced ObservationMedia or ExternalObservation
       is specified by traversing the entryRelationship or reference class, respectively,
       where typeCode equals "SUBJ". A RegionOfInterest must reference exactly one
       ObservationMedia or one ExternalObservation. If the RegionOfInterest is the
       target of a <renderMultimedia> reference, then it shall only reference an
       ObservationMedia and not an ExternalObservation.
  }
  TcdaRegionOfInterest = class (TcdaClinicalStatement)
  private
    Fauthor: TcdaAuthorList;
    FentryRelationship: TcdaEntryRelationshipList;
    Finformant: TcdaInformant12List;
    Fparticipant: TcdaParticipant2List;
    Fperformer: TcdaPerformer2List;
    Fprecondition: TcdaPreconditionList;
    Freference: TcdaReferenceList;
    Fvalue: TcdaRegionOfInterest_valueList;
    Fspecimen: TcdaSpecimenList;
    Fsubject: TcdaSubject;
    Fcode: Tv3CS;
    FID_: String;
    Function Getauthor : TcdaAuthorList;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setcode(const Value: Tv3CS);
    Function GetentryRelationship : TcdaEntryRelationshipList;
    procedure SetentryRelationship(const Value: TcdaEntryRelationshipList);
    Function Getinformant : TcdaInformant12List;
    procedure Setinformant(const Value: TcdaInformant12List);
    Function Getparticipant : TcdaParticipant2List;
    procedure Setparticipant(const Value: TcdaParticipant2List);
    Function Getperformer : TcdaPerformer2List;
    procedure Setperformer(const Value: TcdaPerformer2List);
    Function Getprecondition : TcdaPreconditionList;
    procedure Setprecondition(const Value: TcdaPreconditionList);
    Function Getreference : TcdaReferenceList;
    procedure Setreference(const Value: TcdaReferenceList);
    Function Getspecimen : TcdaSpecimenList;
    procedure Setspecimen(const Value: TcdaSpecimenList);
    procedure Setsubject(const Value: TcdaSubject);
    Function Getvalue : TcdaRegionOfInterest_valueList;
    procedure Setvalue(const Value: TcdaRegionOfInterest_valueList);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function ClassCodeIsFixed : Boolean; Override;
    function MoodCodeIsFixed : Boolean; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaRegionOfInterest; Overload;
    Function Clone(parent : Tv3Base) : TcdaRegionOfInterest; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CS read Fcode write Setcode;
    Property value : TcdaRegionOfInterest_valueList read Getvalue write Setvalue;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimen : TcdaSpecimenList read Getspecimen write Setspecimen;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer2List read Getperformer write Setperformer;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Getauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Getinformant write Setinformant;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant2List read Getparticipant write Setparticipant;
    {
      CDA has identified and modeled various link and reference scenarios. These scenarios
      enable CDA entries to be semantically linked to entries that exist within the same
      document (by traversing the entryRelationship class) or to objects external to
      it (by traversing the reference class).

      NOTE: The CDA specification permits any CDA entry to relate to any CDA entry
      using any of the following relationship types. In many cases, this would result
      in nonsensical relationships. The following table is a guideline for reasonable
      relationships between CDA entries, and is not a conformance constraint.
    }
    Property entryRelationship : TcdaEntryRelationshipList read GetentryRelationship write SetentryRelationship;
    {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
    }
    Property reference : TcdaReferenceList read Getreference write Setreference;
    {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
    }
    Property precondition : TcdaPreconditionList read Getprecondition write Setprecondition;
    // attributes
    {
      XML ID - target of an ID reference in the structured text.

      Note that in XML this is the "ID" attribute, but has been renamed for the sake
      of some non-case sensitive implementations
    }
    Property ID_ : String read FID_ write FID_;
  End;


  {
      represents the source of a document revision, addenda,
      or transformation
  }
  TcdaRelatedDocument = class (TcdaBase)
  private
    FparentDocument: TcdaParentDocument;
    FtypeCode: String;
    procedure SetparentDocument(const Value: TcdaParentDocument);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaRelatedDocument; Overload;
    Function Clone(parent : Tv3Base) : TcdaRelatedDocument; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
      The ParentDocument represents the source of a document revision, addenda,
      or transformation. ParentDocument.text is modeled as an ED data type -
      allowing for the expression of the MIME type of the parent document. It
      is not to be used to embed the related document, and thus
      ParentDocument.text.BIN is precluded from use.
    }
    Property parentDocument : TcdaParentDocument read FparentDocument write SetparentDocument;
    // attributes
    Property typeCode : String read FtypeCode write FtypeCode; // ActRelationshipDocument
  End;


  {
      The RelatedEntity role is used to represent an
      informant without a role.id (e.g. a parent or guy on the street). The informant in this case bears
      some formal or personal relationship to the patient. The role is unscoped, with the assumption that
      the patient is always the implied scoper.
  }
  TcdaRelatedEntity = class (TcdaInformantChoice)
  private
    FrelatedPerson: TcdaPerson;
    FeffectiveTime: Tv3IVL;
    procedure SeteffectiveTime(const Value: Tv3IVL);
    procedure SetrelatedPerson(const Value: TcdaPerson);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaRelatedEntity; Overload;
    Function Clone(parent : Tv3Base) : TcdaRelatedEntity; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    Property relatedPerson : TcdaPerson read FrelatedPerson write SetrelatedPerson;
  End;


  {
    represents the primary target of the entries recorded in the document or section
  }
  TcdaRelatedSubject = class (TcdaBase)
  private
    Fsubject: TcdaSubjectPerson;
    FclassCode: String;
    Fcode: Tv3CD;
    Faddr: Tv3ListAD;
    Ftelecom: Tv3ListTEL;
    procedure Setaddr(const Value: Tv3ListAD);
    procedure Setcode(const Value: Tv3CD);
    procedure Setsubject(const Value: TcdaSubjectPerson);
    procedure Settelecom(const Value: Tv3ListTEL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaRelatedSubject; Overload;
    Function Clone(parent : Tv3Base) : TcdaRelatedSubject; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property addr : Tv3ListAD read Faddr write Setaddr;
    Property telecom : Tv3ListTEL read Ftelecom write Settelecom;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubjectPerson read Fsubject write Setsubject;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // DocumentSubject  default="PRS"
  End;


  {
      The responsibleParty participant represents the participant having primary
      legal responsibility for the encounter. This differs from the legalAuthenticator
      participant in that the legalAuthenticator may or may not be the responsible
      party, and is serving a medical records function by signing off on the document,
      moving it into a completed state.
  }
  TcdaResponsibleParty = class (TcdaBase)
  private
    FassignedEntity: TcdaAssignedEntity;
    FtypeCode: String;
    procedure SetassignedEntity(const Value: TcdaAssignedEntity);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaResponsibleParty; Overload;
    Function Clone(parent : Tv3Base) : TcdaResponsibleParty; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property assignedEntity : TcdaAssignedEntity read FassignedEntity write SetassignedEntity;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType  fixed="RESP"
  End;


  {
       Sections can nest, can override context propagated from the
       header, and can contain narrative and CDA entries.

       The section has an ID attribute. This attribute serves as the target of
       a linkHtml reference in narrative. All values of attributes of type
       XML ID must be unique within the document (per the W3C XML specification).
  }
  TcdaSection = class (TcdaBase)
  private
    Fauthor: TcdaAuthorList;
    Fcomponent: TcdaComponentSectList;
    Fentry: TcdaEntryList;
    Finformant: TcdaInformant12List;
    Fsubject: TcdaSubject;
    FclassCode: String;
    FmoodCode: String;
    Fcode: Tv3CD;
    FconfidentialityCode: Tv3CD;
    FlanguageCode: Tv3CS;
    Fid: Tv3II;
    Ftitle: Tv3ST;
    FID_: String;
    Ftext: TsnText;
    Function Getauthor : TcdaAuthorList;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setcode(const Value: Tv3CD);
    Function Getcomponent : TcdaComponentSectList;
    procedure Setcomponent(const Value: TcdaComponentSectList);
    procedure SetconfidentialityCode(const Value: Tv3CD);
    Function Getentry : TcdaEntryList;
    procedure Setentry(const Value: TcdaEntryList);
    procedure Setid(const Value: Tv3II);
    Function Getinformant : TcdaInformant12List;
    procedure Setinformant(const Value: TcdaInformant12List);
    procedure SetlanguageCode(const Value: Tv3CS);
    procedure Setsubject(const Value: TcdaSubject);
    procedure Settext(const Value: TsnText);
    procedure Settitle(const Value: Tv3ST);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaSection; Overload;
    Function Clone(parent : Tv3Base) : TcdaSection; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3II read Fid write Setid;
    {
      The code specifying the particular kind of section (e.g. Chief Complaint,
      Review of Systems, Assessment). The value set is drawn from LOINC, and has
      a CWE coding strength.
    }
    Property code : Tv3CD read Fcode write Setcode;
    {
      Represents the label of a section. If valued, it is to be rendered as part of
      the narrative content of the clinical document body.
    }
    Property title : Tv3ST read Ftitle write Settitle;
    {
      Used to store narrative to be rendered. Also referred to as the CDA Narrative Block
    }
    Property text : TsnText read Ftext write Settext;
    {
      A value for Section.confidentialityCode overrides the value propagated from StructuredBody
    }
    Property confidentialityCode : Tv3CD read FconfidentialityCode write SetconfidentialityCode;
    Property languageCode : Tv3CS read FlanguageCode write SetlanguageCode;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Getauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Getinformant write Setinformant;
    {
      The relationship between a section and its entries is encoded in the intervening "entry" Act Relationship.
    }
    Property entry : TcdaEntryList read Getentry write Setentry;
    {
      The "component" Act Relationship is used to nest content. Context propagates to nested content
    }
    Property component : TcdaComponentSectList read Getcomponent write Setcomponent;
    // attributes
    {
      XML ID - target of an ID reference in the structured text.

      Note that in XML this is the "ID" attribute, but has been renamed for the sake
      of some non-case sensitive implementations
    }
    Property ID_ : String read FID_ write FID_;
    Property classCode : String read FclassCode; // ActClass  fixed="DOCSECT"
    Property moodCode : String read FmoodCode; // ActMood  fixed="EVN"
  End;


  {
        This class represents the main Act, such as a colonoscopy or an
        appendectomy, being documented.

        In some cases, the ServiceEvent is inherent in the ClinicalDocument.code,
        such as where ClinicalDocument.code is "History and Physical Report" and
        the procedure being documented is a "History and Physical" act. A ServiceEvent
        can further specialize the act inherent in the ClinicalDocument.code, such as
        where the ClinicalDocument.code is simply "Procedure Report" and the
        procedure was a "colonoscopy". If ServiceEvent is included, it must be
        equivalent to or further specialize the value inherent in the ClinicalDocument.code,
        and shall not conflict with the value inherent in the ClinicalDocument.code, as
        such a conflict would constitute an ambiguous situation.

        ServiceEvent.effectiveTime can be used to indicate the time the actual event
        (as opposed to the encounter surrounding the event) took place.
  }
  TcdaServiceEvent = class (TcdaBase)
  private
    Fperformer: TcdaPerformer1List;
    FclassCode: String;
    FmoodCode: String;
    Fcode: Tv3CD;
    FeffectiveTime: Tv3IVL;
    Fid: Tv3ListII;
    procedure Setcode(const Value: Tv3CD);
    procedure SeteffectiveTime(const Value: Tv3IVL);
    procedure Setid(const Value: Tv3ListII);
    Function Getperformer : TcdaPerformer1List;
    procedure Setperformer(const Value: TcdaPerformer1List);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaServiceEvent; Overload;
    Function Clone(parent : Tv3Base) : TcdaServiceEvent; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property code : Tv3CD read Fcode write Setcode;
    Property effectiveTime : Tv3IVL read FeffectiveTime write SeteffectiveTime;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer1List read Getperformer write Setperformer;
    // attributes
    Property classCode : String read FclassCode write FclassCode; // ActClassRoot default="ACT"
    Property moodCode : String read FmoodCode; // ActMood    fixed="EVN"
  End;


  {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
  }
  TcdaSpecimen = class (TcdaBase)
  private
    FspecimenRole: TcdaSpecimenRole;
    FtypeCode: String;
    procedure SetspecimenRole(const Value: TcdaSpecimenRole);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaSpecimen; Overload;
    Function Clone(parent : Tv3Base) : TcdaSpecimen; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimenRole : TcdaSpecimenRole read FspecimenRole write SetspecimenRole;
    // attributes
    Property typeCode : String read FtypeCode; // ParticipationType  fixed="SPC"
  End;


  {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
  }
  TcdaSpecimenRole = class (TcdaBase)
  private
    FspecimenPlayingEntity: TcdaPlayingEntity;
    Fid: Tv3ListII;
    FclassCode: String;
    procedure Setid(const Value: Tv3ListII);
    procedure SetspecimenPlayingEntity(const Value: TcdaPlayingEntity);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaSpecimenRole; Overload;
    Function Clone(parent : Tv3Base) : TcdaSpecimenRole; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property id : Tv3ListII read Fid write Setid;
    Property specimenPlayingEntity : TcdaPlayingEntity read FspecimenPlayingEntity write SetspecimenPlayingEntity;
    // attributes
    Property classCode : String read FclassCode; // RoleClassSpecimen   fixed="SPEC"
  End;


  {
     The StructuredBody class represents a CDA document body that is comprised
     of one or more document sections, optionally with sections
  }
  TcdaStructuredBody = class (TcdaBase)
  private
    Fcomponent: TcdaComponentSectList;
    FclassCode: String;
    FmoodCode: String;
    FconfidentialityCode: Tv3CD;
    FlanguageCode: Tv3CS;
    Function Getcomponent : TcdaComponentSectList;
    procedure Setcomponent(const Value: TcdaComponentSectList);
    procedure SetconfidentialityCode(const Value: Tv3CD);
    procedure SetlanguageCode(const Value: Tv3CS);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaStructuredBody; Overload;
    Function Clone(parent : Tv3Base) : TcdaStructuredBody; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property confidentialityCode : Tv3CD read FconfidentialityCode write SetconfidentialityCode;
    Property languageCode : Tv3CS read FlanguageCode write SetlanguageCode;
    {
       The StructuredBody class is associated with one or more Section classes through a component relationship
    }
    Property component : TcdaComponentSectList read Getcomponent write Setcomponent;
    // attributes
    Property classCode : String read FclassCode; // ActClass  fixed="DOCBODY"
    Property moodCode : String read FmoodCode; // ActMood  fixed="EVN"
  End;


  {
    The subject participant represents the primary target of the entries recorded
    in the document. Most of the time the subject is the same as the recordTarget
    but need not be, for instance when the subject is a fetus observed in an
    obstetrical ultrasound.

    The subject participant can be ascribed to a CDA section or a CDA entry. It
    propagates to nested components, unless overridden. The subject of a document
    is presumed to be the patient.
  }
  TcdaSubject = class (TcdaBase)
  private
    FrelatedSubject: TcdaRelatedSubject;
    FawarenessCode: Tv3CD;
    FcontextControlCode: String;
    FtypeCode: String;
    procedure SetawarenessCode(const Value: Tv3CD);
    procedure SetrelatedSubject(const Value: TcdaRelatedSubject);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaSubject; Overload;
    Function Clone(parent : Tv3Base) : TcdaSubject; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property awarenessCode : Tv3CD read FawarenessCode write SetawarenessCode;
    Property relatedSubject : TcdaRelatedSubject read FrelatedSubject write SetrelatedSubject;
    // attributes
    Property typeCode : String read FtypeCode;  // ParticipationTargetSubject   fixed="SBJ"
    Property contextControlCode : String read FcontextControlCode; // ContextControl    fixed="OP"
  End;


  {
    A person who is a subject
  }
  TcdaSubjectPerson = class (TcdaBase)
  private
    FadministrativeGenderCode: Tv3CD;
    FclassCode: String;
    FdeterminerCode: String;
    Fname: Tv3ListEN;
    FbirthTime: Tv3TS;
    procedure SetadministrativeGenderCode(const Value: Tv3CD);
    procedure SetbirthTime(const Value: Tv3TS);
    procedure Setname(const Value: Tv3ListEN);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaSubjectPerson; Overload;
    Function Clone(parent : Tv3Base) : TcdaSubjectPerson; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property name : Tv3ListEN read Fname write Setname;
    Property administrativeGenderCode : Tv3CD read FadministrativeGenderCode write SetadministrativeGenderCode;
    Property birthTime : Tv3TS read FbirthTime write SetbirthTime;
    // attributes
    Property classCode : String read FclassCode;  // EntityClass       fixed="PSN"
    Property determinerCode : String read FdeterminerCode; // EntityDeterminer   fixed="INSTANCE"
  End;


  {
      A derivative of the RIM SubstanceAdministration class, used for representing
      medication-related events such as medication history or planned medication
      administration orders.
  }
  TcdaSubstanceAdministration = class (TcdaClinicalStatement)
  private
    Fauthor: TcdaAuthorList;
    Fconsumable: TcdaConsumable;
    FentryRelationship: TcdaEntryRelationshipList;
    Finformant: TcdaInformant12List;
    Fparticipant: TcdaParticipant2List;
    Fperformer: TcdaPerformer2List;
    Fprecondition: TcdaPreconditionList;
    Freference: TcdaReferenceList;
    Fspecimen: TcdaSpecimenList;
    Fsubject: TcdaSubject;
    FnegationInd: boolean;
    FHasNegationInd: boolean;
    FpriorityCode: Tv3CD;
    FrouteCode: Tv3CD;
    Fcode: Tv3CD;
    FadministrationUnitCode: Tv3CD;
    FstatusCode: Tv3CS;
    Ftext: Tv3ED;
    FrepeatNumber: Tv3IVL;
    FdoseQuantity: Tv3IVL;
    FrateQuantity: Tv3IVL;
    FapproachSiteCode: Tv3ListCD;
    FeffectiveTime: Tv3QSET;
    FmaxDoseQuantity: Tv3RTO;
    procedure SetadministrationUnitCode(const Value: Tv3CD);
    procedure SetapproachSiteCode(const Value: Tv3ListCD);
    Function Getauthor : TcdaAuthorList;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setcode(const Value: Tv3CD);
    procedure Setconsumable(const Value: TcdaConsumable);
    procedure SetdoseQuantity(const Value: Tv3IVL);
    procedure SeteffectiveTime(const Value: Tv3QSET);
    Function GetentryRelationship : TcdaEntryRelationshipList;
    procedure SetentryRelationship(const Value: TcdaEntryRelationshipList);
    Function Getinformant : TcdaInformant12List;
    procedure Setinformant(const Value: TcdaInformant12List);
    procedure SetmaxDoseQuantity(const Value: Tv3RTO);
    Function Getparticipant : TcdaParticipant2List;
    procedure Setparticipant(const Value: TcdaParticipant2List);
    Function Getperformer : TcdaPerformer2List;
    procedure Setperformer(const Value: TcdaPerformer2List);
    Function Getprecondition : TcdaPreconditionList;
    procedure Setprecondition(const Value: TcdaPreconditionList);
    procedure SetpriorityCode(const Value: Tv3CD);
    procedure SetrateQuantity(const Value: Tv3IVL);
    Function Getreference : TcdaReferenceList;
    procedure Setreference(const Value: TcdaReferenceList);
    procedure SetrepeatNumber(const Value: Tv3IVL);
    procedure SetrouteCode(const Value: Tv3CD);
    Function Getspecimen : TcdaSpecimenList;
    procedure Setspecimen(const Value: TcdaSpecimenList);
    procedure SetstatusCode(const Value: Tv3CS);
    procedure Setsubject(const Value: TcdaSubject);
    procedure Settext(const Value: Tv3ED);
    procedure SetnegationInd(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function ClassCodeIsFixed : Boolean; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaSubstanceAdministration; Overload;
    Function Clone(parent : Tv3Base) : TcdaSubstanceAdministration; Overload;
    procedure Assign(oSource: TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;

  published
    Property code : Tv3CD read Fcode write Setcode;
    Property text : Tv3ED read Ftext write Settext;
    Property statusCode : Tv3CS read FstatusCode write SetstatusCode;
    {
      is used to describe the timing of administration. It is modeled using
      the GTS data type to accommodate various dosing scenarios
    }
    Property effectiveTime : Tv3QSET read FeffectiveTime write SeteffectiveTime;
    {
      categorizes the priority of a substance administration
    }
    Property priorityCode : Tv3CD read FpriorityCode write SetpriorityCode;
    Property repeatNumber : Tv3IVL read FrepeatNumber write SetrepeatNumber;
    Property routeCode : Tv3CD read FrouteCode write SetrouteCode;
    Property approachSiteCode : Tv3ListCD read FapproachSiteCode write SetapproachSiteCode;
    {
      indicates how much medication is given per dose
    }
    Property doseQuantity : Tv3IVL read FdoseQuantity write SetdoseQuantity;
    {
      can be used to indicate the rate at which the dose is to be administered
      (e.g., the flow rate for intravenous infusions)
    }
    Property rateQuantity : Tv3IVL read FrateQuantity write SetrateQuantity;
    {
      is used to capture the maximum dose of the medication that can be given
      over a stated time interval (e.g., maximum daily dose of morphine, maximum
      lifetime dose of doxorubicin)
    }
    Property maxDoseQuantity : Tv3RTO read FmaxDoseQuantity write SetmaxDoseQuantity;
    Property administrationUnitCode : Tv3CD read FadministrationUnitCode write SetadministrationUnitCode;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimen : TcdaSpecimenList read Getspecimen write Setspecimen;
    {
      used to bring in the LabeledDrug or Material entity that describes the administered substance
    }
    Property consumable : TcdaConsumable read Fconsumable write Setconsumable;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer2List read Getperformer write Setperformer;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Getauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Getinformant write Setinformant;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant2List read Getparticipant write Setparticipant;
    {
      CDA has identified and modeled various link and reference scenarios. These scenarios
      enable CDA entries to be semantically linked to entries that exist within the same
      document (by traversing the entryRelationship class) or to objects external to
      it (by traversing the reference class).

      NOTE: The CDA specification permits any CDA entry to relate to any CDA entry
      using any of the following relationship types. In many cases, this would result
      in nonsensical relationships. The following table is a guideline for reasonable
      relationships between CDA entries, and is not a conformance constraint.
    }
    Property entryRelationship : TcdaEntryRelationshipList read GetentryRelationship write SetentryRelationship;
    {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
    }
    Property reference : TcdaReferenceList read Getreference write Setreference;
    {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
    }
    Property precondition : TcdaPreconditionList read Getprecondition write Setprecondition;
    // attributes
    Property negationInd : boolean read FnegationInd write SetnegationInd;
    {
      false if negationInd is null, true if negationInd is either true or false
    }
    Property HasNegationInd : boolean read FHasNegationInd;
  End;


  {
       A derivative of the RIM Supply class, used for representing the
       provision of a material by one entity to another.

       The Supply class represents dispensing, whereas the SubstanceAdministration
       class represents administration. Prescriptions are complex activities that
       involve both an administration request to the patient (e.g. take digoxin
       0.125mg by mouth once per day) and a supply request to the pharmacy (e.g.
       dispense 30 tablets, with 5 refills). This should be represented in CDA by
       a SubstanceAdministration entry that has a component Supply entry. The
       nested Supply entry can have Supply.independentInd set to "false" to
       signal that the Supply cannot stand alone, without it's containing
       SubstanceAdministration.
  }
  TcdaSupply = class (TcdaClinicalStatement)
  private
    Fauthor: TcdaAuthorList;
    FentryRelationship: TcdaEntryRelationshipList;
    Finformant: TcdaInformant12List;
    Fparticipant: TcdaParticipant2List;
    Fperformer: TcdaPerformer2List;
    Fprecondition: TcdaPreconditionList;
    Fproduct: TcdaProduct;
    Freference: TcdaReferenceList;
    Fspecimen: TcdaSpecimenList;
    Fsubject: TcdaSubject;
    FindependentInd: Tv3BL;
    Fcode: Tv3CD;
    FstatusCode: Tv3CS;
    Ftext: Tv3ED;
    FexpectedUseTime: Tv3IVL;
    FrepeatNumber: Tv3IVL;
    FpriorityCode: Tv3ListCD;
    Fquantity: Tv3PQ;
    FeffectiveTime: Tv3QSET;
    Function Getauthor : TcdaAuthorList;
    procedure Setauthor(const Value: TcdaAuthorList);
    procedure Setcode(const Value: Tv3CD);
    procedure SeteffectiveTime(const Value: Tv3QSET);
    Function GetentryRelationship : TcdaEntryRelationshipList;
    procedure SetentryRelationship(const Value: TcdaEntryRelationshipList);
    procedure SetexpectedUseTime(const Value: Tv3IVL);
    procedure SetindependentInd(const Value: Tv3BL);
    Function Getinformant : TcdaInformant12List;
    procedure Setinformant(const Value: TcdaInformant12List);
    Function Getparticipant : TcdaParticipant2List;
    procedure Setparticipant(const Value: TcdaParticipant2List);
    Function Getperformer : TcdaPerformer2List;
    procedure Setperformer(const Value: TcdaPerformer2List);
    Function Getprecondition : TcdaPreconditionList;
    procedure Setprecondition(const Value: TcdaPreconditionList);
    procedure SetpriorityCode(const Value: Tv3ListCD);
    procedure Setproduct(const Value: TcdaProduct);
    procedure Setquantity(const Value: Tv3PQ);
    Function Getreference : TcdaReferenceList;
    procedure Setreference(const Value: TcdaReferenceList);
    procedure SetrepeatNumber(const Value: Tv3IVL);
    Function Getspecimen : TcdaSpecimenList;
    procedure Setspecimen(const Value: TcdaSpecimenList);
    procedure SetstatusCode(const Value: Tv3CS);
    procedure Setsubject(const Value: TcdaSubject);
    procedure Settext(const Value: Tv3ED);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue :TV3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TcdaSupply; Overload;
    Function Clone(parent : Tv3Base) : TcdaSupply; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function CDAClassNameV : String; Override;
    Function CDAClassTypeV : TCDAClassType; Override;
  published
    Property code : Tv3CD read Fcode write Setcode;
    Property text : Tv3ED read Ftext write Settext;
    Property statusCode : Tv3CS read FstatusCode write SetstatusCode;
    Property effectiveTime : Tv3QSET read FeffectiveTime write SeteffectiveTime;
    Property priorityCode : Tv3ListCD read FpriorityCode write SetpriorityCode;
    Property repeatNumber : Tv3IVL read FrepeatNumber write SetrepeatNumber;
    Property independentInd : Tv3BL read FindependentInd write SetindependentInd;
    Property quantity : Tv3PQ read Fquantity write Setquantity;
    Property expectedUseTime : Tv3IVL read FexpectedUseTime write SetexpectedUseTime;
    {
       The subject participant represents the primary target of the entries
       recorded in the document or section. Most of the time the subject is
       the same as the recordTarget, but need not be, for instance when the
       subject is a fetus observed in an obstetrical ultrasound.

       The subject participant can be ascribed to a CDA section or a CDA entry.
       It propagates to nested components, unless overridden. The subject of a
       document is presumed to be the patient.
    }
    Property subject : TcdaSubject read Fsubject write Setsubject;
    {
      A specimen is a part of some entity, typically the subject, that is the target
      of focused laboratory, radiology or other observations. In many clinical
      observations, such as physical examination of a patient, the patient is the
      subject of the observation, and there is no specimen. The specimen participant
      is only used when observations are made against some substance or object that
      is taken or derived from the subject.
    }
    Property specimen : TcdaSpecimenList read Getspecimen write Setspecimen;
    {
      The dispensed product is associated with the Supply act via a product
      participant, which connects to the same ManufacturedProduct role used
      for SubstanceAdministration
    }
    Property product : TcdaProduct read Fproduct write Setproduct;
    {
      The performer is a person who carries out or will carry out a particular
      act. The performer need not be the principal responsible participant,
      e.g. a surgery resident operating under supervision of attending surgeon
      is a performer.
    }
    Property performer : TcdaPerformer2List read Getperformer write Setperformer;
    {
       Represents the humans and/or machines that authored the document or section
    }
    Property author : TcdaAuthorList read Getauthor write Setauthor;
    {
       An informant (or source of information) is a person that provides relevant information,
       such as the parent of a comatose patient who describes the patient's behavior prior to
       the onset of coma.
    }
    Property informant : TcdaInformant12List read Getinformant write Setinformant;
    {
       Used to represent other participants not explicitly mentioned by other
       classes, that were somehow involved in the documented acts.
    }
    Property participant : TcdaParticipant2List read Getparticipant write Setparticipant;
    {
      CDA has identified and modeled various link and reference scenarios. These scenarios
      enable CDA entries to be semantically linked to entries that exist within the same
      document (by traversing the entryRelationship class) or to objects external to
      it (by traversing the reference class).

      NOTE: The CDA specification permits any CDA entry to relate to any CDA entry
      using any of the following relationship types. In many cases, this would result
      in nonsensical relationships. The following table is a guideline for reasonable
      relationships between CDA entries, and is not a conformance constraint.
    }
    Property entryRelationship : TcdaEntryRelationshipList read GetentryRelationship write SetentryRelationship;
    {
      CDA entries can reference external objects such as external images and prior
      reports. These external objects are not part of the authenticated document
      content. They contain sufficient attributes to enable an explicit reference
      rather than duplicating the entire referenced object. The CDA entry that
      wraps the external reference can be used to encode the specific portions
      of the external reference that are addressed in the narrative block.

      Each object allows for an identifier and a code, and contains the
      RIM Act.text attribute, which can be used to store the URL and MIME
      type of the object. External objects always have a fixed moodCode of "EVN".
    }
    Property reference : TcdaReferenceList read Getreference write Setreference;
    {
       The precondition class, derived from the ActRelationship class, is used along
       with the Criterion class to express a condition that must hold true before some over activity occurs.
    }
    Property precondition : TcdaPreconditionList read Getprecondition write Setprecondition;
    // attributes
    //Property classCode : String read FclassCode; // ActClassSupply    fixed="SPLY"
  End;

  {
    A list of Author
  }
  TcdaAuthorList = class (TcdaBaseCollection)
  private
    Function GetAuthors(iIndex : Integer) : TcdaAuthor;
    procedure SetAuthors(iIndex: Integer; const Value: TcdaAuthor);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Procedure InsertObject(iIndex : Integer; oObject : TcdaAuthor);
    Function Link : TcdaAuthorList; Overload;
    Function Clone(parent : Tv3Base) : TcdaAuthorList; Overload;
    {
      Add a Author to the end of the list.
    }
    function Append : TcdaAuthor;
    {
      Add an already existing Author to the end of the list.
    }
    Procedure AddItem(value : TcdaAuthor);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaAuthor) : Integer;
    {
       Insert Author before the designated index (0 = first item)
    }
    function Insert(iIndex : Integer) : TcdaAuthor;
    {
       Insert an existing Author before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaAuthor);
    {
       Get the iIndexth Author. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaAuthor;
    {
       Set the iIndexth Author. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaAuthor);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Authors[iIndex : Integer] : TcdaAuthor read GetAuthors write SetAuthors; default;
  End;

  {
    A list of EntryRelationship
  }
  TcdaEntryRelationshipList = class (TcdaBaseCollection)
  private
    Function GetEntryRelationships(iIndex : Integer) : TcdaEntryRelationship;
    procedure SetEntryRelationships(iIndex: Integer; const Value: TcdaEntryRelationship);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaEntryRelationshipList; Overload;
    Function Clone(parent : Tv3Base) : TcdaEntryRelationshipList; Overload;
    Procedure InsertObject(iIndex : Integer; oObject : TcdaEntryRelationship);
    {
      Add a EntryRelationship to the end of the list.
    }
    Function Append : TcdaEntryRelationship;
    {
      Add an already existing EntryRelationship to the end of the list.
    }
    Procedure AddItem(value : TcdaEntryRelationship);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaEntryRelationship) : Integer;
    {
       Insert EntryRelationship before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaEntryRelationship;
    {
       Insert an existing EntryRelationship before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaEntryRelationship);
    {
       Get the iIndexth EntryRelationship. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaEntryRelationship;
    {
       Set the iIndexth EntryRelationship. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaEntryRelationship);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property EntryRelationships[iIndex : Integer] : TcdaEntryRelationship read GetEntryRelationships write SetEntryRelationships; default;
  End;

  {
    A list of Informant12
  }
  TcdaInformant12List = class (TcdaBaseCollection)
  private
    Function GetInformant12s(iIndex : Integer) : TcdaInformant12;
    procedure SetInformant12s(iIndex: Integer; const Value: TcdaInformant12);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaInformant12List; Overload;
    Function Clone(parent : Tv3Base) : TcdaInformant12List; Overload;
    {
      Add a Informant12 to the end of the list.
    }
    Function Append : TcdaInformant12;
    {
      Add an already existing Informant12 to the end of the list.
    }
    Procedure AddItem(value : TcdaInformant12);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaInformant12) : Integer;
    {
       Insert Informant12 before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaInformant12;
    {
       Insert an existing Informant12 before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaInformant12);
    {
       Get the iIndexth Informant12. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaInformant12;
    {
       Set the iIndexth Informant12. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaInformant12);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Informant12s[iIndex : Integer] : TcdaInformant12 read GetInformant12s write SetInformant12s; default;
  End;

  {
    A list of Participant2
  }
  TcdaParticipant2List = class (TcdaBaseCollection)
  private
    Function GetParticipant2s(iIndex : Integer) : TcdaParticipant2;
    procedure SetParticipant2s(iIndex: Integer; const Value: TcdaParticipant2);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Procedure InsertObject(iIndex : Integer; oObject : TcdaParticipant2);
    Function Link : TcdaParticipant2List; Overload;
    Function Clone(parent : Tv3Base) : TcdaParticipant2List; Overload;
    {
      Add a Participant2 to the end of the list.
    }
    Function Append : TcdaParticipant2;
    {
      Add an already existing Participant2 to the end of the list.
    }
    Procedure AddItem(value : TcdaParticipant2);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaParticipant2) : Integer;
    {
       Insert Participant2 before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaParticipant2;
    {
       Insert an existing Participant2 before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaParticipant2);
    {
       Get the iIndexth Participant2. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaParticipant2;
    {
       Set the iIndexth Participant2. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaParticipant2);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Participant2s[iIndex : Integer] : TcdaParticipant2 read GetParticipant2s write SetParticipant2s; default;
  End;

  {
    A list of Performer2
  }
  TcdaPerformer2List = class (TcdaBaseCollection)
  private
    Function GetPerformer2s(iIndex : Integer) : TcdaPerformer2;
    procedure SetPerformer2s(iIndex: Integer; const Value: TcdaPerformer2);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaPerformer2List; Overload;
    Function Clone(parent : Tv3Base) : TcdaPerformer2List; Overload;
    {
      Add a Performer2 to the end of the list.
    }
    Function Append : TcdaPerformer2;
    {
      Add an already existing Performer2 to the end of the list.
    }
    Procedure AddItem(value : TcdaPerformer2);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaPerformer2) : Integer;
    {
       Insert Performer2 before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaPerformer2;
    {
       Insert an existing Performer2 before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaPerformer2);
    {
       Get the iIndexth Performer2. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaPerformer2;
    {
       Set the iIndexth Performer2. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaPerformer2);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Performer2s[iIndex : Integer] : TcdaPerformer2 read GetPerformer2s write SetPerformer2s; default;
  End;

  {
    A list of Precondition
  }
  TcdaPreconditionList = class (TcdaBaseCollection)
  private
    Function GetPreconditions(iIndex : Integer) : TcdaPrecondition;
    procedure SetPreconditions(iIndex: Integer; const Value: TcdaPrecondition);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaPreconditionList; Overload;
    Function Clone(parent : Tv3Base) : TcdaPreconditionList; Overload;
    {
      Add a Precondition to the end of the list.
    }
    Function Append : TcdaPrecondition;
    {
      Add an already existing Precondition to the end of the list.
    }
    Procedure AddItem(value : TcdaPrecondition);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaPrecondition) : Integer;
    {
       Insert Precondition before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaPrecondition;
    {
       Insert an existing Precondition before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaPrecondition);
    {
       Get the iIndexth Precondition. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaPrecondition;
    {
       Set the iIndexth Precondition. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaPrecondition);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Preconditions[iIndex : Integer] : TcdaPrecondition read GetPreconditions write SetPreconditions; default;
  End;

  {
    A list of Reference
  }
  TcdaReferenceList = class (TcdaBaseCollection)
  private
    Function GetReferences(iIndex : Integer) : TcdaReference;
    procedure SetReferences(iIndex: Integer; const Value: TcdaReference);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaReferenceList; Overload;
    Function Clone(parent : Tv3Base) : TcdaReferenceList; Overload;
    {
      Add a Reference to the end of the list.
    }
    Function Append : TcdaReference;
    {
      Add an already existing Reference to the end of the list.
    }
    Procedure AddItem(value : TcdaReference);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaReference) : Integer;
    {
       Insert Reference before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaReference;
    {
       Insert an existing Reference before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaReference);
    {
       Get the iIndexth Reference. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaReference;
    {
       Set the iIndexth Reference. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaReference);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property References[iIndex : Integer] : TcdaReference read GetReferences write SetReferences; default;
  End;

  {
    A list of Specimen
  }
  TcdaSpecimenList = class (TcdaBaseCollection)
  private
    Function GetSpecimens(iIndex : Integer) : TcdaSpecimen;
    procedure SetSpecimens(iIndex: Integer; const Value: TcdaSpecimen);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaSpecimenList; Overload;
    Function Clone(parent : Tv3Base) : TcdaSpecimenList; Overload;
    {
      Add a Specimen to the end of the list.
    }
    Function Append : TcdaSpecimen;
    {
      Add an already existing Specimen to the end of the list.
    }
    Procedure AddItem(value : TcdaSpecimen);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaSpecimen) : Integer;
    {
       Insert Specimen before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaSpecimen;
    {
       Insert an existing Specimen before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaSpecimen);
    {
       Get the iIndexth Specimen. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaSpecimen;
    {
       Set the iIndexth Specimen. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaSpecimen);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Specimens[iIndex : Integer] : TcdaSpecimen read GetSpecimens write SetSpecimens; default;
  End;

  {
    A list of MaintainedEntity
  }
  TcdaMaintainedEntityList = class (TcdaBaseCollection)
  private
    Function GetMaintainedEntitys(iIndex : Integer) : TcdaMaintainedEntity;
    procedure SetMaintainedEntitys(iIndex: Integer; const Value: TcdaMaintainedEntity);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaMaintainedEntityList; Overload;
    Function Clone(parent : Tv3Base) : TcdaMaintainedEntityList; Overload;
    {
      Add a MaintainedEntity to the end of the list.
    }
    Function Append : TcdaMaintainedEntity;
    {
      Add an already existing MaintainedEntity to the end of the list.
    }
    Procedure AddItem(value : TcdaMaintainedEntity);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaMaintainedEntity) : Integer;
    {
       Insert MaintainedEntity before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaMaintainedEntity;
    {
       Insert an existing MaintainedEntity before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaMaintainedEntity);
    {
       Get the iIndexth MaintainedEntity. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaMaintainedEntity;
    {
       Set the iIndexth MaintainedEntity. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaMaintainedEntity);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property MaintainedEntitys[iIndex : Integer] : TcdaMaintainedEntity read GetMaintainedEntitys write SetMaintainedEntitys; default;
  End;

  {
    A list of Authenticator
  }
  TcdaAuthenticatorList = class (TcdaBaseCollection)
  private
    Function GetAuthenticators(iIndex : Integer) : TcdaAuthenticator;
    procedure SetAuthenticators(iIndex: Integer; const Value: TcdaAuthenticator);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaAuthenticatorList; Overload;
    Function Clone(parent : Tv3Base) : TcdaAuthenticatorList; Overload;
    {
      Add a Authenticator to the end of the list.
    }
    Function Append : TcdaAuthenticator;
    {
      Add an already existing Authenticator to the end of the list.
    }
    Procedure AddItem(value : TcdaAuthenticator);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaAuthenticator) : Integer;
    {
       Insert Authenticator before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaAuthenticator;
    {
       Insert an existing Authenticator before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaAuthenticator);
    {
       Get the iIndexth Authenticator. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaAuthenticator;
    {
       Set the iIndexth Authenticator. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaAuthenticator);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Authenticators[iIndex : Integer] : TcdaAuthenticator read GetAuthenticators write SetAuthenticators; default;
  End;

  {
    A list of Authorization
  }
  TcdaAuthorizationList = class (TcdaBaseCollection)
  private
    Function GetAuthorizations(iIndex : Integer) : TcdaAuthorization;
    procedure SetAuthorizations(iIndex: Integer; const Value: TcdaAuthorization);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaAuthorizationList; Overload;
    Function Clone(parent : Tv3Base) : TcdaAuthorizationList; Overload;
    {
      Add a Authorization to the end of the list.
    }
    Function Append : TcdaAuthorization;
    {
      Add an already existing Authorization to the end of the list.
    }
    Procedure AddItem(value : TcdaAuthorization);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaAuthorization) : Integer;
    {
       Insert Authorization before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaAuthorization;
    {
       Insert an existing Authorization before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaAuthorization);
    {
       Get the iIndexth Authorization. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaAuthorization;
    {
       Set the iIndexth Authorization. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaAuthorization);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Authorizations[iIndex : Integer] : TcdaAuthorization read GetAuthorizations write SetAuthorizations; default;
  End;

  {
    A list of DocumentationOf
  }
  TcdaDocumentationOfList = class (TcdaBaseCollection)
  private
    Function GetDocumentationOfs(iIndex : Integer) : TcdaDocumentationOf;
    procedure SetDocumentationOfs(iIndex: Integer; const Value: TcdaDocumentationOf);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaDocumentationOfList; Overload;
    Function Clone(parent : Tv3Base) : TcdaDocumentationOfList; Overload;
    {
      Add a DocumentationOf to the end of the list.
    }
    Function Append : TcdaDocumentationOf;
    {
      Add an already existing DocumentationOf to the end of the list.
    }
    Procedure AddItem(value : TcdaDocumentationOf);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaDocumentationOf) : Integer;
    {
       Insert DocumentationOf before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaDocumentationOf;
    {
       Insert an existing DocumentationOf before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaDocumentationOf);
    {
       Get the iIndexth DocumentationOf. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaDocumentationOf;
    {
       Set the iIndexth DocumentationOf. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaDocumentationOf);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property DocumentationOfs[iIndex : Integer] : TcdaDocumentationOf read GetDocumentationOfs write SetDocumentationOfs; default;
  End;

  {
    A list of InformationRecipient
  }
  TcdaInformationRecipientList = class (TcdaBaseCollection)
  private
    Function GetInformationRecipients(iIndex : Integer) : TcdaInformationRecipient;
    procedure SetInformationRecipients(iIndex: Integer; const Value: TcdaInformationRecipient);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaInformationRecipientList; Overload;
    Function Clone(parent : Tv3Base) : TcdaInformationRecipientList; Overload;
    {
      Add a TcdaInformationRecipient to the end of the list.
    }
    Function Append : TcdaInformationRecipient;
    {
      Add an already existing TcdaInformationRecipient to the end of the list.
    }
    Procedure AddItem(value : TcdaInformationRecipient);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaInformationRecipient) : Integer;
    {
       Insert InformationRecipient before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaInformationRecipient;
    {
       Insert an existing InformationRecipient before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaInformationRecipient);
    {
       Get the iIndexth InformationRecipient. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaInformationRecipient;
    {
       Set the iIndexth InformationRecipient. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaInformationRecipient);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property InformationRecipients[iIndex : Integer] : TcdaInformationRecipient read GetInformationRecipients write SetInformationRecipients; default;
  End;

  {
    A list of InFulfillmentOf
  }
  TcdaInFulfillmentOfList = class (TcdaBaseCollection)
  private
    Function GetInFulfillmentOfs(iIndex : Integer) : TcdaInFulfillmentOf;
    procedure SetInFulfillmentOfs(iIndex: Integer; const Value: TcdaInFulfillmentOf);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaInFulfillmentOfList; Overload;
    Function Clone(parent : Tv3Base) : TcdaInFulfillmentOfList; Overload;
    {
      Add a InFulfillmentOf to the end of the list.
    }
    Function Append : TcdaInFulfillmentOf;
    {
      Add an already existing InFulfillmentOf to the end of the list.
    }
    Procedure AddItem(value : TcdaInFulfillmentOf);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaInFulfillmentOf) : Integer;
    {
       Insert InFulfillmentOf before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaInFulfillmentOf;
    {
       Insert an existing InFulfillmentOf before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaInFulfillmentOf);
    {
       Get the iIndexth InFulfillmentOf. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaInFulfillmentOf;
    {
       Set the iIndexth InFulfillmentOf. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaInFulfillmentOf);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property InFulfillmentOfs[iIndex : Integer] : TcdaInFulfillmentOf read GetInFulfillmentOfs write SetInFulfillmentOfs; default;
  End;

  {
    A list of Participant1
  }
  TcdaParticipant1List = class (TcdaBaseCollection)
  private
    Function GetParticipant1s(iIndex : Integer) : TcdaParticipant1;
    procedure SetParticipant1s(iIndex: Integer; const Value: TcdaParticipant1);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaParticipant1List; Overload;
    Function Clone(parent : Tv3Base) : TcdaParticipant1List; Overload;
    {
      Add a Participant1 to the end of the list.
    }
    Function Append : TcdaParticipant1;
    {
      Add an already existing Participant1 to the end of the list.
    }
    Procedure AddItem(value : TcdaParticipant1);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaParticipant1) : Integer;
    {
       Insert Participant1 before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaParticipant1;
    {
       Insert an existing Participant1 before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaParticipant1);
    {
       Get the iIndexth Participant1. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaParticipant1;
    {
       Set the iIndexth Participant1. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaParticipant1);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Participant1s[iIndex : Integer] : TcdaParticipant1 read GetParticipant1s write SetParticipant1s; default;
  End;

  {
    A list of RecordTarget
  }
  TcdaRecordTargetList = class (TcdaBaseCollection)
  private
    Function GetRecordTargets(iIndex : Integer) : TcdaRecordTarget;
    procedure SetRecordTargets(iIndex: Integer; const Value: TcdaRecordTarget);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaRecordTargetList; Overload;
    Function Clone(parent : Tv3Base) : TcdaRecordTargetList; Overload;
    {
      Add a RecordTarget to the end of the list.
    }
    Function Append : TcdaRecordTarget;
    {
      Add an already existing RecordTarget to the end of the list.
    }
    Procedure AddItem(value : TcdaRecordTarget);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaRecordTarget) : Integer;
    {
       Insert RecordTarget before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaRecordTarget;
    {
       Insert an existing RecordTarget before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaRecordTarget);
    {
       Get the iIndexth RecordTarget. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaRecordTarget;
    {
       Set the iIndexth RecordTarget. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaRecordTarget);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property RecordTargets[iIndex : Integer] : TcdaRecordTarget read GetRecordTargets write SetRecordTargets; default;
  End;

  {
    A list of RelatedDocument
  }
  TcdaRelatedDocumentList = class (TcdaBaseCollection)
  private
    Function GetRelatedDocuments(iIndex : Integer) : TcdaRelatedDocument;
    procedure SetRelatedDocuments(iIndex: Integer; const Value: TcdaRelatedDocument);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaRelatedDocumentList; Overload;
    Function Clone(parent : Tv3Base) : TcdaRelatedDocumentList; Overload;
    {
      Add a RelatedDocument to the end of the list.
    }
    Function Append : TcdaRelatedDocument;
    {
      Add an already existing RelatedDocument to the end of the list.
    }
    Procedure AddItem(value : TcdaRelatedDocument);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaRelatedDocument) : Integer;
    {
       Insert RelatedDocument before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaRelatedDocument;
    {
       Insert an existing RelatedDocument before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaRelatedDocument);
    {
       Get the iIndexth RelatedDocument. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaRelatedDocument;
    {
       Set the iIndexth RelatedDocument. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaRelatedDocument);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property RelatedDocuments[iIndex : Integer] : TcdaRelatedDocument read GetRelatedDocuments write SetRelatedDocuments; default;
  End;

  {
    A list of EncounterParticipant
  }
  TcdaEncounterParticipantList = class (TcdaBaseCollection)
  private
    Function GetEncounterParticipants(iIndex : Integer) : TcdaEncounterParticipant;
    procedure SetEncounterParticipants(iIndex: Integer; const Value: TcdaEncounterParticipant);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaEncounterParticipantList; Overload;
    Function Clone(parent : Tv3Base) : TcdaEncounterParticipantList; Overload;
    {
      Add a EncounterParticipant to the end of the list.
    }
    Function Append : TcdaEncounterParticipant;
    {
      Add an already existing EncounterParticipant to the end of the list.
    }
    Procedure AddItem(value : TcdaEncounterParticipant);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaEncounterParticipant) : Integer;
    {
       Insert EncounterParticipant before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaEncounterParticipant;
    {
       Insert an existing EncounterParticipant before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaEncounterParticipant);
    {
       Get the iIndexth EncounterParticipant. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaEncounterParticipant;
    {
       Set the iIndexth EncounterParticipant. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaEncounterParticipant);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property EncounterParticipants[iIndex : Integer] : TcdaEncounterParticipant read GetEncounterParticipants write SetEncounterParticipants; default;
  End;

  {
    A list of ReferenceRange
  }
  TcdaReferenceRangeList = class (TcdaBaseCollection)
  private
    Function GetReferenceRanges(iIndex : Integer) : TcdaReferenceRange;
    procedure SetReferenceRanges(iIndex: Integer; const Value: TcdaReferenceRange);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaReferenceRangeList; Overload;
    Function Clone(parent : Tv3Base) : TcdaReferenceRangeList; Overload;
    {
      Add a ReferenceRange to the end of the list.
    }
    Function Append : TcdaReferenceRange;
    {
      Add an already existing ReferenceRange to the end of the list.
    }
    Procedure AddItem(value : TcdaReferenceRange);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaReferenceRange) : Integer;
    {
       Insert ReferenceRange before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaReferenceRange;
    {
       Insert an existing ReferenceRange before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaReferenceRange);
    {
       Get the iIndexth ReferenceRange. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaReferenceRange;
    {
       Set the iIndexth ReferenceRange. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaReferenceRange);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property ReferenceRanges[iIndex : Integer] : TcdaReferenceRange read GetReferenceRanges write SetReferenceRanges; default;
  End;

  {
    A list of Component4
  }
  TcdaComponent4List = class (TcdaBaseCollection)
  private
    Function GetComponent4s(iIndex : Integer) : TcdaComponent4;
    procedure SetComponent4s(iIndex: Integer; const Value: TcdaComponent4);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Procedure InsertObject(iIndex : Integer; oObject : TcdaComponent4);
    Function Link : TcdaComponent4List; Overload;
    Function Clone(parent : Tv3Base) : TcdaComponent4List; Overload;
    {
      Add a Component4 to the end of the list.
    }
    Function Append : TcdaComponent4;
    {
      Add an already existing Component4 to the end of the list.
    }
    Procedure AddItem(value : TcdaComponent4);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaComponent4) : Integer;
    {
       Insert Component4 before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaComponent4;
    {
       Insert an existing Component4 before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaComponent4);
    {
       Get the iIndexth Component4. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaComponent4;
    {
       Set the iIndexth Component4. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaComponent4);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Component4s[iIndex : Integer] : TcdaComponent4 read GetComponent4s write SetComponent4s; default;
  End;

  {
    A list of Guardian
  }
  TcdaGuardianList = class (TcdaBaseCollection)
  private
    Function GetGuardians(iIndex : Integer) : TcdaGuardian;
    procedure SetGuardians(iIndex: Integer; const Value: TcdaGuardian);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaGuardianList; Overload;
    Function Clone(parent : Tv3Base) : TcdaGuardianList; Overload;
    {
      Add a Guardian to the end of the list.
    }
    Function Append : TcdaGuardian;
    {
      Add an already existing Guardian to the end of the list.
    }
    Procedure AddItem(value : TcdaGuardian);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaGuardian) : Integer;
    {
       Insert Guardian before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaGuardian;
    {
       Insert an existing Guardian before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaGuardian);
    {
       Get the iIndexth Guardian. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaGuardian;
    {
       Set the iIndexth Guardian. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaGuardian);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Guardians[iIndex : Integer] : TcdaGuardian read GetGuardians write SetGuardians; default;
  End;

  {
    A list of LanguageCommunication
  }
  TcdaLanguageCommunicationList = class (TcdaBaseCollection)
  private
    Function GetLanguageCommunications(iIndex : Integer) : TcdaLanguageCommunication;
    procedure SetLanguageCommunications(iIndex: Integer; const Value: TcdaLanguageCommunication);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaLanguageCommunicationList; Overload;
    Function Clone(parent : Tv3Base) : TcdaLanguageCommunicationList; Overload;
    {
      Add a LanguageCommunicaion to the end of the list.
    }
    Function Append : TcdaLanguageCommunication;
    {
      Add an already existing LanguageCommunicaion to the end of the list.
    }
    Procedure AddItem(value : TcdaLanguageCommunication);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaLanguageCommunication) : Integer;
    {
       Insert LanguageCommunication before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaLanguageCommunication;
    {
       Insert an existing LanguageCommunication before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaLanguageCommunication);
    {
       Get the iIndexth LanguageCommunication. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaLanguageCommunication;
    {
       Set the iIndexth LanguageCommunication. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaLanguageCommunication);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property LanguageCommunications[iIndex : Integer] : TcdaLanguageCommunication read GetLanguageCommunications write SetLanguageCommunications; default;
  End;

  {
    A list of EntityIdentifier
  }
  TcdaEntityIdentifierList = class (TcdaBaseCollection)
  private
    Function GetEntityIdentifiers(iIndex : Integer) : TcdaEntityIdentifier;
    procedure SetEntityIdentifiers(iIndex: Integer; const Value: TcdaEntityIdentifier);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaEntityIdentifierList; Overload;
    Function Clone(parent : Tv3Base) : TcdaEntityIdentifierList; Overload;
    {
      Add a LanguageCommunicaion to the end of the list.
    }
    Function Append : TcdaEntityIdentifier;
    {
      Add an already existing LanguageCommunicaion to the end of the list.
    }
    Procedure AddItem(value : TcdaEntityIdentifier);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaEntityIdentifier) : Integer;
    {
       Insert EntityIdentifier before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaEntityIdentifier;
    {
       Insert an existing EntityIdentifier before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaEntityIdentifier);
    {
       Get the iIndexth EntityIdentifier. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaEntityIdentifier;
    {
       Set the iIndexth EntityIdentifier. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaEntityIdentifier);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property EntityIdentifiers[iIndex : Integer] : TcdaEntityIdentifier read GetEntityIdentifiers write SetEntityIdentifiers; default;
  End;


  {
    A list of Entry
  }
  TcdaEntryList = class (TcdaBaseCollection)
  private
    Function GetEntrys(iIndex : Integer) : TcdaEntry;
    procedure SetEntrys(iIndex: Integer; const Value: TcdaEntry);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Procedure InsertObject(iIndex : Integer; oObject : TcdaEntry);
    Function Link : TcdaEntryList; Overload;
    Function Clone(parent : Tv3Base) : TcdaEntryList; Overload;
    {
      Add a Entry to the end of the list.
    }
    Function Append : TcdaEntry;
    {
      Add an already existing Entry to the end of the list.
    }
    Procedure AddItem(value : TcdaEntry);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaEntry) : Integer;
    {
       Insert Entry before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaEntry;
    {
       Insert an existing Entry before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaEntry);
    {
       Get the iIndexth Entry. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaEntry;
    {
       Set the iIndexth Entry. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaEntry);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Entrys[iIndex : Integer] : TcdaEntry read GetEntrys write SetEntrys; default;
  End;

  {
    A list of Performer1
  }
  TcdaPerformer1List = class (TcdaBaseCollection)
  private
    Function GetPerformer1s(iIndex : Integer) : TcdaPerformer1;
    procedure SetPerformer1s(iIndex: Integer; const Value: TcdaPerformer1);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaPerformer1List; Overload;
    Function Clone(parent : Tv3Base) : TcdaPerformer1List; Overload;
    {
      Add a Performer1 to the end of the list.
    }
    Function Append : TcdaPerformer1;
    {
      Add an already existing Performer1 to the end of the list.
    }
    Procedure AddItem(value : TcdaPerformer1);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaPerformer1) : Integer;
    {
       Insert Performer1 before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaPerformer1;
    {
       Insert an existing Performer1 before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaPerformer1);
    {
       Get the iIndexth Performer1. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaPerformer1;
    {
       Set the iIndexth Performer1. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaPerformer1);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Performer1s[iIndex : Integer] : TcdaPerformer1 read GetPerformer1s write SetPerformer1s; default;
  End;

  {
    A list of ComponentSect
  }
  TcdaComponentSectList = class (TcdaBaseCollection)
  private
    Function GetComponentSects(iIndex : Integer) : TcdaComponentSect;
    procedure SetComponentSects(iIndex: Integer; const Value: TcdaComponentSect);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaComponentSectList; Overload;
    Function Clone(parent : Tv3Base) : TcdaComponentSectList; Overload;
    {
      Add a Component to the end of the list.
    }
    Function Append : TcdaComponentSect;
    {
      Add an already existing ComponentSect to the end of the list.
    }
    Procedure AddItem(value : TcdaComponentSect);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaComponentSect) : Integer;
    {
       Insert ComponentSect before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaComponentSect;
    {
       Insert an existing ComponentSect before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaComponentSect);
    {
       Get the iIndexth ComponentSect. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaComponentSect;
    {
       Set the iIndexth ComponentSect. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaComponentSect);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property ComponentSects[iIndex : Integer] : TcdaComponentSect read GetComponentSects write SetComponentSects; default;
  End;

  {
    A list of RegionOfInterest_value
  }
  TcdaRegionOfInterest_valueList = class (TcdaBaseCollection)
  private
    Function GetRegionOfInterest_values(iIndex : Integer) : TcdaRegionOfInterest_value;
    procedure SetRegionOfInterest_values(iIndex: Integer; const Value: TcdaRegionOfInterest_value);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Function Link : TcdaRegionOfInterest_valueList; Overload;
    Function Clone(parent : Tv3Base) : TcdaRegionOfInterest_valueList; Overload;
      {
        Add a RegionOfInterest_value to the end of the list.
      }
    Function Append : TcdaRegionOfInterest_value;
      {
        Add an already existing RegionOfInterest_value to the end of the list.
      }
    Procedure AddItem(value : TcdaRegionOfInterest_value);
      {
        See if an item is already in the list. returns -1 if not in the list
      }
    Function IndexOf(value : TcdaRegionOfInterest_value) : Integer;
    {
       Insert RegionOfInterest_value before the designated index (0 = first item)
    }
    Function Insert(iIndex : Integer) : TcdaRegionOfInterest_value;
    {
       Insert an existing RegionOfInterest_value before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaRegionOfInterest_value);
    {
       Get the iIndexth RegionOfInterest_value. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaRegionOfInterest_value;
    {
       Set the iIndexth RegionOfInterest_value. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaRegionOfInterest_value);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property RegionOfInterest_values[iIndex : Integer] : TcdaRegionOfInterest_value read GetRegionOfInterest_values write SetRegionOfInterest_values; default;
  End;

implementation


Function RIMType(const s : String):String;
Begin
  if SameText(s, 'Place') Then
    result := 'Place'
  else if SameText(s, 'HealthCareFacility') Then
    result := 'Role'
  else if SameText(s, 'AssignedCustodian') Then
    result := 'Role'
  else if SameText(s, 'AssociatedEntity') Then
    result := 'Role'
  else if SameText(s, 'IntendedRecipient') Then
    result := 'Role'
  else if SameText(s, 'DocumentationOf') Then
    result := 'ActRelationship'
  else if SameText(s, 'Component1') Then
    result := 'ActRelationship'
  else if SameText(s, 'RelatedDocument') Then
    result := 'ActRelationship'
  else if SameText(s, 'ResponsibleParty') Then
    result := 'Participation'
  else if SameText(s, 'Participant1') Then
    result := 'Participation'
  else if SameText(s, 'Custodian') Then
    result := 'Participation'
  else if SameText(s, 'InformationRecipient') Then
    result := 'Participation'
  else if SameText(s, 'EncounterParticipant') Then
    result := 'Participation'
  else if SameText(s, 'LegalAuthenticator') Then
    result := 'Participation'
  else if SameText(s, 'Authenticator') Then
    result := 'Participation'
  else if SameText(s, 'Author') Then
    result := 'Participation'
  else if SameText(s, 'RecordTarget') Then
    result := 'Participation'
  else if SameText(s, 'Location') Then
    result := 'Participation'
  else if SameText(s, 'Component2') Then
    result := 'ActRelationship'
  else if SameText(s, 'Component5') Then
    result := 'ActRelationship'
  else if SameText(s, 'Entry') Then
    result := 'ActRelationship'
  else if SameText(s, 'Section') Then
    result := 'Act'
  else if SameText(s, 'StructuredBody') Then
    result := 'Act'
  else if SameText(s, 'NonXMLBody') Then
    result := 'Act'
  else if SameText(s, 'ParentDocument') Then
    result := 'Document'
  else if SameText(s, 'ClinicalDocument') Then
    result := 'Document'
  else if SameText(s, 'EncompassingEncounter') Then
    result := 'PatientEncounter'
  else if SameText(s, 'bodyChoice') Then
    result := 'Act'
  else if SameText(s, 'ComponentSect') Then
    result := 'ActRelationship'
  else if SameText(s, 'Informant12') Then
    result := 'Participation'
  else if SameText(s, 'InFulfillmentOf') Then
    result := 'ActRelationship'
  else if SameText(s, 'Order') Then
    result := 'Act'
  else if SameText(s, 'RelatedEntity') Then
    result := 'Role'
  else if SameText(s, 'CustodianOrganization') Then
    result := 'Organization'
  else if SameText(s, 'PatientRole') Then
    result := 'Patient'
  else if SameText(s, 'Patient') Then
    result := 'Person'
  else if SameText(s, 'AssignedAuthor') Then
    result := 'Role'
  else if SameText(s, 'Person') Then
    result := 'Person'
  else if SameText(s, 'RelatedSubject') Then
    result := 'Role'
  else if SameText(s, 'SubjectPerson') Then
    result := 'Person'
  else if SameText(s, 'AuthoringDevice') Then
    result := 'Device'
  else if SameText(s, 'AuthorChoice') Then
    result := 'Entity'
  else if SameText(s, 'MaintainedEntity') Then
    result := 'Role'
  else if SameText(s, 'AssignedEntity') Then
    result := 'Role'
  else if SameText(s, 'Guardian') Then
    result := 'Role'
  else if SameText(s, 'GuardianChoice') Then
    result := 'Entity'
  else if SameText(s, 'Consent') Then
    result := 'Act'
  else if SameText(s, 'Authorization') Then
    result := 'ActRelationship'
  else if SameText(s, 'Subject') Then
    result := 'Participation'
  else if SameText(s, 'DataEnterer') Then
    result := 'Participation'
  else if SameText(s, 'Birthplace') Then
    result := 'Role'
  else if SameText(s, 'LanguageCommunication') Then
    result := 'LanguageCommunication'
  else if SameText(s, 'EntityIdentifier') Then
    result := 'EntityIdentifier'
  else if SameText(s, 'informantChoice') Then
    result := 'Role'
  else if SameText(s, 'Reference') Then
    result := 'ActRelationship'
  else if SameText(s, 'Observation') Then
    result := 'Observation'
  else if SameText(s, 'ObservationMedia') Then
    result := 'Observation'
  else if SameText(s, 'RegionOfInterest') Then
    result := 'Observation'
  else if SameText(s, 'SubstanceAdministration') Then
    result := 'SubstanceAdministration'
  else if SameText(s, 'Supply') Then
    result := 'Supply'
  else if SameText(s, 'Procedure') Then
    result := 'Procedure'
  else if SameText(s, 'ExternalAct') Then
    result := 'Act'
  else if SameText(s, 'ExternalObservation') Then
    result := 'Observation'
  else if SameText(s, 'ExternalProcedure') Then
    result := 'Procedure'
  else if SameText(s, 'externalActChoice') Then
    result := 'Act'
  else if SameText(s, 'clinicalStatement') Then
    result := 'Act'
  else if SameText(s, 'EntryRelationship') Then
    result := 'ActRelationship'
  else if SameText(s, 'Consumable') Then
    result := 'Participation'
  else if SameText(s, 'Precondition') Then
    result := 'ActRelationship'
  else if SameText(s, 'Criterion') Then
    result := 'Observation'
  else if SameText(s, 'ManufacturedProduct') Then
    result := 'Role'
  else if SameText(s, 'Encounter') Then
    result := 'PatientEncounter'
  else if SameText(s, 'ParticipantRole') Then
    result := 'Role'
  else if SameText(s, 'PlayingEntity') Then
    result := 'Entity'
  else if SameText(s, 'Entity') Then
    result := 'Entity'
  else if SameText(s, 'Device') Then
    result := 'Device'
  else if SameText(s, 'EntityChoice') Then
    result := 'Entity'
  else if SameText(s, 'ExternalDocument') Then
    result := 'Document'
  else if SameText(s, 'Participant2') Then
    result := 'Participation'
  else if SameText(s, 'Performer2') Then
    result := 'Participation'
  else if SameText(s, 'ReferenceRange') Then
    result := 'ActRelationship'
  else if SameText(s, 'ObservationRange') Then
    result := 'Observation'
  else if SameText(s, 'Organizer') Then
    result := 'Act'
  else if SameText(s, 'Act') Then
    result := 'Act'
  else if SameText(s, 'Product') Then
    result := 'Participation'
  else if SameText(s, 'Material') Then
    result := 'ManufacturedMaterial'
  else if SameText(s, 'LabeledDrug') Then
    result := 'ManufacturedMaterial'
  else if SameText(s, 'DrugOrOtherMaterial') Then
    result := 'Entity'
  else if SameText(s, 'Organization') Then
    result := 'Organization'
  else if SameText(s, 'Specimen') Then
    result := 'Participation'
  else if SameText(s, 'SpecimenRole') Then
    result := 'Role'
  else if SameText(s, 'ServiceEvent') Then
    result := 'Act'
  else if SameText(s, 'Performer1') Then
    result := 'Participation'
  else if SameText(s, 'OrganizationPartOf') Then
    result := 'Role'
  else if SameText(s, 'Component4') Then
    result := 'ActRelationship'
  else
    result := 'Unknown Type '+s;
End;

{ TcdaClinicalStatement }

Function TcdaClinicalStatement.CDAClassNameV : String;
Begin
  Result := 'ClinicalStatement';
End;

Function TcdaClinicalStatement.CDAClassTypeV : TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaClinicalStatement.Assign(oSource: TFslObject);
begin
  inherited;
  FclassCode := TcdaAct(oSource).FclassCode;
  FmoodCode := TcdaAct(oSource).FmoodCode;
  id := TcdaAct(oSource).id.Clone(self);
end;

procedure TcdaClinicalStatement.DoClear;
begin
  inherited;
  FclassCode := '';
  FmoodCode := '';
  id := Nil;
end;

function TcdaClinicalStatement.Clone(parent : Tv3Base): TcdaClinicalStatement;
begin
  Result := TcdaClinicalStatement(inherited Clone(parent));
end;

constructor TcdaClinicalStatement.Create;
begin
  inherited;
end;

destructor TcdaClinicalStatement.Destroy;
begin
  inherited;
end;

function TcdaClinicalStatement.Link: TcdaClinicalStatement;
begin
  Result := TcdaClinicalStatement(Inherited Link);
end;

procedure TcdaClinicalStatement.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
end;

procedure TcdaClinicalStatement.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else
    Inherited;
end;


function TcdaClinicalStatement.ClassCodeIsFixed: Boolean;
begin
  result := false;
end;

function TcdaClinicalStatement.MoodCodeIsFixed: Boolean;
begin
  result := false;
end;

procedure TcdaClinicalStatement.SetClassCode(const sValue: String);
begin
  if ClassCodeIsFixed Then
    raise ECDAException.create('classCode is fixed')
  Else
    FclassCode := sValue;
end;

procedure TcdaClinicalStatement.SetMoodCode(const sValue: String);
begin
  if MoodCodeIsFixed Then
    raise ECDAException.create('moodCode is fixed')
  Else
    FmoodCode := sValue;
end;

procedure TcdaClinicalStatement.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
end;

function TcdaClinicalStatement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fid.sizeInBytes);
end;

{ TcdaAct }

Function TcdaAct.CDAClassNameV : String;
Begin
  Result := 'Act';
End;

Function TcdaAct.CDAClassTypeV : TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaAct.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaAct(oSource).id.Clone(self);
  code := TcdaAct(oSource).code.Clone(self);
  text := TcdaAct(oSource).text.Clone(self);
  statusCode := TcdaAct(oSource).statusCode.Clone(self);
  effectiveTime := TcdaAct(oSource).effectiveTime.Clone(self);
  priorityCode := TcdaAct(oSource).priorityCode.Clone(self);
  languageCode := TcdaAct(oSource).languageCode.Clone(self);
  subject := TcdaAct(oSource).subject.Clone(self);
  specimen := TcdaAct(oSource).specimen.Clone(self);
  performer := TcdaAct(oSource).performer.Clone(self);
  author := TcdaAct(oSource).author.Clone(self);
  informant := TcdaAct(oSource).informant.Clone(self);
  participant := TcdaAct(oSource).participant.Clone(self);
  entryRelationship := TcdaAct(oSource).entryRelationship.Clone(self);
  reference := TcdaAct(oSource).reference.Clone(self);
  precondition := TcdaAct(oSource).precondition.Clone(self);
  negationInd := TcdaAct(oSource).negationInd;
end;

procedure TcdaAct.DoClear;
begin
  inherited;
  code := nil;
  text := nil;
  statusCode := nil;
  effectiveTime := nil;
  priorityCode := nil;
  languageCode := nil;
  subject := nil;
  negationInd := False;

  author.Clear;
  entryRelationship.Clear;
  id.Clear;
  informant.Clear;
  participant.Clear;
  performer.Clear;
  precondition.Clear;
  reference.Clear;
  specimen.Clear;
end;

function TcdaAct.Clone(parent : Tv3Base): TcdaAct;
begin
  Result := TcdaAct(inherited Clone(parent));
end;

constructor TcdaAct.Create;
begin
  inherited;
  author := TcdaAuthorList.Create(self);
  entryRelationship := TcdaEntryRelationshipList.Create(self);
  id := Tv3ListII.Create(self);
  informant := TcdaInformant12List.Create(self);
  participant := TcdaParticipant2List.Create(self);
  performer := TcdaPerformer2List.Create(self);
  precondition := TcdaPreconditionList.Create(self);
  reference := TcdaReferenceList.Create(self);
  specimen := TcdaSpecimenList.Create(self);
end;

destructor TcdaAct.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Ftext.Free;
  FstatusCode.Free;
  FeffectiveTime.Free;
  FpriorityCode.Free;
  FlanguageCode.Free;
  Fsubject.Free;
  Fspecimen.Free;
  Fperformer.Free;
  Fauthor.Free;
  Finformant.Free;
  Fparticipant.Free;
  FentryRelationship.Free;
  Freference.Free;
  Fprecondition.Free;
  inherited;
end;


function TcdaAct.Link: TcdaAct;
begin
  Result := TcdaAct(Inherited Link);
end;

procedure TcdaAct.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'negationInd', FHasNegationInd, FnegationInd));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD')); // Tv3CD
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'priorityCode', FpriorityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'statusCode', FstatusCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCode', FlanguageCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II')); // Tv3ListII
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entryRelationship', FentryRelationship, rmpctList, 'EntryRelationship')); // TcdaEntryRelationshipList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant2')); // TcdaParticipant2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer2')); // TcdaPerformer2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'precondition', Fprecondition, rmpctList, 'Precondition')); // TcdaPreconditionList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, rmpctList, 'Reference')); // TcdaReferenceList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimen', Fspecimen, rmpctList, 'Specimen')); // TcdaSpecimenList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

procedure TcdaAct.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then  author.parent := self;

end;

procedure TcdaAct.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaAct.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then  effectiveTime.parent := self;

end;

procedure TcdaAct.SetentryRelationship(const Value: TcdaEntryRelationshipList);
begin
  FentryRelationship.Free;
  FentryRelationship := Value;
  if FentryRelationship <> nil then  entryRelationship.parent := self;

end;


procedure TcdaAct.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then  informant.parent := self;

end;

procedure TcdaAct.SetlanguageCode(const Value: Tv3CS);
begin
  FlanguageCode.Free;
  FlanguageCode := Value;
  if FlanguageCode <> nil then  languageCode.parent := self;

end;


function TcdaAct.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, FentryRelationship.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, Fperformer.sizeInBytes);
  inc(result, Fprecondition.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, Fspecimen.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, FpriorityCode.sizeInBytes);
  inc(result, FstatusCode.sizeInBytes);
  inc(result, FlanguageCode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
end;

{ TcdaCustodian }

Function TcdaCustodian.CDAClassNameV : String;
Begin
  Result := 'Custodian';
End;

function TcdaCustodian.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaCustodian.Assign(oSource: TFslObject);
begin
  inherited;
  assignedCustodian := TcdaCustodian(oSource).assignedCustodian.Clone(self);
end;

procedure TcdaCustodian.DoClear;
begin
  inherited;
  assignedCustodian := nil;
end;

function TcdaCustodian.Clone(parent : Tv3Base): TcdaCustodian;
begin
  Result := TcdaCustodian(inherited Clone(parent));
end;

constructor TcdaCustodian.Create;
begin
  inherited;
  FtypeCode := 'CST';
end;

destructor TcdaCustodian.Destroy;
begin
  FassignedCustodian.Free;
  inherited;
end;

function TcdaCustodian.Link: TcdaCustodian;
begin
  Result := TcdaCustodian(Inherited Link);
end;

procedure TcdaCustodian.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedCustodian', FassignedCustodian, 'AssignedCustodian')); // TcdaAssignedCustodian;
end;

procedure TcdaCustodian.SetassignedCustodian(const Value: TcdaAssignedCustodian);
begin
  FassignedCustodian.Free;
  FassignedCustodian := Value;
  if FassignedCustodian <> nil then  assignedCustodian.parent := self;

end;

procedure TcdaCustodian.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedCustodian' Then
    assignedCustodian := TcdaAssignedCustodian(aValue.AsType(TcdaAssignedCustodian)).Clone(self)
  Else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  Else
    inherited;
end;

function TcdaCustodian.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedCustodian.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaCustodianOrganization }

Function TcdaCustodianOrganization.CDAClassNameV : String;
Begin
  Result := 'CustodianOrganization';
End;

function TcdaCustodianOrganization.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaCustodianOrganization.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaCustodianOrganization(oSource).id.Clone(self);
  name := TcdaCustodianOrganization(oSource).name.Clone(self);
  telecom := TcdaCustodianOrganization(oSource).telecom.Clone(self);
  addr := TcdaCustodianOrganization(oSource).addr.Clone(self);
end;

procedure TcdaCustodianOrganization.DoClear;
begin
  inherited;
  id.Clear;
  name := nil;
  telecom := nil;
  addr := nil;
end;

function TcdaCustodianOrganization.Clone(parent : Tv3Base): TcdaCustodianOrganization;
begin
  Result := TcdaCustodianOrganization(inherited Clone(parent));
end;

constructor TcdaCustodianOrganization.Create;
begin
  inherited;
  FclassCode := 'ORG';
  FdeterminerCode := 'INSTANCE';
  id := Tv3ListII.Create(self);
end;

destructor TcdaCustodianOrganization.Destroy;
begin
  Fid.Free;
  Fname.Free;
  Ftelecom.Free;
  Faddr.Free;
  inherited;
end;

function TcdaCustodianOrganization.Link: TcdaCustodianOrganization;
begin
  Result := TcdaCustodianOrganization(Inherited Link);
end;

procedure TcdaCustodianOrganization.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, 'AD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II')); // Tv3ListII
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, 'EN'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, 'TEL'));
end;

procedure TcdaCustodianOrganization.Setaddr(const Value: Tv3AD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then  addr.parent := self;

end;

procedure TcdaCustodianOrganization.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaCustodianOrganization.Setname(const Value: Tv3EN);
begin
  Fname.Free;
  Fname := Value;
  if Fname <> nil then  name.parent := self;

end;

procedure TcdaCustodianOrganization.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'addr' Then
    addr := Tv3AD(aValue.AsType(Tv3AD)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'name' Then
    name := Tv3EN(aValue.AsType(Tv3EN)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3TEL(aValue.AsType(Tv3TEL)).Clone(self)
  else
    inherited;
end;

procedure TcdaCustodianOrganization.Settelecom(const Value: Tv3TEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then  telecom.parent := self;

end;

function TcdaCustodianOrganization.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Faddr.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
  inc(result, Fid.sizeInBytes);
  inc(result, Fname.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
end;

{ TcdaDataEnterer }

Function TcdaDataEnterer.CDAClassNameV : String;
Begin
  Result := 'DataEnterer';
End;

function TcdaDataEnterer.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaDataEnterer.Assign(oSource: TFslObject);
begin
  inherited;
  time := TcdaDataEnterer(oSource).time.Clone(self);
  assignedEntity := TcdaDataEnterer(oSource).assignedEntity.Clone(self);
end;

procedure TcdaDataEnterer.DoClear;
begin
  inherited;
  time := nil;
  assignedEntity := nil;
end;

function TcdaDataEnterer.Clone(parent : Tv3Base): TcdaDataEnterer;
begin
  Result := TcdaDataEnterer(inherited Clone(parent));
end;

constructor TcdaDataEnterer.Create;
begin
  inherited;
  FtypeCode := 'ENT';
  FcontextControlCode := 'OP';
end;

destructor TcdaDataEnterer.Destroy;
begin
  Ftime.Free;
  FassignedEntity.Free;
  inherited;
end;

function TcdaDataEnterer.Link: TcdaDataEnterer;
begin
  Result := TcdaDataEnterer(Inherited Link);
end;

procedure TcdaDataEnterer.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'contextControlCode', FcontextControlCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'time', Ftime, 'TS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedEntity', FassignedEntity, 'AssignedEntity')); // TcdaAssignedEntity;
end;

procedure TcdaDataEnterer.SetassignedEntity(const Value: TcdaAssignedEntity);
begin
  FassignedEntity.Free;
  FassignedEntity := Value;
  if FassignedEntity <> nil then  assignedEntity.parent := self;

end;

procedure TcdaDataEnterer.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedEntity' Then
    assignedEntity := TcdaAssignedEntity(aValue.AsType(TcdaAssignedEntity)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'contextControlCode' Then
    FcontextControlCode := aValue.AsString
  else if aValue.Name = 'time' Then
    time := Tv3TS(aValue.AsType(Tv3TS)).Clone(self)
  else
    inherited;
end;

procedure TcdaDataEnterer.Settime(const Value: Tv3TS);
begin
  Ftime.Free;
  Ftime := Value;
  if Ftime <> nil then  time.parent := self;

end;


function TcdaDataEnterer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedEntity.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, (FcontextControlCode.length * sizeof(char)) + 12);
  inc(result, Ftime.sizeInBytes);
end;

procedure TcdaAct.SetnegationInd(const Value: boolean);
begin
  FHasNegationInd := True;
  FnegationInd := Value;
end;

procedure TcdaAct.Setparticipant(const Value: TcdaParticipant2List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then  participant.parent := self;

end;

procedure TcdaAct.Setperformer(const Value: TcdaPerformer2List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then  performer.parent := self;

end;

procedure TcdaAct.Setprecondition(const Value: TcdaPreconditionList);
begin
  Fprecondition.Free;
  Fprecondition := Value;
  if Fprecondition <> nil then  precondition.parent := self;

end;

procedure TcdaAct.SetpriorityCode(const Value: Tv3CD);
begin
  FpriorityCode.Free;
  FpriorityCode := Value;
  if FpriorityCode <> nil then  priorityCode.parent := self;

end;


procedure TcdaAct.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'entryRelationship' Then
    entryRelationship := TcdaEntryRelationshipList(aValue.AsType(TcdaEntryRelationshipList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant2List(aValue.AsType(TcdaParticipant2List)).Clone(self)
  else if aValue.Name = 'performer' Then
    performer := TcdaPerformer2List(aValue.AsType(TcdaPerformer2List)).Clone(self)
  else if aValue.Name = 'precondition' Then
    precondition := TcdaPreconditionList(aValue.AsType(TcdaPreconditionList)).Clone(self)
  else if aValue.Name = 'reference' Then
    reference := TcdaReferenceList(aValue.AsType(TcdaReferenceList)).Clone(self)
  else if aValue.Name = 'specimen' Then
    specimen := TcdaSpecimenList(aValue.AsType(TcdaSpecimenList)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'negationInd' Then
    aValue.AsBool(FHasNegationInd, FnegationInd)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'priorityCode' Then
    priorityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'statusCode' Then
    statusCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'languageCode' Then
    languageCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    Inherited;
end;

procedure TcdaAct.Setreference(const Value: TcdaReferenceList);
begin
  Freference.Free;
  Freference := Value;
  if Freference <> nil then  reference.parent := self;

end;

procedure TcdaAct.Setspecimen(const Value: TcdaSpecimenList);
begin
  Fspecimen.Free;
  Fspecimen := Value;
  if Fspecimen <> nil then  specimen.parent := self;

end;

procedure TcdaAct.SetstatusCode(const Value: Tv3CS);
begin
  FstatusCode.Free;
  FstatusCode := Value;
  if FstatusCode <> nil then  statusCode.parent := self;

end;

procedure TcdaAct.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then  subject.parent := self;

end;


procedure TcdaAct.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then  text.parent := self;

end;


{ TcdaAssignedAuthor }

Function TcdaAssignedAuthor.CDAClassNameV : String;
Begin
  Result := 'AssignedAuthor';
End;

function TcdaAssignedAuthor.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaAssignedAuthor.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaAssignedAuthor(oSource).id.Clone(self);
  code := TcdaAssignedAuthor(oSource).code.Clone(self);
  addr := TcdaAssignedAuthor(oSource).addr.Clone(self);
  telecom := TcdaAssignedAuthor(oSource).telecom.Clone(self);
  assignedPerson := TcdaAssignedAuthor(oSource).assignedPerson.Clone(self);
  assignedAuthoringDevice := TcdaAssignedAuthor(oSource).assignedAuthoringDevice.Clone(self);
  representedOrganization := TcdaAssignedAuthor(oSource).representedOrganization.Clone(self);
end;

procedure TcdaAssignedAuthor.DoClear;
begin
  inherited;
  id.Clear;
  code := nil;
  addr.Clear;
  telecom.Clear;
  assignedPerson := nil;
  assignedAuthoringDevice := Nil;
  representedOrganization := nil;
end;

function TcdaAssignedAuthor.Clone(parent : Tv3Base): TcdaAssignedAuthor;
begin
  Result := TcdaAssignedAuthor(inherited Clone(parent));
end;

constructor TcdaAssignedAuthor.Create;
begin
  inherited;
  FclassCode := 'ASSIGNED';
  addr := Tv3ListAD.Create(self);
  id := Tv3ListII.Create(self);
  telecom := Tv3ListTEL.Create(self);
end;

destructor TcdaAssignedAuthor.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Faddr.Free;
  Ftelecom.Free;
  FassignedPerson.Free;
  FassignedAuthoringDevice.Free;
  FrepresentedOrganization.Free;
  inherited;
end;

function TcdaAssignedAuthor.Link: TcdaAssignedAuthor;
begin
  Result := TcdaAssignedAuthor(Inherited Link);
end;

procedure TcdaAssignedAuthor.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, rmpctList, 'AD')); // Tv3ListAD
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II')); // Tv3ListII
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, rmpctList, 'TEL')); // Tv3ListTEL
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedAuthoringDevice', FassignedAuthoringDevice, 'AuthoringDevice')); // TcdaAuthoringDevice;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'representedOrganization', FrepresentedOrganization, 'Organization')); // TcdaOrganization;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedPerson', FassignedPerson, 'Person')); // TcdaPerson;
end;

procedure TcdaAssignedAuthor.Setaddr(const Value: Tv3ListAD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then  addr.parent := self;

end;

procedure TcdaAssignedAuthor.SetassignedAuthoringDevice(const Value: TcdaAuthoringDevice);
begin
  FassignedAuthoringDevice.Free;
  FassignedAuthoringDevice := Value;
  if FassignedAuthoringDevice <> nil then  assignedAuthoringDevice.parent := self;

end;

procedure TcdaAssignedAuthor.SetassignedPerson(const Value: TcdaPerson);
begin
  FassignedPerson.Free;
  FassignedPerson := Value;
  if FassignedPerson <> nil then  assignedPerson.parent := self;

end;

procedure TcdaAssignedAuthor.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaAssignedAuthor.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaAssignedAuthor.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedAuthoringDevice' Then
    assignedAuthoringDevice := TcdaAuthoringDevice(aValue.AsType(TcdaAuthoringDevice)).Clone(self)
  else if aValue.Name = 'representedOrganization' Then
    representedOrganization := TcdaOrganization(aValue.AsType(TcdaOrganization)).Clone(self)
  else if aValue.Name = 'assignedPerson' Then
    assignedPerson := TcdaPerson(aValue.AsType(TcdaPerson)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'addr' Then
    addr := Tv3ListAD(aValue.AsType(Tv3ListAD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3ListTEL(aValue.AsType(Tv3ListTEL)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaAssignedAuthor.SetrepresentedOrganization(const Value: TcdaOrganization);
begin
  FrepresentedOrganization.Free;
  FrepresentedOrganization := Value;
  if FrepresentedOrganization <> nil then  representedOrganization.parent := self;

end;

procedure TcdaAssignedAuthor.Settelecom(const Value: Tv3ListTEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then  telecom.parent := self;

end;


function TcdaAssignedAuthor.GetAuthorChoice: TcdaAuthorChoice;
begin
  if FassignedPerson <> nil Then
    result := FassignedPerson
  Else
    result := FassignedAuthoringDevice;
end;

function TcdaAssignedAuthor.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedAuthoringDevice.sizeInBytes);
  inc(result, FrepresentedOrganization.sizeInBytes);
  inc(result, FassignedPerson.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, Faddr.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaAssignedCustodian }

Function TcdaAssignedCustodian.CDAClassNameV : String;
Begin
  Result := 'AssignedCustodian';
End;

function TcdaAssignedCustodian.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaAssignedCustodian.Assign(oSource: TFslObject);
begin
  inherited;
  representedCustodianOrganization := TcdaAssignedCustodian(oSource).representedCustodianOrganization.Clone(self);
end;

procedure TcdaAssignedCustodian.DoClear;
begin
  inherited;
  representedCustodianOrganization := nil;
end;

function TcdaAssignedCustodian.Clone(parent : Tv3Base): TcdaAssignedCustodian;
begin
  Result := TcdaAssignedCustodian(inherited Clone(parent));
end;

constructor TcdaAssignedCustodian.Create;
begin
  inherited;
  FclassCode := 'ASSIGNED';
end;

destructor TcdaAssignedCustodian.Destroy;
begin
  FrepresentedCustodianOrganization.Free;
  inherited;
end;

function TcdaAssignedCustodian.Link: TcdaAssignedCustodian;
begin
  Result := TcdaAssignedCustodian(Inherited Link);
end;

procedure TcdaAssignedCustodian.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'representedCustodianOrganization', FrepresentedCustodianOrganization, 'CustodianOrganization')); // TcdaCustodianOrganization;
end;

procedure TcdaAssignedCustodian.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'representedCustodianOrganization' Then
    representedCustodianOrganization := TcdaCustodianOrganization(aValue.AsType(TcdaCustodianOrganization)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaAssignedCustodian.SetrepresentedCustodianOrganization(const Value: TcdaCustodianOrganization);
begin
  FrepresentedCustodianOrganization.Free;
  FrepresentedCustodianOrganization := Value;
  if FrepresentedCustodianOrganization <> nil then  representedCustodianOrganization.parent := self;

end;

function TcdaAssignedCustodian.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FrepresentedCustodianOrganization.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaInformantChoice }

Function TcdaInformantChoice.CDAClassNameV : String;
Begin
  Result := 'InformantChoice';
End;

function TcdaInformantChoice.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaInformantChoice.Assign(oSource: TFslObject);
begin
  inherited;
  FclassCode := TcdaInformantChoice(oSource).FclassCode;
  code := TcdaInformantChoice(oSource).code.Clone(self);
  addr := TcdaInformantChoice(oSource).addr.Clone(self);
  telecom := TcdaInformantChoice(oSource).telecom.Clone(self);
end;

procedure TcdaInformantChoice.DoClear;
begin
  inherited;
  FclassCode := '';
  code := Nil;
  addr.Clear;
  telecom.Clear;
end;

function TcdaInformantChoice.Clone(parent : Tv3Base): TcdaInformantChoice;
begin
  Result := TcdaInformantChoice(inherited Clone(parent));
end;

constructor TcdaInformantChoice.Create;
begin
  inherited;
  addr := Tv3ListAD.Create(self);
  telecom := Tv3ListTEL.Create(self);
end;

destructor TcdaInformantChoice.Destroy;
begin
  Fcode.Free;
  Faddr.Free;
  Ftelecom.Free;
  inherited;
end;

function TcdaInformantChoice.Link: TcdaInformantChoice;
begin
  Result := TcdaInformantChoice(Inherited Link);
end;

procedure TcdaInformantChoice.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, rmpctList, 'AD')); // Tv3ListAD
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, rmpctList, 'TEL')); // Tv3ListTEL
end;

procedure TcdaInformantChoice.Setaddr(const Value: Tv3ListAD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then  addr.parent := self;

end;

procedure TcdaInformantChoice.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaInformantChoice.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    SetClassCode(aValue.AsString)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'addr' Then
    addr := Tv3ListAD(aValue.AsType(Tv3ListAD)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3ListTEL(aValue.AsType(Tv3ListTEL)).Clone(self)
  else
    inherited;
end;

procedure TcdaInformantChoice.Settelecom(const Value: Tv3ListTEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then  telecom.parent := self;

end;

function TcdaInformantChoice.ClassCodeIsFixed: Boolean;
begin
  result := false;
end;

procedure TcdaInformantChoice.SetClassCode(const sValue: String);
begin
  if ClassCodeIsFixed Then
    raise ECDAException.create('classCode is fixed')
  Else
    FclassCode := sValue;
end;

function TcdaInformantChoice.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, Faddr.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
end;

{ TcdaAssignedEntity }

Function TcdaAssignedEntity.CDAClassNameV : String;
Begin
  Result := 'AssignedEntity';
End;

function TcdaAssignedEntity.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaAssignedEntity.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaAssignedEntity(oSource).id.Clone(self);
  assignedPerson := TcdaAssignedEntity(oSource).assignedPerson.Clone(self);
  representedOrganization := TcdaAssignedEntity(oSource).representedOrganization.Clone(self);
end;

procedure TcdaAssignedEntity.DoClear;
begin
  inherited;
  id.Clear;
  assignedPerson := Nil;
  representedOrganization := Nil;
end;

function TcdaAssignedEntity.Clone(parent : Tv3Base): TcdaAssignedEntity;
begin
  Result := TcdaAssignedEntity(inherited Clone(parent));
end;

constructor TcdaAssignedEntity.Create;
begin
  inherited;
  FclassCode := 'ASSIGNED';
  id := Tv3ListII.Create(self);
end;

destructor TcdaAssignedEntity.Destroy;
begin
  Fid.Free;
  FassignedPerson.Free;
  FrepresentedOrganization.Free;
  inherited;
end;

function TcdaAssignedEntity.Link: TcdaAssignedEntity;
begin
  Result := TcdaAssignedEntity(Inherited Link);
end;

procedure TcdaAssignedEntity.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II')); // Tv3ListII
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'representedOrganization', FrepresentedOrganization, 'Organization')); // TcdaOrganization;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedPerson', FassignedPerson, 'Person')); // TcdaPerson;
end;

procedure TcdaAssignedEntity.SetassignedPerson(const Value: TcdaPerson);
begin
  FassignedPerson.Free;
  FassignedPerson := Value;
  if FassignedPerson <> nil then  assignedPerson.parent := self;

end;

procedure TcdaAssignedEntity.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaAssignedEntity.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'representedOrganization' Then
    representedOrganization := TcdaOrganization(aValue.AsType(TcdaOrganization)).Clone(self)
  else if aValue.Name = 'assignedPerson' Then
    assignedPerson := TcdaPerson(aValue.AsType(TcdaPerson)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

procedure TcdaAssignedEntity.SetrepresentedOrganization(const Value: TcdaOrganization);
begin
  FrepresentedOrganization.Free;
  FrepresentedOrganization := Value;
  if FrepresentedOrganization <> nil then  representedOrganization.parent := self;

end;

function TcdaAssignedEntity.ClassCodeIsFixed: Boolean;
begin
  result := true;
end;

function TcdaAssignedEntity.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FrepresentedOrganization.sizeInBytes);
  inc(result, FassignedPerson.sizeInBytes);
  inc(result, Fid.sizeInBytes);
end;

{ TcdaAssociatedEntity }

Function TcdaAssociatedEntity.CDAClassNameV : String;
Begin
  Result := 'AssociatedEntity';
End;

function TcdaAssociatedEntity.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaAssociatedEntity.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaAssociatedEntity(oSource).id.Clone(self);
  code := TcdaAssociatedEntity(oSource).code.Clone(self);
  addr := TcdaAssociatedEntity(oSource).addr.Clone(self);
  telecom := TcdaAssociatedEntity(oSource).telecom.Clone(self);
  associatedPerson := TcdaAssociatedEntity(oSource).associatedPerson.Clone(self);
  scopingOrganization := TcdaAssociatedEntity(oSource).scopingOrganization.Clone(self);
  FclassCode := TcdaAssociatedEntity(oSource).FclassCode;
end;

procedure TcdaAssociatedEntity.DoClear;
begin
  inherited;
  id.Clear;
  code := nil;
  addr.Clear;
  telecom.Clear;
  associatedPerson := Nil;
  scopingOrganization := Nil;
  FclassCode := '';
end;

function TcdaAssociatedEntity.Clone(parent : Tv3Base): TcdaAssociatedEntity;
begin
  Result := TcdaAssociatedEntity(inherited Clone(parent));
end;

constructor TcdaAssociatedEntity.Create;
begin
  inherited;
  addr := Tv3ListAD.Create(self);
  id := Tv3ListII.Create(self);
  telecom := Tv3ListTEL.Create(self);
end;

destructor TcdaAssociatedEntity.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Faddr.Free;
  Ftelecom.Free;
  FassociatedPerson.Free;
  FscopingOrganization.Free;
  inherited;
end;

function TcdaAssociatedEntity.Link: TcdaAssociatedEntity;
begin
  Result := TcdaAssociatedEntity(Inherited Link);
end;

procedure TcdaAssociatedEntity.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, rmpctList, 'AD')); // Tv3ListAD
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II')); // Tv3ListII
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, rmpctList, 'TEL')); // Tv3ListTEL
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'scopingOrganization', FscopingOrganization, 'Organization')); // TcdaOrganization;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'associatedPerson', FassociatedPerson, 'Person')); // TcdaPerson;
end;

procedure TcdaAssociatedEntity.Setaddr(const Value: Tv3ListAD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then  addr.parent := self;

end;

procedure TcdaAssociatedEntity.SetassociatedPerson(const Value: TcdaPerson);
begin
  FassociatedPerson.Free;
  FassociatedPerson := Value;
  if FassociatedPerson <> nil then  associatedPerson.parent := self;

end;

procedure TcdaAssociatedEntity.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaAssociatedEntity.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaAssociatedEntity.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'scopingOrganization' Then
    scopingOrganization := TcdaOrganization(aValue.AsType(TcdaOrganization)).Clone(self)
  else if aValue.Name = 'associatedPerson' Then
    associatedPerson := TcdaPerson(aValue.AsType(TcdaPerson)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'addr' Then
    addr := Tv3ListAD(aValue.AsType(Tv3ListAD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3ListTEL(aValue.AsType(Tv3ListTEL)).Clone(self)
  else
    inherited;
end;

procedure TcdaAssociatedEntity.SetscopingOrganization(const Value: TcdaOrganization);
begin
  FscopingOrganization.Free;
  FscopingOrganization := Value;
  if FscopingOrganization <> nil then  scopingOrganization.parent := self;

end;

procedure TcdaAssociatedEntity.Settelecom(const Value: Tv3ListTEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then  telecom.parent := self;

end;

function TcdaAssociatedEntity.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FscopingOrganization.sizeInBytes);
  inc(result, FassociatedPerson.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, Faddr.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
end;

{ TcdaAuthenticator }

Function TcdaAuthenticator.CDAClassNameV : String;
Begin
  Result := 'Authenticator';
End;

function TcdaAuthenticator.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaAuthenticator.Assign(oSource: TFslObject);
begin
  inherited;
  time := TcdaAuthenticator(oSource).time.Clone(self);
  signatureCode := TcdaAuthenticator(oSource).signatureCode.Clone(self);
  assignedEntity := TcdaAuthenticator(oSource).assignedEntity.Clone(self);
end;

procedure TcdaAuthenticator.DoClear;
begin
  inherited;
  time := Nil;
  signatureCode := Nil;
  assignedEntity := Nil;
end;

function TcdaAuthenticator.Clone(parent : Tv3Base): TcdaAuthenticator;
begin
  Result := TcdaAuthenticator(inherited Clone(parent));
end;

constructor TcdaAuthenticator.Create;
begin
  inherited;
  FtypeCode := 'AUTHEN';
end;

destructor TcdaAuthenticator.Destroy;
begin
  Ftime.Free;
  FsignatureCode.Free;
  FassignedEntity.Free;
  inherited;
end;

function TcdaAuthenticator.Link: TcdaAuthenticator;
begin
  Result := TcdaAuthenticator(Inherited Link);
end;

procedure TcdaAuthenticator.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'signatureCode', FsignatureCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'time', Ftime, 'TS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedEntity', FassignedEntity, 'AssignedEntity')); // TcdaAssignedEntity;
end;

procedure TcdaAuthenticator.SetassignedEntity(const Value: TcdaAssignedEntity);
begin
  FassignedEntity.Free;
  FassignedEntity := Value;
  if FassignedEntity <> nil then  assignedEntity.parent := self;

end;

procedure TcdaAuthenticator.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedEntity' Then
    assignedEntity := TcdaAssignedEntity(aValue.AsType(TcdaAssignedEntity)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'signatureCode' Then
    signatureCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'time' Then
    time := Tv3TS(aValue.AsType(Tv3TS)).Clone(self)
  else
    inherited;
end;

procedure TcdaAuthenticator.SetsignatureCode(const Value: Tv3CS);
begin
  FsignatureCode.Free;
  FsignatureCode := Value;
  if FsignatureCode <> nil then  signatureCode.parent := self;

end;

procedure TcdaAuthenticator.Settime(const Value: Tv3TS);
begin
  Ftime.Free;
  Ftime := Value;
  if Ftime <> nil then  time.parent := self;

end;

function TcdaAuthenticator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedEntity.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, FsignatureCode.sizeInBytes);
  inc(result, Ftime.sizeInBytes);
end;

{ TcdaAuthor }

Function TcdaAuthor.CDAClassNameV : String;
Begin
  Result := 'Author';
End;

function TcdaAuthor.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaAuthor.Assign(oSource: TFslObject);
begin
  inherited;
  functionCode := TcdaAuthor(oSource).functionCode.Clone(self);
  time := TcdaAuthor(oSource).time.Clone(self);
  assignedAuthor := TcdaAuthor(oSource).assignedAuthor.Clone(self);
end;

procedure TcdaAuthor.DoClear;
begin
  inherited;
  functionCode := Nil;
  time := Nil;
  assignedAuthor := Nil;
end;

function TcdaAuthor.Clone(parent : Tv3Base): TcdaAuthor;
begin
  Result := TcdaAuthor(inherited Clone(parent));
end;

constructor TcdaAuthor.Create;
begin
  inherited;
  FtypeCode := 'AUT';
  FcontextControlCode := 'OP';
end;

destructor TcdaAuthor.Destroy;
begin
  FfunctionCode.Free;
  Ftime.Free;
  FassignedAuthor.Free;
  inherited;
end;

function TcdaAuthor.Link: TcdaAuthor;
begin
  Result := TcdaAuthor(Inherited Link);
end;

procedure TcdaAuthor.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'contextControlCode', FcontextControlCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'functionCode', FfunctionCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'time', Ftime, 'TS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedAuthor', FassignedAuthor, 'AssignedAuthor')); // TcdaAssignedAuthor;
end;

procedure TcdaAuthor.SetassignedAuthor(const Value: TcdaAssignedAuthor);
begin
  FassignedAuthor.Free;
  FassignedAuthor := Value;
  if FassignedAuthor <> nil then  assignedAuthor.parent := self;

end;

procedure TcdaAuthor.SetfunctionCode(const Value: Tv3CD);
begin
  FfunctionCode.Free;
  FfunctionCode := Value;
  if FfunctionCode <> nil then  functionCode.parent := self;

end;

procedure TcdaAuthor.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedAuthor' Then
    assignedAuthor := TcdaAssignedAuthor(aValue.AsType(TcdaAssignedAuthor)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'functionCode' Then
    functionCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'contextControlCode' Then
    FcontextControlCode := aValue.AsString
  else if aValue.Name = 'time' Then
    time := Tv3TS(aValue.AsType(Tv3TS)).Clone(self)
  else
    inherited;
end;

procedure TcdaAuthor.Settime(const Value: Tv3TS);
begin
  Ftime.Free;
  Ftime := Value;
  if Ftime <> nil then  time.parent := self;

end;

function TcdaAuthor.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedAuthor.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, FfunctionCode.sizeInBytes);
  inc(result, (FcontextControlCode.length * sizeof(char)) + 12);
  inc(result, Ftime.sizeInBytes);
end;

{ TcdaAuthorChoice }

Function TcdaAuthorChoice.CDAClassNameV : String;
Begin
  Result := 'AuthorChoice';
End;

function TcdaAuthorChoice.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaAuthorChoice.Assign(oSource: TFslObject);
begin
  inherited;
end;

procedure TcdaAuthorChoice.DoClear;
begin
  inherited;
end;

function TcdaAuthorChoice.Clone(parent : Tv3Base): TcdaAuthorChoice;
begin
  Result := TcdaAuthorChoice(inherited Clone(parent));
end;

constructor TcdaAuthorChoice.Create;
begin
  inherited;
  FdeterminerCode := 'INSTANCE';
  FclassCode := GetClassCode;
end;

destructor TcdaAuthorChoice.Destroy;
begin
  inherited;
end;

function TcdaAuthorChoice.Link: TcdaAuthorChoice;
begin
  Result := TcdaAuthorChoice(Inherited Link);
end;

procedure TcdaAuthorChoice.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
end;


procedure TcdaAuthorChoice.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    raise ECDAException.create('Cannot set read-only property')
  else if aValue.Name = 'determinerCode' Then
    raise ECDAException.create('Cannot set read-only property')
  else
    inherited;
end;

function TcdaAuthorChoice.GetClassCode: String;
begin
  raise ECDAException.create('AuthorChoice is an abstract class and should never be create');
end;

function TcdaAuthorChoice.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaAuthoringDevice }

Function TcdaAuthoringDevice.CDAClassNameV : String;
Begin
  Result := 'AuthoringDevice';
End;

function TcdaAuthoringDevice.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaAuthoringDevice.Assign(oSource: TFslObject);
begin
  inherited;
  code := TcdaAuthoringDevice(oSource).code.Clone(self);
  manufacturerModelName := TcdaAuthoringDevice(oSource).manufacturerModelName.Clone(self);
  softwareName := TcdaAuthoringDevice(oSource).softwareName.Clone(self);
  asMaintainedEntity := TcdaAuthoringDevice(oSource).asMaintainedEntity.Clone(self);
end;

procedure TcdaAuthoringDevice.DoClear;
begin
  inherited;
  code := Nil;
  manufacturerModelName := Nil;
  softwareName := Nil;
  asMaintainedEntity := Nil;
end;

function TcdaAuthoringDevice.Clone(parent : Tv3Base): TcdaAuthoringDevice;
begin
  Result := TcdaAuthoringDevice(inherited Clone(parent));
end;

constructor TcdaAuthoringDevice.Create;
begin
  inherited;
end;

destructor TcdaAuthoringDevice.Destroy;
begin
  Fcode.Free;
  FmanufacturerModelName.Free;
  FsoftwareName.Free;
  FasMaintainedEntity.Free;
  inherited;
end;

function TcdaAuthoringDevice.Link: TcdaAuthoringDevice;
begin
  Result := TcdaAuthoringDevice(Inherited Link);
end;

procedure TcdaAuthoringDevice.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'manufacturerModelName', FmanufacturerModelName, 'SC'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'softwareName', FsoftwareName, 'SC'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'asMaintainedEntity', FasMaintainedEntity, rmpctList, 'MaintainedEntity')); // TcdaMaintainedEntityList;
end;

procedure TcdaAuthoringDevice.SetasMaintainedEntity(const Value: TcdaMaintainedEntityList);
begin
  FasMaintainedEntity.Free;
  FasMaintainedEntity := Value;
  if FasMaintainedEntity <> nil then  asMaintainedEntity.parent := self;

end;

procedure TcdaAuthoringDevice.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    code.parent := self;
end;

procedure TcdaAuthoringDevice.SetmanufacturerModelName(const Value: Tv3SC);
begin
  FmanufacturerModelName.Free;
  FmanufacturerModelName := Value;
  if FmanufacturerModelName <> nil then  manufacturerModelName.parent := self;

end;


procedure TcdaAuthoringDevice.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'asMaintainedEntity' Then
    asMaintainedEntity := TcdaMaintainedEntityList(aValue.AsType(TcdaMaintainedEntityList)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'manufacturerModelName' Then
    manufacturerModelName := Tv3SC(aValue.AsType(Tv3SC)).Clone(self)
  else if aValue.Name = 'softwareName' Then
    softwareName := Tv3SC(aValue.AsType(Tv3SC)).Clone(self)
  else
    inherited;
end;

procedure TcdaAuthoringDevice.SetsoftwareName(const Value: Tv3SC);
begin
  FsoftwareName.Free;
  FsoftwareName := Value;
  if FsoftwareName <> nil then  softwareName.parent := self;

end;


function TcdaAuthoringDevice.GetClassCode: String;
begin
  result := 'DEV';
end;

function TcdaAuthoringDevice.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FasMaintainedEntity.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, FmanufacturerModelName.sizeInBytes);
  inc(result, FsoftwareName.sizeInBytes);
end;

{ TcdaAuthorization }

Function TcdaAuthorization.CDAClassNameV : String;
Begin
  Result := 'Authorization';
End;

function TcdaAuthorization.CDAClassTypeV: TCDAClassType;
begin
  result := etActRel;
end;

procedure TcdaAuthorization.Assign(oSource: TFslObject);
begin
  inherited;
  consent := TcdaAuthorization(oSource).consent.Clone(self);
end;

procedure TcdaAuthorization.DoClear;
begin
  inherited;
  consent := Nil;
end;

function TcdaAuthorization.Clone(parent : Tv3Base): TcdaAuthorization;
begin
  Result := TcdaAuthorization(inherited Clone(parent));
end;

constructor TcdaAuthorization.Create;
begin
  inherited;
  FtypeCode := 'AUTH';
end;

destructor TcdaAuthorization.Destroy;
begin
  Fconsent.Free;
  inherited;
end;

function TcdaAuthorization.Link: TcdaAuthorization;
begin
  Result := TcdaAuthorization(Inherited Link);
end;

procedure TcdaAuthorization.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'consent', Fconsent, 'Consent')); // TcdaConsent;
end;

procedure TcdaAuthorization.Setconsent(const Value: TcdaConsent);
begin
  Fconsent.Free;
  Fconsent := Value;
  if Fconsent <> nil then  consent.parent := self;

end;

procedure TcdaAuthorization.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'consent' Then
    consent := TcdaConsent(aValue.AsType(TcdaConsent)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaAuthorization.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fconsent.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaBirthplace }

Function TcdaBirthplace.CDAClassNameV : String;
Begin
  Result := 'Birthplace';
End;

function TcdaBirthplace.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaBirthplace.Assign(oSource: TFslObject);
begin
  inherited;
  place := TcdaBirthplace(oSource).place.Clone(self);
end;

procedure TcdaBirthplace.DoClear;
begin
  inherited;
  place := nil;
end;

function TcdaBirthplace.Clone(parent : Tv3Base): TcdaBirthplace;
begin
  Result := TcdaBirthplace(inherited Clone(parent));
end;

constructor TcdaBirthplace.Create;
begin
  inherited;
  FclassCode := 'BIRTHPL';
end;

destructor TcdaBirthplace.Destroy;
begin
  Fplace.Free;
  inherited;
end;

function TcdaBirthplace.Link: TcdaBirthplace;
begin
  Result := TcdaBirthplace(Inherited Link);
end;

procedure TcdaBirthplace.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'place', Fplace, 'Place')); // TcdaPlace;
end;

procedure TcdaBirthplace.Setplace(const Value: TcdaPlace);
begin
  Fplace.Free;
  Fplace := Value;
  if Fplace <> nil then  place.parent := self;

end;

procedure TcdaBirthplace.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'place' Then
    place := TcdaPlace(aValue.AsType(TcdaPlace)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

function TcdaBirthplace.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fplace.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaClinicalDocument }

Function TcdaClinicalDocument.CDAClassNameV : String;
Begin
  Result := 'ClinicalDocument';
End;

function TcdaClinicalDocument.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaClinicalDocument.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaClinicalDocument(oSource).id.Clone(self);
  code := TcdaClinicalDocument(oSource).code.Clone(self);
  title := TcdaClinicalDocument(oSource).title.Clone(self);
  effectiveTime := TcdaClinicalDocument(oSource).effectiveTime.Clone(self);
  confidentialityCode := TcdaClinicalDocument(oSource).confidentialityCode.Clone(self);
  languageCode := TcdaClinicalDocument(oSource).languageCode.Clone(self);
  setId := TcdaClinicalDocument(oSource).setId.Clone(self);
  versionNumber := TcdaClinicalDocument(oSource).versionNumber.Clone(self);
  copyTime := TcdaClinicalDocument(oSource).copyTime.Clone(self);
  recordTarget := TcdaClinicalDocument(oSource).recordTarget.Clone(self);
  author := TcdaClinicalDocument(oSource).author.Clone(self);
  dataEnterer := TcdaClinicalDocument(oSource).dataEnterer.Clone(self);
  informant := TcdaClinicalDocument(oSource).informant.Clone(self);
  custodian := TcdaClinicalDocument(oSource).custodian.Clone(self);
  informationRecipient := TcdaClinicalDocument(oSource).informationRecipient.Clone(self);
  legalAuthenticator := TcdaClinicalDocument(oSource).legalAuthenticator.Clone(self);
  authenticator := TcdaClinicalDocument(oSource).authenticator.Clone(self);
  participant := TcdaClinicalDocument(oSource).participant.Clone(self);
  inFulfillmentOf := TcdaClinicalDocument(oSource).inFulfillmentOf.Clone(self);
  documentationOf := TcdaClinicalDocument(oSource).documentationOf.Clone(self);
  relatedDocument := TcdaClinicalDocument(oSource).relatedDocument.Clone(self);
  authorization := TcdaClinicalDocument(oSource).authorization.Clone(self);
  componentOf := TcdaClinicalDocument(oSource).componentOf.Clone(self);
  component := TcdaClinicalDocument(oSource).component.Clone(self);
end;

procedure TcdaClinicalDocument.DoClear;
begin
  inherited;
  id := Nil;
  code := Nil;
  title := Nil;
  effectiveTime := Nil;
  confidentialityCode := Nil;
  languageCode := Nil;
  setId := Nil;
  versionNumber := Nil;
  copyTime := Nil;
  dataEnterer := Nil;
  custodian := Nil;
  legalAuthenticator := Nil;
  componentOf := Nil;
  component := Nil;

  authenticator.Clear;
  author.Clear;
  authorization.Clear;
  documentationOf.Clear;
  informant.Clear;
  informationRecipient.Clear;
  inFulfillmentOf.Clear;
  participant.Clear;
  recordTarget.Clear;
  relatedDocument.Clear;
end;

function TcdaClinicalDocument.Clone(parent : Tv3Base): TcdaClinicalDocument;
begin
  Result := TcdaClinicalDocument(inherited Clone(parent));
end;

constructor TcdaClinicalDocument.Create;
begin
  inherited;
  FclassCode := 'DOCCLIN';
  FmoodCode := 'EVN';
  authenticator := TcdaAuthenticatorList.Create(self);
  author := TcdaAuthorList.Create(self);
  authorization := TcdaAuthorizationList.Create(self);
  documentationOf := TcdaDocumentationOfList.Create(self);
  informant := TcdaInformant12List.Create(self);
  informationRecipient := TcdaInformationRecipientList.Create(self);
  inFulfillmentOf := TcdaInFulfillmentOfList.Create(self);
  participant := TcdaParticipant1List.Create(self);
  recordTarget := TcdaRecordTargetList.Create(self);
  relatedDocument := TcdaRelatedDocumentList.Create(self);
end;

destructor TcdaClinicalDocument.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Ftitle.Free;
  FeffectiveTime.Free;
  FconfidentialityCode.Free;
  FlanguageCode.Free;
  FsetId.Free;
  FversionNumber.Free;
  FcopyTime.Free;
  FrecordTarget.Free;
  Fauthor.Free;
  FdataEnterer.Free;
  Finformant.Free;
  Fcustodian.Free;
  FinformationRecipient.Free;
  FlegalAuthenticator.Free;
  Fauthenticator.Free;
  Fparticipant.Free;
  FinFulfillmentOf.Free;
  FdocumentationOf.Free;
  FrelatedDocument.Free;
  Fauthorization.Free;
  FcomponentOf.Free;
  Fcomponent.Free;
  inherited;
end;

function TcdaClinicalDocument.Link: TcdaClinicalDocument;
begin
  Result := TcdaClinicalDocument(Inherited Link);
end;

procedure TcdaClinicalDocument.Set_id(const Value: Tv3II);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.parent := self;
end;

procedure TcdaClinicalDocument.Setauthenticator(const Value: TcdaAuthenticatorList);
begin
  Fauthenticator.Free;
  Fauthenticator := Value;
  if Fauthenticator <> nil then  authenticator.parent := self;

end;

procedure TcdaClinicalDocument.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then  author.parent := self;

end;

procedure TcdaClinicalDocument.Setauthorization(const Value: TcdaAuthorizationList);
begin
  Fauthorization.Free;
  Fauthorization := Value;
  if Fauthorization <> nil then  authorization.parent := self;

end;

procedure TcdaClinicalDocument.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaClinicalDocument.Setcomponent(const Value: TcdaComponent2);
begin
  Fcomponent.Free;
  Fcomponent := Value;
  if Fcomponent <> nil then  component.parent := self;

end;

procedure TcdaClinicalDocument.SetcomponentOf(const Value: TcdaComponent1);
begin
  FcomponentOf.Free;
  FcomponentOf := Value;
  if FcomponentOf <> nil then  componentOf.parent := self;

end;

procedure TcdaClinicalDocument.SetconfidentialityCode(const Value: Tv3CD);
begin
  FconfidentialityCode.Free;
  FconfidentialityCode := Value;
  if FconfidentialityCode <> nil then  confidentialityCode.parent := self;

end;

procedure TcdaClinicalDocument.SetcopyTime(const Value: Tv3TS);
begin
  FcopyTime.Free;
  FcopyTime := Value;
  if FcopyTime <> nil then  copyTime.parent := self;

end;

procedure TcdaClinicalDocument.Setcustodian(const Value: TcdaCustodian);
begin
  Fcustodian.Free;
  Fcustodian := Value;
  if Fcustodian <> nil then  custodian.parent := self;

end;

procedure TcdaClinicalDocument.SetdataEnterer(const Value: TcdaDataEnterer);
begin
  FdataEnterer.Free;
  FdataEnterer := Value;
  if FdataEnterer <> nil then  dataEnterer.parent := self;

end;

procedure TcdaClinicalDocument.SetdocumentationOf(const Value: TcdaDocumentationOfList);
begin
  FdocumentationOf.Free;
  FdocumentationOf := Value;
  if FdocumentationOf <> nil then  documentationOf.parent := self;

end;

procedure TcdaClinicalDocument.SeteffectiveTime(const Value: Tv3TS);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then  effectiveTime.parent := self;

end;

procedure TcdaClinicalDocument.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then  informant.parent := self;

end;

procedure TcdaClinicalDocument.SetinformationRecipient(const Value: TcdaInformationRecipientList);
begin
  FinformationRecipient.Free;
  FinformationRecipient := Value;
  if FinformationRecipient <> nil then  informationRecipient.parent := self;

end;

procedure TcdaClinicalDocument.SetinFulfillmentOf(const Value: TcdaInFulfillmentOfList);
begin
  FinFulfillmentOf.Free;
  FinFulfillmentOf := Value;
  if FinFulfillmentOf <> nil then  inFulfillmentOf.parent := self;

end;

procedure TcdaClinicalDocument.SetlanguageCode(const Value: Tv3CS);
begin
  FlanguageCode.Free;
  FlanguageCode := Value;
  if FlanguageCode <> nil then  languageCode.parent := self;

end;

procedure TcdaClinicalDocument.SetlegalAuthenticator(const Value: TcdaLegalAuthenticator);
begin
  FlegalAuthenticator.Free;
  FlegalAuthenticator := Value;
  if FlegalAuthenticator <> nil then  legalAuthenticator.parent := self;

end;


procedure TcdaClinicalDocument.Setparticipant(const Value: TcdaParticipant1List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then  participant.parent := self;

end;

procedure TcdaClinicalDocument.SetrecordTarget(const Value: TcdaRecordTargetList);
begin
  FrecordTarget.Free;
  FrecordTarget := Value;
  if FrecordTarget <> nil then  recordTarget.parent := self;

end;

procedure TcdaClinicalDocument.SetrelatedDocument(const Value: TcdaRelatedDocumentList);
begin
  FrelatedDocument.Free;
  FrelatedDocument := Value;
  if FrelatedDocument <> nil then  relatedDocument.parent := self;

end;

procedure TcdaClinicalDocument.SetsetId(const Value: Tv3II);
begin
  FsetId.Free;
  FsetId := Value;
  if FsetId <> nil then  setId.parent := self;

end;

procedure TcdaClinicalDocument.Settitle(const Value: Tv3ST);
begin
  Ftitle.Free;
  Ftitle := Value;
  if Ftitle <> nil then  title.parent := self;

end;

procedure TcdaClinicalDocument.SetversionNumber(const Value: Tv3INT);
begin
  FversionNumber.Free;
  FversionNumber := Value;
  if FversionNumber <> nil then  versionNumber.parent := self;

end;

procedure TcdaClinicalDocument.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'confidentialityCode', FconfidentialityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCode', FlanguageCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'setId', FsetId, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'versionNumber', FversionNumber, 'INT'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'title', Ftitle, 'ST'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'copyTime', FcopyTime, 'TS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'TS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'authenticator', Fauthenticator, rmpctList, 'Authenticator')); // TcdaAuthenticatorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'authorization', Fauthorization, rmpctList, 'Authorization')); // TcdaAuthorizationList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'componentOf', FcomponentOf, 'Component1')); // TcdaComponent1;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'component', Fcomponent, 'Component2')); // TcdaComponent2;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'custodian', Fcustodian, 'Custodian')); // TcdaCustodian;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'dataEnterer', FdataEnterer, 'DataEnterer')); // TcdaDataEnterer;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'documentationOf', FdocumentationOf, rmpctList, 'DocumentationOf')); // TcdaDocumentationOfList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informationRecipient', FinformationRecipient, rmpctList, 'InformationRecipient')); // TcdaInformationRecipientList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'inFulfillmentOf', FinFulfillmentOf, rmpctList, 'InFulfillmentOf')); // TcdaInFulfillmentOfList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'legalAuthenticator', FlegalAuthenticator, 'LegalAuthenticator')); // TcdaLegalAuthenticator;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant1')); // TcdaParticipant1List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'recordTarget', FrecordTarget, rmpctList, 'RecordTarget')); // TcdaRecordTargetList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'relatedDocument', FrelatedDocument, rmpctList, 'RelatedDocument')); // TcdaRelatedDocumentList;
end;

procedure TcdaClinicalDocument.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'authenticator' Then
    authenticator := TcdaAuthenticatorList(aValue.AsType(TcdaAuthenticatorList)).Clone(self)
  else if aValue.Name = 'authorization' Then
    authorization := TcdaAuthorizationList(aValue.AsType(TcdaAuthorizationList)).Clone(self)
  else if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'componentOf' Then
    componentOf := TcdaComponent1(aValue.AsType(TcdaComponent1)).Clone(self)
  else if aValue.Name = 'component' Then
    component := TcdaComponent2(aValue.AsType(TcdaComponent2)).Clone(self)
  else if aValue.Name = 'custodian' Then
    custodian := TcdaCustodian(aValue.AsType(TcdaCustodian)).Clone(self)
  else if aValue.Name = 'dataEnterer' Then
    dataEnterer := TcdaDataEnterer(aValue.AsType(TcdaDataEnterer)).Clone(self)
  else if aValue.Name = 'documentationOf' Then
    documentationOf := TcdaDocumentationOfList(aValue.AsType(TcdaDocumentationOfList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'informationRecipient' Then
    informationRecipient := TcdaInformationRecipientList(aValue.AsType(TcdaInformationRecipientList)).Clone(self)
  else if aValue.Name = 'inFulfillmentOf' Then
    inFulfillmentOf := TcdaInFulfillmentOfList(aValue.AsType(TcdaInFulfillmentOfList)).Clone(self)
  else if aValue.Name = 'legalAuthenticator' Then
    legalAuthenticator := TcdaLegalAuthenticator(aValue.AsType(TcdaLegalAuthenticator)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant1List(aValue.AsType(TcdaParticipant1List)).Clone(self)
  else if aValue.Name = 'recordTarget' Then
    recordTarget := TcdaRecordTargetList(aValue.AsType(TcdaRecordTargetList)).Clone(self)
  else if aValue.Name = 'relatedDocument' Then
    relatedDocument := TcdaRelatedDocumentList(aValue.AsType(TcdaRelatedDocumentList)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'confidentialityCode' Then
    confidentialityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'languageCode' Then
    languageCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3II(aValue.AsType(Tv3II)).Clone(self)
  else if aValue.Name = 'setId' Then
    setId := Tv3II(aValue.AsType(Tv3II)).Clone(self)
  else if aValue.Name = 'versionNumber' Then
    versionNumber := Tv3INT(aValue.AsType(Tv3INT)).Clone(self)
  else if aValue.Name = 'title' Then
    title := Tv3ST(aValue.AsType(Tv3ST)).Clone(self)
  else if aValue.Name = 'copyTime' Then
    copyTime := Tv3TS(aValue.AsType(Tv3TS)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3TS(aValue.AsType(Tv3TS)).Clone(self)
  else
    inherited;
end;


function TcdaClinicalDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthenticator.sizeInBytes);
  inc(result, Fauthorization.sizeInBytes);
  inc(result, Fauthor.sizeInBytes);
  inc(result, FcomponentOf.sizeInBytes);
  inc(result, Fcomponent.sizeInBytes);
  inc(result, Fcustodian.sizeInBytes);
  inc(result, FdataEnterer.sizeInBytes);
  inc(result, FdocumentationOf.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, FinformationRecipient.sizeInBytes);
  inc(result, FinFulfillmentOf.sizeInBytes);
  inc(result, FlegalAuthenticator.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, FrecordTarget.sizeInBytes);
  inc(result, FrelatedDocument.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, FconfidentialityCode.sizeInBytes);
  inc(result, FlanguageCode.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, FsetId.sizeInBytes);
  inc(result, FversionNumber.sizeInBytes);
  inc(result, Ftitle.sizeInBytes);
  inc(result, FcopyTime.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
end;

{ TcdaComponent1 }

Function TcdaComponent1.CDAClassNameV : String;
Begin
  Result := 'Component1';
End;

function TcdaComponent1.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaComponent1.Assign(oSource: TFslObject);
begin
  inherited;
  encompassingEncounter := TcdaComponent1(oSource).encompassingEncounter.Clone(self);
end;

procedure TcdaComponent1.DoClear;
begin
  inherited;
  encompassingEncounter := Nil;
end;

function TcdaComponent1.Clone(parent : Tv3Base): TcdaComponent1;
begin
  Result := TcdaComponent1(inherited Clone(parent));
end;

constructor TcdaComponent1.Create;
begin
  inherited;
  FtypeCode := 'COMP';
end;

destructor TcdaComponent1.Destroy;
begin
  FencompassingEncounter.Free;
  inherited;
end;

function TcdaComponent1.Link: TcdaComponent1;
begin
  Result := TcdaComponent1(Inherited Link);
end;

procedure TcdaComponent1.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'encompassingEncounter', FencompassingEncounter, 'EncompassingEncounter')); // TcdaEncompassingEncounter;
end;

procedure TcdaComponent1.SetencompassingEncounter(const Value: TcdaEncompassingEncounter);
begin
  FencompassingEncounter.Free;
  FencompassingEncounter := Value;
  if FencompassingEncounter <> nil then  encompassingEncounter.parent := self;

end;

procedure TcdaComponent1.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'encompassingEncounter' Then
    encompassingEncounter := TcdaEncompassingEncounter(aValue.AsType(TcdaEncompassingEncounter)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaComponent1.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FencompassingEncounter.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaComponent2 }

Function TcdaComponent2.CDAClassNameV : String;
Begin
  Result := 'Component2';
End;

function TcdaComponent2.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaComponent2.Assign(oSource: TFslObject);
begin
  inherited;
  nonXMLBody := TcdaComponent2(oSource).nonXMLBody.Clone(self);
  structuredBody := TcdaComponent2(oSource).structuredBody.Clone(self);
end;

procedure TcdaComponent2.DoClear;
begin
  inherited;
  nonXMLBody := Nil;
  structuredBody := Nil;
end;

function TcdaComponent2.Clone(parent : Tv3Base): TcdaComponent2;
begin
  Result := TcdaComponent2(inherited Clone(parent));
end;

constructor TcdaComponent2.Create;
begin
  inherited;
  FtypeCode := 'COMP';
  FcontextConductionInd := true;
end;

destructor TcdaComponent2.Destroy;
begin
  FnonXMLBody.Free;
  FstructuredBody.Free;
  inherited;
end;

function TcdaComponent2.Link: TcdaComponent2;
begin
  Result := TcdaComponent2(Inherited Link);
end;

procedure TcdaComponent2.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, true, 'contextConductionInd', FHasContextConductionInd, FcontextConductionInd));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'nonXMLBody', FnonXMLBody, 'NonXMLBody')); // TcdaNonXMLBody;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'structuredBody', FstructuredBody, 'StructuredBody')); // TcdaStructuredBody;
end;

procedure TcdaComponent2.SetcontextConductionInd(const Value: boolean);
begin
  FHasContextConductionInd := True;
  FcontextConductionInd := Value;
end;

procedure TcdaComponent2.SetnonXMLBody(const Value: TcdaNonXMLBody);
begin
  FnonXMLBody.Free;
  FnonXMLBody := Value;
  if FnonXMLBody <> nil then  nonXMLBody.parent := self;

end;

procedure TcdaComponent2.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'nonXMLBody' Then
    nonXMLBody := TcdaNonXMLBody(aValue.AsType(TcdaNonXMLBody)).Clone(self)
  else if aValue.Name = 'structuredBody' Then
    structuredBody := TcdaStructuredBody(aValue.AsType(TcdaStructuredBody)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'contextConductionInd' Then
    aValue.AsBool(FHasContextConductionInd, FcontextConductionInd)
  else
    inherited;
end;

procedure TcdaComponent2.SetstructuredBody(const Value: TcdaStructuredBody);
begin
  FstructuredBody.Free;
  FstructuredBody := Value;
  if FstructuredBody <> nil then  structuredBody.parent := self;

end;

function TcdaComponent2.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FnonXMLBody.sizeInBytes);
  inc(result, FstructuredBody.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaComponentSect }

Function TcdaComponentSect.CDAClassNameV : String;
Begin
  Result := 'ComponentSect';
End;

function TcdaComponentSect.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaComponentSect.Assign(oSource: TFslObject);
begin
  inherited;
  section := TcdaComponentSect(oSource).section.Clone(self);
end;

procedure TcdaComponentSect.DoClear;
begin
  inherited;
  section := NIl;
end;

function TcdaComponentSect.Clone(parent : Tv3Base): TcdaComponentSect;
begin
  Result := TcdaComponentSect(inherited Clone(parent));
end;

constructor TcdaComponentSect.Create;
begin
  inherited;
  FtypeCode := 'COMP';
  FcontextConductionInd := true;
end;

destructor TcdaComponentSect.Destroy;
begin
  Fsection.Free;
  inherited;
end;

function TcdaComponentSect.Link: TcdaComponentSect;
begin
  Result := TcdaComponentSect(Inherited Link);
end;

procedure TcdaComponentSect.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, true, 'contextConductionInd', FHasContextConductionInd, FcontextConductionInd));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'section', Fsection, 'Section')); // TcdaSection;
end;

procedure TcdaComponentSect.SetcontextConductionInd(const Value: boolean);
begin
  FHasContextConductionInd := True;
  FcontextConductionInd := Value;
end;

procedure TcdaComponentSect.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'section' Then
    section := TcdaSection(aValue.AsType(TcdaSection)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'contextConductionInd' Then
    aValue.AsBool(FHasContextConductionInd, FcontextConductionInd)
  else
    inherited;
end;

procedure TcdaComponentSect.Setsection(const Value: TcdaSection);
begin
  Fsection.Free;
  Fsection := Value;
  if Fsection <> nil then  section.parent := self;

end;

function TcdaComponentSect.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fsection.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaComponent4 }

Function TcdaComponent4.CDAClassNameV : String;
Begin
  Result := 'Component4';
End;

function TcdaComponent4.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaComponent4.Assign(oSource: TFslObject);
begin
  inherited;
  sequenceNumber := TcdaComponent4(oSource).sequenceNumber.Clone(self);
  seperatableInd := TcdaComponent4(oSource).seperatableInd.Clone(self);
  act := TcdaComponent4(oSource).act.Clone(self);
  encounter := TcdaComponent4(oSource).encounter.Clone(self);
  observation := TcdaComponent4(oSource).observation.Clone(self);
  observationMedia := TcdaComponent4(oSource).observationMedia.Clone(self);
  organizer := TcdaComponent4(oSource).organizer.Clone(self);
  procedure_ := TcdaComponent4(oSource).procedure_.Clone(self);
  regionOfInterest := TcdaComponent4(oSource).regionOfInterest.Clone(self);
  substanceAdministration := TcdaComponent4(oSource).substanceAdministration.Clone(self);
  supply := TcdaComponent4(oSource).supply.Clone(self);
end;

procedure TcdaComponent4.DoClear;
begin
  inherited;
  sequenceNumber := Nil;
  seperatableInd := Nil;
  act := Nil;
  encounter := Nil;
  observation := Nil;
  observationMedia := Nil;
  organizer := Nil;
  procedure_ := Nil;
  regionOfInterest := Nil;
  substanceAdministration := Nil;
  supply := Nil;
end;

function TcdaComponent4.Clone(parent : Tv3Base): TcdaComponent4;
begin
  Result := TcdaComponent4(inherited Clone(parent));
end;

constructor TcdaComponent4.Create;
begin
  inherited;
  FtypeCode := 'COMP';
  FcontextConductionInd := true;
end;

destructor TcdaComponent4.Destroy;
begin
  FsequenceNumber.Free;
  FseperatableInd.Free;
  Fact.Free;
  Fencounter.Free;
  Fobservation.Free;
  FobservationMedia.Free;
  Forganizer.Free;
  Fprocedure.Free;
  FregionOfInterest.Free;
  FsubstanceAdministration.Free;
  Fsupply.Free;
  inherited;
end;

function TcdaComponent4.Link: TcdaComponent4;
begin
  Result := TcdaComponent4(Inherited Link);
end;

procedure TcdaComponent4.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, true, 'contextConductionInd', FHasContextConductionInd, FcontextConductionInd));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'seperatableInd', FseperatableInd, 'BL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'sequenceNumber', FsequenceNumber, 'INT'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'act', Fact, 'Act')); // TcdaAct;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'encounter', Fencounter, 'Encounter')); // TcdaEncounter;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'observation', Fobservation, 'Observation')); // TcdaObservation;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'observationMedia', FobservationMedia, 'ObservationMedia')); // TcdaObservationMedia;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'organizer', Forganizer, 'Organizer')); // TcdaOrganizer;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'procedure', Fprocedure, 'Procedure')); // TcdaProcedure;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'regionOfInterest', FregionOfInterest, 'RegionOfInterest')); // TcdaRegionOfInterest;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'substanceAdministration', FsubstanceAdministration, 'SubstanceAdministration')); // TcdaSubstanceAdministration;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'supply', Fsupply, 'Supply')); // TcdaSupply;
end;

procedure TcdaComponent4.Setact(const Value: TcdaAct);
begin
  Fact.Free;
  Fact := Value;
  if Fact <> nil then  act.parent := self;

end;

procedure TcdaComponent4.SetcontextConductionInd(const Value: boolean);
begin
  FHasContextConductionInd := True;
  FcontextConductionInd := Value;
end;

procedure TcdaComponent4.Setencounter(const Value: TcdaEncounter);
begin
  Fencounter.Free;
  Fencounter := Value;
  if Fencounter <> nil then  encounter.parent := self;

end;

procedure TcdaComponent4.Setobservation(const Value: TcdaObservation);
begin
  Fobservation.Free;
  Fobservation := Value;
  if Fobservation <> nil then  observation.parent := self;

end;

procedure TcdaComponent4.SetobservationMedia(const Value: TcdaObservationMedia);
begin
  FobservationMedia.Free;
  FobservationMedia := Value;
  if FobservationMedia <> nil then  observationMedia.parent := self;

end;

procedure TcdaComponent4.Setorganizer(const Value: TcdaOrganizer);
begin
  Forganizer.Free;
  Forganizer := Value;
  if Forganizer <> nil then  organizer.parent := self;

end;

procedure TcdaComponent4.Setprocedure(const Value: TcdaProcedure);
begin
  Fprocedure.Free;
  Fprocedure := Value;
  if Fprocedure <> nil then
    Fprocedure.parent := self;
end;

procedure TcdaComponent4.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'act' Then
    act := TcdaAct(aValue.AsType(TcdaAct)).Clone(self)
  else if aValue.Name = 'encounter' Then
    encounter := TcdaEncounter(aValue.AsType(TcdaEncounter)).Clone(self)
  else if aValue.Name = 'observation' Then
    observation := TcdaObservation(aValue.AsType(TcdaObservation)).Clone(self)
  else if aValue.Name = 'observationMedia' Then
    observationMedia := TcdaObservationMedia(aValue.AsType(TcdaObservationMedia)).Clone(self)
  else if aValue.Name = 'organizer' Then
    organizer := TcdaOrganizer(aValue.AsType(TcdaOrganizer)).Clone(self)
  else if aValue.Name = 'procedure' Then
    procedure_ := TcdaProcedure(aValue.AsType(TcdaProcedure)).Clone(self)
  else if aValue.Name = 'regionOfInterest' Then
    regionOfInterest := TcdaRegionOfInterest(aValue.AsType(TcdaRegionOfInterest)).Clone(self)
  else if aValue.Name = 'substanceAdministration' Then
    substanceAdministration := TcdaSubstanceAdministration(aValue.AsType(TcdaSubstanceAdministration)).Clone(self)
  else if aValue.Name = 'supply' Then
    supply := TcdaSupply(aValue.AsType(TcdaSupply)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'contextConductionInd' Then
    aValue.AsBool(FHasContextConductionInd, FcontextConductionInd)
  else if aValue.Name = 'seperatableInd' Then
    seperatableInd := Tv3BL(aValue.AsType(Tv3BL)).Clone(self)
  else if aValue.Name = 'sequenceNumber' Then
    sequenceNumber := Tv3INT(aValue.AsType(Tv3INT)).Clone(self)
  else
    inherited;
end;

procedure TcdaComponent4.SetregionOfInterest(const Value: TcdaRegionOfInterest);
begin
  FregionOfInterest.Free;
  FregionOfInterest := Value;
  if FregionOfInterest <> nil then  regionOfInterest.parent := self;

end;

procedure TcdaComponent4.SetseperatableInd(const Value: Tv3BL);
begin
  FseperatableInd.Free;
  FseperatableInd := Value;
  if FseperatableInd <> nil then  seperatableInd.parent := self;

end;

procedure TcdaComponent4.SetsequenceNumber(const Value: Tv3INT);
begin
  FsequenceNumber.Free;
  FsequenceNumber := Value;
  if FsequenceNumber <> nil then  sequenceNumber.parent := self;

end;

procedure TcdaComponent4.SetsubstanceAdministration(const Value: TcdaSubstanceAdministration);
begin
  FsubstanceAdministration.Free;
  FsubstanceAdministration := Value;
  if FsubstanceAdministration <> nil then  substanceAdministration.parent := self;

end;

procedure TcdaComponent4.Setsupply(const Value: TcdaSupply);
begin
  Fsupply.Free;
  Fsupply := Value;
  if Fsupply <> nil then  supply.parent := self;

end;

function TcdaComponent4.GetClinicalStatement: TcdaClinicalStatement;
begin
  if Fact <> nil Then
    result := Fact
  else if Fencounter <> nil Then
    result := Fencounter
  else if Fobservation <> nil Then
    result := Fobservation
  else if FobservationMedia <> nil Then
    result := FobservationMedia
  else if Forganizer <> nil Then
    result := Forganizer
  else if Fprocedure <> nil Then
    result := Fprocedure
  else if FregionOfInterest <> nil Then
    result := FregionOfInterest
  else if FsubstanceAdministration <> nil Then
    result := FsubstanceAdministration
  else
    result := Fsupply;
end;


function TcdaComponent4.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fact.sizeInBytes);
  inc(result, Fencounter.sizeInBytes);
  inc(result, Fobservation.sizeInBytes);
  inc(result, FobservationMedia.sizeInBytes);
  inc(result, Forganizer.sizeInBytes);
  inc(result, Fprocedure.sizeInBytes);
  inc(result, FregionOfInterest.sizeInBytes);
  inc(result, FsubstanceAdministration.sizeInBytes);
  inc(result, Fsupply.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, FseperatableInd.sizeInBytes);
  inc(result, FsequenceNumber.sizeInBytes);
end;

{ TcdaConsent }

Function TcdaConsent.CDAClassNameV : String;
Begin
  Result := 'Consent';
End;

function TcdaConsent.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaConsent.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaConsent(oSource).id.Clone(self);
  code := TcdaConsent(oSource).code.Clone(self);
  statusCode := TcdaConsent(oSource).statusCode.Clone(self);
end;

procedure TcdaConsent.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  statusCode := Nil;
end;

function TcdaConsent.Clone(parent : Tv3Base): TcdaConsent;
begin
  Result := TcdaConsent(inherited Clone(parent));
end;

constructor TcdaConsent.Create;
begin
  inherited;
  FclassCode := 'CONS';
  FmoodCode := 'EVN';
  id := Tv3ListII.Create(self);
end;

destructor TcdaConsent.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  FstatusCode.Free;
  inherited;
end;

function TcdaConsent.Link: TcdaConsent;
begin
  Result := TcdaConsent(Inherited Link);
end;

procedure TcdaConsent.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'statusCode', FstatusCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
end;

procedure TcdaConsent.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaConsent.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaConsent.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'statusCode' Then
    statusCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

procedure TcdaConsent.SetstatusCode(const Value: Tv3CS);
begin
  FstatusCode.Free;
  FstatusCode := Value;
  if FstatusCode <> nil then  statusCode.parent := self;

end;

function TcdaConsent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, FstatusCode.sizeInBytes);
  inc(result, Fid.sizeInBytes);
end;

{ TcdaConsumable }

Function TcdaConsumable.CDAClassNameV : String;
Begin
  Result := 'Consumable';
End;

function TcdaConsumable.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaConsumable.Assign(oSource: TFslObject);
begin
  inherited;
  manufacturedProduct := TcdaConsumable(oSource).manufacturedProduct.Clone(self);
end;

procedure TcdaConsumable.DoClear;
begin
  inherited;
  manufacturedProduct := Nil;
end;

function TcdaConsumable.Clone(parent : Tv3Base): TcdaConsumable;
begin
  Result := TcdaConsumable(inherited Clone(parent));
end;

constructor TcdaConsumable.Create;
begin
  inherited;
  FtypeCode := 'CSM';
end;

destructor TcdaConsumable.Destroy;
begin
  FmanufacturedProduct.Free;
  inherited;
end;

function TcdaConsumable.Link: TcdaConsumable;
begin
  Result := TcdaConsumable(Inherited Link);
end;

procedure TcdaConsumable.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'manufacturedProduct', FmanufacturedProduct, 'ManufacturedProduct')); // TcdaManufacturedProduct;
end;

procedure TcdaConsumable.SetmanufacturedProduct(const Value: TcdaManufacturedProduct);
begin
  FmanufacturedProduct.Free;
  FmanufacturedProduct := Value;
  if FmanufacturedProduct <> nil then  manufacturedProduct.parent := self;

end;


procedure TcdaConsumable.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'manufacturedProduct' Then
    manufacturedProduct := TcdaManufacturedProduct(aValue.AsType(TcdaManufacturedProduct)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaConsumable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FmanufacturedProduct.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaCriterion }

Function TcdaCriterion.CDAClassNameV : String;
Begin
  Result := 'Criterion';
End;

function TcdaCriterion.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaCriterion.Assign(oSource: TFslObject);
begin
  inherited;
  code := TcdaCriterion(oSource).code.Clone(self);
  text := TcdaCriterion(oSource).text.Clone(self);
  value := TcdaCriterion(oSource).value.Clone(self);
  FclassCode := TcdaCriterion(oSource).FclassCode;
end;

procedure TcdaCriterion.DoClear;
begin
  inherited;
  code := Nil;
  text := Nil;
  value := Nil;
  FclassCode := 'OBS';
end;

function TcdaCriterion.Clone(parent : Tv3Base): TcdaCriterion;
begin
  Result := TcdaCriterion(inherited Clone(parent));
end;

constructor TcdaCriterion.Create;
begin
  inherited;
  FclassCode := 'OBS';
  FmoodCode := 'EVN.CRT';
end;

destructor TcdaCriterion.Destroy;
begin
  Fcode.Free;
  Ftext.Free;
  Fvalue.Free;
  inherited;
end;

procedure TcdaCriterion.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then  text.parent := self;

end;

procedure TcdaCriterion.Setvalue(const Value: Tv3ANY);
begin
  Fvalue.Free;
  Fvalue := Value;
  if Fvalue <> nil then  value.parent := self;

end;

function TcdaCriterion.Link: TcdaCriterion;
begin
  Result := TcdaCriterion(Inherited Link);
end;

procedure TcdaCriterion.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;


procedure TcdaCriterion.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'value', Fvalue, 'ANY'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
end;

procedure TcdaCriterion.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'value' Then
    value := Tv3ANY(aValue.AsType(Tv3ANY)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else
    inherited;
end;

function TcdaCriterion.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fvalue.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
end;

{ TcdaDevice }

Function TcdaDevice.CDAClassNameV : String;
Begin
  Result := 'Device';
End;

function TcdaDevice.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaDevice.Assign(oSource: TFslObject);
begin
  inherited;
  code := TcdaDevice(oSource).code.Clone(self);
  manufacturerModelName := TcdaDevice(oSource).manufacturerModelName.Clone(self);
  softwareName := TcdaDevice(oSource).softwareName.Clone(self);
  FclassCode := TcdaDevice(oSource).FclassCode;
end;

procedure TcdaDevice.DoClear;
begin
  inherited;
  code := Nil;
  manufacturerModelName := Nil;
  softwareName := Nil;
  FclassCode := 'DEV';
end;

function TcdaDevice.Clone(parent : Tv3Base): TcdaDevice;
begin
  Result := TcdaDevice(inherited Clone(parent));
end;

constructor TcdaDevice.Create;
begin
  inherited;
  FclassCode := 'DEV';
  FdeterminerCode := 'INSTANCE';
end;

destructor TcdaDevice.Destroy;
begin
  Fcode.Free;
  FmanufacturerModelName.Free;
  FsoftwareName.Free;
  inherited;
end;

function TcdaDevice.Link: TcdaDevice;
begin
  Result := TcdaDevice(Inherited Link);
end;


procedure TcdaDevice.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'manufacturerModelName', FmanufacturerModelName, 'SC'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'softwareName', FsoftwareName, 'SC'));
end;

procedure TcdaDevice.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaDevice.SetmanufacturerModelName(const Value: Tv3SC);
begin
  FmanufacturerModelName.Free;
  FmanufacturerModelName := Value;
  if FmanufacturerModelName <> nil then  manufacturerModelName.parent := self;

end;

procedure TcdaDevice.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else if aValue.Name = 'manufacturerModelName' Then
    manufacturerModelName := Tv3SC(aValue.AsType(Tv3SC)).Clone(self)
  else if aValue.Name = 'softwareName' Then
    softwareName := Tv3SC(aValue.AsType(Tv3SC)).Clone(self)
  else
    inherited;
end;

procedure TcdaDevice.SetsoftwareName(const Value: Tv3SC);
begin
  FsoftwareName.Free;
  FsoftwareName := Value;
  if FsoftwareName <> nil then  softwareName.parent := self;

end;

function TcdaDevice.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcode.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
  inc(result, FmanufacturerModelName.sizeInBytes);
  inc(result, FsoftwareName.sizeInBytes);
end;

{ TcdaDocumentationOf }

Function TcdaDocumentationOf.CDAClassNameV : String;
Begin
  Result := 'DocumentationOf';
End;

function TcdaDocumentationOf.CDAClassTypeV: TCDAClassType;
begin
  result := etActRel;
end;

procedure TcdaDocumentationOf.Assign(oSource: TFslObject);
begin
  inherited;
  serviceEvent := TcdaDocumentationOf(oSource).serviceEvent.Clone(self);
end;

procedure TcdaDocumentationOf.DoClear;
begin
  inherited;
  serviceEvent := Nil;
end;

function TcdaDocumentationOf.Clone(parent : Tv3Base): TcdaDocumentationOf;
begin
  Result := TcdaDocumentationOf(inherited Clone(parent));
end;

constructor TcdaDocumentationOf.Create;
begin
  inherited;
  FtypeCode := 'DOC';
  FserviceEvent := nil;
end;

destructor TcdaDocumentationOf.Destroy;
begin
  FserviceEvent.Free;
  inherited;
end;

function TcdaDocumentationOf.Link: TcdaDocumentationOf;
begin
  Result := TcdaDocumentationOf(Inherited Link);
end;

procedure TcdaDocumentationOf.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'serviceEvent', FserviceEvent, 'ServiceEvent')); // TcdaServiceEvent;
end;

procedure TcdaDocumentationOf.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'serviceEvent' Then
    serviceEvent := TcdaServiceEvent(aValue.AsType(TcdaServiceEvent)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaDocumentationOf.SetserviceEvent(const Value: TcdaServiceEvent);
begin
  FserviceEvent.Free;
  FserviceEvent := Value;
  if FserviceEvent <> nil then  serviceEvent.parent := self;

end;

function TcdaDocumentationOf.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FserviceEvent.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaEncompassingEncounter }

Function TcdaEncompassingEncounter.CDAClassNameV : String;
Begin
  Result := 'EncompassingEncounter';
End;

function TcdaEncompassingEncounter.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaEncompassingEncounter.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaEncompassingEncounter(oSource).id.Clone(self);
  code := TcdaEncompassingEncounter(oSource).code.Clone(self);
  effectiveTime := TcdaEncompassingEncounter(oSource).effectiveTime.Clone(self);
  dischargeDispositionCode := TcdaEncompassingEncounter(oSource).dischargeDispositionCode.Clone(self);
  responsibleParty := TcdaEncompassingEncounter(oSource).responsibleParty.Clone(self);
  encounterParticipant := TcdaEncompassingEncounter(oSource).encounterParticipant.Clone(self);
  location := TcdaEncompassingEncounter(oSource).location.Clone(self);
end;

procedure TcdaEncompassingEncounter.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  effectiveTime := Nil;
  dischargeDispositionCode := Nil;
  responsibleParty := Nil;
  encounterParticipant.Clear;
  location := Nil;
end;

function TcdaEncompassingEncounter.Clone(parent : Tv3Base): TcdaEncompassingEncounter;
begin
  Result := TcdaEncompassingEncounter(inherited Clone(parent));
end;

constructor TcdaEncompassingEncounter.Create;
begin
  inherited;
  FclassCode := 'ENC';
  FmoodCode := 'EVN';
  id := Tv3ListII.Create(self);
  encounterParticipant := TcdaEncounterParticipantList.Create(self);
end;

destructor TcdaEncompassingEncounter.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  FeffectiveTime.Free;
  FdischargeDispositionCode.Free;
  FresponsibleParty.Free;
  FencounterParticipant.Free;
  Flocation.Free;
  inherited;
end;

function TcdaEncompassingEncounter.Link: TcdaEncompassingEncounter;
begin
  Result := TcdaEncompassingEncounter(Inherited Link);
end;

procedure TcdaEncompassingEncounter.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'dischargeDispositionCode', FdischargeDispositionCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'encounterParticipant', FencounterParticipant, rmpctList, 'EncounterParticipant')); // TcdaEncounterParticipantList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'location', Flocation, 'Location')); // TcdaLocation;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'responsibleParty', FresponsibleParty, 'ResponsibleParty')); // TcdaResponsibleParty;
end;

procedure TcdaEncompassingEncounter.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaEncompassingEncounter.SetdischargeDispositionCode(const Value: Tv3CD);
begin
  FdischargeDispositionCode.Free;
  FdischargeDispositionCode := Value;
  if FdischargeDispositionCode <> nil then  dischargeDispositionCode.parent := self;

end;

procedure TcdaEncompassingEncounter.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then  effectiveTime.parent := self;

end;

procedure TcdaEncompassingEncounter.SetencounterParticipant(const Value: TcdaEncounterParticipantList);
begin
  FencounterParticipant.Free;
  FencounterParticipant := Value;
  if FencounterParticipant <> nil then  encounterParticipant.parent := self;

end;

procedure TcdaEncompassingEncounter.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaEncompassingEncounter.Setlocation(const Value: TcdaLocation);
begin
  Flocation.Free;
  Flocation := Value;
  if Flocation <> nil then  location.parent := self;

end;

procedure TcdaEncompassingEncounter.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'encounterParticipant' Then
    encounterParticipant := TcdaEncounterParticipantList(aValue.AsType(TcdaEncounterParticipantList)).Clone(self)
  else if aValue.Name = 'location' Then
    location := TcdaLocation(aValue.AsType(TcdaLocation)).Clone(self)
  else if aValue.Name = 'responsibleParty' Then
    responsibleParty := TcdaResponsibleParty(aValue.AsType(TcdaResponsibleParty)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'dischargeDispositionCode' Then
    dischargeDispositionCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

procedure TcdaEncompassingEncounter.SetresponsibleParty(const Value: TcdaResponsibleParty);
begin
  FresponsibleParty.Free;
  FresponsibleParty := Value;
  if FresponsibleParty <> nil then  responsibleParty.parent := self;

end;

function TcdaEncompassingEncounter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FencounterParticipant.sizeInBytes);
  inc(result, Flocation.sizeInBytes);
  inc(result, FresponsibleParty.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, FdischargeDispositionCode.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
  inc(result, Fid.sizeInBytes);
end;

{ TcdaEncounter }

Function TcdaEncounter.CDAClassNameV : String;
Begin
  Result := 'Encounter';
End;

function TcdaEncounter.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaEncounter.Assign(oSource: TFslObject);
begin
  inherited;
  code := TcdaEncounter(oSource).code.Clone(self);
  text := TcdaEncounter(oSource).text.Clone(self);
  statusCode := TcdaEncounter(oSource).statusCode.Clone(self);
  effectiveTime := TcdaEncounter(oSource).effectiveTime.Clone(self);
  priorityCode := TcdaEncounter(oSource).priorityCode.Clone(self);
  subject := TcdaEncounter(oSource).subject.Clone(self);
  specimen := TcdaEncounter(oSource).specimen.Clone(self);
  performer := TcdaEncounter(oSource).performer.Clone(self);
  author := TcdaEncounter(oSource).author.Clone(self);
  informant := TcdaEncounter(oSource).informant.Clone(self);
  participant := TcdaEncounter(oSource).participant.Clone(self);
  entryRelationship := TcdaEncounter(oSource).entryRelationship.Clone(self);
  reference := TcdaEncounter(oSource).reference.Clone(self);
  precondition := TcdaEncounter(oSource).precondition.Clone(self);
end;

procedure TcdaEncounter.DoClear;
begin
  inherited;
  code := Nil;
  text := Nil;
  statusCode := Nil;
  effectiveTime := Nil;
  priorityCode := Nil;
  subject := Nil;

  id.Clear;
  specimen.Clear;
  performer.Clear;
  author.Clear;
  informant.Clear;
  participant.Clear;
  entryRelationship.Clear;
  reference.Clear;
  precondition.Clear;
end;

function TcdaEncounter.Clone(parent : Tv3Base): TcdaEncounter;
begin
  Result := TcdaEncounter(inherited Clone(parent));
end;

constructor TcdaEncounter.Create;
begin
  inherited;
  author := TcdaAuthorList.Create(self);
  entryRelationship := TcdaEntryRelationshipList.Create(self);
  id := Tv3ListII.Create(self);
  informant := TcdaInformant12List.Create(self);
  participant := TcdaParticipant2List.Create(self);
  performer := TcdaPerformer2List.Create(self);
  precondition := TcdaPreconditionList.Create(self);
  reference := TcdaReferenceList.Create(self);
  specimen := TcdaSpecimenList.Create(self);
end;

destructor TcdaEncounter.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Ftext.Free;
  FstatusCode.Free;
  FeffectiveTime.Free;
  FpriorityCode.Free;
  Fsubject.Free;
  Fspecimen.Free;
  Fperformer.Free;
  Fauthor.Free;
  Finformant.Free;
  Fparticipant.Free;
  FentryRelationship.Free;
  Freference.Free;
  Fprecondition.Free;
  inherited;
end;


function TcdaEncounter.Link: TcdaEncounter;
begin
  Result := TcdaEncounter(Inherited Link);
end;

procedure TcdaEncounter.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'priorityCode', FpriorityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'statusCode', FstatusCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entryRelationship', FentryRelationship, rmpctList, 'EntryRelationship')); // TcdaEntryRelationshipList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant2')); // TcdaParticipant2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer2')); // TcdaPerformer2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'precondition', Fprecondition, rmpctList, 'Precondition')); // TcdaPreconditionList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, rmpctList, 'Reference')); // TcdaReferenceList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimen', Fspecimen, rmpctList, 'Specimen')); // TcdaSpecimenList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

procedure TcdaEncounter.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then  author.parent := self;

end;

procedure TcdaEncounter.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaEncounter.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then  effectiveTime.parent := self;

end;

procedure TcdaEncounter.SetentryRelationship(const Value: TcdaEntryRelationshipList);
begin
  FentryRelationship.Free;
  FentryRelationship := Value;
  if FentryRelationship <> nil then  entryRelationship.parent := self;

end;

procedure TcdaEncounter.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then  informant.parent := self;

end;

procedure TcdaEncounter.Setparticipant(const Value: TcdaParticipant2List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then  participant.parent := self;

end;

procedure TcdaEncounter.Setperformer(const Value: TcdaPerformer2List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then  performer.parent := self;

end;

procedure TcdaEncounter.Setprecondition(const Value: TcdaPreconditionList);
begin
  Fprecondition.Free;
  Fprecondition := Value;
  if Fprecondition <> nil then  precondition.parent := self;

end;

procedure TcdaEncounter.SetpriorityCode(const Value: Tv3CD);
begin
  FpriorityCode.Free;
  FpriorityCode := Value;
  if FpriorityCode <> nil then  priorityCode.parent := self;

end;

procedure TcdaEncounter.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'entryRelationship' Then
    entryRelationship := TcdaEntryRelationshipList(aValue.AsType(TcdaEntryRelationshipList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant2List(aValue.AsType(TcdaParticipant2List)).Clone(self)
  else if aValue.Name = 'performer' Then
    performer := TcdaPerformer2List(aValue.AsType(TcdaPerformer2List)).Clone(self)
  else if aValue.Name = 'precondition' Then
    precondition := TcdaPreconditionList(aValue.AsType(TcdaPreconditionList)).Clone(self)
  else if aValue.Name = 'reference' Then
    reference := TcdaReferenceList(aValue.AsType(TcdaReferenceList)).Clone(self)
  else if aValue.Name = 'specimen' Then
    specimen := TcdaSpecimenList(aValue.AsType(TcdaSpecimenList)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'priorityCode' Then
    priorityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'statusCode' Then
    statusCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

procedure TcdaEncounter.Setreference(const Value: TcdaReferenceList);
begin
  Freference.Free;
  Freference := Value;
  if Freference <> nil then  reference.parent := self;

end;

procedure TcdaEncounter.Setspecimen(const Value: TcdaSpecimenList);
begin
  Fspecimen.Free;
  Fspecimen := Value;
  if Fspecimen <> nil then  specimen.parent := self;
end;

procedure TcdaEncounter.SetstatusCode(const Value: Tv3CS);
begin
  FstatusCode.Free;
  FstatusCode := Value;
  if FstatusCode <> nil then  FstatusCode.parent := self;
end;

procedure TcdaEncounter.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then  subject.parent := self;

end;

procedure TcdaEncounter.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then  text.parent := self;

end;

function TcdaEncounter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, FentryRelationship.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, Fperformer.sizeInBytes);
  inc(result, Fprecondition.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, Fspecimen.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, FpriorityCode.sizeInBytes);
  inc(result, FstatusCode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
end;

{ TcdaEncounterParticipant }

Function TcdaEncounterParticipant.CDAClassNameV : String;
Begin
  Result := 'EncounterParticipant';
End;

function TcdaEncounterParticipant.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation
end;

procedure TcdaEncounterParticipant.Assign(oSource: TFslObject);
begin
  inherited;
  time := TcdaEncounterParticipant(oSource).time.Clone(self);
  assignedEntity := TcdaEncounterParticipant(oSource).assignedEntity.Clone(self);
  typeCode := TcdaEncounterParticipant(oSource).typeCode;
end;

procedure TcdaEncounterParticipant.DoClear;
begin
  inherited;
  time := Nil;
  assignedEntity := Nil;
  typeCode := '';
end;

function TcdaEncounterParticipant.Clone(parent : Tv3Base): TcdaEncounterParticipant;
begin
  Result := TcdaEncounterParticipant(inherited Clone(parent));
end;

constructor TcdaEncounterParticipant.Create;
begin
  inherited;
end;

destructor TcdaEncounterParticipant.Destroy;
begin
  Ftime.Free;
  FassignedEntity.Free;
  inherited;
end;

function TcdaEncounterParticipant.Link: TcdaEncounterParticipant;
begin
  Result := TcdaEncounterParticipant(Inherited Link);
end;

procedure TcdaEncounterParticipant.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'time', Ftime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedEntity', FassignedEntity, 'AssignedEntity')); // TcdaAssignedEntity;
end;

procedure TcdaEncounterParticipant.SetassignedEntity(const Value: TcdaAssignedEntity);
begin
  FassignedEntity.Free;
  FassignedEntity := Value;
  if FassignedEntity <> nil then  assignedEntity.parent := self;

end;

procedure TcdaEncounterParticipant.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedEntity' Then
    assignedEntity := TcdaAssignedEntity(aValue.AsType(TcdaAssignedEntity)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'time' Then
    time := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else
    inherited;
end;

procedure TcdaEncounterParticipant.Settime(const Value: Tv3IVL);
begin
  Ftime.Free;
  Ftime := Value;
  if Ftime <> nil then
    time.parent := self;
end;

function TcdaEncounterParticipant.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedEntity.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, Ftime.sizeInBytes);
end;

{ TcdaEntity }

Function TcdaEntity.CDAClassNameV : String;
Begin
  Result := 'Entity';
End;

function TcdaEntity.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaEntity.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaEntity(oSource).id.Clone(self);
  code := TcdaEntity(oSource).code.Clone(self);
  desc := TcdaEntity(oSource).desc.Clone(self);
  FclassCode := TcdaEntity(oSource).FclassCode;
end;

procedure TcdaEntity.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  desc := Nil;
  FclassCode := 'ENY';
end;

function TcdaEntity.Clone(parent : Tv3Base): TcdaEntity;
begin
  Result := TcdaEntity(inherited Clone(parent));
end;

constructor TcdaEntity.Create;
begin
  inherited;
  FclassCode := 'ENT';
  FdeterminerCode := 'INSTANCE';
  id := Tv3ListII.Create(self);
end;

destructor TcdaEntity.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Fdesc.Free;
  inherited;
end;

function TcdaEntity.Link: TcdaEntity;
begin
  Result := TcdaEntity(Inherited Link);
end;

procedure TcdaEntity.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'desc', Fdesc, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
end;

procedure TcdaEntity.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaEntity.Setdesc(const Value: Tv3ED);
begin
  Fdesc.Free;
  Fdesc := Value;
  if Fdesc <> nil then  desc.parent := self;

end;

procedure TcdaEntity.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaEntity.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'desc' Then
    desc := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

function TcdaEntity.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcode.sizeInBytes);
  inc(result, Fdesc.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
  inc(result, Fid.sizeInBytes);
end;

{ TcdaEntry }

Function TcdaEntry.CDAClassNameV : String;
Begin
  Result := 'Entry';
End;

function TcdaEntry.CDAClassTypeV: TCDAClassType;
begin
  result := etActRel;
end;

procedure TcdaEntry.Assign(oSource: TFslObject);
begin
  inherited;
  act := TcdaEntry(oSource).act.Clone(self);
  encounter := TcdaEntry(oSource).encounter.Clone(self);
  observation := TcdaEntry(oSource).observation.Clone(self);
  observationMedia := TcdaEntry(oSource).observationMedia.Clone(self);
  organizer := TcdaEntry(oSource).organizer.Clone(self);
  procedure_ := TcdaEntry(oSource).procedure_.Clone(self);
  regionOfInterest := TcdaEntry(oSource).regionOfInterest.Clone(self);
  substanceAdministration := TcdaEntry(oSource).substanceAdministration.Clone(self);
  supply := TcdaEntry(oSource).supply.Clone(self);
  typeCode := TcdaEntry(oSource).typeCode;
end;

procedure TcdaEntry.DoClear;
begin
  inherited;
  act := Nil;
  encounter := Nil;
  observation := Nil;
  observationMedia := Nil;
  organizer := Nil;
  procedure_ := Nil;
  regionOfInterest := Nil;
  substanceAdministration := Nil;
  supply := Nil;
  typeCode := 'COMP';
end;

function TcdaEntry.Clone(parent : Tv3Base): TcdaEntry;
begin
  Result := TcdaEntry(inherited Clone(parent));
end;

constructor TcdaEntry.Create;
begin
  inherited;
  FtypeCode := 'COMP';
  FcontextConductionInd := true;
end;

destructor TcdaEntry.Destroy;
begin
  Fact.Free;
  Fencounter.Free;
  Fobservation.Free;
  FobservationMedia.Free;
  Forganizer.Free;
  Fprocedure.Free;
  FregionOfInterest.Free;
  FsubstanceAdministration.Free;
  Fsupply.Free;
  inherited;
end;

function TcdaEntry.Link: TcdaEntry;
begin
  Result := TcdaEntry(Inherited Link);
end;

procedure TcdaEntry.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, true, 'contextConductionInd', FHasContextConductionInd, FcontextConductionInd));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'act', Fact, 'Act')); // TcdaAct;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'encounter', Fencounter, 'Encounter')); // TcdaEncounter;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'observation', Fobservation, 'Observation')); // TcdaObservation;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'observationMedia', FobservationMedia, 'ObservationMedia')); // TcdaObservationMedia;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'organizer', Forganizer, 'Organizer')); // TcdaOrganizer;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'procedure', Fprocedure, 'Procedure')); // TcdaProcedure;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'regionOfInterest', FregionOfInterest, 'RegionOfInterest')); // TcdaRegionOfInterest;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'substanceAdministration', FsubstanceAdministration, 'SubstanceAdministration')); // TcdaSubstanceAdministration;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'supply', Fsupply, 'Supply')); // TcdaSupply;
end;

procedure TcdaEntry.Setact(const Value: TcdaAct);
begin
  Fact.Free;
  Fact := Value;
  if Fact <> nil then  act.parent := self;

end;

procedure TcdaEntry.SetcontextConductionInd(const Value: boolean);
begin
  FHasContextConductionInd := True;
  FcontextConductionInd := Value;
end;

procedure TcdaEntry.Setencounter(const Value: TcdaEncounter);
begin
  Fencounter.Free;
  Fencounter := Value;
  if Fencounter <> nil then  encounter.parent := self;

end;

procedure TcdaEntry.Setobservation(const Value: TcdaObservation);
begin
  Fobservation.Free;
  Fobservation := Value;
  if Fobservation <> nil then  observation.parent := self;

end;

procedure TcdaEntry.SetobservationMedia(const Value: TcdaObservationMedia);
begin
  FobservationMedia.Free;
  FobservationMedia := Value;
  if FobservationMedia <> nil then  observationMedia.parent := self;

end;

procedure TcdaEntry.Setorganizer(const Value: TcdaOrganizer);
begin
  Forganizer.Free;
  Forganizer := Value;
  if Forganizer <> nil then  organizer.parent := self;

end;

procedure TcdaEntry.Setprocedure(const Value: TcdaProcedure);
begin
  Fprocedure.Free;
  Fprocedure := Value;
  if Fprocedure <> nil then
    Fprocedure.parent := self;
end;

procedure TcdaEntry.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'act' Then
    act := TcdaAct(aValue.AsType(TcdaAct)).Clone(self)
  else if aValue.Name = 'encounter' Then
    encounter := TcdaEncounter(aValue.AsType(TcdaEncounter)).Clone(self)
  else if aValue.Name = 'observation' Then
    observation := TcdaObservation(aValue.AsType(TcdaObservation)).Clone(self)
  else if aValue.Name = 'observationMedia' Then
    observationMedia := TcdaObservationMedia(aValue.AsType(TcdaObservationMedia)).Clone(self)
  else if aValue.Name = 'organizer' Then
    organizer := TcdaOrganizer(aValue.AsType(TcdaOrganizer)).Clone(self)
  else if aValue.Name = 'procedure' Then
    procedure_ := TcdaProcedure(aValue.AsType(TcdaProcedure)).Clone(self)
  else if aValue.Name = 'regionOfInterest' Then
    regionOfInterest := TcdaRegionOfInterest(aValue.AsType(TcdaRegionOfInterest)).Clone(self)
  else if aValue.Name = 'substanceAdministration' Then
    substanceAdministration := TcdaSubstanceAdministration(aValue.AsType(TcdaSubstanceAdministration)).Clone(self)
  else if aValue.Name = 'supply' Then
    supply := TcdaSupply(aValue.AsType(TcdaSupply)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'contextConductionInd' Then
    aValue.AsBool(FHasContextConductionInd, FcontextConductionInd)
  else
    inherited;
end;

procedure TcdaEntry.SetregionOfInterest(const Value: TcdaRegionOfInterest);
begin
  FregionOfInterest.Free;
  FregionOfInterest := Value;
  if FregionOfInterest <> nil then  regionOfInterest.parent := self;

end;

procedure TcdaEntry.SetsubstanceAdministration(const Value: TcdaSubstanceAdministration);
begin
  FsubstanceAdministration.Free;
  FsubstanceAdministration := Value;
  if FsubstanceAdministration <> nil then  substanceAdministration.parent := self;

end;

procedure TcdaEntry.Setsupply(const Value: TcdaSupply);
begin
  Fsupply.Free;
  Fsupply := Value;
  if Fsupply <> nil then  supply.parent := self;

end;

function TcdaEntry.GetClinicalStatement: TcdaClinicalStatement;
begin
  if Fact <> nil Then
    result := Fact
  else if Fencounter <> nil Then
    result := Fencounter
  else if Fobservation <> nil Then
    result := Fobservation
  else if FobservationMedia <> nil Then
    result := FobservationMedia
  else if Forganizer <> nil Then
    result := Forganizer
  else if Fprocedure <> nil Then
    result := Fprocedure
  else if FregionOfInterest <> nil Then
    result := FregionOfInterest
  else if FsubstanceAdministration <> nil Then
    result := FsubstanceAdministration
  else
    result := Fsupply;
end;

function TcdaEntry.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fact.sizeInBytes);
  inc(result, Fencounter.sizeInBytes);
  inc(result, Fobservation.sizeInBytes);
  inc(result, FobservationMedia.sizeInBytes);
  inc(result, Forganizer.sizeInBytes);
  inc(result, Fprocedure.sizeInBytes);
  inc(result, FregionOfInterest.sizeInBytes);
  inc(result, FsubstanceAdministration.sizeInBytes);
  inc(result, Fsupply.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaEntryRelationship }

Function TcdaEntryRelationship.CDAClassNameV : String;
Begin
  Result := 'EntryRelationship';
End;

function TcdaEntryRelationship.CDAClassTypeV: TCDAClassType;
begin
  result := etActRel;
end;

procedure TcdaEntryRelationship.Assign(oSource: TFslObject);
begin
  inherited;
  sequenceNumber := TcdaEntryRelationship(oSource).sequenceNumber.Clone(self);
  seperatableInd := TcdaEntryRelationship(oSource).seperatableInd.Clone(self);
  act := TcdaEntryRelationship(oSource).act.Clone(self);
  encounter := TcdaEntryRelationship(oSource).encounter.Clone(self);
  observation := TcdaEntryRelationship(oSource).observation.Clone(self);
  observationMedia := TcdaEntryRelationship(oSource).observationMedia.Clone(self);
  organizer := TcdaEntryRelationship(oSource).organizer.Clone(self);
  procedure_ := TcdaEntryRelationship(oSource).procedure_.Clone(self);
  regionOfInterest := TcdaEntryRelationship(oSource).regionOfInterest.Clone(self);
  substanceAdministration := TcdaEntryRelationship(oSource).substanceAdministration.Clone(self);
  supply := TcdaEntryRelationship(oSource).supply.Clone(self);
  typeCode := TcdaEntryRelationship(oSource).typeCode;
  inversionInd := TcdaEntryRelationship(oSource).inversionInd;
  contextConductionInd := TcdaEntryRelationship(oSource).contextConductionInd;
  negationInd := TcdaEntryRelationship(oSource).negationInd;
end;

procedure TcdaEntryRelationship.DoClear;
begin
  inherited;
  sequenceNumber := Nil;
  seperatableInd := Nil;
  act := Nil;
  encounter := Nil;
  observation := Nil;
  observationMedia := Nil;
  organizer := Nil;
  procedure_ := Nil;
  regionOfInterest := Nil;
  substanceAdministration := Nil;
  supply := Nil;
  typeCode := '';
  inversionInd := False;
  contextConductionInd := False;
  negationInd := False;
end;

function TcdaEntryRelationship.Clone(parent : Tv3Base): TcdaEntryRelationship;
begin
  Result := TcdaEntryRelationship(inherited Clone(parent));
end;

constructor TcdaEntryRelationship.Create;
begin
  inherited;
  FinversionInd := true;
end;

destructor TcdaEntryRelationship.Destroy;
begin
  FsequenceNumber.Free;
  FseperatableInd.Free;
  Fact.Free;
  Fencounter.Free;
  Fobservation.Free;
  FobservationMedia.Free;
  Forganizer.Free;
  Fprocedure.Free;
  FregionOfInterest.Free;
  FsubstanceAdministration.Free;
  Fsupply.Free;
  inherited;
end;

function TcdaEntryRelationship.Link: TcdaEntryRelationship;
begin
  Result := TcdaEntryRelationship(Inherited Link);
end;

procedure TcdaEntryRelationship.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, true, 'contextConductionInd', FHasContextConductionInd, FcontextConductionInd));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'negationInd', FHasNegationInd, FnegationInd));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'inversionInd', FHasInversionInd, FinversionInd));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'seperatableInd', FseperatableInd, 'BL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'sequenceNumber', FsequenceNumber, 'INT'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'act', Fact, 'Act')); // TcdaAct;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'encounter', Fencounter, 'Encounter')); // TcdaEncounter;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'observation', Fobservation, 'Observation')); // TcdaObservation;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'observationMedia', FobservationMedia, 'ObservationMedia')); // TcdaObservationMedia;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'organizer', Forganizer, 'Organizer')); // TcdaOrganizer;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'procedure', Fprocedure, 'Procedure')); // TcdaProcedure;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'regionOfInterest', FregionOfInterest, 'RegionOfInterest')); // TcdaRegionOfInterest;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'substanceAdministration', FsubstanceAdministration, 'SubstanceAdministration')); // TcdaSubstanceAdministration;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'supply', Fsupply, 'Supply')); // TcdaSupply;
end;

procedure TcdaEntryRelationship.Setact(const Value: TcdaAct);
begin
  Fact.Free;
  Fact := Value;
  if Fact <> nil then  act.parent := self;

end;

procedure TcdaEntryRelationship.SetcontextConductionInd(const Value: boolean);
begin
  FHasContextConductionInd := True;
  FcontextConductionInd := Value;
end;

procedure TcdaEntryRelationship.Setencounter(const Value: TcdaEncounter);
begin
  Fencounter.Free;
  Fencounter := Value;
  if Fencounter <> nil then  encounter.parent := self;

end;

procedure TcdaEntryRelationship.SetinversionInd(const Value: boolean);
begin
  FHasInversionInd := True;
  FinversionInd := Value;
end;

procedure TcdaEntryRelationship.SetnegationInd(const Value: boolean);
begin
  FHasNegationInd := True;
  FnegationInd := Value;
end;

procedure TcdaEntryRelationship.Setobservation(const Value: TcdaObservation);
begin
  Fobservation.Free;
  Fobservation := Value;
  if Fobservation <> nil then  observation.parent := self;

end;

procedure TcdaEntryRelationship.SetobservationMedia(const Value: TcdaObservationMedia);
begin
  FobservationMedia.Free;
  FobservationMedia := Value;
  if FobservationMedia <> nil then  observationMedia.parent := self;

end;

procedure TcdaEntryRelationship.Setorganizer(const Value: TcdaOrganizer);
begin
  Forganizer.Free;
  Forganizer := Value;
  if Forganizer <> nil then  organizer.parent := self;

end;

procedure TcdaEntryRelationship.Setprocedure(const Value: TcdaProcedure);
begin
  Fprocedure.Free;
  Fprocedure := Value;
end;

procedure TcdaEntryRelationship.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'act' Then
    act := TcdaAct(aValue.AsType(TcdaAct)).Clone(self)
  else if aValue.Name = 'encounter' Then
    encounter := TcdaEncounter(aValue.AsType(TcdaEncounter)).Clone(self)
  else if aValue.Name = 'observation' Then
    observation := TcdaObservation(aValue.AsType(TcdaObservation)).Clone(self)
  else if aValue.Name = 'observationMedia' Then
    observationMedia := TcdaObservationMedia(aValue.AsType(TcdaObservationMedia)).Clone(self)
  else if aValue.Name = 'organizer' Then
    organizer := TcdaOrganizer(aValue.AsType(TcdaOrganizer)).Clone(self)
  else if aValue.Name = 'procedure' Then
    procedure_ := TcdaProcedure(aValue.AsType(TcdaProcedure)).Clone(self)
  else if aValue.Name = 'regionOfInterest' Then
    regionOfInterest := TcdaRegionOfInterest(aValue.AsType(TcdaRegionOfInterest)).Clone(self)
  else if aValue.Name = 'substanceAdministration' Then
    substanceAdministration := TcdaSubstanceAdministration(aValue.AsType(TcdaSubstanceAdministration)).Clone(self)
  else if aValue.Name = 'supply' Then
    supply := TcdaSupply(aValue.AsType(TcdaSupply)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'contextConductionInd' Then
    aValue.AsBool(FHasContextConductionInd, FcontextConductionInd)
  else if aValue.Name = 'negationInd' Then
    aValue.AsBool(FHasNegationInd, FnegationInd)
  else if aValue.Name = 'inversionInd' Then
    aValue.AsBool(FHasInversionInd, FinversionInd)
  else if aValue.Name = 'seperatableInd' Then
    seperatableInd := Tv3BL(aValue.AsType(Tv3BL)).Clone(self)
  else if aValue.Name = 'sequenceNumber' Then
    sequenceNumber := Tv3INT(aValue.AsType(Tv3INT)).Clone(self)
  else
    inherited;
end;

procedure TcdaEntryRelationship.SetregionOfInterest(const Value: TcdaRegionOfInterest);
begin
  FregionOfInterest.Free;
  FregionOfInterest := Value;
  if FregionOfInterest <> nil then  regionOfInterest.parent := self;

end;

procedure TcdaEntryRelationship.SetseperatableInd(const Value: Tv3BL);
begin
  FseperatableInd.Free;
  FseperatableInd := Value;
  if FseperatableInd <> nil then  seperatableInd.parent := self;

end;

procedure TcdaEntryRelationship.SetsequenceNumber(const Value: Tv3INT);
begin
  FsequenceNumber.Free;
  FsequenceNumber := Value;
  if FsequenceNumber <> nil then  sequenceNumber.parent := self;

end;

procedure TcdaEntryRelationship.SetsubstanceAdministration(const Value: TcdaSubstanceAdministration);
begin
  FsubstanceAdministration.Free;
  FsubstanceAdministration := Value;
  if FsubstanceAdministration <> nil then  substanceAdministration.parent := self;

end;

procedure TcdaEntryRelationship.Setsupply(const Value: TcdaSupply);
begin
  Fsupply.Free;
  Fsupply := Value;
  if Fsupply <> nil then  supply.parent := self;

end;

function TcdaEntryRelationship.GetClinicalStatement: TcdaClinicalStatement;
begin
  if Fact <> nil Then
    result := Fact
  else if Fencounter <> nil Then
    result := Fencounter
  else if Fobservation <> nil Then
    result := Fobservation
  else if FobservationMedia <> nil Then
    result := FobservationMedia
  else if Forganizer <> nil Then
    result := Forganizer
  else if Fprocedure <> nil Then
    result := Fprocedure
  else if FregionOfInterest <> nil Then
    result := FregionOfInterest
  else if FsubstanceAdministration <> nil Then
    result := FsubstanceAdministration
  else
    result := Fsupply;
end;

function TcdaEntryRelationship.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fact.sizeInBytes);
  inc(result, Fencounter.sizeInBytes);
  inc(result, Fobservation.sizeInBytes);
  inc(result, FobservationMedia.sizeInBytes);
  inc(result, Forganizer.sizeInBytes);
  inc(result, Fprocedure.sizeInBytes);
  inc(result, FregionOfInterest.sizeInBytes);
  inc(result, FsubstanceAdministration.sizeInBytes);
  inc(result, Fsupply.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, FseperatableInd.sizeInBytes);
  inc(result, FsequenceNumber.sizeInBytes);
end;

{ TcdaExternalActChoice }

Function TcdaExternalActChoice.CDAClassNameV : String;
Begin
  Result := 'ExternalActChoice';
End;

function TcdaExternalActChoice.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaExternalActChoice.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaExternalActChoice(oSource).id.Clone(self);
  code := TcdaExternalActChoice(oSource).code.Clone(self);
  text := TcdaExternalActChoice(oSource).text.Clone(self);
  FclassCode := TcdaExternalActChoice(oSource).FclassCode;
end;

procedure TcdaExternalActChoice.DoClear;
begin
  inherited;
  id.clear;
  code := Nil;
  text := Nil;
  FclassCode := 'EVN';
end;

function TcdaExternalActChoice.Clone(parent : Tv3Base): TcdaExternalActChoice;
begin
  Result := TcdaExternalActChoice(inherited Clone(parent));
end;

constructor TcdaExternalActChoice.Create;
begin
  inherited;
  FmoodCode := 'EVN';
  id := Tv3ListII.Create(self);
end;

destructor TcdaExternalActChoice.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Ftext.Free;
  inherited;
end;


function TcdaExternalActChoice.Link: TcdaExternalActChoice;
begin
  Result := TcdaExternalActChoice(Inherited Link);
end;

procedure TcdaExternalActChoice.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
end;

procedure TcdaExternalActChoice.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaExternalActChoice.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    SetclassCode(aValue.AsString)
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

procedure TcdaExternalActChoice.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then
    text.parent := self;
end;

function TcdaExternalActChoice.ClassCodeIsFixed: Boolean;
begin
  result := false;
end;

procedure TcdaExternalActChoice.Set_id(const Value: Tv3ListII);
begin
  Fid.Free;
  FId := value;
  if Fid <> nil then
    Fid.parent := self;
end;

procedure TcdaExternalActChoice.SetClassCode(sValue: String);
begin
  if ClassCodeIsFixed Then
    raise ECDAException.create('classCode is fixed')
  Else
    FclassCode := sValue;
end;

function TcdaExternalActChoice.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
  inc(result, Fid.sizeInBytes);
end;

{ TcdaExternalAct }

Function TcdaExternalAct.CDAClassNameV : String;
Begin
  Result := 'ExternalAct';
End;

function TcdaExternalAct.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

function TcdaExternalAct.Clone(parent : Tv3Base): TcdaExternalAct;
begin
  Result := TcdaExternalAct(inherited Clone(parent));
end;

constructor TcdaExternalAct.Create;
begin
  inherited;
  FclassCode := 'ACT';
end;

function TcdaExternalAct.Link: TcdaExternalAct;
begin
  Result := TcdaExternalAct(Inherited Link);
end;

{ TcdaExternalDocument }

Function TcdaExternalDocument.CDAClassNameV : String;
Begin
  Result := 'ExternalDocument';
End;

function TcdaExternalDocument.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaExternalDocument.Assign(oSource: TFslObject);
begin
  inherited;
  setId := TcdaExternalDocument(oSource).setId.Clone(self);
  versionNumber := TcdaExternalDocument(oSource).versionNumber.Clone(self);
end;

procedure TcdaExternalDocument.DoClear;
begin
  inherited;
  setId := Nil;
  versionNumber := Nil;
end;

function TcdaExternalDocument.Clone(parent : Tv3Base): TcdaExternalDocument;
begin
  Result := TcdaExternalDocument(inherited Clone(parent));
end;

constructor TcdaExternalDocument.Create;
begin
  inherited;
  FclassCode := 'DOC';
end;

destructor TcdaExternalDocument.Destroy;
begin
  FsetId.Free;
  FversionNumber.Free;
  inherited;
end;

function TcdaExternalDocument.Link: TcdaExternalDocument;
begin
  Result := TcdaExternalDocument(Inherited Link);
end;

procedure TcdaExternalDocument.SetsetId(const Value: Tv3II);
begin
  FsetId.Free;
  FsetId := Value;
  if FsetId <> nil then  setId.parent := self;

end;

procedure TcdaExternalDocument.SetversionNumber(const Value: Tv3INT);
begin
  FversionNumber.Free;
  FversionNumber := Value;
  if FversionNumber <> nil then  versionNumber.parent := self;

end;


procedure TcdaExternalDocument.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'setId', FsetId, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'versionNumber', FversionNumber, 'INT'));
end;

procedure TcdaExternalDocument.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'setId' Then
    setId := Tv3II(aValue.AsType(Tv3II)).Clone(self)
  else if aValue.Name = 'versionNumber' Then
    versionNumber := Tv3INT(aValue.AsType(Tv3INT)).Clone(self)
  else
    inherited;
end;

function TcdaExternalDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FsetId.sizeInBytes);
  inc(result, FversionNumber.sizeInBytes);
end;

{ TcdaExternalObservation }

Function TcdaExternalObservation.CDAClassNameV : String;
Begin
  Result := 'ExternalObservation';
End;

function TcdaExternalObservation.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

function TcdaExternalObservation.Clone(parent : Tv3Base): TcdaExternalObservation;
begin
  Result := TcdaExternalObservation(inherited Clone(parent));
end;

constructor TcdaExternalObservation.Create;
begin
  inherited;
  FclassCode := 'OBS';
end;

function TcdaExternalObservation.Link: TcdaExternalObservation;
begin
  Result := TcdaExternalObservation(Inherited Link);
end;

{ TcdaExternalProcedure }

Function TcdaExternalProcedure.CDAClassNameV : String;
Begin
  Result := 'ExternalProcedure';
End;

function TcdaExternalProcedure.Clone(parent : Tv3Base): TcdaExternalProcedure;
begin
  Result := TcdaExternalProcedure(inherited Clone(parent));
end;

constructor TcdaExternalProcedure.Create;
begin
  inherited;
  FclassCode := 'PROC';
end;

function TcdaExternalProcedure.Link: TcdaExternalProcedure;
begin
  Result := TcdaExternalProcedure(Inherited Link);
end;


function TcdaExternalProcedure.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

function TcdaExternalProcedure.ClassCodeIsFixed: Boolean;
begin
  result := true;
end;

{ TcdaGuardian }

Function TcdaGuardian.CDAClassNameV : String;
Begin
  Result := 'Guardian';
End;

function TcdaGuardian.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaGuardian.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaGuardian(oSource).id.Clone(self);
  code := TcdaGuardian(oSource).code.Clone(self);
  addr := TcdaGuardian(oSource).addr.Clone(self);
  telecom := TcdaGuardian(oSource).telecom.Clone(self);
  guardianPerson := TcdaGuardian(oSource).guardianPerson.Clone(self);
  guardianOrganization := TcdaGuardian(oSource).guardianOrganization.Clone(self);
end;

procedure TcdaGuardian.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  addr.Clear;
  telecom.Clear;
  guardianPerson := Nil;
  guardianOrganization := Nil;
end;

function TcdaGuardian.Clone(parent : Tv3Base): TcdaGuardian;
begin
  Result := TcdaGuardian(inherited Clone(parent));
end;

constructor TcdaGuardian.Create;
begin
  inherited;
  FclassCode := 'GUARD';
  addr := Tv3ListAD.Create(self);
  id := Tv3ListII.Create(self);
  telecom := Tv3ListTEL.Create(self);
end;

destructor TcdaGuardian.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Faddr.Free;
  Ftelecom.Free;
  FguardianPerson.Free;
  FguardianOrganization.Free;
  inherited;
end;

function TcdaGuardian.Link: TcdaGuardian;
begin
  Result := TcdaGuardian(Inherited Link);
end;

procedure TcdaGuardian.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, rmpctList, 'AD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, rmpctList, 'TEL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'guardianOrganization', FguardianOrganization, 'Organization')); // TcdaOrganization;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'guardianPerson', FguardianPerson, 'Person')); // TcdaPerson;
end;

procedure TcdaGuardian.Setaddr(const Value: Tv3ListAD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then  addr.parent := self;

end;

procedure TcdaGuardian.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaGuardian.SetguardianOrganization(const Value: TcdaOrganization);
begin
  FguardianOrganization.Free;
  FguardianOrganization := Value;
  if FguardianOrganization <> nil then  guardianOrganization.parent := self;

end;

procedure TcdaGuardian.SetguardianPerson(const Value: TcdaPerson);
begin
  FguardianPerson.Free;
  FguardianPerson := Value;
  if FguardianPerson <> nil then  guardianPerson.parent := self;

end;

procedure TcdaGuardian.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaGuardian.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'guardianOrganization' Then
    guardianOrganization := TcdaOrganization(aValue.AsType(TcdaOrganization)).Clone(self)
  else if aValue.Name = 'guardianPerson' Then
    guardianPerson := TcdaPerson(aValue.AsType(TcdaPerson)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'addr' Then
    addr := Tv3ListAD(aValue.AsType(Tv3ListAD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3ListTEL(aValue.AsType(Tv3ListTEL)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaGuardian.Settelecom(const Value: Tv3ListTEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then  telecom.parent := self;

end;

function TcdaGuardian.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FguardianOrganization.sizeInBytes);
  inc(result, FguardianPerson.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, Faddr.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaHealthCareFacility }

Function TcdaHealthCareFacility.CDAClassNameV : String;
Begin
  Result := 'HealthCareFacility';
End;

function TcdaHealthCareFacility.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaHealthCareFacility.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaHealthCareFacility(oSource).id.Clone(self);
  code := TcdaHealthCareFacility(oSource).code.Clone(self);
  location := TcdaHealthCareFacility(oSource).location.Clone(self);
  serviceProviderOrganization := TcdaHealthCareFacility(oSource).serviceProviderOrganization.Clone(self);
  FclassCode := TcdaHealthCareFacility(oSource).FclassCode;
end;

procedure TcdaHealthCareFacility.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  location := Nil;
  serviceProviderOrganization := Nil;
  FclassCode := 'SDLOC';
end;

function TcdaHealthCareFacility.Clone(parent : Tv3Base): TcdaHealthCareFacility;
begin
  Result := TcdaHealthCareFacility(inherited Clone(parent));
end;

constructor TcdaHealthCareFacility.Create;
begin
  inherited;
  FclassCode := 'SDLOC';
    id := Tv3ListII.Create(self);
end;

destructor TcdaHealthCareFacility.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Flocation.Free;
  FserviceProviderOrganization.Free;
  inherited;
end;

function TcdaHealthCareFacility.Link: TcdaHealthCareFacility;
begin
  Result := TcdaHealthCareFacility(Inherited Link);
end;

procedure TcdaHealthCareFacility.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'serviceProviderOrganization', FserviceProviderOrganization, 'Organization')); // TcdaOrganization;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'location', Flocation, 'Place')); // TcdaPlace;
end;

procedure TcdaHealthCareFacility.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaHealthCareFacility.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaHealthCareFacility.Setlocation(const Value: TcdaPlace);
begin
  Flocation.Free;
  Flocation := Value;
  if Flocation <> nil then  location.parent := self;

end;

procedure TcdaHealthCareFacility.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'serviceProviderOrganization' Then
    serviceProviderOrganization := TcdaOrganization(aValue.AsType(TcdaOrganization)).Clone(self)
  else if aValue.Name = 'location' Then
    location := TcdaPlace(aValue.AsType(TcdaPlace)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaHealthCareFacility.SetserviceProviderOrganization(const Value: TcdaOrganization);
begin
  FserviceProviderOrganization.Free;
  FserviceProviderOrganization := Value;
  if FserviceProviderOrganization <> nil then  serviceProviderOrganization.parent := self;

end;

function TcdaHealthCareFacility.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FserviceProviderOrganization.sizeInBytes);
  inc(result, Flocation.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaInformant12 }

Function TcdaInformant12.CDAClassNameV : String;
Begin
  Result := 'Informant12';
End;

function TcdaInformant12.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaInformant12.Assign(oSource: TFslObject);
begin
  inherited;
  assignedEntity := TcdaInformant12(oSource).assignedEntity.Clone(self);
  relatedEntity := TcdaInformant12(oSource).relatedEntity.Clone(self);
end;

procedure TcdaInformant12.DoClear;
begin
  inherited;
  assignedEntity := Nil;
  relatedEntity := Nil;
end;

function TcdaInformant12.Clone(parent : Tv3Base): TcdaInformant12;
begin
  Result := TcdaInformant12(inherited Clone(parent));
end;

constructor TcdaInformant12.Create;
begin
  inherited;
  FtypeCode := 'INF';
  FcontextControlCode := 'OP';
end;

destructor TcdaInformant12.Destroy;
begin
  FassignedEntity.Free;
  FrelatedEntity.Free;
  inherited;
end;

function TcdaInformant12.Link: TcdaInformant12;
begin
  Result := TcdaInformant12(Inherited Link);
end;

procedure TcdaInformant12.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'contextControlCode', FcontextControlCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedEntity', FassignedEntity, 'AssignedEntity')); // TcdaAssignedEntity;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'relatedEntity', FrelatedEntity, 'RelatedEntity')); // TcdaRelatedEntity;
end;

procedure TcdaInformant12.SetassignedEntity(const Value: TcdaAssignedEntity);
begin
  FassignedEntity.Free;
  FassignedEntity := Value;
  if FassignedEntity <> nil then  assignedEntity.parent := self;

end;

procedure TcdaInformant12.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedEntity' Then
    assignedEntity := TcdaAssignedEntity(aValue.AsType(TcdaAssignedEntity)).Clone(self)
  else if aValue.Name = 'relatedEntity' Then
    relatedEntity := TcdaRelatedEntity(aValue.AsType(TcdaRelatedEntity)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'contextControlCode' Then
    FcontextControlCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaInformant12.SetrelatedEntity(const Value: TcdaRelatedEntity);
begin
  FrelatedEntity.Free;
  FrelatedEntity := Value;
  if FrelatedEntity <> nil then  relatedEntity.parent := self;

end;

function TcdaInformant12.GetInformantChoice: TcdaInformantChoice;
begin
  if FassignedEntity <> nil Then
    result := FassignedEntity
  Else
    result := FrelatedEntity;
end;

function TcdaInformant12.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedEntity.sizeInBytes);
  inc(result, FrelatedEntity.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, (FcontextControlCode.length * sizeof(char)) + 12);
end;

{ TcdaInformationRecipient }

Function TcdaInformationRecipient.CDAClassNameV : String;
Begin
  Result := 'InformationRecipient';
End;

function TcdaInformationRecipient.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaInformationRecipient.Assign(oSource: TFslObject);
begin
  inherited;
  intendedRecipient := TcdaInformationRecipient(oSource).intendedRecipient.Clone(self);
  typeCode := TcdaInformationRecipient(oSource).typeCode;
end;

procedure TcdaInformationRecipient.DoClear;
begin
  inherited;
  intendedRecipient := Nil;
  typeCode := 'PRCP';
end;

function TcdaInformationRecipient.Clone(parent : Tv3Base): TcdaInformationRecipient;
begin
  Result := TcdaInformationRecipient(inherited Clone(parent));
end;

constructor TcdaInformationRecipient.Create;
begin
  inherited;
  FtypeCode := 'PRCP';
end;

destructor TcdaInformationRecipient.Destroy;
begin
  FintendedRecipient.Free;
  inherited;
end;

function TcdaInformationRecipient.Link: TcdaInformationRecipient;
begin
  Result := TcdaInformationRecipient(Inherited Link);
end;

procedure TcdaInformationRecipient.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'intendedRecipient', FintendedRecipient, 'IntendedRecipient')); // TcdaIntendedRecipient;
end;

procedure TcdaInformationRecipient.SetintendedRecipient(const Value: TcdaIntendedRecipient);
begin
  FintendedRecipient.Free;
  FintendedRecipient := Value;
  if FintendedRecipient <> nil then  intendedRecipient.parent := self;

end;

procedure TcdaInformationRecipient.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'intendedRecipient' Then
    intendedRecipient := TcdaIntendedRecipient(aValue.AsType(TcdaIntendedRecipient)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaInformationRecipient.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FintendedRecipient.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaInFulfillmentOf }

Function TcdaInFulfillmentOf.CDAClassNameV : String;
Begin
  Result := 'InFulfillmentOf';
End;

function TcdaInFulfillmentOf.CDAClassTypeV: TCDAClassType;
begin
  result := etActRel;
end;

procedure TcdaInFulfillmentOf.Assign(oSource: TFslObject);
begin
  inherited;
  order := TcdaInFulfillmentOf(oSource).order.Clone(self);
end;

procedure TcdaInFulfillmentOf.DoClear;
begin
  inherited;
  order := Nil;
end;

function TcdaInFulfillmentOf.Clone(parent : Tv3Base): TcdaInFulfillmentOf;
begin
  Result := TcdaInFulfillmentOf(inherited Clone(parent));
end;

constructor TcdaInFulfillmentOf.Create;
begin
  inherited;
  FtypeCode := 'FLFS';
end;

destructor TcdaInFulfillmentOf.Destroy;
begin
  Forder.Free;
  inherited;
end;

function TcdaInFulfillmentOf.Link: TcdaInFulfillmentOf;
begin
  Result := TcdaInFulfillmentOf(Inherited Link);
end;

procedure TcdaInFulfillmentOf.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'order', Forder, 'Order')); // TcdaOrder;
end;

procedure TcdaInFulfillmentOf.Setorder(const Value: TcdaOrder);
begin
  Forder.Free;
  Forder := Value;
  if Forder <> nil then  order.parent := self;

end;

procedure TcdaInFulfillmentOf.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'order' Then
    order := TcdaOrder(aValue.AsType(TcdaOrder)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaInFulfillmentOf.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Forder.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaIntendedRecipient }

Function TcdaIntendedRecipient.CDAClassNameV : String;
Begin
  Result := 'IntendedRecipient';
End;

function TcdaIntendedRecipient.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaIntendedRecipient.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaIntendedRecipient(oSource).id.Clone(self);
  addr := TcdaIntendedRecipient(oSource).addr.Clone(self);
  telecom := TcdaIntendedRecipient(oSource).telecom.Clone(self);
  informationRecipient := TcdaIntendedRecipient(oSource).informationRecipient.Clone(self);
  receivedOrganization := TcdaIntendedRecipient(oSource).receivedOrganization.Clone(self);
  FclassCode := TcdaIntendedRecipient(oSource).FclassCode;
end;

procedure TcdaIntendedRecipient.DoClear;
begin
  inherited;
  id.Clear;
  addr.Clear;
  telecom.Clear;
  informationRecipient := Nil;
  receivedOrganization := Nil;
  FclassCode := 'ASSIGNED';
end;

function TcdaIntendedRecipient.Clone(parent : Tv3Base): TcdaIntendedRecipient;
begin
  Result := TcdaIntendedRecipient(inherited Clone(parent));
end;

constructor TcdaIntendedRecipient.Create;
begin
  inherited;
  FclassCode := 'ASSIGNED';
  addr := Tv3ListAD.Create(self);
  id := Tv3ListII.Create(self);
  telecom := Tv3ListTEL.Create(self);
end;

destructor TcdaIntendedRecipient.Destroy;
begin
  Fid.Free;
  Faddr.Free;
  Ftelecom.Free;
  FinformationRecipient.Free;
  FreceivedOrganization.Free;
  inherited;
end;


function TcdaIntendedRecipient.Link: TcdaIntendedRecipient;
begin
  Result := TcdaIntendedRecipient(Inherited Link);
end;

procedure TcdaIntendedRecipient.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, rmpctList, 'AD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, rmpctList, 'TEL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'receivedOrganization', FreceivedOrganization, 'Organization')); // TcdaOrganization;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informationRecipient', FinformationRecipient, 'Person')); // TcdaPerson;
end;

procedure TcdaIntendedRecipient.Setaddr(const Value: Tv3ListAD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then  addr.parent := self;

end;

procedure TcdaIntendedRecipient.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaIntendedRecipient.SetinformationRecipient(const Value: TcdaPerson);
begin
  FinformationRecipient.Free;
  FinformationRecipient := Value;
  if FinformationRecipient <> nil then  informationRecipient.parent := self;

end;

procedure TcdaIntendedRecipient.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'receivedOrganization' Then
    receivedOrganization := TcdaOrganization(aValue.AsType(TcdaOrganization)).Clone(self)
  else if aValue.Name = 'informationRecipient' Then
    informationRecipient := TcdaPerson(aValue.AsType(TcdaPerson)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'addr' Then
    addr := Tv3ListAD(aValue.AsType(Tv3ListAD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3ListTEL(aValue.AsType(Tv3ListTEL)).Clone(self)
  else
    inherited;
end;

procedure TcdaIntendedRecipient.SetreceivedOrganization(const Value: TcdaOrganization);
begin
  FreceivedOrganization.Free;
  FreceivedOrganization := Value;
  if FreceivedOrganization <> nil then  receivedOrganization.parent := self;

end;

procedure TcdaIntendedRecipient.Settelecom(const Value: Tv3ListTEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then  telecom.parent := self;

end;

function TcdaIntendedRecipient.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FreceivedOrganization.sizeInBytes);
  inc(result, FinformationRecipient.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, Faddr.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
end;

{ TcdaLabeledDrug }

Function TcdaLabeledDrug.CDAClassNameV : String;
Begin
  Result := 'LabeledDrug';
End;

function TcdaLabeledDrug.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaLabeledDrug.Assign(oSource: TFslObject);
begin
  inherited;
  code := TcdaLabeledDrug(oSource).code.Clone(self);
  name := TcdaLabeledDrug(oSource).name.Clone(self);
end;

procedure TcdaLabeledDrug.DoClear;
begin
  inherited;
  code := Nil;
  name := Nil;
end;

function TcdaLabeledDrug.Clone(parent : Tv3Base): TcdaLabeledDrug;
begin
  Result := TcdaLabeledDrug(inherited Clone(parent));
end;

constructor TcdaLabeledDrug.Create;
begin
  inherited;
  FclassCode := 'MMAT';
  FdeterminerCode := 'KIND';
end;

destructor TcdaLabeledDrug.Destroy;
begin
  Fcode.Free;
  Fname.Free;
  inherited;
end;

function TcdaLabeledDrug.Link: TcdaLabeledDrug;
begin
  Result := TcdaLabeledDrug(Inherited Link);
end;

procedure TcdaLabeledDrug.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, 'EN'));
end;

procedure TcdaLabeledDrug.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaLabeledDrug.Setname(const Value: Tv3EN);
begin
  Fname.Free;
  Fname := Value;
  if Fname <> nil then  name.parent := self;

end;

procedure TcdaLabeledDrug.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'name' Then
    name := Tv3EN(aValue.AsType(Tv3EN)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else
    inherited;
end;

function TcdaLabeledDrug.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcode.sizeInBytes);
  inc(result, Fname.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
end;

{ TcdaLanguageCommunication }

Function TcdaLanguageCommunication.CDAClassNameV : String;
Begin
  Result := 'LanguageCommunication';
End;

function TcdaLanguageCommunication.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaLanguageCommunication.Assign(oSource: TFslObject);
begin
  inherited;
  languageCode := TcdaLanguageCommunication(oSource).languageCode.Clone(self);
  modeCode := TcdaLanguageCommunication(oSource).modeCode.Clone(self);
  proficiencyLevelCode := TcdaLanguageCommunication(oSource).proficiencyLevelCode.Clone(self);
  preferenceInd := TcdaLanguageCommunication(oSource).preferenceInd.Clone(self);
end;

procedure TcdaLanguageCommunication.DoClear;
begin
  inherited;
  languageCode := Nil;
  modeCode := Nil;
  proficiencyLevelCode := Nil;
  preferenceInd := Nil;
end;

function TcdaLanguageCommunication.Clone(parent : Tv3Base): TcdaLanguageCommunication;
begin
  Result := TcdaLanguageCommunication(inherited Clone(parent));
end;

constructor TcdaLanguageCommunication.Create;
begin
  inherited;
end;

destructor TcdaLanguageCommunication.Destroy;
begin
  FlanguageCode.Free;
  FmodeCode.Free;
  FproficiencyLevelCode.Free;
  FpreferenceInd.Free;
  inherited;
end;

function TcdaLanguageCommunication.Link: TcdaLanguageCommunication;
begin
  Result := TcdaLanguageCommunication(Inherited Link);
end;

procedure TcdaLanguageCommunication.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'preferenceInd', FpreferenceInd, 'BL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'modeCode', FmodeCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'proficiencyLevelCode', FproficiencyLevelCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCode', FlanguageCode, 'CS'));
end;

procedure TcdaLanguageCommunication.SetlanguageCode(const Value: Tv3CS);
begin
  FlanguageCode.Free;
  FlanguageCode := Value;
  if FlanguageCode <> nil then  languageCode.parent := self;

end;

procedure TcdaLanguageCommunication.SetmodeCode(const Value: Tv3CD);
begin
  FmodeCode.Free;
  FmodeCode := Value;
  if FmodeCode <> nil then  modeCode.parent := self;

end;

procedure TcdaLanguageCommunication.SetpreferenceInd(const Value: Tv3BL);
begin
  FpreferenceInd.Free;
  FpreferenceInd := Value;
  if FpreferenceInd <> nil then  preferenceInd.parent := self;

end;

procedure TcdaLanguageCommunication.SetproficiencyLevelCode(const Value: Tv3CD);
begin
  FproficiencyLevelCode.Free;
  FproficiencyLevelCode := Value;
  if FproficiencyLevelCode <> nil then  proficiencyLevelCode.parent := self;

end;

procedure TcdaLanguageCommunication.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'preferenceInd' Then
    preferenceInd := Tv3BL(aValue.AsType(Tv3BL)).Clone(self)
  else if aValue.Name = 'modeCode' Then
    modeCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'proficiencyLevelCode' Then
    proficiencyLevelCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'languageCode' Then
    languageCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else
    inherited;
end;

function TcdaLanguageCommunication.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FpreferenceInd.sizeInBytes);
  inc(result, FmodeCode.sizeInBytes);
  inc(result, FproficiencyLevelCode.sizeInBytes);
  inc(result, FlanguageCode.sizeInBytes);
end;

{ TcdaEntityIdentifier }

Function TcdaEntityIdentifier.CDAClassNameV : String;
Begin
  Result := 'EntityIdentifier';
End;

function TcdaEntityIdentifier.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaEntityIdentifier.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaEntityIdentifier(oSource).id.Clone(self);
  code := TcdaEntityIdentifier(oSource).code.Clone(self);
end;

procedure TcdaEntityIdentifier.DoClear;
begin
  inherited;
  id := Nil;
  code := Nil;
end;

function TcdaEntityIdentifier.Clone(parent : Tv3Base): TcdaEntityIdentifier;
begin
  Result := TcdaEntityIdentifier(inherited Clone(parent));
end;

constructor TcdaEntityIdentifier.Create;
begin
  inherited;
end;

destructor TcdaEntityIdentifier.Destroy;
begin
  FId.Free;
  FCode.Free;
  inherited;
end;

function TcdaEntityIdentifier.Link: TcdaEntityIdentifier;
begin
  Result := TcdaEntityIdentifier(Inherited Link);
end;

procedure TcdaEntityIdentifier.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', FId, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', FCode, 'CD'));
end;

procedure TcdaEntityIdentifier.SetId(const Value: Tv3II);
begin
  FId.Free;
  FId := Value;
  if FId <> nil then  Id.parent := self;

end;

procedure TcdaEntityIdentifier.SetCode(const Value: Tv3CD);
begin
  FCode.Free;
  FCode := Value;
  if FCode <> nil then  Code.parent := self;

end;

procedure TcdaEntityIdentifier.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'id' Then
    id := Tv3II(aValue.AsType(Tv3II)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else
    inherited;
end;

function TcdaEntityIdentifier.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FId.sizeInBytes);
  inc(result, FCode.sizeInBytes);
end;

{ TcdaLegalAuthenticator }

Function TcdaLegalAuthenticator.CDAClassNameV : String;
Begin
  Result := 'LegalAuthenticator';
End;

function TcdaLegalAuthenticator.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaLegalAuthenticator.Assign(oSource: TFslObject);
begin
  inherited;
  time := TcdaLegalAuthenticator(oSource).time.Clone(self);
  signatureCode := TcdaLegalAuthenticator(oSource).signatureCode.Clone(self);
  assignedEntity := TcdaLegalAuthenticator(oSource).assignedEntity.Clone(self);
end;

procedure TcdaLegalAuthenticator.DoClear;
begin
  inherited;
  time := Nil;
  signatureCode := Nil;
  assignedEntity := Nil;
end;

function TcdaLegalAuthenticator.Clone(parent : Tv3Base): TcdaLegalAuthenticator;
begin
  Result := TcdaLegalAuthenticator(inherited Clone(parent));
end;

constructor TcdaLegalAuthenticator.Create;
begin
  inherited;
  FtypeCode := 'LA';
  FcontextControlCode := 'OP';
end;

destructor TcdaLegalAuthenticator.Destroy;
begin
  Ftime.Free;
  FsignatureCode.Free;
  FassignedEntity.Free;
  inherited;
end;

function TcdaLegalAuthenticator.Link: TcdaLegalAuthenticator;
begin
  Result := TcdaLegalAuthenticator(Inherited Link);
end;

procedure TcdaLegalAuthenticator.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'contextControlCode', FcontextControlCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'signatureCode', FsignatureCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'time', Ftime, 'TS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedEntity', FassignedEntity, 'AssignedEntity')); // TcdaAssignedEntity;
end;

procedure TcdaLegalAuthenticator.SetassignedEntity(const Value: TcdaAssignedEntity);
begin
  FassignedEntity.Free;
  FassignedEntity := Value;
  if FassignedEntity <> nil then  assignedEntity.parent := self;

end;

procedure TcdaLegalAuthenticator.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedEntity' Then
    assignedEntity := TcdaAssignedEntity(aValue.AsType(TcdaAssignedEntity)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'contextControlCode' Then
    FcontextControlCode := aValue.AsString
  else if aValue.Name = 'signatureCode' Then
    signatureCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'time' Then
    time := Tv3TS(aValue.AsType(Tv3TS)).Clone(self)
  else
    inherited;
end;

procedure TcdaLegalAuthenticator.SetsignatureCode(const Value: Tv3CS);
begin
  FsignatureCode.Free;
  FsignatureCode := Value;
  if FsignatureCode <> nil then  signatureCode.parent := self;
end;

procedure TcdaLegalAuthenticator.Settime(const Value: Tv3TS);
begin
  Ftime.Free;
  Ftime := Value;
  if Ftime <> nil then  time.parent := self;

end;

function TcdaLegalAuthenticator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedEntity.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, (FcontextControlCode.length * sizeof(char)) + 12);
  inc(result, FsignatureCode.sizeInBytes);
  inc(result, Ftime.sizeInBytes);
end;

{ TcdaLocation }

Function TcdaLocation.CDAClassNameV : String;
Begin
  Result := 'Location';
End;

function TcdaLocation.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaLocation.Assign(oSource: TFslObject);
begin
  inherited;
  healthCareFacility := TcdaLocation(oSource).healthCareFacility.Clone(self);
end;

procedure TcdaLocation.DoClear;
begin
  inherited;
  healthCareFacility := Nil;
end;

function TcdaLocation.Clone(parent : Tv3Base): TcdaLocation;
begin
  Result := TcdaLocation(inherited Clone(parent));
end;

constructor TcdaLocation.Create;
begin
  inherited;
  FtypeCode := 'LOC';
end;

destructor TcdaLocation.Destroy;
begin
  FhealthCareFacility.Free;
  inherited;
end;

function TcdaLocation.Link: TcdaLocation;
begin
  Result := TcdaLocation(Inherited Link);
end;

procedure TcdaLocation.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'healthCareFacility', FhealthCareFacility, 'HealthCareFacility')); // TcdaHealthCareFacility;
end;

procedure TcdaLocation.SethealthCareFacility(const Value: TcdaHealthCareFacility);
begin
  FhealthCareFacility.Free;
  FhealthCareFacility := Value;
  if FhealthCareFacility <> nil then  healthCareFacility.parent := self;

end;

procedure TcdaLocation.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'healthCareFacility' Then
    healthCareFacility := TcdaHealthCareFacility(aValue.AsType(TcdaHealthCareFacility)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaLocation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FhealthCareFacility.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaMaintainedEntity }

Function TcdaMaintainedEntity.CDAClassNameV : String;
Begin
  Result := 'MaintainedEntity';
End;

function TcdaMaintainedEntity.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaMaintainedEntity.Assign(oSource: TFslObject);
begin
  inherited;
  effectiveTime := TcdaMaintainedEntity(oSource).effectiveTime.Clone(self);
  maintainingPerson := TcdaMaintainedEntity(oSource).maintainingPerson.Clone(self);
end;

procedure TcdaMaintainedEntity.DoClear;
begin
  inherited;
  effectiveTime := Nil;
  maintainingPerson := Nil;
end;

function TcdaMaintainedEntity.Clone(parent : Tv3Base): TcdaMaintainedEntity;
begin
  Result := TcdaMaintainedEntity(inherited Clone(parent));
end;

constructor TcdaMaintainedEntity.Create;
begin
  inherited;
  FclassCode := 'MNT';
end;

destructor TcdaMaintainedEntity.Destroy;
begin
  FeffectiveTime.Free;
  FmaintainingPerson.Free;
  inherited;
end;

function TcdaMaintainedEntity.Link: TcdaMaintainedEntity;
begin
  Result := TcdaMaintainedEntity(Inherited Link);
end;

procedure TcdaMaintainedEntity.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'maintainingPerson', FmaintainingPerson, 'Person')); // TcdaPerson;
end;

procedure TcdaMaintainedEntity.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then  effectiveTime.parent := self;

end;

procedure TcdaMaintainedEntity.SetmaintainingPerson(const Value: TcdaPerson);
begin
  FmaintainingPerson.Free;
  FmaintainingPerson := Value;
  if FmaintainingPerson <> nil then  maintainingPerson.parent := self;

end;

procedure TcdaMaintainedEntity.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'maintainingPerson' Then
    maintainingPerson := TcdaPerson(aValue.AsType(TcdaPerson)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

function TcdaMaintainedEntity.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FmaintainingPerson.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaManufacturedProduct }

Function TcdaManufacturedProduct.CDAClassNameV : String;
Begin
  Result := 'ManufacturedProduct';
End;

function TcdaManufacturedProduct.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaManufacturedProduct.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaManufacturedProduct(oSource).id.Clone(self);
  manufacturedLabeledDrug := TcdaManufacturedProduct(oSource).manufacturedLabeledDrug.Clone(self);
  manufacturedMaterial := TcdaManufacturedProduct(oSource).manufacturedMaterial.Clone(self);
  manufacturerOrganization := TcdaManufacturedProduct(oSource).manufacturerOrganization.Clone(self);
end;

procedure TcdaManufacturedProduct.DoClear;
begin
  inherited;
  id.Clear;
  manufacturedLabeledDrug := Nil;
  manufacturedMaterial := Nil;
  manufacturerOrganization := Nil;
end;

function TcdaManufacturedProduct.Clone(parent : Tv3Base): TcdaManufacturedProduct;
begin
  Result := TcdaManufacturedProduct(inherited Clone(parent));
end;

constructor TcdaManufacturedProduct.Create;
begin
  inherited;
  FclassCode := 'MANU';
  id := Tv3ListII.Create(self);
end;

destructor TcdaManufacturedProduct.Destroy;
begin
  Fid.Free;
  FmanufacturedLabeledDrug.Free;
  FmanufacturedMaterial.Free;
  FmanufacturerOrganization.Free;
  inherited;
end;

function TcdaManufacturedProduct.Link: TcdaManufacturedProduct;
begin
  Result := TcdaManufacturedProduct(Inherited Link);
end;

procedure TcdaManufacturedProduct.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'manufacturedLabeledDrug', FmanufacturedLabeledDrug, 'LabeledDrug')); // TcdaLabeledDrug;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'manufacturedMaterial', FmanufacturedMaterial, 'Material')); // TcdaMaterial;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'manufacturerOrganization', FmanufacturerOrganization, 'Organization')); // TcdaOrganization;
end;

procedure TcdaManufacturedProduct.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then  id.parent := self;

end;

procedure TcdaManufacturedProduct.SetmanufacturedLabeledDrug(const Value: TcdaLabeledDrug);
begin
  FmanufacturedLabeledDrug.Free;
  FmanufacturedLabeledDrug := Value;
  if FmanufacturedLabeledDrug <> nil then  manufacturedLabeledDrug.parent := self;

end;

procedure TcdaManufacturedProduct.SetmanufacturedMaterial(const Value: TcdaMaterial);
begin
  FmanufacturedMaterial.Free;
  FmanufacturedMaterial := Value;
  if FmanufacturedMaterial <> nil then  manufacturedMaterial.parent := self;

end;

procedure TcdaManufacturedProduct.SetmanufacturerOrganization(const Value: TcdaOrganization);
begin
  FmanufacturerOrganization.Free;
  FmanufacturerOrganization := Value;
  if FmanufacturerOrganization <> nil then  manufacturerOrganization.parent := self;

end;

procedure TcdaManufacturedProduct.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'manufacturedLabeledDrug' Then
    manufacturedLabeledDrug := TcdaLabeledDrug(aValue.AsType(TcdaLabeledDrug)).Clone(self)
  else if aValue.Name = 'manufacturedMaterial' Then
    manufacturedMaterial := TcdaMaterial(aValue.AsType(TcdaMaterial)).Clone(self)
  else if aValue.Name = 'manufacturerOrganization' Then
    manufacturerOrganization := TcdaOrganization(aValue.AsType(TcdaOrganization)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

function TcdaManufacturedProduct.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FmanufacturedLabeledDrug.sizeInBytes);
  inc(result, FmanufacturedMaterial.sizeInBytes);
  inc(result, FmanufacturerOrganization.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaMaterial }

Function TcdaMaterial.CDAClassNameV : String;
Begin
  Result := 'Material';
End;

function TcdaMaterial.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaMaterial.Assign(oSource: TFslObject);
begin
  inherited;
  code := TcdaMaterial(oSource).code.Clone(self);
  name := TcdaMaterial(oSource).name.Clone(self);
  lotNumberText := TcdaMaterial(oSource).lotNumberText.Clone(self);
end;

procedure TcdaMaterial.DoClear;
begin
  inherited;
  code := Nil;
  name := Nil;
  lotNumberText := Nil;
end;

function TcdaMaterial.Clone(parent : Tv3Base): TcdaMaterial;
begin
  Result := TcdaMaterial(inherited Clone(parent));
end;

constructor TcdaMaterial.Create;
begin
  inherited;
  FclassCode := 'MMAT';
  FdeterminerCode := 'KIND';
end;

destructor TcdaMaterial.Destroy;
begin
  Fcode.Free;
  Fname.Free;
  FlotNumberText.Free;
  inherited;
end;

function TcdaMaterial.Link: TcdaMaterial;
begin
  Result := TcdaMaterial(Inherited Link);
end;

procedure TcdaMaterial.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, 'EN'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'lotNumberText', FlotNumberText, 'ST'));
end;

procedure TcdaMaterial.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaMaterial.SetlotNumberText(const Value: Tv3ST);
begin
  FlotNumberText.Free;
  FlotNumberText := Value;
  if FlotNumberText <> nil then  lotNumberText.parent := self;

end;

procedure TcdaMaterial.Setname(const Value: Tv3EN);
begin
  Fname.Free;
  Fname := Value;
  if Fname <> nil then  name.parent := self;

end;

procedure TcdaMaterial.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'name' Then
    name := Tv3EN(aValue.AsType(Tv3EN)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else if aValue.Name = 'lotNumberText' Then
    lotNumberText := Tv3ST(aValue.AsType(Tv3ST)).Clone(self)
  else
    inherited;
end;

function TcdaMaterial.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcode.sizeInBytes);
  inc(result, Fname.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
  inc(result, FlotNumberText.sizeInBytes);
end;

{ TcdaNonXMLBody }

Function TcdaNonXMLBody.CDAClassNameV : String;
Begin
  Result := 'NonXMLBody';
End;

function TcdaNonXMLBody.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaNonXMLBody.Assign(oSource: TFslObject);
begin
  inherited;
  text := TcdaNonXMLBody(oSource).text.Clone(self);
  confidentialityCode := TcdaNonXMLBody(oSource).confidentialityCode.Clone(self);
  languageCode := TcdaNonXMLBody(oSource).languageCode.Clone(self);
end;

procedure TcdaNonXMLBody.DoClear;
begin
  inherited;
  text := Nil;
  confidentialityCode := Nil;
  languageCode := Nil;
end;

function TcdaNonXMLBody.Clone(parent : Tv3Base): TcdaNonXMLBody;
begin
  Result := TcdaNonXMLBody(inherited Clone(parent));
end;

constructor TcdaNonXMLBody.Create;
begin
  inherited;
  FclassCode := 'DOCBODY';
  FmoodCode := 'EVN';
end;

destructor TcdaNonXMLBody.Destroy;
begin
  Ftext.Free;
  FconfidentialityCode.Free;
  FlanguageCode.Free;
  inherited;
end;

function TcdaNonXMLBody.Link: TcdaNonXMLBody;
begin
  Result := TcdaNonXMLBody(Inherited Link);
end;

procedure TcdaNonXMLBody.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'confidentialityCode', FconfidentialityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCode', FlanguageCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
end;

procedure TcdaNonXMLBody.SetconfidentialityCode(const Value: Tv3CD);
begin
  FconfidentialityCode.Free;
  FconfidentialityCode := Value;
  if FconfidentialityCode <> nil then  confidentialityCode.parent := self;

end;

procedure TcdaNonXMLBody.SetlanguageCode(const Value: Tv3CS);
begin
  FlanguageCode.Free;
  FlanguageCode := Value;
  if FlanguageCode <> nil then  languageCode.parent := self;

end;

procedure TcdaNonXMLBody.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'confidentialityCode' Then
    confidentialityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'languageCode' Then
    languageCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else
    inherited;
end;

procedure TcdaNonXMLBody.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then  text.parent := self;

end;

function TcdaNonXMLBody.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, FconfidentialityCode.sizeInBytes);
  inc(result, FlanguageCode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
end;

{ TcdaObservation }

Function TcdaObservation.CDAClassNameV : String;
Begin
  Result := 'Observation';
End;

function TcdaObservation.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaObservation.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaObservation(oSource).id.Clone(self);
  code := TcdaObservation(oSource).code.Clone(self);
  derivationExpr := TcdaObservation(oSource).derivationExpr.Clone(self);
  text := TcdaObservation(oSource).text.Clone(self);
  statusCode := TcdaObservation(oSource).statusCode.Clone(self);
  effectiveTime := TcdaObservation(oSource).effectiveTime.Clone(self);
  priorityCode := TcdaObservation(oSource).priorityCode.Clone(self);
  repeatNumber := TcdaObservation(oSource).repeatNumber.Clone(self);
  languageCode := TcdaObservation(oSource).languageCode.Clone(self);
  value := TcdaObservation(oSource).value.Clone(self);
  interpretationCode := TcdaObservation(oSource).interpretationCode.Clone(self);
  methodCode := TcdaObservation(oSource).methodCode.Clone(self);
  targetSiteCode := TcdaObservation(oSource).targetSiteCode.Clone(self);
  subject := TcdaObservation(oSource).subject.Clone(self);
  specimen := TcdaObservation(oSource).specimen.Clone(self);
  performer := TcdaObservation(oSource).performer.Clone(self);
  author := TcdaObservation(oSource).author.Clone(self);
  informant := TcdaObservation(oSource).informant.Clone(self);
  participant := TcdaObservation(oSource).participant.Clone(self);
  entryRelationship := TcdaObservation(oSource).entryRelationship.Clone(self);
  reference := TcdaObservation(oSource).reference.Clone(self);
  precondition := TcdaObservation(oSource).precondition.Clone(self);
  referenceRange := TcdaObservation(oSource).referenceRange.Clone(self);
  negationInd := TcdaObservation(oSource).negationInd;
end;

procedure TcdaObservation.DoClear;
begin
  inherited;
  code := Nil;
  derivationExpr := Nil;
  text := Nil;
  statusCode := Nil;
  effectiveTime := Nil;
  priorityCode := Nil;
  repeatNumber := Nil;
  languageCode := Nil;
  methodCode := Nil;
  subject := Nil;
  negationInd := False;

  author.Clear;
  entryRelationship.Clear;
  id.Clear;
  informant.Clear;
  interpretationCode.Clear;
  methodCode.Clear;
  participant.Clear;
  performer.Clear;
  precondition.Clear;
  reference.Clear;
  referenceRange.Clear;
  specimen.Clear;
  targetSiteCode.Clear;
  value.Clear;
end;

function TcdaObservation.Clone(parent : Tv3Base): TcdaObservation;
begin
  Result := TcdaObservation(inherited Clone(parent));
end;

constructor TcdaObservation.Create;
begin
  inherited;
  author := TcdaAuthorList.Create(self);
  entryRelationship := TcdaEntryRelationshipList.Create(self);
  id := Tv3ListII.Create(self);
  informant := TcdaInformant12List.Create(self);
  interpretationCode := Tv3ListCD.Create(self);
  methodCode := Tv3ListCD.Create(self);
  participant := TcdaParticipant2List.Create(self);
  performer := TcdaPerformer2List.Create(self);
  precondition := TcdaPreconditionList.Create(self);
  reference := TcdaReferenceList.Create(self);
  referenceRange := TcdaReferenceRangeList.Create(self);
  specimen := TcdaSpecimenList.Create(self);
  targetSiteCode := Tv3ListCD.Create(self);
  value := Tv3ListANY.Create(self);
end;

destructor TcdaObservation.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  FderivationExpr.Free;
  Ftext.Free;
  FstatusCode.Free;
  FeffectiveTime.Free;
  FpriorityCode.Free;
  FrepeatNumber.Free;
  FlanguageCode.Free;
  Fvalue.Free;
  FinterpretationCode.Free;
  FmethodCode.Free;
  FtargetSiteCode.Free;
  Fsubject.Free;
  Fspecimen.Free;
  Fperformer.Free;
  Fauthor.Free;
  Finformant.Free;
  Fparticipant.Free;
  FentryRelationship.Free;
  Freference.Free;
  Fprecondition.Free;
  FreferenceRange.Free;
  inherited;
end;

function TcdaObservation.Link: TcdaObservation;
begin
  Result := TcdaObservation(Inherited Link);
end;

procedure TcdaObservation.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'negationInd', FHasNegationInd, FnegationInd));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'priorityCode', FpriorityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'statusCode', FstatusCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCode', FlanguageCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'repeatNumber', FrepeatNumber, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'value', Fvalue, rmpctList, 'ANY'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'targetSiteCode', FtargetSiteCode, rmpctList, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'methodCode', FmethodCode, rmpctList, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'interpretationCode', FinterpretationCode, rmpctList, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'derivationExpr', FderivationExpr, 'ST'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entryRelationship', FentryRelationship, rmpctList, 'EntryRelationship')); // TcdaEntryRelationshipList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant2')); // TcdaParticipant2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer2')); // TcdaPerformer2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'precondition', Fprecondition, rmpctList, 'Precondition')); // TcdaPreconditionList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, rmpctList, 'Reference')); // TcdaReferenceList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'referenceRange', FreferenceRange, rmpctList, 'ReferenceRange')); // TcdaReferenceRangeList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimen', Fspecimen, rmpctList, 'Specimen')); // TcdaSpecimenList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

procedure TcdaObservation.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then  author.parent := self;

end;

procedure TcdaObservation.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure TcdaObservation.SetderivationExpr(const Value: Tv3ST);
begin
  FderivationExpr.Free;
  FderivationExpr := Value;
  if FderivationExpr <> nil then  derivationExpr.parent := self;

end;

procedure TcdaObservation.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then  effectiveTime.parent := self;

end;

procedure TcdaObservation.SetentryRelationship(const Value: TcdaEntryRelationshipList);
begin
  FentryRelationship.Free;
  FentryRelationship := Value;
  if FentryRelationship <> nil then  entryRelationship.parent := self;

end;


procedure TcdaObservation.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then  informant.parent := self;
end;

procedure TcdaObservation.SetinterpretationCode(const Value: Tv3ListCD);
begin
  FinterpretationCode.Free;
  FinterpretationCode := Value;
  if FinterpretationCode <> nil then  interpretationCode.parent := self;

end;

procedure TcdaObservation.SetlanguageCode(const Value: Tv3CS);
begin
  FlanguageCode.Free;
  FlanguageCode := Value;
  if FlanguageCode <> nil then  languageCode.parent := self;

end;

procedure TcdaObservation.SetmethodCode(const Value: Tv3ListCD);
begin
  FmethodCode.Free;
  FmethodCode := Value;
  if FmethodCode <> nil then  methodCode.parent := self;
end;

procedure TcdaObservation.SetnegationInd(const Value: boolean);
begin
  FHasNegationInd := True;
  FnegationInd := Value;
end;

procedure TcdaObservation.Setparticipant(const Value: TcdaParticipant2List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then  participant.parent := self;

end;

procedure TcdaObservation.Setperformer(const Value: TcdaPerformer2List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then  performer.parent := self;

end;

procedure TcdaObservation.Setprecondition(const Value: TcdaPreconditionList);
begin
  Fprecondition.Free;
  Fprecondition := Value;
  if Fprecondition <> nil then  precondition.parent := self;
end;

procedure TcdaObservation.SetpriorityCode(const Value: Tv3CD);
begin
  FpriorityCode.Free;
  FpriorityCode := Value;
  if FpriorityCode <> nil then  priorityCode.parent := self;

end;

procedure TcdaObservation.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'entryRelationship' Then
    entryRelationship := TcdaEntryRelationshipList(aValue.AsType(TcdaEntryRelationshipList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant2List(aValue.AsType(TcdaParticipant2List)).Clone(self)
  else if aValue.Name = 'performer' Then
    performer := TcdaPerformer2List(aValue.AsType(TcdaPerformer2List)).Clone(self)
  else if aValue.Name = 'precondition' Then
    precondition := TcdaPreconditionList(aValue.AsType(TcdaPreconditionList)).Clone(self)
  else if aValue.Name = 'reference' Then
    reference := TcdaReferenceList(aValue.AsType(TcdaReferenceList)).Clone(self)
  else if aValue.Name = 'referenceRange' Then
    referenceRange := TcdaReferenceRangeList(aValue.AsType(TcdaReferenceRangeList)).Clone(self)
  else if aValue.Name = 'specimen' Then
    specimen := TcdaSpecimenList(aValue.AsType(TcdaSpecimenList)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'negationInd' Then
    aValue.AsBool(FHasNegationInd, FnegationInd)
  else if aValue.Name = 'priorityCode' Then
    priorityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'statusCode' Then
    statusCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'languageCode' Then
    languageCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'repeatNumber' Then
    repeatNumber := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'value' Then
    value := Tv3ListANY(aValue.AsType(Tv3ListANY)).Clone(self)
  else if aValue.Name = 'targetSiteCode' Then
    targetSiteCode := Tv3ListCD(aValue.AsType(Tv3ListCD)).Clone(self)
  else if aValue.Name = 'methodCode' Then
    methodCode := Tv3ListCD(aValue.AsType(Tv3ListCD)).Clone(self)
  else if aValue.Name = 'interpretationCode' Then
    interpretationCode := Tv3ListCD(aValue.AsType(Tv3ListCD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'derivationExpr' Then
    derivationExpr := Tv3ST(aValue.AsType(Tv3ST)).Clone(self)
  else
    inherited;
end;

procedure TcdaObservation.Setreference(const Value: TcdaReferenceList);
begin
  Freference.Free;
  Freference := Value;
  if Freference <> nil then  reference.parent := self;

end;

procedure TcdaObservation.SetreferenceRange(const Value: TcdaReferenceRangeList);
begin
  FreferenceRange.Free;
  FreferenceRange := Value;
  if FreferenceRange <> nil then  referenceRange.parent := self;

end;

procedure TcdaObservation.SetrepeatNumber(const Value: Tv3IVL);
begin
  FrepeatNumber.Free;
  FrepeatNumber := Value;
  if FrepeatNumber <> nil then  repeatNumber.parent := self;

end;

procedure TcdaObservation.Setspecimen(const Value: TcdaSpecimenList);
begin
  Fspecimen.Free;
  Fspecimen := Value;
  if Fspecimen <> nil then  specimen.parent := self;

end;

procedure TcdaObservation.SetstatusCode(const Value: Tv3CS);
begin
  FstatusCode.Free;
  FstatusCode := Value;
  if FstatusCode <> nil then  statusCode.parent := self;

end;

procedure TcdaObservation.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then  subject.parent := self;

end;

procedure TcdaObservation.SettargetSiteCode(const Value: Tv3ListCD);
begin
  FtargetSiteCode.Free;
  FtargetSiteCode := Value;
  if FtargetSiteCode <> nil then  targetSiteCode.parent := self;

end;

procedure TcdaObservation.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then  text.parent := self;
end;

procedure TcdaObservation.Setvalue(const Value: Tv3ListANY);
begin
  Fvalue.Free;
  Fvalue := Value;
  if Fvalue <> nil then
    value.parent := self;
end;

function TcdaObservation.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, FentryRelationship.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, Fperformer.sizeInBytes);
  inc(result, Fprecondition.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, FreferenceRange.sizeInBytes);
  inc(result, Fspecimen.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, FpriorityCode.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, FstatusCode.sizeInBytes);
  inc(result, FlanguageCode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
  inc(result, FrepeatNumber.sizeInBytes);
  inc(result, Fvalue.sizeInBytes);
  inc(result, FtargetSiteCode.sizeInBytes);
  inc(result, FmethodCode.sizeInBytes);
  inc(result, FinterpretationCode.sizeInBytes);
  inc(result, FderivationExpr.sizeInBytes);
end;

{ TcdaObservationMedia }

Function TcdaObservationMedia.CDAClassNameV : String;
Begin
  Result := 'ObservationMedia';
End;

function TcdaObservationMedia.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaObservationMedia.Assign(oSource: TFslObject);
begin
  inherited;
  languageCode := TcdaObservationMedia(oSource).languageCode.Clone(self);
  id := TcdaObservationMedia(oSource).id.Clone(self);
  value := TcdaObservationMedia(oSource).value.Clone(self);
  subject := TcdaObservationMedia(oSource).subject.Clone(self);
  specimen := TcdaObservationMedia(oSource).specimen.Clone(self);
  performer := TcdaObservationMedia(oSource).performer.Clone(self);
  author := TcdaObservationMedia(oSource).author.Clone(self);
  informant := TcdaObservationMedia(oSource).informant.Clone(self);
  participant := TcdaObservationMedia(oSource).participant.Clone(self);
  entryRelationship := TcdaObservationMedia(oSource).entryRelationship.Clone(self);
  reference := TcdaObservationMedia(oSource).reference.Clone(self);
  precondition := TcdaObservationMedia(oSource).precondition.Clone(self);
  ID_ := TcdaObservationMedia(oSource).ID_;
end;

procedure TcdaObservationMedia.DoClear;
begin
  inherited;
  languageCode := Nil;
  value := Nil;
  subject := Nil;
  ID_ := '';
  author.Clear;
  entryRelationship.Clear;
  id.Clear;
  informant.Clear;
  participant.Clear;
  performer.Clear;
  precondition.Clear;
  reference.Clear;
  specimen.Clear;
end;

function TcdaObservationMedia.Clone(parent : Tv3Base): TcdaObservationMedia;
begin
  Result := TcdaObservationMedia(inherited Clone(parent));
end;

constructor TcdaObservationMedia.Create;
begin
  inherited;
  author := TcdaAuthorList.Create(self);
  entryRelationship := TcdaEntryRelationshipList.Create(self);
  id := Tv3ListII.Create(self);
  informant := TcdaInformant12List.Create(self);
  participant := TcdaParticipant2List.Create(self);
  performer := TcdaPerformer2List.Create(self);
  precondition := TcdaPreconditionList.Create(self);
  reference := TcdaReferenceList.Create(self);
  specimen := TcdaSpecimenList.Create(self);
end;

destructor TcdaObservationMedia.Destroy;
begin
  Fid.Free;
  FlanguageCode.Free;
  Fvalue.Free;
  Fsubject.Free;
  Fspecimen.Free;
  Fperformer.Free;
  Fauthor.Free;
  Finformant.Free;
  Fparticipant.Free;
  FentryRelationship.Free;
  Freference.Free;
  Fprecondition.Free;
  inherited;
end;


function TcdaObservationMedia.Link: TcdaObservationMedia;
begin
  Result := TcdaObservationMedia(Inherited Link);
end;

procedure TcdaObservationMedia.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'ID', FID_));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCode', FlanguageCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'value', Fvalue, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entryRelationship', FentryRelationship, rmpctList, 'EntryRelationship')); // TcdaEntryRelationshipList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant2')); // TcdaParticipant2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer2')); // TcdaPerformer2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'precondition', Fprecondition, rmpctList, 'Precondition')); // TcdaPreconditionList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, rmpctList, 'Reference')); // TcdaReferenceList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimen', Fspecimen, rmpctList, 'Specimen')); // TcdaSpecimenList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

procedure TcdaObservationMedia.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then
    author.parent := self;
end;

procedure TcdaObservationMedia.SetentryRelationship(const Value: TcdaEntryRelationshipList);
begin
  FentryRelationship.Free;
  FentryRelationship := Value;
  if FentryRelationship <> nil then
    FentryRelationship.parent := self;
end;


procedure TcdaObservationMedia.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then
    Finformant.parent := self;
end;

procedure TcdaObservationMedia.SetlanguageCode(const Value: Tv3CS);
begin
  FlanguageCode.Free;
  FlanguageCode := Value;
  if FlanguageCode <> nil then
    FlanguageCode.parent := self;
end;

procedure TcdaObservationMedia.Setparticipant(const Value: TcdaParticipant2List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then
    Fparticipant.parent := self;
end;

procedure TcdaObservationMedia.Setperformer(const Value: TcdaPerformer2List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then
    Fperformer.parent := self;
end;

procedure TcdaObservationMedia.Setprecondition(const Value: TcdaPreconditionList);
begin
  Fprecondition.Free;
  Fprecondition := Value;
  if Fprecondition <> nil then
    Fprecondition.parent := self;
end;

procedure TcdaObservationMedia.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'entryRelationship' Then
    entryRelationship := TcdaEntryRelationshipList(aValue.AsType(TcdaEntryRelationshipList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant2List(aValue.AsType(TcdaParticipant2List)).Clone(self)
  else if aValue.Name = 'performer' Then
    performer := TcdaPerformer2List(aValue.AsType(TcdaPerformer2List)).Clone(self)
  else if aValue.Name = 'precondition' Then
    precondition := TcdaPreconditionList(aValue.AsType(TcdaPreconditionList)).Clone(self)
  else if aValue.Name = 'reference' Then
    reference := TcdaReferenceList(aValue.AsType(TcdaReferenceList)).Clone(self)
  else if aValue.Name = 'specimen' Then
    specimen := TcdaSpecimenList(aValue.AsType(TcdaSpecimenList)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'languageCode' Then
    languageCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'value' Then
    value := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'ID' Then
    FID_ := aValue.AsString
  else
    inherited;
end;

procedure TcdaObservationMedia.Setreference(const Value: TcdaReferenceList);
begin
  Freference.Free;
  Freference := Value;
  if Freference <> nil then
    Freference.parent := self;
end;

procedure TcdaObservationMedia.Setspecimen(const Value: TcdaSpecimenList);
begin
  Fspecimen.Free;
  Fspecimen := Value;
  if Fspecimen <> nil then
    Fspecimen.parent := self;
end;

procedure TcdaObservationMedia.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then
    Fsubject.parent := self;
end;

procedure TcdaObservationMedia.Setvalue(const Value: Tv3ED);
begin
  Fvalue.Free;
  Fvalue := Value;
  if Fvalue <> nil then
    Fvalue.parent := self;
end;

function TcdaObservationMedia.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, FentryRelationship.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, Fperformer.sizeInBytes);
  inc(result, Fprecondition.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, Fspecimen.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, FlanguageCode.sizeInBytes);
  inc(result, Fvalue.sizeInBytes);
  inc(result, (FID_.length * sizeof(char)) + 12);
end;

{ TcdaObservationRange }

Function TcdaObservationRange.CDAClassNameV : String;
Begin
  Result := 'ObservationRange';
End;

function TcdaObservationRange.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaObservationRange.Assign(oSource: TFslObject);
begin
  inherited;
  code := TcdaObservationRange(oSource).code.Clone(self);
  text := TcdaObservationRange(oSource).text.Clone(self);
  value := TcdaObservationRange(oSource).value.Clone(self);
  interpretationCode := TcdaObservationRange(oSource).interpretationCode.Clone(self);
  FclassCode := TcdaObservationRange(oSource).FclassCode;
end;

procedure TcdaObservationRange.DoClear;
begin
  inherited;
  code := Nil;
  text := Nil;
  value := Nil;
  interpretationCode := Nil;
  FclassCode := 'OBS';
end;

function TcdaObservationRange.Clone(parent : Tv3Base): TcdaObservationRange;
begin
  Result := TcdaObservationRange(inherited Clone(parent));
end;

constructor TcdaObservationRange.Create;
begin
  inherited;
  FclassCode := 'OBS';
  FmoodCode := 'CRT';
end;

destructor TcdaObservationRange.Destroy;
begin
  Fcode.Free;
  Ftext.Free;
  Fvalue.Free;
  FinterpretationCode.Free;
  inherited;
end;

function TcdaObservationRange.Link: TcdaObservationRange;
begin
  Result := TcdaObservationRange(Inherited Link);
end;

procedure TcdaObservationRange.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'value', Fvalue, 'ANY'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'interpretationCode', FinterpretationCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
end;

procedure TcdaObservationRange.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.parent := self;
end;

procedure TcdaObservationRange.SetinterpretationCode(const Value: Tv3CD);
begin
  FinterpretationCode.Free;
  FinterpretationCode := Value;
  if Fcode <> nil then
    FinterpretationCode.parent := self;
end;

procedure TcdaObservationRange.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'value' Then
    value := Tv3ANY(aValue.AsType(Tv3ANY)).Clone(self)
  else if aValue.Name = 'interpretationCode' Then
    interpretationCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else
    inherited;
end;

procedure TcdaObservationRange.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then
   Ftext.parent := self;
end;

procedure TcdaObservationRange.Setvalue(const Value: Tv3ANY);
begin
  Fvalue.Free;
  Fvalue := Value;
  if Fvalue <> nil then
    Fvalue.parent := self;
end;

function TcdaObservationRange.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fvalue.sizeInBytes);
  inc(result, FinterpretationCode.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
end;

{ TcdaOrder }

Function TcdaOrder.CDAClassNameV : String;
Begin
  Result := 'Order';
End;

function TcdaOrder.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaOrder.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaOrder(oSource).id.Clone(self);
  code := TcdaOrder(oSource).code.Clone(self);
  priorityCode := TcdaOrder(oSource).priorityCode.Clone(self);
  FclassCode := TcdaOrder(oSource).FclassCode;
end;

procedure TcdaOrder.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  priorityCode := Nil;
  FclassCode := 'ACT';
end;

function TcdaOrder.Clone(parent : Tv3Base): TcdaOrder;
begin
  Result := TcdaOrder(inherited Clone(parent));
end;

constructor TcdaOrder.Create;
begin
  inherited;
  FclassCode := 'ACT';
  FmoodCode := 'RQO';
  id := Tv3ListII.Create(self);
end;

destructor TcdaOrder.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  FpriorityCode.Free;
  inherited;
end;

function TcdaOrder.Link: TcdaOrder;
begin
  Result := TcdaOrder(Inherited Link);
end;

procedure TcdaOrder.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'priorityCode', FpriorityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
end;

procedure TcdaOrder.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.parent := self;
end;

procedure TcdaOrder.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.parent := self;
end;

procedure TcdaOrder.SetpriorityCode(const Value: Tv3CD);
begin
  FpriorityCode.Free;
  FpriorityCode := Value;
  if FpriorityCode <> nil then
    FpriorityCode.parent := self;
end;

procedure TcdaOrder.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'priorityCode' Then
    priorityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

function TcdaOrder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, FpriorityCode.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, Fid.sizeInBytes);
end;

{ TcdaOrganization }

Function TcdaOrganization.CDAClassNameV : String;
Begin
  Result := 'Organization';
End;

function TcdaOrganization.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaOrganization.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaOrganization(oSource).id.Clone(self);
  name := TcdaOrganization(oSource).name.Clone(self);
  telecom := TcdaOrganization(oSource).telecom.Clone(self);
  addr := TcdaOrganization(oSource).addr.Clone(self);
  standardIndustryClassCode := TcdaOrganization(oSource).standardIndustryClassCode.Clone(self);
  asOrganizationPartOf := TcdaOrganization(oSource).asOrganizationPartOf.Clone(self);
end;

procedure TcdaOrganization.DoClear;
begin
  inherited;
  id.Clear;
  name.Clear;
  telecom.Clear;
  addr.Clear;
  standardIndustryClassCode := Nil;
  asOrganizationPartOf := Nil;
end;

function TcdaOrganization.Clone(parent : Tv3Base): TcdaOrganization;
begin
  Result := TcdaOrganization(inherited Clone(parent));
end;

constructor TcdaOrganization.Create;
begin
  inherited;
  FclassCode := 'ORG';
  FdeterminerCode := 'INSTANCE';
  addr := Tv3ListAD.Create(self);
  id := Tv3ListII.Create(self);
  name := Tv3ListEN.Create(self);
  telecom := Tv3ListTEL.Create(self);
end;

destructor TcdaOrganization.Destroy;
begin
  Fid.Free;
  Fname.Free;
  Ftelecom.Free;
  Faddr.Free;
  FstandardIndustryClassCode.Free;
  FasOrganizationPartOf.Free;
  inherited;
end;


function TcdaOrganization.Link: TcdaOrganization;
begin
  Result := TcdaOrganization(Inherited Link);
end;

procedure TcdaOrganization.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, rmpctList, 'AD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, rmpctList, 'EN'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, rmpctList, 'TEL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'asOrganizationPartOf', FasOrganizationPartOf, 'OrganizationPartOf')); // TcdaOrganizationPartOf;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'standardIndustryClassCode', FstandardIndustryClassCode, 'CD'));
end;

procedure TcdaOrganization.Setaddr(const Value: Tv3ListAD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then
    Faddr.parent := self;
end;

procedure TcdaOrganization.SetasOrganizationPartOf(const Value: TcdaOrganizationPartOf);
begin
  FasOrganizationPartOf.Free;
  FasOrganizationPartOf := Value;
  if FasOrganizationPartOf <> nil then
    FasOrganizationPartOf.parent := self;
end;

procedure TcdaOrganization.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.parent := self;
end;

procedure TcdaOrganization.Setname(const Value: Tv3ListEN);
begin
  Fname.Free;
  Fname := Value;
  if Fname <> nil then
    Fname.parent := self;
end;

procedure TcdaOrganization.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'asOrganizationPartOf' Then
    asOrganizationPartOf := TcdaOrganizationPartOf(aValue.AsType(TcdaOrganizationPartOf)).Clone(self)
  else if aValue.Name = 'standardIndustryClassCode' Then
    standardIndustryClassCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else if aValue.Name = 'addr' Then
    addr := Tv3ListAD(aValue.AsType(Tv3ListAD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'name' Then
    name := Tv3ListEN(aValue.AsType(Tv3ListEN)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3ListTEL(aValue.AsType(Tv3ListTEL)).Clone(self)
  else
    inherited;
end;

procedure TcdaOrganization.SetstandardIndustryClassCode(const Value: Tv3CD);
begin
  FstandardIndustryClassCode.Free;
  FstandardIndustryClassCode := Value;
  if FstandardIndustryClassCode <> nil then
    FstandardIndustryClassCode.parent := self;
end;

procedure TcdaOrganization.Settelecom(const Value: Tv3ListTEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then
    telecom.parent := self;
end;

function TcdaOrganization.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FasOrganizationPartOf.sizeInBytes);
  inc(result, FstandardIndustryClassCode.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
  inc(result, Faddr.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, Fname.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
end;

{ TcdaOrganizationPartOf }

Function TcdaOrganizationPartOf.CDAClassNameV : String;
Begin
  Result := 'OrganizationPartOf';
End;

function TcdaOrganizationPartOf.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaOrganizationPartOf.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaOrganizationPartOf(oSource).id.Clone(self);
  code := TcdaOrganizationPartOf(oSource).code.Clone(self);
  statusCode := TcdaOrganizationPartOf(oSource).statusCode.Clone(self);
  effectiveTime := TcdaOrganizationPartOf(oSource).effectiveTime.Clone(self);
  wholeOrganization := TcdaOrganizationPartOf(oSource).wholeOrganization.Clone(self);
end;

procedure TcdaOrganizationPartOf.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  statusCode := Nil;
  effectiveTime := Nil;
  wholeOrganization := Nil;
end;

function TcdaOrganizationPartOf.Clone(parent : Tv3Base): TcdaOrganizationPartOf;
begin
  Result := TcdaOrganizationPartOf(inherited Clone(parent));
end;

constructor TcdaOrganizationPartOf.Create;
begin
  inherited;
  FclassCode := 'PART';
  id := Tv3ListII.Create(self);
end;

destructor TcdaOrganizationPartOf.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  FstatusCode.Free;
  FeffectiveTime.Free;
  FwholeOrganization.Free;
  inherited;
end;


function TcdaOrganizationPartOf.Link: TcdaOrganizationPartOf;
begin
  Result := TcdaOrganizationPartOf(Inherited Link);
end;

procedure TcdaOrganizationPartOf.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'statusCode', FstatusCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'wholeOrganization', FwholeOrganization, 'Organization')); // TcdaOrganization;
end;

procedure TcdaOrganizationPartOf.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.parent := self;
end;

procedure TcdaOrganizationPartOf.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then
    FeffectiveTime.parent := self;
end;

procedure TcdaOrganizationPartOf.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if FId <> nil then
    Fid.parent := self;
end;

procedure TcdaOrganizationPartOf.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'wholeOrganization' Then
    wholeOrganization := TcdaOrganization(aValue.AsType(TcdaOrganization)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'statusCode' Then
    statusCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaOrganizationPartOf.SetstatusCode(const Value: Tv3CS);
begin
  FstatusCode.Free;
  FstatusCode := Value;
  if FstatusCode <> nil then
    FstatusCode.parent := self;
end;

procedure TcdaOrganizationPartOf.SetwholeOrganization(const Value: TcdaOrganization);
begin
  FwholeOrganization.Free;
  FwholeOrganization := Value;
  if FwholeOrganization <> nil then
    FwholeOrganization.Parent := self;
end;

function TcdaOrganizationPartOf.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FwholeOrganization.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, FstatusCode.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaOrganizer }

Function TcdaOrganizer.CDAClassNameV : String;
Begin
  Result := 'Organizer';
End;

function TcdaOrganizer.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaOrganizer.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaOrganizer(oSource).id.Clone(self);
  code := TcdaOrganizer(oSource).code.Clone(self);
  statusCode := TcdaOrganizer(oSource).statusCode.Clone(self);
  effectiveTime := TcdaOrganizer(oSource).effectiveTime.Clone(self);
  subject := TcdaOrganizer(oSource).subject.Clone(self);
  specimen := TcdaOrganizer(oSource).specimen.Clone(self);
  performer := TcdaOrganizer(oSource).performer.Clone(self);
  author := TcdaOrganizer(oSource).author.Clone(self);
  informant := TcdaOrganizer(oSource).informant.Clone(self);
  participant := TcdaOrganizer(oSource).participant.Clone(self);
  reference := TcdaOrganizer(oSource).reference.Clone(self);
  precondition := TcdaOrganizer(oSource).precondition.Clone(self);
  component := TcdaOrganizer(oSource).component.Clone(self);
end;

procedure TcdaOrganizer.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  statusCode := Nil;
  effectiveTime := Nil;
  subject := Nil;
  specimen.Clear;
  performer.Clear;
  author.Clear;
  informant.Clear;
  participant.Clear;
  reference.Clear;
  precondition.Clear;
  component.Clear;
end;

function TcdaOrganizer.Clone(parent : Tv3Base): TcdaOrganizer;
begin
  Result := TcdaOrganizer(inherited Clone(parent));
end;

constructor TcdaOrganizer.Create;
begin
  inherited;
  author := TcdaAuthorList.Create(self);
  component := TcdaComponent4List.Create(self);
  id := Tv3ListII.Create(self);
  informant := TcdaInformant12List.Create(self);
  participant := TcdaParticipant2List.Create(self);
  performer := TcdaPerformer2List.Create(self);
  precondition := TcdaPreconditionList.Create(self);
  reference := TcdaReferenceList.Create(self);
  specimen := TcdaSpecimenList.Create(self);
end;

destructor TcdaOrganizer.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  FstatusCode.Free;
  FeffectiveTime.Free;
  Fsubject.Free;
  Fspecimen.Free;
  Fperformer.Free;
  Fauthor.Free;
  Finformant.Free;
  Fparticipant.Free;
  Freference.Free;
  Fprecondition.Free;
  Fcomponent.Free;
  inherited;
end;

function TcdaOrganizer.Link: TcdaOrganizer;
begin
  Result := TcdaOrganizer(Inherited Link);
end;

procedure TcdaOrganizer.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'statusCode', FstatusCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'component', Fcomponent, rmpctList, 'Component4')); // TcdaComponent4List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant2')); // TcdaParticipant2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer2')); // TcdaPerformer2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'precondition', Fprecondition, rmpctList, 'Precondition')); // TcdaPreconditionList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, rmpctList, 'Reference')); // TcdaReferenceList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimen', Fspecimen, rmpctList, 'Specimen')); // TcdaSpecimenList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

procedure TcdaOrganizer.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then
    Fauthor.Parent := self;
end;

procedure TcdaOrganizer.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

procedure TcdaOrganizer.Setcomponent(const Value: TcdaComponent4List);
begin
  Fcomponent.Free;
  Fcomponent := Value;
  if Fcomponent <> nil then
    Fcomponent.Parent := self;
end;

procedure TcdaOrganizer.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then
    FeffectiveTime.Parent := self;
end;


procedure TcdaOrganizer.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then
    Finformant.Parent := self;
end;

procedure TcdaOrganizer.Setparticipant(const Value: TcdaParticipant2List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then
    Fparticipant.Parent := self;
end;

procedure TcdaOrganizer.Setperformer(const Value: TcdaPerformer2List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then
    Fperformer.Parent := self;
end;

procedure TcdaOrganizer.Setprecondition(const Value: TcdaPreconditionList);
begin
  Fprecondition.Free;
  Fprecondition := Value;
  if Fprecondition <> nil then
    Fprecondition.Parent := self;
end;

procedure TcdaOrganizer.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'component' Then
    component := TcdaComponent4List(aValue.AsType(TcdaComponent4List)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant2List(aValue.AsType(TcdaParticipant2List)).Clone(self)
  else if aValue.Name = 'performer' Then
    performer := TcdaPerformer2List(aValue.AsType(TcdaPerformer2List)).Clone(self)
  else if aValue.Name = 'precondition' Then
    precondition := TcdaPreconditionList(aValue.AsType(TcdaPreconditionList)).Clone(self)
  else if aValue.Name = 'reference' Then
    reference := TcdaReferenceList(aValue.AsType(TcdaReferenceList)).Clone(self)
  else if aValue.Name = 'specimen' Then
    specimen := TcdaSpecimenList(aValue.AsType(TcdaSpecimenList)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'statusCode' Then
    statusCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

procedure TcdaOrganizer.Setreference(const Value: TcdaReferenceList);
begin
  Freference.Free;
  Freference := Value;
  if Freference <> nil then
    Freference.Parent := self;
end;


procedure TcdaOrganizer.Setspecimen(const Value: TcdaSpecimenList);
begin
  Fspecimen.Free;
  Fspecimen := Value;
  if Fspecimen <> nil then
    Fspecimen.Parent := self;
end;

procedure TcdaOrganizer.SetstatusCode(const Value: Tv3CS);
begin
  FstatusCode.Free;
  FstatusCode := Value;
  if FstatusCode <> nil then
    FstatusCode.Parent := self;
end;

procedure TcdaOrganizer.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then
    Fsubject.Parent := self;
end;

function TcdaOrganizer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, Fcomponent.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, Fperformer.sizeInBytes);
  inc(result, Fprecondition.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, Fspecimen.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, FstatusCode.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
end;

{ TcdaParentDocument }

Function TcdaParentDocument.CDAClassNameV : String;
Begin
  Result := 'ParentDocument';
End;

function TcdaParentDocument.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaParentDocument.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaParentDocument(oSource).id.Clone(self);
  code := TcdaParentDocument(oSource).code.Clone(self);
  text := TcdaParentDocument(oSource).text.Clone(self);
  setId := TcdaParentDocument(oSource).setId.Clone(self);
  versionNumber := TcdaParentDocument(oSource).versionNumber.Clone(self);
end;

procedure TcdaParentDocument.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  text := Nil;
  setId := Nil;
  versionNumber := Nil;
end;

function TcdaParentDocument.Clone(parent : Tv3Base): TcdaParentDocument;
begin
  Result := TcdaParentDocument(inherited Clone(parent));
end;

constructor TcdaParentDocument.Create;
begin
  inherited;
  FclassCode := 'DOCCLIN';
  FmoodCode := 'EVN';
  id := Tv3ListII.Create(self);
end;

destructor TcdaParentDocument.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Ftext.Free;
  FsetId.Free;
  FversionNumber.Free;
  inherited;
end;

function TcdaParentDocument.Link: TcdaParentDocument;
begin
  Result := TcdaParentDocument(Inherited Link);
end;

procedure TcdaParentDocument.Set_id(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.Parent := self;
end;

procedure TcdaParentDocument.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

procedure TcdaParentDocument.SetsetId(const Value: Tv3II);
begin
  FsetId.Free;
  FsetId := Value;
  if FsetId <> nil then
    FsetId.Parent := self;
end;

procedure TcdaParentDocument.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then
    Ftext.Parent := self;
end;

procedure TcdaParentDocument.SetversionNumber(const Value: Tv3INT);
begin
  FversionNumber.Free;
  FversionNumber := Value;
  if FversionNumber <> nil then
    FversionNumber.Parent := self;
end;


procedure TcdaParentDocument.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'setId', FsetId, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'versionNumber', FversionNumber, 'INT'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
end;

procedure TcdaParentDocument.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'setId' Then
    setId := Tv3II(aValue.AsType(Tv3II)).Clone(self)
  else if aValue.Name = 'versionNumber' Then
    versionNumber := Tv3INT(aValue.AsType(Tv3INT)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

function TcdaParentDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
  inc(result, FsetId.sizeInBytes);
  inc(result, FversionNumber.sizeInBytes);
  inc(result, Fid.sizeInBytes);
end;

{ TcdaParticipant1 }

Function TcdaParticipant1.CDAClassNameV : String;
Begin
  Result := 'Participant1';
End;

function TcdaParticipant1.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaParticipant1.Assign(oSource: TFslObject);
begin
  inherited;
  functionCode := TcdaParticipant1(oSource).functionCode.Clone(self);
  time := TcdaParticipant1(oSource).time.Clone(self);
  associatedEntity := TcdaParticipant1(oSource).associatedEntity.Clone(self);
  typeCode := TcdaParticipant1(oSource).typeCode;
end;

procedure TcdaParticipant1.DoClear;
begin
  inherited;
  functionCode := Nil;
  time := Nil;
  associatedEntity := Nil;
  typeCode := '';
end;

function TcdaParticipant1.Clone(parent : Tv3Base): TcdaParticipant1;
begin
  Result := TcdaParticipant1(inherited Clone(parent));
end;

constructor TcdaParticipant1.Create;
begin
  inherited;
  FcontextControlCode := 'OP';
end;

destructor TcdaParticipant1.Destroy;
begin
  FfunctionCode.Free;
  Ftime.Free;
  FassociatedEntity.Free;
  inherited;
end;

function TcdaParticipant1.Link: TcdaParticipant1;
begin
  Result := TcdaParticipant1(Inherited Link);
end;

procedure TcdaParticipant1.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'contextControlCode', FcontextControlCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'functionCode', FfunctionCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'time', Ftime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'associatedEntity', FassociatedEntity, 'AssociatedEntity')); // TcdaAssociatedEntity;
end;

procedure TcdaParticipant1.SetassociatedEntity(const Value: TcdaAssociatedEntity);
begin
  FassociatedEntity.Free;
  FassociatedEntity := Value;
  if FassociatedEntity <> nil then
    FassociatedEntity.Parent := self;
end;

procedure TcdaParticipant1.SetfunctionCode(const Value: Tv3CD);
begin
  FfunctionCode.Free;
  FfunctionCode := Value;
  if FfunctionCode <> nil then
    FfunctionCode.Parent := self;
end;

procedure TcdaParticipant1.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'associatedEntity' Then
    associatedEntity := TcdaAssociatedEntity(aValue.AsType(TcdaAssociatedEntity)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'functionCode' Then
    functionCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'contextControlCode' Then
    FcontextControlCode := aValue.AsString
  else if aValue.Name = 'time' Then
    time := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else
    inherited;
end;

procedure TcdaParticipant1.Settime(const Value: Tv3IVL);
begin
  Ftime.Free;
  Ftime := Value;
  if Ftime <> nil then
    Ftime.Parent := self;
end;

function TcdaParticipant1.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassociatedEntity.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, FfunctionCode.sizeInBytes);
  inc(result, (FcontextControlCode.length * sizeof(char)) + 12);
  inc(result, Ftime.sizeInBytes);
end;

{ TcdaParticipant2 }

Function TcdaParticipant2.CDAClassNameV : String;
Begin
  Result := 'Participant2';
End;

function TcdaParticipant2.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaParticipant2.Assign(oSource: TFslObject);
begin
  inherited;
  time := TcdaParticipant2(oSource).time.Clone(self);
  awarenessCode := TcdaParticipant2(oSource).awarenessCode.Clone(self);
  participantRole := TcdaParticipant2(oSource).participantRole.Clone(self);
  typeCode := TcdaParticipant2(oSource).typeCode;
end;

procedure TcdaParticipant2.DoClear;
begin
  inherited;
  time := Nil;
  awarenessCode := Nil;
  participantRole := Nil;
  typeCode := '';
end;

function TcdaParticipant2.Clone(parent : Tv3Base): TcdaParticipant2;
begin
  Result := TcdaParticipant2(inherited Clone(parent));
end;

constructor TcdaParticipant2.Create;
begin
  inherited;
  FcontextControlCode := 'OP';
end;

destructor TcdaParticipant2.Destroy;
begin
  Ftime.Free;
  FawarenessCode.Free;
  FparticipantRole.Free;
  inherited;
end;

function TcdaParticipant2.Link: TcdaParticipant2;
begin
  Result := TcdaParticipant2(Inherited Link);
end;

procedure TcdaParticipant2.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'contextControlCode', FcontextControlCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'awarenessCode', FawarenessCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'time', Ftime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participantRole', FparticipantRole, 'ParticipantRole')); // TcdaParticipantRole;
end;

procedure TcdaParticipant2.SetawarenessCode(const Value: Tv3CD);
begin
  FawarenessCode.Free;
  FawarenessCode := Value;
  if FawarenessCode <> nil then
    FawarenessCode.Parent := self;
end;

procedure TcdaParticipant2.SetparticipantRole(const Value: TcdaParticipantRole);
begin
  FparticipantRole.Free;
  FparticipantRole := Value;
  if FparticipantRole <> nil then
    FparticipantRole.Parent := self;
end;

procedure TcdaParticipant2.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'participantRole' Then
    participantRole := TcdaParticipantRole(aValue.AsType(TcdaParticipantRole)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'awarenessCode' Then
    awarenessCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'contextControlCode' Then
    FcontextControlCode := aValue.AsString
  else if aValue.Name = 'time' Then
    time := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else
    inherited;
end;

procedure TcdaParticipant2.Settime(const Value: Tv3IVL);
begin
  Ftime.Free;
  Ftime := Value;
  if Ftime <> nil then
    Ftime.Parent := self;
end;

function TcdaParticipant2.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FparticipantRole.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, FawarenessCode.sizeInBytes);
  inc(result, (FcontextControlCode.length * sizeof(char)) + 12);
  inc(result, Ftime.sizeInBytes);
end;

{ TcdaParticipantRole }

Function TcdaParticipantRole.CDAClassNameV : String;
Begin
  Result := 'ParticipantRole';
End;

function TcdaParticipantRole.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaParticipantRole.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaParticipantRole(oSource).id.Clone(self);
  code := TcdaParticipantRole(oSource).code.Clone(self);
  addr := TcdaParticipantRole(oSource).addr.Clone(self);
  telecom := TcdaParticipantRole(oSource).telecom.Clone(self);
  playingDevice := TcdaParticipantRole(oSource).playingDevice.Clone(self);
  playingEntity := TcdaParticipantRole(oSource).playingEntity.Clone(self);
  scopingEntity := TcdaParticipantRole(oSource).scopingEntity.Clone(self);
  FclassCode := TcdaParticipantRole(oSource).FclassCode;
end;

procedure TcdaParticipantRole.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  addr.Clear;
  telecom.Clear;
  playingDevice := Nil;
  playingEntity := Nil;
  scopingEntity := Nil;
  FclassCode := 'ROL';
end;

function TcdaParticipantRole.Clone(parent : Tv3Base): TcdaParticipantRole;
begin
  Result := TcdaParticipantRole(inherited Clone(parent));
end;

constructor TcdaParticipantRole.Create;
begin
  inherited;
  FclassCode := 'ROL';
  addr := Tv3ListAD.Create(self);
  id := Tv3ListII.Create(self);
  telecom := Tv3ListTEL.Create(self);
end;

destructor TcdaParticipantRole.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Faddr.Free;
  Ftelecom.Free;
  FplayingDevice.Free;
  FplayingEntity.Free;
  FscopingEntity.Free;
  inherited;
end;

function TcdaParticipantRole.Link: TcdaParticipantRole;
begin
  Result := TcdaParticipantRole(Inherited Link);
end;

procedure TcdaParticipantRole.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, rmpctList, 'AD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, rmpctList, 'TEL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'playingDevice', FplayingDevice, 'Device')); // TcdaDevice;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'scopingEntity', FscopingEntity, 'Entity')); // TcdaEntity;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'playingEntity', FplayingEntity, 'PlayingEntity')); // TcdaPlayingEntity;
end;

procedure TcdaParticipantRole.Setaddr(const Value: Tv3ListAD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then
    Faddr.Parent := self;
end;

procedure TcdaParticipantRole.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

procedure TcdaParticipantRole.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.Parent := self;
end;

procedure TcdaParticipantRole.SetplayingDevice(const Value: TcdaDevice);
begin
  FplayingDevice.Free;
  FplayingDevice := Value;
  if FplayingDevice <> nil then
    FplayingDevice.Parent := self;
end;

procedure TcdaParticipantRole.SetplayingEntity(const Value: TcdaPlayingEntity);
begin
  FplayingEntity.Free;
  FplayingEntity := Value;
  if FplayingEntity <> nil then
    FplayingEntity.Parent := self;
end;

procedure TcdaParticipantRole.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'playingDevice' Then
    playingDevice := TcdaDevice(aValue.AsType(TcdaDevice)).Clone(self)
  else if aValue.Name = 'scopingEntity' Then
    scopingEntity := TcdaEntity(aValue.AsType(TcdaEntity)).Clone(self)
  else if aValue.Name = 'playingEntity' Then
    playingEntity := TcdaPlayingEntity(aValue.AsType(TcdaPlayingEntity)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'addr' Then
    addr := Tv3ListAD(aValue.AsType(Tv3ListAD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3ListTEL(aValue.AsType(Tv3ListTEL)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaParticipantRole.SetscopingEntity(const Value: TcdaEntity);
begin
  FscopingEntity.Free;
  FscopingEntity := Value;
  if FscopingEntity <> nil then
    FscopingEntity.Parent := self;
end;

procedure TcdaParticipantRole.Settelecom(const Value: Tv3ListTEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then
    Ftelecom.Parent := self;
end;

function TcdaParticipantRole.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FplayingDevice.sizeInBytes);
  inc(result, FscopingEntity.sizeInBytes);
  inc(result, FplayingEntity.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, Faddr.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaPatient }

Function TcdaPatient.CDAClassNameV : String;
Begin
  Result := 'Patient';
End;

function TcdaPatient.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaPatient.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaPatient(oSource).id.Clone(self);
  name := TcdaPatient(oSource).name.Clone(self);
  administrativeGenderCode := TcdaPatient(oSource).administrativeGenderCode.Clone(self);
  birthTime := TcdaPatient(oSource).birthTime.Clone(self);
  maritalStatusCode := TcdaPatient(oSource).maritalStatusCode.Clone(self);
  religiousAffiliationCode := TcdaPatient(oSource).religiousAffiliationCode.Clone(self);
  raceCode := TcdaPatient(oSource).raceCode.Clone(self);
  ethnicGroupCode := TcdaPatient(oSource).ethnicGroupCode.Clone(self);
  guardian := TcdaPatient(oSource).guardian.Clone(self);
  birthplace := TcdaPatient(oSource).birthplace.Clone(self);
  languageCommunication := TcdaPatient(oSource).languageCommunication.Clone(self);
  asEntityIdentifier := TcdaPatient(oSource).asEntityIdentifier.Clone(self);
end;

procedure TcdaPatient.DoClear;
begin
  inherited;
  id := Nil;
  name.Clear;
  administrativeGenderCode := Nil;
  birthTime := Nil;
  maritalStatusCode := Nil;
  religiousAffiliationCode := Nil;
  raceCode := Nil;
  ethnicGroupCode := Nil;
  guardian.Clear;
  birthplace := Nil;
  languageCommunication.Clear;
  asEntityIdentifier.Clear;
end;

function TcdaPatient.Clone(parent : Tv3Base): TcdaPatient;
begin
  Result := TcdaPatient(inherited Clone(parent));
end;

constructor TcdaPatient.Create;
begin
  inherited;
  FclassCode := 'PSN';
  FdeterminerCode := 'INSTANCE';
  guardian := TcdaGuardianList.Create(self);
  languageCommunication := TcdaLanguageCommunicationList.Create(self);
  asEntityIdentifier := TcdaEntityIdentifierList.Create(self);
  name := Tv3ListEN.Create(self);
end;

destructor TcdaPatient.Destroy;
begin
  Fid.Free;
  Fname.Free;
  FadministrativeGenderCode.Free;
  FbirthTime.Free;
  FmaritalStatusCode.Free;
  FreligiousAffiliationCode.Free;
  FraceCode.Free;
  FethnicGroupCode.Free;
  Fguardian.Free;
  Fbirthplace.Free;
  FlanguageCommunication.Free;
  FEntityIdentifier.free;
  inherited;
end;


function TcdaPatient.Link: TcdaPatient;
begin
  Result := TcdaPatient(Inherited Link);
end;

procedure TcdaPatient.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'administrativeGenderCode', FadministrativeGenderCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'raceCode', FraceCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'maritalStatusCode', FmaritalStatusCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'religiousAffiliationCode', FreligiousAffiliationCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'ethnicGroupCode', FethnicGroupCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, rmpctList, 'EN'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'birthTime', FbirthTime, 'TS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'birthplace', Fbirthplace, 'Birthplace')); // TcdaBirthplace;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'guardian', Fguardian, rmpctList, 'Guardian')); // TcdaGuardianList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCommunication', FlanguageCommunication, rmpctList, 'LanguageCommunication')); // TcdaLanguageCommunicationList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'EntityIdentifier', FEntityIdentifier, rmpctList, 'EntityIdentifier')); // TcdaEntityIdentifierList;
end;

procedure TcdaPatient.SetadministrativeGenderCode(const Value: Tv3CD);
begin
  FadministrativeGenderCode.Free;
  FadministrativeGenderCode := Value;
  if FadministrativeGenderCode <> nil then
    FadministrativeGenderCode.Parent := self;
end;

procedure TcdaPatient.Setbirthplace(const Value: TcdaBirthplace);
begin
  Fbirthplace.Free;
  Fbirthplace := Value;
  if Fbirthplace <> nil then
    Fbirthplace.Parent := self;
end;

procedure TcdaPatient.SetbirthTime(const Value: Tv3TS);
begin
  FbirthTime.Free;
  FbirthTime := Value;
  if FbirthTime <> nil then
    FbirthTime.Parent := self;
end;

procedure TcdaPatient.SetethnicGroupCode(const Value: Tv3CD);
begin
  FethnicGroupCode.Free;
  FethnicGroupCode := Value;
  if FethnicGroupCode <> nil then
    FethnicGroupCode.Parent := self;
end;

Function TcdaPatient.Getguardian : TcdaGuardianList;
begin
  if Fguardian = nil Then
    Fguardian := TcdaGuardianList.Create(self);
  result := Fguardian;
end;

procedure TcdaPatient.Setguardian(const Value: TcdaGuardianList);
begin
  Fguardian.Free;
  Fguardian := Value;
  if Fguardian <> nil then
    Fguardian.Parent := self;
end;

procedure TcdaPatient.Setid(const Value: Tv3II);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.Parent := self;
end;

Function TcdaPatient.GetlanguageCommunication : TcdaLanguageCommunicationList;
begin
  if FlanguageCommunication = nil Then
    FlanguageCommunication := TcdaLanguageCommunicationList.Create(self);
  result := FlanguageCommunication;
end;

procedure TcdaPatient.SetlanguageCommunication(const Value: TcdaLanguageCommunicationList);
begin
  FlanguageCommunication.Free;
  FlanguageCommunication := Value;
  if FlanguageCommunication <> nil then
    FlanguageCommunication.Parent := self;
end;

Function TcdaPatient.GetEntityIdentifier : TcdaEntityIdentifierList;
begin
  if FEntityIdentifier = nil Then
    FEntityIdentifier := TcdaEntityIdentifierList.Create(self);
  result := FEntityIdentifier;
end;

procedure TcdaPatient.SetEntityIdentifier(const Value: TcdaEntityIdentifierList);
begin
  FEntityIdentifier.Free;
  FEntityIdentifier := Value;
  if FEntityIdentifier <> nil then
    FEntityIdentifier.Parent := self;
end;

procedure TcdaPatient.SetmaritalStatusCode(const Value: Tv3CD);
begin
  FmaritalStatusCode.Free;
  FmaritalStatusCode := Value;
  if FmaritalStatusCode <> nil then
    FmaritalStatusCode.Parent := self;
end;

procedure TcdaPatient.Setname(const Value: Tv3ListEN);
begin
  Fname.Free;
  Fname := Value;
  if Fname <> nil then
    Fname.Parent := self;
end;

procedure TcdaPatient.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'birthplace' Then
    birthplace := TcdaBirthplace(aValue.AsType(TcdaBirthplace)).Clone(self)
  else if aValue.Name = 'guardian' Then
    guardian := TcdaGuardianList(aValue.AsType(TcdaGuardianList)).Clone(self)
  else if aValue.Name = 'languageCommunication' Then
    languageCommunication := TcdaLanguageCommunicationList(aValue.AsType(TcdaLanguageCommunicationList)).Clone(self)
  else if aValue.Name = 'EntityIdentifier' Then
    asEntityIdentifier := TcdaEntityIdentifierList(aValue.AsType(TcdaEntityIdentifierList)).Clone(self)
  else if aValue.Name = 'administrativeGenderCode' Then
    administrativeGenderCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'raceCode' Then
    raceCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'maritalStatusCode' Then
    maritalStatusCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'religiousAffiliationCode' Then
    religiousAffiliationCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'ethnicGroupCode' Then
    ethnicGroupCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else if aValue.Name = 'id' Then
    id := Tv3II(aValue.AsType(Tv3II)).Clone(self)
  else if aValue.Name = 'name' Then
    name := Tv3ListEN(aValue.AsType(Tv3ListEN)).Clone(self)
  else if aValue.Name = 'birthTime' Then
    birthTime := Tv3TS(aValue.AsType(Tv3TS)).Clone(self)
  else
    inherited;
end;

procedure TcdaPatient.SetraceCode(const Value: Tv3CD);
begin
  FraceCode.Free;
  FraceCode := Value;
  if FraceCode <> nil then
    FraceCode.Parent := self;
end;

procedure TcdaPatient.SetreligiousAffiliationCode(const Value: Tv3CD);
begin
  FreligiousAffiliationCode.Free;
  FreligiousAffiliationCode := Value;
  if FreligiousAffiliationCode <> nil then
    FreligiousAffiliationCode.Parent := self;
end;

function TcdaPatient.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fbirthplace.sizeInBytes);
  inc(result, Fguardian.sizeInBytes);
  inc(result, FlanguageCommunication.sizeInBytes);
  inc(result, FadministrativeGenderCode.sizeInBytes);
  inc(result, FraceCode.sizeInBytes);
  inc(result, FmaritalStatusCode.sizeInBytes);
  inc(result, FreligiousAffiliationCode.sizeInBytes);
  inc(result, FethnicGroupCode.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
  inc(result, Fid.sizeInBytes);
  inc(result, Fname.sizeInBytes);
  inc(result, FbirthTime.sizeInBytes);
  inc(result, FEntityIdentifier.sizeInBytes);
end;

{ TcdaPatientRole }

Function TcdaPatientRole.CDAClassNameV : String;
Begin
  Result := 'PatientRole';
End;

function TcdaPatientRole.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaPatientRole.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaPatientRole(oSource).id.Clone(self);
  addr := TcdaPatientRole(oSource).addr.Clone(self);
  telecom := TcdaPatientRole(oSource).telecom.Clone(self);
  patient := TcdaPatientRole(oSource).patient.Clone(self);
  providerOrganization := TcdaPatientRole(oSource).providerOrganization.Clone(self);
end;

procedure TcdaPatientRole.DoClear;
begin
  inherited;
  id.Clear;
  addr.Clear;
  telecom.Clear;
  patient := Nil;
  providerOrganization := Nil;
end;

function TcdaPatientRole.Clone(parent : Tv3Base): TcdaPatientRole;
begin
  Result := TcdaPatientRole(inherited Clone(parent));
end;

constructor TcdaPatientRole.Create;
begin
  inherited;
  FclassCode := 'PAT';
  addr := Tv3ListAD.Create(self);
  id := Tv3ListII.Create(self);
  telecom := Tv3ListTEL.Create(self);
end;

destructor TcdaPatientRole.Destroy;
begin
  Fid.Free;
  Faddr.Free;
  Ftelecom.Free;
  Fpatient.Free;
  FproviderOrganization.Free;
  inherited;
end;


function TcdaPatientRole.Link: TcdaPatientRole;
begin
  Result := TcdaPatientRole(Inherited Link);
end;

procedure TcdaPatientRole.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, rmpctList, 'AD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, rmpctList, 'TEL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'providerOrganization', FproviderOrganization, 'Organization')); // TcdaOrganization;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'patient', Fpatient, 'Patient')); // TcdaPatient;
end;

procedure TcdaPatientRole.Setaddr(const Value: Tv3ListAD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then
    Faddr.Parent := self;
end;

procedure TcdaPatientRole.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.Parent := self;
end;

procedure TcdaPatientRole.Setpatient(const Value: TcdaPatient);
begin
  Fpatient.Free;
  Fpatient := Value;
  if Fpatient <> nil then
    Fpatient.Parent := self;
end;

procedure TcdaPatientRole.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'providerOrganization' Then
    providerOrganization := TcdaOrganization(aValue.AsType(TcdaOrganization)).Clone(self)
  else if aValue.Name = 'patient' Then
    patient := TcdaPatient(aValue.AsType(TcdaPatient)).Clone(self)
  else if aValue.Name = 'addr' Then
    addr := Tv3ListAD(aValue.AsType(Tv3ListAD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3ListTEL(aValue.AsType(Tv3ListTEL)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaPatientRole.SetproviderOrganization(const Value: TcdaOrganization);
begin
  FproviderOrganization.Free;
  FproviderOrganization := Value;
  if FproviderOrganization <> nil then
    FproviderOrganization.Parent := self;
end;

procedure TcdaPatientRole.Settelecom(const Value: Tv3ListTEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then
    Ftelecom.Parent := self;
end;

function TcdaPatientRole.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FproviderOrganization.sizeInBytes);
  inc(result, Fpatient.sizeInBytes);
  inc(result, Faddr.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaPerformer1 }

Function TcdaPerformer1.CDAClassNameV : String;
Begin
  Result := 'Performer1';
End;

function TcdaPerformer1.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaPerformer1.Assign(oSource: TFslObject);
begin
  inherited;
  functionCode := TcdaPerformer1(oSource).functionCode.Clone(self);
  time := TcdaPerformer1(oSource).time.Clone(self);
  assignedEntity := TcdaPerformer1(oSource).assignedEntity.Clone(self);
  typeCode := TcdaPerformer1(oSource).typeCode;
end;

procedure TcdaPerformer1.DoClear;
begin
  inherited;
  functionCode := Nil;
  time := Nil;
  assignedEntity := Nil;
  typeCode := '';
end;

function TcdaPerformer1.Clone(parent : Tv3Base): TcdaPerformer1;
begin
  Result := TcdaPerformer1(inherited Clone(parent));
end;

constructor TcdaPerformer1.Create;
begin
  inherited;
end;

destructor TcdaPerformer1.Destroy;
begin
  FfunctionCode.Free;
  Ftime.Free;
  FassignedEntity.Free;
  inherited;
end;

function TcdaPerformer1.Link: TcdaPerformer1;
begin
  Result := TcdaPerformer1(Inherited Link);
end;

procedure TcdaPerformer1.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'functionCode', FfunctionCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'time', Ftime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedEntity', FassignedEntity, 'AssignedEntity')); // TcdaAssignedEntity;
end;

procedure TcdaPerformer1.SetassignedEntity(const Value: TcdaAssignedEntity);
begin
  FassignedEntity.Free;
  FassignedEntity := Value;
  if FassignedEntity <> nil then
    FassignedEntity.Parent := self;
end;

procedure TcdaPerformer1.SetfunctionCode(const Value: Tv3CD);
begin
  FfunctionCode.Free;
  FfunctionCode := Value;
  if FfunctionCode <> nil then
    FfunctionCode.Parent := self;
end;

procedure TcdaPerformer1.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedEntity' Then
    assignedEntity := TcdaAssignedEntity(aValue.AsType(TcdaAssignedEntity)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'functionCode' Then
    functionCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'time' Then
    time := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else
    inherited;
end;

procedure TcdaPerformer1.Settime(const Value: Tv3IVL);
begin
  Ftime.Free;
  Ftime := Value;
  if Ftime <> nil then
    Ftime.Parent := self;
end;

function TcdaPerformer1.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedEntity.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, FfunctionCode.sizeInBytes);
  inc(result, Ftime.sizeInBytes);
end;

{ TcdaPerformer2 }

Function TcdaPerformer2.CDAClassNameV : String;
Begin
  Result := 'Performer2';
End;

function TcdaPerformer2.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaPerformer2.Assign(oSource: TFslObject);
begin
  inherited;
  time := TcdaPerformer2(oSource).time.Clone(self);
  modeCode := TcdaPerformer2(oSource).modeCode.Clone(self);
  assignedEntity := TcdaPerformer2(oSource).assignedEntity.Clone(self);
end;

procedure TcdaPerformer2.DoClear;
begin
  inherited;
  time := Nil;
  modeCode := Nil;
  assignedEntity := Nil;
end;

function TcdaPerformer2.Clone(parent : Tv3Base): TcdaPerformer2;
begin
  Result := TcdaPerformer2(inherited Clone(parent));
end;

constructor TcdaPerformer2.Create;
begin
  inherited;
  FtypeCode := 'PRF';
end;

destructor TcdaPerformer2.Destroy;
begin
  Ftime.Free;
  FmodeCode.Free;
  FassignedEntity.Free;
  inherited;
end;

function TcdaPerformer2.Link: TcdaPerformer2;
begin
  Result := TcdaPerformer2(Inherited Link);
end;

procedure TcdaPerformer2.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'modeCode', FmodeCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'time', Ftime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedEntity', FassignedEntity, 'AssignedEntity')); // TcdaAssignedEntity;
end;

procedure TcdaPerformer2.SetassignedEntity(const Value: TcdaAssignedEntity);
begin
  FassignedEntity.Free;
  FassignedEntity := Value;
  if FassignedEntity <> nil then
    FassignedEntity.Parent := self;
end;

procedure TcdaPerformer2.SetmodeCode(const Value: Tv3CD);
begin
  FmodeCode.Free;
  FmodeCode := Value;
  if FmodeCode <> nil then
    FmodeCode.Parent := self;
end;

procedure TcdaPerformer2.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedEntity' Then
    assignedEntity := TcdaAssignedEntity(aValue.AsType(TcdaAssignedEntity)).Clone(self)
  else if aValue.Name = 'modeCode' Then
    modeCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'time' Then
    time := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaPerformer2.Settime(const Value: Tv3IVL);
begin
  Ftime.Free;
  Ftime := Value;
  if Ftime <> nil then
    Ftime.Parent := self;
end;

function TcdaPerformer2.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedEntity.sizeInBytes);
  inc(result, FmodeCode.sizeInBytes);
  inc(result, Ftime.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaPerson }

Function TcdaPerson.CDAClassNameV : String;
Begin
  Result := 'Person';
End;

function TcdaPerson.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaPerson.Assign(oSource: TFslObject);
begin
  inherited;
  name := TcdaPerson(oSource).name.Clone(self);
end;

procedure TcdaPerson.DoClear;
begin
  inherited;
  name.Clear;
end;

function TcdaPerson.Clone(parent : Tv3Base): TcdaPerson;
begin
  Result := TcdaPerson(inherited Clone(parent));
end;

constructor TcdaPerson.Create;
begin
  inherited;
  name := Tv3ListEN.Create(self);
end;

destructor TcdaPerson.Destroy;
begin
  Fname.Free;
  inherited;
end;


function TcdaPerson.Link: TcdaPerson;
begin
  Result := TcdaPerson(Inherited Link);
end;

procedure TcdaPerson.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, rmpctList, 'EN'));
end;

procedure TcdaPerson.Setname(const Value: Tv3ListEN);
begin
  Fname.Free;
  Fname := Value;
  if Fname <> nil then
    Fname.Parent := self;
end;

procedure TcdaPerson.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'name' Then
    name := Tv3ListEN(aValue.AsType(Tv3ListEN)).Clone(self)
  else
    inherited;
end;

function TcdaPerson.GetClassCode: String;
begin
  result := 'PSN';
end;

function TcdaPerson.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fname.sizeInBytes);
end;

{ TcdaPlace }

Function TcdaPlace.CDAClassNameV : String;
Begin
  Result := 'Place';
End;

function TcdaPlace.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaPlace.Assign(oSource: TFslObject);
begin
  inherited;
  name := TcdaPlace(oSource).name.Clone(self);
  addr := TcdaPlace(oSource).addr.Clone(self);
end;

procedure TcdaPlace.DoClear;
begin
  inherited;
  name := Nil;
  addr := Nil;
end;

function TcdaPlace.Clone(parent : Tv3Base): TcdaPlace;
begin
  Result := TcdaPlace(inherited Clone(parent));
end;

constructor TcdaPlace.Create;
begin
  inherited;
  FclassCode := 'PLC';
  FdeterminerCode := 'INSTANCE';
end;

destructor TcdaPlace.Destroy;
begin
  Fname.Free;
  Faddr.Free;
  inherited;
end;

function TcdaPlace.Link: TcdaPlace;
begin
  Result := TcdaPlace(Inherited Link);
end;

procedure TcdaPlace.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, 'AD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, 'EN'));
end;

procedure TcdaPlace.Setaddr(const Value: Tv3AD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then
    Faddr.Parent := self;
end;

procedure TcdaPlace.Setname(const Value: Tv3EN);
begin
  Fname.Free;
  Fname := Value;
  if Fname <> nil then
    Fname.Parent := self;
end;

procedure TcdaPlace.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'addr' Then
    addr := Tv3AD(aValue.AsType(Tv3AD)).Clone(self)
  else if aValue.Name = 'name' Then
    name := Tv3EN(aValue.AsType(Tv3EN)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else
    inherited;
end;

function TcdaPlace.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Faddr.sizeInBytes);
  inc(result, Fname.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
end;

{ TcdaPlayingEntity }

Function TcdaPlayingEntity.CDAClassNameV : String;
Begin
  Result := 'PlayingEntity';
End;

function TcdaPlayingEntity.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaPlayingEntity.Assign(oSource: TFslObject);
begin
  inherited;
  code := TcdaPlayingEntity(oSource).code.Clone(self);
  quantity := TcdaPlayingEntity(oSource).quantity.Clone(self);
  name := TcdaPlayingEntity(oSource).name.Clone(self);
  desc := TcdaPlayingEntity(oSource).desc.Clone(self);
  FclassCode := TcdaPlayingEntity(oSource).FclassCode;
end;

procedure TcdaPlayingEntity.DoClear;
begin
  inherited;
  code := Nil;
  quantity.Clear;
  name.Clear;
  desc := Nil;
  FclassCode := 'ENT';
end;

function TcdaPlayingEntity.Clone(parent : Tv3Base): TcdaPlayingEntity;
begin
  Result := TcdaPlayingEntity(inherited Clone(parent));
end;

constructor TcdaPlayingEntity.Create;
begin
  inherited;
  FclassCode := 'ENT';
  FdeterminerCode := 'INSTANCE';
  name := Tv3ListEN.Create(self);
  quantity := Tv3ListPQ.Create(self);
end;

destructor TcdaPlayingEntity.Destroy;
begin
  Fcode.Free;
  Fquantity.Free;
  Fname.Free;
  Fdesc.Free;
  inherited;
end;


function TcdaPlayingEntity.Link: TcdaPlayingEntity;
begin
  Result := TcdaPlayingEntity(Inherited Link);
end;

procedure TcdaPlayingEntity.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'desc', Fdesc, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, rmpctList, 'EN'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'quantity', Fquantity, rmpctList, 'PQ'));
end;

procedure TcdaPlayingEntity.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

procedure TcdaPlayingEntity.Setdesc(const Value: Tv3ED);
begin
  Fdesc.Free;
  Fdesc := Value;
  if Fdesc <> nil then
    Fdesc.Parent := self;
end;

procedure TcdaPlayingEntity.Setname(const Value: Tv3ListEN);
begin
  Fname.Free;
  Fname := Value;
  if Fname <> nil then
    Fname.Parent := self;
end;

procedure TcdaPlayingEntity.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'desc' Then
    desc := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else if aValue.Name = 'name' Then
    name := Tv3ListEN(aValue.AsType(Tv3ListEN)).Clone(self)
  else if aValue.Name = 'quantity' Then
    quantity := Tv3ListPQ(aValue.AsType(Tv3ListPQ)).Clone(self)
  else
    inherited;
end;

procedure TcdaPlayingEntity.Setquantity(const Value: Tv3ListPQ);
begin
  Fquantity.Free;
  Fquantity := Value;
  if Fquantity <> nil then
    Fquantity.Parent := self;
end;

function TcdaPlayingEntity.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcode.sizeInBytes);
  inc(result, Fdesc.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
  inc(result, Fname.sizeInBytes);
  inc(result, Fquantity.sizeInBytes);
end;

{ TcdaPrecondition }

Function TcdaPrecondition.CDAClassNameV : String;
Begin
  Result := 'Precondition';
End;

function TcdaPrecondition.CDAClassTypeV: TCDAClassType;
begin
  result := etActRel;
end;

procedure TcdaPrecondition.Assign(oSource: TFslObject);
begin
  inherited;
  criterion := TcdaPrecondition(oSource).criterion.Clone(self);
end;

procedure TcdaPrecondition.DoClear;
begin
  inherited;
  criterion := Nil;
end;

function TcdaPrecondition.Clone(parent : Tv3Base): TcdaPrecondition;
begin
  Result := TcdaPrecondition(inherited Clone(parent));
end;

constructor TcdaPrecondition.Create;
begin
  inherited;
  FtypeCode := 'PRCN';
end;

destructor TcdaPrecondition.Destroy;
begin
  Fcriterion.Free;
  inherited;
end;

function TcdaPrecondition.Link: TcdaPrecondition;
begin
  Result := TcdaPrecondition(Inherited Link);
end;

procedure TcdaPrecondition.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'criterion', Fcriterion, 'Criterion')); // TcdaCriterion;
end;

procedure TcdaPrecondition.Setcriterion(const Value: TcdaCriterion);
begin
  Fcriterion.Free;
  Fcriterion := Value;
  if Fcriterion <> nil then
    Fcriterion.Parent := self;
end;

procedure TcdaPrecondition.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'criterion' Then
    criterion := TcdaCriterion(aValue.AsType(TcdaCriterion)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaPrecondition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcriterion.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaProcedure }

Function TcdaProcedure.CDAClassNameV : String;
Begin
  Result := 'Procedure';
End;

function TcdaProcedure.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaProcedure.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaProcedure(oSource).id.Clone(self);
  code := TcdaProcedure(oSource).code.Clone(self);
  text := TcdaProcedure(oSource).text.Clone(self);
  statusCode := TcdaProcedure(oSource).statusCode.Clone(self);
  effectiveTime := TcdaProcedure(oSource).effectiveTime.Clone(self);
  priorityCode := TcdaProcedure(oSource).priorityCode.Clone(self);
  languageCode := TcdaProcedure(oSource).languageCode.Clone(self);
  methodCode := TcdaProcedure(oSource).methodCode.Clone(self);
  approachSiteCode := TcdaProcedure(oSource).approachSiteCode.Clone(self);
  targetSiteCode := TcdaProcedure(oSource).targetSiteCode.Clone(self);
  subject := TcdaProcedure(oSource).subject.Clone(self);
  specimen := TcdaProcedure(oSource).specimen.Clone(self);
  performer := TcdaProcedure(oSource).performer.Clone(self);
  author := TcdaProcedure(oSource).author.Clone(self);
  informant := TcdaProcedure(oSource).informant.Clone(self);
  participant := TcdaProcedure(oSource).participant.Clone(self);
  entryRelationship := TcdaProcedure(oSource).entryRelationship.Clone(self);
  reference := TcdaProcedure(oSource).reference.Clone(self);
  precondition := TcdaProcedure(oSource).precondition.Clone(self);
  negationInd := TcdaProcedure(oSource).negationInd;
end;

procedure TcdaProcedure.DoClear;
begin
  inherited;
  code := Nil;
  text := Nil;
  statusCode := Nil;
  effectiveTime := Nil;
  priorityCode := Nil;
  languageCode := Nil;
  subject := Nil;
  negationInd := False;

  approachSiteCode.Clear;
  author.Clear;
  entryRelationship.Clear;
  id.Clear;
  informant.Clear;
  methodCode.Clear;
  participant.Clear;
  performer.Clear;
  precondition.Clear;
  reference.Clear;
  specimen.Clear;
  targetSiteCode.Clear;
end;

function TcdaProcedure.Clone(parent : Tv3Base): TcdaProcedure;
begin
  Result := TcdaProcedure(inherited Clone(parent));
end;

constructor TcdaProcedure.Create;
begin
  inherited;
  approachSiteCode := Tv3ListCD.Create(self);
  author := TcdaAuthorList.Create(self);
  entryRelationship := TcdaEntryRelationshipList.Create(self);
  id := Tv3ListII.Create(self);
  informant := TcdaInformant12List.Create(self);
  methodCode := Tv3ListCD.Create(self);
  participant := TcdaParticipant2List.Create(self);
  performer := TcdaPerformer2List.Create(self);
  precondition := TcdaPreconditionList.Create(self);
  reference := TcdaReferenceList.Create(self);
  specimen := TcdaSpecimenList.Create(self);
  targetSiteCode := Tv3ListCD.Create(self);
end;


destructor TcdaProcedure.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Ftext.Free;
  FstatusCode.Free;
  FeffectiveTime.Free;
  FpriorityCode.Free;
  FlanguageCode.Free;
  FmethodCode.Free;
  FapproachSiteCode.Free;
  FtargetSiteCode.Free;
  Fsubject.Free;
  Fspecimen.Free;
  Fperformer.Free;
  Fauthor.Free;
  Finformant.Free;
  Fparticipant.Free;
  FentryRelationship.Free;
  Freference.Free;
  Fprecondition.Free;
  inherited;
end;


function TcdaProcedure.Link: TcdaProcedure;
begin
  Result := TcdaProcedure(Inherited Link);
end;

procedure TcdaProcedure.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'negationInd', FHasNegationInd, FnegationInd));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'priorityCode', FpriorityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCode', FlanguageCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'statusCode', FstatusCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'methodCode', FmethodCode, rmpctList, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'approachSiteCode', FapproachSiteCode, rmpctList, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'targetSiteCode', FtargetSiteCode, rmpctList, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entryRelationship', FentryRelationship, rmpctList, 'EntryRelationship')); // TcdaEntryRelationshipList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant2')); // TcdaParticipant2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer2')); // TcdaPerformer2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'precondition', Fprecondition, rmpctList, 'Precondition')); // TcdaPreconditionList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, rmpctList, 'Reference')); // TcdaReferenceList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimen', Fspecimen, rmpctList, 'Specimen')); // TcdaSpecimenList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

procedure TcdaProcedure.SetapproachSiteCode(const Value: Tv3ListCD);
begin
  FapproachSiteCode.Free;
  FapproachSiteCode := Value;
  if FapproachSiteCode <> nil then
    FapproachSiteCode.Parent := self;
end;

Function TcdaProcedure.Getauthor : TcdaAuthorList;
begin
  if Fauthor = nil Then
    Fauthor := TcdaAuthorList.Create(self);
  result := Fauthor;
end;

procedure TcdaProcedure.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then
    Fauthor.Parent := self;
end;

procedure TcdaProcedure.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

procedure TcdaProcedure.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then
    FeffectiveTime.Parent := self;
end;

Function TcdaProcedure.GetentryRelationship : TcdaEntryRelationshipList;
begin
  if FentryRelationship = nil Then
    FentryRelationship := TcdaEntryRelationshipList.Create(self);
  result := FentryRelationship;
end;

procedure TcdaProcedure.SetentryRelationship(const Value: TcdaEntryRelationshipList);
begin
  FentryRelationship.Free;
  FentryRelationship := Value;
  if FentryRelationship <> nil then
    FentryRelationship.Parent := self;
end;


Function TcdaProcedure.Getinformant : TcdaInformant12List;
begin
  if Finformant = nil Then
    Finformant := TcdaInformant12List.Create(self);
  result := Finformant;
end;

procedure TcdaProcedure.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then
    Finformant.Parent := self;
end;

procedure TcdaProcedure.SetlanguageCode(const Value: Tv3CS);
begin
  FlanguageCode.Free;
  FlanguageCode := Value;
  if FlanguageCode <> nil then
    FlanguageCode.Parent := self;
end;

procedure TcdaProcedure.SetmethodCode(const Value: Tv3ListCD);
begin
  FmethodCode.Free;
  FmethodCode := Value;
  if FmethodCode <> nil then
    FmethodCode.Parent := self;
end;

procedure TcdaProcedure.SetnegationInd(const Value: boolean);
begin
  FHasNegationInd := True;
  FnegationInd := Value;
end;

Function TcdaProcedure.Getparticipant : TcdaParticipant2List;
begin
  if Fparticipant = nil Then
    Fparticipant := TcdaParticipant2List.Create(self);
  result := Fparticipant;
end;

procedure TcdaProcedure.Setparticipant(const Value: TcdaParticipant2List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then
    Fparticipant.Parent := self;
end;

Function TcdaProcedure.Getperformer : TcdaPerformer2List;
begin
  if Fperformer = nil Then
    Fperformer := TcdaPerformer2List.Create(self);
  result := Fperformer;
end;

procedure TcdaProcedure.Setperformer(const Value: TcdaPerformer2List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then
    Fperformer.Parent := self;
end;

Function TcdaProcedure.Getprecondition : TcdaPreconditionList;
begin
  if Fprecondition = nil Then
    Fprecondition := TcdaPreconditionList.Create(self);
  result := Fprecondition;
end;

procedure TcdaProcedure.Setprecondition(const Value: TcdaPreconditionList);
begin
  Fprecondition.Free;
  Fprecondition := Value;
  if Fprecondition <> nil then
    Fprecondition.Parent := self;
end;

procedure TcdaProcedure.SetpriorityCode(const Value: Tv3CD);
begin
  FpriorityCode.Free;
  FpriorityCode := Value;
  if FpriorityCode <> nil then
    FpriorityCode.Parent := self;
end;

procedure TcdaProcedure.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'entryRelationship' Then
    entryRelationship := TcdaEntryRelationshipList(aValue.AsType(TcdaEntryRelationshipList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant2List(aValue.AsType(TcdaParticipant2List)).Clone(self)
  else if aValue.Name = 'performer' Then
    performer := TcdaPerformer2List(aValue.AsType(TcdaPerformer2List)).Clone(self)
  else if aValue.Name = 'precondition' Then
    precondition := TcdaPreconditionList(aValue.AsType(TcdaPreconditionList)).Clone(self)
  else if aValue.Name = 'reference' Then
    reference := TcdaReferenceList(aValue.AsType(TcdaReferenceList)).Clone(self)
  else if aValue.Name = 'specimen' Then
    specimen := TcdaSpecimenList(aValue.AsType(TcdaSpecimenList)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'negationInd' Then
    aValue.AsBool(FHasNegationInd, FnegationInd)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'priorityCode' Then
    priorityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'languageCode' Then
    languageCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'statusCode' Then
    statusCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'methodCode' Then
    methodCode := Tv3ListCD(aValue.AsType(Tv3ListCD)).Clone(self)
  else if aValue.Name = 'approachSiteCode' Then
    approachSiteCode := Tv3ListCD(aValue.AsType(Tv3ListCD)).Clone(self)
  else if aValue.Name = 'targetSiteCode' Then
    targetSiteCode := Tv3ListCD(aValue.AsType(Tv3ListCD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

Function TcdaProcedure.Getreference : TcdaReferenceList;
begin
  if Freference = nil Then
    Freference := TcdaReferenceList.Create(self);
  result := Freference;
end;

procedure TcdaProcedure.Setreference(const Value: TcdaReferenceList);
begin
  Freference.Free;
  Freference := Value;
  if Freference <> nil then
    Freference.Parent := self;
end;

Function TcdaProcedure.Getspecimen : TcdaSpecimenList;
begin
  if Fspecimen = nil Then
    Fspecimen := TcdaSpecimenList.Create(self);
  result := Fspecimen;
end;

procedure TcdaProcedure.Setspecimen(const Value: TcdaSpecimenList);
begin
  Fspecimen.Free;
  Fspecimen := Value;
  if Fspecimen <> nil then
    Fspecimen.Parent := self;
end;

procedure TcdaProcedure.SetstatusCode(const Value: Tv3CS);
begin
  FstatusCode.Free;
  FstatusCode := Value;
  if FstatusCode <> nil then
    FstatusCode.Parent := self;
end;

procedure TcdaProcedure.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then
    Fsubject.Parent := self;
end;

procedure TcdaProcedure.SettargetSiteCode(const Value: Tv3ListCD);
begin
  FtargetSiteCode.Free;
  FtargetSiteCode := Value;
  if FtargetSiteCode <> nil then
    FtargetSiteCode.Parent := self;
end;

procedure TcdaProcedure.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then
    Ftext.Parent := self;
end;

function TcdaProcedure.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, FentryRelationship.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, Fperformer.sizeInBytes);
  inc(result, Fprecondition.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, Fspecimen.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, FpriorityCode.sizeInBytes);
  inc(result, FlanguageCode.sizeInBytes);
  inc(result, FstatusCode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
  inc(result, FmethodCode.sizeInBytes);
  inc(result, FapproachSiteCode.sizeInBytes);
  inc(result, FtargetSiteCode.sizeInBytes);
end;

{ TcdaProduct }

Function TcdaProduct.CDAClassNameV : String;
Begin
  Result := 'Product';
End;

function TcdaProduct.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaProduct.Assign(oSource: TFslObject);
begin
  inherited;
  manufacturedProduct := TcdaProduct(oSource).manufacturedProduct.Clone(self);
end;

procedure TcdaProduct.DoClear;
begin
  inherited;
  manufacturedProduct := Nil;
end;

function TcdaProduct.Clone(parent : Tv3Base): TcdaProduct;
begin
  Result := TcdaProduct(inherited Clone(parent));
end;

constructor TcdaProduct.Create;
begin
  inherited;
  FtypeCode := 'PRD';
end;

destructor TcdaProduct.Destroy;
begin
  FmanufacturedProduct.Free;
  inherited;
end;

function TcdaProduct.Link: TcdaProduct;
begin
  Result := TcdaProduct(Inherited Link);
end;

procedure TcdaProduct.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'manufacturedProduct', FmanufacturedProduct, 'ManufacturedProduct')); // TcdaManufacturedProduct;
end;

procedure TcdaProduct.SetmanufacturedProduct(const Value: TcdaManufacturedProduct);
begin
  FmanufacturedProduct.Free;
  FmanufacturedProduct := Value;
  if FmanufacturedProduct <> nil then
    FmanufacturedProduct.Parent := self;
end;

procedure TcdaProduct.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'manufacturedProduct' Then
    manufacturedProduct := TcdaManufacturedProduct(aValue.AsType(TcdaManufacturedProduct)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaProduct.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FmanufacturedProduct.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaRecordTarget }

Function TcdaRecordTarget.CDAClassNameV : String;
Begin
  Result := 'RecordTarget';
End;

function TcdaRecordTarget.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaRecordTarget.Assign(oSource: TFslObject);
begin
  inherited;
  patientRole := TcdaRecordTarget(oSource).patientRole.Clone(self);
end;

procedure TcdaRecordTarget.DoClear;
begin
  inherited;
  patientRole := Nil;
end;

function TcdaRecordTarget.Clone(parent : Tv3Base): TcdaRecordTarget;
begin
  Result := TcdaRecordTarget(inherited Clone(parent));
end;

constructor TcdaRecordTarget.Create;
begin
  inherited;
  FtypeCode := 'RCT';
  FcontextControlCode := 'OP';
end;

destructor TcdaRecordTarget.Destroy;
begin
  FpatientRole.Free;
  inherited;
end;

function TcdaRecordTarget.Link: TcdaRecordTarget;
begin
  Result := TcdaRecordTarget(Inherited Link);
end;

procedure TcdaRecordTarget.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'contextControlCode', FcontextControlCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'patientRole', FpatientRole, 'PatientRole')); // TcdaPatientRole;
end;

procedure TcdaRecordTarget.SetpatientRole(const Value: TcdaPatientRole);
begin
  FpatientRole.Free;
  FpatientRole := Value;
  if FpatientRole <> nil then
    FpatientRole.Parent := self;
end;

procedure TcdaRecordTarget.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'patientRole' Then
    patientRole := TcdaPatientRole(aValue.AsType(TcdaPatientRole)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'contextControlCode' Then
    FcontextControlCode := aValue.AsString
  else
    inherited;
end;

function TcdaRecordTarget.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FpatientRole.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, (FcontextControlCode.length * sizeof(char)) + 12);
end;

{ TcdaReference }

Function TcdaReference.CDAClassNameV : String;
Begin
  Result := 'Reference';
End;

function TcdaReference.CDAClassTypeV: TCDAClassType;
begin
  result := etActRel;
end;

procedure TcdaReference.Assign(oSource: TFslObject);
begin
  inherited;
  seperatableInd := TcdaReference(oSource).seperatableInd.Clone(self);
  externalAct := TcdaReference(oSource).externalAct.Clone(self);
  externalObservation := TcdaReference(oSource).externalObservation.Clone(self);
  externalProcedure := TcdaReference(oSource).externalProcedure.Clone(self);
  externalDocument := TcdaReference(oSource).externalDocument.Clone(self);
  typeCode := TcdaReference(oSource).typeCode;
end;

procedure TcdaReference.DoClear;
begin
  inherited;
  seperatableInd := Nil;
  externalAct := Nil;
  externalObservation := Nil;
  externalProcedure := Nil;
  externalDocument := Nil;
  typeCode := '';
end;

function TcdaReference.Clone(parent : Tv3Base): TcdaReference;
begin
  Result := TcdaReference(inherited Clone(parent));
end;

constructor TcdaReference.Create;
begin
  inherited;
end;

destructor TcdaReference.Destroy;
begin
  FseperatableInd.Free;
  FexternalAct.Free;
  FexternalObservation.Free;
  FexternalProcedure.Free;
  FexternalDocument.Free;
  inherited;
end;

function TcdaReference.Link: TcdaReference;
begin
  Result := TcdaReference(Inherited Link);
end;

procedure TcdaReference.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'seperatableInd', FseperatableInd, 'BL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'externalAct', FexternalAct, 'ExternalAct')); // TcdaExternalAct;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'externalDocument', FexternalDocument, 'ExternalDocument')); // TcdaExternalDocument;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'externalObservation', FexternalObservation, 'ExternalObservation')); // TcdaExternalObservation;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'externalProcedure', FexternalProcedure, 'ExternalProcedure')); // TcdaExternalProcedure;
end;

procedure TcdaReference.SetexternalAct(const Value: TcdaExternalAct);
begin
  FexternalAct.Free;
  FexternalAct := Value;
  if FexternalAct <> nil then
    FexternalAct.Parent := self;
end;

procedure TcdaReference.SetexternalDocument(const Value: TcdaExternalDocument);
begin
  FexternalDocument.Free;
  FexternalDocument := Value;
  if FexternalDocument <> nil then
    FexternalDocument.Parent := self;
end;

procedure TcdaReference.SetexternalObservation(const Value: TcdaExternalObservation);
begin
  FexternalObservation.Free;
  FexternalObservation := Value;
  if FexternalObservation <> nil then
    FexternalObservation.Parent := self;
end;

procedure TcdaReference.SetexternalProcedure(const Value: TcdaExternalProcedure);
begin
  FexternalProcedure.Free;
  FexternalProcedure := Value;
  if FexternalProcedure <> nil then
    FexternalProcedure.Parent := self;
end;

procedure TcdaReference.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'externalAct' Then
    externalAct := TcdaExternalAct(aValue.AsType(TcdaExternalAct)).Clone(self)
  else if aValue.Name = 'externalDocument' Then
    externalDocument := TcdaExternalDocument(aValue.AsType(TcdaExternalDocument)).Clone(self)
  else if aValue.Name = 'externalObservation' Then
    externalObservation := TcdaExternalObservation(aValue.AsType(TcdaExternalObservation)).Clone(self)
  else if aValue.Name = 'externalProcedure' Then
    externalProcedure := TcdaExternalProcedure(aValue.AsType(TcdaExternalProcedure)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else if aValue.Name = 'seperatableInd' Then
    seperatableInd := Tv3BL(aValue.AsType(Tv3BL)).Clone(self)
  else
    inherited;
end;

procedure TcdaReference.SetseperatableInd(const Value: Tv3BL);
begin
  FseperatableInd.Free;
  FseperatableInd := Value;
  if FseperatableInd <> nil then
    FseperatableInd.Parent := self;
end;

function TcdaReference.GetExternalActChoice: TcdaExternalActChoice;
begin
  if FexternalAct <> nil Then
    result := FexternalAct
  else if FexternalDocument <> nil Then
    result := FexternalDocument
  else if FexternalObservation <> nil Then
    result := FexternalObservation
  else
    result := FexternalProcedure;
end;

function TcdaReference.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FexternalAct.sizeInBytes);
  inc(result, FexternalDocument.sizeInBytes);
  inc(result, FexternalObservation.sizeInBytes);
  inc(result, FexternalProcedure.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
  inc(result, FseperatableInd.sizeInBytes);
end;

{ TcdaReferenceRange }

Function TcdaReferenceRange.CDAClassNameV : String;
Begin
  Result := 'ReferenceRange';
End;

function TcdaReferenceRange.CDAClassTypeV: TCDAClassType;
begin
  result := etActRel;
end;

procedure TcdaReferenceRange.Assign(oSource: TFslObject);
begin
  inherited;
  observationRange := TcdaReferenceRange(oSource).observationRange.Clone(self);
end;

procedure TcdaReferenceRange.DoClear;
begin
  inherited;
  observationRange := Nil;
end;

function TcdaReferenceRange.Clone(parent : Tv3Base): TcdaReferenceRange;
begin
  Result := TcdaReferenceRange(inherited Clone(parent));
end;

constructor TcdaReferenceRange.Create;
begin
  inherited;
  FtypeCode := 'REFV';
end;

destructor TcdaReferenceRange.Destroy;
begin
  FobservationRange.Free;
  inherited;
end;

function TcdaReferenceRange.Link: TcdaReferenceRange;
begin
  Result := TcdaReferenceRange(Inherited Link);
end;

procedure TcdaReferenceRange.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'observationRange', FobservationRange, 'ObservationRange')); // TcdaObservationRange;
end;

procedure TcdaReferenceRange.SetobservationRange(const Value: TcdaObservationRange);
begin
  FobservationRange.Free;
  FobservationRange := Value;
  if FobservationRange <> nil then
    FobservationRange.Parent := self;
end;

procedure TcdaReferenceRange.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'observationRange' Then
    observationRange := TcdaObservationRange(aValue.AsType(TcdaObservationRange)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaReferenceRange.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FobservationRange.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaRegionOfInterest }

Function TcdaRegionOfInterest.CDAClassNameV : String;
Begin
  Result := 'RegionOfInterest';
End;

function TcdaRegionOfInterest.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaRegionOfInterest.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaRegionOfInterest(oSource).id.Clone(self);
  code := TcdaRegionOfInterest(oSource).code.Clone(self);
  value := TcdaRegionOfInterest(oSource).value.Clone(self);
  subject := TcdaRegionOfInterest(oSource).subject.Clone(self);
  specimen := TcdaRegionOfInterest(oSource).specimen.Clone(self);
  performer := TcdaRegionOfInterest(oSource).performer.Clone(self);
  author := TcdaRegionOfInterest(oSource).author.Clone(self);
  informant := TcdaRegionOfInterest(oSource).informant.Clone(self);
  participant := TcdaRegionOfInterest(oSource).participant.Clone(self);
  entryRelationship := TcdaRegionOfInterest(oSource).entryRelationship.Clone(self);
  reference := TcdaRegionOfInterest(oSource).reference.Clone(self);
  precondition := TcdaRegionOfInterest(oSource).precondition.Clone(self);
  ID_ := TcdaRegionOfInterest(oSource).ID_;
end;

procedure TcdaRegionOfInterest.DoClear;
begin
  inherited;
  code := Nil;
  subject := Nil;
  ID_ := '';
  author.Clear;
  entryRelationship.Clear;
  id.Clear;
  informant.Clear;
  participant.Clear;
  performer.Clear;
  precondition.Clear;
  reference.Clear;
  specimen.Clear;
  value.Clear;
end;

function TcdaRegionOfInterest.Clone(parent : Tv3Base): TcdaRegionOfInterest;
begin
  Result := TcdaRegionOfInterest(inherited Clone(parent));
end;

constructor TcdaRegionOfInterest.Create;
begin
  inherited;
  FclassCode := 'ROIOVL';
  FmoodCode := 'EVN';
  author := TcdaAuthorList.Create(self);
  entryRelationship := TcdaEntryRelationshipList.Create(self);
  id := Tv3ListII.Create(self);
  informant := TcdaInformant12List.Create(self);
  participant := TcdaParticipant2List.Create(self);
  performer := TcdaPerformer2List.Create(self);
  precondition := TcdaPreconditionList.Create(self);
  reference := TcdaReferenceList.Create(self);
  specimen := TcdaSpecimenList.Create(self);
  value := TcdaRegionOfInterest_valueList.Create(self);
end;

destructor TcdaRegionOfInterest.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Fvalue.Free;
  Fsubject.Free;
  Fspecimen.Free;
  Fperformer.Free;
  Fauthor.Free;
  Finformant.Free;
  Fparticipant.Free;
  FentryRelationship.Free;
  Freference.Free;
  Fprecondition.Free;
  inherited;
end;


function TcdaRegionOfInterest.Link: TcdaRegionOfInterest;
begin
  Result := TcdaRegionOfInterest(Inherited Link);
end;

procedure TcdaRegionOfInterest.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'ID', FID_));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entryRelationship', FentryRelationship, rmpctList, 'EntryRelationship')); // TcdaEntryRelationshipList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant2')); // TcdaParticipant2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer2')); // TcdaPerformer2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'precondition', Fprecondition, rmpctList, 'Precondition')); // TcdaPreconditionList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, rmpctList, 'Reference')); // TcdaReferenceList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'value', Fvalue, rmpctList, 'RegionOfInterest_value')); // TcdaRegionOfInterest_valueList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimen', Fspecimen, rmpctList, 'Specimen')); // TcdaSpecimenList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

Function TcdaRegionOfInterest.Getauthor : TcdaAuthorList;
begin
  if Fauthor = nil Then
    Fauthor := TcdaAuthorList.Create(self);
  result := Fauthor;
end;

procedure TcdaRegionOfInterest.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then
    Fauthor.Parent := self;
end;

procedure TcdaRegionOfInterest.Setcode(const Value: Tv3CS);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

Function TcdaRegionOfInterest.GetentryRelationship : TcdaEntryRelationshipList;
begin
  if FentryRelationship = nil Then
    FentryRelationship := TcdaEntryRelationshipList.Create(self);
  result := FentryRelationship;
end;

procedure TcdaRegionOfInterest.SetentryRelationship(const Value: TcdaEntryRelationshipList);
begin
  FentryRelationship.Free;
  FentryRelationship := Value;
  if FentryRelationship <> nil then
    FentryRelationship.Parent := self;
end;


Function TcdaRegionOfInterest.Getinformant : TcdaInformant12List;
begin
  if Finformant = nil Then
    Finformant := TcdaInformant12List.Create(self);
  result := Finformant;
end;

procedure TcdaRegionOfInterest.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then
    Finformant.Parent := self;
end;

Function TcdaRegionOfInterest.Getparticipant : TcdaParticipant2List;
begin
  if Fparticipant = nil Then
    Fparticipant := TcdaParticipant2List.Create(self);
  result := Fparticipant;
end;

procedure TcdaRegionOfInterest.Setparticipant(const Value: TcdaParticipant2List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then
    Fparticipant.Parent := self;
end;

Function TcdaRegionOfInterest.Getperformer : TcdaPerformer2List;
begin
  if Fperformer = nil Then
    Fperformer := TcdaPerformer2List.Create(self);
  result := Fperformer;
end;

procedure TcdaRegionOfInterest.Setperformer(const Value: TcdaPerformer2List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then
    Fperformer.Parent := self;
end;

Function TcdaRegionOfInterest.Getprecondition : TcdaPreconditionList;
begin
  if Fprecondition = nil Then
    Fprecondition := TcdaPreconditionList.Create(self);
  result := Fprecondition;
end;

procedure TcdaRegionOfInterest.Setprecondition(const Value: TcdaPreconditionList);
begin
  Fprecondition.Free;
  Fprecondition := Value;
  if Fprecondition <> nil then
    Fprecondition.Parent := self;
end;

procedure TcdaRegionOfInterest.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'entryRelationship' Then
    entryRelationship := TcdaEntryRelationshipList(aValue.AsType(TcdaEntryRelationshipList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant2List(aValue.AsType(TcdaParticipant2List)).Clone(self)
  else if aValue.Name = 'performer' Then
    performer := TcdaPerformer2List(aValue.AsType(TcdaPerformer2List)).Clone(self)
  else if aValue.Name = 'precondition' Then
    precondition := TcdaPreconditionList(aValue.AsType(TcdaPreconditionList)).Clone(self)
  else if aValue.Name = 'reference' Then
    reference := TcdaReferenceList(aValue.AsType(TcdaReferenceList)).Clone(self)
  else if aValue.Name = 'value' Then
    value := TcdaRegionOfInterest_valueList(aValue.AsType(TcdaRegionOfInterest_valueList)).Clone(self)
  else if aValue.Name = 'specimen' Then
    specimen := TcdaSpecimenList(aValue.AsType(TcdaSpecimenList)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'ID' Then
    FID_ := aValue.AsString
  else
    inherited;
end;

Function TcdaRegionOfInterest.Getreference : TcdaReferenceList;
begin
  if Freference = nil Then
    Freference := TcdaReferenceList.Create(self);
  result := Freference;
end;

procedure TcdaRegionOfInterest.Setreference(const Value: TcdaReferenceList);
begin
  Freference.Free;
  Freference := Value;
  if Freference <> nil then
    Freference.Parent := self;
end;

Function TcdaRegionOfInterest.Getspecimen : TcdaSpecimenList;
begin
  if Fspecimen = nil Then
    Fspecimen := TcdaSpecimenList.Create(self);
  result := Fspecimen;
end;

procedure TcdaRegionOfInterest.Setspecimen(const Value: TcdaSpecimenList);
begin
  Fspecimen.Free;
  Fspecimen := Value;
  if Fspecimen <> nil then
    Fspecimen.Parent := self;
end;

procedure TcdaRegionOfInterest.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then
    Fsubject.Parent := self;
end;

Function TcdaRegionOfInterest.Getvalue : TcdaRegionOfInterest_valueList;
begin
  if Fvalue = nil Then
    Fvalue := TcdaRegionOfInterest_valueList.Create(self);
  result := Fvalue;
end;

procedure TcdaRegionOfInterest.Setvalue(const Value: TcdaRegionOfInterest_valueList);
begin
  Fvalue.Free;
  Fvalue := Value;
  if Fvalue <> nil then
    Fvalue.Parent := self;
end;

function TcdaRegionOfInterest.ClassCodeIsFixed: Boolean;
begin
  result := true;
end;

function TcdaRegionOfInterest.MoodCodeIsFixed: Boolean;
begin
  result := true;
end;

function TcdaRegionOfInterest.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, FentryRelationship.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, Fperformer.sizeInBytes);
  inc(result, Fprecondition.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, Fvalue.sizeInBytes);
  inc(result, Fspecimen.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, (FID_.length * sizeof(char)) + 12);
end;

{ TcdaRelatedDocument }

Function TcdaRelatedDocument.CDAClassNameV : String;
Begin
  Result := 'RelatedDocument';
End;

function TcdaRelatedDocument.CDAClassTypeV: TCDAClassType;
begin
  result := etActRel;
end;

procedure TcdaRelatedDocument.Assign(oSource: TFslObject);
begin
  inherited;
  parentDocument := TcdaRelatedDocument(oSource).parentDocument.Clone(self);
  typeCode := TcdaRelatedDocument(oSource).typeCode;
end;

procedure TcdaRelatedDocument.DoClear;
begin
  inherited;
  parentDocument := Nil;
  typeCode := '';
end;

function TcdaRelatedDocument.Clone(parent : Tv3Base): TcdaRelatedDocument;
begin
  Result := TcdaRelatedDocument(inherited Clone(parent));
end;

constructor TcdaRelatedDocument.Create;
begin
  inherited;
end;

destructor TcdaRelatedDocument.Destroy;
begin
  FparentDocument.Free;
  inherited;
end;

function TcdaRelatedDocument.Link: TcdaRelatedDocument;
begin
  Result := TcdaRelatedDocument(Inherited Link);
end;

procedure TcdaRelatedDocument.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'parentDocument', FparentDocument, 'ParentDocument')); // TcdaParentDocument;
end;

procedure TcdaRelatedDocument.SetparentDocument(const Value: TcdaParentDocument);
begin
  FparentDocument.Free;
  FparentDocument := Value;
  if FparentDocument <> nil then
    FparentDocument.Parent := self;
end;

procedure TcdaRelatedDocument.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'parentDocument' Then
    parentDocument := TcdaParentDocument(aValue.AsType(TcdaParentDocument)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaRelatedDocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FparentDocument.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaRelatedEntity }

Function TcdaRelatedEntity.CDAClassNameV : String;
Begin
  Result := 'RelatedEntity';
End;

function TcdaRelatedEntity.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaRelatedEntity.Assign(oSource: TFslObject);
begin
  inherited;
  effectiveTime := TcdaRelatedEntity(oSource).effectiveTime.Clone(self);
  relatedPerson := TcdaRelatedEntity(oSource).relatedPerson.Clone(self);
end;

procedure TcdaRelatedEntity.DoClear;
begin
  inherited;
  effectiveTime := Nil;
  relatedPerson := Nil;
end;

function TcdaRelatedEntity.Clone(parent : Tv3Base): TcdaRelatedEntity;
begin
  Result := TcdaRelatedEntity(inherited Clone(parent));
end;

constructor TcdaRelatedEntity.Create;
begin
  inherited;
end;


destructor TcdaRelatedEntity.Destroy;
begin
  FeffectiveTime.Free;
  FrelatedPerson.Free;
  inherited;
end;



function TcdaRelatedEntity.Link: TcdaRelatedEntity;
begin
  Result := TcdaRelatedEntity(Inherited Link);
end;

procedure TcdaRelatedEntity.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'relatedPerson', FrelatedPerson, 'Person')); // TcdaPerson;
end;

procedure TcdaRelatedEntity.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then
    FeffectiveTime.Parent := self;
end;

procedure TcdaRelatedEntity.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'relatedPerson' Then
    relatedPerson := TcdaPerson(aValue.AsType(TcdaPerson)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else
    inherited;
end;

procedure TcdaRelatedEntity.SetrelatedPerson(const Value: TcdaPerson);
begin
  FrelatedPerson.Free;
  FrelatedPerson := Value;
  if FrelatedPerson <> nil then
    FrelatedPerson.Parent := self;
end;


function TcdaRelatedEntity.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FrelatedPerson.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
end;

{ TcdaRelatedSubject }

Function TcdaRelatedSubject.CDAClassNameV : String;
Begin
  Result := 'RelatedSubject';
End;

function TcdaRelatedSubject.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaRelatedSubject.Assign(oSource: TFslObject);
begin
  inherited;
  code := TcdaRelatedSubject(oSource).code.Clone(self);
  addr := TcdaRelatedSubject(oSource).addr.Clone(self);
  telecom := TcdaRelatedSubject(oSource).telecom.Clone(self);
  subject := TcdaRelatedSubject(oSource).subject.Clone(self);
  FclassCode := TcdaRelatedSubject(oSource).FclassCode;
end;

procedure TcdaRelatedSubject.DoClear;
begin
  inherited;
  code := Nil;
  addr.Clear;
  telecom.Clear;
  subject := Nil;
  FclassCode := 'PRS';
end;

function TcdaRelatedSubject.Clone(parent : Tv3Base): TcdaRelatedSubject;
begin
  Result := TcdaRelatedSubject(inherited Clone(parent));
end;

constructor TcdaRelatedSubject.Create;
begin
  inherited;
  FclassCode := 'PRS';
  addr := Tv3ListAD.Create(self);
  telecom := Tv3ListTEL.Create(self);
end;

destructor TcdaRelatedSubject.Destroy;
begin
  Fcode.Free;
  Faddr.Free;
  Ftelecom.Free;
  Fsubject.Free;
  inherited;
end;


function TcdaRelatedSubject.Link: TcdaRelatedSubject;
begin
  Result := TcdaRelatedSubject(Inherited Link);
end;

procedure TcdaRelatedSubject.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'addr', Faddr, rmpctList, 'AD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'telecom', Ftelecom, rmpctList, 'TEL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'SubjectPerson')); // TcdaSubjectPerson;
end;

procedure TcdaRelatedSubject.Setaddr(const Value: Tv3ListAD);
begin
  Faddr.Free;
  Faddr := Value;
  if Faddr <> nil then
    Faddr.Parent := self;
end;

procedure TcdaRelatedSubject.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

procedure TcdaRelatedSubject.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'subject' Then
    subject := TcdaSubjectPerson(aValue.AsType(TcdaSubjectPerson)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'addr' Then
    addr := Tv3ListAD(aValue.AsType(Tv3ListAD)).Clone(self)
  else if aValue.Name = 'telecom' Then
    telecom := Tv3ListTEL(aValue.AsType(Tv3ListTEL)).Clone(self)
  else
    inherited;
end;

procedure TcdaRelatedSubject.Setsubject(const Value: TcdaSubjectPerson);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then
    Fsubject.Parent := self;
end;

procedure TcdaRelatedSubject.Settelecom(const Value: Tv3ListTEL);
begin
  Ftelecom.Free;
  Ftelecom := Value;
  if Ftelecom <> nil then
    Ftelecom.Parent := self;
end;

function TcdaRelatedSubject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fsubject.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, Faddr.sizeInBytes);
  inc(result, Ftelecom.sizeInBytes);
end;

{ TcdaResponsibleParty }

Function TcdaResponsibleParty.CDAClassNameV : String;
Begin
  Result := 'ResponsibleParty';
End;

function TcdaResponsibleParty.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaResponsibleParty.Assign(oSource: TFslObject);
begin
  inherited;
  assignedEntity := TcdaResponsibleParty(oSource).assignedEntity.Clone(self);
end;

procedure TcdaResponsibleParty.DoClear;
begin
  inherited;
  assignedEntity := Nil;
end;

function TcdaResponsibleParty.Clone(parent : Tv3Base): TcdaResponsibleParty;
begin
  Result := TcdaResponsibleParty(inherited Clone(parent));
end;

constructor TcdaResponsibleParty.Create;
begin
  inherited;
  FtypeCode := 'RESP';
end;

destructor TcdaResponsibleParty.Destroy;
begin
  FassignedEntity.Free;
  inherited;
end;

function TcdaResponsibleParty.Link: TcdaResponsibleParty;
begin
  Result := TcdaResponsibleParty(Inherited Link);
end;

procedure TcdaResponsibleParty.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'assignedEntity', FassignedEntity, 'AssignedEntity')); // TcdaAssignedEntity;
end;

procedure TcdaResponsibleParty.SetassignedEntity(const Value: TcdaAssignedEntity);
begin
  FassignedEntity.Free;
  FassignedEntity := Value;
  if FassignedEntity <> nil then
    FassignedEntity.Parent := self;
end;

procedure TcdaResponsibleParty.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'assignedEntity' Then
    assignedEntity := TcdaAssignedEntity(aValue.AsType(TcdaAssignedEntity)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

function TcdaResponsibleParty.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FassignedEntity.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaSection }

Function TcdaSection.CDAClassNameV : String;
Begin
  Result := 'Section';
End;

function TcdaSection.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaSection.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaSection(oSource).id.Clone(self);
  code := TcdaSection(oSource).code.Clone(self);
  title := TcdaSection(oSource).title.Clone(self);
  text := TcdaSection(oSource).text.Clone(self);
  confidentialityCode := TcdaSection(oSource).confidentialityCode.Clone(self);
  languageCode := TcdaSection(oSource).languageCode.Clone(self);
  subject := TcdaSection(oSource).subject.Clone(self);
  author := TcdaSection(oSource).author.Clone(self);
  informant := TcdaSection(oSource).informant.Clone(self);
  entry := TcdaSection(oSource).entry.Clone(self);
  component := TcdaSection(oSource).component.Clone(self);
  ID_ := TcdaSection(oSource).ID_;
end;

procedure TcdaSection.DoClear;
begin
  inherited;
  id := Nil;
  code := Nil;
  title := Nil;
  text := Nil;
  confidentialityCode := Nil;
  languageCode := Nil;
  subject := Nil;
  ID_ := '';
  author.Clear;
  informant.Clear;
  entry.Clear;
  component.Clear;
end;

function TcdaSection.Clone(parent : Tv3Base): TcdaSection;
begin
  Result := TcdaSection(inherited Clone(parent));
end;

constructor TcdaSection.Create;
begin
  inherited;
  FclassCode := 'DOCSECT';
  FmoodCode := 'EVN';
  author := TcdaAuthorList.Create(self);
  component := TcdaComponentSectList.Create(self);
  entry := TcdaEntryList.Create(self);
  informant := TcdaInformant12List.Create(self);
end;

destructor TcdaSection.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Ftitle.Free;
  Ftext.Free;
  FconfidentialityCode.Free;
  FlanguageCode.Free;
  Fsubject.Free;
  Fauthor.Free;
  Finformant.Free;
  Fentry.Free;
  Fcomponent.Free;
  inherited;
end;


function TcdaSection.Link: TcdaSection;
begin
  Result := TcdaSection(Inherited Link);
end;

procedure TcdaSection.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'ID', FID_));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'confidentialityCode', FconfidentialityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCode', FlanguageCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'title', Ftitle, 'ST'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'SD.TEXT'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'component', Fcomponent, rmpctList, 'Component5')); // TcdaComponentSectList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entry', Fentry, rmpctList, 'Entry')); // TcdaEntryList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

Function TcdaSection.Getauthor : TcdaAuthorList;
begin
  if Fauthor = nil Then
    Fauthor := TcdaAuthorList.Create(self);
  result := Fauthor;
end;

procedure TcdaSection.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then
    Fauthor.Parent := self;
end;

procedure TcdaSection.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

Function TcdaSection.Getcomponent : TcdaComponentSectList;
begin
  if Fcomponent = nil Then
    Fcomponent := TcdaComponentSectList.Create(self);
  result := Fcomponent;
end;

procedure TcdaSection.Setcomponent(const Value: TcdaComponentSectList);
begin
  Fcomponent.Free;
  Fcomponent := Value;
  if Fcomponent <> nil then
    Fcomponent.Parent := self;
end;

procedure TcdaSection.SetconfidentialityCode(const Value: Tv3CD);
begin
  FconfidentialityCode.Free;
  FconfidentialityCode := Value;
  if FconfidentialityCode <> nil then
    FconfidentialityCode.Parent := self;
end;

Function TcdaSection.Getentry : TcdaEntryList;
begin
  if Fentry = nil Then
    Fentry := TcdaEntryList.Create(self);
  result := Fentry;
end;

procedure TcdaSection.Setentry(const Value: TcdaEntryList);
begin
  Fentry.Free;
  Fentry := Value;
  if Fentry <> nil then
    Fentry.Parent := self;
end;

procedure TcdaSection.Setid(const Value: Tv3II);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.Parent := self;
end;

Function TcdaSection.Getinformant : TcdaInformant12List;
begin
  if Finformant = nil Then
    Finformant := TcdaInformant12List.Create(self);
  result := Finformant;
end;

procedure TcdaSection.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then
    Finformant.Parent := self;
end;

procedure TcdaSection.SetlanguageCode(const Value: Tv3CS);
begin
  FlanguageCode.Free;
  FlanguageCode := Value;
  if FlanguageCode <> nil then
    FlanguageCode.Parent := self;
end;

procedure TcdaSection.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'component' Then
    component := TcdaComponentSectList(aValue.AsType(TcdaComponentSectList)).Clone(self)
  else if aValue.Name = 'entry' Then
    entry := TcdaEntryList(aValue.AsType(TcdaEntryList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'confidentialityCode' Then
    confidentialityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'languageCode' Then
    languageCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3II(aValue.AsType(Tv3II)).Clone(self)
  else if aValue.Name = 'title' Then
    title := Tv3ST(aValue.AsType(Tv3ST)).Clone(self)
  else if aValue.Name = 'ID' Then
    FID_ := aValue.AsString
  else
    inherited;
end;

procedure TcdaSection.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then
    Fsubject.Parent := self;
end;


procedure TcdaSection.Settext(const Value: TsnText);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then
    Ftext.Parent := self;
end;

procedure TcdaSection.Settitle(const Value: Tv3ST);
begin
  Ftitle.Free;
  Ftitle := Value;
  if Ftitle <> nil then
    Ftitle.Parent := self;
end;

function TcdaSection.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, Fcomponent.sizeInBytes);
  inc(result, Fentry.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, FconfidentialityCode.sizeInBytes);
  inc(result, FlanguageCode.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, Ftitle.sizeInBytes);
  inc(result, (FID_.length * sizeof(char)) + 12);
  inc(result, Ftext.sizeInBytes);
end;

{ TcdaServiceEvent }

Function TcdaServiceEvent.CDAClassNameV : String;
Begin
  Result := 'ServiceEvent';
End;

function TcdaServiceEvent.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaServiceEvent.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaServiceEvent(oSource).id.Clone(self);
  code := TcdaServiceEvent(oSource).code.Clone(self);
  effectiveTime := TcdaServiceEvent(oSource).effectiveTime.Clone(self);
  performer := TcdaServiceEvent(oSource).performer.Clone(self);
  FclassCode := TcdaServiceEvent(oSource).FclassCode;
end;

procedure TcdaServiceEvent.DoClear;
begin
  inherited;
  id.Clear;
  code := Nil;
  effectiveTime := Nil;
  performer.Clear;
  FclassCode := 'ACT';
end;

function TcdaServiceEvent.Clone(parent : Tv3Base): TcdaServiceEvent;
begin
  Result := TcdaServiceEvent(inherited Clone(parent));
end;

constructor TcdaServiceEvent.Create;
begin
  inherited;
  FclassCode := 'ACT';
  FmoodCode := 'EVN';
  id := Tv3ListII.Create(self);
  performer := TcdaPerformer1List.Create(self);
end;

destructor TcdaServiceEvent.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  FeffectiveTime.Free;
  Fperformer.Free;
  inherited;
end;



function TcdaServiceEvent.Link: TcdaServiceEvent;
begin
  Result := TcdaServiceEvent(Inherited Link);
end;

procedure TcdaServiceEvent.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer1')); // TcdaPerformer1List;
end;

procedure TcdaServiceEvent.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

procedure TcdaServiceEvent.SeteffectiveTime(const Value: Tv3IVL);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then
    FeffectiveTime.Parent := self;
end;

procedure TcdaServiceEvent.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.Parent := self;
end;

Function TcdaServiceEvent.Getperformer : TcdaPerformer1List;
begin
  if Fperformer = nil Then
    Fperformer := TcdaPerformer1List.Create(self);
  result := Fperformer;
end;

procedure TcdaServiceEvent.Setperformer(const Value: TcdaPerformer1List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then
    Fperformer.Parent := self;
end;

procedure TcdaServiceEvent.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'performer' Then
    performer := TcdaPerformer1List(aValue.AsType(TcdaPerformer1List)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

function TcdaServiceEvent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fperformer.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, Fcode.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
  inc(result, Fid.sizeInBytes);
end;

{ TcdaSpecimen }

Function TcdaSpecimen.CDAClassNameV : String;
Begin
  Result := 'Specimen';
End;

function TcdaSpecimen.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaSpecimen.Assign(oSource: TFslObject);
begin
  inherited;
  specimenRole := TcdaSpecimen(oSource).specimenRole.Clone(self);
end;

procedure TcdaSpecimen.DoClear;
begin
  inherited;
  specimenRole := Nil;
end;

function TcdaSpecimen.Clone(parent : Tv3Base): TcdaSpecimen;
begin
  Result := TcdaSpecimen(inherited Clone(parent));
end;

constructor TcdaSpecimen.Create;
begin
  inherited;
  FtypeCode := 'SPC';
end;

destructor TcdaSpecimen.Destroy;
begin
  FspecimenRole.Free;
  inherited;
end;

function TcdaSpecimen.Link: TcdaSpecimen;
begin
  Result := TcdaSpecimen(Inherited Link);
end;

procedure TcdaSpecimen.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimenRole', FspecimenRole, 'SpecimenRole')); // TcdaSpecimenRole;
end;

procedure TcdaSpecimen.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'specimenRole' Then
    specimenRole := TcdaSpecimenRole(aValue.AsType(TcdaSpecimenRole)).Clone(self)
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaSpecimen.SetspecimenRole(const Value: TcdaSpecimenRole);
begin
  FspecimenRole.Free;
  FspecimenRole := Value;
  if FspecimenRole <> nil then
    FspecimenRole.Parent := self;
end;

function TcdaSpecimen.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FspecimenRole.sizeInBytes);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaSpecimenRole }

Function TcdaSpecimenRole.CDAClassNameV : String;
Begin
  Result := 'SpecimenRole';
End;

function TcdaSpecimenRole.CDAClassTypeV: TCDAClassType;
begin
  result := etRole;
end;

procedure TcdaSpecimenRole.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaSpecimenRole(oSource).id.Clone(self);
  specimenPlayingEntity := TcdaSpecimenRole(oSource).specimenPlayingEntity.Clone(self);
end;

procedure TcdaSpecimenRole.DoClear;
begin
  inherited;
  id.Clear;
  specimenPlayingEntity := Nil;
end;

function TcdaSpecimenRole.Clone(parent : Tv3Base): TcdaSpecimenRole;
begin
  Result := TcdaSpecimenRole(inherited Clone(parent));
end;

constructor TcdaSpecimenRole.Create;
begin
  inherited;
  FclassCode := 'SPEC';
  id := Tv3ListII.Create(self);
end;

destructor TcdaSpecimenRole.Destroy;
begin
  Fid.Free;
  FspecimenPlayingEntity.Free;
  inherited;
end;


function TcdaSpecimenRole.Link: TcdaSpecimenRole;
begin
  Result := TcdaSpecimenRole(Inherited Link);
end;

procedure TcdaSpecimenRole.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimenPlayingEntity', FspecimenPlayingEntity, 'PlayingEntity')); // TcdaPlayingEntity;
end;

procedure TcdaSpecimenRole.Setid(const Value: Tv3ListII);
begin
  Fid.Free;
  Fid := Value;
  if Fid <> nil then
    Fid.Parent := self;
end;

procedure TcdaSpecimenRole.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'specimenPlayingEntity' Then
    specimenPlayingEntity := TcdaPlayingEntity(aValue.AsType(TcdaPlayingEntity)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaSpecimenRole.SetspecimenPlayingEntity(const Value: TcdaPlayingEntity);
begin
  FspecimenPlayingEntity.Free;
  FspecimenPlayingEntity := Value;
  if FspecimenPlayingEntity <> nil then
    FspecimenPlayingEntity.Parent := self;
end;

function TcdaSpecimenRole.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FspecimenPlayingEntity.sizeInBytes);
  inc(result, Fid.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
end;

{ TcdaStructuredBody }

Function TcdaStructuredBody.CDAClassNameV : String;
Begin
  Result := 'StructuredBody';
End;

function TcdaStructuredBody.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaStructuredBody.Assign(oSource: TFslObject);
begin
  inherited;
  confidentialityCode := TcdaStructuredBody(oSource).confidentialityCode.Clone(self);
  languageCode := TcdaStructuredBody(oSource).languageCode.Clone(self);
  component := TcdaStructuredBody(oSource).component.Clone(self);
end;

procedure TcdaStructuredBody.DoClear;
begin
  inherited;
  confidentialityCode := Nil;
  languageCode := Nil;
  component.Clear;
end;

function TcdaStructuredBody.Clone(parent : Tv3Base): TcdaStructuredBody;
begin
  Result := TcdaStructuredBody(inherited Clone(parent));
end;

constructor TcdaStructuredBody.Create;
begin
  inherited;
  FclassCode := 'DOCBODY';
  FmoodCode := 'EVN';
  component := TcdaComponentSectList.Create(self);
end;

destructor TcdaStructuredBody.Destroy;
begin
  FconfidentialityCode.Free;
  FlanguageCode.Free;
  Fcomponent.Free;
  inherited;
end;


function TcdaStructuredBody.Link: TcdaStructuredBody;
begin
  Result := TcdaStructuredBody(Inherited Link);
end;

procedure TcdaStructuredBody.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'moodCode', FmoodCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'confidentialityCode', FconfidentialityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'languageCode', FlanguageCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'component', Fcomponent, rmpctList, 'ComponentSect')); // TcdaComponentSectList;
end;

Function TcdaStructuredBody.Getcomponent : TcdaComponentSectList;
begin
  if Fcomponent = nil Then
    Fcomponent := TcdaComponentSectList.Create(self);
  result := Fcomponent;
end;

procedure TcdaStructuredBody.Setcomponent(const Value: TcdaComponentSectList);
begin
  Fcomponent.Free;
  Fcomponent := Value;
  if Fcomponent <> nil then
    Fcomponent.Parent := self;
end;

procedure TcdaStructuredBody.SetconfidentialityCode(const Value: Tv3CD);
begin
  FconfidentialityCode.Free;
  FconfidentialityCode := Value;
  if FconfidentialityCode <> nil then
    FconfidentialityCode.Parent := self;
end;

procedure TcdaStructuredBody.SetlanguageCode(const Value: Tv3CS);
begin
  FlanguageCode.Free;
  FlanguageCode := Value;
  if FlanguageCode <> nil then
    FlanguageCode.Parent := self;
end;

procedure TcdaStructuredBody.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'component' Then
    component := TcdaComponentSectList(aValue.AsType(TcdaComponentSectList)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'moodCode' Then
    FmoodCode := aValue.AsString
  else if aValue.Name = 'confidentialityCode' Then
    confidentialityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'languageCode' Then
    languageCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else
    inherited;
end;

function TcdaStructuredBody.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcomponent.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FmoodCode.length * sizeof(char)) + 12);
  inc(result, FconfidentialityCode.sizeInBytes);
  inc(result, FlanguageCode.sizeInBytes);
end;

{ TcdaSubject }

Function TcdaSubject.CDAClassNameV : String;
Begin
  Result := 'Subject';
End;

function TcdaSubject.CDAClassTypeV: TCDAClassType;
begin
  result := etParticipation;
end;

procedure TcdaSubject.Assign(oSource: TFslObject);
begin
  inherited;
  awarenessCode := TcdaSubject(oSource).awarenessCode.Clone(self);
  relatedSubject := TcdaSubject(oSource).relatedSubject.Clone(self);
end;

procedure TcdaSubject.DoClear;
begin
  inherited;
  awarenessCode := Nil;
  relatedSubject := Nil;
end;

function TcdaSubject.Clone(parent : Tv3Base): TcdaSubject;
begin
  Result := TcdaSubject(inherited Clone(parent));
end;

constructor TcdaSubject.Create;
begin
  inherited;
  FtypeCode := 'SBJ';
  FcontextControlCode := 'OP';
end;

destructor TcdaSubject.Destroy;
begin
  FawarenessCode.Free;
  FrelatedSubject.Free;
  inherited;
end;

function TcdaSubject.Link: TcdaSubject;
begin
  Result := TcdaSubject(Inherited Link);
end;

procedure TcdaSubject.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'typeCode', FtypeCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'contextControlCode', FcontextControlCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'awarenessCode', FawarenessCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'relatedSubject', FrelatedSubject, 'RelatedSubject')); // TcdaRelatedSubject;
end;

procedure TcdaSubject.SetawarenessCode(const Value: Tv3CD);
begin
  FawarenessCode.Free;
  FawarenessCode := Value;
  if FawarenessCode <> nil then
    FawarenessCode.Parent := self;
end;

procedure TcdaSubject.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'relatedSubject' Then
    relatedSubject := TcdaRelatedSubject(aValue.AsType(TcdaRelatedSubject)).Clone(self)
  else if aValue.Name = 'awarenessCode' Then
    awarenessCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'contextControlCode' Then
    FcontextControlCode := aValue.AsString
  else if aValue.Name = 'typeCode' Then
    FtypeCode := aValue.AsString
  else
    inherited;
end;

procedure TcdaSubject.SetrelatedSubject(const Value: TcdaRelatedSubject);
begin
  FrelatedSubject.Free;
  FrelatedSubject := Value;
  if FrelatedSubject <> nil then
    FrelatedSubject.Parent := self;
end;

function TcdaSubject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FrelatedSubject.sizeInBytes);
  inc(result, FawarenessCode.sizeInBytes);
  inc(result, (FcontextControlCode.length * sizeof(char)) + 12);
  inc(result, (FtypeCode.length * sizeof(char)) + 12);
end;

{ TcdaSubjectPerson }

Function TcdaSubjectPerson.CDAClassNameV : String;
Begin
  Result := 'SubjectPerson';
End;

function TcdaSubjectPerson.CDAClassTypeV: TCDAClassType;
begin
  result := etEntity;
end;

procedure TcdaSubjectPerson.Assign(oSource: TFslObject);
begin
  inherited;
  name := TcdaSubjectPerson(oSource).name.Clone(self);
  administrativeGenderCode := TcdaSubjectPerson(oSource).administrativeGenderCode.Clone(self);
  birthTime := TcdaSubjectPerson(oSource).birthTime.Clone(self);
end;

procedure TcdaSubjectPerson.DoClear;
begin
  inherited;
  name.Clear;
  administrativeGenderCode := Nil;
  birthTime := Nil;
end;

function TcdaSubjectPerson.Clone(parent : Tv3Base): TcdaSubjectPerson;
begin
  Result := TcdaSubjectPerson(inherited Clone(parent));
end;

constructor TcdaSubjectPerson.Create;
begin
  inherited;
  FclassCode := 'PSN';
  FdeterminerCode := 'INSTANCE';
  name := Tv3ListEN.Create(self);
end;

destructor TcdaSubjectPerson.Destroy;
begin
  Fname.Free;
  FadministrativeGenderCode.Free;
  FbirthTime.Free;
  inherited;
end;


function TcdaSubjectPerson.Link: TcdaSubjectPerson;
begin
  Result := TcdaSubjectPerson(Inherited Link);
end;

procedure TcdaSubjectPerson.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'classCode', FclassCode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'determinerCode', FdeterminerCode));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, rmpctList, 'EN'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'birthTime', FbirthTime, 'TS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'administrativeGenderCode', FadministrativeGenderCode, 'CD'));
end;

procedure TcdaSubjectPerson.SetadministrativeGenderCode(const Value: Tv3CD);
begin
  FadministrativeGenderCode.Free;
  FadministrativeGenderCode := Value;
  if FadministrativeGenderCode <> nil then
    FadministrativeGenderCode.Parent := self;
end;

procedure TcdaSubjectPerson.SetbirthTime(const Value: Tv3TS);
begin
  FbirthTime.Free;
  FbirthTime := Value;
  if FbirthTime <> nil then
    FbirthTime.Parent := self;
end;

procedure TcdaSubjectPerson.Setname(const Value: Tv3ListEN);
begin
  Fname.Free;
  Fname := Value;
  if Fname <> nil then
    Fname.Parent := self;
end;

procedure TcdaSubjectPerson.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'administrativeGenderCode' Then
    administrativeGenderCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'classCode' Then
    FclassCode := aValue.AsString
  else if aValue.Name = 'determinerCode' Then
    FdeterminerCode := aValue.AsString
  else if aValue.Name = 'name' Then
    name := Tv3ListEN(aValue.AsType(Tv3ListEN)).Clone(self)
  else if aValue.Name = 'birthTime' Then
    birthTime := Tv3TS(aValue.AsType(Tv3TS)).Clone(self)
  else
    inherited;
end;

function TcdaSubjectPerson.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FadministrativeGenderCode.sizeInBytes);
  inc(result, (FclassCode.length * sizeof(char)) + 12);
  inc(result, (FdeterminerCode.length * sizeof(char)) + 12);
  inc(result, Fname.sizeInBytes);
  inc(result, FbirthTime.sizeInBytes);
end;

{ TcdaSubstanceAdministration }

Function TcdaSubstanceAdministration.CDAClassNameV : String;
Begin
  Result := 'SubstanceAdministration';
End;

function TcdaSubstanceAdministration.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaSubstanceAdministration.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaSubstanceAdministration(oSource).id.Clone(self);
  code := TcdaSubstanceAdministration(oSource).code.Clone(self);
  text := TcdaSubstanceAdministration(oSource).text.Clone(self);
  statusCode := TcdaSubstanceAdministration(oSource).statusCode.Clone(self);
  effectiveTime := TcdaSubstanceAdministration(oSource).effectiveTime.Clone(self);
  priorityCode := TcdaSubstanceAdministration(oSource).priorityCode.Clone(self);
  repeatNumber := TcdaSubstanceAdministration(oSource).repeatNumber.Clone(self);
  routeCode := TcdaSubstanceAdministration(oSource).routeCode.Clone(self);
  approachSiteCode := TcdaSubstanceAdministration(oSource).approachSiteCode.Clone(self);
  doseQuantity := TcdaSubstanceAdministration(oSource).doseQuantity.Clone(self);
  rateQuantity := TcdaSubstanceAdministration(oSource).rateQuantity.Clone(self);
  maxDoseQuantity := TcdaSubstanceAdministration(oSource).maxDoseQuantity.Clone(self);
  administrationUnitCode := TcdaSubstanceAdministration(oSource).administrationUnitCode.Clone(self);
  subject := TcdaSubstanceAdministration(oSource).subject.Clone(self);
  specimen := TcdaSubstanceAdministration(oSource).specimen.Clone(self);
  consumable := TcdaSubstanceAdministration(oSource).consumable.Clone(self);
  performer := TcdaSubstanceAdministration(oSource).performer.Clone(self);
  author := TcdaSubstanceAdministration(oSource).author.Clone(self);
  informant := TcdaSubstanceAdministration(oSource).informant.Clone(self);
  participant := TcdaSubstanceAdministration(oSource).participant.Clone(self);
  entryRelationship := TcdaSubstanceAdministration(oSource).entryRelationship.Clone(self);
  reference := TcdaSubstanceAdministration(oSource).reference.Clone(self);
  precondition := TcdaSubstanceAdministration(oSource).precondition.Clone(self);
  negationInd := TcdaSubstanceAdministration(oSource).negationInd;
end;

procedure TcdaSubstanceAdministration.DoClear;
begin
  inherited;
  code := Nil;
  text := Nil;
  statusCode := Nil;
  effectiveTime := Nil;
  priorityCode := Nil;
  repeatNumber := Nil;
  routeCode := Nil;
  doseQuantity := Nil;
  rateQuantity := Nil;
  maxDoseQuantity := Nil;
  administrationUnitCode := Nil;
  subject := Nil;
  consumable := Nil;
  negationInd := False;
  approachSiteCode.Clear;
  author.Clear;
  entryRelationship.Clear;
  id.Clear;
  informant.Clear;
  participant.Clear;
  performer.Clear;
  precondition.Clear;
  reference.Clear;
  specimen.Clear;
end;

function TcdaSubstanceAdministration.Clone(parent : Tv3Base): TcdaSubstanceAdministration;
begin
  Result := TcdaSubstanceAdministration(inherited Clone(parent));
end;

constructor TcdaSubstanceAdministration.Create;
begin
  inherited;
  FclassCode := 'SBADM';
  approachSiteCode := Tv3ListCD.Create(self);
  author := TcdaAuthorList.Create(self);
  entryRelationship := TcdaEntryRelationshipList.Create(self);
  id := Tv3ListII.Create(self);
  informant := TcdaInformant12List.Create(self);
  participant := TcdaParticipant2List.Create(self);
  performer := TcdaPerformer2List.Create(self);
  precondition := TcdaPreconditionList.Create(self);
  reference := TcdaReferenceList.Create(self);
  specimen := TcdaSpecimenList.Create(self);
end;

destructor TcdaSubstanceAdministration.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Ftext.Free;
  FstatusCode.Free;
  FeffectiveTime.Free;
  FpriorityCode.Free;
  FrepeatNumber.Free;
  FrouteCode.Free;
  FapproachSiteCode.Free;
  FdoseQuantity.Free;
  FrateQuantity.Free;
  FmaxDoseQuantity.Free;
  FadministrationUnitCode.Free;
  Fsubject.Free;
  Fspecimen.Free;
  Fconsumable.Free;
  Fperformer.Free;
  Fauthor.Free;
  Finformant.Free;
  Fparticipant.Free;
  FentryRelationship.Free;
  Freference.Free;
  Fprecondition.Free;
  inherited;
end;


function TcdaSubstanceAdministration.Link: TcdaSubstanceAdministration;
begin
  Result := TcdaSubstanceAdministration(Inherited Link);
end;

procedure TcdaSubstanceAdministration.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'negationInd', FHasNegationInd, FnegationInd));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'priorityCode', FpriorityCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'routeCode', FrouteCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'administrationUnitCode', FadministrationUnitCode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'statusCode', FstatusCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'repeatNumber', FrepeatNumber, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'doseQuantity', FdoseQuantity, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'rateQuantity', FrateQuantity, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'approachSiteCode', FapproachSiteCode, rmpctList, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'QSET'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'maxDoseQuantity', FmaxDoseQuantity, 'RTO'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'consumable', Fconsumable, 'Consumable')); // TcdaConsumable;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entryRelationship', FentryRelationship, rmpctList, 'EntryRelationship')); // TcdaEntryRelationshipList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant2')); // TcdaParticipant2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer2')); // TcdaPerformer2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'precondition', Fprecondition, rmpctList, 'Precondition')); // TcdaPreconditionList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, rmpctList, 'Reference')); // TcdaReferenceList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimen', Fspecimen, rmpctList, 'Specimen')); // TcdaSpecimenList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

procedure TcdaSubstanceAdministration.SetadministrationUnitCode(const Value: Tv3CD);
begin
  FadministrationUnitCode.Free;
  FadministrationUnitCode := Value;
  if FadministrationUnitCode <> nil then
    FadministrationUnitCode.Parent := self;
end;

procedure TcdaSubstanceAdministration.SetapproachSiteCode(const Value: Tv3ListCD);
begin
  FapproachSiteCode.Free;
  FapproachSiteCode := Value;
  if FapproachSiteCode <> nil then
    FapproachSiteCode.Parent := self;
end;

Function TcdaSubstanceAdministration.Getauthor : TcdaAuthorList;
begin
  if Fauthor = nil Then
    Fauthor := TcdaAuthorList.Create(self);
  result := Fauthor;
end;

procedure TcdaSubstanceAdministration.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then
    Fauthor.Parent := self;
end;

procedure TcdaSubstanceAdministration.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

procedure TcdaSubstanceAdministration.Setconsumable(const Value: TcdaConsumable);
begin
  Fconsumable.Free;
  Fconsumable := Value;
  if Fconsumable <> nil then
    Fconsumable.Parent := self;
end;

procedure TcdaSubstanceAdministration.SetdoseQuantity(const Value: Tv3IVL);
begin
  FdoseQuantity.Free;
  FdoseQuantity := Value;
  if FdoseQuantity <> nil then
    FdoseQuantity.Parent := self;
end;

procedure TcdaSubstanceAdministration.SeteffectiveTime(const Value: Tv3QSET);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then
    FeffectiveTime.Parent := self;
end;

Function TcdaSubstanceAdministration.GetentryRelationship : TcdaEntryRelationshipList;
begin
  if FentryRelationship = nil Then
    FentryRelationship := TcdaEntryRelationshipList.Create(self);
  result := FentryRelationship;
end;

procedure TcdaSubstanceAdministration.SetentryRelationship(const Value: TcdaEntryRelationshipList);
begin
  FentryRelationship.Free;
  FentryRelationship := Value;
  if FentryRelationship <> nil then
    FentryRelationship.Parent := self;
end;


Function TcdaSubstanceAdministration.Getinformant : TcdaInformant12List;
begin
  if Finformant = nil Then
    Finformant := TcdaInformant12List.Create(self);
  result := Finformant;
end;

procedure TcdaSubstanceAdministration.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then
    Finformant.Parent := self;
end;

procedure TcdaSubstanceAdministration.SetmaxDoseQuantity(const Value: Tv3RTO);
begin
  FmaxDoseQuantity.Free;
  FmaxDoseQuantity := Value;
  if FmaxDoseQuantity <> nil then
    FmaxDoseQuantity.Parent := self;
end;

procedure TcdaSubstanceAdministration.SetnegationInd(const Value: boolean);
begin
  FHasNegationInd := True;
  FnegationInd := Value;
end;

Function TcdaSubstanceAdministration.Getparticipant : TcdaParticipant2List;
begin
  if Fparticipant = nil Then
    Fparticipant := TcdaParticipant2List.Create(self);
  result := Fparticipant;
end;

procedure TcdaSubstanceAdministration.Setparticipant(const Value: TcdaParticipant2List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then
    Fparticipant.Parent := self;
end;

Function TcdaSubstanceAdministration.Getperformer : TcdaPerformer2List;
begin
  if Fperformer = nil Then
    Fperformer := TcdaPerformer2List.Create(self);
  result := Fperformer;
end;

procedure TcdaSubstanceAdministration.Setperformer(const Value: TcdaPerformer2List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then
    Fperformer.Parent := self;
end;

Function TcdaSubstanceAdministration.Getprecondition : TcdaPreconditionList;
begin
  if Fprecondition = nil Then
    Fprecondition := TcdaPreconditionList.Create(self);
  result := Fprecondition;
end;

procedure TcdaSubstanceAdministration.Setprecondition(const Value: TcdaPreconditionList);
begin
  Fprecondition.Free;
  Fprecondition := Value;
  if Fprecondition <> nil then
    Fprecondition.Parent := self;
end;

procedure TcdaSubstanceAdministration.SetpriorityCode(const Value: Tv3CD);
begin
  FpriorityCode.Free;
  FpriorityCode := Value;
  if FpriorityCode <> nil then
    FpriorityCode.Parent := self;
end;

procedure TcdaSubstanceAdministration.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'consumable' Then
    consumable := TcdaConsumable(aValue.AsType(TcdaConsumable)).Clone(self)
  else if aValue.Name = 'entryRelationship' Then
    entryRelationship := TcdaEntryRelationshipList(aValue.AsType(TcdaEntryRelationshipList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant2List(aValue.AsType(TcdaParticipant2List)).Clone(self)
  else if aValue.Name = 'performer' Then
    performer := TcdaPerformer2List(aValue.AsType(TcdaPerformer2List)).Clone(self)
  else if aValue.Name = 'precondition' Then
    precondition := TcdaPreconditionList(aValue.AsType(TcdaPreconditionList)).Clone(self)
  else if aValue.Name = 'reference' Then
    reference := TcdaReferenceList(aValue.AsType(TcdaReferenceList)).Clone(self)
  else if aValue.Name = 'specimen' Then
    specimen := TcdaSpecimenList(aValue.AsType(TcdaSpecimenList)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'negationInd' Then
    aValue.AsBool(FHasNegationInd, FnegationInd)
  else if aValue.Name = 'priorityCode' Then
    priorityCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'routeCode' Then
    routeCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'administrationUnitCode' Then
    administrationUnitCode := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'statusCode' Then
    statusCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'repeatNumber' Then
    repeatNumber := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'doseQuantity' Then
    doseQuantity := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'rateQuantity' Then
    rateQuantity := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'approachSiteCode' Then
    approachSiteCode := Tv3ListCD(aValue.AsType(Tv3ListCD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3QSET(aValue.AsType(Tv3QSET)).Clone(self)
  else if aValue.Name = 'maxDoseQuantity' Then
    maxDoseQuantity := Tv3RTO(aValue.AsType(Tv3RTO)).Clone(self)
  else
    inherited;
end;

procedure TcdaSubstanceAdministration.SetrateQuantity(const Value: Tv3IVL);
begin
  FrateQuantity.Free;
  FrateQuantity := Value;
  if FrateQuantity <> nil then
    FrateQuantity.Parent := self;
end;

Function TcdaSubstanceAdministration.Getreference : TcdaReferenceList;
begin
  if Freference = nil Then
    Freference := TcdaReferenceList.Create(self);
  result := Freference;
end;

procedure TcdaSubstanceAdministration.Setreference(const Value: TcdaReferenceList);
begin
  Freference.Free;
  Freference := Value;
  if Freference <> nil then
    Freference.Parent := self;
end;

procedure TcdaSubstanceAdministration.SetrepeatNumber(const Value: Tv3IVL);
begin
  FrepeatNumber.Free;
  FrepeatNumber := Value;
  if FrepeatNumber <> nil then
    FrepeatNumber.Parent := self;
end;

procedure TcdaSubstanceAdministration.SetrouteCode(const Value: Tv3CD);
begin
  FrouteCode.Free;
  FrouteCode := Value;
  if FrouteCode <> nil then
    FrouteCode.Parent := self;
end;

Function TcdaSubstanceAdministration.Getspecimen : TcdaSpecimenList;
begin
  if Fspecimen = nil Then
    Fspecimen := TcdaSpecimenList.Create(self);
  result := Fspecimen;
end;

procedure TcdaSubstanceAdministration.Setspecimen(const Value: TcdaSpecimenList);
begin
  Fspecimen.Free;
  Fspecimen := Value;
  if Fspecimen <> nil then
    Fspecimen.Parent := self;
end;

procedure TcdaSubstanceAdministration.SetstatusCode(const Value: Tv3CS);
begin
  FstatusCode.Free;
  FstatusCode := Value;
  if FstatusCode <> nil then
    FstatusCode.Parent := self;
end;

procedure TcdaSubstanceAdministration.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then
    Fsubject.Parent := self;
end;

procedure TcdaSubstanceAdministration.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then
    Ftext.Parent := self;
end;

function TcdaSubstanceAdministration.ClassCodeIsFixed: Boolean;
begin
  result := true;
end;

function TcdaSubstanceAdministration.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, Fconsumable.sizeInBytes);
  inc(result, FentryRelationship.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, Fperformer.sizeInBytes);
  inc(result, Fprecondition.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, Fspecimen.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, FpriorityCode.sizeInBytes);
  inc(result, FrouteCode.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, FadministrationUnitCode.sizeInBytes);
  inc(result, FstatusCode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
  inc(result, FrepeatNumber.sizeInBytes);
  inc(result, FdoseQuantity.sizeInBytes);
  inc(result, FrateQuantity.sizeInBytes);
  inc(result, FapproachSiteCode.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
  inc(result, FmaxDoseQuantity.sizeInBytes);
end;

{ TcdaSupply }

Function TcdaSupply.CDAClassNameV : String;
Begin
  Result := 'Supply';
End;

function TcdaSupply.CDAClassTypeV: TCDAClassType;
begin
  result := etAct;
end;

procedure TcdaSupply.Assign(oSource: TFslObject);
begin
  inherited;
  id := TcdaSupply(oSource).id.Clone(self);
  code := TcdaSupply(oSource).code.Clone(self);
  text := TcdaSupply(oSource).text.Clone(self);
  statusCode := TcdaSupply(oSource).statusCode.Clone(self);
  effectiveTime := TcdaSupply(oSource).effectiveTime.Clone(self);
  priorityCode := TcdaSupply(oSource).priorityCode.Clone(self);
  repeatNumber := TcdaSupply(oSource).repeatNumber.Clone(self);
  independentInd := TcdaSupply(oSource).independentInd.Clone(self);
  quantity := TcdaSupply(oSource).quantity.Clone(self);
  expectedUseTime := TcdaSupply(oSource).expectedUseTime.Clone(self);
  subject := TcdaSupply(oSource).subject.Clone(self);
  specimen := TcdaSupply(oSource).specimen.Clone(self);
  product := TcdaSupply(oSource).product.Clone(self);
  performer := TcdaSupply(oSource).performer.Clone(self);
  author := TcdaSupply(oSource).author.Clone(self);
  informant := TcdaSupply(oSource).informant.Clone(self);
  participant := TcdaSupply(oSource).participant.Clone(self);
  entryRelationship := TcdaSupply(oSource).entryRelationship.Clone(self);
  reference := TcdaSupply(oSource).reference.Clone(self);
  precondition := TcdaSupply(oSource).precondition.Clone(self);
end;

procedure TcdaSupply.DoClear;
begin
  inherited;
  code := Nil;
  text := Nil;
  statusCode := Nil;
  effectiveTime := Nil;
  priorityCode := Nil;
  repeatNumber := Nil;
  independentInd := Nil;
  quantity := Nil;
  expectedUseTime := Nil;
  subject := Nil;
  product := Nil;

  author.Clear;
  entryRelationship.Clear;
  id.Clear;
  informant.Clear;
  participant.Clear;
  performer.Clear;
  precondition.Clear;
  priorityCode.Clear;
  reference.Clear;
  specimen.Clear;
end;

function TcdaSupply.Clone(parent : Tv3Base): TcdaSupply;
begin
  Result := TcdaSupply(inherited Clone(parent));
end;

constructor TcdaSupply.Create;
begin
  inherited;
  FclassCode := 'SPLY';
  author := TcdaAuthorList.Create(self);
  entryRelationship := TcdaEntryRelationshipList.Create(self);
  id := Tv3ListII.Create(self);
  informant := TcdaInformant12List.Create(self);
  participant := TcdaParticipant2List.Create(self);
  performer := TcdaPerformer2List.Create(self);
  precondition := TcdaPreconditionList.Create(self);
  priorityCode := Tv3ListCD.Create(self);
  reference := TcdaReferenceList.Create(self);
  specimen := TcdaSpecimenList.Create(self);
end;

destructor TcdaSupply.Destroy;
begin
  Fid.Free;
  Fcode.Free;
  Ftext.Free;
  FstatusCode.Free;
  FeffectiveTime.Free;
  FpriorityCode.Free;
  FrepeatNumber.Free;
  FindependentInd.Free;
  Fquantity.Free;
  FexpectedUseTime.Free;
  Fsubject.Free;
  Fspecimen.Free;
  Fproduct.Free;
  Fperformer.Free;
  Fauthor.Free;
  Finformant.Free;
  Fparticipant.Free;
  FentryRelationship.Free;
  Freference.Free;
  Fprecondition.Free;
  inherited;
end;


function TcdaSupply.Link: TcdaSupply;
begin
  Result := TcdaSupply(Inherited Link);
end;

procedure TcdaSupply.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'independentInd', FindependentInd, 'BL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'statusCode', FstatusCode, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'text', Ftext, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'expectedUseTime', FexpectedUseTime, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'repeatNumber', FrepeatNumber, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'priorityCode', FpriorityCode, rmpctList, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'id', Fid, rmpctList, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'quantity', Fquantity, 'PQ'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'effectiveTime', FeffectiveTime, 'QSET'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'author', Fauthor, rmpctList, 'Author')); // TcdaAuthorList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'entryRelationship', FentryRelationship, rmpctList, 'EntryRelationship')); // TcdaEntryRelationshipList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'informant', Finformant, rmpctList, 'Informant12')); // TcdaInformant12List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'participant', Fparticipant, rmpctList, 'Participant2')); // TcdaParticipant2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'performer', Fperformer, rmpctList, 'Performer2')); // TcdaPerformer2List;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'precondition', Fprecondition, rmpctList, 'Precondition')); // TcdaPreconditionList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'product', Fproduct, 'Product')); // TcdaProduct;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, rmpctList, 'Reference')); // TcdaReferenceList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'specimen', Fspecimen, rmpctList, 'Specimen')); // TcdaSpecimenList;
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subject', Fsubject, 'Subject')); // TcdaSubject;
end;

Function TcdaSupply.Getauthor : TcdaAuthorList;
begin
  if Fauthor = nil Then
    Fauthor := TcdaAuthorList.Create(self);
  result := Fauthor;
end;

procedure TcdaSupply.Setauthor(const Value: TcdaAuthorList);
begin
  Fauthor.Free;
  Fauthor := Value;
  if Fauthor <> nil then
    Fauthor.Parent := self;
end;

procedure TcdaSupply.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then
    Fcode.Parent := self;
end;

procedure TcdaSupply.SeteffectiveTime(const Value: Tv3QSET);
begin
  FeffectiveTime.Free;
  FeffectiveTime := Value;
  if FeffectiveTime <> nil then
    FeffectiveTime.Parent := self;
end;

Function TcdaSupply.GetentryRelationship : TcdaEntryRelationshipList;
begin
  if FentryRelationship = nil Then
    FentryRelationship := TcdaEntryRelationshipList.Create(self);
  result := FentryRelationship;
end;

procedure TcdaSupply.SetentryRelationship(const Value: TcdaEntryRelationshipList);
begin
  FentryRelationship.Free;
  FentryRelationship := Value;
  if FentryRelationship <> nil then
    FentryRelationship.Parent := self;
end;

procedure TcdaSupply.SetexpectedUseTime(const Value: Tv3IVL);
begin
  FexpectedUseTime.Free;
  FexpectedUseTime := Value;
  if FexpectedUseTime <> nil then
    FexpectedUseTime.Parent := self;
end;


procedure TcdaSupply.SetindependentInd(const Value: Tv3BL);
begin
  FindependentInd.Free;
  FindependentInd := Value;
  if FindependentInd <> nil then
    FindependentInd.Parent := self;
end;

Function TcdaSupply.Getinformant : TcdaInformant12List;
begin
  if Finformant = nil Then
    Finformant := TcdaInformant12List.Create(self);
  result := Finformant;
end;

procedure TcdaSupply.Setinformant(const Value: TcdaInformant12List);
begin
  Finformant.Free;
  Finformant := Value;
  if Finformant <> nil then
    Finformant.Parent := self;
end;

Function TcdaSupply.Getparticipant : TcdaParticipant2List;
begin
  if Fparticipant = nil Then
    Fparticipant := TcdaParticipant2List.Create(self);
  result := Fparticipant;
end;

procedure TcdaSupply.Setparticipant(const Value: TcdaParticipant2List);
begin
  Fparticipant.Free;
  Fparticipant := Value;
  if Fparticipant <> nil then
    Fparticipant.Parent := self;
end;

Function TcdaSupply.Getperformer : TcdaPerformer2List;
begin
  if Fperformer = nil Then
    Fperformer := TcdaPerformer2List.Create(self);
  result := Fperformer;
end;

procedure TcdaSupply.Setperformer(const Value: TcdaPerformer2List);
begin
  Fperformer.Free;
  Fperformer := Value;
  if Fperformer <> nil then
    Fperformer.Parent := self;
end;

Function TcdaSupply.Getprecondition : TcdaPreconditionList;
begin
  if Fprecondition = nil Then
    Fprecondition := TcdaPreconditionList.Create(self);
  result := Fprecondition;
end;

procedure TcdaSupply.Setprecondition(const Value: TcdaPreconditionList);
begin
  Fprecondition.Free;
  Fprecondition := Value;
  if Fprecondition <> nil then
    Fprecondition.Parent := self;
end;

procedure TcdaSupply.SetpriorityCode(const Value: Tv3ListCD);
begin
  FpriorityCode.Free;
  FpriorityCode := Value;
  if FpriorityCode <> nil then
    FpriorityCode.Parent := self;
end;

procedure TcdaSupply.Setproduct(const Value: TcdaProduct);
begin
  Fproduct.Free;
  Fproduct := Value;
  if Fproduct <> nil then
    Fproduct.Parent := self;
end;

procedure TcdaSupply.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'author' Then
    author := TcdaAuthorList(aValue.AsType(TcdaAuthorList)).Clone(self)
  else if aValue.Name = 'entryRelationship' Then
    entryRelationship := TcdaEntryRelationshipList(aValue.AsType(TcdaEntryRelationshipList)).Clone(self)
  else if aValue.Name = 'informant' Then
    informant := TcdaInformant12List(aValue.AsType(TcdaInformant12List)).Clone(self)
  else if aValue.Name = 'participant' Then
    participant := TcdaParticipant2List(aValue.AsType(TcdaParticipant2List)).Clone(self)
  else if aValue.Name = 'performer' Then
    performer := TcdaPerformer2List(aValue.AsType(TcdaPerformer2List)).Clone(self)
  else if aValue.Name = 'precondition' Then
    precondition := TcdaPreconditionList(aValue.AsType(TcdaPreconditionList)).Clone(self)
  else if aValue.Name = 'product' Then
    product := TcdaProduct(aValue.AsType(TcdaProduct)).Clone(self)
  else if aValue.Name = 'reference' Then
    reference := TcdaReferenceList(aValue.AsType(TcdaReferenceList)).Clone(self)
  else if aValue.Name = 'specimen' Then
    specimen := TcdaSpecimenList(aValue.AsType(TcdaSpecimenList)).Clone(self)
  else if aValue.Name = 'subject' Then
    subject := TcdaSubject(aValue.AsType(TcdaSubject)).Clone(self)
  else if aValue.Name = 'independentInd' Then
    independentInd := Tv3BL(aValue.AsType(Tv3BL)).Clone(self)
  else if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Clone(self)
  else if aValue.Name = 'statusCode' Then
    statusCode := Tv3CS(aValue.AsType(Tv3CS)).Clone(self)
  else if aValue.Name = 'text' Then
    text := Tv3ED(aValue.AsType(Tv3ED)).Clone(self)
  else if aValue.Name = 'expectedUseTime' Then
    expectedUseTime := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'repeatNumber' Then
    repeatNumber := Tv3IVL(aValue.AsType(Tv3IVL)).Clone(self)
  else if aValue.Name = 'priorityCode' Then
    priorityCode := Tv3ListCD(aValue.AsType(Tv3ListCD)).Clone(self)
  else if aValue.Name = 'id' Then
    id := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else if aValue.Name = 'quantity' Then
    quantity := Tv3PQ(aValue.AsType(Tv3PQ)).Clone(self)
  else if aValue.Name = 'effectiveTime' Then
    effectiveTime := Tv3QSET(aValue.AsType(Tv3QSET)).Clone(self)
  else
    inherited;
end;

procedure TcdaSupply.Setquantity(const Value: Tv3PQ);
begin
  Fquantity.Free;
  Fquantity := Value;
  if Fquantity <> nil then
    Fquantity.Parent := self;
end;

Function TcdaSupply.Getreference : TcdaReferenceList;
begin
  if Freference = nil Then
    Freference := TcdaReferenceList.Create(self);
  result := Freference;
end;

procedure TcdaSupply.Setreference(const Value: TcdaReferenceList);
begin
  Freference.Free;
  Freference := Value;
  if Freference <> nil then
    Freference.Parent := self;
end;

procedure TcdaSupply.SetrepeatNumber(const Value: Tv3IVL);
begin
  FrepeatNumber.Free;
  FrepeatNumber := Value;
  if FrepeatNumber <> nil then
    FrepeatNumber.Parent := self;
end;

Function TcdaSupply.Getspecimen : TcdaSpecimenList;
begin
  if Fspecimen = nil Then
    Fspecimen := TcdaSpecimenList.Create(self);
  result := Fspecimen;
end;

procedure TcdaSupply.Setspecimen(const Value: TcdaSpecimenList);
begin
  Fspecimen.Free;
  Fspecimen := Value;
  if Fspecimen <> nil then
    Fspecimen.Parent := self;
end;

procedure TcdaSupply.SetstatusCode(const Value: Tv3CS);
begin
  FstatusCode.Free;
  FstatusCode := Value;
  if FstatusCode <> nil then
    FstatusCode.Parent := self;
end;

procedure TcdaSupply.Setsubject(const Value: TcdaSubject);
begin
  Fsubject.Free;
  Fsubject := Value;
  if Fsubject <> nil then
    Fsubject.Parent := self;
end;

procedure TcdaSupply.Settext(const Value: Tv3ED);
begin
  Ftext.Free;
  Ftext := Value;
  if Ftext <> nil then
    Ftext.Parent := self;
end;

function TcdaSupply.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fauthor.sizeInBytes);
  inc(result, FentryRelationship.sizeInBytes);
  inc(result, Finformant.sizeInBytes);
  inc(result, Fparticipant.sizeInBytes);
  inc(result, Fperformer.sizeInBytes);
  inc(result, Fprecondition.sizeInBytes);
  inc(result, Fproduct.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, Fspecimen.sizeInBytes);
  inc(result, Fsubject.sizeInBytes);
  inc(result, FindependentInd.sizeInBytes);
  inc(result, Fcode.sizeInBytes);
  inc(result, FstatusCode.sizeInBytes);
  inc(result, Ftext.sizeInBytes);
  inc(result, FexpectedUseTime.sizeInBytes);
  inc(result, FrepeatNumber.sizeInBytes);
  inc(result, FpriorityCode.sizeInBytes);
  inc(result, Fquantity.sizeInBytes);
  inc(result, FeffectiveTime.sizeInBytes);
end;

{ TcdaBase }

Function TcdaBase.RIMClassNameV : String;
Begin
  Result := RIMType(CDAClassNameV);
End;

Function TcdaBase.CDAClassNameV : String;
Begin
  Result := 'Base';
End;

procedure TcdaBase.Assign(oSource: TFslObject);
begin
  inherited;
  xmlId := TcdaBase(oSource).xmlId;
  realmCode := TcdaBase(oSource).realmCode.Clone(self);
  typeId := TcdaBase(oSource).typeId.Clone(self);
  templateId := TcdaBase(oSource).templateId.Clone(self);
  nullFlavor := TcdaBase(oSource).nullFlavor;
end;

procedure TcdaBase.DoClear;
begin
  inherited;
  xmlId := '';
  realmCode.Clear;
  typeId := nil;
  templateId.Clear;
  nullFlavor := nfNull;
end;

function TcdaBase.Clone(parent : Tv3Base): TcdaBase;
begin
  result := TcdaBase(inherited Clone(parent));
end;

constructor TcdaBase.Create;
begin
  inherited;
  FrealmCode := Tv3ListCS.Create(self);
  FtemplateId := Tv3ListII.Create(self);
end;

destructor TcdaBase.Destroy;
begin
  FrealmCode.Free;
  FtypeId.Free;
  FtemplateId.Free;
  inherited;
end;

function TcdaBase.Link: TcdaBase;
begin
  result := TcdaBase(inherited Link);
end;

procedure TcdaBase.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'xml-id', FxmlId));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, true, 'nullFlavor', Ord(FnullFlavor), CODES_Tv3NullFlavor));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, true, 'typeId', FtypeId, 'II'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, true, 'realmCode', FrealmCode, rmpctList, 'CS'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, true, 'templateId', FtemplateId, rmpctList, 'II'));
end;

procedure TcdaBase.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'xml-id' Then
    FxmlId := aValue.AsString
  else if aValue.Name = 'nullFlavor' Then
    FnullFlavor := Tv3NullFlavor(aValue.AsEnum)
  else if aValue.Name = 'typeId' Then
    typeId := Tv3II(aValue.AsType(Tv3II)).Clone(self)
  else if aValue.Name = 'realmCode' Then
    realmCode := Tv3ListCS(aValue.AsType(Tv3ListCS)).Clone(self)
  else if aValue.Name = 'templateId' Then
    templateId := Tv3ListII(aValue.AsType(Tv3ListII)).Clone(self)
  else
    inherited;
end;

procedure TcdaBase.SetrealmCode(const Value: Tv3ListCS);
begin
  FrealmCode.Free;
  FrealmCode := Value;
  if FrealmCode <> nil then
    FrealmCode.Parent := self;
end;

procedure TcdaBase.SettemplateId(const Value: Tv3ListII);
begin
  FtemplateId.Free;
  FtemplateId := Value;
  if FtemplateId <> nil then
    FtemplateId.Parent := self;
end;

procedure TcdaBase.SettypeId(const Value: Tv3II);
begin
  FtypeId.Free;
  FtypeId := Value;
  if FtypeId <> nil then
    FtypeId.Parent := self;
end;

function TcdaBase.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FxmlId.length * sizeof(char)) + 12);
  inc(result, FtypeId.sizeInBytes);
  inc(result, FrealmCode.sizeInBytes);
  inc(result, FtemplateId.sizeInBytes);
end;

{ TcdaAuthorList }

Function TcdaAuthorList.Link : TcdaAuthorList;
Begin
  Result := TcdaAuthorList(Inherited Link);
End;

Function TcdaAuthorList.Clone(parent : Tv3Base) : TcdaAuthorList;
Begin
  result := TcdaAuthorList(inherited Clone(parent));
End;

Function TcdaAuthorList.GetAuthors(iIndex : Integer) : TcdaAuthor;
Begin
  Result := TcdaAuthor(ObjectByIndex[iIndex]);
End;

Function TcdaAuthorList.Append : TcdaAuthor;
Begin
  Result := TcdaAuthor.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaAuthorList.AddItem(value : TcdaAuthor);
Begin
  Add(value.Link);
End;

Function TcdaAuthorList.IndexOf(value : TcdaAuthor) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaAuthorList.Insert(iIndex : Integer) : TcdaAuthor;
Begin
  Result := TcdaAuthor.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaAuthorList.InsertItem(iIndex : Integer; value : TcdaAuthor);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaAuthorList.Item(iIndex : Integer) : TcdaAuthor;
Begin
  Result := Authors[iIndex];
End;

Procedure TcdaAuthorList.SetItemByIndex(iIndex : Integer; value: TcdaAuthor);
Begin
  Authors[iIndex] := value;
End;

Procedure TcdaAuthorList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaAuthorList.ClearItems;
Begin
  Clear;
End;

{ TcdatryRelationshipList }
Function TcdaEntryRelationshipList.Link : TcdaEntryRelationshipList;
Begin
  Result := TcdaEntryRelationshipList(Inherited Link);
End;

Function TcdaEntryRelationshipList.Clone(parent : Tv3Base) : TcdaEntryRelationshipList;
Begin
  result := TcdaEntryRelationshipList(inherited Clone(parent));
End;

Function TcdaEntryRelationshipList.GetEntryRelationships(iIndex : Integer) : TcdaEntryRelationship;
Begin
  Result := TcdaEntryRelationship(ObjectByIndex[iIndex]);
End;

Function TcdaEntryRelationshipList.Append : TcdaEntryRelationship;
Begin
  Result := TcdaEntryRelationship.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaEntryRelationshipList.AddItem(value : TcdaEntryRelationship);
Begin
  Add(value.Link);
End;

Function TcdaEntryRelationshipList.IndexOf(value : TcdaEntryRelationship) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaEntryRelationshipList.Insert(iIndex : Integer) : TcdaEntryRelationship;
Begin
  Result := TcdaEntryRelationship.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaEntryRelationshipList.InsertItem(iIndex : Integer; value : TcdaEntryRelationship);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaEntryRelationshipList.Item(iIndex : Integer) : TcdaEntryRelationship;
Begin
  Result := EntryRelationships[iIndex];
End;

Procedure TcdaEntryRelationshipList.SetItemByIndex(iIndex : Integer; value: TcdaEntryRelationship);
Begin
  EntryRelationships[iIndex] := value;
End;

Procedure TcdaEntryRelationshipList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaEntryRelationshipList.ClearItems;
Begin
  Clear;
End;

function TcdaAuthorList.Count: Integer;
begin
  result := Inherited Count;
end;

Procedure TcdaAuthorList.InsertObject(iIndex: Integer; oObject : TcdaAuthor);
begin
  Inherited Insert(iIndex, oObject);
end;

procedure TcdaAuthorList.SetAuthors(iIndex: Integer; const Value: TcdaAuthor);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaAuthorList.ItemClass: TFslObjectClass;
begin
  Result := TcdaAuthor;
end;

{ TcdaInformant12List }
Function TcdaInformant12List.Link : TcdaInformant12List;
Begin
  Result := TcdaInformant12List(Inherited Link);
End;

Function TcdaInformant12List.Clone(parent : Tv3Base): TcdaInformant12List;
Begin
  result := TcdaInformant12List(inherited Clone(parent));
End;

Function TcdaInformant12List.GetInformant12s(iIndex : Integer) : TcdaInformant12;
Begin
  Result := TcdaInformant12(ObjectByIndex[iIndex]);
End;

Function TcdaInformant12List.Append : TcdaInformant12;
Begin
  Result := TcdaInformant12.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaInformant12List.AddItem(value : TcdaInformant12);
Begin
  Add(value.Link);
End;

Function TcdaInformant12List.IndexOf(value : TcdaInformant12) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaInformant12List.Insert(iIndex : Integer) : TcdaInformant12;
Begin
  Result := TcdaInformant12.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaInformant12List.InsertItem(iIndex : Integer; value : TcdaInformant12);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaInformant12List.Item(iIndex : Integer) : TcdaInformant12;
Begin
  Result := Informant12s[iIndex];
End;

Procedure TcdaInformant12List.SetItemByIndex(iIndex : Integer; value: TcdaInformant12);
Begin
  Informant12s[iIndex] := value;
End;

Procedure TcdaInformant12List.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaInformant12List.ClearItems;
Begin
  Clear;
End;

function TcdaInformant12List.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaInformant12List.SetInformant12s(iIndex: Integer;
  const Value: TcdaInformant12);
begin

end;

function TcdaInformant12List.ItemClass: TFslObjectClass;
begin
  Result := TcdaInformant12;
end;

{ TcdaParticipant2List }
Function TcdaParticipant2List.Link : TcdaParticipant2List;
Begin
  Result := TcdaParticipant2List(Inherited Link);
End;

Function TcdaParticipant2List.Clone(parent : Tv3Base): TcdaParticipant2List;
Begin
  result := TcdaParticipant2List(inherited Clone(parent));
End;

Function TcdaParticipant2List.GetParticipant2s(iIndex : Integer) : TcdaParticipant2;
Begin
  Result := TcdaParticipant2(ObjectByIndex[iIndex]);
End;

Function TcdaParticipant2List.Append : TcdaParticipant2;
Begin
  Result := TcdaParticipant2.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaParticipant2List.AddItem(value : TcdaParticipant2);
Begin
  Add(value.Link);
End;

Function TcdaParticipant2List.IndexOf(value : TcdaParticipant2) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaParticipant2List.Insert(iIndex : Integer) : TcdaParticipant2;
Begin
  Result := TcdaParticipant2.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaParticipant2List.InsertItem(iIndex : Integer; value : TcdaParticipant2);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaParticipant2List.Item(iIndex : Integer) : TcdaParticipant2;
Begin
  Result := Participant2s[iIndex];
End;

Procedure TcdaParticipant2List.SetItemByIndex(iIndex : Integer; value: TcdaParticipant2);
Begin
  Participant2s[iIndex] := value;
End;

Procedure TcdaParticipant2List.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaParticipant2List.ClearItems;
Begin
  Clear;
End;

function TcdaParticipant2List.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaParticipant2List.InsertObject(iIndex: Integer; oObject: TcdaParticipant2);
begin
  Inherited Insert(iIndex, oObject);
end;

procedure TcdaParticipant2List.SetParticipant2s(iIndex: Integer; const Value: TcdaParticipant2);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaParticipant2List.ItemClass: TFslObjectClass;
begin
  Result := TcdaParticipant2;
end;

{ TcdaPerformer2List }
Function TcdaPerformer2List.Link : TcdaPerformer2List;
Begin
  Result := TcdaPerformer2List(Inherited Link);
End;

Function TcdaPerformer2List.Clone(parent : Tv3Base): TcdaPerformer2List;
Begin
  result := TcdaPerformer2List(inherited Clone(parent));
End;

Function TcdaPerformer2List.GetPerformer2s(iIndex : Integer) : TcdaPerformer2;
Begin
  Result := TcdaPerformer2(ObjectByIndex[iIndex]);
End;

Function TcdaPerformer2List.Append : TcdaPerformer2;
Begin
  Result := TcdaPerformer2.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaPerformer2List.AddItem(value : TcdaPerformer2);
Begin
  Add(value.Link);
End;

Function TcdaPerformer2List.IndexOf(value : TcdaPerformer2) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaPerformer2List.Insert(iIndex : Integer) : TcdaPerformer2;
Begin
  Result := TcdaPerformer2.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaPerformer2List.InsertItem(iIndex : Integer; value : TcdaPerformer2);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaPerformer2List.Item(iIndex : Integer) : TcdaPerformer2;
Begin
  Result := Performer2s[iIndex];
End;

Procedure TcdaPerformer2List.SetItemByIndex(iIndex : Integer; value: TcdaPerformer2);
Begin
  Performer2s[iIndex] := value;
End;

Procedure TcdaPerformer2List.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaPerformer2List.ClearItems;
Begin
  Clear;
End;

function TcdaPerformer2List.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaPerformer2List.SetPerformer2s(iIndex: Integer; const Value: TcdaPerformer2);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaPerformer2List.ItemClass: TFslObjectClass;
begin
  Result := TcdaPerformer2;
end;

{ TcdaPreconditionList }
Function TcdaPreconditionList.Link : TcdaPreconditionList;
Begin
  Result := TcdaPreconditionList(Inherited Link);
End;

Function TcdaPreconditionList.Clone(parent : Tv3Base): TcdaPreconditionList;
Begin
  result := TcdaPreconditionList(inherited Clone(parent));
End;

Function TcdaPreconditionList.GetPreconditions(iIndex : Integer) : TcdaPrecondition;
Begin
  Result := TcdaPrecondition(ObjectByIndex[iIndex]);
End;

Function TcdaPreconditionList.Append : TcdaPrecondition;
Begin
  Result := TcdaPrecondition.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaPreconditionList.AddItem(value : TcdaPrecondition);
Begin
  Add(value.Link);
End;

Function TcdaPreconditionList.IndexOf(value : TcdaPrecondition) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaPreconditionList.Insert(iIndex : Integer) : TcdaPrecondition;
Begin
  Result := TcdaPrecondition.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaPreconditionList.InsertItem(iIndex : Integer; value : TcdaPrecondition);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaPreconditionList.Item(iIndex : Integer) : TcdaPrecondition;
Begin
  Result := Preconditions[iIndex];
End;

Procedure TcdaPreconditionList.SetItemByIndex(iIndex : Integer; value: TcdaPrecondition);
Begin
  Preconditions[iIndex] := value;
End;

Procedure TcdaPreconditionList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaPreconditionList.ClearItems;
Begin
  Clear;
End;

function TcdaPreconditionList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaPreconditionList.SetPreconditions(iIndex: Integer; const Value: TcdaPrecondition);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaPreconditionList.ItemClass: TFslObjectClass;
begin
  Result := TcdaPrecondition;
end;

{ TcdaReferenceList }
Function TcdaReferenceList.Link : TcdaReferenceList;
Begin
  Result := TcdaReferenceList(Inherited Link);
End;

Function TcdaReferenceList.Clone(parent : Tv3Base): TcdaReferenceList;
Begin
  result := TcdaReferenceList(inherited Clone(parent));
End;

Function TcdaReferenceList.GetReferences(iIndex : Integer) : TcdaReference;
Begin
  Result := TcdaReference(ObjectByIndex[iIndex]);
End;

Function TcdaReferenceList.Append : TcdaReference;
Begin
  Result := TcdaReference.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaReferenceList.AddItem(value : TcdaReference);
Begin
  Add(value.Link);
End;

Function TcdaReferenceList.IndexOf(value : TcdaReference) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaReferenceList.Insert(iIndex : Integer) : TcdaReference;
Begin
  Result := TcdaReference.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaReferenceList.InsertItem(iIndex : Integer; value : TcdaReference);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaReferenceList.Item(iIndex : Integer) : TcdaReference;
Begin
  Result := References[iIndex];
End;

Procedure TcdaReferenceList.SetItemByIndex(iIndex : Integer; value: TcdaReference);
Begin
  References[iIndex] := value;
End;

Procedure TcdaReferenceList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaReferenceList.ClearItems;
Begin
  Clear;
End;

function TcdaReferenceList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaReferenceList.SetReferences(iIndex: Integer; const Value: TcdaReference);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaReferenceList.ItemClass: TFslObjectClass;
begin
  Result := TcdaReference;
end;

{ TcdaSpecimenList }
Function TcdaSpecimenList.Link : TcdaSpecimenList;
Begin
  Result := TcdaSpecimenList(Inherited Link);
End;

Function TcdaSpecimenList.Clone(parent : Tv3Base): TcdaSpecimenList;
Begin
  result := TcdaSpecimenList(inherited Clone(parent));
End;

Function TcdaSpecimenList.GetSpecimens(iIndex : Integer) : TcdaSpecimen;
Begin
  Result := TcdaSpecimen(ObjectByIndex[iIndex]);
End;

Function TcdaSpecimenList.Append : TcdaSpecimen;
Begin
  Result := TcdaSpecimen.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaSpecimenList.AddItem(value : TcdaSpecimen);
Begin
  Add(value.Link);
End;

Function TcdaSpecimenList.IndexOf(value : TcdaSpecimen) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaSpecimenList.Insert(iIndex : Integer) : TcdaSpecimen;
Begin
  Result := TcdaSpecimen.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaSpecimenList.InsertItem(iIndex : Integer; value : TcdaSpecimen);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaSpecimenList.Item(iIndex : Integer) : TcdaSpecimen;
Begin
  Result := Specimens[iIndex];
End;

Procedure TcdaSpecimenList.SetItemByIndex(iIndex : Integer; value: TcdaSpecimen);
Begin
  Specimens[iIndex] := value;
End;

Procedure TcdaSpecimenList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaSpecimenList.ClearItems;
Begin
  Clear;
End;

function TcdaSpecimenList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaSpecimenList.SetSpecimens(iIndex: Integer; const Value: TcdaSpecimen);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaSpecimenList.ItemClass: TFslObjectClass;
begin
  Result := TcdaSpecimen;
end;

{ TcdaMaintainedEntityList }
Function TcdaMaintainedEntityList.Link : TcdaMaintainedEntityList;
Begin
  Result := TcdaMaintainedEntityList(Inherited Link);
End;

Function TcdaMaintainedEntityList.Clone(parent : Tv3Base): TcdaMaintainedEntityList;
Begin
  result := TcdaMaintainedEntityList(inherited Clone(parent));
End;

Function TcdaMaintainedEntityList.GetMaintainedEntitys(iIndex : Integer) : TcdaMaintainedEntity;
Begin
  Result := TcdaMaintainedEntity(ObjectByIndex[iIndex]);
End;

Function TcdaMaintainedEntityList.Append : TcdaMaintainedEntity;
Begin
  Result := TcdaMaintainedEntity.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaMaintainedEntityList.AddItem(value : TcdaMaintainedEntity);
Begin
  Add(value.Link);
End;

Function TcdaMaintainedEntityList.IndexOf(value : TcdaMaintainedEntity) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaMaintainedEntityList.Insert(iIndex : Integer) : TcdaMaintainedEntity;
Begin
  Result := TcdaMaintainedEntity.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaMaintainedEntityList.InsertItem(iIndex : Integer; value : TcdaMaintainedEntity);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaMaintainedEntityList.Item(iIndex : Integer) : TcdaMaintainedEntity;
Begin
  Result := MaintainedEntitys[iIndex];
End;

Procedure TcdaMaintainedEntityList.SetItemByIndex(iIndex : Integer; value: TcdaMaintainedEntity);
Begin
  MaintainedEntitys[iIndex] := value;
End;

Procedure TcdaMaintainedEntityList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaMaintainedEntityList.ClearItems;
Begin
  Clear;
End;

function TcdaMaintainedEntityList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaMaintainedEntityList.SetMaintainedEntitys(iIndex: Integer; const Value: TcdaMaintainedEntity);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaMaintainedEntityList.ItemClass: TFslObjectClass;
begin
  Result := TcdaMaintainedEntity;
end;

{ TcdaAuthenticatorList }
Function TcdaAuthenticatorList.Link : TcdaAuthenticatorList;
Begin
  Result := TcdaAuthenticatorList(Inherited Link);
End;

Function TcdaAuthenticatorList.Clone(parent : Tv3Base): TcdaAuthenticatorList;
Begin
  result := TcdaAuthenticatorList(inherited Clone(parent));
End;

Function TcdaAuthenticatorList.GetAuthenticators(iIndex : Integer) : TcdaAuthenticator;
Begin
  Result := TcdaAuthenticator(ObjectByIndex[iIndex]);
End;

Function TcdaAuthenticatorList.Append : TcdaAuthenticator;
Begin
  Result := TcdaAuthenticator.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaAuthenticatorList.AddItem(value : TcdaAuthenticator);
Begin
  Add(value.Link);
End;

Function TcdaAuthenticatorList.IndexOf(value : TcdaAuthenticator) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaAuthenticatorList.Insert(iIndex : Integer) : TcdaAuthenticator;
Begin
  Result := TcdaAuthenticator.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaAuthenticatorList.InsertItem(iIndex : Integer; value : TcdaAuthenticator);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaAuthenticatorList.Item(iIndex : Integer) : TcdaAuthenticator;
Begin
  Result := Authenticators[iIndex];
End;

Procedure TcdaAuthenticatorList.SetItemByIndex(iIndex : Integer; value: TcdaAuthenticator);
Begin
  Authenticators[iIndex] := value;
End;

Procedure TcdaAuthenticatorList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaAuthenticatorList.ClearItems;
Begin
  Clear;
End;

function TcdaAuthenticatorList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaAuthenticatorList.SetAuthenticators(iIndex: Integer; const Value: TcdaAuthenticator);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaAuthenticatorList.ItemClass: TFslObjectClass;
begin
  Result := TcdaAuthenticator;
end;

{ TcdaAuthorizationList }
Function TcdaAuthorizationList.Link : TcdaAuthorizationList;
Begin
  Result := TcdaAuthorizationList(Inherited Link);
End;

Function TcdaAuthorizationList.Clone(parent : Tv3Base): TcdaAuthorizationList;
Begin
  result := TcdaAuthorizationList(inherited Clone(parent));
End;

Function TcdaAuthorizationList.GetAuthorizations(iIndex : Integer) : TcdaAuthorization;
Begin
  Result := TcdaAuthorization(ObjectByIndex[iIndex]);
End;

Function TcdaAuthorizationList.Append : TcdaAuthorization;
Begin
  Result := TcdaAuthorization.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaAuthorizationList.AddItem(value : TcdaAuthorization);
Begin
  Add(value.Link);
End;

Function TcdaAuthorizationList.IndexOf(value : TcdaAuthorization) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaAuthorizationList.Insert(iIndex : Integer) : TcdaAuthorization;
Begin
  Result := TcdaAuthorization.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaAuthorizationList.InsertItem(iIndex : Integer; value : TcdaAuthorization);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaAuthorizationList.Item(iIndex : Integer) : TcdaAuthorization;
Begin
  Result := Authorizations[iIndex];
End;

Procedure TcdaAuthorizationList.SetItemByIndex(iIndex : Integer; value: TcdaAuthorization);
Begin
  Authorizations[iIndex] := value;
End;

Procedure TcdaAuthorizationList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaAuthorizationList.ClearItems;
Begin
  Clear;
End;

function TcdaAuthorizationList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaAuthorizationList.SetAuthorizations(iIndex: Integer; const Value: TcdaAuthorization);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaAuthorizationList.ItemClass: TFslObjectClass;
begin
  Result := TcdaAuthorization;
end;

{ TcdaDocumentationOfList }
Function TcdaDocumentationOfList.Link : TcdaDocumentationOfList;
Begin
  Result := TcdaDocumentationOfList(Inherited Link);
End;

Function TcdaDocumentationOfList.Clone(parent : Tv3Base): TcdaDocumentationOfList;
Begin
  result := TcdaDocumentationOfList(inherited Clone(parent));
End;

Function TcdaDocumentationOfList.GetDocumentationOfs(iIndex : Integer) : TcdaDocumentationOf;
Begin
  Result := TcdaDocumentationOf(ObjectByIndex[iIndex]);
End;

Function TcdaDocumentationOfList.Append : TcdaDocumentationOf;
Begin
  Result := TcdaDocumentationOf.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaDocumentationOfList.AddItem(value : TcdaDocumentationOf);
Begin
  Add(value.Link);
End;

Function TcdaDocumentationOfList.IndexOf(value : TcdaDocumentationOf) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaDocumentationOfList.Insert(iIndex : Integer) : TcdaDocumentationOf;
Begin
  Result := TcdaDocumentationOf.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaDocumentationOfList.InsertItem(iIndex : Integer; value : TcdaDocumentationOf);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaDocumentationOfList.Item(iIndex : Integer) : TcdaDocumentationOf;
Begin
  Result := DocumentationOfs[iIndex];
End;

Procedure TcdaDocumentationOfList.SetItemByIndex(iIndex : Integer; value: TcdaDocumentationOf);
Begin
  DocumentationOfs[iIndex] := value;
End;

Procedure TcdaDocumentationOfList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaDocumentationOfList.ClearItems;
Begin
  Clear;
End;

function TcdaDocumentationOfList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaDocumentationOfList.SetDocumentationOfs(iIndex: Integer; const Value: TcdaDocumentationOf);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaDocumentationOfList.ItemClass: TFslObjectClass;
begin
  Result := TcdaDocumentationOf;
end;

{ TcdaInformationRecipientList }
Function TcdaInformationRecipientList.Link : TcdaInformationRecipientList;
Begin
  Result := TcdaInformationRecipientList(Inherited Link);
End;

Function TcdaInformationRecipientList.Clone(parent : Tv3Base): TcdaInformationRecipientList;
Begin
  Result := TcdaInformationRecipientList(inherited Clone(parent));
End;

Function TcdaInformationRecipientList.GetInformationRecipients(iIndex : Integer) : TcdaInformationRecipient;
Begin
  Result := TcdaInformationRecipient(ObjectByIndex[iIndex]);
End;

Function TcdaInformationRecipientList.Append : TcdaInformationRecipient;
Begin
  Result := TcdaInformationRecipient.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaInformationRecipientList.AddItem(value : TcdaInformationRecipient);
Begin
  Add(value.Link);
End;

Function TcdaInformationRecipientList.IndexOf(value : TcdaInformationRecipient) : Integer;
Begin
  Result := IndexByReference(value);
End;

Function TcdaInformationRecipientList.Insert(iIndex : Integer) : TcdaInformationRecipient;
Begin
  Result := TcdaInformationRecipient.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaInformationRecipientList.InsertItem(iIndex : Integer; value : TcdaInformationRecipient);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaInformationRecipientList.Item(iIndex : Integer) : TcdaInformationRecipient;
Begin
  Result := InformationRecipients[iIndex];
End;

Procedure TcdaInformationRecipientList.SetItemByIndex(iIndex : Integer; value: TcdaInformationRecipient);
Begin
  InformationRecipients[iIndex] := value;
End;

Procedure TcdaInformationRecipientList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaInformationRecipientList.ClearItems;
Begin
  Clear;
End;

function TcdaInformationRecipientList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaInformationRecipientList.SetInformationRecipients( iIndex: Integer; const Value: TcdaInformationRecipient);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaInformationRecipientList.ItemClass: TFslObjectClass;
begin
  Result := TcdaInformationRecipient;
end;

{ TcdaInFulfillmentOfList }
Function TcdaInFulfillmentOfList.Link : TcdaInFulfillmentOfList;
Begin
  Result := TcdaInFulfillmentOfList(Inherited Link);
End;

Function TcdaInFulfillmentOfList.Clone(parent : Tv3Base): TcdaInFulfillmentOfList;
Begin
  result := TcdaInFulfillmentOfList(inherited Clone(parent));
End;

Function TcdaInFulfillmentOfList.GetInFulfillmentOfs(iIndex : Integer) : TcdaInFulfillmentOf;
Begin
  Result := TcdaInFulfillmentOf(ObjectByIndex[iIndex]);
End;

Function TcdaInFulfillmentOfList.Append : TcdaInFulfillmentOf;
Begin
  Result := TcdaInFulfillmentOf.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaInFulfillmentOfList.AddItem(value : TcdaInFulfillmentOf);
Begin
  Add(value.Link);
End;

Function TcdaInFulfillmentOfList.IndexOf(value : TcdaInFulfillmentOf) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaInFulfillmentOfList.Insert(iIndex : Integer) : TcdaInFulfillmentOf;
Begin
  Result := TcdaInFulfillmentOf.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaInFulfillmentOfList.InsertItem(iIndex : Integer; value : TcdaInFulfillmentOf);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaInFulfillmentOfList.Item(iIndex : Integer) : TcdaInFulfillmentOf;
Begin
  Result := InFulfillmentOfs[iIndex];
End;

Procedure TcdaInFulfillmentOfList.SetItemByIndex(iIndex : Integer; value: TcdaInFulfillmentOf);
Begin
  InFulfillmentOfs[iIndex] := value;
End;

Procedure TcdaInFulfillmentOfList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaInFulfillmentOfList.ClearItems;
Begin
  Clear;
End;

function TcdaInFulfillmentOfList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaInFulfillmentOfList.SetInFulfillmentOfs(iIndex: Integer; const Value: TcdaInFulfillmentOf);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaInFulfillmentOfList.ItemClass: TFslObjectClass;
begin
  Result := TcdaInFulfillmentOf;
end;

{ TcdaParticipant1List }
Function TcdaParticipant1List.Link : TcdaParticipant1List;
Begin
  Result := TcdaParticipant1List(Inherited Link);
End;

Function TcdaParticipant1List.Clone(parent : Tv3Base): TcdaParticipant1List;
Begin
  result := TcdaParticipant1List(inherited Clone(parent));
End;

Function TcdaParticipant1List.GetParticipant1s(iIndex : Integer) : TcdaParticipant1;
Begin
  Result := TcdaParticipant1(ObjectByIndex[iIndex]);
End;

Function TcdaParticipant1List.Append : TcdaParticipant1;
Begin
  Result := TcdaParticipant1.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaParticipant1List.AddItem(value : TcdaParticipant1);
Begin
  Add(value.Link);
End;

Function TcdaParticipant1List.IndexOf(value : TcdaParticipant1) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaParticipant1List.Insert(iIndex : Integer) : TcdaParticipant1;
Begin
  Result := TcdaParticipant1.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaParticipant1List.InsertItem(iIndex : Integer; value : TcdaParticipant1);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaParticipant1List.Item(iIndex : Integer) : TcdaParticipant1;
Begin
  Result := Participant1s[iIndex];
End;

Procedure TcdaParticipant1List.SetItemByIndex(iIndex : Integer; value: TcdaParticipant1);
Begin
  Participant1s[iIndex] := value;
End;

Procedure TcdaParticipant1List.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaParticipant1List.ClearItems;
Begin
  Clear;
End;

function TcdaParticipant1List.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaParticipant1List.SetParticipant1s(iIndex: Integer; const Value: TcdaParticipant1);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaParticipant1List.ItemClass: TFslObjectClass;
begin
  Result := TcdaParticipant1;
end;

{ TcdaRecordTargetList }
Function TcdaRecordTargetList.Link : TcdaRecordTargetList;
Begin
  Result := TcdaRecordTargetList(Inherited Link);
End;

Function TcdaRecordTargetList.Clone(parent : Tv3Base): TcdaRecordTargetList;
Begin
  result := TcdaRecordTargetList(inherited Clone(parent));
End;

Function TcdaRecordTargetList.GetRecordTargets(iIndex : Integer) : TcdaRecordTarget;
Begin
  Result := TcdaRecordTarget(ObjectByIndex[iIndex]);
End;

Function TcdaRecordTargetList.Append : TcdaRecordTarget;
Begin
  Result := TcdaRecordTarget.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaRecordTargetList.AddItem(value : TcdaRecordTarget);
Begin
  Add(value.Link);
End;

Function TcdaRecordTargetList.IndexOf(value : TcdaRecordTarget) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaRecordTargetList.Insert(iIndex : Integer) : TcdaRecordTarget;
Begin
  Result := TcdaRecordTarget.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaRecordTargetList.InsertItem(iIndex : Integer; value : TcdaRecordTarget);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaRecordTargetList.Item(iIndex : Integer) : TcdaRecordTarget;
Begin
  Result := RecordTargets[iIndex];
End;

Procedure TcdaRecordTargetList.SetItemByIndex(iIndex : Integer; value: TcdaRecordTarget);
Begin
  RecordTargets[iIndex] := value;
End;

Procedure TcdaRecordTargetList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaRecordTargetList.ClearItems;
Begin
  Clear;
End;

function TcdaRecordTargetList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaRecordTargetList.SetRecordTargets(iIndex: Integer; const Value: TcdaRecordTarget);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaRecordTargetList.ItemClass: TFslObjectClass;
begin
  Result := TcdaRecordTarget;
end;

{ TcdaRelatedDocumentList }
Function TcdaRelatedDocumentList.Link : TcdaRelatedDocumentList;
Begin
  Result := TcdaRelatedDocumentList(Inherited Link);
End;

Function TcdaRelatedDocumentList.Clone(parent : Tv3Base): TcdaRelatedDocumentList;
Begin
  result := TcdaRelatedDocumentList(inherited Clone(parent));
End;

Function TcdaRelatedDocumentList.GetRelatedDocuments(iIndex : Integer) : TcdaRelatedDocument;
Begin
  Result := TcdaRelatedDocument(ObjectByIndex[iIndex]);
End;

Function TcdaRelatedDocumentList.Append : TcdaRelatedDocument;
Begin
  Result := TcdaRelatedDocument.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaRelatedDocumentList.AddItem(value : TcdaRelatedDocument);
Begin
  Add(value.Link);
End;

Function TcdaRelatedDocumentList.IndexOf(value : TcdaRelatedDocument) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaRelatedDocumentList.Insert(iIndex : Integer) : TcdaRelatedDocument;
Begin
  Result := TcdaRelatedDocument.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaRelatedDocumentList.InsertItem(iIndex : Integer; value : TcdaRelatedDocument);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaRelatedDocumentList.Item(iIndex : Integer) : TcdaRelatedDocument;
Begin
  Result := RelatedDocuments[iIndex];
End;

Procedure TcdaRelatedDocumentList.SetItemByIndex(iIndex : Integer; value: TcdaRelatedDocument);
Begin
  RelatedDocuments[iIndex] := value;
End;

Procedure TcdaRelatedDocumentList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaRelatedDocumentList.ClearItems;
Begin
  Clear;
End;

function TcdaRelatedDocumentList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaRelatedDocumentList.SetRelatedDocuments(iIndex: Integer; const Value: TcdaRelatedDocument);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaRelatedDocumentList.ItemClass: TFslObjectClass;
begin
  Result := TcdaRelatedDocument;
end;

{ TcdaEncounterParticipantList }
Function TcdaEncounterParticipantList.Link : TcdaEncounterParticipantList;
Begin
  Result := TcdaEncounterParticipantList(Inherited Link);
End;

Function TcdaEncounterParticipantList.Clone(parent : Tv3Base): TcdaEncounterParticipantList;
Begin
  result := TcdaEncounterParticipantList(inherited Clone(parent));
End;

Function TcdaEncounterParticipantList.GetEncounterParticipants(iIndex : Integer) : TcdaEncounterParticipant;
Begin
  Result := TcdaEncounterParticipant(ObjectByIndex[iIndex]);
End;

Function TcdaEncounterParticipantList.Append : TcdaEncounterParticipant;
Begin
  Result := TcdaEncounterParticipant.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaEncounterParticipantList.AddItem(value : TcdaEncounterParticipant);
Begin
  Add(value.Link);
End;

Function TcdaEncounterParticipantList.IndexOf(value : TcdaEncounterParticipant) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaEncounterParticipantList.Insert(iIndex : Integer) : TcdaEncounterParticipant;
Begin
  Result := TcdaEncounterParticipant.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaEncounterParticipantList.InsertItem(iIndex : Integer; value : TcdaEncounterParticipant);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaEncounterParticipantList.Item(iIndex : Integer) : TcdaEncounterParticipant;
Begin
  Result := EncounterParticipants[iIndex];
End;

Procedure TcdaEncounterParticipantList.SetItemByIndex(iIndex : Integer; value: TcdaEncounterParticipant);
Begin
  EncounterParticipants[iIndex] := value;
End;

Procedure TcdaEncounterParticipantList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaEncounterParticipantList.ClearItems;
Begin
  Clear;
End;

function TcdaEncounterParticipantList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaEncounterParticipantList.SetEncounterParticipants( iIndex: Integer; const Value: TcdaEncounterParticipant);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaEncounterParticipantList.ItemClass: TFslObjectClass;
begin
  Result := TcdaEncounterParticipant;
end;

{ TcdaReferenceRangeList }
Function TcdaReferenceRangeList.Link : TcdaReferenceRangeList;
Begin
  Result := TcdaReferenceRangeList(Inherited Link);
End;

Function TcdaReferenceRangeList.Clone(parent : Tv3Base): TcdaReferenceRangeList;
Begin
  result := TcdaReferenceRangeList(inherited Clone(parent));
End;

Function TcdaReferenceRangeList.GetReferenceRanges(iIndex : Integer) : TcdaReferenceRange;
Begin
  Result := TcdaReferenceRange(ObjectByIndex[iIndex]);
End;

Function TcdaReferenceRangeList.Append : TcdaReferenceRange;
Begin
  Result := TcdaReferenceRange.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaReferenceRangeList.AddItem(value : TcdaReferenceRange);
Begin
  Add(value.Link);
End;

Function TcdaReferenceRangeList.IndexOf(value : TcdaReferenceRange) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaReferenceRangeList.Insert(iIndex : Integer) : TcdaReferenceRange;
Begin
  Result := TcdaReferenceRange.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaReferenceRangeList.InsertItem(iIndex : Integer; value : TcdaReferenceRange);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaReferenceRangeList.Item(iIndex : Integer) : TcdaReferenceRange;
Begin
  Result := ReferenceRanges[iIndex];
End;

Procedure TcdaReferenceRangeList.SetItemByIndex(iIndex : Integer; value: TcdaReferenceRange);
Begin
  ReferenceRanges[iIndex] := value;
End;

Procedure TcdaReferenceRangeList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaReferenceRangeList.ClearItems;
Begin
  Clear;
End;

function TcdaReferenceRangeList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaReferenceRangeList.SetReferenceRanges(iIndex: Integer; const Value: TcdaReferenceRange);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaReferenceRangeList.ItemClass: TFslObjectClass;
begin
  Result := TcdaReferenceRange;
end;

{ TcdaComponent4List }
Function TcdaComponent4List.Link : TcdaComponent4List;
Begin
  Result := TcdaComponent4List(Inherited Link);
End;

Function TcdaComponent4List.Clone(parent : Tv3Base): TcdaComponent4List;
Begin
  result := TcdaComponent4List(inherited Clone(parent));
End;

Function TcdaComponent4List.GetComponent4s(iIndex : Integer) : TcdaComponent4;
Begin
  Result := TcdaComponent4(ObjectByIndex[iIndex]);
End;

Function TcdaComponent4List.Append : TcdaComponent4;
Begin
  Result := TcdaComponent4.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaComponent4List.AddItem(value : TcdaComponent4);
Begin
  Add(value.Link);
End;

Function TcdaComponent4List.IndexOf(value : TcdaComponent4) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaComponent4List.Insert(iIndex : Integer) : TcdaComponent4;
Begin
  Result := TcdaComponent4.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaComponent4List.InsertItem(iIndex : Integer; value : TcdaComponent4);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaComponent4List.Item(iIndex : Integer) : TcdaComponent4;
Begin
  Result := Component4s[iIndex];
End;

Procedure TcdaComponent4List.SetItemByIndex(iIndex : Integer; value: TcdaComponent4);
Begin
  Component4s[iIndex] := value;
End;

Procedure TcdaComponent4List.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaComponent4List.ClearItems;
Begin
  Clear;
End;

function TcdaComponent4List.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaComponent4List.InsertObject(iIndex: Integer; oObject: TcdaComponent4);
begin
  Inherited Insert(iIndex, oObject);
end;

procedure TcdaComponent4List.SetComponent4s(iIndex: Integer; const Value: TcdaComponent4);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaComponent4List.ItemClass: TFslObjectClass;
begin
  Result := TcdaComponent4;
end;

{ TcdaGuardianList }
Function TcdaGuardianList.Link : TcdaGuardianList;
Begin
  Result := TcdaGuardianList(Inherited Link);
End;

Function TcdaGuardianList.Clone(parent : Tv3Base): TcdaGuardianList;
Begin
  result := TcdaGuardianList(inherited Clone(parent));
End;

Function TcdaGuardianList.GetGuardians(iIndex : Integer) : TcdaGuardian;
Begin
  Result := TcdaGuardian(ObjectByIndex[iIndex]);
End;

Function TcdaGuardianList.Append : TcdaGuardian;
Begin
  Result := TcdaGuardian.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaGuardianList.AddItem(value : TcdaGuardian);
Begin
  Add(value.Link);
End;

Function TcdaGuardianList.IndexOf(value : TcdaGuardian) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaGuardianList.Insert(iIndex : Integer) : TcdaGuardian;
Begin
  Result := TcdaGuardian.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaGuardianList.InsertItem(iIndex : Integer; value : TcdaGuardian);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaGuardianList.Item(iIndex : Integer) : TcdaGuardian;
Begin
  Result := Guardians[iIndex];
End;

Procedure TcdaGuardianList.SetItemByIndex(iIndex : Integer; value: TcdaGuardian);
Begin
  Guardians[iIndex] := value;
End;

Procedure TcdaGuardianList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaGuardianList.ClearItems;
Begin
  Clear;
End;

function TcdaGuardianList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaGuardianList.SetGuardians(iIndex: Integer; const Value: TcdaGuardian);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaGuardianList.ItemClass: TFslObjectClass;
begin
  Result := TcdaGuardian;
end;

{ TcdaLanguageCommunicationList }
Function TcdaLanguageCommunicationList.Link : TcdaLanguageCommunicationList;
Begin
  Result := TcdaLanguageCommunicationList(Inherited Link);
End;

Function TcdaLanguageCommunicationList.Clone(parent : Tv3Base): TcdaLanguageCommunicationList;
Begin
  result := TcdaLanguageCommunicationList(inherited Clone(parent));
End;

Function TcdaLanguageCommunicationList.GetLanguageCommunications(iIndex : Integer) : TcdaLanguageCommunication;
Begin
  Result := TcdaLanguageCommunication(ObjectByIndex[iIndex]);
End;

Function TcdaLanguageCommunicationList.Append : TcdaLanguageCommunication;
Begin
  Result := TcdaLanguageCommunication.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaLanguageCommunicationList.AddItem(value : TcdaLanguageCommunication);
Begin
  Add(value.Link);
End;

Function TcdaLanguageCommunicationList.IndexOf(value : TcdaLanguageCommunication) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaLanguageCommunicationList.Insert(iIndex : Integer) : TcdaLanguageCommunication;
Begin
  Result := TcdaLanguageCommunication.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaLanguageCommunicationList.InsertItem(iIndex : Integer; value : TcdaLanguageCommunication);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaLanguageCommunicationList.Item(iIndex : Integer) : TcdaLanguageCommunication;
Begin
  Result := LanguageCommunications[iIndex];
End;

Procedure TcdaLanguageCommunicationList.SetItemByIndex(iIndex : Integer; value: TcdaLanguageCommunication);
Begin
  LanguageCommunications[iIndex] := value;
End;

Procedure TcdaLanguageCommunicationList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaLanguageCommunicationList.ClearItems;
Begin
  Clear;
End;

function TcdaLanguageCommunicationList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaLanguageCommunicationList.SetLanguageCommunications( iIndex: Integer; const Value: TcdaLanguageCommunication);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaLanguageCommunicationList.ItemClass: TFslObjectClass;
begin
  Result := TcdaLanguageCommunication;
end;

{ TcdaEntityIdentifierList }
Function TcdaEntityIdentifierList.Link : TcdaEntityIdentifierList;
Begin
  Result := TcdaEntityIdentifierList(Inherited Link);
End;

Function TcdaEntityIdentifierList.Clone(parent : Tv3Base): TcdaEntityIdentifierList;
Begin
  result := TcdaEntityIdentifierList(inherited Clone(parent));
End;

Function TcdaEntityIdentifierList.GetEntityIdentifiers(iIndex : Integer) : TcdaEntityIdentifier;
Begin
  Result := TcdaEntityIdentifier(ObjectByIndex[iIndex]);
End;

Function TcdaEntityIdentifierList.Append : TcdaEntityIdentifier;
Begin
  Result := TcdaEntityIdentifier.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaEntityIdentifierList.AddItem(value : TcdaEntityIdentifier);
Begin
  Add(value.Link);
End;

Function TcdaEntityIdentifierList.IndexOf(value : TcdaEntityIdentifier) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaEntityIdentifierList.Insert(iIndex : Integer) : TcdaEntityIdentifier;
Begin
  Result := TcdaEntityIdentifier.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaEntityIdentifierList.InsertItem(iIndex : Integer; value : TcdaEntityIdentifier);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaEntityIdentifierList.Item(iIndex : Integer) : TcdaEntityIdentifier;
Begin
  Result := EntityIdentifiers[iIndex];
End;

Procedure TcdaEntityIdentifierList.SetItemByIndex(iIndex : Integer; value: TcdaEntityIdentifier);
Begin
  EntityIdentifiers[iIndex] := value;
End;

Procedure TcdaEntityIdentifierList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaEntityIdentifierList.ClearItems;
Begin
  Clear;
End;

function TcdaEntityIdentifierList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaEntityIdentifierList.SetEntityIdentifiers( iIndex: Integer; const Value: TcdaEntityIdentifier);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaEntityIdentifierList.ItemClass: TFslObjectClass;
begin
  Result := TcdaEntityIdentifier;
end;


{ TcdaEntryList }
Function TcdaEntryList.Link : TcdaEntryList;
Begin
  Result := TcdaEntryList(Inherited Link);
End;

Function TcdaEntryList.Clone(parent : Tv3Base): TcdaEntryList;
Begin
  result := TcdaEntryList(inherited Clone(parent));
End;

Function TcdaEntryList.GetEntrys(iIndex : Integer) : TcdaEntry;
Begin
  Result := TcdaEntry(ObjectByIndex[iIndex]);
End;

Function TcdaEntryList.Append : TcdaEntry;
Begin
  Result := TcdaEntry.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaEntryList.AddItem(value : TcdaEntry);
Begin
  Add(value.Link);
End;

Function TcdaEntryList.IndexOf(value : TcdaEntry) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaEntryList.Insert(iIndex : Integer) : TcdaEntry;
Begin
  Result := TcdaEntry.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaEntryList.InsertItem(iIndex : Integer; value : TcdaEntry);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaEntryList.Item(iIndex : Integer) : TcdaEntry;
Begin
  Result := Entrys[iIndex];
End;

Procedure TcdaEntryList.SetItemByIndex(iIndex : Integer; value: TcdaEntry);
Begin
  Entrys[iIndex] := value;
End;

Procedure TcdaEntryList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaEntryList.ClearItems;
Begin
  Clear;
End;

function TcdaEntryList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaEntryList.InsertObject(iIndex: Integer; oObject: TcdaEntry);
begin
  Inherited Insert(iIndex, oObject);
end;

procedure TcdaEntryList.SetEntrys(iIndex: Integer; const Value: TcdaEntry);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaEntryList.ItemClass: TFslObjectClass;
begin
  Result := TcdaEntry;
end;

{ TcdaPerformer1List }
Function TcdaPerformer1List.Link : TcdaPerformer1List;
Begin
  Result := TcdaPerformer1List(Inherited Link);
End;

Function TcdaPerformer1List.Clone(parent : Tv3Base): TcdaPerformer1List;
Begin
  result := TcdaPerformer1List(inherited Clone(parent));
End;

Function TcdaPerformer1List.GetPerformer1s(iIndex : Integer) : TcdaPerformer1;
Begin
  Result := TcdaPerformer1(ObjectByIndex[iIndex]);
End;

Function TcdaPerformer1List.Append : TcdaPerformer1;
Begin
  Result := TcdaPerformer1.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaPerformer1List.AddItem(value : TcdaPerformer1);
Begin
  Add(value.Link);
End;

Function TcdaPerformer1List.IndexOf(value : TcdaPerformer1) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaPerformer1List.Insert(iIndex : Integer) : TcdaPerformer1;
Begin
  Result := TcdaPerformer1.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaPerformer1List.InsertItem(iIndex : Integer; value : TcdaPerformer1);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaPerformer1List.Item(iIndex : Integer) : TcdaPerformer1;
Begin
  Result := Performer1s[iIndex];
End;

Procedure TcdaPerformer1List.SetItemByIndex(iIndex : Integer; value: TcdaPerformer1);
Begin
  Performer1s[iIndex] := value;
End;

Procedure TcdaPerformer1List.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaPerformer1List.ClearItems;
Begin
  Clear;
End;

function TcdaPerformer1List.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaPerformer1List.SetPerformer1s(iIndex: Integer; const Value: TcdaPerformer1);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaPerformer1List.ItemClass: TFslObjectClass;
begin
  Result := TcdaPerformer1;
end;

{ TcdaComponentSectList }
Function TcdaComponentSectList.Link : TcdaComponentSectList;
Begin
  Result := TcdaComponentSectList(Inherited Link);
End;

Function TcdaComponentSectList.Clone(parent : Tv3Base): TcdaComponentSectList;
Begin
  result := TcdaComponentSectList(inherited Clone(parent));
End;

Function TcdaComponentSectList.GetComponentSects(iIndex : Integer) : TcdaComponentSect;
Begin
  Result := TcdaComponentSect(ObjectByIndex[iIndex]);
End;

Function TcdaComponentSectList.Append : TcdaComponentSect;
Begin
  Result := TcdaComponentSect.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaComponentSectList.AddItem(value : TcdaComponentSect);
Begin
  Add(value.Link);
End;

Function TcdaComponentSectList.IndexOf(value : TcdaComponentSect) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaComponentSectList.Insert(iIndex : Integer) : TcdaComponentSect;
Begin
  Result := TcdaComponentSect.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaComponentSectList.InsertItem(iIndex : Integer; value : TcdaComponentSect);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaComponentSectList.Item(iIndex : Integer) : TcdaComponentSect;
Begin
  Result := ComponentSects[iIndex];
End;

Procedure TcdaComponentSectList.SetItemByIndex(iIndex : Integer; value: TcdaComponentSect);
Begin
  ComponentSects[iIndex] := value;
End;

Procedure TcdaComponentSectList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaComponentSectList.ClearItems;
Begin
  Clear;
End;

function TcdaComponentSectList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaComponentSectList.SetComponentSects(iIndex: Integer; const Value: TcdaComponentSect);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaComponentSectList.ItemClass: TFslObjectClass;
begin
  Result := TcdaComponentSect;
end;

{ TcdaRegionOfInterest_value }

Function TcdaRegionOfInterest_value.CDAClassNameV : String;
Begin
  Result := 'RegionOfInterest_value';
End;

function TcdaRegionOfInterest_value.CDAClassTypeV: TCDAClassType;
begin
  result := etDatatype;
end;

procedure TcdaRegionOfInterest_value.Assign(oSource: TFslObject);
begin
  inherited;
  FHasUnsorted := TcdaRegionOfInterest_value(oSource).FHasUnsorted;
  Funsorted := TcdaRegionOfInterest_value(oSource).Funsorted;
end;

procedure TcdaRegionOfInterest_value.DoClear;
begin
  inherited;
  FHasUnsorted := False;
  Funsorted := False;
end;

function TcdaRegionOfInterest_value.Clone(parent : Tv3Base): TcdaRegionOfInterest_value;
begin
  Result := TcdaRegionOfInterest_value(inherited Clone(parent));
end;


function TcdaRegionOfInterest_value.Link: TcdaRegionOfInterest_value;
begin
  Result := TcdaRegionOfInterest_value(Inherited Link);
end;

procedure TcdaRegionOfInterest_value.ListProperties(oList: Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  If bInheritedProperties Then
    inherited;
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, true, 'unsorted', FHasUnsorted , Funsorted));
end;

procedure TcdaRegionOfInterest_value.SetPropertyValue(const aValue: TV3PropertyDefinition);
begin
  if aValue.Name = 'unsorted' Then
    aValue.AsBool(FHasUnsorted , Funsorted)
  else
    inherited;
end;

procedure TcdaRegionOfInterest_value.Setunsorted(const Value: Boolean);
begin
  FHasUnsorted := true;
  Funsorted := value;
end;

function TcdaRegionOfInterest_value.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TcdaRegionOfInterest_valueList }

function TcdaRegionOfInterest_valueList.Clone(parent : Tv3Base): TcdaRegionOfInterest_valueList;
begin
  Result := TcdaRegionOfInterest_valueList(inherited Clone(parent));
end;

function TcdaRegionOfInterest_valueList.GetRegionOfInterest_values(iIndex: Integer): TcdaRegionOfInterest_value;
begin
  Result := TcdaRegionOfInterest_value(ObjectByIndex[iIndex]);
end;

Function TcdaRegionOfInterest_valueList.Append : TcdaRegionOfInterest_value;
Begin
  Result := TcdaRegionOfInterest_value.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaRegionOfInterest_valueList.AddItem(value : TcdaRegionOfInterest_value);
Begin
  Add(value.Link);
End;

Function TcdaRegionOfInterest_valueList.IndexOf(value : TcdaRegionOfInterest_value) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaRegionOfInterest_valueList.Insert(iIndex : Integer) : TcdaRegionOfInterest_value;
Begin
  Result := TcdaRegionOfInterest_value.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaRegionOfInterest_valueList.InsertItem(iIndex : Integer; value : TcdaRegionOfInterest_value);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaRegionOfInterest_valueList.Item(iIndex : Integer) : TcdaRegionOfInterest_value;
Begin
  Result := RegionOfInterest_values[iIndex];
End;

Procedure TcdaRegionOfInterest_valueList.SetItemByIndex(iIndex : Integer; value: TcdaRegionOfInterest_value);
Begin
  RegionOfInterest_values[iIndex] := value;
End;

Procedure TcdaRegionOfInterest_valueList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaRegionOfInterest_valueList.ClearItems;
Begin
  Clear;
End;

function TcdaRegionOfInterest_valueList.Link: TcdaRegionOfInterest_valueList;
begin
  Result := TcdaRegionOfInterest_valueList(Inherited Link);
end;

function TcdaEntryRelationshipList.Count: Integer;
begin
  result := Inherited Count;
end;

function TcdaRegionOfInterest_valueList.Count: Integer;
begin
  result := Inherited Count;
end;

procedure TcdaEntryRelationshipList.InsertObject(iIndex: Integer; oObject: TcdaEntryRelationship);
begin
  Inherited Insert(iIndex, oObject);
end;

procedure TcdaEntryRelationshipList.SetEntryRelationships(iIndex: Integer; const Value: TcdaEntryRelationship);
begin
  ObjectByIndex[iIndex] := value;
end;

procedure TcdaRegionOfInterest_valueList.SetRegionOfInterest_values( iIndex: Integer; const Value: TcdaRegionOfInterest_value);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaEntryRelationshipList.ItemClass: TFslObjectClass;
begin
  Result := TcdaEntryRelationship;
end;

function TcdaRegionOfInterest_valueList.ItemClass: TFslObjectClass;
begin
  Result := TcdaRegionOfInterest_value;
end;

End.

