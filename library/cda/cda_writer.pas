Unit cda_writer;

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

Interface

Uses
  Sysutils,
  fsl_base, fsl_xml, fsl_utilities, fsl_stream,
  cda_base, cda_types, cda_narrative, cda_objects;

Type
  TCDAWriter = class (TFslObject)
  Private
    FErrors : Boolean;
    FDefaults : Boolean;

    Procedure Attribute(Const sPath: string; oXml : TXmlBuilder; const sName, sContent, sDefault : string); Overload;
    Procedure Attribute(Const sPath: string; oXml : TXmlBuilder; const sName, sContent : string; bOptional : Boolean); Overload;
    Function WriteNullFlavor(Const sPath: string; aNull : Tv3NullFlavor) : string;
    Function WriteBoolean(Const sPath: string; bBool : Boolean) : string;

    Procedure WriteComments(oXml : TXmlBuilder; oDT : Tv3ANY); Overload;
    Procedure WriteComments(oXml : TXmlBuilder; oDT : Tv3XP); Overload;
    Procedure WriteComments(oXml : TXmlBuilder; oFocus : TcdaBase); Overload;

    Procedure WriteANYAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3ANY);
    Procedure WriteCDAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3CD);
    Procedure WriteCDElements(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3CD);
    Procedure WriteADXP(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3ADXP);
    Procedure WriteENXP(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3ENXP);

    Procedure WriteANY(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3ANY; bOptional : Boolean);
    Procedure WriteQTY(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QTY; bOptional : Boolean; aExpected : Tv3QTYDatatype);
    Procedure WriteQTYAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3QTY);

    Procedure WriteBL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3BL; bOptional : Boolean);
    Procedure WriteCS(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3CS; bOptional : Boolean);
    Procedure WriteED(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3ED; bOptional : Boolean);
    Procedure WriteII(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3II; bOptional : Boolean);
    Procedure WriteCD(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3CD; bOptional : Boolean; bFlavor : boolean = false);
    Procedure WriteCR(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3CR; bOptional : Boolean);
    Procedure WriteSC(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3SC; bOptional : Boolean);
    Procedure WriteIVL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3IVL; bOptional : Boolean; aExpected : Tv3QTYDatatype);
    Procedure WritePIVL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3PIVL; bOptional : Boolean);
    Procedure WriteEIVL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3EIVL; bOptional : Boolean);
    Procedure WriteQSS(Const sPath: string; oXml : TXmlBuilder; const sName : string; oDT : Tv3QSS; bOptional : Boolean);
    Procedure WriteQSU(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSU; bOptional : Boolean);
    Procedure WriteQSP(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSP; bOptional : Boolean);
    Procedure WriteQSD(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSD; bOptional : Boolean);
    Procedure WriteQSI(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSI; bOptional : Boolean);
    Procedure WriteAD(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3AD; bOptional : Boolean);
    Procedure WriteEN(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3EN; bOptional : Boolean; bFlavor : boolean = false);
    Procedure WriteTS(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3TS; bOptional : Boolean);
    Procedure WriteTEL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3TEL; bOptional : Boolean);
    Procedure WriteST(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3ST; bOptional : Boolean);
    Procedure WriteINT(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3INT; bOptional : Boolean);
    Procedure WritePQ(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3PQ; bOptional : Boolean);
    Procedure WriteMO(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3MO; bOptional : Boolean);
    Procedure WritePQR(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3PQR; bOptional : Boolean);
    Procedure WriteRTO(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT  : Tv3RTO; bOptional : Boolean; aExpected1, aExpected2 : Tv3QTYDatatype);
    Procedure WriteTSAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3TS);
    Procedure WritePQAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3PQ);
    Procedure WriteMOAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3MO);
    Procedure WriteINTAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3INT);

    Procedure WriteGTS(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSET; bOptional : Boolean);

    Procedure WriteRegionOfInterestValue(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : TcdaRegionOfInterest_value; bOptional : Boolean);

    Procedure WriteSNText(Const sPath: string; oXml : TXmlBuilder; const sName : string; oFocus : TsnText);
    Procedure WriteCMGeneralList(Const sPath: string; oXml : TXmlBuilder; oParts : TsnCMGeneralList);
    Procedure WriteCMInlineList(Const sPath: string; oXml : TXmlBuilder; oParts : TsnCMInlineList);
    Procedure WriteCMContentList(Const sPath: string; oXml : TXmlBuilder; oParts : TsnCMContentList);
    Procedure WriteCMFootnotesList(Const sPath: string; oXml : TXmlBuilder; oParts : TsnCMFootnotesList);
    Procedure WriteSNBase(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnBase);
    Procedure WriteSNTableBase(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnTableItem);
    Procedure WriteSNColBase(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnColItem);
    Procedure WriteSNstring(Const sPath: string; oXml : TXmlBuilder; oText : Tsnstring);
    Procedure WriteSNEntity(Const sPath: string; oXml : TXmlBuilder; oText : Tsnstring);
    Procedure WriteSNContent(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnContent);
    Procedure WriteSNLinkHtml(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnLinkHtml);
    Procedure WriteSNFootnote(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnFootNote);
    Procedure WriteSNFootnoteRef(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnFootNoteRef);
    Procedure WriteSNParagraph(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnParagraph);
    Procedure WriteSNList(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnList);
    Procedure WriteSNListItem(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnItem);
    Procedure WriteSNTable(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnTable);
    Procedure WriteSNRenderMultiMedia(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnRenderMultiMedia);
    Procedure WriteSNSub(Const sPath: string; oXml : TXmlBuilder; oSub : Tsnstring);
    Procedure WriteSNSup(Const sPath: string; oXml : TXmlBuilder; oSup : Tsnstring);
    Procedure WriteSNBr(Const sPath: string; oXml : TXmlBuilder; oBr : TsnBr);
    Procedure WriteSNCaption(Const sPath: string; oXml : TXmlBuilder; oFocus  : TsnCaption);
    Procedure WriteSNColGroup(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnColGroup);
    Procedure WriteSNCol(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnCol);
    Procedure WriteSNRowGroup(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TsnTRowGroup);
    Procedure WriteSNRow(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnTRow);
    Procedure WriteSNCell(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TsnTCell);

    Procedure WriteAct(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAct; bOptional : Boolean);
    Procedure WriteAssignedAuthor(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAssignedAuthor; bOptional : Boolean);
    Procedure WriteAssignedCustodian(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAssignedCustodian; bOptional : Boolean);
    Procedure WriteAssignedEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAssignedEntity; bOptional : Boolean);
    Procedure WriteAssociatedEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAssociatedEntity; bOptional : Boolean);
    Procedure WriteAuthenticator(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAuthenticator; bOptional : Boolean);
    Procedure WriteAuthor(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAuthor; bOptional : Boolean);
    Procedure WriteAuthoringDevice(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAuthoringDevice; bOptional : Boolean);
    Procedure WriteAuthorization(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAuthorization; bOptional : Boolean);
    Procedure WriteBirthplace(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaBirthplace; bOptional : Boolean);
    Procedure WriteClinicalDocument(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaClinicalDocument; bOptional : Boolean);
    Procedure WriteComponent1(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaComponent1; bOptional : Boolean);
    Procedure WriteComponent2(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaComponent2; bOptional : Boolean);
    Procedure WriteComponentSect(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaComponentSect; bOptional : Boolean);
    Procedure WriteComponent4(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaComponent4; bOptional : Boolean);
    Procedure WriteConsent(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaConsent; bOptional : Boolean);
    Procedure WriteConsumable(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaConsumable; bOptional : Boolean);
    Procedure WriteCriterion(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaCriterion; bOptional : Boolean);
    Procedure WriteCustodian(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaCustodian; bOptional : Boolean);
    Procedure WriteCustodianOrganization(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaCustodianOrganization; bOptional : Boolean);
    Procedure WriteDataEnterer(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaDataEnterer; bOptional : Boolean);
    Procedure WriteDevice(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaDevice; bOptional : Boolean);
    Procedure WriteDocumentationOf(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaDocumentationOf; bOptional : Boolean);
    Procedure WriteEncompassingEncounter(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEncompassingEncounter; bOptional : Boolean);
    Procedure WriteEncounter(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEncounter; bOptional : Boolean);
    Procedure WriteEncounterParticipant(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEncounterParticipant; bOptional : Boolean);
    Procedure WriteEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEntity; bOptional : Boolean);
    Procedure WriteEntry(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEntry; bOptional : Boolean);
    Procedure WriteEntryRelationship(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEntryRelationship; bOptional : Boolean);
    Procedure WriteExternalAct(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaExternalAct; bOptional : Boolean);
    Procedure WriteExternalDocument(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaExternalDocument; bOptional : Boolean);
    Procedure WriteExternalObservation(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaExternalObservation; bOptional : Boolean);
    Procedure WriteExternalProcedure(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaExternalProcedure; bOptional : Boolean);
    Procedure WriteGuardian(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaGuardian; bOptional : Boolean);
    Procedure WriteHealthCareFacility(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaHealthCareFacility; bOptional : Boolean);
    Procedure WriteInformant12(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaInformant12; bOptional : Boolean);
    Procedure WriteInformationRecipient(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaInformationRecipient; bOptional : Boolean);
    Procedure WriteInFulfillmentOf(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaInFulfillmentOf; bOptional : Boolean);
    Procedure WriteIntendedRecipient(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaIntendedRecipient; bOptional : Boolean);
    Procedure WriteLabeledDrug(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaLabeledDrug; bOptional : Boolean);
    Procedure WriteLanguageCommunication(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaLanguageCommunication; bOptional : Boolean);
    Procedure WriteEntityIdentifier(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEntityIdentifier; bOptional : Boolean);
    Procedure WriteLegalAuthenticator(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaLegalAuthenticator; bOptional : Boolean);
    Procedure WriteLocation(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaLocation; bOptional : Boolean);
    Procedure WriteMaintainedEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaMaintainedEntity; bOptional : Boolean);
    Procedure WriteManufacturedProduct(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaManufacturedProduct; bOptional : Boolean);
    Procedure WriteMaterial(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaMaterial; bOptional : Boolean);
    Procedure WriteNonXMLBody(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaNonXMLBody; bOptional : Boolean);
    Procedure WriteObservation(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaObservation; bOptional : Boolean);
    Procedure WriteObservationMedia(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaObservationMedia; bOptional : Boolean);
    Procedure WriteObservationRange(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaObservationRange; bOptional : Boolean);
    Procedure WriteOrder(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaOrder; bOptional : Boolean);
    Procedure WriteOrganization(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaOrganization; bOptional : Boolean);
    Procedure WriteOrganizationPartOf(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaOrganizationPartOf; bOptional : Boolean);
    Procedure WriteOrganizer(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaOrganizer; bOptional : Boolean);
    Procedure WriteParentDocument(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaParentDocument; bOptional : Boolean);
    Procedure WriteParticipant1(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaParticipant1; bOptional : Boolean);
    Procedure WriteParticipant2(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaParticipant2; bOptional : Boolean);
    Procedure WriteParticipantRole(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaParticipantRole; bOptional : Boolean);
    Procedure WritePatient(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPatient; bOptional : Boolean);
    Procedure WritePatientRole(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPatientRole; bOptional : Boolean);
    Procedure WritePerformer1(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPerformer1; bOptional : Boolean);
    Procedure WritePerformer2(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPerformer2; bOptional : Boolean);
    Procedure WritePerson(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPerson; bOptional : Boolean);
    Procedure WritePlace(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPlace; bOptional : Boolean);
    Procedure WritePlayingEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPlayingEntity; bOptional : Boolean);
    Procedure WritePrecondition(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPrecondition; bOptional : Boolean);
    Procedure WriteProcedure(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaProcedure; bOptional : Boolean);
    Procedure WriteProduct(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaProduct; bOptional : Boolean);
    Procedure WriteRecordTarget(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRecordTarget; bOptional : Boolean);
    Procedure WriteReference(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaReference; bOptional : Boolean);
    Procedure WriteReferenceRange(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaReferenceRange; bOptional : Boolean);
    Procedure WriteRegionOfInterest(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRegionOfInterest; bOptional : Boolean);
    Procedure WriteRelatedDocument(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRelatedDocument; bOptional : Boolean);
    Procedure WriteRelatedEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRelatedEntity; bOptional : Boolean);
    Procedure WriteRelatedSubject(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRelatedSubject; bOptional : Boolean);
    Procedure WriteResponsibleParty(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaResponsibleParty; bOptional : Boolean);
    Procedure WriteSection(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSection; bOptional : Boolean);
    Procedure WriteServiceEvent(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaServiceEvent; bOptional : Boolean);
    Procedure WriteSpecimen(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSpecimen; bOptional : Boolean);
    Procedure WriteSpecimenRole(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSpecimenRole; bOptional : Boolean);
    Procedure WriteStructuredBody(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaStructuredBody; bOptional : Boolean);
    Procedure WriteSubject(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSubject; bOptional : Boolean);
    Procedure WriteSubjectPerson(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSubjectPerson; bOptional : Boolean);
    Procedure WriteSubstanceAdministration(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSubstanceAdministration; bOptional : Boolean);
    Procedure WriteSupply(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSupply; bOptional : Boolean);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Procedure WriteCDA(oXml : TXmlBuilder; oDoc : TcdaClinicalDocument);
    Procedure WritePiece(oXml: TXmlBuilder; oPart : Tv3Base);

    Property Errors : Boolean read FErrors write FErrors;
    Property Defaults : Boolean read FDefaults write FDefaults;
  End;

Implementation

{ TCDAWriter }

procedure TCDAWriter.WriteCDA(oXml: TXmlBuilder; oDoc: TcdaClinicalDocument);
begin
  // todo:
  //<?xml version="1.0"?>
  //<?xml-stylesheet type="text/xsl" href="CDA.xsl"?>

  oXml.Comment(' Software: Health Intersections CDA Editor ');
  oXml.AddAttribute('xmlns', 'urn:hl7-org:v3');
  oXml.AddAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance');

  WriteClinicalDocument('ClinicalDocument', oXml, 'ClinicalDocument', oDoc, false);
end;


Procedure TCDAWriter.Attribute(Const sPath: string; oXml : TXmlBuilder; const sName, sContent : string; bOptional : Boolean);
Begin
  if not bOptional and FErrors And (sContent = '') Then
    RaiseError('Attribute', 'Attribute '+sName+' is mandatory, but has no content'+' @'+sPath+'\@'+sName);
  if sContent <> '' Then
    oXml.AddAttribute(sName, sContent);
End;


Procedure TCDAWriter.Attribute(Const sPath: string; oXml : TXmlBuilder; const sName, sContent, sDefault : string);
Begin
  if (sContent <> '') And (Defaults or (sContent <> sDefault)) Then
    oXml.AddAttribute(sName, sContent);
End;


Function TCDAWriter.WriteNullFlavor(Const sPath: string; aNull : Tv3NullFlavor) : string;
Begin
  result := CODES_Tv3NullFlavor[aNull];
End;


Function TCDAWriter.WriteBoolean(Const sPath: string; bBool : Boolean) : string;
Begin
  if bBool Then
    Result := 'true'
  Else
    Result := 'false';
End;


Procedure TCDAWriter.WriteComments(oXml : TXmlBuilder; oDT : Tv3ANY);
var
  iLoop : Integer;
Begin
  if oDT.HasComments Then
    for iLoop := 0 to oDt.comments.Count - 1 Do
      oXml.comment(oDt.Comments[iLoop]);
End;

Procedure TCDAWriter.WriteComments(oXml : TXmlBuilder; oDT : Tv3XP);
var
  iLoop : Integer;
Begin
  if oDT.HasComments Then
    for iLoop := 0 to oDt.comments.Count - 1 Do
      oXml.comment(oDt.Comments[iLoop]);
End;

Procedure TCDAWriter.WriteComments(oXml : TXmlBuilder; oFocus : TcdaBase);
var
  iLoop : Integer;
Begin
  if oFocus.HasComments Then
    for iLoop := 0 to oFocus.comments.Count - 1 Do
      oXml.comment(oFocus.Comments[iLoop]);
End;


Procedure TCDAWriter.WriteANYAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3ANY);
Begin
  Attribute(sPath, oXml, 'nullFLavor', writeNullFlavor(sPath, oDt.NullFlavor), true);
  // ignore updateMode and flavorId for now
End;

Procedure TCDAWriter.WriteBL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3BL; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  if oDT.HasValue Then
    Attribute(sPath, oXml, 'value', BoolToStr(oDT.value), true);
  oDT.sourcelocation := oXml.Tag(sName);
End;

Procedure TCDAWriter.WriteCS(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3CS; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  Attribute(sPath, oXml, 'code', oDT.code, true);
  oDT.sourcelocation := oXml.Tag(sName);
End;


Procedure TCDAWriter.WriteII(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3II; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  Attribute(sPath, oXml, 'root', oDT.root, true);
  Attribute(sPath, oXml, 'extension', oDT.extension, true);
  Attribute(sPath, oXml, 'assigningAuthorityName', oDT.identifierName, true);
  if oDT.hasDisplayable Then
    Attribute(sPath, oXml, 'displayable', BoolToStr(oDT.displayable), true);
  Attribute(sPath, oXml, 'scope', CODES_Tv3IdentifierScope[oDT.scope], true);
  Attribute(sPath, oXml, 'reliability', CODES_Tv3IdentifierReliability[oDT.reliability], true);
  oDT.sourcelocation := oXml.Tag(sName);
End;


Procedure TCDAWriter.WriteINTAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3INT);
Begin
  if oDT.HasValue Then
    Attribute(sPath, oXml, 'value', IntToStr(oDT.value), true);
End;

Procedure TCDAWriter.WriteINT(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3INT; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  WriteINTAttributes(sPath, oXml, oDT);
  oDT.sourcelocation := oXml.Tag(sName);
End;


Procedure TCDAWriter.WriteTSAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3TS);
Begin
  Attribute(sPath, oXml, 'value', oDT.value, true);
End;

Procedure TCDAWriter.WriteTS(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3TS; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  WriteTSAttributes(sPath, oXml, oDT);
  oDT.sourcelocation := oXml.Tag(sName);
End;

Procedure TCDAWriter.WriteST(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3ST; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  Attribute(sPath, oXml, 'language', oDT.language, true);
  if (oDT.value <> '') Or ((oDT.translation <> nil) And (oDT.translation.Count > 0)) Then
  Begin
    if (oDT.translation <> nil) And (oDT.translation.Count > 0) Then
    Begin
      oDT.sourcelocation := oXml.Open(sName);
      if oDT.translation <> nil Then
        For iLoop := 0 to oDT.translation.Count - 1 Do
          WriteST(sPath+'\translation', oXml, 'translation', oDT.translation[iLoop], true);
      oXml.Text(oDT.Value);
      oXml.Close(sName);
    End
    Else
      oXml.TagText(sName, oDT.Value);
  End
  Else
    oDT.sourcelocation := oXml.Tag(sName);
End;


Procedure TCDAWriter.WriteCDAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3CD);
Begin
  Attribute(sPath, oXml, 'code', oDT.code, true);
  Attribute(sPath, oXml, 'codeSystem', oDT.codeSystem, true);
  Attribute(sPath, oXml, 'codeSystemName', oDT.codeSystemName, true);
  Attribute(sPath, oXml, 'codeSystemVersion', oDT.codeSystemVersion, true);
  Attribute(sPath, oXml, 'valueSet', oDT.valueSet, true);
  Attribute(sPath, oXml, 'valueSetVersion', oDT.valueSetVersion, true);
  Attribute(sPath, oXml, 'codingRationale', CODES_Tv3CodingRationale[oDT.codingRationale], true);
  if (oDT.displayName <> nil) Then
    Attribute(sPath, oXml, 'displayName', oDT.displayName.value, true);
End;


Procedure TCDAWriter.WriteCDElements(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3CD);
var
  iLoop : Integer;
Begin
  WriteED(sPath+'\originalText', oXml, 'originalText', oDT.originalText, true);
  if oDT.qualifier <> nil Then
    For iLoop := 0 to oDT.qualifier.Count - 1 Do
      WriteCR(sPath+'\qualifier', oXml, 'qualifier', oDT.qualifier[iLoop], true);
  if oDT.translation <> nil Then
    For iLoop := 0 to oDT.translation.Count - 1 Do
      WriteCD(sPath+'\translation', oXml, 'translation', oDT.translation[iLoop], true);
End;


Procedure TCDAWriter.WriteCD(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3CD; bOptional : Boolean; bFlavor : boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  If bFlavor Then
    if oDT.flavorId = '' Then
      Attribute(sPath, oXml, 'xsi:type', 'CD', false)
    Else
      Attribute(sPath, oXml, 'xsi:type', oDT.flavorId, false);
  WriteANYAttributes(sPath, oXml, oDT);
  WriteCDAttributes(sPath, oXml, oDT);
  if (oDT.originalText <> nil) Or ((oDT.translation <> nil) And (oDT.translation.Count >= 1)) Or ((oDT.qualifier <> nil) And (oDT.qualifier.Count >= 1)) Then
  Begin
    oDT.sourcelocation := oXml.Open(sName);
    WriteCDElements(sPath, oXml, oDT);
    oXml.Close(sName);
  End
  Else
    oDT.sourcelocation := oXml.Tag(sName);
End;


Procedure TCDAWriter.WriteSC(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3SC; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  if oDT.code <> nil Then
    WriteCDAttributes(sPath, oXml, oDT.code);
  Attribute(sPath, oXml, 'language', oDT.language, true);
  if (oDT.value <> '') Or ((oDT.translation <> nil) And (oDT.translation.Count > 0)) Then
  Begin
    if (oDT.translation <> nil) And (oDT.translation.Count > 0) Then
    Begin
      oDT.sourcelocation := oXml.Open(sName);
      if oDT.translation <> nil Then
        For iLoop := 0 to oDT.translation.Count - 1 Do
          WriteST(sPath+'\translation', oXml, 'translation', oDT.translation[iLoop], true);
      oXml.Text(oDT.Value);
      oXml.Close(sName);
    End
    Else
      oXml.TagText(sName, oDT.Value);
  End
  Else
    oDT.sourcelocation := oXml.Tag(sName);
End;

Procedure TCDAWriter.WritePQR(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3PQR; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  if not oDt.isNull then
    Attribute(sPath, oXml, 'value', oDT.value.AsString, true);
  WriteCDAttributes(sPath, oXml, oDT);
  oDT.sourcelocation := oXml.Open(sName);
  WriteCDElements(sPath, oXml, oDT);
  oXml.Close(sName);
End;

Procedure TCDAWriter.WritePQAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3PQ);
Begin
  if not oDt.value.isNull then
    Attribute(sPath, oXml, 'value', oDT.value.Asstring, true)
  else if oDt.originalText <> nil then
    Attribute(sPath, oXml, 'value', oDT.originalText.dataAsString, true);
  Attribute(sPath, oXml, 'unit', oDT.unit_, true);
  Attribute(sPath, oXml, 'codingRationale', CODES_Tv3CodingRationale[oDT.codingRationale], true);
End;

Procedure TCDAWriter.WriteMOAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3MO);
Begin
  if not oDt.value.isNull then
    Attribute(sPath, oXml, 'value', oDT.value.asstring, true);
  Attribute(sPath, oXml, 'currency', oDT.currency, true);
End;

Procedure TCDAWriter.WritePQ(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3PQ; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  WritePQAttributes(sPath, oXml, oDT);
  if (oDT.translation <> nil) And (oDT.translation.Count > 0) Then
  Begin
    oDT.sourcelocation := oXml.Open(sName);
    For iLoop := 0 to oDT.translation.Count - 1 Do
      WritePQR(sPath+'\translation', oXml, 'translation', oDT.translation[iLoop], true);
    oXml.Close(sName);
  End
  Else
    oDT.sourcelocation := oXml.Tag(sName);
End;

Procedure TCDAWriter.WriteMO(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3MO; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  WriteMOAttributes(sPath, oXml, oDT);
  oDT.sourcelocation := oXml.Tag(sName);
End;

Procedure TCDAWriter.WriteCR(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3CR; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);

  WriteANYAttributes(sPath, oXml, oDT);
  if (oDT.Hasinverted) Then
    Attribute(sPath, oXml, 'inverted', WriteBoolean(sPath, oDT.inverted), true);

  oDT.sourcelocation := oXml.Open(sName);
  WriteCD(sPath+'\name', oXml, 'name', oDT.name, true);
  WriteCD(sPath+'\value', oXml, 'value', oDT.value, true);
  oXml.Close(sName);
End;

Procedure TCDAWriter.WriteTEL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3TEL; bOptional : Boolean);
var
  sText : string;
  aLoop1 : Tv3TelecommunicationAddressUse;
  aLoop2 : Tv3TelecommunicationCapability;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  Attribute(sPath, oXml, 'value', oDT.value, true);

  sText := '';
  For aLoop1 := Low(Tv3TelecommunicationAddressUse) To High(Tv3TelecommunicationAddressUse) Do
    if aLoop1 in oDT.use Then
      sText := sText + ' ' + CODES_Tv3TelecommunicationAddressUse[aLoop1];
  Attribute(sPath, oXml, 'use', sText.Trim, true);

  sText := '';
  For aLoop2 := Low(Tv3TelecommunicationCapability) To High(Tv3TelecommunicationCapability) Do
    if aLoop2 in oDT.capabilities Then
      sText := sText + ' ' + CODES_Tv3TelecommunicationCapability[aLoop2];
  Attribute(sPath, oXml, 'capabilities', sText.Trim, true);

  if (oDT.useablePeriod <> nil) Then
  Begin
    oDT.sourcelocation := oXml.Open(sName);
    WriteGTS(sPath+'\useablePeriod', oXml, 'useablePeriod', oDT.useablePeriod, true);
    oXml.Close(sName);
  End
  Else
    oDT.sourcelocation := oXml.Tag(sName);
End;

Procedure TCDAWriter.WriteRTO(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT  : Tv3RTO; bOptional : Boolean; aExpected1, aExpected2 : Tv3QTYDatatype);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  if (oDT.numerator <> Nil) Or (oDT.denominator <> nil) Then
  Begin
    oDT.sourcelocation := oXml.Open(sName);
    WriteQTY(sPath+'\numerator', oXml, 'numerator', oDT.numerator, true, aExpected1);
    WriteQTY(sPath+'\denominator', oXml, 'denominator', oDT.denominator, true, aExpected2);
    oXml.Close(sName);
  End
  Else
    oDT.sourcelocation := oXml.Tag(sName);
End;


Procedure TCDAWriter.WriteED(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3ED; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  Attribute(sPath, oXml, 'mediaType', oDT.mediaType, 'text/plain');
  Attribute(sPath, oXml, 'charset', oDT.charset, true);
  Attribute(sPath, oXml, 'language', oDT.language, true);
  Attribute(sPath, oXml, 'compression', CODES_Tv3Compression[oDT.compression], true);
  if (oDT.integrityCheck <> nil) Then
    Attribute(sPath, oXml, 'integrityCheck', oDT.integrityCheck.asText, true);
  Attribute(sPath, oXml, 'integrityCheckAlgorithm', CODES_Tv3IntegrityCheckAlgorithm[oDT.integrityCheckAlgorithm], true);
  if (oDT.description <> nil) Then
    Attribute(sPath, oXml, 'description', oDT.description.value, true);

  if (oDT.value = '') And (oDT.data <> nil) Then
    Attribute(sPath, oXml, 'representation', 'B64', false);

  oDT.sourcelocation := oXml.Open(sName);
  WriteTEL(sPath+'\reference', oXml, 'reference', oDT.reference, true);
  WriteED(sPath+'\thumbnail', oXml, 'thumbnail', oDT.thumbnail, true);
  if oDT.translation <> nil Then
    For iLoop := 0 to oDT.translation.Count - 1 Do
      WriteED(sPath+'\translation', oXml, 'translation', oDT.translation[iLoop], true);

  if oDT.value <> '' Then
    oXml.Text(oDT.value)
  Else if oDT.data <> nil Then
    oXml.Text(String(EncodeBase64(oDT.data.AsBytes)))
  Else if oDt.xml <> nil Then
    oXml.WriteXml(oDt.xml);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WritePIVL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3PIVL; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);

  WriteANYAttributes(sPath, oXml, oDT);

  if oDT.HasIsFlexible Then
    Attribute(sPath, oXml, 'institutionSpecified', WriteBoolean(sPath, oDT.isFlexible), true);
  Attribute(sPath, oXml, 'alignment', CODES_Tv3CalendarCycle[oDT.alignment], true);

  oDT.sourcelocation := oXml.Open(sName);
  WriteED(sPath+'\originalText', oXml, 'originalText', oDT.originalText, true);
  WriteIVL(sPath+'\phase', oXml, 'phase', oDT.phase, true, Tv3TS);
  WritePQ(sPath+'\period', oXml, 'period', oDT.period, true);
  WriteRTO(sPath+'\frequency', oXml, 'frequency', oDT.frequency, true, Tv3INT, Tv3PQ);
  WriteINT(sPath+'\count', oXml, 'count', oDT.count, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteEIVL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3EIVL; bOptional : Boolean);
var
  oEvent : Tv3CD;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);

  WriteANYAttributes(sPath, oXml, oDT);

  oDT.sourcelocation := oXml.Open(sName);
  WriteED(sPath+'\originalText', oXml, 'originalText', oDT.originalText, true);
  WriteIVL(sPath+'\offset', oXml, 'offset', oDT.offset, true, Tv3TS);
  if oDT.event <> teNull Then
  Begin
    oEvent := Tv3CD.Create;
    Try
      oEvent.code := CODES_Tv3TimingEvent[oDT.event];
      WriteCD(sPath+'\event', oXml, 'event', oEvent, false, false);
    Finally
      oEvent.Free;
    End;
  End;
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteQSS(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSS; bOptional : Boolean);
Var
  iLoop : Integer;
Begin
  if (oDT = nil) Or (oDT.terms = nil) Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;

  For iLoop := 0 to oDT.terms.Count - 1 do
    WriteTS(sPath, oXML, sName, Tv3TS(oDT.terms[iLoop]), true);
End;


Procedure TCDAWriter.WriteIVL(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3IVL; bOptional : Boolean; aExpected : Tv3QTYDatatype);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  if oDt.anyIsValue And (oDT.any <> Nil) Then
    WriteQTYAttributes(sPath, oXml, oDT.any);

  If (oDT.low <> nil) Or (oDT.high <> nil) Or (oDT.width <> nil) Or (oDT.any <> nil) Then
  Begin
    oDT.sourcelocation := oXml.Open(sName);
    if (oDT.low <> nil) Then
    Begin
      if oDT.HaslowClosed Then
        Attribute(sPath, oXml, 'inclusive', WriteBoolean(sPath, oDT.lowClosed), true);
      WriteQTY(sPath+'\low', oXml, 'low', oDT.low, true, aExpected);
    End;
    if (oDT.high <> nil) Then
    Begin
      if oDT.HashighClosed Then
        Attribute(sPath, oXml, 'inclusive', WriteBoolean(sPath, oDT.highClosed), true);
      WriteQTY(sPath+'\high', oXml, 'high', oDT.high, true, aExpected);
    End;
    WriteQTY(sPath+'\width', oXml, 'width', oDT.width, true, aExpected.DiffType);
    if Not oDt.anyIsValue And (oDT.any <> Nil) Then
      WriteQTY(sPath+'\center', oXml, 'center', oDT.any, true, aExpected);
    oXml.Close(sName);
  End
  Else
    oDT.sourcelocation := oXml.Tag(sName);
End;


Procedure TCDAWriter.WriteAD(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3AD; bOptional : Boolean);
var
  iLoop : Integer;
  sText : string;
  aLoop1 : Tv3PostalAddressUse;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  WriteANYAttributes(sPath, oXml, oDT);
  if (oDT.HasIsNotOrdered) Then
    Attribute(sPath, oXml, 'isNotOrdered', WriteBoolean(sPath, oDT.isNotOrdered), true);

  sText := '';
  For aLoop1 := Low(Tv3PostalAddressUse) To High(Tv3PostalAddressUse) Do
    if aLoop1 in oDT.use Then
      sText := sText + ' ' + CODES_Tv3PostalAddressUse[aLoop1];
  Attribute(sPath, oXml, 'use', stringTrimWhitespace(sText), true);

  If (oDT.part <> nil) And (oDT.part.COunt > 0) Then
  Begin
    oDT.sourcelocation := oXml.Open(sName);
    For iLoop := 0 to oDT.Part.Count - 1 Do
      WriteADXP(sPath+'\part', oXml, oDT.Part[iLoop]);
    WriteGTS(sPath+'\useablePeriod', oXml, 'useablePeriod', oDT.useablePeriod, true);
    oXml.Close(sName);
  End
  Else
    oDT.sourcelocation := oXml.Tag(sName);
End;

Procedure TCDAWriter.WriteADXP(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3ADXP);
Begin
  WriteComments(oXml, oDT);
  if oDT.type_ = aptNull Then
    oXml.Text(oDT.value)
  Else
    oXml.TagText(TAG_NAMES_ADDRESS[oDT.type_], oDT.value);
End;


Procedure TCDAWriter.WriteENXP(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3ENXP);
Var
  sText : string;
  aLoop1 : Tv3EntityNamePartQualifier;
Begin
  WriteComments(oXml, oDT);
  if oDT.type_ = nptNull Then
    oXml.Text(oDT.value)
  Else
  Begin
    sText := '';
    For aLoop1 := Low(Tv3EntityNamePartQualifier) To High(Tv3EntityNamePartQualifier) Do
      if (aLoop1 in oDT.qualifier) And not (aLoop1 in [npqPFX, npqSFX]) Then
        sText := sText + ' ' + CODES_Tv3EntityNamePartQualifier[aLoop1];
    Attribute(sPath, oXml, 'qualifier', stringTrimWhitespace(sText), true);

    if oDT.type_ = nptTITLE Then
      if npqSFX in oDT.qualifier Then
        oXml.TagText('suffix', oDT.value)
      Else
        oXml.TagText('prefix', oDT.value)
    Else
      oXml.TagText(TAG_NAMES_NAME[oDT.type_], oDT.value);
  End;
End;


Procedure TCDAWriter.WriteEN(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3EN; bOptional : Boolean; bFlavor : boolean = false);
var
  iLoop : Integer;
  sText : string;
  aLoop1 : Tv3EntityNameUse;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);
  If bFlavor Then
    if oDT.flavorId = '' Then
      Attribute(sPath, oXml, 'xsi:type', 'EN', false)
    Else
      Attribute(sPath, oXml, 'xsi:type', oDT.flavorId, false);

  WriteANYAttributes(sPath, oXml, oDT);

  sText := '';
  For aLoop1 := Low(Tv3EntityNameUse) To High(Tv3EntityNameUse) Do
    if aLoop1 in oDT.use Then
      sText := sText + ' ' + CODES_Tv3EntityNameUse[aLoop1];
  Attribute(sPath, oXml, 'use', stringTrimWhitespace(sText), true);

  If (oDT.part <> nil) And (oDT.part.COunt > 0) Then
  Begin
    oDT.sourcelocation := oXml.Open(sName);
    For iLoop := 0 to oDT.Part.Count - 1 Do
      WriteENXP(sPath+'\part', oXml, oDT.Part[iLoop]);
    oXml.Close(sName);
  End
  Else
    oDT.sourcelocation := oXml.Tag(sName);
End;


Procedure TCDAWriter.WriteANY(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3ANY; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;

  if (oDT is Tv3IVL) Then
  Begin
    Attribute(sPath, oXml, 'xsi:type', 'IVL_'+Copy((oDT as Tv3IVL).ParamType.className, 4, $FF), false);
    WriteIVL(sPath, oXml, sName, oDT as Tv3IVL, bOptional, (oDT as Tv3IVL).ParamType)
  End
  Else if (oDT is Tv3RTO) Then
  Begin
    Attribute(sPath, oXml, 'xsi:type', 'RTO_'+
       Copy((oDT as Tv3RTO).NumeratorType.className, 4, $FF)+'_'+
       Copy((oDT as Tv3RTO).DenominatorType.className, 4, $FF),
       false);
    WriteRTO(sPath, oXml, sName, oDT as Tv3RTO, bOptional, (oDT as Tv3RTO).NumeratorType, (oDT as Tv3RTO).DenominatorType)
  End
  Else
  Begin
    Attribute(sPath, oXml, 'xsi:type', Copy(oDT.className, 4, $FF), false);
    if (oDT is Tv3PQ) Then
      WritePQ(sPath, oXml, sName, oDT as Tv3PQ, bOptional)
    Else if (oDT is Tv3CD) Then
      WriteCD(sPath, oXml, sName, oDT as Tv3CD, bOptional, true)
    Else if (oDT is Tv3BL) Then
      WriteBL(sPath, oXml, sName, oDT as Tv3BL, bOptional)
    Else if (oDT is Tv3CS) Then
      WriteCS(sPath, oXml, sName, oDT as Tv3CS, bOptional)
    Else if (oDT is Tv3ED) Then
      WriteED(sPath, oXml, sName, oDT as Tv3ED, bOptional)
    Else if (oDT is Tv3II) Then
      WriteII(sPath, oXml, sName, oDT as Tv3II, bOptional)
    Else if (oDT is Tv3SC) Then
      WriteSC(sPath, oXml, sName, oDT as Tv3SC, bOptional)
    Else if (oDT is Tv3AD) Then
      WriteAD(sPath, oXml, sName, oDT as Tv3AD, bOptional)
    Else if (oDT is Tv3EN) Then
      WriteEN(sPath, oXml, sName, oDT as Tv3EN, bOptional)
    Else if (oDT is Tv3TS) Then
      WriteTS(sPath, oXml, sName, oDT as Tv3TS, bOptional)
    Else if (oDT is Tv3MO) Then
      WriteMO(sPath, oXml, sName, oDT as Tv3MO, bOptional)
    Else if (oDT is Tv3TEL) Then
      WriteTEL(sPath, oXml, sName, oDT as Tv3TEL, bOptional)
    Else if (oDT is Tv3ST) Then
      WriteST(sPath, oXml, sName, oDT as Tv3ST, bOptional)
    Else if (oDT is Tv3INT) Then
      WriteINT(sPath, oXml, sName, oDT as Tv3INT, bOptional)
    Else
      RaiseError('WriteANY', 'unknown type '+oDt.classname+' @'+sPath);
  End;
End;


Procedure TCDAWriter.WriteQTYAttributes(Const sPath: string; oXml : TXmlBuilder; oDT : Tv3QTY);
Begin
  if oDT is Tv3TS Then
    WriteTSAttributes(sPath, oXml, Tv3TS(oDT))
  Else if oDT is Tv3PQ Then
    WritePQAttributes(sPath, oXml, Tv3PQ(oDT))
  Else if oDT is Tv3INT Then
    WriteINTAttributes(sPath, oXml, Tv3INT(oDT))
  Else
    RaiseError('WriteQTYAttributes', 'Still under development'+' @'+sPath);
End;

Procedure TCDAWriter.WriteQTY(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QTY; bOptional : Boolean; aExpected : Tv3QTYDatatype);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;

  if aExpected <> oDT.ClassType Then
    RaiseError('WriteQTY', 'Still under development'+' @'+sPath);
  if oDT is Tv3TS Then
    WriteTS(sPath, oXml, sName, Tv3TS(oDT), bOptional)
  Else if oDT is Tv3PQ Then
    WritePQ(sPath, oXml, sName, Tv3PQ(oDT), bOptional)
  Else if oDT is Tv3INT Then
    WriteINT(sPath, oXml, sName, Tv3INT(oDT), bOptional)
  Else if oDT is Tv3MO Then
    WriteMO(sPath, oXml, sName, Tv3MO(oDT), bOptional)
  Else
    RaiseError('WriteQTY', 'Still under development'+' @'+sPath);
End;


Procedure TCDAWriter.WriteQSU(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSU; bOptional : Boolean);
var
  sOp : string;
  sWorking : string;
  iLoop : Integer;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);

  if (oDT.Flat) Then
    sWorking := sName
  Else
  Begin
    sWorking := 'comp';
    Attribute(sPath, oXml, 'xsi:type', 'SXPR_TS', false);
    oDT.sourcelocation := oXml.Open(sName);
  End;
  if (oDT.terms <> nil) Then
    For iLoop := 0 to oDT.terms.Count - 1 do
    Begin
      Attribute(sPath, oXml, 'operator', sOp, true);
      WriteGTS(sPath+'\'+sWorking, oXml, sWorking, oDt.terms[iLoop], false);
      sOp := 'I';
    End;
  if (Not oDT.Flat) Then
    oXml.Close(sName);
End;

Procedure TCDAWriter.WriteQSP(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSP; bOptional : Boolean);
var
  sWorking : string;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);

  if (oDT.Flat) Then
    sWorking := sName
  Else
  Begin
    sWorking := 'comp';
    Attribute(sPath, oXml, 'xsi:type', 'SXPR_TS', false);
    oDT.sourcelocation := oXml.Open(sName);
  End;
  if (oDT.high <> nil) Then
    WriteGTS(sPath+'\'+sWorking, oXml, sWorking, oDt.high, true);
  if (oDT.low <> nil) Then
  Begin
    Attribute(sPath, oXml, 'operator', 'E', false);
    WriteGTS(sPath+'\'+sWorking, oXml, sWorking, oDt.low, true);
  End;
  if (Not oDT.Flat) Then
    oXml.Close(sName);
End;

Procedure TCDAWriter.WriteQSD(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSD; bOptional : Boolean);
var
  sWorking : string;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);

  if (oDT.Flat) Then
    sWorking := sName
  Else
  Begin
    sWorking := 'comp';
    Attribute(sPath, oXml, 'xsi:type', 'SXPR_TS', false);
    oDT.sourcelocation := oXml.Open(sName);
  End;
  if (oDT.subtrahend <> nil) Then
    WriteGTS(sPath+'\'+sWorking, oXml, sWorking, oDt.subtrahend, true);
  if (oDT.minuend <> nil) Then
  Begin
    Attribute(sPath, oXml, 'operator', 'E', false);
    WriteGTS(sPath+'\'+sWorking, oXml, sWorking, oDt.minuend, true);
  End;
  if (Not oDT.Flat) Then
    oXml.Close(sName);
End;

Procedure TCDAWriter.WriteQSI(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSI; bOptional : Boolean);
var
  sOp : string;
  sWorking : string;
  iLoop : Integer;
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oDT);

  if (oDT.Flat) Then
    sWorking := sName
  Else
  Begin
    sWorking := 'comp';
    Attribute(sPath, oXml, 'xsi:type', 'SXPR_TS', false);
    oDT.sourcelocation := oXml.Open(sName);
  End;
  if (oDT.terms <> nil) Then
    For iLoop := 0 to oDT.terms.Count - 1 do
    Begin
      Attribute(sPath, oXml, 'operator', sOp, true);
      WriteGTS(sPath+'\'+sWorking, oXml, sWorking, oDt.terms[iLoop], false);
      sOp := 'A';
    End;
  if (Not oDT.Flat) Then
    oXml.Close(sName);
End;


Procedure TCDAWriter.WriteGTS(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oDT : Tv3QSET; bOptional : Boolean);
Begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;

  if oDT is Tv3PIVL then
  Begin
    Attribute(sPath, oXml, 'xsi:type', 'PIVL_TS', false);
    WritePIVL(sPath, oXml, sName, Tv3PIVL(oDT), bOptional)
  End
  Else if oDT is Tv3IVL then
  Begin
    Attribute(sPath, oXml, 'xsi:type', 'IVL_TS', false);
    WriteIVL(sPath, oXml, sName, Tv3IVL(oDT), bOptional, Tv3TS)
  End
  Else if oDT is Tv3EIVL then
  Begin
    Attribute(sPath, oXml, 'xsi:type', 'EIVL_TS', false);
    WriteEIVL(sPath, oXml, sName, Tv3EIVL(oDT), bOptional)
  End
  Else if oDT is Tv3QSU Then
    WriteQSU(sPath, oXml, sName, Tv3QSU(oDT), bOptional)
  Else if oDT is Tv3QSI Then
    WriteQSI(sPath, oXml, sName, Tv3QSI(oDT), bOptional)
  Else if oDT is Tv3QSD Then
    WriteQSD(sPath, oXml, sName, Tv3QSD(oDT), bOptional)
  Else if oDT is Tv3QSP Then
    WriteQSP(sPath, oXml, sName, Tv3QSP(oDT), bOptional)
  Else if oDT is Tv3QSS Then
    WriteQSS(sPath, oXml, sName, Tv3QSS(oDT), bOptional);
End;


Procedure TCDAWriter.WriteAct(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAct; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Act is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), '');
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, false);
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, false);
  If oFocus.HasnegationInd Then
    Attribute(sPath, oXml, 'negationInd', WriteBoolean(sPath, oFocus.negationInd), true);

  oFocus.sourcelocation := oXml.Open(sName);
  If oFocus.realmCode <> Nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> Nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> Nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, false);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteCS(sPath+'\statusCode', oXml, 'statusCode', oFocus.statusCode, true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  WriteCD(sPath+'\priorityCode', oXml, 'priorityCode', oFocus.priorityCode, true);
  WriteCS(sPath+'\languageCode', oXml, 'languageCode', oFocus.languageCode, true);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.specimen <> nil Then
    For iLoop := 0 to oFocus.specimen.Count - 1 Do
      WriteSpecimen(sPath+'\specimen', oXml, 'specimen', oFocus.specimen[iLoop], true);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer2(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant2(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.entryRelationship <> nil Then
    For iLoop := 0 to oFocus.entryRelationship.Count - 1 Do
      WriteEntryRelationship(sPath+'\entryRelationship', oXml, 'entryRelationship', oFocus.entryRelationship[iLoop], true);
  if oFocus.reference <> nil Then
    For iLoop := 0 to oFocus.reference.Count - 1 Do
      WriteReference(sPath+'\reference', oXml, 'reference', oFocus.reference[iLoop], true);
  if oFocus.precondition <> nil Then
    For iLoop := 0 to oFocus.precondition.Count - 1 Do
      WritePrecondition(sPath+'\precondition', oXml, 'precondition', oFocus.precondition[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteAssignedAuthor(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAssignedAuthor; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'AssignedAuthor is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ASSIGNED');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  WritePerson(sPath+'\assignedPerson', oXml, 'assignedPerson', oFocus.assignedPerson, true);
  WriteAuthoringDevice(sPath+'\assignedAuthoringDevice', oXml, 'assignedAuthoringDevice', oFocus.assignedAuthoringDevice, true);
  WriteOrganization(sPath+'\representedOrganization', oXml, 'representedOrganization', oFocus.representedOrganization, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteAssignedCustodian(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAssignedCustodian; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'AssignedCustodian is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ASSIGNED');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCustodianOrganization(sPath+'\representedCustodianOrganization', oXml, 'representedCustodianOrganization', oFocus.representedCustodianOrganization, false);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteAssignedEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAssignedEntity; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'AssignedEntity is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ASSIGNED');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr',
       oFocus.addr[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  WritePerson(sPath+'\assignedPerson', oXml, 'assignedPerson', oFocus.assignedPerson, true);
  WriteOrganization(sPath+'\representedOrganization', oXml, 'representedOrganization', oFocus.representedOrganization, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteAssociatedEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAssociatedEntity; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'AssociatedEntity is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  WritePerson(sPath+'\associatedPerson', oXml, 'associatedPerson', oFocus.associatedPerson, true);
  WriteOrganization(sPath+'\scopingOrganization', oXml, 'scopingOrganization', oFocus.scopingOrganization, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteAuthenticator(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAuthenticator; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Authenticator is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'AUTHEN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteTS(sPath+'\time', oXml, 'time', oFocus.time, true);
  WriteCS(sPath+'\signatureCode', oXml, 'signatureCode', oFocus.signatureCode, true);
  WriteAssignedEntity(sPath+'\assignedEntity', oXml, 'assignedEntity', oFocus.assignedEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteAuthor(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAuthor; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Author is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'AUT');
  Attribute(sPath, oXml, 'contextControlCode', oFocus.contextControlCode, 'OP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\functionCode', oXml, 'functionCode', oFocus.functionCode, true);
  WriteTS(sPath+'\time', oXml, 'time', oFocus.time, true);
  WriteAssignedAuthor(sPath+'\assignedAuthor', oXml, 'assignedAuthor', oFocus.assignedAuthor, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteAuthoringDevice(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAuthoringDevice; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'AuthoringDevice is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'DEV');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteSC(sPath+'\manufacturerModelName', oXml, 'manufacturerModelName', oFocus.manufacturerModelName, true);
  WriteSC(sPath+'\softwareName', oXml, 'softwareName', oFocus.softwareName, true);
  if oFocus.asMaintainedEntity <> nil Then
    For iLoop := 0 to oFocus.asMaintainedEntity.Count - 1 Do
      WriteMaintainedEntity(sPath+'\asMaintainedEntity', oXml, 'asMaintainedEntity', oFocus.asMaintainedEntity[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteAuthorization(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaAuthorization; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Authorization is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'AUTH');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteConsent(sPath+'\consent', oXml, 'consent', oFocus.consent, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteBirthplace(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaBirthplace; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Birthplace is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'BIRTHPL');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WritePlace(sPath+'\place', oXml, 'place', oFocus.place, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteClinicalDocument(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaClinicalDocument; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ClinicalDocument is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'DOCCLIN');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteII(sPath+'\id', oXml, 'id', oFocus.id, true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteST(sPath+'\title', oXml, 'title', oFocus.title, true);
  WriteTS(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true);
  WriteCD(sPath+'\confidentialityCode', oXml, 'confidentialityCode', oFocus.confidentialityCode, true);
  WriteCS(sPath+'\languageCode', oXml, 'languageCode', oFocus.languageCode, true);
  WriteII(sPath+'\setId', oXml, 'setId', oFocus.setId, true);
  WriteINT(sPath+'\versionNumber', oXml, 'versionNumber', oFocus.versionNumber, true);
  WriteTS(sPath+'\copyTime', oXml, 'copyTime', oFocus.copyTime, true);
  if oFocus.recordTarget <> nil Then
    For iLoop := 0 to oFocus.recordTarget.Count - 1 do
      WriteRecordTarget(sPath+'\recordTarget', oXml, 'recordTarget', oFocus.recordTarget[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  WriteDataEnterer(sPath+'\dataEnterer', oXml, 'dataEnterer', oFocus.dataEnterer, true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  WriteCustodian(sPath+'\custodian', oXml, 'custodian', oFocus.custodian, true);
  if oFocus.informationRecipient <> nil Then
    For iLoop := 0 to oFocus.informationRecipient.Count - 1 Do
      WriteInformationRecipient(sPath+'\informationRecipient', oXml, 'informationRecipient', oFocus.informationRecipient[iLoop], true);
  WriteLegalAuthenticator(sPath+'\legalAuthenticator', oXml, 'legalAuthenticator', oFocus.legalAuthenticator, true);
  if oFocus.authenticator <> nil Then
    For iLoop := 0 to oFocus.authenticator.Count - 1 Do
      WriteAuthenticator(sPath+'\authenticator', oXml, 'authenticator', oFocus.authenticator[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant1(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.inFulfillmentOf <> nil Then
    For iLoop := 0 to oFocus.inFulfillmentOf.Count - 1 Do
      WriteInFulfillmentOf(sPath+'\inFulfillmentOf', oXml, 'inFulfillmentOf', oFocus.inFulfillmentOf[iLoop], true);
  if oFocus.documentationOf <> nil Then
    For iLoop := 0 to oFocus.documentationOf.Count - 1 Do
      WriteDocumentationOf(sPath+'\documentationOf', oXml, 'documentationOf', oFocus.documentationOf[iLoop], true);
  if oFocus.relatedDocument <> nil Then
    For iLoop := 0 to oFocus.relatedDocument.Count - 1 Do
      WriteRelatedDocument(sPath+'\relatedDocument', oXml, 'relatedDocument', oFocus.relatedDocument[iLoop], true);
  if oFocus.authorization <> nil Then
    For iLoop := 0 to oFocus.authorization.Count - 1 Do
      WriteAuthorization(sPath+'\authorization', oXml, 'authorization', oFocus.authorization[iLoop], true);
  WriteComponent1(sPath+'\componentOf', oXml, 'componentOf', oFocus.componentOf, true);
  WriteComponent2(sPath+'\component', oXml, 'component', oFocus.component, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteComponent1(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaComponent1; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Component1 is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'COMP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteEncompassingEncounter(sPath+'\encompassingEncounter', oXml, 'encompassingEncounter', oFocus.encompassingEncounter, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteComponent2(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaComponent2; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Component2 is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'COMP');
  If oFocus.HascontextConductionInd Then
    Attribute(sPath, oXml, 'contextConductionInd', WriteBoolean(sPath, oFocus.contextConductionInd), 'true');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteNonXMLBody(sPath+'\nonXMLBody', oXml, 'nonXMLBody', oFocus.nonXMLBody, true);
  WriteStructuredBody(sPath+'\structuredBody', oXml, 'structuredBody', oFocus.structuredBody, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteComponentSect(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaComponentSect; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Component3 is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'COMP');
  If oFocus.HascontextConductionInd Then
    Attribute(sPath, oXml, 'contextConductionInd', WriteBoolean(sPath, oFocus.contextConductionInd), 'true');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteSection(sPath+'\section', oXml, 'section', oFocus.section, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteComponent4(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaComponent4; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Component4 is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'COMP');
  If oFocus.HascontextConductionInd Then
    Attribute(sPath, oXml, 'contextConductionInd', WriteBoolean(sPath, oFocus.contextConductionInd), 'true');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteINT(sPath+'\sequenceNumber', oXml, 'sequenceNumber', oFocus.sequenceNumber, true);
  WriteBL(sPath+'\seperatableInd', oXml, 'seperatableInd', oFocus.seperatableInd, true);
  WriteAct(sPath+'\act', oXml, 'act', oFocus.act, true);
  WriteEncounter(sPath+'\encounter', oXml, 'encounter', oFocus.encounter, true);
  WriteObservation(sPath+'\observation', oXml, 'observation', oFocus.observation, true);
  WriteObservationMedia(sPath+'\observationMedia', oXml, 'observationMedia', oFocus.observationMedia, true);
  WriteOrganizer(sPath+'\organizer', oXml, 'organizer', oFocus.organizer, true);
  WriteProcedure(sPath+'\procedure', oXml, 'procedure', oFocus.procedure_, true);
  WriteRegionOfInterest(sPath+'\regionOfInterest', oXml, 'regionOfInterest', oFocus.regionOfInterest, true);
  WriteSubstanceAdministration(sPath+'\substanceAdministration', oXml, 'substanceAdministration', oFocus.substanceAdministration, true);
  WriteSupply(sPath+'\supply', oXml, 'supply', oFocus.supply, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteConsent(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaConsent; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Consent is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'CONS');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteCS(sPath+'\statusCode', oXml, 'statusCode', oFocus.statusCode, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteConsumable(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaConsumable; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Consumable is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'CSM');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteManufacturedProduct(sPath+'\manufacturedProduct', oXml, 'manufacturedProduct', oFocus.manufacturedProduct, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteCriterion(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaCriterion; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Criterion is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'OBS');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN.CRT');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteANY(sPath+'\value', oXml, 'value', oFocus.value, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteCustodian(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaCustodian; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Custodian is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'CST');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteAssignedCustodian(sPath+'\assignedCustodian', oXml, 'assignedCustodian', oFocus.assignedCustodian, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteCustodianOrganization(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaCustodianOrganization; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'CustodianOrganization is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ORG');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteEN(sPath+'\name', oXml, 'name', oFocus.name, true);
  WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom, true);
  WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteDataEnterer(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaDataEnterer; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'DataEnterer is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'ENT');
  Attribute(sPath, oXml, 'contextControlCode', oFocus.contextControlCode, 'OP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteTS(sPath+'\time', oXml, 'time', oFocus.time, true);
  WriteAssignedEntity(sPath+'\assignedEntity', oXml, 'assignedEntity', oFocus.assignedEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteDevice(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaDevice; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Device is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'DEV');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteSC(sPath+'\manufacturerModelName', oXml, 'manufacturerModelName', oFocus.manufacturerModelName, true);
  WriteSC(sPath+'\softwareName', oXml, 'softwareName', oFocus.softwareName, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteDocumentationOf(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaDocumentationOf; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'DocumentationOf is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'DOC');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteServiceEvent(sPath+'\serviceEvent', oXml, 'serviceEvent', oFocus.serviceEvent, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteEncompassingEncounter(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEncompassingEncounter; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'EncompassingEncounter is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ENC');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  WriteCD(sPath+'\dischargeDispositionCode', oXml, 'dischargeDispositionCode', oFocus.dischargeDispositionCode, true);
  WriteResponsibleParty(sPath+'\responsibleParty', oXml, 'responsibleParty', oFocus.responsibleParty, true);
  if oFocus.encounterParticipant <> nil Then
    For iLoop := 0 to oFocus.encounterParticipant.Count - 1 Do
      WriteEncounterParticipant(sPath+'\encounterParticipant', oXml, 'encounterParticipant', oFocus.encounterParticipant[iLoop], true);
  WriteLocation(sPath+'\location', oXml, 'location', oFocus.location, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteEncounter(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEncounter; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Encounter is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, false);
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteCS(sPath+'\statusCode', oXml, 'statusCode', oFocus.statusCode, true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  WriteCD(sPath+'\priorityCode', oXml, 'priorityCode', oFocus.priorityCode, true);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.specimen <> nil Then
    For iLoop := 0 to oFocus.specimen.Count - 1 Do
      WriteSpecimen(sPath+'\specimen', oXml, 'specimen', oFocus.specimen[iLoop], true);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer2(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant2(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.entryRelationship <> nil Then
    For iLoop := 0 to oFocus.entryRelationship.Count - 1 Do
      WriteEntryRelationship(sPath+'\entryRelationship', oXml, 'entryRelationship', oFocus.entryRelationship[iLoop], true);
  if oFocus.reference <> nil Then
    For iLoop := 0 to oFocus.reference.Count - 1 Do
      WriteReference(sPath+'\reference', oXml, 'reference', oFocus.reference[iLoop], true);
  if oFocus.precondition <> nil Then
    For iLoop := 0 to oFocus.precondition.Count - 1 Do
      WritePrecondition(sPath+'\precondition', oXml, 'precondition', oFocus.precondition[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteEncounterParticipant(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEncounterParticipant; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'EncounterParticipant is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteIVL(sPath+'\time', oXml, 'time', oFocus.time, true, Tv3TS);
  WriteAssignedEntity(sPath+'\assignedEntity', oXml, 'assignedEntity', oFocus.assignedEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEntity; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Entity is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ENT');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\desc', oXml, 'desc', oFocus.desc, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteEntry(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEntry; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Entry is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'COMP');
  If oFocus.HascontextConductionInd Then
    Attribute(sPath, oXml, 'contextConductionInd', WriteBoolean(sPath, oFocus.contextConductionInd), 'true');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteAct(sPath+'\act', oXml, 'act', oFocus.act, true);
  WriteEncounter(sPath+'\encounter', oXml, 'encounter', oFocus.encounter, true);
  WriteObservation(sPath+'\observation', oXml, 'observation', oFocus.observation, true);
  WriteObservationMedia(sPath+'\observationMedia', oXml, 'observationMedia', oFocus.observationMedia, true);
  WriteOrganizer(sPath+'\organizer', oXml, 'organizer', oFocus.organizer, true);
  WriteProcedure(sPath+'\procedure', oXml, 'procedure', oFocus.procedure_, true);
  WriteRegionOfInterest(sPath+'\regionOfInterest', oXml, 'regionOfInterest', oFocus.regionOfInterest, true);
  WriteSubstanceAdministration(sPath+'\substanceAdministration', oXml, 'substanceAdministration', oFocus.substanceAdministration, true);
  WriteSupply(sPath+'\supply', oXml, 'supply', oFocus.supply, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteEntryRelationship(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEntryRelationship; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'EntryRelationship is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, false);
  If oFocus.HasinversionInd Then
    Attribute(sPath, oXml, 'inversionInd', WriteBoolean(sPath, oFocus.inversionInd), true);
  If oFocus.HascontextConductionInd Then
    Attribute(sPath, oXml, 'contextConductionInd', WriteBoolean(sPath, oFocus.contextConductionInd), 'true');
  If oFocus.HasnegationInd Then
    Attribute(sPath, oXml, 'negationInd', WriteBoolean(sPath, oFocus.negationInd), true);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteINT(sPath+'\sequenceNumber', oXml, 'sequenceNumber', oFocus.sequenceNumber, true);
  WriteBL(sPath+'\seperatableInd', oXml, 'seperatableInd', oFocus.seperatableInd, true);
  WriteAct(sPath+'\act', oXml, 'act', oFocus.act, true);
  WriteEncounter(sPath+'\encounter', oXml, 'encounter', oFocus.encounter, true);
  WriteObservation(sPath+'\observation', oXml, 'observation', oFocus.observation, true);
  WriteObservationMedia(sPath+'\observationMedia', oXml, 'observationMedia', oFocus.observationMedia, true);
  WriteOrganizer(sPath+'\organizer', oXml, 'organizer', oFocus.organizer, true);
  WriteProcedure(sPath+'\procedure', oXml, 'procedure', oFocus.procedure_, true);
  WriteRegionOfInterest(sPath+'\regionOfInterest', oXml, 'regionOfInterest', oFocus.regionOfInterest, true);
  WriteSubstanceAdministration(sPath+'\substanceAdministration', oXml, 'substanceAdministration', oFocus.substanceAdministration, true);
  WriteSupply(sPath+'\supply', oXml, 'supply', oFocus.supply, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteExternalAct(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaExternalAct; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ExternalAct is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ACT');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteExternalDocument(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaExternalDocument; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ExternalDocument is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'DOC');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteII(sPath+'\setId', oXml, 'setId', oFocus.setId, true);
  WriteINT(sPath+'\versionNumber', oXml, 'versionNumber', oFocus.versionNumber, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteExternalObservation(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaExternalObservation; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ExternalObservation is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'OBS');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteExternalProcedure(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaExternalProcedure; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ExternalProcedure is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'PROC');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteGuardian(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaGuardian; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Guardian is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'GUARD');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  WritePerson(sPath+'\guardianPerson', oXml, 'guardianPerson', oFocus.guardianPerson, true);
  WriteOrganization(sPath+'\guardianOrganization', oXml, 'guardianOrganization', oFocus.guardianOrganization, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteHealthCareFacility(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaHealthCareFacility; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'HealthCareFacility is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'SDLOC');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WritePlace(sPath+'\location', oXml, 'location', oFocus.location, true);
  WriteOrganization(sPath+'\serviceProviderOrganization', oXml, 'serviceProviderOrganization', oFocus.serviceProviderOrganization, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteInformant12(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaInformant12; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Informant12 is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'INF');
  Attribute(sPath, oXml, 'contextControlCode', oFocus.contextControlCode, 'OP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteAssignedEntity(sPath+'\assignedEntity', oXml, 'assignedEntity', oFocus.assignedEntity, true);
  WriteRelatedEntity(sPath+'\relatedEntity', oXml, 'relatedEntity', oFocus.relatedEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteInformationRecipient(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaInformationRecipient; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'InformationRecipient is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'PRCP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteIntendedRecipient(sPath+'\intendedRecipient', oXml, 'intendedRecipient', oFocus.intendedRecipient, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteInFulfillmentOf(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaInFulfillmentOf; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'InFulfillmentOf is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'FLFS');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteOrder(sPath+'\order', oXml, 'order', oFocus.order, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteIntendedRecipient(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaIntendedRecipient; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'IntendedRecipient is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ASSIGNED');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  WritePerson(sPath+'\informationRecipient', oXml, 'informationRecipient', oFocus.informationRecipient, true);
  WriteOrganization(sPath+'\receivedOrganization', oXml, 'receivedOrganization', oFocus.receivedOrganization, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteLabeledDrug(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaLabeledDrug; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'LabeledDrug is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'MMAT');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'KIND');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteEN(sPath+'\name', oXml, 'name', oFocus.name, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteLanguageCommunication(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaLanguageCommunication; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'LanguageCommunication is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCS(sPath+'\languageCode', oXml, 'languageCode', oFocus.languageCode, true);
  WriteCD(sPath+'\modeCode', oXml, 'modeCode', oFocus.modeCode, true);
  WriteCD(sPath+'\proficiencyLevelCode', oXml, 'proficiencyLevelCode', oFocus.proficiencyLevelCode, true);
  WriteBL(sPath+'\preferenceInd', oXml, 'preferenceInd', oFocus.preferenceInd, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteEntityIdentifier(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaEntityIdentifier; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'EntityIdentifier is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteII(sPath+'\id', oXml, 'id', oFocus.id, false);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteLegalAuthenticator(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaLegalAuthenticator; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'LegalAuthenticator is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'LA');
  Attribute(sPath, oXml, 'contextControlCode', oFocus.contextControlCode, 'OP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteTS(sPath+'\time', oXml, 'time', oFocus.time, true);
  WriteCS(sPath+'\signatureCode', oXml, 'signatureCode', oFocus.signatureCode, true);
  WriteAssignedEntity(sPath+'\assignedEntity', oXml, 'assignedEntity', oFocus.assignedEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteLocation(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaLocation; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Location is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'LOC');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteHealthCareFacility(sPath+'\healthCareFacility', oXml, 'healthCareFacility', oFocus.healthCareFacility, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteMaintainedEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaMaintainedEntity; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'MaintainedEntity is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'MNT');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  WritePerson(sPath+'\maintainingPerson', oXml, 'maintainingPerson', oFocus.maintainingPerson, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteManufacturedProduct(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaManufacturedProduct; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ManufacturedProduct is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'MANU');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteLabeledDrug(sPath+'\manufacturedLabeledDrug', oXml, 'manufacturedLabeledDrug', oFocus.manufacturedLabeledDrug, true);
  WriteMaterial(sPath+'\manufacturedMaterial', oXml, 'manufacturedMaterial', oFocus.manufacturedMaterial, true);
  WriteOrganization(sPath+'\manufacturerOrganization', oXml, 'manufacturerOrganization', oFocus.manufacturerOrganization, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteMaterial(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaMaterial; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Material is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'MMAT');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'KIND');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteEN(sPath+'\name', oXml, 'name', oFocus.name, true);
  WriteST(sPath+'\lotNumberText', oXml, 'lotNumberText', oFocus.lotNumberText, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteNonXMLBody(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaNonXMLBody; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'NonXMLBody is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'DOCBODY');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteCD(sPath+'\confidentialityCode', oXml, 'confidentialityCode', oFocus.confidentialityCode, true);
  WriteCS(sPath+'\languageCode', oXml, 'languageCode', oFocus.languageCode, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteObservation(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaObservation; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Observation is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, false);
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, false);
  If oFocus.HasnegationInd Then
    Attribute(sPath, oXml, 'negationInd', WriteBoolean(sPath, oFocus.negationInd), true);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteST(sPath+'\derivationExpr', oXml, 'derivationExpr', oFocus.derivationExpr, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteCS(sPath+'\statusCode', oXml, 'statusCode', oFocus.statusCode, true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  WriteCD(sPath+'\priorityCode', oXml, 'priorityCode', oFocus.priorityCode, true);
  WriteIVL(sPath+'\repeatNumber', oXml, 'repeatNumber', oFocus.repeatNumber, true, Tv3INT);
  WriteCS(sPath+'\languageCode', oXml, 'languageCode', oFocus.languageCode, true);
  if oFocus.value <> nil Then
    For iLoop := 0 to oFocus.value.Count - 1 Do
      WriteANY(sPath+'\value', oXml, 'value', oFocus.value[iLoop], true);
  if oFocus.interpretationCode <> nil Then
    For iLoop := 0 to oFocus.interpretationCode.Count - 1 Do
      WriteCD(sPath+'\interpretationCode', oXml, 'interpretationCode', oFocus.interpretationCode[iLoop], true);
  if oFocus.methodCode <> nil Then
    For iLoop := 0 to oFocus.methodCode.Count - 1 Do
      WriteCD(sPath+'\methodCode', oXml, 'methodCode', oFocus.methodCode[iLoop], true);
  if oFocus.targetSiteCode <> nil Then
    For iLoop := 0 to oFocus.targetSiteCode.Count - 1 Do
      WriteCD(sPath+'\targetSiteCode', oXml, 'targetSiteCode', oFocus.targetSiteCode[iLoop], true);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.specimen <> nil Then
    For iLoop := 0 to oFocus.specimen.Count - 1 Do
      WriteSpecimen(sPath+'\specimen', oXml, 'specimen', oFocus.specimen[iLoop], true);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer2(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant2(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.entryRelationship <> nil Then
    For iLoop := 0 to oFocus.entryRelationship.Count - 1 Do
      WriteEntryRelationship(sPath+'\entryRelationship', oXml, 'entryRelationship', oFocus.entryRelationship[iLoop], true);
  if oFocus.reference <> nil Then
    For iLoop := 0 to oFocus.reference.Count - 1 Do
      WriteReference(sPath+'\reference', oXml, 'reference', oFocus.reference[iLoop], true);
  if oFocus.precondition <> nil Then
    For iLoop := 0 to oFocus.precondition.Count - 1 Do
      WritePrecondition(sPath+'\precondition', oXml, 'precondition', oFocus.precondition[iLoop], true);
  if oFocus.referenceRange <> nil Then
    For iLoop := 0 to oFocus.referenceRange.Count - 1 Do
      WriteReferenceRange(sPath+'\referenceRange', oXml, 'referenceRange', oFocus.referenceRange[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteObservationMedia(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaObservationMedia; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ObservationMedia is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'ID', oFocus.ID_, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, false);
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCS(sPath+'\languageCode', oXml, 'languageCode', oFocus.languageCode, true);
  WriteED(sPath+'\value', oXml, 'value', oFocus.value, true);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.specimen <> nil Then
    For iLoop := 0 to oFocus.specimen.Count - 1 Do
      WriteSpecimen(sPath+'\specimen', oXml, 'specimen', oFocus.specimen[iLoop], true);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer2(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant2(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.entryRelationship <> nil Then
    For iLoop := 0 to oFocus.entryRelationship.Count - 1 Do
      WriteEntryRelationship(sPath+'\entryRelationship', oXml, 'entryRelationship', oFocus.entryRelationship[iLoop], true);
  if oFocus.reference <> nil Then
    For iLoop := 0 to oFocus.reference.Count - 1 Do
      WriteReference(sPath+'\reference', oXml, 'reference', oFocus.reference[iLoop], true);
  if oFocus.precondition <> nil Then
    For iLoop := 0 to oFocus.precondition.Count - 1 Do
      WritePrecondition(sPath+'\precondition', oXml, 'precondition', oFocus.precondition[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteObservationRange(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaObservationRange; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ObservationRange is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'OBS');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN.CRT');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteANY(sPath+'\value', oXml, 'value', oFocus.value, true);
  WriteCD(sPath+'\interpretationCode', oXml, 'interpretationCode', oFocus.interpretationCode, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteOrder(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaOrder; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Order is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ACT');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'RQO');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteCD(sPath+'\priorityCode', oXml, 'priorityCode', oFocus.priorityCode, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteOrganization(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaOrganization; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Organization is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ORG');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  if oFocus.name <> nil Then
    For iLoop := 0 to oFocus.name.Count - 1 Do
      WriteEN(sPath+'\name', oXml, 'name', oFocus.name[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr[iLoop], true);
  WriteCD(sPath+'\standardIndustryClassCode', oXml, 'standardIndustryClassCode', oFocus.standardIndustryClassCode, true);
  WriteOrganizationPartOf(sPath+'\asOrganizationPartOf', oXml, 'asOrganizationPartOf', oFocus.asOrganizationPartOf, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteOrganizationPartOf(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaOrganizationPartOf; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'OrganizationPartOf is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'PART');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteCS(sPath+'\statusCode', oXml, 'statusCode', oFocus.statusCode, true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  WriteOrganization(sPath+'\wholeOrganization', oXml, 'wholeOrganization', oFocus.wholeOrganization, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteOrganizer(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaOrganizer; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Organizer is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, false);
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteCS(sPath+'\statusCode', oXml, 'statusCode', oFocus.statusCode, true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.specimen <> nil Then
    For iLoop := 0 to oFocus.specimen.Count - 1 Do
      WriteSpecimen(sPath+'\specimen', oXml, 'specimen', oFocus.specimen[iLoop], true);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer2(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant2(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.reference <> nil Then
    For iLoop := 0 to oFocus.reference.Count - 1 Do
      WriteReference(sPath+'\reference', oXml, 'reference', oFocus.reference[iLoop], true);
  if oFocus.precondition <> nil Then
    For iLoop := 0 to oFocus.precondition.Count - 1 Do
      WritePrecondition(sPath+'\precondition', oXml, 'precondition', oFocus.precondition[iLoop], true);
  if oFocus.component <> nil Then
    For iLoop := 0 to oFocus.component.Count - 1 Do
      WriteComponent4(sPath+'\component', oXml, 'component', oFocus.component[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteParentDocument(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaParentDocument; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ParentDocument is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'DOCCLIN');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteII(sPath+'\setId', oXml, 'setId', oFocus.setId, true);
  WriteINT(sPath+'\versionNumber', oXml, 'versionNumber', oFocus.versionNumber, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteParticipant1(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaParticipant1; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Participant1 is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, false);
  Attribute(sPath, oXml, 'contextControlCode', oFocus.contextControlCode, 'OP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\functionCode', oXml, 'functionCode', oFocus.functionCode, true);
  WriteIVL(sPath+'\time', oXml, 'time', oFocus.time, true, Tv3TS);
  WriteAssociatedEntity(sPath+'\associatedEntity', oXml, 'associatedEntity', oFocus.associatedEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteParticipant2(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaParticipant2; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Participant2 is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, false);
  Attribute(sPath, oXml, 'contextControlCode', oFocus.contextControlCode, 'OP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteIVL(sPath+'\time', oXml, 'time', oFocus.time, true, Tv3TS);
  WriteCD(sPath+'\awarenessCode', oXml, 'awarenessCode', oFocus.awarenessCode, true);
  WriteParticipantRole(sPath+'\participantRole', oXml, 'participantRole', oFocus.participantRole, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteParticipantRole(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaParticipantRole; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ParticipantRole is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ROL');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  WriteDevice(sPath+'\playingDevice', oXml, 'playingDevice', oFocus.playingDevice, true);
  WritePlayingEntity(sPath+'\playingEntity', oXml, 'playingEntity', oFocus.playingEntity, true);
  WriteEntity(sPath+'\scopingEntity', oXml, 'scopingEntity', oFocus.scopingEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WritePatient(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPatient; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Patient is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'PSN');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteII(sPath+'\id', oXml, 'id', oFocus.id, true);
  if oFocus.name <> nil Then
    For iLoop := 0 to oFocus.name.Count - 1 Do
      WriteEN(sPath+'\name', oXml, 'name', oFocus.name[iLoop], true);
  WriteCD(sPath+'\administrativeGenderCode', oXml, 'administrativeGenderCode', oFocus.administrativeGenderCode, true);
  WriteTS(sPath+'\birthTime', oXml, 'birthTime', oFocus.birthTime, true);
  WriteCD(sPath+'\maritalStatusCode', oXml, 'maritalStatusCode', oFocus.maritalStatusCode, true);
  WriteCD(sPath+'\religiousAffiliationCode', oXml, 'religiousAffiliationCode', oFocus.religiousAffiliationCode, true);
  WriteCD(sPath+'\raceCode', oXml, 'raceCode', oFocus.raceCode, true);
  WriteCD(sPath+'\ethnicGroupCode', oXml, 'ethnicGroupCode', oFocus.ethnicGroupCode, true);
  if oFocus.guardian <> nil Then
    For iLoop := 0 to oFocus.guardian.Count - 1 Do
      WriteGuardian(sPath+'\guardian', oXml, 'guardian', oFocus.guardian[iLoop], true);
  WriteBirthplace(sPath+'\birthplace', oXml, 'birthplace', oFocus.birthplace, true);
  if oFocus.languageCommunication <> nil Then
    For iLoop := 0 to oFocus.languageCommunication.Count - 1 Do
      WriteLanguageCommunication(sPath+'\languageCommunication', oXml, 'languageCommunication', oFocus.languageCommunication[iLoop], true);
  if oFocus.asEntityIdentifier <> nil Then
    For iLoop := 0 to oFocus.asEntityIdentifier.Count - 1 Do
      WriteEntityIdentifier(sPath+'\asEntityIdentifier', oXml, 'asEntityIdentifier', oFocus.asEntityIdentifier[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WritePatientRole(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPatientRole; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'PatientRole is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'PAT');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  WritePatient(sPath+'\patient', oXml, 'patient', oFocus.patient, true);
  WriteOrganization(sPath+'\providerOrganization', oXml, 'providerOrganization', oFocus.providerOrganization, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WritePerformer1(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPerformer1; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Performer1 is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\functionCode', oXml, 'functionCode', oFocus.functionCode, true);
  WriteIVL(sPath+'\time', oXml, 'time', oFocus.time, true, Tv3TS);
  WriteAssignedEntity(sPath+'\assignedEntity', oXml, 'assignedEntity', oFocus.assignedEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WritePerformer2(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPerformer2; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Performer2 is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'PRF');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteIVL(sPath+'\time', oXml, 'time', oFocus.time, true, Tv3TS);
  WriteCD(sPath+'\modeCode', oXml, 'modeCode', oFocus.modeCode, true);
  WriteAssignedEntity(sPath+'\assignedEntity', oXml, 'assignedEntity', oFocus.assignedEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WritePerson(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPerson; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Person is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'PSN');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.name <> nil Then
    For iLoop := 0 to oFocus.name.Count - 1 Do
      WriteEN(sPath+'\name', oXml, 'name', oFocus.name[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WritePlace(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPlace; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Place is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'PLC');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteEN(sPath+'\name', oXml, 'name', oFocus.name, true);
  WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WritePlayingEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPlayingEntity; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'PlayingEntity is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ENT');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  if oFocus.quantity <> nil Then
    For iLoop := 0 to oFocus.quantity.Count - 1 Do
      WritePQ(sPath+'\quantity', oXml, 'quantity', oFocus.quantity[iLoop], true);
  if oFocus.name <> nil Then
    For iLoop := 0 to oFocus.name.Count - 1 Do
      WriteEN(sPath+'\name', oXml, 'name', oFocus.name[iLoop], true);
  WriteED(sPath+'\desc', oXml, 'desc', oFocus.desc, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WritePrecondition(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaPrecondition; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Precondition is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'PRCN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCriterion(sPath+'\criterion', oXml, 'criterion', oFocus.criterion, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteProcedure(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaProcedure; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Procedure is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, false);
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, false);
  If oFocus.HasnegationInd Then
    Attribute(sPath, oXml, 'negationInd', WriteBoolean(sPath, oFocus.negationInd), true);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteCS(sPath+'\statusCode', oXml, 'statusCode', oFocus.statusCode, true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  WriteCD(sPath+'\priorityCode', oXml, 'priorityCode', oFocus.priorityCode, true);
  WriteCS(sPath+'\languageCode', oXml, 'languageCode', oFocus.languageCode, true);
  if oFocus.methodCode <> nil Then
    For iLoop := 0 to oFocus.methodCode.Count - 1 Do
      WriteCD(sPath+'\methodCode', oXml, 'methodCode', oFocus.methodCode[iLoop], true);
  if oFocus.approachSiteCode <> nil Then
    For iLoop := 0 to oFocus.approachSiteCode.Count - 1 Do
      WriteCD(sPath+'\approachSiteCode', oXml, 'approachSiteCode', oFocus.approachSiteCode[iLoop], true);
  if oFocus.targetSiteCode <> nil Then
    For iLoop := 0 to oFocus.targetSiteCode.Count - 1 Do
      WriteCD(sPath+'\targetSiteCode', oXml, 'targetSiteCode', oFocus.targetSiteCode[iLoop], true);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.specimen <> nil Then
    For iLoop := 0 to oFocus.specimen.Count - 1 Do
      WriteSpecimen(sPath+'\specimen', oXml, 'specimen', oFocus.specimen[iLoop], true);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer2(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant2(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.entryRelationship <> nil Then
    For iLoop := 0 to oFocus.entryRelationship.Count - 1 Do
      WriteEntryRelationship(sPath+'\entryRelationship', oXml, 'entryRelationship', oFocus.entryRelationship[iLoop], true);
  if oFocus.reference <> nil Then
    For iLoop := 0 to oFocus.reference.Count - 1 Do
      WriteReference(sPath+'\reference', oXml, 'reference', oFocus.reference[iLoop], true);
  if oFocus.precondition <> nil Then
    For iLoop := 0 to oFocus.precondition.Count - 1 Do
      WritePrecondition(sPath+'\precondition', oXml, 'precondition', oFocus.precondition[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteProduct(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaProduct; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Product is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'PRD');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteManufacturedProduct(sPath+'\manufacturedProduct', oXml, 'manufacturedProduct', oFocus.manufacturedProduct, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteRecordTarget(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRecordTarget; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'RecordTarget is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'RCT');
  Attribute(sPath, oXml, 'contextControlCode', oFocus.contextControlCode, 'OP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WritePatientRole(sPath+'\patientRole', oXml, 'patientRole', oFocus.patientRole, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteReference(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaReference; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Reference is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteBL(sPath+'\seperatableInd', oXml, 'seperatableInd', oFocus.seperatableInd, true);
  WriteExternalAct(sPath+'\externalAct', oXml, 'externalAct', oFocus.externalAct, true);
  WriteExternalObservation(sPath+'\externalObservation', oXml, 'externalObservation', oFocus.externalObservation, true);
  WriteExternalProcedure(sPath+'\externalProcedure', oXml, 'externalProcedure', oFocus.externalProcedure, true);
  WriteExternalDocument(sPath+'\externalDocument', oXml, 'externalDocument', oFocus.externalDocument, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteReferenceRange(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaReferenceRange; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ReferenceRange is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'REFV');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteObservationRange(sPath+'\observationRange', oXml, 'observationRange', oFocus.observationRange, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteRegionOfInterest(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRegionOfInterest; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'RegionOfInterest is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'ID', oFocus.ID_, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ROIOVL');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCS(sPath+'\code', oXml, 'code', oFocus.code, true);
  if oFocus.value <> nil Then
    For iLoop := 0 to oFocus.value.Count - 1 Do
      WriteRegionOfInterestValue(sPath+'\value', oXml, 'value', oFocus.value[iLoop], true);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.specimen <> nil Then
    For iLoop := 0 to oFocus.specimen.Count - 1 Do
      WriteSpecimen(sPath+'\specimen', oXml, 'specimen', oFocus.specimen[iLoop], true);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer2(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant2(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.entryRelationship <> nil Then
    For iLoop := 0 to oFocus.entryRelationship.Count - 1 Do
      WriteEntryRelationship(sPath+'\entryRelationship', oXml, 'entryRelationship', oFocus.entryRelationship[iLoop], true);
  if oFocus.reference <> nil Then
    For iLoop := 0 to oFocus.reference.Count - 1 Do
      WriteReference(sPath+'\reference', oXml, 'reference', oFocus.reference[iLoop], true);
  if oFocus.precondition <> nil Then
    For iLoop := 0 to oFocus.precondition.Count - 1 Do
      WritePrecondition(sPath+'\precondition', oXml, 'precondition', oFocus.precondition[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteRelatedDocument(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRelatedDocument; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'RelatedDocument is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteParentDocument(sPath+'\parentDocument', oXml, 'parentDocument', oFocus.parentDocument, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteRelatedEntity(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRelatedEntity; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'RelatedEntity is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  WritePerson(sPath+'\relatedPerson', oXml, 'relatedPerson', oFocus.relatedPerson, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteRelatedSubject(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaRelatedSubject; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'RelatedSubject is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'PRS');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  if oFocus.addr <> nil Then
    For iLoop := 0 to oFocus.addr.Count - 1 Do
      WriteAD(sPath+'\addr', oXml, 'addr', oFocus.addr[iLoop], true);
  if oFocus.telecom <> nil Then
    For iLoop := 0 to oFocus.telecom.Count - 1 Do
      WriteTEL(sPath+'\telecom', oXml, 'telecom', oFocus.telecom[iLoop], true);
  WriteSubjectPerson(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteResponsibleParty(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaResponsibleParty; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ResponsibleParty is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'RESP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteAssignedEntity(sPath+'\assignedEntity', oXml, 'assignedEntity', oFocus.assignedEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteSection(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSection; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Section is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'ID', oFocus.ID_, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'DOCSECT');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteII(sPath+'\id', oXml, 'id', oFocus.id, true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteST(sPath+'\title', oXml, 'title', oFocus.title, true);
  WriteSNText(sPath+'\text', oXml, 'text', oFocus.text);
  WriteCD(sPath+'\confidentialityCode', oXml, 'confidentialityCode', oFocus.confidentialityCode, true);
  WriteCS(sPath+'\languageCode', oXml, 'languageCode', oFocus.languageCode, true);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.entry <> nil Then
    For iLoop := 0 to oFocus.entry.Count - 1 Do
      WriteEntry(sPath+'\entry', oXml, 'entry', oFocus.entry[iLoop], true);
  if oFocus.component <> nil Then
    For iLoop := 0 to oFocus.component.Count - 1 Do
      WriteComponentSect(sPath+'\component', oXml, 'component', oFocus.component[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteServiceEvent(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaServiceEvent; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'ServiceEvent is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'ACT');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteIVL(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true, Tv3TS);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer1(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteSpecimen(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSpecimen; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Specimen is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'SPC');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteSpecimenRole(sPath+'\specimenRole', oXml, 'specimenRole', oFocus.specimenRole, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteSpecimenRole(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSpecimenRole; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'SpecimenRole is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'SPEC');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WritePlayingEntity(sPath+'\specimenPlayingEntity', oXml, 'specimenPlayingEntity', oFocus.specimenPlayingEntity, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteStructuredBody(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaStructuredBody; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'StructuredBody is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'DOCBODY');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, 'EVN');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\confidentialityCode', oXml, 'confidentialityCode', oFocus.confidentialityCode, true);
  WriteCS(sPath+'\languageCode', oXml, 'languageCode', oFocus.languageCode, true);
  if oFocus.component <> nil Then
    For iLoop := 0 to oFocus.component.Count - 1 Do
      WriteComponentSect(sPath+'\component', oXml, 'component', oFocus.component[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteSubject(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSubject; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Subject is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'typeCode', oFocus.typeCode, 'SBJ');
  Attribute(sPath, oXml, 'contextControlCode', oFocus.contextControlCode, 'OP');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  WriteCD(sPath+'\awarenessCode', oXml, 'awarenessCode', oFocus.awarenessCode, true);
  WriteRelatedSubject(sPath+'\relatedSubject', oXml, 'relatedSubject', oFocus.relatedSubject, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteSubjectPerson(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSubjectPerson; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'SubjectPerson is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'PSN');
  Attribute(sPath, oXml, 'determinerCode', oFocus.determinerCode, 'INSTANCE');
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.name <> nil Then
    For iLoop := 0 to oFocus.name.Count - 1 Do
      WriteEN(sPath+'\name', oXml, 'name', oFocus.name[iLoop], true);
  WriteCD(sPath+'\administrativeGenderCode', oXml, 'administrativeGenderCode', oFocus.administrativeGenderCode, true);
  WriteTS(sPath+'\birthTime', oXml, 'birthTime', oFocus.birthTime, true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteSubstanceAdministration(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSubstanceAdministration; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'SubstanceAdministration is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'SBADM');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, false);
  If oFocus.HasnegationInd Then
    Attribute(sPath, oXml, 'negationInd', WriteBoolean(sPath, oFocus.negationInd), true);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteCS(sPath+'\statusCode', oXml, 'statusCode', oFocus.statusCode, true);
  WriteGTS(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true);
  WriteCD(sPath+'\priorityCode', oXml, 'priorityCode', oFocus.priorityCode, true);
  WriteIVL(sPath+'\repeatNumber', oXml, 'repeatNumber', oFocus.repeatNumber, true, Tv3INT);
  WriteCD(sPath+'\routeCode', oXml, 'routeCode', oFocus.routeCode, true);
  if oFocus.approachSiteCode <> nil Then
    For iLoop := 0 to oFocus.approachSiteCode.Count - 1 Do
      WriteCD(sPath+'\approachSiteCode', oXml, 'approachSiteCode', oFocus.approachSiteCode[iLoop], true);
  WriteIVL(sPath+'\doseQuantity', oXml, 'doseQuantity', oFocus.doseQuantity, true, Tv3PQ);
  WriteIVL(sPath+'\rateQuantity', oXml, 'rateQuantity', oFocus.rateQuantity, true, Tv3PQ);
  WriteRTO(sPath+'\maxDoseQuantity', oXml, 'maxDoseQuantity', oFocus.maxDoseQuantity, true, Tv3PQ, Tv3PQ);
  WriteCD(sPath+'\administrationUnitCode', oXml, 'administrationUnitCode', oFocus.administrationUnitCode, true);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.specimen <> nil Then
    For iLoop := 0 to oFocus.specimen.Count - 1 Do
      WriteSpecimen(sPath+'\specimen', oXml, 'specimen', oFocus.specimen[iLoop], true);
  WriteConsumable(sPath+'\consumable', oXml, 'consumable', oFocus.consumable, true);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer2(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant2(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.entryRelationship <> nil Then
    For iLoop := 0 to oFocus.entryRelationship.Count - 1 Do
      WriteEntryRelationship(sPath+'\entryRelationship', oXml, 'entryRelationship', oFocus.entryRelationship[iLoop], true);
  if oFocus.reference <> nil Then
    For iLoop := 0 to oFocus.reference.Count - 1 Do
      WriteReference(sPath+'\reference', oXml, 'reference', oFocus.reference[iLoop], true);
  if oFocus.precondition <> nil Then
    For iLoop := 0 to oFocus.precondition.Count - 1 Do
      WritePrecondition(sPath+'\precondition', oXml, 'precondition', oFocus.precondition[iLoop], true);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteSupply(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TcdaSupply; bOptional : Boolean);
var
  iLoop : Integer;
Begin
  if oFocus = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', 'Supply is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteComments(oXml, oFocus);

  Attribute(sPath, oXml, 'xml:id', oFocus.xmlId, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'classCode', oFocus.classCode, 'SPLY');
  Attribute(sPath, oXml, 'moodCode', oFocus.moodCode, false);
  oFocus.sourcelocation := oXml.Open(sName);
  if oFocus.realmCode <> nil Then
    For iLoop := 0 to oFocus.realmCode.Count - 1 Do
      WriteCS(sPath+'\realmCode', oXml, 'realmCode', oFocus.realmCode[iLoop], true);
  WriteII(sPath+'\typeId', oXml, 'typeId', oFocus.typeId, true);
  if oFocus.templateId <> nil Then
    For iLoop := 0 to oFocus.templateId.Count - 1 Do
      WriteII(sPath+'\templateId', oXml, 'templateId', oFocus.templateId[iLoop], true);
  if oFocus.id <> nil Then
    For iLoop := 0 to oFocus.id.Count - 1 Do
      WriteII(sPath+'\id', oXml, 'id', oFocus.id[iLoop], true);
  WriteCD(sPath+'\code', oXml, 'code', oFocus.code, true);
  WriteED(sPath+'\text', oXml, 'text', oFocus.text, true);
  WriteCS(sPath+'\statusCode', oXml, 'statusCode', oFocus.statusCode, true);
  WriteGTS(sPath+'\effectiveTime', oXml, 'effectiveTime', oFocus.effectiveTime, true);
  if oFocus.priorityCode <> nil Then
    For iLoop := 0 to oFocus.priorityCode.Count - 1 Do
      WriteCD(sPath+'\priorityCode', oXml, 'priorityCode', oFocus.priorityCode[iLoop], true);
  WriteIVL(sPath+'\repeatNumber', oXml, 'repeatNumber', oFocus.repeatNumber, true, Tv3INT);
  WriteBL(sPath+'\independentInd', oXml, 'independentInd', oFocus.independentInd, true);
  WritePQ(sPath+'\quantity', oXml, 'quantity', oFocus.quantity, true);
  WriteIVL(sPath+'\expectedUseTime', oXml, 'expectedUseTime', oFocus.expectedUseTime, true, Tv3TS);
  WriteSubject(sPath+'\subject', oXml, 'subject', oFocus.subject, true);
  if oFocus.specimen <> nil Then
    For iLoop := 0 to oFocus.specimen.Count - 1 Do
      WriteSpecimen(sPath+'\specimen', oXml, 'specimen', oFocus.specimen[iLoop], true);
  WriteProduct(sPath+'\product', oXml, 'product', oFocus.product, true);
  if oFocus.performer <> nil Then
    For iLoop := 0 to oFocus.performer.Count - 1 Do
      WritePerformer2(sPath+'\performer', oXml, 'performer', oFocus.performer[iLoop], true);
  if oFocus.author <> nil Then
    For iLoop := 0 to oFocus.author.Count - 1 Do
      WriteAuthor(sPath+'\author', oXml, 'author', oFocus.author[iLoop], true);
  if oFocus.informant <> nil Then
    For iLoop := 0 to oFocus.informant.Count - 1 Do
      WriteInformant12(sPath+'\informant', oXml, 'informant', oFocus.informant[iLoop], true);
  if oFocus.participant <> nil Then
    For iLoop := 0 to oFocus.participant.Count - 1 Do
      WriteParticipant2(sPath+'\participant', oXml, 'participant', oFocus.participant[iLoop], true);
  if oFocus.entryRelationship <> nil Then
    For iLoop := 0 to oFocus.entryRelationship.Count - 1 Do
      WriteEntryRelationship(sPath+'\entryRelationship', oXml, 'entryRelationship', oFocus.entryRelationship[iLoop], true);
  if oFocus.reference <> nil Then
    For iLoop := 0 to oFocus.reference.Count - 1 Do
      WriteReference(sPath+'\reference', oXml, 'reference', oFocus.reference[iLoop], true);
  if oFocus.precondition <> nil Then
    For iLoop := 0 to oFocus.precondition.Count - 1 Do
      WritePrecondition(sPath+'\precondition', oXml, 'precondition', oFocus.precondition[iLoop], true);
  oXml.Close(sName);
End;



procedure TCDAWriter.WriteSNText(Const sPath: string; oXml: TXmlBuilder; const sName: string; oFocus: TsnText);
var
  sText : string;
  iLoop : Integer;
begin
  if oFocus = nil Then
    Exit;

  Attribute(sPath, oXml, 'ID', oFocus.ID, true);
  Attribute(sPath, oXml, 'nullFlavor', WriteNullFlavor(sPath, oFocus.nullFlavor), true);
  Attribute(sPath, oXml, 'language', oFocus.language, true);
  Attribute(sPath, oXml, 'mediaType', 'text/x-hl7-text+xml', 'text/x-hl7-text+xml');

  sText := '';
  For iLoop := 0 to oFocus.styleCode.Count - 1 Do
    sText := sText + ' ' + oFocus.styleCode[iLoop];
  Attribute(sPath, oXml, 'styleCode', stringTrimWhitespace(sText), true);

  oFocus.sourcelocation := oXml.Open(sName);
  WriteCMGeneralList(sPath, oXml, oFocus.parts);
  oXml.Close(sName);
end;

procedure TCDAWriter.WriteCMGeneralList(Const sPath: string; oXml: TXmlBuilder; oParts: TsnCMGeneralList);
var
  iLoop : Integer;
  oPart : TsnCMGeneral;
begin
  for iLoop := 0 to oParts.Count - 1 Do
  Begin
    oPart := oParts[iLoop];
    if oPart.text <> nil Then
      WriteSNstring(sPath+'\[text]', oXml, oPart.text)
    else if oPart.entity <> nil Then
      WriteSNEntity(sPath+'\[entity]', oXml, oPart.entity)
    else if oPart.content <> Nil Then
      WriteSNContent(sPath+'\content', oXml, oPart.content)
    else if oPart.linkHtml <> Nil Then
      WriteSNLinkHtml(sPath+'\linkHtml', oXml, oPart.linkHtml)
    else if oPart.footnote <> Nil Then
      WriteSNFootnote(sPath+'\footnote', oXml, oPart.footnote)
    else if oPart.footnoteRef <> Nil Then
      WriteSNFootnoteRef(sPath+'\footnoteRef', oXml, oPart.footnoteRef)
    else if oPart.paragraph <> Nil Then
      WriteSNParagraph(sPath+'\paragraph', oXml, oPart.paragraph)
    else if oPart.list <> Nil Then
      WriteSNList(sPath+'\list', oXml, oPart.list)
    else if oPart.table <> Nil Then
      WriteSNTable(sPath+'\table', oXml, oPart.table)
    else if oPart.renderMultiMedia <> Nil Then
      WriteSNRenderMultiMedia(sPath+'\renderMultiMedia', oXml, oPart.renderMultiMedia)
    else if oPart.sub <> nil Then
      WriteSNSub(sPath+'\sub', oXml, oPart.sub)
    else if oPart.sup <> nil Then
      WriteSNSup(sPath+'\sup', oXml, oPart.sup)
    else if oPart.br <> nil Then
      WriteSNBr(sPath+'\br', oXml, oPart.br)
    else If Errors Then
      RaiseError('Parse', 'CMGeneral Part has no value'+' @'+sPath);
  End;
end;

procedure TCDAWriter.WriteCMInlineList(Const sPath: string; oXml: TXmlBuilder; oParts: TsnCMInlineList);
var
  iLoop : Integer;
  oPart : TsnCMInline;
begin
  for iLoop := 0 to oParts.Count - 1 Do
  Begin
    oPart := oParts[iLoop];
    if oPart.text <> nil Then
      WriteSNstring(sPath+'\[text]', oXml, oPart.text)
    else if oPart.entity <> nil Then
      WriteSNEntity(sPath+'\[entity]', oXml, oPart.entity)
    else if oPart.linkHtml <> Nil Then
      WriteSNLinkHtml(sPath+'\linkHtml', oXml, oPart.linkHtml)
    else if oPart.footnote <> Nil Then
      WriteSNFootnote(sPath+'\footnote', oXml, oPart.footnote)
    else if oPart.footnoteRef <> Nil Then
      WriteSNFootnoteRef(sPath+'\footnoteRef', oXml, oPart.footnoteRef)
    else if oPart.sub <> nil Then
      WriteSNSub(sPath+'\sub', oXml, oPart.sub)
    else if oPart.sup <> nil Then
      WriteSNSup(sPath+'\sup', oXml, oPart.sup)
    else If Errors Then
      RaiseError('Parse', 'CMGeneral Part has no value'+' @'+sPath);
  End;
end;

procedure TCDAWriter.WriteCMContentList(Const sPath: string; oXml: TXmlBuilder; oParts: TsnCMContentList);
var
  iLoop : Integer;
  oPart : TsnCMContent;
begin
  for iLoop := 0 to oParts.Count - 1 Do
  Begin
    oPart := oParts[iLoop];
    if oPart.text <> nil Then
      WriteSNstring(sPath+'\[text]', oXml, oPart.text)
    else if oPart.entity <> nil Then
      WriteSNEntity(sPath+'\[entity]', oXml, oPart.entity)
    else if oPart.content <> Nil Then
      WriteSNContent(sPath+'\content', oXml, oPart.content)
    else if oPart.linkHtml <> Nil Then
      WriteSNLinkHtml(sPath+'\linkHtml', oXml, oPart.linkHtml)
    else if oPart.footnote <> Nil Then
      WriteSNFootnote(sPath+'\footnote', oXml, oPart.footnote)
    else if oPart.footnoteRef <> Nil Then
      WriteSNFootnoteRef(sPath+'\footnoteRef', oXml, oPart.footnoteRef)
    else if oPart.renderMultiMedia <> Nil Then
      WriteSNRenderMultiMedia(sPath+'\renderMultiMedia', oXml, oPart.renderMultiMedia)
    else if oPart.sub <> nil Then
      WriteSNSub(sPath+'\sub', oXml, oPart.sub)
    else if oPart.sup <> nil Then
      WriteSNSup(sPath+'\sup', oXml, oPart.sup)
    else if oPart.br <> nil Then
      WriteSNBr(sPath+'\br', oXml, oPart.br)
    else If Errors Then
      RaiseError('Parse', 'CMGeneral Part has no value'+' @'+sPath);
  End;
end;

procedure TCDAWriter.WriteCMFootnotesList(Const sPath: string; oXml: TXmlBuilder; oParts: TsnCMFootnotesList);
var
  iLoop : Integer;
  oPart : TsnCMFootnotes;
begin
  for iLoop := 0 to oParts.Count - 1 Do
  Begin
    oPart := oParts[iLoop];
    if oPart.text <> Nil Then
      WriteSNstring(sPath+'\[text]', oXml, oPart.text)
    else if oPart.entity <> nil Then
      WriteSNEntity(sPath+'\[entity]', oXml, oPart.entity)
    else if oPart.footnote <> Nil Then
      WriteSNFootnote(sPath+'\footnote', oXml, oPart.footnote)
    else if oPart.footnoteRef <> Nil Then
      WriteSNFootnoteRef(sPath+'\footnoteRef', oXml, oPart.footnoteRef)
    else If Errors Then
      RaiseError('Parse', 'CMGeneral Part has no value'+' @'+sPath);
  End;
end;

Procedure TCDAWriter.WriteSNstring(Const sPath: string; oXml: TXmlBuilder; oText: Tsnstring);
begin
  if (oText <> nil) Then
    oText.sourcelocation := oXml.Text(oText.value);
end;

procedure TCDAWriter.WriteSNEntity(Const sPath: string; oXml: TXmlBuilder; oText: Tsnstring);
begin
  if (oText <> nil) and (not oXml.IsPretty Or not stringIsWhitespace(oText.value)) Then
    oText.sourcelocation := oXml.Entity(oText.value);
end;


procedure TCDAWriter.WriteSNBr(Const sPath: string; oXml: TXmlBuilder; oBr : TsnBr);
begin
  oBr.sourcelocation := oXml.Tag('br');
end;

procedure TCDAWriter.WriteSNSub(Const sPath: string; oXml: TXmlBuilder; oSub: Tsnstring);
begin
  oSub.sourcelocation := oXml.TagText('sub', oSub.value);
end;

procedure TCDAWriter.WriteSNSup(Const sPath: string; oXml: TXmlBuilder; oSup: Tsnstring);
begin
  oSup.sourcelocation := oXml.TagText('sup', oSup.value);
end;


Procedure TCDAWriter.WriteSNBase(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnBase);
var
  sText : string;
  iLoop : Integer;
begin
  Attribute(sPath, oXml, 'ID', oFocus.ID, true);
  Attribute(sPath, oXml, 'language', oFocus.language, true);
  sText := '';
  For iLoop := 0 to oFocus.styleCode.Count - 1 Do
    sText := sText + ' ' + oFocus.styleCode[iLoop];
  Attribute(sPath, oXml, 'styleCode', stringTrimWhitespace(sText), true);
End;

procedure TCDAWriter.WriteSNContent(Const sPath: string; oXml: TXmlBuilder; oFocus: TsnContent);
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  Attribute(sPath, oXml, 'revised', CODES_TsnRevised[oFocus.revised], true);
  oFocus.sourcelocation := oXml.Open('content');
  WriteCMContentList(sPath, oXml, oFocus.parts);
  oXml.Close('content');
End;


procedure TCDAWriter.WriteSNFootnote(Const sPath: string; oXml: TXmlBuilder; oFocus : TsnFootNote);
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  oFocus.sourcelocation := oXml.Open('footnote');
  WriteCMGeneralList(sPath, oXml, oFocus.parts);
  oXml.Close('footnote');
end;

procedure TCDAWriter.WriteSNFootnoteRef(Const sPath: string; oXml: TXmlBuilder; oFocus : TsnFootNoteRef);
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  Attribute(sPath, oXml, 'IDREF', oFocus.IDREF, true);
  oFocus.sourcelocation := oXml.Tag('footnoteRef');
end;

procedure TCDAWriter.WriteSNLinkHtml(Const sPath: string; oXml: TXmlBuilder; oFocus : TsnLinkHtml);
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  Attribute(sPath, oXml, 'name', oFocus.name, true);
  Attribute(sPath, oXml, 'href', oFocus.href, true);
  Attribute(sPath, oXml, 'rel', oFocus.rel, true);
  Attribute(sPath, oXml, 'rev', oFocus.rev, true);
  Attribute(sPath, oXml, 'title', oFocus.title, true);
  oFocus.sourcelocation := oXml.Open('linkHtml');
  WriteCMFootnotesList(sPath, oXml, oFocus.parts);
  oXml.Close('linkHtml');
end;

procedure TCDAWriter.WriteSNList(Const sPath: string; oXml: TXmlBuilder; oFocus : TsnList);
var
  iLoop : integer;
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  Attribute(sPath, oXml, 'listType', CODES_TsnListType[oFocus.listType], true);
  oFocus.sourcelocation := oXml.Open('list');
  for iLoop := 0 to oFocus.item.Count - 1 Do
    WriteSNListItem(sPath+'\item', oXml, oFocus.item[iLoop]);
  oXml.Close('list');
end;

Procedure TCDAWriter.WriteSNListItem(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnItem);
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  oFocus.sourcelocation := oXml.Open('item');
  WriteSNCaption(sPath, oXml, oFocus.caption);
  WriteCMGeneralList(sPath, oXml, oFocus.parts);
  oXml.Close('item');
End;

procedure TCDAWriter.WriteSNParagraph(Const sPath: string; oXml: TXmlBuilder; oFocus : TsnParagraph);
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  oFocus.sourcelocation := oXml.Open('paragraph');
  WriteSNCaption(sPath, oXml, oFocus.caption);
  WriteCMContentList(sPath, oXml, oFocus.parts);
  oXml.Close('paragraph');
end;

Procedure TCDAWriter.WriteSNCaption(Const sPath: string; oXml : TXmlBuilder; oFocus  : TsnCaption);
Begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  oFocus.sourcelocation := oXml.Open('caption');
  WriteCMInlineList(sPath, oXml, oFocus.parts);
  oXml.Close('caption');
End;

procedure TCDAWriter.WriteSNRenderMultiMedia(Const sPath: string; oXml: TXmlBuilder; oFocus : TsnRenderMultiMedia);
var
  sText : string;
  iLoop : Integer;
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  sText := '';
  For iLoop := 0 to oFocus.referencedObject.Count - 1 Do
    sText := sText + ' ' + oFocus.referencedObject[iLoop];
  Attribute(sPath, oXml, 'referencedObject', stringTrimWhitespace(sText), true);

  oFocus.sourcelocation := oXml.Open('renderMultiMedia');
  WriteSNCaption(sPath, oXml, oFocus.caption);
  oXml.Close('renderMultiMedia');
end;

procedure TCDAWriter.WriteSNTable(Const sPath: string; oXml: TXmlBuilder; oFocus : TsnTable);
var
  iLoop : Integer;
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  Attribute(sPath, oXml, 'summary', oFocus.summary, true);
  Attribute(sPath, oXml, 'width', oFocus.width, true);
  Attribute(sPath, oXml, 'border', oFocus.border, true);
  Attribute(sPath, oXml, 'frame', CODES_TsnFrame[oFocus.frame], true);
  Attribute(sPath, oXml, 'rules', CODES_TsnRules[oFocus.rules], true);
  Attribute(sPath, oXml, 'cellspacing', oFocus.cellspacing, true);
  Attribute(sPath, oXml, 'cellpadding', oFocus.cellpadding, true);
  oFocus.sourcelocation := oXml.Open('table');
  WriteSNCaption(sPath, oXml, oFocus.caption);
  for iLoop := 0 to oFocus.col.Count - 1 Do
    WriteSNCol(sPath+'\col', oXml, oFocus.col[iLoop]);
  for iLoop := 0 to oFocus.colgroup.Count - 1 Do
    WriteSNColGroup(sPath+'\colgroup', oXml, oFocus.colgroup[iLoop]);
  WriteSNRowGroup(sPath+'\thead', oXml, 'thead', oFocus.thead);
  WriteSNRowGroup(sPath+'\tfoot', oXml, 'tfoot', oFocus.tfoot);
  for iLoop := 0 to oFocus.tbody.Count - 1 Do
    WriteSNRowGroup(sPath+'\tbody', oXml, 'tbody', oFocus.tbody[iLoop]);
  oXml.Close('table');
end;

Procedure TCDAWriter.WriteSNColGroup(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnColGroup);
var
  iLoop : Integer;
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  writeSNTableBase(sPath, oXml, oFocus);
  WriteSNColBase(sPath, oXml, oFocus);
  oFocus.sourcelocation := oXml.Open('colgroup');
  for iLoop := 0 to oFocus.col.Count - 1 Do
    WriteSNCol(sPath+'\col', oXml, oFocus.col[iLoop]);
  oXml.Close('colgroup');
End;

Procedure TCDAWriter.WriteSNTableBase(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnTableItem);
Begin
  Attribute(sPath, oXml, 'char', oFocus.char_, true);
  Attribute(sPath, oXml, 'charoff', oFocus.charoff, true);
  Attribute(sPath, oXml, 'align', CODES_TsnAlign[oFocus.align], true);
  Attribute(sPath, oXml, 'valign', CODES_TsnVAlign[oFocus.valign], true);
End;

Procedure TCDAWriter.WriteSNColBase(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnColItem);
Begin
  Attribute(sPath, oXml, 'width', oFocus.width, true);
  Attribute(sPath, oXml, 'span', IntToStr(oFocus.span), '1');
End;

Procedure TCDAWriter.WriteSNCol(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnCol);
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  writeSNTableBase(sPath, oXml, oFocus);
  WriteSNColBase(sPath, oXml, oFocus);
  oFocus.sourcelocation := oXml.Tag('col');
End;

Procedure TCDAWriter.WriteSNRowGroup(Const sPath: string; oXml : TXmlBuilder; Const sName : string; oFocus : TsnTRowGroup);
var
  iLoop : Integer;
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  writeSNTableBase(sPath, oXml, oFocus);
  oFocus.sourcelocation := oXml.Open(sName);
  for iLoop := 0 to oFocus.tr.Count - 1 Do
    WriteSNRow(sPath+'\tr', oXml, oFocus.tr[iLoop]);
  oXml.Close(sName);
End;


Procedure TCDAWriter.WriteSNRow(Const sPath: string; oXml : TXmlBuilder; oFocus : TsnTRow);
var
  iLoop : Integer;
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  writeSNTableBase(sPath, oXml, oFocus);
  oFocus.sourcelocation := oXml.Open('tr');
  for iLoop := 0 to oFocus.parts.Count - 1 Do
    if oFocus.parts[iLoop].th <> Nil Then
      WriteSNCell(sPath+'\th', oXml, 'th', oFocus.parts[iLoop].th)
    Else
      WriteSNCell(sPath+'\td', oXml, 'td', oFocus.parts[iLoop].td);
  oXml.Close('tr');
End;


procedure TCDAWriter.WriteSNCell(Const sPath: string; oXml: TXmlBuilder; const sName: string; oFocus: TsnTCell);
var
  sText : string;
  iLoop : Integer;
begin
  if oFocus = nil Then
    Exit;

  writeSNBase(sPath, oXml, oFocus);
  writeSNTableBase(sPath, oXml, oFocus);

  sText := '';
  For iLoop := 0 to oFocus.headers.Count - 1 Do
    sText := sText + ' ' + oFocus.headers[iLoop];
  Attribute(sPath, oXml, 'headers', stringTrimWhitespace(sText), true);

  Attribute(sPath, oXml, 'abbr', oFocus.abbr, true);
  Attribute(sPath, oXml, 'axis', oFocus.axis, true);
  Attribute(sPath, oXml, 'rowspan', IntToStr(oFocus.rowspan), '1');
  Attribute(sPath, oXml, 'colspan', IntToStr(oFocus.colspan), '1');
  Attribute(sPath, oXml, 'scope', CODES_TsnCellScope[oFocus.scope], true);
  oFocus.sourcelocation := oXml.Open(sName);
  WriteCMGeneralList(sPath, oXml, oFocus.parts);
  oXml.Close(sName);
end;

procedure TCDAWriter.WriteRegionOfInterestValue(Const sPath: string; oXml: TXmlBuilder; const sName: string; oDT: TcdaRegionOfInterest_value; bOptional: Boolean);
begin
  if oDT = nil Then
  Begin
    If Errors and not bOptional Then
      RaiseError('write', sName+' is required but not present'+' @'+sPath);
    Exit;
  End;
  WriteANYAttributes(sPath, oXml, oDT);
  if oDT.HasValue Then
    Attribute(sPath, oXml, 'value', IntToStr(oDT.value), true);
  if oDT.Hasunsorted Then
      Attribute(sPath, oXml, 'unsorted', WriteBoolean(sPath, oDT.unsorted), 'false');
  oDT.sourcelocation := oXml.Tag(sName);
end;

procedure TCDAWriter.WritePiece(oXml: TXmlBuilder; oPart: Tv3Base);
begin
  if oPart.RIMClassName = 'ANY' Then
    WriteANY('ANY', oXml, 'any', oPart as Tv3ANY, false)
  Else if oPart.RimClassName = 'BL' Then
    WriteBL('BL', oXml, 'BL', oPart as Tv3BL, false)
  Else if oPart.RimClassName = 'CS' Then
    WriteCS('CS', oXml, 'CS', oPart as Tv3CS, false)
  Else if oPart.RimClassName = 'ED' Then
    WriteED('ED', oXml, 'ED', oPart as Tv3ED, false)
  Else if oPart.RimClassName = 'II' Then
    WriteII('II', oXml, 'II', oPart as Tv3II, false)
  Else if oPart.RimClassName = 'CD' Then
    WriteCD('CD', oXml, 'CD', oPart as Tv3CD, false)
  Else if oPart.RimClassName = 'CR' Then
    WriteCR('CR', oXml, 'CR', oPart as Tv3CR, false)
  Else if oPart.RimClassName = 'SC' Then
    WriteSC('SC', oXml, 'SC', oPart as Tv3SC, false)
  Else if oPart.RimClassName = 'IVL' Then
    WriteIVL('IVL', oXml, 'IVL', oPart as Tv3IVL, false, Tv3IVL(oPart).ParamType)
  Else if oPart.RimClassName = 'AD' Then
    WriteAD('AD', oXml, 'AD', oPart as Tv3AD, false)
  Else if oPart.RimClassName = 'EN' Then
    WriteEN('EN', oXml, 'EN', oPart as Tv3EN, false)
  Else if oPart.RimClassName = 'TS' Then
    WriteTS('TS', oXml, 'TS', oPart as Tv3TS, false)
  Else if oPart.RimClassName = 'TEL' Then
    WriteTEL('TEL', oXml, 'TEL', oPart as Tv3TEL, false)
  Else if oPart.RimClassName = 'ST' Then
    WriteST('ST', oXml, 'ST', oPart as Tv3ST, false)
  Else if oPart.RimClassName = 'INT' Then
    WriteINT('INT', oXml, 'INT', oPart as Tv3INT, false)
  Else if oPart.RimClassName = 'MO' Then
    WriteMO('MO', oXml, 'MO', oPart as Tv3MO, false)
  Else if oPart.RimClassName = 'PQ' Then
    WritePQ('PQ', oXml, 'PQ', oPart as Tv3PQ, false)
  Else if oPart.RimClassName = 'PQR' Then
    WritePQR('PQR', oXml, 'PQR', oPart as Tv3PQR, false)
  Else if oPart.RimClassName = 'RTO<PQ,PQ>' Then
    WriteRTO('RTO', oXml, 'RTO', oPart as Tv3RTO, false, Tv3RTO(oPart).NumeratorType, Tv3RTO(oPart).DenominatorType)
  Else if oPart.RimClassName = 'PIVL' Then
    WritePIVL('PIVL', oXml, 'PIVL', oPart as Tv3PIVL, false)
  Else if oPart.RimClassName = 'EIVL' Then
    WriteEIVL('EIVL', oXml, 'EIVL', oPart as Tv3EIVL, false)
  Else if oPart.RimClassName = 'GTS' Then
    WriteGTS('GTS', oXml, 'GTS', oPart as Tv3qset, false)
  Else if oPart.RimClassName = 'SNText' Then
    WriteSNText('SNText', oXml, 'SNText', oPart as TsnText)
  Else if oPart.RimClassName = 'Content' Then
    WriteSNContent('Content', oXml, oPart as TsnContent)
  Else if oPart.RimClassName = 'LinkHTML' Then
    WriteSNLinkHTML('LinkHTML', oXml, oPart as TsnLinkHtml)
  Else if oPart.RimClassName = 'Footnote' Then
    WriteSNFootnote('Footnote', oXml, oPart as TsnFootnote)
  Else if oPart.RimClassName = 'FootnoteRef' Then
    WriteSNFootnoteRef('FootnoteRef', oXml, oPart as TsnFootnoteRef)
  Else if oPart.RimClassName = 'Paragraph' Then
    WriteSNParagraph('Paragraph', oXml, oPart as TsnParagraph)
  Else if oPart.RimClassName = 'List' Then
    WriteSNList('List', oXml, oPart as TsnList)
  Else if oPart.RimClassName = 'Table' Then
    WriteSNTable('Table', oXml, oPart as TsnTable)
  Else if oPart.RimClassName = 'RenderMultiMedia' Then
    WriteSNRenderMultiMedia('RenderMultiMedia', oXml, oPart as TsnRenderMultiMedia)
  Else if oPart.RimClassName = 'Caption' Then
    WriteSNCaption('Caption', oXml, oPart as TsnCaption)
  Else if oPart.RimClassName = 'Item' Then
    WriteSNListItem('Item', oXml, oPart as TsnItem)
  Else if oPart.RimClassName = 'Col' Then
    WriteSNCol('Col', oXml, oPart as TsnCol)
  Else if oPart.RimClassName = 'ColGroup' Then
    WriteSNColGroup('ColGroup', oXml, oPart as TsnColGroup)
  Else if oPart.RimClassName = 'RowGroup' Then
    WriteSNRowGroup('RowGroup', oXml, 'RowGroup', oPart as TsnTRowGroup)
  Else if oPart.RimClassName = 'Row' Then
    WriteSNRow('Row', oXml, oPart as TsnTRow)
  Else if oPart.RimClassName = 'Cell' Then
    WriteSNCell('Cell', oXml, 'Cell', oPart as TsnTCell)
  Else if oPart.RimClassName = 'RegionOfInterestValue' Then
    WriteRegionOfInterestValue('RegionOfInterestValue', oXml, 'RegionOfInterestValue', oPart as TcdaRegionOfInterest_value, false)
  Else if oPart.RimClassName = 'Act' Then
    WriteAct('Act', oXml, 'Act', oPart as TcdaAct, false)
  Else if oPart.RimClassName = 'AssignedAuthor' Then
    WriteAssignedAuthor('AssignedAuthor', oXml, 'AssignedAuthor', oPart as TcdaAssignedAuthor, false)
  Else if oPart.RimClassName = 'AssignedCustodian' Then
    WriteAssignedCustodian('AssignedCustodian', oXml, 'AssignedCustodian', oPart as TcdaAssignedCustodian, false)
  Else if oPart.RimClassName = 'AssignedEntity' Then
    WriteAssignedEntity('AssignedEntity', oXml, 'AssignedEntity', oPart as TcdaAssignedEntity, false)
  Else if oPart.RimClassName = 'AssociatedEntity' Then
    WriteAssociatedEntity('AssociatedEntity', oXml, 'AssociatedEntity', oPart as TcdaAssociatedEntity, false)
  Else if oPart.RimClassName = 'Authenticator' Then
    WriteAuthenticator('Authenticator', oXml, 'Authenticator', oPart as TcdaAuthenticator, false)
  Else if oPart.RimClassName = 'Author' Then
    WriteAuthor('Author', oXml, 'Author', oPart as TcdaAuthor, false)
  Else if oPart.RimClassName = 'AuthoringDevice' Then
    WriteAuthoringDevice('AuthoringDevice', oXml, 'AuthoringDevice', oPart as TcdaAuthoringDevice, false)
  Else if oPart.RimClassName = 'Authorization' Then
    WriteAuthorization('Authorization', oXml, 'Authorization', oPart as TcdaAuthorization, false)
  Else if oPart.RimClassName = 'Birthplace' Then
    WriteBirthplace('Birthplace', oXml, 'Birthplace', oPart as TcdaBirthplace, false)
  Else if oPart.RimClassName = 'ClinicalDocument' Then
    WriteClinicalDocument('ClinicalDocument', oXml, 'ClinicalDocument', oPart as TcdaClinicalDocument, false)
  Else if oPart.RimClassName = 'Component1' Then
    WriteComponent1('Component1', oXml, 'Component1', oPart as TcdaComponent1, false)
  Else if oPart.RimClassName = 'Component2' Then
    WriteComponent2('Component2', oXml, 'Component2', oPart as TcdaComponent2, false)
  Else if oPart.RimClassName = 'Component3' Then
    WriteComponentSect('Component3', oXml, 'Component3', oPart as TcdaComponentSect, false)
  Else if oPart.RimClassName = 'Component4' Then
    WriteComponent4('Component4', oXml, 'Component4', oPart as TcdaComponent4, false)
  Else if oPart.RimClassName = 'Component5' Then
    WriteComponentSect('Component5', oXml, 'Component5', oPart as TcdaComponentSect, false)
  Else if oPart.RimClassName = 'Consent' Then
    WriteConsent('Consent', oXml, 'Consent', oPart as TcdaConsent, false)
  Else if oPart.RimClassName = 'Consumable' Then
    WriteConsumable('Consumable', oXml, 'Consumable', oPart as TcdaConsumable, false)
  Else if oPart.RimClassName = 'Criterion' Then
    WriteCriterion('Criterion', oXml, 'Criterion', oPart as TcdaCriterion, false)
  Else if oPart.RimClassName = 'Custodian' Then
    WriteCustodian('Custodian', oXml, 'Custodian', oPart as TcdaCustodian, false)
  Else if oPart.RimClassName = 'CustodianOrganization' Then
    WriteCustodianOrganization('CustodianOrganization', oXml, 'CustodianOrganization', oPart as TcdaCustodianOrganization, false)
  Else if oPart.RimClassName = 'DataEnterer' Then
    WriteDataEnterer('DataEnterer', oXml, 'DataEnterer', oPart as TcdaDataEnterer, false)
  Else if oPart.RimClassName = 'Device' Then
    WriteDevice('Device', oXml, 'Device', oPart as TcdaDevice, false)
  Else if oPart.RimClassName = 'DocumentationOf' Then
    WriteDocumentationOf('DocumentationOf', oXml, 'DocumentationOf', oPart as TcdaDocumentationOf, false)
  Else if oPart.RimClassName = 'EncompassingEncounter' Then
    WriteEncompassingEncounter('EncompassingEncounter', oXml, 'EncompassingEncounter', oPart as TcdaEncompassingEncounter, false)
  Else if oPart.RimClassName = 'Encounter' Then
    WriteEncounter('Encounter', oXml, 'Encounter', oPart as TcdaEncounter, false)
  Else if oPart.RimClassName = 'EncounterParticipant' Then
    WriteEncounterParticipant('EncounterParticipant', oXml, 'EncounterParticipant', oPart as TcdaEncounterParticipant, false)
  Else if oPart.RimClassName = 'Entity' Then
    WriteEntity('Entity', oXml, 'Entity', oPart as TcdaEntity, false)
  Else if oPart.RimClassName = 'Entry' Then
    WriteEntry('Entry', oXml, 'Entry', oPart as TcdaEntry, false)
  Else if oPart.RimClassName = 'EntryRelationship' Then
    WriteEntryRelationship('EntryRelationship', oXml, 'EntryRelationship', oPart as TcdaEntryRelationship, false)
  Else if oPart.RimClassName = 'ExternalAct' Then
    WriteExternalAct('ExternalAct', oXml, 'ExternalAct', oPart as TcdaExternalAct, false)
  Else if oPart.RimClassName = 'ExternalDocument' Then
    WriteExternalDocument('ExternalDocument', oXml, 'ExternalDocument', oPart as TcdaExternalDocument, false)
  Else if oPart.RimClassName = 'ExternalObservation' Then
    WriteExternalObservation('ExternalObservation', oXml, 'ExternalObservation', oPart as TcdaExternalObservation, false)
  Else if oPart.RimClassName = 'ExternalProcedure' Then
    WriteExternalProcedure('ExternalProcedure', oXml, 'ExternalProcedure', oPart as TcdaExternalProcedure, false)
  Else if oPart.RimClassName = 'Guardian' Then
    WriteGuardian('Guardian', oXml, 'Guardian', oPart as TcdaGuardian, false)
  Else if oPart.RimClassName = 'HealthCareFacility' Then
    WriteHealthCareFacility('HealthCareFacility', oXml, 'HealthCareFacility', oPart as TcdaHealthCareFacility, false)
  Else if oPart.RimClassName = 'Informant12' Then
    WriteInformant12('Informant12', oXml, 'Informant12', oPart as TcdaInformant12, false)
  Else if oPart.RimClassName = 'InformationRecipient' Then
    WriteInformationRecipient('InformationRecipient', oXml, 'InformationRecipient', oPart as TcdaInformationRecipient, false)
  Else if oPart.RimClassName = 'InFulfillmentOf' Then
    WriteInFulfillmentOf('InFulfillmentOf', oXml, 'InFulfillmentOf', oPart as TcdaInFulfillmentOf, false)
  Else if oPart.RimClassName = 'IntendedRecipient' Then
    WriteIntendedRecipient('IntendedRecipient', oXml, 'IntendedRecipient', oPart as TcdaIntendedRecipient, false)
  Else if oPart.RimClassName = 'LabeledDrug' Then
    WriteLabeledDrug('LabeledDrug', oXml, 'LabeledDrug', oPart as TcdaLabeledDrug, false)
  Else if oPart.RimClassName = 'LanguageCommunication' Then
    WriteLanguageCommunication('LanguageCommunication', oXml, 'LanguageCommunication', oPart as TcdaLanguageCommunication, false)
  Else if oPart.RimClassName = 'EntityIdentifier' Then
    WriteEntityIdentifier('EntityIdentifier', oXml, 'EntityIdentifier', oPart as TcdaEntityIdentifier, false)
  Else if oPart.RimClassName = 'LegalAuthenticator' Then
    WriteLegalAuthenticator('LegalAuthenticator', oXml, 'LegalAuthenticator', oPart as TcdaLegalAuthenticator, false)
  Else if oPart.RimClassName = 'Location' Then
    WriteLocation('Location', oXml, 'Location', oPart as TcdaLocation, false)
  Else if oPart.RimClassName = 'MaintainedEntity' Then
    WriteMaintainedEntity('MaintainedEntity', oXml, 'MaintainedEntity', oPart as TcdaMaintainedEntity, false)
  Else if oPart.RimClassName = 'ManufacturedProduct' Then
    WriteManufacturedProduct('ManufacturedProduct', oXml, 'ManufacturedProduct', oPart as TcdaManufacturedProduct, false)
  Else if oPart.RimClassName = 'Material' Then
    WriteMaterial('Material', oXml, 'Material', oPart as TcdaMaterial, false)
  Else if oPart.RimClassName = 'NonXMLBody' Then
    WriteNonXMLBody('NonXMLBody', oXml, 'NonXMLBody', oPart as TcdaNonXMLBody, false)
  Else if oPart.RimClassName = 'Observation' Then
    WriteObservation('Observation', oXml, 'Observation', oPart as TcdaObservation, false)
  Else if oPart.RimClassName = 'ObservationMedia' Then
    WriteObservationMedia('ObservationMedia', oXml, 'ObservationMedia', oPart as TcdaObservationMedia, false)
  Else if oPart.RimClassName = 'ObservationRange' Then
    WriteObservationRange('ObservationRange', oXml, 'ObservationRange', oPart as TcdaObservationRange, false)
  Else if oPart.RimClassName = 'Order' Then
    WriteOrder('Order', oXml, 'Order', oPart as TcdaOrder, false)
  Else if oPart.RimClassName = 'Organization' Then
    WriteOrganization('Organization', oXml, 'Organization', oPart as TcdaOrganization, false)
  Else if oPart.RimClassName = 'OrganizationPartOf' Then
    WriteOrganizationPartOf('OrganizationPartOf', oXml, 'OrganizationPartOf', oPart as TcdaOrganizationPartOf, false)
  Else if oPart.RimClassName = 'Organizer' Then
    WriteOrganizer('Organizer', oXml, 'Organizer', oPart as TcdaOrganizer, false)
  Else if oPart.RimClassName = 'ParentDocument' Then
    WriteParentDocument('ParentDocument', oXml, 'ParentDocument', oPart as TcdaParentDocument, false)
  Else if oPart.RimClassName = 'Participant1' Then
    WriteParticipant1('Participant1', oXml, 'Participant1', oPart as TcdaParticipant1, false)
  Else if oPart.RimClassName = 'Participant2' Then
    WriteParticipant2('Participant2', oXml, 'Participant2', oPart as TcdaParticipant2, false)
  Else if oPart.RimClassName = 'ParticipantRole' Then
    WriteParticipantRole('ParticipantRole', oXml, 'ParticipantRole', oPart as TcdaParticipantRole, false)
  Else if oPart.RimClassName = 'Patient' Then
    WritePatient('Patient', oXml, 'Patient', oPart as TcdaPatient, false)
  Else if oPart.RimClassName = 'PatientRole' Then
    WritePatientRole('PatientRole', oXml, 'PatientRole', oPart as TcdaPatientRole, false)
  Else if oPart.RimClassName = 'Performer1' Then
    WritePerformer1('Performer1', oXml, 'Performer1', oPart as TcdaPerformer1, false)
  Else if oPart.RimClassName = 'Performer2' Then
    WritePerformer2('Performer2', oXml, 'Performer2', oPart as TcdaPerformer2, false)
  Else if oPart.RimClassName = 'Person' Then
    WritePerson('Person', oXml, 'Person', oPart as TcdaPerson, false)
  Else if oPart.RimClassName = 'Place' Then
    WritePlace('Place', oXml, 'Place', oPart as TcdaPlace, false)
  Else if oPart.RimClassName = 'PlayingEntity' Then
    WritePlayingEntity('PlayingEntity', oXml, 'PlayingEntity', oPart as TcdaPlayingEntity, false)
  Else if oPart.RimClassName = 'Precondition' Then
    WritePrecondition('Precondition', oXml, 'Precondition', oPart as TcdaPrecondition, false)
  Else if oPart.RimClassName = 'Procedure' Then
    WriteProcedure('Procedure', oXml, 'Procedure', oPart as TcdaProcedure, false)
  Else if oPart.RimClassName = 'Product' Then
    WriteProduct('Product', oXml, 'Product', oPart as TcdaProduct, false)
  Else if oPart.RimClassName = 'RecordTarget' Then
    WriteRecordTarget('RecordTarget', oXml, 'RecordTarget', oPart as TcdaRecordTarget, false)
  Else if oPart.RimClassName = 'Reference' Then
    WriteReference('Reference', oXml, 'Reference', oPart as TcdaReference, false)
  Else if oPart.RimClassName = 'ReferenceRange' Then
    WriteReferenceRange('ReferenceRange', oXml, 'ReferenceRange', oPart as TcdaReferenceRange, false)
  Else if oPart.RimClassName = 'RegionOfInterest' Then
    WriteRegionOfInterest('RegionOfInterest', oXml, 'RegionOfInterest', oPart as TcdaRegionOfInterest, false)
  Else if oPart.RimClassName = 'RelatedDocument' Then
    WriteRelatedDocument('RelatedDocument', oXml, 'RelatedDocument', oPart as TcdaRelatedDocument, false)
  Else if oPart.RimClassName = 'RelatedEntity' Then
    WriteRelatedEntity('RelatedEntity', oXml, 'RelatedEntity', oPart as TcdaRelatedEntity, false)
  Else if oPart.RimClassName = 'RelatedSubject' Then
    WriteRelatedSubject('RelatedSubject', oXml, 'RelatedSubject', oPart as TcdaRelatedSubject, false)
  Else if oPart.RimClassName = 'ResponsibleParty' Then
    WriteResponsibleParty('ResponsibleParty', oXml, 'ResponsibleParty', oPart as TcdaResponsibleParty, false)
  Else if oPart.RimClassName = 'Section' Then
    WriteSection('Section', oXml, 'Section', oPart as TcdaSection, false)
  Else if oPart.RimClassName = 'ServiceEvent' Then
    WriteServiceEvent('ServiceEvent', oXml, 'ServiceEvent', oPart as TcdaServiceEvent, false)
  Else if oPart.RimClassName = 'Specimen' Then
    WriteSpecimen('Specimen', oXml, 'Specimen', oPart as TcdaSpecimen, false)
  Else if oPart.RimClassName = 'SpecimenRole' Then
    WriteSpecimenRole('SpecimenRole', oXml, 'SpecimenRole', oPart as TcdaSpecimenRole, false)
  Else if oPart.RimClassName = 'StructuredBody' Then
    WriteStructuredBody('StructuredBody', oXml, 'StructuredBody', oPart as TcdaStructuredBody, false)
  Else if oPart.RimClassName = 'Subject' Then
    WriteSubject('Subject', oXml, 'Subject', oPart as TcdaSubject, false)
  Else if oPart.RimClassName = 'SubjectPerson' Then
    WriteSubjectPerson('SubjectPerson', oXml, 'SubjectPerson', oPart as TcdaSubjectPerson, false)
  Else if oPart.RimClassName = 'SubstanceAdministration' Then
    WriteSubstanceAdministration('SubstanceAdministration', oXml, 'SubstanceAdministration', oPart as TcdaSubstanceAdministration, false)
  Else if oPart.RimClassName = 'Supply' Then
    WriteSupply('Supply', oXml, 'Supply', oPart as TcdaSupply, false)
  Else
    raise ECDAException.create('Unknown type '+oPart.ClassName);

end;

function TCDAWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

End.

