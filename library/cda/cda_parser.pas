unit cda_parser;

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

interface

Uses
  SysUtils,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_xml,
  cda_base, cda_types, cda_narrative, cda_objects;


Const
  MAGIC_ATTRIBUTE_NAME = 'k_id';

Type
  TStringArray = Array of WideString;

  TTextAction = (ttAsIs, ttTrim, ttTrimPad);

  TCDAParser = class (TFslObject)
  private
    FErrors : Boolean;
    FStripWhitespace : Boolean;
    FComments : TFslStringList;
    FMapping : Boolean;
    FIdentifiedObjects : TFslStringObjectMatch;

    Function CheckNS(oElement : TMXmlElement; extensions : array of string): Boolean; overload;
    Function CheckNS(oElement : TMXmlElement) : Boolean; overload;
    Function GetXsiType(oElement : TMXmlElement) : String;
    Function GetXmlId(oElement : TMXmlElement) : String;
    Function TextContent(oElement : TMXmlElement; aTextAction : TTextAction) : WideString;

    Procedure AddToMap(oElement : TMXmlElement; oObject : Tv3Base);

    Procedure TakeComments(oDT : Tv3Base); Overload;
    Procedure TakeComments(oDT : Tv3XP); Overload;
    Procedure TakeComments(oFocus : TcdaBase); Overload;

    Procedure CheckXsiType(Const sPath : String; oElement : TMXmlElement; const sType : String; bInComplete : boolean = false); Overload;
    Procedure CheckXsiType(Const sPath : String; oElement : TMXmlElement; const aTypes : array of String); Overload;

    Function isXmlContent(oElement : TMXmlElement) : Boolean;
    Procedure SeeComment(oNode : TMXmlElement);
    Function AddNamePart(oEN : Tv3EN; Const sText : WideString; aNull : Tv3NullFlavor; aType : Tv3EntityNamePartType) : Tv3ENXP;
    Function AddAddressPart(oAD : Tv3AD; Const sText : WideString; aNull : Tv3NullFlavor; aType : Tv3AddressPartType) : Tv3ADXP;

    Function ParseNullFlavor(Const sPath : String; Const sValue : WideString) : Tv3NullFlavor;
    Function ParseBoolean(Const sPath : String; Const sValue : WideString) : Boolean;
    Function ParseInteger(Const sPath : String; Const sValue : WideString) : Int64;
    Function ParseDecimal(Const sPath : String; sValue : WideString) : TFslDecimal;
    Function ParseEnum(Const sPath : String; Const sValue : WideString; Const aNames : Array of String) : Byte;
    Function ParseSet(Const sPath : String; Const sValue : String) : TStringArray; Overload;
    Procedure ParseSet(Const sPath : String; oTokens : TFslStringList; Const sValue : String); Overload;
    function ParseExtension(offset : integer; Const sPath : String; oElement : TMXmlElement) : Tv3Extension;

    Procedure ParseANYValue(Const sPath : String; oElement : TMXmlElement; oValue : Tv3ANY);
    Procedure ParseQTYValue(Const sPath : String; oElement : TMXmlElement; oValue : Tv3QTY);
    Procedure ParseCDValue(Const sPath : String; oElement : TMXmlElement; oValue : Tv3CD);

    Function ParseANY(Const sPath : String; oElement : TMXmlElement) : Tv3ANY;
    Function ParseQTY(Const sPath : String; oElement : TMXmlElement; aExpected : Tv3Datatype) : Tv3QTY;
    Function ParseBL(Const sPath : String; oElement : TMXmlElement) : Tv3BL;
    Function ParseCS(Const sPath : String; oElement : TMXmlElement) : Tv3CS;
    Function ParseED(Const sPath : String; oElement : TMXmlElement) : Tv3ED;
    Function ParseII(Const sPath : String; oElement : TMXmlElement) : Tv3II;
    Function ParseCD(Const sPath : String; oElement : TMXmlElement; const sFlavor : String = '') : Tv3CD;
    Function ParseCR(Const sPath : String; oElement : TMXmlElement) : Tv3CR;
    Function ParseSC(Const sPath : String; oElement : TMXmlElement) : Tv3SC;
    Function ParseIVL(Const sPath : String; oElement : TMXmlElement; aExpected : Tv3QTYDatatype) : Tv3IVL;
    Function ParseAD(Const sPath : String; oElement : TMXmlElement) : Tv3AD;
    Function ParseEN(Const sPath : String; oElement : TMXmlElement; const sFlavor : String = '') : Tv3EN;
    Function ParseTS(Const sPath : String; oElement : TMXmlElement) : Tv3TS;
    Function ParseTEL(Const sPath : String; oElement : TMXmlElement) : Tv3TEL;
    Function ParseST(Const sPath : String; oElement : TMXmlElement) : Tv3ST;
    Function ParseINT(Const sPath : String; oElement : TMXmlElement) : Tv3INT;
    Function ParseMO(Const sPath : String; oElement : TMXmlElement) : Tv3MO;
    Function ParsePQ(Const sPath : String; oElement : TMXmlElement) : Tv3PQ;
    Function ParsePQR(Const sPath : String; oElement : TMXmlElement) : Tv3PQR;
    Function ParseRTO(Const sPath : String; oElement : TMXmlElement; aExpected1, aExpected2 : Tv3QTYDatatype) : Tv3RTO;
    Function ParsePIVL(Const sPath : String; oElement : TMXmlElement) : Tv3PIVL;
    Function ParseEIVL(Const sPath : String; oElement : TMXmlElement) : Tv3EIVL;

    Function ParseSXPR(Const sPath : String; var oElement : TMXmlElement; aExpected : Tv3QTYDatatype) : Tv3QSET;
    Function DetermineSXPRType(Const sPath : String; const sOp : String) : Tv3QSETDatatype;
    Function ParseGTSItem(Const sPath : String; oElement : TMXmlElement) : Tv3QSET;
    Function WrapTSasIVL(oTS : Tv3TS) : Tv3IVL;
    Function ParseGTS(Const sPath : String; var oElement : TMXmlElement) : Tv3QSET;

    Function ParseSNText(Const sPath : String; oElement : TMXmlElement) : TsnText;
    Function ParseContent(Const sPath : String; oElement : TMXmlElement) : TsnContent;
    Function ParseLinkHTML(Const sPath : String; oElement : TMXmlElement) : TsnLinkHtml;
    Function ParseFootnote(Const sPath : String; oElement : TMXmlElement) : TsnFootnote;
    Function ParseFootnoteRef(Const sPath : String; oElement : TMXmlElement) : TsnFootnoteRef;
    Function ParseParagraph(Const sPath : String; oElement : TMXmlElement) : TsnParagraph;
    Function ParseList(Const sPath : String; oElement : TMXmlElement) : TsnList;
    Function ParseTable(Const sPath : String; oElement : TMXmlElement) : TsnTable;
    Function ParseRenderMultiMedia(Const sPath : String; oElement : TMXmlElement) : TsnRenderMultiMedia;
    Function ParseSub(Const sPath : String; oElement : TMXmlElement) : TsnString;
    Function ParseSup(Const sPath : String; oElement : TMXmlElement) : TsnString;
    Function ParseBr(Const sPath : String; oElement : TMXmlElement) : TsnBr;
    Function ParseCaption(Const sPath : String; oElement : TMXmlElement) : TsnCaption;
    Function ParseItem(Const sPath : String; oElement : TMXmlElement) : TsnItem;
    Function ParseCol(Const sPath : String; oElement : TMXmlElement) : TsnCol;
    Function ParseColGroup(Const sPath : String; oElement : TMXmlElement) : TsnColGroup;
    Function ParseRowGroup(Const sPath : String; oElement : TMXmlElement) : TsnTRowGroup;
    Function ParseRow(Const sPath : String; oElement : TMXmlElement) : TsnTRow;
    Function ParseCell(Const sPath : String; oElement : TMXmlElement) : TsnTCell;

    Procedure ParseSNBase(Const sPath : String; oElement : TMXmlElement; oBase : TsnBase);
    Procedure ParseColItem(Const sPath : String; oElement : TMXmlElement; oBase : TsnColItem);
    Procedure ParseTableItem(Const sPath : String; oElement : TMXmlElement; oBase : TsnTableItem);
    Procedure ParseCMGeneralList(Const sPath : String; oParts : TsnCMGeneralList; oElement: TMXmlElement; oOwner : TsnCaptioned);
    Procedure ParseCMContentList(Const sPath : String; oParts : TsnCMContentList; oElement: TMXmlElement; oOwner : TsnCaptioned);
    Procedure ParseCMFootnotesList(Const sPath : String; oParts : TsnCMFootnotesList; oElement: TMXmlElement);
    Procedure ParseCMInlineList(Const sPath : String; oParts : TsnCMInlineList; oElement: TMXmlElement);


    Function ParseRegionOfInterestValue(Const sPath : String; oElement : TMXmlElement) : TcdaRegionOfInterest_value;

    Function ParseAct(Const sPath : String; oElement : TMXmlElement) : TcdaAct;
    Function ParseAssignedAuthor(Const sPath : String; oElement : TMXmlElement) : TcdaAssignedAuthor;
    Function ParseAssignedCustodian(Const sPath : String; oElement : TMXmlElement) : TcdaAssignedCustodian;
    Function ParseAssignedEntity(Const sPath : String; oElement : TMXmlElement) : TcdaAssignedEntity;
    Function ParseAssociatedEntity(Const sPath : String; oElement : TMXmlElement) : TcdaAssociatedEntity;
    Function ParseAuthenticator(Const sPath : String; oElement : TMXmlElement) : TcdaAuthenticator;
    Function ParseAuthor(Const sPath : String; oElement : TMXmlElement) : TcdaAuthor;
    Function ParseAuthoringDevice(Const sPath : String; oElement : TMXmlElement) : TcdaAuthoringDevice;
    Function ParseAuthorization(Const sPath : String; oElement : TMXmlElement) : TcdaAuthorization;
    Function ParseBirthplace(Const sPath : String; oElement : TMXmlElement) : TcdaBirthplace;
    Function ParseClinicalDocument(Const sPath : String; oElement : TMXmlElement) : TcdaClinicalDocument;
    Function ParseComponent1(Const sPath : String; oElement : TMXmlElement) : TcdaComponent1;
    Function ParseComponent2(Const sPath : String; oElement : TMXmlElement) : TcdaComponent2;
    Function ParseComponentSect(Const sPath : String; oElement : TMXmlElement) : TcdaComponentSect;
    Function ParseComponent4(Const sPath : String; oElement : TMXmlElement) : TcdaComponent4;
    Function ParseConsent(Const sPath : String; oElement : TMXmlElement) : TcdaConsent;
    Function ParseConsumable(Const sPath : String; oElement : TMXmlElement) : TcdaConsumable;
    Function ParseCriterion(Const sPath : String; oElement : TMXmlElement) : TcdaCriterion;
    Function ParseCustodian(Const sPath : String; oElement : TMXmlElement) : TcdaCustodian;
    Function ParseCustodianOrganization(Const sPath : String; oElement : TMXmlElement) : TcdaCustodianOrganization;
    Function ParseDataEnterer(Const sPath : String; oElement : TMXmlElement) : TcdaDataEnterer;
    Function ParseDevice(Const sPath : String; oElement : TMXmlElement) : TcdaDevice;
    Function ParseDocumentationOf(Const sPath : String; oElement : TMXmlElement) : TcdaDocumentationOf;
    Function ParseEncompassingEncounter(Const sPath : String; oElement : TMXmlElement) : TcdaEncompassingEncounter;
    Function ParseEncounter(Const sPath : String; oElement : TMXmlElement) : TcdaEncounter;
    Function ParseEncounterParticipant(Const sPath : String; oElement : TMXmlElement) : TcdaEncounterParticipant;
    Function ParseEntity(Const sPath : String; oElement : TMXmlElement) : TcdaEntity;
    Function ParseEntry(Const sPath : String; oElement : TMXmlElement) : TcdaEntry;
    Function ParseEntryRelationship(Const sPath : String; oElement : TMXmlElement) : TcdaEntryRelationship;
    Function ParseExternalAct(Const sPath : String; oElement : TMXmlElement) : TcdaExternalAct;
    Function ParseExternalDocument(Const sPath : String; oElement : TMXmlElement) : TcdaExternalDocument;
    Function ParseExternalObservation(Const sPath : String; oElement : TMXmlElement) : TcdaExternalObservation;
    Function ParseExternalProcedure(Const sPath : String; oElement : TMXmlElement) : TcdaExternalProcedure;
    Function ParseGuardian(Const sPath : String; oElement : TMXmlElement) : TcdaGuardian;
    Function ParseHealthCareFacility(Const sPath : String; oElement : TMXmlElement) : TcdaHealthCareFacility;
    Function ParseInformant12(Const sPath : String; oElement : TMXmlElement) : TcdaInformant12;
    Function ParseInformationRecipient(Const sPath : String; oElement : TMXmlElement) : TcdaInformationRecipient;
    Function ParseInFulfillmentOf(Const sPath : String; oElement : TMXmlElement) : TcdaInFulfillmentOf;
    Function ParseIntendedRecipient(Const sPath : String; oElement : TMXmlElement) : TcdaIntendedRecipient;
    Function ParseLabeledDrug(Const sPath : String; oElement : TMXmlElement) : TcdaLabeledDrug;
    Function ParseLanguageCommunication(Const sPath : String; oElement : TMXmlElement) : TcdaLanguageCommunication;
    Function ParseEntityIdentifier(Const sPath : String; oElement : TMXmlElement) : TcdaEntityIdentifier;
    Function ParseLegalAuthenticator(Const sPath : String; oElement : TMXmlElement) : TcdaLegalAuthenticator;
    Function ParseLocation(Const sPath : String; oElement : TMXmlElement) : TcdaLocation;
    Function ParseMaintainedEntity(Const sPath : String; oElement : TMXmlElement) : TcdaMaintainedEntity;
    Function ParseManufacturedProduct(Const sPath : String; oElement : TMXmlElement) : TcdaManufacturedProduct;
    Function ParseMaterial(Const sPath : String; oElement : TMXmlElement) : TcdaMaterial;
    Function ParseNonXMLBody(Const sPath : String; oElement : TMXmlElement) : TcdaNonXMLBody;
    Function ParseObservation(Const sPath : String; oElement : TMXmlElement) : TcdaObservation;
    Function ParseObservationMedia(Const sPath : String; oElement : TMXmlElement) : TcdaObservationMedia;
    Function ParseObservationRange(Const sPath : String; oElement : TMXmlElement) : TcdaObservationRange;
    Function ParseOrder(Const sPath : String; oElement : TMXmlElement) : TcdaOrder;
    Function ParseOrganization(Const sPath : String; oElement : TMXmlElement) : TcdaOrganization;
    Function ParseOrganizationPartOf(Const sPath : String; oElement : TMXmlElement) : TcdaOrganizationPartOf;
    Function ParseOrganizer(Const sPath : String; oElement : TMXmlElement) : TcdaOrganizer;
    Function ParseParentDocument(Const sPath : String; oElement : TMXmlElement) : TcdaParentDocument;
    Function ParseParticipant1(Const sPath : String; oElement : TMXmlElement) : TcdaParticipant1;
    Function ParseParticipant2(Const sPath : String; oElement : TMXmlElement) : TcdaParticipant2;
    Function ParseParticipantRole(Const sPath : String; oElement : TMXmlElement) : TcdaParticipantRole;
    Function ParsePatient(Const sPath : String; oElement : TMXmlElement) : TcdaPatient;
    Function ParsePatientRole(Const sPath : String; oElement : TMXmlElement) : TcdaPatientRole;
    Function ParsePerformer1(Const sPath : String; oElement : TMXmlElement) : TcdaPerformer1;
    Function ParsePerformer2(Const sPath : String; oElement : TMXmlElement) : TcdaPerformer2;
    Function ParsePerson(Const sPath : String; oElement : TMXmlElement) : TcdaPerson;
    Function ParsePlace(Const sPath : String; oElement : TMXmlElement) : TcdaPlace;
    Function ParsePlayingEntity(Const sPath : String; oElement : TMXmlElement) : TcdaPlayingEntity;
    Function ParsePrecondition(Const sPath : String; oElement : TMXmlElement) : TcdaPrecondition;
    Function ParseProcedure(Const sPath : String; oElement : TMXmlElement) : TcdaProcedure;
    Function ParseProduct(Const sPath : String; oElement : TMXmlElement) : TcdaProduct;
    Function ParseRecordTarget(Const sPath : String; oElement : TMXmlElement) : TcdaRecordTarget;
    Function ParseReference(Const sPath : String; oElement : TMXmlElement) : TcdaReference;
    Function ParseReferenceRange(Const sPath : String; oElement : TMXmlElement) : TcdaReferenceRange;
    Function ParseRegionOfInterest(Const sPath : String; oElement : TMXmlElement) : TcdaRegionOfInterest;
    Function ParseRelatedDocument(Const sPath : String; oElement : TMXmlElement) : TcdaRelatedDocument;
    Function ParseRelatedEntity(Const sPath : String; oElement : TMXmlElement) : TcdaRelatedEntity;
    Function ParseRelatedSubject(Const sPath : String; oElement : TMXmlElement) : TcdaRelatedSubject;
    Function ParseResponsibleParty(Const sPath : String; oElement : TMXmlElement) : TcdaResponsibleParty;
    Function ParseSection(Const sPath : String; oElement : TMXmlElement) : TcdaSection;
    Function ParseServiceEvent(Const sPath : String; oElement : TMXmlElement) : TcdaServiceEvent;
    Function ParseSpecimen(Const sPath : String; oElement : TMXmlElement) : TcdaSpecimen;
    Function ParseSpecimenRole(Const sPath : String; oElement : TMXmlElement) : TcdaSpecimenRole;
    Function ParseStructuredBody(Const sPath : String; oElement : TMXmlElement) : TcdaStructuredBody;
    Function ParseSubject(Const sPath : String; oElement : TMXmlElement) : TcdaSubject;
    Function ParseSubjectPerson(Const sPath : String; oElement : TMXmlElement) : TcdaSubjectPerson;
    Function ParseSubstanceAdministration(Const sPath : String; oElement : TMXmlElement) : TcdaSubstanceAdministration;
    Function ParseSupply(Const sPath : String; oElement : TMXmlElement) : TcdaSupply;
  Protected
    Function ErrorClass : EFslExceptionClass; Override;
    function sizeInBytesV : cardinal; override;

  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Parse(oDoc : TMXmlElement) : TcdaClinicalDocument; Overload;
    Function Parse(oDoc : TMXmlDocument) : TcdaClinicalDocument; Overload;
    Function ParsePiece(oDoc : TMXmlElement; const sClass : String) : Tv3Base; Overload;
    Function ParsePiece(oDoc : TMXmlDocument; const sClass : String) : Tv3Base; Overload;

    Property Errors : Boolean read FErrors write FErrors;
    Property StripWhitespace : Boolean read FStripWhitespace write FStripWhitespace;
    Property Mapping : Boolean read FMapping Write FMapping;
    Property IdentifiedObjects : TFslStringObjectMatch read FIdentifiedObjects;
  End;

implementation


Function ArrayIndexOf(Const aNames: Array Of String; Const sName: String): Integer;
Begin
  Result := High(aNames);
  While (Result >= Low(aNames)) And (CompareStr(sName, aNames[Result]) <> 0) Do
    Dec(Result);
End;


{ TCDAParser }

function TCDAParser.Parse(oDoc: TMXmlElement): TcdaClinicalDocument;
begin
  result := ParseClinicalDocument('ClinicalDocument', oDoc);
End;

function TCDAParser.ParsePiece(oDoc: TMXmlElement; const sClass : String): Tv3Base;
begin
  if sClass = '' Then
    raise ECDAException.create('Class name is required');

  if sClass = 'ANY' Then
    result := ParseANY('ANY', oDoc)
  Else if sClass = 'BL' Then
    result := ParseBL('BL', oDoc)
  Else if sClass = 'CS' Then
    result := ParseCS('CS', oDoc)
  Else if sClass = 'ED' Then
    result := ParseED('ED', oDoc)
  Else if sClass = 'II' Then
    result := ParseII('II', oDoc)
  Else if sClass = 'CD' Then
    result := ParseCD('CD', oDoc)
  Else if sClass = 'CR' Then
    result := ParseCR('CR', oDoc)
  Else if sClass = 'SC' Then
    result := ParseSC('SC', oDoc)
  Else if sClass = 'IVL<TS>' Then
    result := ParseIVL('IVL', oDoc, Tv3TS)
  Else if sClass = 'IVL<PQ>' Then
    result := ParseIVL('IVL', oDoc, Tv3PQ)
  Else if sClass = 'AD' Then
    result := ParseAD('AD', oDoc)
  Else if sClass = 'EN' Then
    result := ParseEN('EN', oDoc)
  Else if sClass = 'TS' Then
    result := ParseTS('TS', oDoc)
  Else if sClass = 'TEL' Then
    result := ParseTEL('TEL', oDoc)
  Else if sClass = 'ST' Then
    result := ParseST('ST', oDoc)
  Else if sClass = 'INT' Then
    result := ParseINT('INT', oDoc)
  Else if sClass = 'MO' Then
    result := ParseMO('MO', oDoc)
  Else if sClass = 'PQ' Then
    result := ParsePQ('PQ', oDoc)
  Else if sClass = 'PQR' Then
    result := ParsePQR('PQR', oDoc)
  Else if sClass = 'RTO<PQ,PQ>' Then
    result := ParseRTO('RTO', oDoc, Tv3PQ, Tv3PQ)
  Else if sClass = 'RTO<PQ,MO>' Then
    result := ParseRTO('RTO', oDoc, Tv3MO, Tv3PQ)
  Else if sClass = 'RTO<MO,PQ>' Then
    result := ParseRTO('RTO', oDoc, Tv3PQ, Tv3MO)
  Else if sClass = 'RTO<MO,MO>' Then
    result := ParseRTO('RTO', oDoc, Tv3MO, Tv3MO)
  Else if sClass = 'PIVL' Then
    result := ParsePIVL('PIVL', oDoc)
  Else if sClass = 'EIVL' Then
    result := ParseEIVL('EIVL', oDoc)
  Else if sClass = 'GTS' Then
    result := ParseGTS('GTS', oDoc)
  Else if sClass = 'SNText' Then
    result := ParseSNText('SNText', oDoc)
  Else if sClass = 'Content' Then
    result := ParseContent('Content', oDoc)
  Else if sClass = 'LinkHTML' Then
    result := ParseLinkHTML('LinkHTML', oDoc)
  Else if sClass = 'Footnote' Then
    result := ParseFootnote('Footnote', oDoc)
  Else if sClass = 'FootnoteRef' Then
    result := ParseFootnoteRef('FootnoteRef', oDoc)
  Else if sClass = 'Paragraph' Then
    result := ParseParagraph('Paragraph', oDoc)
  Else if sClass = 'List' Then
    result := ParseList('List', oDoc)
  Else if sClass = 'Table' Then
    result := ParseTable('Table', oDoc)
  Else if sClass = 'RenderMultiMedia' Then
    result := ParseRenderMultiMedia('RenderMultiMedia', oDoc)
  Else if sClass = 'Caption' Then
    result := ParseCaption('Caption', oDoc)
  Else if sClass = 'Item' Then
    result := ParseItem('Item', oDoc)
  Else if sClass = 'Col' Then
    result := ParseCol('Col', oDoc)
  Else if sClass = 'ColGroup' Then
    result := ParseColGroup('ColGroup', oDoc)
  Else if sClass = 'RowGroup' Then
    result := ParseRowGroup('RowGroup', oDoc)
  Else if sClass = 'Row' Then
    result := ParseRow('Row', oDoc)
  Else if sClass = 'Cell' Then
    result := ParseCell('Cell', oDoc)
  Else if sClass = 'RegionOfInterestValue' Then
    result := ParseRegionOfInterestValue('RegionOfInterestValue', oDoc)
  Else if sClass = 'Act' Then
    result := ParseAct('Act', oDoc)
  Else if sClass = 'AssignedAuthor' Then
    result := ParseAssignedAuthor('AssignedAuthor', oDoc)
  Else if sClass = 'AssignedCustodian' Then
    result := ParseAssignedCustodian('AssignedCustodian', oDoc)
  Else if sClass = 'AssignedEntity' Then
    result := ParseAssignedEntity('AssignedEntity', oDoc)
  Else if sClass = 'AssociatedEntity' Then
    result := ParseAssociatedEntity('AssociatedEntity', oDoc)
  Else if sClass = 'Authenticator' Then
    result := ParseAuthenticator('Authenticator', oDoc)
  Else if sClass = 'Author' Then
    result := ParseAuthor('Author', oDoc)
  Else if sClass = 'AuthoringDevice' Then
    result := ParseAuthoringDevice('AuthoringDevice', oDoc)
  Else if sClass = 'Authorization' Then
    result := ParseAuthorization('Authorization', oDoc)
  Else if sClass = 'Birthplace' Then
    result := ParseBirthplace('Birthplace', oDoc)
  Else if sClass = 'ClinicalDocument' Then
    result := ParseClinicalDocument('ClinicalDocument', oDoc)
  Else if sClass = 'Component1' Then
    result := ParseComponent1('Component1', oDoc)
  Else if sClass = 'Component2' Then
    result := ParseComponent2('Component2', oDoc)
  Else if sClass = 'Component3' Then
    result := ParseComponentSect('Component3', oDoc)
  Else if sClass = 'Component4' Then
    result := ParseComponent4('Component4', oDoc)
  Else if sClass = 'Component5' Then
    result := ParseComponentSect('Component5', oDoc)
  Else if sClass = 'Consent' Then
    result := ParseConsent('Consent', oDoc)
  Else if sClass = 'Consumable' Then
    result := ParseConsumable('Consumable', oDoc)
  Else if sClass = 'Criterion' Then
    result := ParseCriterion('Criterion', oDoc)
  Else if sClass = 'Custodian' Then
    result := ParseCustodian('Custodian', oDoc)
  Else if sClass = 'CustodianOrganization' Then
    result := ParseCustodianOrganization('CustodianOrganization', oDoc)
  Else if sClass = 'DataEnterer' Then
    result := ParseDataEnterer('DataEnterer', oDoc)
  Else if sClass = 'Device' Then
    result := ParseDevice('Device', oDoc)
  Else if sClass = 'DocumentationOf' Then
    result := ParseDocumentationOf('DocumentationOf', oDoc)
  Else if sClass = 'EncompassingEncounter' Then
    result := ParseEncompassingEncounter('EncompassingEncounter', oDoc)
  Else if sClass = 'Encounter' Then
    result := ParseEncounter('Encounter', oDoc)
  Else if sClass = 'EncounterParticipant' Then
    result := ParseEncounterParticipant('EncounterParticipant', oDoc)
  Else if sClass = 'Entity' Then
    result := ParseEntity('Entity', oDoc)
  Else if sClass = 'Entry' Then
    result := ParseEntry('Entry', oDoc)
  Else if sClass = 'EntryRelationship' Then
    result := ParseEntryRelationship('EntryRelationship', oDoc)
  Else if sClass = 'ExternalAct' Then
    result := ParseExternalAct('ExternalAct', oDoc)
  Else if sClass = 'ExternalDocument' Then
    result := ParseExternalDocument('ExternalDocument', oDoc)
  Else if sClass = 'ExternalObservation' Then
    result := ParseExternalObservation('ExternalObservation', oDoc)
  Else if sClass = 'ExternalProcedure' Then
    result := ParseExternalProcedure('ExternalProcedure', oDoc)
  Else if sClass = 'Guardian' Then
    result := ParseGuardian('Guardian', oDoc)
  Else if sClass = 'HealthCareFacility' Then
    result := ParseHealthCareFacility('HealthCareFacility', oDoc)
  Else if sClass = 'Informant12' Then
    result := ParseInformant12('Informant12', oDoc)
  Else if sClass = 'InformationRecipient' Then
    result := ParseInformationRecipient('InformationRecipient', oDoc)
  Else if sClass = 'InFulfillmentOf' Then
    result := ParseInFulfillmentOf('InFulfillmentOf', oDoc)
  Else if sClass = 'IntendedRecipient' Then
    result := ParseIntendedRecipient('IntendedRecipient', oDoc)
  Else if sClass = 'LabeledDrug' Then
    result := ParseLabeledDrug('LabeledDrug', oDoc)
  Else if sClass = 'LanguageCommunication' Then
    result := ParseLanguageCommunication('LanguageCommunication', oDoc)
  Else if sClass = 'EntityIdentifier' Then
    result := ParseEntityIdentifier('EntityIdentifier', oDoc)
  Else if sClass = 'LegalAuthenticator' Then
    result := ParseLegalAuthenticator('LegalAuthenticator', oDoc)
  Else if sClass = 'Location' Then
    result := ParseLocation('Location', oDoc)
  Else if sClass = 'MaintainedEntity' Then
    result := ParseMaintainedEntity('MaintainedEntity', oDoc)
  Else if sClass = 'ManufacturedProduct' Then
    result := ParseManufacturedProduct('ManufacturedProduct', oDoc)
  Else if sClass = 'Material' Then
    result := ParseMaterial('Material', oDoc)
  Else if sClass = 'NonXMLBody' Then
    result := ParseNonXMLBody('NonXMLBody', oDoc)
  Else if sClass = 'Observation' Then
    result := ParseObservation('Observation', oDoc)
  Else if sClass = 'ObservationMedia' Then
    result := ParseObservationMedia('ObservationMedia', oDoc)
  Else if sClass = 'ObservationRange' Then
    result := ParseObservationRange('ObservationRange', oDoc)
  Else if sClass = 'Order' Then
    result := ParseOrder('Order', oDoc)
  Else if sClass = 'Organization' Then
    result := ParseOrganization('Organization', oDoc)
  Else if sClass = 'OrganizationPartOf' Then
    result := ParseOrganizationPartOf('OrganizationPartOf', oDoc)
  Else if sClass = 'Organizer' Then
    result := ParseOrganizer('Organizer', oDoc)
  Else if sClass = 'ParentDocument' Then
    result := ParseParentDocument('ParentDocument', oDoc)
  Else if sClass = 'Participant1' Then
    result := ParseParticipant1('Participant1', oDoc)
  Else if sClass = 'Participant2' Then
    result := ParseParticipant2('Participant2', oDoc)
  Else if sClass = 'ParticipantRole' Then
    result := ParseParticipantRole('ParticipantRole', oDoc)
  Else if sClass = 'Patient' Then
    result := ParsePatient('Patient', oDoc)
  Else if sClass = 'PatientRole' Then
    result := ParsePatientRole('PatientRole', oDoc)
  Else if sClass = 'Performer1' Then
    result := ParsePerformer1('Performer1', oDoc)
  Else if sClass = 'Performer2' Then
    result := ParsePerformer2('Performer2', oDoc)
  Else if sClass = 'Person' Then
    result := ParsePerson('Person', oDoc)
  Else if sClass = 'Place' Then
    result := ParsePlace('Place', oDoc)
  Else if sClass = 'PlayingEntity' Then
    result := ParsePlayingEntity('PlayingEntity', oDoc)
  Else if sClass = 'Precondition' Then
    result := ParsePrecondition('Precondition', oDoc)
  Else if sClass = 'Procedure' Then
    result := ParseProcedure('Procedure', oDoc)
  Else if sClass = 'Product' Then
    result := ParseProduct('Product', oDoc)
  Else if sClass = 'RecordTarget' Then
    result := ParseRecordTarget('RecordTarget', oDoc)
  Else if sClass = 'Reference' Then
    result := ParseReference('Reference', oDoc)
  Else if sClass = 'ReferenceRange' Then
    result := ParseReferenceRange('ReferenceRange', oDoc)
  Else if sClass = 'RegionOfInterest' Then
    result := ParseRegionOfInterest('RegionOfInterest', oDoc)
  Else if sClass = 'RelatedDocument' Then
    result := ParseRelatedDocument('RelatedDocument', oDoc)
  Else if sClass = 'RelatedEntity' Then
    result := ParseRelatedEntity('RelatedEntity', oDoc)
  Else if sClass = 'RelatedSubject' Then
    result := ParseRelatedSubject('RelatedSubject', oDoc)
  Else if sClass = 'ResponsibleParty' Then
    result := ParseResponsibleParty('ResponsibleParty', oDoc)
  Else if sClass = 'Section' Then
    result := ParseSection('Section', oDoc)
  Else if sClass = 'ServiceEvent' Then
    result := ParseServiceEvent('ServiceEvent', oDoc)
  Else if sClass = 'Specimen' Then
    result := ParseSpecimen('Specimen', oDoc)
  Else if sClass = 'SpecimenRole' Then
    result := ParseSpecimenRole('SpecimenRole', oDoc)
  Else if sClass = 'StructuredBody' Then
    result := ParseStructuredBody('StructuredBody', oDoc)
  Else if sClass = 'Subject' Then
    result := ParseSubject('Subject', oDoc)
  Else if sClass = 'SubjectPerson' Then
    result := ParseSubjectPerson('SubjectPerson', oDoc)
  Else if sClass = 'SubstanceAdministration' Then
    result := ParseSubstanceAdministration('SubstanceAdministration', oDoc)
  Else if sClass = 'Supply' Then
    result := ParseSupply('Supply', oDoc)
  Else
    raise ECDAException.create('unknown piece type '+sClass);
end;


Function TCDAParser.Parse(oDoc : TMXmlDocument) : TcdaClinicalDocument;
var
  oChild : TMXmlElement;
Begin
  result := Nil;
  oChild := oDoc.docElement;
  if (oChild = nil) Then
    RaiseError('Parse', 'Unable to find root element')
  Else if checkNS(oChild) and (oChild.Name = 'ClinicalDocument') then
    result := ParseClinicalDocument('ClinicalDocument', oChild)
  Else
    RaiseError('Parse', 'Root element is not a CDA document');
End;


Function TCDAParser.ParsePiece(oDoc : TMXmlDocument; const sClass : String) : Tv3Base;
var
  oChild : TMXmlElement;
Begin
  result := Nil;
  oChild := oDoc.docElement;
  if (oChild = nil) Then
    RaiseError('Parse', 'Unable to find root element')
  Else
    result := ParsePiece(oChild, sClass);
End;


Function TCDAParser.CheckNS(oElement : TMXmlElement; extensions : array of string) : Boolean;
Begin
  result := (oElement.NamespaceURI = 'urn:hl7-org:v3') or ( (oElement.NamespaceURI = 'http://ns.electronichealth.net.au/Ci/Cda/Extensions/3.0') and StringArrayExistsSensitive(extensions, oElement.Name));
End;


Function TCDAParser.CheckNS(oElement : TMXmlElement) : Boolean;
Begin
  result := (oElement.NamespaceURI = 'urn:hl7-org:v3'){ or (oElement.NamespaceURI = 'http://ns.electronichealth.net.au/Ci/Cda/Extensions/3.0')};
End;


Function TCDAParser.GetXsiType(oElement : TMXmlElement) : String;
Begin
  Result := oElement.attributeNS['http://www.w3.org/2001/XMLSchema-instance', 'type'];
End;

function TCDAParser.GetXmlId(oElement: TMXmlElement): String;
Begin
  Result := oElement.attributeNS['http://www.w3.org/XML/1998/namespace', 'id'];
end;


Function Trim(Const sValue : WideString; bWhitespaceWithMeaning : Boolean):WideString;
Begin
  result := StringTrimWhitespace(sValue);
  If bWhitespaceWithMeaning And (Result = '') Then
    result := ' ';
End;


Function TCDAParser.TextContent(oElement : TMXmlElement; aTextAction : TTextAction) : WideString;
Begin
  result := oElement.allText;
  if (aTextAction <> ttAsIs) And StripWhitespace Then
    Result := Trim(result, aTextAction = ttTrimPad);
End;

Procedure TCDAParser.CheckXsiType(Const sPath : String; oElement : TMXmlElement; const sType : String; bInComplete : boolean = false);
var
  sX : string;
Begin
  if Errors Then
  Begin
    sX := GetXsiType(oElement);
    if (sX <> '') then
      if (bIncomplete and not StringStartsWith(sX, sType)) or (not bInComplete and Not (sx = stype)) Then
        RaiseError('CheckXsiType', 'The type '+sX+' is not compatible with the expected type ('+sType+')'+' @'+sPath);
  End;
End;

Procedure TCDAParser.CheckXsiType(Const sPath : String; oElement : TMXmlElement; const aTypes : array of String);
var
  sX : string;
  bOk : Boolean;
  iLoop : Integer;
Begin
  if Errors Then
  Begin
    sX := GetXsiType(oElement);
    if (sX <> '') then
    begin
      bOk := false;
      For iLoop := Low(aTypes) to High(aTypes) Do
        bOk := bOk or (sX = aTypes[iLoop]);
      If Not bOk Then
        RaiseError('CheckXsiType', 'The type '+sX+' is not compatible with the expected type'+' @'+sPath);
    End;
  End;
End;

Function TCDAParser.ParseNullFlavor(Const sPath : String; Const sValue : WideString) : Tv3NullFlavor;
var
  iIndex : integer;
Begin
  result := nfNull;
  iIndex := ArrayIndexOf(CODES_Tv3NullFlavor, sValue);
  if iIndex > -1 Then
    Result := Tv3NullFlavor(iIndex)
  Else if Errors Then
    RaiseError('ParseNullFlavor', 'NullFlavor "'+sValue+'" is not valid'+' @'+sPath)
End;

Function TCDAParser.ParseBoolean(Const sPath : String; Const sValue : WideString) : Boolean;
Begin
  result := False;
  if (sValue = 'true') then
    Result := true
  Else if (sValue = '') or (sValue = 'false') Then
    result := False
  Else if Errors then
    RaiseError('ParseBoolean', 'Boolean value "'+sValue+'" is not valid'+' @'+sPath)
End;

Function TCDAParser.ParseInteger(Const sPath : String; Const sValue : WideString) : Int64;
Begin
  result := 0;
  if StringIsInteger64(sValue) Then
    result := StringToInteger64(sValue)
  Else if Errors Then
    RaiseError('ParseInteger', 'Integer value "'+sValue+'" is not valid'+' @'+sPath);
End;

Function TCDAParser.ParseDecimal(Const sPath : String; sValue : WideString) : TFslDecimal;
Begin
  sValue := Trim(sValue, false);
  if sValue = '' Then
    result := TFslDecimal.makeNull
  Else
  begin
    if (pos(',', sValue) > 0) then
      sValue := StringReplace(sValue, ',', '');
    result := TFslDecimal.Create(sValue);
  end;
End;

Function TCDAParser.ParseEnum(Const sPath : String; Const sValue : WideString; Const aNames : Array of String) : Byte;
var
  iIndex : Integer;
Begin
  iIndex := ArrayIndexOf(aNames, sValue);
  if iIndex = -1 then
  Begin
    result := 0;
    if Errors Then
      RaiseError('ParseEnum', 'Enum value "'+sValue+'" is not valid'+' @'+sPath);
  End
  Else
    Result := iIndex;
End;

Procedure TCDAParser.ParseSet(Const sPath : String; oTokens : TFslStringList; Const sValue : String);
var
  sTail, sWord : String;
Begin
  oTokens.Clear;
  sTail := sValue;
  while (sTail <> '') Do
  Begin
    StringSplit(sTail, ' ', sWord, sTail);
    if sWord <> '' Then
      oTokens.Add(sWord);
  End;
End;

Function TCDAParser.ParseSet(Const sPath : String; Const sValue : String) : TStringArray;
var
  sTail, sWord : String;
Begin
  SetLength(Result, 0);
  sTail := sValue;
  while (sTail <> '') Do
  Begin
    StringSplit(sTail, ' ', sWord, sTail);
    if sWord <> '' Then
    Begin
      SetLength(result, Length(result)+1);
      result[High(result)] := sWord;
    End;
  End;
End;

Procedure TCDAParser.ParseANYValue(Const sPath : String; oElement : TMXmlElement; oValue : Tv3ANY);
Begin
  oValue.nullFlavor := ParseNullFlavor(sPath, oElement.Attribute['nullFlavor']);
  // ignore updateMode and flavorId for now
End;

Procedure TCDAParser.ParseQTYValue(Const sPath : String; oElement : TMXmlElement; oValue : Tv3QTY);
Begin
  ParseANYValue(sPath, oElement, oValue);
  // todo: uncertainty, originaltext, uncertainrange, expression
End;

Function TCDAParser.ParseBL(Const sPath : String; oElement : TMXmlElement) : Tv3BL;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3BL.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'BL');
    ParseANYValue(sPath, oElement, result);
    if (oElement.Attribute['value'] <> '') Then
      Result.value := ParseBoolean(sPath+'\@value', oElement.Attribute['value']);

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if Errors Then
          RaiseError('ParseBL', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseCS(Const sPath : String; oElement : TMXmlElement) : Tv3CS;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3CS.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'CS');
    ParseANYValue(sPath, oElement, result);
    Result.code := oElement.Attribute['code'];

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if Errors Then
          RaiseError('ParseCS', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseII(Const sPath : String; oElement : TMXmlElement) : Tv3II;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3II.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'II');
    ParseANYValue(sPath, oElement, result);
    Result.root := oElement.Attribute['root'];
    Result.extension := oElement.Attribute['extension'];
    Result.identifierName := oElement.Attribute['assigningAuthorityName'];
    if (oElement.Attribute['displayable'] <> '') Then
      Result.displayable := ParseBoolean(sPath+'\@displayable', oElement.Attribute['displayable']);
    Result.scope := Tv3IdentifierScope(ParseEnum(sPath+'\@scope', oElement.Attribute['scope'], CODES_Tv3IdentifierScope));
    Result.reliability := Tv3IdentifierReliability(ParseEnum(sPath+'\@scope', oElement.Attribute['scope'], CODES_Tv3IdentifierReliability));

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if Errors Then
          RaiseError('ParseII', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseTS(Const sPath : String; oElement : TMXmlElement) : Tv3TS;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3TS.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'TS');
    ParseQTYValue(sPath, oElement, result);
    Result.value := oElement.Attribute['value'];

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if Errors Then
          RaiseError('ParseTS', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseINT(Const sPath : String; oElement : TMXmlElement) : Tv3INT;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3INT.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'INT');
    ParseQTYValue(sPath, oElement, result);
    If oElement.Attribute['value'] <> '' Then
      Result.value := ParseInteger(sPath+'\@value', oElement.Attribute['value']);

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if Errors Then
          RaiseError('ParseINT', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseMO(Const sPath : String; oElement : TMXmlElement) : Tv3MO;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3MO.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'MO');
    ParseQTYValue(sPath, oElement, result);
    Result.value := ParseDecimal(sPath+'\@value', oElement.Attribute['value']);
    Result.currency := oElement.Attribute['currency'];

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if Errors Then
          RaiseError('ParsePQ', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParsePQ(Const sPath : String; oElement : TMXmlElement) : Tv3PQ;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3PQ.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'PQ');
    ParseQTYValue(sPath, oElement, result);
    try
      Result.value := ParseDecimal(sPath+'\@value', oElement.Attribute['value']);
    except
      result.originalText := Tv3ED.create;
      result.originalText.dataAsString := oElement.Attribute['value'];
    end;
    Result.unit_ := oElement.attribute['unit'];
    Result.codingRationale := Tv3CodingRationale(ParseEnum(sPath+'\@scope', oElement.attribute['scope'], CODES_Tv3CodingRationale));

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'translation' Then
        Begin
          if result.translation = nil Then
            result.translation := Tv3SetPQR.Create(result);
          result.translation.Add(parsePQR(sPath+'\translation', oChild));
        End
        Else if Errors Then
          RaiseError('ParsePQ', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseTEL(Const sPath : String; oElement : TMXmlElement) : Tv3TEL;
Var
  oChild : TMXmlElement;
  i : integer;
  aCodes : TStringArray;
  iLoop : integer;
Begin
  i := 0;
  Result := Tv3TEL.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'TEL');
    ParseANYValue(sPath, oElement, result);
    Result.value := oElement.Attribute['value'];
    aCodes := ParseSet(sPath+'\@use', oElement.Attribute['use']);
    For iLoop := Low(aCodes) To High(aCodes) Do
      Result.use := Result.use + [Tv3TelecommunicationAddressUse(ParseEnum(sPath+'\@use', aCodes[iLoop], CODES_Tv3TelecommunicationAddressUse))];
    aCodes := ParseSet(sPath+'\@capabilities', oElement.Attribute['capabilities']);
    For iLoop := Low(aCodes) To High(aCodes) Do
      Result.capabilities := Result.capabilities + [Tv3TelecommunicationCapability(ParseEnum(sPath+'\@capabilities', aCodes[iLoop], CODES_Tv3TelecommunicationCapability))];

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'useablePeriod' Then
          result.useablePeriod := ParseGTS(sPath+'\useablePeriod', oChild) // var
        Else if Errors Then
          RaiseError('ParsePQ', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseCR(Const sPath : String; oElement : TMXmlElement) : Tv3CR;
Var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3CR.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'CR');
    ParseANYValue(sPath, oElement, result);
    if oElement.Attribute['inverted'] <> '' Then
      Result.inverted := ParseBoolean(sPath+'\@inverted', oElement.Attribute['inverted']);

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'name' Then
          result.name := ParseCD(sPath+'\name', oChild, '')
        Else if oChild.Name = 'value' Then
          result.value := ParseCD(sPath+'\value', oChild)
        Else if Errors Then
          RaiseError('ParseCR', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TCDAParser.ParseCDValue(Const sPath : String; oElement : TMXmlElement; oValue : Tv3CD);
Var
  oChild : TMXmlElement;
  i : integer;
  sDisplay : WideString;
Begin
  i := 0;
  ParseANYValue(sPath, oElement, oValue);
  oValue.code := oElement.Attribute['code'];
  oValue.codeSystem := oElement.Attribute['codeSystem'];
  oValue.codeSystemName := oElement.Attribute['codeSystemName'];
  oValue.codeSystemVersion := oElement.Attribute['codeSystemVersion'];
  oValue.valueSet := oElement.Attribute['valueSet'];
  oValue.valueSetVersion := oElement.Attribute['valueSetVersion'];
  oValue.codingRationale := Tv3CodingRationale(ParseEnum(sPath+'\@scope', oElement.Attribute['scope'], CODES_Tv3CodingRationale));
  sDisplay := oElement.Attribute['displayName'];
  if (sDisplay <> '') Then
  Begin
    oValue.displayName := Tv3ST.Create;
    oValue.displayName.value := sDisplay;
  End;

  oChild := oElement.firstElement;
  While oChild <> Nil Do
  Begin
    if CheckNS(oChild) Then
    Begin
      if oChild.Name = 'translation' Then
      Begin
        if oValue.translation = nil Then
          oValue.translation := Tv3SetCD.Create(oValue);
        oValue.translation.Add(parseCD(sPath+'\translation', oChild));
      End
      Else if oChild.Name = 'qualifier' Then
      Begin
        if oValue.qualifier = nil Then
          oValue.qualifier := Tv3ListCR.Create(oValue);
        oValue.qualifier.Add(parseCR(sPath+'\qualifier', oChild));
      End
      Else if oChild.Name = 'originalText' Then
        oValue.originalText := ParseED(sPath+'\originalText', oChild)
      Else if Errors Then
        RaiseError('ParseCD', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        oValue.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
    oChild := oChild.nextElement;
  End;
End;


Function TCDAParser.ParseCD(Const sPath : String; oElement : TMXmlElement; const sFlavor : String = '') : Tv3CD;
Begin
  Result := Tv3CD.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, ['CD', 'CV', 'CE']);
    Result.flavorId := sFlavor;
    ParseCDValue(sPath, oElement, Result);
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParsePQR(Const sPath : String; oElement : TMXmlElement) : Tv3PQR;
Begin
  Result := Tv3PQR.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'PQR');
    ParseCDValue(sPath, oElement, Result);
    result.Value := ParseDecimal(sPath+'\@value', oElement.Attribute['value']);
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParsePIVL(Const sPath : String; oElement : TMXmlElement) : Tv3PIVL;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3PIVL.Create(Tv3TS);
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'PIVL_', true);
    ParseANYValue(sPath, oElement, result);
    Result.alignment := Tv3CalendarCycle(ParseEnum(sPath+'\@alignment', oElement.Attribute['alignment'], CODES_Tv3CalendarCycle));
    Result.isFlexible := ParseBoolean(sPath+'\@institutionSpecified', oElement.Attribute['institutionSpecified']);

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'originalText' Then
          result.originalText := parseED(sPath+'\originalText', oChild)
        Else if oChild.Name = 'period' Then
          result.period := parsePQ(sPath+'\period', oChild)
        Else if oChild.Name = 'frequency' Then
          result.frequency := parseRTO(sPath+'\frequency', oChild, Tv3INT, Tv3PQ)
        Else if oChild.Name = 'phase' Then
          result.phase := parseIVL(sPath+'\phase', oChild, Tv3TS)
        Else if oChild.Name = 'count' Then
          result.count := parseINT(sPath+'\count', oChild)
        Else if Errors Then
          RaiseError('ParseRTO', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseEIVL(Const sPath : String; oElement : TMXmlElement) : Tv3EIVL;
var
  oChild : TMXmlElement;
  i : integer;
  oEvent : Tv3CD;
Begin
  i := 0;
  Result := Tv3EIVL.Create(Tv3TS);
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'EIVL_', true);
    ParseANYValue(sPath, oElement, result);
//    Result.event := Tv3TimingEvent(ParseEnum(sPath+'\@event', oElement.Attribute['event'), CODES_Tv3TimingEvent));

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'originalText' Then
          result.originalText := parseED(sPath+'\originalText', oChild)
        Else if oChild.Name = 'offset' Then
          result.offset := parseIVL(sPath+'\offset', oChild, Tv3TS)
        Else if oChild.Name = 'event' Then
        Begin
          oEvent := ParseCD(sPath+'\event', oChild);
          Try
            result.event := Tv3TimingEvent(ParseEnum(sPath+'\event\@code', oEvent.code, CODES_Tv3TimingEvent))
          Finally
            oEvent.Free;
          End;
        End
        Else if Errors Then
          RaiseError('ParseEIVL', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseRTO(Const sPath : String; oElement : TMXmlElement; aExpected1, aExpected2 : Tv3QTYDatatype) : Tv3RTO;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3RTO.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'RTO_', true);
    Result.NumeratorType := aExpected1;
    Result.DenominatorType := aExpected2;
    ParseQTYValue(sPath, oElement, result);
    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'numerator' Then
          result.numerator := parseQTY(sPath+'\numerator', oChild, aExpected1)
        Else if oChild.Name = 'denominator' Then
          result.denominator := parseQTY(sPath+'\denominator', oChild, aExpected2)
        Else if Errors Then
          RaiseError('ParseRTO', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseST(Const sPath : String; oElement : TMXmlElement) : Tv3ST;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3ST.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'ST');
    ParseANYValue(sPath, oElement, result);
    Result.value := TextContent(oElement, ttAsIs);
    Result.language := oElement.Attribute['language'];

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'translation' Then
        Begin
          if result.translation = nil Then
            result.translation := Tv3SetST.Create(result);
          result.translation.Add(parseST(sPath+'\translation', oChild))
        End
        Else if Errors Then
          RaiseError('ParseST', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseSC(Const sPath : String; oElement : TMXmlElement) : Tv3SC;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3SC.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'SC');
    Result.code := Tv3CD.Create;
    ParseCDValue(sPath, oElement, result.code);
    Result.value := TextContent(oElement, ttAsIs);
    Result.language := oElement.Attribute['language'];

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'translation' Then
        Begin
          if result.translation = nil Then
            result.translation := Tv3SetST.Create(result);
          result.translation.Add(parseST(sPath+'\translation', oChild))
        End
        Else if Errors Then
          RaiseError('ParseST', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;



Function TCDAParser.ParseED(Const sPath : String; oElement : TMXmlElement) : Tv3ED;
var
  sRepresentation : WideString;
Begin
  Result := Tv3ED.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'ED');
    ParseANYValue(sPath, oElement, result);
    Result.mediaType := oElement.Attribute['mediaType'];
    Result.charset := oElement.Attribute['charset'];
    Result.language := oElement.Attribute['language'];
    Result.compression := Tv3Compression(parseEnum(sPath+'\@compression', oElement.Attribute['compression'], CODES_Tv3Compression));
    if oElement.Attribute['integrityCheck'] <> '' Then
    Begin
      Result.integrityCheck := TFslBuffer.Create;
      Result.integrityCheck.AsBytes := DecodeBase64(oElement.Attribute['integrityCheck']);
    End;
    Result.integrityCheckAlgorithm := Tv3IntegrityCheckAlgorithm(parseEnum(sPath+'\@integrityCheckAlgorithm', oElement.Attribute['integrityCheckAlgorithm'], CODES_Tv3IntegrityCheckAlgorithm));
    if oElement.Attribute['description'] <> '' Then
    Begin
      result.description := Tv3ST.Create;
      result.description.value := oElement.Attribute['description'];
    End;
    sRepresentation := oElement.Attribute['representation'];


(*    result.xml := oElement.CloneNode(false) as TMXmlElement;
    oChild := oElement.firstChild;
    bIsXml := isXmlContent(oElement);
    iContent := nil;
    while (oChild <> nil) Do
    Begin
      case oChild.nodeType of
        NODE_ELEMENT :
          if (oChild.namespaceURI = 'urn:hl7-org:v3') And (oChild.nodeName = 'translation') Then
          Begin
            if result.translation = nil Then
              result.translation := Tv3SetED.Create(result);
            result.translation.Add(parseED(sPath+'\translation', oChild as TMXmlElement))
          End
          Else if (oChild.namespaceURI = 'urn:hl7-org:v3') And (oChild.nodeName = 'reference') Then
            result.reference := ParseTEL(sPath+'\reference', oChild as TMXmlElement)
          Else if (oChild.namespaceURI = 'urn:hl7-org:v3') And (oChild.nodeName = 'thumbnail') Then
            result.thumbnail := ParseED(sPath+'\thumbnail', oChild as TMXmlElement)
          Else
          Begin
            GoXml;
            result.xml.appendChild(oChild.CloneNode(true));
          End;
        NODE_TEXT :
            if bIsXml or (iContent <> nil) Then
            Begin
              GoXml;
              result.xml.appendChild(oChild.CloneNode(true));
            End
            else if not StringIsWhitespace(oChild.Text) Then
              iContent := oChild;
        NODE_INVALID :
          Begin
            GoXml;
            result.xml.appendChild(oChild.CloneNode(true));
          End;
        NODE_ATTRIBUTE : ; // ignore attributes
        NODE_CDATA_SECTION :
          Begin
            GoXml;
            result.xml.appendChild(oChild.CloneNode(true));
          End;
       NODE_ENTITY_REFERENCE, NODE_ENTITY : ; // ignore
       NODE_PROCESSING_INSTRUCTION :
          Begin
            GoXml;
            result.xml.appendChild(oChild.CloneNode(true));
          End;
       NODE_COMMENT :
          Begin
            GoXml;
            result.xml.appendChild(oChild.CloneNode(true));
          End;
      End;
      oChild := oChild.NextSibling;
    End;
    if iContent <> nil Then
    begin
      if (sRepresentation = 'B64') Then
      Begin
        Result.data := TFslBuffer.Create;
        Result.data.AsText := DecodeBase64(iContent.text);
      End
      Else
        result.value :=  StringTrimWhitespace(iContent.text);
    End;

    if result.xml.childNodes.length = 0 then
      result.xml := nil;*)

    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseIVL(Const sPath : String; oElement : TMXmlElement; aExpected : Tv3QTYDatatype) : Tv3IVL;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := Tv3IVL.Create(aExpected);
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'IVL_', true);
    ParseANYValue(sPath, oElement, result);

    If oElement.Attribute['value'] <> '' Then
    Begin
      result.any := ParseQTY(sPath, oElement, aExpected);
      result.anyIsValue := true;
    End;

    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'low' Then
        Begin
          result.low := parseQTY(sPath+'\low', oChild, aExpected);
          if oChild.Attribute['inclusive'] <> '' Then
            result.lowClosed := ParseBoolean(sPath+'\low\@inclusive', oChild.Attribute['inclusive']);
        End
        Else if oChild.Name = 'high' Then
        Begin
          result.high := parseQTY(sPath+'\high', oChild, aExpected);
          if oChild.Attribute['inclusive'] <> '' Then
            result.highClosed := ParseBoolean(sPath+'\high\@inclusive', oChild.Attribute['inclusive']);
        End
        Else if oChild.Name = 'width' Then
          result.width := ParseQTY(sPath+'\width', oChild, aExpected.DiffType)
        Else if oChild.Name = 'center' Then
        Begin
          result.any := ParseQTY(sPath+'\center', oChild, aExpected);
          result.anyIsValue := false;
        End
        Else if Errors Then
        Begin
          RaiseError('ParseIVL', 'Unexpected Element '+oChild.Name+' @'+sPath);
        End;
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.AddNamePart(oEN : Tv3EN; Const sText : WideString; aNull : Tv3NullFlavor; aType : Tv3EntityNamePartType) : Tv3ENXP;
Begin
  Result := Tv3ENXP.Create;
  Try
    TakeComments(Result);
    Result.value := sText;
    Result.nullFlavor := aNull;
    Result.type_ := aType;
    oEN.part.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Function TypeFromNameElementName(Const sName : WideString): Tv3EntityNamePartType;
Begin
  if sName = 'given' Then
    Result := nptGIV
  Else if sName = 'family' Then
    result := nptFAM
  Else if (sName = 'prefix') or (sName = 'suffix') Then
    Result := nptTITLE
  Else if (sName = 'delimiter') then
    Result := nptDEL
  ELse
    result := nptNull;
End;

Function TCDAParser.ParseEN(Const sPath : String; oElement : TMXmlElement; const sFlavor : String = '') : Tv3EN;
var
  child : TMXmlElement;
  oPart : Tv3ENXP;
  aCodes : TStringArray;
  iLoop : integer;
  aType : Tv3EntityNamePartType;
Begin
  aCodes := nil;
  Result := Tv3EN.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, ['EN', 'PN', 'ON', 'TN']);
    Result.flavorId := sFlavor;
    ParseANYValue(sPath, oElement, result);

    aCodes := ParseSet(sPath+'\@use', oElement.Attribute['use']);
    For iLoop := Low(aCodes) To High(aCodes) Do
      Result.use := Result.use + [Tv3EntityNameUse(ParseEnum(sPath+'\@use', aCodes[iLoop], CODES_Tv3EntityNameUse))];

    result.part := TV3ListENXP.Create(result);

    child := oElement.first;
    while (child <> nil) Do
    Begin
      if (child.nodeType = ntElement) Then
      Begin
        aType := TypeFromNameElementName(child.Name);
        if aType = nptNull Then
          aType := Tv3EntityNamePartType(ParseEnum(sPath+'\@type', child.Attribute['type'], CODES_Tv3EntityNamePartType));
        oPart := addNamePart(Result, TextContent(child, ttTrim), ParseNullFlavor(sPath+'\'+child.Name, child.Attribute['nullFlavor']), aType);
        aCodes := ParseSet(sPath+'\@qualifier', child.Attribute['qualifier']);
        For iLoop := Low(aCodes) To High(aCodes) Do
          oPart.qualifier := oPart.qualifier + [Tv3EntityNamePartQualifier(ParseEnum(sPath+'\@qualifier', aCodes[iLoop], CODES_Tv3EntityNamePartQualifier))];
        if child.Name = 'suffix' then
          oPart.qualifier := oPart.qualifier + [npqSFX];
        if child.Name = 'prefix' then
          oPart.qualifier := oPart.qualifier + [npqPFX];
      End
      Else if (child.nodeType = ntComment) Then
        SeeComment(child)
      Else if (child.nodeType = ntText) Then
      Begin
        If not StringIsWhitespace(child.Text) Then
          if StripWhitespace Then
            addNamePart(Result, Trim(child.text, false), nfNull, nptNull)
          else
            addNamePart(Result, child.text, nfNull, nptNull);
      End
      Else
        ; // ignore this node
      child := child.next;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.AddAddressPart(oAD : Tv3AD; Const sText : WideString; aNull : Tv3NullFlavor; aType : Tv3AddressPartType) : Tv3ADXP;
Begin
  Result := Tv3ADXP.Create;
  Try
    TakeComments(Result);
    Result.value := sText;
    Result.nullFlavor := aNull;
    Result.type_ := aType;
    oAD.part.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseAD(Const sPath : String; oElement : TMXmlElement) : Tv3AD;
var
  child : TMXmlElement;
  aType : Tv3AddressPartType;
  aCodes : TStringArray;
  iLoop : integer;
Begin
  Result := Tv3AD.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    CheckXsiType(sPath, oElement, 'AD');
    ParseANYValue(sPath, oElement, result);
    if (oElement.Attribute['isNotOrdered']<> '') Then
      Result.isNotOrdered := ParseBoolean(sPath+'\@isNotOrdered', oElement.Attribute['isNotOrdered']);

    aCodes := ParseSet(sPath+'\@use', oElement.Attribute['use']);
    For iLoop := Low(aCodes) To High(aCodes) Do
      Result.use := Result.use + [Tv3PostalAddressUse(ParseEnum(sPath+'\@use', aCodes[iLoop], CODES_Tv3PostalAddressUse))];

    result.part := TV3ListADXP.Create(result);

    child := oElement.first;
    while (child <> Nil) Do
    Begin
      if (child.nodeType = ntElement) Then
      Begin
        aType := Tv3AddressPartType(ParseEnum(sPath+'\'+child.Name, child.Name, TAG_NAMES_ADDRESS));
        if aType = aptNull Then
          aType := Tv3AddressPartType(ParseEnum(sPath+'\@type', child.Attribute['type'], CODES_Tv3AddressPartType));
        addAddressPart(Result, TextContent(child, ttTrim), ParseNullFlavor(sPath+'\'+child.Name+'\@nullFlavor', child.Attribute['nullFlavor']), aType);
      End
      Else if (child.nodeType = ntComment) Then
        SeeComment(child)
      Else if (child.nodeType = ntText) Then
      Begin
        if StripWhitespace Then
          addAddressPart(Result, Trim(child.text, false), nfNull, aptNull)
        else
          addAddressPart(Result, child.text, nfNull, aptNull);
      End
      Else
        ; // ignore this node
      child := child.next;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.DetermineSXPRType(Const sPath : String; const sOp : String) : Tv3QSETDatatype;
Begin
  Result := Tv3QSI; // well, if we're not in error mode, we'll just treat it as an intersection
  if (sOp = 'A') Then
    result := Tv3QSI
  Else if (sOp = 'E') Then
    result := Tv3QSD
//  Else if (sOp = 'H') Then
//    result := Tv3QSH
  Else if (sOp = 'I') Then
    result := Tv3QSU
  Else if (sOp = 'P') Then
    result := Tv3QSP
  Else If Errors Then
    RaiseError('DetermineSXPRType', 'Unknown operator value "'+sOP+'" @'+sPath);
End;

Function TCDAParser.ParseSXPR(Const sPath : String; var oElement : TMXmlElement; aExpected : Tv3QTYDatatype) : Tv3QSET;
var
  oChild : TMXmlElement;
  i : integer;
  oText : Tv3ED;
  oFirst : Tv3QSET;
  oTemp : Tv3QSET;
  oComments : TFslStringList;
Begin
  i := 0;
  CheckXsiType(sPath, oElement, 'SXPR_', true);
  // the problem is that we don't know what type this will be until we've read the second comp

  Result := nil;
  oText := nil;
  oFirst := nil;
  oComments := TFslStringList.Create;
  Try
    oComments.assign(FComments);
    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if oChild.Name = 'originalText' Then
          oText := parseED(sPath+'\originalText', oChild)
        Else if oChild.Name = 'comp' Then
        Begin
          if oFirst = nil Then
            oFirst := ParseGTSItem(sPath+'\comp', oChild)
          Else if Result = nil Then
          Begin
            Result := DetermineSXPRType(sPath+'\comp\@operator', oChild.Attribute['operator']).Create(aExpected);
            if (oComments.Count > 0) Then
              Result.comments.Assign(oComments);
            ParseANYValue(sPath, oElement, result);
            Result.OriginalText := oText.Link;
            Result.AddComp(oFirst.Link);
            oTemp := ParseGTSItem(sPath+'\comp', oChild);
            Try
              Result.AddComp(oTemp.Link);
            Finally
              oTemp.Free;
            End;
          End
          Else
          Begin
            if (DetermineSXPRType(sPath+'\comp\@operator', oChild.Attribute['operator']) <> Result.ClassType) Then
              RaiseError('ParseSXPR', 'Inconsistent Operator values @'+sPath)
            else
            Begin
              oTemp := ParseGTSItem(sPath+'\comp', oChild);
              Try
                If Not Result.AddComp(oTemp.Link) Then
                  RaiseError('ParseSXPR', 'Too many components for the operator provided @'+sPath);
              Finally
                oTemp.Free;
              End;
            End;
          End
        End
        Else if Errors Then
          RaiseError('ParseRTO', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    if (Result = nil) Then
    Begin
      Result := Tv3QSU.Create(aExpected);
      AddToMap(oElement, Result);
      ParseANYValue(sPath, oElement, result);
      Result.OriginalText := oText.Link;
      if oFirst <> nil Then
        Result.AddComp(oFirst.Link);
    End;
    Result.Link;
  Finally
    oComments.Free;
    Result.Free;
    oFirst.Free;
    oText.Free;
  End;
End;

Function TCDAParser.WrapTSasIVL(oTS : Tv3TS) : Tv3IVL;
Begin
  Result := Tv3IVL.Create(Tv3TS);
  Try
    result.any := oTS;
    result.anyIsValue := false;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseGTSItem(Const sPath : String; oElement : TMXmlElement) : Tv3QSET;
Var
  sXsiType : String;
Begin
  // well, what this can be depends

  sXsiType := GetXsiType(oElement);
  if sXsiType = '' Then
  Begin
    Result := Tv3QSS.Create(Tv3TS);
    AddToMap(oElement, Result);
    Tv3QSS(Result).terms := Tv3SetQTY.create(result);
    Tv3QSS(Result).terms.Add(ParseTS(sPath, oElement));
  End
  Else if (sXsiType = 'PIVL_TS') Then
    Result := parsePIVL(sPath, oElement)
  Else if (sXsiType = 'EIVL_TS') Then
    Result := parseEIVL(sPath, oElement)
  Else if (sXsiType = 'IVL_TS') Then
    Result := parseIVL(sPath, oElement, Tv3TS)
  Else if (sXsiType = 'SXPR_TS') Then
    Result := parseSXPR(sPath, oElement, Tv3TS)
  Else if (sXsiType = 'TS') Then
    Result := WrapTSasIVL(parseTS(sPath, oElement))
  Else
  Begin
    RaiseError('ParseGTSItem', 'Unhandled type: '+sXsiType+' @'+sPath);
    Result := Nil;
  End;
End;

Function TCDAParser.ParseGTS(Const sPath : String; var oElement : TMXmlElement) : Tv3QSET;
var
  sName : String;
  oNext : TMXmlElement;
  bMore : Boolean;
  oTemp : Tv3QSET;
Begin
  Result := Nil;
  Try
    sName := oElement.Name;
    bMore := True;

    While bMore Do
    Begin
      if (result = nil) Then
        Result := ParseGTSItem(sPath, oElement)
      Else
      Begin
        oTemp := Result;
        Result := DetermineSXPRType(sPath, oElement.Attribute['operator']).Create(Tv3TS);
        result.AddComp(oTemp);
        result.AddComp(ParseGTSItem(sPath, oElement));
        Result.Flat := true;
      End;
      oNext := oElement.nextElement;
      bMore := (oNext <> Nil) And (oNext.Name = sName);
      if (bMore) Then
        oElement := oNext;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseANY(Const sPath : String; oElement : TMXmlElement) : Tv3ANY;
var
  sType : String;
Begin
  Result := nil;
  sType := GetXsiType(oElement);
  if (sType = 'PQ') Then
    Result := ParsePQ(sPath, oElement)
  Else if (sType = 'CD') Then
    result := ParseCD(sPath, oElement)
  Else if (sType = 'CE') Then
    result := ParseCD(sPath, oElement, 'CE')
  Else if (sType = 'CV') Then
    result := ParseCD(sPath, oElement, 'CV')
  Else if (sType = 'BL') Then
    result := ParseBL(sPath, oElement)
  Else if (sType = 'CS') Then
    result := ParseCS(sPath, oElement)
  Else if (sType = 'ED') Then
    result := ParseED(sPath, oElement)
  Else if (sType = 'II') Then
    result := ParseII(sPath, oElement)
  Else if (sType = 'SC') Then
    result := ParseSC(sPath, oElement)
  Else if (sType = 'AD') Then
    result := ParseAD(sPath, oElement)
  Else if (sType = 'EN') Then
    result := ParseEN(sPath, oElement)
  Else if (sType = 'PN') Then
    result := ParseEN(sPath, oElement, 'PN')
  Else if (sType = 'ON') Then
    result := ParseEN(sPath, oElement, 'ON')
  Else if (sType = 'TN') Then
    result := ParseEN(sPath, oElement, 'TN')
  Else if (sType = 'TS') Then
    result := ParseTS(sPath, oElement)
  Else if (sType = 'TEL') Then
    result := ParseTEL(sPath, oElement)
  Else if (sType = 'ST') Then
    result := ParseST(sPath, oElement)
  Else if (sType = 'MO') Then
    result := ParseMO(sPath, oElement)
  Else if (sType = 'INT') Then
    result := ParseINT(sPath, oElement)
  Else if (sType = 'IVL_PQ') Then
    result := ParseIVL(sPath, oElement, Tv3PQ)
  Else if (sType = 'IVL_INT') Then
    result := ParseIVL(sPath, oElement, Tv3INT)
  Else if (sType = 'IVL_TS') Then
    result := ParseIVL(sPath, oElement, Tv3TS)
  Else if (sType = 'RTO_PQ_PQ') Then
    result := ParseRTO(sPath, oElement, Tv3PQ, Tv3PQ)
  Else
    RaiseError('ParseANY', 'unknown type "'+sType+'" @'+sPath);
End;

Function TCDAParser.ParseQTY(Const sPath : String; oElement : TMXmlElement; aExpected : Tv3Datatype) : Tv3QTY;
var
  sType : String;
Begin
  Result := Nil;
  if aExpected = nil Then
    RaiseError('ParseQTY', 'Still under development'+' @'+sPath);
  if aExpected = Tv3TS Then
    Result := ParseTS(sPath, oElement)
  Else if aExpected = Tv3PQ Then
    result := ParsePQ(sPath, oElement)
  Else if aExpected = Tv3MO Then
    result := ParsePQ(sPath, oElement)
  Else if aExpected = Tv3INT Then
    result := ParseINT(sPath, oElement)
  Else
  Begin
    sType := GetXsiType(oElement);
    if (sType = 'PQ') Then
      Result := ParsePQ(sPath, oElement)
    Else if (sType = 'TS') Then
      result := ParseTS(sPath, oElement)
    Else if (sType = 'MO') Then
      result := ParseMO(sPath, oElement)
    Else if (sType = 'INT') Then
      result := ParseINT(sPath, oElement)
    Else
      RaiseError('ParseQTY', 'unknown type '+sType+' @'+sPath);
  End;
End;


Function TCDAParser.ParseAct(Const sPath : String; oElement : TMXmlElement) : TcdaAct;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaAct.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
        If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'statusCode' Then
          Result.statusCode := ParseCS(sPath+'\statusCode', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'priorityCode' Then
          Result.priorityCode := ParseCD(sPath+'\priorityCode', oChild)
  Else If oChild.Name = 'languageCode' Then
          Result.languageCode := ParseCS(sPath+'\languageCode', oChild)
  Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubject(sPath+'\subject', oChild)
  Else If oChild.Name = 'specimen' Then
          Result.specimen.Add(ParseSpecimen(sPath+'\specimen', oChild))
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer2(sPath+'\performer', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant2(sPath+'\participant', oChild))
  Else If oChild.Name = 'entryRelationship' Then
          Result.entryRelationship.Add(ParseEntryRelationship(sPath+'\entryRelationship', oChild))
  Else If oChild.Name = 'reference' Then
          Result.reference.Add(ParseReference(sPath+'\reference', oChild))
  Else If oChild.Name = 'precondition' Then
          Result.precondition.Add(ParsePrecondition(sPath+'\precondition', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    If oElement.Attribute['moodCode'] <> '' Then
      Result.moodCode := oElement.Attribute['moodCode'];
    If oElement.Attribute['negationInd'] <> '' Then
      Result.negationInd := ParseBoolean(sPath+'\@negationInd', oElement.Attribute['negationInd']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseAssignedAuthor(Const sPath : String; oElement : TMXmlElement) : TcdaAssignedAuthor;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaAssignedAuthor.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
        // choice :
  Else If oChild.Name = 'assignedPerson' Then
          Result.assignedPerson := ParsePerson(sPath+'\assignedPerson', oChild)
  Else If oChild.Name = 'assignedAuthoringDevice' Then
          Result.assignedAuthoringDevice := ParseAuthoringDevice(sPath+'\assignedAuthoringDevice', oChild)
        // end choice
  Else If oChild.Name = 'representedOrganization' Then
          Result.representedOrganization := ParseOrganization(sPath+'\representedOrganization', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseAssignedCustodian(Const sPath : String; oElement : TMXmlElement) : TcdaAssignedCustodian;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaAssignedCustodian.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'representedCustodianOrganization' Then
          Result.representedCustodianOrganization := ParseCustodianOrganization(sPath+'\representedCustodianOrganization', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseAssignedEntity(Const sPath : String; oElement : TMXmlElement) : TcdaAssignedEntity;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaAssignedEntity.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
  Else If oChild.Name = 'assignedPerson' Then
          Result.assignedPerson := ParsePerson(sPath+'\assignedPerson', oChild)
  Else If oChild.Name = 'representedOrganization' Then
          Result.representedOrganization := ParseOrganization(sPath+'\representedOrganization', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseAssociatedEntity(Const sPath : String; oElement : TMXmlElement) : TcdaAssociatedEntity;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaAssociatedEntity.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
  Else If oChild.Name = 'associatedPerson' Then
          Result.associatedPerson := ParsePerson(sPath+'\associatedPerson', oChild)
  Else If oChild.Name = 'scopingOrganization' Then
          Result.scopingOrganization := ParseOrganization(sPath+'\scopingOrganization', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseAuthenticator(Const sPath : String; oElement : TMXmlElement) : TcdaAuthenticator;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaAuthenticator.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'signatureCode' Then
          Result.signatureCode := ParseCS(sPath+'\signatureCode', oChild)
  Else If oChild.Name = 'time' Then
          Result.time := ParseTS(sPath+'\time', oChild)
  Else If oChild.Name = 'assignedEntity' Then
          Result.assignedEntity := ParseAssignedEntity(sPath+'\assignedEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseAuthor(Const sPath : String; oElement : TMXmlElement) : TcdaAuthor;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaAuthor.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'functionCode' Then
          Result.functionCode := ParseCD(sPath+'\functionCode', oChild)
  Else If oChild.Name = 'time' Then
          Result.time := ParseTS(sPath+'\time', oChild)
        Else if oChild.Name = 'assignedAuthor' Then
          Result.assignedAuthor := ParseAssignedAuthor(sPath+'\assignedAuthor', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseAuthoringDevice(Const sPath : String; oElement : TMXmlElement) : TcdaAuthoringDevice;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaAuthoringDevice.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'manufacturerModelName' Then
          Result.manufacturerModelName := ParseSC(sPath+'\manufacturerModelName', oChild)
  Else If oChild.Name = 'softwareName' Then
          Result.softwareName := ParseSC(sPath+'\softwareName', oChild)
  Else If oChild.Name = 'asMaintainedEntity' Then
          Result.asMaintainedEntity.Add(ParseMaintainedEntity(sPath+'\asMaintainedEntity', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseAuthorization(Const sPath : String; oElement : TMXmlElement) : TcdaAuthorization;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaAuthorization.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'consent' Then
          Result.consent := ParseConsent(sPath+'\consent', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseBirthplace(Const sPath : String; oElement : TMXmlElement) : TcdaBirthplace;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaBirthplace.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'place' Then
          Result.place := ParsePlace(sPath+'\place', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseClinicalDocument(Const sPath : String; oElement : TMXmlElement) : TcdaClinicalDocument;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaClinicalDocument.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id := ParseII(sPath+'\id', oChild)
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'title' Then
          Result.title := ParseST(sPath+'\title', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseTS(sPath+'\effectiveTime', oChild)
  Else If oChild.Name = 'confidentialityCode' Then
          Result.confidentialityCode := ParseCD(sPath+'\confidentialityCode', oChild)
  Else If oChild.Name = 'languageCode' Then
          Result.languageCode := ParseCS(sPath+'\languageCode', oChild)
  Else If oChild.Name = 'setId' Then
          Result.setId := ParseII(sPath+'\setId', oChild)
  Else If oChild.Name = 'versionNumber' Then
          Result.versionNumber := ParseINT(sPath+'\versionNumber', oChild)
  Else If oChild.Name = 'copyTime' Then
          Result.copyTime := ParseTS(sPath+'\copyTime', oChild)
  Else If oChild.Name = 'recordTarget' Then
          Result.recordTarget.Add(ParseRecordTarget(sPath+'\recordTarget', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'dataEnterer' Then
          Result.dataEnterer := ParseDataEnterer(sPath+'\dataEnterer', oChild)
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'custodian' Then
          Result.custodian := ParseCustodian(sPath+'\custodian', oChild)
  Else If oChild.Name = 'informationRecipient' Then
          Result.informationRecipient.Add(ParseInformationRecipient(sPath+'\informationRecipient', oChild))
  Else If oChild.Name = 'legalAuthenticator' Then
          Result.legalAuthenticator := ParseLegalAuthenticator(sPath+'\legalAuthenticator', oChild)
  Else If oChild.Name = 'authenticator' Then
          Result.authenticator.Add(ParseAuthenticator(sPath+'\authenticator', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant1(sPath+'\participant', oChild))
  Else If oChild.Name = 'inFulfillmentOf' Then
          Result.inFulfillmentOf.Add(ParseInFulfillmentOf(sPath+'\inFulfillmentOf', oChild))
  Else If oChild.Name = 'documentationOf' Then
          Result.documentationOf.Add(ParseDocumentationOf(sPath+'\documentationOf', oChild))
  Else If oChild.Name = 'relatedDocument' Then
          Result.relatedDocument.Add(ParseRelatedDocument(sPath+'\relatedDocument', oChild))
  Else If oChild.Name = 'authorization' Then
          Result.authorization.Add(ParseAuthorization(sPath+'\authorization', oChild))
  Else If oChild.Name = 'componentOf' Then
          Result.componentOf := ParseComponent1(sPath+'\componentOf', oChild)
  Else If oChild.Name = 'component' Then
          Result.component := ParseComponent2(sPath+'\component', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath)
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseComponent1(Const sPath : String; oElement : TMXmlElement) : TcdaComponent1;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaComponent1.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'encompassingEncounter' Then
          Result.encompassingEncounter := ParseEncompassingEncounter(sPath+'\encompassingEncounter', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseComponent2(Const sPath : String; oElement : TMXmlElement) : TcdaComponent2;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaComponent2.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
        // choice :
  Else If oChild.Name = 'nonXMLBody' Then
          Result.nonXMLBody := ParseNonXMLBody(sPath+'\nonXMLBody', oChild)
  Else If oChild.Name = 'structuredBody' Then
          Result.structuredBody := ParseStructuredBody(sPath+'\structuredBody', oChild)
        // end choice
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseComponentSect(Const sPath : String; oElement : TMXmlElement) : TcdaComponentSect;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaComponentSect.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'section' Then
          Result.section := ParseSection(sPath+'\section', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseComponent4(Const sPath : String; oElement : TMXmlElement) : TcdaComponent4;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaComponent4.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'sequenceNumber' Then
          Result.sequenceNumber := ParseINT(sPath+'\sequenceNumber', oChild)
  Else If oChild.Name = 'seperatableInd' Then
          Result.seperatableInd := ParseBL(sPath+'\seperatableInd', oChild)
        // choice :
  Else If oChild.Name = 'act' Then
          Result.act := ParseAct(sPath+'\act', oChild)
  Else If oChild.Name = 'encounter' Then
          Result.encounter := ParseEncounter(sPath+'\encounter', oChild)
  Else If oChild.Name = 'observation' Then
          Result.observation := ParseObservation(sPath+'\observation', oChild)
  Else If oChild.Name = 'observationMedia' Then
          Result.observationMedia := ParseObservationMedia(sPath+'\observationMedia', oChild)
  Else If oChild.Name = 'organizer' Then
          Result.organizer := ParseOrganizer(sPath+'\organizer', oChild)
  Else If oChild.Name = 'procedure' Then
          Result.procedure_ := ParseProcedure(sPath+'\procedure', oChild)
  Else If oChild.Name = 'regionOfInterest' Then
          Result.regionOfInterest := ParseRegionOfInterest(sPath+'\regionOfInterest', oChild)
  Else If oChild.Name = 'substanceAdministration' Then
          Result.substanceAdministration := ParseSubstanceAdministration(sPath+'\substanceAdministration', oChild)
  Else If oChild.Name = 'supply' Then
          Result.supply := ParseSupply(sPath+'\supply', oChild)
        // end choice
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;



Function TCDAParser.ParseConsent(Const sPath : String; oElement : TMXmlElement) : TcdaConsent;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaConsent.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'statusCode' Then
          Result.statusCode := ParseCS(sPath+'\statusCode', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseConsumable(Const sPath : String; oElement : TMXmlElement) : TcdaConsumable;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaConsumable.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'manufacturedProduct' Then
          Result.manufacturedProduct := ParseManufacturedProduct(sPath+'\manufacturedProduct', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseCriterion(Const sPath : String; oElement : TMXmlElement) : TcdaCriterion;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaCriterion.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'value' Then
          Result.value := ParseANY(sPath+'\value', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseCustodian(Const sPath : String; oElement : TMXmlElement) : TcdaCustodian;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaCustodian.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'assignedCustodian' Then
          Result.assignedCustodian := ParseAssignedCustodian(sPath+'\assignedCustodian', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseCustodianOrganization(Const sPath : String; oElement : TMXmlElement) : TcdaCustodianOrganization;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaCustodianOrganization.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'name' Then
          Result.Name := ParseEN(sPath+'\name', oChild)
  Else If oChild.Name = 'telecom' Then
          Result.telecom := ParseTEL(sPath+'\telecom', oChild)
  Else If oChild.Name = 'addr' Then
          Result.addr := ParseAD(sPath+'\addr', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseDataEnterer(Const sPath : String; oElement : TMXmlElement) : TcdaDataEnterer;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaDataEnterer.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'time' Then
          Result.time := ParseTS(sPath+'\time', oChild)
  Else If oChild.Name = 'assignedEntity' Then
          Result.assignedEntity := ParseAssignedEntity(sPath+'\assignedEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseDevice(Const sPath : String; oElement : TMXmlElement) : TcdaDevice;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaDevice.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'manufacturerModelName' Then
          Result.manufacturerModelName := ParseSC(sPath+'\manufacturerModelName', oChild)
  Else If oChild.Name = 'softwareName' Then
          Result.softwareName := ParseSC(sPath+'\softwareName', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseDocumentationOf(Const sPath : String; oElement : TMXmlElement) : TcdaDocumentationOf;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaDocumentationOf.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'serviceEvent' Then
          Result.serviceEvent := ParseServiceEvent(sPath+'\serviceEvent', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseEncompassingEncounter(Const sPath : String; oElement : TMXmlElement) : TcdaEncompassingEncounter;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaEncompassingEncounter.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'dischargeDispositionCode' Then
          Result.dischargeDispositionCode := ParseCD(sPath+'\dischargeDispositionCode', oChild)
  Else If oChild.Name = 'responsibleParty' Then
          Result.responsibleParty := ParseResponsibleParty(sPath+'\responsibleParty', oChild)
  Else If oChild.Name = 'encounterParticipant' Then
          Result.encounterParticipant.Add(ParseEncounterParticipant(sPath+'\encounterParticipant', oChild))
  Else If oChild.Name = 'location' Then
          Result.location := ParseLocation(sPath+'\location', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseEncounter(Const sPath : String; oElement : TMXmlElement) : TcdaEncounter;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaEncounter.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'statusCode' Then
          Result.statusCode := ParseCS(sPath+'\statusCode', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'priorityCode' Then
          Result.priorityCode := ParseCD(sPath+'\priorityCode', oChild)
  Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubject(sPath+'\subject', oChild)
  Else If oChild.Name = 'specimen' Then
          Result.specimen.Add(ParseSpecimen(sPath+'\specimen', oChild))
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer2(sPath+'\performer', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant2(sPath+'\participant', oChild))
  Else If oChild.Name = 'entryRelationship' Then
          Result.entryRelationship.Add(ParseEntryRelationship(sPath+'\entryRelationship', oChild))
  Else If oChild.Name = 'reference' Then
          Result.reference.Add(ParseReference(sPath+'\reference', oChild))
  Else If oChild.Name = 'precondition' Then
          Result.precondition.Add(ParsePrecondition(sPath+'\precondition', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    If oElement.Attribute['moodCode'] <> '' Then
      Result.moodCode := oElement.Attribute['moodCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseEncounterParticipant(Const sPath : String; oElement : TMXmlElement) : TcdaEncounterParticipant;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaEncounterParticipant.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'time' Then
          Result.time := ParseIVL(sPath+'\time', oChild, Tv3TS)
  Else If oChild.Name = 'assignedEntity' Then
          Result.assignedEntity := ParseAssignedEntity(sPath+'\assignedEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['typeCode'] <> '' Then
      Result.typeCode := oElement.Attribute['typeCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseEntity(Const sPath : String; oElement : TMXmlElement) : TcdaEntity;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaEntity.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'desc' Then
          Result.desc := ParseED(sPath+'\desc', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseEntry(Const sPath : String; oElement : TMXmlElement) : TcdaEntry;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaEntry.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
        // choice :
  Else If oChild.Name = 'act' Then
          Result.act := ParseAct(sPath+'\act', oChild)
  Else If oChild.Name = 'encounter' Then
          Result.encounter := ParseEncounter(sPath+'\encounter', oChild)
  Else If oChild.Name = 'observation' Then
          Result.observation := ParseObservation(sPath+'\observation', oChild)
  Else If oChild.Name = 'observationMedia' Then
          Result.observationMedia := ParseObservationMedia(sPath+'\observationMedia', oChild)
  Else If oChild.Name = 'organizer' Then
          Result.organizer := ParseOrganizer(sPath+'\organizer', oChild)
  Else If oChild.Name = 'procedure' Then
          Result.procedure_ := ParseProcedure(sPath+'\procedure', oChild)
  Else If oChild.Name = 'regionOfInterest' Then
          Result.regionOfInterest := ParseRegionOfInterest(sPath+'\regionOfInterest', oChild)
  Else If oChild.Name = 'substanceAdministration' Then
          Result.substanceAdministration := ParseSubstanceAdministration(sPath+'\substanceAdministration', oChild)
  Else If oChild.Name = 'supply' Then
          Result.supply := ParseSupply(sPath+'\supply', oChild)
        // end choice
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['typeCode'] <> '' Then
      Result.typeCode := oElement.Attribute['typeCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseEntryRelationship(Const sPath : String; oElement : TMXmlElement) : TcdaEntryRelationship;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaEntryRelationship.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'sequenceNumber' Then
          Result.sequenceNumber := ParseINT(sPath+'\sequenceNumber', oChild)
  Else If oChild.Name = 'seperatableInd' Then
          Result.seperatableInd := ParseBL(sPath+'\seperatableInd', oChild)
        // choice :
  Else If oChild.Name = 'act' Then
          Result.act := ParseAct(sPath+'\act', oChild)
  Else If oChild.Name = 'encounter' Then
          Result.encounter := ParseEncounter(sPath+'\encounter', oChild)
  Else If oChild.Name = 'observation' Then
          Result.observation := ParseObservation(sPath+'\observation', oChild)
  Else If oChild.Name = 'observationMedia' Then
          Result.observationMedia := ParseObservationMedia(sPath+'\observationMedia', oChild)
  Else If oChild.Name = 'organizer' Then
          Result.organizer := ParseOrganizer(sPath+'\organizer', oChild)
  Else If oChild.Name = 'procedure' Then
          Result.procedure_ := ParseProcedure(sPath+'\procedure', oChild)
  Else If oChild.Name = 'regionOfInterest' Then
          Result.regionOfInterest := ParseRegionOfInterest(sPath+'\regionOfInterest', oChild)
  Else If oChild.Name = 'substanceAdministration' Then
          Result.substanceAdministration := ParseSubstanceAdministration(sPath+'\substanceAdministration', oChild)
  Else If oChild.Name = 'supply' Then
          Result.supply := ParseSupply(sPath+'\supply', oChild)
        // end choice
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['typeCode'] <> '' Then
      Result.typeCode := oElement.Attribute['typeCode'];
    If oElement.Attribute['inversionInd'] <> '' Then
      Result.inversionInd := ParseBoolean(sPath+'\@inversionInd', oElement.Attribute['inversionInd']);
    If oElement.Attribute['contextConductionInd'] <> '' Then
      Result.contextConductionInd := ParseBoolean(sPath+'\@contextConductionInd', oElement.Attribute['contextConductionInd']);
    If oElement.Attribute['negationInd'] <> '' Then
      Result.negationInd := ParseBoolean(sPath+'\@negationInd', oElement.Attribute['negationInd']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseExternalAct(Const sPath : String; oElement : TMXmlElement) : TcdaExternalAct;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaExternalAct.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseExternalDocument(Const sPath : String; oElement : TMXmlElement) : TcdaExternalDocument;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaExternalDocument.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'setId' Then
          Result.setId := ParseII(sPath+'\setId', oChild)
  Else If oChild.Name = 'versionNumber' Then
          Result.versionNumber := ParseINT(sPath+'\versionNumber', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseExternalObservation(Const sPath : String; oElement : TMXmlElement) : TcdaExternalObservation;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaExternalObservation.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseExternalProcedure(Const sPath : String; oElement : TMXmlElement) : TcdaExternalProcedure;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaExternalProcedure.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseGuardian(Const sPath : String; oElement : TMXmlElement) : TcdaGuardian;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaGuardian.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
        // choice :
  Else If oChild.Name = 'guardianPerson' Then
          Result.guardianPerson := ParsePerson(sPath+'\guardianPerson', oChild)
  Else If oChild.Name = 'guardianOrganization' Then
          Result.guardianOrganization := ParseOrganization(sPath+'\guardianOrganization', oChild)
        // end choice
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseHealthCareFacility(Const sPath : String; oElement : TMXmlElement) : TcdaHealthCareFacility;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaHealthCareFacility.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'location' Then
          Result.location := ParsePlace(sPath+'\location', oChild)
  Else If oChild.Name = 'serviceProviderOrganization' Then
          Result.serviceProviderOrganization := ParseOrganization(sPath+'\serviceProviderOrganization', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseInformant12(Const sPath : String; oElement : TMXmlElement) : TcdaInformant12;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaInformant12.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
        // choice :
  Else If oChild.Name = 'assignedEntity' Then
          Result.assignedEntity := ParseAssignedEntity(sPath+'\assignedEntity', oChild)
  Else If oChild.Name = 'relatedEntity' Then
          Result.relatedEntity := ParseRelatedEntity(sPath+'\relatedEntity', oChild)
        // end choice
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseInformationRecipient(Const sPath : String; oElement : TMXmlElement) : TcdaInformationRecipient;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaInformationRecipient.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'intendedRecipient' Then
          Result.intendedRecipient := ParseIntendedRecipient(sPath+'\intendedRecipient', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['typeCode'] <> '' Then
      Result.typeCode := oElement.Attribute['typeCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseInFulfillmentOf(Const sPath : String; oElement : TMXmlElement) : TcdaInFulfillmentOf;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaInFulfillmentOf.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'order' Then
          Result.order := ParseOrder(sPath+'\order', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseIntendedRecipient(Const sPath : String; oElement : TMXmlElement) : TcdaIntendedRecipient;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaIntendedRecipient.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
  Else If oChild.Name = 'informationRecipient' Then
          Result.informationRecipient := ParsePerson(sPath+'\informationRecipient', oChild)
  Else If oChild.Name = 'receivedOrganization' Then
          Result.receivedOrganization := ParseOrganization(sPath+'\receivedOrganization', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseLabeledDrug(Const sPath : String; oElement : TMXmlElement) : TcdaLabeledDrug;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaLabeledDrug.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'name' Then
          Result.Name := ParseEN(sPath+'\name', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseLanguageCommunication(Const sPath : String; oElement : TMXmlElement) : TcdaLanguageCommunication;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaLanguageCommunication.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'languageCode' Then
          Result.languageCode := ParseCS(sPath+'\languageCode', oChild)
  Else If oChild.Name = 'modeCode' Then
          Result.modeCode := ParseCD(sPath+'\modeCode', oChild)
  Else If oChild.Name = 'proficiencyLevelCode' Then
          Result.proficiencyLevelCode := ParseCD(sPath+'\proficiencyLevelCode', oChild)
  Else If oChild.Name = 'preferenceInd' Then
          Result.preferenceInd := ParseBL(sPath+'\preferenceInd', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseEntityIdentifier(Const sPath : String; oElement : TMXmlElement) : TcdaEntityIdentifier;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaEntityIdentifier.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id := ParseII(sPath+'\id', oChild)
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseLegalAuthenticator(Const sPath : String; oElement : TMXmlElement) : TcdaLegalAuthenticator;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaLegalAuthenticator.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'time' Then
          Result.time := ParseTS(sPath+'\time', oChild)
  Else If oChild.Name = 'signatureCode' Then
          Result.signatureCode := ParseCS(sPath+'\signatureCode', oChild)
  Else If oChild.Name = 'assignedEntity' Then
          Result.assignedEntity := ParseAssignedEntity(sPath+'\assignedEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseLocation(Const sPath : String; oElement : TMXmlElement) : TcdaLocation;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaLocation.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'healthCareFacility' Then
          Result.healthCareFacility := ParseHealthCareFacility(sPath+'\healthCareFacility', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseMaintainedEntity(Const sPath : String; oElement : TMXmlElement) : TcdaMaintainedEntity;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaMaintainedEntity.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'maintainingPerson' Then
          Result.maintainingPerson := ParsePerson(sPath+'\maintainingPerson', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseManufacturedProduct(Const sPath : String; oElement : TMXmlElement) : TcdaManufacturedProduct;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaManufacturedProduct.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
        // choice :
  Else If oChild.Name = 'manufacturedLabeledDrug' Then
          Result.manufacturedLabeledDrug := ParseLabeledDrug(sPath+'\manufacturedLabeledDrug', oChild)
  Else If oChild.Name = 'manufacturedMaterial' Then
          Result.manufacturedMaterial := ParseMaterial(sPath+'\manufacturedMaterial', oChild)
        // end choice
  Else If oChild.Name = 'manufacturerOrganization' Then
          Result.manufacturerOrganization := ParseOrganization(sPath+'\manufacturerOrganization', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseMaterial(Const sPath : String; oElement : TMXmlElement) : TcdaMaterial;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaMaterial.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'name' Then
          Result.Name := ParseEN(sPath+'\name', oChild)
  Else If oChild.Name = 'lotNumberText' Then
          Result.lotNumberText := ParseST(sPath+'\lotNumberText', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseNonXMLBody(Const sPath : String; oElement : TMXmlElement) : TcdaNonXMLBody;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaNonXMLBody.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'languageCode' Then
          Result.languageCode := ParseCS(sPath+'\languageCode', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseObservation(Const sPath : String; oElement : TMXmlElement) : TcdaObservation;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaObservation.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'statusCode' Then
          Result.statusCode := ParseCS(sPath+'\statusCode', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'priorityCode' Then
          Result.priorityCode := ParseCD(sPath+'\priorityCode', oChild)
  Else If oChild.Name = 'repeatNumber' Then
          Result.repeatNumber := ParseIVL(sPath+'\repeatNumber', oChild, Tv3INT)
  Else If oChild.Name = 'languageCode' Then
          Result.languageCode := ParseCS(sPath+'\languageCode', oChild)
  Else If oChild.Name = 'value' Then
          Result.value.Add(ParseANY(sPath+'\value', oChild))
  Else If oChild.Name = 'interpretationCode' Then
          Result.interpretationCode.Add(ParseCD(sPath+'\interpretationCode', oChild))
  Else If oChild.Name = 'methodCode' Then
          Result.methodCode.Add(ParseCD(sPath+'\methodCode', oChild))
  Else If oChild.Name = 'targetSiteCode' Then
          Result.targetSiteCode.Add(ParseCD(sPath+'\targetSiteCode', oChild))
  Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubject(sPath+'\subject', oChild)
  Else If oChild.Name = 'specimen' Then
          Result.specimen.Add(ParseSpecimen(sPath+'\specimen', oChild))
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer2(sPath+'\performer', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant2(sPath+'\participant', oChild))
  Else If oChild.Name = 'entryRelationship' Then
          Result.entryRelationship.Add(ParseEntryRelationship(sPath+'\entryRelationship', oChild))
  Else If oChild.Name = 'reference' Then
          Result.reference.Add(ParseReference(sPath+'\reference', oChild))
  Else If oChild.Name = 'precondition' Then
          Result.precondition.Add(ParsePrecondition(sPath+'\precondition', oChild))
  Else If oChild.Name = 'referenceRange' Then
          Result.referenceRange.Add(ParseReferenceRange(sPath+'\referenceRange', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    If oElement.Attribute['moodCode'] <> '' Then
      Result.moodCode := oElement.Attribute['moodCode'];
    If oElement.Attribute['negationInd'] <> '' Then
      Result.negationInd := ParseBoolean(sPath+'\@negationInd', oElement.Attribute['negationInd']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseObservationMedia(Const sPath : String; oElement : TMXmlElement) : TcdaObservationMedia;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaObservationMedia.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'languageCode' Then
          Result.languageCode := ParseCS(sPath+'\languageCode', oChild)
  Else If oChild.Name = 'value' Then
          Result.value := ParseED(sPath+'\value', oChild)
  Else If oChild.Name = 'specimen' Then
          Result.specimen.Add(ParseSpecimen(sPath+'\specimen', oChild))
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer2(sPath+'\performer', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant2(sPath+'\participant', oChild))
  Else If oChild.Name = 'entryRelationship' Then
          Result.entryRelationship.Add(ParseEntryRelationship(sPath+'\entryRelationship', oChild))
  Else If oChild.Name = 'reference' Then
          Result.reference.Add(ParseReference(sPath+'\reference', oChild))
  Else If oChild.Name = 'precondition' Then
          Result.precondition.Add(ParsePrecondition(sPath+'\precondition', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['ID'] <> '' Then
      Result.ID_ := oElement.Attribute['ID'];
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    If oElement.Attribute['moodCode'] <> '' Then
      Result.moodCode := oElement.Attribute['moodCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseObservationRange(Const sPath : String; oElement : TMXmlElement) : TcdaObservationRange;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaObservationRange.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'value' Then
          Result.value := ParseANY(sPath+'\value', oChild)
  Else If oChild.Name = 'interpretationCode' Then
          Result.interpretationCode := ParseCD(sPath+'\interpretationCode', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseOrder(Const sPath : String; oElement : TMXmlElement) : TcdaOrder;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaOrder.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'priorityCode' Then
          Result.priorityCode := ParseCD(sPath+'\priorityCode', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseOrganization(Const sPath : String; oElement : TMXmlElement) : TcdaOrganization;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaOrganization.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'name' Then
          Result.name.Add(ParseEN(sPath+'\name', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'standardIndustryClassCode' Then
          Result.standardIndustryClassCode := ParseCD(sPath+'\standardIndustryClassCode', oChild)
  Else If oChild.Name = 'asOrganizationPartOf' Then
          Result.asOrganizationPartOf := ParseOrganizationPartOf(sPath+'\asOrganizationPartOf', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseOrganizationPartOf(Const sPath : String; oElement : TMXmlElement) : TcdaOrganizationPartOf;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaOrganizationPartOf.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'statusCode' Then
          Result.statusCode := ParseCS(sPath+'\statusCode', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'wholeOrganization' Then
          Result.wholeOrganization := ParseOrganization(sPath+'\wholeOrganization', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseOrganizer(Const sPath : String; oElement : TMXmlElement) : TcdaOrganizer;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaOrganizer.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'statusCode' Then
          Result.statusCode := ParseCS(sPath+'\statusCode', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubject(sPath+'\subject', oChild)
  Else If oChild.Name = 'specimen' Then
          Result.specimen.Add(ParseSpecimen(sPath+'\specimen', oChild))
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer2(sPath+'\performer', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant2(sPath+'\participant', oChild))
  Else If oChild.Name = 'reference' Then
          Result.reference.Add(ParseReference(sPath+'\reference', oChild))
  Else If oChild.Name = 'precondition' Then
          Result.precondition.Add(ParsePrecondition(sPath+'\precondition', oChild))
  Else If oChild.Name = 'component' Then
          Result.component.Add(ParseComponent4(sPath+'\component', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    If oElement.Attribute['moodCode'] <> '' Then
      Result.moodCode := oElement.Attribute['moodCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseParentDocument(Const sPath : String; oElement : TMXmlElement) : TcdaParentDocument;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaParentDocument.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'setId' Then
          Result.setId := ParseII(sPath+'\setId', oChild)
  Else If oChild.Name = 'versionNumber' Then
          Result.versionNumber := ParseINT(sPath+'\versionNumber', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseParticipant1(Const sPath : String; oElement : TMXmlElement) : TcdaParticipant1;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaParticipant1.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'functionCode' Then
          Result.functionCode := ParseCD(sPath+'\functionCode', oChild)
  Else If oChild.Name = 'time' Then
          Result.time := ParseIVL(sPath+'\time', oChild, Tv3TS)
  Else If oChild.Name = 'associatedEntity' Then
          Result.associatedEntity := ParseAssociatedEntity(sPath+'\associatedEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['typeCode'] <> '' Then
      Result.typeCode := oElement.Attribute['typeCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseParticipant2(Const sPath : String; oElement : TMXmlElement) : TcdaParticipant2;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaParticipant2.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'time' Then
          Result.time := ParseIVL(sPath+'\time', oChild, Tv3TS)
  Else If oChild.Name = 'awarenessCode' Then
          Result.awarenessCode := ParseCD(sPath+'\awarenessCode', oChild)
  Else If oChild.Name = 'participantRole' Then
          Result.participantRole := ParseParticipantRole(sPath+'\participantRole', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['typeCode'] <> '' Then
      Result.typeCode := oElement.Attribute['typeCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseParticipantRole(Const sPath : String; oElement : TMXmlElement) : TcdaParticipantRole;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaParticipantRole.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
        // choice :
  Else If oChild.Name = 'playingDevice' Then
          Result.playingDevice := ParseDevice(sPath+'\playingDevice', oChild)
  Else If oChild.Name = 'playingEntity' Then
          Result.playingEntity := ParsePlayingEntity(sPath+'\playingEntity', oChild)
        // end choice
  Else If oChild.Name = 'scopingEntity' Then
          Result.scopingEntity := ParseEntity(sPath+'\scopingEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParsePatient(Const sPath : String; oElement : TMXmlElement) : TcdaPatient;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaPatient.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild, ['asEntityIdentifier']) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id := ParseII(sPath+'\id', oChild)
  Else If oChild.Name = 'name' Then
          Result.name.Add(ParseEN(sPath+'\name', oChild))
  Else If oChild.Name = 'administrativeGenderCode' Then
          Result.administrativeGenderCode := ParseCD(sPath+'\administrativeGenderCode', oChild)
  Else If oChild.Name = 'birthTime' Then
          Result.birthTime := ParseTS(sPath+'\birthTime', oChild)
  Else If oChild.Name = 'maritalStatusCode' Then
          Result.maritalStatusCode := ParseCD(sPath+'\maritalStatusCode', oChild)
  Else If oChild.Name = 'religiousAffiliationCode' Then
          Result.religiousAffiliationCode := ParseCD(sPath+'\religiousAffiliationCode', oChild)
  Else If oChild.Name = 'raceCode' Then
          Result.raceCode := ParseCD(sPath+'\raceCode', oChild)
  Else If oChild.Name = 'ethnicGroupCode' Then
          Result.ethnicGroupCode := ParseCD(sPath+'\ethnicGroupCode', oChild)
  Else If oChild.Name = 'guardian' Then
          Result.guardian.Add(ParseGuardian(sPath+'\guardian', oChild))
  Else If oChild.Name = 'birthplace' Then
          Result.birthplace := ParseBirthplace(sPath+'\birthplace', oChild)
  Else If oChild.Name = 'languageCommunication' Then
          Result.languageCommunication.Add(ParseLanguageCommunication(sPath+'\languageCommunication', oChild))
  Else If oChild.Name = 'asEntityIdentifier' Then
          Result.asEntityIdentifier.Add(ParseEntityIdentifier(sPath+'\EntityIdentifier', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParsePatientRole(Const sPath : String; oElement : TMXmlElement) : TcdaPatientRole;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaPatientRole.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
  Else If oChild.Name = 'patient' Then
          Result.patient := ParsePatient(sPath+'\patient', oChild)
  Else If oChild.Name = 'providerOrganization' Then
          Result.providerOrganization := ParseOrganization(sPath+'\providerOrganization', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParsePerformer1(Const sPath : String; oElement : TMXmlElement) : TcdaPerformer1;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaPerformer1.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'functionCode' Then
          Result.functionCode := ParseCD(sPath+'\functionCode', oChild)
  Else If oChild.Name = 'time' Then
          Result.time := ParseIVL(sPath+'\time', oChild, Tv3TS)
  Else If oChild.Name = 'assignedEntity' Then
          Result.assignedEntity := ParseAssignedEntity(sPath+'\assignedEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['typeCode'] <> '' Then
      Result.typeCode := oElement.Attribute['typeCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParsePerformer2(Const sPath : String; oElement : TMXmlElement) : TcdaPerformer2;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaPerformer2.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'time' Then
          Result.time := ParseIVL(sPath+'\time', oChild, Tv3TS)
  Else If oChild.Name = 'modeCode' Then
          Result.modeCode := ParseCD(sPath+'\modeCode', oChild)
  Else If oChild.Name = 'assignedEntity' Then
          Result.assignedEntity := ParseAssignedEntity(sPath+'\assignedEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParsePerson(Const sPath : String; oElement : TMXmlElement) : TcdaPerson;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaPerson.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'name' Then
          Result.name.Add(ParseEN(sPath+'\name', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParsePlace(Const sPath : String; oElement : TMXmlElement) : TcdaPlace;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaPlace.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'name' Then
          Result.Name := ParseEN(sPath+'\name', oChild)
  Else If oChild.Name = 'addr' Then
          Result.addr := ParseAD(sPath+'\addr', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParsePlayingEntity(Const sPath : String; oElement : TMXmlElement) : TcdaPlayingEntity;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaPlayingEntity.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'quantity' Then
          Result.quantity.Add(ParsePQ(sPath+'\quantity', oChild))
  Else If oChild.Name = 'name' Then
          Result.name.Add(ParseEN(sPath+'\name', oChild))
  Else If oChild.Name = 'desc' Then
          Result.desc := ParseED(sPath+'\desc', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParsePrecondition(Const sPath : String; oElement : TMXmlElement) : TcdaPrecondition;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaPrecondition.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'criterion' Then
          Result.criterion := ParseCriterion(sPath+'\criterion', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseProcedure(Const sPath : String; oElement : TMXmlElement) : TcdaProcedure;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaProcedure.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'statusCode' Then
          Result.statusCode := ParseCS(sPath+'\statusCode', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'priorityCode' Then
          Result.priorityCode := ParseCD(sPath+'\priorityCode', oChild)
  Else If oChild.Name = 'languageCode' Then
          Result.languageCode := ParseCS(sPath+'\languageCode', oChild)
  Else If oChild.Name = 'methodCode' Then
          Result.methodCode.Add(ParseCD(sPath+'\methodCode', oChild))
  Else If oChild.Name = 'approachSiteCode' Then
          Result.approachSiteCode.Add(ParseCD(sPath+'\approachSiteCode', oChild))
  Else If oChild.Name = 'targetSiteCode' Then
          Result.targetSiteCode.Add(ParseCD(sPath+'\targetSiteCode', oChild))
  Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubject(sPath+'\subject', oChild)
  Else If oChild.Name = 'specimen' Then
          Result.specimen.Add(ParseSpecimen(sPath+'\specimen', oChild))
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer2(sPath+'\performer', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant2(sPath+'\participant', oChild))
  Else If oChild.Name = 'entryRelationship' Then
          Result.entryRelationship.Add(ParseEntryRelationship(sPath+'\entryRelationship', oChild))
  Else If oChild.Name = 'reference' Then
          Result.reference.Add(ParseReference(sPath+'\reference', oChild))
  Else If oChild.Name = 'precondition' Then
          Result.precondition.Add(ParsePrecondition(sPath+'\precondition', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    If oElement.Attribute['moodCode'] <> '' Then
      Result.moodCode := oElement.Attribute['moodCode'];
    If oElement.Attribute['negationInd'] <> '' Then
      Result.negationInd := ParseBoolean(sPath+'\@negationInd', oElement.Attribute['negationInd']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseProduct(Const sPath : String; oElement : TMXmlElement) : TcdaProduct;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaProduct.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'manufacturedProduct' Then
          Result.manufacturedProduct := ParseManufacturedProduct(sPath+'\manufacturedProduct', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseRecordTarget(Const sPath : String; oElement : TMXmlElement) : TcdaRecordTarget;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaRecordTarget.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'patientRole' Then
          Result.patientRole := ParsePatientRole(sPath+'\patientRole', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseReference(Const sPath : String; oElement : TMXmlElement) : TcdaReference;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaReference.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'seperatableInd' Then
          Result.seperatableInd := ParseBL(sPath+'\seperatableInd', oChild)
        // choice :
  Else If oChild.Name = 'externalAct' Then
          Result.externalAct := ParseExternalAct(sPath+'\externalAct', oChild)
  Else If oChild.Name = 'externalObservation' Then
          Result.externalObservation := ParseExternalObservation(sPath+'\externalObservation', oChild)
  Else If oChild.Name = 'externalProcedure' Then
          Result.externalProcedure := ParseExternalProcedure(sPath+'\externalProcedure', oChild)
  Else If oChild.Name = 'externalDocument' Then
          Result.externalDocument := ParseExternalDocument(sPath+'\externalDocument', oChild)
        // end choice
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['typeCode'] <> '' Then
      Result.typeCode := oElement.Attribute['typeCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseReferenceRange(Const sPath : String; oElement : TMXmlElement) : TcdaReferenceRange;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaReferenceRange.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'observationRange' Then
          Result.observationRange := ParseObservationRange(sPath+'\observationRange', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseRegionOfInterest(Const sPath : String; oElement : TMXmlElement) : TcdaRegionOfInterest;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaRegionOfInterest.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCS(sPath+'\code', oChild)
  Else If oChild.Name = 'value' Then
          Result.value.Add(ParseRegionOfInterestValue(sPath+'\value', oChild))
        Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubject(sPath+'\subject', oChild)
  Else If oChild.Name = 'specimen' Then
          Result.specimen.Add(ParseSpecimen(sPath+'\specimen', oChild))
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer2(sPath+'\performer', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant2(sPath+'\participant', oChild))
  Else If oChild.Name = 'entryRelationship' Then
          Result.entryRelationship.Add(ParseEntryRelationship(sPath+'\entryRelationship', oChild))
  Else If oChild.Name = 'reference' Then
          Result.reference.Add(ParseReference(sPath+'\reference', oChild))
  Else If oChild.Name = 'precondition' Then
          Result.precondition.Add(ParsePrecondition(sPath+'\precondition', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['ID'] <> '' Then
      Result.ID_ := oElement.Attribute['ID'];
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseRelatedDocument(Const sPath : String; oElement : TMXmlElement) : TcdaRelatedDocument;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaRelatedDocument.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'parentDocument' Then
          Result.parentDocument := ParseParentDocument(sPath+'\parentDocument', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['typeCode'] <> '' Then
      Result.typeCode := oElement.Attribute['typeCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseRelatedEntity(Const sPath : String; oElement : TMXmlElement) : TcdaRelatedEntity;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaRelatedEntity.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'relatedPerson' Then
          Result.relatedPerson := ParsePerson(sPath+'\relatedPerson', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseRelatedSubject(Const sPath : String; oElement : TMXmlElement) : TcdaRelatedSubject;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaRelatedSubject.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'addr' Then
          Result.addr.Add(ParseAD(sPath+'\addr', oChild))
  Else If oChild.Name = 'telecom' Then
          Result.telecom.Add(ParseTEL(sPath+'\telecom', oChild))
  Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubjectPerson(sPath+'\subject', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseResponsibleParty(Const sPath : String; oElement : TMXmlElement) : TcdaResponsibleParty;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaResponsibleParty.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'assignedEntity' Then
          Result.assignedEntity := ParseAssignedEntity(sPath+'\assignedEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseSection(Const sPath : String; oElement : TMXmlElement) : TcdaSection;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaSection.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id := ParseII(sPath+'\id', oChild)
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'title' Then
          Result.title := ParseST(sPath+'\title', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseSNText(sPath+'\text', oChild)
  Else If oChild.Name = 'confidentialityCode' Then
          Result.confidentialityCode := ParseCD(sPath+'\confidentialityCode', oChild)
  Else If oChild.Name = 'languageCode' Then
          Result.languageCode := ParseCS(sPath+'\languageCode', oChild)
  Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubject(sPath+'\subject', oChild)
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'entry' Then
          Result.entry.Add(ParseEntry(sPath+'\entry', oChild))
  Else If oChild.Name = 'component' Then
          Result.component.Add(ParseComponentSect(sPath+'\component', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['ID'] <> '' Then
      Result.ID_ := oElement.Attribute['ID'];
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseServiceEvent(Const sPath : String; oElement : TMXmlElement) : TcdaServiceEvent;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaServiceEvent.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseIVL(sPath+'\effectiveTime', oChild, Tv3TS)
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer1(sPath+'\performer', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['classCode'] <> '' Then
      Result.classCode := oElement.Attribute['classCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseSpecimen(Const sPath : String; oElement : TMXmlElement) : TcdaSpecimen;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaSpecimen.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'specimenRole' Then
          Result.specimenRole := ParseSpecimenRole(sPath+'\specimenRole', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseSpecimenRole(Const sPath : String; oElement : TMXmlElement) : TcdaSpecimenRole;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaSpecimenRole.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'specimenPlayingEntity' Then
          Result.specimenPlayingEntity := ParsePlayingEntity(sPath+'\specimenPlayingEntity', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseStructuredBody(Const sPath : String; oElement : TMXmlElement) : TcdaStructuredBody;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaStructuredBody.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'confidentialityCode' Then
          Result.confidentialityCode := ParseCD(sPath+'\confidentialityCode', oChild)
  Else If oChild.Name = 'languageCode' Then
          Result.languageCode := ParseCS(sPath+'\languageCode', oChild)
  Else If oChild.Name = 'component' Then
          Result.component.Add(ParseComponentSect(sPath+'\component', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseSubject(Const sPath : String; oElement : TMXmlElement) : TcdaSubject;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaSubject.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'awarenessCode' Then
          Result.awarenessCode := ParseCD(sPath+'\awarenessCode', oChild)
  Else If oChild.Name = 'relatedSubject' Then
          Result.relatedSubject := ParseRelatedSubject(sPath+'\relatedSubject', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseSubjectPerson(Const sPath : String; oElement : TMXmlElement) : TcdaSubjectPerson;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaSubjectPerson.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'name' Then
          Result.name.Add(ParseEN(sPath+'\name', oChild))
  Else If oChild.Name = 'administrativeGenderCode' Then
          Result.administrativeGenderCode := ParseCD(sPath+'\administrativeGenderCode', oChild)
  Else If oChild.Name = 'birthTime' Then
          Result.birthTime := ParseTS(sPath+'\birthTime', oChild)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseSubstanceAdministration(Const sPath : String; oElement : TMXmlElement) : TcdaSubstanceAdministration;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaSubstanceAdministration.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'statusCode' Then
          Result.statusCode := ParseCS(sPath+'\statusCode', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseGTS(sPath+'\effectiveTime', oChild) // note is var
  Else If oChild.Name = 'priorityCode' Then
          Result.priorityCode := ParseCD(sPath+'\priorityCode', oChild)
  Else If oChild.Name = 'repeatNumber' Then
          Result.repeatNumber := ParseIVL(sPath+'\repeatNumber', oChild, Tv3INT)
  Else If oChild.Name = 'routeCode' Then
          Result.routeCode := ParseCD(sPath+'\routeCode', oChild)
  Else If oChild.Name = 'approachSiteCode' Then
          Result.approachSiteCode.Add(ParseCD(sPath+'\approachSiteCode', oChild))
  Else If oChild.Name = 'doseQuantity' Then
          Result.doseQuantity := ParseIVL(sPath+'\doseQuantity', oChild, Tv3PQ)
  Else If oChild.Name = 'rateQuantity' Then
          Result.rateQuantity := ParseIVL(sPath+'\rateQuantity', oChild, Tv3PQ)
  Else If oChild.Name = 'maxDoseQuantity' Then
          Result.maxDoseQuantity := ParseRTO(sPath+'\maxDoseQuantity', oChild, Tv3PQ, Tv3PQ)
  Else If oChild.Name = 'administrationUnitCode' Then
          Result.administrationUnitCode := ParseCD(sPath+'\administrationUnitCode', oChild)
  Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubject(sPath+'\subject', oChild)
  Else If oChild.Name = 'specimen' Then
          Result.specimen.Add(ParseSpecimen(sPath+'\specimen', oChild))
  Else If oChild.Name = 'consumable' Then
          Result.consumable := ParseConsumable(sPath+'\consumable', oChild)
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer2(sPath+'\performer', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant2(sPath+'\participant', oChild))
  Else If oChild.Name = 'entryRelationship' Then
          Result.entryRelationship.Add(ParseEntryRelationship(sPath+'\entryRelationship', oChild))
  Else If oChild.Name = 'reference' Then
          Result.reference.Add(ParseReference(sPath+'\reference', oChild))
  Else If oChild.Name = 'precondition' Then
          Result.precondition.Add(ParsePrecondition(sPath+'\precondition', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['moodCode'] <> '' Then
      Result.moodCode := oElement.Attribute['moodCode'];
    If oElement.Attribute['negationInd'] <> '' Then
      Result.negationInd := ParseBoolean(sPath+'\@negationInd', oElement.Attribute['negationInd']);
    Result.Link;
  Finally
    result.Free;
  End;
End;


Function TCDAParser.ParseSupply(Const sPath : String; oElement : TMXmlElement) : TcdaSupply;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaSupply.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> nil Do
    Begin
      If checkNS(oChild) Then
      Begin
  If oChild.Name = 'realmCode' Then
          Result.realmCode.Add(ParseCS(sPath+'\realmCode', oChild))
  Else If oChild.Name = 'typeId' Then
          Result.typeId := ParseII(sPath+'\typeId', oChild)
  Else If oChild.Name = 'templateId' Then
          Result.templateId.Add(ParseII(sPath+'\templateId', oChild))
  Else If oChild.Name = 'id' Then
          Result.id.Add(ParseII(sPath+'\id', oChild))
  Else If oChild.Name = 'code' Then
          Result.code := ParseCD(sPath+'\code', oChild)
  Else If oChild.Name = 'text' Then
          Result.text := ParseED(sPath+'\text', oChild)
  Else If oChild.Name = 'statusCode' Then
          Result.statusCode := ParseCS(sPath+'\statusCode', oChild)
  Else If oChild.Name = 'effectiveTime' Then
          Result.effectiveTime := ParseGTS(sPath+'\effectiveTime', oChild) // var
  Else If oChild.Name = 'priorityCode' Then
          Result.priorityCode.Add(ParseCD(sPath+'\priorityCode', oChild))
  Else If oChild.Name = 'repeatNumber' Then
          Result.repeatNumber := ParseIVL(sPath+'\repeatNumber', oChild, Tv3INT)
  Else If oChild.Name = 'independentInd' Then
          Result.independentInd := ParseBL(sPath+'\independentInd', oChild)
  Else If oChild.Name = 'quantity' Then
          Result.quantity := ParsePQ(sPath+'\quantity', oChild)
  Else If oChild.Name = 'expectedUseTime' Then
          Result.expectedUseTime := ParseIVL(sPath+'\expectedUseTime', oChild, Tv3TS)
  Else If oChild.Name = 'subject' Then
          Result.subject := ParseSubject(sPath+'\subject', oChild)
  Else If oChild.Name = 'specimen' Then
          Result.specimen.Add(ParseSpecimen(sPath+'\specimen', oChild))
  Else If oChild.Name = 'product' Then
          Result.product := ParseProduct(sPath+'\product', oChild)
  Else If oChild.Name = 'performer' Then
          Result.performer.Add(ParsePerformer2(sPath+'\performer', oChild))
  Else If oChild.Name = 'author' Then
          Result.author.Add(ParseAuthor(sPath+'\author', oChild))
  Else If oChild.Name = 'informant' Then
          Result.informant.Add(ParseInformant12(sPath+'\informant', oChild))
  Else If oChild.Name = 'participant' Then
          Result.participant.Add(ParseParticipant2(sPath+'\participant', oChild))
  Else If oChild.Name = 'entryRelationship' Then
          Result.entryRelationship.Add(ParseEntryRelationship(sPath+'\entryRelationship', oChild))
  Else If oChild.Name = 'reference' Then
          Result.reference.Add(ParseReference(sPath+'\reference', oChild))
  Else If oChild.Name = 'precondition' Then
          Result.precondition.Add(ParsePrecondition(sPath+'\precondition', oChild))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
    // now, attributes:
    if GetXmlId(oElement) <> '' Then
      Result.xmlId := GetXmlId(oElement);
    If oElement.Attribute['nullFlavor'] <> '' Then
      Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    If oElement.Attribute['moodCode'] <> '' Then
      Result.moodCode := oElement.Attribute['moodCode'];
    Result.Link;
  Finally
    result.Free;
  End;
End;




function TCDAParser.ParseSNText(Const sPath : String; oElement: TMXmlElement): TsnText;
begin
  Result := TsnText.Create;
  Try
    AddToMap(oElement, Result);
    Result.nullFlavor := ParseNullFlavor(sPath+'\@nullFlavor', oElement.Attribute['nullFlavor']);
    Result.ID := oElement.Attribute['ID'];
    Result.language := oElement.Attribute['language'];
    ParseSet(sPath+'\@styleCode', Result.StyleCode, oElement.Attribute['styleCode']);
    ParseCMGeneralList(sPath, Result.parts, oElement, nil);
    Result.Link;
  Finally
    Result.Free;
  End;
end;

Procedure TCDAParser.ParseSNBase(Const sPath : String; oElement : TMXmlElement; oBase : TsnBase);
Begin
  oBase.ID := oElement.Attribute['ID'];
  oBase.language := oElement.Attribute['language'];
  ParseSet(sPath+'\@styleCode', oBase.StyleCode, oElement.Attribute['styleCode']);
End;

Procedure TCDAParser.ParseCMGeneralList(Const sPath : String; oParts : TsnCMGeneralList; oElement: TMXmlElement; oOwner : TsnCaptioned);
var
  oChild : TMXmlElement;
Begin
  oChild := oElement.first;
  while (oChild <> nil) Do
  Begin
    if oChild.nodeType = ntText then
      if StripWhitespace Then
        oParts.Append.text := TsnString.create(Trim(oChild.text, true))
      else
        oParts.Append.text := TsnString.create(oChild.text)
    else if oChild.nodeType = ntElement then
    Begin
      if oChild.Name = 'content' Then
        oParts.Append.content := ParseContent(sPath+'\content', oChild as TMXmlElement)
      Else if oChild.Name = 'linkHtml' Then
        oParts.Append.linkHtml := ParseLinkHTML(sPath+'\linkHtml', oChild as TMXmlElement)
      Else if oChild.Name = 'footnote' Then
        oParts.Append.footnote := ParseFootnote(sPath+'\footnote', oChild as TMXmlElement)
      Else if oChild.Name = 'footnoteRef' Then
        oParts.Append.footnoteRef := ParseFootnoteRef(sPath+'\footnoteRef', oChild as TMXmlElement)
      Else if oChild.Name = 'paragraph' Then
        oParts.Append.paragraph := ParseParagraph(sPath+'\paragraph', oChild as TMXmlElement)
      Else if oChild.Name = 'list' Then
        oParts.Append.list := ParseList(sPath+'\list', oChild as TMXmlElement)
      Else if oChild.Name = 'table' Then
        oParts.Append.table := ParseTable(sPath+'\table', oChild as TMXmlElement)
      Else if oChild.Name = 'renderMultiMedia' Then
        oParts.Append.renderMultiMedia := ParseRenderMultiMedia(sPath+'\renderMultiMedia', oChild as TMXmlElement)
      Else if oChild.Name = 'sub' Then
        oParts.Append.sub := ParseSub(sPath+'\sub', oChild as TMXmlElement)
      Else if oChild.Name = 'sup' Then
        oParts.Append.sup := ParseSup(sPath+'\sup', oChild as TMXmlElement)
      Else if oChild.Name = 'br' Then
        oParts.Append.br := ParseBr(sPath+'\br', oChild as TMXmlElement)
      Else if (oChild.Name = 'caption') And (oOwner <> nil) Then
        oOwner.caption := ParseCaption(sPath+'\nil', oChild as TMXmlElement)
      Else If Errors Then
        RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
    End;
    oChild := oChild.next;
  End;
End;

Procedure TCDAParser.ParseCMContentList(Const sPath : String; oParts : TsnCMContentList; oElement: TMXmlElement; oOwner : TsnCaptioned);
var
  oChild : TMXmlelement;
Begin
  oChild := oElement.first;
  while (oChild <> nil) Do
  Begin
    if oChild.nodeType = ntText then
      if StripWhitespace Then
        oParts.Append.text := TsnString.create(Trim(oChild.text, true))
      else
        oParts.Append.text := TsnString.create(oChild.text)
    else if oChild.nodeType = ntElement then
    Begin
      if oChild.Name = 'content' Then
        oParts.Append.content := ParseContent(sPath+'\content', oChild as TMXmlElement)
      Else if oChild.Name = 'linkHtml' Then
        oParts.Append.linkHtml := ParseLinkHTML(sPath+'\linkHtml', oChild as TMXmlElement)
      Else if oChild.Name = 'footnote' Then
        oParts.Append.footnote := ParseFootnote(sPath+'\footnote', oChild as TMXmlElement)
      Else if oChild.Name = 'footnoteRef' Then
        oParts.Append.footnoteRef := ParseFootnoteRef(sPath+'\footnoteRef', oChild as TMXmlElement)
      Else if oChild.Name = 'renderMultiMedia' Then
        oParts.Append.renderMultiMedia := ParseRenderMultiMedia(sPath+'\renderMultiMedia', oChild as TMXmlElement)
      Else if oChild.Name = 'sub' Then
        oParts.Append.sub := ParseSub(sPath+'\sub', oChild as TMXmlElement)
      Else if oChild.Name = 'sup' Then
        oParts.Append.sup := ParseSup(sPath+'\sup', oChild as TMXmlElement)
      Else if oChild.Name = 'br' Then
        oParts.Append.br := ParseBr(sPath+'\br', oChild as TMXmlElement)
      Else if (oChild.Name = 'caption') And (oOwner <> nil) Then
        oOwner.caption := ParseCaption(sPath+'\nil', oChild as TMXmlElement)
      Else If Errors Then
        RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
    End;
    oChild := oChild.next;
  End;
End;



Procedure TCDAParser.ParseCMFootnotesList(Const sPath : String; oParts : TsnCMFootnotesList; oElement: TMXmlElement);
var
  oChild : TMXmlelement;
Begin
  oChild := oElement.first;
  while (oChild <> nil) Do
  Begin
    if oChild.nodeType = ntText then
      if StripWhitespace Then
        oParts.Append.text := TsnString.create(Trim(oChild.text, true))
      else
        oParts.Append.text := TsnString.create(oChild.text)
    else if oChild.nodeType = ntElement then
    Begin
      if oChild.Name = 'footnote' Then
        oParts.Append.footnote := ParseFootnote(sPath+'\footnote', oChild as TMXmlElement)
      Else if oChild.Name = 'footnoteRef' Then
        oParts.Append.footnoteRef := ParseFootnoteRef(sPath+'\footnoteRef', oChild as TMXmlElement)
      Else If Errors Then
        RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
    End;
    oChild := oChild.next;
  End;
End;


Procedure TCDAParser.ParseCMInlineList(Const sPath : String; oParts : TsnCMInlineList; oElement: TMXmlElement);
var
  oChild : TMXmlelement;
Begin
  oChild := oElement.first;
  while (oChild <> nil) Do
  Begin
    if oChild.nodeType = ntText then
      if StripWhitespace Then
        oParts.Append.text := TsnString.create(Trim(oChild.text, true))
      else
        oParts.Append.text := TsnString.create(oChild.text)
    else if oChild.nodeType = ntElement then
    Begin
      if oChild.Name = 'linkHtml' Then
        oParts.Append.linkHtml := ParseLinkHTML(sPath+'\linkHtml', oChild as TMXmlElement)
      Else if oChild.Name = 'footnote' Then
        oParts.Append.footnote := ParseFootnote(sPath+'\footnote', oChild as TMXmlElement)
      Else if oChild.Name = 'footnoteRef' Then
        oParts.Append.footnoteRef := ParseFootnoteRef(sPath+'\footnoteRef', oChild as TMXmlElement)
      Else if oChild.Name = 'sub' Then
        oParts.Append.sub := ParseSub(sPath+'\sub', oChild as TMXmlElement)
      Else if oChild.Name = 'sup' Then
        oParts.Append.sup := ParseSup(sPath+'\sup', oChild as TMXmlElement)
      Else If Errors Then
        RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
    End;
    oChild := oChild.next;
  End;
End;


Function TCDAParser.ParseBr(Const sPath : String; oElement : TMXmlElement) : TsnBr;
Begin
  Result := TsnBr.create;
  Try
    AddToMap(oElement, Result);
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseSup(Const sPath : String; oElement : TMXmlElement) : TsnString;
Begin
  Result := TsnString.Create(TextContent(oElement, ttTrimPad));
  Try
    AddToMap(oElement, Result);
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseSub(Const sPath : String; oElement : TMXmlElement) : TsnString;
Begin
  Result := TsnString.Create(TextContent(oElement, ttTrimPad));
  Try
    AddToMap(oElement, Result);
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseContent(Const sPath : String; oElement : TMXmlElement) : TsnContent;
Begin
  Result := TsnContent.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    Result.revised := TsnRevised(ParseEnum(sPath+'\@revised', oElement.Attribute['revised'], CODES_TsnRevised));
    ParseCMContentList(sPath, Result.parts, oElement, nil);
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseLinkHTML(Const sPath : String; oElement : TMXmlElement) : TsnLinkHtml;
Begin
  Result := TsnLinkHtml.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    Result.name := oElement.Attribute['name'];
    Result.href := oElement.Attribute['href'];
    Result.rel := oElement.Attribute['rel'];
    Result.rev := oElement.Attribute['rev'];
    Result.title := oElement.Attribute['title'];
    ParseCMFootnotesList(sPath, Result.parts, oElement);
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseFootnote(Const sPath : String; oElement : TMXmlElement) : TsnFootnote;
Begin
  Result := TsnFootNote.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseCMGeneralList(sPath, Result.parts, oElement, nil);
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseFootnoteRef(Const sPath : String; oElement : TMXmlElement) : TsnFootnoteRef;
Begin
  Result := TsnFootNoteRef.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    Result.IDREF := oElement.Attribute['IDREF'];
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseParagraph(Const sPath : String; oElement : TMXmlElement) : TsnParagraph;
Begin
  Result := TsnParagraph.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseCMContentList(sPath, Result.parts, oElement, Result);
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseList(Const sPath : String; oElement : TMXmlElement) : TsnList;
var
  oChild : TMXmlelement;
Begin
  Result := TsnList.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    Result.listType := TsnListType(ParseEnum(sPath+'\@listType', oElement.Attribute['listType'], CODES_TsnListType));

    oChild := oElement.first;
    while (oChild <> nil) Do
    Begin
      if oChild.nodeType = ntText then
      Begin
        if not StringIsWhitespace(oChild.text) And Errors Then
          RaiseError('Parse', 'Unexpected Text content in list: '+oChild.text+' @'+sPath);
      End
      else if oChild.nodeType = ntElement then
      Begin
        if oChild.Name = 'item' Then
          result.item.Add(ParseItem(sPath+'\item', oChild as TMXmlElement))
        Else if oChild.Name = 'caption' Then
          result.caption := ParseCaption(sPath+'\caption', oChild as TMXmlElement)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End;
      oChild := oChild.next;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseItem(Const sPath : String; oElement : TMXmlElement) : TsnItem;
Begin
  Result := TsnItem.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseCMGeneralList(sPath, Result.parts, oElement, Result);
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseRenderMultiMedia(Const sPath : String; oElement : TMXmlElement) : TsnRenderMultiMedia;
var
  oChild : TMXmlelement;
Begin
  Result := TsnRenderMultiMedia.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseSet(sPath+'\@referencedObject', Result.referencedObject, oElement.Attribute['referencedObject']);

    oChild := oElement.first;
    while (oChild <> nil) Do
    Begin
      if oChild.nodeType = ntText then
      Begin
        if not StringIsWhitespace(oChild.text) And Errors Then
          RaiseError('Parse', 'Unexpected Text content in list: '+oChild.text+' @'+sPath);
      End
      else if oChild.nodeType = ntElement then
      Begin
        if oChild.Name = 'caption' Then
          result.caption := ParseCaption(sPath+'\caption', oChild as TMXmlElement)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End;
      oChild := oChild.next;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Function TCDAParser.ParseCaption(Const sPath : String; oElement : TMXmlElement) : TsnCaption;
Begin
  Result := TsnCaption.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseCMInlineList(sPath, Result.parts, oElement);
    Result.Link;
  Finally
    Result.Free;
  End;
End;



function TCDAParser.ParseTable(Const sPath : String; oElement: TMXmlElement): TsnTable;
var
  oChild : TMXmlelement;
begin
  Result := TsnTable.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    Result.summary := oElement.Attribute['summary'];
    Result.width := oElement.Attribute['width'];
    Result.border := oElement.Attribute['border'];
    Result.frame := TsnFrame(ParseEnum(sPath+'\@frame', oElement.Attribute['frame'], CODES_TsnFrame));
    Result.rules := TsnRules(ParseEnum(sPath+'\@rules', oElement.Attribute['rules'], CODES_TsnRules));
    Result.cellspacing := oElement.Attribute['cellspacing'];
    Result.cellpadding := oElement.Attribute['cellpadding'];

    oChild := oElement.first;
    while (oChild <> nil) Do
    Begin
      if oChild.nodeType = ntText then
      Begin
        if not StringIsWhitespace(oChild.text) And Errors Then
          RaiseError('Parse', 'Unexpected Text content in list: '+oChild.text+' @'+sPath);
      End
      else if oChild.nodeType = ntElement then
      Begin
        if oChild.Name = 'caption' Then
          result.caption := ParseCaption(sPath+'\caption', oChild as TMXmlElement)
        else if oChild.Name = 'col' Then
          result.col.Add(ParseCol(sPath+'\col', oChild as TMXmlElement))
        else if oChild.Name = 'colgroup' Then
          result.colgroup.Add(ParseColGroup(sPath+'\colgroup', oChild as TMXmlElement))
        else if oChild.Name = 'thead' Then
          result.thead := ParseRowGroup(sPath+'\thead', oChild as TMXmlElement)
        else if oChild.Name = 'tfoot' Then
          result.tfoot := ParseRowGroup(sPath+'\tfoot', oChild as TMXmlElement)
        else if oChild.Name = 'tbody' Then
          result.tbody.Add(ParseRowGroup(sPath+'\tbody', oChild as TMXmlElement))
        else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End;
      oChild := oChild.next;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;
end;


Function TCDAParser.ParseCol(Const sPath : String; oElement : TMXmlElement) : TsnCol;
begin
  Result := TsnCol.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseColItem(sPath, oElement, Result);
    Result.Link;
  Finally
    Result.Free;
  End;
End;

Procedure TCDAParser.ParseColItem(Const sPath : String; oElement : TMXmlElement; oBase : TsnColItem);
Begin
  If oElement.Attribute['span'] <> '' Then
    oBase.span := ParseInteger(sPath+'\@span', oElement.Attribute['span']);
  oBase.width := oElement.Attribute['width'];
  ParseTableItem(sPath, oElement, oBase);
End;


Procedure TCDAParser.ParseTableItem(Const sPath : String; oElement : TMXmlElement; oBase : TsnTableItem);
Begin
  oBase.char_ := oElement.Attribute['char'];
  oBase.charoff := oElement.Attribute['charoff'];
  oBase.align := TsnAlign(ParseEnum(sPath+'\@align', oElement.Attribute['align'], CODES_TsnAlign));
  oBase.valign := TsnVAlign(ParseEnum(sPath+'\@valign', oElement.Attribute['valign'], CODES_TsnVAlign));
End;

Function TCDAParser.ParseColGroup(Const sPath : String; oElement : TMXmlElement) : TsnColGroup;
var
  oChild : TMXmlelement;
begin
  Result := TsnColGroup.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseColItem(sPath, oElement, Result);

    oChild := oElement.first;
    while (oChild <> nil) Do
    Begin
      if oChild.nodeType = ntText then
      Begin
        if not StringIsWhitespace(oChild.text) And Errors Then
          RaiseError('Parse', 'Unexpected Text content in list: '+oChild.text+' @'+sPath);
      End
      else if oChild.nodeType = ntElement then
      Begin
        if oChild.Name = 'col' Then
          result.col.Add(ParseCol(sPath+'\col', oChild as TMXmlElement))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End;
      oChild := oChild.next;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;


Function TCDAParser.ParseRowGroup(Const sPath : String; oElement : TMXmlElement) : TsnTRowGroup;
var
  oChild : TMXmlelement;
begin
  Result := TsnTRowGroup.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseTableItem(sPath, oElement, Result);

    oChild := oElement.first;
    while (oChild <> nil) Do
    Begin
      if oChild.nodeType = ntText then
      Begin
        if not StringIsWhitespace(oChild.text) And Errors Then
          RaiseError('Parse', 'Unexpected Text content in list: '+oChild.text+' @'+sPath);
      End
      else if oChild.nodeType = ntElement then
      Begin
        if oChild.Name = 'tr' Then
          result.tr.Add(ParseRow(sPath+'\tr', oChild as TMXmlElement))
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End;
      oChild := oChild.next;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
End;

function TCDAParser.ParseRow(Const sPath : String; oElement: TMXmlElement): TsnTRow;
var
  oChild : TMXmlelement;
begin
  Result := TsnTRow.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseTableItem(sPath, oElement, Result);

    oChild := oElement.first;
    while (oChild <> nil) Do
    Begin
      if oChild.nodeType = ntText then
      Begin
        if not StringIsWhitespace(oChild.text) And Errors Then
          RaiseError('Parse', 'Unexpected Text content in list: '+oChild.text+' @'+sPath);
      End
      else if oChild.nodeType = ntElement then
      Begin
        if oChild.Name = 'th' Then
          result.parts.Append.th := ParseCell(sPath+'\th', oChild as TMXmlElement)
        Else if oChild.Name = 'td' Then
          result.parts.Append.td := ParseCell(sPath+'\td', oChild as TMXmlElement)
        Else If Errors Then
          RaiseError('Parse', 'Unexpected element '+oChild.Name+' @'+sPath);
      End;
      oChild := oChild.next;
    End;
    Result.Link;
  Finally
    Result.Free;
  End;
end;

function TCDAParser.ParseCell(Const sPath : String; oElement: TMXmlElement): TsnTCell;
begin
  Result := TsnTCell.Create;
  Try
    AddToMap(oElement, Result);
    ParseSNBase(sPath, oElement, Result);
    ParseTableItem(sPath, oElement, Result);
    Result.scope := TsnCellScope(ParseEnum(sPath+'\@scope', oElement.Attribute['scope'], CODES_TsnCellScope));
    ParseSet(sPath+'\@headers', result.headers, oElement.Attribute['headers']);
    result.abbr := oElement.Attribute['abbr'];
    result.axis := oElement.Attribute['axis'];
    if oElement.Attribute['rowspan'] <> '' Then
      result.rowspan := ParseInteger(sPath+'\@rowspan', oElement.Attribute['rowspan']);
    if oElement.Attribute['colspan'] <> '' Then
      result.colspan := ParseInteger(sPath+'\@colspan', oElement.Attribute['colspan']);
    ParseCMGeneralList(sPath, Result.parts, oElement, nil);
    Result.Link;
  Finally
    Result.Free;
  End;
end;

function TCDAParser.ParseRegionOfInterestValue(Const sPath : String; oElement: TMXmlElement): TcdaRegionOfInterest_value;
var
  oChild : TMXmlElement;
  i : integer;
Begin
  i := 0;
  Result := TcdaRegionOfInterest_value.Create;
  Try
    AddToMap(oElement, Result);
    ParseQTYValue(sPath, oElement, result);
    If oElement.Attribute['value'] <> '' Then
      Result.value := ParseInteger(sPath+'\@value', oElement.Attribute['value']);

    If oElement.Attribute['unsorted'] <> '' Then
      Result.unsorted := ParseBoolean(sPath+'\@unsorted', oElement.Attribute['unsorted']);

    TakeComments(Result);
    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      if CheckNS(oChild) Then
      Begin
        if Errors Then
          RaiseError('ParseINT', 'Unexpected Element '+oChild.Name+' @'+sPath);
      End
      else
        Result.Extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;

    Result.Link;
  Finally
    Result.Free;
  End;

end;

procedure TCDAParser.SeeComment(oNode: TMXmlelement);
begin
  FComments.Add(oNode.text);
end;

constructor TCDAParser.Create;
begin
  inherited;
  FComments := TFslStringList.Create;
  FIdentifiedObjects := TFslStringObjectMatch.Create;
end;

destructor TCDAParser.Destroy;
begin
  FIdentifiedObjects.Free;
  FComments.Free;
  inherited;
end;

procedure TCDAParser.TakeComments(oDT: Tv3Base);
begin
  if FComments.Count > 0 Then
  Begin
    oDt.comments.Assign(FComments);
    FComments.Clear;
  End;
end;

procedure TCDAParser.TakeComments(oFocus: TcdaBase);
begin
  if FComments.Count > 0 Then
  Begin
    oFocus.comments.Assign(FComments);
    FComments.Clear;
  End;
end;

procedure TCDAParser.TakeComments(oDT: Tv3XP);
begin
  if FComments.Count > 0 Then
  Begin
    oDt.comments.Assign(FComments);
    FComments.Clear;
  End;
end;

function TCDAParser.ErrorClass: EFslExceptionClass;
begin
  Result := ECDAException;
end;

procedure TCDAParser.AddToMap(oElement: TMXmlElement; oObject: Tv3Base);
var
  id : string;
begin
  If FMapping Then
  Begin
    oObject.Element := oElement;
    oElement.attribute[MAGIC_ATTRIBUTE_NAME] := IntToHex(Integer(oObject), 8);
  End;
  id := oElement.Attribute['ID'];
  if id <> '' then
    FIdentifiedObjects.Add(id, oObject.Link);
end;

function TCDAParser.isXmlContent(oElement: TMXmlElement): Boolean;
var
  oNode : TMXmlelement;
  iText : Integer;
  bElement : Boolean;
begin
  // it's not xml content if there is
  //   all text children
  //   a mix of whitespace text, and elements translation, reference, thumbnail
  oNode := oElement.first;
  result := false;
  iText := 0;
  bElement := false;
  while (oNode <> nil) do
  Begin
    case oNode.NodeType Of
      ntElement :
        bElement := (oNode.name <> 'translation') and (oNode.name <> 'reference') and (oNode.name <> 'thumbnail');
      ntText:
        if Not StringIsWhitespace(oNode.text) then
          inc(iText);
      ntCData:
        result := true;
    Else
      ; // ignore
    End;
    oNode := oNode.next;
  End;
  result := result or bElement or (iText > 1);
end;

function TCDAParser.ParseExtension(offset: integer; const sPath: String; oElement: TMXmlElement): Tv3Extension;
//var
//  oChild : TMXmlElement;
//  extension : Tv3Extension;
//  i : integer;
Begin
  Result := Tv3Extension.Create;
  Try
    AddToMap(oElement, Result);
    TakeComments(Result);

    result.Name := oElement.Name;
    result.namespace := oElement.namespaceURI;
    result.offset := offset;
    raise Exception.Create('to do');
(*
    for i := 0 to oElement.attributes.Count - 1 Do
    begin
      extension := Tv3Extension.create;
      try
        extension.namespace := oElement.attributes[i].namespaceURI;
        extension.name := '@'+oElement.attributee[i].Name;
        extension.text := oElement.attributes[i].text;
        result.extensions.add(extension.link);
      finally
        extension.free;
      end;
    end;

    i := 0;
    oChild := oElement.firstElement;
    While oChild <> Nil Do
    Begin
      result.extensions.add(ParseExtension(i, sPath+'\'+oChild.Name, oChild));
      inc(i);
      oChild := oChild.nextElement;
    End;
*)
    Result.Link;
  Finally
    Result.Free;
  End;
end;

function TCDAParser.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FComments.sizeInBytes);
  inc(result, FIdentifiedObjects.sizeInBytes);
end;

end.

