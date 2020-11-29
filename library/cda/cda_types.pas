unit cda_types;

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
  fsl_utilities, fsl_stream, fsl_collections, fsl_xml,
  fsl_ucum,
  cda_base;

 Const
  UID_SNOMED = '2.16.840.1.113883.6.96';
// \b(([\w-]+://?|www[.])[^\s()<>]+(?:\([\w\d]+\)|([^[:punct:]\s]|/)))n

Type
  {
    NullFlavor that applies to the property. Note that nfNull means Not Null
  }
  Tv3NullFlavor = (nfNull, nfNI, nfINV, nfOTH, nfNINF, nfPINF, nfUNC, nfDER, nfUNK, nfASKU, nfNAV, nfQS, nfNASK, nfTRC, nfMSK, nfNA);

  {
    Update mode. Should always be umNull in a CDA document
  }
  Tv3UpdateMode = (umNull, umA, umD, umR, umAR, umN, umU, umK);

  {
    Coding Rationale for why a code is provided (CD and PQ/PQR)
  }
  Tv3CodingRationale = (crNull, crO, crP, crR, crOR, crPR);

  {
    Lists possible uses for telecommunication addresses
  }
  Tv3TelecommunicationAddressUse = (tauNull, tauH, tauHP, tauHV, tauWP, tauDIR, tauPUB, tauBAD, tauTMP, tauAS, tauEC, tauMC, tauPG);

  {
    Lists capabilities a telecommunication device may have (associated with a phone number)
  }
  Tv3TelecommunicationCapability = (tcNull, tcVoice, tcFax, tcData, tcTty, tcSms);

  {
    Possible scopes for an identifier
  }
  Tv3IdentifierScope = (isNull, isBUSN, isOBJ, isVER, isVW);

  {
    Possible reliability markers for an identifier
  }
  Tv3IdentifierReliability = (irNull, irISS, irVRF, irUNV);

  {
    Possible address part types
  }
  Tv3AddressPartType = (aptNull, aptAL, aptADL, aptUNID, aptUNIT, aptDAL, aptDINST, aptDINSTA, aptDINSTQ, aptDMOD, aptDMODID, aptSAL, aptBNR, aptBNN,
         aptBNS, aptSTR, aptSTB, aptSTTYP, aptDIR, aptINT, aptCAR, aptCEN, aptCNT, aptCPA, aptCTY, aptDEL, aptPOB, aptPRE, aptSTA, aptZIP);

  {
    Possible types for a name part
  }
  Tv3EntityNamePartType = (nptNull, nptFAM, nptGIV, nptTITLE, nptDEL);

  {
    Possible qualifiers on a name part
  }
  Tv3EntityNamePartQualifier = (npqNull, npqLS, npqAC, npqNB, npqPR, npqHON, npqBR, npqAD, npqSP, npqMID, npqCL, npqIN, npqPFX, npqSFX, npqVV);

  {
    Lists Timing events that a repeating event may relate to
  }
  Tv3TimingEvent = (teNull, teHS, teWAKE, teAC, teACM, teACD, teACV, teIC, teICM, teICD, teICV, tePC, tePCM, tePCD, tePCV, teC, teCM, teCD, teCV);

  {
    Possible compression types for content
  }
  Tv3Compression = (cNull, cDF, cGZ, cZL, cZ, cBZ, cZ7);

  {
    Possible integrity check alorithms for referenced data
  }
  Tv3IntegrityCheckAlgorithm = (icaNull, icaSHA1, icaSHA256);

  {
    Lists possible uses for an address
  }
  Tv3PostalAddressUse = (pauNull, pauH, pauHP, pauHV, pauWP, pauDIR, pauPUB, pauBAD, pauTMP, pauABC, pauIDE, pauSYL, pauPHYS, pauPST, pauSRCH, pauSNDX, pauPHON);

  {
    Lists possible uses for a name
  }
  Tv3EntityNameUse = (nuNull, nuC, nuL,  nuOR, nuT, nuI, nuP, nuA, nuR, nuOLD, nuDN, nuM, nuSRCH, nuPHON, nuABC, nuSYL, nuIDE);

  {
    List of possible uncertainty distribution algorithms
  }
  Tv3UncertaintyType = (utNull, utU, utN, utLN, utG, utE, utX2, utT, utF, utB);

  {
    possible relationships between a releating event and the calender
  }
  Tv3CalendarCycle = (ccNull, ccCY, ccMY, ccCM, ccCW, ccWY, ccDM, ccCD, ccDY, ccDW, ccHD, ccCH, ccNH, ccCN, ccSN, ccCS);


Const
  CODES_Tv3NullFlavor : Array[Tv3NullFlavor] of String = ('', 'NI', 'INV', 'OTH', 'NINF', 'PINF', 'UNC', 'DER', 'UNK', 'ASKU', 'NAV', 'QS', 'NASK', 'TRC', 'MSK', 'NA');
  CODES_Tv3UpdateMode : Array[Tv3UpdateMode] of String = ('', 'A', 'D', 'R', 'AR', 'N', 'U', 'K');
  CODES_Tv3CodingRationale : Array[Tv3CodingRationale] of String = ('', 'O', 'P', 'R', 'OR', 'PR');
  CODES_Tv3TelecommunicationAddressUse : Array[Tv3TelecommunicationAddressUse] of String = ('', 'H', 'HP', 'HV', 'WP', 'DIR', 'PUB', 'BAD', 'TMP', 'AS', 'EC', 'MC', 'PG');
  CODES_Tv3TelecommunicationCapability : Array[Tv3TelecommunicationCapability] of String = ('', 'voice', 'fax', 'data', 'tty', 'sms');
  CODES_Tv3IdentifierScope : Array[Tv3IdentifierScope] of String = ('', 'BUSN', 'OBJ', 'VER', 'VW');
  CODES_Tv3IdentifierReliability : Array[Tv3IdentifierReliability] of String = ('', 'ISS', 'VRF', 'UNV');
  CODES_Tv3AddressPartType : Array[Tv3AddressPartType] of String = ('', 'AL', 'ADL', 'UNID', 'UNIT', 'DAL', 'DINST', 'DINSTA', 'DINSTQ', 'DMOD', 'DMODID', 'SAL', 'BNR', 'BNN',
         'BNS', 'STR', 'STB', 'STTYP', 'DIR', 'INT', 'CAR', 'CEN', 'CNT', 'CPA', 'CTY', 'DEL', 'POB', 'PRE', 'STA', 'ZIP');
  CODES_Tv3EntityNamePartType : Array[Tv3EntityNamePartType] of String = ('', 'FAM', 'GIV', 'TITLE', 'DEL');
  CODES_Tv3EntityNamePartQualifier : Array[Tv3EntityNamePartQualifier] of String = ('', 'LS', 'AC', 'NB', 'PR', 'HON', 'BR', 'AD', 'SP', 'MID', 'CL', 'IN', 'PFX', 'SFX', 'VV');
  CODES_Tv3TimingEvent : Array[Tv3TimingEvent] of String = ('', 'HS', 'WAKE', 'AC', 'ACM', 'ACD', 'ACV', 'IC', 'ICM', 'ICD', 'ICV', 'PC', 'PCM', 'PCD', 'PCV', 'C', 'CM', 'CD', 'CV');
  CODES_Tv3Compression : Array[Tv3Compression] of String = ('', 'DF', 'GZ', 'ZL', 'Z', 'BZ', 'Z7');
  CODES_Tv3IntegrityCheckAlgorithm : Array[Tv3IntegrityCheckAlgorithm] of String = ('', 'SHA-1', 'SHA-256');
  CODES_Tv3PostalAddressUse : Array[Tv3PostalAddressUse] of String = ('', 'H', 'HP', 'HV', 'WP', 'DIR', 'PUB', 'BAD', 'TMP', 'ABC', 'IDE', 'SYL', 'PHYS', 'PST', 'SRCH', 'SNDX', 'PHON');
  CODES_Tv3EntityNameUse : Array[Tv3EntityNameUse] of String = ('', 'C', 'L', 'OR', 'T', 'I', 'P', 'A', 'R', 'OLD', 'DN', 'M', 'SRCH', 'PHON', 'ABC', 'SYL', 'IDE');
  CODES_Tv3UncertaintyType : Array[Tv3UncertaintyType] of String = ('', 'U', 'N', 'LN', 'G', 'E', 'X2', 'T', 'F', 'B');
  CODES_Tv3CalendarCycle : Array[Tv3CalendarCycle] of String = ('', 'CY', 'MY', 'CM', 'CW', 'WY', 'DM', 'CD', 'DY', 'DW', 'HD', 'CH', 'NH', 'CN', 'SN', 'CS');

  TAG_NAMES_ADDRESS : Array[Tv3AddressPartType] of String = ('', 'addressLine', 'additionalLocator', 'unitID', 'unitType', 'deliveryAddressLine', 'deliveryInstallationType',
         'deliveryInstallationArea', 'deliveryInstallationQualifier', 'deliveryMode', 'deliveryModeIdentifier', 'streetAddressLine', 'houseNumber', 'houseNumberNumeric',
         'buildingNumberSuffix', 'streetName', 'streetNameBase', 'streetNameType', 'direction', 'intersection', 'careOf', 'censusTract',
         'country', 'county', 'city', 'delimiter', 'postBox', 'precinct', 'state', 'postalCode');
  TAG_NAMES_NAME  : Array[Tv3EntityNamePartType] of String = ('', 'family', 'given', 'title', 'delimiter');

Type

  Tv3SetTelecommunicationAddressUse = Set of Tv3TelecommunicationAddressUse;
  Tv3SetTelecommunicationCapability = Set of Tv3TelecommunicationCapability;
  Tv3SetPostalAddressUse = Set Of Tv3PostalAddressUse;
  Tv3SetEntityNamePartQualifier = Set Of Tv3EntityNamePartQualifier;
  Tv3SetEntityNameUse = Set of Tv3EntityNameUse;


Function SetAsInteger(aSet : Tv3SetTelecommunicationAddressUse) : Integer; Overload;
Function SetAsInteger(aSet : Tv3SetTelecommunicationCapability) : Integer; Overload;
Function SetAsInteger(aSet : Tv3SetPostalAddressUse) : Integer; Overload;
Function SetAsInteger(aSet : Tv3SetEntityNamePartQualifier) : Integer; Overload;
Function SetAsInteger(aSet : Tv3SetEntityNameUse) : Integer; Overload;

Function IntegerAsSetTelecommunicationAddressUse(i : Integer) : Tv3SetTelecommunicationAddressUse;
Function IntegerAsSetTelecommunicationCapability(i : Integer) : Tv3SetTelecommunicationCapability;
Function IntegerAsSetPostalAddressUse(i : Integer) : Tv3SetPostalAddressUse;
Function IntegerAsSetEntityNamePartQualifier(i : Integer) : Tv3SetEntityNamePartQualifier;
Function IntegerAsSetEntityNameUse(i : Integer) : Tv3SetEntityNameUse;


Type
  Tv3ANY = class;
  Tv3ST = class;
  Tv3TEL = class;
  Tv3CD = class;
  Tv3QSET = class;
  Tv3IVL = class;

  Tv3ListANY = class;
  Tv3SetED = class;
  Tv3SetST = class;
  Tv3SetCD = class;
  Tv3ListCR = class;
  Tv3ListADXP = class;
  Tv3ListENXP = class;
  Tv3SetPQR = class;
  Tv3SetQSET = class;
  Tv3SetQTY = class;
  Tv3SetUVP = class;

  Tv3DataValue = class (Tv3Base)
  protected
    Function CDAClassTypeV : TCDAClassType; Override;
  End;
  Tv3DataType = class of Tv3DataValue;

  Tv3DataTypeCollection = class (Tv3BaseList)
  End;
  Tv3DataTypeCollectionType = class of Tv3DataTypeCollection;

  Tv3HXIT = class (Tv3DataValue)
  private
    FvalidTimeHigh: String;
    FvalidTimeLow: String;
    FcontrolInformationExtension: String;
    FcontrolInformationRoot: String;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public

    function Link : Tv3HXIT; Overload;
    Function Clone(parent : Tv3Base) : Tv3HXIT; Overload;
    procedure Assign(oSource : TFslObject); Override;

  published
    Property validTimeLow : String read FvalidTimeLow Write FvalidTimeLow;
    Property validTimeHigh : String read FvalidTimeHigh Write FvalidTimeHigh;
    Property controlInformationRoot : String read FcontrolInformationRoot Write FcontrolInformationRoot;
    Property controlInformationExtension : String read FcontrolInformationExtension Write FcontrolInformationExtension;
  End;

  Tv3ANY = class (Tv3HXIT)
  private
    FnullFlavor: Tv3NullFlavor;
    FflavorId: String;  // space separated?
    FupdateMode: Tv3UpdateMode;
  Protected
    Function nullFlavorLiteral : String;
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Virtual;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    function Link : Tv3ANY; Overload;
    Function Clone(parent : Tv3Base) : Tv3ANY; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;


    {
      A literal string representation of the data, where one is defined in the specification
    }
    Function Literal : String;
    {
       Convenience property
    }
    Function isNonNull : Boolean;
    {
       Convenience property
    }
    Function isNull : Boolean;
  published
    Property nullFlavor : Tv3NullFlavor read FnullFlavor Write FnullFlavor;
    Property updateMode : Tv3UpdateMode read FupdateMode Write FupdateMode;
    Property flavorId : String read FflavorId Write FflavorId;
  End;


  Tv3BL = class (Tv3ANY)
  private
    FHasValue : Boolean;
    Fvalue: boolean;
    procedure Setvalue(const Value: boolean); Overload;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Overload; Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;

    function Link : Tv3BL; Overload;
    Function Clone(parent : Tv3Base) : Tv3BL; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;

  published
    Property value : boolean read Fvalue Write Setvalue;

    {
      false if value is null, true if value is either true or false
    }
    Property HasValue : Boolean read FHasValue;
  End;


  Tv3ED = class (Tv3ANY)
  private
    FmediaType: String;
    Fcharset: String;
    Flanguage: String;
    Fdata: TFslBuffer;
    FintegrityCheck: TFslBuffer;
    Fxml: TMXmlElement;
    Fcompression: Tv3Compression;
    Fthumbnail: Tv3ED;
    Ftranslation: Tv3SetED;
    FintegrityCheckAlgorithm: Tv3IntegrityCheckAlgorithm;
    Fdescription: Tv3ST;
    Freference: Tv3TEL;
    Fvalue: String;
    FRepresentation: String;
    procedure Setdata(const Value: TFslBuffer);
    procedure Setdescription(const Value: Tv3ST);
    procedure SetintegrityCheck(const Value: TFslBuffer);
    procedure Setreference(const Value: Tv3TEL);
    procedure Setthumbnail(const Value: Tv3ED);
    procedure Settranslation(const Value: Tv3SetED);
    function GetDataAsString: String;
    procedure SetDataAsString(const Value: String);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Link : Tv3ED; Overload;
    Function Clone(parent : Tv3Base) : Tv3ED; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;
    function Getbytes : TBytes;
    property representation : String read FRepresentation write FRepresentation;


    {
      Fill out the data from a file. If text or xml is populated, they will be set to null
    }
    Procedure LoadDataFromFile(Const sFilename : String);
    {
      Save the data to a file. If the data property is not populated, test or xml will be used
    }
    Procedure SaveDataToFile(Const sFilename : String);
    {
      Fill out the data from a stream (such as a sql.ColMemory). If text or xml is populated, they will be set to null
    }
    Procedure LoadDataFromStream(oStream : TStream);
    {
      Save the data to a stream (use with BindBlob). If the data property is not populated, test or xml will be used
    }
    Procedure SaveDataToStream(oStream : TStream);
    {
      Shortcut method. Add an ED to the list of translations.
    }
    Function AddTranslation : Tv3ED;

  published
    Property value : String read Fvalue write Fvalue;

    Property xml : TMXmlElement read Fxml write Fxml;
    Property data : TFslBuffer read Fdata write Setdata;

    {
      Access the raw bytes of data as a string. This may not work on all platforms
    }
    Property dataAsString : String read GetDataAsString write SetDataAsString;
    Property reference : Tv3TEL read Freference write Setreference;
    Property mediaType : String read FmediaType write FmediaType;
    Property charset : String read Fcharset write Fcharset;
    Property language : String read Flanguage write Flanguage;
    Property compression : Tv3Compression read Fcompression write Fcompression;
    Property integrityCheck : TFslBuffer read FintegrityCheck write SetintegrityCheck;
    Property integrityCheckAlgorithm : Tv3IntegrityCheckAlgorithm read FintegrityCheckAlgorithm write FintegrityCheckAlgorithm;
    Property description : Tv3ST read Fdescription write Setdescription;
    Property thumbnail : Tv3ED read Fthumbnail write Setthumbnail;
    Property translation : Tv3SetED read Ftranslation write Settranslation;
  End;

  Tv3ST = class (Tv3ANY)
  private
    Flanguage: String;
    Ftranslation: Tv3SetST;
    Fvalue: String;
    procedure Settranslation(const Value: Tv3SetST);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Link : Tv3ST; Overload;
    Function Clone(parent : Tv3Base) : Tv3ST; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;

    {
      Shortcut method. Add an ST to the list of translations.
    }
    Function AddTranslation : Tv3ST;
  published
    Property value : String read Fvalue write Fvalue;
    Property language : String read Flanguage write Flanguage;
    Property translation : Tv3SetST read Ftranslation write Settranslation;
  End;

  Tv3SC = class (Tv3ST)
  private
    Fcode: Tv3CD;
    procedure Setcode(const Value: Tv3CD);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    function Link : Tv3SC; Overload;
    Function Clone(parent : Tv3Base) : Tv3SC; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;

  published
     Property code : Tv3CD read Fcode write Setcode;
  End;

  Tv3CR = class (Tv3ANY)
  private
    Finverted: boolean;
    FHasinverted: boolean;
    Fvalue: Tv3CD;
    Fname: Tv3CD;
    procedure Setinverted(const Value: boolean);
    procedure Setname(const Value: Tv3CD);
    procedure Setvalue(const Value: Tv3CD);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    function Link : Tv3CR; Overload;
    Function Clone(parent : Tv3Base) : Tv3CR; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;

  published
   {
      Specifies the manner in which the concept role value contributes to the meaning of a code phrase. For example, if SNOMED RT defines a concept "leg", a role relation "has-laterality", and another concept "left", the concept role relation allows to add the qualifier "has-laterality: left" to a primary code "leg" to construct the meaning "left leg". In this example, name is "has-laterality".
    }
    property name : Tv3CD read Fname write Setname;

   {
      The concept that modifies the primary code of a code phrase through the role relation. For example, if SNOMED RT defines a concept "leg", a role relation "has-laterality", and another concept "left", the concept role relation allows adding the qualifier "has-laterality: left" to a primary code "leg" to construct the meaning "left leg". In this example, value is "left".
    }
    property value : Tv3CD read Fvalue write Setvalue;

   {
      Indicates if the sense of name is inverted. This can be used in cases where the underlying code system defines inversion but does not provide reciprocal pairs of role names. By default, inverted is false.
    }
    property inverted : boolean read Finverted write Setinverted;

   {
      false if inverted is null, true if inverted is either true or false
    }
    property HasInverted : boolean read FHasinverted write FHasinverted;
  End;

  Tv3CD = class (Tv3ANY)
  private
    ForiginalText: Tv3ED;
    FdisplayName: Tv3ST;
    FvalueSetVersion: String;
    FvalueSet: String;
    FcodeSystem: String;
    FcodeSystemName: String;
    Fcode: String;
    FcodeSystemVersion: String;
    FcodingRationale: Tv3CodingRationale;
    Ftranslation: Tv3SetCD;
    Fqualifier: Tv3ListCR;
    procedure SetdisplayName(const Value: Tv3ST);
    procedure SetoriginalText(const Value: Tv3ED);
    procedure Settranslation(const Value: Tv3SetCD);
    procedure Setqualifier(const Value: Tv3ListCR);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Link : Tv3CD; Overload;
    Function Clone(parent : Tv3Base) : Tv3CD; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;
    function render : String;

    {
      Shortcut method. Add an ED to the list of translations.
    }
    Function AddTranslation(code : String; codeSystem : String) : Tv3CD;
  published
    Property code : String read Fcode write Fcode;
    Property codeSystem : String read FcodeSystem write FcodeSystem;
    Property codeSystemName : String read FcodeSystemName write FcodeSystemName;
    Property codeSystemVersion : String read FcodeSystemVersion write FcodeSystemVersion;
    Property valueSet : String read FvalueSet write FvalueSet;
    Property valueSetVersion : String read FvalueSetVersion write FvalueSetVersion;
    Property displayName : Tv3ST read FdisplayName write SetdisplayName;
    Property originalText : Tv3ED read ForiginalText write SetoriginalText;
    Property codingRationale : Tv3CodingRationale read FcodingRationale write FcodingRationale;
    Property translation : Tv3SetCD read Ftranslation write Settranslation;

    {
      Specifies additional codes that increase the specificity of the the primary code.
    }
    Property qualifier : Tv3ListCR read Fqualifier write Setqualifier;
  End;

  Tv3CS = class (Tv3ANY)
  private
    Fcode: String;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
    function sizeInBytesV : cardinal; override;
  public

    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    function Link : Tv3CS; Overload;
    Function RIMClassNameV: String; Override;
    Function Clone(parent : Tv3Base) : Tv3CS; Overload;

  published
    Property code : String read Fcode write Fcode;
  End;

  Tv3TEL = class (Tv3ANY)
  private
    Fvalue: String;
    FuseablePeriod: Tv3QSET;
    Fuse: Tv3SetTelecommunicationAddressUse;
    Fcapabilities: Tv3SetTelecommunicationCapability;
    procedure SetuseablePeriod(const Value: Tv3QSET);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    function Link : Tv3TEL; Overload;
    Function RIMClassNameV: String; Override;
    Function Clone(parent : Tv3Base) : Tv3TEL; Overload;
    function render : String;

    {
      the canonical representation of the URL (consult ISO 21090 for further details)
    }
    Function canonical : String;
  published
    Property value : String read Fvalue write Fvalue;
    Property use : Tv3SetTelecommunicationAddressUse read Fuse write Fuse;
    Property capabilities : Tv3SetTelecommunicationCapability read Fcapabilities write Fcapabilities;
    Property useablePeriod : Tv3QSET read FuseablePeriod write SetuseablePeriod;
  End;

  Tv3II = class (Tv3ANY)
  private
    Fdisplayable: boolean;
    Fextension: String;
    FidentifierName: String;
    Froot: String;
    Freliability: Tv3IdentifierReliability;
    Fscope: Tv3IdentifierScope;
    FHasDisplayable : Boolean;
    procedure Setdisplayable(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
    function sizeInBytesV : cardinal; override;
  public

    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    function Link : Tv3II; Overload;
    Function RIMClassNameV: String; Override;
    Function Clone(parent : Tv3Base) : Tv3II; Overload;
    function render : String;

  published
    Property root : String read Froot write Froot;
    Property extension : String read Fextension write Fextension;
    Property identifierName : String read FidentifierName write FidentifierName;
    Property displayable : boolean read Fdisplayable write Setdisplayable;
    {
      false if displayable is null, true if displayable is either true or false
    }
    Property HasDisplayable : Boolean read FHasDisplayable;
    Property scope: Tv3IdentifierScope read Fscope write Fscope;
    Property reliability : Tv3IdentifierReliability read Freliability write Freliability;
  End;

  Tv3XP = class (Tv3DataValue)
  private
    FcodeSystem: String;
    FcodeSystemVersion: String;
    Fcode: String;
    Flanguage: String;
    FnullFlavor: Tv3NullFlavor;
    Fvalue: String;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public

    function Link : Tv3XP; Overload;
    Function Clone(parent : Tv3Base) : Tv3XP; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;

    {
       Convenience property
    }
    Function isNonNull : Boolean;
  published
    Property nullFlavor : Tv3NullFlavor read FnullFlavor write FnullFlavor;
    Property value : String read Fvalue write Fvalue;
    Property code : String read Fcode write Fcode;
    Property codeSystem : String read FcodeSystem write FcodeSystem;
    Property codeSystemVersion : String read FcodeSystemVersion write FcodeSystemVersion;
    Property language : String read Flanguage write Flanguage;
  End;

  Tv3ADXP = class (Tv3XP)
  private
    Ftype: Tv3AddressPartType;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
  public

    function Link : Tv3ADXP; Overload;
    Function Clone(parent : Tv3Base) : Tv3ADXP; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;

  published
    Property type_ : Tv3AddressPartType read Ftype write Ftype;
  End;

  Tv3AD = class (Tv3ANY)
  private
    FisNotOrdered: boolean;
    FHasIsNotOrdered: boolean;
    Fpart: Tv3ListADXP;
    FuseablePeriod: Tv3QSET;
    Fuse: Tv3SetPostalAddressUse;
    procedure Setpart(const Value: Tv3ListADXP);
    procedure SetuseablePeriod(const Value: Tv3QSET);
    procedure SetisNotOrdered(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Link : Tv3AD; Overload;
    Function Clone(parent : Tv3Base) : Tv3AD; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;


    {
      Shortcut method. Add an address part with a type which is one of the Tv3AddressPartType enumerated constants.

      [[Tv3AddressPartType]]
    }
    Function AddPart(sValue : String; type_ : Tv3AddressPartType) : Tv3ADXP;

    {
       true if the address includes a part that matches oPart (same type and value)
    }
    Function HasPart(oPart : Tv3ADXP) : Boolean;
  published
    Property part : Tv3ListADXP read Fpart write Setpart;
    Property use : Tv3SetPostalAddressUse read Fuse write Fuse;
    Property useablePeriod : Tv3QSET read FuseablePeriod write SetuseablePeriod;
    Property isNotOrdered : boolean read FisNotOrdered write SetisNotOrdered;
    {
      false if isNotOrdered is null, true if isNotOrdered is either true or false
    }
    Property HasIsNotOrdered : boolean read FHasIsNotOrdered write FHasIsNotOrdered;
  End;

  Tv3ENXP = class (Tv3XP)
  private
    Ftype: Tv3EntityNamePartType;
    Fqualifier: Tv3SetEntityNamePartQualifier;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
  public

    function Link : Tv3ENXP; Overload;
    Function Clone(parent : Tv3Base) : Tv3ENXP; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;

  published
     Property type_ : Tv3EntityNamePartType read Ftype write Ftype;
     Property qualifier : Tv3SetEntityNamePartQualifier read Fqualifier write Fqualifier;
  End;

  Tv3EN = class (Tv3ANY)
  private
    Fpart: Tv3ListENXP;
    Fuse: Tv3SetEntityNameUse;
    procedure Setpart(const Value: Tv3ListENXP);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Link : Tv3EN; Overload;
    Function Clone(parent : Tv3Base) : Tv3EN; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;

    function render : String;

    {
      Shortcut method. Add a name part with a type which is one of the Tv3EntityNamePartType enumerated constants.

      [[Tv3EntityNamePartType]]
    }
    Function AddPart(sValue : String; type_ : Tv3EntityNamePartType) : Tv3ENXP;

    {
      Shortcut method. Add an part and mark it up as a prefix
    }
    function AddPlain(sValue : String): Tv3ENXP;

    {
      Shortcut method. Add an part and mark it up as a given name
    }
    function AddGiven(sValue : String): Tv3ENXP;

    {
      Shortcut method. Add an part and mark it up as a family name
    }
    function AddFamily(sValue : String): Tv3ENXP;

    {
      Shortcut method. Add an part and mark it up as a prefix
    }
    function AddPrefix(sValue : String): Tv3ENXP;

    {
      Shortcut method. Add an part and mark it up as a prefix
    }
    function AddSuffix(sValue : String): Tv3ENXP;

    {
      The name with a standard name ordering imposed
    }
    Function canonical : Tv3EN;
  published
    Property part : Tv3ListENXP read Fpart write Setpart;
    Property use : Tv3SetEntityNameUse read Fuse write Fuse;
  End;


  Tv3QTYDatatype = class of Tv3QTY;

  Tv3QTY = class (Tv3ANY)
  private
    ForiginalText: Tv3ED;
    Fexpression: Tv3ED;
    FuncertainRange: Tv3IVL;
    Funcertainty: Tv3QTY;
    FuncertaintyType: Tv3UncertaintyType;
    procedure Setexpression(const Value: Tv3ED);
    procedure SetoriginalText(const Value: Tv3ED);
    procedure SetuncertainRange(const Value: Tv3IVL);
    procedure Setuncertainty(const Value: Tv3QTY);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    function Link : Tv3QTY; Overload;
    Function Clone(parent : Tv3Base) : Tv3QTY; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Class function DiffType : Tv3QTYDatatype; Virtual;
    Function RIMClassNameV: String; Override;

  published
    Property expression : Tv3ED read Fexpression write Setexpression;
    Property originalText : Tv3ED read ForiginalText write SetoriginalText;
    Property uncertainty : Tv3QTY read Funcertainty write Setuncertainty;
    Property uncertaintyType : Tv3UncertaintyType read FuncertaintyType write FuncertaintyType;
    Property uncertainRange : Tv3IVL read FuncertainRange write SetuncertainRange;
  End;

  Tv3INT = class (Tv3QTY)
  private
    FHasValue : Boolean;
    Fvalue: int64;
    procedure Setvalue(const Value: int64);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
  public
    constructor Create; Override;

    function Link : Tv3INT; Overload;
    Function Clone(parent : Tv3Base) : Tv3INT; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Class function DiffType : Tv3QTYDatatype; Override;
    Function RIMClassNameV: String; Override;

  published
    {
      false if value is null, true if value is either true or false
    }
    Property HasValue : Boolean read FHasValue;
    Property value : int64 read Fvalue write Setvalue;
  End;

  Tv3CO = class (Tv3QTY)
  private
    Fvalue: TFslDecimal;
    Fcode: Tv3CD;
    procedure Setcode(const Value: Tv3CD);
    procedure Setvalue(const Value: TFslDecimal);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    function Link : Tv3CO; Overload;
    Function Clone(parent : Tv3Base) : Tv3CO; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Class function DiffType : Tv3QTYDatatype; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property value : TFslDecimal read Fvalue write Setvalue;
    Property code : Tv3CD read Fcode write Setcode;
  End;

  Tv3REAL = class (Tv3QTY)
  private
    Fvalue: TFslDecimal;
    procedure Setvalue(const Value: TFslDecimal);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
  public
    destructor Destroy; Override;

    function Link : Tv3REAL; Overload;
    Function Clone(parent : Tv3Base) : Tv3REAL; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Class function DiffType : Tv3QTYDatatype; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property value : TFslDecimal read Fvalue write Setvalue;
  End;

 Tv3RTO = class (Tv3QTY)
  private
    Fdenominator: Tv3QTY;
    Fnumerator: Tv3QTY;
    FDenominatorType: Tv3QTYDatatype;
    FNumeratorType: Tv3QTYDatatype;
    procedure Setdenominator(const Value: Tv3QTY);
    procedure Setnumerator(const Value: Tv3QTY);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    function Link : Tv3RTO; Overload;
    Function Clone(parent : Tv3Base) : Tv3RTO; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Class function DiffType : Tv3QTYDatatype; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property numerator : Tv3QTY read Fnumerator write Setnumerator;
    Property NumeratorType : Tv3QTYDatatype read FNumeratorType write FNumeratorType;
    Property denominator : Tv3QTY read Fdenominator write Setdenominator;
    Property DenominatorType : Tv3QTYDatatype read FDenominatorType write FDenominatorType;
  End;

  Tv3PQ = class (Tv3QTY)
  private
    Fvalue: TFslDecimal;
    Funit: String;
    FcodingRationale: Tv3CodingRationale;
    Ftranslation: Tv3SetPQR;
    procedure Settranslation(const Value: Tv3SetPQR);
    procedure Setvalue(const Value: TFslDecimal);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Link : Tv3PQ; Overload;
    Function Clone(parent : Tv3Base) : Tv3PQ; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Class function DiffType : Tv3QTYDatatype; Override;
    Function RIMClassNameV: String; Override;

    {
      The canonical form for the PQ (canonical UCUM units with appropriate conversion)
    }
    Function canonical : TV3PQ;
    {
      whether other is a PQ and has the same canonical units
    }
    Function comparable(other : TV3QTY) : Tv3BL;
  {$IFNDEF FPC}Published{$ENDIF}
    Property value : TFslDecimal read Fvalue write Setvalue;
    Property codingRationale : Tv3CodingRationale read FcodingRationale write FcodingRationale;
    Property unit_ : String read Funit write Funit;
    Property translation : Tv3SetPQR read Ftranslation write Settranslation;
  End;

  Tv3PQR = class (Tv3CD)
  private
    Fvalue: TFslDecimal;
    procedure Setvalue(const Value: TFslDecimal);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    function Link : Tv3PQR; Overload;
    Function Clone(parent : Tv3Base) : Tv3PQR; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property value : TFslDecimal read Fvalue write Setvalue;
  End;

  Tv3MO = class (Tv3QTY)
  private
    Fvalue: TFslDecimal;
    Fcurrency: String;
    procedure Setvalue(const Value: TFslDecimal);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    function Link : Tv3MO; Overload;
    Function Clone(parent : Tv3Base) : Tv3MO; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Class function DiffType : Tv3QTYDatatype; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property value : TFslDecimal read Fvalue write Setvalue;
    Property currency : String read Fcurrency write Fcurrency;
  End;

  Tv3TS = class (Tv3QTY)
  private
    Fvalue: String;
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    Function LiteralV : String; Override;
    function sizeInBytesV : cardinal; override;
  public

    function Link : Tv3TS; Overload;
    Function Clone(parent : Tv3Base) : Tv3TS; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Class function DiffType : Tv3QTYDatatype; Override;
    Function RIMClassNameV: String; Override;
    function render : String;

  {$IFNDEF FPC}Published{$ENDIF}
    Property value : String read Fvalue write Fvalue;
  End;


  Tv3QSETDatatype = class of Tv3QSET;

  Tv3QSET = class (Tv3ANY)
  private
    ForiginalText: Tv3ED;
    FParamType : Tv3QTYDatatype;
    FFlat: Boolean;
    procedure SetoriginalText(const Value: Tv3ED);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Virtual;
    destructor Destroy; Override;

    function Link : Tv3QSET; Overload;
    Function Clone(parent : Tv3Base) : Tv3QSET; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function AddComp(oComp : Tv3QSET) : Boolean; Virtual;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    {
      Whether a GTS is flat (parts of the base QSET part are encoded as a series
      of repeated "effectiveTime" elements) or whether they are encoded as "comp"
      elements in a single "effectiveTime". This flag has no meaning inside a GTS,
      where comp elements are required.
    }
    Property Flat : Boolean read FFlat write FFlat;
    Property originalText : Tv3ED read ForiginalText write SetoriginalText;
    Property ParamType : Tv3QTYDatatype read FParamType write FParamType;
  End;

  Tv3QSU = class (Tv3QSET)
  private
    Fterms: Tv3SetQSET;
    procedure Setterms(const Value: Tv3SetQSET);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Override;
    destructor Destroy; Override;

    function Link : Tv3QSU; Overload;
    Function Clone(parent : Tv3Base) : Tv3QSU; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function AddComp(oComp : Tv3QSET) : Boolean; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property terms : Tv3SetQSET read Fterms write Setterms;
  End;

  Tv3QSI = class (Tv3QSET)
  private
    Fterms: Tv3SetQSET;
    procedure Setterms(const Value: Tv3SetQSET);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Override;
    destructor Destroy; Override;

    function Link : Tv3QSI; Overload;
    Function Clone(parent : Tv3Base) : Tv3QSI; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function AddComp(oComp : Tv3QSET) : Boolean; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property terms : Tv3SetQSET read Fterms write Setterms;
  End;

  Tv3QSD = class (Tv3QSET)
  private
    Fsubtrahend: Tv3QSET;
    Fminuend: Tv3QSET;
    procedure Setminuend(const Value: Tv3QSET);
    procedure Setsubtrahend(const Value: Tv3QSET);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Override;
    destructor Destroy; Override;

    function Link : Tv3QSD; Overload;
    Function Clone(parent : Tv3Base) : Tv3QSD; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function AddComp(oComp : Tv3QSET) : Boolean; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property minuend : Tv3QSET read Fminuend write Setminuend;
    Property subtrahend : Tv3QSET read Fsubtrahend write Setsubtrahend;
  End;

  Tv3QSP = class (Tv3QSET)
  private
    Fhigh: Tv3QSET;
    Flow: Tv3QSET;
    procedure Sethigh(const Value: Tv3QSET);
    procedure Setlow(const Value: Tv3QSET);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Override;
    destructor Destroy; Override;

    function Link : Tv3QSP; Overload;
    Function Clone(parent : Tv3Base) : Tv3QSP; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function AddComp(oComp : Tv3QSET) : Boolean; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property low : Tv3QSET read Flow write Setlow;
    Property high : Tv3QSET read Fhigh write Sethigh;
  End;

  Tv3QSS = class (Tv3QSET)
  private
    Fterms: Tv3SetQTY;
    procedure Setterms(const Value: Tv3SetQTY);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Override;
    destructor Destroy; Override;

    function Link : Tv3QSS; Overload;
    Function Clone(parent : Tv3Base) : Tv3QSS; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property terms : Tv3SetQTY read Fterms write Setterms;
  End;

  Tv3QSC = class (Tv3QSET)
  private
    Fcode: Tv3CD;
    procedure Setcode(const Value: Tv3CD);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Override;
    destructor Destroy; Override;

    function Link : Tv3QSC; Overload;
    Function Clone(parent : Tv3Base) : Tv3QSC; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property code : Tv3CD read Fcode write Setcode;
  End;

  Tv3IVL = class (Tv3QSET)
  private
    FHaslowClosed: boolean;
    FlowClosed: boolean;
    FHashighClosed: boolean;
    FhighClosed: boolean;
    Flow: Tv3QTY;
    Fany: Tv3QTY;
    Fwidth: Tv3QTY;
    Fhigh: Tv3QTY;
    FanyIsValue: Boolean;
    procedure Setany(const Value: Tv3QTY);
    procedure Sethigh(const Value: Tv3QTY);
    procedure Setlow(const Value: Tv3QTY);
    procedure Setwidth(const Value: Tv3QTY);
    procedure SethighClosed(const Value: boolean);
    procedure SetlowClosed(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Override;
    destructor Destroy; Override;

    function Link : Tv3IVL; Overload;
    Function Clone(parent : Tv3Base) : Tv3IVL; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function EqualsV(oOther : Tv3Base) : Boolean; Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property low : Tv3QTY read Flow write Setlow;
    {
      false if lowClosed is null, true if lowClosed is either true or false
    }
    Property HasLowClosed : boolean read FlowClosed write FlowClosed;
    Property lowClosed : boolean read FlowClosed write SetlowClosed;
    Property high : Tv3QTY read Fhigh write Sethigh;
    {
      false if highClosed is null, true if highClosed is either true or false
    }
    Property HasHighClosed : boolean read FhighClosed write FhighClosed;
    Property highClosed : boolean read FhighClosed write SethighClosed;
    Property width : Tv3QTY read Fwidth write Setwidth;
    Property any : Tv3QTY read Fany write Setany;
    {
      This is a consequence of the mapping from CDA data types to ISO 21090.
      If the interval is represented as <x value="time"> then any will be populated,
      and anyIsValue will be set to true. If, however, the interval is encoded as
      <x><center value="time"/></x> (which has the same meaning), then anyIsValue will
      be set to true. The two forms are semantically equivalent; this attribute exists
      to control the xml rendition only.

      Note that center in CDA is mapped to Any in ISO 21090. This is somewhat equivocal,
      but it appears that if matches the general intent of the somewhat odd construct
      of only specifying a center.
    }
    Property anyIsValue : Boolean read FanyIsValue Write FanyIsValue;
  End;

  Tv3PIVL = class (Tv3QSET)
  private
    FisFlexible: boolean;
    Falignment: Tv3CalendarCycle;
    Fcount: Tv3INT;
    Fphase: Tv3IVL;
    Fperiod: Tv3PQ;
    Ffrequency: Tv3RTO;
    FHasisFlexible: boolean;
    procedure Setcount(const Value: Tv3INT);
    procedure Setfrequency(const Value: Tv3RTO);
    procedure Setperiod(const Value: Tv3PQ);
    procedure Setphase(const Value: Tv3IVL);
    procedure SetisFlexible(const Value: boolean);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Override;
    destructor Destroy; Override;

    function Link : Tv3PIVL; Overload;
    Function Clone(parent : Tv3Base) : Tv3PIVL; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;

    {
      false if isFlexible is null, true if isFlexible is either true or false
    }
    Property HasIsFlexible : boolean read FHasisFlexible write FHasisFlexible;
  {$IFNDEF FPC}Published{$ENDIF}
    Property phase : Tv3IVL read Fphase write Setphase;
    Property period : Tv3PQ read Fperiod write Setperiod;
    Property frequency : Tv3RTO read Ffrequency write Setfrequency;
    Property alignment : Tv3CalendarCycle read Falignment write Falignment;
    Property isFlexible : boolean read FisFlexible write SetisFlexible;
    Property count : Tv3INT read Fcount write Setcount;
  End;

  Tv3EIVL = class (Tv3QSET)
  private
    Foffset: Tv3IVL;
    Fevent: Tv3TimingEvent;
    procedure Setoffset(const Value: Tv3IVL);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aParamType : Tv3QTYDatatype); Override;
    destructor Destroy; Override;

    function Link : Tv3EIVL; Overload;
    Function Clone(parent : Tv3Base) : Tv3EIVL; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property event : Tv3TimingEvent read Fevent write Fevent;
    Property offset : Tv3IVL read Foffset write Setoffset;
  End;

  Tv3UVP = class (Tv3ANY)
  private
    Fprobability: TFslDecimal;
    Fvalue: Tv3ANY;
    procedure Setvalue(const Value: Tv3ANY);
    procedure Setprobability(const Value: TFslDecimal);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    function Link : Tv3UVP; Overload;
    Function Clone(parent : Tv3Base) : Tv3UVP; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property probability : TFslDecimal read Fprobability write Setprobability;
    Property value : Tv3ANY read Fvalue write Setvalue;
  End;

  Tv3NPPD = class (Tv3ANY)
  private
    Fitem: Tv3SetUVP;
    procedure Setitem(const Value: Tv3SetUVP);
  Protected
    Procedure ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean); Override;
    Procedure SetPropertyValue(Const aValue : Tv3PropertyDefinition); Override;
    Procedure DoClear; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Link : Tv3NPPD; Overload;
    Function Clone(parent : Tv3Base) : Tv3NPPD; Overload;
    procedure Assign(oSource : TFslObject); Override;
    Function RIMClassNameV: String; Override;

  {$IFNDEF FPC}Published{$ENDIF}
    Property item : Tv3SetUVP read Fitem write Setitem;
  End;

  Tv3SetED = class (Tv3DataTypeCollection)
  private
    Function GetEDs(iIndex : Integer) : Tv3ED;
    procedure SetEDs(iIndex: Integer; const Value: Tv3ED);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    function Link : Tv3SetED; Overload;
    Function Clone(parent : Tv3Base) : Tv3SetED; Overload;

    {
      Add an ED to the Set
    }
    function Add : Tv3ED; Overload;
    {
      Add an existing ED to the set
    }
    Function AddItem(value : Tv3ED) : Boolean;
    {
      Get the iIndexth ED (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3ED;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3ED) : Integer;
    {
      Set the iIndexth ED (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3ED);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth Item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property EDs[iIndex : Integer] : Tv3ED read GetEDs write SetEDs; default;
  End;

  Tv3SetST = class (Tv3DataTypeCollection)
  private
    Function GetSTs(iIndex : Integer) : Tv3ST;
    procedure SetSTs(iIndex: Integer; const Value: Tv3ST);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3SetST; Overload;
    Function Clone(parent : Tv3Base) :Tv3SetST; Overload;

    {
      Add an ST to the Set
    }
    function Add : Tv3ST; Overload;
    {
      Add an existing ED to the set
    }
    Function AddItem(value : Tv3ST) : Boolean;
    {
      Get the iIndexth ST (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3ST;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3ST) : Integer;
    {
      Set the iIndexth ST (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3ST);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property STs[iIndex : Integer] : Tv3ST read GetSTs write SetSTs; default;
  End;

  Tv3SetCD = class (Tv3DataTypeCollection)
  private
    Function GetCDs(iIndex : Integer) : Tv3CD;
    procedure SetCDs(iIndex: Integer; const Value: Tv3CD);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3SetCD; Overload;
    Function Clone(parent : Tv3Base) :Tv3SetCD; Overload;

    {
      Add an CD to the Set
    }
    function Add : Tv3CD; Overload;
    {
      Add an existing ED to the set
    }
    Function AddItem(value : Tv3CD) : Boolean;
    {
      Get the iIndexth CD (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3CD;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3CD) : Integer;
    {
      Set the iIndexth CD (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3CD);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property CDs[iIndex : Integer] : Tv3CD read GetCDs write SetCDs; default;
  End;

  Tv3ListADXP = class (Tv3DataTypeCollection)
  private
    Function GetADXPs(iIndex : Integer) : Tv3ADXP;
    procedure SetADXPs(iIndex: Integer; const Value: Tv3ADXP);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListADXP; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListADXP; Overload;

    {
      Add a ADXP to the end of the list
    }
    function Append : Tv3ADXP;
    {
      Insert a existing ADXP before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3ADXP);
    {
      Insert a ADXP before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3ADXP;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3ADXP);
    {
      Get the iIndexth ADXP (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3ADXP;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3ADXP) : Integer;
    {
      Set the iIndexth ADXP (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3ADXP);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property ADXPs[iIndex : Integer] : Tv3ADXP read GetADXPs write SetADXPs; default;
  End;

  Tv3ListENXP = class (Tv3DataTypeCollection)
  private
    Function GetENXPs(iIndex : Integer) : Tv3ENXP;
    procedure SetENXPs(iIndex: Integer; const Value: Tv3ENXP);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListENXP; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListENXP; Overload;

    {
      Add a ENXP to the end of the list
    }
    function Append : Tv3ENXP;
    {
      Insert a existing ENXP before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3ENXP);
    {
      Insert a ENXP before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3ENXP;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3ENXP);
    {
      Get the iIndexth ENXP (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3ENXP;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3ENXP) : Integer;
    {
      Set the iIndexth ENXP (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3ENXP);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property ENXPs[iIndex : Integer] : Tv3ENXP read GetENXPs write SetENXPs; default;
  End;

  Tv3SetPQR = class (Tv3DataTypeCollection)
  private
    Function GetPQRs(iIndex : Integer) : Tv3PQR;
    procedure SetPQRs(iIndex: Integer; const Value: Tv3PQR);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3SetPQR; Overload;
    Function Clone(parent : Tv3Base) :Tv3SetPQR; Overload;

    {
      Add an PQR to the Set
    }
    function Add : Tv3PQR; Overload;
    {
      Add an existing ED to the set
    }
    Function AddItem(value : Tv3PQR) : Boolean;
    {
      Get the iIndexth PQR (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3PQR;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3PQR) : Integer;
    {
      Set the iIndexth PQR (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3PQR);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property PQRs[iIndex : Integer] : Tv3PQR read GetPQRs write SetPQRs; default;
  End;

  Tv3SetQSET = class (Tv3DataTypeCollection)
  private
    FParamType : Tv3QTYDatatype;
    Function GetQSETs(iIndex : Integer) : Tv3QSET;
  protected
    Function ItemClass : TFslObjectClass; Override;
    procedure SetQSETs(iIndex: Integer; const Value: Tv3QSET);
  public
    constructor Create(aParamType : Tv3QTYDatatype; Parent : Tv3Base);

    Function Link : Tv3SetQSET; Overload;
    Function Clone(parent : Tv3Base) :Tv3SetQSET; Overload;

    {
      Add an QSET to the Set
    }
    function Add : Tv3QSET; Overload;
    {
      Add an existing ED to the set
    }
    Function AddItem(value : Tv3QSET) : Boolean;
    {
      Get the iIndexth QSET (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3QSET;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3QSET) : Integer;
    {
      Set the iIndexth QSET (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3QSET);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property QSETs[iIndex : Integer] : Tv3QSET read GetQSETs write SetQSETs; default;
  End;

  Tv3SetQTY = class (Tv3DataTypeCollection)
  private
    Function GetQTYs(iIndex : Integer) : Tv3QTY;
    procedure SetQTYs(iIndex: Integer; const Value: Tv3QTY);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3SetQTY; Overload;
    Function Clone(parent : Tv3Base) :Tv3SetQTY; Overload;

    {
      Add an QTY to the Set
    }
    function Add : Tv3QTY; Overload;
    {
      Add an existing ED to the set
    }
    Function AddItem(value : Tv3QTY) : Boolean;
    {
      Get the iIndexth QTY (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3QTY;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3QTY) : Integer;
    {
      Set the iIndexth QTY (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3QTY);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property QTYs[iIndex : Integer] : Tv3QTY read GetQTYs write SetQTYs; default;
  End;

  Tv3SetUVP = class (Tv3DataTypeCollection)
  private
    Function GetUVPs(iIndex : Integer) : Tv3UVP;
    procedure SetUVPs(iIndex: Integer; const Value: Tv3UVP);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3SetUVP; Overload;
    Function Clone(parent : Tv3Base) :Tv3SetUVP; Overload;

    {
      Add an UVP to the Set
    }
    function Add : Tv3UVP; Overload;
    {
      Add an existing ED to the set
    }
    Function AddItem(value : Tv3UVP) : Boolean;
    {
      Get the iIndexth UVP (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3UVP;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3UVP) : Integer;
    {
      Set the iIndexth UVP (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3UVP);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property UVPs[iIndex : Integer] : Tv3UVP read GetUVPs write SetUVPs; default;
  End;

  Tv3ListCS = class (Tv3DataTypeCollection)
  private
    Function GetCSs(iIndex : Integer) : Tv3CS;
    procedure SetCSs(iIndex: Integer; const Value: Tv3CS);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListCS; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListCS; Overload;

    {
      Add a CS to the end of the list
    }
    function Append : Tv3CS;
    {
      Insert a existing CS before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3CS);
    {
      Insert a CS before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3CS;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3CS);
    {
      Get the iIndexth CS (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3CS;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3CS) : Integer;
    {
      Set the iIndexth CS (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3CS);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property CSs[iIndex : Integer] : Tv3CS read GetCSs write SetCSs; default;
  End;

  Tv3ListII = class (Tv3DataTypeCollection)
  private
    Function GetIIs(iIndex : Integer) : Tv3II;
    procedure SetIIs(iIndex: Integer; const Value: Tv3II);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListII; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListII; Overload;

    {
      Add a II to the end of the list
    }
    function Append : Tv3II;
    {
      Insert a existing II before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3II);
    {
      Insert a II before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3II;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3II);
    {
      Get the iIndexth II (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3II;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3II) : Integer;
    {
      Set the iIndexth II (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3II);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property IIs[iIndex : Integer] : Tv3II read GetIIs write SetIIs; default;
  End;

  Tv3ListAD = class (Tv3DataTypeCollection)
  private
    Function GetADs(iIndex : Integer) : Tv3AD;
    procedure SetADs(iIndex: Integer; const Value: Tv3AD);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListAD; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListAD; Overload;

    {
      Add a AD to the end of the list
    }
    function Append : Tv3AD;
    {
      Insert a existing AD before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3AD);
    {
      Insert a AD before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3AD;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3AD);
    {
      Get the iIndexth AD (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3AD;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3AD) : Integer;
    {
      Set the iIndexth AD (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3AD);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property ADs[iIndex : Integer] : Tv3AD read GetADs write SetADs; default;
  End;

  Tv3ListTEL = class (Tv3DataTypeCollection)
  private
    Function GetTELs(iIndex : Integer) : Tv3TEL;
    procedure SetTELs(iIndex: Integer; const Value: Tv3TEL);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListTEL; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListTEL Overload;

    {
      Add a TEL to the end of the list
    }
    function Append : Tv3TEL;
    {
      Insert a existing TEL before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3TEL);
    {
      Insert a TEL before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3TEL;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3TEL);
    {
      Get the iIndexth TEL (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3TEL;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3TEL) : Integer;
    {
      Set the iIndexth TEL (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3TEL);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property TELs[iIndex : Integer] : Tv3TEL read GetTELs write SetTELs; default;
  End;

  Tv3ListANY = class (Tv3DataTypeCollection)
  private
    Function GetANYs(iIndex : Integer) : Tv3ANY;
    procedure SetANYs(iIndex: Integer; const Value: Tv3ANY);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListANY; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListANY Overload;

    {
      Add a ANY to the end of the list
    }
    function Append : Tv3ANY;
    {
      Insert a existing ANY before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3ANY);
    {
      Insert a ANY before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3ANY;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3ANY);
    {
      Get the iIndexth ANY (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3ANY;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3ANY) : Integer;
    {
      Set the iIndexth ANY (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3ANY);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property ANYs[iIndex : Integer] : Tv3ANY read GetANYs write SetANYs; default;
  End;

  Tv3ListCD = class (Tv3DataTypeCollection)
  private
    Function GetCDs(iIndex : Integer) : Tv3CD;
    procedure SetCDs(iIndex: Integer; const Value: Tv3CD);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListCD; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListCD; Overload;

    {
      Add a CD to the end of the list
    }
    function Append : Tv3CD;
    {
      Insert a existing CD before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3CD);
    {
      Insert a CD before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3CD;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3CD);
    {
      Get the iIndexth CD (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3CD;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3CD) : Integer;
    {
      Set the iIndexth CD (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3CD);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property CDs[iIndex : Integer] : Tv3CD read GetCDs write SetCDs; default;
  End;

  Tv3ListEN = class (Tv3DataTypeCollection)
  private
    Function GetENs(iIndex : Integer) : Tv3EN;
    procedure SetENs(iIndex: Integer; const Value: Tv3EN);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListEN; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListEN; Overload;

    {
      Add a EN to the end of the list
    }
    function Append : Tv3EN;
    {
      Insert a existing EN before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3EN);
    {
      Insert a EN before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3EN;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3EN);
    {
      Get the iIndexth EN (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3EN;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3EN) : Integer;
    {
      Set the iIndexth EN (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3EN);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property ENs[iIndex : Integer] : Tv3EN read GetENs write SetENs; default;
  End;

  Tv3ListCR = class (Tv3DataTypeCollection)
  private
    Function GetCRs(iIndex : Integer) : Tv3CR;
    procedure SetCRs(iIndex: Integer; const Value: Tv3CR);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListCR; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListCR; Overload;

    {
      Add a CR to the end of the list
    }
    function Append : Tv3CR;
    {
      Insert a existing CR before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3CR);
    {
      Insert a CR before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3CR;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3CR);
    {
      Get the iIndexth CR (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3CR;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3CR) : Integer;
    {
      Set the iIndexth CR (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3CR);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property CRs[iIndex : Integer] : Tv3CR read GetCRs write SetCRs; default;
  End;

  Tv3ListPQ = class (Tv3DataTypeCollection)
  private
    Function GetPQs(iIndex : Integer) : Tv3PQ;
    procedure SetPQs(iIndex: Integer; const Value: Tv3PQ);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public

    Function Link : Tv3ListPQ; Overload;
    Function Clone(parent : Tv3Base) :Tv3ListPQ; Overload;

    {
      Add a PQ to the end of the list
    }
    function Append : Tv3PQ;
    {
      Insert a existing PQ before the specified iten (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : Tv3PQ);
    {
      Insert a PQ before the specified iten (0 = first item)
    }
    Function Insert(iIndex : Integer) : Tv3PQ;
    {
      Add an existing ED to the list
    }
    Procedure AddItem(value : Tv3PQ);
    {
      Get the iIndexth PQ (0 = first item)
    }
    Function Item(iIndex : Integer) : Tv3PQ;
    {
      Get the index of the object
    }
    Function IndexOf(value : Tv3PQ) : Integer;
    {
      Set the iIndexth PQ (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : Tv3PQ);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item (0 = first item)
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property PQs[iIndex : Integer] : Tv3PQ read GetPQs write SetPQs; default;
  End;

implementation


Function SetAsInteger(aSet : Tv3SetTelecommunicationAddressUse) : Integer;
var
  aLoop : Tv3TelecommunicationAddressUse;
Begin
  result := 0;
  for aLoop := Low(Tv3TelecommunicationAddressUse) to High(Tv3TelecommunicationAddressUse) Do
  Begin
    Assert(ord(aLoop) < 32);
    if aLoop in aSet Then
      result := result + 1 shl (Ord(aLoop));
  End;
End;


Function SetAsInteger(aSet : Tv3SetTelecommunicationCapability) : Integer;
var
  aLoop : Tv3TelecommunicationCapability;
Begin
  result := 0;
  for aLoop := Low(Tv3TelecommunicationCapability) to High(Tv3TelecommunicationCapability) Do
  Begin
    Assert(ord(aLoop) < 32);
    if aLoop in aSet Then
      result := result + 1 shl (Ord(aLoop));
  End;
End;

Function SetAsInteger(aSet : Tv3SetPostalAddressUse) : Integer;
var
  aLoop : Tv3PostalAddressUse;
Begin
  result := 0;
  for aLoop := Low(Tv3PostalAddressUse) to High(Tv3PostalAddressUse) Do
  Begin
    Assert(ord(aLoop) < 32);
    if aLoop in aSet Then
      result := result + 1 shl (Ord(aLoop));
  End;
End;

Function SetAsInteger(aSet : Tv3SetEntityNamePartQualifier) : Integer;
var
  aLoop : Tv3EntityNamePartQualifier;
Begin
  result := 0;
  for aLoop := Low(Tv3EntityNamePartQualifier) to High(Tv3EntityNamePartQualifier) Do
  Begin
    Assert(ord(aLoop) < 32);
    if aLoop in aSet Then
      result := result + 1 shl (Ord(aLoop));
  End;
End;

Function SetAsInteger(aSet : Tv3SetEntityNameUse) : Integer;
var
  aLoop : Tv3EntityNameUse;
Begin
  result := 0;
  for aLoop := Low(Tv3EntityNameUse) to High(Tv3EntityNameUse) Do
  Begin
    Assert(ord(aLoop) < 32);
    if aLoop in aSet Then
      result := result + 1 shl (Ord(aLoop));
  End;
End;


Function IntegerAsSetTelecommunicationAddressUse(i : Integer) : Tv3SetTelecommunicationAddressUse;
var
  aLoop : Tv3TelecommunicationAddressUse;
Begin
  result := [];
  for aLoop := Low(Tv3TelecommunicationAddressUse) to High(Tv3TelecommunicationAddressUse) Do
  Begin
    Assert(ord(aLoop) < 32);
    if i and (1 shl (Ord(aLoop))) > 0 Then
      result := result + [aLoop];
  End;
End;


Function IntegerAsSetTelecommunicationCapability(i : Integer) : Tv3SetTelecommunicationCapability;
var
  aLoop : Tv3TelecommunicationCapability;
Begin
  result := [];
  for aLoop := Low(Tv3TelecommunicationCapability) to High(Tv3TelecommunicationCapability) Do
  Begin
    Assert(ord(aLoop) < 32);
    if i and (1 shl (Ord(aLoop))) > 0 Then
      result := result + [aLoop];
  End;
End;

Function IntegerAsSetPostalAddressUse(i : Integer) : Tv3SetPostalAddressUse;
var
  aLoop : Tv3PostalAddressUse;
Begin
  result := [];
  for aLoop := Low(Tv3PostalAddressUse) to High(Tv3PostalAddressUse) Do
  Begin
    Assert(ord(aLoop) < 32);
    if i and (1 shl (Ord(aLoop))) > 0 Then
      result := result + [aLoop];
  End;
End;

Function IntegerAsSetEntityNamePartQualifier(i : Integer) : Tv3SetEntityNamePartQualifier;
var
  aLoop : Tv3EntityNamePartQualifier;
Begin
  result := [];
  for aLoop := Low(Tv3EntityNamePartQualifier) to High(Tv3EntityNamePartQualifier) Do
  Begin
    Assert(ord(aLoop) < 32);
    if i and (1 shl (Ord(aLoop))) > 0 Then
      result := result + [aLoop];
  End;
End;

Function IntegerAsSetEntityNameUse(i : Integer) : Tv3SetEntityNameUse;
var
  aLoop : Tv3EntityNameUse;
Begin
  result := [];
  for aLoop := Low(Tv3EntityNameUse) to High(Tv3EntityNameUse) Do
  Begin
    Assert(ord(aLoop) < 32);
    if i and (1 shl (Ord(aLoop))) > 0 Then
      result := result + [aLoop];
  End;
End;


{ Tv3HXIT }

procedure Tv3HXIT.Assign(oSource: TFslObject);
begin
  inherited;
  validTimeHigh := Tv3HXIT(oSource).FvalidTimeHigh;
  validTimeLow := Tv3HXIT(oSource).FvalidTimeLow;
  controlInformationExtension := Tv3HXIT(oSource).FcontrolInformationExtension;
  controlInformationRoot := Tv3HXIT(oSource).FcontrolInformationRoot;
end;

function Tv3HXIT.Clone(parent : Tv3Base): Tv3HXIT;
begin
  Result := Tv3HXIT(inherited Clone(parent));
end;

Procedure Tv3HXIT.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'validTimeHigh', FvalidTimeHigh));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'validTimeLow', FvalidTimeLow));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'controlInformationExtension', FcontrolInformationExtension));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'controlInformationRoot', FcontrolInformationRoot));
end;

function Tv3HXIT.Link: Tv3HXIT;
begin
  Result := Tv3HXIT(Inherited Link);
end;

procedure Tv3HXIT.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'validTimeHigh' Then
    FvalidTimeHigh := aValue.AsString
  else if aValue.Name = 'validTimeLow' Then
    FvalidTimeLow := aValue.AsString
  else if aValue.Name = 'controlInformationExtension' Then
    FcontrolInformationExtension := aValue.AsString
  else if aValue.Name = 'controlInformationRoot' Then
    FcontrolInformationRoot := aValue.AsString
  else
    Inherited SetPropertyValue(aValue);
end;

procedure Tv3HXIT.DoClear;
begin
  inherited;
  FvalidTimeHigh := '';
  FvalidTimeLow := '';
  FcontrolInformationExtension := '';
  FcontrolInformationRoot := '';
end;

function Tv3HXIT.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FvalidTimeHigh.length * sizeof(char)) + 12);
  inc(result, (FvalidTimeLow.length * sizeof(char)) + 12);
  inc(result, (FcontrolInformationExtension.length * sizeof(char)) + 12);
  inc(result, (FcontrolInformationRoot.length * sizeof(char)) + 12);
end;

{ Tv3ANY }

Function Tv3ANY.RIMClassNameV : String;
Begin
  Result := 'ANY';
End;

procedure Tv3ANY.Assign(oSource: TFslObject);
begin
  inherited;
  nullFlavor := Tv3ANY(oSource).FnullFlavor;
  flavorId := Tv3ANY(oSource).FflavorId;
  updateMode := Tv3ANY(oSource).FupdateMode;
end;

function Tv3ANY.Clone(parent : Tv3Base): Tv3ANY;
begin
  Result := Tv3ANY(inherited Clone(parent));
end;


destructor Tv3ANY.Destroy;
begin
  SetLength(FflavorId, 0);
  inherited;
end;

Procedure Tv3ANY.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'nullFlavor', Ord(FnullFlavor), CODES_Tv3NullFlavor));
  oList.Add(Tv3PropertyDefinition.CreateString(self, true, 'flavorId', FflavorId));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, true, 'updateMode', Ord(FupdateMode), CODES_Tv3UpdateMode));
end;

function Tv3ANY.Link: Tv3ANY;
begin
  Result := Tv3ANY(Inherited Link);
end;


procedure Tv3ANY.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'nullFlavor' Then
    FnullFlavor := Tv3NullFlavor(aValue.AsEnum)
  else if aValue.Name = 'flavorId' Then
    FflavorId := aValue.AsString
  else if aValue.Name = 'updateMode' Then
    FupdateMode := Tv3UpdateMode(aValue.AsEnum)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3ANY.Literal: String;
Begin
  result := LiteralV;
End;

function Tv3ANY.LiteralV: String;
begin
  Result := 'There is no literal form defined for the type '+RIMClassNameV;
end;

function Tv3ANY.IsNonNull: Boolean;
begin
  result := nullFlavor = nfNull;
end;

function Tv3ANY.isNull: Boolean;
begin
  result := nullFlavor <> nfNull;
end;

function Tv3ANY.nullFlavorLiteral: String;
begin
  result := 'NullFlavor.'+CODES_Tv3NullFlavor[FnullFlavor];
end;

procedure Tv3ANY.DoClear;
begin
  inherited;
  FnullFlavor := nfNull;
  FflavorId := '';
  FupdateMode := umNull;
end;

function Tv3ANY.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FflavorId.length * sizeof(char)) + 12);
end;

{ Tv3BL }

Function Tv3BL.RIMClassNameV : String;
Begin
  Result := 'BL';
End;

procedure Tv3BL.Assign(oSource: TFslObject);
begin
  inherited;
  Fvalue := Tv3BL(oSource).Fvalue;
  FHasvalue := Tv3BL(oSource).FHasvalue;
end;

function Tv3BL.Clone(parent : Tv3Base): Tv3BL;
begin
  Result := Tv3BL(inherited Clone(parent));
end;

constructor Tv3BL.Create;
begin
  inherited;
  FHasValue := False;
end;

Procedure Tv3BL.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'value', FHasValue, Fvalue));
end;

function Tv3BL.Link: Tv3BL;
begin
  Result := Tv3BL(Inherited Link);
end;

procedure Tv3BL.Setvalue(const Value: boolean);
begin
  FHasValue := true;
  Fvalue := Value;
end;

procedure Tv3BL.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'value' Then
    aValue.AsBool(FHasValue, Fvalue)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3BL.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3BL) and IsNonNull and (Tv3BL(oOther).IsNonNull) and (Tv3BL(oOther).value = value);
end;

function Tv3BL.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := BooleanToString(FValue);
end;

procedure Tv3BL.DoClear;
begin
  inherited;
  FHasValue := false;
end;

function Tv3BL.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ Tv3ED }

Function Tv3ED.RIMClassNameV : String;
Begin
  Result := 'ED';
End;

procedure Tv3ED.Assign(oSource: TFslObject);
begin
  inherited;
  mediaType := Tv3ED(oSource).FmediaType;
  charset := Tv3ED(oSource).Fcharset;
  language := Tv3ED(oSource).Flanguage;
  data := Tv3ED(oSource).Fdata.Clone;
  integrityCheck := Tv3ED(oSource).FintegrityCheck.Clone;
  xml := Tv3ED(oSource).Fxml;
  compression := Tv3ED(oSource).Fcompression;
  thumbnail := Tv3ED(oSource).Fthumbnail.Clone(self);
  translation := Tv3ED(oSource).Ftranslation.Clone(self);
  integrityCheckAlgorithm := Tv3ED(oSource).FintegrityCheckAlgorithm;
  description := Tv3ED(oSource).Fdescription.Clone(self);
  reference := Tv3ED(oSource).Freference.Clone(self);
  value := Tv3ED(oSource).Fvalue;
end;

function Tv3ED.Clone(parent : Tv3Base): Tv3ED;
begin
  Result := Tv3ED(inherited Clone(parent));
end;


destructor Tv3ED.Destroy;
begin
  Fdata.Free;
  FintegrityCheck.Free;
  Fxml := nil;
  Fthumbnail.Free;
  Ftranslation.Free;
  Fdescription.Free;
  Freference.Free;
  inherited;
end;

Procedure Tv3ED.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'mediaType', FmediaType));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'charset', Fcharset));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'language', Flanguage));
  oList.Add(Tv3PropertyDefinition.CreateBinary(self, false, 'data', Fdata));
  oList.Add(Tv3PropertyDefinition.CreateBinary(self, false, 'integrityCheck', FintegrityCheck));
//  else if aValue.Name = 'xml' Then
//    result. := Fxml; // ..TFslObject
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'compression', Ord(Fcompression), Codes_Tv3Compression));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'thumbnail', Fthumbnail, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'translation', Ftranslation, rmpctSet, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'integrityCheckAlgorithm', Ord(FintegrityCheckAlgorithm), CODES_Tv3IntegrityCheckAlgorithm));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'description', Fdescription, 'ST'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'reference', Freference, 'TEL'));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'value', Fvalue));
end;

function Tv3ED.Link: Tv3ED;
begin
  Result := Tv3ED(Inherited Link);
end;

procedure Tv3ED.Setdata(const Value: TFslBuffer);
begin
  Fdata.Free;
  Fdata := Value;
end;

procedure Tv3ED.Setdescription(const Value: Tv3ST);
begin
  Fdescription.Free;
  Fdescription := Value;
  Fdescription.Parent := self;
end;

procedure Tv3ED.SetintegrityCheck(const Value: TFslBuffer);
begin
  FintegrityCheck.Free;
  FintegrityCheck := Value;
end;

procedure Tv3ED.Setreference(const Value: Tv3TEL);
begin
  Freference.Free;
  Freference := Value;
  Freference.Parent := self;
end;

procedure Tv3ED.Setthumbnail(const Value: Tv3ED);
begin
  Fthumbnail.Free;
  Fthumbnail := Value;
  Fthumbnail.Parent := self;
end;

procedure Tv3ED.Settranslation(const Value: Tv3SetED);
begin
  Ftranslation.Free;
  Ftranslation := Value;
  Ftranslation.Parent := self;
end;

procedure Tv3ED.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
var
  s : String;
begin
  if aValue.Name = 'mediaType' Then
    FmediaType := aValue.AsString
  else if aValue.Name = 'charset' Then
    Fcharset := aValue.AsString
  else if aValue.Name = 'language' Then
    Flanguage := aValue.AsString
  else if aValue.Name = 'data' Then
  Begin
    s := aValue.AsString;
    if s = '' Then
      data := nil
    Else
    Begin
      if Fdata = nil Then
        Fdata := TFslBuffer.Create;
      Fdata.AsText := s;
    End
  End
  else if aValue.Name = 'integrityCheck' Then
  Begin
    s := aValue.AsString;
    if s = '' Then
      integrityCheck := nil
    Else
    Begin
      if FintegrityCheck = nil Then
        FintegrityCheck := TFslBuffer.Create;
      FintegrityCheck.AsText := s;
    End
  End
//  else if aValue.Name = 'xml' Then
//    Fxml := TFslObject(aValue.)
  else if aValue.Name = 'compression' Then
    Fcompression := Tv3Compression(aValue.AsEnum)
  else if aValue.Name = 'thumbnail' Then
    thumbnail := Tv3ED(aValue.AsType(Tv3ED)).Link
  else if aValue.Name = 'translation' Then
    translation := Tv3SetED(aValue.AsType(Tv3SetED)).Clone(self)
  else if aValue.Name = 'integrityCheckAlgorithm' Then
    FintegrityCheckAlgorithm := Tv3IntegrityCheckAlgorithm(aValue.AsEnum)
  else if aValue.Name = 'description' Then
    description := Tv3ST(aValue.AsType(Tv3ST)).Link
  else if aValue.Name = 'reference' Then
    reference := Tv3TEL(aValue.AsType(Tv3TEL)).Link
  else if aValue.Name = 'value' Then
    Fvalue := aValue.AsString
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3ED.AddTranslation: Tv3ED;
begin
  if translation = nil Then
    translation := Tv3SetED.create(self);
  Result := translation.Add;
end;

function Tv3ED.GetDataAsString: String;
begin
  if (data = nil) then
    Result := ''
  Else
    Result := data.AsText;
end;

procedure Tv3ED.LoadDataFromFile(const sFilename: String);
begin
  SetDataAsString(FileToString(sFilename, TEncoding.UTF8));
end;

procedure Tv3ED.LoadDataFromStream(oStream: TStream);
begin
  SetDataAsString(StreamToString(oStream, TEncoding.UTF8));
end;

procedure Tv3ED.SaveDataToFile(const sFilename: String);
var
  s : String;
begin
  if data <> nil Then
    s := data.AsText
  else if value <> '' Then
    s := value;
  // todo else if xml....
  StringtoFile(s, sFilename, TEncoding.UTF8);
end;

procedure Tv3ED.SaveDataToStream(oStream: TStream);
var
  s : String;
begin
  if data <> nil Then
    s := data.AsText
  else if value <> '' Then
    s := value;
  // todo else if xml....
  StringtoStream(s, oStream, TEncoding.UTF8);
end;

procedure Tv3ED.SetDataAsString(const Value: String);
begin
  xml := nil;
  self.value := '';
  data := TFslBuffer.Create;
  data.AsText := Value;
end;

Function SameData(oBuffer1, oBuffer2 : TFslBuffer) : Boolean;
Begin
  result := ((oBuffer1 = nil) and (oBuffer2 = nil)) Or
    ((oBuffer1 <> nil) and (oBuffer2 <> nil) And (oBuffer1.AsText = oBuffer2.AsText));
End;

Function SameXML(oXml1, oXml2 : TMXmlElement) : Boolean;
Begin
  // todo: compare byte sequences of rendered XML
  result := oXml1 = oXml2;
End;

function Tv3ED.EqualsV(oOther: Tv3Base): Boolean;
var
  b1, b2 : TBytes;
begin
  SetLength(b1, 0);
  SetLength(b2, 0);
  if (oOther is Tv3ED) and isNonNull and Tv3ED(oOther).isNonNull Then
  Begin
    b1 := Getbytes;
    b2 := Tv3ED(oOther).Getbytes;
    result := SameBytes(b1, b2);
  End
  Else
    result := False;
end;

constructor Tv3ED.Create;
begin
  inherited;
  Ftranslation := Tv3SetED.Create(self);
end;

function Tv3ED.Getbytes: TBytes;
begin
  if isNull then
    SetLength(result, 0)
  else if value <> '' Then
    {$IFDEF VER130}
    result := AnsiStringAsBytes(value)
    {$ELSE}
    result := TEncoding.UTF8.GetBytes(value)
    {$ENDIF}
  else if (data <> nil) Then
    result := data.AsBytes
  else if (xml <> nil) Then
    {$IFDEF VER130}
    result := AnsiStringAsBytes(xml.text)
    {$ELSE}
    result := TEncoding.UTF8.GetBytes(xml.text)
    {$ENDIF}
  else if reference <> nil Then
    raise ECDAException.create('Reference resolution is not yet handled');
end;

procedure Tv3ED.DoClear;
begin
  inherited;
  FmediaType := 'text/plain';
  Fcharset := '';
  Flanguage := '';
  data := nil;
  integrityCheck := nil;
  Fxml := nil;
  Fcompression := cNull;
  thumbnail := nil;
  translation.ClearItems;
  FintegrityCheckAlgorithm := icaNull;
  description := nil;
  reference := Nil;
  value := '';
end;

function Tv3ED.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FmediaType.length * sizeof(char)) + 12);
  inc(result, (Fcharset.length * sizeof(char)) + 12);
  inc(result, (Flanguage.length * sizeof(char)) + 12);
  inc(result, Fdata.sizeInBytes);
  inc(result, FintegrityCheck.sizeInBytes);
  inc(result, Fxml.sizeInBytes);
  inc(result, Fthumbnail.sizeInBytes);
  inc(result, Ftranslation.sizeInBytes);
  inc(result, Fdescription.sizeInBytes);
  inc(result, Freference.sizeInBytes);
  inc(result, (Fvalue.length * sizeof(char)) + 12);
  inc(result, (FRepresentation.length * sizeof(char)) + 12);
end;

{ Tv3ST }

Function Tv3ST.RIMClassNameV : String;
Begin
  Result := 'ST';
End;

procedure Tv3ST.Assign(oSource: TFslObject);
begin
  inherited;
  language := Tv3ST(oSource).Flanguage;
  translation := Tv3ST(oSource).Ftranslation.Clone(self);
  value := Tv3ST(oSource).Fvalue;
end;

function Tv3ST.Clone(parent : Tv3Base): Tv3ST;
begin
  Result := Tv3ST(inherited Clone(parent));
end;


destructor Tv3ST.Destroy;
begin
  Ftranslation.Free;
  inherited;
end;

Procedure Tv3ST.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'language', Flanguage));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'translation', Ftranslation, rmpctSet, 'ST'));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'value', Fvalue));
end;

function Tv3ST.Link: Tv3ST;
begin
  Result := Tv3ST(Inherited Link);
end;

procedure Tv3ST.Settranslation(const Value: Tv3SetST);
begin
  Ftranslation.Free;
  Ftranslation := Value;
  Ftranslation.Parent := self;
end;

procedure Tv3ST.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'language' Then
    Flanguage := aValue.AsString
  else if aValue.Name = 'translation' Then
    translation := Tv3SetST(aValue.AsType(Tv3SetST)).Clone(self)
  else if aValue.Name = 'value' Then
    Fvalue := aValue.AsString
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3ST.AddTranslation: Tv3ST;
begin
  if translation = nil then
    translation := Tv3SetST.Create(self);
  result := translation.Add;
end;

function Tv3ST.EqualsV(oOther: Tv3Base): Boolean;
begin
  // TODO: access data by reference
  result :=
    (
      (oOther is Tv3ED) and
      (
        IsNonNull and
        Tv3ED(oOther).IsNonNull and
        (Tv3ED(oOther).charset = 'text/plain') and
        (Tv3ED(oOther).value = value) or
        (
          (Tv3ED(oOther).data <> nil) And
          (Tv3ED(oOther).data.AsText = value)
        )
      )
    )
    Or
    (
      (oOther is Tv3ST) and
      (
        IsNonNull and
        Tv3ST(oOther).IsNonNull and
        (Tv3ST(oOther).value = value)
      )
    );

end;

constructor Tv3ST.Create;
begin
  inherited;
  Ftranslation := Tv3SetST.Create(self);
end;

function Tv3ST.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := Fvalue;
end;

procedure Tv3ST.DoClear;
begin
  inherited;
  Flanguage := '';
  Ftranslation.ClearItems;
  Fvalue := '';
end;

function Tv3ST.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Flanguage.length * sizeof(char)) + 12);
  inc(result, Ftranslation.sizeInBytes);
  inc(result, (Fvalue.length * sizeof(char)) + 12);
end;

{ Tv3SC }

Function Tv3SC.RIMClassNameV : String;
Begin
  Result := 'SC';
End;

procedure Tv3SC.Assign(oSource: TFslObject);
begin
  inherited;
  code := Tv3SC(oSource).Fcode.Clone(self);
end;

function Tv3SC.Clone(parent : Tv3Base): Tv3SC;
begin
  Result := Tv3SC(inherited Clone(parent));
end;


destructor Tv3SC.Destroy;
begin
  Fcode.Free;
  inherited;
end;

Procedure Tv3SC.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
end;

function Tv3SC.Link: Tv3SC;
begin
  Result := Tv3SC(Inherited Link);
end;

procedure Tv3SC.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if FCode <> nil then
    Fcode.Parent := self;
end;

procedure Tv3SC.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'code' Then
    code := Tv3CD(aValue.AsType(Tv3CD)).Link
  else
    Inherited SetPropertyValue(aValue);
end;

procedure Tv3SC.DoClear;
begin
  inherited;
  Code := nil;
end;

function Tv3SC.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcode.sizeInBytes);
end;

{ Tv3CD }

Function Tv3CD.RIMClassNameV : String;
Begin
  Result := 'CD';
End;

procedure Tv3CD.Assign(oSource: TFslObject);
begin
  inherited;
  originalText := Tv3CD(oSource).ForiginalText.Clone(self);
  displayName := Tv3CD(oSource).FdisplayName.Clone(self);
  valueSetVersion := Tv3CD(oSource).FvalueSetVersion;
  valueSet := Tv3CD(oSource).FvalueSet;
  codeSystem := Tv3CD(oSource).FcodeSystem;
  codeSystemName := Tv3CD(oSource).FcodeSystemName;
  code := Tv3CD(oSource).Fcode;
  codeSystemVersion := Tv3CD(oSource).FcodeSystemVersion;
  codingRationale := Tv3CD(oSource).FcodingRationale;
  translation := Tv3CD(oSource).Ftranslation.Clone(self);
  qualifier := Tv3CD(oSource).Fqualifier.Clone(self);
end;

function Tv3CD.Clone(parent : Tv3Base): Tv3CD;
begin
  Result := Tv3CD(inherited Clone(parent));
end;


destructor Tv3CD.Destroy;
begin
  ForiginalText.Free;
  FdisplayName.Free;
  Ftranslation.Free;
  Fqualifier.Free;
  inherited;
end;

Procedure Tv3CD.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'code', Fcode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'codeSystem', FcodeSystem));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'codeSystemName', FcodeSystemName));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'codeSystemVersion', FcodeSystemVersion));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'valueSetVersion', FvalueSetVersion));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'valueSet', FvalueSet));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'codingRationale', Ord(FcodingRationale), CODES_Tv3CodingRationale));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'originalText', ForiginalText, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'displayName', FdisplayName, 'ST'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'translation', Ftranslation, rmpctSet, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'qualifier', Fqualifier, rmpctList, 'CR'));
end;

function Tv3CD.Link: Tv3CD;
begin
  Result := Tv3CD(Inherited Link);
end;

procedure Tv3CD.SetdisplayName(const Value: Tv3ST);
begin
  FdisplayName.Free;
  FdisplayName := Value;
  if FdisplayName <> nil then
    displayName.Parent := self;
end;

procedure Tv3CD.SetoriginalText(const Value: Tv3ED);
begin
  ForiginalText.Free;
  ForiginalText := Value;
  if ForiginalText <> nil then  originalText.parent := self;

end;

procedure Tv3CD.Setqualifier(const Value: Tv3ListCR);
begin
  Fqualifier.Free;
  Fqualifier := Value;
  if Fqualifier <> nil then  qualifier.parent := self;

end;

procedure Tv3CD.Settranslation(const Value: Tv3SetCD);
begin
  Ftranslation.Free;
  Ftranslation := Value;
  if Ftranslation <> nil then  translation.parent := self;

end;

procedure Tv3CD.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'originalText' Then
    originalText := Tv3ED(aValue.AsType(Tv3ED)).Link
  else if aValue.Name = 'displayName' Then
    displayName := Tv3ST(aValue.AsType(Tv3ST)).Link
  else if aValue.Name = 'valueSetVersion' Then
    FvalueSetVersion := aValue.AsString
  else if aValue.Name = 'valueSet' Then
    FvalueSet := aValue.AsString
  else if aValue.Name = 'codeSystem' Then
    FcodeSystem := aValue.AsString
  else if aValue.Name = 'codeSystemName' Then
    FcodeSystemName := aValue.AsString
  else if aValue.Name = 'code' Then
    Fcode := aValue.AsString
  else if aValue.Name = 'codeSystemVersion' Then
    FcodeSystemVersion := aValue.AsString
  else if aValue.Name = 'codingRationale' Then
    FcodingRationale := Tv3CodingRationale(aValue.AsEnum)
  else if aValue.Name = 'translation' Then
    translation := Tv3SetCD(aValue.AsType(Tv3SetCD)).Clone(self)
  else if aValue.Name = 'qualifier' Then
    qualifier := Tv3ListCR(aValue.AsType(Tv3ListCR)).Clone(self)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3CD.AddTranslation(code, codeSystem: String): Tv3CD;
begin
  if translation = nil then
    translation := Tv3SetCD.create(self);
  result := translation.Add;
  result.code := code;
  result.codeSystem := codeSystem;
end;

function Tv3CD.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3CD) and not (oOther is Tv3PQR) and IsNonNull and (Tv3CD(oOther).IsNonNull) and (Tv3CD(oOther).code = code) and (Tv3CD(oOther).codeSystem = codeSystem);
end;

constructor Tv3CD.Create;
begin
  inherited;
  Ftranslation := Tv3SetCD.Create(self);
  Fqualifier := Tv3ListCR.Create(self);
end;

function Tv3CD.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := Fcode+'::'+FcodeSystem;
end;

function Tv3CD.render: String;
begin
  if self = nil then
    result := ''
  else if assigned(originalText) and (originalText.value <> '') then
    result := originalText.value
  else if assigned(displayName) and (displayName.value <> '') then
    result := displayName.value
  else if (code <> '')  then
    result := code
  else
    result := '';
end;

procedure Tv3CD.DoClear;
begin
  inherited;
  originalText := nil;
  displayName := nil;
  valueSetVersion := '';
  valueSet := '';
  codeSystem := '';
  codeSystemName := '';
  code := '';
  codeSystemVersion := '';
  codingRationale := crNull;
  translation.ClearItems;
  qualifier.ClearItems;
end;

function Tv3CD.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, ForiginalText.sizeInBytes);
  inc(result, FdisplayName.sizeInBytes);
  inc(result, (FvalueSetVersion.length * sizeof(char)) + 12);
  inc(result, (FvalueSet.length * sizeof(char)) + 12);
  inc(result, (FcodeSystem.length * sizeof(char)) + 12);
  inc(result, (FcodeSystemName.length * sizeof(char)) + 12);
  inc(result, (Fcode.length * sizeof(char)) + 12);
  inc(result, (FcodeSystemVersion.length * sizeof(char)) + 12);
  inc(result, Ftranslation.sizeInBytes);
  inc(result, Fqualifier.sizeInBytes);
end;

{ Tv3CS }

Function Tv3CS.RIMClassNameV : String;
Begin
  Result := 'CS';
End;

procedure Tv3CS.Assign(oSource: TFslObject);
begin
  inherited;
  code := Tv3CS(oSource).Fcode;
end;

function Tv3CS.Clone(parent : Tv3Base): Tv3CS;
begin
  Result := Tv3CS(inherited Clone(parent));
end;


Procedure Tv3CS.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'code', Fcode));
end;

function Tv3CS.Link: Tv3CS;
begin
  Result := Tv3CS(Inherited Link);
end;

procedure Tv3CS.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'code' Then
    Fcode := aValue.AsString
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3CS.EqualsV(oOther: Tv3Base): Boolean;
begin
  // todo: we're supposed to check the code system too.
  result := (oOther is Tv3CS) and IsNonNull and (Tv3CS(oOther).IsNonNull) and (Tv3CS(oOther).code = code);
end;

function Tv3CS.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := Fcode;
end;

procedure Tv3CS.DoClear;
begin
  inherited;
  code := '';
end;

function Tv3CS.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fcode.length * sizeof(char)) + 12);
end;

{ Tv3TEL }

Function Tv3TEL.RIMClassNameV : String;
Begin
  Result := 'TEL';
End;

procedure Tv3TEL.Assign(oSource: TFslObject);
begin
  inherited;
  value := Tv3TEL(oSource).Fvalue;
  useablePeriod := Tv3TEL(oSource).FuseablePeriod.Clone(self);
  use := Tv3TEL(oSource).Fuse;
  capabilities := Tv3TEL(oSource).Fcapabilities;
end;

function Tv3TEL.Clone(parent : Tv3Base): Tv3TEL;
begin
  Result := Tv3TEL(inherited Clone(parent));
end;


destructor Tv3TEL.Destroy;
begin
  FuseablePeriod.Free;
  inherited;
end;

Procedure Tv3TEL.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'value', Fvalue));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'useablePeriod', FuseablePeriod, 'QSET<TS>'));
  oList.Add(Tv3PropertyDefinition.CreateSet(self, false, 'use', Fuse, CODES_Tv3TelecommunicationAddressUse));
  oList.Add(Tv3PropertyDefinition.CreateSet(self, false, 'capabilities', FCapabilities, CODES_Tv3TelecommunicationCapability));
end;

function Tv3TEL.Link: Tv3TEL;
begin
  Result := Tv3TEL(Inherited Link);
end;

procedure Tv3TEL.SetuseablePeriod(const Value: Tv3QSET);
begin
  FuseablePeriod.Free;
  FuseablePeriod := Value;
  if FuseablePeriod <> nil then  useablePeriod.parent := self;

end;

procedure Tv3TEL.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'value' Then
    Fvalue := aValue.AsString
  else if aValue.Name = 'useablePeriod' Then
    FuseablePeriod := Tv3QSET(aValue.asType(Tv3QSET)).Link
  else if aValue.Name = 'use' Then
    aValue.AsSet(Fuse)
  else if aValue.Name = 'capabilities' Then
    aValue.AsSet(Fcapabilities)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3TEL.canonical: String;
begin
  result := Fvalue;
  if StringStartsWithInsensitive(result, 'tel:') Then
    result := StringReplace(result, ['(', ' ', ')'], '')
  Else if StringStartsWithInsensitive(result, 'mailto') And (pos('?', result) > 0) Then
    result := copy(result, 1, pos('?', result)-1);
end;

function Tv3TEL.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3TEL) and IsNonNull and (Tv3TEL(oOther).IsNonNull) and (Tv3TEL(oOther).canonical = canonical);
end;

function Tv3TEL.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := Fvalue;
end;

function Tv3TEL.render: String;
var
  l, r : String;
begin
  if (self = nil) then
    result := ''
  else
  begin
    StringSplit(value, ':', l, r);
    if (l = 'tel') then
      result := r
    else if (l = 'mailto') then
      result := r
    else
      result := value;
  end;
end;

procedure Tv3TEL.DoClear;
begin
  inherited;
  value := '';
  useablePeriod := nil;
  use := [];
  capabilities := [];
end;

function Tv3TEL.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fvalue.length * sizeof(char)) + 12);
  inc(result, FuseablePeriod.sizeInBytes);
end;

{ Tv3II }

Function Tv3II.RIMClassNameV : String;
Begin
  Result := 'II';
End;

procedure Tv3II.Assign(oSource: TFslObject);
begin
  inherited;
  Fdisplayable := Tv3II(oSource).Fdisplayable;
  FHasdisplayable := Tv3II(oSource).FHasdisplayable;
  extension := Tv3II(oSource).Fextension;
  identifierName := Tv3II(oSource).FidentifierName;
  root := Tv3II(oSource).Froot;
  reliability := Tv3II(oSource).Freliability;
  scope := Tv3II(oSource).Fscope;
end;

function Tv3II.Clone(parent : Tv3Base): Tv3II;
begin
  Result := Tv3II(inherited Clone(parent));
end;


Procedure Tv3II.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'extension', Fextension));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'identifierName', FidentifierName));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'root', Froot));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'reliability', Ord(Freliability), CODES_Tv3IdentifierReliability));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'scope', Ord(Fscope), CODES_Tv3IdentifierScope));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'displayable', FHasDisplayable, Fdisplayable));
end;

function Tv3II.Link: Tv3II;
begin
  Result := Tv3II(Inherited Link);
end;

procedure Tv3II.Setdisplayable(const Value: boolean);
begin
  FHasDisplayable := true;
  Fdisplayable := Value;

end;

procedure Tv3II.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'extension' Then
    Fextension := aValue.AsString
  else if aValue.Name = 'identifierName' Then
    FidentifierName := aValue.AsString
  else if aValue.Name = 'root' Then
    Froot := aValue.AsString
  else if aValue.Name = 'reliability' Then
    Freliability := Tv3IdentifierReliability(aValue.AsEnum)
  else if aValue.Name = 'scope' Then
    Fscope := Tv3IdentifierScope(aValue.AsEnum)
  else if aValue.Name = 'displayable' Then
    aValue.AsBool(FHasDisplayable, Fdisplayable)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3II.LiteralV: String;
begin
  if FnullFlavor <> nfNull Then
    Result := nullFlavorLiteral
  Else if Fextension <> '' Then
    result := Froot + '::'+FExtension
  Else
    Result := Froot;
end;

Const
  HI_ROOT = '1.2.36.1.2001.1003.0.';

function renderHI(s : String) : String;
begin
  result := copy(s, length(HI_ROOT)+1, $FFFF);
  insert(' ', result, 5);
  insert(' ', result, 10);
  insert(' ', result, 15);
//  if StringStartsWith(copy(s, length(HI_ROOT)+1, $FF), '800360') then
//    result := result + 'IHI: ' + result
//  else if StringStartsWith(copy(s, length(HI_ROOT)+1, $FF), '800361') then
//    result := result + 'HPI-I: ' + result
//  else if StringStartsWith(copy(s, length(HI_ROOT)+1, $FF), '800362') then
//    result := result + 'HPI-O: ' + result
//  else if StringStartsWith(copy(s, length(HI_ROOT)+1, $FF), '800363') then
//    result := result + 'CSP-I: ' + result
//  else
//    result := result + 'HI: ' + result;
end;

function Tv3II.render: String;
begin
  if self = nil  then
    result := ''
  else if (extension <> '') then
    result := extension
  else if (StringStartsWith(root, HI_ROOT)) then
    result := renderHI(root)
  else
    result := root;
  if identifierName <> '' then
    result := identifierName+': ' + result;
end;

function Tv3II.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3II) and IsNonNull and (Tv3II(oOther).IsNonNull) and (Tv3II(oOther).root = root) and (Tv3II(oOther).extension = extension);
end;

procedure Tv3II.DoClear;
begin
  inherited;
  displayable := false;
  extension := '';
  identifierName := '';
  root := '';
  reliability := irNull;
  scope := cda_types.isNull;
  FHasDisplayable := false;
end;

function Tv3II.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fextension.length * sizeof(char)) + 12);
  inc(result, (FidentifierName.length * sizeof(char)) + 12);
  inc(result, (Froot.length * sizeof(char)) + 12);
end;

{ Tv3XP }

Function Tv3XP.RIMClassNameV : String;
Begin
  Result := 'XP';
End;

procedure Tv3XP.Assign(oSource: TFslObject);
begin
  inherited;
  codeSystem := Tv3XP(oSource).FcodeSystem;
  codeSystemVersion := Tv3XP(oSource).FcodeSystemVersion;
  code := Tv3XP(oSource).Fcode;
  language := Tv3XP(oSource).Flanguage;
  nullFlavor := Tv3XP(oSource).FnullFlavor;
  value := Tv3XP(oSource).Fvalue;
end;

function Tv3XP.Clone(parent : Tv3Base): Tv3XP;
begin
  Result := Tv3XP(inherited Clone(parent));
end;


Procedure Tv3XP.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'codeSystem', FcodeSystem));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'codeSystemVersion', FcodeSystemVersion));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'code', Fcode));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'language', Flanguage));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'nullFlavor', Ord(FnullFlavor), CODES_Tv3NullFlavor));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'value', Fvalue));
end;

function Tv3XP.Link: Tv3XP;
begin
  Result := Tv3XP(Inherited Link);
end;

procedure Tv3XP.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'codeSystem' Then
    FcodeSystem := aValue.AsString
  else if aValue.Name = 'codeSystemVersion' Then
    FcodeSystemVersion := aValue.AsString
  else if aValue.Name = 'code' Then
    Fcode := aValue.AsString
  else if aValue.Name = 'language' Then
    Flanguage := aValue.AsString
  else if aValue.Name = 'nullFlavor' Then
    FnullFlavor := Tv3NullFlavor(aValue.AsEnum)
  else if aValue.Name = 'value' Then
    Fvalue := aValue.AsString
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3XP.isNonNull: Boolean;
begin
  result := nullFlavor = nfNull;
end;

procedure Tv3XP.DoClear;
begin
  inherited;
  codeSystem := '';
  codeSystemVersion := '';
  code := '';
  language := '';
  nullFlavor := nfNull;
  value := '';
end;

function Tv3XP.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FcodeSystem.length * sizeof(char)) + 12);
  inc(result, (FcodeSystemVersion.length * sizeof(char)) + 12);
  inc(result, (Fcode.length * sizeof(char)) + 12);
  inc(result, (Flanguage.length * sizeof(char)) + 12);
  inc(result, (Fvalue.length * sizeof(char)) + 12);
end;

{ Tv3ADXP }

Function Tv3ADXP.RIMClassNameV : String;
Begin
  Result := 'ADXP';
End;

procedure Tv3ADXP.Assign(oSource: TFslObject);
begin
  inherited;
  type_ := Tv3ADXP(oSource).Ftype;
end;

function Tv3ADXP.Clone(parent : Tv3Base): Tv3ADXP;
begin
  Result := Tv3ADXP(inherited Clone(parent));
end;


Procedure Tv3ADXP.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'type', Ord(Ftype), CODES_Tv3AddressPartType));
end;

function Tv3ADXP.Link: Tv3ADXP;
begin
  Result := Tv3ADXP(Inherited Link);
end;

procedure Tv3ADXP.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'type' Then
    Ftype := Tv3AddressPartType(aValue.AsEnum)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3ADXP.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3ADXP) and IsNonNull and (Tv3ADXP(oOther).IsNonNull) and (Tv3ADXP(oOther).value = value) and (Tv3ADXP(oOther).type_ = type_);
end;

procedure Tv3ADXP.DoClear;
begin
  inherited;
  Ftype := aptNull;
end;

{ Tv3AD }

Function Tv3AD.RIMClassNameV : String;
Begin
  Result := 'AD';
End;

procedure Tv3AD.Assign(oSource: TFslObject);
begin
  inherited;
  isNotOrdered := Tv3AD(oSource).isNotOrdered;
  HasIsNotOrdered := Tv3AD(oSource).HasIsNotOrdered;
  part := Tv3AD(oSource).Fpart.Clone(self);
  useablePeriod := Tv3AD(oSource).FuseablePeriod.Clone(self);
  use := Tv3AD(oSource).Fuse;
end;

function Tv3AD.Clone(parent : Tv3Base): Tv3AD;
begin
  Result := Tv3AD(inherited Clone(parent));
end;


destructor Tv3AD.Destroy;
begin
  Fpart.Free;
  FuseablePeriod.Free;
  inherited;
end;

Procedure Tv3AD.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'part', Fpart, rmpctList, 'ADXP'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'useablePeriod', FuseablePeriod, 'TEL'));
  oList.Add(Tv3PropertyDefinition.CreateSet(self, false, 'use', Fuse, CODES_Tv3PostalAddressUse));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'isNotOrdered', FHasIsNotOrdered, FIsNotOrdered));
end;

function Tv3AD.Link: Tv3AD;
begin
  Result := Tv3AD(Inherited Link);
end;

procedure Tv3AD.Setpart(const Value: Tv3ListADXP);
begin
  Fpart.Free;
  Fpart := Value;
  if Fpart <> nil then  part.parent := self;

end;

procedure Tv3AD.SetuseablePeriod(const Value: Tv3QSET);
begin
  FuseablePeriod.Free;
  FuseablePeriod := Value;
  if FuseablePeriod <> nil then  useablePeriod.parent := self;

end;

procedure Tv3AD.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'isNotOrdered' Then
    aValue.AsBool(FHasIsNotOrdered, FisNotOrdered)
  else if aValue.Name = 'part' Then
    part := Tv3ListADXP(aValue.AsType(Tv3ListADXP)).Clone(self)
  else if aValue.Name = 'useablePeriod' Then
    FuseablePeriod := Tv3QSET(aValue.asType(Tv3QSET)).Link
  else if aValue.Name = 'use' Then
    aValue.AsSet(Fuse)
  else
    Inherited SetPropertyValue(aValue);
end;

procedure Tv3AD.SetisNotOrdered(const Value: boolean);
begin
  FisNotOrdered := Value;
  HasIsNotOrdered := true;
end;

function Tv3AD.AddPart(sValue: String; type_: Tv3AddressPartType): Tv3ADXP;
begin
  if Part = nil Then
    Part := Tv3ListADXP.Create(self);
  result := Part.Append;
  result.value := sValue;
  result.type_ := type_;
end;

function Tv3AD.EqualsV(oOther: Tv3Base): Boolean;
var
  iLoop : integer;
begin
  result := (oOther is Tv3AD) and IsNonNull and (Tv3AD(oOther).IsNonNull) and (part <> nil) And (Tv3AD(oOther).part <> nil);
  if result Then
  Begin
    for iLoop := 0 to part.count - 1 Do
      if not Tv3AD(oOther).HasPart(part[iLoop]) Then
        result := False;
    for iLoop := 0 to Tv3AD(oOther).part.count - 1 Do
      if not HasPart(Tv3AD(oOther).part[iLoop]) Then
        result := False;
  End;
end;

function Tv3AD.HasPart(oPart: Tv3ADXP): Boolean;
var
  iLoop : integer;
begin
  result := False;
  if part <> nil Then
    for iLoop := 0 to part.count - 1 Do
      result := oPart.EqualsV(part[iLoop]);
end;

constructor Tv3AD.Create;
begin
  inherited;
  Fpart := Tv3ListADXP.Create(self);
end;

procedure Tv3AD.DoClear;
begin
  inherited;
  isNotOrdered := False;
  HasIsNotOrdered := False;
  part.ClearItems;
  useablePeriod := nil;
  use := [];
end;

function Tv3AD.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fpart.sizeInBytes);
  inc(result, FuseablePeriod.sizeInBytes);
end;

{ Tv3ENXP }

Function Tv3ENXP.RIMClassNameV : String;
Begin
  Result := 'ENXP';
End;

procedure Tv3ENXP.Assign(oSource: TFslObject);
begin
  inherited;
  type_ := Tv3ENXP(oSource).Ftype;
  qualifier := Tv3ENXP(oSource).Fqualifier;
end;

function Tv3ENXP.Clone(parent : Tv3Base): Tv3ENXP;
begin
  Result := Tv3ENXP(inherited Clone(parent));
end;


Procedure Tv3ENXP.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'type', Ord(Ftype), CODES_Tv3EntityNamePartType));
  oList.Add(Tv3PropertyDefinition.CreateSet(self, false, 'qualifier', Fqualifier, CODES_Tv3EntityNamePartQualifier));
end;

function Tv3ENXP.Link: Tv3ENXP;
begin
  Result := Tv3ENXP(Inherited Link);
end;

procedure Tv3ENXP.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'type' Then
    Ftype := Tv3EntityNamePartType(aValue.AsEnum)
  else if aValue.Name = 'qualifier' Then
    aValue.AsSet(Fqualifier)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3ENXP.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3ENXP) and IsNonNull and (Tv3ENXP(oOther).IsNonNull) and (Tv3ENXP(oOther).value = value) and (Tv3ENXP(oOther).type_ = type_);
end;

procedure Tv3ENXP.DoClear;
begin
  inherited;
  type_ := nptNull;
  qualifier := [];
end;

{ Tv3EN }

Function Tv3EN.RIMClassNameV : String;
Begin
  Result := 'EN';
End;

procedure Tv3EN.Assign(oSource: TFslObject);
begin
  inherited;
  part := Tv3EN(oSource).Fpart.Clone(self);
  use := Tv3EN(oSource).Fuse;
end;

function Tv3EN.Clone(parent : Tv3Base): Tv3EN;
begin
  Result := Tv3EN(inherited Clone(parent));
end;


destructor Tv3EN.Destroy;
begin
  Fpart.Free;
  inherited;
end;

Procedure Tv3EN.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'part', Fpart, rmpctList, 'ENXP'));
  oList.Add(Tv3PropertyDefinition.CreateSet(self, false, 'use', Fuse, CODES_Tv3EntityNameUse));
end;

function Tv3EN.render: String;
var
  i : integer;
  title, surname, given : String;
  p : Tv3ENXP;
begin
  if self = nil then
    exit('');

  if (part.Count = 1) and (part[0].type_ = nptNull) then
    result := part[0].value
  else
  begin
    for i := 0 to part.Count - 1 do
    begin
      p := part[i];
      if (p.type_ = nptTITLE) and (npqPFX in p.qualifier) then
        title := p.value;
      if (p.type_ = nptFAM) then
        surname := p.value;
      if (p.type_ = nptGIV) then
        given := p.value;
    end;
    if surname = '' then
      result := given
    else if title = '' then
      result := uppercase(surname)+', '+given
    else
      result := uppercase(surname)+', '+title+' '+given;
  end;
end;

function Tv3EN.Link: Tv3EN;
begin
  Result := Tv3EN(Inherited Link);
end;

procedure Tv3EN.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'part' Then
    part := Tv3ListENXP(aValue.AsType(Tv3ListENXP)).Clone(self)
  else if aValue.Name = 'use' Then
    aValue.AsSet(Fuse)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3EN.AddPart(sValue: String; type_: Tv3EntityNamePartType): Tv3ENXP;
begin
  if Part = nil Then
    Part := Tv3ListENXP.Create(self);
  result := Part.Append;
  result.value := sValue;
  result.type_ := type_;
end;

function Tv3EN.AddFamily(sValue: String): Tv3ENXP;
begin
  result := AddPart(sValue, nptFAM);
end;

function Tv3EN.AddGiven(sValue: String): Tv3ENXP;
begin
  result := AddPart(sValue, nptGIV);
end;

function Tv3EN.AddPrefix(sValue: String): Tv3ENXP;
begin
  result := AddPart(sValue, nptTITLE);
  result.qualifier := [npqSFX];
end;

function Tv3EN.AddSuffix(sValue: String): Tv3ENXP;
begin
  result := AddPart(sValue, nptTITLE);
  result.qualifier := [npqPFX];
end;

function Tv3EN.EqualsV(oOther: Tv3Base): Boolean;
var
  o1, o2 : Tv3EN;
  iLoop : integer;
begin
  result := (oOther is Tv3EN) and IsNonNull and (Tv3EN(oOther).IsNonNull) and (part <> nil) And (Tv3EN(oOther).part <> nil) And (Tv3EN(oOther).part.Count = part.Count);
  if result Then
  Begin
    o1 := Canonical;
    o2 := Tv3EN(oOther).Canonical;
    Try
      For iLoop := 0 to o1.part.count Do
        if not o2.part[iLoop].EqualsV(o1.part[iLoop]) Then
          result := false;
    Finally
      o2.Free;
      o1.Free;
    End;
  End;
end;

function Tv3EN.canonical: Tv3EN;
var
  i : Integer;
begin
  result := Tv3EN.Create;
  Try
    result.use := use;
    // titles not attached to a part
    For i := 0 to part.Count - 1 do
      if (part[i].Ftype = nptTITLE) And not (npqPFX in part[i].Fqualifier) And not (npqSFX in part[i].Fqualifier) Then
        result.part.Add(part[i].Link);

    // given names with their attached parts
    for i := 0 to part.Count - 1 do
      if (part[i].Ftype = nptGIV) Then
      Begin
        if (i > 0) And (part[i-1].Ftype = nptTITLE) And (npqPFX in part[i-1].Fqualifier) Then
          result.part.Add(part[i-1].Link);
        result.part.Add(part[i].Link);
        if (i > 0) And (part[i+1].Ftype = nptTITLE) And (npqSFX in part[i+1].Fqualifier) Then
          result.part.Add(part[i+1].Link);
      End;

    // surnames with their attached parts
    for i := 0 to part.Count - 1 do
      if (part[i].Ftype = nptFAM) Then
      Begin
        if (i > 0) And (part[i-1].Ftype = nptTITLE) And (npqPFX in part[i-1].Fqualifier) Then
          result.part.Add(part[i-1].Link);
        result.part.Add(part[i].Link);
        if (i > 0) And (part[i+1].Ftype = nptTITLE) And (npqSFX in part[i+1].Fqualifier) Then
          result.part.Add(part[i+1].Link);
      End;

    result.Link;
  Finally
    result.Free;
  End;
end;

constructor Tv3EN.Create;
begin
  inherited;
  Fpart := Tv3ListENXP.Create(self);
end;

procedure Tv3EN.Setpart(const Value: Tv3ListENXP);
begin
  FPart.Free;
  Fpart := Value;
  if Fpart <> nil then  part.parent := self;

end;

function Tv3EN.AddPlain(sValue: String): Tv3ENXP;
begin
  result := AddPart(sValue, nptNull);
end;

procedure Tv3EN.DoClear;
begin
  inherited;
  part.ClearItems;
  use := [];
end;

function Tv3EN.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fpart.sizeInBytes);
end;

{ Tv3QTY }

Function Tv3QTY.RIMClassNameV : String;
Begin
  Result := 'QTY';
End;

procedure Tv3QTY.Assign(oSource: TFslObject);
begin
  inherited;
  originalText := Tv3QTY(oSource).ForiginalText.Clone(self);
  expression := Tv3QTY(oSource).Fexpression.Clone(self);
  uncertainRange := Tv3QTY(oSource).FuncertainRange.Clone(self);
  uncertainty := Tv3QTY(oSource).Funcertainty.Clone(self);
  uncertaintyType := Tv3QTY(oSource).FuncertaintyType;
end;

function Tv3QTY.Clone(parent : Tv3Base): Tv3QTY;
begin
  Result := Tv3QTY(inherited Clone(parent));
end;


destructor Tv3QTY.Destroy;
begin
  ForiginalText.Free;
  Fexpression.Free;
  FuncertainRange.Free;
  Funcertainty.Free;
  inherited;
end;

function Tv3QTY.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, ForiginalText.sizeInBytes);
  inc(result, Fexpression.sizeInBytes);
  inc(result, FuncertainRange.sizeInBytes);
  inc(result, Funcertainty.sizeInBytes);
end;

class function Tv3QTY.DiffType: Tv3QTYDatatype;
begin
  result := Tv3QTY;
end;

Procedure Tv3QTY.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'originalText', ForiginalText, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'expression', Fexpression, 'ED'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'uncertainRange', FuncertainRange, 'URG_QTY'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'uncertainty', Funcertainty, 'QTY'));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'uncertaintyType', Ord(FuncertaintyType), CODES_Tv3UncertaintyType));
end;

function Tv3QTY.Link: Tv3QTY;
begin
  Result := Tv3QTY(Inherited Link);
end;

procedure Tv3QTY.Setexpression(const Value: Tv3ED);
begin
  Fexpression.Free;
  Fexpression := Value;
  if Fexpression <> nil then  expression.parent := self;

end;

procedure Tv3QTY.SetoriginalText(const Value: Tv3ED);
begin
  ForiginalText.Free;
  ForiginalText := Value;
  if ForiginalText <> nil then  originalText.parent := self;

end;

procedure Tv3QTY.SetuncertainRange(const Value: Tv3IVL);
begin
  FuncertainRange.Free;
  FuncertainRange := Value;
  if FuncertainRange <> nil then  uncertainRange.parent := self;

end;

procedure Tv3QTY.Setuncertainty(const Value: Tv3QTY);
begin
  Funcertainty.Free;
  Funcertainty := Value;
  if Funcertainty <> nil then  uncertainty.parent := self;

end;

procedure Tv3QTY.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'originalText' Then
    originalText := Tv3ED(aValue.AsType(Tv3ED)).Link
  else if aValue.Name = 'expression' Then
    Fexpression := Tv3ED(aValue.AsType(Tv3ED)).Link
  else if aValue.Name = 'uncertainRange' Then
    FuncertainRange := Tv3IVL(aValue.AsType(Tv3IVL)).Link
  else if aValue.Name = 'uncertainty' Then
    Funcertainty := Tv3QTY(aValue.AsType(Tv3QTY)).Link
  else if aValue.Name = 'uncertaintyType' Then
    FuncertaintyType := Tv3UncertaintyType(aValue.AsEnum)
  else
    Inherited SetPropertyValue(aValue);
end;

procedure Tv3QTY.DoClear;
begin
  inherited;
  originalText := nil;
  expression := nil;
  uncertainRange := nil;
  uncertainty := nil;
  uncertaintyType := utNull;
end;

{ Tv3INT }

Function Tv3INT.RIMClassNameV : String;
Begin
  Result := 'INT';
End;

procedure Tv3INT.Assign(oSource: TFslObject);
begin
  inherited;
  Fvalue := Tv3INT(oSource).Fvalue;
  FHasvalue := Tv3INT(oSource).FHasvalue;
end;

function Tv3INT.Clone(parent : Tv3Base): Tv3INT;
begin
  Result := Tv3INT(inherited Clone(parent));
end;

constructor Tv3INT.Create;
begin
  Inherited;
  FHasValue := False;
end;


class function Tv3INT.DiffType: Tv3QTYDatatype;
begin
  Result := Tv3INT;
end;

Procedure Tv3INT.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateInteger(self, false, 'value', FHasValue, Fvalue));
end;

function Tv3INT.Link: Tv3INT;
begin
  Result := Tv3INT(Inherited Link);
end;

procedure Tv3INT.Setvalue(const Value: int64);
begin
  FHasValue := true;
  Fvalue := Value;
end;

procedure Tv3INT.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'value' Then
    aValue.AsInt(FHasValue, Fvalue)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3INT.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3INT) and IsNonNull and (Tv3INT(oOther).IsNonNull) and (Tv3INT(oOther).value = value);
end;

function Tv3INT.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := IntegerToString(Fvalue);
end;

procedure Tv3INT.DoClear;
begin
  inherited;
  FHasValue := False;
  Fvalue := 0;
end;

{ Tv3CO }

Function Tv3CO.RIMClassNameV : String;
Begin
  Result := 'CO';
End;

procedure Tv3CO.Assign(oSource: TFslObject);
begin
  inherited;
  value := Tv3CO(oSource).Fvalue;
  code := Tv3CO(oSource).Fcode.Clone(self);
end;

function Tv3CO.Clone(parent : Tv3Base): Tv3CO;
begin
  Result := Tv3CO(inherited Clone(parent));
end;


destructor Tv3CO.Destroy;
begin
  Fcode.Free;
  inherited;
end;

function Tv3CO.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcode.sizeInBytes);
end;

class function Tv3CO.DiffType: Tv3QTYDatatype;
begin
  Result := Tv3CO;
end;

Procedure Tv3CO.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateDecimal(self, false, 'value', Fvalue));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
end;

function Tv3CO.Link: Tv3CO;
begin
  Result := Tv3CO(Inherited Link);
end;

procedure Tv3CO.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure Tv3CO.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'value' Then
    Fvalue := TFslDecimal.ValueOf(aValue.AsString)
  else if aValue.Name = 'code' Then
    Fcode := Tv3CD(aValue.AsType(Tv3CD)).Link
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3CO.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3CO) and IsNonNull and (Tv3CO(oOther).IsNonNull) and (code <> nil) and (Tv3CO(oOther).code.equals(code));
end;

procedure Tv3CO.Setvalue(const Value: TFslDecimal);
begin
  Fvalue := Value;
end;

procedure Tv3CO.DoClear;
begin
  inherited;
  value := TFslDecimal.ValueOf('0');
  code := nil;
end;

{ Tv3REAL }

Function Tv3REAL.RIMClassNameV : String;
Begin
  Result := 'REAL';
End;

procedure Tv3REAL.Assign(oSource: TFslObject);
begin
  inherited;
  Fvalue := Tv3CO(oSource).Fvalue;
end;

function Tv3REAL.Clone(parent : Tv3Base): Tv3REAL;
begin
  Result := Tv3REAL(inherited Clone(parent));
end;


class function Tv3REAL.DiffType: Tv3QTYDatatype;
begin
  result := Tv3REAL;
end;

Procedure Tv3REAL.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateDecimal(self, false, 'value', Fvalue));
end;

function Tv3REAL.Link: Tv3REAL;
begin
  Result := Tv3REAL(Inherited Link);
end;

procedure Tv3REAL.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'value' Then
    Fvalue := TFslDecimal.ValueOF(aValue.AsString)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3REAL.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3REAL) and IsNonNull and (Tv3REAL(oOther).IsNonNull) and (TFslDecimal.Equals(Tv3REAL(oOther).value, value));
end;

destructor Tv3REAL.Destroy;
begin
  inherited;
end;

procedure Tv3REAL.Setvalue(const Value: TFslDecimal);
begin
  Fvalue := Value;
end;

function Tv3REAL.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := Fvalue.AsString;
end;

procedure Tv3REAL.DoClear;
begin
  inherited;
  value := TFslDecimal.makeZero;
end;

{ Tv3RTO }

Function Tv3RTO.RIMClassNameV : String;
Begin
  Result := 'RTO';
End;

procedure Tv3RTO.Assign(oSource: TFslObject);
begin
  inherited;
  NumeratorType := Tv3RTO(oSource).NumeratorType;
  DenominatorType := Tv3RTO(oSource).DenominatorType;
  denominator := Tv3RTO(oSource).Fdenominator.Clone(self);
  numerator := Tv3RTO(oSource).Fnumerator.Clone(self);
end;

function Tv3RTO.Clone(parent : Tv3Base): Tv3RTO;
begin
  Result := Tv3RTO(inherited Clone(parent));
end;


destructor Tv3RTO.Destroy;
begin
  Fdenominator.Free;
  Fnumerator.Free;
  inherited;
end;

function Tv3RTO.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fdenominator.sizeInBytes);
  inc(result, Fnumerator.sizeInBytes);
end;

class function Tv3RTO.DiffType: Tv3QTYDatatype;
begin
  result := Tv3RTO;
end;

Procedure Tv3RTO.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'denominator', Fdenominator, 'QTY'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'numerator', Fnumerator, 'QTY'));
//  else if aValue.Name = 'DenominatorType' Then
//    result := FDe(Tv3PropertynominatorTyp.CreateClass)
//  else if aValue.Name = 'NumeratorType' Then
//    result := FNu(Tv3PropertymeratorTyp.CreateClass)
end;

function Tv3RTO.Link: Tv3RTO;
begin
  Result := Tv3RTO(Inherited Link);
end;

procedure Tv3RTO.Setdenominator(const Value: Tv3QTY);
begin
  Fdenominator.Free;
  Fdenominator := Value;
  if Fdenominator <> nil then  denominator.parent := self;

end;

procedure Tv3RTO.Setnumerator(const Value: Tv3QTY);
begin
  Fnumerator.Free;
  Fnumerator := Value;
  if Fnumerator <> nil then  numerator.parent := self;

end;

procedure Tv3RTO.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'denominator' Then
    Fdenominator := Tv3QTY(aValue.AsType(Tv3QTY)).Link
  else if aValue.Name = 'numerator' Then
    Fnumerator := Tv3QTY(aValue.AsType(Tv3QTY)).Link
//  else if aValue.Name = 'DenominatorType' Then
//    FDenominatorType := Tv3QTYDatatype(aValue.AsType(Tv3QTYDatatype)).Link
//  else if aValue.Name = 'NumeratorType' Then
//    FNumeratorType := Tv3QTYDatatype(aValue.AsType(Tv3QTYDatatype)).Link
  else
    Inherited SetPropertyValue(aValue);
end;

procedure Tv3RTO.DoClear;
begin
  inherited;
  denominator := nil;
  numerator := nil;
  DenominatorType := Tv3QTY;
  NumeratorType := Tv3QTY;
end;

{ Tv3PQ }

Function Tv3PQ.RIMClassNameV : String;
Begin
  Result := 'PQ';
End;

procedure Tv3PQ.Assign(oSource: TFslObject);
begin
  inherited;
  value := Tv3PQ(oSource).Fvalue;
  unit_ := Tv3PQ(oSource).Funit;
  codingRationale := Tv3PQ(oSource).FcodingRationale;
  translation := Tv3PQ(oSource).Ftranslation.Clone(self);
end;

function Tv3PQ.Clone(parent : Tv3Base): Tv3PQ;
begin
  Result := Tv3PQ(inherited Clone(parent));
end;


destructor Tv3PQ.Destroy;
begin
  Ftranslation.Free;
  inherited;
end;

function Tv3PQ.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Funit.length * sizeof(char)) + 12);
  inc(result, Ftranslation.sizeInBytes);
end;

class function Tv3PQ.DiffType: Tv3QTYDatatype;
begin
  result := Tv3PQ;
end;

Procedure Tv3PQ.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateDecimal(self, false, 'value', Fvalue));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'unit', Funit));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'codingRationale', Ord(FcodingRationale), CODES_Tv3CodingRationale));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'translation', Ftranslation, rmpctSet, 'PQR'));
end;

function Tv3PQ.Link: Tv3PQ;
begin
  Result := Tv3PQ(Inherited Link);
end;

procedure Tv3PQ.Settranslation(const Value: Tv3SetPQR);
begin
  Ftranslation.Free;
  Ftranslation := Value;
  if Ftranslation <> nil then  translation.parent := self;

end;

procedure Tv3PQ.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'value' Then
    Fvalue := TFslDecimal.ValueOf(aValue.AsString)
  else if aValue.Name = 'unit' Then
    Funit := aValue.AsString
  else if aValue.Name = 'codingRationale' Then
    FcodingRationale := Tv3CodingRationale(aValue.AsEnum)
  else if aValue.Name = 'translation' Then
    translation := Tv3SetPQR(aValue.AsType(Tv3SetPQR)).Clone(self)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3PQ.EqualsV(oOther: Tv3Base): Boolean;
var
  o1, o2 : Tv3PQ;
begin
  if oOther is Tv3PQ then
  Begin
    o1 := Tv3PQ(oOther).canonical;
    Try
      o2 := canonical;
      Try
        result := o1.isNonNull and o2.isNonNull and TFslDecimal.Equals(o1.value, o2.value) and (o1.unit_ = o2.unit_);
      Finally
        o2.Free;
      End;
    Finally
      o1.free;
    End
  End
  Else
    result := false;
end;

procedure Tv3PQ.Setvalue(const Value: TFslDecimal);
begin
  Fvalue := Value;
end;

constructor Tv3PQ.Create;
begin
  Inherited;
  Ftranslation := Tv3SetPQR.Create(self);
end;

function Tv3PQ.canonical: TV3PQ;
var
  oPair1, oPair2 : TUcumPair;
begin
  result := Tv3PQ.Create;
  Try
    if (self = nil) or isNull or (value.isNull) or (unit_ = '') Then
      result.nullFlavor := nfNI
    else
    Begin
      Try
        oPair1 := TUcumPair.Create(value.Link, unit_);
        Try
          oPair2 := nil; // todo GUcums.DefaultDefinition.getCanonicalForm(oPair1);
          Try
            result.value := oPair2.Value.Link;
            result.unit_ := oPair2.UnitCode;
            result.translation.Add(self.Link);
          Finally
            oPair2.Free;
          End;
        Finally
          oPair1.Free;
        End;
      Except
        result.nullFlavor := nfUNK;
      End;
    End;
  Finally
    result.Free;
  End;
end;

function Tv3PQ.comparable(other: Tv3QTY): Tv3BL;
begin
  result := Tv3BL.Create;
  Try
    if isNull or other.isNull or not (other is TV3PQ) or (unit_ = '') or (TV3PQ(other).unit_ = '') Then
      result.nullFlavor := nfNI
    Else
      result.value := false; // GUcums.DefaultDefinition.getCanonicalUnits(unit_) = GUcums.DefaultDefinition.getCanonicalUnits(TV3PQ(other).unit_);
    result.Link;
  finally
    result.Free;
  end;
end;

function Tv3PQ.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := Fvalue.AsString+' ' +Funit;
end;

procedure Tv3PQ.DoClear;
begin
  inherited;
  value := TFslDecimal.makeNull;
  unit_ := '';
  codingRationale := crNull;
  translation.ClearItems;
end;

{ Tv3PQR }

Function Tv3PQR.RIMClassNameV : String;
Begin
  Result := 'PQR';
End;

procedure Tv3PQR.Assign(oSource: TFslObject);
begin
  inherited;
  value := Tv3PQR(oSource).Fvalue.Link;
end;

function Tv3PQR.Clone(parent : Tv3Base): Tv3PQR;
begin
  Result := Tv3PQR(inherited Clone(parent));
end;


Procedure Tv3PQR.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateDecimal(self, false, 'value', Fvalue));
end;

function Tv3PQR.Link: Tv3PQR;
begin
  Result := Tv3PQR(Inherited Link);
end;

procedure Tv3PQR.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'value' Then
    Fvalue := TFslDecimal.ValueOf(aValue.AsString)
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3PQR.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3PQR) and IsNonNull and (Tv3PQR(oOther).IsNonNull) and (Tv3PQR(oOther).value.Equals(value)) and (Tv3PQR(oOther).code = code) and (Tv3PQR(oOther).codeSystem = codeSystem);
end;

destructor Tv3PQR.Destroy;
begin
  inherited;
end;

procedure Tv3PQR.Setvalue(const Value: TFslDecimal);
begin
  Fvalue := Value;
end;

procedure Tv3PQR.DoClear;
begin
  inherited;
  value := TFslDecimal.makeNull;
end;

function Tv3PQR.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ Tv3MO }

Function Tv3MO.RIMClassNameV : String;
Begin
  Result := 'MO';
End;

procedure Tv3MO.Assign(oSource: TFslObject);
begin
  inherited;
  value := Tv3MO(oSource).Fvalue.Clone;
  currency := Tv3MO(oSource).Fcurrency;
end;

function Tv3MO.Clone(parent : Tv3Base): Tv3MO;
begin
  Result := Tv3MO(inherited Clone(parent));
end;


function Tv3MO.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fcurrency.length * sizeof(char)) + 12);
end;

class function Tv3MO.DiffType: Tv3QTYDatatype;
begin
  Result := Tv3MO;
end;

Procedure Tv3MO.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateDecimal(self, false, 'value', Fvalue));
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'currency', Fcurrency));
end;

function Tv3MO.Link: Tv3MO;
begin
  Result := Tv3MO(Inherited Link);
end;

procedure Tv3MO.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'value' Then
    Fvalue := TFslDecimal.Create(aValue.AsString)
  else if aValue.Name = 'currency' Then
    Fcurrency := aValue.AsString
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3MO.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3MO) and IsNonNull and (Tv3MO(oOther).IsNonNull) and TFslDecimal.Equals(Tv3MO(oOther).value, value) and (Tv3MO(oOther).currency = currency);
end;

destructor Tv3MO.Destroy;
begin
  inherited;
end;

procedure Tv3MO.Setvalue(const Value: TFslDecimal);
begin
  Fvalue := Value;
end;

function Tv3MO.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := Fvalue.AsString+' ' +Fcurrency;
end;

procedure Tv3MO.DoClear;
begin
  inherited;
  value := TFslDecimal.makeNull;
  currency := '';
end;

{ Tv3TS }

Function Tv3TS.RIMClassNameV : String;
Begin
  Result := 'TS';
End;

procedure Tv3TS.Assign(oSource: TFslObject);
begin
  inherited;
  value := Tv3TS(oSource).Fvalue;
end;

function Tv3TS.Clone(parent : Tv3Base): Tv3TS;
begin
  Result := Tv3TS(inherited Clone(parent));
end;


function Tv3TS.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fvalue.length * sizeof(char)) + 12);
end;

class function Tv3TS.DiffType: Tv3QTYDatatype;
begin
  result := Tv3PQ;
end;

Procedure Tv3TS.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateString(self, false, 'value', Fvalue));
end;

function Tv3TS.Link: Tv3TS;
begin
  Result := Tv3TS(Inherited Link);
end;

procedure Tv3TS.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'value' Then
    Fvalue := aValue.AsString
  else
    Inherited SetPropertyValue(aValue);
end;

function Tv3TS.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3TS) and IsNonNull and (Tv3TS(oOther).IsNonNull) and (Tv3TS(oOther).value = value);
end;

function Tv3TS.LiteralV: String;
begin
  if FnullFlavor <> nfNull then
    result := nullFlavorLiteral
  Else
    result := Fvalue;
end;

function month(s : string) : TMonthOfYear;
var
  i : integer;
begin
  i := strtoint(s);
  result := TMonthOfYear(i-1);
end;

function Tv3TS.render: String;
var
  t, z : string;
begin
  if self = nil then
  begin
    result := '';
    exit;
  end;

  if pos('-', value) > 0 then
    StringSplit(value, '-', t, z)
  else
    StringSplit(value, '+', t, z);

  if ((length(t) = 14) and (copy(t, 9, 6) = '000000')) or ((length(t) = 12) and (copy(t, 9, 4) = '0000')) then
    t := copy(t, 1, 8);
  case length(t) of
    4: result := t;
    6: result := MONTHOFYEAR_SHORT[month(copy(t, 5, 2))]+ ' '+ copy(t, 1, 4);
    8: result := copy(t, 7, 2)+' '+MONTHOFYEAR_SHORT[month(copy(t, 5, 2))]+ ' '+ copy(t, 1, 4);
    12: result := copy(t, 7, 2)+' '+MONTHOFYEAR_SHORT[month(copy(t, 5, 2))]+ ' '+ copy(t, 1, 4) +' '+copy(t, 9, 2)+':'+copy(t, 11, 2);
    14..20: result := copy(t, 7, 2)+' '+MONTHOFYEAR_SHORT[month(copy(t, 5, 2))]+ ' '+ copy(t, 1, 4) +' '+copy(t, 9, 2)+':'+copy(t, 11, 2)+':'+copy(t, 13, 2);
  else
    result := '??';
  end;

  if (z <> '') and (length(t) > 8) then
  begin
    System.insert(':', z, 3);
    if pos('-', value) > 0 then
      result := result + '(-'+z+')'
    else
      result := result + '(+'+z+')';
  end;
end;

procedure Tv3TS.DoClear;
begin
  inherited;
  value := '';
end;

{ Tv3QSET }

Function Tv3QSET.AddComp(oComp: Tv3QSET) : Boolean;
begin
  Result := False;
  RaiseError('AddComp', 'Must override '+ClassName+'.AddComp');
end;

Function Tv3QSET.RIMClassNameV : String;
Begin
  Result := 'QSET';
End;

procedure Tv3QSET.Assign(oSource: TFslObject);
begin
  inherited;
  FParamType := Tv3QSET(oSource).FParamType;
  originalText := Tv3QSET(oSource).ForiginalText.Clone(self);
  Flat := Tv3QSET(oSource).Flat;
end;

function Tv3QSET.Clone(parent : Tv3Base): Tv3QSET;
begin
  Result := Tv3QSET(inherited Clone(parent));
end;


destructor Tv3QSET.Destroy;
begin
  ForiginalText.Free;
  inherited;
end;

Procedure Tv3QSET.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'originalText', ForiginalText, 'QSET'));
//  else if aValue.Name = 'ParamType' Then
//    result := FPa(Tv3PropertyramType.CreateClass)
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, true, 'Flat', true, FFlat));
end;

function Tv3QSET.Link: Tv3QSET;
begin
  Result := Tv3QSET(Inherited Link);
end;

procedure Tv3QSET.SetoriginalText(const Value: Tv3ED);
begin
  ForiginalText.Free;
  ForiginalText := Value;
  if ForiginalText <> nil then  originalText.parent := self;

end;

procedure Tv3QSET.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'originalText' Then
    ForiginalText := Tv3ED(aValue.AsType(Tv3ED)).Link
//  else if aValue.Name = 'ParamType' Then
//    FParamType  := Tv3QTYDatatype(aValue.AsType(Tv3QTYDatatype)).Link
  else if aValue.Name = 'Flat' Then
    FFlat := StrToBoolDef(aValue.AsString, false)
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3QSET.Create(aParamType: Tv3QTYDatatype);
begin
  Inherited Create;
  FParamType := aParamType;
end;

procedure Tv3QSET.DoClear;
begin
  inherited;
  originalText := nil;
  ParamType  := Tv3QTY;
  Flat := false;
end;

function Tv3QSET.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, ForiginalText.sizeInBytes);
end;

{ Tv3QSU }

Function Tv3QSU.AddComp(oComp: Tv3QSET) : Boolean;
begin
  Result := True;
  If Fterms = nil then
    FTerms := Tv3SetQSET.Create(FParamType, self);
  Fterms.add(oComp);
end;

Function Tv3QSU.RIMClassNameV : String;
Begin
  Result := 'QSU';
End;

procedure Tv3QSU.Assign(oSource: TFslObject);
begin
  inherited;
  terms := Tv3QSU(oSource).Fterms.Clone(self);
end;

function Tv3QSU.Clone(parent : Tv3Base): Tv3QSU;
begin
  Result := Tv3QSU(inherited Clone(parent));
end;


destructor Tv3QSU.Destroy;
begin
  Fterms.Free;
  inherited;
end;

Procedure Tv3QSU.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'terms', Fterms, rmpctSet, 'QSET'));
end;

function Tv3QSU.Link: Tv3QSU;
begin
  Result := Tv3QSU(Inherited Link);
end;

procedure Tv3QSU.Setterms(const Value: Tv3SetQSET);
begin
  Fterms.Free;
  Fterms := Value;
  if Fterms <> nil then  terms.parent := self;

end;

procedure Tv3QSU.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'terms' Then
    terms := Tv3SetQSET(aValue.AsType(Tv3SetQSET)).Clone(self)
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3QSU.Create(aParamType: Tv3QTYDatatype);
begin
  Inherited;
  Fterms := Tv3SetQSET.Create(aParamType, self);
  FParamType := aParamType;
end;

procedure Tv3QSU.DoClear;
begin
  inherited;
  terms.clearItems;
end;

function Tv3QSU.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fterms.sizeInBytes);
end;

{ Tv3QSI }

Function Tv3QSI.AddComp(oComp: Tv3QSET) : Boolean;
begin
  Result := True;
  If Fterms = nil then
    FTerms := Tv3SetQSET.Create(FParamType,self);
  Fterms.add(oComp);
end;

Function Tv3QSI.RIMClassNameV : String;
Begin
  Result := 'QSI';
End;

procedure Tv3QSI.Assign(oSource: TFslObject);
begin
  inherited;
  terms := Tv3QSI(oSource).Fterms.Clone(self);

end;

function Tv3QSI.Clone(parent : Tv3Base): Tv3QSI;
begin
  Result := Tv3QSI(inherited Clone(parent));
end;


destructor Tv3QSI.Destroy;
begin
  Fterms.Free;
  inherited;
end;

Procedure Tv3QSI.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'terms', Fterms, rmpctSet, 'QSET'));
end;

function Tv3QSI.Link: Tv3QSI;
begin
  Result := Tv3QSI(Inherited Link);
end;

procedure Tv3QSI.Setterms(const Value: Tv3SetQSET);
begin
  Fterms.Free;
  Fterms := Value;
  if Fterms <> nil then  terms.parent := self;

end;

procedure Tv3QSI.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'terms' Then
    terms := Tv3SetQSET(avalue.AsType(Tv3SetQSET)).Clone(self)
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3QSI.Create(aParamType: Tv3QTYDatatype);
begin
  inherited;
  Fterms := Tv3SetQSET.Create(aParamType, self);
  FParamType := aParamType;
end;

procedure Tv3QSI.DoClear;
begin
  inherited;
  terms.ClearItems;
end;

function Tv3QSI.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fterms.sizeInBytes);
end;

{ Tv3QSD }

Function Tv3QSD.AddComp(oComp: Tv3QSET) : Boolean;
begin
  Result := True;
  if subtrahend = nil Then
    subtrahend := oComp
  Else if minuend = nil Then
    minuend := oComp
  Else
    Result := false;
end;

Function Tv3QSD.RIMClassNameV : String;
Begin
  Result := 'QSD';
End;

procedure Tv3QSD.Assign(oSource: TFslObject);
begin
  inherited;
  subtrahend := Tv3QSD(oSource).Fsubtrahend.Clone(self);
  minuend := Tv3QSD(oSource).Fminuend.Clone(self);
end;

function Tv3QSD.Clone(parent : Tv3Base): Tv3QSD;
begin
  Result := Tv3QSD(inherited Clone(parent));
end;


destructor Tv3QSD.Destroy;
begin
  Fsubtrahend.Free;
  Fminuend.Free;
  inherited;
end;

Procedure Tv3QSD.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'subtrahend', Fsubtrahend, 'QSET'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'minuend', Fminuend, 'QSET'));
end;

function Tv3QSD.Link: Tv3QSD;
begin
  Result := Tv3QSD(Inherited Link);
end;

procedure Tv3QSD.Setminuend(const Value: Tv3QSET);
begin
  Fminuend.Free;
  Fminuend := Value;
  if Fminuend <> nil then  minuend.parent := self;

end;

procedure Tv3QSD.Setsubtrahend(const Value: Tv3QSET);
begin
  Fsubtrahend.Free;
  Fsubtrahend := Value;
  if Fsubtrahend <> nil then  subtrahend.parent := self;

end;

procedure Tv3QSD.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'subtrahend' Then
    Fsubtrahend := Tv3QSET(aValue.asType(Tv3QSET)).Link
  else if aValue.Name = 'minuend' Then
    Fminuend := Tv3QSET(aValue.asType(Tv3QSET)).Link
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3QSD.Create(aParamType: Tv3QTYDatatype);
begin
  inherited;
  FParamType := aParamType;
end;

procedure Tv3QSD.DoClear;
begin
  inherited;
  subtrahend := nil;
  minuend := nil;
end;

function Tv3QSD.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fsubtrahend.sizeInBytes);
  inc(result, Fminuend.sizeInBytes);
end;

{ Tv3QSP }

function Tv3QSP.AddComp(oComp: Tv3QSET): Boolean;
begin
  Result := True;
  if high = nil Then
    high := oComp
  Else if low = nil Then
    low := oComp
  Else
    Result := false;
end;

Function Tv3QSP.RIMClassNameV : String;
Begin
  Result := 'QSP';
End;

procedure Tv3QSP.Assign(oSource: TFslObject);
begin
  inherited;
  high := Tv3QSP(oSource).Fhigh.Clone(self);
  low := Tv3QSP(oSource).Flow.Clone(self);
end;

function Tv3QSP.Clone(parent : Tv3Base): Tv3QSP;
begin
  Result := Tv3QSP(inherited Clone(parent));
end;


destructor Tv3QSP.Destroy;
begin
  Fhigh.Free;
  Flow.Free;
  inherited;
end;

Procedure Tv3QSP.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'high', Fhigh, 'QSET'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'low', Flow, 'QSET'));
end;

function Tv3QSP.Link: Tv3QSP;
begin
  Result := Tv3QSP(Inherited Link);
end;

procedure Tv3QSP.Sethigh(const Value: Tv3QSET);
begin
  Fhigh.Free;
  Fhigh := Value;
  if Fhigh <> nil then  high.parent := self;

end;

procedure Tv3QSP.Setlow(const Value: Tv3QSET);
begin
  Flow.Free;
  Flow := Value;
  if Flow <> nil then  low.parent := self;

end;

procedure Tv3QSP.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'high' Then
    Fhigh := Tv3QSET(aValue.asType(Tv3QSET)).Link
  else if aValue.Name = 'low' Then
    Flow := Tv3QSET(aValue.asType(Tv3QSET)).Link
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3QSP.Create(aParamType: Tv3QTYDatatype);
begin
  inherited;
  FParamType := aParamType;
end;

procedure Tv3QSP.DoClear;
begin
  inherited;
  high := nil;
  low := nil;
end;

function Tv3QSP.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fhigh.sizeInBytes);
  inc(result, Flow.sizeInBytes);
end;

{ Tv3QSS }

Function Tv3QSS.RIMClassNameV : String;
Begin
  Result := 'QSS';
End;

procedure Tv3QSS.Assign(oSource: TFslObject);
begin
  inherited;
  terms := Tv3QSS(oSource).Fterms.Clone(self);
end;

function Tv3QSS.Clone(parent : Tv3Base): Tv3QSS;
begin
  Result := Tv3QSS(inherited Clone(parent));
end;


destructor Tv3QSS.Destroy;
begin
  Fterms.Free;
  inherited;
end;

Procedure Tv3QSS.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'terms', Fterms, rmpctSet, 'QTY'));
end;

function Tv3QSS.Link: Tv3QSS;
begin
  Result := Tv3QSS(Inherited Link);
end;

procedure Tv3QSS.Setterms(const Value: Tv3SetQTY);
begin
  Fterms.Free;
  Fterms := Value;
  if Fterms <> nil then  terms.parent := self;

end;

procedure Tv3QSS.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'terms' Then
    terms := Tv3SetQTY(aValue.AsType(Tv3SetQTY)).Clone(self)
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3QSS.Create(aParamType: Tv3QTYDatatype);
begin
  inherited;
  Fterms := Tv3SetQTY.Create(self);
  FParamType := aParamType;
end;

procedure Tv3QSS.DoClear;
begin
  inherited;
  Terms.ClearItems;
end;

function Tv3QSS.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fterms.sizeInBytes);
end;

{ Tv3QSC }

Function Tv3QSC.RIMClassNameV : String;
Begin
  Result := 'QSC';
End;

procedure Tv3QSC.Assign(oSource: TFslObject);
begin
  inherited;
  code := Tv3QSC(oSource).Fcode.Clone(self);
end;

function Tv3QSC.Clone(parent : Tv3Base): Tv3QSC;
begin
  Result := Tv3QSC(inherited Clone(parent));
end;


destructor Tv3QSC.Destroy;
begin
  Fcode.Free;
  inherited;
end;

Procedure Tv3QSC.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'code', Fcode, 'CD'));
end;

function Tv3QSC.Link: Tv3QSC;
begin
  Result := Tv3QSC(Inherited Link);
end;

procedure Tv3QSC.Setcode(const Value: Tv3CD);
begin
  Fcode.Free;
  Fcode := Value;
  if Fcode <> nil then  code.parent := self;

end;

procedure Tv3QSC.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'code' Then
    Fcode := Tv3CD(aValue.AsType(Tv3CD)).Link
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3QSC.Create(aParamType: Tv3QTYDatatype);
begin
  inherited;
  FParamType := aParamType;
end;

procedure Tv3QSC.DoClear;
begin
  inherited;
  code := nil;
end;

function Tv3QSC.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcode.sizeInBytes);
end;

{ Tv3IVL }

Function Tv3IVL.RIMClassNameV : String;
Begin
  Result := 'IVL';
End;

procedure Tv3IVL.Assign(oSource: TFslObject);
begin
  inherited;
  lowClosed := Tv3IVL(oSource).FlowClosed;
  highClosed := Tv3IVL(oSource).FhighClosed;
  haslowClosed := Tv3IVL(oSource).FlowClosed;
  hashighClosed := Tv3IVL(oSource).FhighClosed;
  low := Tv3IVL(oSource).Flow.Clone(self);
  any := Tv3IVL(oSource).Fany.Clone(self);
  anyIsValue := Tv3IVL(oSource).anyIsValue;
  width := Tv3IVL(oSource).Fwidth.Clone(self);
  high := Tv3IVL(oSource).Fhigh.Clone(self);
end;

function Tv3IVL.Clone(parent : Tv3Base): Tv3IVL;
begin
  Result := Tv3IVL(inherited Clone(parent));
end;


destructor Tv3IVL.Destroy;
begin
  Flow.Free;
  Fany.Free;
  Fwidth.Free;
  Fhigh.Free;
  inherited;
end;

Procedure Tv3IVL.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'lowClosed', FHaslowClosed, FlowClosed));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'highClosed', FHashighClosed, FhighClosed));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'low', Flow, 'QTY'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'any', Fany, 'QTY'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'width', Fwidth, 'QTY'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'high', Fhigh, 'QTY'));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, true, 'anyIsValue', true, FanyIsValue));
end;

function Tv3IVL.Link: Tv3IVL;
begin
  Result := Tv3IVL(Inherited Link);
end;

procedure Tv3IVL.Setany(const Value: Tv3QTY);
begin
  Fany.Free;
  Fany := Value;
  if Fany <> nil then  any.parent := self;

end;

procedure Tv3IVL.Sethigh(const Value: Tv3QTY);
begin
  Fhigh.Free;
  Fhigh := Value;
  if Fhigh <> nil then  high.parent := self;

end;

procedure Tv3IVL.SethighClosed(const Value: boolean);
begin
  FHashighClosed := true;
  FhighClosed := Value;
end;

procedure Tv3IVL.Setlow(const Value: Tv3QTY);
begin
  Flow.Free;
  Flow := Value;
  if Flow <> nil then  low.parent := self;

end;

procedure Tv3IVL.SetlowClosed(const Value: boolean);
begin
  FHaslowClosed := True;
  FlowClosed := Value;
end;

procedure Tv3IVL.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'lowClosed' Then
    aValue.AsBool(FHaslowClosed, FlowClosed)
  else if aValue.Name = 'highClosed' Then
    aValue.AsBool(FHashighClosed, FhighClosed)
  else if aValue.Name = 'low' Then
    Flow := Tv3QTY(aValue.AsType(Tv3QTY)).Link
  else if aValue.Name = 'any' Then
    Fany := Tv3QTY(aValue.AsType(Tv3QTY)).Link
  else if aValue.Name = 'width' Then
    Fwidth := Tv3QTY(aValue.AsType(Tv3QTY)).Link
  else if aValue.Name = 'high' Then
    Fhigh := Tv3QTY(aValue.AsType(Tv3QTY)).Link
  else if aValue.Name = 'anyIsValue' Then
    FanyIsValue := StrToBoolDef(aValue.AsString, false)
  else
    Inherited SetPropertyValue(aValue);
end;

procedure Tv3IVL.Setwidth(const Value: Tv3QTY);
begin
  Fwidth.Free;
  Fwidth := Value;
  if Fwidth <> nil then  width.parent := self;

end;

constructor Tv3IVL.Create(aParamType: Tv3QTYDatatype);
begin
  inherited;
  FParamType := aParamType;
end;

Function BoundaryMatches(q1, q2 : Tv3QTY) : Boolean;
Begin
  result := (q1 <> nil) And (q2 <> nil);
  if result Then
    if q1.isNonNull and q2.isNonNull Then
      result := q1.equals(q2)
    Else
      result := ((q1.nullFlavor = nfNINF) And (q2.nullFlavor = nfNINF)) Or ((q1.nullFlavor = nfPINF) And (q2.nullFlavor = nfPINF));
End;

function Tv3IVL.EqualsV(oOther: Tv3Base): Boolean;
begin
  result := (oOther is Tv3IVL) and IsNonNull and (Tv3IVL(oOther).IsNonNull) And (ParamType = Tv3IVL(oOther).ParamType);
  if result Then
    result := (lowClosed = Tv3IVL(oOther).lowClosed) and (highClosed = Tv3IVL(oOther).highClosed) And BoundaryMatches(low, Tv3IVL(oOther).low) And BoundaryMatches(high, Tv3IVL(oOther).high);
end;

procedure Tv3IVL.DoClear;
begin
  inherited;
  HaslowClosed := false;
  lowClosed := false;
  HashighClosed := false;
  highClosed := false;
  low := Nil;
  any := Nil;
  width := nil;
  high := Nil;
  anyIsValue := false;
end;

function Tv3IVL.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Flow.sizeInBytes);
  inc(result, Fany.sizeInBytes);
  inc(result, Fwidth.sizeInBytes);
  inc(result, Fhigh.sizeInBytes);
end;

{ Tv3PIVL }

Function Tv3PIVL.RIMClassNameV : String;
Begin
  Result := 'PIVL';
End;

procedure Tv3PIVL.Assign(oSource: TFslObject);
begin
  inherited;
  isFlexible := Tv3PIVL(oSource).FisFlexible;
  HasisFlexible := Tv3PIVL(oSource).FHasisFlexible;
  alignment := Tv3PIVL(oSource).Falignment;
  count := Tv3PIVL(oSource).Fcount.Clone(self);
  phase := Tv3PIVL(oSource).Fphase.Clone(self);
  period := Tv3PIVL(oSource).Fperiod.Clone(self);
  frequency := Tv3PIVL(oSource).Ffrequency.Clone(self);
end;

function Tv3PIVL.Clone(parent : Tv3Base): Tv3PIVL;
begin
  Result := Tv3PIVL(inherited Clone(parent));
end;


destructor Tv3PIVL.Destroy;
begin
  Fcount.Free;
  Fphase.Free;
  Fperiod.Free;
  Ffrequency.Free;
  inherited;
end;

Procedure Tv3PIVL.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'alignment', Ord(Falignment), CODES_Tv3CalendarCycle));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'count', Fcount, 'INT'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'phase', Fphase, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'period', Fperiod, 'PQ'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'frequency', Ffrequency, 'RTO'));
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'isFlexible', FHasisFlexible, FisFlexible));
end;

function Tv3PIVL.Link: Tv3PIVL;
begin
  Result := Tv3PIVL(Inherited Link);
end;

procedure Tv3PIVL.Setcount(const Value: Tv3INT);
begin
  Fcount.Free;
  Fcount := Value;
  if Fcount <> nil then  count.parent := self;

end;

procedure Tv3PIVL.Setfrequency(const Value: Tv3RTO);
begin
  Ffrequency.Free;
  Ffrequency := Value;
  if Ffrequency <> nil then  frequency.parent := self;

end;

procedure Tv3PIVL.SetisFlexible(const Value: boolean);
begin
  FHasisFlexible := True;
  FisFlexible := Value;
end;

procedure Tv3PIVL.Setperiod(const Value: Tv3PQ);
begin
  Fperiod.Free;
  Fperiod := Value;
  if Fperiod <> nil then  period.parent := self;

end;

procedure Tv3PIVL.Setphase(const Value: Tv3IVL);
begin
  Fphase.Free;
  Fphase := Value;
  if Fphase <> nil then  phase.parent := self;

end;

procedure Tv3PIVL.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'alignment' Then
    Falignment := Tv3CalendarCycle(aValue.AsEnum)
  else if aValue.Name = 'count' Then
    Fcount := Tv3INT(aValue.AsType(Tv3INT)).Link
  else if aValue.Name = 'phase' Then
    Fphase := Tv3IVL(aValue.AsType(Tv3IVL)).Link
  else if aValue.Name = 'period' Then
    Fperiod := Tv3PQ(aValue.AsType(Tv3PQ)).Link
  else if aValue.Name = 'frequency' Then
    Ffrequency := Tv3RTO(aValue.AsType(Tv3RTO)).Link
  else if aValue.Name = 'isFlexible' Then
    aValue.AsBool(FHasisFlexible, FisFlexible)
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3PIVL.Create(aParamType: Tv3QTYDatatype);
begin
  inherited;
  FParamType := aParamType;
end;

procedure Tv3PIVL.DoClear;
begin
  inherited;
  isFlexible := false;
  alignment := ccNull;
  count := nil;
  phase := nil;
  period := nil;
  frequency := Nil;
  HasisFlexible := False;
end;

function Tv3PIVL.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fcount.sizeInBytes);
  inc(result, Fphase.sizeInBytes);
  inc(result, Fperiod.sizeInBytes);
  inc(result, Ffrequency.sizeInBytes);
end;

{ Tv3EIVL }

Function Tv3EIVL.RIMClassNameV : String;
Begin
  Result := 'EIVL';
End;

procedure Tv3EIVL.Assign(oSource: TFslObject);
begin
  inherited;
  offset := Tv3EIVL(oSource).Foffset.Clone(self);
  event := Tv3EIVL(oSource).Fevent;
end;

function Tv3EIVL.Clone(parent : Tv3Base): Tv3EIVL;
begin
  Result := Tv3EIVL(inherited Clone(parent));
end;


destructor Tv3EIVL.Destroy;
begin
  Foffset.Free;
  inherited;
end;

Procedure Tv3EIVL.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'offset', Foffset, 'IVL'));
  oList.Add(Tv3PropertyDefinition.CreateEnum(self, false, 'event', Ord(Fevent), CODES_Tv3TimingEvent));
end;

function Tv3EIVL.Link: Tv3EIVL;
begin
  Result := Tv3EIVL(Inherited Link);
end;

procedure Tv3EIVL.Setoffset(const Value: Tv3IVL);
begin
  Foffset.Free;
  Foffset := Value;
  if Foffset <> nil then  offset.parent := self;

end;

procedure Tv3EIVL.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'offset' Then
    Foffset := Tv3IVL(aValue.AsType(Tv3IVL)).Link
  else if aValue.Name = 'event' Then
    Fevent := Tv3TimingEvent(aValue.AsEnum)
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3EIVL.Create(aParamType: Tv3QTYDatatype);
begin
  inherited;
  FParamType := aParamType;
end;

procedure Tv3EIVL.DoClear;
begin
  inherited;
  offset := nil;
  event := teNull;
end;

function Tv3EIVL.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Foffset.sizeInBytes);
end;

{ Tv3UVP }

Function Tv3UVP.RIMClassNameV : String;
Begin
  Result := 'UVP';
End;

procedure Tv3UVP.Assign(oSource: TFslObject);
begin
  inherited;
  probability := Tv3UVP(oSource).Fprobability.Clone;
  value := Tv3UVP(oSource).Fvalue.Clone(self);
end;

function Tv3UVP.Clone(parent : Tv3Base): Tv3UVP;
begin
  Result := Tv3UVP(inherited Clone(parent));
end;


destructor Tv3UVP.Destroy;
begin
  Fvalue.Free;
  inherited;
end;

Procedure Tv3UVP.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateDecimal(self, false, 'probability', Fprobability));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'value', Fvalue, 'ANY'));
end;

function Tv3UVP.Link: Tv3UVP;
begin
  Result := Tv3UVP(Inherited Link);
end;

procedure Tv3UVP.Setvalue(const Value: Tv3ANY);
begin
  Fvalue.Free;
  Fvalue := Value;
  if Fvalue <> nil then  value.parent := self;

end;

procedure Tv3UVP.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'probability' Then
    Fprobability := TFslDecimal.Create(aValue.AsString)
  else if aValue.Name = 'value' Then
    Fvalue := Tv3ANY(aValue.AsType(Tv3ANY)).Link
  else
    Inherited SetPropertyValue(aValue);
end;

procedure Tv3UVP.Setprobability(const Value: TFslDecimal);
begin
  Fprobability := Value;
end;

procedure Tv3UVP.DoClear;
begin
  inherited;
  probability := TFslDecimal.makeNull;
  value := nil;
end;

function Tv3UVP.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fvalue.sizeInBytes);
end;

{ Tv3NPPD }

Function Tv3NPPD.RIMClassNameV : String;
Begin
  Result := 'NPPD';
End;

procedure Tv3NPPD.Assign(oSource: TFslObject);
begin
  inherited;
  item := Tv3NPPD(oSource).Fitem.Clone(self);
end;

function Tv3NPPD.Clone(parent : Tv3Base): Tv3NPPD;
begin
  Result := Tv3NPPD(inherited Clone(parent));
end;


destructor Tv3NPPD.Destroy;
begin
  Fitem.Free;
  inherited;
end;

Procedure Tv3NPPD.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'item', Fitem, rmpctSet, 'ANY'));
end;

function Tv3NPPD.Link: Tv3NPPD;
begin
  Result := Tv3NPPD(Inherited Link);
end;

procedure Tv3NPPD.Setitem(const Value: Tv3SetUVP);
begin
  Fitem.Free;
  Fitem := Value;
  if Fitem <> nil then  item.parent := self;

end;

procedure Tv3NPPD.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'item' Then
    item := Tv3SetUVP(aValue.AsType(Tv3SetUVP)).Clone(self)
  else
    Inherited SetPropertyValue(aValue);
end;

constructor Tv3NPPD.Create;
begin
  Inherited;
  Fitem := Tv3SetUVP.Create(self);
end;

procedure Tv3NPPD.DoClear;
begin
  inherited;
  item := nil;
end;

function Tv3NPPD.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fitem.sizeInBytes);
end;

{ Tv3SetED }

function Tv3SetED.Clone(parent : Tv3Base): Tv3SetED;
begin
  Result := Tv3SetED(inherited Clone(parent));
end;

function Tv3SetED.Link: Tv3SetED;
begin
  Result := Tv3SetED(Inherited Link);
end;

Function Tv3SetED.GetEDs(iIndex : Integer) : Tv3ED;
Begin
  Result := Tv3ED(ObjectByIndex[iIndex]);
End;

function Tv3SetED.Add: Tv3ED;
begin
  Result := Tv3ED.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3SetED.Item(iIndex: Integer): Tv3ED;
begin
  Result := EDs[iIndex];
end;

function Tv3SetED.IndexOf(value : Tv3ED): Integer;
Begin
  result := IndexByReference(value);
End;

function Tv3SetED.AddItem(value : Tv3ED): Boolean;
begin
  Result := IndexOf(value) = -1;
  if Result Then
    Add(value)
  Else
    value.Free;
end;

Procedure Tv3SetED.SetItemByIndex(iIndex: Integer; value: Tv3ED);
begin
  EDs[iIndex] := value;
end;

procedure Tv3SetED.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3SetED.ClearItems;
begin
  Clear;
end;

function Tv3SetED.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3SetED.SetEDs(iIndex: Integer; const Value: Tv3ED);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3SetED.ItemClass: TFslObjectClass;
begin
  result := Tv3ED;
end;

{ Tv3SetST }

function Tv3SetST.Clone(parent : Tv3Base): Tv3SetST;
begin
  Result := Tv3SetST(inherited Clone(parent));
end;

function Tv3SetST.Link: Tv3SetST;
begin
  Result := Tv3SetST(Inherited Link);
end;

Function Tv3SetST.GetSTs(iIndex : Integer) : Tv3ST;
Begin
  Result := Tv3ST(ObjectByIndex[iIndex]);
End;

function Tv3SetST.Add: Tv3ST;
begin
  Result := Tv3ST.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3SetST.Item(iIndex: Integer): Tv3ST;
begin
  Result := STs[iIndex];
end;

function Tv3SetST.IndexOf(value : Tv3ST): Integer;
Begin
  result := IndexByReference(value);
End;

function Tv3SetST.AddItem(value : Tv3ST): Boolean;
begin
  Result := IndexOf(value) = -1;
  if Result Then
    Add(value)
  Else
    value.Free;
end;

Procedure Tv3SetST.SetItemByIndex(iIndex: Integer; value: Tv3ST);
begin
  STs[iIndex] := value;
end;

procedure Tv3SetST.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3SetST.ClearItems;
begin
  Clear;
end;

function Tv3SetST.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3SetST.SetSTs(iIndex: Integer; const Value: Tv3ST);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3SetST.ItemClass: TFslObjectClass;
begin
  result := Tv3ST;
end;

{ Tv3SetCD }

function Tv3SetCD.Clone(parent : Tv3Base): Tv3SetCD;
begin
  Result := Tv3SetCD(inherited Clone(parent));
end;

function Tv3SetCD.Link: Tv3SetCD;
begin
  Result := Tv3SetCD(Inherited Link);
end;

Function Tv3SetCD.GetCDs(iIndex : Integer) : Tv3CD;
Begin
  Result := Tv3CD(ObjectByIndex[iIndex]);
End;

function Tv3SetCD.Add: Tv3CD;
begin
  Result := Tv3CD.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3SetCD.Item(iIndex: Integer): Tv3CD;
begin
  Result := CDs[iIndex];
end;

function Tv3SetCD.IndexOf(value : Tv3CD): Integer;
Begin
  result := IndexByReference(value);
End;

function Tv3SetCD.AddItem(value : Tv3CD): Boolean;
begin
  Result := IndexOf(value) = -1;
  if Result Then
    Add(value)
  Else
    value.Free;
end;

Procedure Tv3SetCD.SetItemByIndex(iIndex: Integer; value: Tv3CD);
begin
  CDs[iIndex] := value;
end;

procedure Tv3SetCD.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3SetCD.ClearItems;
begin
  Clear;
end;

function Tv3SetCD.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3SetCD.SetCDs(iIndex: Integer; const Value: Tv3CD);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3SetCD.ItemClass: TFslObjectClass;
begin
  result := Tv3CD;
end;

{ Tv3ListADXP }

function Tv3ListADXP.Clone(parent : Tv3Base): Tv3ListADXP;
begin
  Result := Tv3ListADXP(inherited Clone(parent));
end;

function Tv3ListADXP.Link: Tv3ListADXP;
begin
  Result := Tv3ListADXP(Inherited Link);
end;

Function Tv3ListADXP.GetADXPs(iIndex : Integer) : Tv3ADXP;
Begin
  Result := Tv3ADXP(ObjectByIndex[iIndex]);
End;

function Tv3ListADXP.Append: Tv3ADXP;
begin
  Result := Tv3ADXP.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListADXP.Insert(iIndex: Integer): Tv3ADXP;
begin
  Result := Tv3ADXP.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListADXP.InsertItem(iIndex: Integer; value: Tv3ADXP);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListADXP.Item(iIndex: Integer): Tv3ADXP;
begin
  Result := ADXPs[iIndex];
end;

function Tv3ListADXP.IndexOf(value : Tv3ADXP): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListADXP.AddItem(value : Tv3ADXP);
begin
  Add(value);
end;

Procedure Tv3ListADXP.SetItemByIndex(iIndex: Integer; value: Tv3ADXP);
begin
  ADXPs[iIndex] := value;
end;

procedure Tv3ListADXP.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListADXP.ClearItems;
begin
  Clear;
end;

function Tv3ListADXP.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListADXP.SetADXPs(iIndex: Integer; const Value: Tv3ADXP);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListADXP.ItemClass: TFslObjectClass;
begin
  result := Tv3ADXP;
end;

{ Tv3ListENXP }

function Tv3ListENXP.Clone(parent : Tv3Base): Tv3ListENXP;
begin
  Result := Tv3ListENXP(inherited Clone(parent));
end;

function Tv3ListENXP.Link: Tv3ListENXP;
begin
  Result := Tv3ListENXP(Inherited Link);
end;

Function Tv3ListENXP.GetENXPs(iIndex : Integer) : Tv3ENXP;
Begin
  Result := Tv3ENXP(ObjectByIndex[iIndex]);
End;

function Tv3ListENXP.Append: Tv3ENXP;
begin
  Result := Tv3ENXP.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListENXP.Insert(iIndex: Integer): Tv3ENXP;
begin
  Result := Tv3ENXP.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListENXP.InsertItem(iIndex: Integer; value: Tv3ENXP);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListENXP.Item(iIndex: Integer): Tv3ENXP;
begin
  Result := ENXPs[iIndex];
end;

function Tv3ListENXP.IndexOf(value : Tv3ENXP): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListENXP.AddItem(value : Tv3ENXP);
begin
  Add(value);
end;

Procedure Tv3ListENXP.SetItemByIndex(iIndex: Integer; value: Tv3ENXP);
begin
  ENXPs[iIndex] := value;
end;

procedure Tv3ListENXP.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListENXP.ClearItems;
begin
  Clear;
end;

function Tv3ListENXP.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListENXP.SetENXPs(iIndex: Integer; const Value: Tv3ENXP);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListENXP.ItemClass: TFslObjectClass;
begin
  result := Tv3ENXP;
end;

{ Tv3SetPQR }

function Tv3SetPQR.Clone(parent : Tv3Base): Tv3SetPQR;
begin
  Result := Tv3SetPQR(inherited Clone(parent));
end;

function Tv3SetPQR.Link: Tv3SetPQR;
begin
  Result := Tv3SetPQR(Inherited Link);
end;

Function Tv3SetPQR.GetPQRs(iIndex : Integer) : Tv3PQR;
Begin
  Result := Tv3PQR(ObjectByIndex[iIndex]);
End;


function Tv3SetPQR.Add: Tv3PQR;
begin
  Result := Tv3PQR.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3SetPQR.Item(iIndex: Integer): Tv3PQR;
begin
  Result := PQRs[iIndex];
end;

function Tv3SetPQR.IndexOf(value : Tv3PQR): Integer;
Begin
  result := IndexByReference(value);
End;

function Tv3SetPQR.AddItem(value : Tv3PQR): Boolean;
begin
  Result := IndexOf(value) = -1;
  if Result Then
    Add(value)
  Else
    value.Free;
end;

Procedure Tv3SetPQR.SetItemByIndex(iIndex: Integer; value: Tv3PQR);
begin
  PQRs[iIndex] := value;
end;

procedure Tv3SetPQR.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3SetPQR.ClearItems;
begin
  Clear;
end;

function Tv3SetPQR.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3SetPQR.SetPQRs(iIndex: Integer; const Value: Tv3PQR);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3SetPQR.ItemClass: TFslObjectClass;
begin
  result := Tv3PQR;
end;

{ Tv3SetQSET }

function Tv3SetQSET.Clone(parent : Tv3Base): Tv3SetQSET;
begin
  Result := Tv3SetQSET(inherited Clone(parent));
end;

function Tv3SetQSET.Link: Tv3SetQSET;
begin
  Result := Tv3SetQSET(Inherited Link);
end;

Function Tv3SetQSET.GetQSETs(iIndex : Integer) : Tv3QSET;
Begin
  Result := Tv3QSET(ObjectByIndex[iIndex]);
End;

function Tv3SetQSET.Add: Tv3QSET;
begin
  Result := Tv3QSET.Create(FParamType);
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3SetQSET.Item(iIndex: Integer): Tv3QSET;
begin
  Result := QSETs[iIndex];
end;

function Tv3SetQSET.IndexOf(value : Tv3QSET): Integer;
Begin
  result := IndexByReference(value);
End;

function Tv3SetQSET.AddItem(value : Tv3QSET): Boolean;
begin
  Result := IndexOf(value) = -1;
  if Result Then
    Add(value)
  Else
    value.Free;
end;

Procedure Tv3SetQSET.SetItemByIndex(iIndex: Integer; value: Tv3QSET);
begin
  QSETs[iIndex] := value;
end;

procedure Tv3SetQSET.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3SetQSET.ClearItems;
begin
  Clear;
end;

function Tv3SetQSET.Count: Integer;
begin
  result := Inherited Count;
end;

constructor Tv3SetQSET.Create(aParamType: Tv3QTYDatatype; Parent : Tv3Base);
begin
  Inherited Create(Parent);
  FParamType := aParamType;
end;

procedure Tv3SetQSET.SetQSETs(iIndex: Integer; const Value: Tv3QSET);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3SetQSET.ItemClass: TFslObjectClass;
begin
  result := Tv3QSET;
end;

{ Tv3SetQTY }

function Tv3SetQTY.Clone(parent : Tv3Base): Tv3SetQTY;
begin
  Result := Tv3SetQTY(inherited Clone(parent));
end;

function Tv3SetQTY.Link: Tv3SetQTY;
begin
  Result := Tv3SetQTY(Inherited Link);
end;

Function Tv3SetQTY.GetQTYs(iIndex : Integer) : Tv3QTY;
Begin
  Result := Tv3QTY(ObjectByIndex[iIndex]);
End;

function Tv3SetQTY.Add: Tv3QTY;
begin
  Result := Tv3QTY.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3SetQTY.Item(iIndex: Integer): Tv3QTY;
begin
  Result := QTYs[iIndex];
end;

function Tv3SetQTY.IndexOf(value : Tv3QTY): Integer;
Begin
  result := IndexByReference(value);
End;

function Tv3SetQTY.AddItem(value : Tv3QTY): Boolean;
begin
  Result := IndexOf(value) = -1;
  if Result Then
    Add(value)
  Else
    value.Free;
end;

Procedure Tv3SetQTY.SetItemByIndex(iIndex: Integer; value: Tv3QTY);
begin
  QTYs[iIndex] := value;
end;

procedure Tv3SetQTY.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3SetQTY.ClearItems;
begin
  Clear;
end;

function Tv3SetQTY.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3SetQTY.SetQTYs(iIndex: Integer; const Value: Tv3QTY);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3SetQTY.ItemClass: TFslObjectClass;
begin
  result := Tv3QTY;
end;

{ Tv3SetUVP }

function Tv3SetUVP.Clone(parent : Tv3Base): Tv3SetUVP;
begin
  Result := Tv3SetUVP(inherited Clone(parent));
end;

function Tv3SetUVP.Link: Tv3SetUVP;
begin
  Result := Tv3SetUVP(Inherited Link);
end;

Function Tv3SetUVP.GetUVPs(iIndex : Integer) : Tv3UVP;
Begin
  Result := Tv3UVP(ObjectByIndex[iIndex]);
End;



function Tv3SetUVP.Add: Tv3UVP;
begin
  Result := Tv3UVP.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3SetUVP.Item(iIndex: Integer): Tv3UVP;
begin
  Result := UVPs[iIndex];
end;

function Tv3SetUVP.IndexOf(value : Tv3UVP): Integer;
Begin
  result := IndexByReference(value);
End;

function Tv3SetUVP.AddItem(value : Tv3UVP): Boolean;
begin
  Result := IndexOf(value) = -1;
  if Result Then
    Add(value)
  Else
    value.Free;
end;

Procedure Tv3SetUVP.SetItemByIndex(iIndex: Integer; value: Tv3UVP);
begin
  UVPs[iIndex] := value;
end;

procedure Tv3SetUVP.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3SetUVP.ClearItems;
begin
  Clear;
end;

function Tv3SetUVP.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3SetUVP.SetUVPs(iIndex: Integer; const Value: Tv3UVP);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3SetUVP.ItemClass: TFslObjectClass;
begin
  result := Tv3UVP;
end;

{ Tv3ListCS }
Function Tv3ListCS.Link : Tv3ListCS;
Begin
  Result := Tv3ListCS(Inherited Link);
End;

Function Tv3ListCS.Clone(parent : Tv3Base): Tv3ListCS;
Begin
  Result := Tv3ListCS(inherited Clone(parent));
End;


Function Tv3ListCS.GetCSs(iIndex : Integer) : Tv3CS;
Begin
  Result := Tv3CS(ObjectByIndex[iIndex]);
End;
function Tv3ListCS.Append: Tv3CS;
begin
  Result := Tv3CS.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListCS.Insert(iIndex: Integer): Tv3CS;
begin
  Result := Tv3CS.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListCS.InsertItem(iIndex: Integer; value: Tv3CS);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListCS.Item(iIndex: Integer): Tv3CS;
begin
  Result := CSs[iIndex];
end;

function Tv3ListCS.IndexOf(value : Tv3CS): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListCS.AddItem(value : Tv3CS);
begin
  Add(value);
end;

Procedure Tv3ListCS.SetItemByIndex(iIndex: Integer; value: Tv3CS);
begin
  CSs[iIndex] := value;
end;

procedure Tv3ListCS.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListCS.ClearItems;
begin
  Clear;
end;

function Tv3ListCS.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListCS.SetCSs(iIndex: Integer; const Value: Tv3CS);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListCS.ItemClass: TFslObjectClass;
begin
  result := Tv3CS;
end;

{ Tv3ListII }
Function Tv3ListII.Link : Tv3ListII;
Begin
  Result := Tv3ListII(Inherited Link);
End;

Function Tv3ListII.Clone(parent : Tv3Base): Tv3ListII;
Begin
  Result := Tv3ListII(inherited Clone(parent));
End;


Function Tv3ListII.GetIIs(iIndex : Integer) : Tv3II;
Begin
  Result := Tv3II(ObjectByIndex[iIndex]);
End;

function Tv3ListII.Append: Tv3II;
begin
  Result := Tv3II.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListII.Insert(iIndex: Integer): Tv3II;
begin
  Result := Tv3II.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListII.InsertItem(iIndex: Integer; value: Tv3II);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListII.Item(iIndex: Integer): Tv3II;
begin
  Result := IIs[iIndex];
end;

function Tv3ListII.IndexOf(value : Tv3II): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListII.AddItem(value : Tv3II);
begin
  Add(value);
end;

Procedure Tv3ListII.SetItemByIndex(iIndex: Integer; value: Tv3II);
begin
  IIs[iIndex] := value;
end;

procedure Tv3ListII.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListII.ClearItems;
begin
  Clear;
end;

function Tv3ListII.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListII.SetIIs(iIndex: Integer; const Value: Tv3II);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListII.ItemClass: TFslObjectClass;
begin
  result := Tv3II;
end;

{ Tv3ListAD }
Function Tv3ListAD.Link : Tv3ListAD;
Begin
  Result := Tv3ListAD(Inherited Link);
End;

Function Tv3ListAD.Clone(parent : Tv3Base): Tv3ListAD;
Begin
  Result := Tv3ListAD(inherited Clone(parent));
End;


Function Tv3ListAD.GetADs(iIndex : Integer) : Tv3AD;
Begin
  Result := Tv3AD(ObjectByIndex[iIndex]);
End;
function Tv3ListAD.Append: Tv3AD;
begin
  Result := Tv3AD.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListAD.Insert(iIndex: Integer): Tv3AD;
begin
  Result := Tv3AD.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListAD.InsertItem(iIndex: Integer; value: Tv3AD);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListAD.Item(iIndex: Integer): Tv3AD;
begin
  Result := ADs[iIndex];
end;

function Tv3ListAD.IndexOf(value : Tv3AD): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListAD.AddItem(value : Tv3AD);
begin
  Add(value);
end;

Procedure Tv3ListAD.SetItemByIndex(iIndex: Integer; value: Tv3AD);
begin
  ADs[iIndex] := value;
end;

procedure Tv3ListAD.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListAD.ClearItems;
begin
  Clear;
end;

function Tv3ListAD.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListAD.SetADs(iIndex: Integer; const Value: Tv3AD);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListAD.ItemClass: TFslObjectClass;
begin
  result := Tv3AD;
end;

{ Tv3ListTEL }
Function Tv3ListTEL.Link : Tv3ListTEL;
Begin
  Result := Tv3ListTEL(Inherited Link);
End;

Function Tv3ListTEL.Clone(parent : Tv3Base): Tv3ListTEL;
Begin
  Result := Tv3ListTEL(inherited Clone(parent));
End;


Function Tv3ListTEL.GetTELs(iIndex : Integer) : Tv3TEL;
Begin
  Result := Tv3TEL(ObjectByIndex[iIndex]);
End;
function Tv3ListTEL.Append: Tv3TEL;
begin
  Result := Tv3TEL.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListTEL.Insert(iIndex: Integer): Tv3TEL;
begin
  Result := Tv3TEL.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListTEL.InsertItem(iIndex: Integer; value: Tv3TEL);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListTEL.Item(iIndex: Integer): Tv3TEL;
begin
  Result := TELs[iIndex];
end;

function Tv3ListTEL.IndexOf(value : Tv3TEL): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListTEL.AddItem(value : Tv3TEL);
begin
  Add(value);
end;

Procedure Tv3ListTEL.SetItemByIndex(iIndex: Integer; value: Tv3TEL);
begin
  TELs[iIndex] := value;
end;

procedure Tv3ListTEL.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListTEL.ClearItems;
begin
  Clear;
end;

function Tv3ListTEL.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListTEL.SetTELs(iIndex: Integer; const Value: Tv3TEL);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListTEL.ItemClass: TFslObjectClass;
begin
  result := Tv3TEL;
end;

{ Tv3ListANY }
Function Tv3ListANY.Link : Tv3ListANY;
Begin
  Result := Tv3ListANY(Inherited Link);
End;

Function Tv3ListANY.Clone(parent : Tv3Base): Tv3ListANY;
Begin
  Result := Tv3ListANY(inherited Clone(parent));
End;


Function Tv3ListANY.GetANYs(iIndex : Integer) : Tv3ANY;
Begin
  Result := Tv3ANY(ObjectByIndex[iIndex]);
End;
function Tv3ListANY.Append: Tv3ANY;
begin
  Result := Tv3ANY.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListANY.Insert(iIndex: Integer): Tv3ANY;
begin
  Result := Tv3ANY.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListANY.InsertItem(iIndex: Integer; value: Tv3ANY);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListANY.Item(iIndex: Integer): Tv3ANY;
begin
  Result := ANYs[iIndex];
end;

function Tv3ListANY.IndexOf(value : Tv3ANY): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListANY.AddItem(value : Tv3ANY);
begin
  Add(value);
end;

Procedure Tv3ListANY.SetItemByIndex(iIndex: Integer; value: Tv3ANY);
begin
  ANYs[iIndex] := value;
end;

procedure Tv3ListANY.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListANY.ClearItems;
begin
  Clear;
end;

function Tv3ListANY.Count: Integer;
begin
  result := Inherited Count;

end;

procedure Tv3ListANY.SetANYs(iIndex: Integer; const Value: Tv3ANY);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListANY.ItemClass: TFslObjectClass;
begin
  result := Tv3ANY;
end;

{ Tv3ListCD }
Function Tv3ListCD.Link : Tv3ListCD;
Begin
  Result := Tv3ListCD(Inherited Link);
End;

Function Tv3ListCD.Clone(parent : Tv3Base): Tv3ListCD;
Begin
  Result := Tv3ListCD(inherited Clone(parent));
End;


Function Tv3ListCD.GetCDs(iIndex : Integer) : Tv3CD;
Begin
  Result := Tv3CD(ObjectByIndex[iIndex]);
End;
function Tv3ListCD.Append: Tv3CD;
begin
  Result := Tv3CD.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListCD.Insert(iIndex: Integer): Tv3CD;
begin
  Result := Tv3CD.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListCD.InsertItem(iIndex: Integer; value: Tv3CD);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListCD.Item(iIndex: Integer): Tv3CD;
begin
  Result := CDs[iIndex];
end;

function Tv3ListCD.IndexOf(value : Tv3CD): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListCD.AddItem(value : Tv3CD);
begin
  Add(value);
end;

Procedure Tv3ListCD.SetItemByIndex(iIndex: Integer; value: Tv3CD);
begin
  CDs[iIndex] := value;
end;

procedure Tv3ListCD.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListCD.ClearItems;
begin
  Clear;
end;

function Tv3ListCD.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListCD.SetCDs(iIndex: Integer; const Value: Tv3CD);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListCD.ItemClass: TFslObjectClass;
begin
  result := Tv3CD;
end;

{ Tv3ListEN }
Function Tv3ListEN.Link : Tv3ListEN;
Begin
  Result := Tv3ListEN(Inherited Link);
End;

Function Tv3ListEN.Clone(parent : Tv3Base): Tv3ListEN;
Begin
  Result := Tv3ListEN(inherited Clone(parent));
End;


Function Tv3ListEN.GetENs(iIndex : Integer) : Tv3EN;
Begin
  Result := Tv3EN(ObjectByIndex[iIndex]);
End;
function Tv3ListEN.Append: Tv3EN;
begin
  Result := Tv3EN.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListEN.Insert(iIndex: Integer): Tv3EN;
begin
  Result := Tv3EN.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListEN.InsertItem(iIndex: Integer; value: Tv3EN);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListEN.Item(iIndex: Integer): Tv3EN;
begin
  Result := ENs[iIndex];
end;

function Tv3ListEN.IndexOf(value : Tv3EN): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListEN.AddItem(value : Tv3EN);
begin
  Add(value);
end;

Procedure Tv3ListEN.SetItemByIndex(iIndex: Integer; value: Tv3EN);
begin
  ENs[iIndex] := value;
end;

procedure Tv3ListEN.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListEN.ClearItems;
begin
  Clear;
end;

function Tv3ListEN.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListEN.SetENs(iIndex: Integer; const Value: Tv3EN);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListEN.ItemClass: TFslObjectClass;
begin
  result := Tv3EN;
end;

{ Tv3ListCR }
Function Tv3ListCR.Link : Tv3ListCR;
Begin
  Result := Tv3ListCR(Inherited Link);
End;

Function Tv3ListCR.Clone(parent : Tv3Base): Tv3ListCR;
Begin
  Result := Tv3ListCR(inherited Clone(parent));
End;


Function Tv3ListCR.GetCRs(iIndex : Integer) : Tv3CR;
Begin
  Result := Tv3CR(ObjectByIndex[iIndex]);
End;
function Tv3ListCR.Append: Tv3CR;
begin
  Result := Tv3CR.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListCR.Insert(iIndex: Integer): Tv3CR;
begin
  Result := Tv3CR.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListCR.InsertItem(iIndex: Integer; value: Tv3CR);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListCR.Item(iIndex: Integer): Tv3CR;
begin
  Result := CRs[iIndex];
end;

function Tv3ListCR.IndexOf(value : Tv3CR): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListCR.AddItem(value : Tv3CR);
begin
  Add(value);
end;

Procedure Tv3ListCR.SetItemByIndex(iIndex: Integer; value: Tv3CR);
begin
  CRs[iIndex] := value;
end;

procedure Tv3ListCR.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListCR.ClearItems;
begin
  Clear;
end;

function Tv3ListCR.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListCR.SetCRs(iIndex: Integer; const Value: Tv3CR);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListCR.ItemClass: TFslObjectClass;
begin
  result := Tv3CR;
end;

{ Tv3ListPQ }
Function Tv3ListPQ.Link : Tv3ListPQ;
Begin
  Result := Tv3ListPQ(Inherited Link);
End;

Function Tv3ListPQ.Clone(parent : Tv3Base): Tv3ListPQ;
Begin
  Result := Tv3ListPQ(inherited Clone(parent));
End;


Function Tv3ListPQ.GetPQs(iIndex : Integer) : Tv3PQ;
Begin
  Result := Tv3PQ(ObjectByIndex[iIndex]);
End;


function Tv3ListPQ.Append: Tv3PQ;
begin
  Result := Tv3PQ.Create;
  Try
    Add(Result.Link);
  Finally
    Result.Free;
  End;
end;

function Tv3ListPQ.Insert(iIndex: Integer): Tv3PQ;
begin
  Result := Tv3PQ.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
end;

Procedure Tv3ListPQ.InsertItem(iIndex: Integer; value: Tv3PQ);
begin
  Inherited Insert(iIndex, value);
end;

function Tv3ListPQ.Item(iIndex: Integer): Tv3PQ;
begin
  Result := PQs[iIndex];
end;

function Tv3ListPQ.IndexOf(value : Tv3PQ): Integer;
Begin
  result := IndexByReference(value);
End;

Procedure Tv3ListPQ.AddItem(value : Tv3PQ);
begin
  Add(value);
end;

Procedure Tv3ListPQ.SetItemByIndex(iIndex: Integer; value: Tv3PQ);
begin
  PQs[iIndex] := value;
end;

procedure Tv3ListPQ.Remove(iIndex: Integer);
begin
  DeleteByIndex(iIndex);
end;

procedure Tv3ListPQ.ClearItems;
begin
  Clear;
end;

function Tv3ListPQ.Count: Integer;
begin
  result := Inherited Count;
end;

procedure Tv3ListPQ.SetPQs(iIndex: Integer; const Value: Tv3PQ);
begin
  ObjectByIndex[iIndex] := Value;
end;

function Tv3ListPQ.ItemClass: TFslObjectClass;
begin
  result := Tv3PQ;
end;

{ Tv3CR }

Function Tv3CR.RIMClassNameV : String;
Begin
  Result := 'CR';
End;

procedure Tv3CR.Assign(oSource: TFslObject);
begin
  inherited;
  name := Tv3CR(oSource).name.Clone(self);
  value := Tv3CR(oSource).value.Clone(self);
  inverted := Tv3CR(oSource).inverted;
  Hasinverted := Tv3CR(oSource).Hasinverted;
end;

function Tv3CR.Clone(parent : Tv3Base): Tv3CR;
begin
  result := Tv3CR(inherited Clone(parent));
end;

destructor Tv3CR.Destroy;
begin
  Fname.Free;
  Fvalue.Free;
  inherited;
end;

Procedure Tv3CR.ListProperties(oList : Tv3PropertyDefinitionList; bInheritedProperties : Boolean);
begin
  if (bInheritedProperties) Then
    Inherited ListProperties(oList, bInheritedProperties);
  oList.Add(Tv3PropertyDefinition.CreateBoolean(self, false, 'inverted', FHasinverted, Finverted));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'value', Fvalue, 'CD'));
  oList.Add(Tv3PropertyDefinition.CreateClass(self, false, 'name', Fname, 'CD'));
end;

function Tv3CR.Link: Tv3CR;
begin
  result := Tv3CR(Inherited Link);
end;

procedure Tv3CR.Setinverted(const Value: boolean);
begin
  FHasinverted := true;
  Finverted := Value;
end;

procedure Tv3CR.Setname(const Value: Tv3CD);
begin
  Fname.Free;
  Fname := Value;
end;

procedure Tv3CR.Setvalue(const Value: Tv3CD);
begin
  Fvalue.Free;
  Fvalue := Value;
end;

function Tv3CR.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, Fvalue.sizeInBytes);
  inc(result, Fname.sizeInBytes);
end;

{ Tv3PropertyDefinition }

(*procedure Tv3PropertyDefinition.SetCollection(const Value: Tv3ListANY);
begin
  FCollection := Value;
  FFocus.SetProperty(FName, Value);
end;

procedure Tv3PropertyDefinition.SetValue(const Value: Tv3ANY);
begin
  FValue := Value;
  FFocus.SetProperty(FName, Value);
end;
 *)

procedure Tv3CR.SetPropertyValue(Const aValue : Tv3PropertyDefinition);
begin
  if aValue.Name = 'inverted' Then
    aValue.AsBool(FHasinverted, Finverted)
  else if aValue.Name = 'value' Then
    Fvalue := Tv3CD(aValue.AsType(Tv3CD)).Link
  else if aValue.Name = 'name' Then
    Fname := Tv3CD(aValue.AsType(Tv3CD)).Link
  else
    Inherited SetPropertyValue(aValue);
end;




procedure Tv3CR.DoClear;
begin
  inherited;
  inverted := false;
  Hasinverted := false;
  value := nil;
  name := nil;
end;

{ Tv3DataValue }

function Tv3DataValue.CDAClassTypeV: TCDAClassType;
begin
  result := etDatatype;
end;

end.

