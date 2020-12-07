Unit dicom_dictionary;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

{$i fhir.inc}

Interface

Uses
  SysUtils,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream, fsl_xml;

Type
  EDicomException = class (EFslException);

  {
  An abstract entry in a DICOM Dictionary
  }
  TDicomDictionaryEntity = class (TFslObject)
  Private
    FUID: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Function Clone : TDicomDictionaryEntity; Overload;
    Function Link : TDicomDictionaryEntity; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
    {
    Unique Identifier (UID)  a globally unique dotted decimal string that
    identifies a specific object or a class of objects;
  an ISO-8824 Object Identifier.

    DICOM Unique Identifiers (UIDs) provide the capability to uniquely identify
    a wide variety of items. They guarantee uniqueness across multiple countries,
    sites, vendors and equipment. Different classes of objects, instance of objects
    and information entities can be distinguished from one another across the
  DICOM universe of discourse irrespective of any semantic context.
    }
    Property UID : String read FUID write FUID;
  End;

  {
  A list of abstract entry in a DICOM Dictionary
  }
  TDicomDictionaryEntityList = Class(TFslObjectList)
  Protected
    Function ItemClass : TFslObjectClass; Override;

    Function CompareByUID(pA, pB: Pointer): Integer; Virtual;

    Procedure DefaultCompare(Out aEvent : TFslItemListCompare); Override;

    Function FindByUID(Const sUID: String; Out iIndex: Integer): Boolean; Overload;

  Public
    constructor Create; Override;

    Function Link : TDicomDictionaryEntityList; Overload;
    Function Clone : TDicomDictionaryEntityList; Overload;


    {
      Sorted Dictionary entities by their UID
    }
    Procedure SortedByUID;

    {
      True if items are sorted by their UID
    }
    Function IsSortedByUID : Boolean;

    {
      The index of the first item with matching UID
    }
    Function IndexByUID(Const sUID : String) : Integer; Overload;

    {
      True if there existed an item with matching UID
    }
    Function ExistsByUID(Const sUID : String) : Boolean; Overload;

    {
      Return the item with matching UID, or Null
    }
    Function GetByUID(Const sUID : String) : TDicomDictionaryEntity; Overload;

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomDictionaryEntity) : Integer;

    {
       Get the iIndexth TDicomDictionaryEntity. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomDictionaryEntity;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;

    Procedure RemoveByUID(Const sUID : String);
  End;


  {
  A transfer syntax, which is set of encoding rules,
  allows communicating Application Entities to negotiate common encoding
  techniques they both support (e.g., byte ordering, compression, etc.).

  A Transfer Syntax is an attribute of a Presentation Context, one or more of which are negotiated
  at the establishment of an Association between DICOM Application Entities.
  This Association negotiation is specified in PS 3.8 and discussed in PS 3.7.
  }
  TDicomDictionaryTransferSyntax = class (TDicomDictionaryEntity)
  Private
    FName: String;
    FIsBigEndian: Boolean;
    FIsExplicitVR: Boolean;
    FIsEncapsulated: Boolean;
    FIsLossy: Boolean;
    FIsDeflated: Boolean;

    FProperties: TFslStringMatch;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    constructor Create(Const sName: String; Const sUID: String = '1.2.840.10008.1.2';
      Const isBigEndian: Boolean = False; Const isExplicitVR: Boolean = False;
      Const isEncapsulated: Boolean = False; Const isLossy: Boolean = False; Const isDeflated: Boolean = False); Overload;

    destructor Destroy; Override;

    Function Clone : TDicomDictionaryTransferSyntax; Overload;
    Function Link : TDicomDictionaryTransferSyntax; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    {
    Facade functions to expose custom properties to script
    }
    {
      Return the value of custom property with given key
    }
    Function GetCustomProperty(Const sPropertyName: String): String;

    {
      Return the list of custom property keys
    }
    Function GetCustomPropertyKeys: TFslStringList;


    {
    Name of this transfer syntax
    }
    Property Name: String Read FName Write FName;

    {
    True if Big-Endian encoding is used
    }
    Property IsBigEndian: Boolean Read FIsBigEndian Write FIsBigEndian;

    {
    True if VR are explicitly stated
    }
    Property IsExplicitVR: Boolean Read FIsExplicitVR Write FIsExplicitVR;

    {
    True if pixel data are sent in Encapsulated Format (e.g. compressed)
    defined outside the DICOM standard.
    }
    Property IsEncapsulated: Boolean Read FIsEncapsulated Write FIsEncapsulated;

    {
    True if a lossy compression process is used
    }
    Property IsLossy: Boolean Read FIsLossy Write FIsLossy;

    {
    True if the byte stream is compressed using the Deflate algorithm
    defined in Internet RFC 1951
    }
    Property IsDeflated: Boolean Read FIsDeflated Write FIsDeflated;

    // hide as TFslStringMatch is not defined, replaced with facade functions defined above
    {
    Custom properties
    }
    Property Properties: TFslStringMatch read FProperties;
  End;

  {
  A list of transfer syntax
  }
  TDicomDictionaryTransferSyntaxList = Class(TDicomDictionaryEntityList)
  private
    function GetTransferSyntax(iIndex: integer): TDicomDictionaryTransferSyntax;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Clone : TDicomDictionaryTransferSyntaxList; Overload;
    Function Link : TDicomDictionaryTransferSyntaxList; Overload;


    {
      Return a transfer syntax with matching UID, or NULL
    }
    Function GetTransferSyntaxByUID(Const sUID : String) : TDicomDictionaryTransferSyntax; Overload;

    {
       Get the iIndexth TDicomDictionaryTransferSyntax. (0 = first item)
    }
    Function ItemTransferSyntax(iIndex : Integer) : TDicomDictionaryTransferSyntax;

    Property TransferSyntaxes[iIndex : integer] : TDicomDictionaryTransferSyntax Read GetTransferSyntax; Default;
  End;


  {
  List of possible Presentation Context type
  }
  TDicomPresContextType = (dpcProposed, dpcAccept, dpcRejectUser,
    dpcRejectNoReason, dpcRejectAbstractSyntaxNotSupported, dpcRejectTransferSyntaxesNotSupported);


  {
  A Presentation Context defines the presentation of the data on an Association.
  It provides a lower level of negotiation and one or more Presentation Contexts
  can be offered and accepted per Association.

  A Presentation Context consists of three components, a Presentation Context ID,
  an Abstract Syntax Name, and a list of one or more Transfer Syntax Names.

  Only one Abstract Syntax shall be offered per Presentation Context. However,
  multiple Transfer Syntaxes may be offered per Presentation Context,
  but only one shall be accepted.
  }
  TDicomPresentationContext = class (TFslObject)
  Private
    FAbstractSyntaxUID: String;
    FContextType: TDicomPresContextType;

    FTransferSyntaxList: TDicomDictionaryTransferSyntaxList;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(Const sAbstractSyntaxUID: String; contextType: TDicomPresContextType = dpcProposed); Overload;

    destructor Destroy; Override;

    Function Clone : TDicomPresentationContext; Overload;
    Function Link : TDicomPresentationContext; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
    {
    The UID of the Abstract Syntax that is supported for this presentation context
    }
    Property AbstractSyntaxUID: String Read FAbstractSyntaxUID Write FAbstractSyntaxUID;

    {
    The type of this context (e.g. proposed, accept, reject etc.)
    }
    Property ContextType: TDicomPresContextType Read FContextType Write FContextType;

    {
    A list of transfer syntaxes that are supported for this presentation context
    }
    Property TransferSyntaxList: TDicomDictionaryTransferSyntaxList Read FTransferSyntaxList;
  End;

  {
  A list of presentation context
  Different from transfer syntax, multiple presentation contextes
  with same abstract syntax UID often co-exist in a list
  }
  TDicomPresentationContextList = Class(TFslObjectList)
  Private
    function GetPresentationContext(iIndex: integer): TDicomPresentationContext;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomPresentationContext) : Integer;

    {
       Get the iIndexth TDicomPresentationContext. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomPresentationContext;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;

    Property PresentContextes[iIndex : integer] : TDicomPresentationContext read GetPresentationContext; default;
  End;

  {
  List of possible Value Representation (VR) type
  }
  TDicomVRType = (dvtCS,dvtSH,dvtLO,dvtST,dvtLT,dvtUT,dvtAE,dvtPN,dvtUI,dvtDA,dvtTM,dvtDT,dvtAS,dvtIS,dvtDS,dvtSS,dvtUS,dvtSL,dvtUL,dvtAT,dvtFL,dvtFD,dvtOB,dvtOW,dvtOF,dvtUN);

  // Not documentable
  TDicomVRTypes = set of TDicomVRType;

  {
  The Value Representation of a Data Element, describes the data type
  and format of that Data Element's Value(s).

  PS 3.6 lists the VR of each Data Element by Data Element Tag.
  }
  TDicomDictionaryVR = class (TFslObject)
  Private
    FCode : String;
    FLength: Cardinal;
    FDescription: String;
    FDoco: String;
    FFixed: Boolean;
    FType: TDicomVRType;
    FRepeatable: Boolean;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(aType : TDicomVRType; sCode : String; iLength: Cardinal; bFixed, bRepeatable: Boolean; sDescription, sDoco: String); Overload; Virtual;

    Function Clone : TDicomDictionaryVR; Overload;
    Function Link : TDicomDictionaryVR; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    {
      True if this VR is string-based
    }
    Function IsString : Boolean;

  Published

    {
    The type of the VR
    }
    Property VRType : TDicomVRType read FType;

    {
    The code represented this VR
    }
    Property Code : String read FCode;

    {
    Short description of this VR (can be seen as its name)
    }
    Property Description : String read FDescription;

    {
    Full description of this VR
    }
    Property Doco : String read FDoco;

    {
    Maximum length in bytes of value instance of this VR
    }
    Property Length : Cardinal read FLength;

    {
    True if value instance of this VR is of fixed length
    }
    Property Fixed : Boolean read FFixed;

    {
    True if an element of this VR could contains multiple values
    }
    Property Repeatable : Boolean read FRepeatable write FRepeatable;
  End;

  // Not documentable
  TDicomDictionaryVRSet = Array [TDicomVRType] of TDicomDictionaryVR;

  {
  A list of Value Representation.
  }
  TDicomDictionaryVRList = Class(TDicomDictionaryEntityList)
  private
    function GetVR(iIndex: integer): TDicomDictionaryVR;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Clone : TDicomDictionaryVRList; Overload;
    Function Link : TDicomDictionaryVRList; Overload;


    {
       Get the iIndexth TDicomDictionaryVR. (0 = first item)
    }
    Function ItemVR(iIndex : Integer) : TDicomDictionaryVR;

    Property VRs[iIndex : integer] : TDicomDictionaryVR read GetVR; default;
  End;

  {
  A data element, which is a unit of information in an object definition
  
  A Data Set represents an instance of a real world Information Object. 
  A Data Set is constructed of Data Elements. Data Elements contain 
  the encoded Values of Attributes of that object. The specific content 
  and semantics of these Attributes are specified in Information Object Definitions (see PS 3.3). 
  
  A Data Element is uniquely identified by a Data Element Tag. The Data Elements 
  in a Data Set shall be ordered by increasing Data Element Tag Number 
  and shall occur at most once in a Data Set. 
  }
  TDicomDictionaryElement = class (TFslObject)
  private
    FGroupId: String;
    FElementId: String;
    FGroupName: String;
    FName: String;
    FCode: String;

    FVRs: TDicomDictionaryVRList;
    // VM (value multiplicity)
    FMin: integer;
    FMax: integer;

  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Function Clone : TDicomDictionaryElement; Overload;
    Function Link : TDicomDictionaryElement; Overload;
    Procedure Assign(oSource : TFslObject); Override;

    // don't need to expose this to script
    Function VRTypes : TDicomVRTypes;

  {
  True if this element is a sequence
  }
    Function IsSequence : Boolean;

  {
  Return a string representation of this element
  }
    Function AsString : String;
  
  Published
    {
    The group Id of the element tag
    }
    Property GroupId : String read FGroupId write FGroupId;
    
  {
    The element id of the element tag
    }
    Property ElementId : String read FElementId write FElementId;
    
  {
    The name of the group this element belongs to
    }
    Property GroupName : String read FGroupName write FGroupName;

  {
    The name of this element
    }
    Property Name: String Read FName Write FName;
    
  {
    Code Name of this element
    }
    Property Code: String Read FCode Write FCode;
    
  {
    List of VR that values of this element can be encoded in
    }
    Property VRs : TDicomDictionaryVRList read FVRs;
    
  {
    The minimum number of values that this element contains
    }
    Property Min: integer Read FMin Write FMin;
    
  {
    The maximum number of values that this element contains
    }
    Property Max: integer Read FMax Write FMax;
  End;

  {
  List of Data Element
  }
  TDicomDictionaryElementList = Class(TFslObjectList)
  private
    function GetElements(iIndex: Integer): TDicomDictionaryElement;
  Protected
    Function ItemClass : TFslObjectClass; Override;

    Function CompareByTags(pA, pB: Pointer): Integer; Virtual;

    Procedure DefaultCompare(Out aEvent : TFslItemListCompare); Override;

    Function FindByTags(Const sGroup, sElement: String; Out iIndex: Integer): Boolean; Overload;

  Public
    constructor Create; Override;

    Function Link : TDicomDictionaryElementList; Overload;
    Function Clone : TDicomDictionaryElementList; Overload;

    {
      Sorted elements by their tags (Group ID first, then Element ID)
    }
    Procedure SortedByTags;

    {
      True if elements are sorted by their tags
    }
    Function IsSortedByTags : Boolean;

    {
      The index of an item with matching tag, or -1 if not found
    }
    Function IndexByTags(Const sGroup, sElement: String) : Integer; Overload;

    {
      True if existed item with matching tag
    }
    Function ExistsByTags(Const sGroup, sElement: String) : Boolean; Overload;

    {
      The item with matching tag, or NULL if not found
    }
    Function GetByTags(Const sGroup, sElement: String) : TDicomDictionaryElement; Overload;

    Procedure RemoveByTags(Const sGroup, sElement: String);

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomDictionaryElement) : Integer;

    {
       Get the iIndexth TDicomDictionaryElement. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomDictionaryElement;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;

    Property Elements[iIndex : Integer] :  TDicomDictionaryElement read GetElements; default;
  End;

  {
  List of Elements, organised into groups
  }
  TDicomDictionaryElementGroupList = Class (TFslStringObjectMatch)
  Private
    function GetElementGroup(sKey: String): TDicomDictionaryElementList;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public

    {
       Get the iIndexth TDicomDictionaryElementList. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomDictionaryElementList;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;


    {
  Get a list of elements of the same group, key by the group key
  }
    Property ElementGroup[sKey : String] : TDicomDictionaryElementList Read GetElementGroup; Default;
  End;


  {
  List of possible DICOM Message Service Element (DIMSE) types
  }
  TDicomDimseType = (dimseCStore, dimseCFind, dimseCGet, dimseCMove, dimseCEcho,
    dimseNEvent, dimseNGet, dimseNSet, dimseNAction, dimseNCreate, dimseNDelete);

  {
  List of possible Dicom param types
  }
  TDicomDimseParamType = (dimseParamFixed, dimseParamAuto, dimseParamRequired);

  {
  Represent a Dicom DIMSE message's parameter.

  It contains information such as the element type,
  whether the parameter is fixed/auto-calculated or to be supplied
  }
  TDicomDimseParam = Class (TFslObject)
  Private
    FName: String;
    FFixedValue: String;
    FParamType: TDicomDimseParamType;

    // these are duplicated information to FElement
    FElementId: String;
    FElement: TDicomDictionaryElement;

    Procedure SetElement(oValue: TDicomDictionaryElement);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;

    destructor Destroy; Override;

    Function Clone : TDicomDimseParam; Overload;
    Function Link : TDicomDimseParam; Overload;
    Procedure Assign(oSource : TFslObject); Override;


  Published
    {
  The parameter name
  }
    Property Name: String Read FName Write FName;

    {
  The fixed value, if it exists, for this parameter
  }
    Property FixedValue: String Read FFixedValue Write FFixedValue;

    {
  The parameter type (fixed/auto-calculated or to be supplied)
  }
    Property ParamType: TDicomDimseParamType Read FParamType Write FParamType;

    {
  The element tag id, used to connected this parameter to a valid element instance
  }
    Property ElementId: String Read FElementId Write FElementId;

    {
    The dictionary element this parameter belonged to.
    }
    Property Element: TDicomDictionaryElement Read FElement Write SetElement;
  End;

  {
  List of DIMSE message parameters
  }
  TDicomDimseParamList = Class(TFslObjectList)
  private
    function GetDicomParam(iIndex: Integer): TDicomDimseParam;
  Protected
    Function ItemClass : TFslObjectClass; Override;

  Public
    Function Link : TDicomDimseParamList; Overload;
    Function Clone : TDicomDimseParamList; Overload;

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomDimseParam) : Integer;

    {
       Get the iIndexth TDicomDimseParam. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomDimseParam;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;

    Property DimseParams[iIndex : Integer] :  TDicomDimseParam read GetDicomParam; default;
  End;

  {
  Represent a DICOM Message Service Element (DIMSE) message.

  The DIMSE defines an Application Service Element
  (both the service and protocol) used by peer DICOM Application Entities
  for the purpose of exchanging medical images and related information.

  The DIMSE provides its services by relying on the DIMSE protocol. The DIMSE
  protocol defines the encoding rules necessary to construct Messages. A Message
  is composed of a Command Set (defined in this part of the DICOM Standard)
  followed by a conditional Data Set (defined in PS 3.5).
  }
  TDicomDimseMessage = Class (TFslObject)
  Private
    FName: String;
    FParams : TDicomDimseParamList;

  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;

    destructor Destroy; Override;

    Function Clone : TDicomDimseMessage; Overload;
    Function Link : TDicomDimseMessage; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
    {
  The DIMSE message name (e.g. E-Echo Request/Response etc.)
  }
    Property Name: String Read FName Write FName;

  {
  List of data elements used as parameter for this DIMSE message
  }
  Property Params: TDicomDimseParamList Read FParams;  // make this readonly ?
  End;

  {
  List of DIMSE message
  }
  TDicomDimseService = Class(TFslObjectList)
  Private
    FServiceType: TDicomDimseType;

    Function GetDimseMessage(iIndex: integer): TDicomDimseMessage;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    constructor Create(ServiceType: TDicomDimseType); Overload;

    Function Clone : TDicomDimseService; Overload;
    Function Link : TDicomDimseService; Overload;
    Procedure Assign(oSource : TFslObject); Override;


    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomDimseMessage) : Integer;

    {
       Get the iIndexth TDicomDimseMessage message. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomDimseMessage;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;

    Property DimseMessage[iIndex : integer] : TDicomDimseMessage read GetDimseMessage; default;
  Published
    {
  The DIMSE service type
  }
    Property ServiceType: TDicomDimseType Read FServiceType Write FServiceType;
  End;

  {
  List of DIMSE message, organised into service type names
  }
  TDicomDimseServiceDict = Class (TFslStringObjectMatch)
  Private
    function GetDimseService(sKey: String): TDicomDimseService;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public

    {
       Get the iIndexth TDicomDimseService. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomDimseService;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;


    {
  Get a DIMSE service, key by the service name
  }
    Property Service[sKey : String] : TDicomDimseService Read GetDimseService; Default;
  End;


  {
  Start the Information Object model.
  The models have three level: IOD contains modules, which can use macros.
  Both Macros and Modules specify the attributes and its nesting.
  }
  
  {
  List of possible required type for attribute
  }
  TDicomInfoRequiredType = (irtRequire, irtRequireCond, irtRequireNullable, irtRequireNullableCond, irtOptional);

  TDicomInfoEntryList = Class;
  TDicomInfoGroup = Class;
  
  {
  An abstract attribute entry, with name, comment and other entries as its chidren
  
  Concrete subclasses are InfoEntryAttr or InfoEntryReference
  }
  TDicomInfoEntry = Class (TFslObject)
  Private
    FName: String;
    FChildren: TDicomInfoEntryList;

    FComment: String; // for any text that's are not yet used
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;

    destructor Destroy; Override;

    Function Clone : TDicomInfoEntry; Overload;
    Function Link : TDicomInfoEntry; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
    {
  The entry name, which is a human-readable name
  }
    Property Name: String Read FName Write FName;
    
  {
  List of entries that are children of this entry
  }
    Property Children: TDicomInfoEntryList Read FChildren;
    
  {
  Further comment about this entry, can be used as the entry documentation
  }
    Property Comment: String Read FComment Write FComment;
  End;

  {
  List of abstract attribute entry
  }
  TDicomInfoEntryList = Class (TFslObjectList)
  Private
    function GetEntry(iIndex: integer): TDicomInfoEntry;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomInfoEntry) : Integer;

    {
       Get the iIndexth TDicomInfoEntry. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomInfoEntry;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;

    Property Entry[iIndex : integer] : TDicomInfoEntry read GetEntry; default;
  End;

  {
  A macro/module reference
  
  Module/Macro may reference other module/macro, incorporated the referenced
  module/macro entries as its own
  }
  TDicomInfoEntryReference = Class (TDicomInfoEntry)
  Private
    FRefId: String;
    FRefEntry: TDicomInfoGroup;

    FContextId: String;
    FBaselineContextId: String;
    FDefinedTemplateId: String;
    FBaselineTemplateId: String;

    Procedure SetRefId(sValue: String);
    Procedure SetRefEntry(sValue: TDicomInfoGroup);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Clone : TDicomInfoEntryReference; Overload;
    Function Link : TDicomInfoEntryReference; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
    
  {
  The referenced module/macro unique key, which can be used to resolved this
  entry into a module/macro
  }
    Property RefId: String Read FRefId Write SetRefId;
    
  {
  The referenced module/macro.
  
  Initially, this would be null. After reference resolution, it would be set
  to be the referenced module/macro
  }
    Property RefEntry: TDicomInfoGroup Read FRefEntry Write SetRefEntry;
    
  {
  }
    Property ContextId: String Read FContextId Write FContextId;
    
  {
  }
    Property BaselineContextId: String Read FBaselineContextId Write FBaselineContextId;
    
  {
  }
    Property DefinedTemplateId: String Read FDefinedTemplateId Write FDefinedTemplateId;

  {
  }
    Property BaselineTemplateId: String Read FBaselineTemplateId Write FBaselineTemplateId;
  End;

  {
  Describe each attribute entry in the Information Object model
  }
  TDicomInfoEntryAttr = Class (TDicomInfoEntry)
  Private
    FTag: String; // should we split the tag or not ?
    FRequiredType: TDicomInfoRequiredType;

    FRefElement: TDicomDictionaryElement;  // the resolved tag element

    Procedure SetRefElement(oValue: TDicomDictionaryElement);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Clone : TDicomInfoEntryAttr; Overload;
    Function Link : TDicomInfoEntryAttr; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published

  {
  The unique tag to identify the data element that corresponds to this attribute
  }
    Property Tag: String Read FTag Write FTag;
    
  {
  The compulsary requirement for this attribute (e.g. required, nullable, optional, etc.)
  }
    Property RequiredType: TDicomInfoRequiredType Read FRequiredType Write FRequiredType;
    
  {
  The resolved data element corresponding to this attribute
  }
    Property RefElement: TDicomDictionaryElement Read FRefElement Write SetRefElement;
  End;

  {
  An abstract collection of attributes and references.
  
  Each group have a human readable name, a unique key and any further comment
  }
  TDicomInfoGroup = Class (TFslObject)
  Private
    FKey: String; // the unique key identify this group
    FName: String; // human readable name
    FComment: String; // for any text that's are not yet used

    FChildren: TDicomInfoEntryList;

    Procedure SetKey(sValue: String);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;

    destructor Destroy; Override;

    Function Clone : TDicomInfoGroup; Overload;
    Function Link : TDicomInfoGroup; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
    
  {
  The unique key of this group (e.g. Table number)
  }
    Property Key: String Read FKey Write SetKey;
    
  {
  Human-readable name of this group
  }
    Property Name: String Read FName Write FName;
    
  {
  Documentational comment about this group
  }
    Property Comment: String Read FComment Write FComment;
    
  {
  The list of attributes and references contained in this group
  The attributes and references my contain further attributes and references
  within themselves.
  }
    Property Children: TDicomInfoEntryList Read FChildren;
  End;

  {
  A set of Attributes that are described in a single table that is referenced by 
  multiple Module or other tables. 
  }
  TDicomMacro = Class (TDicomInfoGroup)
  Public
    destructor Destroy; Override;
  End;

  {
  A Dictionary of Macros, indexed by macro's reference key (i.e. table number)
  }
  TDicomMacroDict = Class (TFslStringObjectMatch)
  Private
    function GetMacro(sKey: String): TDicomMacro;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomMacro) : Integer;

    {
       Get the iIndexth TDicomMacro. (0 = first item)
    }
    Function ItemMacro(iIndex : Integer) : TDicomMacro;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;

    {
      Find a macro by it's name (not case sensitive)
    }
    Function FindMacroByName(name : String): TDicomMacro;

  {
  Retrieve a macro using its reference key (i.e. table number)
  }
    Property Macro[sKey : String] : TDicomMacro Read GetMacro; Default;
  End;

  {
  A set of Attributes within an Information Entity or Normalized IOD 
  which are logically related to each other.
  (Very similar to macro, only separated for conceptual reason)
  }
  TDicomModule = Class (TDicomInfoGroup)
  Private
  Public
    destructor Destroy; Override;

    Function Clone : TDicomModule; Overload;
    Function Link : TDicomModule; Overload;
  End;

  {
  A Dictionary of Modules, index by module's reference key (i.e. table number)
  }
  TDicomModuleDict = Class (TDicomMacroDict)
  Private
    function GetModule(sKey: String): TDicomModule;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    {
       Get the iIndexth TDicomModule. (0 = first item)
    }
    Function ItemModule(iIndex : Integer) : TDicomModule;

    {
      Find a module by it's name (not case sensitive)
    }
    Function FindModuleByName(name : String): TDicomModule;

    {
  Retrieve a module using its reference key (i.e. table number)
  }
    Property Module[sKey : String] : TDicomModule Read GetModule; Default;
  End;

  {
  List of possible required type for module in Information Object Definition (IOD)
  }
  TDicomModuleUsageType = (mutMandatory, mutConditional, mutUserOption);

  {
  For used in IOD, is a reference to a module (using module names)
  }
  TDicomModuleReference = Class (TFslObject)
  Private
    FName: String;    // will be used as key to work out the table reference
    FKey: String;          // this is the module key, to be resolved from name
    FRefEntry: TDicomModule;  // this is the real entry
    FUsageType: TDicomModuleUsageType;

    FSectionReference: String;  // not used yet
    FComment: String;

    Procedure SetRefEntry(sValue: TDicomModule);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Clone : TDicomModuleReference; Overload;
    Function Link : TDicomModuleReference; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
  
    {
  The referenced module name, currently used to resolved this reference
  }
    Property Name: String Read FName Write FName;
    
  {
  The key of the referenced module, after reference resolution
  (i.e. table number)
  }
    Property Key: String Read FKey Write FKey;
    
  {
  The referenced module, after reference resolution
  }
    Property RefEntry: TDicomModule Read FRefEntry Write SetRefEntry;
    
  {
  The usage type of this module, within the parent IOD
  (e.g. Mandatory, Conditional or User Option)
  }
    Property UsageType: TDicomModuleUsageType Read FUsageType Write FUsageType;

    {
  The section number, which refer to the section that defined the referenced module
  }
    Property SectionReference: String Read FSectionReference Write FSectionReference;

  {
  Documentational comment about the referenced module usage within the parent  IOD
  }
    Property Comment: String Read FComment Write FComment;
  End;

  {
  A list of module references
  }
  TDicomModuleReferenceList = Class (TFslObjectList)
  Private
    Function GetReference(iIndex : integer):TDicomModuleReference;
  Protected
    Function ItemClass : TFslObjectClass; override;
  Public

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomModuleReference) : Integer;

    {
       Get the iIndexth TDicomModuleReference. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomModuleReference;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;

    Property References[iIndex : integer] : TDicomModuleReference Read GetReference; Default;
  End;

  {
  Information Object Definition (IOD): a data abstraction of a class of 
  similar Real-World Objects which defines the nature and Attributes 
  relevant to the class of Real-World Objects represented.
  }
  TDicomIOD = Class (TFslObject)
  Private
    FName: String;
    FKey: String;

    FModules: TDicomModuleReferenceList;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;

    destructor Destroy; Override;

    Function Clone : TDicomIOD; Overload;
    Function Link : TDicomIOD; Overload;
    Procedure Assign(oSource : TFslObject); Override;
  Published
  
    {
  The human-readable name of this IOD
  }
    Property Name: String Read FName Write FName;
    
  {
  The unique key identified this IOD (e.g. table number)
  }
    Property Key: String Read FKey Write FKey;

    {
  The list of modules that make up this Information Object Definition
  }
    Property Modules: TDicomModuleReferenceList Read FModules;
  End;

  {
  A dictionary of Information Object Definition, indexed by its key 
  (e.g. table number)
  }
  TDicomIODDict = Class (TFslStringObjectMatch)
  Private
    Function GetEntry(sKey: String): TDicomIOD;
//    Function FindEntry(sKey: String): TDicomIOD;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public

    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TDicomIOD) : Integer;

    {
       Get the iIndexth TDicomIOD. (0 = first item)
    }
    Function Item(iIndex : Integer) : TDicomIOD;

    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;

    {
      Find an IOD by it's name (not case sensitive)
    }
    Function FindIODByName(name : String): TDicomIOD;

    Property Entry[sKey : String] : TDicomIOD Read GetEntry; Default;
  End;

  {
  A Service-Object Pair (SOP) Class is defined by the union of an IOD 
  and a DIMSE Service Group. The SOP Class definition contains the rules 
  and semantics which may restrict the use of the services in the DIMSE 
  Service Group and/or the Attributes of the IOD. 
  
  The selection of SOP Classes is used by Application Entities to establish 
  an agreed set of capabilities to support their interaction. This negotiation 
  is performed at association establishment time as described in PS 3.7. 
  An extended negotiation allows Application Entities to further agree on 
  specific options within a SOP Class. 
  }
  TDicomDictionarySOP = class (TDIcomDictionaryEntity)
  Private
    FStatedIOD: String;
    FIODRef: String;
    FName: String;
    FIOD: TDicomIOD;
    procedure SetIOD(const Value: TDicomIOD);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Function Clone : TDicomDictionarySOP; Overload;
    Function Link : TDicomDictionarySOP; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  Published
  
    {
  The human-readable name of this SOP
  }
    Property Name : String read FName write FName;
    
  {
  The IOD stated in the document, but doesn't semantically matched the SOP
  }
    Property StatedIOD : String read FStatedIOD write FStatedIOD;
    
  {
  The referenced key to the IOD used for this SOP
  }
    Property IODRef : String read FIODRef write FIODRef;
    
  {
  The IOD referenced by this SOP
  }
    Property IOD : TDicomIOD read FIOD write SetIOD;
  End;

  {
  A list of Service-Object pair
  }
  TDicomDictionarySOPList = Class(TDicomDictionaryEntityList)
  private
    function GetSOP(iIndex: integer): TDicomDictionarySOP;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Clone : TDicomDictionarySOPList; Overload;
    Function Link : TDicomDictionarySOPList; Overload;


    {
      Return the SOP object given its UID
    }
    Function GetDictionarySOPByUID(Const sUID : String) : TDicomDictionarySOP; Overload;

    {
       Get the iIndexth TDicomDictionarySOP. (0 = first item)
    }
    Function ItemSOP(iIndex : Integer) : TDicomDictionarySOP;

    Property SOPs[iIndex : integer] : TDicomDictionarySOP read GetSOP; default;
  End;

  {
  A collection of all DICOM data models extracted from the standard, possibly
  with local extensions, to facilitate our working with DICOM.
  }
  TDicomDictionary = class (TFslObject)
  Private
    FName : String;

    FSectionReferences : TFslStringMatch;
    FTransferSyntaxes: TDicomDictionaryTransferSyntaxList;
    FVRSet: TDicomDictionaryVRSet;
    FElements: TDicomDictionaryElementList;
    FElementGroups: TDicomDictionaryElementGroupList;
    FWildcardElements : TDicomDictionaryElementList;
    FDimses: TDicomDimseServiceDict;

    FMacros: TDicomMacroDict;
    FModules: TDicomModuleDict;
    FIODs: TDicomIODDict;
    FSOPs: TDicomDictionarySOPList;

    Procedure DeclareFixedData;

    // Conceptually, module entry can reference both macro or module
    // while macro entry can only reference other macros
    Procedure ResolveModuleEntry(oEntry: TDicomInfoEntry; oMissingRefs: TFslStringList);
    Procedure ResolveMacroEntry(oEntry: TDicomInfoEntry; Const sSelfId: String; oMissingRefs: TFslStringList);
    Function UnlinkCircularRefs(oEntry: TDicomInfoEntry; oPreviousRefs: TFslStringList): Boolean;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Clone : TDicomDictionary; Overload;
    Function Link : TDicomDictionary; Overload;
    Procedure Assign(oSource : TFslObject); Override;

  {
  Determinine the VR matching the given VR code
  }
    {$IFDEF DELPHI}
    Function DetermineVRType(sCode : String) : TDicomVRType; overload;
    {$ENDIF}
    Function DetermineVRType(sCode : AnsiString) : TDicomVRType; overload;

  {
  Deterimine if the given VR code is valid
  }
  {$IFDEF DELPHI}
  Function isValidVRType(sCode : String) : Boolean; overload;
  {$ENDIF}
  Function isValidVRType(sCode : AnsiString) : Boolean; overload;

  {
  Find a matching element in the dictionary, giving the element tag
  (i.e. group id and element id)
  }
  Function FindMatchingElement(Const iGroup, iElement: Integer; bErrorIfNotFound : Boolean) : TDicomDictionaryElement;

  {
  Find a matching element in the dictionary, giving the element tag
  (i.e. group id and element id as 0000,0000)
  }
  Function FindMatchingElementForTag(Const sTag: String; bErrorIfNotFound : Boolean) : TDicomDictionaryElement;


    Procedure FinishLoading;
    Procedure ResolveReferences(Out oMissingRefs: TFslStringList);
    Procedure ResolveDataElementReferences(oMissingRefs: TFslStringList);
    Procedure ResolveMacroReferences(oMissingRefs: TFslStringList);
    Procedure ResolveModuleReferences(oMissingRefs: TFslStringList);
    Procedure ResolveIODModuleReferences(oMissingRefs: TFslStringList);
    Procedure ResolveSOPModuleReferences(oMissingRefs: TFslStringList);

    {
    Facade to access VR
    }
    {
      Return the VR giving its VR Type
    }
    Function GetVRByType(Const aType: TDicomVRType): TDicomDictionaryVR;

    {
      Return the VR giving its VR Type code
    }
    Function GetVRByTypeCode(Const sCode: String): TDicomDictionaryVR;

    {
      Get the list of VR Type codes
    }
    Function GetVRTypeCodes: TFslStringList;

    {
       Get the name for the specified abstract syntax/SOP class UID.

       "unknown" if no match
    }
    function AbstractSyntaxName(uid : String):String;

    {
       Get the name for the specified transfer syntax class UID.

       "unknown" if no match
    }
    function TransferSyntaxName(uid : String):String;

    {
    The set of VR defined in this dictionary
  }
    Property VRSet : TDicomDictionaryVRSet read FVRSet;

  Published

    {
  The human-readable name of this dictionary
  }
    Property Name : String read FName write FName;

  {
  The list of transfer syntaxes defined in this dictionary
  }
    Property TransferSyntaxes : TDicomDictionaryTransferSyntaxList read FTransferSyntaxes;
    
  {
  The list of Service-Object pair defined in this dictionary
  }
    Property SOPs : TDicomDictionarySOPList read FSOPs;
    
  {
  The list of elements defined in this dictionary
  }
    Property Elements : TDicomDictionaryElementList Read FElements;
    
  {
  The list of element groups defined in this dictionary
  }
    Property ElementGroups : TDicomDictionaryElementGroupList Read FElementGroups;

  {
  The list of DICOM Message service element defined in this dictionary
  }
    Property Dimses: TDicomDimseServiceDict Read FDimses;
  
    {
  The list of Macros defined in this dictionary
  }
    Property Macros: TDicomMacroDict Read FMacros;
    
  {
  The list of Modules defined in this dictionary
  }
    Property Modules: TDicomModuleDict Read FModules;
    
  {
  The list of Information Object Definition (IOD) defined in this dictionary
  }
    Property IODs: TDicomIODDict Read FIODs;
  End;

  (**
    The workflow here is:
      create a DicomDictionary
      Load n xml files into the dicomDictionary using the parser

      The parser must check that UIDs are unique, and that referential
      integrity is maintained
  **)
  TDicomDictionaryParser = class (TFslObject)
  private
    FSource : TFslStream;
    FDictionary: TDicomDictionary;

    // temporary cache for parsing DIMSE
    // should be released and set to null at end of parse
    // this assume that DIMSE is parse after command element
    FCmdElementCache : TFslStringObjectMatch;

    Procedure CreateCmdElementCache;

    Procedure SetDictionary(const Value: TDicomDictionary);

    Function ReadUID(oElement : TMXmlElement) : String;
    Function ReadBoolean(oElement : TMXmlElement; sName : String) : Boolean;
    Procedure Parse(oElement : TMXmlElement);
    Procedure ParseTransferSyntax(oElement : TMXmlElement);
    Procedure ParseDataElementGroup(oNode: TMXmlElement);
    Procedure ParseSectionReferences(oNode: TMXmlElement);
    Procedure ParseDataElement(Const sGroupName: String; oNode: TMXmlElement);

    Procedure ParseDimse(oNode: TMXmlElement);
    Function ReadDimseParam(oNode: TMXmlElement): TDicomDimseParam;

    Procedure ParseMacro(oNode: TMXmlElement);
    Procedure ParseModule(oNode: TMXmlElement);
    Procedure ReadDicomInfoEntries(Const oNode: TMXmlElement; oEntries: TDicomInfoEntryList);
    Function ReadDicomInfoEntryReference(Const oNode: TMXmlElement): TDicomInfoEntryReference;
    Function ReadDicomInfoEntryAttr(Const oNode: TMXmlElement): TDicomInfoEntryAttr;

    Procedure ParseIOD(oNode: TMXmlElement);
    Procedure ParseSOP(oNode: TMXmlElement);
    Procedure ReadIODModules(Const oNode: TMXmlElement; oReferences: TDicomModuleReferenceList);
    procedure SetSource(const Value: TFslStream);

  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; Override;

    Procedure Execute;

    Procedure LoadFromFile(Const sSource : String);
    Property Dictionary : TDicomDictionary read FDictionary write SetDictionary;
    Property Source : TFslStream read FSource write SetSource;
  End;

Const
  DICOM_PRESCONTEXT_TYPE_NAMES : Array [TDicomPresContextType] of String =
    ('Proposed', 'Accept', 'RejectUser', 'RejectNoReason', 'RejectAbstractSyntaxNotSupported', 'RejectTransferSyntaxesNotSupported');

  DICOM_DIMSE_PARAM_TYPE_NAMES : Array[TDicomDimseParamType] of String =
    ('Fixed Value', 'Auto Calculated', 'Required');

  DICOM_DIMSE_TYPE_NAMES : Array [TDicomDimseType] of String =
    ('C-STORE', 'C-FIND', 'C-GET', 'C-MOVE', 'C-ECHO',
    'N-EVENT', 'N-GET', 'N-SET', 'N-ACTION', 'N-CREATE', 'N-DELETE');

  DICOM_VR_TYPE_NAMES_S : Array [TDicomVRType] of String =
    ('CS','SH','LO','ST','LT','UT','AE','PN','UI','DA','TM','DT','AS','IS','DS','SS','US','SL','UL','AT','FL','FD','OB','OW','OF','UN');
  DICOM_VR_TYPE_NAMES_A : Array [TDicomVRType] of AnsiString =
    ('CS','SH','LO','ST','LT','UT','AE','PN','UI','DA','TM','DT','AS','IS','DS','SS','US','SL','UL','AT','FL','FD','OB','OW','OF','UN');

  DICOM_VRTYPES_NOTREPEATED = [dvtSH,dvtLO,dvtST,dvtLT,dvtUT,dvtOB,dvtOW,dvtOF,dvtUN];

  DICOM_INFO_REQUIRE_TYPE_NAMES : Array [TDicomInfoRequiredType] of String =
    ('Require', 'RequireConditional', 'RequireNullable', 'RequireNullableConditional', 'Optional');

  DICOM_INFO_REQUIRE_TYPE_VALUES : Array [TDicomInfoRequiredType] of String = ('1', '1C', '2', '2C', '3');

  DICOM_MODULE_USAGE_TYPE_NAMES : Array [TDicomModuleUsageType] of String = ('Mandatory', 'Conditional', 'User-Option');

  DICOM_MODULE_USAGE_TYPE_VALUES : Array [TDicomModuleUsageType] of String = ('M', 'C', 'U');

Function PreferredVRType(aTypes : TDicomVRTypes) : TDicomVRType;
Function DescribeTypes(aTypes : TDicomVRTypes) : String;
Function ValueIsCompatible(Const sValue : String; aTypes : TDicomVRTypes) : Boolean;

Function ParseInfoRequiredTypeValue(Const sValue: String): TDicomInfoRequiredType;
Function ParseInfoRequiredTypeName(Const sName: String): TDicomInfoRequiredType;
Function ParseModuleUsageTypeValue(Const sValue: String): TDicomModuleUsageType;
Function ParseModuleUsageTypeName(Const sName: String): TDicomModuleUsageType;
Function ParseDimseTypeValue(Const sName: String): TDicomDimseType;

Const
  REP_CHAR = '/';
  DICOM_CUSTOM_ERROR_BASE = $7FFF;
  DICOM_ERR_REMOTE_NO_ANSWER = DICOM_CUSTOM_ERROR_BASE + 1;
  DICOM_ERR_TIMEOUT = DICOM_ERR_REMOTE_NO_ANSWER + 1;
  DICOM_ERR_EXCEPTION = DICOM_ERR_TIMEOUT + 1;


Procedure ReadTagIds(Const sLocation, sValue : String; var sGroup, sElement : String);
Procedure ReadTagValues(Const sLocation, sValue : String; var iGroup, iElement : Word);
Function FormatTagValues(iGroup, iElement : Word) : String;

Function NameDicomCommand(iCommand : Integer) : String;

Implementation


Function DescribeTypes(aTypes : TDicomVRTypes) : String;
var
  oBuilder : TFslStringBuilder;
  bFirst : Boolean;
  aLoop : TDicomVRType;
Begin
  oBuilder := TFslStringBuilder.Create;
  Try
    bFirst := true;
    for aLoop := Low(TDicomVRType) To High(TDicomVRType) Do
      if aLoop in aTypes then
      begin
        if bFirst then
          bFirst := false
        Else
          oBuilder.Append(',');
        oBuilder.Append(DICOM_VR_TYPE_NAMES_S[aLoop]);
      End;
    result := oBuilder.AsString;      
  Finally
    oBuilder.Free;
  End;
End;

Function PreferredVRType(aTypes : TDicomVRTypes) : TDicomVRType;
Begin
  if dvtCS in aTypes Then result := dvtCS
  else if dvtSH in aTypes Then result := dvtSH
  else if dvtLO in aTypes Then result := dvtLO
  else if dvtST in aTypes Then result := dvtST
  else if dvtLT in aTypes Then result := dvtLT
  else if dvtUT in aTypes Then result := dvtUT
  else if dvtAE in aTypes Then result := dvtAE
  else if dvtPN in aTypes Then result := dvtPN
  else if dvtUI in aTypes Then result := dvtUI
  else if dvtDA in aTypes Then result := dvtDA
  else if dvtTM in aTypes Then result := dvtTM
  else if dvtDT in aTypes Then result := dvtDT
  else if dvtAS in aTypes Then result := dvtAS
  else if dvtIS in aTypes Then result := dvtIS
  else if dvtDS in aTypes Then result := dvtDS
  else if dvtSS in aTypes Then result := dvtSS
  else if dvtUS in aTypes Then result := dvtUS
  else if dvtSL in aTypes Then result := dvtSL
  else if dvtUL in aTypes Then result := dvtUL
  else if dvtAT in aTypes Then result := dvtAT
  else if dvtFL in aTypes Then result := dvtFL
  else if dvtFD in aTypes Then result := dvtFD
  else if dvtOW in aTypes Then result := dvtOW // OW is preferred over OB
  else if dvtOB in aTypes Then result := dvtOB
  else if dvtOF in aTypes Then result := dvtOF
  else result := dvtUN;
End;

Function ParseInfoRequiredTypeValue(Const sValue: String): TDicomInfoRequiredType;
Begin
  Result := TDicomInfoRequiredType(StringArrayIndexOf(DICOM_INFO_REQUIRE_TYPE_VALUES, StringUpper(StringTrimWhiteSpace(sValue))));
End;

Function ParseInfoRequiredTypeName(Const sName: String): TDicomInfoRequiredType;
Begin
  Result := TDicomInfoRequiredType(StringArrayIndexOf(DICOM_INFO_REQUIRE_TYPE_NAMES, sName));
End;

Function ParseModuleUsageTypeValue(Const sValue: String): TDicomModuleUsageType;
Begin
  Result := TDicomModuleUsageType(StringArrayIndexOf(DICOM_MODULE_USAGE_TYPE_VALUES, StringUpper(StringTrimWhiteSpace(sValue))));
End;

Function ParseModuleUsageTypeName(Const sName: String): TDicomModuleUsageType;
Begin
  Result := TDicomModuleUsageType(StringArrayIndexOf(DICOM_MODULE_USAGE_TYPE_NAMES, sName));
End;

Function ParseDimseTypeValue(Const sName: String): TDicomDimseType;
Begin
  Result := TDicomDimseType(StringArrayIndexOf(DICOM_DIMSE_TYPE_NAMES, sName));
End;

{ TDicomDictionaryParser }

destructor TDicomDictionaryParser.Destroy;
begin
  FDictionary.Free;
  FSource.Free;
  inherited;
end;

procedure TDicomDictionaryParser.SetDictionary(const Value: TDicomDictionary);
begin
  FDictionary.Free;
  FDictionary := Value;
end;

procedure TDicomDictionaryParser.Execute;
var
  oParser : TMXmlParser;
  oDoc : TMXmlDocument;
begin
  assert(invariants('Execute', FDictionary, TDicomDictionary, 'Dictionary'));
  if FSource = nil Then
    raise EDicomException.create('A source must be provided loading Dicom Dictionary');

  oParser := TMXmlParser.Create;
  Try
    Try
      oDoc := oParser.Parse(FSource, [xpDropWhitespace, xpDropComments]);
    Except
      on E:exception do
        raise EDicomException.create('Error loading Dicom Dictionary: '+e.message);
    End;
    try
      Try
        Parse(oDoc.docElement);
      Except
        on E:exception do
          raise EDicomException.create('Error parsing Dicom Dictionary: '+e.message);
      End;
    finally
      oDoc.Free;
    end;
  Finally
    oParser.Free;
  End;
  FDictionary.FinishLoading;
end;

procedure TDicomDictionaryParser.Parse(oElement: TMXmlElement);
Var
  oChild : TMXmlElement;
Begin
  Try
    oChild := oElement.firstElement;
    While (oChild <> Nil) Do
    Begin
      If oChild.Name = 'transferSyntax' Then
        ParseTransferSyntax(oChild)
      Else If oChild.name = 'elementGroup' Then
        ParseDataElementGroup(oChild)
      Else If oChild.name = 'Dimse' Then
        ParseDimse(oChild)
      Else If oChild.name = 'Macro' Then
        ParseMacro(oChild)
      Else If oChild.name = 'Module' Then
        ParseModule(oChild)
      Else If oChild.name = 'IOD' Then
        ParseIOD(oChild)
      Else If oChild.name = 'section-references' Then
        ParseSectionReferences(oChild)
      Else If oChild.name = 'SOP' Then
        ParseSOP(oChild)
      Else
        raise EDicomException.create('Unknown element name '+oChild.Name);

     oChild := oChild.nextElement;
    End;
  Finally
    FCmdElementCache.Free;
    FCmdElementCache := Nil;
  End;
End;

Procedure TDicomDictionaryParser.ParseTransferSyntax(oElement: TMXmlElement);
Var
  sUID : String;
  oTransferSyntax : TDicomDictionaryTransferSyntax;
Begin
  { <transferSyntax name="ImplicitVRLittleEndian" uid="1.2.840.10008.1.2"
        bigEndian="false" explicitVR="false"
        encapsulated="false" lossy="false" deflate="false" /> }
  sUID := ReadUID(oElement);
  If FDictionary.TransferSyntaxes.ExistsByUID(sUID) Then
    raise EDicomException.create('The transfer syntax '+sUid+' is already defined');
  oTransferSyntax := TDicomDictionaryTransferSyntax.Create;
  Try
    oTransferSyntax.Name := oElement.Attribute['name'];
    oTransferSyntax.UID := sUID;
    oTransferSyntax.IsBigEndian := ReadBoolean(oElement, 'bigEndian');
    oTransferSyntax.IsExplicitVR := ReadBoolean(oElement, 'explicitVR');
    oTransferSyntax.IsEncapsulated := ReadBoolean(oElement, 'encapsulated');
    oTransferSyntax.IsLossy := ReadBoolean(oElement, 'lossy');
    oTransferSyntax.IsDeflated := ReadBoolean(oElement, 'deflate');
    FDictionary.TransferSyntaxes.Add(oTransferSyntax.Link);
  Finally
    oTransferSyntax.Free;
  End;
End;

Procedure TDicomDictionaryParser.ParseDataElement(Const sGroupName: String; oNode: TMXmlElement);
Var
  oElement: TDicomDictionaryElement;
  sTag1, sTag2, sVR, sVM, s: String;
  sMin, sMax: String;
Begin
  sVR := oNode.Attribute['vr'];
  sVM := oNode.Attribute['vm'];

  oElement := TDicomDictionaryElement.Create;
  Try
    ReadTagIds('ParseDataElement', oNode.Attribute['tag'], sTag1, sTag2);
    oElement.GroupId := sTag1;
    oElement.ElementId := sTag2;
    oElement.GroupName := sGroupName;
    oElement.Name := oNode.Attribute['name'];
    oElement.Code := oNode.Attribute['code'];
    while sVR <> '' Do
    Begin
      StringSplit(sVr, 'or', s, sVR);
      s := Trim(s);
      if s <> 'SQ' Then
        oElement.VRs.Add(FDictionary.VRSet[FDictionary.DetermineVRType(s)].Link);
    End;

    If StringSplit(sVM, '-', sMin, sMax) Then
    Begin
      oElement.Min := StrToInt(sMin);
      If StringFind(sMax, 'n') <> 0 Then
        oElement.Max := MaxInt
      Else
        oElement.Max := StrToInt(sMax);
    End
    Else
    Begin
      oElement.Min := StrToInt(sMin);
      oElement.Max := oElement.Min;
    End;

    FDictionary.FElements.Add(oElement.Link);
    // assume the group is already created
    FDictionary.FElementGroups[sGroupName].Add(oElement.Link);
  Finally
    oElement.Free;
  End;
End;


Procedure TDicomDictionaryParser.ParseDataElementGroup(oNode: TMXmlElement);
Var
  sGroup: String;
  oGroupList: TDicomDictionaryElementList;
  oChild : TMXmlElement;
Begin
  sGroup := oNode.Attribute['name'];

  // add new group list if not yet existed
  If Not FDictionary.ElementGroups.ExistsByKey(sGroup) Then
  Begin
    oGroupList := TDicomDictionaryElementList.Create;
    Try
      FDictionary.ElementGroups.Add(sGroup, oGroupList.Link);
    Finally
      oGroupList.Free;
    End;
  End;

  // read all element of this group
  oChild := oNode.firstElement;
  while (oChild <> nil) Do
    Begin
      If oChild.name = 'element' Then
        ParseDataElement(sGroup, oChild)
      Else
        raise EDicomException.create('Unknown element name ' + oChild.Name);

     oChild := oChild.nextElement;
    End;
End;

Function TDicomDictionaryParser.ReadUID(oElement: TMXmlElement): String;
Begin
  result := oElement.attribute['uid'];
  If result = '' Then
    raise EDicomException.create('No UID defined on element '+oElement.name);
End;

Function TDicomDictionaryParser.ReadBoolean(oElement: TMXmlElement; sName: String): Boolean;
Var
  s : String;
Begin
  s := oElement.attribute['sName'];
  if (s = '') or SameText(s, 'false') or (s = '0') then
    result := false
  else if SameText(s, 'true') or (s = '1') then
    result := true
  Else
    raise EDicomException.create('Invalid boolean value "'+s+'" on element '+oElement.name);
End;


Procedure TDicomDictionaryParser.ParseDimse(oNode: TMXmlElement);
Var
  sDimseType : String;
  aDimseType: TDicomDimseType;
  oDimseMsg : TDicomDimseMessage;
  oDimseService: TDicomDimseService;
  oChild : TMXmlElement;
Begin
  If FCmdElementCache = Nil Then
    CreateCmdElementCache;

  // read attributes
  sDimseType := oNode.attribute['type'];
  aDimseType := ParseDimseTypeValue(sDimseType);

  If (Not FDictionary.Dimses.ExistsByKey(sDimseType)) Then
  Begin
    oDimseService := TDicomDimseService.Create(aDimseType);
    Try
      FDictionary.Dimses.Add(sDimseType, oDimseService.Link);
    Finally
      oDimseService.Free;
    End;
  End;
  oDimseService := FDictionary.Dimses[sDimseType];

  oDimseMsg := TDicomDimseMessage.Create;
  Try
    oDimseMsg.Name := oNode.attribute['name'];

    oChild := oNode.FirstElement;
    While (oChild <> Nil) Do
    Begin
      If oChild.name = 'param' Then
    Begin
        oDimseMsg.Params.Add(ReadDimseParam(oChild));
      End
      Else
        raise EDicomException.create('Unknown element name '+oChild.name);

      oChild := oChild.NextElement;
    End;

    oDimseService.Add(oDimseMsg.Link);
  Finally
    oDimseMsg.Free;
  End;

End;


Function TDicomDictionaryParser.ReadDimseParam(oNode: TMXmlElement): TDicomDimseParam;
Var
  oParam: TDicomDimseParam;
  sTemp : String;
  sLeft, sRight: String;
Begin
  oParam := TDicomDimseParam.Create;
  Try
    oParam.Name := oNode.attribute['name'];
    oParam.FFixedValue := oNode.attribute['fixedValue'];

    // we got the tag, but only interested in the element id
    sTemp := oNode.attribute['tag'];
    If StringSplit(sTemp, ',', sLeft, sRight) Then
      oParam.ElementId := sRight
    Else
      oParam.ElementId := sTemp;

    // Map the element id to a real dictionary element
    If oParam.ElementId <> '' Then
    Begin
      If FCmdElementCache.ExistsByKey(oParam.ElementId) Then
        oParam.Element := TDicomDictionaryElement(FCmdElementCache.GetValueByKey(oParam.ElementId)).Link
      Else
        raise EDicomException.create('Invalid DIMSE param: ' + oParam.Name + '. Parameter type not found for element ID ' + oParam.ElementId);
    End;

    // based on fixed value & element id to determined param type
    If oParam.FFixedValue <> '' Then
      oParam.ParamType := dimseParamFixed
    Else If oParam.ElementId = '0000' Then
      oParam.ParamType := dimseParamAuto
    Else
      oParam.ParamType := dimseParamRequired;

    Result := oParam.Link;
  Finally
    oParam.Free;
  End;
End;


Procedure TDicomDictionaryParser.CreateCmdElementCache;
Var
  iIndex : Integer;
Begin
  FCmdElementCache := TFslStringObjectMatch.Create;
  For iIndex := 0 To FDictionary.Elements.Count - 1 Do
  Begin
    If FDictionary.Elements[iIndex].GroupId = '0000' Then
    Begin
      FCmdElementCache.Add(FDictionary.Elements[iIndex].ElementId, FDictionary.Elements[iIndex].Link);
    End;
  End;
End;

Procedure TDicomDictionaryParser.ParseMacro(oNode: TMXmlElement);
Var
  oResult: TDicomMacro;
Begin
  oResult := TDicomMacro.Create;
  Try
    oResult.Key := oNode.Attribute['refId'];
    oResult.Name := oNode.Attribute['name'];
    oResult.Comment := oNode.Text;

    If (oResult.Key = '') Then
      raise EDicomException.create('Invalid Macro, macro key not found')
    Else
    Begin
      ReadDicomInfoEntries(oNode, oResult.Children);
      FDictionary.Macros.Add(oResult.Key, oResult.Link);
    End;
  Finally
    oResult.Free;
  End;
End;

Procedure TDicomDictionaryParser.ParseModule(oNode: TMXmlElement);
Var
  oResult: TDicomModule;
Begin
  oResult := TDicomModule.Create;
  Try
    oResult.Key := oNode.Attribute['refId'];
    oResult.Name := oNode.Attribute['name'];
    oResult.Comment := oNode.Text.Trim;

    // remove all 'MODULE', 'ATTRIBUTES' from the name
    oResult.Name := StringStrip(oResult.Name, 'MODULE');
    oResult.Name := StringStrip(oResult.Name, 'ATTRIBUTES');
    oResult.Name := StringTrimWhitespace(oResult.Name);

    If (oResult.Key = '') Then
      raise EDicomException.create('Invalid Module, Module Key not found')
    Else
    Begin
      ReadDicomInfoEntries(oNode, oResult.Children);
      FDictionary.Modules.Add(oResult.Key, oResult.Link);
    End;
  Finally
    oResult.Free;
  End;
End;

Procedure TDicomDictionaryParser.ReadDicomInfoEntries(Const oNode: TMXmlElement; oEntries: TDicomInfoEntryList);
Var
  oChild: TMXmlElement;
  oEntry: TDicomInfoEntry;
Begin
  oChild := oNode.firstElement;
  While (oChild <> Nil) Do
  Begin
    If oChild.name = 'reference' Then
      oEntry := ReadDicomInfoEntryReference(oChild)
    Else If oChild.name = 'attribute' Then
      oEntry := ReadDicomInfoEntryAttr(oChild)
    Else
      raise EDicomException.create('Unexpected entry: ' + oChild.name + '. Expected <reference> or <attribute> node.');

    Try
      If oEntry <> Nil Then
        oEntries.Add(oEntry.Link);
    Finally
      oEntry.Free;
    End;

    oChild := oChild.NextElement;
  End;
End;

Function TDicomDictionaryParser.ReadDicomInfoEntryAttr(Const oNode: TMXmlElement): TDicomInfoEntryAttr;
Var
  oAttr: TDicomInfoEntryAttr;
Begin
  oAttr := TDicomInfoEntryAttr.Create;
  Try
    oAttr.Name := oNode.Attribute['name'];
    oAttr.Comment := oNode.Text.Trim;
    oAttr.Tag := StringTrimWhitespace(oNode.Attribute['tag']);
    oAttr.RequiredType := ParseInfoRequiredTypeValue(oNode.Attribute['type']);

    If oAttr.Tag = '' Then
      raise EDicomException.create('Attribute entry without specified tag');

    ReadDicomInfoEntries(oNode, oAttr.Children);
    Result := oAttr.Link;
  Finally
    oAttr.Free;
  End;
End;

Function TDicomDictionaryParser.ReadDicomInfoEntryReference(Const oNode: TMXmlElement): TDicomInfoEntryReference;
Var
  oRef: TDicomInfoEntryReference;
Begin
  oRef := TDicomInfoEntryReference.Create;
  Try
    oRef.Name := oNode.Attribute['name'];
    oRef.Comment := oNode.Text.Trim;
    oRef.RefId := oNode.Attribute['refId'];
    oRef.ContextId := StringTrimWhitespace(oNode.Attribute['CID']);
    oRef.BaselineContextId := StringTrimWhitespace(oNode.Attribute['baselineCID']);
    oRef.DefinedTemplateId := StringTrimWhitespace(oNode.Attribute['TemplateID']);
    oRef.BaselineTemplateId := StringTrimWhitespace(oNode.Attribute['baselineTemplateID']);

    If oRef.RefId = '' Then
      raise EDicomException.create('Reference entry without reference');

    ReadDicomInfoEntries(oNode, oRef.Children);
    Result := oRef.Link;
  Finally
    oRef.Free;
  End;
End;


Procedure TDicomDictionaryParser.ParseIOD(oNode: TMXmlElement);
Var
  oResult: TDicomIOD;
Begin
  oResult := TDicomIOD.Create;
  Try
    oResult.Key := oNode.Attribute['refId'];
    oResult.Name := oNode.Attribute['name'];

    If (oResult.Key = '') Then
      raise EDicomException.create('Invalid IOD, key not found')
    Else
    Begin
      ReadIODModules(oNode, oResult.Modules);
      FDictionary.IODs.Add(oResult.Key, oResult.Link);
    End;
  Finally
    oResult.Free;
  End;
End;

Procedure TDicomDictionaryParser.ParseSOP(oNode: TMXmlElement);
Var
  oResult: TDicomDictionarySOP;
Begin
  oResult := TDicomDictionarySOP.Create;
  Try
    oResult.UID := oNode.Attribute['uid'];
    oResult.Name := oNode.Attribute['name'];
    oResult.StatedIOD := oNode.Attribute['statedIOD'];
    oResult.IODRef := oNode.Attribute['iodRef'];

    FDictionary.SOPs.Add(oResult.Link);
  Finally
    oResult.Free;
  End;
End;

Procedure TDicomDictionaryParser.ReadIODModules(Const oNode: TMXmlElement; oReferences: TDicomModuleReferenceList);
Var
  oChild: TMXmlElement;
  oRef: TDicomModuleReference;
Begin
  oChild := oNode.firstElement;
  While (oChild <> Nil) Do
  Begin
    If oChild.name <> 'reference' Then
      raise EDicomException.create('Unexpected entry: ' + oChild.name + '. Expected <reference> node.')
    Else
    Begin
      oRef := TDicomModuleReference.Create;
      Try
        oRef.Name := oChild.Attribute['name'];
        oRef.FSectionReference := oChild.Attribute['secRef'];
        oRef.UsageType := ParseModuleUsageTypeValue(oChild.attribute['usage']);
        oRef.Comment := oChild.Text.Trim;

        // clean up module name
        oRef.Name := StringStrip(oRef.Name, 'MODULE');
        oRef.Name := StringStrip(oRef.Name, 'ATTRIBUTES');
        oRef.Name := StringTrimWhitespace(oRef.Name);

        oReferences.Add(oRef.Link);
      Finally
        oRef.Free;
      End;
    End;

    oChild := oChild.nextElement;
  End;
End;


procedure TDicomDictionaryParser.SetSource(const Value: TFslStream);
begin
  FSource.Free;
  FSource := Value;
end;

procedure TDicomDictionaryParser.LoadFromFile(const sSource: String);
var
  oFile : TFslFile;
begin
  oFile :=  TFslFile.Create(sSource, fmOpenRead);
  Try
    Source := oFile.Link;
  Finally
    oFile.Free;
  End;
end;

procedure TDicomDictionaryParser.ParseSectionReferences(oNode: TMXmlElement);
Var
  oChild : TMXmlElement;
Begin
  // read all element of this group
  oChild := oNode.FirstElement;
  while (oChild <> nil) Do
    Begin
      If oChild.name = 'section' Then
        FDictionary.FSectionReferences.Add(oChild.Attribute['name'], oChild.Attribute['ref'])
      Else
        raise EDicomException.create('Unknown element name ' + oChild.name);
     oChild := oChild.NextElement;
    End;
end;

function TDicomDictionaryParser.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSource.sizeInBytes);
  inc(result, FDictionary.sizeInBytes);
  inc(result, FCmdElementCache.sizeInBytes);
end;

{ TDicomDictionaryEntity }

procedure TDicomDictionaryEntity.Assign(oSource: TFslObject);
begin
  inherited;
  FUID := TDicomDictionaryEntity(oSource).FUID;
end;

function TDicomDictionaryEntity.Clone: TDicomDictionaryEntity;
begin
  result := TDicomDictionaryEntity(Inherited Clone);
end;

function TDicomDictionaryEntity.Link: TDicomDictionaryEntity;
begin
  result := TDicomDictionaryEntity(Inherited Link);
end;

function TDicomDictionaryEntity.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FUID.length * sizeof(char)) + 12);
end;

{ TDicomDictionaryEntityList }

Constructor TDicomDictionaryEntityList.Create;
Begin
  Inherited;

  SortedByUID;
End;


Function TDicomDictionaryEntityList.Link : TDicomDictionaryEntityList;
Begin
  Result := TDicomDictionaryEntityList(Inherited Link);
End;


Function TDicomDictionaryEntityList.Clone : TDicomDictionaryEntityList;
Begin
  Result := TDicomDictionaryEntityList(Inherited Clone);
End;


Function TDicomDictionaryEntityList.ItemClass : TFslObjectClass;
Begin
  Result := TDicomDictionaryEntity;
End;

Procedure TDicomDictionaryEntityList.DefaultCompare(Out aEvent: TFslItemListCompare);
Begin
  aEvent := CompareByUID;
End;

Function TDicomDictionaryEntityList.CompareByUID(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TDicomDictionaryEntity(pA).UID, TDicomDictionaryEntity(pB).UID);
End;

Function TDicomDictionaryEntityList.FindByUID(Const sUID : String; Out iIndex : Integer) : Boolean;
Var
  oUID : TDicomDictionaryEntity;
Begin
  oUID := TDicomDictionaryEntity(ItemNew);
  Try
    oUID.UID := sUID;

    Result := Find(oUID, iIndex, CompareByUID);
  Finally
    oUID.Free;
  End;
End;

Function TDicomDictionaryEntityList.GetByUID(Const sUID: String): TDicomDictionaryEntity;
Var
  iIndex : Integer;
Begin 
  If FindByUID(sUID, iIndex) Then
    Result := TDicomDictionaryEntity(ObjectByIndex[iIndex])
  Else
    Result := Nil;
End;  


Function TDicomDictionaryEntityList.IndexByUID(Const sUID : String) : Integer;
Begin
  If Not FindByUID(sUID, Result) Then
    Result := -1;
End;  


Function TDicomDictionaryEntityList.ExistsByUID(Const sUID: String): Boolean;
Begin 
  Result := ExistsByIndex(IndexByUID(sUID));
End;  


Procedure TDicomDictionaryEntityList.RemoveByUID(Const sUID: String);
Var
  iIndex : Integer;
Begin
  If Not FindByUID(sUID, iIndex) Then
    RaiseError('RemoveByUID', StringFormat('Object ''%s'' not found in list.', [sUID]));

  DeleteByIndex(iIndex);
End;


Function TDicomDictionaryEntityList.IsSortedByUID : Boolean;
Begin
  Result := IsSortedBy(CompareByUID);
End;


Procedure TDicomDictionaryEntityList.SortedByUID;
Begin 
  SortedBy(CompareByUID);
End;


Function TDicomDictionaryEntityList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomDictionaryEntityList.IndexOf(value: TDicomDictionaryEntity): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomDictionaryEntityList.Item(iIndex: Integer): TDicomDictionaryEntity;
Begin
  Result := TDicomDictionaryEntity(ObjectByIndex[iIndex]);
End;

{ TDicomDictionaryTransferSyntax }

Constructor TDicomDictionaryTransferSyntax.Create;
Begin
  Inherited;

  FName := '';

  FProperties := TFslStringMatch.Create;
End;

Constructor TDicomDictionaryTransferSyntax.Create(Const sName, sUID: String;
  Const isBigEndian, isExplicitVR, isEncapsulated, isLossy,
  isDeflated: Boolean);
Begin
  Inherited Create;

  FName := sName;
  UID := sUID;

  FIsBigEndian := isBigEndian;
  FIsExplicitVR := isExplicitVR;
  FIsEncapsulated := isEncapsulated;
  FIsLossy := isLossy;
  FIsDeflated := isDeflated;

  FProperties := TFslStringMatch.Create;
End;

Procedure TDicomDictionaryTransferSyntax.Assign(oSource: TFslObject);
Begin
  Inherited;

  FName := TDicomDictionaryTransferSyntax(oSource).FName;

  FIsBigEndian := TDicomDictionaryTransferSyntax(oSource).FIsBigEndian;
  FIsExplicitVR := TDicomDictionaryTransferSyntax(oSource).FIsExplicitVR;
  FIsEncapsulated := TDicomDictionaryTransferSyntax(oSource).FIsEncapsulated;
  FIsLossy := TDicomDictionaryTransferSyntax(oSource).FIsLossy;
  FIsDeflated := TDicomDictionaryTransferSyntax(oSource).FIsDeflated;

  FProperties.Assign(TDicomDictionaryTransferSyntax(oSource).FProperties);
End;

Function TDicomDictionaryTransferSyntax.Clone: TDicomDictionaryTransferSyntax;
Begin
  result := TDicomDictionaryTransferSyntax(Inherited Clone);
End;

Function TDicomDictionaryTransferSyntax.Link: TDicomDictionaryTransferSyntax;
Begin
  result := TDicomDictionaryTransferSyntax(Inherited Link);
End;

Destructor TDicomDictionaryTransferSyntax.Destroy;
Begin
  FProperties.Free;

  Inherited;
End;

Function TDicomDictionaryTransferSyntax.GetCustomProperty(Const sPropertyName: String): String;
Begin
  Result := Properties.GetValueByKey(sPropertyName);
End;

Function TDicomDictionaryTransferSyntax.GetCustomPropertyKeys: TFslStringList;
Var
  iIndex: Integer;
  oKeys: TFslStringList;
Begin
  Result := Nil;
  oKeys := TFslStringList.Create;
  Try
    For iIndex := 0 To Properties.Count -1 Do
    Begin
      Result.Add(Properties.KeyByIndex[iIndex]);
    End;

    Result := oKeys.Link;
  Finally
    oKeys.Free;
  End;
End;

function TDicomDictionaryTransferSyntax.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FProperties.sizeInBytes);
end;

{ TDicomDictionaryTransferSyntaxList }

Function TDicomDictionaryTransferSyntaxList.Clone: TDicomDictionaryTransferSyntaxList;
Begin
  Result := TDicomDictionaryTransferSyntaxList(Inherited Clone);
End;

Function TDicomDictionaryTransferSyntaxList.GetTransferSyntaxByUID(const sUID: String): TDicomDictionaryTransferSyntax;
Begin
  result := TDicomDictionaryTransferSyntax(Inherited GetByUid(sUid));
End;

Function TDicomDictionaryTransferSyntaxList.GetTransferSyntax(iIndex: integer): TDicomDictionaryTransferSyntax;
Begin
  result := TDicomDictionaryTransferSyntax(Inherited ObjectByIndex[iIndex]);
End;

Function TDicomDictionaryTransferSyntaxList.ItemClass: TFslObjectClass;
Begin
  result := TDicomDictionaryTransferSyntax;
End;

Function TDicomDictionaryTransferSyntaxList.Link: TDicomDictionaryTransferSyntaxList;
Begin
  Result := TDicomDictionaryTransferSyntaxList(Inherited Link);
End;

Function TDicomDictionaryTransferSyntaxList.ItemTransferSyntax(iIndex: Integer): TDicomDictionaryTransferSyntax;
Begin
  Result := TransferSyntaxes[iIndex];
End;

{ TDicomDictionary }

Procedure TDicomDictionary.Assign(oSource: TFslObject);
Var
  i : TDicomVRType;
Begin
  Inherited;
  FTransferSyntaxes.Assign(TDicomDictionary(oSource).FTransferSyntaxes);
  FSOPs.Assign(TDicomDictionary(oSource).FSOPs);
  For i := Low(TDicomVRType) to High(TDicomVRType) Do
    FVRSet[i] := TDicomDictionary(oSource).FVRSet[i].Link;
  FElements.Assign(TDicomDictionary(oSource).FElements);
  FElementGroups.Assign(TDicomDictionary(oSource).FElementGroups);
  FDimses.Assign(TDicomDictionary(oSource).FDimses);

  FMacros.Assign(TDicomDictionary(oSource).FMacros);
  FModules.Assign(TDicomDictionary(oSource).FModules);
  FIODs.Assign(TDicomDictionary(oSource).FIODs);
End;

Function TDicomDictionary.Clone: TDicomDictionary;
Begin
  result := TDicomDictionary(Inherited Clone);
End;

Constructor TDicomDictionary.Create;
Begin
  Inherited;
  FSectionReferences := TFslStringMatch.Create;
  FTransferSyntaxes := TDicomDictionaryTransferSyntaxList.Create;
  FSOPs := TDicomDictionarySOPList.Create;
  FElements := TDicomDictionaryElementList.Create;
  FElementGroups := TDicomDictionaryElementGroupList.Create;
  FWildcardElements := TDicomDictionaryElementList.Create;
  FDimses := TDicomDimseServiceDict.Create;

  FMacros := TDicomMacroDict.Create;
  FModules := TDicomModuleDict.Create;
  FIODs := TDicomIODDict.Create;

  DeclareFixedData;
End;

Destructor TDicomDictionary.Destroy;
Var
  i : TDicomVRType;
Begin
  FSectionReferences.Free;
  FElements.Free;
  FElementGroups.Free;
  FWildcardElements.Free;
  FTransferSyntaxes.Free;
  FSOPs.Free;
  FDimses.Free;

  FMacros.Free;
  FModules.Free;
  FIODs.Free;
  For i := Low(TDicomVRType) to High(TDicomVRType) Do
    FVRSet[i].Free;
    
  Inherited;
End;

procedure TDicomDictionary.DeclareFixedData;
const
  fix = true;
  nfix = false;
  rep = true;
  nrep = false;
  bin = true;
  nbin = false;
begin
  FVRSet[dvtCS] := TDicomDictionaryVR.Create(dvtCS, 'CS', 16,        nfix,  nrep,  'Code String', 'A string of characters with leading or trailing spaces being nonsignificant. Example: CD123_4');
  FVRSet[dvtSH] := TDicomDictionaryVR.Create(dvtSH, 'SH', 16,        nfix,  nrep,  'Short String', 'A short character string. Example: telephone numbers, IDs');
  FVRSet[dvtLO] := TDicomDictionaryVR.Create(dvtLO, 'LO', 64,        nfix,  nrep,  'Long String', 'A character string that may be padded with leading and/or trailing spaces. Example: Introduction to DICOM');
  FVRSet[dvtST] := TDicomDictionaryVR.Create(dvtST, 'ST', 1024,      nfix,  nrep,  'Short Text', 'A character string that may contain one or more paragraphs.');
  FVRSet[dvtLT] := TDicomDictionaryVR.Create(dvtLT, 'LT', 10240,     nfix,  nrep,  'Long Text', 'A character string that may contain one or more paragraphs, the same as LO, but can be much longer.');
  FVRSet[dvtUT] := TDicomDictionaryVR.Create(dvtUT, 'UT', 4294967294,nfix,  nrep,  'Unlimited Text', 'A character string that may contain one or more paragraphs, similar to LT.');
  FVRSet[dvtAE] := TDicomDictionaryVR.Create(dvtAE, 'AE', 16,        nfix,  rep,   'Application Entity', 'A string of characters that identifies a device name with leading and trailing spaces being nonsignificant. Example: MyPC01');
  FVRSet[dvtPN] := TDicomDictionaryVR.Create(dvtPN, 'PN', 64,        nfix,  rep,   'Person Name', 'Persons name, with a caret character (^) used as a name delimiter. Examples: SMITH^JOHN, Morrison-Jones^Susan^^^Ph.D,Chief Executive Officer');
  FVRSet[dvtUI] := TDicomDictionaryVR.Create(dvtUI, 'UI', 64,        nfix,  rep,   'Unique Identifier (UID)', 'A character string containing a UID that is used to uniquely identify a wide variety of items. Example: 1.2.840.10008.1.1');
  FVRSet[dvtDA] := TDicomDictionaryVR.Create(dvtDA, 'DA', 8,         nfix,  rep,   'Date', 'A string of characters of the format YYYYMMDD; where YYYY shall contain year, MM shall contain the month, and DD shall contain the day. Example: 20050822 would represent August 22, 2005.');
  FVRSet[dvtTM] := TDicomDictionaryVR.Create(dvtTM, 'TM', 16,        nfix,  rep,   'Time', 'A string of characters of the format HHMMSS.FRAC; where HH contains hours (range 00  23), MM contains minutes (range 00  59), '+'SS contains seconds (range 00 59), and FRAC contains a fractional part of a second as small as one millionth of a second. Example: 183200.00 stands for 6:32 PM.');
  FVRSet[dvtDT] := TDicomDictionaryVR.Create(dvtDT, 'DT', 26,        nfix,  rep,   'Date Time', 'Concatenated datetime string in the format: YYYYMMDDHHMMSS.FFFFFF The components of this string, from left to right, are YYYY = Year, '+'MM = Month, DD = Day, HH = Hour, MM = Minute, SS = Second, FFFFFF = Fractional Second. Example: 20050812183000.00 stands for 6:30 PM, August 12, 2005');
  FVRSet[dvtAS] := TDicomDictionaryVR.Create(dvtAS, 'AS', 4,         fix,   rep,   'Age String', 'A string of characters with one of the following formats: nnnD, nnnW, nnnM, nnnY; where nnn contains the number of days for D, weeks for W, months for M, or years for Y. Example: 018M would represent an age of 18 months.');
  FVRSet[dvtIS] := TDicomDictionaryVR.Create(dvtIS, 'IS', 12,        nfix,  rep,   'Integer String', 'A string of characters representing an integer. Example: -1234567.');
  FVRSet[dvtDS] := TDicomDictionaryVR.Create(dvtDS, 'DS', 16,        nfix,  rep,   'Decimal String', 'A string of characters representing either a fixed point number or a floating point number. Example: 12345.67, -5.0e3');
  FVRSet[dvtSS] := TDicomDictionaryVR.Create(dvtSS, 'SS', 2,         fix,   rep,   'Signed Short', 'Signed binary integer 16 bits long.');
  FVRSet[dvtUS] := TDicomDictionaryVR.Create(dvtUS, 'US', 2,         fix,   rep,   'Unsigned Short', 'Unsigned binary integer 16 bits long.');
  FVRSet[dvtSL] := TDicomDictionaryVR.Create(dvtSL, 'SL', 4,         fix,   rep,   'Signed Long', 'Signed binary integer.');
  FVRSet[dvtUL] := TDicomDictionaryVR.Create(dvtUL, 'UL', 4,         fix,   rep,   'Unsigned Long', 'Unsigned binary integer 32 bits long.');
  FVRSet[dvtAT] := TDicomDictionaryVR.Create(dvtAT, 'AT', 4,         fix,   rep,   'Attribute Tag', 'Ordered pair of 16-bit (2-byte) unsigned integers that is the value of a Data Element Tag.');
  FVRSet[dvtFL] := TDicomDictionaryVR.Create(dvtFL, 'FL', 4,         fix,   rep,   'Floating Point Single', 'Single precision binary floating point number.');
  FVRSet[dvtFD] := TDicomDictionaryVR.Create(dvtFD, 'FD', 8,         fix,   rep,   'Floating Point Double', 'Double precision binary floating point number.');
  FVRSet[dvtOB] := TDicomDictionaryVR.Create(dvtOB, 'OB', 0,         nfix,  nrep,  'Other Byte String', 'A string of bytes (other means not defined in any other VR).');
  FVRSet[dvtOW] := TDicomDictionaryVR.Create(dvtOW, 'OW', 0,         nfix,  nrep,  'Other Word String', 'A string of 16-bit (2-byte) words.');
  FVRSet[dvtOF] := TDicomDictionaryVR.Create(dvtOF, 'OF', 0,         nfix,  nrep,  'Other Float String', 'A string of 32-bit (4-byte) floating point words.');
//  FVRSet[dvtSQ] := TDicomDictionaryVR.Create(dvtSQ, 'SQ', 0,       nfix,  nrep,  'Sequence of Items', 'Sequence of items.');
  FVRSet[dvtUN] := TDicomDictionaryVR.Create(dvtUN, 'UN', 0,         nfix,  nrep,  'Unknown', 'A string of bytes where the encoding of the contents is unknown.');
end;

{$IFDEF DELPHI}
Function TDicomDictionary.isValidVRType(sCode : String) : Boolean;
Begin
  result := (sCode = 'CS') Or (sCode = 'SH') Or (sCode = 'LO') Or (sCode = 'ST') Or (sCode = 'LT') Or (sCode = 'UT') Or (sCode = 'AE') Or (sCode = 'PN') Or (sCode = 'UI') Or (sCode = 'DA') Or (sCode = 'TM') Or (sCode = 'DT') Or (sCode = 'AS') Or (sCode = 'IS') Or (sCode = 'DS') Or (sCode = 'SS') Or (sCode = 'US') Or (sCode = 'SL') Or (sCode = 'UL') Or (sCode = 'AT') Or (sCode = 'FL') Or (sCode = 'FD') Or (sCode = 'OB') Or (sCode = 'OW') Or (sCode = 'OF') Or (sCode = 'UN');
End;
{$ENDIF}

Function TDicomDictionary.isValidVRType(sCode : AnsiString) : Boolean;
Begin
  result := (sCode = 'CS') Or (sCode = 'SH') Or (sCode = 'LO') Or (sCode = 'ST') Or (sCode = 'LT') Or (sCode = 'UT') Or (sCode = 'AE') Or (sCode = 'PN') Or (sCode = 'UI') Or (sCode = 'DA') Or (sCode = 'TM') Or (sCode = 'DT') Or (sCode = 'AS') Or (sCode = 'IS') Or (sCode = 'DS') Or (sCode = 'SS') Or (sCode = 'US') Or (sCode = 'SL') Or (sCode = 'UL') Or (sCode = 'AT') Or (sCode = 'FL') Or (sCode = 'FD') Or (sCode = 'OB') Or (sCode = 'OW') Or (sCode = 'OF') Or (sCode = 'UN');
End;

Function TDicomDictionary.DetermineVRType(sCode : String):TDicomVRType;
Begin
  if sCode = 'CS' Then
    result := dvtCS
  else if sCode = 'SH' Then
    result := dvtSH
  else if sCode = 'LO' Then
    result := dvtLO
  else if sCode = 'ST' Then
    result := dvtST
  else if sCode = 'LT' Then
    result := dvtLT
  else if sCode = 'UT' Then
    result := dvtUT
  else if sCode = 'AE' Then
    result := dvtAE
  else if sCode = 'PN' Then
    result := dvtPN
  else if sCode = 'UI' Then
    result := dvtUI
  else if sCode = 'DA' Then
    result := dvtDA
  else if sCode = 'TM' Then
    result := dvtTM
  else if sCode = 'DT' Then
    result := dvtDT
  else if sCode = 'AS' Then
    result := dvtAS
  else if sCode = 'IS' Then
    result := dvtIS
  else if sCode = 'DS' Then
    result := dvtDS
  else if sCode = 'SS' Then
    result := dvtSS
  else if sCode = 'US' Then
    result := dvtUS
  else if sCode = 'SL' Then
    result := dvtSL
  else if sCode = 'UL' Then
    result := dvtUL
  else if sCode = 'AT' Then
    result := dvtAT
  else if sCode = 'FL' Then
    result := dvtFL
  else if sCode = 'FD' Then
    result := dvtFD
  else if sCode = 'OB' Then
    result := dvtOB
  else if sCode = 'OW' Then
    result := dvtOW
  else if sCode = 'OF' Then
    result := dvtOF
  else if sCode = 'UN' Then
    result := dvtUN
  else
    raise EDicomException.create('Unknown VR Type "'+sCode+'"');
End;

{$IFDEF DELPHI}
Function TDicomDictionary.DetermineVRType(sCode : AnsiString):TDicomVRType;
Begin
  if sCode = 'CS' Then
    result := dvtCS
  else if sCode = 'SH' Then
    result := dvtSH
  else if sCode = 'LO' Then
    result := dvtLO
  else if sCode = 'ST' Then
    result := dvtST
  else if sCode = 'LT' Then
    result := dvtLT
  else if sCode = 'UT' Then
    result := dvtUT
  else if sCode = 'AE' Then
    result := dvtAE
  else if sCode = 'PN' Then
    result := dvtPN
  else if sCode = 'UI' Then
    result := dvtUI
  else if sCode = 'DA' Then
    result := dvtDA
  else if sCode = 'TM' Then
    result := dvtTM
  else if sCode = 'DT' Then
    result := dvtDT
  else if sCode = 'AS' Then
    result := dvtAS
  else if sCode = 'IS' Then
    result := dvtIS
  else if sCode = 'DS' Then
    result := dvtDS
  else if sCode = 'SS' Then
    result := dvtSS
  else if sCode = 'US' Then
    result := dvtUS
  else if sCode = 'SL' Then
    result := dvtSL
  else if sCode = 'UL' Then
    result := dvtUL
  else if sCode = 'AT' Then
    result := dvtAT
  else if sCode = 'FL' Then
    result := dvtFL
  else if sCode = 'FD' Then
    result := dvtFD
  else if sCode = 'OB' Then
    result := dvtOB
  else if sCode = 'OW' Then
    result := dvtOW
  else if sCode = 'OF' Then
    result := dvtOF
  else if sCode = 'UN' Then
    result := dvtUN
  else
    raise EDicomException.create('Unknown VR Type "'+String(sCode)+'"');
End;
{$ENDIF}

Function Matches(sPattern, sInstance : String): Boolean;
var
  i : Integer;
Begin
  result := (length(sPattern) = 4) And (Length(sInstance) = 4);
  if result then
    for i := 1 to 4 Do
      result := result and ((sPattern[i] = 'x') or (sPattern[i] = sInstance[i]));
End;


function TDicomDictionary.FindMatchingElement(const iGroup, iElement: Integer; bErrorIfNotFound : Boolean): TDicomDictionaryElement;
var
  sGroup, sElement : String;
  i : integer;
Begin
  sGroup := IntToHex(iGroup, 4);
  sElement := IntToHex(iElement, 4);
  result := FElements.GetByTags(sGroup, sElement);
  if result = nil Then
  Begin
    for i := 0 to FWildcardElements.Count -1 Do
    Begin
      If Matches(FWildcardElements[i].GroupId, sGroup) and Matches(FWildcardElements[i].ElementId, sElement) Then
      Begin
        result := FWildcardElements[i];
        exit;
      End;
    End;
  End;
  if bErrorIfNotFound and (result = nil) Then
    if FName = '' Then
      RaiseError('FindMatchingElement', 'The dictionary does not contain a definition for the element ('+sGroup+'/'+sElement+')')
    Else
      RaiseError('FindMatchingElement', 'The dictionary "'+Name+'" does not contain a definition for the element ('+sGroup+'/'+sElement+')');
End;


Procedure TDicomDictionary.FinishLoading;
Var
  i : Integer;
Begin
  FWildcardElements.Clear;
  For i := 0 to FElements.Count - 1 do
    if (pos('x', FElements[i].GroupId) > 0) or (pos('x', FElements[i].ElementId) > 0) Then
      FWildcardElements.Add(FElements[i].Link);
End;

Function TDicomDictionary.Link: TDicomDictionary;
Begin
  result := TDicomDictionary(Inherited Link);
End;

Procedure TDicomDictionary.ResolveReferences(Out oMissingRefs: TFslStringList);
Begin
  oMissingRefs := TFslStringList.Create;

  ResolveMacroReferences(oMissingRefs);
  ResolveModuleReferences(oMissingRefs);
  ResolveDataElementReferences(oMissingRefs);
  ResolveIODModuleReferences(oMissingRefs);
  ResolveSOPModuleReferences(oMissingRefs);

End;

Procedure TDicomDictionary.ResolveMacroReferences(oMissingRefs: TFslStringList);
Var
  iX, iY: Integer;
  oMacro: TDicomMacro;
Begin
  For iX := 0 To FMacros.Count - 1 Do
  Begin
    oMacro := TDicomMacro(FMacros.ValueByIndex[iX]);
    For iY := 0 To oMacro.Children.Count - 1 Do
    Begin
      ResolveMacroEntry(TDicomInfoEntry(oMacro.Children[iY]), oMacro.Key, oMissingRefs);
    End;
  End;
End;

Procedure TDicomDictionary.ResolveMacroEntry(oEntry: TDicomInfoEntry; Const sSelfId: String; oMissingRefs: TFslStringList);
Var
  oRef: TDicomInfoEntryReference;
  iX: Integer;
Begin
  If oEntry is TDicomInfoEntryReference Then
  Begin
    oRef := TDicomInfoEntryReference(oEntry);

    If FMacros.ExistsByKey(oRef.RefId) Then
    Begin
      // Avoid the simple case of reference itself
      If oRef.RefId <> sSelfId Then
        oRef.RefEntry := FMacros[oRef.RefId].Link;
    End
    Else
      oMissingRefs.Add(oRef.RefId);
  End;

  For iX := 0 To oEntry.Children.Count - 1 Do
  Begin
    ResolveMacroEntry(oEntry.Children[iX], sSelfId, oMissingRefs);
  End;
End;


// NOT USED, BUT KEEP FOR LATER DEBUGING OF THE MODEL
Function TDicomDictionary.UnlinkCircularRefs(oEntry: TDicomInfoEntry; oPreviousRefs: TFslStringList): Boolean;
Var
  oRef: TDicomInfoEntryReference;
  oGroup: TDicomInfoGroup;
  iX: Integer;
Begin
  Result := False;
  If oEntry is TDicomInfoEntryReference Then
  Begin
    oRef := TDicomInfoEntryReference(oEntry);

    If oPreviousRefs.ExistsByValue(oRef.RefId) Then
    Begin
      Result := True;
      oRef.RefEntry := Nil;
    End
    Else
    Begin
      oGroup := oRef.RefEntry;
      oPreviousRefs.Add(oRef.RefId);
      For iX := 0 To oGroup.Children.Count - 1 Do
      Begin
        Result := Result Or UnlinkCircularRefs(oGroup.Children[iX], oPreviousRefs);
      End;
      oPreviousRefs.DeleteByValue(oRef.RefId);
    End;
  End;


  If Result = False THen
  Begin
    For iX := 0 To oEntry.Children.Count - 1 Do
    Begin
      Result := Result Or UnlinkCircularRefs(oEntry.Children[iX], oPreviousRefs);
    End;
  End;
End;


Procedure TDicomDictionary.ResolveModuleReferences(oMissingRefs: TFslStringList);
Var
  iX, iY: Integer;
  oModule: TDicomModule;
Begin
  For iX := 0 To FModules.Count - 1 Do
  Begin
    oModule := TDicomModule(FModules.ValueByIndex[iX]);
    For iY := 0 To oModule.Children.Count - 1 Do
    Begin
      ResolveModuleEntry(TDicomInfoEntry(oModule.Children[iY]), oMissingRefs);
    End;
  End;
End;

Procedure TDicomDictionary.ResolveModuleEntry(oEntry: TDicomInfoEntry; oMissingRefs: TFslStringList);
Var
  oRef: TDicomInfoEntryReference;
  iX: Integer;
Begin
  If oEntry is TDicomInfoEntryReference Then
  Begin
    oRef := TDicomInfoEntryReference(oEntry);

    If FMacros.ExistsByKey(oRef.RefId) Then
      oRef.RefEntry := FMacros[oRef.RefId].Link
    Else If FModules.ExistsByKey(oRef.RefId) Then
      oRef.RefEntry := FModules[oRef.RefId].Link
    Else
      oMissingRefs.Add(oRef.RefId);
  End;

  For iX := 0 To oEntry.Children.Count - 1 Do
  Begin
    ResolveModuleEntry(oEntry.Children[iX], oMissingRefs);
  End;
End;

Procedure TDicomDictionary.ResolveDataElementReferences(oMissingRefs: TFslStringList);
Var
  oModule: TDicomModule;
  oMacro: TDicomMacro;
  iX, iY, iIndex: Integer;
  sGroup, sElement: String;
  oAttr: TDicomInfoEntryAttr;
Begin
  For iX := 0 To Macros.Count - 1 Do
  Begin
    oMacro := TDicomMacro(Macros.ValueByIndex[iX]);
    For iY := 0 To oMacro.Children.Count - 1 Do
    Begin
      If (oMacro.Children[iY] is TDicomInfoEntryAttr) Then
      Begin
        oAttr := TDicomInfoEntryAttr(oMacro.Children[iY]);
    StringSplit(oAttr.Tag, ',', sGroup, sElement);
        If (Elements.FindByTags(sGroup, sElement, iIndex)) Then
          oAttr.RefElement := Elements[iIndex].Link
        Else
          oMissingRefs.Add(oAttr.Tag);
      End;
    End;
  End;

  For iX := 0 To Modules.Count - 1 Do
  Begin
    oModule := TDicomModule(Modules.ValueByIndex[iX]);
    For iY := 0 To oModule.Children.Count - 1 Do
    Begin
      If (oModule.Children[iY] is TDicomInfoEntryAttr) Then
      Begin
        oAttr := TDicomInfoEntryAttr(oModule.Children[iY]);
    StringSplit(oAttr.Tag, ',', sGroup, sElement);
        If (Elements.FindByTags(sGroup, sElement, iIndex)) Then
          oAttr.RefElement := Elements[iIndex].Link
        Else
          oMissingRefs.Add(oAttr.Tag);
      End;
    End;
  End;
End;

Procedure TDicomDictionary.ResolveIODModuleReferences(oMissingRefs: TFslStringList);
Var
  oTemp: TFslStringObjectMatch;
  iX, iY: Integer;
  oIOD: TDicomIOD;
  oModule: TDicomModule;
  oModuleRef: TDicomModuleReference;
Begin
  oTemp := TFslStringObjectMatch.Create;
  Try
    // first, we create a module's name -> key map
    For iX := 0 To Modules.Count - 1 Do
    Begin
      oModule := TDicomModule(Modules.ValueByIndex[iX]);
      oTemp.Add(oModule.Name, oModule.Link);
    End;

    // resolve reference
    For iX := 0 To IODs.Count - 1 Do
    Begin
      oIOD := TDicomIOD(IODs.ValueByIndex[iX]);
      For iY := 0 To oIOD.Modules.Count - 1 Do
      Begin
        oModuleRef := oIOD.Modules[iY];
        If oTemp.ExistsByKey(oModuleRef.Name) Then
        Begin
          oModuleRef.RefEntry := TDicomModule(oTemp.GetValueByKey(oMoDuleRef.Name)).Link;
          oModuleRef.Key := oModuleRef.RefEntry.Key;
        End
        Else
          oMissingRefs.Add(oModuleRef.Name);
      End;
    End;
  Finally
    oTemp.Free;
  End;
End;

Procedure TDicomDictionary.ResolveSOPModuleReferences(oMissingRefs: TFslStringList);
Var
  iSop : integer;
Begin
  for iSop := 0 to FSOPs.Count - 1 Do
  Begin
    If FIODs.ExistsByKey(FSops[iSop].IODRef) Then
      FSops[iSop].IOD := FIODs.Entry[FSops[iSop].IODRef].Link
    else if FSops[iSop].IODRef <> '' Then
      oMissingRefs.Add('IOD "'+FSops[iSop].Name+'": '+FSops[iSop].IODRef);
  End;
End;

Function TDicomDictionary.GetVRByType(Const aType: TDicomVRType): TDicomDictionaryVR;
Begin
  Result := FVRSet[aType];
End;

Function TDicomDictionary.GetVRByTypeCode(Const sCode: String): TDicomDictionaryVR;
Begin
  Result := FVRSet[DetermineVRType(sCode)];
End;

Function TDicomDictionary.GetVRTypeCodes: TFslStringList;
Var
  oCodes: TFslStringList;
  i: TDicomVRType;
Begin
  oCodes := TFslStringList.Create;
  Try
    For i := Low(TDicomVRType) To High(TDicomVRType) Do
      oCodes.Add(DICOM_VR_TYPE_NAMES_S[i]);

    Result := oCodes.Link;
  Finally
    oCodes.Free;
  End;
End;

function TDicomDictionary.AbstractSyntaxName(uid: String): String;
var
  iIndex : integer;
begin
  iIndex := SOPs.IndexByUID(uid);
  if iIndex = -1 Then
    result := 'unknown'
  Else
    result := SOPs[iIndex].Name;
end;

function TDicomDictionary.TransferSyntaxName(uid: String): String;
var
  iIndex : integer;
begin
  iIndex := TransferSyntaxes.IndexByUID(uid);
  if iIndex = -1 Then
    result := 'unknown'
  Else
    result := TransferSyntaxes[iIndex].Name;
end;

function TDicomDictionary.FindMatchingElementForTag(const sTag: String; bErrorIfNotFound: Boolean): TDicomDictionaryElement;
var
  sGroup, sElement : String;
  i : integer;
Begin
  StringSplit(sTag, ',', sGroup, sElement);
  result := FElements.GetByTags(sGroup, sElement);
  if result = nil Then
  Begin
    for i := 0 to FWildcardElements.Count -1 Do
    Begin
      If Matches(FWildcardElements[i].GroupId, sGroup) and Matches(FWildcardElements[i].ElementId, sElement) Then
      Begin
        result := FWildcardElements[i];
        exit;
      End;
    End;
  End;
  if bErrorIfNotFound and (result = nil) Then
    if FName = '' Then
      RaiseError('FindMatchingElementForTag', 'The dictionary does not contain a definition for the element ('+sGroup+'/'+sElement+')')
    Else
      RaiseError('FindMatchingElementForTag', 'The dictionary "'+Name+'" does not contain a definition for the element ('+sGroup+'/'+sElement+')');
end;

function TDicomDictionary.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FSectionReferences.sizeInBytes);
  inc(result, FTransferSyntaxes.sizeInBytes);
  inc(result, FElements.sizeInBytes);
  inc(result, FElementGroups.sizeInBytes);
  inc(result, FWildcardElements.sizeInBytes);
  inc(result, FDimses.sizeInBytes);
  inc(result, FMacros.sizeInBytes);
  inc(result, FModules.sizeInBytes);
  inc(result, FIODs.sizeInBytes);
  inc(result, FSOPs.sizeInBytes);
end;

{ TDicomDictionaryVR }

Procedure TDicomDictionaryVR.Assign(oSource: TFslObject);
Begin
  Inherited;
  FCode := TDicomDictionaryVR(oSource).Code;
  FLength := TDicomDictionaryVR(oSource).Length;
  FDescription := TDicomDictionaryVR(oSource).Description;
  FLength := TDicomDictionaryVR(oSource).Length;
  FFixed := TDicomDictionaryVR(oSource).Fixed;
End;

Function TDicomDictionaryVR.Clone: TDicomDictionaryVR;
Begin
  Result := TDicomDictionaryVR(Inherited Clone);
End;

Constructor TDicomDictionaryVR.Create(aType : TDicomVRType; sCode: String; iLength: Cardinal; bFixed, bRepeatable: Boolean; sDescription, sDoco: String);
Begin
  Create;
  FType := aType;
  FCode := sCode;
  FLength := iLength;
  FFixed := bFixed;
  FRepeatable := bRepeatable;
  FDescription := sDescription;
  FDoco := sDoco;
End;


Function TDicomDictionaryVR.IsString: Boolean;
Begin
  Result := FType in [dvtCS,dvtSH,dvtLO,dvtST,dvtLT,dvtUT,dvtAE,dvtDA,dvtTM,dvtDT,dvtAS,dvtIS,dvtDS,dvtUI];
End;

Function TDicomDictionaryVR.Link: TDicomDictionaryVR;
Begin
  Result := TDicomDictionaryVR(Inherited Link);
End;


function TDicomDictionaryVR.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, (FDoco.length * sizeof(char)) + 12);
end;

{ TDicomDictionaryElementList }

function TDicomDictionaryElementList.Clone: TDicomDictionaryElementList;
begin
  result := TDicomDictionaryElementList(Inherited Clone);
end;

function TDicomDictionaryElementList.CompareByTags(pA, pB: Pointer): Integer;
begin
  Result := StringCompare(TDicomDictionaryElement(pA).FGroupId, TDicomDictionaryElement(pB).FGroupId);
  if result = 0 Then
    Result := StringCompare(TDicomDictionaryElement(pA).FElementId, TDicomDictionaryElement(pB).FElementId);
end;

constructor TDicomDictionaryElementList.Create;
begin
  inherited;
  SortedByTags;
end;

procedure TDicomDictionaryElementList.DefaultCompare(out aEvent: TFslItemListCompare);
begin
  aEvent := CompareByTags;
end;

function TDicomDictionaryElementList.ExistsByTags(const sGroup, sElement: String): Boolean;
begin
  Result := ExistsByIndex(IndexByTags(sGroup, sElement));
end;

function TDicomDictionaryElementList.FindByTags(const sGroup, sElement: String; out iIndex: Integer): Boolean;
Var
  oElement : TDicomDictionaryElement;
Begin
  oElement := TDicomDictionaryElement(ItemNew);
  Try
    oElement.FGroupId := sGroup;
    oElement.FElementId := sElement;

    Result := Find(oElement, iIndex, CompareByTags);
  Finally
    oElement.Free;
  End;
end;

function TDicomDictionaryElementList.GetByTags(const sGroup, sElement: String): TDicomDictionaryElement;
Var
  iIndex : Integer;
Begin
  If FindByTags(sGroup, sElement, iIndex) Then
    Result := TDicomDictionaryElement(ObjectByIndex[iIndex])
  Else
    Result := Nil;
End;

function TDicomDictionaryElementList.IndexByTags(const sGroup, sElement: String): Integer;
begin
  If Not FindByTags(sGroup, sElement, Result) Then
    Result := -1;
end;

function TDicomDictionaryElementList.IsSortedByTags: Boolean;
begin
  Result := IsSortedBy(CompareByTags);
end;

function TDicomDictionaryElementList.ItemClass: TFslObjectClass;
begin
  result := TDicomDictionaryElement;
end;

function TDicomDictionaryElementList.Link: TDicomDictionaryElementList;
begin
  result := TDicomDictionaryElementList(Inherited Link);
end;

procedure TDicomDictionaryElementList.RemoveByTags(const sGroup, sElement: String);
Var
  iIndex : Integer;
Begin
  If Not FindByTags(sGroup, sElement, iIndex) Then
    RaiseError('RemoveByTags', StringFormat('Object (''%s'',''%s'') not found in list.', [sGroup, sElement]));

  DeleteByIndex(iIndex);
end;

procedure TDicomDictionaryElementList.SortedByTags;
begin
  SortedBy(CompareByTags);
end;


function TDicomDictionaryElementList.GetElements(iIndex: Integer): TDicomDictionaryElement;
begin
  Result := TDicomDictionaryElement(ObjectByIndex[iIndex]);
end;

Function TDicomDictionaryElementList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomDictionaryElementList.IndexOf(value: TDicomDictionaryElement): Integer;
Begin
  Result := Inherited IndexByReference(value);
End;

Function TDicomDictionaryElementList.Item(iIndex: Integer): TDicomDictionaryElement;
Begin
  Result := Elements[iIndex];
End;

{ TDicomDictionaryElement }

Procedure TDicomDictionaryElement.Assign(oSource: TFslObject);
Begin
  Inherited;
  FGroupId := TDicomDictionaryElement(oSource).FGroupId;
  FElementId := TDicomDictionaryElement(oSource).FElementId;
  FGroupName := TDicomDictionaryElement(oSource).FGroupName;
  FName := TDicomDictionaryElement(oSource).FName;
  FCode := TDicomDictionaryElement(oSource).FCode;

  FVRs.Assign(TDicomDictionaryElement(oSource).FVRs);
  FMin := TDicomDictionaryElement(oSource).FMin;
  FMax := TDicomDictionaryElement(oSource).FMax;
End;

Function TDicomDictionaryElement.AsString: String;
Begin
  result := Name;
End;

Function TDicomDictionaryElement.Clone: TDicomDictionaryElement;
Begin
  result := TDicomDictionaryElement(Inherited Clone);
End;


Constructor TDicomDictionaryElement.Create;
Begin
  Inherited;
  FVRs := TDicomDictionaryVRList.Create;
End;

Destructor TDicomDictionaryElement.Destroy;
Begin
  FVRs.Free;
  Inherited;
End;

Function TDicomDictionaryElement.IsSequence: Boolean;
Begin
  result := VRs.Count = 0;
End;

Function TDicomDictionaryElement.Link: TDicomDictionaryElement;
Begin
  result := TDicomDictionaryElement(Inherited Link);
End;



Function TDicomDictionaryElement.VRTypes: TDicomVRTypes;
Var
  i : integer;
Begin
  result := [];
  For i := 0 To VRs.Count - 1 Do
    result := result + [VRs[i].VRType];
End;

function TDicomDictionaryElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FGroupId.length * sizeof(char)) + 12);
  inc(result, (FElementId.length * sizeof(char)) + 12);
  inc(result, (FGroupName.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, FVRs.sizeInBytes);
end;

{ TDicomPresentationContext }
Constructor TDicomPresentationContext.Create(
  Const sAbstractSyntaxUID: String; contextType: TDicomPresContextType);
Begin
  Inherited Create;

  FAbstractSyntaxUID := sAbstractSyntaxUID;
  FContextType := contextType;
  FTransferSyntaxList := TDicomDictionaryTransferSyntaxList.Create;
End;

Procedure TDicomPresentationContext.Assign(oSource: TFslObject);
Begin
  Inherited;

  FAbstractSyntaxUID := TDicomPresentationContext(oSource).FAbstractSyntaxUID;
  FContextType := TDicomPresentationContext(oSource).FContextType;

  FTransferSyntaxList.Assign(TDicomPresentationContext(oSource).FTransferSyntaxList);
End;

Function TDicomPresentationContext.Clone: TDicomPresentationContext;
Begin
  Result := TDicomPresentationContext(Inherited Clone);
End;


Function TDicomPresentationContext.Link: TDicomPresentationContext;
Begin
  Result := TDicomPresentationContext(Inherited Link);
End;

Destructor TDicomPresentationContext.Destroy;
Begin
  FTransferSyntaxList.Free;
  
  Inherited;
End;

function TDicomPresentationContext.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FAbstractSyntaxUID.length * sizeof(char)) + 12);
  inc(result, FTransferSyntaxList.sizeInBytes);
end;

{ TDicomPresentationContextList }

Function TDicomPresentationContextList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomPresentationContextList.GetPresentationContext(
  iIndex: integer): TDicomPresentationContext;
Begin
  result := TDicomPresentationContext(Inherited GetByIndex(iIndex));
End;

Function TDicomPresentationContextList.IndexOf(value: TDicomPresentationContext): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomPresentationContextList.Item(iIndex: Integer): TDicomPresentationContext;
Begin
  Result := PresentContextes[iIndex];
End;

Function TDicomPresentationContextList.ItemClass: TFslObjectClass;
Begin
  Result := TDicomPresentationContext;
End;

{ TDicomDimseMessage }
Constructor TDicomDimseMessage.Create;
Begin
  Inherited Create;

  FParams := TDicomDimseParamList.Create;
End;

Procedure TDicomDimseMessage.Assign(oSource: TFslObject);
Begin
  Inherited;

  FName := TDicomDimseMessage(oSource).FName;
  FParams.Assign(TDicomDimseMessage(oSource).FParams);
End;

Function TDicomDimseMessage.Clone: TDicomDimseMessage;
Begin
  Result := TDicomDimseMessage(Inherited Clone);
End;

Function TDicomDimseMessage.Link: TDicomDimseMessage;
Begin
  Result := TDicomDimseMessage(Inherited Link);
End;

Destructor TDicomDimseMessage.Destroy;
Begin
  FParams.Free;

  Inherited;
End;

function TDicomDimseMessage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FParams.sizeInBytes);
end;

{ TDicomDictionaryVRList }

Function TDicomDictionaryVRList.Clone: TDicomDictionaryVRList;
Begin
  result := TDicomDictionaryVRList(inherited Clone);
End;

Function TDicomDictionaryVRList.GetVR(iIndex: integer): TDicomDictionaryVR;
Begin
  result := TDicomDictionaryVR(ObjectByIndex[iIndex]);
End;

Function TDicomDictionaryVRList.ItemClass: TFslObjectClass;
Begin
  result := TDicomDictionaryVR;
End;

Function TDicomDictionaryVRList.Link: TDicomDictionaryVRList;
Begin
  result := TDicomDictionaryVRList(inherited Link);
End;

Function TDicomDictionaryVRList.ItemVR(iIndex: Integer): TDicomDictionaryVR;
Begin
  Result := VRs[iIndex];
End;

{ TDicomDimseService }

Procedure TDicomDimseService.Assign(oSource: TFslObject);
Begin
  Inherited;

  FServiceType := TDicomDimseService(oSource).FServiceType;
End;

Function TDicomDimseService.Clone: TDicomDimseService;
Begin
  Result := TDicomDimseService(Inherited Clone);
End;

Function TDicomDimseService.Count: Integer;
Begin
  Result := Inherited Count;
End;

Constructor TDicomDimseService.Create(ServiceType: TDicomDimseType);
Begin
  Inherited Create;

  FServiceType := ServiceType;
End;

Function TDicomDimseService.GetDimseMessage(iIndex: integer): TDicomDimseMessage;
Begin
  Result := TDicomDimseMessage(Inherited GetByIndex(iIndex));
End;

Function TDicomDimseService.IndexOf(value: TDicomDimseMessage): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomDimseService.Item(iIndex: Integer): TDicomDimseMessage;
Begin
  Result := DimseMessage[iIndex];
End;

Function TDicomDimseService.ItemClass: TFslObjectClass;
Begin
  Result := TDicomDimseMessage;
End;

Function TDicomDimseService.Link: TDicomDimseService;
Begin
  Result := TDicomDimseService(Inherited Link);
End;

{ TDicomInfoEntry }

Constructor TDicomInfoEntry.Create;
Begin
  Inherited;

  FChildren := TDicomInfoEntryList.Create;
End;

Destructor TDicomInfoEntry.Destroy;
Begin
  FChildren.Free;

  Inherited;
End;

Procedure TDicomInfoEntry.Assign(oSource: TFslObject);
Begin
  Inherited;

  FName := TDicomInfoEntry(oSource).FName;
  FComment := TDicomInfoEntry(oSource).FComment;

  FChildren.Assign(TDicomInfoEntryAttr(oSource).FChildren);
End;

Function TDicomInfoEntry.Clone: TDicomInfoEntry;
Begin
  Result := TDicomInfoEntry(Inherited Clone);
End;

Function TDicomInfoEntry.Link: TDicomInfoEntry;
Begin
  Result := TDicomInfoEntry(Inherited Link);
End;

function TDicomInfoEntry.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FChildren.sizeInBytes);
  inc(result, (FComment.length * sizeof(char)) + 12);
end;

{ TDicomInfoEntryList }

Function TDicomInfoEntryList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomInfoEntryList.GetEntry(iIndex: integer): TDicomInfoEntry;
Begin
  Result := TDicomInfoEntry(Inherited GetByIndex(iIndex));
End;

Function TDicomInfoEntryList.IndexOf(value: TDicomInfoEntry): Integer;
Begin
  Result := Inherited IndexByReference(value);
End;

Function TDicomInfoEntryList.Item(iIndex: Integer): TDicomInfoEntry;
Begin
  Result := Entry[iIndex]; 
End;

Function TDicomInfoEntryList.ItemClass: TFslObjectClass;
Begin
  Result := TDicomInfoEntry;
End;

{ TDicomInfoEntryReference }

Procedure TDicomInfoEntryReference.Assign(oSource: TFslObject);
Begin
  Inherited;

  FRefId := TDicomInfoEntryReference(oSource).FRefId;
  FBaselineContextId := TDicomInfoEntryReference(oSource).FBaselineContextId;
  FContextId := TDicomInfoEntryReference(oSource).FContextId;
  FBaselineTemplateId := TDicomInfoEntryReference(oSource).FBaselineTemplateId;
  FDefinedTemplateId := TDicomInfoEntryReference(oSource).FDefinedTemplateId;

  SetRefEntry(TDicomInfoEntryReference(oSource).FRefEntry);
End;

Function TDicomInfoEntryReference.Clone: TDicomInfoEntryReference;
Begin
  Result := TDicomInfoEntryReference(Inherited Clone);
End;

Destructor TDicomInfoEntryReference.Destroy;
Begin
  FRefEntry.Free;
  FRefEntry := Nil;

  Inherited;
End;

Function TDicomInfoEntryReference.Link: TDicomInfoEntryReference;
Begin
  Result := TDicomInfoEntryReference(Inherited Link);
End;

Procedure TDicomInfoEntryReference.SetRefEntry(sValue: TDicomInfoGroup);
Begin
  FRefEntry.Free;
  FRefEntry := sValue;
End;

Procedure TDicomInfoEntryReference.SetRefId(sValue: String);
Begin
  FRefId := StringUpper(StringTrimWhitespace(sValue));
End;

function TDicomInfoEntryReference.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FRefId.length * sizeof(char)) + 12);
  inc(result, FRefEntry.sizeInBytes);
  inc(result, (FContextId.length * sizeof(char)) + 12);
  inc(result, (FBaselineContextId.length * sizeof(char)) + 12);
  inc(result, (FDefinedTemplateId.length * sizeof(char)) + 12);
  inc(result, (FBaselineTemplateId.length * sizeof(char)) + 12);
end;

{ TDicomInfoEntryAttr }

Function TDicomInfoEntryAttr.Clone: TDicomInfoEntryAttr;
Begin
  Result := TDicomInfoEntryAttr(Inherited Clone);
End;


Function TDicomInfoEntryAttr.Link: TDicomInfoEntryAttr;
Begin
  Result := TDicomInfoEntryAttr(Inherited Link);
End;

Procedure TDicomInfoEntryAttr.Assign(oSource: TFslObject);
Begin
  Inherited;

  FTag := TDicomInfoEntryAttr(oSource).FTag;
  FRequiredType := TDicomInfoEntryAttr(oSource).FRequiredType;

  FRefElement := TDicomInfoEntryAttr(oSource).FRefElement.Link;
End;

Destructor TDicomInfoEntryAttr.Destroy;
Begin
  FRefElement.Free;

  Inherited;
End;

Procedure TDicomInfoEntryAttr.SetRefElement(oValue: TDicomDictionaryElement);
Begin
  FRefElement.Free;
  FRefElement := oValue;
End;

function TDicomInfoEntryAttr.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FTag.length * sizeof(char)) + 12);
  inc(result, FRefElement.sizeInBytes);
end;

{ TDicomInfoGroup }

Constructor TDicomInfoGroup.Create;
Begin
  Inherited;

  FChildren := TDicomInfoEntryList.Create;
End;

Destructor TDicomInfoGroup.Destroy;
Begin
  FChildren.Free;

  Inherited;
End;

Function TDicomInfoGroup.Clone: TDicomInfoGroup;
Begin
  Result := TDicomInfoGroup(Inherited Clone);
End;

Function TDicomInfoGroup.Link: TDicomInfoGroup;
Begin
  Result := TDicomInfoGroup(Inherited Link);
End;

Procedure TDicomInfoGroup.Assign(oSource: TFslObject);
Begin
  Inherited;

  FKey := TDicomInfoGroup(oSOurce).FKey;
  FName := TDicomInfoGroup(oSOurce).FName;
  Comment := TDicomInfoGroup(oSOurce).FComment;
  FChildren.Assign(TDicomInfoGroup(oSOurce).FChildren);
End;

Procedure TDicomInfoGroup.SetKey(sValue: String);
Begin
  FKey := StringUpper(StringTrimWhitespace(sValue));
End;

function TDicomInfoGroup.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FKey.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FComment.length * sizeof(char)) + 12);
  inc(result, FChildren.sizeInBytes);
end;

{ TDicomMacroDict }

Function TDicomMacroDict.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomMacroDict.GetMacro(sKey: String): TDicomMacro;
Begin
  Result := TDicomMacro(Inherited GetValueByKey(sKey));
End;

Function TDicomMacroDict.IndexOf(value: TDicomMacro): Integer;
Begin
  Result := IndexByValue(value);
End;

Function TDicomMacroDict.ItemMacro(iIndex: Integer): TDicomMacro;
Begin
  Result := TDicomMacro(ValueByIndex[iIndex]);
End;

Function TDicomMacroDict.ItemClass: TFslObjectClass;
Begin
  Result := TDicomMacro;
End;

function TDicomMacroDict.FindMacroByName(name: String): TDicomMacro;
var
  i : integer;
begin
  result := nil;
  i := 0;
  while (i < Count) and (result = nil) do
  Begin
    if SameText(ItemMacro(i).Name, name) Then
      result := ItemMacro(i);
    inc(i);
  End;
end;

{ TDicomModuleDict }

Function TDicomModuleDict.GetModule(sKey: String): TDicomModule;
Begin
  Result := TDicomModule(Inherited GetValueByKey(sKey));
End;

Function TDicomModuleDict.ItemModule(iIndex: Integer): TDicomModule;
Begin
  Result := TDicomModule(ValueByIndex[iIndex]);
End;

Function TDicomModuleDict.ItemClass: TFslObjectClass;
Begin
  Result := TDicomModule;
End;

function TDicomModuleDict.FindModuleByName(name: String): TDicomModule;
var
  i : integer;
begin
  result := nil;
  i := 0;
  while (i < Count) and (result = nil) do
  Begin
    if SameText(ItemModule(i).Name, name) Then
      result := ItemModule(i);
    inc(i);
  End;
end;

{ TDicomModuleReference }

Procedure TDicomModuleReference.Assign(oSource: TFslObject);
Begin
  Inherited;

  FName := TDicomModuleReference(oSource).FName;
  FKey := TDicomModuleReference(oSource).FKey;
  FUsageType := TDicomModuleReference(oSource).FUsageType;
  FSectionReference := TDicomModuleReference(oSource).FSectionReference;
  FComment := TDicomModuleReference(oSource).FComment;

  SetRefEntry(TDicomModuleReference(oSource).FRefEntry);
End;

Function TDicomModuleReference.Clone: TDicomModuleReference;
Begin
  Result := TDicomModuleReference(Inherited Clone);
End;

Destructor TDicomModuleReference.Destroy;
Begin
  FRefEntry.Free;

  Inherited;
End;

Function TDicomModuleReference.Link: TDicomModuleReference;
Begin
  Result := TDicomModuleReference(Inherited Link);
End;

Procedure TDicomModuleReference.SetRefEntry(sValue: TDicomModule);
Begin
  FRefEntry.Free;
  FRefEntry := sValue;
End;

function TDicomModuleReference.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FKey.length * sizeof(char)) + 12);
  inc(result, FRefEntry.sizeInBytes);
  inc(result, (FSectionReference.length * sizeof(char)) + 12);
  inc(result, (FComment.length * sizeof(char)) + 12);
end;

{ TDicomModuleReferenceList }

Function TDicomModuleReferenceList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomModuleReferenceList.GetReference(iIndex: integer): TDicomModuleReference;
Begin
  Result := TDicomModuleReference(ObjectByIndex[iIndex]);
End;

Function TDicomModuleReferenceList.IndexOf(value: TDicomModuleReference): Integer;
Begin
  Result := IndexByReference(value);
End;

Function TDicomModuleReferenceList.Item(iIndex: Integer): TDicomModuleReference;
Begin
  Result := References[iIndex]; 
End;

Function TDicomModuleReferenceList.ItemClass: TFslObjectClass;
Begin
  Result := TDicomModuleReference;
End;

{ TDicomIOD }

Constructor TDicomIOD.Create;
Begin
  Inherited;

  FModules := TDicomModuleReferenceList.Create;
End;

Destructor TDicomIOD.Destroy;
Begin
  FModules.Free;
  FModules := Nil;

  Inherited;
End;

Procedure TDicomIOD.Assign(oSource: TFslObject);
Begin
  Inherited;

  FName := TDicomIOD(oSource).FName;
  FKey := TDicomIOD(oSource).FKey;

  FModules.Assign(TDicomIOD(oSource).FModules);
End;

Function TDicomIOD.Clone: TDicomIOD;
Begin
  Result := TDicomIOD(Inherited Clone);
End;

Function TDicomIOD.Link: TDicomIOD;
Begin
  Result := TDicomIOD(Inherited Link);
End;

function TDicomIOD.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FKey.length * sizeof(char)) + 12);
  inc(result, FModules.sizeInBytes);
end;

{ TDicomIODDict }

Function TDicomIODDict.Count: Integer;
Begin
  Result := Inherited Count;
End;

function TDicomIODDict.FindIODByName(name: String): TDicomIOD;
var
  i : integer;
begin
  result := nil;
  i := 0;
  while (i < Count) and (result = nil) do
  Begin
    if SameText(Item(i).Name, name) Then
      result := Item(i);
    inc(i);
  End;
end;

{
function TDicomIODDict.FindEntry(sKey: String): TDicomIOD;
begin
  if ExistsByKey(sKey) then
    result := GetEntry(sKey)
  Else
    result := nil;
end;
 }
 
Function TDicomIODDict.GetEntry(sKey: String): TDicomIOD;
Begin
  Result := TDicomIOD(Inherited GetValueByKey(sKey));
End;

Function TDicomIODDict.IndexOf(value: TDicomIOD): Integer;
Begin
  Result := IndexByValue(value);
End;

Function TDicomIODDict.Item(iIndex: Integer): TDicomIOD;
Begin
  Result := TDicomIOD(ValueByIndex[iIndex]);
End;

Function TDicomIODDict.ItemClass: TFslObjectClass;
Begin
  Result := TDicomIOD;
End;

{ TDicomModule }

Function TDicomModule.Clone: TDicomModule;
Begin
  Result := TDicomModule(Inherited Clone);
End;

Destructor TDicomModule.Destroy;
Begin
  Inherited;
End;

Function TDicomModule.Link: TDicomModule;
Begin
  Result := TDicomModule(Inherited Link);
End;

{ TDicomDictionarySOP }

procedure TDicomDictionarySOP.Assign(oSource: TFslObject);
begin
  inherited;
  FStatedIOD := TDicomDictionarySOP(oSource).FStatedIOD;
  FIODRef := TDicomDictionarySOP(oSource).FIODRef;
  FName := TDicomDictionarySOP(oSource).FName;
  IOD := TDicomDictionarySOP(oSource).FIOD.Link;
end;

function TDicomDictionarySOP.Clone: TDicomDictionarySOP;
begin
  result := TDicomDictionarySOP(Inherited Clone);
end;

destructor TDicomDictionarySOP.Destroy;
begin
  FIOD.Free;
  inherited;
end;

function TDicomDictionarySOP.Link: TDicomDictionarySOP;
begin
  result := TDicomDictionarySOP(Inherited Link);
end;

procedure TDicomDictionarySOP.SetIOD(const Value: TDicomIOD);
begin
  FIOD.Free;
  FIOD := Value;
end;


function TDicomDictionarySOP.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FStatedIOD.length * sizeof(char)) + 12);
  inc(result, (FIODRef.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FIOD.sizeInBytes);
end;

{ TDicomDictionarySOPList }

Function TDicomDictionarySOPList.Clone: TDicomDictionarySOPList;
Begin
  Result := TDicomDictionarySOPList(Inherited Clone);
End;

Function TDicomDictionarySOPList.ItemSOP(iIndex: Integer): TDicomDictionarySOP;
Begin
  Result := TDicomDictionarySOP(ObjectByIndex[iIndex]);
End;

Function TDicomDictionarySOPList.GetDictionarySOPByUID(const sUID: String): TDicomDictionarySOP;
Begin
  result := TDicomDictionarySOP(Inherited GetByUid(sUid));
End;

Function TDicomDictionarySOPList.GetSOP(iIndex: integer): TDicomDictionarySOP;
Begin
  result := TDicomDictionarySOP(Inherited ObjectByIndex[iIndex]);
End;

Function TDicomDictionarySOPList.ItemClass: TFslObjectClass;
Begin
  result := TDicomDictionarySOP;
End;

Function TDicomDictionarySOPList.Link: TDicomDictionarySOPList;
Begin
  Result := TDicomDictionarySOPList(Inherited Link);
End;

{ TDicomMacro }

Destructor TDicomMacro.Destroy;
Begin
  Inherited;
End;

{ TDicomDictionaryElementGroupList }

Function TDicomDictionaryElementGroupList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomDictionaryElementGroupList.Item(iIndex: Integer): TDicomDictionaryElementList;
Begin
  Result := TDicomDictionaryElementList(ValueByIndex[iIndex]);
End;

Function TDicomDictionaryElementGroupList.GetElementGroup(sKey: String): TDicomDictionaryElementList;
Begin
  Result := TDicomDictionaryElementList(Inherited GetValueByKey(sKey));
End;

Function TDicomDictionaryElementGroupList.ItemClass: TFslObjectClass;
Begin
  Result := TDicomDictionaryElementList;
End;

{ TDicomDimseServiceDict }

Function TDicomDimseServiceDict.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomDimseServiceDict.GetDimseService(sKey: String): TDicomDimseService;
Begin
  Result := TDicomDimseService(Inherited GetValueByKey(sKey));
End;

Function TDicomDimseServiceDict.Item(iIndex: Integer): TDicomDimseService;
Begin
  Result := TDicomDimseService(ValueByIndex[iIndex]);
End;

Function TDicomDimseServiceDict.ItemClass: TFslObjectClass;
Begin
  Result := TDicomDimseService;
End;

{ TDicomDimseParam }

Procedure TDicomDimseParam.Assign(oSource: TFslObject);
Begin
  Inherited;

  FName := TDicomDimseParam(oSource).FName;
  FFixedValue := TDicomDimseParam(oSource).FFixedValue;
  FParamType  := TDicomDimseParam(oSource).FParamType;

  FElementId := TDicomDimseParam(oSource).FElementId;
  SetElement(TDicomDimseParam(oSource).Element.Link);
End;

Function TDicomDimseParam.Clone: TDicomDimseParam;
Begin
  Result := TDicomDimseParam(Inherited Clone);
End;

Constructor TDicomDimseParam.Create;
Begin
  Inherited;
End;

Destructor TDicomDimseParam.Destroy;
Begin
  FElement.Free;
  
  Inherited;
End;

function TDicomDimseParam.Link: TDicomDimseParam;
Begin
  Result := TDicomDimseParam(Inherited Link);
End;

Procedure TDicomDimseParam.SetElement(oValue: TDicomDictionaryElement);
Begin
  FElement.Free;
  FElement := oValue;
End;

function TDicomDimseParam.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FFixedValue.length * sizeof(char)) + 12);
  inc(result, (FElementId.length * sizeof(char)) + 12);
  inc(result, FElement.sizeInBytes);
end;

{ TDicomDimseParamList }

Function TDicomDimseParamList.Clone: TDicomDimseParamList;
Begin
  Result := TDicomDimseParamList(Inherited Clone);
End;

Function TDicomDimseParamList.Count: Integer;
Begin
  Result := Inherited Count;
End;

Function TDicomDimseParamList.GetDicomParam(iIndex: Integer): TDicomDimseParam;
Begin
  Result := TDicomDimseParam(ObjectByIndex[iIndex]);
End;

Function TDicomDimseParamList.IndexOf(value: TDicomDimseParam): Integer;
Begin
  Result := Inherited IndexByReference(value);
End;

Function TDicomDimseParamList.Item(iIndex: Integer): TDicomDimseParam;
Begin
  Result := DimseParams[iIndex];
End;

Function TDicomDimseParamList.ItemClass: TFslObjectClass;
Begin
  Result := TDicomDimseParam;
End;

Function TDicomDimseParamList.Link: TDicomDimseParamList;
Begin
  Result := TDicomDimseParamList(Inherited Link);
End;

Function ValueIsCompatible(Const sValue : String; aTypes : TDicomVRTypes) : Boolean;
var
  aLoop : TDicomVRType;
  s : String;
Begin
  result := False;
  for aLoop := Low(TDicomVRType) to High(TDicomVRType) Do
  Begin
    if (aLoop in aTypes) and not result Then
      case aLoop of
        dvtCS, dvtSH, dvtAE : result := length(sValue) <= 16;
        dvtLO : result := length(sValue) <= 64;
        dvtST : result := length(sValue) <= 1024;
        dvtLT : result := length(sValue) <= 10240;
        dvtUT : result := true;
        dvtPN : result := (Pos('^', sValue) > 0) and (length(s) < 64);
        dvtUI : result := IsOid(sValue) and (length(sValue) < 64);
        dvtDA : result := CheckDateFormat('yyyymmdd', sValue, s);
        dvtTM : result := CheckDateFormat('hhnnss', sValue, s);
        dvtDT : result := CheckDateFormat('yyyymmddhhnnss', sValue, s);
        dvtAS : result := false;
        dvtIS : result := StringIsInteger64(sValue);
        dvtSS : result := StringIsInteger16(sValue);
        dvtUS : result := StringIsInteger16(sValue) and (pos('-', sValue) = 0);
        dvtSL : result := StringIsInteger32(sValue);
        dvtUL : result := StringIsInteger32(sValue) and (pos('-', sValue) = 0);
        dvtAT : result := false;
        dvtDS,dvtFL,dvtFD : result := IsNumericString(sValue);
        dvtOB,dvtOW,dvtOF,dvtUN : result := true;
      End;
  End;
End;



Procedure CheckTag(const sLocation, sValue, sTag : String; bWantInteger : Boolean);
var
  i : integer;
Begin
  if length(sTag) <> 4 Then
    raise EDicomException.create('Error in '+sLocation+':'+ sValue+' is not valid tag');
  for i := 1 to 4 do
    if bWantInteger and not CharInSet(sTag[i], ['0'..'9', 'A'..'F', 'a'..'f']) Then
      raise EDicomException.create('Error in '+sLocation+':'+ sValue+' is not valid tag')
    Else if not CharInSet(sTag[i], ['0'..'9', 'A'..'F', 'a'..'f', 'x']) Then
      raise EDicomException.create('Error in '+sLocation+':'+ sValue+' is not valid tag');
End;


Procedure ReadTagIds(Const sLocation, sValue : String; var sGroup, sElement : String);
Begin
  StringSplit(sValue, ',', sGroup, sElement);
  CheckTag(sLocation, sValue, sGroup, false);
  CheckTag(sLocation, sValue, sElement, false);
End;

Procedure ReadTagValues(Const sLocation, sValue : String; var iGroup, iElement : Word);
var
  sGroup, sElement : String;
Begin
  StringSplit(sValue, ',', sGroup, sElement);
  CheckTag(sLocation, sValue, sGroup, true);
  CheckTag(sLocation, sValue, sElement, true);
  iGroup := StrToInt('$'+sGroup);
  iElement := StrToInt('$'+sElement);
End;

Function FormatTagValues(iGroup, iElement : Word) : String;
Begin
  result := IntToHex(iGroup, 4)+','+ IntToHex(iElement, 4);
End;

Function NameDicomCommand(iCommand : Integer) : String;
Begin
  case iCommand of
    1 : result := 'C-Store';
  Else
    result := inttostr(iCommand);
  End;
End;

End.

