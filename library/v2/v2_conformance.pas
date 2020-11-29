Unit v2_conformance;

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

// HCP = HL7 Conformance Profile
{$I fhir.inc}

Interface

Uses
  SysUtils, Classes, Graphics,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_xml,
  v2_base, v2_dictionary, v2_objects;

Type
  THCPValidateEventClass = (
                             eHCP_Information,
                             eHCP_Warning,
                             eHCP_Error,
                             eHCP_Fatal
                            );
  TOid = String;
  THCPImpNote = String;
  THCPHL7Version = String;
  THCPEncodings = TFslStringList;

Const
  HCP_ERR_COLOR : Array [THCPValidateEventClass] Of TColour =
                            (
                             clGray,
                             clBlack,
                             clMaroon,
                             clRed
                            );

  GHCP_VALIDATE_EVENT_CLASSSTRING : Array[THCPValidateEventClass] Of String =
                            (
                             'Inform',
                             'Warning',
                             'Error',
                             'Fatal'
                            );

Type

  // enumerated constants
  THCPUsage = (hcpuR,    // Required (must always be present);
               hcpuRE,   // Required or Empty (must be present if available);
               hcpuO,    // Optional (no guidance on when the element should appear);
               hcpuC,    // Conditional (the element is required or allowed to be present when the condition specified in the Predicate element is true);
               hcpuCE,   // Conditional or Empty (the element is required or allowed to be present when the condition specified in the Predicate element is true and the information is available)
               hcpuX,    // Not supported (the element will not be sent)
               hcpuB);   // ?

  THCPRoleType = (hcprtSender, hcprtReceiver);

  THCPMsgAckModeType = (hcpaImmediate, hcpaDeferred);
  THCPQueryMsgType = (hcpqmNonQuery, hcpqmQuery, hcpqmResponse, hcpqmPublish);
  THCPQueryModeType = (hcpqmBatch, hcpqmRealTime, hcpqmBoth);
  THCPAcknowledgmentType = (hcpaAL, hcpaNE, hcpaSU, hcpaER);

  THCPProfileType = (hcpptHL7,              // represents a specific HL7 published standard (may only be submitted by the HL7 Organization)
                     hcpptConstrainable,    // May contain "Optional" elements which must be further constrained in order to create implementation profiles
                     hcpptImplementation);  // Fully constrained with no optionality (reflects the behavior of a runtime system)

  THCPElement = Class (THL7V2WorkerObject)
  Public
    Procedure Clear; Virtual;
  End;

  THCPObjects           = Class (TFslObjectList);
  THCPIterator          = Class (TFslObjectListIterator);
  THCPPropertyElement   = Class;
  THCPNode              = Class;
  THCPSpecification     = Class;

  THCPValidateEvent = Class(THCPElement)
  Private
    FMsg: String;
    FEventClass: THCPValidateEventClass;
    Function GetEventClassName: String;
    function GetAsString: String;
  Protected
    function sizeInBytesV : cardinal; override;
  Public
    Procedure Assign(oSource : TFslObject); Override;
    Property EventClass   : THCPValidateEventClass      Read FEventClass Write FEventClass;
    Property Msg          : String                      Read FMsg        Write FMsg;
    Property EventClassName : String                    Read GetEventClassName;

    Property AsString : String read GetAsString;
  End;

 THCPValidateEventIterator = Class(THCPIterator);

 THCPValidateEvents = Class(THCPObjects)
 Private
   FContext : THCPSpecification;
   FHL7Message : THL7V2Message;
   FErrorCollector : THL7V2ErrorCollector;
   Function GetHCPValidateEvent(AIndex: Integer): THCPValidateEvent;
    function GetAsString: String;
 Protected
   Function ItemClass:TFslObjectClass; Override;
    function sizeInBytesV : cardinal; override;
 Public
   Function  Iterator : TFslIterator; Override;
   Function  RecordError(Const AClass : THCPValidateEventClass; Const APath,AMsg : String; Const AElement: THL7V2BaseObject = Nil):Integer;
   Property  ValidateEvent[AIndex : Integer]:THCPValidateEvent Read GetHCPValidateEvent; Default;
   Property  ErrorCollector : THL7V2ErrorCollector Read FErrorCollector Write FErrorCollector;
   Property  HL7Message : THL7V2Message Read FHL7Message Write FHL7Message;
   Property AsString : String read GetAsString;
 End;

  THCPProperty = Class (THCPElement)
  Private
    FOwner : THCPPropertyElement;
    FValue: String;
    FName: String;
    Procedure SetValue(Const AValue: String);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create(AOwner : THCPPropertyElement);
    Procedure Assign(oSource : TFslObject); Override;

    Property Owner : THCPPropertyElement Read FOwner Write FOwner;
    Property Name : String Read FName;
    Property Value : String Read FValue Write SetValue;
  End;

  THCPPropIterator = Class (THCPIterator)
  Public
    Function Current : THCPProperty; Reintroduce; Overload; Virtual;
  End;

  THCPPropList = Class (THCPObjects)
  Private
    Function GetProp(iIndex: Integer): THCPProperty;
  Protected
    Function ItemClass : TFslObjectClass;  Override;
  Public
    Function Iterator : TFslIterator;  Override;

    Property Props[iIndex : Integer] : THCPProperty Read GetProp; Default;
  End;

  // to establish a node in the GUI
  THCPProperties = Class(THCPElement)
  Private
    FPropList: THCPPropList;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Assign(oSource : TFslObject); Override;
    Procedure Clear; Override;

    Property PropList : THCPPropList Read FPropList;
  End;

  THCPPropertyElement = Class (THCPElement)
  Private
    FProperties : THCPProperties;
    Procedure SetProp(AName, AValue : String);
    Function GetProp(AName : String) : String;
    Function GetEnumeratedProp(APath, AName : String; Const AValues : Array Of String; ADef : Integer = -1): Integer;
  Protected
    Procedure updateProperties; Virtual;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Assign(oSource : TFslObject); Override;
    Procedure  Clear; Override;
    Property Properties : THCPProperties Read FProperties;
  End;

  THCPNode = Class (THCPPropertyElement)
  Private
    FSequence : Integer;
    FPath : String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Property Sequence : Integer Read FSequence Write FSequence;
    Procedure Assign(oSource : TFslObject); Override;
    Property Path : String Read FPath;
 End;

  THCPList = Class (THCPObjects)
  Private
    Function GetNode(iIndex: Integer): THCPNode;
  Public
    Property Nodes[iIndex : Integer] : THCPNode Read GetNode; Default;
  End;

  THCPTableItem = Class (THCPNode)
  Private
    FOrder: Integer;
    FDescription: String;
    FInstruction: String;
    FUsage: THCPUsage;
    FDisplayName: String;
    FSource: String;
    FCode: String;
  Protected
    Procedure Read(AElement : TMXmlElement; sPath : String);
    Procedure updateProperties; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Procedure Assign(oSource : TFslObject); Override;
    Function  ValidItem(Const ACode : String;Out VCaseCorrect : Boolean; Out VTableCode : String):Boolean;
    Property Order : Integer Read FOrder;
    Property Code : String Read FCode;
    Property Description : String Read FDescription;
    Property DisplayName : String Read FDisplayName;
    Property Source : String Read FSource;
    Property Usage : THCPUsage Read FUsage;
    Property Instruction : String Read FInstruction;
  End;

  THCPTableIterator = Class (THCPIterator)
  Public
    Function Current : THCPTableItem; Reintroduce; Overload; Virtual;
  End;

  THCPTableItems = Class (THCPList)
  Protected
    Function ItemClass : TFslObjectClass;  Override;
  Public
    Function Iterator : TFslIterator;  Override;
  End;

  THCPTable = Class (THCPNode)
  Private
    FTableType: String;
    FCodeSys: String;
    FId: String;
    FName: String;
    FItems: THCPTableItems;
  Protected
    Procedure Read(AElement : TMXmlElement; sPath : String);
    Procedure updateProperties; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Assign(oSource : TFslObject); Override;
    Function  ValidItem(Const ACode: String;Out VCaseCorrect: Boolean; Out VTableCode : String): Boolean;
    Property Id : String Read FId;
    Property Name : String Read FName;
    Property CodeSys : String Read FCodeSys;
    Property TableType : String Read FTableType;
    Property Items : THCPTableItems Read FItems;
  End;

  THCPTables = Class (TFslStringObjectMatch)
  Private
    Procedure Read(AElement : TMXmlElement; sPath : String);
    Function GetTable(AId: String): THCPTable;
  Public
    Constructor Create; Override;
    Property Table[AId : String] : THCPTable Read GetTable;
  End;

  THCPCell = Class (THCPNode)
  Private
    FName : String;
    FUsage : THCPUsage;
    FDatatype : String;
    FLength : Integer;
    FTable : String;
    FConstantValue : String;
    FDataValues : TFslStringList;
    FPredicate : String;
    FImpNote : THCPImpNote;
    FDescription : String;
    FReference : String;

    Function GetRequired: Boolean;
    Function GetMustValidate: Boolean;
    Function HL7DictTable(oObj : THL7V2BaseObject; Const AVersion : THL7V2Version): THL7V2ModelTable;
  Protected
    Procedure Read(AElement : TMXmlElement; sPath : String); Virtual;
    Procedure updateProperties; Override;
    Procedure ValidateCell(Const AHL7CommonDataCell,AHL7CommonDataCellParent: THL7V2Cell; Const AFieldName : String; Const AHCPValidateEvents: THCPValidateEvents);
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Assign(oSource : TFslObject); Override;
    Function DisplayDetail : String;

    Property Name : String Read FName;  // The descriptive name for the field/component/subcomponent
    Property Usage : THCPUsage Read FUsage; // Usage identifies the circumstances under which an element appears in a message
    Property Datatype : String Read FDatatype; // Identifies the HL7 datatype associated with the element
    Property DataLength : Integer Read FLength; // Identifies the maximum allowed length for the content of the element
    Property Table : String Read FTable;  // Identifies the name {id} of the table associated with the content of this element
    Property ConstantValue : String Read FConstantValue; // Identifies the fixed value associated with this element
    Property DataValues : TFslStringList Read FDataValues; // Identifies individual example values
    Property Predicate : String Read FPredicate;  // Identifies the conditionality rule for this element, if applicable
    Property ImpNote : THCPImpNote Read FImpNote; // Implementation Notes provide a general description about how the element is intended to be used, as well as hints on using or interpreting the it.
    Property Description : String Read FDescription; // Provides an explanation or definition of what the element represents
    Property Reference : String Read FReference; // Identifies external sources or other locations within the profile where additional information can be found about this item

    Property Required: Boolean Read GetRequired;
    Property MustValidate: Boolean Read GetMustValidate;
  End;

  // Documents the characteristics of a single sub-component within the context of a component
  THCPSubComponent = Class (THCPCell)
  Protected
    Procedure ValidateSubComponent(Const AHL7SubComponent, AHL7Component : THL7V2Component; Const AFieldName : String; Const AHCPValidateEvents: THCPValidateEvents);
    Procedure Read(AElement : TMXmlElement; sPath : String); Override;
  End;

  THCPSubComponentIterator = Class(THCPIterator)
  Public
    Function Current : THCPSubComponent; Reintroduce; Overload; Virtual;
  End;

  THCPSubComponentList = Class (THCPList)
  Private
    Procedure Read(AElement : TMXmlElement; sPath : String);
    Function GetSubComponent(AIndex: Integer): THCPSubComponent;
  Protected
    Function ItemClass : TFslObjectClass;  Override;
  Public
    Function Iterator : TFslIterator;  Override;
    Function Defined(Const ASequence : Integer):Boolean;
    Property SubComponent[AIndex : Integer]:THCPSubComponent Read GetSubComponent; Default;
  End;

  // Documents the characteristics of a single component within the context of a field
  THCPComponent = Class (THCPCell)
  Private
    FSubComponents: THCPSubComponentList;
  Protected
    Procedure Read(AElement : TMXmlElement; sPath : String); Override;
    Procedure ValidateComponent(Const AHL7Component : THL7V2Component; Const AHL7DataElement: THL7V2DataElement; Const AFieldName : String; Const AHCPValidateEvents: THCPValidateEvents);
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Assign(oSource : TFslObject); Override;
    Property SubComponents : THCPSubComponentList Read FSubComponents;
  End;

  THCPComponentIterator = Class(THCPIterator)
  Public
    Function Current : THCPComponent; Reintroduce; Overload; Virtual;
  End;

  THCPComponentList = Class (THCPList)
  Private
    Procedure Read(AElement : TMXmlElement; sPath : String);
    Function GetComponent(AIndex: Integer): THCPComponent;
  Protected
    Function ItemClass : TFslObjectClass;  Override;
  Public
    Function Iterator : TFslIterator;  Override;
    Function Defined(Const ASequence : Integer):Boolean;
    Property Component[AIndex : Integer]:THCPComponent Read GetComponent; Default;
  End;

  // Documents the characteristics of a single HL7 field within the context of a particular message segment
  THCPField = Class (THCPCell)
  Private
    FItemNo: String;
    FMin : Integer;
    FMax : Integer;
    FComponents: THCPComponentList;
    Procedure ValidateBase(Const AHL7DataElement: THL7V2DataElement;Const ASegment: THL7V2Segment;Const AHCPValidateEvents: THCPValidateEvents);
    Procedure ValidateRepeat(Const AHL7DataElement: THL7V2DataElement;Const ASegment: THL7V2Segment; Const AHCPValidateEvents: THCPValidateEvents);
  Protected
    Procedure Read(AElement : TMXmlElement; sPath : String); Override;
    Procedure updateProperties; Override;
    Procedure ValidateField(Const AHL7DataElement: THL7V2DataElement; Const ASegment : THL7V2Segment; Const AHCPValidateEvents: THCPValidateEvents);
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Assign(oSource : TFslObject); Override;
    Property Components : THCPComponentList Read FComponents;

    Property Min : Integer Read FMin; // This identifies the minimum number of repetitions of the element that are permitted in a message instance. This attribute should only be specified if the minimum number of repetitions is greater than 1, as the minimum for other elements is always '0'
    Property Max : Integer Read FMax; // This identifies the maximum number of repetitions of the element that are permitted in a message instance. This attribute should only be specified if the maximum number of repetitions is greater than 1 and differs from the minimum attribute (i.e. the maximum number of repetitions is greater than the minimum number of repetitions). The special value '*' may be used to represent 'unlimited' repetitions
    Property ItemNo: String Read FItemNo; // The HL7-assigned item number corresponding with the semantic meaning of the field
  End;

  THCPFieldIterator = Class(THCPIterator)
  Public
    Function Current : THCPField; Reintroduce; Overload; Virtual;
  End;

  THCPFieldList = Class (THCPList)
  Private
    Procedure Read(AElement : TMXmlElement; sPath : String);
    Function GetField(AIndex: Integer): THCPField;
  Protected
    Function ItemClass : TFslObjectClass;  Override;
  Public
    Function Iterator : TFslIterator;  Override;
    Function Defined(Const ASequence : Integer):Boolean;
    Property Field[AIndex : Integer]:THCPField Read GetField; Default;
  End;

  THCPSegGroup = Class;
  THCPSegmentBase = Class (THCPNode)
  Private
    // internal
    FOwnerGroup  : THCPSegGroup;            // Used during validation - no need to clone
    FNumberofRepeatsFound : Integer;        // Used during validation - no need to clone

    // defined
    FPredicate : String;
    FImpNote : THCPImpNote;
    FDescription : String;
    FReference : String;
    FName : String;
    FLongName : String;
    FUsage : THCPUsage;
    FMin : Integer;
    FMax : Integer;

    Function GetRequired: Boolean;
  Protected
    Procedure Read(AElement : TMXmlElement; sPath : String); Virtual;
    Procedure updateProperties; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Function StillRequired: Boolean; Virtual;
    Function StillPossible: Boolean;
    Procedure OwnerGroupisNowRequired;
    Procedure Assign(oSource : TFslObject); Override;

    Property OwnerGroup : THCPSegGroup Read FOwnerGroup Write FOwnerGroup;
    Property Required   : Boolean Read GetRequired;
    Property NumberofRepeatsFound : Integer Read FNumberofRepeatsFound Write FNumberofRepeatsFound;

    // from schema
    Property Predicate : String Read FPredicate; // Identifies the conditionality rule for this element, if applicable
    Property ImpNote : THCPImpNote Read FImpNote; // Implementation Notes provide a general description about how the element is intended to be used, as well as hints on using or interpreting the it.
    Property Description : String Read FDescription; // Provides an explanation or definition of what the element represents
    Property Reference : String Read FReference; // Identifies external sources or other locations within the profile where additional information can be found about this item
    Property Name : String Read FName; // This is the short, formal name for the segment. It is used to identify the segment in both ER7 and XML encodings
    Property LongName : String Read FLongName; // This is the descriptive name for the element. It does not appear in any encodings
    Property Usage : THCPUsage Read FUsage; //Usage identifies the circumstances under which an element appears in a message.
    Property Min : Integer Read FMin; // This identifies the minimum number of repetitions of the element that are permitted in a message instance. This attribute should only be specified if the minimum number of repetitions is greater than 1, as the minimum for other elements is always '0'
    Property Max : Integer Read FMax; // This identifies the maximum number of repetitions of the element that are permitted in a message instance. This attribute should only be specified if the maximum number of repetitions is greater than 1 and differs from the minimum attribute (i.e. the maximum number of repetitions is greater than the minimum number of repetitions). The special value '*' may be used to represent 'unlimited' repetitions
  End;

  THCPSegmentBaseIterator = Class(THCPIterator)
  Public
    Function Current : THCPSegmentBase; Reintroduce; Overload; Virtual;
  End;

  THCPSegmentBaseList = Class (THCPList)
  Private
    Procedure Read(AElement : TMXmlElement; sPath : String);
    Function GetSegmentBase(Const AIndex: Integer): THCPSegmentBase;
  Protected
    Function ItemClass : TFslObjectClass;  Override;
  Public
    Function Iterator : TFslIterator;  Override;
    Property SegmentBase[Const AIndex : Integer] : THCPSegmentBase Read GetSegmentBase; Default;
  End;

  THCPSegment = Class (THCPSegmentBase)
  Private
    FFields: THCPFieldList;
  Protected
    Procedure Read(AElement : TMXmlElement; sPath : String); Override;
    Procedure ValidateSegment(Const ASegment : THL7V2Segment; Const AHCPValidateEvents : THCPValidateEvents);
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Assign(oSource : TFslObject); Override;
    Property  Fields : THCPFieldList Read FFields;
  End;

  // Documents the characteristics of a grouping of HL7 segments within the context of a particular message or segment group
  THCPSegGroup = Class (THCPSegmentBase)
  Private
    FNowRequired : Boolean;
    FSegments    : THCPSegmentBaseList;
  Protected
    Procedure Read(AElement : TMXmlElement; sPath : String); Override;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor  Destroy; Override;
    Function StillRequired: Boolean; Override;
    Procedure Assign(oSource : TFslObject); Override;
    Property Segments : THCPSegmentBaseList Read FSegments;
    Property NowRequired : Boolean Read FNowRequired Write FNowRequired;
  End;

  THCPMetaData = Class (THCPNode)
  Private
    FName : String; // Provides a name that clearly and concisely defines the message exchange being profiled.</xs:documentation>
    FOrgName : String; // Name of the organization that submitted the profile.</xs:documentation>
    FVersion : String; // The version identifier assigned to this profile by the author. There is no prescribed version numbering scheme. However 'higher' versions should generally be interpreted to be more resent.</xs:documentation>
    FStatus : String; // Status of this profile, as assigned by the author. There is no prescribed status scheme at this time. Possible values might include: 'Draft', 'Active', 'Superceded', 'Withdrawn'</xs:documentation>
    FTopics : String; // This provides a list of key-words that relate to the profile and that may be useful in profile searches.</xs:documentation>
    Procedure Read(AElement : TMXmlElement; sPath : String);
  Protected
    Procedure updateProperties; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Procedure Assign(oSource : TFslObject); Override;
    Property Name : String Read FName;
    Property OrgName : String Read FOrgName;
    Property Version : String Read FVersion;
    Property Status : String Read FStatus;
    Property Topics : String Read FTopics;
  End;

  // This represents a detailed profile of a single message. It provides a detailed breakdown of exactly what the message may contain, including optionality and cardinality
  THCPStaticDef = Class (THCPNode)
  Private
    FMsgType : String;
    FEventType : String;
    FMsgStructID : String;
    FOrderControl : String;
    FEventDesc : String;
    FIdentifier : String;
    FRole : THCPRoleType;
    FMetaData : THCPMetaData;
    FImpNote : THCPImpNote;
    FDescription : String;
    FReference : String;
    FSegments : THCPSegmentBaseList;

    Procedure Read(AElement : TMXmlElement; sPath : String);
    Procedure InitialiseHCPSegmentsforvalidation(Const ASegments : THCPSegmentBaseList; Const AOwnerGroup : THCPSegGroup);

    Function  ValidateMessageProperty(Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Out VErrMsg : String):Boolean;
    Function  ValidateMessageSegments(Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Out VErrMsg : String):Boolean;

    Function  ValidateHCPSegment(Const AHCPSegment : THCPSegment; Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Var VHL7MessageSegmentCounter : Integer; Out VErrMsg : String):Boolean;
    Function  ValidateSegmentList(Const ASegments : THCPSegmentBaseList; Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Var VHL7MessageSegmentCounter : Integer; Out VErrMsg : String):Boolean;
    Function  ValidateHCPSegmentGroup(Const AHCPSegGroup : THCPSegGroup; Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Var VHL7MessageSegmentCounter : Integer; Out VErrMsg : String):Boolean;
    Procedure ResetNumberofRepeatsFound(Const AHCPSegGroup: THCPSegGroup);
  Protected
    Procedure updateProperties; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure  Clear; Override;
    Procedure Assign(oSource : TFslObject); Override;

    // from schema
    Property MsgType : String Read FMsgType;    // The HL7 message type code, as identified in MSH-9.1 (see HL7 Table 0076 - Message type).
    Property EventType : String Read FEventType; // The HL7 event type code, as identified in MSH-9.2 (see HL7 Table 0003 - Event type)
    Property MsgStructID : String Read FMsgStructID; // The HL7 message structure code, as identified in MSH-9.3 (see HL7 Table 0354 - Message Structure Type)
    Property OrderControl : String Read FOrderControl; // The HL7 Order control code, as identified in ORC 1 (see HL7 Table 0119 - Order Control Codes).
    Property EventDesc : String Read FEventDesc; // A description of the event carried by this message.
    Property Identifier : String Read FIdentifier; // A unique identifier for this specific version of this static definition. If not specified, one will be assigned to the profile upon submission to a registry.
    Property Role : THCPRoleType Read FRole; // Identifies whether the profile is constructed from the perspective of the message generator (Sender) or parser (Receiver). Default is 'Sender'
    Property MetaData : THCPMetaData Read FMetaData; // Provides descriptive information about the life-cycle of the HL7 v2x Static Definition, as well as authorship and control information
    Property ImpNote : THCPImpNote Read FImpNote; // Implementation Notes provide a general description about how the element is intended to be used, as well as hints on using or interpreting the it.
    Property Description : String Read FDescription; // Provides an explanation or definition of what the element represents
    Property Reference : String Read FReference; // Identifies external sources or other locations within the profile where additional information can be found about this item

    Property Segments : THCPSegmentBaseList Read FSegments;
  End;

  THCPStaticDefIterator = Class(THCPIterator)
  Public
    Function Current : THCPStaticDef; Reintroduce; Overload; Virtual;
  End;

  THCPStaticDefList = Class (THCPList)
  Private
    Function GetMessage(Const AIndex: Integer): THCPStaticDef;
  Protected
    Function ItemClass : TFslObjectClass;  Override;
  Public
    Function Iterator : TFslIterator;  Override;
    Property Message[Const AIndex : Integer] : THCPStaticDef Read GetMessage; Default;
  End;

  THCPUseCaseElement = Class (THCPPropertyElement)
  Private
    FName : String; // The unique name or number associated with a particular use-case element
  Protected
    Procedure updateProperties; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create(sName : String);
  End;

  // Identifies and defines the entities involved in the use-case. This includes the sending and receiving applications
  THCPActor = Class (THCPUseCaseElement);
  // Identifies a circumstance that must hold true prior to the use-case being invoked.
  THCPPreCondition = Class (THCPUseCaseElement);
  // Identifies a circumstance that will hold true after the successful completion of the usecase
  THCPPostCondition = Class (THCPUseCaseElement);
  // Identifies a step within the chain of occurrences that lead to the successful completion of the use-case. This includes the exchange of messages between applications
  THCPEventFlow = Class (THCPUseCaseElement);
  // no doco?
  THCPDerivedEvent = Class (THCPUseCaseElement);

  THCPUseCaseElementIterator = Class(THCPIterator)
  Public
    Function Current : THCPUseCaseElement; Reintroduce; Overload; Virtual;
  End;

  THCPUseCaseElementList = Class (THCPList)
  Private
    Function GetUseCase(Const AIndex: Integer): THCPUseCaseElement;
  Protected
    Function ItemClass : TFslObjectClass;  Override;
  Public
    Function Iterator : TFslIterator;  Override;
    Property UseCase[Const AIndex : Integer] : THCPUseCaseElement Read GetUseCase; Default;
  End;

  // A use case model documents the scope and requirements for an HL7 message profile or set of message profiles.
  THCPUseCase = Class (THCPNode)
  Private
    FPurpose : String; //Identifies the reason and/or objectives for the usecase
    FDescription : String;   // Descriptive text for the use-case. In cases where the use-case is not broken down into component elements, this will include the complete details of the usecase. Otherwise, it will contain a basic overview
    FElements : THCPUseCaseElementList;
    Procedure Read(AElement : TMXmlElement; sPath : String);
  Protected
    Procedure updateProperties; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Property Purpose : String Read FPurpose; // Identifies the reason and/or objectives for the usecase
    Property Description : String Read FDescription; // Descriptive text for the use-case. In cases where the usecase is not broken down into component elements, this will include the complete details of the usecase. Otherwise, it will contain a basic overview.
    Property Elements : THCPUseCaseElementList Read FElements;
  End;

  // The dynamic definition is an interaction specification for a conversation between 2 or more systems
  THCPDynamicDef = Class (THCPNode)
  Private
    FAccAck : THCPAcknowledgmentType;
    FAppAck : THCPAcknowledgmentType;
    FMsgAckMode : THCPMsgAckModeType;
    FQueryMessageType : THCPQueryMsgType;
    FQueryMode : THCPQueryModeType;
    FStaticDefs : THCPStaticDefList;
    Procedure Read(AElement : TMXmlElement; sPath : String);
  Protected
    Procedure updateProperties; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure  Clear; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Property AccAck : THCPAcknowledgmentType Read FAccAck; // Identifies when and if HL7 'Accept' acknowledgments are required. Allowed values are: AL (always), NE (never), SU (on success), ER (on error). Default is 'NE'
    Property AppAck : THCPAcknowledgmentType Read FAppAck; // Identifies when and if HL7 'Application' acknowledgments are required. Allowed values are: AL (always), NE (never), SU (on success), ER (on error). Default is 'AL'
    Property MsgAckMode : THCPMsgAckModeType Read FMsgAckMode; // Identifies the type of acknowledgment expected by the sender of a message. Allowed values are: Immediate and Deferred. Default is Immediate
    Property QueryMessageType : THCPQueryMsgType Read FQueryMessageType; // Identifies whether the message is query-related, and if so, what type of query message it is. Allowed values are: NonQuery, Query, Response and Publish. Default is NonQuery
    Property QueryMode : THCPQueryModeType Read FQueryMode; // Identifies the type of query being performed. Allowed values are: Batch, RealTime or Both
    Property Messages : THCPStaticDefList Read FStaticDefs;
  End;

  THCPDynamicDefIterator = Class(THCPIterator)
  Public
    Function Current : THCPDynamicDef; Reintroduce; Overload; Virtual;
  End;

  THCPDynamicDefList = Class (THCPList)
  Private
    Function GetDynamicDef(Const AIndex: Integer): THCPDynamicDef;
    Procedure Read(AElement : TMXmlElement; sPath : String);
  Protected
    Function ItemClass : TFslObjectClass;  Override;
  Public
    Function Iterator : TFslIterator;  Override;
    Property DynamicDef[Const AIndex : Integer] : THCPDynamicDef Read GetDynamicDef; Default;
  End;

  THCPSpecification = Class (THCPPropertyElement)   // An unambiguous specification of one or more standard HL7 messages that have been analyzed for a particular use case. It prescribes a set of precise constraints upon one or more standard HL7 messages
  Private
    FHL7Version: THCPHL7Version;
    FProfileType : THCPProfileType;
    FIdentifier : TOid;
    FMetaData : THCPMetaData;
    FImpNote : THCPImpNote;
    FUseCase : THCPUseCase;
    FEncodings : THCPEncodings;
    FDynamicDefs : THCPDynamicDefList;
    FTables : THCPTables;

    Procedure Read(AElement : TMXmlElement);
  Protected
    Procedure updateProperties; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure  Clear; Override;
    Procedure Assign(oSource : TFslObject); Override;

    Function ValidateMessageOk(Const AMsg: THL7V2Message; Out VHCPValidateEvents : THCPValidateEvents;Const AErrorCollector : THL7V2ErrorCollector = Nil):Boolean;
    Property Tables : THCPTables Read FTables;

    // from HCP schema
    Property HL7Version: THCPHL7Version Read FHL7Version; // Identifies the HL7 2.x version on which the profile is based and with which it is expected to comply.
    Property ProfileType : THCPProfileType Read FProfileType; // Categorizes the profile into one of 3 types: HL7 - represents a specific HL7 published standard (may only be submitted by the HL7 Organization); Constrainable - May contain "Optional" elements which must be further constrained in order to create implementation profiles; Implementation - Fully constrained with no optionality (reflects the behavior of a runtime system)</
    Property Identifier : TOid Read FIdentifier; // A unique identifier for this specific version of this dynamic profile. If not specified, one will be assigned to the profile upon submission to a registry.
    Property MetaData : THCPMetaData Read FMetaData; // Provides descriptive information about the life-cycle of the HL7v2xConformanceProfile, as well as authorship and control information
    Property ImpNote : THCPImpNote Read FImpNote; // Implementation Notes provide a general description about how the profile is intended to be used, as well as hints on using or interpreting the profile
    Property UseCase : THCPUseCase Read FUseCase; // A use case model documents the scope and requirements for an HL7 message profile or set of message profiles
    Property Encodings : THCPEncodings Read FEncodings; // Identifies all of the message encoding mechanisms supported by the profile. Non-traditional encoding mechanisms may be identified if desired
    Property DynamicDefs : THCPDynamicDefList Read FDynamicDefs; // The dynamic definition is an interaction specification for a conversation between 2 or more systems
  End;

Function LoadHCPSpec(oSource : TFslStream) : THCPSpecification; Overload;
Function LoadHCPSpec(const sFileName : String) : THCPSpecification; Overload;

Const
  HCP_Usage_VAL : Array [THCPUsage] Of String = ('R', 'RE', 'O', 'C', 'CE', 'X', 'B');

  HCP_RoleType_VAL : Array [THCPRoleType] Of String = ('Sender', 'Receiver');

  HCP_MsgAckModeType_VAL : Array [THCPMsgAckModeType] Of String = ('Immediate', 'Deferred');
  HCP_QueryMsgType_VAL : Array [THCPQueryMsgType] Of String = ('NonQuery', 'Query', 'Response', 'Publish');
  HCP_QueryModeType_VAL : Array [THCPQueryModeType] Of String = ('Batch', 'RealTime', 'Both');
  HCP_AcknowledgmentType_VAL : Array [THCPAcknowledgmentType] Of String = ('AL', 'NE', 'SU', 'ER');
  HCP_ProfileType_VAL : Array [THCPProfileType] Of String = ('HL7', 'Constrainable', 'Implementation');

Function MinToStr(iValue : Integer):String;
Function MaxToStr(iValue : Integer):String;
Function LengthToStr(iValue : Integer):String;

Implementation

{ utils }

Function ToStringWithError(Const sValue, sDesc : String):Integer;
Begin
  If Not StringIsInteger32(sValue) Then
    Raise EHL7V2Exception.Create(Nil, hecApplicationError, 'StringToInt', sDesc+' ['+sValue+'] is not an integer')
  Else
    Result := StringToInteger32(sValue);
End;

Function MinToStr(iValue : Integer):String;
Begin
  Result := IntegerToString(iValue);
End;

Function MaxToStr(iValue : Integer):String;
Begin
  If iValue = -1 Then
    Result := '*'
  Else
    Result := IntegerToString(iValue);
End;

Function LengthToStr(iValue : Integer):String;
Begin
  If iValue < 1 Then
    Result := ''
  Else
    Result := IntegerToString(iValue);
End;

Function ReadMax(sValue, sLocation : String) : Integer;
Begin
  If sValue = '' Then
    Result := 1
  Else If sValue = '*' Then
    Result := -1
  Else
    Result := ToStringWithError(sValue, 'Illegal Value for Max at '+sLocation);
End;

Function ReadLength(sValue, sLocation : String) : Integer;
Begin
  If sValue = '' Then
    Result := -1
  Else If sValue = '*' Then
    Result := -1
  Else
    Result := ToStringWithError(sValue, 'Illegal Value for Length at '+sLocation);
End;

Function ReadMin(sValue, sLocation : String) : Integer;
Begin
  If sValue = '' Then
    Result := 1
  Else
    Result := ToStringWithError(sValue, 'Illegal Value for Max at '+sLocation);
End;

{ THCPElement }

Procedure THCPElement.Clear;
Begin
End;

{ THCPValidateEvent }

Procedure THCPValidateEvent.Assign(oSource : TFslObject);
Begin
  Inherited;
  FMsg          := THCPValidateEvent(oSource).FMsg;
  FEventClass   := THCPValidateEvent(oSource).FEventClass;
End;

function THCPValidateEvent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMsg.length * sizeof(char)) + 12);
end;

function THCPValidateEvents.GetAsString: String;
Var
  iLoop : Integer;
begin
  Result := '';
  For iLoop := 0 to Count - 1 Do
  Begin
    if iLoop > 0 Then
      Result := Result + ', ';
    Result := Result + ValidateEvent[iLoop].asString;
  End;

end;

Function THCPValidateEvents.GetHCPValidateEvent(AIndex: Integer): THCPValidateEvent;
Begin
  Result := ObjectByIndex[AIndex] As THCPValidateEvent;
End;

Function THCPValidateEvents.ItemClass:TFslObjectClass;
Begin
  Result := THCPValidateEvent;
End;

Function THCPValidateEvents.Iterator: TFslIterator;
Begin
  Result := THCPValidateEventIterator.Create;
  THCPValidateEventIterator(Result).List := THCPValidateEvents(Self.Link);
End;

Function  THCPValidateEvents.RecordError(Const AClass : THCPValidateEventClass; Const APath,AMsg : String; Const AElement: THL7V2BaseObject = Nil):Integer;
Var
  LHCPValidateEvent : THCPValidateEvent;
Begin
  Result := 0;
  if AClass = eHCP_Information then
    exit;

  LHCPValidateEvent := THCPValidateEvent.Create;
  Try
    LHCPValidateEvent.EventClass := AClass;
//    if APath <> '' then
//       LHCPValidateEvent.Msg        := APath + ' "'+ AMsg + '"';
    LHCPValidateEvent.Msg := AMsg;
    Result := Add(LHCPValidateEvent.Link);
  Finally
    LHCPValidateEvent.Free;
    End;
  If Assigned(ErrorCollector) And Assigned(AElement) Then
    Begin
    ErrorCollector.RecordError(AElement,AMsg);
    End;
End;

function THCPValidateEvents.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContext.sizeInBytes);
  inc(result, FHL7Message.sizeInBytes);
  inc(result, FErrorCollector.sizeInBytes);
end;

Function THCPValidateEvent.GetEventClassName: String;
Begin
  Result := GHCP_VALIDATE_EVENT_CLASSSTRING[FEventClass];
End;

function THCPValidateEvent.GetAsString: String;
begin
  Result := GHCP_VALIDATE_EVENT_CLASSSTRING[FEventClass]+': '+FMsg;
end;

{ THCPProperty }

Procedure THCPProperty.Assign(oSource: TFslObject);
Begin
  Inherited;
  FValue  := THCPProperty(oSource).FValue;
  FName  := THCPProperty(oSource).FName;
End;

Constructor THCPProperty.Create(AOwner: THCPPropertyElement);
Begin
  Inherited Create;
  Assert(Invariants('v2_conformance', AOwner, THCPPropertyElement, 'AOwner'));
  FOwner := AOwner;
End;

Procedure THCPProperty.SetValue(Const AValue: String);
Var
  LOld : String;
Begin
  LOld := FValue;
  Try
    FValue := AValue;
    FOwner.updateProperties;
  Except
    FValue := LOld;
    Raise;
  End;
End;

function THCPProperty.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOwner.sizeInBytes);
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
end;

{ THCPPropIterator }

Function THCPPropIterator.Current: THCPProperty;
Begin
  Result := Inherited current As THCPProperty;
End;

{ THCPPropList }

Function THCPPropList.GetProp(iIndex: Integer): THCPProperty;
Begin
  Result := THCPProperty(ObjectByIndex[iIndex]);
End;

Function THCPPropList.ItemClass: TFslObjectClass;
Begin
  Result := THCPProperty;
End;

Function THCPPropList.Iterator: TFslIterator;
Begin
  Result := THCPPropIterator.Create;
  THCPPropIterator(Result).List := Self.Link;
End;

{ THCPProperties }

Procedure THCPProperties.Assign(oSource: TFslObject);
Begin
  Inherited;
  FPropList.Assign(THCPProperties(oSource).FPropList);
End;

Procedure THCPProperties.Clear;
Begin
  Inherited;
  FPropList.Clear;
End;

Constructor THCPProperties.Create;
Begin
  Inherited;
  FPropList := THCPPropList.Create;
End;

Destructor THCPProperties.Destroy;
Begin
  FPropList.Free;
  Inherited;
End;

function THCPProperties.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPropList.sizeInBytes);
end;

{ THCPPropertyElement }

Procedure THCPPropertyElement.Assign(oSource: TFslObject);
Var
  iLoop : Integer;
Begin
  Inherited;
  FProperties.Assign(THCPPropertyElement(oSource).FProperties);

  For iLoop := 0 To FProperties.PropList.Count - 1 Do
    FProperties.PropList[iLoop].Owner := Self;
End;

Procedure THCPPropertyElement.Clear;
Begin
  Inherited;
  FProperties.Clear;
  UpdateProperties;
End;

Constructor THCPPropertyElement.Create;
Begin
  Inherited Create;
  FProperties := THCPProperties.Create;
End;

Destructor THCPPropertyElement.Destroy;
Begin
  FProperties.Free;
  Inherited;
End;

Function THCPPropertyElement.GetProp(AName: String): String;
Var
  LIter : THCPPropIterator;
  LProp : THCPProperty;
Begin
  Result := '';
  LIter := FProperties.FPropList.Iterator As THCPPropIterator;
  Try
    LIter.First;
    While LIter.More Do
      Begin
      LProp := LIter.Current;
      If StringEquals(LProp.Name, AName) Then
        Begin
        Result := LProp.Value;
        Exit;
        End;
      LIter.Next;
      End;
  Finally
    LIter.Free;
  End;
End;

Function THCPPropertyElement.GetEnumeratedProp(APath, AName : String; Const AValues : Array Of String; ADef : Integer = -1) : Integer;
Var
  s : String;
  i : Integer;
Begin
  s := GetProp(AName);
  Result := ADef;
  For i := Low(AValues) To High(AValues) Do
    Begin
    If s = AValues[i] Then
      Begin
      Result := i;
      Exit;
      End;
    End;
  If Result = -1 Then
    Begin
    Assert(False, 'v2_conformance'+': no valid value for '+APath+'.'+AName+' - value is "'+s+'"');
    End;
End;

Procedure THCPPropertyElement.SetProp(AName, AValue: String);
Var
  LItem : THCPProperty;
Begin
  LItem := THCPProperty.Create(Self);
  Try
    LItem.FName := AName;
    LItem.FValue := AValue;
    FProperties.FPropList.Add(LItem.Link);
  Finally
    Litem.Free;
    End;
End;

Procedure THCPPropertyElement.updateProperties;
Begin
 // nothing
End;

function THCPPropertyElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FProperties.sizeInBytes);
end;

{ THCPNode }

Procedure THCPNode.Assign(oSource: TFslObject);
Begin
  Inherited;
  FPath := THCPNode(oSource).FPath;
  FSequence := THCPNode(oSource).FSequence;
End;

function THCPNode.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPath.length * sizeof(char)) + 12);
end;

{ THCPList }

Function THCPList.GetNode(iIndex: Integer): THCPNode;
Begin
  Result := ObjectByIndex[iIndex] As THCPNode;
End;

{ THCPTableItem }

Procedure THCPTableItem.Assign(oSource: TFslObject);
Begin
  Inherited;
  FOrder        := THCPTableItem(oSource).FOrder;
  FDescription  := THCPTableItem(oSource).FDescription;
  FInstruction  := THCPTableItem(oSource).FInstruction;
  FUsage        := THCPTableItem(oSource).FUsage;
  FDisplayName  := THCPTableItem(oSource).FDisplayName;
  FSource       := THCPTableItem(oSource).FSource;
  FCode         := THCPTableItem(oSource).FCode;
End;

Procedure THCPTableItem.Read(AElement: TMXmlElement; sPath : String);
Begin
  Assert(Self.Invariants('v2_conformance', THCPTableItem));

  SetProp('order', AElement.attribute['order']);
  SetProp('description', AElement.attribute['description']);
  SetProp('instruction', AElement.attribute['instruction']);
  SetProp('usage', AElement.attribute['usage']);
  SetProp('displayName', AElement.attribute['displayName']);
  SetProp('source', AElement.attribute['source']);
  SetProp('code', AElement.attribute['code']);
  updateProperties;
  FPath := sPath+'.code['+FCode+']';
End;

Procedure THCPTableItem.updateProperties;
Begin
  Inherited;
  FOrder := StrToIntDef(GetProp('order'), 0);
  FDescription := GetProp('description');
  FInstruction := GetProp('instruction');
  FUsage := THCPUsage(GetEnumeratedProp(FCode, 'usage', HCP_Usage_VAL));
  FDisplayName := GetProp('displayName');
  FSource := GetProp('source');
  FCode := GetProp('code');
End;

Function THCPTableItem.ValidItem(Const ACode: String;Out VCaseCorrect: Boolean; Out VTableCode : String): Boolean;
Begin
  If StringEquals(ACode,FCode) Then
    Begin
    Result       := True;
    VCaseCorrect := ACode = FCode;
    VTableCode   := FCode;
    End
  Else
    Begin
    Result       := False;
    End;
End;

function THCPTableItem.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, (FInstruction.length * sizeof(char)) + 12);
  inc(result, (FDisplayName.length * sizeof(char)) + 12);
  inc(result, (FSource.length * sizeof(char)) + 12);
  inc(result, (FCode.length * sizeof(char)) + 12);
end;

{ THCPTableIterator }

Function THCPTableIterator.Current: THCPTableItem;
Begin
  Result := THCPTableItem(Inherited current);
End;

{ THCPTableItems }

Function THCPTableItems.ItemClass: TFslObjectClass;
Begin
  Result := THCPTableItem;
End;

Function THCPTableItems.Iterator: TFslIterator;
Begin
  Result := THCPTableIterator.Create;
  THCPTableIterator(Result).List := Self.Link;
End;

{ THCPTable }

Procedure THCPTable.Assign(oSource: TFslObject);
Begin
  Inherited;
  FTableType := THCPTable(oSource).FTableType;
  FCodeSys := THCPTable(oSource).FCodeSys;
  FId := THCPTable(oSource).FId;
  FName := THCPTable(oSource).FName;
  FItems.Assign(THCPTable(oSource).FItems);
End;

Constructor THCPTable.Create;
Begin
  Inherited Create;
  FItems := THCPTableItems.Create;
End;

Destructor THCPTable.Destroy;
Begin
  FItems.Free;
  Inherited;
End;

Procedure THCPTable.Read(AElement: TMXmlElement; sPath : String);
Var
  LElem : TMXmlElement;
  LItem : THCPTableItem;
Begin
  Assert(Self.Invariants('v2_conformance', THCPTable));

  SetProp('type', AElement.attribute['type']);
  SetProp('codesys', AElement.attribute['codesys']);
  SetProp('id', AElement.attribute['id']);
  SetProp('name', AElement.attribute['name']);
  updateProperties;
  FPath := sPath+'.Table['+FId+']';

  LElem := AElement.firstElement;
  While Assigned(LElem) Do
    Begin
    If LElem.Name = 'tableElement' Then
       Begin
       LItem := THCPTableItem.Create;
       Try
         LItem.Read(LElem, Path);
         FItems.Add(LItem.Link);
       Finally
         LItem.UnLink;
       End;
       End;
    LElem := LElem.nextElement;
    End;
End;

Procedure THCPTable.updateProperties;
Begin
  Inherited;
  FTableType := GetProp('type');
  FCodeSys := GetProp('codesys');
  FId := GetProp('id');
  FName := GetProp('name');
End;

Function THCPTable.ValidItem(Const ACode: String;Out VCaseCorrect: Boolean; Out VTableCode : String): Boolean;
Var
  LIterator     : THCPTableIterator;
  LCaseCorrect  : Boolean;
  LTableCode    : String;
Begin
  Result        := False;
  LIterator     := Items.Iterator As THCPTableIterator;
  Try
    LIterator.First;
    While LIterator.More Do
      Begin
      If (LIterator.Current As THCPTableItem).ValidItem(ACode,LCaseCorrect,LTableCode) Then
        Begin
        Result     := True;
        VTableCode := LTableCode;
        If LCaseCorrect Then
          Begin
          VCaseCorrect := True;
          Exit;
          End;
        End;
      LIterator.Next;
      End;
  Finally
    LIterator.Free;
    End;
End;

function THCPTable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FTableType.length * sizeof(char)) + 12);
  inc(result, (FCodeSys.length * sizeof(char)) + 12);
  inc(result, (FId.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FItems.sizeInBytes);
end;

{ THCPTables }

Constructor THCPTables.Create;
Begin
  Inherited;
  Sorted;
  PreventDuplicates;
End;

Function THCPTables.GetTable(AId: String): THCPTable;
Var
  LIndex : Integer;
Begin
  Assert(Self.Invariants('v2_conformance', THCPTables));
  Assert(AId <> '', 'v2_conformance'+': Attempt to find an unidentified table');
  LIndex := IndexByKey(AId);
  If LIndex = -1 Then
    Begin
    Result := Nil;
    End
  Else
    Begin
    Result := ValueByIndex[LIndex] As THCPTable;
    End;
End;

Procedure THCPTables.Read(AElement: TMXmlElement; sPath : String);
Var
  LElem : TMXmlElement;
  LTable : THCPTable;
Begin
  LElem := AElement.FirstElement;
  While Assigned(LElem) Do
    Begin
    If (LElem.Name = 'hl7table') And (LElem.attribute['id'] <> '') Then
      Begin
      LTable := THCPTable.Create;
      Try
        LTable.Read(LElem, sPath);
        Add(LTable.Id, LTable.Link);
      Finally
        LTable.UnLink;
      End;
      End;
    LElem := LElem.nextElement;
    End;
End;

{ THCPCell }

Constructor THCPCell.Create;
Begin
  Inherited Create;
  FUsage := hcpuX;
  FDataValues := TFslStringList.Create;
End;

Destructor THCPCell.Destroy;
Begin
  FDataValues.Free;
  Inherited;
End;

Procedure THCPCell.Assign(oSource: TFslObject);
Var
  LSrc : THCPCell;
Begin
  Inherited;
  LSrc := oSource As THCPCell;
  FDataValues.Assign(LSrc.FDataValues);

  FName := LSrc.FName;
  FUsage := LSrc.FUsage;
  FDatatype := LSrc.FDatatype;
  FLength := LSrc.FLength;
  FTable := LSrc.FTable;
  FConstantValue := LSrc.FConstantValue;
  FPredicate := LSrc.FPredicate;
  FImpNote := LSrc.FImpNote;
  FDescription := LSrc.FDescription;
  FReference := LSrc.FReference;
End;

Function THCPCell.DisplayDetail: String;
Begin
  Assert(Self.Invariants('v2_conformance', THCPCell));
  Assert(FSequence > 0,'v2_conformance'+' negative sequences not supported');
  Result := IntegerToString(FSequence);
  Result := StringPadLeft(Result,'0', 3);
  Result := Result + ' "'+FName+'"';
End;

Function THCPCell.GetMustValidate: Boolean;
Begin
  Result := FUsage <> hcpuO;
End;

Function THCPCell.GetRequired: Boolean;
Begin
  Result := FUsage = hcpuR;
End;

Procedure THCPCell.Read(AElement: TMXmlElement; sPath : String);
Var
  LElem : TMXmlElement;
  elems : TFslList<TMXmlElement>;
Begin
  Inherited;
  SetProp('Name', AElement.attribute['Name']);
  SetProp('Usage', AElement.attribute['Usage']);
  SetProp('Datatype', AElement.attribute['Datatype']);
  SetProp('Length', AElement.attribute['Length']);
  SetProp('Table', AElement.attribute['Table']);
  SetProp('ConstantValue', AElement.attribute['ConstantValue']);

  If Assigned(AElement.element('Predicate')) Then
    SetProp('Predicate', AElement.element('Predicate').Text);
  If Assigned(AElement.element('ImpNote')) Then
    SetProp('ImpNote', AElement.element('ImpNote').Text);
  If Assigned(AElement.element('Description')) Then
    SetProp('Description', AElement.element('Description').Text);
  If Assigned(AElement.element('Reference')) Then
    SetProp('Reference', AElement.element('Reference').Text);

  elems := TFslList<TMXmlElement>.create;
  try
    AElement.listElements('DataValues', elems);
    for LElem in elems do
      FDataValues.Add(LElem.attribute['ExValue']);
  finally
    elems.Free;
  end;
End;

Procedure THCPCell.updateProperties;
Begin
  Inherited;
  FName := GetProp('Name');
  FUsage := THCPUsage(GetEnumeratedProp(Path, 'Usage', HCP_Usage_VAL));
  FDatatype := GetProp('Datatype');
  FLength := ReadLength(GetProp('Length'), 'v2_conformance'+': Length');
  FTable := GetProp('Table');
  FConstantValue := GetProp('ConstantValue');
  FPredicate := GetProp('Predicate');
  FImpNote := GetProp('ImpNote');
  FDescription := GetProp('Description');
  FReference := GetProp('Reference');
End;

Function  THCPCell.HL7DictTable(oObj : THL7V2BaseObject; Const AVersion : THL7V2Version):THL7V2ModelTable;
Var
  LTable : Integer;
  i : Integer;
  LHL7Dictionary : THL7V2Model;
Begin
  If Table = '' Then
    Begin
    Result := Nil;
    End
  Else
    Begin
    Val(Table,LTable,i);
    If i = 0 Then
      Begin
      LHL7Dictionary := oObj.Model;
      If Assigned(LHL7Dictionary) Then
        Begin
        Result := LHL7Dictionary.Tables.GetByID(LTable);
        End
      Else
        Begin
        Result := Nil;
        End;
      End
    Else
      Begin
      Result := Nil;
      End;
    End;
End;

Procedure THCPCell.ValidateCell(Const AHL7CommonDataCell, AHL7CommonDataCellParent: THL7V2Cell; Const AFieldName : String; Const AHCPValidateEvents: THCPValidateEvents);
Var
  LDefined : Boolean;
  LDataLen : Integer;
  LHCPTable : THCPTable;
  LCorrectCase : Boolean;
  LTableCode : String;
  LHL7Table : THL7V2ModelTable;
  LHL7CommonDataCell : THL7V2Cell;
Begin
  If Not MustValidate Then
    Begin
    Exit;
    End;

  If Assigned(AHL7CommonDataCell) Then
    Begin
    LDefined            := AHL7CommonDataCell.HasContent;
    LHL7CommonDataCell  := AHL7CommonDataCell;
    End
  Else
    Begin
    LDefined            := False;
    LHL7CommonDataCell  := AHL7CommonDataCellParent;
    End;

  If Not LDefined Then
    Begin
    If Required Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Required HL7 Message Cell '+AFieldName+' not found',LHL7CommonDataCell);
      End;
    Exit;
    End;
  If AFieldName = 'MSH-2' Then // we don't validate MSH-2 - it must be right to get this far but the internal structure is complicated
    Exit;

  LDataLen := Length(AHL7CommonDataCell.RawContent);
  If (DataLength > 0) And (LDataLen > DataLength) Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Error,Path,'HL7 Message Cell '+AFieldName+' Length is ' + IntegerToString(LDataLen) + ' but maximum allowed length is ' + IntegerToString(DataLength),AHL7CommonDataCell);
    End;

  If (ConstantValue <> '') And (ConstantValue <> AHL7CommonDataCell.AsString) Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Error,Path,'HL7 Message Cell '+AFieldName+' expected value ''' + ConstantValue + ''' but actual value is '''+AHL7CommonDataCell.AsString+'''',AHL7CommonDataCell);
    End;

  If (Table <> '') And (AHL7CommonDataCell.RawContent <> '') And StringEquals(Datatype, 'ID') Then
    Begin
    Try
      LHCPTable := AHCPValidateEvents.FContext.Tables.Table[Table];
    Except
      LHCPTable := Nil;
      End;
    If Assigned(LHCPTable) Then
      Begin
      If LHCPTable.ValidItem(AHL7CommonDataCell.RawContent,LCorrectCase,LTableCode) Then
        Begin
        If Not LCorrectCase Then
          Begin
          AHCPValidateEvents.RecordError(eHCP_Error,Path,'HL7 Message Cell '+AFieldName+' specification table '''+Table+''' has entry '''+LTableCode+''' which does not match the case of the value ''' + AHL7CommonDataCell.RawContent + '''',AHL7CommonDataCell);
          End;
        End
      Else
        Begin
        AHCPValidateEvents.RecordError(eHCP_Error,Path,'HL7 Message Cell '+AFieldName+' specification table '''+Table+''' does not contain value ''' + AHL7CommonDataCell.RawContent + '''',AHL7CommonDataCell);
        End;
      End
    Else
      Begin
      LHL7Table := HL7DictTable(AHL7CommonDataCell, AHCPValidateEvents.HL7Message.Version);
      If Assigned(LHL7Table) Then
        Begin
        If Not LHL7Table.Items.ExistsByCode(AHL7CommonDataCell.RawContent) Then
          Begin
          AHCPValidateEvents.RecordError(eHCP_Error,Path,'HL7 Message Cell '+AFieldName+' HL7 Dictionary table '''+Table+''' does not contain value ''' + AHL7CommonDataCell.RawContent + '''',AHL7CommonDataCell);
          End;
        End
      Else
        Begin
        AHCPValidateEvents.RecordError(eHCP_Error,Path,'HL7 Message Cell '+AFieldName+' HL7 Dictionary table '''+Table+''' was not found',AHL7CommonDataCell);
        End;
      End;
    End;
End;

function THCPCell.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FDatatype.length * sizeof(char)) + 12);
  inc(result, (FTable.length * sizeof(char)) + 12);
  inc(result, (FConstantValue.length * sizeof(char)) + 12);
  inc(result, FDataValues.sizeInBytes);
  inc(result, (FPredicate.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, (FReference.length * sizeof(char)) + 12);
end;

{ THCPSubComponent }

Procedure THCPSubComponent.Read(AElement: TMXmlElement; sPath: String);
Begin
  Assert(Self.Invariants('v2_conformance', THCPSubComponent));
  Inherited;
  updateProperties;
  FPath := sPath+'.SubComponent['+IntegerToString(FSequence)+']';
End;

Procedure THCPSubComponent.ValidateSubComponent(Const AHL7SubComponent,AHL7Component : THL7V2Component; Const AFieldName : String; Const AHCPValidateEvents: THCPValidateEvents);
Begin
  ValidateCell(AHL7SubComponent,AHL7Component,AFieldName,AHCPValidateEvents);
End;

{ THCPSubComponentIterator }

Function THCPSubComponentIterator.Current: THCPSubComponent;
Begin
  Result := THCPSubComponent(Inherited current);
End;

{ THCPSubComponentList }

Function THCPSubComponentList.Defined(Const ASequence: Integer): Boolean;
Var
  i : Integer;
Begin
  Result := False;
  For i := 0 To Count-1 Do
    Begin
    If SubComponent[i].Sequence = ASequence Then
      Begin
      Result := True;
      Exit;
      End;
    End;
End;

Function THCPSubComponentList.GetSubComponent(AIndex: Integer): THCPSubComponent;
Begin
  Result := ObjectByIndex[AIndex] As THCPSubComponent;
End;

Function THCPSubComponentList.ItemClass: TFslObjectClass;
Begin
  Result := THCPSubComponent;
End;

Function THCPSubComponentList.Iterator: TFslIterator;
Begin
  Result := THCPSubComponentIterator.Create;
  THCPSubComponentIterator(Result).List := Self.Link;
End;

Procedure THCPSubComponentList.Read(AElement: TMXmlElement; sPath : String);
Var
  LElem : TMXmlElement;
  LItem : THCPSubComponent;
Begin
  Assert(Self.Invariants('v2_conformance', THCPSubComponentList));

  LElem := AElement.firstElement;
  While Assigned(LElem) Do
    Begin
    If LElem.Name = 'SubComponent' Then
      Begin
      LItem := THCPSubComponent.Create;
      Try
        LItem.FSequence := count+1;
        LItem.Read(LElem, sPath);
        Add(LItem.Link);
      Finally
        LItem.Unlink;
      End;
      End;
    LElem := LELem.NextElement;
    End;
End;

{ THCPComponent }

Procedure THCPComponent.Assign(oSource: TFslObject);
Begin
  Inherited;
  FSubComponents.Assign(THCPComponent(oSource).FSubComponents);
End;

Constructor THCPComponent.Create;
Begin
  Inherited Create;
  FSubComponents := THCPSubComponentList.Create;
End;

Destructor THCPComponent.Destroy;
Begin
  FSubComponents.Free;
  Inherited;
End;

Procedure THCPComponent.Read(AElement: TMXmlElement; sPath : String);
Begin
  Assert(Self.Invariants('v2_conformance', THCPComponent));
  Inherited;
  updateProperties;
  FPath := sPath+'.Component['+IntegerToString(FSequence)+']';
  FSubComponents.Read(AElement, Path);
End;

Procedure THCPComponent.ValidateComponent(Const AHL7Component: THL7V2Component;Const AHL7DataElement: THL7V2DataElement; Const AFieldName : String; Const AHCPValidateEvents: THCPValidateEvents);
Var
  i                     : Integer;
  LHCPSubComponent      : THCPSubComponent;
  LHL7SubComponent      : THL7V2Component;
  LFieldName            : String;
Begin
  If Not MustValidate Then
    Begin
    Exit;
    End;
  LFieldName := AFieldName + '.' + IntegerToString(Sequence);
  If Not Assigned(AHL7Component) Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Error,Path,'Component '+LFieldname +' is not provided',AHL7DataElement);
    Exit;
    End;
  Try
    If (AHL7Component.Components.Count = 0) And (SubComponents.Count = 0) Then
      Begin
      ValidateCell(AHL7Component,AHL7Component,LFieldName,AHCPValidateEvents);
      End
    Else If (AHL7Component.Components.Count > 0) And ((SubComponents.Count = 0) And Not AHL7Component.isSimpleContent) Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Component '+LFieldName+' should not have subcomponents',AHL7Component);
      End
    Else If (AHL7Component.Components.Count = 0) And (SubComponents.Count > 0) Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Component '+LFieldName+' should have subcomponents but none are found',AHL7Component);
      End
    Else
      Begin
      For i := 0 To SubComponents.Count-1 Do
        Begin
        LHCPSubComponent := SubComponents.SubComponent[i];
        Try
          LHL7SubComponent := AHL7Component.Component[LHCPSubComponent.Sequence];
        Except
          LHL7SubComponent := Nil;
          End;
        LHCPSubComponent.ValidateSubComponent(LHL7SubComponent,AHL7Component,LFieldName,AHCPValidateEvents);
        End;

      // Validate each Component that appears is allowed
      For i := 1 To AHL7Component.Components.Count Do
        Begin
        If AHL7Component.Component[i].Defined Then
          Begin
          If Not FSubComponents.Defined(i) Then
            Begin
            AHCPValidateEvents.RecordError(eHCP_Error,Path,'Subcomponent '+LFieldName + '.' + IntegerToString(i) + ' is found but NOT defined in Specification',AHL7Component.Component[i]);
            End;
          End;
        End;
      End;
  Except
    On e:Exception Do
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Exception error while validating component '+LFieldName+ ' - ' + e.Message,AHL7Component);
      End;
    End;
End;

function THCPComponent.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSubComponents.sizeInBytes);
end;

{ THCPComponentIterator }

Function THCPComponentIterator.Current: THCPComponent;
Begin
  Result := THCPComponent(Inherited current);
End;

{ THCPComponentList }

Function THCPComponentList.Defined(Const ASequence: Integer): Boolean;
Var
  i : Integer;
Begin
  Result := False;
  For i := 0 To Count-1 Do
    Begin
    If Component[i].Sequence = ASequence Then
      Begin
      Result := True;
      Exit;
      End;
    End;
End;

Function THCPComponentList.GetComponent(AIndex: Integer): THCPComponent;
Begin
  Result := ObjectByIndex[AIndex] As THCPComponent;
End;

Function THCPComponentList.ItemClass: TFslObjectClass;
Begin
  Result := THCPComponent;
End;

Function THCPComponentList.Iterator: TFslIterator;
Begin
  Result := THCPComponentIterator.Create;
  THCPComponentIterator(Result).List := Self.Link;
End;

Procedure THCPComponentList.Read(AElement: TMXmlElement; sPath : String);
Var
  LElem : TMXmlElement;
  LItem : THCPComponent;
Begin
  Assert(Self.Invariants('v2_conformance', THCPComponentList));

  LElem := AElement.firstElement;
  While Assigned(LElem) Do
    Begin
    If LElem.Name = 'Component' Then
      Begin
      LItem := THCPComponent.Create;
      Try
        LItem.FSequence := count+1;
        LItem.Read(LElem, sPath);
        Add(LItem.Link);
      Finally
        LItem.Unlink;
      End;
      End;
    LElem := LELem.NextElement;
    End;
End;

{ THCPField }

Constructor THCPField.Create;
Begin
  Inherited Create;
  FComponents := THCPComponentList.Create;
End;

Destructor THCPField.Destroy;
Begin
  FComponents.Free;
  Inherited;
End;

Procedure THCPField.Assign(oSource: TFslObject);
Begin
  Inherited;
  FMax := THCPField(oSource).FMax;
  FMin := THCPField(oSource).FMin;
  FItemNo := THCPField(oSource).FItemNo;
  FComponents.Assign(THCPField(oSource).FComponents);
End;

Procedure THCPField.Read(AElement: TMXmlElement; sPath : String);
Begin
  Inherited;
  SetProp('Max', AElement.attribute['Max']);
  SetProp('Min', AElement.attribute['Min']);
  SetProp('ItemNo', AElement.attribute['ItemNo']);
  updateProperties;
  FPath := sPath+'.Field['+IntegerToString(FSequence)+']';
  FComponents.Read(AElement, Path);
End;

Procedure THCPField.updateProperties;
Begin
  Inherited;
  FMax := ReadMax(GetProp('Max'), 'v2_conformance');
  FMin := ReadMin(GetProp('Min'), 'v2_conformance');
  FItemNo := GetProp('ItemNo');
End;

Procedure THCPField.ValidateBase(Const AHL7DataElement: THL7V2DataElement; Const ASegment : THL7V2Segment; Const AHCPValidateEvents: THCPValidateEvents);
Var
  LHCPRepeats : Integer;
  LDefined : Boolean;
  LFieldName : String;
Begin
  LFieldName := ASegment.Code+'.'+IntegerToString(Sequence);
  LDefined := Assigned(AHL7DataElement);
  If LDefined Then
    Begin
    LDefined := AHL7DataElement.HasContent;
    End;
  If Not LDefined Then
    Begin
    If Required Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Required Field '+LFieldName+' not found',AHL7DataElement);
      End;
    Exit;
    End;

  If (FMax > 1) Or (FMax = -1) Then
    Begin
    LHCPRepeats := AHL7DataElement.RepeatCount;
    If LHCPRepeats < Min Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Field '+LFieldName+' should have at least '+IntegerToString(Min)+' repeats',AHL7DataElement);
      End;
    If (Max > 0) And (LHCPRepeats > Max) Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Field '+LFieldName+' should have at most '+IntegerToString(Max)+' repeats',AHL7DataElement);
      End;
    End
  Else
    Begin
    If AHL7DataElement.RepeatCount > 1 Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Field '+LFieldName+' should not have repeats',AHL7DataElement);
      End;
    End;
End;

Procedure THCPField.ValidateRepeat(Const AHL7DataElement: THL7V2DataElement; Const ASegment : THL7V2Segment; Const AHCPValidateEvents: THCPValidateEvents);
Var
  i : Integer;
  LHCPComponent : THCPComponent;
  LHL7Component : THL7V2Component;
  LFieldName : String;
Begin
  LFieldName := ASegment.Code+'.'+IntegerToString(Sequence);
  If (AHL7DataElement.Components.Count = 0) And (Components.Count = 0) Then       // There are no components
    Begin
    ValidateCell(AHL7DataElement,AHL7DataElement,LFieldName,AHCPValidateEvents);
    End
  Else If (AHL7DataElement.Components.Count > 0) And (Components.Count = 0) Then       // There are no components expected but are found
    Begin
    AHCPValidateEvents.RecordError(eHCP_Error,Path,'Field '+LFieldName+' should not have components',AHL7DataElement);
    End
  Else If (AHL7DataElement.Components.Count = 0) And (Components.Count > 0) Then       // There are components expected but are none are found
    Begin
    AHCPValidateEvents.RecordError(eHCP_Error,Path,'Field '+LFieldName+' should have components but none are found',AHL7DataElement);
    End
  Else If AHL7DataElement.HasContent Then
    Begin
    For i := 0 To Components.Count-1 Do
      Begin
      LHCPComponent := Components.Component[i];
      Try
        LHL7Component := AHL7DataElement.Component[LHCPComponent.Sequence];
      Except
        LHL7Component := Nil;
        End;
      LHCPComponent.ValidateComponent(LHL7Component,AHL7DataElement,LFieldName,AHCPValidateEvents);
      End;

    // Validate each Component that appears is allowed
    For i := 1 To AHL7DataElement.Components.Count Do
      Begin
      If AHL7DataElement.Component[i].HasContent Then
        Begin
        If Not Components.Defined(i) Then
          Begin
          AHCPValidateEvents.RecordError(eHCP_Error,Path,'Field '+LFieldName + '.' + IntegerToString(i) + ' is found but NOT defined in Specification',AHL7DataElement.Component[i]);
          End;
        End;
      End;
    End;
End;

Procedure THCPField.ValidateField(Const AHL7DataElement: THL7V2DataElement; Const ASegment : THL7V2Segment; Const AHCPValidateEvents: THCPValidateEvents);
Var
  i : Integer;
  LFieldName : String;
Begin
  If Not MustValidate Then
    Begin
    Exit;
    End;
  LFieldName := ASegment.Code+'.'+IntegerToString(Sequence);
  Try
//    AHCPValidateEvents.RecordError(eHCP_Information,Path,'   Validate Field: '+LFieldName);
    ValidateBase  (AHL7DataElement,ASegment,AHCPValidateEvents);
    For i := 0 To AHL7DataElement.RepeatCount - 1 Do
      Begin
      ValidateRepeat(AHL7DataElement._Repeat[i],ASegment,AHCPValidateEvents);
      End;
  Except
    On e:Exception Do
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Exception error while validating - '+e.Message,AHL7DataElement);
      End;
    End;
End;

function THCPField.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FItemNo.length * sizeof(char)) + 12);
  inc(result, FComponents.sizeInBytes);
end;

{ THCPFieldIterator }

Function THCPFieldIterator.Current: THCPField;
Begin
  Result := THCPField(Inherited current);
End;

{ THCPFieldList }

Function THCPFieldList.Defined(Const ASequence: Integer): Boolean;
Var
  i : Integer;
Begin
  Result := False;
  For i := 0 To Count-1 Do
    Begin
    If Field[i].Sequence = ASequence Then
      Begin
      Result := True;
      Exit;
      End;
    End;
End;

Function THCPFieldList.GetField(AIndex: Integer): THCPField;
Begin
  Result := ObjectByIndex[AIndex] As THCPField;
End;

Function THCPFieldList.ItemClass: TFslObjectClass;
Begin
  Result := THCPField;
End;

Function THCPFieldList.Iterator: TFslIterator;
Begin
  Result := THCPFieldIterator.Create;
  THCPFieldIterator(Result).List := Self.Link;
End;

Procedure THCPFieldList.Read(AElement: TMXmlElement; sPath : String);
Var
  LElem : TMXmlElement;
  LItem : THCPField;
Begin
  Assert(Self.Invariants('v2_conformance', THCPFieldList));

  LElem := AElement.firstElement;
  While Assigned(LElem) Do
    Begin
    If LElem.Name = 'Field' Then
      Begin
      LItem := THCPField.Create;
      Try
        LItem.FSequence := count+1;
        LItem.Read(LElem, sPath);
        Add(LItem.Link);
      Finally
        LItem.Unlink;
      End;
      End;
    LElem := LELem.NextElement;
    End;
End;

{ THCPSegmentBase }

Function THCPSegmentBase.GetRequired: Boolean;
Begin
  Result := FUsage = hcpuR;
End;

Function THCPSegmentBase.StillRequired: Boolean;
Begin
  If Required And (FMax > 1) Then          // Required and repeatable
    Begin
    If FMin = 0 Then
      Begin
      Result := FNumberofRepeatsFound < 1;
      End
    Else
      Begin
      Result := FNumberofRepeatsFound < FMin;
      End;
    End
  Else If Required Then                    // Required and NOT repeatable
    Begin
    Result := FNumberofRepeatsFound < 1;
    End
  Else If (FMax > 1) Then                  // Optional and repeatable
    Begin
    Result := False;
    End
  Else                                     // Optional and NOT repeatable
    Begin
    Result := False;
    End;
End;

Function THCPSegmentBase.StillPossible: Boolean;
Begin
  If StillRequired Then
    Begin
    Result := True;
    End
  Else If Required And (FMax > 1) Then      // Required and repeatable
    Begin
    Result := (FMax = 0) Or (FNumberofRepeatsFound < FMax);
    End
  Else If Required Then                    // Required and NOT repeatable
    Begin
    Result := False;
    End
  Else If (FMax > 1) Then                  // Optional and repeatable
    Begin
    Result := (FMax = 0) Or (FNumberofRepeatsFound < FMax);
    End
  Else                                     // Optional and NOT repeatable
    Begin
    Result := FNumberofRepeatsFound = 0;
    End;
End;

Procedure THCPSegmentBase.Read(AElement: TMXmlElement; sPath : String);
Begin
  Assert(Self.Invariants('v2_conformance', THCPSegmentBase));
  If Assigned(AElement.element('Predicate')) Then
    SetProp('Predicate', AElement.element('Predicate').Text);
  If Assigned(AElement.element('ImpNote')) Then
    SetProp('ImpNote', AElement.element('ImpNote').Text);
  If Assigned(AElement.element('Description')) Then
    SetProp('Description', AElement.element('Description').Text);
  If Assigned(AElement.element('Reference')) Then
    SetProp('Reference', AElement.element('Reference').Text);
  SetProp('Name', AElement.attribute['Name']);
  SetProp('LongName', AElement.attribute['LongName']);
  SetProp('Min', AElement.attribute['Min']);
  SetProp('Max', AElement.attribute['Max']);
  SetProp('Usage', AElement.attribute['Usage']);
End;

Procedure THCPSegmentBase.updateProperties;
Begin
  Inherited;
  FPredicate := GetProp('Predicate');
  FImpNote := GetProp('ImpNote');
  FDescription := GetProp('Description');
  FReference := GetProp('Reference');
  FName := GetProp('Name');
  FLongName := GetProp('LongName');
  FUsage := THCPUsage(GetEnumeratedProp(Path, 'Usage', HCP_Usage_VAL));
  FMax := ReadMax(GetProp('Max'), 'v2_conformance');
  FMin := ReadMin(GetProp('Min'), 'v2_conformance');
End;

Procedure THCPSegmentBase.OwnerGroupisNowRequired;
Begin
  If FNumberofRepeatsFound > 0 Then
    Begin
    If Assigned(FOwnerGroup) Then
      Begin
      FOwnerGroup.NowRequired := True;
      End;
    End;
End;

Procedure THCPSegmentBase.Assign(oSource: TFslObject);
Var
  LSrc : THCPSegmentBase;
Begin
  Inherited;
  LSrc := oSource As THCPSegmentBase;
  FPredicate := LSrc.FPredicate;
  FImpNote := LSrc.FImpNote;
  FDescription := LSrc.FDescription;
  FReference := LSrc.FReference;
  FName := LSrc.FName;
  FLongName := LSrc.FLongName;
  FUsage := LSrc.FUsage;
  FMin := LSrc.FMin;
  FMax := LSrc.FMax;
End;

function THCPSegmentBase.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOwnerGroup.sizeInBytes);
  inc(result, (FPredicate.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, (FReference.length * sizeof(char)) + 12);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FLongName.length * sizeof(char)) + 12);
end;

{ THCPSegmentBaseIterator }

Function THCPSegmentBaseIterator.Current: THCPSegmentBase;
Begin
  Result := THCPSegmentBase(Inherited current);
End;

{ THCPSegmentBaseList }

Function THCPSegmentBaseList.GetSegmentBase(Const AIndex: Integer): THCPSegmentBase;
Begin
  Assert(Self.Invariants('v2_conformance', THCPSegmentBaseList));
  Assert(Count > 0,'v2_conformance'+'AIndex has value ['+IntegerToString(AIndex)+'] but list has no entries');
  Assert((AIndex >= 0) And (AIndex < Count),'v2_conformance'+'AIndex has value ['+IntegerToString(AIndex)+']. Expected "0" to ['+IntegerToString(Count-1)+']');
  Result := ObjectByIndex[AIndex] As THCPSegmentBase;
End;

Function THCPSegmentBaseList.ItemClass: TFslObjectClass;
Begin
  Result := THCPSegmentBase;
End;

Function THCPSegmentBaseList.Iterator: TFslIterator;
Begin
  Result := THCPSegmentBaseIterator.Create;
  THCPSegmentBaseIterator(Result).List := Self.Link;
End;

Procedure THCPSegmentBaseList.Read(AElement: TMXmlElement; sPath : String);
Var
  LElem : TMXmlElement;
  LItem : THCPSegmentBase;
Begin
  Assert(Self.Invariants('v2_conformance', THCPSegmentBaseList));

  LElem := AElement.firstElement;
  While Assigned(LElem) Do
    Begin
    If LElem.Name = 'Segment' Then
      LItem := THCPSegment.Create
    Else If LElem.Name = 'SegGroup' Then
      LItem := THCPSegGroup.Create
    Else
      LItem := Nil; // suppress warning. we ignore these errors - they arise when reading standard HL7 registry version
    If Assigned(LItem) Then
      Begin
      Try
        LItem.FSequence := count;
        LItem.Read(LElem, sPath);
        Add(LItem.Link);
      Finally
        LItem.Unlink;
      End;
      End;
    LElem := LELem.nextElement;
    End;
End;

{ THCPSegment }

Constructor THCPSegment.Create;
Begin
  Inherited Create;
  FFields := THCPFieldList.Create;
End;

Destructor THCPSegment.Destroy;
Begin
  FFields.Free;
  Inherited;
End;

Procedure THCPSegment.Assign(oSource: TFslObject);
Begin
  Inherited;
  FFields.Assign(THCPSEgment(oSource).FFields);
End;

Procedure THCPSegment.Read(AElement: TMXmlElement; sPath : String);
Begin
  Inherited;
  updateProperties;
  FPath := sPath+'.Segment['+FName+']';
  FFields.Read(AElement, Path);
End;

Procedure THCPSegment.ValidateSegment(Const ASegment: THL7V2Segment;Const AHCPValidateEvents: THCPValidateEvents);
Var
  i         : Integer;
  LHCPField : THCPField;
  LHL7Field : THL7V2DataElement;
Begin
  Try
    AHCPValidateEvents.RecordError(eHCP_Information,Path,'Validate HL7 segment '+ASegment.Code);

    // Validate against Specification
    For i := 0 To FFields.Count-1 Do
      Begin
      LHCPField := FFields.Field[i];
      Try
        LHL7Field := ASegment.Field[LHCPField.Sequence];
      Except
        LHL7Field := Nil;
        End;
      If Assigned(LHL7Field) Then
        Begin
        LHCPField.ValidateField(LHL7Field,ASegment,AHCPValidateEvents);
        End
      Else If LHCPField.Required Then
        Begin
        AHCPValidateEvents.RecordError(eHCP_Error,LHCPField.Path,'Field is required but was not found',ASegment);
        End;
      End;

    // Validate each field that appears is allowed
    For i := 1 To ASegment.Fields.Count Do
      Begin
      If ASegment.Field[i].HasContent Then
        Begin
        If Not FFields.Defined(i) Then
          Begin
          AHCPValidateEvents.RecordError(eHCP_Error,Path,'Field '+ASegment.Code+ '.' + IntegerToString(i) + ' is found but NOT defined in Specification',ASegment.Field[i]);
          End;
        End;
      End;

  Except
    On e:Exception Do
      Begin
      AHCPValidateEvents.RecordError(eHCP_Error,Path,'Exception error while validating - ' + e.Message,ASegment);
      End;
    End;
End;

function THCPSegment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFields.sizeInBytes);
end;

{ THCPSegGroup }

Constructor THCPSegGroup.Create;
Begin
  Inherited Create;
  FSegments := THCPSegmentBaseList.Create;
End;

Destructor THCPSegGroup.Destroy;
Begin
  FSegments.Free;
  Inherited;
End;

Procedure THCPSegGroup.Assign(oSource: TFslObject);
Var
  iLoop : Integer;
Begin
  Inherited;
  FNowRequired :=  THCPSegGroup(oSource).FNowRequired;
  FSegments.Assign(THCPSegGroup(oSource).FSegments);
  For iLoop := 0 To FSegments.Count - 1 Do
    FSegments[iLoop].OwnerGroup := Self;
End;

Procedure THCPSegGroup.Read(AElement: TMXmlElement; sPath : String);
Begin
  Inherited;
  updateProperties;
  FPath := sPath+'.SegGroup['+FName+']';
  FSegments.Read(AElement, Path);
End;

Function THCPSegGroup.StillRequired: Boolean;
Begin
  If FNowRequired Then
    Begin
    Result := True;
    End
  Else
    Begin
    Result := Inherited StillRequired;
    End;
End;

function THCPSegGroup.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSegments.sizeInBytes);
end;

{ THCPMetaData }

Procedure THCPMetaData.Assign(oSource: TFslObject);
Var
  LSrc : THCPMetaData;
Begin
  Inherited;
  LSrc := oSource As THCPMetaData;
  FName := LSrc.FName;
  FOrgName := LSrc.FOrgName;
  FVersion := LSrc.FVersion;
  FStatus := LSrc.FStatus;
  FTopics := LSrc.FTopics;
End;

Procedure THCPMetaData.Read(AElement: TMXmlElement; sPath : String);
Begin
  SetProp('Name', AElement.attribute['SpecName']);
  SetProp('OrgName', AElement.attribute['OrgName']);
  SetProp('Version', AElement.attribute['SpecVersion']);
  SetProp('Status', AElement.attribute['Status']);
  updateProperties;
  FPath := sPath+'.Metadata';
End;

Procedure THCPMetaData.updateProperties;
Begin
  Inherited;
  FName := GetProp('Name');
  FOrgName := GetProp('OrgName');
  FVersion := GetProp('Version');
  FStatus := GetProp('Status');
  FTopics := GetProp('Topics');
End;

function THCPMetaData.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FOrgName.length * sizeof(char)) + 12);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FStatus.length * sizeof(char)) + 12);
  inc(result, (FTopics.length * sizeof(char)) + 12);
end;

{ THCPStaticDef }

Constructor THCPStaticDef.Create;
Begin
  Inherited Create;
  FSegments := THCPSegmentBaseList.Create;
  FMetaData := THCPMetaData.Create;
End;

Destructor THCPStaticDef.Destroy;
Begin
  FMetaData.Free;
  FSegments.Free;
  Inherited;
End;

Procedure THCPStaticDef.Assign(oSource: TFslObject);
Var
  LSrc : THCPStaticDef;
Begin
  Inherited;
  LSrc := oSource As THCPStaticDef;
  FMetaData.Assign(LSrc.FMetaData);
  FSegments.Assign(LSrc.FSegments);

  FMsgType := LSrc.FMsgType;
  FEventType := LSrc.FEventType;
  FMsgStructID := LSrc.FMsgStructID;
  FOrderControl := LSrc.FOrderControl;
  FEventDesc := LSrc.FEventDesc;
  FIdentifier := LSrc.FIdentifier;
  FRole := LSrc.FRole;
  FImpNote := LSrc.FImpNote;
  FDescription := LSrc.FDescription;
  FReference := LSrc.FReference;
End;

Procedure THCPStaticDef.Clear;
Begin
  Inherited;
  FSegments.Clear;
  FMetaData.Clear;
End;

Procedure THCPStaticDef.Read(AElement: TMXmlElement; sPath : String);
Begin
  Assert(Self.Invariants('v2_conformance', THCPStaticDef));

  SetProp('MsgType', AElement.attribute['MsgType']);
  SetProp('EventType', AElement.attribute['EventType']);
  SetProp('MsgStructID', AElement.attribute['MsgStructID']);
  SetProp('OrderControl', AElement.attribute['OrderControl']);
  SetProp('EventDesc', AElement.attribute['EventDesc']);
  SetProp('Identifier', AElement.attribute['Identifier']);
  SetProp('Role', AElement.Parent.attribute['Role']);
  SetProp('ImpNote', AElement.attribute['ImpNote']);
  SetProp('Description', AElement.attribute['Description']);
  SetProp('Reference', AElement.attribute['Reference']);
  updateProperties;
  FPath := sPath+'.StaticDef['+IntegerToString(FSequence)+']';

  FMetaData.Read(AElement, Path);
  FSegments.Read(AElement, Path);
End;

Procedure THCPStaticDef.updateProperties;
Begin
  Inherited;
  FMsgType := GetProp('MsgType');
  FEventType := GetProp('EventType');
  FMsgStructID := GetProp('MsgStructID');
  FOrderControl := GetProp('OrderControl');
  FEventDesc := GetProp('EventDesc');
  FIdentifier := GetProp('Identifier');
  FRole := THCPRoleType(GetEnumeratedProp(Path, 'Role', HCP_RoleType_VAL, ord(hcprtSender)));
  FImpNote := GetProp('ImpNote');
  FDescription := GetProp('Description');
  FReference := GetProp('Reference');
End;

Procedure THCPStaticDef.InitialiseHCPSegmentsforValidation(Const ASegments : THCPSegmentBaseList; Const AOwnerGroup : THCPSegGroup);
Var
  i               : Integer;
  LHCPSegmentBase : THCPSegmentBase;
Begin
  For i := 0 To ASegments.Count-1 Do
    Begin
    LHCPSegmentBase                       := ASegments.SegmentBase[i];
    LHCPSegmentBase.FOwnerGroup           := AOwnerGroup;
    If LHCPSegmentBase Is THCPSegGroup Then
      Begin
      InitialiseHCPSegmentsforValidation((LHCPSegmentBase As THCPSegGroup).Segments,LHCPSegmentBase As THCPSegGroup);
      End;
    End;
End;

Function  THCPStaticDef.ValidateMessageProperty(Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Out VErrMsg : String):Boolean;
Begin
  Assert(Self.Invariants('v2_conformance', THCPStaticDef));
  Assert(Invariants('v2_conformance', AHL7Message, THL7V2Message, 'AMsg'));
  Assert(Assigned(AHCPValidateEvents), 'v2_conformance'+': AHCPValidateEvents is Not valid');

  Result := True;

  AHCPValidateEvents.RecordError(eHCP_Information,'','Validate Message Properties');

  // Event Type Validation
  If Not StringEquals(AHL7Message.Event,EventType) Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Error,'','Event Type mismatch: Expected "'+EventType+'" Got "'+AHL7Message.Event+'"');
    End;

  // Message Type Validation
  If Not StringEquals(AHL7Message.MessageType,  MsgType  ) Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Error,'','Message Type mismatch: Expected "'+MsgType+'" Got "'+AHL7Message.MessageType+'"');
    End;

  // Message Structure ID Validation
  If (AHL7Message.StructName = '')  And (MsgStructID = '') Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Warning,'','HL7 Message and Specification do not specify the Message Structure ID');
    End
  Else If (AHL7Message.StructName = '')  And (MsgStructID <> '') Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Warning,'','HL7 Message does not specify the Message Structure ID. Expected "'+MsgStructID+'"');
    End
  Else If (AHL7Message.StructName <> '')  And (MsgStructID = '') Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Warning,'','Specification does not specify the Message Structure ID. Message has "' + AHL7Message.StructName+'"');
    End
  Else If Not StringEquals(AHL7Message.StructName,MsgStructID) Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Error,'','Message Structure ID mismatch. Expected "' + MsgStructID + '" Got "' + AHL7Message.StructName + '"');
    End;
End;

Function THCPStaticDef.ValidateHCPSegment(Const AHCPSegment : THCPSegment; Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Var VHL7MessageSegmentCounter : Integer; Out VErrMsg : String):Boolean;
Var
  LSegment  : THL7V2Segment;
Begin
  Assert(Self.Invariants('v2_conformance', THCPStaticDef));
  Assert(AHCPSegment.Invariants('v2_conformance',THCPSegment));
  Assert(Assigned(AHL7Message),'v2_conformance'+'AHL7Message is not assigned');

  If AHCPSegment.StillPossible Then
    Begin
    AHCPValidateEvents.RecordError(eHCP_Information,AHCPSegment.Path,'Repeat Counter is '+IntegerToString(AHCPSegment.NumberofRepeatsFound));
    If VHL7MessageSegmentCounter >= AHL7Message.Segments.Count Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Information,AHCPSegment.Path,'No Segments left in message');
      If AHCPSegment.StillRequired Then
        Begin
        AHCPValidateEvents.RecordError(eHCP_Information,AHCPSegment.Path,'Since this segment is required we cannot continue');
        VErrMsg := 'Expected Segment ' + AHCPSegment.Name+' when no segments are left in message';
        Result  := False;
        End
      Else
        Begin
        AHCPValidateEvents.RecordError(eHCP_Information,AHCPSegment.Path,'Since this segment is NOT required we can continue');
        Result := True;
        End;
      End
    Else
      Begin
      LSegment := AHL7Message.Segments[VHL7MessageSegmentCounter];
      AHCPValidateEvents.RecordError(eHCP_Information,AHCPSegment.Path,'Next HL7 Message segment is '+LSegment.Code);
      If StringEquals(LSegment.Code,AHCPSegment.Name) Then
        Begin
        VHL7MessageSegmentCounter           := VHL7MessageSegmentCounter + 1;
        AHCPSegment.NumberofRepeatsFound    := AHCPSegment.NumberofRepeatsFound + 1;
        AHCPSegment.ValidateSegment(LSegment,AHCPValidateEvents);
        If AHCPSegment.StillRequired Then
          Begin
          Result := ValidateHCPSegment(AHCPSegment,AHL7Message,AHCPValidateEvents,VHL7MessageSegmentCounter,VErrMsg);
          End
        Else If AHCPSegment.StillPossible Then
          Begin
          Result := True;
          ValidateHCPSegment(AHCPSegment,AHL7Message,AHCPValidateEvents,VHL7MessageSegmentCounter,VErrMsg);
          End
        Else
          Begin
          Result := True;
          End;
        End
      Else If AHCPSegment.StillRequired Then
        Begin
        VErrMsg := 'Expected Segment '+AHCPSegment.Name+' but next message segment is '+LSegment.Code;
        Result  := False;
        End
      Else
        Begin
        AHCPValidateEvents.RecordError(eHCP_Information,AHCPSEgment.Path,'Next segment is ' + AHCPSegment.Name + ' and it is not required to be so continue.');
        Result := True;
        End;
      End;
    End
  Else
    Begin
    Result := False;
    End;
End;

Procedure THCPStaticDef.ResetNumberofRepeatsFound(Const AHCPSegGroup : THCPSegGroup);
Var
  i                     : Integer;
  LHCPSegmentBase       : THCPSegmentBase;
Begin
  For i := 0 To AHCPSegGroup.Segments.Count-1 Do
    Begin
    LHCPSegmentBase                       := AHCPSegGroup.Segments.SegmentBase[i];
    LHCPSegmentBase.NumberofRepeatsFound  := 0;
    If LHCPSegmentBase Is THCPSegGroup Then
      Begin
      ResetNumberofRepeatsFound(LHCPSegmentBase As THCPSegGroup);
      End;
    End;
End;

Function THCPStaticDef.ValidateHCPSegmentGroup(Const AHCPSegGroup : THCPSegGroup; Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Var VHL7MessageSegmentCounter : Integer; Out VErrMsg : String):Boolean;
Var
  LHL7MessageSegmentCounter : Integer;
Begin
  Assert(Self.Invariants('v2_conformance', THCPStaticDef));
  Assert(AHCPSegGroup.Invariants('v2_conformance',THCPSegGroup));
  Assert(Invariants('v2_conformance', AHL7Message, THL7V2Message, 'AMsg'));
  Assert(Assigned(AHCPValidateEvents), 'v2_conformance'+': AHCPValidateEvents is Not valid');

  ResetNumberofRepeatsFound(AHCPSegGroup);

  If AHCPSegGroup.StillPossible Then
    Begin
    LHL7MessageSegmentCounter := VHL7MessageSegmentCounter;
    AHCPValidateEvents.RecordError(eHCP_Information,AHCPSegGroup.Path,'Validate Segment Group');
    AHCPSegGroup.NowRequired       := False;
    If ValidateSegmentList(AHCPSegGroup.Segments,AHL7Message,AHCPValidateEvents,LHL7MessageSegmentCounter,VErrMsg) Then
      Begin
      VHL7MessageSegmentCounter         := LHL7MessageSegmentCounter;
      If AHCPSegGroup.NowRequired Then
        Begin
        AHCPSegGroup.NumberofRepeatsFound := AHCPSegGroup.NumberofRepeatsFound + 1;
        If AHCPSegGroup.StillRequired Then
          Begin
          Result  := ValidateHCPSegmentGroup(AHCPSegGroup,AHL7Message,AHCPValidateEvents,VHL7MessageSegmentCounter,VErrMsg);
          End
        Else If AHCPSegGroup.StillPossible Then
          Begin
          Result  := True;
          ValidateHCPSegmentGroup(AHCPSegGroup,AHL7Message,AHCPValidateEvents,VHL7MessageSegmentCounter,VErrMsg);
          End
        Else
          Begin
          Result := True;
          End;
        End
      Else If AHCPSegGroup.StillRequired Then
        Begin
        VErrMsg := 'Segment Group '+AHCPSegGroup.Name+' expected but not found';
        Result  := False;
        End
      Else
        Begin
        Result  := True;
        End;
      End
    Else If AHCPSegGroup.StillRequired Then
      Begin
      Result  := False;
      VErrMsg := 'Segment Group '+AHCPSegGroup.Name+' expected but not found';
      End
    Else
      Begin
      Result := True;
      End;
    End
  Else
    Begin
    Result := True;
    End;

End;

Function THCPStaticDef.ValidateSegmentList(Const ASegments : THCPSegmentBaseList; Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Var VHL7MessageSegmentCounter : Integer; Out VErrMsg : String):Boolean;
Var
  i                     : Integer;
  LHCPSegmentBase       : THCPSegmentBase;
Begin
  Assert(Self.Invariants('v2_conformance', THCPStaticDef));
  Assert(ASegments.Invariants('v2_conformance',THCPSegmentBaseList));
  Assert(Invariants('v2_conformance', AHL7Message, THL7V2Message, 'AMsg'));
  Assert(Assigned(AHCPValidateEvents), 'v2_conformance'+': AHCPValidateEvents is Not valid');

  Result := True;
  For i := 0 To ASegments.Count-1 Do
    Begin
    LHCPSegmentBase                      := ASegments.SegmentBase[i];
    If LHCPSegmentBase Is THCPSegment Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Information,LHCPSegmentBase.Path,'Validate Segment '+(LHCPSegmentBase As THCPSegment).FName);
      If ValidateHCPSegment(LHCPSegmentBase As THCPSegment,AHL7Message,AHCPValidateEvents,VHL7MessageSegmentCounter,VErrMsg) Then
        Begin
        LHCPSegmentBase.OwnerGroupisNowRequired;
        End
      Else
        Begin
        Result := False;
        Exit;
        End;
      End
    Else If LHCPSegmentBase Is THCPSegGroup Then
      Begin
      AHCPValidateEvents.RecordError(eHCP_Information,LHCPSegmentBase.Path,'Validate Segment Group '+(LHCPSegmentBase As THCPSegGroup).FName);
      If Not ValidateHCPSegmentGroup(LHCPSegmentBase As THCPSegGroup,AHL7Message,AHCPValidateEvents,VHL7MessageSegmentCounter,VErrMsg) Then
        Begin
        Result := False;
        Exit;
        End;
      End
    Else
      Begin
      ErrorValidationFailed('ValidateSegmentList', 'THCPSegmentBase is an abstract class type. Unrecognised type was found');
      End;
    End;
End;

Function  THCPStaticDef.ValidateMessageSegments(Const AHL7Message : THL7V2Message; Const AHCPValidateEvents : THCPValidateEvents; Out VErrMsg : String):Boolean;
Var
  LHL7MessageSegmentCounter : Integer;
  LS1 : String;
  LS2 : String;
Begin
  Assert(Self.Invariants('v2_conformance', THCPStaticDef));
  Assert(Invariants('v2_conformance', AHL7Message, THL7V2Message, 'AMsg'));
  Assert(Assigned(AHCPValidateEvents), 'v2_conformance'+': AHCPValidateEvents is Not valid');
  LHL7MessageSegmentCounter := 0;
  InitialiseHCPSegmentsforValidation(Segments,Nil);
  AHCPValidateEvents.RecordError(eHCP_Information,'','Validate Segments');
  If ValidateSegmentList(Segments,AHL7Message,AHCPValidateEvents,LHL7MessageSegmentCounter,VErrMsg) Then
    Begin
    If LHL7MessageSegmentCounter >= AHL7Message.Segments.Count Then
      Begin
      Result := True;
      End
    Else
      Begin
      LS1 := '';
      While LHL7MessageSegmentCounter < AHL7Message.Segments.Count Do
        Begin
        LS2 := '"'+AHL7Message.Segments[LHL7MessageSegmentCounter].Code+'"';
        If LS1 = '' Then
          Begin
          LS1 := LS2;
          End
        Else
          Begin
          LS1 := LS1 + ' ' + LS2;
          End;
        LHL7MessageSegmentCounter := LHL7MessageSegmentCounter + 1;
        End;
      VErrMsg := 'Segments appear at end of message that are not specified '+ Ls1;
      Result := False;
      End;
    End
  Else
    Begin
    Result := False;
    End;
End;

function THCPStaticDef.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMsgType.length * sizeof(char)) + 12);
  inc(result, (FEventType.length * sizeof(char)) + 12);
  inc(result, (FMsgStructID.length * sizeof(char)) + 12);
  inc(result, (FOrderControl.length * sizeof(char)) + 12);
  inc(result, (FEventDesc.length * sizeof(char)) + 12);
  inc(result, (FIdentifier.length * sizeof(char)) + 12);
  inc(result, FMetaData.sizeInBytes);
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, (FReference.length * sizeof(char)) + 12);
  inc(result, FSegments.sizeInBytes);
end;

{ THCPStaticDefList }

Function THCPStaticDefList.GetMessage(Const AIndex: Integer): THCPStaticDef;
Begin
  Assert(Self.Invariants('v2_conformance', THCPStaticDefList));
  Assert(Count > 0,'v2_conformance'+': AIndex has value ['+IntegerToString(AIndex)+'] but list has no entries');
  Assert((AIndex >= 0) And (AIndex < Count),'v2_conformance'+': AIndex has value ['+IntegerToString(AIndex)+']. Expected "0" to ['+IntegerToString(Count-1)+']');
  Result := ObjectByIndex[AIndex] As THCPStaticDef;
End;

Function THCPStaticDefList.ItemClass: TFslObjectClass;
Begin
  Result := THCPStaticDef;
End;

Function THCPStaticDefList.Iterator: TFslIterator;
Begin
  Result := THCPStaticDefIterator.Create;
  THCPStaticDefIterator(Result).List := Self.Link;
End;

{ THCPStaticDefIterator }

Function THCPStaticDefIterator.Current: THCPStaticDef;
Begin
  Result := Inherited current As THCPStaticDef;
End;

{ THCPDynamicDef }

Constructor THCPDynamicDef.Create;
Begin
  Inherited Create;
  FStaticDefs := THCPStaticDefList.Create;
End;

Destructor THCPDynamicDef.Destroy;
Begin
  FStaticDefs.Free;
  Inherited;
End;

Procedure THCPDynamicDef.Assign(oSource: TFslObject);
Var
  LSrc : THCPDynamicDef;
Begin
  Inherited;
  LSrc := oSource As THCPDynamicDef;
  FAccAck := LSrc.FAccAck;
  FAppAck := LSrc.FAppAck;
  FMsgAckMode := LSrc.FMsgAckMode;
  FQueryMessageType := LSrc.FQueryMessageType;
  FQueryMode := LSrc.FQueryMode;
  FStaticDefs := LSrc.FStaticDefs;
End;

Procedure THCPDynamicDef.Clear;
Begin
  Inherited;
  FStaticDefs.Clear;
End;

Procedure THCPDynamicDef.Read(AElement: TMXmlElement; sPath : String);
Var
  LElem : TMXmlElement;
  LStaticDef : THCPStaticDef;
Begin
  SetProp('AppAck', AElement.attribute['AppAck']);
  SetProp('AccAck', AElement.attribute['AccAck']);
  SetProp('MsgAckMode', AElement.attribute['MsgAckMode']);
  SetProp('QueryStatus', AElement.attribute['QueryStatus']);
  SetProp('QueryMode', AElement.attribute['QueryMode']);
  updateProperties;
  FPath := sPath+'.DynamicDef['+IntegerToString(FSequence)+']';

  LElem := AElement.element('HL7v2xStaticDefRef');
  if Assigned(LElem) Then
    Raise Exception.Create('v2_conformance'+': Static Def Ref not yet supported');

  LElem := AElement.nextElement;
  While Assigned(LElem) And (LElem.Name = 'HL7v2xStaticDef') Do
    Begin
    LStaticDef := THCPStaticDef.Create;
    Try
      LStaticDef.FSequence := FStaticDefs.Count;
      LStaticDef.Read(LElem, Path);
      FStaticDefs.Add(LStaticDef.Link);
    Finally
      LStaticDef.Free;
    End;
    LElem := LElem.nextElement;
    End;
End;

Procedure THCPDynamicDef.updateProperties;
Begin
  Inherited;
  FAccAck := THCPAcknowledgmentType(GetEnumeratedProp(Path, 'AccAck', HCP_AcknowledgmentType_VAL, ord(hcpaNE)));
  FAppAck := THCPAcknowledgmentType(GetEnumeratedProp(Path, 'AppAck', HCP_AcknowledgmentType_VAL, ord(hcpaAL)));
  FMsgAckMode := THCPMsgAckModeType(GetEnumeratedProp(Path, 'MsgAckMode', HCP_MsgAckModeType_VAL, ord(hcpaDeferred)));
  FQueryMessageType := THCPQueryMsgType(GetEnumeratedProp(Path, 'QueryMessageType', HCP_QueryMsgType_VAL, ord(hcpqmNonQuery)));
  FQueryMode := THCPQueryModeType(GetEnumeratedProp(Path, 'QueryMode', HCP_QueryModeType_VAL, ord(hcpqmRealTime)));
End;

function THCPDynamicDef.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FStaticDefs.sizeInBytes);
end;

{ THCPDynamicDefList }

Function THCPDynamicDefList.GetDynamicDef(Const AIndex: Integer): THCPDynamicDef;
Begin
  Assert(Self.Invariants('v2_conformance', THCPDynamicDefList));
  Assert(Count > 0,'v2_conformance'+'AIndex has value ['+IntegerToString(AIndex)+'] but list has no entries');
  Assert((AIndex >= 0) And (AIndex < Count),'v2_conformance'+'AIndex has value ['+IntegerToString(AIndex)+']. Expected "0" to ['+IntegerToString(Count-1)+']');
  Result := ObjectByIndex[AIndex] As THCPDynamicDef;
End;

Function THCPDynamicDefList.ItemClass: TFslObjectClass;
Begin
  Result := THCPDynamicDef;
End;

Function THCPDynamicDefList.Iterator: TFslIterator;
Begin
  Result := THCPDynamicDefIterator.Create;
  THCPDynamicDefIterator(Result).List := Self.Link;
End;

Procedure THCPDynamicDefList.Read(AElement: TMXmlElement; sPath : String);
Var
  oELem : TMXmlElement;
  oDynDef : THCPDynamicDef;
  list : TFslList<TMXmlElement>;
Begin
  list := TFslList<TMXmlElement>.create;
  try
    AElement.listElements('DynamicDef', list);
    for oElem in list do
    Begin
      oDynDef := THCPDynamicDef.Create;
      Try
        oDynDef.FSequence := count;
        oDynDef.Read(oElem, sPath);
        Add(oDynDef.Link);
      Finally
        oDynDef.Free;
      End;
    End;
  finally
    list.Free;
  end;
End;

{ THCPDynamicDefIterator }

Function THCPDynamicDefIterator.Current: THCPDynamicDef;
Begin
  Result := Inherited current As THCPDynamicDef;
End;

{ THCPSpecification }

Constructor THCPSpecification.Create;
Begin
  Inherited;
  FMetaData := THCPMetaData.Create;
  FUseCase := THCPUseCase.Create;
  FEncodings := THCPEncodings.Create;
  FDynamicDefs := THCPDynamicDefList.Create;
  FTables := THCPTables.Create;
End;

Destructor THCPSpecification.Destroy;
Begin
  FMetaData.Free;
  FUseCase.Free;
  FEncodings.Free;
  FDynamicDefs.Free;
  FTables.Free;
  Inherited;
End;

Procedure THCPSpecification.Assign(oSource: TFslObject);
Var
  LSrc  : THCPSpecification;
Begin
  Inherited;
  updateProperties;

  LSrc := oSource As THCPSpecification;
  FMetaData.Assign(LSrc.FMetaData);
  FUseCase.Assign(LSrc.FUseCase);
  FEncodings.Assign(LSrc.FEncodings);
  FDynamicDefs.Assign(LSrc.FDynamicDefs);
  FTables.Assign(LSrc.FTables);
End;

Procedure THCPSpecification.Clear;
Begin
  Inherited;
  FMetaData.clear;
  FUseCase.clear;
  FEncodings.clear;
  FDynamicDefs.clear;
  FTables.clear;
End;

Procedure THCPSpecification.updateProperties;
Begin
  Inherited;
  FHL7Version := GetProp('HL7Version');
  FProfileType := THCPProfileType(GetEnumeratedProp('Spec', 'ProfileType', HCP_ProfileType_VAL));
  FIdentifier := GetProp('Identifier');
  FImpNote := GetProp('ImpNote');
End;

Procedure THCPSpecification.Read(AElement: TMXmlElement);
Var
  LElem : TMXmlElement;
Begin
  Assert(Self.Invariants('v2_conformance', THCPSpecification));
  SetProp('HL7Version', AElement.attribute['HL7Version']);
  SetProp('Identifier', AElement.attribute['Identifier']);
  SetProp('ProfileType', AElement.attribute['ProfileType']);
  SetProp('ImpNote', AElement.attribute['ImpNote']);
  updateProperties;

  FMetaData.Read(AElement, '');
  FUseCase.Read(AElement.element('UseCase'), '');
  FTables.Read(AElement, '');
  FDynamicDefs.Read(AElement, '');

  LElem := AElement.element('Encodings');
  LElem := LElem.element('Encoding');
  While Assigned(LElem) Do
    Begin
    FEncodings.Add(LElem.Text);
    LElem := LElem.nextElement;
    End;
End;

Function THCPSpecification.ValidateMessageOk(Const AMsg: THL7V2Message; Out VHCPValidateEvents : THCPValidateEvents;Const AErrorCollector : THL7V2ErrorCollector = Nil):Boolean;
Var
  LErrMsg : String;
  LMessage : THCPStaticDef;
Begin
  Assert(Self.Invariants('v2_conformance', THCPSpecification));
  Assert(Invariants('v2_conformance', AMsg, THL7V2Message, 'AMsg'));

  LMessage := FDynamicDefs[0].FStaticDefs[0].Clone As THCPStaticDef;
  Try
    VHCPValidateEvents                := THCPValidateEvents.Create;
    VHCPValidateEvents.ErrorCollector := AErrorCollector;
    VHCPValidateEvents.HL7Message     := AMsg;
    VHCPValidateEvents.FContext := Self;

    If LMessage.ValidateMessageProperty(AMsg, VHCPValidateEvents,LErrMsg) Then
      Begin
      VHCPValidateEvents.RecordError(eHCP_Information,'','Message Properties validated without fatal error');
      If LMessage.ValidateMessageSegments(AMsg, VHCPValidateEvents, LErrMsg) Then
        Begin
        Result := True;
        VHCPValidateEvents.RecordError(eHCP_Information,'','Message Segments validated without fatal error');
        End
      Else
        Begin
        Result := False;
        VHCPValidateEvents.RecordError(eHCP_Fatal,'',LErrMsg);
        End;
      End
    Else
      Begin
      Result := False;
      VHCPValidateEvents.RecordError(eHCP_Fatal,'',LErrMsg);
      End;
  Finally
    LMessage.Free;
    End;
End;

function THCPSpecification.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMetaData.sizeInBytes);
  inc(result, FUseCase.sizeInBytes);
  inc(result, FEncodings.sizeInBytes);
  inc(result, FDynamicDefs.sizeInBytes);
  inc(result, FTables.sizeInBytes);
end;

{ THCPUseCase }

Procedure THCPUseCase.Read(AElement: TMXmlElement; sPath : String);
Var
  LElem : TMXmlElement;
Begin
  FPath := sPath+'.UseCase';
  If Assigned(AElement) Then
    Begin
    LElem := AElement.element('Purpose');
    Condition(Assigned(LElem), 'v2_conformance', 'No Purpose Element found in use case');
    SetProp('Purpose', LElem.Text);
    LElem := AElement.element('Description');
    If Assigned(LElem) Then
      Begin
      SetProp('Description', LElem.Text);
      updateProperties;

      LElem := LElem.nextElement;
      While Assigned(LElem) Do
        Begin
        If LElem.Name = 'Actor' Then
          FElements.Add(THCPActor.Create(LElem.attribute['Name']))
        Else If LElem.Name = 'PreCondition' Then
          FElements.Add(THCPPreCondition.Create(LElem.attribute['Name']))
        Else If LElem.Name = 'PostCondition' Then
          FElements.Add(THCPPostCondition.Create(LElem.attribute['Name']))
        Else If LElem.Name = 'EventFlow' Then
          FElements.Add(THCPEventFlow.Create(LElem.attribute['Name']))
        Else If LElem.Name = 'DerivedEvent' Then
          FElements.Add(THCPDerivedEvent.Create(LElem.attribute['Name']))
        Else
          Condition(False, 'v2_conformance', 'Unexpected element "'+LElem.Name+'" in Use case');
        LElem := LElem.nextElement;
        End;
      End;
    End;
End;

Procedure THCPUseCase.updateProperties;
Begin
  Inherited;
  FPurpose := GetProp('Purpose');
  FDescription := GetProp('Description');
End;

Function LoadHCPSpec(oDocument : TMXmlDocument) : THCPSpecification; Overload;
Begin
  If StringEquals('HL7v2xConformanceProfile', oDocument.docElement.Name) Then
  Begin
    Result := THCPSpecification.Create;
    Try
      Result.Read(oDocument.docElement);
      Result.Link;
    Finally
      Result.Free;
    End;
  End
  Else If StringEquals('Specification', oDocument.docElement.Name) Then
    Raise EHL7V2Exception.Create(Nil, hecApplicationError, 'LoadHCPSpec', 'Not an hl7 conformance profile (Probably a MWB specification, root node is '+oDocument.docElement.Name+')')
  Else
    Raise EHL7V2Exception.Create(Nil, hecApplicationError, 'LoadHCPSpec', 'Unknown format for HCP, root node is '+oDocument.docElement.Name);
End;

Function LoadHCPSpec(oSource : TFslStream) : THCPSpecification;
var
  oDoc : TMXmlDocument;
Begin
  oDoc := TMXmlParser.parse(oSource, [xpResolveNamespaces, xpDropWhitespace, xpDropComments]);
  Try
    Result := LoadHCPSpec(oDoc);
  Finally
    oDoc.Free;
  End;
End;

Function LoadHCPSpec(const sFileName : String) : THCPSpecification;
var
  f : TFileStream;
  oDoc : TMXmlDocument;
Begin
  f := TFileStream.Create(sFileName, fmOpenRead + fmShareDenyWrite);
  try
    oDoc := TMXmlParser.parse(f, [xpResolveNamespaces, xpDropWhitespace, xpDropComments]);
    Try
      Result := LoadHCPSpec(oDoc);
    Finally
      oDoc.Free;
    End;
  finally
    f.Free;
  end;
End;

function THCPUseCase.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPurpose.length * sizeof(char)) + 12);
  inc(result, (FDescription.length * sizeof(char)) + 12);
  inc(result, FElements.sizeInBytes);
end;

{ THCPUseCaseElement }

Constructor THCPUseCaseElement.Create(sName: String);
Begin
  Inherited Create;
  SetProp('Name', sName);
  updateProperties;
End;

Procedure THCPUseCaseElement.updateProperties;
Begin
  Inherited;
  FName := GetProp('Name');
End;

function THCPUseCaseElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

{ THCPUseCaseElementIterator }

Function THCPUseCaseElementIterator.Current: THCPUseCaseElement;
Begin
  Result := Inherited current As THCPUseCaseElement;
End;

{ THCPUseCaseElementList }

Function THCPUseCaseElementList.GetUseCase(Const AIndex: Integer): THCPUseCaseElement;
Begin
  Result := THCPUseCaseElement(ItemByIndex[AIndex]);
End;

Function THCPUseCaseElementList.ItemClass: TFslObjectClass;
Begin
  Result := THCPUseCaseElement;
End;

Function THCPUseCaseElementList.Iterator: TFslIterator;
Begin
  Result := THCPUseCaseElementIterator.Create;
  THCPUseCaseElementIterator(Result).List := Self.Link;
End;

End.
