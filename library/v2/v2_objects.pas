unit v2_objects;

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

uses
  SysUtils, Classes, {$IFDEF DELPHI} RegularExpressions, {$ENDIF}
  fsl_base, fsl_utilities, fsl_collections, fsl_stream, fsl_xml, fsl_fpc,
  v2_base, v2_dictionary;

Type
  THL7V2Format = (hfUnknown, hfER7, hfXML);

Const
  HL7V2_VALID_FORMATS = [hfER7..hfXML];

  NAMES_HL7V2FORMAT : Array [THL7V2Format] Of String = ('Unknown', 'ER7', 'XML');

Function FromHL7V2FormatCode(Const sValue : String) : THL7V2Format;

type
  THL7V2BatchLoadProgressEvent = Procedure (Const sMessage : String; iPosition, iMax: Integer) Of Object;
  THL7v2DecodingLevelOperation = (DecodeLevelNormal, DecodeLevelUp, DecodeLevelDown);

  THL7V2DecodingOptions = class (THL7V2WorkerObject)
  private
    FSegmentLimit: integer;
    FVersionOverride: String;
    FDontWipeContent: Boolean;
    FFormat: THL7V2Format;
    FSuppressErrors: Boolean;
    FDecodeBatchMessages: Boolean;
    FBatchProgressEvent: THL7V2BatchLoadProgressEvent;
    FPreventOddBinaryLengths: Boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Function Link : THL7V2DecodingOptions;
    Function Clone : THL7V2DecodingOptions;

    Procedure Assign(oObject : TFslObject); Overload; Override;

    procedure Defaults; overload; virtual;

    property Format : THL7V2Format read FFormat write FFormat;

    // blank means use message specified version.
    // has no meaning when not decoding messages as message version cannot be changed
    property VersionOverride : String read FVersionOverride write FVersionOverride;

    // <1 means no limit
    property SegmentLimit : integer read FSegmentLimit write FSegmentLimit;

    // used when decoding cells - leave existing content when decoding content is blank
    // does not apply when decoding messages
    property DontWipeContent : Boolean read FDontWipeContent write FDontWipeContent;

    // decoder should do it's best to ignore errors and keep reading (mostly XML)
    property SuppressErrors : Boolean read FSuppressErrors write FSuppressErrors;

    // when decoding batches, decode the messages as well
    Property DecodeBatchMessages : Boolean Read FDecodeBatchMessages Write FDecodeBatchMessages;

    Property BatchProgressEvent : THL7V2BatchLoadProgressEvent Read FBatchProgressEvent Write FBatchProgressEvent;

    // by default, if \X \ escapes are an odd length, prefixes 0 to make then an even
    // length instead of blowing up. This prohibits that behaviour
    Property PreventOddBinaryLengths : Boolean read FPreventOddBinaryLengths write FPreventOddBinaryLengths;

    class function getDefaults : THL7V2DecodingOptions;
  end;

type
  THL7V2EncodingOption = (eoOptimisticMapping, eoAllowMappingToFail, eoEscapeExtendedCharacters,
                          eoExtraFieldDelimiter, eoNoAddStructureName, eoPretty);

  THL7V2EncodingOptionSet = set of THL7V2EncodingOption;

  THL7V2EncodingOptions = class (THL7V2WorkerObject)
  private
    FExtraFieldDelimiter: Boolean;
    FEscapeExtendedCharacters: Boolean;
    FAllowMappingToFail: Boolean;
    FOptimisticMapping: Boolean;
    FNoAddStructureName: Boolean;
    FFormat: THL7V2Format;
    FBatchProgressEvent: THL7V2BatchLoadProgressEvent;
    FPretty: Boolean;
  public
    Constructor Create(aFormat : THL7V2Format; aOptions : THL7V2EncodingOptionSet); overload; virtual;

    function Link : THL7V2EncodingOptions;
    function Clone : THL7V2EncodingOptions;

    Procedure Assign(oSource : TFslObject); Overload; Override;

    property Format : THL7V2Format read FFormat write FFormat;

    // when writing XML, how the segments are mapped against the segment
    // structure. If this is not defined, then the list of segments must
    // match the expected list. If this is not defined, then the mapping
    // code will attempt to deal with unexpected segments. Note that in
    // optimistic mapping, things can go quite badly wrong if they do go
    // wrong
    property OptimisticMapping : Boolean read FOptimisticMapping write FOptimisticMapping;

    // If this is set, then if mapping fails, any remaining segments not
    // mapped will be mapped into the root segment. If this is not set,
    // and mapping fails, then an exception will be raised and encoding
    // will fail. Relevent for XML
    property AllowMappingToFail : Boolean read FAllowMappingToFail write FAllowMappingToFail;

    // When this is set the extended characters will automatically
    // be escaped in ER7 format. This was added to maintain backward
    // compatibility with older style HL7 interfaces
    property EscapeExtendedCharacters : Boolean read FEscapeExtendedCharacters write FEscapeExtendedCharacters;

    // for stupid OACIS system that requires an extra Field Delimiter at
    // the end of every segment
    property ExtraFieldDelimiter : Boolean read FExtraFieldDelimiter write FExtraFieldDelimiter;

    // whether to automatically encode the structure if it can be determined
    // by default it is added
    property NoAddStructureName : Boolean read FNoAddStructureName write FNoAddStructureName;

    Property Pretty : Boolean read FPretty write FPretty;

    Property BatchProgressEvent : THL7V2BatchLoadProgressEvent Read FBatchProgressEvent Write FBatchProgressEvent;

  end;

const
  NAMES_HL7V2ENCODINGOPTION : array [THL7V2EncodingOption] of String =
    ('OptimisticMapping', 'AllowMappingToFail', 'EscapeExtendedCharacters', 'ExtraFieldDelimiter', 'NoAddStructureName', 'Pretty');

type
  THL7V2Delimiters = class (THL7V2WorkerObject)
  private
    FEscapeCharacter: Char;
    FRepetitionDelimiter: Char;
    FFieldDelimiter: Char;
    FSubComponentDelimiter: Char;
    FComponentDelimiter: Char;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor Create; override;

    function Link : THL7V2Delimiters; overload;
    function Clone : THL7V2Delimiters; overload;
    procedure Assign(oSource : TFslObject); override;

    procedure Check;
    procedure Reset;

    Function isDelimiterEscape(ch : Char) : Boolean;
    Function getDelimiterEscapeChar(ch : Char) : Char;

    property FieldDelimiter : Char read FFieldDelimiter write FFieldDelimiter;
    property ComponentDelimiter : Char read FComponentDelimiter write FComponentDelimiter;
    property RepetitionDelimiter : Char read FRepetitionDelimiter write FRepetitionDelimiter;
    property EscapeCharacter : Char read FEscapeCharacter write FEscapeCharacter;
    property SubComponentDelimiter : Char read FSubComponentDelimiter write FSubComponentDelimiter;

  end;

type
  THL7V2BaseObjects = class;

  THL7V2BaseObject = class (TFslTree)
  private
    FSlot: TObject;
    function GetChildren: THL7V2BaseObjects;
    function GetParent: THL7V2BaseObject;
    procedure SetChildren(const oValue: THL7V2BaseObjects);
    procedure SetParent(const oValue: THL7V2BaseObject);
    Procedure Error(aCondition : THL7V2ErrorCondition; Const sMethod, sMessage : String);
  Protected
    Function ChildrenClass : TFslTreeListClass; override;
    function GetModel: THL7V2Model; virtual;

    Function Condition(bCorrect : Boolean; Const sMethod, sMessage : String) : Boolean;
    Procedure ErrorSequence(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorRequiredField(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorDataType(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorNoTableValue(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnsMsgType(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnsEvntCode(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnsProcID(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnsVersion(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorSuperfluousSeg(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorRequiredSeg(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorUnknownKey(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorDuplicateKey(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorRecordLocked(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorInternal(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorDictionary(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorBadSegCode(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorNoDictionary(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorHL7Library(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorApplication(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorBadField(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorBadMessage(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorXML(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorValidationFailed(const sMethod, sMessage : String); overload; virtual;
    Procedure ErrorDuplicateMsgId(const sMethod, sMessage : String); overload; virtual;

  Public
    function Link : THL7V2BaseObject; overload;
    function Clone : THL7V2BaseObject; overload;

    function IndexInSiblings : Integer; // in some circumstances, this may be -1; there is no parent, or the parent was manually assigned

    function FullPathName: String; overload; virtual;
    function IsElement(const sName: String): Boolean; overload; virtual;

    Procedure Clear; overload; virtual;

    Property Parent : THL7V2BaseObject Read GetParent Write SetParent;
    Property Children : THL7V2BaseObjects Read GetChildren Write SetChildren;
    Property Model : THL7V2Model read GetModel;
    Property Slot : TObject read FSlot write FSlot; // this is deprecated, but parts of HL7Connect use this, so cannot currently be removed
  end;

  THL7V2BaseObjectClass = class of THL7V2BaseObject;

  THL7V2BaseObjects = class (TFslTreeList)
  private
    function GetObject(iIndex: integer): THL7V2BaseObject;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  public
    property BaseObject[iIndex : integer] : THL7V2BaseObject read GetObject; default;
  End;
  THL7V2BaseObjectsClass = class of THL7V2BaseObjects;

  THL7V2BaseObjectList = class (TFslObjectList)
  private
    function GetObject(iIndex: integer): THL7V2BaseObject;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  public
    property BaseObject[iIndex : integer] : THL7V2BaseObject read GetObject; default;
  End;

Type
  // you have to give this a model
  // you can assign a model directly. But this is an unusual thing to do
  // more customarily, you assign a dictionary, and set the version, which
  // will load the model for that version out of the dictionary
  // if no model or dictionary is assigned, then the global dictionary
  // in HL7V2Dictionaries will be used

  THL7V2ModelProvider = Class (THL7V2BaseObject)
  Private
    FDictionary : THL7V2Dictionary;
    FModel : THL7V2Model;
    Function GetVersionCode: String;
    Procedure SetVersionCode(Const sValue: String);
    Function GetVersion : THL7V2Version;
    Procedure SetDictionary(Const Value: THL7V2Dictionary);
    Procedure SetModel(Const Value: THL7V2Model);
  Protected
    Function GetModel: THL7V2Model; Override;
    Procedure SetVersion(aValue  : THL7V2Version); Virtual;
    Function CanChangeVersion : Boolean; Virtual;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create(oModel : THL7V2Model); Overload;
    Constructor Create(oDictionary : THL7V2Dictionary); Overload;
    Constructor Create(oProvider : THL7V2ModelProvider); Overload; // copy providers setup regarding dictionary and model
    Destructor Destroy; Override;

    procedure ChangeVersion(ANewVersion: String; ADeleteInvalidContent: Boolean); virtual;

    Procedure Assign(oSource : TFslObject); Override;

    Procedure Clear; Override;

    Function HasVersion : Boolean;

    Property Dictionary : THL7V2Dictionary Read FDictionary Write SetDictionary;
    Property Model : THL7V2Model Read FModel Write SetModel;
    Property Version: THL7V2Version Read GetVersion Write SetVersion;
    Property VersionCode: String Read GetVersionCode Write SetVersionCode;

  End;

Type
  THL7V2ContentType = (CONTENT_TYPE_NULL, CONTENT_TYPE_TEXT, CONTENT_TYPE_BINARY, CONTENT_TYPE_ESCAPE);

  THL7V2Content = Class (THL7V2BaseObject)
  Private
  Public
    Function Link : THL7V2Content; Overload;
    Function Clone : THL7V2Content; Overload;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    Function ContentType : THL7V2ContentType; Overload; Virtual;
    Function Text : String; Overload; Virtual;
    Function Encoded : String; Overload; Virtual;
  End;

  THL7V2ContentNull = class (THL7V2Content)
  Public
    Function ContentType : THL7V2ContentType; Override;
    Function Text : String; Override;
    Function Encoded : String; Override;
  End;

  THL7V2ContentText = Class (THL7V2Content)
  Private
    FText : String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create(Const sValue : String); Overload; Virtual;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    Procedure SetText(Const sValue : String); Overload; Virtual;

    Function ContentType : THL7V2ContentType; Overload; Override;
    Function Text : String; Overload; Override;
    Function Encoded : String; Override;
  End;

  THL7V2ContentBinary = Class (THL7V2Content)
  Private
    FBinary : String; // but it's really a sequence of bytes
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create(Const sValue : String); Overload; Virtual;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    Function XMLText : String; Overload; Virtual;

    Function ContentType : THL7V2ContentType; Overload; Override;
    Function Text : String; Overload; Override;
    Function Encoded : String; Override;
  End;

  THL7V2ContentEscape = Class (THL7V2Content)
  Private
    FEscape : String; // but it's really a sequence of bytes
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create(Const sValue : String); Overload; Virtual;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    Function ContentType : THL7V2ContentType; Overload; Override;
    Function Text : String; Overload; Override;
    Function Encoded : String; Override;
  End;

  THL7V2Contents = Class (THL7V2BaseObjects)
  Private
    FComplex : Boolean;
    FPath : String;
    Function GetContent(iIndex: Integer): THL7V2Content;
  Protected
    Function ItemClass : TFslObjectClass; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create(Const sPath : String; const bComplex : Boolean);

    Function Link : THL7V2Contents; Overload;
    Function Clone : THL7V2Contents; Overload;

    Function IsSimple : Boolean;

    Property Contents[iIndex : Integer] : THL7V2Content Read GetContent; Default;
  End;

Type
  THL7V2CellType = (ctUnknown, ctDataElement, ctComponent, ctSubComponent);

  THL7V2Cells = Class;

  THL7V2Cell = Class (THL7V2BaseObject)
  Private
    FIndex: Integer; // 1 based
    FCellType : THL7V2CellType;
    FContents : THL7V2Contents;

    // the semantics of cells in HL7 V2 mean that if the cell has children, the
    // properties of the cell are delegated to first child. These property servers
    // implement this
    Function GetAsDate: TDateTime;
    Function GetAsDateTime: TDateTime;
    Function GetAsDateTimeNoTZ: TDateTime;
    Function GetAsFloat: Double;
    Function GetAsInteger: Integer;
    Function GetAsString: String;
    Function GetDefined: Boolean;
    Function GetIsRelevent: Boolean;
    Function GetRawContent: String;
    Procedure SetAsDate(Const aValue: TDateTime);
    Procedure SetAsDateTime(Const aValue: TDateTime);
    Procedure SetAsDateTimeNoTZ(Const aValue: TDateTime);
    Procedure SetAsFloat(Const rValue: Double);
    Procedure SetAsInteger(Const iValue: Integer);
    Procedure SetAsString(Const sValue: String);
    Procedure SetDefined(Const bValue: Boolean);

    Procedure CreateContents;
    Procedure ClearContents;

    Function GetSimpleTextContent : String;
    Procedure SetRawContent(Const sValue: String);

    Function GetChildren: THL7V2Cells;
    Procedure SetChildren(Const oValue: THL7V2Cells);

    Function GetElement(Const sCode: String): THL7V2Cell;
    function GetContents: THL7V2Contents;
    function GetNullify: Boolean;
    procedure SetNullify(const Value: Boolean);
    function GetAsPlainText: String;
    function GetDisplayForCode: String;
  Protected
    Function ChildrenClass : TFslTreeListClass; Override;

    // descendents must override
    Function GetTable: THL7V2ModelTable; Virtual;
    Function GetFieldName: String; Virtual;
    Function GetDataType: THL7V2ModelDataType; Virtual;

    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create(aType : THL7V2CellType; iIndex: Integer); Overload; Virtual;
    Destructor Destroy; Override;

    Function Link : THL7V2Cell; Overload;
    Function Clone : THL7V2Cell; Overload;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    Function AsTableCode(Var sCode: String): Boolean; Overload; Virtual;
    Function DateIsValid : Boolean; Overload; Virtual;
    Function DateIsValid(Var sError : String) : Boolean; Overload; Virtual;
    Function Encode(aFormat : THL7V2Format = hfUnknown; aOptions : THL7V2EncodingOptionSet = []): TBytes; Overload; Virtual;
    Function Encode(oOptions : THL7V2EncodingOptions): TBytes; Overload; Virtual;
    Function Escapable : Boolean; Overload; Virtual;
    Function ForceChild(iIndex : Integer) : THL7V2Cell; Overload; Virtual; // NOTE: 1 based not 0 based
    Function FullPathName: String; Override;
    Function HasContent : Boolean;
    Function HasDate: Boolean; Overload; Virtual;
    Function HasTable: Boolean; Overload; Virtual;
    Function TableValueValid: Boolean; Overload; Virtual;
    Procedure AddTableCode(Const sCode: String); Overload; Virtual;
    Procedure Clear; Override;
    Procedure ClearContent; Overload; Virtual;
    Procedure Clone(oSource : THL7V2Cell; bForce : Boolean); Overload; Virtual;
    Procedure Decode(oSource : TFslBuffer; oOptions : THL7V2DecodingOptions = Nil); Overload; Virtual;
    Procedure Decode(sSource : TBytes; oOptions : THL7V2DecodingOptions = Nil); Overload; Virtual;
    Procedure DecodeEx(iStart : integer; sSource : String; oOptions : THL7V2DecodingOptions; aLevelOperation : THL7v2DecodingLevelOperation); Overload; Virtual;
    Procedure Encode(oBuffer : TFslBuffer; oOptions : THL7V2EncodingOptions = Nil); Overload; Virtual;
    Procedure ForceElement(Const sCode: String);
    Procedure Commit; Overload; Virtual; // changes are done externally, update as appropriate. (OBX-2 sets type of OBX-5)
    Procedure TakeContent(oSrc : THL7V2Cell); Overload; Virtual;

    Function GetRawContentForScripting : String;

    function IsSimple : Boolean;
    function IsSimpleContent : Boolean;

    Property AsDate: TDateTime Read GetAsDate Write SetAsDate;
    Property AsDateTime: TDateTime Read GetAsDateTime Write SetAsDateTime;
    Property AsDateTimeNoTZ: TDateTime Read GetAsDateTimeNoTZ Write SetAsDateTimeNoTz;
    Property AsFloat: Double Read GetAsFloat Write SetAsFloat;
    Property AsInteger: Integer Read GetAsInteger Write SetAsInteger;
    Property AsString: String Read GetAsString Write SetAsString;
    Property CellType : THL7V2CellType Read FCellType Write FCellType;
    Property Children : THL7V2Cells Read GetChildren Write SetChildren;
    Property DataType: THL7V2ModelDataType Read GetDataType;
    Property Defined: Boolean Read GetDefined Write SetDefined;
    Property Element[Const sName : String] : THL7V2Cell Read GetElement; Default;
    Property Index: Integer Read FIndex Write FIndex; // NOTE: 1 based not 0 based
    Property IsRelevent: Boolean Read GetIsRelevent;
    Property RawContent: String Read GetRawContent Write SetRawContent;
    Property AsPlainText : String Read GetAsPlainText write SetRawContent;
    Property Contents : THL7V2Contents read GetContents;
    Property IsNullify : Boolean read GetNullify write SetNullify;
    Property Table: THL7V2ModelTable read GetTable;
    Property DisplayForCode : String read GetDisplayForCode;
  End;

  THL7V2Cells = Class (THL7V2BaseObjects)
  Private
    Function GetCell(iIndex: Integer): THL7V2Cell;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Cells[iIndex : Integer] : THL7V2Cell Read GetCell; Default;
  End;

Const
  NAMES_HL7V2CELLTYPE : Array [THL7V2CellType] Of String = ('Unknown', 'DataElement', 'Component', 'SubComponent');

type
  THL7V2Components = class;

  THL7V2Component = class(THL7V2Cell)
  Private
    FDefinition : THL7V2ModelComponent;
    function GetChildren: THL7V2Components;
    function GetComponent(iIndex: Integer): THL7V2Component;
    function GetParent: THL7V2Cell;
    procedure SetChildren(const oValue: THL7V2Components);
    procedure SetDefinition(const oValue: THL7V2ModelComponent);
    procedure SetParent(const oValue: THL7V2Cell);
  Protected
    Function ChildrenClass : TFslTreeListClass; override;
    function GetTable: THL7V2ModelTable; override;
    function GetFieldName: String; override;
    function GetDataType: THL7V2ModelDataType; override;

    function sizeInBytesV : cardinal; override;
  Public
    Destructor Destroy; override;

    Function Link : THL7V2Component; Overload;
    Function Clone : THL7V2Component; Overload;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    Function ForceChild(iIndex : Integer) : THL7V2Cell; override; // will return a THL7V2Component NOTE: 1 based not 0 based
    Function ForceComponent(iIndex : Integer) : THL7V2Component; overload; virtual; // NOTE: 1 based not 0 based
    function IsElement(const sName: String): Boolean; override;
    procedure MakeForBuild(bClear : Boolean = true);
    procedure StripUndefinedContent;
    procedure MapToDefinitions(Model: THL7V2ModelStructure; ndx: integer);

    Property Children : THL7V2Components Read GetChildren Write SetChildren;
    property Component[iIndex : Integer] : THL7V2Component read GetComponent; // NOTE: 1 based not 0 based
    Property Components : THL7V2Components Read GetChildren Write SetChildren;
    property Definition : THL7V2ModelComponent read FDefinition write SetDefinition;
    Property Parent : THL7V2Cell Read GetParent Write SetParent; // may be either a Component or a dataelement
  end;

  THL7V2Components = class (THL7V2Cells)
  Private
    function GetActiveCount: Integer;
    Function GetComponent(iIndex: Integer): THL7V2Component;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Component[iIndex : Integer] : THL7V2Component Read GetComponent; Default;
    Property ActiveCount : Integer Read GetActiveCount;
  End;

type

  THL7V2DataElementCloneOption = (coAllowUnknownContent, coOverwriteEmpty, coOverwriteUnDefined);
  THL7V2DataElementCloneOptions = Set of THL7V2DataElementCloneOption;

const
  HL7V2_ALL_CLONE_OPTIONS = [coAllowUnknownContent, coOverwriteEmpty, coOverwriteUnDefined];
  NAMES_HL7V2DATAELEMENTCLONEOPTIONS : array [THL7V2DataElementCloneOption] of String = ('AllowUnknownContent', 'OverwriteEmpty', 'OverwriteUnDefined');

type
  THL7V2DataElements = class;
  THL7V2DataElement = class(THL7V2Cell)
  Private
    FDefinition: THL7V2ModelField;
    FSpecifiedDataType : String; // for OBX-5 and for XML encoding outside the normal definitions
    FRepeatIndex : Integer;
    FRepeats : THL7V2DataElements;
    function GetChildren: THL7V2Components;
    function GetComponent(iIndex: Integer): THL7V2Component;
    function GetRepeat(iIndex: Integer): THL7V2DataElement;
    function GetRepeats: THL7V2DataElements; // will be nil in repeats, where the parent will be a THL7V2DataElement
    procedure SetChildren(const oValue: THL7V2Components);
    procedure SetDefinition(const oValue: THL7V2ModelField);
    procedure SetRepeats(const oValue: THL7V2DataElements);
    function GetRepeatCount: Integer;
    procedure CloneRepeat(oSource: THL7V2DataElement; aOptions: THL7V2DataElementCloneOptions);
  Protected
    Function ChildrenClass : TFslTreeListClass; override;
    Procedure Reparent; Override;
    function GetTable: THL7V2ModelTable; override;
    function GetFieldName: String; override;
    function GetDataType: THL7V2ModelDataType; override;

    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    Function Link : THL7V2DataElement; Overload;
    Function Clone : THL7V2DataElement; Overload;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    function AddRepeat(bFirstIfEmpty : Boolean): THL7V2DataElement;
    function CanHaveRepeats : Boolean;
    function ChooseRepeat(const sLocation, sValue: String) : THL7V2DataElement;
    Procedure DeleteRepeat(iIndex : Integer); Overload; Virtual;
    Function ForceRepeat(iIndex : Integer) : THL7V2DataElement; Overload; Virtual;
    Function ForceChild(iIndex : Integer) : THL7V2Cell; override; // will return a THL7V2DataElement NOTE: 1 based not 0 based
    Function ForceComponent(iIndex : Integer) : THL7V2Component; overload; virtual; // NOTE: 1 based not 0 based
    function FullPathName: String; override;
    function HasRepeats : Boolean;
    function IsElement(const sName: String): Boolean; override;
//    procedure ChangeVersion(ANewDict: THL7Dictionary; ADeleteInvalidContent: Boolean; ADefinition: THL7DictSegmentField);
    procedure ClearContent; override;
    Procedure Commit; Override;
    procedure StripUndefinedContent;

    Procedure Decode(oSource : TFslBuffer; oOptions : THL7V2DecodingOptions = Nil); Overload; Override;
    Procedure Decode(sSource : TBytes; oOptions : THL7V2DecodingOptions = Nil); Overload; Override;
    Procedure Encode(oBuffer : TFslBuffer; oOptions : THL7V2EncodingOptions = Nil); Overload; Override;
    Function Encode(aFormat : THL7V2Format = hfUnknown; aOptions : THL7V2EncodingOptionSet = []): TBytes; Overload; Override;
    Function Encode(oOptions : THL7V2EncodingOptions): TBytes; Overload; Override;

    procedure Clone(oSource : THL7V2DataElement; aOptions: THL7V2DataElementCloneOptions); overload;
    procedure MakeForBuild(bClear : Boolean = true);
    procedure SpecifyDataType(const sType: String); // used for whether escaping is required/allowed on OBX-5
    procedure MapToDefinitions(Model: THL7V2ModelSegment; ndx : integer);

    property _Repeat[iIndex : Integer] : THL7V2DataElement read GetRepeat; // NOTE: 1 based not 0 based - and 0 is self in this case
    Property Children : THL7V2Components Read GetChildren Write SetChildren;
    property Component[iIndex : Integer] : THL7V2Component read GetComponent; // NOTE: 1 based not 0 based
    Property Components : THL7V2Components Read GetChildren Write SetChildren;
    property Definition : THL7V2ModelField read FDefinition write SetDefinition;
//    Property Parent : THL7V2Cell Read GetParent Write SetParent; // may be either a THL7V2DataElement or a THL7V2Segment, but we can't refer to the latter from here
    property RepeatCount : Integer read GetRepeatCount;
    property RepeatIndex : Integer read FRepeatIndex write FRepeatIndex;
    Property Repeats : THL7V2DataElements Read GetRepeats Write SetRepeats; // only created if needed (must be this way to stop infinite recursion)
    property SpecifiedDataType : String read FSpecifiedDataType write FSpecifiedDataType;
  end;

  THL7V2DataElements = class (THL7V2Cells)
  Private
    Function GetDataElement(iIndex: Integer): THL7V2DataElement;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Function Link : THL7V2DataElements; Overload;
    Function Clone : THL7V2DataElements; Overload;
    Property DataElement[iIndex : Integer] : THL7V2DataElement Read GetDataElement; Default;
  End;

type
  THL7V2Segment = class (THL7V2BaseObject)
  Private
    FCode : String;
    FDefinition : THL7V2ModelSegment;
    function GetChildren: THL7V2DataElements;
    function GetElement(const sCode: String): THL7V2Cell;
    function GetField(iIndex: Integer): THL7V2DataElement;
    procedure SetChildren(const oValue: THL7V2DataElements);
    procedure SetCode(const sValue: String);
    procedure SetDefinition(const oValue: THL7V2ModelSegment);
    function GetIndex: Integer;
    Function SequenceNumber : Integer;
  Protected
    Function ChildrenClass : TFslTreeListClass; override;
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;

    Function Link : THL7V2Segment; Overload;
    Function Clone : THL7V2Segment; Overload;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    Function Encode(aFormat : THL7V2Format = hfUnknown; aOptions : THL7V2EncodingOptionSet = []): TBytes; overload;
    Function Encode(oOptions : THL7V2EncodingOptions): TBytes; overload;
    Function ForceField(iIndex : Integer) : THL7V2DataElement;
    function FullPathName: String; override;
    function IsElement(const sName: String): Boolean; override;
    function Matches(sMask: String): Boolean;
    procedure BuildFields;
    //  procedure ChangeVersion(ANewDict: THL7Dictionary; ADeleteInvalidContent: Boolean; ADefinition: THL7DictSegment);
    procedure Clear; override;
    procedure ClearContent;
    procedure Clone(oSource: THL7V2Segment; aOptions: THL7V2DataElementCloneOptions); overload;
    procedure Decode(oSource : TFslBuffer; oOptions : THL7V2DecodingOptions = nil); overload;
    procedure Decode(sSource : TBytes; oOptions : THL7V2DecodingOptions = nil); overload;
    procedure Encode(oBuffer : TFslBuffer; oOptions : THL7V2EncodingOptions = nil); overload;
    procedure ForceElement(const sCode : String);
    procedure StripUndefinedContent;
    procedure MapToDefinitions(Model : THL7V2Model);

    Property Children : THL7V2DataElements Read GetChildren Write SetChildren;
    Property Code : String read FCode write SetCode;
    property Definition : THL7V2ModelSegment read FDefinition write SetDefinition;
    property Element[const sCode: String]: THL7V2Cell Read GetElement; default;
    property Field[iIndex : Integer] : THL7V2DataElement read GetField; // NOTE: 1 based not 0 based
    Property Fields : THL7V2DataElements Read GetChildren Write SetChildren;
    property Index : Integer read GetIndex;
//    Property Parent : THL7V2BaseObject Read GetParent Write SetParent; can be either a message or a batch....
  end;

  THL7V2Segments = class (THL7V2Cells)
  Private
    Function GetSegment(iIndex: Integer): THL7V2Segment;
  Protected
    Function ItemClass : TFslObjectClass; Override;
    Function CompareByCode(pA, pB : Pointer) : Integer; Overload; Virtual;
    Function Get(Const iIndex : Integer) : THL7V2Segment; Reintroduce; Overload; Virtual;
  Public
    Function IndexByCode(Const sValue : String) : Integer; Overload; Virtual;
    Function GetByCode(Const sValue : String) : THL7V2Segment; Overload; Virtual;
    Function ExistsByCode(Const sValue : String) : Boolean; Overload; Virtual;

    function CountByType(const sCode : String) : integer;
    procedure DropByType(const sCode : String);
    function GetByCodeAndIndex(const sCode: String; iIndex: Integer): THL7V2Segment;

    function IndexInType(oSegment : THL7V2Segment) : integer;
    function AsText : String;

    Property Segment[iIndex : Integer] : THL7V2Segment Read GetSegment; Default;
  End;

  THL7V2SegmentList = class (THL7V2BaseObjectList)
  private
    function GetSegment(iIndex: integer): THL7V2Segment;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  public
    property Segment[iIndex : integer] : THL7V2Segment read GetSegment; default;
  End;

  // can't refer to segments directly as part of the tree - can't have dual ownership in the tree.
  // so segments are contained in unnamed groups

  THL7V2SegmentGroups = class;
  THL7V2SegmentGroup = class (TFslTree)
  private
    FName: String;
    FSegments: THL7V2SegmentList;
    function GetChildren: THL7V2SegmentGroups;
    function GetParent: THL7V2SegmentGroup;
    procedure SetChildren(const oValue: THL7V2SegmentGroups);
    procedure SetParent(const oValue: THL7V2SegmentGroup);
    procedure SetSegments(const oValue: THL7V2SegmentList);

  Protected
    Function ChildrenClass : TFslTreeListClass; override;
    Function ParentClass : TFslTreeClass; override;
    function sizeInBytesV : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Link : THL7V2SegmentGroup; overload;
    function Clone : THL7V2SegmentGroup; overload;

    procedure AddSegment(oSegment : THL7V2Segment);

    Property Parent : THL7V2SegmentGroup Read GetParent Write SetParent;
    Property Children : THL7V2SegmentGroups Read GetChildren Write SetChildren;

    property Name : String read FName write FName;
    property Segments : THL7V2SegmentList read FSegments write SetSegments;
  end;

  THL7V2SegmentGroups = class (TFslTreeList)
  Private
    Function GetGroup(iIndex: Integer): THL7V2SegmentGroup;
  Protected
      Function ItemClass : TFslObjectClass; Override;
  Public
    Property Group[iIndex : Integer] : THL7V2SegmentGroup Read GetGroup; Default;
  end;

Const
  DEFAULT_ENCODING_OPTIONS = [eoEscapeExtendedCharacters];

Type
  THL7V2Message = Class (THL7V2ModelProvider)
  Private
    FDefinition : THL7V2ModelMessageStructure;
    FSegmentMap: THL7V2SegmentGroup;
    FEvent : String;
    FMessageType : String;
    FStructID :  String;
    FMessageID: String;
    FFormat : THL7V2Format;
    FDelimiters : THL7V2Delimiters;
    FSource: TBytes;

    Function GetEvent: String;
    Function GetMessageType : String;
    Function GetStructID :  String;

    Procedure SetEvent(Const sValue : String);
    Procedure SetMessageType(Const sValue : String);
    Procedure SetStructID(Const sValue : String);

    // MSH Wrappers
    Function GetMessageID : String;
    Procedure SetMessageID(Const sValue : String);

    Function GetChildren: THL7V2Segments;
    Procedure SetChildren(Const Value: THL7V2Segments);
    Function GetElement(Const sCode: String): THL7V2Cell;
    Function GetSegment(Const sCode: String; iIndex: Integer): THL7V2Segment;

    Procedure DropMap;
    Procedure DropDefinition;
    Procedure TryLoadStruct;
    Procedure SetDelimiters(Const Value: THL7V2Delimiters);
    Procedure PrepareForEncoding(oOptions: THL7V2EncodingOptions);
    Procedure ResolveVariableTypes;

    Procedure CheckSegment(oGroup, oChild : THL7V2ModelSegmentGroup; oOptions : THL7V2EncodingOptions; Var iSegment : Integer; Var bFound, bFinished : Boolean; Var oResult : THL7V2SegmentGroup; bOptional : Boolean);
    Procedure CheckGroup(oGroup, oChild : THL7V2ModelSegmentGroup; oOptions : THL7V2EncodingOptions; Var iSegment : Integer; Var bFound, bFinished : Boolean; Var oResult : THL7V2SegmentGroup; bOptional : Boolean);
    Function BuildSegmentGroup(oGroup : THL7V2ModelSegmentGroup; Var iSegment : Integer; oOptions : THL7V2EncodingOptions; bOptional : Boolean) : THL7V2SegmentGroup;
  Protected
    Function ChildrenClass : TFslTreeListClass; Override;
    Procedure SetVersion(aValue  : THL7V2Version); Override;
    Function CanChangeVersion : Boolean; Override;
    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Link : THL7V2Message;
    Function Clone : THL7V2Message; overload;
    Procedure Assign(oObject : TFslObject); Overload; Override;

    Function AddMSHForBuild : THL7V2Segment;
    Function AddSegment(Const sCode: String): THL7V2Segment;
    Function CountSegment(Const sCode: String): Integer;
    Function Encode(aFormat : THL7V2Format = hfUnknown; aOptions : THL7V2EncodingOptionSet = []): TBytes; Overload;
    Function Encode(oOptions : THL7V2EncodingOptions): TBytes; Overload;
    Function FullPathName: String; Override;
    Function InsertSegment(iIndex: Integer; Const sCode: String): THL7V2Segment;
    Function IsElement(Const sName: String): Boolean; Override;
    Function Query(Const sCode : String; oResults : THL7V2BaseObjectList) : Boolean;
    Function QueryType(Const sCode : String) : THL7V2BaseObjectClass;
    Procedure BindToMSH; // called by the parsers
    Procedure BuildAsReply(oSource: THL7V2Message; bUseSequenceNum: Boolean = False; iSequenceNum: Int64 = 0);
    Procedure BuildSegmentMap(bForXML : Boolean; oOptions: THL7V2EncodingOptions); // called by the XML Parser. No other reason to use it
    Procedure Clear; Override; // drop metadata and data
    Procedure CloneSegment(oSource: THL7V2Segment); // copy a segment from another message
    Procedure Decode(oSource : TFslBuffer; oOptions : THL7V2DecodingOptions = Nil); Overload;
    Procedure Decode(sSource : TBytes; oOptions : THL7V2DecodingOptions = Nil); Overload;
    Procedure DropSegments(Const sCode: String);
    Procedure Encode(oBuffer : TFslBuffer; oOptions : THL7V2EncodingOptions = Nil); Overload;
    Procedure ForceElement(Const sCode : String);
    Procedure SetException(oExcept: Exception; bOldStyle : Boolean);
    Procedure SetExceptionMessage(Const sMessage: String; bMakeReject: Boolean = False);
    Procedure SetExceptionCode(Const sMessage, sResponseCode, SErrorCode : String);

    Procedure StripEmptyRepeats(bLeading: Boolean);
    Procedure UpdateSource; // deprecated, but used widely by HL7Connect
    procedure Clone(oSource: THL7V2Message; aOptions: THL7V2DataElementCloneOptions); overload;
    Function SegmentQuery(query : String):THL7V2SegmentList;
    procedure ChangeVersion(ANewVersion: String; ADeleteInvalidContent: Boolean); Reintroduce; overload;

    Function AddNOKToMsg(AName, AGiven, ATitle, AAddress1, AAddress2, AAddress3, AAddress4, APostCode, AHomephone, AWorkphone, ARelationship : String) : THL7V2Segment;
    Function GetHL7V2MessagePatientID(SuppressExceptions: Boolean): String;
    class Function HardCodedNack(AVersion, AErrMsg, AMsgid: String): String;
    class Function HL7PacketReplaceMSHField(FieldNumber: Integer; Field: String; Var HL7Msg: TBytes): Boolean;
    class procedure CheckHL7MessagesEquivalent(AMsg1, AMsg2: TBytes; const ALeafNodesThatCanDiffer: array of String); // unit testing support

    Function HasDelimiters : Boolean;

    Property Children : THL7V2Segments Read GetChildren Write SetChildren;
    Property Definition : THL7V2ModelMessageStructure Read FDefinition;
    Property Delimiters : THL7V2Delimiters Read FDelimiters Write SetDelimiters;
    Property Element[Const sCode: String]: THL7V2Cell Read GetElement; Default;
    Property Event: String Read GetEvent Write SetEvent;
    Property Format : THL7V2Format Read FFormat Write FFormat;
    Property MessageID: String Read GetMessageID Write SetMessageID;
    Property MessageType: String Read GetMessageType Write SetMessageType;
//    Property Parent : THL7V2BaseObject Read GetParent Write SetParent; can be nothing or a BatchMessage
    Property Segment[Const sCode: String; iIndex: Integer]: THL7V2Segment Read GetSegment; // Segment 0 is the first instance of a given segment
    Property SegmentMap: THL7V2SegmentGroup Read FSegmentMap;
    Property Segments : THL7V2Segments Read GetChildren Write SetChildren;
    Property Source : TBytes Read FSource; // deprecated, but used widely by HL7Connect
    Property StructName: String Read GetStructID Write SetStructID;
  End;

Type
  // BatchElements don't have children - but they do have a message,
  // if the content can be parsed.
  THL7V2BatchMessage = class(THL7V2BaseObject)
  Private
    FContent: TBytes;
    FMessage : THL7V2Message;
    FId: String;
    procedure SetMessage(const oValue: THL7V2Message);
    procedure SetContent(const sValue: TBytes);
  Protected
    function sizeInBytesV : cardinal; override;
  Public
    Destructor Destroy; override;
    function Link : THL7V2BatchMessage; overload;
    function Clone : THL7V2BatchMessage; overload;

    function FullPathName: String; override;
    function IsElement(const sName: String): Boolean; override;

    procedure Decode(oOptions : THL7V2DecodingOptions);

    property Id : String read FId write FId;
    property Content: TBytes Read FContent Write SetContent;
    property Message : THL7V2Message read FMessage write SetMessage;

    Function ForceMessage : THL7V2Message;
  end;

  THL7V2BatchMessages = class (THL7V2BaseObjects)
  Private
    Function GetBatchMessage(iIndex: Integer): THL7V2BatchMessage;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property BatchMessages[iIndex : Integer] : THL7V2BatchMessage Read GetBatchMessage; Default;
  End;

type
  // note regarding versions on batches:
  // the BHS does not carry a version code, so the version assigned does not go in the
  // message. Hence the recommendation is that as high a version as possible is
  // used when reading batches, and as low a version as appropriate when writing

  THL7V2Batch = class (THL7V2ModelProvider)
  private
    FFormat : THL7V2Format;
    FDelimiters : THL7V2Delimiters;
    FHeader: THL7V2Segment;
    FTrailer: THL7V2Segment;
    function GetBatchID: String;
    function GetChildren: THL7V2BatchMessages;
    function GetReplyBatchID: String;
    procedure SetBatchID(const Value: String);
    procedure SetChildren(const Value: THL7V2BatchMessages);
    procedure SetDelimiters(const Value: THL7V2Delimiters);
    procedure SetReplyBatchID(const Value: String);
  protected
    Function ChildrenClass : TFslTreeListClass; override;
    procedure SetVersion(aValue  : THL7V2Version); override;

    function sizeInBytesV : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function Link : THL7V2Batch; overload;
    function Clone : THL7V2Batch; overload;

    function FullPathName: String; override;
    function IsElement(const sName: String): Boolean; override;

    procedure Clear; override;

    Property Children : THL7V2BatchMessages Read GetChildren Write SetChildren;
    Property Messages : THL7V2BatchMessages Read GetChildren Write SetChildren;
//    Property Parent : THL7V2BaseObject Read GetParent Write SetParent; can be either nothing or a batch....
    property Header : THL7V2Segment read FHeader;
    property Trailer : THL7V2Segment read FTrailer;
    property Delimiters : THL7V2Delimiters read FDelimiters write SetDelimiters;
    property Format : THL7V2Format read FFormat write FFormat;
    property BatchID: String Read GetBatchID Write SetBatchID;
    property ReplyBatchID: String Read GetReplyBatchID Write SetReplyBatchID;
  end;

  THL7V2Batches = class (THL7V2BaseObjects)
  Private
    Function GetBatch(iIndex: Integer): THL7V2Batch;
  Protected
    Function ItemClass : TFslObjectClass; Override;
  Public
    Property Batches[iIndex : Integer] : THL7V2Batch Read GetBatch; Default;
  End;

Type
  // note regarding versions on Files:
  // the BHS does not carry a version code, so the version assigned does not go in the
  // message. Hence the recommendation is that as high a version as possible is
  // used when reading Files, and as low a version as appropriate when writing

  THL7V2File = Class (THL7V2ModelProvider)
  Private
    FFormat : THL7V2Format;
    FDelimiters : THL7V2Delimiters;
    FHeader: THL7V2Segment;
    FTrailer: THL7V2Segment;
    Function GetFileID: String;
    Function GetChildren: THL7V2Batches;
    Function GetReplyFileID: String;
    Procedure SetFileID(Const Value: String);
    Procedure SetChildren(Const Value: THL7V2Batches);
    Procedure SetDelimiters(Const Value: THL7V2Delimiters);
    Procedure SetReplyFileID(Const Value: String);
  Protected
    Function ChildrenClass : TFslTreeListClass; Override;
    Procedure SetVersion(aValue  : THL7V2Version); Override;

    function sizeInBytesV : cardinal; override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Function Link : THL7V2File; Overload;
    Function Clone : THL7V2File; Overload;

    Procedure Clear; Override;
    Function MessageTotal : Integer;

    Function FullPathName: String; Override;
    Function IsElement(Const sName: String): Boolean; Override;

    Procedure Load(Const sFileName : String; oOptions : THL7V2DecodingOptions = nil); Overload; Virtual;
    Procedure Save(Const sFileName : String; oOptions : THL7V2EncodingOptions = nil); Overload; Virtual;
    Procedure Load(Const oStream : TFslStream; oOptions : THL7V2DecodingOptions = nil); Overload; Virtual;
    Procedure Save(Const oStream : TFslStream; oOptions : THL7V2EncodingOptions = nil); Overload; Virtual;
    Procedure Load(Const bytes : TBytes; oOptions : THL7V2DecodingOptions = nil); Overload; Virtual;
    Procedure Save(var bytes : TBytes; oOptions : THL7V2EncodingOptions = nil); Overload; Virtual;

    Property Children : THL7V2Batches Read GetChildren Write SetChildren;
    Property Batches : THL7V2Batches Read GetChildren Write SetChildren;
//    Property Parent : THL7V2BaseObject Read GetParent Write SetParent; can be either nothing or a File....
    Property Header : THL7V2Segment Read FHeader;
    Property Trailer : THL7V2Segment Read FTrailer;
    Property Delimiters : THL7V2Delimiters Read FDelimiters Write SetDelimiters;
    Property Format : THL7V2Format Read FFormat Write FFormat;
    Property FileID: String Read GetFileID Write SetFileID;
    Property ReplyFileID: String Read GetReplyFileID Write SetReplyFileID;
  End;

type
  THL7V2BatchIterator = class (THL7V2WorkerObject)
  private
    FBatch: THL7V2File;
    FCurrent : THL7V2BatchMessage;
    FBatchIndex : Integer;
    FMessageIndex : Integer;
    procedure SetBatch(const Value: THL7V2File);
    function GetPosition: integer;
    procedure SetPosition(iValue: integer);
  protected
    function sizeInBytesV : cardinal; override;
  public
    Destructor Destroy; override;
    procedure Reset;
    procedure First;
    procedure Previous;
    procedure Next;
    procedure Last;
    function More : Boolean;
    function Current : THL7V2BatchMessage;
    function CurrentBatch : THL7V2Batch;

    Procedure SetCurrent(oBatch : THL7V2BatchMessage);

    property Batch : THL7V2File read FBatch write SetBatch;
    property Position : integer read GetPosition write SetPosition;
  end;

Type
  THL7V2ParsedQueryIntegerRange = record
    Low : Integer;
    High : Integer;
  end;

  THL7V2ParsedQueryRange = class (THL7V2WorkerObject)
  private
    FList : Array of THL7V2ParsedQueryIntegerRange;
    function Selected (AValue : Integer) : Boolean;
    function Min : Integer;
    function Max(AListCount : Integer):Integer;
    procedure Parse(var VQuery : String);
  end;

  THL7V2ParsedQuery = class (THL7V2WorkerObject)
  private
    FSegQuery : String;
    FSegReg : TRegEx;
    FHasSegReg : boolean;
    FSegIndex : THL7V2ParsedQueryRange;
    FDE : THL7V2ParsedQueryRange;
    FRep : THL7V2ParsedQueryRange;
    FDERegEx : TRegEx;
    FHasDERegEx : boolean;
    FComp : THL7V2ParsedQueryRange;
    FCompRegEx : TRegEx;
    FHasCompRegEx : boolean;
    FSubComp : THL7V2ParsedQueryRange;
    FSubCompRegEx : TRegEx;
    FHasSubCompRegEx : boolean;
    procedure parseDE(var VQuery : String);
    procedure parseComp(var VQuery : String);
    procedure parseSubComp(var VQuery : String);

    function SegMatches(ACode : String):Boolean;

    procedure SelectSubComponents(AComp : THL7V2Component; AResults: THL7V2BaseObjectList);
    procedure SelectComponents(AField : THL7V2DataElement; AResults: THL7V2BaseObjectList);
    procedure SelectDataElements(ASegment : THL7V2Segment; AResults: THL7V2BaseObjectList);
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    procedure parse(AQuery : String);
    procedure SelectSegments(ASegList : THL7V2Segments; AResults : THL7V2BaseObjectList);
    function ResultType : THL7V2BaseObjectClass;
  end;

Type
  THL7V2QueryParser = class (THL7V2WorkerObject)
  private
    FContent : String;
    FCursor : Integer;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor Create(sQuery : String);
    function Peek : String;
    function Consume : String;
    function Finished : Boolean;

    procedure NextNonWhitespace;
    procedure Check(bCondition : Boolean; sMethod, sMessage : String);

    property Content : String read FContent;
  end;

  THL7V2QueryContextType = (qctMessage, qctSegment, qctField);

  THL7V2Query = Class;

  THL7V2QueryOp = (qoVoid, qoAdd, qoPlus, qoConcat, qoSubtract, qoEqual, qoSame, qoNotEqual, qoLess, qoGreater, qoLessEqual, qoGreaterEqual, qoAnd, qoOr, qoXor, qoRegex, qoStartsWith, qoEndsWith);

  THL7V2QueryItem = class (THL7V2WorkerObject)
  public
    function Evaluate(oSegment : THL7V2Segment; iInstance : integer): String; overload; virtual;
    function Evaluate(oDataElement : THL7V2DataElement; iInstance : integer): String; overload; virtual;
  end;

  THL7V2QueryNode = class (THL7V2WorkerObject)
  private
    FItem : THL7V2QueryItem;
    FOp : THL7V2QueryOp;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Destructor Destroy; override;
  end;

  THL7V2QueryNodes = class (TFslObjectList)
  private
    function GetNode(iIndex : integer):THL7V2QueryNode;
  protected
    function ItemClass : TFslObjectClass; override;
  public
    property Node[iIndex : integer] : THL7V2QueryNode read GetNode; default;
  end;

  THL7V2QueryConditional = class (THL7V2WorkerObject)
  private
    FLeft : THL7V2QueryNodes;
    FRight : THL7V2QueryNodes;
    FCompOp : THL7V2QueryOp;
    FLinkOp : THL7V2QueryOp;
    function Add(sLeft, sRight : String) : String;
    function Plus(sLeft, sRight : String) : String;
    function Concat(sLeft, sRight : String) : String;
    function Subtract(sLeft, sRight : String) : String;
    function Equal(sLeft, sRight : String):Boolean;
    function DoRegex(sLeft, sRight : String):Boolean;
    function Same(sLeft, sRight : String):Boolean;
    function NotEqual(sLeft, sRight : String):Boolean;
    function Less(sLeft, sRight : String):Boolean;
    function Greater(sLeft, sRight : String):Boolean;
    function LessEqual(sLeft, sRight : String):Boolean;
    function GreaterEqual(sLeft, sRight : String):Boolean;
    function Check(oSegment : THL7V2Segment; iInstance : integer): Boolean; overload;
    function Check(oDataElement : THL7V2DataElement; iInstance : integer): Boolean; overload;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;
  end;

  THL7V2QueryConditionals = class (TFslObjectList)
  private
    function GetConditional(iIndex : integer):THL7V2QueryConditional;
  protected
    function ItemClass : TFslObjectClass; override;
  public
    property Conditional[iIndex : integer] : THL7V2QueryConditional read GetConditional; default;
  end;

  THL7V2QueryCondition = class (THL7V2WorkerObject)
  private
    FConditions : THL7V2QueryConditionals;
    procedure ParseQuery(oParser : THL7V2QueryParser; aContextType : THL7V2QueryContextType);
    procedure ParseIndex(oParser : THL7V2QueryParser);
    function  GetOp(sToken: String; out aOp : THL7V2QueryOp):Boolean;
    procedure AddOp(oParser : THL7V2QueryParser; aOp: THL7V2QueryOp);
    procedure AddItem(oParser : THL7V2QueryParser; oItem: THL7V2QueryItem);
    procedure CheckCondition(oParser : THL7V2QueryParser; oCond : THL7V2QueryConditional);
    procedure CheckTree(oParser : THL7V2QueryParser);
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    function CheckConditions(oSegment : THL7V2Segment; iInstance : integer): Boolean; overload;
    function CheckConditions(oDataElement : THL7V2DataElement; iInstance : integer): Boolean; overload;
  end;

  THL7V2Query = class (THL7V2WorkerObject)
  private
    FSegCode : String;
    FSegCondition : THL7V2QueryCondition;
    FFieldId : String;
    FRepeatCondition : THL7V2QueryCondition;
    FCompId : String;
    FSubCompId : String;
    procedure ParseQuery(oParser : THL7V2QueryParser; aContextType : THL7V2QueryContextType); overload;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Destructor Destroy; override;
    procedure ParseQuery(sQuery : String; aContextType : THL7V2QueryContextType); overload;
    function Execute(oMessage : THL7V2Message; bForce : Boolean) : THL7V2Cell; overload;
    function Execute(oSegment : THL7V2Segment; bForce : Boolean) : THL7V2Cell; overload;
    function Execute(oField   : THL7V2DataElement; bForce : Boolean) : THL7V2Cell; overload;
  end;

  THL7V2QueryStringConstant = class (THL7V2QueryItem)
  private
    FValue : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor create(sToken : String);
    function Evaluate(oSegment : THL7V2Segment; iInstance : integer): String; override;
    function Evaluate(oDataElement : THL7V2DataElement; iInstance : integer): String; override;
  end;

  THL7V2QueryIndexRef = class (THL7V2QueryItem)
  public
    Constructor create(sToken : String);
    function Evaluate(oSegment : THL7V2Segment; iInstance : integer): String; override;
    function Evaluate(oDataElement : THL7V2DataElement; iInstance : integer): String; override;
  end;

  THL7V2QueryNestedQuery = class (THL7V2QueryItem)
  private
    FQuery : THL7V2Query;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor create(oParser : THL7V2QueryParser; aContextType : THL7V2QueryContextType);
    Destructor Destroy; override;
    function Evaluate(oSegment : THL7V2Segment; iInstance : integer): String; override;
    function Evaluate(oDataElement : THL7V2DataElement; iInstance : integer): String; override;
  end;

const
  NAMES_QUERY_OPS : Array [THL7V2QueryOp] of String = ('Void', 'Add', 'Plus', 'Concat', 'Subtract', 'Equal', 'Same',
             'NotEqual', 'Less', 'Greater', 'LessEqual', 'GreaterEqual', 'And', 'Or', 'Xor', 'Regex', 'StartsWith', 'EndsWith');

Type
  THL7V2Decoder = Class (THL7V2DictionaryProvider)
  Protected
    Function GetDelimiter(sName, sStr: String; iIndex: Integer): Char;
    function getOptions(opt : THL7V2DecodingOptions) : THL7V2DecodingOptions;

  Public
    Procedure DecodeCell(oCell : THL7V2Cell; oContent : TFslBuffer; oOptions : THL7V2DecodingOptions); Overload; Virtual;
    Procedure DecodeCell(oCell : THL7V2Cell; Const sContent : TBytes; oOptions : THL7V2DecodingOptions); Overload; Virtual;
    Procedure DecodeCellEx(oCell : THL7V2Cell; iStart : integer; Const sContent : TBytes; oOptions : THL7V2DecodingOptions; aLevelOperation : THL7v2DecodingLevelOperation); Overload; Virtual;
    Procedure DecodeDataElement(oDataElement : THL7V2DataElement; oContent : TFslBuffer; oOptions : THL7V2DecodingOptions); Overload; Virtual;
    Procedure DecodeDataElement(oDataElement : THL7V2DataElement; Const sContent : TBytes; oOptions : THL7V2DecodingOptions); Overload; Virtual;
    Procedure DecodeSegment(oSegment : THL7V2Segment; oContent : TFslBuffer; oOptions : THL7V2DecodingOptions); Overload; Virtual;
    Procedure DecodeSegment(oSegment : THL7V2Segment; Const sContent : TBytes; oOptions : THL7V2DecodingOptions); Overload; Virtual;
    Procedure DecodeMessage(oMessage : THL7V2Message; oContent : TFslBuffer; oOptions : THL7V2DecodingOptions); Overload; Virtual;
    Procedure DecodeMessage(oMessage : THL7V2Message; Const sContent : TBytes; oOptions : THL7V2DecodingOptions); Overload; Virtual;
    Procedure DecodeFile(oFile : THL7V2File; oStream : TFslStream; oOptions : THL7V2DecodingOptions); Overload; Virtual;

    function parse(msg : TBytes; options : THL7V2DecodingOptions = nil) : THL7V2Message; overload;
    function parse(msg : String; options : THL7V2DecodingOptions = nil) : THL7V2Message; overload;
    function parse(msg : TStream; options : THL7V2DecodingOptions = nil) : THL7V2Message; overload;
    function parse(msg : TFslStream; options : THL7V2DecodingOptions = nil) : THL7V2Message; overload;
    function parse(msg : TFslBuffer; options : THL7V2DecodingOptions = nil) : THL7V2Message; overload;
  End;

Type
  THL7V2DecoderUse = (fuMessage, fuSegment, fuDataElement, fuComponent);

  THL7V2DecoderFactory = class (THL7V2DictionaryProvider)
  private
    function RecogniseMessageFormat(const sSource: TBytes): THL7V2Format;
    function RecogniseSegmentFormat(const sSource: TBytes): THL7V2Format;
    function RecogniseCellFormat(const sSource: TBytes): THL7V2Format;
  public
    Function RecogniseFormat(oBuffer : TFslBuffer; oOptions : THL7V2DecodingOptions = nil; aUse : THL7V2DecoderUse = fuMessage) : THL7V2Format; overload; virtual;
    Function RecogniseFormat(const sSource : TBytes; oOptions : THL7V2DecodingOptions = nil; aUse : THL7V2DecoderUse = fuMessage) : THL7V2Format; overload; virtual;
    Function ProduceDecoder(aFormat : THL7V2Format) : THL7V2Decoder; overload; virtual;
    Function ProduceDecoder(oBuffer : TFslBuffer; oOptions : THL7V2DecodingOptions = nil; aUse : THL7V2DecoderUse = fuMessage) : THL7V2Decoder; overload; virtual;
    Function ProduceDecoder(const sSource : TBytes; oOptions : THL7V2DecodingOptions = nil; aUse : THL7V2DecoderUse = fuMessage) : THL7V2Decoder; overload; virtual;
    Procedure ConsumeDecoder(oDecoder: THL7V2Decoder); overload; virtual;
  end;

var
  GHL7V2DecoderFactory : THL7V2DecoderFactory;

type
  THL7V2Encoder = class (THL7V2DictionaryProvider)
  public
    Function EncodeCell(oCell : THL7V2Cell; oOptions : THL7V2EncodingOptions) : TBytes; overload; virtual;
    procedure EncodeCell(oBuffer : TFslBuffer; oCell : THL7V2Cell; oOptions : THL7V2EncodingOptions); overload; virtual;
    Function EncodeDataElement(oDataElement : THL7V2DataElement; oOptions : THL7V2EncodingOptions) : TBytes; overload; virtual;
    procedure EncodeDataElement(oBuffer : TFslBuffer; oDataElement : THL7V2DataElement; oOptions : THL7V2EncodingOptions); overload; virtual;
    Function EncodeSegment(oSegment : THL7V2Segment; oOptions : THL7V2EncodingOptions) : TBytes; overload; virtual;
    procedure EncodeSegment(oBuffer : TFslBuffer; oSegment : THL7V2Segment; oOptions : THL7V2EncodingOptions); overload; virtual;
    Function EncodeMessage(oMessage : THL7V2Message; oOptions : THL7V2EncodingOptions) : TBytes; overload; virtual;
    procedure EncodeMessage(oBuffer : TFslBuffer; oMessage : THL7V2Message; oOptions : THL7V2EncodingOptions); overload; virtual;
    procedure EncodeFile(oFile : THL7V2File; oStream : TFslStream; oOptions : THL7V2EncodingOptions); overload; virtual;
  end;

Type
  THL7V2EncoderFactory = class (THL7V2DictionaryProvider)
  public
    Function ProduceEncoder(aFormat : THL7V2Format) : THL7V2Encoder; overload; virtual;
    Procedure ConsumeEncoder(oEncoder: THL7V2Encoder); overload; virtual;
  end;

var
  GHL7V2EncoderFactory : THL7V2EncoderFactory;

Type
  THL7V2ER7Formatter = Class(TFslFormatter)
    Public
      Function Link : THL7V2ER7Formatter; Overload;

      Procedure ProduceContent(Const sValue : TBytes); Overload; Virtual;
  End;

type
  THL7V2ER7Decoder = class (THL7V2Decoder)
  private
    FSource : TBytes;
    FDecodingOptions : THL7V2DecodingOptions;
    FDelimiters : THL7V2Delimiters;
    procedure ReadDelimiters;
    function ReadMessageVersion(oOptions: THL7V2DecodingOptions) : THL7V2Version;

    Procedure ReadBinary(oCell : THL7V2Cell; Const sContent : String; var iCursor : integer);
    Procedure ReadEscape(oCell : THL7V2Cell; Const sContent : String; var iCursor : integer);
    Procedure ReadCellContent(oCell : THL7V2Cell; Const sContent : String; bNoEscape : Boolean);

    procedure ParseCell(oCell : THL7V2Cell; const sContent : String; bNoEscape : Boolean);
    procedure ParseCellEx(oCell : THL7V2Cell; iStart : integer; const sContent : String; bNoEscape : Boolean; aLevelOperation : THL7v2DecodingLevelOperation);
    procedure ParseDataElement(oDataElement : THL7V2DataElement; var iCursor: Integer);
    procedure ParseSegment(oMessage : THL7V2Message; var iCursor : Integer);
    procedure ParseSegmentInner(oSegment: THL7V2Segment; var iCursor: Integer);

    function DecodeBatchMessage(sLine : String; oBatch : THL7V2Batch; oExtractor : TFslByteExtractor) : String;
    function DecodeBatch(sLine : String; oFile : THL7V2File; oExtractor : TFslByteExtractor) : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    procedure DecodeMessage(oMessage : THL7V2Message; const sContent : TBytes; oOptions : THL7V2DecodingOptions); override;
    procedure DecodeSegment(oSegment : THL7V2Segment; const sContent : TBytes; oOptions : THL7V2DecodingOptions); override;
    procedure DecodeDataElement(oDataElement : THL7V2DataElement; const sContent : TBytes; oOptions : THL7V2DecodingOptions); override;
    procedure DecodeCell(oCell : THL7V2Cell; const sContent : TBytes; oOptions : THL7V2DecodingOptions); override;
    Procedure DecodeCellEx(oCell : THL7V2Cell; iStart : integer; Const sContent : TBytes; oOptions : THL7V2DecodingOptions; aLevelOperation : THL7v2DecodingLevelOperation); Override;
    procedure DecodeFile(oFile : THL7V2File; oStream : TFslStream; oOptions : THL7V2DecodingOptions); override;

  end;

type
  THL7V2ER7Encoder = class (THL7V2Encoder)
  private
    FEncodingOptions : THL7V2EncodingOptions;
    FDelimiters : THL7V2Delimiters;
    FStream : TMemoryStream;

    Function ReadStream : TBytes;
    procedure WriteChars(s : String);

    Procedure buildComplexContent(oCell: THL7V2Cell; oOptions: THL7V2EncodingOptions);
    Procedure Escape(sContent: String; oOptions: THL7V2EncodingOptions);
    Procedure FormatCell(oCell: THL7V2Cell; oOptions: THL7V2EncodingOptions);
    Procedure FormatDataElement(oDataElement: THL7V2DataElement; oOptions: THL7V2EncodingOptions);
    Procedure FormatSegment(oSegment: THL7V2Segment; oOptions: THL7V2EncodingOptions);
    procedure EncodeBatch(oBatch : THL7V2Batch; oFormatter : THL7V2ER7Formatter; oOptions: THL7V2EncodingOptions);
  protected
    function sizeInBytesV : cardinal; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    Function EncodeCell(oCell : THL7V2Cell; oOptions : THL7V2EncodingOptions) : TBytes; override;
    Function EncodeDataElement(oDataElement : THL7V2DataElement; oOptions : THL7V2EncodingOptions) : TBytes; override;
    Function EncodeSegment(oSegment : THL7V2Segment; oOptions : THL7V2EncodingOptions) : TBytes; override;
    Function EncodeMessage(oMessage : THL7V2Message; oOptions : THL7V2EncodingOptions) : TBytes; override;
    procedure EncodeFile(oFile : THL7V2File; oStream : TFslStream; oOptions : THL7V2EncodingOptions); override;
 end;

Const
  V2_XML_NS = 'urn:hl7-org:v2xml';

Type
  THL7V2XMLDecoder = Class (THL7V2Decoder)
  Private
    FOptions : THL7V2DecodingOptions;
    Function SuppressErrors : Boolean;
    Function  LoadDom(Const sContent: TBytes):TMXmlDocument;
    Procedure XMLCheck(bCondition : Boolean; Const sMethod, sMessage:String);

    Procedure PreProcessMSHSegment(oMessage : THL7V2Message; oRoot: TMXmlElement);

    Procedure ReadCell(oCell: THL7V2Cell; oRoot: TMXmlElement);
    Procedure ReadSegmentInner(oSegment : THL7V2Segment; oRoot : TMXmlElement);
    Procedure ReadSegment(oMessage : THL7V2Message; oRoot : TMXmlElement);
    Procedure ReadSegmentSeries(oMessage : THL7V2Message; oNode: TMXmlElement);
    Procedure ReadDelimiters(oMessage : THL7V2Message);

  protected
    function sizeInBytesV : cardinal; override;
  Public
    Destructor Destroy; Override;

    Procedure DecodeMessage(oMessage : THL7V2Message; Const sContent : TBytes; oOptions : THL7V2DecodingOptions); Override;
    Procedure DecodeCell(oCell : THL7V2Cell; Const sContent : TBytes; oOptions : THL7V2DecodingOptions); Override;
    Procedure DecodeCellEx(oCell : THL7V2Cell; iStart : integer; Const sContent : TBytes; oOptions : THL7V2DecodingOptions; aLevelOperation : THL7v2DecodingLevelOperation); Override;
    Procedure DecodeDataElement(oDataElement : THL7V2DataElement; Const sContent : TBytes; oOptions : THL7V2DecodingOptions); Override;
    Procedure DecodeSegment(oSegment : THL7V2Segment; Const sContent : TBytes; oOptions : THL7V2DecodingOptions); Override;
  End;

Type
  THL7V2XMLEncoder = Class (THL7V2Encoder)
  Private
    FOptions : THL7V2EncodingOptions;

    Procedure XMLCheck(bCondition : Boolean; Const sMethod, sMessage:String);
    Function buildComplexContent(oCell : THL7V2Cell) : String;
    Function FormatCell(oCell : THL7V2Cell; Const sName : String) : String;
    Function FormatSegmentContent(oSegment : THL7V2Segment) : String;
    Function FormatSegment(oSegment : THL7V2Segment) : String;
    Function FormatSegments(oGroup : THL7V2SegmentGroup) : String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Destructor Destroy; Override;

    Function EncodeCell(oCell : THL7V2Cell; oOptions : THL7V2EncodingOptions) : TBytes; Override;
    Function EncodeDataElement(oDataElement : THL7V2DataElement; oOptions : THL7V2EncodingOptions) : TBytes; Override;
    Function EncodeSegment(oSegment : THL7V2Segment; oOptions : THL7V2EncodingOptions) : TBytes; Override;
    Function EncodeMessage(oMessage : THL7V2Message; oOptions : THL7V2EncodingOptions) : TBytes; Override;
  End;

type
  THL7V2ErrorCollector = class (TFslObject)
  Public
    procedure RecordError(AElement: THL7V2BaseObject; const sError: String); overload; Virtual;
  end;

type
  THL7V2ValidationOption = (voSegmentOrder, voRequired, voDateFormat);
  THL7V2ValidationOptions = set of THL7V2ValidationOption;

  THL7V2DictionaryValidator = class (THL7V2WorkerObject)
  private
    function ValidateCell(oCell : THL7V2Cell; aOptions : THL7V2ValidationOptions; oErrors : THL7V2ErrorCollector) : Boolean;
    function ValidateDataElement(oDataElement : THL7V2DataElement; aOptions : THL7V2ValidationOptions; oErrors : THL7V2ErrorCollector) : Boolean;
    function ValidateSegment(oSegment : THL7V2Segment; aOptions : THL7V2ValidationOptions; oErrors : THL7V2ErrorCollector) : Boolean;
  public
    function Validate(oMessage : THL7V2Message; aOptions : THL7V2ValidationOptions; oErrors : THL7V2ErrorCollector) : Boolean;
  end;

Const
  FULL_VALIDATION = [voSegmentOrder, voRequired, voDateFormat];

Type
  THL7V2ValidationErrorError = String;

  THL7V2ValidationError = Class(TFslObject)
    Private
      FError : THL7V2ValidationErrorError;
      FElement: THL7V2BaseObject;
      Procedure SetElement(Const Value: THL7V2BaseObject);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Overload; Override;
      Destructor Destroy; Overload; Override;

      Function Link : THL7V2ValidationError; Overload;
      Function Clone : THL7V2ValidationError; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function AsText : String;

      Property ErrorMsg : THL7V2ValidationErrorError Read FError Write FError;
      Property Element : THL7V2BaseObject Read FElement Write SetElement;
  End;

  THL7V2ValidationErrors = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : THL7V2ValidationError;
      Procedure SetElement(Const iIndex : Integer; Const oValue : THL7V2ValidationError);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByError(pA, pB : Pointer) : Integer; Overload; Virtual;

      Function Get(Const aValue : Integer) : THL7V2ValidationError; Reintroduce; Overload; Virtual;

    Public
      Function Link : THL7V2ValidationErrors; Overload;
      Function Clone : THL7V2ValidationErrors; Overload;

      Function New : THL7V2ValidationError; Reintroduce; Overload; Virtual;

      Function IndexByError(Const aValue : THL7V2ValidationErrorError) : Integer; Overload; Virtual;
      Function GetByError(Const aValue : THL7V2ValidationErrorError) : THL7V2ValidationError; Overload; Virtual;
      Function ExistsByError(Const aValue : THL7V2ValidationErrorError) : Boolean; Overload; Virtual;

      Procedure SortedByError; Overload; Virtual;

      Function IsSortedByError : Boolean; Overload; Virtual;

      Function AsCommaText : String;
      Function AsText : String;

      Property Elements[Const iIndex : Integer] : THL7V2ValidationError Read GetElement Write SetElement; Default;
  End;

Const
  THL7V2VALIDATIONERROR_ERROR_FIELD = 'Error';
  THL7V2VALIDATIONERROR_ELEMENT_FIELD = 'Element';

type
  THL7V2SimpleErrorCollector = class (THL7V2ErrorCollector)
  Private
    FErrors: THL7V2ValidationErrors;
    procedure SetErrors(const Value: THL7V2ValidationErrors);
  Public
    constructor Create; override;
    destructor Destroy; Override;
    procedure RecordError(oElement: THL7V2BaseObject; const sError: String); override;
    property Errors : THL7V2ValidationErrors read FErrors write SetErrors;
  end;

implementation

Function FromHL7V2FormatCode(Const sValue : String) : THL7V2Format;
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOfSensitive(NAMES_HL7V2FORMAT, sValue);
  If iIndex = -1 Then
    Raise EHL7V2Exception.Create(Nil, hecApplicationError, 'FromHL7V2FormatCode', 'Bad HL7 V2 Encoding Format '+sValue);
  Result :=  THL7V2Format(iIndex);
End;

{ THL7V2BaseObject }

function THL7V2BaseObject.ChildrenClass: TFslTreeListClass;
begin
  result := THL7V2BaseObjects;
end;

procedure THL7V2BaseObject.Error(aCondition: THL7V2ErrorCondition; const sMethod, sMessage: String);
begin
  Raise EHL7V2Exception.Create(Self, aCondition, sMethod, sMessage);
end;

function THL7V2BaseObject.FullPathName: String;
begin
  result := '';
  Error(hecHL7LibraryError, 'FullPathName', 'must override FullPathName ion '+ClassName);
end;

function THL7V2BaseObject.GetChildren: THL7V2BaseObjects;
begin
  result := THL7V2BaseObjects(inherited Children);
end;

function THL7V2BaseObject.GetModel: THL7V2Model;
begin
  result := Parent.Model;
end;

function THL7V2BaseObject.GetParent: THL7V2BaseObject;
begin
  result := THL7V2BaseObject(inherited Parent);
end;

procedure THL7V2BaseObject.SetChildren(const oValue: THL7V2BaseObjects);
begin
  inherited Children := oValue;
end;

procedure THL7V2BaseObject.SetParent(const oValue: THL7V2BaseObject);
begin
  inherited Parent := oValue;
end;

function THL7V2BaseObject.IsElement(const sName: String): Boolean;
begin
  result := False;
  Error(hecHL7LibraryError, 'IsElement', 'Must override IsElement in '+ClassName);
end;

function THL7V2BaseObject.Condition(bCorrect: Boolean; const sMethod, sMessage: String): Boolean;
begin
  result := True;
  if not bCorrect then
    Error(hecApplicationError, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorApplication(const sMethod, sMessage: String);
begin
  Error(hecApplicationError, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorBadField(const sMethod, sMessage: String);
begin
  Error(hecBadField, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorBadMessage(const sMethod, sMessage: String);
begin
  Error(hecBadMessage, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorBadSegCode(const sMethod, sMessage: String);
begin
  Error(hecBadSegCode, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorDataType(const sMethod, sMessage: String);
begin
  Error(hecDataTypeError, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorDictionary(const sMethod, sMessage: String);
begin
  Error(hecDictionaryError, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorDuplicateKey(const sMethod, sMessage: String);
begin
  Error(hecDuplicateKey, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorDuplicateMsgId(const sMethod, sMessage: String);
begin
  Error(hecDuplicateMsgId, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorHL7Library(const sMethod, sMessage: String);
begin
  Error(hecHL7LibraryError, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorInternal(const sMethod, sMessage: String);
begin
  Error(hecInternalError, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorNoDictionary(const sMethod, sMessage: String);
begin
  Error(hecNoDictionary, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorNoTableValue(const sMethod, sMessage: String);
begin
  Error(hecNoTableValue, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorRecordLocked(const sMethod, sMessage: String);
begin
  Error(hecRecordLocked, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorRequiredField(const sMethod, sMessage: String);
begin
  Error(hecRequiredField, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorRequiredSeg(const sMethod, sMessage: String);
begin
  Error(hecRequiredSeg, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorSequence(const sMethod, sMessage: String);
begin
  Error(hecSequenceError, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorSuperfluousSeg(const sMethod, sMessage: String);
begin
  Error(hecSuperfluousSeg, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorUnknownKey(const sMethod, sMessage: String);
begin
  Error(hecUnknownKey, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorUnsEvntCode(const sMethod, sMessage: String);
begin
  Error(hecUnsEvntCode, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorUnsMsgType(const sMethod, sMessage: String);
begin
  Error(hecUnsMsgType, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorUnsProcID(const sMethod, sMessage: String);
begin
  Error(hecUnsProcID, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorUnsVersion(const sMethod, sMessage: String);
begin
  Error(hecUnsVersion, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorValidationFailed(const sMethod, sMessage: String);
begin
  Error(hecValidationFailed, sMethod, sMessage);
end;

procedure THL7V2BaseObject.ErrorXML(const sMethod, sMessage: String);
begin
  Error(hecXML, sMethod, sMessage);
end;

function THL7V2BaseObject.Clone: THL7V2BaseObject;
begin
  result := THL7V2BaseObject(inherited Clone);
end;

function THL7V2BaseObject.Link: THL7V2BaseObject;
begin
  result := THL7V2BaseObject(inherited Link);
end;

function THL7V2BaseObject.IndexInSiblings: Integer;
begin
  if HasParent then
    result := Parent.Children.IndexByReference(self)
  else
    result := -1;
end;

procedure THL7V2BaseObject.Clear;
begin
  Children.Clear;
end;

{ THL7V2BaseObjects }

function THL7V2BaseObjects.GetObject(iIndex: integer): THL7V2BaseObject;
begin
  result := THL7V2BaseObject(ObjectByIndex[iIndex]);
end;

function THL7V2BaseObjects.ItemClass: TFslObjectClass;
begin
  result := THL7V2BaseObject;
end;

{ THL7V2BaseObjectList }

function THL7V2BaseObjectList.GetObject(iIndex: integer): THL7V2BaseObject;
begin
  result := THL7V2BaseObject(ObjectByIndex[iIndex]);
end;

function THL7V2BaseObjectList.ItemClass: TFslObjectClass;
begin
  result := THL7V2BaseObject;
end;

{ THL7V2DecodingOptions }

procedure THL7V2DecodingOptions.Assign(oObject: TFslObject);
begin
  inherited;
  FSegmentLimit := THL7V2DecodingOptions(Oobject).FSegmentLimit;
  FVersionOverride := THL7V2DecodingOptions(Oobject).FVersionOverride;
  FDontWipeContent := THL7V2DecodingOptions(Oobject).FDontWipeContent;
  FFormat := THL7V2DecodingOptions(Oobject).FFormat;
  FSuppressErrors := THL7V2DecodingOptions(Oobject).FSuppressErrors;
  FDecodeBatchMessages := THL7V2DecodingOptions(Oobject).FDecodeBatchMessages;
  FBatchProgressEvent := THL7V2DecodingOptions(Oobject).FBatchProgressEvent;
  FPreventOddBinaryLengths := THL7V2DecodingOptions(Oobject).FPreventOddBinaryLengths;
end;

function THL7V2DecodingOptions.Clone: THL7V2DecodingOptions;
begin
  result := THL7V2DecodingOptions(inherited Clone);
end;

procedure THL7V2DecodingOptions.Defaults;
begin
  FSegmentLimit := 0;
  FVersionOverride := '';
  FDontWipeContent := False;
  FFormat := hfUnknown;
  FSuppressErrors := False;
  FDecodeBatchMessages := False;
  FBatchProgressEvent := Nil;
  FPreventOddBinaryLengths := false;
end;

function THL7V2DecodingOptions.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FVersionOverride.length * sizeof(char)) + 12);
end;

class function THL7V2DecodingOptions.getDefaults: THL7V2DecodingOptions;
begin
  result := THL7V2DecodingOptions.Create;
  result.Defaults;
end;

function THL7V2DecodingOptions.Link: THL7V2DecodingOptions;
begin
  result := THL7V2DecodingOptions(inherited Link);
end;

{ THL7V2EncodingOptions }

procedure THL7V2EncodingOptions.Assign(oSource: TFslObject);
begin
  inherited;
  FExtraFieldDelimiter  := THL7V2EncodingOptions(oSource).FExtraFieldDelimiter;
  FEscapeExtendedCharacters  := THL7V2EncodingOptions(oSource).FEscapeExtendedCharacters;
  FAllowMappingToFail  := THL7V2EncodingOptions(oSource).FAllowMappingToFail;
  FOptimisticMapping  := THL7V2EncodingOptions(oSource).FOptimisticMapping;
  FNoAddStructureName  := THL7V2EncodingOptions(oSource).FNoAddStructureName;
  FFormat  := THL7V2EncodingOptions(oSource).FFormat;
  FBatchProgressEvent  := THL7V2EncodingOptions(oSource).FBatchProgressEvent;
  FPretty := THL7V2EncodingOptions(oSource).FPretty;
end;

function THL7V2EncodingOptions.Clone: THL7V2EncodingOptions;
begin
  result := THL7V2EncodingOptions(inherited Clone);
end;

constructor THL7V2EncodingOptions.Create(aFormat : THL7V2Format; aOptions: THL7V2EncodingOptionSet);
begin
  Create;
  Format := aFormat;
  FOptimisticMapping := eoOptimisticMapping in aOptions;
  FAllowMappingToFail := eoAllowMappingToFail in aOptions;
  FEscapeExtendedCharacters := eoEscapeExtendedCharacters in aOptions;
  FExtraFieldDelimiter := eoExtraFieldDelimiter in aOptions;
  FNoAddStructureName := eoNoAddStructureName in aOptions;
  FPretty := eoPretty in aOptions;
end;

function THL7V2EncodingOptions.Link: THL7V2EncodingOptions;
begin
  result := THL7V2EncodingOptions(inherited Link);
end;

{ THL7V2Delimiters }

constructor THL7V2Delimiters.Create;
begin
  inherited;
  Reset;
end;

procedure THL7V2Delimiters.Assign(oSource: TFslObject);
begin
  inherited;
  EscapeCharacter := THL7V2Delimiters(oSource).EscapeCharacter;
  RepetitionDelimiter := THL7V2Delimiters(oSource).RepetitionDelimiter;
  FieldDelimiter := THL7V2Delimiters(oSource).FieldDelimiter;
  SubComponentDelimiter := THL7V2Delimiters(oSource).SubComponentDelimiter;
  ComponentDelimiter := THL7V2Delimiters(oSource).ComponentDelimiter;
end;

procedure THL7V2Delimiters.Check;
begin
  if FComponentDelimiter = FFieldDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FComponentDelimiter+'" is used for both Component and Field');
  if FSubComponentDelimiter = FFieldDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FSubComponentDelimiter+'" is used for both SubComponent and Field');
  if FSubComponentDelimiter = FComponentDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FSubComponentDelimiter+'" is used for both SubComponent and Component');
  if FRepetitionDelimiter = FFieldDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FRepetitionDelimiter+'" is used for both Repetition and Field');
  if FRepetitionDelimiter = FComponentDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FRepetitionDelimiter+'" is used for both Repetition and Component');
  if FRepetitionDelimiter = FSubComponentDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FRepetitionDelimiter+'" is used for both Repetition and SubComponent');
  if FEscapeCharacter = FFieldDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and Field');
  if FEscapeCharacter = FComponentDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and Component');
  if FEscapeCharacter = FSubComponentDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and SubComponent');
  if FEscapeCharacter = FRepetitionDelimiter then
    ErrorBadMessage('CheckDelimiters', 'Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and Repetition');
end;

function THL7V2Delimiters.Clone: THL7V2Delimiters;
begin
  result := THL7V2Delimiters(inherited Clone);
end;

function THL7V2Delimiters.Link: THL7V2Delimiters;
begin
  result := THL7V2Delimiters(inherited Link);
end;

procedure THL7V2Delimiters.Reset;
begin
  FFieldDelimiter := DEFAULT_DELIMITER_FIELD;
  FComponentDelimiter := DEFAULT_DELIMITER_COMPONENT;
  FSubComponentDelimiter := DEFAULT_DELIMITER_SUBCOMPONENT;
  FRepetitionDelimiter := DEFAULT_DELIMITER_REPETITION;
  FEscapeCharacter := DEFAULT_CHARACTER_ESCAPE;
end;

Function THL7V2Delimiters.isDelimiterEscape(ch : Char) : Boolean;
Begin
  Result := (ch = 'F') or (ch = 'S') or (ch = 'E') or (ch = 'T') or (ch = 'R');
End;

Function THL7V2Delimiters.getDelimiterEscapeChar(ch : Char) : Char;
Begin
  Result := #0;
  if (ch = 'E') Then
    Result := EscapeCharacter
  else if (ch = 'F') Then
    Result := FieldDelimiter
  else if (ch = 'S') Then
    Result := ComponentDelimiter
  else if (ch = 'T') Then
    Result := SubComponentDelimiter
  else if (ch = 'R') Then
    Result := RepetitionDelimiter
  else
    RaiseError('getDelimiterEscapeChar', 'internal error in getDelimiterEscapeChar, illegal char '+ch);
End;

function THL7V2Delimiters.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ THL7V2Contents }

Constructor THL7V2Contents.Create(Const sPath : String; const bComplex : Boolean);
Begin
  Inherited Create();
  FPath := sPath;
  FComplex := bComplex;
End;

Function THL7V2Contents.GetContent(iIndex: Integer): THL7V2Content;
Begin
  Result := THL7V2Content(objectByIndex[iIndex]);
End;

Function THL7V2Contents.ItemClass: TFslObjectClass;
Begin
  Result := THL7V2Content;
End;

Function THL7V2Contents.IsSimple : Boolean;
Begin
  Result := (Count = 0) or ((Count = 1) and (Contents[0].ContentType in [CONTENT_TYPE_TEXT, CONTENT_TYPE_NULL]));
End;

function THL7V2Contents.Clone: THL7V2Contents;
begin
  result := THL7V2Contents(inherited Clone);
end;

function THL7V2Contents.Link: THL7V2Contents;
begin
  result := THL7V2Contents(inherited Link);
end;

function THL7V2Contents.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPath.length * sizeof(char)) + 12);
end;

{ THL7V2Content }

procedure THL7V2Content.Assign(oObject: TFslObject);
begin
  inherited;
end;

function THL7V2Content.Clone: THL7V2Content;
begin
  result := THL7V2Content(inherited Clone);
end;

function THL7V2Content.Link: THL7V2Content;
begin
  result := THL7V2Content(inherited Link);
end;

Function THL7V2Content.ContentType : THL7V2ContentType;
Begin
  RaiseError('ContentType', 'Neec to override ContentType in '+ClassName);
  Result := CONTENT_TYPE_NULL;
End;

Function THL7V2Content.Text : String;
Begin
  RaiseError('Text', 'Need to override Text in '+ClassName);
End;

function THL7V2Content.Encoded: String;
begin
  RaiseError('Text', 'Need to override Encoded in '+ClassName);
end;

{ THL7V2ContentNull }

Function THL7V2ContentNull.ContentType : THL7V2ContentType;
Begin
  Result := CONTENT_TYPE_NULL;
End;

function THL7V2ContentNull.Encoded: String;
begin
  Result := Text;
end;

Function THL7V2ContentNull.Text : String;
Begin
  Result := '';
End;

{ THL7V2ContentText }

constructor THL7V2ContentText.Create(const sValue: String);
begin
  inherited Create;
  FText := sValue;
end;

Procedure THL7V2ContentText.Assign(oObject : TFslObject);
Begin
  Inherited;
  FText := THL7V2ContentText(oObject).FText;
End;

Function THL7V2ContentText.ContentType : THL7V2ContentType;
Begin
  Result := CONTENT_TYPE_TEXT;
End;

Function THL7V2ContentText.Text : String;
Begin
  Result := FText;
End;

procedure THL7V2ContentText.SetText(const sValue: String);
begin
  FText := sValue;
end;

function THL7V2ContentText.Encoded: String;
begin
  Result := Text;
end;

function THL7V2ContentText.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FText.length * sizeof(char)) + 12);
end;

{ THL7V2ContentBinary }

constructor THL7V2ContentBinary.Create(const sValue: String);
begin
  inherited Create;
  FBinary := sValue;
end;

Procedure THL7V2ContentBinary.Assign(oObject : TFslObject);
Begin
  Inherited;
  FBinary := THL7V2ContentBinary(oObject).FBinary;
End;

Function THL7V2ContentBinary.ContentType : THL7V2ContentType;
Begin
  Result := CONTENT_TYPE_BINARY;
End;

Function THL7V2ContentBinary.Text : String;
Begin
  Result := FBinary;
End;

Function THL7V2ContentBinary.XMLText : String;
var
  buf : TFslStringBuilder;
  iLoop : integer;
Begin
  buf := TFslStringBuilder.Create;
  try
    for iLoop := 1 to Length(FBinary) Do
      buf.Append('&#x'+IntToHex(ord(FBinary[iLoop]), 2)+';');
    result := buf.AsString;
  finally
    buf.Free;
  end;
End;

function ValidHexString(AStr: String; var VMsg: String): Boolean;
var
  i: Integer;
begin
  if (Length(AStr) mod 2 = 1) then
    begin
    Result := False;
    VMsg := 'Length is odd (' + IntToStr(Length(AStr)) + ')';
    end
  else
    begin
    Result := True;
    for i := 1 to Length(AStr) do
      begin
      if not CharInSet(AStr[i], ['0'..'9', 'A'..'F']) then
        begin
        VMsg := 'Character ' + AStr[i] + ' at location ' + IntToStr(i) + ' is not valid';
        Result := False;
        break;
        end;
      end;
    end;
end;

function HexEncode(ARaw: String): String;
var
  i: Integer;
  j: Integer;
  LStr: String;
  LResult: PChar;
begin
  SetLength(Result, Length(ARaw) * 2);
  j := 0;
  LResult := PChar(Result);
  for i := 1 to Length(ARaw) do
    begin
    LStr := IntToHex(Ord(ARaw[i]), 2);
    LResult[j] := LStr[1];
    LResult[j + 1] := LStr[2];
    Inc(j, 2);
    end;
end;

function HexDecode(ARaw: String): String;
var
  i: Integer;
  LResult: PChar;
  s: String;
begin
  if not ValidHexString(ARaw, s) then
    raise EFslException.Create('Error HEX Decoding "' + ARaw + '": ' + s);
  SetLength(Result, Length(ARaw) div 2);
  LResult := PChar(Result);
  for i := 1 to length(ARaw) div 2 do
    begin
    s := Copy(ARaw, (i * 2) - 1, 2);
    LResult[i - 1] := Chr(StrToInt('$' + s));
    end;
end;

function THL7V2ContentBinary.Encoded: String;
begin
  Result := '\X'+HexEncode(FBinary)+'\';
end;

function THL7V2ContentBinary.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FBinary.length * sizeof(char)) + 12);
end;

{ THL7V2ContentEscape }

constructor THL7V2ContentEscape.Create(const sValue: String);
begin
  inherited Create;
  FEscape := sValue;
end;

Procedure THL7V2ContentEscape.Assign(oObject : TFslObject);
Begin
  Inherited;
  FEscape := THL7V2ContentEscape(oObject).FEscape;
End;

Function THL7V2ContentEscape.ContentType : THL7V2ContentType;
Begin
  Result := CONTENT_TYPE_Escape;
End;

Function THL7V2ContentEscape.Text : String;
Begin
  Result := FEscape;
End;

function THL7V2ContentEscape.Encoded: String;
begin
  Result := '\'+FEscape+'\';
end;

function THL7V2ContentEscape.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FEscape.length * sizeof(char)) + 12);
end;

{ THL7V2Cell }

Constructor THL7V2Cell.Create(aType: THL7V2CellType; iIndex: Integer);
Begin
  Create;
  CellType := aType;
  Index := iIndex;
End;

Destructor THL7V2Cell.Destroy;
Begin
  ClearContents;
  Inherited;
End;

Procedure THL7V2Cell.CreateContents;
Begin
  If (FContents = nil) Then
    FContents := THL7V2Contents.create(FullPathName, Escapable)
  Else
    FContents.Clear;
End;

Procedure THL7V2Cell.ClearContents;
Begin
  FContents.Free;
  FContents := Nil;
End;

Function THL7V2Cell.GetChildren: THL7V2Cells;
Begin
  Result := THL7V2Cells(Inherited Children);
End;

Procedure THL7V2Cell.SetChildren(Const oValue: THL7V2Cells);
Begin
  Inherited Children := oValue;
End;

Function THL7V2Cell.GetAsDate: TDateTime;
Begin
  If Self = Nil Then
    Result := 0
  Else If HasChildren Then
    Result := Children[0].GetAsDate
  Else If Not Defined Or Not HasContent Then
    Result := 0
  Else
    Result := HL7StringToDate('YYYYMMDD', GetSimpleTextContent, False);
End;

Function THL7V2Cell.GetAsDateTime: TDateTime;
Begin
  If Self = Nil Then
    Result := 0
  Else If HasChildren Then
    Result := Children[0].GetAsDateTime
  Else If Not Defined Or Not HasContent Then
    Result := 0
  Else
    Result := HL7StringToDate('YYYYMMDDHHNNSS', GetSimpleTextContent, True);
End;

Function THL7V2Cell.GetAsDateTimeNoTZ: TDateTime;
Begin
  If Self = Nil Then
    Result := 0
  Else If HasChildren Then
    Result := Children[0].GetAsDateTimeNoTZ
  Else If Not Defined Or Not HasContent Then
    Result := 0
  Else
    Result := HL7StringToDate('YYYYMMDDHHNNSS', GetSimpleTextContent, False);
End;

Function THL7V2Cell.GetAsFloat: Double;
Begin
  Result := 0;
  If Self = Nil Then
    Result := 0
  Else If HasChildren Then
    Result := Children[0].GetAsFloat
  Else If Not Defined Or Not HasContent Then
    Result := 0
  Else If Not StringIsReal(GetSimpleTextContent) Then
    ErrorDataType('GetAsFloat', 'Value is not a proper floating point number ['+GetSimpleTextContent+']')
  Else
    Result := StringToReal(GetSimpleTextContent);
End;

Function THL7V2Cell.GetAsInteger: Integer;
Begin
  Result := 0;
  If Self = Nil Then
    Result := 0
  Else If HasChildren Then
    Result := Children[0].GetAsInteger
  Else If Not Defined Or Not HasContent Then
    Result := 0
  Else If Not StringIsInteger32(GetSimpleTextContent) Then
    ErrorDataType('GetAsInteger', 'Value is not a proper integer ['+GetSimpleTextContent+']')
  Else
    Result := StringToInteger32(GetSimpleTextContent);
End;

Function THL7V2Cell.GetAsString: String;
Begin
  If Self = Nil Then
    Result := ''
  Else If HasChildren Then
    Result := Children[0].GetAsString
  Else
    Result := GetSimpleTextContent;
End;

Function THL7V2Cell.GetDefined: Boolean;
Begin
  If Self = Nil Then
    Result := False
  Else If HasChildren Then
    Result := Children[0].GetDefined
  Else
    Result := Assigned(FContents) and (FContents.Count > 0);
End;

Function THL7V2Cell.GetIsRelevent: Boolean;
Var
  iLoop : Integer;
Begin
  If HasChildren Then
    Begin
    Result := False;
    For iLoop := 0 To Children.Count - 1 Do
      Result := Result Or Children[iLoop].GetIsRelevent
    End
  Else
    Result := GetDefined;
End;

Function THL7V2Cell.GetRawContent: String;
Begin
  If Self = Nil Then
    Result := ''
  Else If HasChildren Then
    Result := Children[0].GetRawContent
  Else If not assigned(FContents) or (FContents.Count = 0) Then
    Result := ''
  Else If not IsSimple Then
    RaiseError('GetRawContent', 'Attempt to access rawContent when complex content exists')
  Else If FContents[0].ContentType = CONTENT_TYPE_NULL Then
    Result := ''
  Else
    Result := FContents[0].Text;
End;

Function THL7V2Cell.GetRawContentForScripting: String;
// well, this exists because of a old backwards compatibility issue in scripting
// too hard to update the scripts to handle complex content properly, so we
// just er7 encode the complex stuff
var
  iLoop : Integer;
  oBuilder : TFslStringBuilder;
Begin
  If Self = Nil Then
    Result := ''
  Else If HasChildren Then
    Result := Children[0].GetRawContentForScripting
  Else If not assigned(FContents) or (FContents.Count = 0) Then
    Result := ''
  Else If not IsSimple Then
  Begin
    oBuilder := TFslStringBuilder.Create;
    Try
      For iLoop := 0 to FContents.Count - 1 Do
        oBuilder.Append(FContents[iLoop].Encoded);
      Result := oBuilder.AsString;
    Finally
      oBuilder.Free;
    End;
  End
  Else If FContents[0].ContentType = CONTENT_TYPE_NULL Then
    Result := ''
  Else
    Result := FContents[0].Text;
End;

Procedure THL7V2Cell.SetAsDate(Const aValue: TDateTime);
Begin
  Assert(Condition(Assigned(Self), 'SetAsDate', 'Attempt to use an undefined Cell'));
  If HasChildren Then
    Children[0].SetAsDate(aValue)
  Else
    RawContent := HL7DateToString(aValue, 'YYYYMMDD', False);
End;

Procedure THL7V2Cell.SetAsDateTime(Const aValue: TDateTime);
Begin
  Assert(Condition(Assigned(Self), 'SetAsDate', 'Attempt to use an undefined Cell'));
  If HasChildren Then
    Children[0].SetAsDateTime(aValue)
  Else
    RawContent := HL7DateToString(aValue, 'YYYYMMDDHHNNSS', True);
End;

Procedure THL7V2Cell.SetAsDateTimeNoTZ(Const aValue: TDateTime);
Begin
  Assert(Condition(Assigned(Self), 'SetAsDate', 'Attempt to use an undefined Cell'));
  If HasChildren Then
    Children[0].SetAsDateTimeNoTZ(aValue)
  Else
    RawContent := HL7DateToString(aValue, 'YYYYMMDDHHNNSS', False);
End;

Procedure THL7V2Cell.SetAsFloat(Const rValue: Double);
Begin
  Assert(Condition(Assigned(Self), 'SetAsFloat', 'Attempt to use an undefined Cell'));
  If HasChildren Then
    Children[0].SetAsFloat(rValue)
  Else
    RawContent := RealToString(rValue)
End;

Procedure THL7V2Cell.SetAsInteger(Const iValue: Integer);
Begin
  Assert(Condition(Assigned(Self), 'SetAsInteger', 'Attempt to use an undefined Cell'));
  If HasChildren Then
    Children[0].SetAsInteger(iValue)
  Else
    RawContent := IntegerToString(iValue);
End;

Procedure THL7V2Cell.SetAsString(Const sValue: String);
Begin
  Assert(Condition(Assigned(Self), 'SetAsString', 'Attempt to use an undefined Cell'));
  If HasChildren Then
    Children[0].SetAsString(sValue)
  Else
    RawContent := StringStrip(sValue, #0);
End;

Procedure THL7V2Cell.SetDefined(Const bValue: Boolean);
Begin
  Assert(Condition(Assigned(Self), 'SetDefined', 'Attempt to use an undefined Cell'));
  If HasChildren Then
    Children[0].SetDefined(bValue)
  Else If bValue Then
    Begin
    If Not assigned(FContents) Then
      CreateContents;
    If FContents.Count = 0 then
      FContents.add(THL7V2ContentNull.create());
    End
  Else
    Begin
    FContents.free;
    FContents := nil;
    End;
End;

Function THL7V2Cell.GetSimpleTextContent : String;
Begin
  If not IsSimple Then
    RaiseError('GetSimpleTextContent', 'Attempt to access rawContent when complex content exists');
  Result := GetRawContent;
End;

Procedure THL7V2Cell.SetRawContent(Const sValue: String);
Begin
  Assert(Condition(Assigned(Self), 'SetRawContent', 'Attempt to use an undefined Cell'));
  If HasChildren Then
    Children[0].SetRawContent(sValue)
  Else If sValue = '""' Then
    Begin
    CreateContents; // clear it anyway
    FContents.add(THL7V2ContentNull.create());
    End
  Else If (sValue = '') Then
    ClearContents
  Else
    Begin
    CreateContents; // clear it anyway
    FContents.add(THL7V2ContentText.create(sValue));
    End;
End;

Function THL7V2Cell.GetElement(Const sCode: String): THL7V2Cell;
Var
  sLeft: String;
  sRight: String;
  iFieldIndex: Integer;
Begin
  If Self = Nil Then
    Result := Nil
  Else
    Begin
    If Pos('.', sCode) > 0 Then
      StringSplit(sCode, '.', sLeft, sRight)
    Else
      StringSplit(sCode, '-', sLeft, sRight);

    Condition(StringIsInteger32(sLeft), 'GetElement', 'You must specify a numeric index');
    iFieldIndex := StringToInteger32(sLeft) - 1;
    If iFieldIndex < Children.Count Then
      Result := Children[iFieldIndex]
    Else If iFieldIndex = 0 Then
      Result := Self
    Else
      Result := Nil;
    If Length(sRight) > 0 Then
      Result := Result.GetElement(sRight);
    End;
End;

Procedure THL7V2Cell.ForceElement(Const sCode: String);
Var
  sLeft: String;
  sRight: String;
  iFieldIndex: Integer;
  oCell : THL7V2Cell;
Begin
  Assert(Condition(Assigned(Self), 'ForceElement', 'Attempt to force on element an an undefined element'));
  If Pos('.', sCode) > 0 Then
    StringSplit(sCode, '.', sLeft, sRight)
  Else
    StringSplit(sCode, '-', sLeft, sRight);

  Condition(StringIsInteger32(sLeft), 'GetElement', 'You must specify a numeric index');
  iFieldIndex := StringToInteger32(sLeft);
  oCell := ForceChild(iFieldIndex);
  If Length(sRight) > 0 Then
    oCell.ForceElement(sRight);
End;

Function THL7V2Cell.ChildrenClass : TFslTreeListClass;
Begin
  Result := THL7V2Cells;
End;

Function THL7V2Cell.GetTable: THL7V2ModelTable;
Begin
  Result := Nil;
  ErrorHL7Library('GetTable', 'Must override GetTable in '+ClassName);
End;

Function THL7V2Cell.GetFieldName: String;
Begin
  Result := '';
  ErrorHL7Library('GetFieldName', 'Must override GetFieldName in '+ClassName);
End;

Function THL7V2Cell.Escapable : Boolean;
Var
  oDataType : THL7V2ModelDataType;
Begin
  oDataType := GetDataType;
  Result := Assigned(oDataType) And oDataType.Escapable;
End;

Function THL7V2Cell.GetDataType: THL7V2ModelDataType;
Begin
  Result := Nil;
  ErrorHL7Library('GetDataType', 'Must override GetDataType in '+ClassName);
End;

Function THL7V2Cell.FullPathName: String;
Begin
  if HasParent Then
    Result := Parent.FullPathName + '-'+IntegerToString(Index)
  Else
    Result := '-'+IntegerToString(Index);
End;

Procedure THL7V2Cell.Clone(oSource : THL7V2Cell; bForce : Boolean);
Var
  iLoop : Integer;
Begin
  Assert(Condition(Assigned(Self), 'ForceElement', 'Attempt to use an undefined element'));
  Assert(Invariants('CopyCell', oSource, THL7V2Cell, 'Source'));

  ClearContent;
  ClearContents;
  if oSource.HasChildren Then
    For iLoop := 0 To oSource.Children.Count - 1 Do
      If bForce Or (iLoop < Children.Count) Then
        ForceChild(iLoop+1).Clone(oSource.Children[iLoop], bForce);

  if oSource.HasContent Then
    Contents.Assign(oSource.Contents);
End;

Procedure THL7V2Cell.ClearContent;
Var
  iLoop : Integer;
Begin
  Assert(Condition(Assigned(Self), 'ForceElement', 'Attempt to use an undefined element'));
  ClearContents;
  For iLoop := 0 To Children.Count - 1 Do
    Children[iLoop].ClearContent;
End;

Function THL7V2Cell.HasTable: Boolean;
Begin
  Assert(Condition(Assigned(Self), 'HasTable', 'Attempt to use an undefined element'));
  Result := GetTable <> Nil;
End;

Function THL7V2Cell.TableValueValid: Boolean;
Var
  oTable : THL7V2ModelTable;
Begin
  Assert(Condition(Assigned(Self), 'ForceElement', 'Attempt to use an undefined element'));
  oTable := GetTable;
  Result := Assigned(oTable) And oTable.Items.ExistsByCode(GetSimpleTextContent);
End;

Function THL7V2Cell.HasDate: Boolean;
Var
  oDataType : THL7V2ModelDataType;
Begin
  If Self = Nil Then
    Result := False
  Else If HasChildren Then
    Result := Children[0].HasDate
  Else
    Begin
    oDataType := GetDataType;
    Result := Assigned(oDataType) And StringArrayExistsInsensitive(['DT', 'TM', 'TS'], oDataType.Name);
    End;
End;

Function THL7V2Cell.ForceChild(iIndex : Integer) : THL7V2Cell;
Begin
  Result := Nil;
  ErrorHL7Library('ForceChild', 'Must override ForceChild in '+ClassName);
End;

Procedure THL7V2Cell.Decode(sSource: TBytes; oOptions: THL7V2DecodingOptions);
Var
  oDecoder : THL7V2Decoder;
Begin
  If Not Assigned(oOptions) Or Not (oOptions.DontWipeContent) Then
    ClearContent;
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(sSource, oOptions, fuComponent);
  Try
    oDecoder.DecodeCell(Self, sSource, oOptions);
  Finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  End;
End;

Procedure THL7V2Cell.Decode(oSource: TFslBuffer; oOptions: THL7V2DecodingOptions);
Var
  oDecoder : THL7V2Decoder;
Begin
  If Not Assigned(oOptions) Or Not (oOptions.DontWipeContent) Then
    ClearContent;
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(oSource, oOptions, fuComponent);
  Try
    oDecoder.DecodeCell(Self, oSource, oOptions);
  Finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  End;
End;

Function THL7V2Cell.HasContent: Boolean;
Var
  iLoop : Integer;
Begin
  if self = nil Then
    result := false
  else If HasChildren Then
    Begin
    Result := False;
    For iLoop := 0 To Children.Count -1 Do
      Result := Result Or Children[iLoop].HasContent;
    End
  Else
    Result := Defined;
End;

Function THL7V2Cell.Encode(aFormat: THL7V2Format; aOptions: THL7V2EncodingOptionSet): TBytes;
Var
  oOptions: THL7V2EncodingOptions;
Begin
  oOptions := THL7V2EncodingOptions.Create(aFormat, aOptions);
  Try
    Result := Encode(oOptions);
  Finally
    oOptions.Free;
  End;
End;

Function THL7V2Cell.Encode(oOptions: THL7V2EncodingOptions): TBytes;
Var
  oLocal : THL7V2EncodingOptions;
  oEncoder : THL7V2Encoder;
Begin
  If Assigned(oOptions) Then
    oLocal := oOptions.Link
  Else
    oLocal := THL7V2EncodingOptions.Create;
  Try
// todo - do we want to do this?
//    if (oOptions.Format = hfUnknown) then
//      oOptions.Format := Message.Format
    oEncoder := GHL7V2EncoderFactory.ProduceEncoder(oOptions.Format);
    Try
      Result := oEncoder.EncodeCell(Self, oOptions);
    Finally
      GHL7V2EncoderFactory.ConsumeEncoder(oEncoder);
    End;
  Finally
    oLocal.Free;
  End;
End;

Procedure THL7V2Cell.Encode(oBuffer: TFslBuffer; oOptions: THL7V2EncodingOptions);
Var
  oLocal : THL7V2EncodingOptions;
  oEncoder : THL7V2Encoder;
Begin
  If Assigned(oOptions) Then
    oLocal := oOptions.Link
  Else
    oLocal := THL7V2EncodingOptions.Create;
  Try
// todo - do we want to do this?
//    if (oOptions.Format = hfUnknown) then
//      oOptions.Format := Message.Format
    oEncoder := GHL7V2EncoderFactory.ProduceEncoder(oOptions.Format);
    Try
      oEncoder.EncodeCell(oBuffer, Self, oOptions);
    Finally
      GHL7V2EncoderFactory.ConsumeEncoder(oEncoder);
    End;
  Finally
    oLocal.Free;
  End;
End;

Procedure THL7V2Cell.Assign(oObject: TFslObject);
Begin
  Inherited;
  FIndex := THL7V2Cell(oObject).FIndex;
  FCellType  := THL7V2Cell(oObject).FCellType;
  If (THL7V2Cell(oObject).FContents <> nil) Then
    FContents := THL7V2Cell(oObject).FContents.Clone;
End;

Function THL7V2Cell.Clone: THL7V2Cell;
Begin
  Result := THL7V2Cell(Inherited Clone);
End;

Function THL7V2Cell.Link: THL7V2Cell;
Begin
  Result := THL7V2Cell(Inherited Link);
End;

Procedure THL7V2Cell.AddTableCode(Const sCode: String);
Var
  oTable: THL7V2ModelTable;
  oItem : THL7V2ModelTableItem;
Begin
  Assert(Condition(Assigned(Self), 'AddTableCode', 'Attempt to use an undefined Cell'));

  oTable := GetTable;
  If Not Assigned(oTable) Then
    ErrorNoTableValue('AddTableCode', 'No Table Defined');

  oItem := oTable.items.GetByCode(sCode);
  If Not Assigned(oItem) And StringIsInteger32(sCode) Then
    oItem := oTable.items.GetByID(StringToInteger32(sCode));

  If Not Assigned(oItem) Then
    ErrorNoTableValue('AddTableCode', 'Table value "'+sCode+'" not found');

  RawContent := oItem.Code;
End;

Function THL7V2Cell.AsTableCode(Var sCode: String): Boolean;
Var
  oTable: THL7V2ModelTable;
  oItem : THL7V2ModelTableItem;
Begin
  Assert(Condition(Assigned(Self), 'AddTableCode', 'Attempt to use an undefined Cell'));

  oTable := GetTable;
  If Not Assigned(oTable) Then
    ErrorNoTableValue('AddTableCode', 'No Table Defined');

  oItem := oTable.items.GetByCode(GetSimpleTextContent);
  If Not Assigned(oItem) And StringIsInteger32(GetSimpleTextContent) Then
    oItem := oTable.items.GetByID(StringToInteger32(GetSimpleTextContent));

  Result := Assigned(oItem);
  If Result Then
    sCode := oItem.Code;
End;

Function THL7V2Cell.DateIsValid(Var sError : String): Boolean;
Var
  sFormat : String;
Begin
  Result := HasDate And HasContent;
  If Result Then
    Begin
    DateFormatForType(DataType.Name, Model.Version, sFormat, True);
    Result := CheckDateFormat(sFormat, RawContent, sError);
    End;
End;

Function THL7V2Cell.DateIsValid: Boolean;
Var
  sError : String;
  sFormat : String;
Begin
  Result := HasDate And HasContent;
  If Result Then
    Begin
    DateFormatForType(DataType.Name, Model.Version, sFormat, True);
    Result := CheckDateFormat(sFormat, RawContent, sError);
    End;
End;

Procedure THL7V2Cell.Clear;
Begin
  ErrorApplication('Clear', 'Cannot clear a cell');
  Inherited;
End;

function THL7V2Cell.isSimple : Boolean;
Begin
  If (HasChildren) Then
    Result := Children[0].IsSimple
  Else
    Result := not Assigned(FContents) or FContents.IsSimple;
(* used to be called isSimpleContent, and be
  Result := true;
  oCells := GetChildren;
  For i := 1 to oCells.Count - 1 do
    if oCells[i].HasContent then
      Result := false;                       *)
End;

function THL7V2Cell.isSimpleContent : Boolean;
var
  oCells : THL7V2Cells;
  i : integer;
Begin
  Result := true;
  oCells := GetChildren;
  For i := 1 to oCells.Count - 1 do
    if oCells[i].HasContent then
      Result := false;
End;

function THL7V2Cell.GetContents: THL7V2Contents;
begin
  if HasChildren Then
    Result := GetChildren[0].Contents
  Else
    Begin
    if (FContents = nil) then
      CreateContents;
    Result := FContents;
    End;
end;

function THL7V2Cell.GetNullify: Boolean;
begin
  Result := (FContents <> nil) and (FContents.Count = 1) and (FContents[0].ContentType = CONTENT_TYPE_NULL);
end;

procedure THL7V2Cell.SetNullify(const Value: Boolean);
begin
  CreateContents;
  FContents.add(THL7V2ContentNull.create());
end;

function THL7V2Cell.GetAsPlainText: String;
var
  iLoop : integer;
  oContents : THL7V2Contents;
begin
  oContents := Contents;
  for iLoop := 0 to oContents.Count - 1 do
    If (oContents[iLoop].ContentType = CONTENT_TYPE_TEXT) then
      Result := result + oContents[iLoop].Text
    Else If (oContents[iLoop].ContentType = CONTENT_TYPE_NULL) then
      Result := result + '""';
end;

procedure THL7V2Cell.Commit;
begin
 // nothing
end;

procedure THL7V2Cell.TakeContent(oSrc: THL7V2Cell);
begin
  if oSrc.FContents <> nil Then
  Begin
    Contents.Assign(oSrc.FContents);
    oSrc.ClearContents;
  End
  Else
    ClearContents;
end;

procedure THL7V2Cell.DecodeEx(iStart: integer; sSource: String; oOptions: THL7V2DecodingOptions; aLevelOperation : THL7v2DecodingLevelOperation);
Var
  oDecoder : THL7V2Decoder;
Begin
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(StringAsBytes(sSource), oOptions, fuComponent);
  Try
    oDecoder.DecodeCellEx(Self, iStart, StringAsBytes(sSource), oOptions, aLevelOperation);
  Finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  End;

end;

function THL7V2Cell.GetDisplayForCode: String;
Var
  oTable : THL7V2ModelTable;
begin
  result := '';
  oTable := GetTable;
  if Assigned(oTable) And oTable.Items.ExistsByCode(GetSimpleTextContent) then
    result := oTable.Items.getByCode(GetSimpleTextContent).Description;
end;

function THL7V2Cell.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContents.sizeInBytes);
end;

{ THL7V2Cells }

Function THL7V2Cells.GetCell(iIndex: Integer): THL7V2Cell;
Begin
  Result := THL7V2Cell(objectByIndex[iIndex]);
End;

Function THL7V2Cells.ItemClass: TFslObjectClass;
Begin
  Result := THL7V2Cell;
End;

{ THL7V2Component }

destructor THL7V2Component.Destroy;
begin
  FDefinition.Free;
  inherited;
end;

function THL7V2Component.ChildrenClass: TFslTreeListClass;
begin
  result := THL7V2Components;
end;

function THL7V2Component.ForceChild(iIndex: Integer): THL7V2Cell;
begin
  result := ForceComponent(iIndex);
end;

function THL7V2Component.ForceComponent(iIndex: Integer): THL7V2Component;
begin
  result := nil;
  if iIndex <= 0 then
    ErrorApplication('ForceComponent', 'Can''t force component '+IntegerToString(iIndex))
  else
    begin
    while iIndex > Children.Count do
      Children.Add(THL7V2Component.create(ctSubComponent, Children.Count+1));
    result := Children[iIndex - 1];
    end;
end;

function THL7V2Component.GetComponent(iIndex: Integer): THL7V2Component;
begin
  if Children.ExistsByIndex(iIndex - 1) then
    result := Children[iIndex - 1]
  else
    result := nil;
end;

function THL7V2Component.GetChildren: THL7V2Components;
begin
  result := THL7V2Components(inherited Children);
end;

function THL7V2Component.GetParent: THL7V2Cell;
begin
  result := THL7V2Cell(inherited Parent);
end;

procedure THL7V2Component.SetChildren(const oValue: THL7V2Components);
begin
  inherited Children := oValue;
end;

procedure THL7V2Component.SetDefinition(const oValue: THL7V2ModelComponent);
begin
  FDefinition.Free;
  FDefinition := oValue;
end;

procedure THL7V2Component.SetParent(const oValue: THL7V2Cell);
begin
  inherited Parent := oValue;
end;

function THL7V2Component.GetDataType: THL7V2ModelDataType;
begin
  if assigned(FDefinition) then
    result := FDefinition.RefDataType
  else
    result := nil;
end;

function THL7V2Component.GetFieldName: String;
begin
  if assigned(FDefinition) then
    result := FDefinition.Name
  else
    result := 'Component-'+IntegerToString(Index);
end;

function THL7V2Component.GetTable: THL7V2ModelTable;
begin
  if assigned(FDefinition) then
    result := FDefinition.RefTable
  else
    result := nil;
end;

function THL7V2Component.IsElement(const sName: String): Boolean;
var
  sLeft : String;
  sRight : String;
begin
  StringSplitRight(sName, ['-', '.'], sLeft, sRight);
  result := Parent.IsElement(sLeft) and StringIsInteger32(sRight) and (StringToInteger32(sRight) = Index);
end;

procedure THL7V2Component.MakeForBuild(bClear : Boolean = true);
var
  iLoop : integer;
  oChild : THL7V2Component;
  oStructure : THL7V2ModelStructure;
begin
  assert(Condition(assigned(self), 'MakeForBuild', 'Attempt to use an undefined element'));
  if bClear Then
    Children.Clear;

  if (FDefinition = Nil) Or (FDefinition.RefStructure = Nil) Then
  Begin
    For iLoop := 0 to Children.Count - 1 Do
    begin
      oChild := Children[iLoop];
      oChild.Definition := nil;
      oChild.MakeForBuild(bClear);
    End;
  End
  Else
  Begin
    oStructure := THL7V2ModelStructure(FDefinition.RefStructure);
    For iLoop := 0 to IntegerMax(oStructure.Components.Count, Children.Count) - 1 do
      If iLoop >= Children.Count Then
      Begin
        oChild := THL7V2Component.Create(ctSubComponent, iLoop + 1);
        try
          oChild.Definition := oStructure.Components[iLoop].Link;
          oChild.MakeForBuild;
          Children.add(oChild.Link);
        finally
          oChild.Free;
        end;
        if iLoop = 0 then
          oChild.TakeContent(self);
      End
      Else
      Begin
        oChild := Children[iLoop];
        if iLoop <= oStructure.Components.Count Then
          oChild.Definition := oStructure.Components[iLoop].Link
        Else
          oChild.Definition := Nil;
        oChild.MakeForBuild(false);
      End;
  End;
end;

procedure THL7V2Component.Assign(oObject: TFslObject);
begin
  inherited;
  Definition  := THL7V2Component(oObject).Definition.Link;
end;

function THL7V2Component.Clone: THL7V2Component;
begin
  result := THL7V2Component(inherited Clone);
end;

function THL7V2Component.Link: THL7V2Component;
begin
  result := THL7V2Component(inherited Link);
end;

procedure THL7V2Component.StripUndefinedContent;
var
  i : integer;
begin
  for i := 0 to Components.Count - 1 do
    if Components[i].Definition = nil then
      Components[i].ClearContent
    else
      Components[i].StripUndefinedContent;
end;

procedure THL7V2Component.MapToDefinitions(Model: THL7V2ModelStructure; ndx : integer);
var
  i : integer;
begin
  if (Model = nil)  or (Model.Components.Count <= ndx) then
    Definition := nil
  else
    Definition := Model.Components[ndx].Link;
  for i := 0 to Components.Count - 1 do
    if Definition = nil then
      Components[i].MapToDefinitions(nil, i)
    else
      Components[i].MapToDefinitions(THL7V2ModelStructure(Definition.RefStructure), i);
end;

function THL7V2Component.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDefinition.sizeInBytes);
end;

{ THL7V2Components }

function THL7V2Components.GetComponent(iIndex: Integer): THL7V2Component;
begin
  result := THL7V2Component(ObjectByIndex[iIndex]);
end;

function THL7V2Components.ItemClass: TFslObjectClass;
begin
  result := THL7V2Component;
end;

function THL7V2Components.GetActiveCount: Integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to Count - 1Do
    if Component[i].Defined then
      result := i + 1;
end;

{ THL7V2DataElement }

destructor THL7V2DataElement.Destroy;
begin
  FDefinition.Free;
  FRepeats.Free;
  inherited;
end;

procedure THL7V2DataElement.SpecifyDataType(const sType: String);
begin
  if (FSpecifiedDataType <> sType) Then
  Begin
    FSpecifiedDataType := sType;
    MakeForBuild(false);
    End;
end;

procedure THL7V2DataElement.SetDefinition(const oValue: THL7V2ModelField);
begin
  FDefinition.Free;
  FDefinition := oValue;
end;

function THL7V2DataElement.GetDataType: THL7V2ModelDataType;
begin
  if assigned(FDefinition) and assigned(FDefinition.RefDataElement) and assigned(FDefinition.RefDataElement.RefStructure) then
    result := FDefinition.RefDataElement.RefStructure.RefDataType
  else
    result := nil;
  if ((result = nil) or (result.Name = '*')) and (FSpecifiedDataType <> '') then
    result := Model.DataTypes.GetByName(FSpecifiedDataType);
end;

function THL7V2DataElement.GetFieldName: String;
begin
  if assigned(FDefinition) then
    result := FDefinition.RefDataElement.Description
  else
    begin
    result := 'Field-'+IntegerToString(Index);
    if repeatIndex <> 0 then
      result := result +':'+IntegerToString(RepeatIndex);
    end;
end;

function THL7V2DataElement.GetTable: THL7V2ModelTable;
begin
  if assigned(FDefinition) then
    result := FDefinition.RefDataElement.RefTable
  else
    result := nil;
end;

function THL7V2DataElement.IsElement(const sName: String): Boolean;
var
  sLeft : String;
  sRight : String;
begin
  StringSplit(sName, '-', sLeft, sRight);
  if RepeatIndex = 0 then
    Result := Parent.isElement(sLeft)
  else
    Result := Parent.Parent.isElement(sLeft);
  if Result then
    begin
    StringSplit(sRight, ':', sLeft, sRight);
    Result := StringIsInteger32(sLeft) and (StringToInteger32(sLeft) = Index);
    if Result and (sRight <> '') then
      Result := StringIsInteger32(sRight) and (StringToInteger32(sRight) = RepeatIndex);
    end;
end;

procedure THL7V2DataElement.MakeForBuild(bClear : Boolean);
var
  oStructure : THL7V2ModelStructure;
  iLoop : integer;
  oChild : THL7V2Component;
begin
  assert(Condition(assigned(self), 'MakeForBuild', 'Attempt to use an undefined element'));
  if ((FSpecifiedDataType <> '') And (model.Structures.ExistsByName(FSpecifiedDataType))) Or (assigned(FDefinition) and assigned(FDefinition.RefDataElement) and assigned(FDefinition.RefDataElement.RefStructure)) then
    begin
    if (FSpecifiedDataType <> '') And (model.Structures.ExistsByName(FSpecifiedDataType)) Then
      oStructure := model.Structures.GetByName(FSpecifiedDataType)
    Else
      oStructure := FDefinition.RefDataElement.RefStructure;
    if bClear Then
      Children.Clear;
    for iLoop := 0 to IntegerMax(Children.Count, oStructure.Components.count) - 1 do
      If iLoop >= Children.Count Then
      Begin
        oChild := THL7V2Component.Create(ctComponent, iLoop+1);
        try
          oChild.Defined := False;
          oChild.Definition := oStructure.Components[iLoop].Link;
          oChild.MakeForBuild;
          Children.Add(oChild.Link);
        finally
          oChild.Free;
        end;
        if iLoop = 0 then
          oChild.TakeContent(self);
      End
      Else
      Begin
        oChild := Children[iLoop];
        if iLoop < oStructure.Components.Count Then
          oChild.Definition := oStructure.Components[iLoop].Link
        Else
          oChild.Definition := Nil;
        oChild.MakeForBuild(false);
      End
  End;
end;

function THL7V2DataElement.ChildrenClass: TFslTreeListClass;
begin
  result := THL7V2Components;
end;

function THL7V2DataElement.GetChildren: THL7V2Components;
begin
  result := THL7V2Components(inherited Children);
end;

procedure THL7V2DataElement.SetChildren(const oValue: THL7V2Components);
begin
  inherited Children := oValue;
end;

function THL7V2DataElement.GetComponent(iIndex: Integer): THL7V2Component;
begin
  if Children.ExistsByIndex(iIndex - 1) then
    result := Children[iIndex - 1]
  else
    result := nil;
end;

function THL7V2DataElement.ForceChild(iIndex: Integer): THL7V2Cell;
begin
  result := ForceComponent(iIndex);
end;

function THL7V2DataElement.ForceComponent(iIndex: Integer): THL7V2Component;
begin
  result := nil;
  if iIndex <= 0 then
    ErrorApplication('ForceComponent', 'Can''t force component '+IntegerToString(iIndex))
  else
    begin
    while iIndex > Children.Count do
    Begin
      Children.Add(THL7V2Component.create(ctComponent, Children.Count+1));
      if Children.Count = 1 then
        Children[0].TakeContent(self);
    End;
    result := Children[iIndex - 1];
    end;
end;

function THL7V2DataElement.FullPathName: String;
var
  s : String;
begin
  if HasParent then
    s := Parent.FullPathname
  Else
    s := '';
  if CanHaveRepeats then
    result := s+'-'+IntegerToString(Index)
  else
    result := s+':'+IntegerToString(Index);
end;

function THL7V2DataElement.CanHaveRepeats: Boolean;
begin
  result := FRepeatIndex = 0;
end;

Procedure THL7V2DataElement.Reparent;
Var
  iLoop : Integer;
begin
  inherited;
  if assigned(FRepeats) then
    begin
    FRepeats.Parent := self;
    For iLoop := 0 To FRepeats.Count - 1 Do
      FRepeats[iLoop].Parent := Self;
    end;
end;

function THL7V2DataElement.GetRepeats: THL7V2DataElements;
begin
  assert(Condition(CanHaveRepeats, 'GetRepeats', 'Data Elements that are already repeats cannot contain repeats ['+FullPathName+']'));
  if not assigned(FRepeats) then
    Repeats := THL7V2DataElements.create;
  result := FRepeats;
end;

procedure THL7V2DataElement.SetRepeats(const oValue: THL7V2DataElements);
begin
  assert(Condition(CanHaveRepeats, 'GetRepeats', 'Data Elements that are already repeats cannot contain repeats ['+FullPathName+']'));
  FRepeats.Free;
  FRepeats := oValue;
  Reparent;
end;

function THL7V2DataElement.GetRepeat(iIndex: Integer): THL7V2DataElement;
var
  oRepeats : THL7V2DataElements;
begin
  if iIndex = 0 then
    result := self
  else
    begin
    oRepeats := GetRepeats;
    result := oRepeats[iIndex - 1];
    end;
end;

function THL7V2DataElement.AddRepeat(bFirstIfEmpty: Boolean): THL7V2DataElement;
var
  oRepeats : THL7V2DataElements;
begin
  oRepeats := GetRepeats;
  if bFirstIfEmpty and not Defined then
    result := self
  else
    begin
    Result := THL7V2DataElement.Create(ctDataElement, Index);
    try
      Result.FRepeatIndex := oRepeats.Count + 1;
      Result.FDefinition := FDefinition.Link;
      Result.MakeForBuild;
      oRepeats.Add(Result.Link);
    finally
      Result.Free;
    end;
    end;
end;

Function THL7V2DataElement.ForceRepeat(iIndex : Integer) : THL7V2DataElement;
var
  oRepeats : THL7V2DataElements;
begin
  If (iIndex = 0) Then
    Result := Self
  Else
  Begin
    oRepeats := GetRepeats;
    while oRepeats.Count < iIndex Do
      AddRepeat(false);
    Result := oRepeats[iIndex - 1];
  End;
End;

function THL7V2DataElement.ChooseRepeat(const sLocation, sValue: String): THL7V2DataElement;
var
  oRepeats : THL7V2DataElements;
  iLoop : integer;
  oElem : THL7V2DataElement;
begin
  oRepeats := GetRepeats;
  iLoop := 0;
  result := nil;
  while not assigned(result) and (iLoop <= oRepeats.Count) do
    begin
    if iLoop = 0 then
      oElem := self
    else
      oElem := oRepeats[iLoop - 1];
    if StringEquals(oElem[sLocation].AsString, sValue) then
      result := oElem;
    inc(iLoop);
    end;
end;

// coAllowUnknownContent, coOverwriteEmpty, coOverwriteUnDefined
procedure THL7V2DataElement.Clone(oSource: THL7V2DataElement; aOptions: THL7V2DataElementCloneOptions);
var
  iLoop : Integer;
Begin
  Self.CloneRepeat(oSource, aOptions);
  if oSource.HasRepeats Then
    For iLoop := 0 to oSource.FRepeats.Count - 1 Do
      AddRepeat(false).CloneRepeat(oSource.FRepeats[iLoop], aOptions);
End;

procedure THL7V2DataElement.CloneRepeat(oSource: THL7V2DataElement; aOptions: THL7V2DataElementCloneOptions);
var
  oSrc: THL7V2Component;
  oDst: THL7V2Component;
  iLoop : Integer;
begin
  if (oSource.Components.count = 0) and (self.Components.Count = 0) then
    begin
    if oSource.Defined then
      begin
      if (oSource.Contents.Count > 0) or (coOverwriteEmpty in aOptions) then
        Contents.Assign(oSource.Contents);
      end
    else if (coOverwriteUnDefined in aOptions) then
      Defined := False;
    end

  else if oSource.Components.count = 0 then
    begin
    if oSource.Defined then
      begin
      if (oSource.Contents.Count > 0) or (coOverwriteEmpty in aOptions) then
        Components[0].Contents.Assign(oSource.Contents);
      end
    else if (coOverwriteUnDefined in aOptions) then
      Components[0].Defined := False;
    end

  else
    begin
    { if there is no components existing on Self then we are leaving the dictionary.
      If we are allowing this, then we need to move the content of DataElement into the
      a component prior to beginning the copy }
    if (Components.Count = 0) and (coAllowUnknownContent in AOptions) then
      begin
      oDst := THL7V2Component.Create(ctComponent, 1);
      try
        oDst.Clone(self, false); // clone self into new component - before children.add
        Children.Add(oDst.Link);
      finally
        oDst.Free;
      end;
      end;
    for iLoop := 0 to oSource.Components.Count - 1 do
      begin
      oSrc := oSource.Components[iLoop];
      if // known check:
          (((iLoop < Components.Count) and assigned(Components[iLoop].Definition)) or (coAllowUnknownContent in aOptions)) and
         // defined
          (oSrc.Defined or (coOverwriteUnDefined in aOptions)) and
         // empty
          ((oSrc.Contents.Count > 0) or (coOverwriteEmpty in aOptions)) then
        begin
        oDst := ForceComponent(iLoop+1);
        oDst.Clone(oSrc, true);
        end;
      end;
    end;
end;

function THL7V2DataElement.HasRepeats: Boolean;
begin
  result := CanHaveRepeats and assigned(FRepeats) and (FRepeats.Count > 0);
end;

function THL7V2DataElement.GetRepeatCount: Integer;
begin
  if not HasRepeats then
    result := 1
  else
    result := FRepeats.Count+1;
end;

procedure THL7V2DataElement.Assign(oObject: TFslObject);
begin
  inherited;
  Definition := THL7V2DataElement(oObject).Definition.Link;
  FSpecifiedDataType := THL7V2DataElement(oObject).FSpecifiedDataType;
  FRepeatIndex  := THL7V2DataElement(oObject).FRepeatIndex;
  FRepeats  := THL7V2DataElement(oObject).FRepeats.Clone;
end;

function THL7V2DataElement.Clone: THL7V2DataElement;
begin
  result := THL7V2DataElement(inherited Clone);
end;

function THL7V2DataElement.Link: THL7V2DataElement;
begin
  result := THL7V2DataElement(inherited Link);
end;

procedure THL7V2DataElement.ClearContent;
begin
  inherited;
  if HasRepeats then
    FRepeats.Clear;
end;

procedure THL7V2DataElement.Commit;
begin
  inherited;
  If (Index = 2) And (Parent is THL7V2Segment) And (THL7V2Segment(Parent).Code = 'OBX') Then
    THL7V2Segment(Parent).Field[5].SpecifyDataType(AsString);
end;

procedure THL7V2DataElement.DeleteRepeat(iIndex: Integer);
var
  iLoop : Integer;
  oRep : THL7V2DataElement;
begin
  if iIndex = 0 Then
  Begin
    oRep := FRepeats[0];
    if oRep.HasChildren Then
    Begin
      Contents.Clear;
      for iLoop := 0 to FRepeats[0].Children.Count - 1 Do
        ForceChild(iLoop+1).Decode(FRepeats[0].Children[iLoop].Encode);
    End
    Else
    Begin
      Children.Clear;
      Contents.Assign(FRepeats[0].Contents);
    End;

    FRepeats.DeleteByIndex(0);
  End
  Else if iIndex <= FRepeats.Count Then
    FRepeats.DeleteByIndex(iIndex - 1)
  Else
    RaiseError('DeleteRepeat', 'Invalid reference to repeat #'+IntegerToString(iIndex));
end;

function THL7V2DataElement.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDefinition.sizeInBytes);
  inc(result, (FSpecifiedDataType.length * sizeof(char)) + 12);
  inc(result, FRepeats.sizeInBytes);
end;

{ THL7V2DataElements }

function THL7V2DataElements.Clone: THL7V2DataElements;
begin
  result := THL7V2DataElements(inherited Clone);
end;

function THL7V2DataElements.GetDataElement(iIndex: Integer): THL7V2DataElement;
begin
  result := THL7V2DataElement(objectByIndex[iIndex]);
end;

function THL7V2DataElements.ItemClass: TFslObjectClass;
begin
  result := THL7V2DataElement;
end;

function THL7V2DataElements.Link: THL7V2DataElements;
begin
  result := THL7V2DataElements(inherited Link);
end;

Procedure THL7V2DataElement.Decode(sSource: TBytes; oOptions: THL7V2DecodingOptions);
Var
  oDecoder : THL7V2Decoder;
Begin
  If Not Assigned(oOptions) Or Not (oOptions.DontWipeContent) Then
    ClearContent;
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(sSource, oOptions, fuDataElement);
  Try
    oDecoder.DecodeDataElement(Self, sSource, oOptions);
  Finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  End;
End;

Procedure THL7V2DataElement.Decode(oSource: TFslBuffer; oOptions: THL7V2DecodingOptions);
Var
  oDecoder : THL7V2Decoder;
Begin
  If Not Assigned(oOptions) Or Not (oOptions.DontWipeContent) Then
    ClearContent;
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(oSource, oOptions, fuDataElement);
  Try
    oDecoder.DecodeDataElement(Self, oSource, oOptions);
  Finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  End;
End;

Function THL7V2DataElement.Encode(aFormat: THL7V2Format; aOptions: THL7V2EncodingOptionSet): TBytes;
Var
  oOptions: THL7V2EncodingOptions;
Begin
  oOptions := THL7V2EncodingOptions.Create(aFormat, aOptions);
  Try
    Result := Encode(oOptions);
  Finally
    oOptions.Free;
  End;
End;

Function THL7V2DataElement.Encode(oOptions: THL7V2EncodingOptions): TBytes;
Var
  oLocal : THL7V2EncodingOptions;
  oEncoder : THL7V2Encoder;
Begin
  If Assigned(oOptions) Then
    oLocal := oOptions.Link
  Else
    oLocal := THL7V2EncodingOptions.Create;
  Try
// todo - do we want to do this?
//    if (oOptions.Format = hfUnknown) then
//      oOptions.Format := Message.Format
    oEncoder := GHL7V2EncoderFactory.ProduceEncoder(oOptions.Format);
    Try
      Result := oEncoder.EncodeDataElement(Self, oOptions);
    Finally
      GHL7V2EncoderFactory.ConsumeEncoder(oEncoder);
    End;
  Finally
    oLocal.Free;
  End;
End;

Procedure THL7V2DataElement.Encode(oBuffer: TFslBuffer; oOptions: THL7V2EncodingOptions);
Var
  oLocal : THL7V2EncodingOptions;
  oEncoder : THL7V2Encoder;
Begin
  If Assigned(oOptions) Then
    oLocal := oOptions.Link
  Else
    oLocal := THL7V2EncodingOptions.Create;
  Try
// todo - do we want to do this?
//    if (oOptions.Format = hfUnknown) then
//      oOptions.Format := Message.Format
    oEncoder := GHL7V2EncoderFactory.ProduceEncoder(oOptions.Format);
    Try
      oEncoder.EncodeDataElement(oBuffer, Self, oOptions);
    Finally
      GHL7V2EncoderFactory.ConsumeEncoder(oEncoder);
    End;
  Finally
    oLocal.Free;
  End;
End;

procedure THL7V2DataElement.StripUndefinedContent;
var
  i : integer;
  s : TBytes;
begin
  SetLength(s, 0);

  if HasRepeats then
    for i := 0 to Repeats.count - 1 do
      Repeats[i].StripUndefinedContent;
  // special case:
  if (Definition <> nil) And ((Definition.RefDataElement = nil) or (Definition.RefDataElement.RefStructure = nil) or (Definition.RefDataElement.RefStructure.Components.Count = 0)) and
    (Components.Count > 0) then
    s := Components[0].Encode;

  for i := 0 to Components.Count - 1 do
    if Components[i].Definition = nil then
      Components[i].ClearContent
    else
      Components[i].StripUndefinedContent;
  if Length(s) > 0 then
    decode(s);
end;

procedure THL7V2DataElement.MapToDefinitions(Model: THL7V2ModelSegment; ndx : integer);
var
  i : integer;
begin
  if (Model = nil) or (Model.Fields.Count <= ndx) then
    Definition := nil
  else
    Definition := Model.Fields[ndx].Link;
  if HasRepeats then
    for i := 0 to Repeats.Count - 1 do
      Repeats[i].MapToDefinitions(Model, i);

  for i := 0 to Components.Count - 1 do
    if (FDefinition <> nil) and (Definition.RefDataElement <> nil) then
      Components[i].MapToDefinitions(Definition.RefDataElement.RefStructure, i)
    else
      Components[i].MapToDefinitions(nil, i);
end;

{ THL7V2Segment }

destructor THL7V2Segment.Destroy;
begin
  FDefinition.Free;
  inherited;
end;

procedure THL7V2Segment.SetDefinition(const oValue: THL7V2ModelSegment);
begin
  FDefinition.Free;
  FDefinition := oValue;
end;

procedure THL7V2Segment.SetCode(const sValue: String);
begin
  if FCode <> '' then
    ErrorApplication('SetCode', 'Cannot change a Segment type once it has been created');
  FCode := sValue;
  if HasParent then
    FDefinition := Model.Segments.GetByCode(FCode).Link;
end;

function THL7V2Segment.FullPathName: String;
var
  iSeq : integer;
begin
  if HasParent Then
    result := Parent.FullPathName
  Else
    result := '';
  if result <> '' then
    result := result + '-';
  result := result + Code;
  iSeq := SequenceNumber;
  if iSeq > 0 then
    result := result + ':'+IntegerToString(iSeq);
end;

function THL7V2Segment.ChildrenClass: TFslTreeListClass;
begin
  result := THL7V2DataElements;
end;

function THL7V2Segment.GetChildren: THL7V2DataElements;
begin
  result := THL7V2DataElements(inherited Children);
end;

procedure THL7V2Segment.SetChildren(const oValue: THL7V2DataElements);
begin
  inherited Children := oValue;
end;

function THL7V2Segment.GetField(iIndex: Integer): THL7V2DataElement;
begin
  if self = nil then
    Result  := Nil
  Else if Children.ExistsByIndex(iIndex - 1) then
    result := Children[iIndex - 1]
  else
    result := nil;
end;

function THL7V2Segment.ForceField(iIndex: Integer): THL7V2DataElement;
begin
  result := nil;
  if iIndex <= 0 then
    ErrorApplication('ForceField', 'Can''t force Field '+IntegerToString(iIndex))
  else
    begin
    while iIndex > Children.Count do
      Children.Add(THL7V2DataElement.create(ctDataElement, Children.Count+1));
    result := Children[iIndex - 1];
    end;
end;

procedure THL7V2Segment.BuildFields;
var
  iLoop : integer;
  oField : THL7V2DataElement;
begin
  if assigned(FDefinition) then
    begin
    for iLoop := 0 to FDefinition.Fields.count - 1 do
      begin
      oField := THL7V2DataElement.create(ctDataElement, iLoop + 1);
      try
        oField.Definition := FDefinition.Fields[iLoop].Link;
        oFIeld.MakeForBuild;
        Children.Add(oField.Link);
      finally
        oField.Free;
      end;
      end;
    end;
end;

function THL7V2Segment.GetElement(const sCode: String): THL7V2Cell;
var
  sField: String;
  sOther : String;
  oParser : THL7V2Query;
begin
  result := nil;
  if self = nil then
    result := nil
  else if (pos('[', sCode) > 0) or (pos(':', sCode) > 0) then
    begin
    oParser := THL7V2Query.create;
    try
      oParser.ParseQuery(sCode, qctSegment);
      result := oParser.Execute(self, false);
    finally
      oParser.Free;
    end
    end
  else
    begin
    StringSplit(sCode, ['-', '.'], sField, sOther);
    if not StringIsInteger32(sField) then
      ErrorApplication('GetElement', 'Cannot look for field "'+sField+'" in segment '+FullPathName)
    else
      begin
      result := Field[StringToInteger32(sField)];
      if sOther <> '' then
        result := result[sOther];
      end;
    end;
end;

procedure THL7V2Segment.ForceElement(const sCode: String);
var
  sField: String;
  sRepeat : String;
  sOther : String;
  oField : THL7V2DataElement;
begin
  assert(Condition(assigned(self), 'ForceElement', 'Attempt to force on element an an undefined element'));
  if (pos('[', sCode) > 0) then
    ErrorHL7Library('ForceElement', 'Cannot force an element with advanced query syntax ['+sCode+']')
  else
    begin
    StringSplit(sCode, ['-', '.'], sField, sOther);
    if Pos(':', sField) > 0 Then
      StringSplit(sField, ':', sField, sRepeat);
    if not StringIsInteger32(sField) then
      ErrorApplication('GetElement', 'Cannot force field "'+sField+'" in segment '+FullPathName)
    Else if (sRepeat <> '') And not StringIsInteger32(sRepeat) then
      ErrorApplication('GetElement', 'Cannot force field repeat "'+sRepeat+'" in segment '+FullPathName)
    else
      begin
      oField := ForceField(StringToInteger32(sField));
      if (sRepeat <> '') Then
        oField := oField.ForceRepeat(StringToInteger32(sRepeat));
      if sOther <> '' then
        oField.ForceElement(sOther);
      end;
    end;
end;

function THL7V2Segment.IsElement(const sName: String): Boolean;
var
  sLeft, sRight : String;
begin
  StringSplit(sName, '-', sLeft, sRight);
  result := (sRight = '') or (HasParent and Parent.IsElement(sLeft));
  if result then
    begin
    if sRight = '' then
      StringSplit(sLeft, ':', sLeft, sRight)
    else
      StringSplit(sRight, ':', sLeft, sRight);
    result := Matches(sLeft);
    if result and (sRight <> '') then
      result := StrToIntDef(sRight, -1) = SequenceNumber;
    end;
end;

procedure THL7V2Segment.Clone(oSource: THL7V2Segment; aOptions: THL7V2DataElementCloneOptions);
var
  iLoop : integer;
begin
  for iLoop := 0 to oSource.fields.Count - 1 do
    if (iLoop < Fields.count) or (coAllowUnknownContent in aOptions) then
      ForceField(iLoop + 1).Clone(oSource.Fields[iLoop], aOptions);
end;

function THL7V2Segment.GetIndex: Integer;
begin
  if HasParent then
    result := Parent.Children.IndexByReference(self)
  else
    result := 0;
end;

procedure THL7V2Segment.Decode(sSource: TBytes; oOptions: THL7V2DecodingOptions);
var
  oDecoder : THL7V2Decoder;
begin
  if not assigned(oOptions) or not (oOptions.DontWipeContent) then
    ClearContent;
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(sSource, oOptions, fuSegment);
  try
    oDecoder.DecodeSegment(self, sSource, oOptions); // if you get a compile error here, you need to rebuild (compiler bug?)
  finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  end;
end;

procedure THL7V2Segment.Decode(oSource: TFslBuffer; oOptions: THL7V2DecodingOptions);
var
  oDecoder : THL7V2Decoder;
begin
  if not assigned(oOptions) or not (oOptions.DontWipeContent) then
    ClearContent;
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(oSource, oOptions, fuSegment);
  try
    oDecoder.DecodeSegment(self, oSource, oOptions);
  finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  end;
end;

procedure THL7V2Segment.ClearContent;
var
  iLoop : integer;
begin
  for iLoop := 0 to Fields.count - 1 do
    Fields[iLoop].ClearContent;
end;

function THL7V2Segment.Encode(aFormat: THL7V2Format; aOptions: THL7V2EncodingOptionSet): TBytes;
var
  oOptions: THL7V2EncodingOptions;
begin
  oOptions := THL7V2EncodingOptions.create(aFormat, aOptions);
  try
    result := Encode(oOptions);
  finally
    oOptions.Free;
  end;
end;

function THL7V2Segment.Encode(oOptions: THL7V2EncodingOptions): TBytes;
var
  oLocal : THL7V2EncodingOptions;
  oEncoder : THL7V2Encoder;
begin
  if assigned(oOptions) then
    oLocal := oOptions.Link
  else
    oLocal := THL7V2EncodingOptions.create;
  try
// todo - do we want to do this?
//    if (oOptions.Format = hfUnknown) then
//      oOptions.Format := Message.Format
    oEncoder := GHL7V2EncoderFactory.ProduceEncoder(oOptions.Format);
    try
      result := oEncoder.EncodeSegment(self, oOptions);
    finally
      GHL7V2EncoderFactory.ConsumeEncoder(oEncoder);
    end;
  finally
    oLocal.Free;
  end;
end;

procedure THL7V2Segment.Encode(oBuffer: TFslBuffer; oOptions: THL7V2EncodingOptions);
var
  oLocal : THL7V2EncodingOptions;
  oEncoder : THL7V2Encoder;
begin
  if assigned(oOptions) then
    oLocal := oOptions.Link
  else
    oLocal := THL7V2EncodingOptions.create;
  try
// todo - do we want to do this?
//    if (oOptions.Format = hfUnknown) then
//      oOptions.Format := Message.Format
    oEncoder := GHL7V2EncoderFactory.ProduceEncoder(oOptions.Format);
    try
      oEncoder.EncodeSegment(oBuffer, self, oOptions);
    finally
      GHL7V2EncoderFactory.ConsumeEncoder(oEncoder);
    end;
  finally
    oLocal.Free;
  end;
end;

procedure THL7V2Segment.Assign(oObject: TFslObject);
begin
  inherited;
  FCode  := THL7V2Segment(oObject).FCode;
  Definition  := THL7V2Segment(oObject).Definition.Link;

end;

function THL7V2Segment.Clone: THL7V2Segment;
begin
  result := THL7V2Segment(inherited Clone);
end;

function THL7V2Segment.Link: THL7V2Segment;
begin
  result := THL7V2Segment(inherited Link);
end;

function THL7V2Segment.SequenceNUmber: Integer;
var
  iLoop : integer;
begin
  result := -1;
  if HasParent Then
    for iLoop := 0 to Parent.Children.Count -1 do
    begin
      if (Parent.Children[iLoop] is THL7V2Segment) and (THL7V2Segment(Parent.Children[iLoop]).Code = Code) then
      begin
        inc(result);
        if Parent.Children[iLoop] = self then
          exit;
      end;
    end;

end;

function THL7V2Segment.Matches(sMask: String): Boolean;
var
  iLoop : Integer;
  sCode : String;
begin
  sCode := FCode;
  if sMask = '' then
    sMask := '*';
  for iLoop := 1 to length(sMask) do
    if sMask[iLoop] = '?' then
      sCode[iLoop] := '?';
  if sMask[length(sMask)] = '*' then
    begin
    delete(sCode, length(sMask)+1, $FF);
    sCode[length(sMask)] := '*';
    end;
  Result := sMask = sCode;
end;

procedure THL7V2Segment.Clear;
begin
  ErrorApplication('Clear', 'Cannot clear a segment');
  inherited;
end;

procedure THL7V2Segment.StripUndefinedContent;
var
  i : integer;
begin
  for i := 0 to Fields.Count - 1 do
    if Fields[i].Definition = nil then
      Fields[i].ClearContent
    else
      Fields[i].StripUndefinedContent;
end;

procedure THL7V2Segment.MapToDefinitions(Model: THL7V2Model);
var
  i : integer;
begin
  if Model = nil then
    Definition := nil
  else
    Definition := Model.Segments.GetByCode(FCode).Link;
  for i := 0 to Fields.Count - 1 do
    Fields[i].MapToDefinitions(Definition, i);
end;

function THL7V2Segment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FCode.length * sizeof(char)) + 12);
  inc(result, FDefinition.sizeInBytes);
end;

{ THL7V2Segments }

function THL7V2Segments.AsText: String;
var
  iLoop : integer;
begin
  result := '';
  for iLoop := 0 to Count - 1 do
    StringAppend(result, Segment[iLoop].Code, ',');
end;

function THL7V2Segments.CompareByCode(pA, pB: Pointer): Integer;
begin
  Result := StringCompare(THL7V2Segment(pA).Code, THL7V2Segment(pB).Code);
end;

function THL7V2Segments.CountByType(const sCode: String): integer;
var
  iLoop : integer;
begin
  result := 0;
  for iLoop := 0 to Count - 1 do
    if Segment[iLoop].Code = sCode then
      inc(result);
end;

procedure THL7V2Segments.DropByType(const sCode: String);
var
  iLoop : integer;
begin
  for iLoop := Count - 1 downto 0 do
    if Segment[iLoop].Code = sCode then
      DeleteByIndex(iLoop);
end;

function THL7V2Segments.ExistsByCode(const sValue: String): Boolean;
begin
  result := ExistsByIndex(IndexByCode(sValue));
end;

function THL7V2Segments.Get(const iIndex: Integer): THL7V2Segment;
begin
  result := THL7V2Segment(inherited Get(iIndex));
end;

function THL7V2Segments.GetByCode(const sValue: String): THL7V2Segment;
begin
  result := Get(IndexByCode(sValue));
end;

function THL7V2Segments.GetByCodeAndIndex(const sCode: String; iIndex: Integer): THL7V2Segment;
var
  iCount : integer;
  iLoop : integer;
begin
  iCount := 0;
  iLoop := 0;
  result := nil;
  while not assigned(result) and (iLoop < count) do
    begin
    if StringEquals(Segment[iLoop].Code, sCode) then
      if (iCount = iIndex) then
        result := Segment[iLoop]
      else
        inc(iCount);
    inc(iLoop);
    end;
end;

function THL7V2Segments.GetSegment(iIndex: Integer): THL7V2Segment;
begin
  result := THL7V2Segment(objectByIndex[iIndex]);
end;

function THL7V2Segments.IndexByCode(const sValue: String): Integer;
Var
  oElement : THL7V2Segment;
Begin
  oElement := THL7V2Segment.create;
  Try
    oElement.Code := sValue;

    If Not Find(oElement, Result, CompareByCode) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
end;

function THL7V2Segments.IndexInType(oSegment: THL7V2Segment): integer;
var
  iLoop : integer;
begin
  result := -1;
  for iLoop := 0 to Count -1 do
    begin
    if Segment[iLoop].Code = oSegment.Code then
      begin
      inc(result);
      if Segment[iLoop] = oSegment then
        exit
      end;
    end;
end;

function THL7V2Segments.ItemClass: TFslObjectClass;
begin
  result := THL7V2Segment;
end;

{ THL7V2SegmentList }

function THL7V2SegmentList.GetSegment(iIndex: integer): THL7V2Segment;
begin
  result := THL7V2Segment(ObjectByIndex[iIndex]);
end;

function THL7V2SegmentList.ItemClass: TFslObjectClass;
begin
  result := THL7V2Segment;
end;

{ THL7V2SegmentGroup }

constructor THL7V2SegmentGroup.Create;
begin
  inherited;
  FSegments := THL7V2SegmentList.create;
end;

destructor THL7V2SegmentGroup.Destroy;
begin
  FSegments.Free;
  inherited;
end;

function THL7V2SegmentGroup.ChildrenClass: TFslTreeListClass;
begin
  result := THL7V2SegmentGroups;
end;

function THL7V2SegmentGroup.GetChildren: THL7V2SegmentGroups;
begin
  result := THL7V2SegmentGroups(inherited Children);
end;

function THL7V2SegmentGroup.GetParent: THL7V2SegmentGroup;
begin
  result := THL7V2SegmentGroup(inherited Parent);
end;

function THL7V2SegmentGroup.ParentClass: TFslTreeClass;
begin
  result := THL7V2SegmentGroup;
end;

procedure THL7V2SegmentGroup.SetChildren(const oValue: THL7V2SegmentGroups);
begin
  inherited Children := oValue;
end;

procedure THL7V2SegmentGroup.SetParent(const oValue: THL7V2SegmentGroup);
begin
  inherited Parent := oValue;
end;

procedure THL7V2SegmentGroup.SetSegments(const oValue: THL7V2SegmentList);
begin
  FSegments.Free;
  FSegments := oValue;
end;

function THL7V2SegmentGroup.Clone: THL7V2SegmentGroup;
begin
  result := THL7V2SegmentGroup(inherited Clone);
end;

function THL7V2SegmentGroup.Link: THL7V2SegmentGroup;
begin
  result := THL7V2SegmentGroup(inherited Link);
end;

procedure THL7V2SegmentGroup.AddSegment(oSegment: THL7V2Segment);
var
  oChild : THL7V2SegmentGroup;
begin
  oChild := nil;
  try
    if (Children.Count > 0) and (Children[Children.Count - 1].Name = '') then
      oChild := Children[Children.Count - 1].Link
    else
      begin
      oChild := THL7V2SegmentGroup.Create;
      Children.Add(oChild.Link);
      end;
    oChild.Segments.add(oSegment)
  finally
    oChild.Free;
  end;
end;

function THL7V2SegmentGroup.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FSegments.sizeInBytes);
end;

{ THL7V2SegmentGroups }

function THL7V2SegmentGroups.GetGroup(iIndex: Integer): THL7V2SegmentGroup;
begin
  result := THL7V2SegmentGroup(objectByIndex[iIndex]);
end;

function THL7V2SegmentGroups.ItemClass: TFslObjectClass;
begin
  result := THL7V2SegmentGroup;
end;

{ THL7V2Message }

Constructor THL7V2Message.Create;
Begin
  Inherited;
  FDelimiters := THL7V2Delimiters.Create;
End;

Destructor THL7V2Message.Destroy;
Begin
  Clear;
  FDelimiters.Free;
  Inherited;
End;

Procedure THL7V2Message.Clear;
Begin
  Children.Clear;
  FDelimiters.Reset;
  FDefinition.Free;
  FDefinition := Nil;
  FSegmentMap.Free;
  FSegmentMap := Nil;
  FEvent := '';
  FMessageType := '';
  FStructID := '';
  FFormat := hfUnknown;
  FMessageID := '';
  Inherited;
End;

Function THL7V2Message.FullPathName: String;
Begin
  If HasParent Then
    Result := Parent.FullPathName+'-'+IntegerToString(Parent.Children.IndexByReference(Self))
  Else
    Result := '';
End;

Function THL7V2Message.IsElement(Const sName: String): Boolean;
Var
  sLeft, sRight : String;
Begin
  If HasParent Then
    Begin
    StringSplit(sName, '-', sLeft, sRight);
    Result := ((sLeft = '') Or Parent.IsElement(sLeft)) And StringIsInteger32(sRight) And (StringToInteger32(sRight) = Parent.Children.IndexByReference(Self));
    End
  Else
    Result := (sName = '');
End;

Function THL7V2Message.ChildrenClass: TFslTreeListClass;
Begin
  Result := THL7V2Segments;
End;

Function THL7V2Message.GetChildren: THL7V2Segments;
Begin
  Result := THL7V2Segments(Inherited Children);
End;

Procedure THL7V2Message.SetChildren(Const Value: THL7V2Segments);
Begin
  Inherited Children := Value;
End;

Function THL7V2Message.InsertSegment(iIndex: Integer; Const sCode: String): THL7V2Segment;
Begin
  DropMap;
  Result := THL7V2Segment.Create;
  Try
    Segments.Insert(iIndex, Result.Link);
    Result.Code := sCode;
    Result.BuildFields;
  Finally
    Result.Free;
  End;
End;

Function THL7V2Message.AddSegment(Const sCode: String): THL7V2Segment;
Begin
  DropMap;
  Result := THL7V2Segment.Create;
  Try
    Segments.Add(Result.Link);
    Result.Code := sCode;
    Result.BuildFields;
  Finally
    Result.Free;
  End;
End;

Procedure THL7V2Message.CloneSegment(oSource : THL7V2Segment);
Begin
  AddSegment(oSource.Code).Clone(oSource, [coAllowUnknownContent, coOverwriteEmpty, coOverwriteUnDefined]);
End;

Function THL7V2Message.CountSegment(Const sCode: String): Integer;
Begin
  Result := Segments.CountByType(sCode);
End;

Procedure THL7V2Message.DropSegments(Const sCode: String);
Begin
  Segments.DropByType(sCode);
End;

Function THL7V2Message.GetSegment(Const sCode: String; iIndex: Integer): THL7V2Segment;
Begin
  Result := Segments.GetByCodeAndIndex(sCode, iIndex);
End;

Function THL7V2Message.GetElement(Const sCode: String): THL7V2Cell;
Var
  sSeg: String;
  sOther : String;
  oParser : THL7V2Query;
Begin
  If (Pos('[', sCode) > 0) Or (Pos(':', sCode) > 0) Then
    Begin
    oParser := THL7V2Query.Create;
    Try
      oParser.ParseQuery(sCode, qctMessage);
      Result := oParser.Execute(Self, False);
    Finally
      oParser.Free;
    End
    End
  Else
    Begin
    StringSplit(sCode, '-', sSeg, sOther);
    If Not Length(sSeg) = 3 Then
      ErrorBadSegCode('GetElement', 'Error processing Query "'+sCode+'", Segment code "'+sSeg+'" is illegal');
    Result := Segment[sSeg, 0].Element[sOther];
    End;
End;

Procedure THL7V2Message.ForceElement(Const sCode: String);
Var
  sSeg: String;
  sOther : String;
  oSegment : THL7V2Segment;
Begin
  If (Pos('[', sCode) > 0) Then
    ErrorHL7Library('GetElement', 'cannot use advanced query options for ForceElement')
  Else
    Begin
    StringSplit(sCode, '-', sSeg, sOther);
    If Not Length(sSeg) = 3 Then
      ErrorBadSegCode('GetElement', 'Error processing Query "'+sCode+'", Segment code "'+sSeg+'" is illegal');
    oSegment := Segment[sSeg, 0];
    If Not Assigned(oSegment) Then
      ErrorBadSegCode('GetElement', 'Error processing Query "'+sCode+'", Segment "'+sSeg+'" not found');
    oSegment.ForceElement(sOther);
    End;
End;

Function THL7V2Message.AddMSHForBuild : THL7V2Segment;
Begin
  Assert(CountSegment('MSH') = 0, 'Attempt to add a second MSH to a message');
  Result := AddSegment('MSH');
  Result.Field[1].AsString := Delimiters.FieldDelimiter;
  Result.Field[2].AsString := Delimiters.ComponentDelimiter+Delimiters.RepetitionDelimiter+Delimiters.EscapeCharacter+Delimiters.SubComponentDelimiter;
  Result.Field[12].AsString := NAMES_HL7V2_VERSION[Version];
  If Version = hv21 Then
    Result.Field[9].AsString := FMessageType
  Else
    Begin
    Result.Field[9].Component[2].AsString := FEvent;
    Result.Field[9].Component[1].AsString := FMessageType;
    If Version >= hv231 Then
      Begin
      TryLoadStruct;
      If FStructID <> '' Then
        Result.Field[9].Component[3].AsString := FStructID;
      End;
    End;
  Result.Field[7].AsDateTime := LocalDateTime;
  If FMessageID <> '' Then
    Result.Field[10].AsString := FMessageID;
End;

Procedure THL7V2Message.BuildAsReply(oSource: THL7V2Message; bUseSequenceNum: Boolean; iSequenceNum: Int64);
Var
  oSegment : THL7V2Segment;
  oSrc : THL7V2Segment;
Begin
  Clear;
  if oSource.Format <> hfUnknown Then
    Format := oSource.Format;

  if oSource.hasVersion Then
    Version := oSource.Version
  Else If Not HasVersion Then
    If Format = hfXML Then
      Version := hv231
    Else
      Version := hv22;

  if oSource.HasDelimiters Then
    Delimiters.Assign(oSource.Delimiters);
  If oSource.Event <> '' Then
    Event := oSource.Event;
  MessageType := 'ACK'; // later, figure this out properly
  MessageID := FormatDateTime('yyyymmddhhnnsszzz', LocalDateTime);
  oSegment := AddMSHForBuild;
  oSrc := oSource.Segment['MSH', 0];

  If oSrc <> Nil Then
  Begin
    // echo MSH3-6 flipped around. Application can modify if desired
    oSegment.Field[3].Clone(oSrc.Field[5], HL7V2_ALL_CLONE_OPTIONS);
    oSegment.Field[4].Clone(oSrc.Field[6], HL7V2_ALL_CLONE_OPTIONS);
    oSegment.Field[5].Clone(oSrc.Field[3], HL7V2_ALL_CLONE_OPTIONS);
    oSegment.Field[6].Clone(oSrc.Field[4], HL7V2_ALL_CLONE_OPTIONS);
    // processingId
    oSegment.Field[11].Clone(oSrc.Field[11], HL7V2_ALL_CLONE_OPTIONS);
    // not sure if this is correct? echo MSH Sequence num in MSH.
    If bUseSequenceNum Then
      oSegment.Field[13].Clone(oSrc.Field[13], HL7V2_ALL_CLONE_OPTIONS);
  End
  Else
  Begin
    oSegment.Field[3].AsString := 'HL7Connect';
    oSegment.Field[4].AsString := 'Unknown';
    oSegment.Field[5].AsString := 'Unknown';
    oSegment.Field[6].AsString := 'Unknown';
    oSegment.Field[11].AsString := 'P';
  End;

  oSegment := AddSegment('MSA');
  oSegment.Field[1].AsString := 'AA'; // OK by default
  If oSrc <> Nil Then
    oSegment.Field[2].Clone(oSrc.Field[10], HL7V2_ALL_CLONE_OPTIONS);

  // application may need to overrule in case of sequence sync
  If bUseSequenceNum Then
    oSegment.Field[4].AsString := IntegerToString(iSequenceNum);
End;

Function StringStringEoln(s : String): String;
Begin
  result := StringReplace(s, #13#10, ' ');
  result := StringReplace(result, #13, ' ');
  result := StringReplace(result, #10, ' ');
End;

Procedure THL7V2Message.SetException(oExcept: Exception; bOldStyle : Boolean);
var
  oEv : EHL7V2Exception;
Begin
  if CountSegment('MSA') = 0 Then
    // we're going to add one whether or not it was there. in a sense this may
    // be innappropriate; we cannot easily tell. We will just assume that the
    // caller knows what they are doing
    AddSegment('MSA');

  If oExcept Is EHL7V2Exception Then
    Begin
    If oExcept Is EHL7V2RejectException Then
      Element['MSA-1'].AsString := 'AR'
    Else If oExcept Is EHL7V2ErrorException Then
      Element['MSA-1'].AsString := 'AE'
    Else
    Begin
      oEv := oExcept As EHL7V2Exception;
      if oEv.Code <> '' Then
        Element['MSA-1'].AsString := oEv.Code
      else if bOldStyle then
        Element['MSA-1'].AsString := RESPONSES_HL7V2ERRORCONDITION_OLD[oEv.Condition]
      Else
        Element['MSA-1'].AsString := RESPONSES_HL7V2ERRORCONDITION_NEW[oEv.Condition];
    End;
    If Version > hv21 Then
      Element['MSA-6.1'].AsString := CODES_HL7V2ERRORCONDITION[(oExcept As EHL7V2Exception).Condition];
    End
  Else
  // not sure whether to put AE or AR here. this error presumably rates as an
  // internal error since it is a plain error - a database problem? We will
  // use AE but use a 207 code - in a sense this is a dollar each way but
  // common custom is to put AE. New style is AR of course.
    Begin
    If Version > hv21 Then
      Element['MSA-6.1'].AsString := '207';
    if bOldStyle Then
      Element['MSA-1'].AsString := 'AE'
    Else
      Element['MSA-1'].AsString := 'AR';
    End;
  Element['MSA-3'].AsString := StringStringEoln(oExcept.Message);
End;

Procedure THL7V2Message.SetExceptionMessage(Const sMessage: String; bMakeReject: Boolean);
Begin
  Assert(CountSegment('MSA') = 1, 'Attempt to encode an exception in a message but it is not an answer message');

  If bMakeReject Then
    Segment['MSA', 0].Field[1].AsString := 'AR'
  Else
    Segment['MSA', 0].Field[1].AsString := 'AE';
  Segment['MSA', 0].Field[3].AsString := sMessage;
End;

Function THL7V2Message.GetEvent: String;
Var
  oSegment : THL7V2Segment;
Begin
  Result := FEvent;
  If Result = '' Then
    Begin
    oSegment := GetSegment('MSH', 0);
    If Assigned(oSegment) Then
      Result := oSegment.Field[9].Component[2].AsString
    Else
      Begin
      oSegment := GetSegment('EVN', 0);
      If Assigned(oSegment) Then
        Result := oSegment.Field[1].AsString;
      End;
    End;
End;

Function THL7V2Message.GetMessageID: String;
Var
  oSegment : THL7V2Segment;
Begin
  Result := FMessageID;
  If Result = '' Then
    Begin
    oSegment := GetSegment('MSH', 0);
    If Assigned(oSegment) Then
      Result := oSegment.Field[10].AsString
    End;
End;

Function THL7V2Message.GetMessageType: String;
Var
  oSegment : THL7V2Segment;
Begin
  Result := FMessageType;
  If Result = '' Then
    Begin
    oSegment := GetSegment('MSH', 0);
    If Assigned(oSegment) Then
      Result := oSegment.Field[9].Component[1].AsString;
    End;
End;

Function THL7V2Message.GetStructID: String;
Var
  oSegment : THL7V2Segment;
Begin
  Result := FStructID;
  If Result = '' Then
    Begin
    oSegment := GetSegment('MSH', 0);
    If Assigned(oSegment) Then
      Result := oSegment.Field[9].Component[3].AsString;
    End;
End;

Function THL7V2Message.Query(Const sCode: String; oResults: THL7V2BaseObjectList): Boolean;
Var
  oQuery : THL7V2ParsedQuery;
Begin
  oResults.Clear;
  oQuery := THL7V2ParsedQuery.Create;
  Try
    oQuery.Parse(sCode);
    oQuery.SelectSegments(Segments, oResults);
    Result := oResults.Count > 0;
  Finally
    oQuery.Free;
  End;
End;

Function THL7V2Message.QueryType(Const sCode: String): THL7V2BaseObjectClass;
Var
  oQuery : THL7V2ParsedQuery;
Begin
  oQuery := THL7V2ParsedQuery.Create;
  Try
    oQuery.Parse(sCode);
    Result := oQUery.ResultType;
  Finally
    oQuery.Free;
  End;
End;

Procedure THL7V2Message.StripEmptyRepeats(bLeading: Boolean);
Var
  oSegment: THL7V2Segment;
  oField: THL7V2DataElement;
  iSegment : Integer;
  iField : Integer;
  iRepeat : Integer;
Begin
  For iSegment := 0 To Segments.Count - 1 Do
    Begin
    oSegment := Segments[iSegment];
    For iField := 0 To oSegment.Fields.Count - 1 Do
      Begin
      oField := oSegment.Fields[iField];
      If oField.HasRepeats Then
        Begin
        For iRepeat := oField.Repeats.Count - 1 DownTo 0 Do
          If Not oField.Repeats[iRepeat].HasContent Then
            oField.Repeats.DeleteByIndex(iRepeat);

        If bLeading And Not oField.HasContent And (oField.Repeats.Count > 0) Then
          Begin
          oField.Clone(oField.repeats[0], HL7V2_ALL_CLONE_OPTIONS);
          oField.Repeats.DeleteByIndex(0);
          End;
        End;
      End;
    End;
End;

Procedure THL7V2Message.SetEvent(Const sValue: String);
Var
  oSegment : THL7V2Segment;
Begin
  DropDefinition;
  FEvent := sValue;
  If Version > hv21 Then
    Begin
    oSegment := GetSegment('MSH', 0);
    If oSegment <> Nil Then
      oSegment.Field[9].Component[2].AsString := sValue;
    End;
  TryLoadStruct;
End;

Procedure THL7V2Message.SetMessageID(Const sValue: String);
Var
  oSegment : THL7V2Segment;
Begin
  FMessageID := sValue;
  oSegment := GetSegment('MSH', 0);
  If oSegment <> Nil Then
    oSegment.Field[10].AsString := sValue;
End;

Procedure THL7V2Message.SetMessageType(Const sValue: String);
Var
  oSegment : THL7V2Segment;
Begin
  DropDefinition;
  FMessageType := sValue;
  oSegment := GetSegment('MSH', 0);
  If oSegment <> Nil Then
    If Version = hv21 Then
      oSegment.Field[9].AsString := sValue
    Else
      oSegment.Field[9].Component[2].AsString := sValue;
  TryLoadStruct;
End;

Procedure THL7V2Message.SetStructID(Const sValue: String);
Var
  oSegment : THL7V2Segment;
Begin
  DropDefinition;
  FStructID := sValue;
  If Version>= hv231 Then
    Begin
    oSegment := GetSegment('MSH', 0);
    If oSegment <> Nil Then
      oSegment.Field[9].Component[3].AsString := sValue;
    End;
  TryLoadStruct;
End;

Procedure THL7V2Message.DropDefinition;
Begin
  DropMap;
  FStructId := '';
  FDefinition.Free;
  FDefinition := Nil;
End;

Procedure THL7V2Message.DropMap;
Begin
  FSegmentMap.Free;
  FSegmentMap := Nil;
End;

Procedure THL7V2Message.TryLoadStruct;
Var
  oEvent: THL7V2ModelEvent;
  oMessage : THL7V2ModelEventMessage;
Begin
  If (FStructID = '') Then
  Begin
    If (FEvent <> '') And (FMessageType <> '') Then
    Begin
      oEvent := Model.Events.GetByName(FEvent);
      If Assigned(oEvent) Then
      Begin
        oMessage := oEvent.Messages.GetByMessage(FMessageType);
        If Assigned(oMessage) Then
          FStructId := oMessage.Structure
        Else
        Begin
          oMessage := oEvent.Messages.GetByReply(FMessageType);
          If Assigned(oMessage) Then
            FStructId := oMessage.ReplyStructure
          Else If FMessageType = 'ACK' Then
            FStructID := 'ACK';
        End;
      End
    End
    Else If (FMessageType = 'ACK') Then
      FStructID := 'ACK';
  End;

  If FStructID <> '' Then
    Begin
    FDefinition.Free;
    FDefinition := Model.MessageStructures.GetByName(FStructID).Link;
    End;
End;

Procedure THL7V2Message.Decode(sSource: TBytes; oOptions : THL7V2DecodingOptions = Nil);
Var
  oDecoder : THL7V2Decoder;
Begin
  Clear;
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(sSource);
  Try
    oDecoder.DecodeMessage(Self, sSource, oOptions);
  Finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  End;
  FSource := sSource;
End;

Procedure THL7V2Message.Decode(oSource : TFslBuffer; oOptions : THL7V2DecodingOptions = Nil);
Var
  oDecoder : THL7V2Decoder;
Begin
  Clear;
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(oSource);
  Try
    oDecoder.DecodeMessage(Self, oSource, oOptions);
  Finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  End;
  FSource := oSource.AsBytes;
End;

Procedure THL7V2Message.BindToMSH;
Var
  oMSH : THL7V2Segment;
Begin
  oMSH := Segments[0];
 If Version = hv21 Then
   Begin
   FMessageType := oMSH.Element['9'].AsString;
   FEvent := '';
   End
 Else
   Begin
   FMessageType := oMSH.Element['9.1'].AsString;
   FEvent := oMSH.Element['9.2'].AsString;
   If Version >= hv231 Then
     Begin
     If oMSH.Element['9.3'].AsString <> '' Then
       FStructID := oMSH.Element['9.3'].AsString;
     TryLoadStruct;
     End;
   End;
 FMessageID := oMSH.Element['10'].AsString;
End;

Function THL7V2Message.Clone: THL7V2Message;
Begin
  Result := THL7V2Message(Inherited Clone);
End;

Function THL7V2Message.Link: THL7V2Message;
Begin
  Result := THL7V2Message(Inherited Link);
End;

Procedure THL7V2Message.SetDelimiters(Const Value: THL7V2Delimiters);
Begin
  FDelimiters.Free;
  FDelimiters := Value;
End;

Procedure THL7V2Message.PrepareForEncoding(oOptions: THL7V2EncodingOptions);
Begin
  If (Segments.Count = 0)  Then
    ErrorBadMessage('PrepareForEncoding', 'There is no segments, so the message cannot be encoded');

  If (Segments[0].Code <> 'MSH') Then
    ErrorBadMessage('PrepareForEncoding', 'The first segment is not MSH, so the message cannot be encoded');

  If Version > hv21 Then
    Begin
    If (FStructID = '') And (Not Assigned(oOptions) Or Not oOptions.NoAddStructureName) Then
      TryLoadStruct;
    If (FStructID <> '') And Assigned(Element['MSH-9-3']) And (Element['MSH-9-3'].AsString = '') And (Not Assigned(oOptions) Or Not oOptions.NoAddStructureName) Then
      Element['MSH-9-3'].AsString := FStructID;
    End;
  ResolveVariableTypes;
  If oOptions.Format = hfUnknown Then
    oOptions.Format := Format;
End;

Procedure THL7V2Message.ResolveVariableTypes;
Var
  iLoop : Integer;
  sType : String;
  oSeg : THL7V2Segment;
Begin
  For iLoop := 0 To Segments.Count - 1 Do
    Begin
    oSeg := Segments[iLoop];
    If oSeg.Code = 'OBX' Then
      Begin
      sType := oSeg.Field[2].AsString;
      If sType = '' Then
        sType := 'ST';
      oSeg.Field[5].SpecifyDataType(sType);
      End;
    End;
End;

Function THL7V2Message.Encode(oOptions: THL7V2EncodingOptions): TBytes;
Var
  oLocal : THL7V2EncodingOptions;
  oEncoder : THL7V2Encoder;
Begin
  If Assigned(oOptions) Then
    oLocal := oOptions.Link
  Else
    oLocal := THL7V2EncodingOptions.Create;
  Try
    PrepareForEncoding(oOptions);
    oEncoder := GHL7V2EncoderFactory.ProduceEncoder(oOptions.Format);
    Try
      Result := oEncoder.EncodeMessage(Self, oOptions);
    Finally
      GHL7V2EncoderFactory.ConsumeEncoder(oEncoder);
    End;
  Finally
    oLocal.Free;
  End;
End;

Function THL7V2Message.Encode(aFormat: THL7V2Format; aOptions: THL7V2EncodingOptionSet): TBytes;
Var
  oOptions: THL7V2EncodingOptions;
Begin
  oOptions := THL7V2EncodingOptions.Create(aFormat, aOptions);
  Try
    Result := Encode(oOptions);
  Finally
    oOptions.Free;
  End;
  FSource := Result;
End;

Procedure THL7V2Message.Encode(oBuffer: TFslBuffer; oOptions: THL7V2EncodingOptions);
Var
  oLocal : THL7V2EncodingOptions;
  oEncoder : THL7V2Encoder;
Begin
  If Assigned(oOptions) Then
    oLocal := oOptions.Link
  Else
    oLocal := THL7V2EncodingOptions.Create;
  Try
    PrepareForEncoding(oOptions);
    oEncoder := GHL7V2EncoderFactory.ProduceEncoder(oOptions.Format);
    Try
      oEncoder.EncodeMessage(oBuffer, Self, oOptions);
    Finally
      GHL7V2EncoderFactory.ConsumeEncoder(oEncoder);
    End;
  Finally
    oLocal.Free;
  End;
  FSource := oBuffer.AsBytes;
End;

Procedure THL7V2Message.Assign(oObject: TFslObject);
Begin
  Inherited;
  FDefinition.Free;
  FDefinition  := THL7V2Message(oObject).Definition.Link;
  FSegmentMap.Free;
  FSegmentMap := THL7V2Message(oObject).FSegmentMap.Link;
  Delimiters  := THL7V2Message(oObject).Delimiters.Link;
  FEvent  := THL7V2Message(oObject).FEvent;
  FMessageType  := THL7V2Message(oObject).FMessageType;
  FStructID := THL7V2Message(oObject).FStructID;
  FFormat  := THL7V2Message(oObject).FFormat;
  FMessageID := THL7V2Message(oObject).FMessageID;
End;

Function MakeGroupExist(oGroup : THL7V2ModelSegmentGroup; oExisting : THL7V2SegmentGroup) : THL7V2SegmentGroup;
Begin
  If Assigned(oExisting) Then
    Result := oExisting
  Else
    Begin
    Result := THL7V2SegmentGroup.Create;
    Result.Name := oGroup.Code;
    End;
End;

Procedure THL7V2Message.CheckSegment(oGroup, oChild : THL7V2ModelSegmentGroup; oOptions : THL7V2EncodingOptions; Var iSegment : Integer; Var bFound, bFinished : Boolean; Var oResult : THL7V2SegmentGroup; bOptional : Boolean);
Begin
  If (iSegment < Segments.Count) And Segments[iSegment].Matches(oChild.Code) Then
    Begin
    oResult := MakeGroupExist(oGroup, oResult);
    oResult.AddSegment(Segments[iSegment].Link);
    Inc(iSegment);
    bFound := True;
    If oGroup.GroupType = gtChoice Then
      bFinished := True
    Else If oChild.Repeating Then
      While (iSegment < Segments.Count) And Segments[iSegment].Matches(oChild.Code) Do
        Begin
        oResult.AddSegment(Segments[iSegment].Link);
        Inc(iSegment);
        End
    End
  Else If Not oChild.Optional Then
    Begin
    If oGroup.GroupType = gtChoice Then
      // well, we aren't worried about it yet - we have to check once all the segments have failed.
      // Actually, of course, we shouldn't be here - you can't have non optional segments in a choice group
    Else If Not Assigned(oResult) And oGroup.Optional Then
      // well, we didn't have to find this segment, and the first required segment has not been found.
      // so we conclude that this optional segment is not present and *stop looking*
      bFinished := True
    Else If bOptional Then
      // repeats are always optional
      bFinished := True
    Else If (iSegment < Segments.Count) Then
      ErrorSequence('BuildSegmentGroup/CheckSegment', 'The Segment "' + oChild.Code + '" in the Group "' + oGroup.Code + '" was not found but the segment has already been found, instead "' + Segments[iSegment].Code + '" was found')
    Else If oOptions.AllowMappingToFail Then
      bFinished := True
    Else
      ErrorSequence('BuildSegmentGroup/CheckSegment', 'The Segment "' + oChild.Code + '" in the Group "' + oGroup.Code + '" was not found and the message has no more segments');
    End;
End;

Procedure THL7V2Message.CheckGroup(oGroup, oChild : THL7V2ModelSegmentGroup; oOptions : THL7V2EncodingOptions; Var iSegment : Integer; Var bFound, bFinished : Boolean; Var oResult : THL7V2SegmentGroup; bOptional : Boolean);
Var
  oMap : THL7V2SegmentGroup;
Begin
  oMap := BuildSegmentGroup(oChild, iSegment, oOptions, bOptional Or oChild.Optional);
  If Assigned(oMap) Then
    Begin
    oResult := MakeGroupExist(oGroup, oResult);
    oResult.Children.Add(oMap);
    If oChild.Repeating Then
      Repeat
        oMap := BuildSegmentGroup(oChild, iSegment, oOptions, True);
        If Assigned(oMap) Then
          oResult.Children.Add(oMap);
      Until Not Assigned(oMap);
    End
  Else
    If Not oGroup.Optional And Not oChild.Optional And Not bOptional Then
      ErrorSequence('BuildSegmentGroup', 'The Segment Group "' + oChild.Code + '" in the Group "' + oGroup.Code + '" was not found');
End;

Function THL7V2Message.BuildSegmentGroup(oGroup : THL7V2ModelSegmentGroup; Var iSegment : Integer; oOptions : THL7V2EncodingOptions; bOptional : Boolean) : THL7V2SegmentGroup;
Var
  iLoop : Integer;
  oChild : THL7V2ModelSegmentGroup;
  bFound: Boolean;
  bFinished : Boolean;
Begin
//  writeln('Inspect
  bFound := False;
  bFinished := False;
  Result := Nil;
  Try
    iLoop := 0;
    While Not bFinished And (iLoop < oGroup.Children.Count) Do
      Begin
      oChild := oGroup.Children[iLoop];
      If oChild.GroupType = gtSingle Then
        CheckSegment(oGroup, oChild, oOptions, iSegment, bFound, bFinished, Result, bOptional)
      Else
        CheckGroup(oGroup, oChild, oOptions, iSegment, bFound, bFinished, Result, bOptional);
      Inc(iLoop);
      End;
    If (oGroup.GroupType <> gtChoice) And Assigned(Result) And (oOptions.OptimisticMapping) Then
      // while there's Z segments, add them to this group
      While (iSegment < Segments.Count) And Segments[iSegment].Matches('Z*') Do
        Begin
        Result.AddSegment(Segments[iSegment]);
        Inc(iSegment);
        End;
    If (oGroup.GroupType = gtChoice) And Not bFound And Not oGroup.Optional Then
      ErrorSequence('BuildSegmentGroup', 'No segments found in the The choice Group "' + oGroup.Code);
    Result := Result.Link;
  Finally
    Result.Free;
  End;
End;

Procedure THL7V2Message.SetVersion(aValue  : THL7V2Version);
var
  i : integer;
Begin
  Inherited;
  if Segments.Count > 0 then
    Element['MSH-12'].AsString := NAMES_HL7V2_VERSION[aValue];
  DropDefinition; // not sure whether to DropDefinition before or after inherited
  for i := 0 to Segments.count - 1 do
    Segments[i].MapToDefinitions(Model);
  TryLoadStruct;
End;

Procedure THL7V2Message.BuildSegmentMap(bForXML : Boolean; oOptions: THL7V2EncodingOptions);
Var
  iIndex: Integer;
  oDefinition : THL7V2ModelSegmentGroup;
  oMap : THL7V2SegmentGroup;
Begin
  DropMap;
  oDefinition := Nil;
  If Assigned(FDefinition) Then
    Begin
    If bForXML Then
      Begin
      If Not Assigned(FDefinition.XMLMap) And Assigned(Dictionary) And Assigned(Dictionary.SchemaStore) Then
        FDefinition.XMLMap := Dictionary.SchemaStore.ProduceSchemaMap(Version, FDefinition.Name);
      oDefinition := FDefinition.XMLMap;
      End
    Else If Assigned(FDefinition.SegmentMap) Then
      oDefinition := FDefinition.SegmentMap;

    If Assigned(oDefinition) Then
      Begin
      iIndex := 0;
      oMap := BuildSegmentGroup(oDefinition, iIndex, oOptions, False);
      Try
        If iIndex < Segments.Count Then
          ErrorSequence('BuildXMLSegmentMap', 'The message "' + FMessageID + '" contained some segments not included in the message segment list. The First Segment is "' + Segments[iIndex].Code + '" (#' + IntegerToString(iIndex) + ')');
        FSegmentMap := oMap.Link;
      Finally
        oMap.Free;
      End;
      End;
    End;
End;

Procedure THL7V2Message.UpdateSource;
Begin
  FSource := Encode;
End;

function THL7V2Message.HasDelimiters: Boolean;
begin
  Result := (FDelimiters <> Nil);
end;

procedure THL7V2Message.Clone(oSource: THL7V2Message; aOptions: THL7V2DataElementCloneOptions);
var
  iLoop : integer;
begin
  Clear;
  Version := oSource.Version;
  for iLoop := 0 to oSource.Segments.Count - 1 Do
    AddSegment(oSource.Segments[iLoop].Code).Clone(oSource.Segments[iLoop], aOptions);
  BindToMSH;
end;

Function THL7V2Message.SegmentQuery(query : String):THL7V2SegmentList;
var
  i : integer;
  s : String;
Begin
  result := THL7V2SegmentList.Create;
  Try
    for i := 0 to Segments.Count - 1 Do
    Begin
      s := Segments[i].Code;
      if (s = query) or ((Length(query) = 2) and (query[2] = '*') And (query[1] = s[1])) Or
        (query = '*') Then
        result.Add(Segments[i].Link);
    End;
    result.Link;
  Finally
    result.Free;
  End;
End;

procedure THL7V2Message.SetExceptionCode(const sMessage, sResponseCode, SErrorCode: String);
begin
  Assert(CountSegment('MSA') = 1, 'Attempt to encode an exception in a message but it is not an answer message');

  Segment['MSA', 0].Field[1].AsString := sResponseCode;
  Segment['MSA', 0].Field[3].AsString := sMessage;
  Segment['MSA', 0].Field[6].AsString := sErrorCode;
end;

function THL7V2Message.CanChangeVersion: Boolean;
begin
  result := true;
end;

procedure THL7V2Message.ChangeVersion(ANewVersion: String; ADeleteInvalidContent: Boolean);
var
  i : integer;
begin
  inherited;
  if ADeleteInvalidContent then
    for i := Segments.Count - 1 downto 0 do
      if Segments[i].Definition = nil then
        Segments.DeleteByIndex(i)
      else
        Segments[i].StripUndefinedContent;
end;

function THL7V2Message.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDefinition.sizeInBytes);
  inc(result, FSegmentMap.sizeInBytes);
  inc(result, (FEvent.length * sizeof(char)) + 12);
  inc(result, (FMessageType.length * sizeof(char)) + 12);
  inc(result, (FStructID.length * sizeof(char)) + 12);
  inc(result, (FMessageID.length * sizeof(char)) + 12);
  inc(result, FDelimiters.sizeInBytes);
  inc(result, length(FSource));
end;

Class Function THL7V2Message.HardCodedNack(AVersion, AErrMsg, AMsgid: String): String;
Begin
  If AErrMsg = '' Then
    AErrMsg := 'Total Failure of HL7 Processing';
  If AVersion = '' Then
    AVersion := '2.2';
  Result := 'MSH|^~\&|Health Intersections|HL7engine|||200008251511||ADT^AXX|0|P|' + AVersion + #13 +
    'MSA|AE|' + AMsgId + '|' + AErrMsg + ' (Hardcoded NACK)' + #13;
End;

Function ReadPID(oField : THL7V2DataElement):String;
Var
  iLoop : Integer;
Begin
  If oField.repeatCount <= 1 Then
    Result := oField.AsString
  Else
    Begin
    For iLoop := 0 To oField.RepeatCount - 1 Do
      If oField._Repeat[iLoop].Component[5].AsString = 'MR' Then
        Result := oField._Repeat[iLoop].Component[1].AsString;
    If Result = '' Then
      Result := oField.AsString;
    End;
End;

Function THL7V2Message.GetHL7V2MessagePatientID(SuppressExceptions: Boolean): String;
Var
  seg: THL7V2Segment;
Begin
  Try
    seg := Segment['PID', 0];
    If seg = Nil Then
      Result := '' // No PID so can't ID patient
    Else
      Begin
      Result := ReadPID(seg.Field[3]);
      If Result = '' Then
        Result := ReadPID(seg.Field[2]);
      End;

    seg := Segment['PID', 1];
    If Assigned(seg) Then
      // can't identify patient when multiple PID's are present
      If SuppressExceptions Then
        Result := ''
      Else
        Raise EHL7V2RejectException.Create(Nil, hecSequenceError, 'GetHL7V2MessagePatientID', 'Can''t identify patient when more than one PID is present in the message');
  Except
    On e: Exception Do
      Begin
      Result := '';
      If Not SuppressExceptions Then
        Raise EHL7V2RejectException.Create(Nil, hecUnknownKey, 'GetHL7V2MessagePatientID', 'Exception determining patient #: ' + e.Message);
      End;
    End;
End;

Function StartOfField(InSegment: String; FieldDelim: Char; FieldNum: Integer): Integer;
Var
  CurPos, CurField: Integer;
Begin
  Result := 0;
  //Skip the MSH|^~\&|
  CurPos := 10;
  CurField := 3;
  While CurPos < Length(InSegment) Do
    Begin
    If CurField = FieldNum Then
      Begin
      Result := CurPos;
      Exit;
      End;
    If (InSegment[CurPos] = FieldDelim) Then
      Inc(CurField);
    Inc(CurPos);
    End;
End;

Function NextFieldDelimeter(InSegment: String; FieldDelim: Char; InitialPos: Integer): Integer;

Var
    CurPos: Integer;

Begin
    Result := 0;
    //Skip the MSH|^~\&|
    CurPos := InitialPos;
    While CurPos < Length(InSegment) Do

    Begin
        If InSegment[CurPos] = FieldDelim Then

      Begin
            Result := CurPos;
            Exit;

      End;
        Inc(CurPos);

    End;

End;

Class Function THL7V2Message.HL7PacketReplaceMSHField(FieldNumber: Integer; Field: String; Var HL7Msg: TBytes): Boolean;
Var
  StartPos, EndPos: Integer;
  Buffer: String;
  FieldDelimiter: Char;
  msg : String;
Begin
  msg := BytesAsString(HL7Msg);
  Result := False;
  If FieldNumber < 3 Then
      Exit;
  FieldDelimiter := msg[4];
  Buffer := Copy(msg, 1, Pos(#13, msg) + 1);
  StartPos := StartOfField(Buffer, FieldDelimiter, FieldNumber);
  If StartPos = 0 Then
      Exit;
  EndPos := NextFieldDelimeter(Buffer, FieldDelimiter, StartPos);
  If EndPos = 0 Then
      EndPos := Length(Buffer);
  msg := Copy(msg, 1, StartPos - 1) + Field + Copy(msg, EndPos - 1, Length(msg));
  Result := True;
  HL7Msg := StringAsBytes(msg);
End;

procedure CompareHL7Messages(AMsg1, AMsg2 : THL7V2Message);
var
  SegCount : integer;
  FieldCount : integer;
  CompCount : integer;
  SubCount : integer;
  FSeg1, FSeg2 : THL7V2Segment;
  FF1, FF2 : THL7V2DataElement;
  FC1, FC2 : THL7V2Component;
  FS1, FS2 : THL7V2Component;
begin
  if AMsg1.Segments.AsText <> AMsg2.Segments.AsText then
    begin
    raise EFslException.create('Segment lists differ: '+AMsg1.Segments.AsText+'/'+AMsg2.Segments.AsText);
    end;
  for SegCount := 0 to AMsg1.Segments.count - 1 do
    begin
    FSeg1 := AMsg1.Segments[SegCount];
    FSeg2 := AMsg2.Segments[SegCount];
    if FSeg1.Fields.count <> FSeg2.Fields.count then
      begin
      raise EFslException.create(FSeg1.Code+ ': Field count differs: '+IntegerToString(FSeg1.fields.Count)+'/'+IntegerToString(FSeg2.fields.Count));
      end;
    for FieldCount := 0 to FSeg1.Fields.count - 1 do
      begin
      FF1 := FSeg1.Fields[FieldCount];
      FF2 := FSeg2.Fields[FieldCount];
      if FF1.Components.count <> FF2.Components.count then
        begin
        raise EFslException.create(FSeg1.Code+'-'+IntegerToString(FieldCount+1)+ ': Component count differs: '+IntegerToString(FF1.Components.Count)+'/'+IntegerToString(FF2.Components.Count));
        end;
      if (FF1.Components.count = 0) and (FF1.RawContent <> FF2.RawContent) then
        begin
        raise EFslException.create(FSeg1.Code+'-'+IntegerToString(FieldCount+1)+ ': Content differs: '+FF1.RawContent+'/'+FF2.RawContent);
        end;
      for CompCount := 0 to FF1.Components.count - 1 do
        begin
        FC1 := FF1.Components[CompCount];
        FC2 := FF2.Components[CompCount];
        if FC1.Components.Count <> FC2.Components.Count then
          begin
          raise EFslException.create(FSeg1.Code+'-'+IntegerToString(FieldCount+1)+'.'+IntegerToString(CompCount+1)+ ': Component count differs: '+IntegerToString(FF1.Components.Count)+'/'+IntegerToString(FF2.Components.Count));
          end;
        if (FC1.Components.Count = 0) and (FC1.RawContent <> FC2.RawContent) then
          begin
          raise EFslException.create(FSeg1.Code+'-'+IntegerToString(FieldCount+1)+'.'+IntegerToString(CompCount+1)+ ': Content differs: '+FC1.RawContent+'/'+FC2.RawContent);
          end;
        for SubCount := 0 to FC1.Components.Count - 1 do
          begin
          FS1 := FC1.Components[SubCount];
          FS2 := FC2.Components[SubCount];
          if FS1.RawContent <> FS2.RawContent then
            begin
            raise EFslException.create(FSeg1.Code+'-'+IntegerToString(FieldCount+1)+'.'+IntegerToString(CompCount+1)+'.'+IntegerToString(SubCount+1)+ ': Content differs: '+FS1.RawContent+'/'+FS2.RawContent);
            end;
          end;
        end;
      end;
    end;
end;

class procedure THL7V2Message.CheckHL7MessagesEquivalent(AMsg1, AMsg2: TBytes; const ALeafNodesThatCanDiffer: array of String);
  function isDifferent(s1, s2: String; var VReason: String): Boolean;
  var
    i: Integer;
    begin
    Result := False;
    i := 0;
    repeat
      inc(i);
      if i <= length(s1) then
        begin
        if s1[i] <> s2[i] then
          begin
          Result := True;
          VReason := 'Strings differ as from character ' + IntegerToString(i)+' ['+copy(s1, i-4,10)+']/['+copy(s2, i-4, 10)+']';
          end;
        end;
    until (Result) or (i > length(s1));
    if not result and (length(s1) <> length(s2)) then
      begin
      Result := True;
      VReason := 'Different Lengths ' + IntegerToString(length(s1)) + ' and ' + IntegerToString(length(s2));
      end
    end;
var
  LMsg1: THL7V2Message;
  LMsg2: THL7V2Message;
  i, j: Integer;
  LReason: String;
  ts: TStringList;
begin
  LMsg1 := THL7V2Message.Create;
  try
    LMsg2 := THL7V2Message.Create;
    try
      LMsg1.Decode(AMsg1);
      LMsg2.Decode(AMsg2);
      for i := Low(ALeafNodesThatCanDiffer) to High(ALeafNodesThatCanDiffer) do
        begin
        ts := TStringList.Create;
        try
          ts.CommaText := ALeafNodesThatCanDiffer[i];
          for j := 0 to ts.Count - 1 do
            begin
            if assigned(LMsg1[ts[j]]) then
              begin
              if assigned(LMsg2[ts[j]]) then
                begin
                LMsg1[ts[j]].AsString := LMsg2[ts[j]].AsString
                end
              else
                begin
                LMsg1[ts[j]].AsString := '';
                end;
              end;
            end;
        finally
          ts.Free;
          end;
        end;
      CompareHL7Messages(LMsg1, LMsg2);
      if isDifferent(BytesAsString(LMsg1.EnCode), BytesAsString(LMsg2.EnCode), LReason) then
        begin
        raise EFslException.Create('Messages are different - ' + LReason);
        end;
    finally
      LMsg2.Free;
      end;
  finally
    LMsg1.Free;
    end;
end;

Function THL7V2Message.AddNOKToMsg(AName, AGiven, ATitle, AAddress1, AAddress2, AAddress3, AAddress4, APostCode, AHomephone, AWorkphone, ARelationship : String) : THL7V2Segment;
Var
  LNokCount : Integer;
  LNok : THL7V2Segment;
  i : Integer;
  LIndex : Integer;
Begin
  LNokCount := CountSegment('NK1');
  LNok := Nil;
  For i := 0 To LNokCount -1 Do
    Begin
    LNok := Segment['NK1', i];
    If (LNok.Element['2-1'].AsString = '') Then
      Break
    Else
      LNok := Nil;
    End;
  If LNok = Nil Then
    Begin
    If LNokCount = 0 Then
      Begin
      LIndex := Segments.IndexByCode('PID');
      If LIndex = 0 Then
        LNok := AddSegment('NK1')
      Else
        LNok := InsertSegment(LIndex + 1, 'NK1');
      End
    Else
      Begin
      LIndex := -1;
      For i := 0 To Segments.count - 1 Do
        If Segments[i].Code = 'NK1' Then
          LIndex := i;
      Assert(LIndex <> -1, 'HL7_Utilities.AddNOKToMsg: Cannot find Nok though NOKCount > 0');
      LNok := InsertSegment(LIndex + 1, 'NK1');
      End;
    LNok.Element['1'].AsInteger := LNokCount + 1;
    End;
  LNok.Element['2-1'].AsString := AName;
  If AGiven <> '' Then
    LNok.Element['2-2'].AsString := AGiven;
  If ATitle <> '' Then
    LNok.Element['2-5'].AsString := ATitle;
  If AAddress1 <> '' Then
    LNok.Element['4-1'].AsString := AAddress1;
  If AAddress2 <> '' Then
    LNok.Element['4-2'].AsString := AAddress2;
  If AAddress3 <> '' Then
    LNok.Element['4-3'].AsString := AAddress3;
  If AAddress4 <> '' Then
    LNok.Element['4-4'].AsString := AAddress4;
  If APostCode <> '' Then
    LNok.Element['4-5'].AsString := APostCode;
  If AHomephone <> '' Then
    LNok.Element['5'].AsString := AHomephone;
  If AWorkphone <> '' Then
    LNok.Element['6'].AsString := AWorkphone;
  If ARelationship <> '' Then
    LNok.Element['3'].AsString := ARelationship;
  Result := LNok;
End;

{ THL7V2BatchMessage }

destructor THL7V2BatchMessage.Destroy;
begin
  FMessage.Free;
  inherited;
end;

function THL7V2BatchMessage.Clone: THL7V2BatchMessage;
begin
  result := THL7V2BatchMessage(inherited Clone);
end;

function THL7V2BatchMessage.Link: THL7V2BatchMessage;
begin
  result := THL7V2BatchMessage(inherited Link);
end;

function THL7V2BatchMessage.FullPathName: String;
begin
  result := Parent.FullPathName+'-'+IntegerToString(IndexInSiblings);
end;

function THL7V2BatchMessage.IsElement(const sName: String): Boolean;
begin
  result := Parent.IsElement(sName);
end;

procedure THL7V2BatchMessage.SetContent(const sValue: TBytes);
begin
  FMessage.Free;
  FMessage := nil;
  FContent := sValue;
end;

procedure THL7V2BatchMessage.SetMessage(const oValue: THL7V2Message);
begin
  FMessage.Free;
  FMessage := oValue;
  FContent := oValue.Encode;
end;

procedure THL7V2BatchMessage.Decode(oOptions: THL7V2DecodingOptions);
begin
  if assigned(FMessage) then
    begin
    FMessage.Free;
    FMessage := nil;
    end;
  FMessage := THL7V2Message.create;
  FMessage.Decode(FContent, oOptions);
end;

function THL7V2BatchMessage.ForceMessage: THL7V2Message;
begin
  if (FMessage = Nil) Then
    Decode(nil);
  Result := FMessage;
end;

function THL7V2BatchMessage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, length(FContent));
  inc(result, FMessage.sizeInBytes);
  inc(result, (FId.length * sizeof(char)) + 12);
end;

{ THL7V2BatchMessages }

function THL7V2BatchMessages.GetBatchMessage(iIndex: Integer): THL7V2BatchMessage;
begin
  result := THL7V2BatchMessage(objectByIndex[iIndex]);
end;

function THL7V2BatchMessages.ItemClass: TFslObjectClass;
begin
  result := THL7V2BatchMessage;
end;

{ THL7V2Batch }

constructor THL7V2Batch.Create;
begin
  inherited;
  FDelimiters := THL7V2Delimiters.create;
end;

destructor THL7V2Batch.Destroy;
begin
  FHeader.Free;
  FTrailer.Free;
  FDelimiters.Free;
  inherited;
end;

function THL7V2Batch.ChildrenClass: TFslTreeListClass;
begin
  result := THL7V2BatchMessages;
end;

function THL7V2Batch.Clone: THL7V2Batch;
begin
  result := THL7V2Batch(Inherited Clone);
end;

function THL7V2Batch.Link: THL7V2Batch;
begin
  result := THL7V2Batch(Inherited Link);
end;

function THL7V2Batch.FullPathName: String;
begin
  if HasParent Then
    result := Parent.FullPathName+'Batch:'+IntegerToString(IndexInSiblings)
  Else
    Result := 'Batch:'+IntegerToString(IndexInSiblings);
end;

function THL7V2Batch.IsElement(const sName: String): Boolean;
begin
  result := False; // not sure what to do with this?
end;

function THL7V2Batch.GetChildren: THL7V2BatchMessages;
begin
  result := THL7V2BatchMessages(inherited Children);
end;

procedure THL7V2Batch.SetChildren(const Value: THL7V2BatchMessages);
begin
  inherited Children := Value;
end;

procedure THL7V2Batch.SetDelimiters(const Value: THL7V2Delimiters);
begin
  FDelimiters.Free;
  FDelimiters := THL7V2Delimiters.create;
end;

procedure THL7V2Batch.SetVersion(aValue: THL7V2Version);
begin
  inherited;

  FHeader := THL7V2Segment.create;
  FHeader.Parent := self;
  FHeader.Code := 'BHS';
  FHeader.BuildFields;

  FTrailer := THL7V2Segment.create;
  FTrailer.Parent := self;
  FTrailer.Code := 'BTS';
  FTrailer.BuildFields;
end;

function THL7V2Batch.GetReplyBatchID: String;
begin
  Assert(invariants('GetReplyBatchID', FHeader, THL7V2Segment, 'Header'));
  result := FHeader.Field[12].AsString
end;

function THL7V2Batch.GetBatchID: String;
begin
  Assert(invariants('GetBatchID', FHeader, THL7V2Segment, 'Header'));
  result := FHeader.Field[11].AsString
end;

procedure THL7V2Batch.SetBatchID(const Value: String);
begin
  Assert(invariants('SetBatchID', FHeader, THL7V2Segment, 'Header'));
  FHeader.Field[11].AsString := Value;
end;

procedure THL7V2Batch.SetReplyBatchID(const Value: String);
begin
  Assert(invariants('SetReplyBatchID', FHeader, THL7V2Segment, 'Header'));
  FHeader.Field[12].AsString := Value;
end;

procedure THL7V2Batch.Clear;
begin
  Children.Clear;
  if assigned(FHeader) then
    FHeader.Free;
  if assigned(FTrailer) then
    FTrailer.Free;
  FHeader := nil;
  FTrailer := nil;
  inherited;
end;

function THL7V2Batch.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDelimiters.sizeInBytes);
  inc(result, FHeader.sizeInBytes);
  inc(result, FTrailer.sizeInBytes);
end;

{ THL7V2Batches }

function THL7V2Batches.GetBatch(iIndex: Integer): THL7V2Batch;
begin
  result := THL7V2Batch(ObjectByIndex[iIndex]);
end;

function THL7V2Batches.ItemClass: TFslObjectClass;
begin
  result := THL7V2Batch;
end;

{ THL7V2File }

Constructor THL7V2File.Create;
Begin
  Inherited;
  FDelimiters := THL7V2Delimiters.Create;
End;

Destructor THL7V2File.Destroy;
Begin
  FHeader.Free;
  FTrailer.Free;
  FDelimiters.Free;
  Inherited;
End;

Function THL7V2File.ChildrenClass: TFslTreeListClass;
Begin
  Result := THL7V2Batches;
End;

Function THL7V2File.Clone: THL7V2File;
Begin
  Result := THL7V2File(Inherited Clone);
End;

Function THL7V2File.Link: THL7V2File;
Begin
  Result := THL7V2File(Inherited Link);
End;

Function THL7V2File.FullPathName: String;
Begin
  Result := 'File';
End;

Function THL7V2File.IsElement(Const sName: String): Boolean;
Begin
  Result := sName=''; // not sure what to do with this?
End;

Function THL7V2File.GetChildren: THL7V2Batches;
Begin
  Result := THL7V2Batches(Inherited Children);
End;

Procedure THL7V2File.SetChildren(Const Value: THL7V2Batches);
Begin
  Inherited Children := Value;
End;

Procedure THL7V2File.SetDelimiters(Const Value: THL7V2Delimiters);
Begin
  FDelimiters.Free;
  FDelimiters := THL7V2Delimiters.Create;
End;

Procedure THL7V2File.SetVersion(aValue: THL7V2Version);
Begin
  Inherited;

  FHeader := THL7V2Segment.Create;
  FHeader.Parent := Self;
  FHeader.Code := 'FHS';
  FHeader.BuildFields;

  FTrailer := THL7V2Segment.Create;
  FTrailer.Parent := Self;
  FTrailer.Code := 'FTS';
  FTrailer.BuildFields;
End;

Function THL7V2File.GetReplyFileID: String;
Begin
  Assert(Invariants('GetReplyFileID', FHeader, THL7V2Segment, 'Header'));
  Result := FHeader.Field[12].AsString
End;

Function THL7V2File.GetFileID: String;
Begin
  Assert(Invariants('GetFileID', FHeader, THL7V2Segment, 'Header'));
  Result := FHeader.Field[11].AsString
End;

Procedure THL7V2File.SetFileID(Const Value: String);
Begin
  Assert(Invariants('SetFileID', FHeader, THL7V2Segment, 'Header'));
  FHeader.Field[11].AsString := Value;
End;

Procedure THL7V2File.SetReplyFileID(Const Value: String);
Begin
  Assert(Invariants('SetReplyFileID', FHeader, THL7V2Segment, 'Header'));
  FHeader.Field[12].AsString := Value;
End;

Procedure THL7V2File.Clear;
Begin
  Children.Clear;
  If Assigned(FHeader) Then
    FHeader.Free;
  If Assigned(FTrailer) Then
    FTrailer.Free;
  FHeader := Nil;
  FTrailer := Nil;
  Inherited;
End;

Function THL7V2File.MessageTotal: Integer;
Var
  iLoop : Integer;
Begin
  Result := 0;
  For iLoop := 0 To Batches.count - 1 Do
    Inc(Result, Batches[iLoop].Messages.count);
End;

Procedure THL7V2File.Load(Const oStream: TFslStream; oOptions : THL7V2DecodingOptions = nil);
Var
  oDecoder : THL7V2Decoder;
Begin
  oDecoder := GHL7V2DecoderFactory.ProduceDecoder(hfER7);
  Try
    oDecoder.DecodeFile(Self, oStream, oOptions);
  Finally
    GHL7V2DecoderFactory.ConsumeDecoder(oDecoder);
  End;
End;

Procedure THL7V2File.Load(Const sFileName: String; oOptions : THL7V2DecodingOptions = nil);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFileName, fmOpenRead + fmShareDenyWrite);
  Try
    Load(oFile, oOptions);
  Finally
    oFile.Free;
  End;
End;

Procedure THL7V2File.Save(Const oStream: TFslStream; oOptions : THL7V2EncodingOptions = nil);
Var
  oEncoder : THL7V2Encoder;
  iLoop : Integer;
Begin
  Trailer.Field[1].AsInteger := Batches.Count;
  For iLoop :=  0 To Batches.Count - 1 Do
    Batches[iLoop].Trailer.Field[1].AsInteger := Batches[iLoop].Messages.Count;

  oEncoder := GHL7V2EncoderFactory.ProduceEncoder(hfER7);
  Try
    oEncoder.EncodeFile(Self, oStream, oOptions);
  Finally
    GHL7V2EncoderFactory.ConsumeEncoder(oEncoder);
  End;
End;

Procedure THL7V2File.Save(Const sFileName: String; oOptions : THL7V2EncodingOptions = nil);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFileName, fmCreate);
  Try
    Save(oFile, oOptions);
  Finally
    oFile.Free;
  End;
End;

procedure THL7V2File.Load(const bytes: TBytes; oOptions: THL7V2DecodingOptions);
var
  bs : TBytesStream;
  vcl : TFslVCLStream;
begin
  bs := TBytesStream.create(bytes);
  try
    vcl := TFslVCLStream.create;
    try
      vcl.Stream := bs;
      Load(vcl);
    finally
      vcl.free;
    end;
  finally
    bs.free;
  end;
end;

procedure THL7V2File.Save(var bytes: TBytes; oOptions: THL7V2EncodingOptions);
var
  bs : TBytesStream;
  vcl : TFslVCLStream;
begin
  bs := TBytesStream.create;
  try
    vcl := TFslVCLStream.create;
    try
      vcl.Stream := bs;
      Save(vcl);
    finally
      vcl.free;
    end;
    bytes := bs.Bytes;
  finally
    bs.free;
  end;

end;

function THL7V2File.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDelimiters.sizeInBytes);
  inc(result, FHeader.sizeInBytes);
  inc(result, FTrailer.sizeInBytes);
end;

{ THL7V2BatchIterator }

destructor THL7V2BatchIterator.Destroy;
begin
  FBatch.Free;
  inherited;
end;

procedure THL7V2BatchIterator.SetBatch(const Value: THL7V2File);
begin
  FBatch.Free;
  FBatch := Value;
end;

function THL7V2BatchIterator.Current: THL7V2BatchMessage;
begin
  assert(invariants('Current', FCurrent, THL7V2BatchMessage, 'Current'));
  result := FCurrent;
end;

procedure THL7V2BatchIterator.First;
begin
  assert(invariants('First', FBatch, THL7V2File, 'Batch'));
  FBatchIndex := 0;
  FMessageIndex := -1;
  Next;
end;

function THL7V2BatchIterator.GetPosition: integer;
var
  iLoop : integer;
begin
  assert(invariants('Index', FBatch, THL7V2File, 'Batch'));
  result := FMessageIndex;
  for iLoop := 0 to FBatchIndex - 1 do
    inc(result, FBatch.Batches[iLoop].Messages.count);
end;

procedure THL7V2BatchIterator.SetPosition(iValue: integer);
var
  iLoop : integer;
begin
  assert(invariants('Index', FBatch, THL7V2File, 'Batch'));
  iLoop := 0;
  while (iLoop < FBatch.Batches.Count) and (iValue >= FBatch.Batches[iLoop].Messages.Count) do
    begin
    dec(iValue, FBatch.Batches[iLoop].Messages.Count);
    inc(iLoop);
    end;

  if iLoop < FBatch.Batches.Count then
    begin
    FBatchIndex := iLoop;
    FMessageIndex := IntegerMax(iValue, 0);
    if FBatch.Batches[FBatchIndex].Messages.ExistsByIndex(FMessageIndex) then
      FCurrent := FBatch.Batches[FBatchIndex].Messages[FMessageIndex]
    else
      FCurrent := nil;
    end
  else
    begin
    FBatchIndex := Batch.Batches.Count;
    FMessageIndex := 0;
    FCurrent := nil;
    end;
end;

procedure THL7V2BatchIterator.Last;
begin
  assert(invariants('Last', FBatch, THL7V2File, 'Batch'));
  FBatchIndex := Batch.Batches.Count-1;
  if Batch.Batches.ExistsByIndex(FBatchIndex) Then
    FMessageIndex := Batch.Batches[FBatchIndex].Messages.Count
  Else
    FMessageIndex := 0;
  Previous;
end;

function THL7V2BatchIterator.More: Boolean;
begin
  assert(invariants('More', FBatch, THL7V2File, 'Batch'));
  result := assigned(FCurrent);
end;

procedure THL7V2BatchIterator.Reset;
begin
  assert(invariants('Reset', FBatch, THL7V2File, 'Batch'));
  First;
end;

procedure THL7V2BatchIterator.Next;
var
  oBatch : THL7V2Batch;
begin
  assert(invariants('Next', FBatch, THL7V2File, 'Batch'));
  FCurrent := nil;
  if Batch.Batches.ExistsByIndex(FBatchIndex) then
    begin
    inc(FMessageIndex);
    oBatch := Batch.Batches[FBatchIndex];
    if FMessageIndex < oBatch.Messages.Count then
      FCurrent := oBatch.Messages[FMessageIndex]
    else
      begin
      repeat
        inc(FBatchIndex);
      until (FBatchIndex >= Batch.Batches.Count) or (Batch.Batches[FBatchIndex].Messages.Count > 0);
      FMessageIndex := 0;
      if FBatchIndex < Batch.Batches.Count then
        FCurrent := Batch.Batches[FBatchIndex].Messages[0]
      else
        begin
        FBatchIndex := Batch.Batches.Count;
        FMessageIndex := 0;
        end;
      end;
    end;
end;

procedure THL7V2BatchIterator.Previous;
var
  oBatch : THL7V2Batch;
begin
  assert(invariants('Previous', FBatch, THL7V2File, 'Batch'));
  FCurrent := nil;
  if Batch.Batches.ExistsByIndex(FBatchIndex) then
    begin
    oBatch := Batch.Batches[FBatchIndex];
    Dec(FMessageIndex);
    if FMessageIndex >= 0 then
      FCurrent := oBatch.Messages[FMessageIndex]
    else
      begin
      repeat
        dec(FBatchIndex);
      until (FBatchIndex < 0) or (Batch.Batches[FBatchIndex].Messages.Count > 0);
      if (FBatchIndex > 0) And (FBatchIndex < Batch.Batches.Count) then
        begin
        FMessageIndex := Batch.Batches[FBatchIndex].Messages.Count-1;
        FCurrent := Batch.Batches[FBatchIndex].Messages[0];
        end
      else
        begin
        FBatchIndex := 0;
        FMessageIndex := -1;
        end;
      end;
    end;
end;

function THL7V2BatchIterator.CurrentBatch: THL7V2Batch;
begin
  result := FBatch.Batches[FBatchIndex];
end;

Procedure THL7V2BatchIterator.SetCurrent(oBatch: THL7V2BatchMessage);
Var
  iIndex : Integer;
  iIndex2 : Integer;

Begin
  For iIndex := 0 to FBatch.Batches.Count - 1 Do
  Begin
    iIndex2 := FBatch.Batches[iIndex].Messages.IndexByReference(oBatch);
    If (iIndex2 > -1) Then
    Begin
      FCurrent := oBatch;
      FBatchIndex := iIndex;
      FMessageIndex := iIndex2;
    End;
  End;
End;

function THL7V2BatchIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBatch.sizeInBytes);
  inc(result, FCurrent.sizeInBytes);
end;

{ THL7V2ParsedQueryRange }

function THL7V2ParsedQueryRange.Max(AListCount: Integer): Integer;
var
  i : integer;
begin
  result := FList[0].High;
  for i := 1 to High(FList) do
    if FList[i].High > result then
      result := FList[i].High;
  if result > AListCount then
    result := AListCount;
end;

function THL7V2ParsedQueryRange.Min: Integer;
var
  i : integer;
begin
  result := FList[0].Low;
  for i := 1 to High(FList) do
    if FList[i].Low < result then
      result := FList[i].Low;
end;

procedure THL7V2ParsedQueryRange.Parse(var VQuery: String);
  procedure AddRange(ALow, AHigh : integer);
  begin
    SetLength(FList, length(FList)+1);
    FList[High(FList)].Low := ALow;
    FList[High(FList)].High := AHigh;
  end;
var
  s, l, r : string;
begin
  if VQuery[1] = '[' then
    begin
    delete(VQuery, 1, 1);
    StringSplit(VQuery, ']', s, VQuery);
    if s = '' then
      ErrorApplication('Parse', 'Range is empty');
    // s = comma delimited
    while s <> '' do
      begin
      StringSplit(s, ',', l, s);
      // l = 3+ or 1..2
      if l[length(l)] = '+' then
        begin
        delete(l, Length(l), 1);
        if not StringIsInteger32(l) then
          ErrorApplication('Parse', 'Range "'+l+'+" is not valid');
        AddRange(StringToInteger32(l), MAXINT);
        end
      else
        begin
        StringSplit(l, '..', l, r);
        if not StringIsInteger32(l) then
          ErrorApplication('Parse', 'Range begin "'+l+'" is not valid');
        if not StringIsInteger32(r) then
          ErrorApplication('Parse', 'Range end "'+r+'" is not valid');
        AddRange(StringToInteger32(l), StringToInteger32(r));
        end;
      end;
    end
  else
    begin
    while (VQuery <> '') and CharInSet(VQuery[1], ['0'..'9']) do
      begin
      s := s + VQuery[1];
      delete(VQuery, 1, 1);
      end;
    if s = '' then
      ErrorApplication('Parse', 'Found '+VQuery+' looking for a number');
    SetLength(FList, 1);
    FList[0].Low := StringToInteger32(s);
    FList[0].High := FList[0].Low;
    end;
end;

function THL7V2ParsedQueryRange.Selected(AValue : Integer): Boolean;
var
  i : integer;
begin
  result := false;
  for i := Low(FList) to High(FList) do
    if (FList[i].Low <= AValue) and (FList[i].High >= AValue) then
      begin
      result := true;
      exit;
      end;
end;

{ THL7V2ParsedQuery }

destructor THL7V2ParsedQuery.destroy;
begin
  FSegIndex.Free;
  FDE.Free;
  FComp.Free;
  FSubComp.Free;
  FRep.Free;
  inherited;
end;

procedure THL7V2ParsedQuery.SelectSubComponents(AComp : THL7V2Component; AResults: THL7V2BaseObjectList);
var
  i: Integer;
begin
  for i := FSubComp.Min to FSubComp.Max(AComp.Components.count) do
    if FSubComp.selected(i) and (not FHasSubCompRegEx or FSubCompRegEx.IsMatch(AComp.Components[i-1].RawContent)) then
      AResults.Add(AComp.Components[i-1].Link);
end;

procedure THL7V2ParsedQuery.SelectComponents(AField : THL7V2DataElement; AResults: THL7V2BaseObjectList);
var
  i: Integer;
begin
  for i := FComp.Min to FComp.Max(AField.Components.count) do
    if FComp.selected(i) and (not FHasCompRegEx or FCompRegEx.IsMatch(AField.Components[i-1].RawContent)) then
      if assigned(FSubComp) then
        SelectSubComponents(AField.Components[i-1], AResults)
      else
        AResults.Add(AField.Components[i-1].Link);
end;

procedure THL7V2ParsedQuery.SelectDataElements(ASegment : THL7V2Segment; AResults: THL7V2BaseObjectList);
var
  i, j: Integer;
  LDe : THL7V2DataElement;
begin
  for i := FDe.Min to FDE.Max(ASegment.Fields.count) do
    if FDE.selected(i) then
      begin
      LDe := ASegment.Fields[i-1];
      for j := 0 to LDe.RepeatCount - 1 do
        begin
        if (not assigned(FRep) or FRep.Selected(j)) and
           (not FHasDERegEx or FDERegEx.IsMatch(LDe._Repeat[j].RawContent)) then
         if assigned(FComp) then
           SelectComponents(LDe._Repeat[j], AResults)
         else
           AResults.Add(LDe._Repeat[j].Link);
        end;
      end;
end;

procedure THL7V2ParsedQuery.SelectSegments(ASegList: THL7V2Segments; AResults: THL7V2BaseObjectList);
var
  i : integer;
  LMatch : Boolean;
begin
  for i := 0 to ASegList.Count - 1 do
    begin
    if FHasSegReg then
      LMatch := FSegReg.IsMatch(ASegList[i].Code)
    else
      LMatch := SegMatches(ASegList[i].Code);
    if LMatch and (not assigned(FSegIndex) or FSegIndex.Selected(ASegList.IndexInType(ASegList[i]))) then
      if assigned(FDE) then
        SelectDataElements(ASegList[i], AResults)
      else
        AResults.add(ASegList[i].Link);
    end;
end;

procedure THL7V2ParsedQuery.parse(AQuery: String);
var
  s : String;
begin
  //  range = number or [range]
  //
  //  3 letter code or ^*&
  //  optional range
  //  -
  //  range with optional (regexp)
  //  - or .
  //  range with optional (regexp)
  //  - or .
  //  range with optional (regexp)
  if AQuery[1] = ^ then
    begin
    StringSplit(AQuery, '$', s, AQuery);
    FSegReg := TRegEx.create(s + '$');
    FHasSegReg := true;
    end
  else
     begin
     FSegQuery := copy(AQuery, 1, 3);
     delete(AQuery, 1, 3);
     end;
  if AQuery <> '' then
    begin
    if AQuery[1] <> '-' then
      begin
      FSegIndex := THL7V2ParsedQueryRange.Create;
      FSegIndex.Parse(AQuery);
      end;
    if AQuery <> '' then
      begin
      if AQuery[1] <> '-' then
        ErrorApplication('Parse', 'Invalid Query. Expecting "-", but found "'+AQuery[1]+'"');
      delete(AQuery, 1, 1);
      ParseDE(AQuery);
      end;
    end;
end;

procedure THL7V2ParsedQuery.parseDE(var VQuery: String);
var
  s : String;
begin
  FDE := THL7V2ParsedQueryRange.create;
  FDE.Parse(VQuery);
  if VQuery <> '' then
    begin
    if VQuery[1] = '(' then
      begin
      StringSplit(VQuery, ')', s, VQuery);
      delete(s, 1, 1);
      FDERegEx := TRegEx.create(s);
      FHasDERegEx := true;
      end;
    if VQuery <> '' then
      begin
      if VQuery[1] = ':' then
        begin
        delete(VQuery, 1, 1);
        FRep := THL7V2ParsedQueryRange.create;
        FRep.Parse(VQuery);
        end;
      if VQuery <> '' then
        begin
        if not CharInSet(VQuery[1], ['-', '.']) then
          ErrorApplication('ParseDE', 'Invalid Query. Expecting "-" or ".", but found "'+VQuery[1]+'"');
        delete(VQuery, 1, 1);
        ParseComp(VQuery);
        end;
      end;
    end;
end;

procedure THL7V2ParsedQuery.parseComp(var VQuery: String);
var
  s : String;
begin
  FComp := THL7V2ParsedQueryRange.create;
  FComp.Parse(VQuery);
  if VQuery <> '' then
    begin
    if VQuery[1] = '(' then
      begin
      StringSplit(VQuery, ')', s, VQuery);
      delete(s, 1, 1);
      FCompRegEx := TRegEx.create(s);
      FHasCompRegEx := true;
      end;
    if VQuery <> '' then
      begin
      if not CharInSet(VQuery[1], ['-', '.']) then
        ErrorApplication('ParseComp', 'Invalid Query. Expecting "-" or ".", but found "'+VQuery[1]+'"');
      delete(VQuery, 1, 1);
      ParseSubComp(VQuery);
      end;
    end;
end;

procedure THL7V2ParsedQuery.parseSubComp(var VQuery: String);
var
  s : String;
begin
  FSubComp := THL7V2ParsedQueryRange.create;
  FSubComp.Parse(VQuery);
  if VQuery <> '' then
    begin
    if VQuery[1] = '(' then
      begin
      StringSplit(VQuery, ')', s, VQuery);
      delete(s, 1, 1);
      FSubCompRegEx := TRegEx.create(s);
      FHasSubCompRegEx := true;
      end;
    if VQuery <> '' then
      begin
      ErrorApplication('ParseSubComp', 'Invalid Query - not at end of string processing subcomponent criteria');
      end;
    end;
end;

function THL7V2ParsedQuery.SegMatches(ACode: String): Boolean;
begin
  result := (length(ACode) = 3) and (length(FSegQuery) = 3) and
             ((FSegQuery[1] = '#') or (FSegQuery[1] = Upcase(ACode[1]))) and
             ((FSegQuery[2] = '#') or (FSegQuery[2] = Upcase(ACode[2]))) and
             ((FSegQuery[3] = '#') or (FSegQuery[3] = Upcase(ACode[3])))
end;

function THL7V2ParsedQuery.ResultType: THL7V2BaseObjectClass;
begin
  if assigned(FDE) then
    if assigned(FComp) then
      result := THL7V2Component
    else
      result := THL7V2DataElement
  else
    result := THL7V2Segment;
end;

function THL7V2ParsedQuery.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSegQuery.length * sizeof(char)) + 12);
  inc(result, FSegIndex.sizeInBytes);
  inc(result, FDE.sizeInBytes);
  inc(result, FRep.sizeInBytes);
  inc(result, FComp.sizeInBytes);
  inc(result, FSubComp.sizeInBytes);
end;

{ THL7V2QueryParser }

constructor THL7V2QueryParser.Create(sQuery: String);
begin
  inherited create;
  FContent := sQuery;
  FCursor := 1;
end;

procedure THL7V2QueryParser.Check(bCondition: Boolean; sMethod, sMessage: String);
begin
  if not bCondition then
    ErrorHL7Library(sMethod, sMessage);
end;

function THL7V2QueryParser.Finished: Boolean;
begin
  result := FCursor > length(FContent);
end;

procedure THL7V2QueryParser.NextNonWhitespace;
begin
  while not Finished and (Peek = '') do
    Consume;
  Check(not Finished, 'NextNonWhitespace', 'Unexpected end of Query');
end;

function THL7V2QueryParser.Peek: String;
var
  iCursor : integer;
begin
  if Finished then
    result := ''
  else
    begin
    iCursor := FCursor;
    result := Consume;
    FCursor := iCursor;
    end;
end;

function THL7V2QueryParser.Consume: String;
begin
  Check(not Finished, 'Consume', 'Unexpected end of Query');
  case FContent[FCursor] of
    ' ', #9, #13, #10:
      begin
      while not Finished and CharInSet(FContent[FCursor], [' ', #9, #13, #10]) do
        inc(FCursor);
      result := '';
      end;
    '"':
      begin
      inc(FCursor);
      result := '';
      while not Finished and not ((FContent[FCursor] = '"') and ((FCursor = length(FContent)) or (FContent[FCursor+1] <> '"'))) do
        begin
        if not ((FContent[FCursor] = '"') and ((FCursor = length(FContent)) or (FContent[FCursor+1] <> '"'))) then
          result := result + FContent[FCursor];
        inc(FCursor);
        end;
      inc(FCursor); // for final "
      result := '"'+result +'"';
      end;
    '0'..'9':
      begin
      result := '';
      while not Finished and CharInSet(FContent[FCursor], ['0'..'9']) do
        begin
        result := result + FContent[FCursor];
        inc(FCursor);
        end;
      end;
    'A'..'Z','a'..'z', '_':
      begin
      result := '';
      while not Finished and CharInSet(FContent[FCursor], ['A'..'Z','a'..'z', '_', '0'..'9']) do
        begin
        result := result + FContent[FCursor];
        inc(FCursor);
        end;
      end;
    '[', '(', ']', ')', '-', '&', '@', ':', '.' :
      begin
      result := FContent[FCursor];
      inc(FCursor);
      end;
    '=', '+', '!', '~', '>', '<', '|'  :
      begin
      result := FContent[FCursor];
      inc(FCursor);
      if not Finished and CharInSet(FContent[FCursor], ['=', '+', '!', '~', '>', '<', '|']) then
        begin
        result := result + FContent[FCursor];
        inc(FCursor);
        end;
      end;
  else
    ErrorApplication('Consume', 'Unexpected Token '+FContent[FCursor]);
  end;
end;

function THL7V2QueryParser.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FContent.length * sizeof(char)) + 12);
end;

{ THL7V2QueryNode }

destructor THL7V2QueryNode.Destroy;
begin
  FItem.Free;
  inherited;
end;

function THL7V2QueryNode.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FItem.sizeInBytes);
end;

{ THL7V2QueryItem }

function THL7V2QueryItem.Evaluate(oSegment: THL7V2Segment; iInstance: integer): String;
begin
  result := '';
end;

function THL7V2QueryItem.Evaluate(oDataElement: THL7V2DataElement; iInstance: integer): String;
begin
  result := '';
end;

{ THL7V2QueryStringConstant }

constructor THL7V2QueryStringConstant.create(sToken: String);
begin
  inherited create;
  FValue := copy(sToken, 2, length(sToken)-2);
end;

function THL7V2QueryStringConstant.Evaluate(oSegment: THL7V2Segment; iInstance: integer): String;
begin
  result := FValue;
end;

function THL7V2QueryStringConstant.Evaluate(oDataElement: THL7V2DataElement; iInstance: integer): String;
begin
  result := FValue;
end;

function THL7V2QueryStringConstant.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ THL7V2QueryIndexRef }

Constructor THL7V2QueryIndexRef.create(sToken : String);
begin
  inherited create;
end;

function THL7V2QueryIndexRef.Evaluate(oSegment: THL7V2Segment; iInstance: integer): String;
begin
  result := IntegerToString(iInstance);
end;

function THL7V2QueryIndexRef.Evaluate(oDataElement: THL7V2DataElement; iInstance: integer): String;
begin
  result := IntegerToString(iInstance);
end;

{ THL7V2QueryNestedQuery }

constructor THL7V2QueryNestedQuery.create(oParser: THL7V2QueryParser; aContextType: THL7V2QueryContextType);
begin
  inherited create;
  FQuery := THL7V2Query.create;
  FQuery.ParseQuery(oParser, aContextType);
end;

function THL7V2QueryNestedQuery.Evaluate(oSegment: THL7V2Segment; iInstance: integer): String;
var
  oCell : THL7V2Cell;
begin
  oCell := FQuery.Execute(oSegment, false);
  result := oCell.AsString;
end;

destructor THL7V2QueryNestedQuery.Destroy;
begin
  FQuery.Free;
  inherited;
end;

function THL7V2QueryNestedQuery.Evaluate(oDataElement: THL7V2DataElement;  iInstance: integer): String;
var
  oCell : THL7V2Cell;
begin
  oCell := FQuery.Execute(oDataElement, false);
  result := oCell.AsString;
end;

function THL7V2QueryNestedQuery.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FQuery.sizeInBytes);
end;

{ THL7V2QueryConditional }

constructor THL7V2QueryConditional.create;
begin
  inherited;
  FLeft := THL7V2QueryNodes.create;
  FRight := THL7V2QueryNodes.create;
  FCompOp := qoVoid;
  FLinkOp := qoVoid;
end;

destructor THL7V2QueryConditional.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

function THL7V2QueryConditional.Check(oSegment: THL7V2Segment; iInstance: integer): Boolean;
var
  oNode : THL7V2QueryNode;
  aOp : THL7V2QueryOp;
  sLeft : String;
  sRight : String;
  iLoop : Integer;
begin
  Result := False;

  oNode := FLeft[0];
  sLeft := oNode.FItem.Evaluate(oSegment, iInstance);
  for iLoop := 1 to FLeft.count -1 do
    begin
    aOp := oNode.FOp;
    oNode := FLeft[iLoop];
    case aOp of
      qoAdd : sLeft := Add(sLeft, oNode.FItem.Evaluate(oSegment, iInstance));
      qoPlus : sLeft := Plus(sLeft, oNode.FItem.Evaluate(oSegment, iInstance));
      qoConcat : sLeft := Concat(sLeft, oNode.FItem.Evaluate(oSegment, iInstance));
      qoSubtract : sLeft := Subtract(sLeft, oNode.FItem.Evaluate(oSegment, iInstance));
    else
      ErrorHL7Library('Check', 'unknown op type '+NAMES_QUERY_OPS[aOp]);
    end;
    end;
  oNode := THL7V2QueryNode(FRight[0]);
  sRight := oNode.FItem.Evaluate(oSegment, iInstance);
  for iLoop := 1 to FRight.count -1 do
    begin
    aOp := oNode.FOp;
    oNode := THL7V2QueryNode(FRight[iLoop]);
    case aOp of
      qoAdd : sRight := Add(sRight, oNode.FItem.Evaluate(oSegment, iInstance));
      qoPlus : sRight := Plus(sRight, oNode.FItem.Evaluate(oSegment, iInstance));
      qoConcat : sRight := Concat(sRight, oNode.FItem.Evaluate(oSegment, iInstance));
      qoSubtract : sRight := Subtract(sRight, oNode.FItem.Evaluate(oSegment, iInstance));
    else
      ErrorHL7Library('Check', 'unknown op type '+NAMES_QUERY_OPS[aOp]);
    end;
    end;

  case FCompOp of
    qoEqual : result := Equal(sLeft, sRight);
    qoSame : result := Same(sLeft, sRight);
    qoNotEqual : result := NotEqual(sLeft, sRight);
    qoLess : result := Less(sLeft, sRight);
    qoGreater : result := Greater(sLeft, sRight);
    qoLessEqual : result := LessEqual(sLeft, sRight);
    qoGreaterEqual : result := GreaterEqual(sLeft, sRight);
  else
    ErrorHL7Library('Check', 'unknown op type '+NAMES_QUERY_OPS[FCompOp]);
  end;
end;

function THL7V2QueryConditional.Check(oDataElement: THL7V2DataElement; iInstance: integer): Boolean;
var
  oNode : THL7V2QueryNode;
  aOp : THL7V2QueryOp;
  sLeft : String;
  sRight : String;
  iLoop : Integer;
begin
  Result := False;

  oNode := FLeft[0];
  sLeft := oNode.FItem.Evaluate(oDataElement, iInstance);
  for iLoop := 1 to FLeft.count -1 do
    begin
    aOp := oNode.FOp;
    oNode := FLeft[iLoop];
    case aOp of
      qoAdd : sLeft := Add(sLeft, oNode.FItem.Evaluate(oDataElement, iInstance));
      qoPlus : sLeft := Plus(sLeft, oNode.FItem.Evaluate(oDataElement, iInstance));
      qoConcat : sLeft := Concat(sLeft, oNode.FItem.Evaluate(oDataElement, iInstance));
      qoSubtract : sLeft := Subtract(sLeft, oNode.FItem.Evaluate(oDataElement, iInstance));
    else
      ErrorHL7Library('Check', 'unknown op type '+NAMES_QUERY_OPS[aOp]);
    end;
    end;
  oNode := THL7V2QueryNode(FRight[0]);
  sRight := oNode.FItem.Evaluate(oDataElement, iInstance);
  for iLoop := 1 to FRight.count -1 do
    begin
    aOp := oNode.FOp;
    oNode := THL7V2QueryNode(FRight[iLoop]);
    case aOp of
      qoAdd : sRight := Add(sRight, oNode.FItem.Evaluate(oDataElement, iInstance));
      qoPlus : sRight := Plus(sRight, oNode.FItem.Evaluate(oDataElement, iInstance));
      qoConcat : sRight := Concat(sRight, oNode.FItem.Evaluate(oDataElement, iInstance));
      qoSubtract : sRight := Subtract(sRight, oNode.FItem.Evaluate(oDataElement, iInstance));
    else
      ErrorHL7Library('Check', 'unknown op type '+NAMES_QUERY_OPS[aOp]);
    end;
    end;

  case FCompOp of
    qoEqual : result := Equal(sLeft, sRight);
    qoSame : result := Same(sLeft, sRight);
    qoNotEqual : result := NotEqual(sLeft, sRight);
    qoLess : result := Less(sLeft, sRight);
    qoGreater : result := Greater(sLeft, sRight);
    qoLessEqual : result := LessEqual(sLeft, sRight);
    qoGreaterEqual : result := GreaterEqual(sLeft, sRight);
    qoRegex : result := DoRegex(sLeft, sRight);
    qoStartsWith : result := StringStartsWithInsensitive(sLeft, sRight);
    qoEndsWith : result := StringEndsWithInsensitive(sLeft, sRight);
  else
    ErrorHL7Library('Check', 'unknown op type '+NAMES_QUERY_OPS[FCompOp]);
  end;
end;

function THL7V2QueryConditional.Add(sLeft, sRight: String): String;
begin
  if StringIsInteger32(sLeft) and StringIsInteger32(sRight) then
    result := IntegerToString(StringToInteger32(sLeft) + StringToInteger32(sRight))
  else
    result := sLeft + sRight;
end;

function THL7V2QueryConditional.Plus(sLeft, sRight: String): String;
begin
  if StringIsInteger32(sLeft) and StringIsInteger32(sRight) then
    result := IntegerToString(StringToInteger32(sLeft) + StringToInteger32(sRight))
  else
    ErrorApplication('Plus', 'Can''t add "'+sLeft+'" and "'+sRight+'"');
end;

function THL7V2QueryConditional.Concat(sLeft, sRight: String): String;
begin
  result := sLeft + sRight;
end;

function THL7V2QueryConditional.Subtract(sLeft, sRight: String): String;
begin
  if StringIsInteger32(sLeft) and StringIsInteger32(sRight) then
    result := IntegerToString(StringToInteger32(sLeft) - StringToInteger32(sRight))
  else
    ErrorApplication('Subtract', 'Can''t subtract "'+sLeft+'" and "'+sRight+'"');
end;

function THL7V2QueryConditional.Equal(sLeft, sRight: String): Boolean;
begin
  result := sLeft = sRight;
end;

function THL7V2QueryConditional.Greater(sLeft, sRight: String): Boolean;
begin
  result := sLeft > sRight;
end;

function THL7V2QueryConditional.GreaterEqual(sLeft, sRight: String): Boolean;
begin
  result := sLeft >= sRight;
end;

function THL7V2QueryConditional.Less(sLeft, sRight: String): Boolean;
begin
  result := sLeft < sRight;
end;

function THL7V2QueryConditional.LessEqual(sLeft, sRight: String): Boolean;
begin
  result := sLeft <= sRight;
end;

function THL7V2QueryConditional.NotEqual(sLeft, sRight: String): Boolean;
begin
  result := sLeft <> sRight;
end;

function THL7V2QueryConditional.Same(sLeft, sRight: String): Boolean;
begin
  result := StringEquals(sLeft, sRight);
end;

function THL7V2QueryConditional.DoRegex(sLeft, sRight: String): Boolean;
var
  oExpr : TRegEx;
begin
  oExpr := TRegEx.Create(sRight);
  result := oExpr.isMatch(sLeft);
end;

function THL7V2QueryConditional.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLeft.sizeInBytes);
  inc(result, FRight.sizeInBytes);
end;

{ THL7V2QueryCondition }

constructor THL7V2QueryCondition.create;
begin
  inherited;
  FConditions := THL7V2QueryConditionals.Create;
end;

destructor THL7V2QueryCondition.Destroy;
begin
  FConditions.Free;
  inherited;
end;

function THL7V2QueryCondition.GetOp(sToken: String; out aOp : THL7V2QueryOp):Boolean;
begin
  result := true;
  if sToken = '=' then
    aOp := qoEqual
  else if sToken = '==' then
    aOp := qoSame
  else if sToken = '!=' then
    aOp := qoNotEqual
  else if sToken = '<' then
    aOp := qoLess
  else if sToken = '>' then
    aOp := qoGreater
  else if sToken = '<=' then
    aOp := qoLessEqual
  else if sToken = '>=' then
    aOp := qoGreaterEqual
  else if sToken = 'and' then
    aOp := qoAnd
  else if sToken = 'or' then
    aOp := qoOr
  else if sToken = 'xor' then
    aOp := qoXor
  else if sToken = '+' then
    aOp := qoAdd
  else if sToken = '++' then
    aOp := qoPlus
  else if sToken = '&' then
    aOp := qoConcat
  else if sToken = '-' then
    aOp := qoSubtract
  else if sToken = '~' then
    aOp := qoRegex
  else if sToken = '|~' then
    aOp := qoStartsWith
  else if sToken ='~|' then
    aOp := qoEndsWith
  else
    result := false;
end;

procedure THL7V2QueryCondition.ParseQuery(oParser : THL7V2QueryParser; aContextType : THL7V2QueryContextType);
var
  sEnd : String;
  sPeek : String;
  aOp : THL7V2QueryOp;
  oCondition : THL7V2QueryConditional;
begin
  oCondition := THL7V2QueryConditional.create;
  FConditions.add(oCondition);

  sEnd := oParser.Consume;
  if sEnd = '[' then
    sEnd := ']'
  else if sEnd = '(' then
    sEnd := ')'
  else
    ErrorApplication('ParseQuery', 'Unrecognised start of expression "'+sEnd+'"');
  oParser.NextNonWhitespace;
  repeat
    sPeek := oParser.Peek;
    {if sPeek = '(' then
      AddItem(oParser, THL7V2QueryNestedCondition.Create(oParser, aContextType))
    else }if sPeek[1] = '"' then
      AddItem(oParser, THL7V2QueryStringConstant.Create(oParser.Consume))
    else if StringIsInteger32(sPeek) then
      AddItem(oParser, THL7V2QueryStringConstant.Create('"'+oParser.Consume+'"'))
    else if sPeek[1] = '@' then
      begin
      oParser.Consume;
      if oParser.Peek = 'index' then
        AddItem(oParser, THL7V2QueryIndexRef.create(oParser.Consume))
      else
        AddItem(oParser, THL7V2QueryNestedQuery.Create(oParser, aContextType));
      end
    else if GetOp(sPeek, aOp) then
      begin
      oParser.Consume;
      AddOp(oParser, aOp)
      end
    else
      ErrorApplication('ParseQuery', 'Unexpected Expression Token "'+sPeek+'"');
    oParser.NextNonWhitespace;
  until (oParser.Peek = sEnd) or oParser.Finished;
  oParser.Check(not oParser.Finished, 'ParseQuery', 'Unexpected end to query parsing condition');
  oParser.Consume;
  CheckTree(oParser);
end;

procedure THL7V2QueryCondition.ParseIndex(oParser: THL7V2QueryParser);
var
  oCondition : THL7V2QueryConditional;
begin
  oCondition := THL7V2QueryConditional.create;
  FConditions.add(oCondition);
  oParser.Consume;
  oParser.Check(StringIsInteger32(oParser.Peek), 'ParseIndex', 'Simple index reference must be a number, not "'+oParser.Peek+'"');
  Additem(oParser, THL7V2QueryStringConstant.Create('"'+oParser.Consume+'"'));
  CheckTree(oParser);
end;

procedure THL7V2QueryCondition.AddItem(oParser : THL7V2QueryParser; oItem: THL7V2QueryItem);
var
  oCond : THL7V2QueryConditional;
  oList : THL7V2QueryNodes;
  oNode : THL7V2QueryNode;
begin
  oCond := FConditions[FConditions.count - 1];
  if oCond.FCompOp = qoVoid then
    oList := oCond.FLeft
  else
    oList := oCond.FRight;
  if oList.Count > 0 then
    oParser.Check(oList[0].FOp <> qoVoid, 'AddItem', 'Missing Operation');
  oNode := THL7V2QueryNode.create;
  oNode.FItem := oItem;
  oNode.FOp := qoVoid;
  oList.Add(oNode);
end;

procedure THL7V2QueryCondition.AddOp(oParser : THL7V2QueryParser; aOp: THL7V2QueryOp);
var
  oCond : THL7V2QueryConditional;
  oList : THL7V2QueryNodes;
  oNode : THL7V2QueryNode;
begin
  oCond := FConditions[FConditions.count - 1];

  if aOp in [qoAdd, qoPlus, qoConcat, qoSubtract] then
    begin
    if oCond.FCompOp = qoVoid then
      oList := oCond.FLeft
    else
      oList := oCond.FRight;
    oParser.Check((oList.Count > 0), 'AddOp', 'Must have an expression before an operation');
    oNode := oList[oList.Count-1];
    oParser.Check(oNode.FOp = qoVoid, 'AddOp', 'Cannot repeat operations - an expression is missing');
    oNode.FOp := aOp;
    end
  else if aOp in [qoEqual, qoSame, qoNotEqual, qoLess, qoGreater, qoLessEqual, qoGreaterEqual, qoRegex, qoStartsWith, qoEndsWith] then
    begin
    oParser.Check(oCond.FCompOp = qoVoid, 'AddOp', 'Cannot string comparison operations together');
    oCond.FCompOp := aOp;
    end
  else if aOp in [qoAnd, qoOr, qoXor] then
    begin
    oCond.FLinkOp := aOp;
    oCond := THL7V2QueryConditional.create;
    FConditions.add(oCond);
    end
  else
    oParser.ErrorApplication('AddOp', 'unknown op type '+NAMES_QUERY_OPS[aOp]);
end;

procedure THL7V2QueryCondition.CheckCondition(oParser : THL7V2QueryParser; oCond : THL7V2QueryConditional);
begin
  oParser.Check(oCond.FCompOp <> qoVoid, 'CheckCondition', 'Missing Comparison operator');
  oParser.Check(oCond.FLeft.count > 0, 'CheckCondition', 'No operations on left of comparison Operator');
  oParser.Check(THL7V2QueryNode(oCond.FLeft[oCond.FLeft.Count-1]).FOp  =qoVoid, 'CheckCondition', 'Unterminated expression');
  oParser.Check(oCond.FRight.count > 0, 'CheckCondition', 'No operations on Right of comparison Operator');
  oParser.Check(THL7V2QueryNode(oCond.FRight[oCond.FRight.Count-1]).FOp  =qoVoid, 'CheckCondition', 'Unterminated expression');
end;

procedure THL7V2QueryCondition.CheckTree(oParser : THL7V2QueryParser);
var
  oCond : THL7V2QueryConditional;
  oList : THL7V2QueryNodes;
  oNode : THL7V2QueryNode;
  iLoop : Integer;
begin
  oCond := FConditions[0];
  oList := oCond.FLeft;
  if oList.Count > 0 then
    oNode := oList[0]
  else
    oNode := nil;

  if (oCond.FCompOp = qoVoid) and assigned(oNode) and (oList.Count = 1) and
       (oNode.FItem is THL7V2QueryStringConstant) and StringIsInteger32(THL7V2QueryStringConstant(oNode.FItem).FValue) then
    begin
    AddOp(oParser, qoEqual);
    AddItem(oParser, THL7V2QueryIndexRef.create(''));
    end;

  for iLoop := 0 to FConditions.Count - 1 do
    begin
    oCond := FConditions[iLoop];
    CheckCondition(oParser, oCond);
    end;
end;

function THL7V2QueryCondition.CheckConditions(oSegment: THL7V2Segment; iInstance: integer): Boolean;
var
  oCond : THL7V2QueryConditional;
  aOp : THL7V2QueryOp;
  iLoop : Integer;
begin
  if FConditions.count = 0 then
    result := true
  else
    begin
    oCond := FConditions[0];
    result := oCond.Check(oSegment, iInstance);
    for iLoop := 1 to FConditions.Count - 1 do
      begin
      aOp := oCond.FLinkOp;
      oCond := FConditions[iLoop];
      case aOp of
        qoAnd :  result := result and oCond.Check(oSegment, iInstance);
        qoOr : result := result or oCond.Check(oSegment, iInstance);
        qoXor : result := result xor oCond.Check(oSegment, iInstance);
      else
        ErrorApplication('CheckConditions', 'unknown op type');
      end;
      end;
    end;
end;

function THL7V2QueryCondition.CheckConditions(oDataElement: THL7V2DataElement; iInstance: integer): Boolean;
var
  oCond : THL7V2QueryConditional;
  aOp : THL7V2QueryOp;
  iLoop : Integer;
begin
  if FConditions.count = 0 then
    result := true
  else
    begin
    oCond := FConditions[0];
    result := oCond.Check(oDataElement, iInstance);
    for iLoop := 1 to FConditions.Count - 1 do
      begin
      aOp := oCond.FLinkOp;
      oCond := FConditions[iLoop];
      case aOp of
        qoAnd :  result := result and oCond.Check(oDataElement, iInstance);
        qoOr : result := result or oCond.Check(oDataElement, iInstance);
        qoXor : result := result xor oCond.Check(oDataElement, iInstance);
      else
        ErrorApplication('CheckConditions', 'unknown op type');
      end;
      end;
    end;
end;

function THL7V2QueryCondition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FConditions.sizeInBytes);
end;

{ THL7V2Query }

procedure THL7V2Query.ParseQuery(sQuery: String; aContextType: THL7V2QueryContextType);
var
  oParser : THL7V2QueryParser;
begin
  oParser := THL7V2QueryParser.create(sQuery);
  try
    ParseQuery(oParser, aContextType);
    oParser.Check(oParser.Finished, 'ParseQuery', 'Query parsing is finished but expression continues: "'+oParser.Peek+'"');
  finally
    oParser.Free;
  end;
end;

procedure THL7V2Query.ParseQuery(oParser : THL7V2QueryParser; aContextType : THL7V2QueryContextType);
var
  sPeek : String;
begin
  if aContextType = qctMessage then
    begin
    FSegCode := oParser.Consume;
    oParser.Check((Length(FSegCode) = 3) and CharInSet(fSegCode[1], ['A'..'Z']) and CharInSet(fSegCode[2], ['A'..'Z', '0'..'9']) and CharInSet(fSegCode[3], ['A'..'Z', '0'..'9']) , 'ParseQuery', 'Invalid SegmentCode');
    FSegCondition := THL7V2QueryCondition.Create;
    if oParser.Peek = '[' then
      FSegCondition.ParseQuery(oParser, qctSegment)
    else if oParser.Peek = ':' then
      FSegCondition.ParseIndex(oParser);
    // it's a bit loose at the end here - the user has to separate
    // expressions using - with a space to differentiate - as a minus
    // operator from - as a field separator
    oParser.Check(oParser.Peek = '-', 'ParseQuery', 'Invalid Segment-Field Separator "'+oParser.Peek+'"');
    oParser.Consume;
    end;
  if aContextType in [qctMessage, qctSegment] then
    begin
    FFieldId := oParser.Consume;
    oParser.Check(StringIsInteger32(FFieldId), 'ParseQuery', 'Invalid Field Id "'+FFieldId+'"');
    FRepeatCondition := THL7V2QueryCondition.Create;
    if oParser.Peek = '[' then
      FRepeatCondition.ParseQuery(oParser, qctField)
    else if oParser.Peek = ':' then
      FRepeatCondition.ParseIndex(oParser);
    // it's a bit loose at the end here - the user has to separate
    // expressions using - with a space to differentiate - as a minus
    // operator from - as a field separator
    if (oParser.Peek = '-') or (oParser.Peek = '.') then
      oParser.Consume;
    end;
  if StringIsInteger32(oParser.Peek) then
    begin
    FCompId := oParser.Consume;
    sPeek := oParser.Peek;
    if (sPeek = '-') or (sPeek = '.') then
      begin
      oParser.Consume;
      if StringIsInteger32(oParser.Peek) then
        FSubCompId := oParser.Consume;
      end;
    end
  else
    oParser.Check(aContextType <> qctField, 'ParseQuery', 'Unexpected end of field specification');
end;

function THL7V2Query.Execute(oMessage: THL7V2Message; bForce : Boolean): THL7V2Cell;
var
  iLoop : integer;
  iInstance : integer;
  oSeg : THL7V2Segment;
begin
  result := nil;
  iInstance := 0;
  for iLoop := 0 to oMessage.Segments.count - 1 do
    begin
    oSeg := oMessage.Segments[iLoop];
    if StringEquals(oSeg.Code, FSegCode) then
      begin
      if FSegCondition.CheckConditions(oSeg, iInstance) then
        begin
        result := Execute(oSeg, bForce);
        exit;
        end;
      inc(iInstance);
      end;
    end;
end;

function THL7V2Query.Execute(oSegment: THL7V2Segment; bForce : Boolean): THL7V2Cell;
var
  oField : THL7V2DataElement;
  iLoop : integer;
begin
  oField := oSegment.Field[StringToInteger32(FFieldId)];
  if bForce and (oField = nil) then
    oField := oSegment.ForceField(StringToInteger32(FFieldId));
  if FRepeatCondition.CheckConditions(oField, 0) then
    result := Execute(oField, bForce)
  else
    begin
    result := nil;
    if oField.HasRepeats then
      for iLoop := 0 to oField.Repeats.count - 1 do
        if FRepeatCondition.CheckConditions(oField.Repeats[iLoop], iLoop+1) then
          begin
          result := Execute(oField.Repeats[iLoop], bForce);
          exit;
          end;
    end;
end;

function THL7V2Query.Execute(oField: THL7V2DataElement; bForce : Boolean): THL7V2Cell;
var
  oComp : THL7V2Component;
begin
  if FCompId = '' then
    result := oField
  else
    begin
    oComp := oField.Component[StringToInteger32(FCompId)];
    if bForce and (oComp = nil) then
      oComp := oField.ForceComponent(StringToInteger32(FCompId));
    if FSubCompId = '' then
      result := oComp
    else
      begin
      result := oComp.Component[StringToInteger32(FSubCompId)];
      if bForce and (result = nil) then
        result := oComp.ForceComponent(StringToInteger32(FSubCompId));
      end;
    end;
end;

destructor THL7V2Query.Destroy;
begin
  FSegCondition.Free;
  FRepeatCondition.Free;
  inherited;
end;

function THL7V2Query.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FSegCode.length * sizeof(char)) + 12);
  inc(result, FSegCondition.sizeInBytes);
  inc(result, (FFieldId.length * sizeof(char)) + 12);
  inc(result, FRepeatCondition.sizeInBytes);
  inc(result, (FCompId.length * sizeof(char)) + 12);
  inc(result, (FSubCompId.length * sizeof(char)) + 12);
end;

{ THL7V2QueryNodes }

function THL7V2QueryNodes.GetNode(iIndex: integer): THL7V2QueryNode;
begin
  result := THL7V2QueryNode(objectByIndex[iIndex]);
end;

function THL7V2QueryNodes.ItemClass: TFslObjectClass;
begin
  result := THL7V2QueryNode;
end;

{ THL7V2QueryConditionals }

function THL7V2QueryConditionals.GetConditional(iIndex: integer): THL7V2QueryConditional;
begin
  result := THL7V2QueryConditional(objectByIndex[iIndex]);
end;

function THL7V2QueryConditionals.ItemClass: TFslObjectClass;
begin
  result := THL7V2QueryConditional;
end;

{ THL7V2Decoder }

Procedure THL7V2Decoder.DecodeMessage(oMessage: THL7V2Message; oContent: TFslBuffer; oOptions: THL7V2DecodingOptions);
Begin
  DecodeMessage(oMessage, oContent.AsBytes, oOptions);
End;

Procedure THL7V2Decoder.DecodeMessage(oMessage: THL7V2Message; Const sContent: TBytes; oOptions: THL7V2DecodingOptions);
Begin
  ErrorHL7Library('DecodeMessage', 'Need to override DecodeMessage in '+ClassName);
End;

Procedure THL7V2Decoder.DecodeSegment(oSegment: THL7V2Segment; oContent: TFslBuffer; oOptions: THL7V2DecodingOptions);
Begin
  DecodeSegment(oSegment, oContent.AsBytes, oOptions);
End;

Procedure THL7V2Decoder.DecodeSegment(oSegment: THL7V2Segment; Const sContent: TBytes; oOptions: THL7V2DecodingOptions);
Begin
  ErrorHL7Library('DecodeSegment', 'Need to override DecodeSegment in '+ClassName);
End;

Procedure THL7V2Decoder.DecodeCell(oCell: THL7V2Cell; oContent: TFslBuffer; oOptions: THL7V2DecodingOptions);
Begin
  DecodeCell(oCell, oContent.AsBytes, oOptions);
End;

Procedure THL7V2Decoder.DecodeCell(oCell: THL7V2Cell; Const sContent: TBytes; oOptions: THL7V2DecodingOptions);
Begin
  ErrorHL7Library('DecodeCell', 'Need to override DecodeCell in '+ClassName);
End;

Procedure THL7V2Decoder.DecodeDataElement(oDataElement: THL7V2DataElement; oContent: TFslBuffer; oOptions: THL7V2DecodingOptions);
Begin
  DecodeDataElement(oDataElement, oContent.AsBytes, oOptions);
End;

Procedure THL7V2Decoder.DecodeDataElement(oDataElement: THL7V2DataElement; Const sContent: TBytes; oOptions: THL7V2DecodingOptions);
Begin
  ErrorHL7Library('DecodeDataElement', 'Need to override DecodeDataElement in '+ClassName);
End;

Function THL7V2Decoder.GetDelimiter(sName, sStr : String; iIndex : Integer) : Char;
Begin
  If Length(sStr) >= iIndex Then
    Result := SStr[iIndex]
  Else
    Begin
    Result := #0;
    ErrorBadMessage('GetDelimiter', sName+' Delimiter not found in MSH-2: "'+sStr+'"');
    End;
End;

function THL7V2Decoder.getOptions(opt: THL7V2DecodingOptions): THL7V2DecodingOptions;
begin
  if opt <> nil then
    result := opt.Link
  else
    result := THL7V2DecodingOptions.getDefaults;
end;

function THL7V2Decoder.parse(msg: TBytes; options : THL7V2DecodingOptions = nil): THL7V2Message;
var
  opt : THL7V2DecodingOptions;
begin
  opt := getOptions(options);
  try
    result := THL7V2Message.Create(Dictionary.Link);
    try
      DecodeMessage(result, msg, opt);
      result.link;
    finally
      result.Free;
    end;
  finally
    opt.free;
  end;
end;

function THL7V2Decoder.parse(msg: String; options : THL7V2DecodingOptions = nil): THL7V2Message;
begin
  result := parse(TEncoding.UTF8.GetBytes(msg));
end;

function THL7V2Decoder.parse(msg: TStream; options : THL7V2DecodingOptions = nil): THL7V2Message;
begin
  result := parse(StreamToBytes(msg));
end;

function THL7V2Decoder.parse(msg: TFslStream; options : THL7V2DecodingOptions = nil): THL7V2Message;
var
  s : TVCLStream;
begin
  s := TVCLStream.Create(msg.Link);
  try
    result := parse(StreamToBytes(s));
  finally
    s.Free;
  end;
end;

function THL7V2Decoder.parse(msg: TFslBuffer; options : THL7V2DecodingOptions = nil): THL7V2Message;
begin
  result := parse(msg.AsBytes);
end;

Procedure THL7V2Decoder.DecodeFile(oFile: THL7V2File; oStream: TFslStream; oOptions : THL7V2DecodingOptions);
Begin
  ErrorHL7Library('DecodeFile', 'Need to override DecodeFile in '+ClassName);
End;

procedure THL7V2Decoder.DecodeCellEx(oCell: THL7V2Cell; iStart: integer; const sContent: TBytes; oOptions: THL7V2DecodingOptions; aLevelOperation : THL7v2DecodingLevelOperation);
begin
  ErrorHL7Library('DecodeCell', 'Need to override DecodeCellEx in '+ClassName);
end;

{ THL7V2Encoder }

Function THL7V2Encoder.EncodeMessage(oMessage: THL7V2Message; oOptions: THL7V2EncodingOptions) : TBytes;
begin
  ErrorHL7Library('EncodeMessage', 'Need to override EncodeMessage in '+ClassName);
end;

procedure THL7V2Encoder.EncodeMessage(oBuffer: TFslBuffer; oMessage: THL7V2Message; oOptions: THL7V2EncodingOptions);
begin
  oBuffer.AsBytes := EncodeMessage(oMessage, oOptions);
end;

Function THL7V2Encoder.EncodeCell(oCell : THL7V2Cell; oOptions : THL7V2EncodingOptions) : TBytes;
begin
  ErrorHL7Library('EncodeCell', 'Need to override EncodeCell in '+ClassName);
end;

procedure THL7V2Encoder.EncodeCell(oBuffer : TFslBuffer; oCell : THL7V2Cell; oOptions : THL7V2EncodingOptions);
begin
  oBuffer.AsBytes := EncodeCell(oCell, oOptions);
end;

Function THL7V2Encoder.EncodeDataElement(oDataElement : THL7V2DataElement; oOptions : THL7V2EncodingOptions) : TBytes;
begin
  ErrorHL7Library('EncodeDataElement', 'Need to override EncodeDataElement in '+ClassName);
end;

procedure THL7V2Encoder.EncodeDataElement(oBuffer : TFslBuffer; oDataElement : THL7V2DataElement; oOptions : THL7V2EncodingOptions);
begin
  oBuffer.AsBytes := EncodeDataElement(oDataElement, oOptions);
end;

Function THL7V2Encoder.EncodeSegment(oSegment : THL7V2Segment; oOptions : THL7V2EncodingOptions) : TBytes;
begin
  ErrorHL7Library('EncodeSegment', 'Need to override EncodeSegment in '+ClassName);
end;

procedure THL7V2Encoder.EncodeSegment(oBuffer : TFslBuffer; oSegment : THL7V2Segment; oOptions : THL7V2EncodingOptions);
begin
  oBuffer.AsBytes := EncodeSegment(oSegment, oOptions);
end;

procedure THL7V2Encoder.EncodeFile(oFile: THL7V2File; oStream: TFslStream; oOptions : THL7V2EncodingOptions);
begin
  ErrorHL7Library('EncodeFile', 'Need to override EncodeFile in '+ClassName);
end;

{ THL7V2DecoderFactory }

procedure THL7V2DecoderFactory.ConsumeDecoder(oDecoder: THL7V2Decoder);
begin
  oDecoder.Free;
end;

function THL7V2DecoderFactory.ProduceDecoder(aFormat: THL7V2Format): THL7V2Decoder;
begin
  if aFormat = hfXML then
    result := THL7V2XMLDecoder.create(Dictionary.Link)
  else
    result := THL7V2ER7Decoder.create(Dictionary.Link);
end;

function StartMatches(AStr : String; AMatchList : Array of String; ACase : Boolean = true):Boolean;
var
  i : integer;
begin
  result := false;
  if ACase then
    for i := Low(AMatchList) to High(AMatchList) do
      result := result or (copy(AStr, 1, length(AMatchList[i])) = AMatchList[i])
  else
    for i := Low(AMatchList) to High(AMatchList) do
      result := result or (Lowercase(copy(AStr, 1, length(AMatchList[i]))) = Lowercase(AMatchList[i]));
end;

function THL7V2DecoderFactory.ProduceDecoder(const sSource: TBytes; oOptions : THL7V2DecodingOptions = nil; aUse : THL7V2DecoderUse = fuMessage): THL7V2Decoder;
begin
  result := ProduceDecoder(RecogniseFormat(sSource, oOptions, aUse));
end;

function THL7V2DecoderFactory.ProduceDecoder(oBuffer: TFslBuffer; oOptions : THL7V2DecodingOptions = nil; aUse : THL7V2DecoderUse = fuMessage): THL7V2Decoder;
begin
  result := ProduceDecoder(oBuffer.AsBytes, oOptions, aUse);
end;

function THL7V2DecoderFactory.RecogniseFormat(oBuffer: TFslBuffer; oOptions : THL7V2DecodingOptions = nil; aUse : THL7V2DecoderUse = fuMessage): THL7V2Format;
begin
  result := RecogniseFormat(oBuffer.AsBytes, oOptions, aUse);
end;

function THL7V2DecoderFactory.RecogniseFormat(const sSource: TBytes; oOptions : THL7V2DecodingOptions = nil; aUse : THL7V2DecoderUse = fuMessage): THL7V2Format;
begin
  if assigned(oOptions) then
    result := oOptions.Format

  // first, does this have an endian marker or another conformant XML marker
  // we consider anything with a matching signature to be XML,
  // since old-style HL7Messages are ASCII
  // this list taken from http://www.w3.org/TR/2000/REC-xml-20001006
  else if StartMatches(BytesAsString(sSource), [#$00#$00#$FE#$FF, #$FF#$FE#$00#$00, #$00#$00#$FF#$FE, #$FE#$FF#$00#$00,
                            #$FE#$FF, #$FF#$FE, #$EF#$BB#$BF, #$00#$00#$00#$3C, #$3C#$00#$00#$00,
                            #$00#$00#$3C#$00, #$00#$3C#$00#$00, #$00#$3C#$00#$3F, #$3C#$00#$3F#$00,
                            #$3C#$3F#$78#$6D, #$4C#$6F#$A7#$94]) or StringStartsWith(BytesAsString(sSource), '<') then
    result := hfXML

  else
    case aUse of
      fuMessage : result := RecogniseMessageFormat(sSource);
      fuSegment : result := RecogniseSegmentFormat(sSource);
      fuDataElement, fuComponent    : result := RecogniseCellFormat(sSource);
    else
      result := hfUnknown;
      ErrorHL7Library('RecogniseFormat', 'Unknown use');
    end;
end;

function THL7V2DecoderFactory.RecogniseMessageFormat(const sSource: TBytes): THL7V2Format;
var
  sTemp : String;
begin
  result := hfUnknown;
  if length(sSource) < 2 then
    ErrorBadMessage('ProduceDecoder', 'The Message encoding type was not defined and the message is too short to determine the type (Packet="' + EncodePercent(BytesAsString(sSource)) + ')"');

  sTemp := StringUpper(StringTrimWhitespaceLeft(BytesAsString(copy(sSource, 0, 100))));
  if StartMatches(sTemp, ['MSH']) then
    result := hfER7
  else if StartMatches(sTemp, ['FHS']) then
    ErrorBadMessage('ProduceDecoder', 'The Message is actually an HL7 batch file')
  else if (pos('DOCTYPE', sTemp) > 0) or  (pos('<XML', sTemp) > 0) then
    result := hfXML
  else
    ErrorBadMessage('ProduceDecoder', 'The Message encoding type was not defined and could not be observed (starting text = "' + copy(sTemp, 0, 10) + '")');
end;

function isUppercase(s : String) : boolean;
begin
  result := s = UpperCase(s);
end;

function THL7V2DecoderFactory.RecogniseSegmentFormat(const sSource: TBytes): THL7V2Format;
begin
  if (StringIsAlphanumeric(BytesAsString(copy(sSource, 0, 3))) and isUppercase(BytesAsString(copy(sSource, 0, 3)))) and
     ((length(sSource) = 3) or ((length(sSource) > 3) and (sSource[4] = ord('|')))) then // limitation : only possible to detect standard encoding chars
    result := hfER7
  else if (pos('DOCTYPE', BytesAsString(sSource)) > 0) or  (pos('<', BytesAsString(sSource)) > 0) then
    result := hfXML
  else
    begin
    result := hfUnknown;
    ErrorBadMessage('ProduceDecoder', 'The Segment Fragment Format type could not be determined (starting text = "' + BytesAsString(copy(sSource, 1, 10)) + '")');
    end;
end;

function THL7V2DecoderFactory.RecogniseCellFormat(const sSource: TBytes): THL7V2Format;
var
  sSrc : String;
begin
  sSrc := BytesAsString(sSource);
  if sSrc = '' then
    result := hfER7
  else if (StringFind(sSrc, '</') > 0) or (sSrc[1] = '<') then
    result := hfXML
  else if StringFind(sSrc, ['^', '~', '&', '\']) > 0 then
    result := hfER7
  else
    result := hfER7; // at this point, there's no reason not to
end;

{ THL7V2EncoderFactory }

procedure THL7V2EncoderFactory.ConsumeEncoder(oEncoder: THL7V2Encoder);
begin
  oEncoder.Free;
end;

function THL7V2EncoderFactory.ProduceEncoder(aFormat: THL7V2Format): THL7V2Encoder;
begin
  if aFormat = hfXML then
    result := THL7V2XMLEncoder.create(Dictionary.Link)
  else
    result := THL7V2ER7Encoder.create(Dictionary.Link);
end;

Constructor THL7V2ModelProvider.Create(oModel : THL7V2Model);
Begin
  Create;
  Model := oModel;
End;

Constructor THL7V2ModelProvider.Create(oDictionary : THL7V2Dictionary);
Begin
  Create;
  Dictionary := oDictionary;
End;

Constructor THL7V2ModelProvider.Create(oProvider: THL7V2ModelProvider);
Begin
  Create;
  Dictionary := oProvider.Dictionary.Link;
  If Not Assigned(Dictionary) Then
    Model := oProvider.Model.Link;
End;

Destructor THL7V2ModelProvider.Destroy;
Begin
  FModel.Free;
  FDictionary.Free;
  Inherited;
End;

Function THL7V2ModelProvider.GetModel: THL7V2Model;
Begin
  Assert(Invariants('GetModel', FModel, THL7V2Model, 'Model (=Version)'));
  Result := FModel;
End;

Function THL7V2ModelProvider.GetVersion: THL7V2Version;
Begin
  Assert(Invariants('GetModel', FModel, THL7V2Model, 'Model (=Version)'));
  Result := FModel.Version;
End;

Function THL7V2ModelProvider.HasVersion: Boolean;
Begin
  Result := (FModel <> Nil);
End;

Function THL7V2ModelProvider.GetVersionCode: String;
Begin
  Result := NAMES_HL7V2_VERSION[GetVersion];
End;

Procedure THL7V2ModelProvider.SetVersionCode(Const sValue: String);
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOfSensitive(NAMES_HL7V2_VERSION, sValue);
  If iIndex = -1 Then
    ErrorUnsVersion('FromVersionCode', 'Bad HL7 Version '+sValue);
  Version := FromVersionCode(sValue);
End;

Procedure THL7V2ModelProvider.SetVersion(aValue: THL7V2Version);
Begin
  If Not Assigned(FModel) Or (FModel.Version <> aValue) Then
    Begin
    If Not Assigned(FDictionary) Then
      ErrorUnsVersion('SetVersion', 'No support for changing the version as no dictionary has been created');

    If Assigned(FModel) And Not CanChangeVersion Then
      ErrorApplication('SetVersion', 'Cannot change the version of this construct using SetVersion. Version may be changed using ChangeVersion');

    If Not (aValue In FDictionary.Versions) Then
      ErrorUnsVersion('SetVersion', 'No support for version '+NAMES_HL7V2_VERSION[aValue]);

    If Assigned(FModel) Then
      Begin
      FModel.Free;
      FModel := Nil;
      End;

    FModel := FDictionary.Model[aValue].Link;
    End;
End;

Procedure THL7V2ModelProvider.Clear;
Begin
  Inherited;
  If Assigned(FDictionary) Then
    Begin
    FModel.Free;
    FModel := Nil;
    End;
End;

Function THL7V2ModelProvider.CanChangeVersion: Boolean;
Begin
  Result := Not Assigned(FModel);
End;

Procedure THL7V2ModelProvider.SetDictionary(Const Value: THL7V2Dictionary);
Begin
  If Assigned(Value) And Assigned(FModel) Then
    ErrorApplication('SetDictionary', 'Unable to set dictionary when a model is in use');
  FDictionary.Free;
  FDictionary := Value;
End;

Procedure THL7V2ModelProvider.SetModel(Const Value: THL7V2Model);
Begin
  If Assigned(FDictionary) Then
    ErrorApplication('SetModel', 'Unable to set a Model when the message is attached to a dictionary - set the version instead');
  FModel := Value;
End;

Procedure THL7V2ModelProvider.Assign(oSource: TFslObject);
Begin
  Inherited;
  FDictionary.Free;
  FDictionary := THL7V2ModelProvider(oSource).Dictionary.Link;
  FModel.Free;
  FModel := THL7V2ModelProvider(oSource).Model.Link;
End;

procedure THL7V2ModelProvider.ChangeVersion(ANewVersion: String; ADeleteInvalidContent: Boolean);
begin
  SetVersion(FromVersionCode(aNewVersion));
end;

function THL7V2ModelProvider.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDictionary.sizeInBytes);
  inc(result, FModel.sizeInBytes);
end;

{ THL7V2ER7Formatter }

function THL7V2ER7Formatter.Link: THL7V2ER7Formatter;
begin
  result := THL7V2ER7Formatter(inherited Link);
end;

procedure THL7V2ER7Formatter.ProduceContent(const sValue: TBytes);
begin
  Produce(BytesAsString(sValue));
end;

{ THL7V2ER7Decoder }

Constructor THL7V2ER7Decoder.Create;
begin
  inherited;
  FDeCodingOptions := THL7V2DeCodingOptions.create;
  FDelimiters := THL7V2Delimiters.create;
end;

Destructor THL7V2ER7Decoder.Destroy;
begin
  FDelimiters.Free;
  FDeCodingOptions.Free;
  inherited;
end;

Function CleanPacket(packet : TBytes) : TBytes;
begin
  // some systems are somewhat careless about how they wrap a packet
  // some systems prepend a #13 to the message
  // some fail to append a #13.
  // some even send HL7 segment delimiters as  #13#10 instead of #13
  result := BytesReplace(packet, Bytes([13, 10]), Bytes([13]));
  if length(result) > 0 then
  begin
    while result[0] = TByte(13) do
      result := copy(result, 1, length(result) -1);
    while result[length(result)-1] = TByte(13) do
      SetLength(result, length(result) - 1);
    result := BytesAdd(result, TByte(13));
  end;
end;

function ToBoolean(const s: String): Boolean;
begin
  Result := (s <> '') and (s <> '0') and (s <> 'F') and (s <> 'f') and (s.ToLower <> 'false');
end;

Procedure THL7V2ER7Decoder.ReadBinary(oCell : THL7V2Cell; Const sContent : String; var iCursor : integer);
var
  sBuffer : String;
  iStart : Integer;
Begin
  inc(iCursor); // skip the X at the start
  iStart := iCursor;
  While (iCursor <= Length(sContent)) And Not (sContent[iCursor] = FDelimiters.EscapeCharacter) Do
    Inc(iCursor);
  If (iCursor > length(sContent)) Then
    RaiseError('ReadBinary', 'unterminated binary escape in '+sContent);
  sBuffer := copy(sContent, iStart, iCursor - iStart);
  if Length(sBuffer) mod 2 = 1 then
    If Not FDecodingOptions.PreventOddBinaryLengths Then
      sBuffer := '0'+sBuffer
    Else
      RaiseError('ReadBinary', 'Odd length of binary escape in "'+sContent+'"');

  oCell.Contents.add(THL7V2ContentBinary.create(HexDecode(sBuffer)));
End;

Procedure THL7V2ER7Decoder.ReadEscape(oCell : THL7V2Cell; Const sContent : String; var iCursor : integer);
var
  iStart : Integer;
Begin
  iStart := iCursor;
  While (iCursor <= Length(sContent)) And Not (sContent[iCursor] = FDelimiters.EscapeCharacter) Do
    Inc(iCursor);
  If (iCursor > length(sContent)) Then
  Begin
    // very often turkeys sending us the escape character do not escape.
    // rather than raising the error, we are going to treat the escape
    // as an escape
    // RaiseError('ReadBinary', 'unterminated escape in '+sContent);
    iCursor := iStart - 1;
    oCell.Contents.add(THL7V2ContentText.create(FDelimiters.EscapeCharacter));
  End
  Else
    oCell.Contents.add(THL7V2ContentEscape.create(copy(sContent, iStart, iCursor - iStart)));
End;

Procedure THL7V2ER7Decoder.ReadCellContent(oCell : THL7V2Cell; Const sContent : String; bNoEscape : Boolean);
var
  buf : TFslStringBuilder;
  iCursor : Integer;
  Procedure Commit();
  Begin
    if buf.Length > 0 Then
      Begin
      oCell.Contents.Add(THL7V2ContentText.create(buf.AsString));
      buf.Clear;
      End;
  End;
Begin
  oCell.ClearContent;
  if (sContent = FDelimiters.EscapeCharacter) Then
    oCell.RawContent := sContent
  Else If (sContent = '""') then
    oCell.Contents.add(THL7V2ContentNull.Create())
  Else If bNoEscape or (pos(FDelimiters.EscapeCharacter, sContent) = 0) Then
  Begin
    If sContent <> '' Then
      oCell.Contents.Add(THL7V2ContentText.create(sContent));
  End
  Else
  Begin
    buf := TFslStringBuilder.Create;
    try
      iCursor := 1;
      While (iCursor <= Length(sContent)) Do
        Begin
        if (iCursor < Length(sContent)) and (sContent[iCursor] = FDelimiters.EscapeCharacter) Then
          Begin
          inc(iCursor);
          if (FDelimiters.isDelimiterEscape(sContent[iCursor])) Then
            Begin
            buf.append(FDelimiters.getDelimiterEscapeChar(sContent[iCursor]));
            inc(iCursor);
            if sContent[iCursor] <> FDelimiters.EscapeCharacter Then
              dec(iCursor);
              // cause it will be inc next. this is really an error, an improperly terminated escape sequence,
              // but we do not report it
            End
          Else If (sContent[iCursor] = 'X') Then
            Begin
            Commit;
            readBinary(oCell, sContent, iCursor);
            End
          Else
            Begin
            Commit;
            readEscape(oCell, sContent, iCursor);
            End;
          End
        Else
          buf.append(sContent[iCursor]);
        inc(iCursor);
        End;
      Commit;
    finally
      buf.Free;
    end;
  End;
End;

procedure THL7V2ER7Decoder.ReadDelimiters;
var
  sLeft : String;
  sRight : String;
begin
  // basics: this checks that the MSH appears to be valid, records the delimiters,
  // and checks the version
  if Not SameBytes(copy(FSource, 0, 3), 'MSH') then
    ErrorBadMessage('ReadDelimiters', 'Packet "' + BytesAsMime(copy(FSource, 0, 12)) + '"does not appear to be valid HL7/ER7: starting MSH not found');
  if not BytesContains(FSource, Tbyte(13)) then
    ErrorBadMessage('ReadDelimiters', 'Packet does not appear to be valid HL7/ER7: Segment Delimiter not found');

  FDelimiters.FieldDelimiter := Char(FSource[3]);
  StringSplit(BytesAsString(FSource), FDelimiters.FieldDelimiter, sLeft, sRight);
  StringSplit(sRight, FDelimiters.FieldDelimiter, sLeft, sRight);

  FDelimiters.ComponentDelimiter := GetDelimiter('Component', sLeft, 1);
  FDelimiters.RepetitionDelimiter := GetDelimiter('Repetition', sLeft, 2);
  FDelimiters.EscapeCharacter := GetDelimiter('Escape', sLeft, 3);
  if Length(sLeft) > 3 then
    FDelimiters.SubComponentDelimiter := GetDelimiter('SubComponent', sLeft, 4)
  Else
    FDelimiters.SubComponentDelimiter := DEFAULT_DELIMITER_SUBCOMPONENT;
  FDelimiters.Check;
end;

procedure THL7V2ER7Decoder.DecodeMessage(oMessage: THL7V2Message; const sContent: TBytes; oOptions: THL7V2DecodingOptions);
var
  iCursor : Integer;
  iLimit : integer;
begin
  oMessage.Format := hfER7;
  FSource := CleanPacket(sContent);
  if assigned(oOptions) then
    FDecodingOptions.Assign(oOptions)
  else
    FDecodingOptions.Defaults;

  // 1. initial check of MSH: version and delimiters
  ReadDelimiters;
  oMessage.Delimiters.assign(FDelimiters);
  oMessage.Version := ReadMessageVersion(oOptions);

  // 2. Decode MSH
  iCursor := 0;
  ParseSegment(oMessage, iCursor);

  // 3. Post MSH processing
  oMessage.BindToMSH;

  // 4. decode remaining segments
  iLimit := FDecodingOptions.SegmentLimit;
  while (iCursor < length(FSource)) and ((iLimit = 0) or (oMessage.Segments.Count < iLimit)) do
    ParseSegment(oMessage, iCursor);
end;

function THL7V2ER7Decoder.ReadMessageVersion(oOptions: THL7V2DecodingOptions): THL7V2Version;
var
  sSegment : String;
  sCode : String;
  iOffset : integer;
  iCount : integer;
begin
  result := hv21;
  if assigned(oOptions) and (oOptions.VersionOverride <> '') then
    sCode := oOptions.VersionOverride
  else
    begin
    StringSplit(BytesAsString(FSource), #13, sSegment, sCode);
    iOffset := 1;
    iCount := 0;
    while (iCount < 11) and (iOffset <= length(sSegment)) do
      begin
      if sSegment[iOffset] = FDelimiters.FieldDelimiter then
        inc(iCount);
      inc(iOffset);
      end;
    StringSplit(copy(sSegment, iOffset, $FF), [FDelimiters.FieldDelimiter, FDelimiters.ComponentDelimiter, FDelimiters.RepetitionDelimiter, FDelimiters.EscapeCharacter, FDelimiters.SubComponentDelimiter], sCode, sSegment);
    end;

  if StringArrayExists(NAMES_HL7V2_VERSION, sCode) then
    result := FromVersionCode(sCode)
  else
    ErrorUnsVersion('ReadMessageVersion', 'Message Version "'+sCode+'" unknown');
end;

procedure THL7V2ER7Decoder.ParseSegment(oMessage: THL7V2Message; var iCursor: Integer);
var
  sSegCode : String;
  oSegment : THL7V2Segment;
begin
  if (iCursor + 3 > length(FSource)) then //we are either missing a #13 or have an invalid segment code
    ErrorBadMessage('ParseSegment', 'Remaining length in message too short for a valid segment.' + #13 +
                         'Remainder of packet: ' + StringReplace(copy(BytesAsString(FSource), iCursor, length(FSource)), #13, '<CR>'));

  //recognise segment
  sSegCode := BytesAsString(copy(FSource, iCursor, 3));
  inc(iCursor, 3);

  if not StringIsAlphanumeric(sSegCode) then //check that the segcode is a valid length (length must be 3 chars)
    ErrorBadMessage('ParseSegment', 'Segment code too short or contains invalid content: ' + StringReplace(sSegCode, #13, '<CR>'));

  if not StringArrayExists(['MSH', 'FHS', 'BHS'], sSegCode) and (FSource[iCursor] <> 13) then
      inc(iCursor); // special case: field is it's own delimiter for first MSH field

  oSegment := THL7V2Segment.Create;
  try
    oMessage.Segments.Add(oSegment.Link); // do this to provide a model before setting the code
  finally
    oSegment.Free;
  end;

  oSegment.Code := sSegCode;
  oSegment.BuildFields;

  ParseSegmentInner(oSegment, iCursor);
end;

procedure THL7V2ER7Decoder.ParseSegmentInner(oSegment: THL7V2Segment; var iCursor: Integer);
var
  oField : THL7V2DataElement;
  iLoop : Integer;
begin
  for iLoop := 0 to oSegment.Fields.Count - 1 do
    ParseDataElement(oSegment.Fields[iLoop], iCursor);

  // special hacks for MSH, etc
  if StringArrayExists(['MSH', 'FHS', 'BHS'], oSegment.Code) then
    begin
    oSegment.Fields[0].RawContent := FDelimiters.FieldDelimiter;
    oSegment.Fields[1].Repeats.Clear;
            oSegment.Fields[1].Components.Clear;
    oSegment.Fields[1].RawContent := FDelimiters.ComponentDelimiter + FDelimiters.RepetitionDelimiter + FDelimiters.EscapeCharacter + FDelimiters.SubComponentDelimiter;
    end;

  while FSource[iCursor] <> 13 do
    begin
    oField := THL7V2DataElement.create(ctDataElement, oSegment.Fields.Count + 1);
    try
      oSegment.Fields.Add(oField.Link);
      ParseDataElement(oField, iCursor);
    finally
      oField.Free;
    end;
    end;

  while (iCursor < Length(FSource)) and (FSource[iCursor] = 13) do
    inc(iCursor);
end;

Function BytesSlice(Const sValue : TBytes; iBegin, iEnd : Integer) : TBytes;
Begin
  Result := Copy(sValue, iBegin, iEnd - iBegin + 1);
End;

procedure THL7V2ER7Decoder.ParseDataElement(oDataElement: THL7V2DataElement; var iCursor : Integer);
var
  oFocus : THL7V2DataElement;
  iStart : Integer;
  bRepeat : Boolean;
  bEscape : Boolean;
//  LContent: String;
begin
  oFocus := oDataElement;
  repeat
    iStart := iCursor;
    bRepeat := False;
    bEscape := False;
    while (iCursor < length(FSource)) and not (FSource[iCursor] in [13, ord(FDelimiters.FieldDelimiter), ord(FDelimiters.RepetitionDelimiter)]) do
    Begin
      bEscape := bEscape or (FSource[iCursor] = ord(FDelimiters.EscapeCharacter));
      inc(iCursor);
    End;
    ParseCell(oFocus, BytesAsString(BytesSlice(FSource, iStart, iCursor-1)), not bEscape);
    if (iCursor < length(FSource)) and (FSource[iCursor] = ord(FDelimiters.RepetitionDelimiter)) then
      begin
      inc(iCursor);
      bRepeat := True;
      oFocus := oDataElement.AddRepeat(False);
      end;
  until not bRepeat;
  if (iCursor < length(FSource)) and (FSource[iCursor] <> 13) then
    inc(iCursor);
end;

procedure THL7V2ER7Decoder.ParseCell(oCell : THL7V2Cell; const sContent : String; bNoEscape : Boolean);
var
  cFirst : Char;
  cSecond : Char;
  oChild : THL7V2Cell;
  iCount : integer;
  sLeft : String;
  sRight : String;
begin

  cFirst := #0;
  cSecond := #0;
  case oCell.CellType of
    ctUnknown : ErrorHL7Library('ParseCell', 'The Type of the cell has not been assigned');
    ctSubComponent :
      begin
      cFirst := #0;
      cSecond := #0;
      end;
    ctDataElement :
      begin
      cFirst := FDelimiters.ComponentDelimiter;
      cSecond := FDelimiters.SubComponentDelimiter;
      end;
    ctComponent :
      begin
      cFirst := FDelimiters.SubComponentDelimiter;
      cSecond := #0;
      end;
  end;

  // if children are defined or a seperator is found, then we will break content up into children
  if oCell.HasChildren or (pos(cFirst, sContent) > 0) or ((cSecond <> #0) and (pos(cSecond, sContent) > 0)) then
    begin
    oCell.Defined := False;
    iCount := 1;
    sRight := sContent;
    while sRight <> '' do
      begin
      StringSplit(sRight, cFirst, sLeft, sRight);
      oChild := oCell.ForceChild(iCount);
      ParseCell(oChild, sLeft, bNoEscape);
      inc(iCount);
      end;
    end
  else
    begin
    if pos(#0, sContent) > 0 Then
      sLeft := StringStrip(sContent, [#0]) // yes, we have encountered a system that drops #0's randomly into the message.....
    Else
      sLeft := sContent;
    ReadCellContent(oCell, sLeft, bNoEscape);
    oCell.Commit;
    end;
end;

procedure THL7V2ER7Decoder.ParseCellEx(oCell : THL7V2Cell; iStart : integer; const sContent : String; bNoEscape : Boolean; aLevelOperation : THL7v2DecodingLevelOperation);
var
  cFirst : Char;
  cSecond : Char;
  oChild : THL7V2Cell;
  iCount : integer;
  sLeft : String;
  sRight : String;
begin
  cFirst := #0;
  cSecond := #0;
  case oCell.CellType of
    ctUnknown : ErrorHL7Library('ParseCellEx', 'The Type of the cell has not been assigned');
    ctSubComponent :
      begin
      case aLevelOperation of
        DecodeLevelNormal : Begin   cFirst := #0;    cSecond := #0; End;
        DecodeLevelDown : Begin   cFirst := FDelimiters.SubComponentDelimiter;    cSecond := #0; End;
        DecodeLevelUp : ErrorHL7Library('ParseCellEx', 'Can''t decode up to a sub component');
      End;
      end;
    ctDataElement :
      begin
      case aLevelOperation of
        DecodeLevelNormal : Begin   cFirst := FDelimiters.ComponentDelimiter;     cSecond := FDelimiters.SubComponentDelimiter; End;
        DecodeLevelDown : ErrorHL7Library('ParseCellEx', 'Can''t decode down to an element');
        DecodeLevelUp : Begin   cFirst := FDelimiters.SubComponentDelimiter;    cSecond := #0; End;
      End;
      end;
    ctComponent :
      begin
      case aLevelOperation of
        DecodeLevelNormal : Begin   cFirst := FDelimiters.SubComponentDelimiter;    cSecond := #0; End;
        DecodeLevelDown : Begin   cFirst := FDelimiters.ComponentDelimiter;     cSecond := #0; End;
        DecodeLevelUp : Begin   cFirst := #0;    cSecond := #0; End;
      End;
      end;
  end;

  // if children are defined or a seperator is found, then we will break content up into children
  if oCell.HasChildren or (pos(cFirst, sContent) > 0) or ((cSecond <> #0) and (pos(cSecond, sContent) > 0)) then
    begin
    oCell.Defined := False;
    iCount := 1 + iStart;
    sRight := sContent;
    while sRight <> '' do
      begin
      StringSplit(sRight, cFirst, sLeft, sRight);
      oChild := oCell.ForceChild(iCount);
      ParseCell(oChild, sLeft, bNoEscape);
      inc(iCount);
      end;
    end
  else
    begin
    if pos(#0, sContent) > 0 Then
      sLeft := StringStrip(sContent, [#0]) // yes, we have encountered a system that drops #0's randomly into the message.....
    Else
      sLeft := sContent;
    ReadCellContent(oCell, sLeft, bNoEscape);
    oCell.Commit;
    end;
end;

procedure THL7V2ER7Decoder.DecodeSegment(oSegment: THL7V2Segment; const sContent: TBytes; oOptions: THL7V2DecodingOptions);
var
  iCursor : integer;
  sCode : String;
begin
  FSource := CleanPacket(sContent);
  if assigned(oOptions) then
    FDecodingOptions.Assign(oOptions)
  else
    FDecodingOptions.Defaults;

  // we are going to assume that the delimiters match the default delimiters and that the version matches

  if length(sContent) < 3 then
    ErrorBadMessage('DecodeSegment', 'Message length is too short: '+BytesAsString(sContent));

  sCode := BytesAsString(copy(sContent, 0, 3));
  if not StringIsAlphanumeric(sCode) and not isUppercase(sCode) then
    ErrorBadMessage('DecodeSegment', 'Segment Type illegal: '+sCode);

  if (oSegment.Code <> '') And (sCode <> oSegment.Code) then
    ErrorBadMessage('DecodeSegment', 'Cannot decode a different segment type over the top of an existing segment (is "'+oSegment.Code+'" decoding "'+BytesAsString(copy(sContent, 0, 3))+'")');

  iCursor := 5; // jump the first "|"

  ParseSegmentInner(oSegment, iCursor);
  // ignore anything left over
end;

procedure THL7V2ER7Decoder.DecodeCell(oCell: THL7V2Cell; const sContent: TBytes; oOptions: THL7V2DecodingOptions);
begin
  if assigned(oOptions) then
    FDecodingOptions.Assign(oOptions)
  else
    FDecodingOptions.Defaults;

  // delimiters must be default delimiters
  ParseCell(oCell, BytesAsString(sContent), false);
end;

procedure THL7V2ER7Decoder.DecodeDataElement(oDataElement: THL7V2DataElement; const sContent: TBytes; oOptions: THL7V2DecodingOptions);
var
  iCursor : Integer;
begin
  if assigned(oOptions) then
    FDecodingOptions.Assign(oOptions)
  else
    FDecodingOptions.Defaults;

  // delimiters must be default delimiters
  FSource := sContent;
  iCursor := 0;
  ParseDataElement(oDataElement, iCursor);
end;

procedure THL7V2ER7Decoder.DecodeFile(oFile: THL7V2File; oStream: TFslStream; oOptions : THL7V2DecodingOptions);
var
  oExtractor : TFslByteExtractor;
  iCursor, iTotal : Integer;
  sLine : String;
  oBatch : THL7V2Batch;
begin
  if assigned(oOptions) then
    FDecodingOptions.Assign(oOptions)
  else
    FDecodingOptions.Defaults;

  oFile.Clear;
  oFile.Version := hv25;
  oExtractor := TFslByteExtractor.create;
  try
    oExtractor.stream := oStream.Link;
    FSource := BytesAdd(oExtractor.ConsumeLine, TByte(13));
    if BytesAsString(copy(FSource, 0, 3)) = 'MSH' then
    begin
      // special case - a series of messages
      oBatch := THL7V2Batch.create(oFile);
      oFile.Batches.add(oBatch.Link);
      oBatch.Version := oFile.Version;

      iTotal := oExtractor.Stream.Readable;
      sLine := BytesAsString(FSource)+#13;
      while (copy(sLine, 1, 3) = 'MSH') do
      Begin
        sLine := DecodeBatchMessage(sLine, oBatch, oExtractor);
        If Assigned(FDecodingOptions.BatchProgressEvent) Then
          FDecodingOptions.BatchProgressEvent('Loading', oExtractor.StreamPosition, iTotal);
      End;
    end
    else
    begin
      if BytesAsString(copy(FSource, 0, 3)) <> 'FHS' then
        ErrorSequence('DecodeFile', 'File does not start with a FHS segment: '+BytesAsString(FSource));
      iCursor := 4;
      ParseSegmentInner(oFile.Header, iCursor);

      sLine := BytesAsString(oExtractor.ConsumeLine)+#13;
      while (copy(sLine, 1, 3) <> 'FTS') do
        sLine := DecodeBatch(sLine, oFile, oExtractor);

      FSource := StringAsBytes(sLine);
      iCursor := 5;
      ParseSegmentInner(oFile.Trailer, iCursor);
    end;
  finally
    oExtractor.Free;
  end;
end;

function THL7V2ER7Decoder.DecodeBatch(sLine : String; oFile: THL7V2File; oExtractor: TFslByteExtractor): String;
var
  oBatch : THL7V2Batch;
  iCursor : Integer;
  iTotal : Integer;
begin
  iTotal := oExtractor.Stream.Readable;

  oBatch := THL7V2Batch.create(oFile);
  try
    oFile.Batches.add(oBatch.Link);
    oBatch.Version := oFile.Version;
    if not (copy(sLine, 1, 3) = 'BHS') then
      ErrorSequence('decodeBatch', 'Expected BHS segment but found '+sLine);
    FSource := StringAsBytes(sLine);
    iCursor := 4;
    ParseSegmentInner(oBatch.Header, iCursor);

    sLine := BytesAsString(oExtractor.ConsumeLine)+#13;
    while (copy(sLine, 1, 3) = 'MSH') do
    Begin
      sLine := DecodeBatchMessage(sLine, oBatch, oExtractor);
      If Assigned(FDecodingOptions.BatchProgressEvent) Then
        FDecodingOptions.BatchProgressEvent('Loading', oExtractor.StreamPosition, iTotal);
    End;

    if not (copy(sLine, 1, 3) = 'BTS') then
      ErrorSequence('decodeBatch', 'Expected BTS segment but found '+sLine);

    FSource := StringAsBytes(sLine);
    iCursor := 5;
    ParseSegmentInner(oBatch.Trailer, iCursor);

    result := BytesAsString(oExtractor.ConsumeLine)+#13;

  finally
    oBatch.Free;
  end;
end;

function THL7V2ER7Decoder.DecodeBatchMessage(sLine: String; oBatch: THL7V2Batch; oExtractor: TFslByteExtractor): String;
var
  oBatchMessage : THL7V2BatchMessage;
begin
  oBatchMessage := THL7V2BatchMessage.create;
  try
    oBatchMessage.Content := StringAsBytes(sLine);

    sLine := BytesAsString(oExtractor.ConsumeLine)+#13;
    while not StringArrayExists(['MSH', 'BTS', 'FHS', 'BHS', 'FTS'], copy(sLine, 1, 3)) and not (sLine = #13) do
      begin
      oBatchMessage.Content := StringAsBytes(BytesAsString(oBatchMessage.Content) + sLine);
      sLine := BytesAsString(oExtractor.ConsumeLine)+#13;
      end;

    If (assigned(FDecodingOptions) and FDecodingOptions.DecodeBatchMessages) Then
    Begin
      oBatchMessage.Decode(FDecodingOptions);
      oBatchMessage.Id := oBatchMessage.Message.MessageID;
    End;
    oBatch.Messages.add(oBatchMessage.Link);
    result := sLine;
  finally
    oBatchMessage.Free;
  end;
end;

procedure THL7V2ER7Decoder.DecodeCellEx(oCell: THL7V2Cell; iStart: integer; const sContent: TBytes; oOptions: THL7V2DecodingOptions; aLevelOperation : THL7v2DecodingLevelOperation);
begin
  FDecodingOptions.Assign(oOptions);
  // delimiters must be default delimiters
  ParseCellEx(oCell, iStart, BytesAsString(sContent), false, aLevelOperation);
end;

function THL7V2ER7Decoder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDecodingOptions.sizeInBytes);
  inc(result, FDelimiters.sizeInBytes);
end;

{ THL7V2ER7Encoder }

Constructor THL7V2ER7Encoder.Create;
begin
  inherited;
  FEncodingOptions := THL7V2EncodingOptions.create;
  FDelimiters := THL7V2Delimiters.create;
  FStream := TMemoryStream.Create;
end;

Destructor THL7V2ER7Encoder.Destroy;
begin
  FDelimiters.Free;
  FStream.Free;
  FEncodingOptions.Free;
  inherited;
end;

function THL7V2ER7Encoder.EncodeMessage(oMessage: THL7V2Message; oOptions: THL7V2EncodingOptions): TBytes;
var
  iLoop : integer;
begin
  FDelimiters.assign(oMessage.Delimiters);
  FDelimiters.Check;
  Result := Bytes([]);
  FStream.Clear;
  for iLoop := 0 to oMessage.Segments.Count - 1 do
    FormatSegment(oMessage.Segments[iLoop], oOptions);
  Result := ReadStream;
end;

Procedure THL7V2ER7Encoder.FormatSegment(oSegment: THL7V2Segment; oOptions: THL7V2EncodingOptions);
var
  iLoop : integer;
  iStart : Integer;
  iEnd : integer;
  ch : Char;
  //oField : THL7V2DataElement;
begin
  WriteChars(oSegment.Code);
  iStart := 0;
  if StringArrayExists(['MSH', 'FHS', 'BHS'], oSegment.Code) then
    begin
    WriteChars(FDelimiters.FieldDelimiter);
    WriteChars(FDelimiters.ComponentDelimiter);
    WriteChars(FDelimiters.RepetitionDelimiter);
    WriteChars(FDelimiters.EscapeCharacter);
    WriteChars(FDelimiters.SubComponentDelimiter);
    iStart := 2;
    end;
  iEnd := oSegment.Fields.Count - 1;
  While (iEnd > iStart) and Not oSegment.Fields[iEnd].IsRelevent Do
    dec(iEnd);
  for iLoop := iStart to iEnd do
    begin
    WriteChars(FDelimiters.FieldDelimiter);
    FormatDataElement(oSegment.Fields[iLoop], oOptions);
    end;
  if assigned(oOptions) and oOptions.ExtraFieldDelimiter then
    WriteChars(FDelimiters.FieldDelimiter);
  ch := #13;
  WriteChars(ch);
end;

Procedure THL7V2ER7Encoder.Escape(sContent: String; oOptions: THL7V2EncodingOptions);
var
  iLoop : Integer;
  bEscape : Boolean;
  s : String;
begin
  bEscape := false;
  iLoop := 1;
  // this loop is very much faster than the lower one, and we don't need to escape very often
  While not bEscape and (iLoop <= length(sContent)) Do
  Begin
    bEscape := CharInSet(sContent[iLoop], [FDelimiters.FieldDelimiter, FDelimiters.ComponentDelimiter, FDelimiters.SubComponentDelimiter,
       FDelimiters.RepetitionDelimiter, FDelimiters.EscapeCharacter, #10, #13, #9]) Or
       ((Ord(sContent[iLoop]) >= 128)) and (assigned(oOptions) and oOptions.EscapeExtendedCharacters);
    inc(iLoop);
  End;

  if bEscape Then
  Begin
    for iLoop := 1 to length(sContent) do
    begin
      if sContent[iLoop] = FDelimiters.FieldDelimiter then
        s := FDelimiters.EscapeCharacter + 'F' + FDelimiters.EscapeCharacter
      else if sContent[iLoop] = FDelimiters.ComponentDelimiter then
        s := FDelimiters.EscapeCharacter + 'S' + FDelimiters.EscapeCharacter
      else if sContent[iLoop] = FDelimiters.SubComponentDelimiter then
        s := FDelimiters.EscapeCharacter + 'T' + FDelimiters.EscapeCharacter
      else if sContent[iLoop] = FDelimiters.RepetitionDelimiter then
        s := FDelimiters.EscapeCharacter + 'R' + FDelimiters.EscapeCharacter
      else if sContent[iLoop] = FDelimiters.EscapeCharacter then
        s := FDelimiters.EscapeCharacter + 'E' + FDelimiters.EscapeCharacter
      else if (StringArrayExists([#10, #13, #9], sContent[iLoop]) or (ord(sContent[iLoop]) >= 128)) and (assigned(oOptions) and oOptions.EscapeExtendedCharacters) then
        s := FDelimiters.EscapeCharacter + 'X' + IntToHex(Ord(sContent[iLoop]), 2) + FDelimiters.EscapeCharacter
      else
        s := sContent[iLoop];
      WriteChars(s);
    end;
  End
  Else if sContent <> '' Then
    WriteChars(sContent);
end;

Procedure THL7V2ER7Encoder.buildComplexContent(oCell: THL7V2Cell; oOptions: THL7V2EncodingOptions);
var
  iIndex : integer;
  oContent : THL7V2Content;
  s : String;
Begin
  For iIndex := 0 to oCell.Contents.Count - 1 do
    Begin
    oContent := oCell.Contents[iIndex];
    If (oContent.ContentType = CONTENT_TYPE_NULL) Then
      RaiseError('buildComplexContent', 'cannot find a null here')
    Else If (oContent.ContentType = CONTENT_TYPE_TEXT) Then
      escape(oContent.Text, oOptions)
    Else if (oContent.ContentType = CONTENT_TYPE_ESCAPE) Then
    Begin
      WriteChars(FDelimiters.EscapeCharacter);
      s := oContent.Text;
      WriteChars(s);
      WriteChars(FDelimiters.EscapeCharacter);
    End
    Else if (oContent.ContentType = CONTENT_TYPE_BINARY) Then
    Begin
      WriteChars(FDelimiters.EscapeCharacter);
      s := 'X';
      WriteChars(s);
      s := HexEncode(oContent.Text);
      WriteChars(s);
      WriteChars(FDelimiters.EscapeCharacter);
    End
    Else
      RaiseError('buildComplexContent', 'unknown content type');
    End;
End;

Procedure THL7V2ER7Encoder.FormatCell(oCell: THL7V2Cell; oOptions: THL7V2EncodingOptions);
var
  iLoop : integer;
  cChar : string;
  iEnd : Integer;
  s : String;
begin
  case oCell.CellType of
    ctUnknown : ErrorHL7Library('FormatCell', 'Cell type is not set at '+oCell.FullPathName);
    ctDataElement : cChar := FDelimiters.ComponentDelimiter;
    ctComponent : cChar := FDelimiters.SubComponentDelimiter;
  else //  ctSubComponent
    cChar := #0;
  end;

  if oCell.HasContent then
  Begin
    if oCell.HasChildren then
    begin
      FormatCell(oCell.Children[0], oOptions);
      iEnd := oCell.Children.Count - 1;
      While (iEnd > 0) And not oCell.Children[iEnd].Defined Do
        dec(iEnd);
      for iLoop := 1 to iEnd do
      begin
        if cChar = #0 then
          ErrorHL7Library('FormatCell', 'Attempt to encode sub-sub-components at '+oCell.FullPathName);
        WriteChars(cChar);
        FormatCell(oCell.Children[iLoop], oOptions);
      end;
    end
    else if oCell.IsNullify Then
    Begin
      s := '""';
      WriteChars(s)
    End
    else if oCell.IsSimple Then
      Escape(oCell.RawContent, oOptions)
    else
      BuildComplexContent(oCell, oOptions);
  End;
end;

Procedure THL7V2ER7Encoder.FormatDataElement(oDataElement: THL7V2DataElement; oOptions: THL7V2EncodingOptions);
var
  iLoop : integer;
begin
  FormatCell(oDataElement, oOptions);

  if oDataElement.HasRepeats then
    for iLoop := 0 to oDataElement.Repeats.Count - 1 do
    Begin
      WriteChars(FDelimiters.RepetitionDelimiter);
      FormatCell(oDataElement.Repeats[iLoop], oOptions);
    End;
end;

function THL7V2ER7Encoder.EncodeCell(oCell: THL7V2Cell; oOptions: THL7V2EncodingOptions): TBytes;
begin
  if oCell = nil then
    result := Bytes([])
  else if oCell.CellType = ctDataElement then
  Begin
    FStream.Clear;
    FormatDataElement(THL7V2DataElement(oCell), oOptions);
    result := ReadStream;
  End
  else
  Begin
    FStream.Clear;
    FormatCell(oCell, oOptions);
    result := ReadStream;
  End;
end;

function THL7V2ER7Encoder.EncodeDataElement(oDataElement: THL7V2DataElement; oOptions: THL7V2EncodingOptions): TBytes;
begin
  if oDataElement = nil then
    result := Bytes([])
  else
  Begin
    FStream.Clear;
    FormatDataElement(oDataElement, oOptions);
    result := ReadStream;
  End;
end;

function THL7V2ER7Encoder.EncodeSegment(oSegment: THL7V2Segment; oOptions: THL7V2EncodingOptions): TBytes;
begin
  FStream.Clear;
  FormatSegment(oSegment, oOptions);
  result := ReadStream;
end;

procedure THL7V2ER7Encoder.EncodeFile(oFile: THL7V2File; oStream: TFslStream; oOptions : THL7V2EncodingOptions);
var
  oFormatter : THL7V2ER7Formatter;
  iLoop : integer;
begin
  oFormatter := THL7V2ER7Formatter.create;
  try
    oFormatter.Stream := oStream.Link;
    FStream.Clear;
    FormatSegment(oFile.Header, oOptions);
    oFormatter.ProduceContent(ReadStream);
    for iLoop := 0 to oFile.Batches.Count - 1 do
      EncodeBatch(oFile.Batches[iLoop], oFormatter, oOptions);
    FStream.Clear;
    FormatSegment(oFile.Trailer, oOptions);
    oFormatter.ProduceContent(ReadStream);
  finally
    oFormatter.Free;
  end;
end;

procedure THL7V2ER7Encoder.EncodeBatch(oBatch: THL7V2Batch; oFormatter: THL7V2ER7Formatter; oOptions: THL7V2EncodingOptions);
var
  iLoop : integer;
begin
  FStream.Clear;
  FormatSegment(oBatch.Header, oOptions);
  oFormatter.ProduceContent(ReadStream);
  for iLoop := 0 to oBatch.Messages.count - 1 do
  Begin
    oFormatter.ProduceContent(oBatch.Messages[iLoop].Content);
    If Assigned(oOptions) And Assigned(oOptions.BatchProgressEvent) Then
      oOptions.BatchProgressEvent('Saving', iLoop, oBatch.Messages.count);
  End;
  FStream.Clear;
  FormatSegment(oBatch.Trailer, oOptions);
  oFormatter.ProduceContent(ReadStream);
end;

function THL7V2ER7Encoder.ReadStream: TBytes;
begin
  SetLength(result, FStream.Size);
  if FStream.Size > 0 Then
    Move(FStream.Memory^, result[0], FStream.Size);
end;

procedure THL7V2ER7Encoder.WriteChars(s: String);
var
  i : integer;
  c : AnsiChar;
begin
  for i := 1 to length(s) do
  begin
    c := AnsiChar(s[i]);
    FStream.Write(c, 1);
  end;
end;

function THL7V2ER7Encoder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FEncodingOptions.sizeInBytes);
  inc(result, FDelimiters.sizeInBytes);
end;

{ THL7V2XMLDecoder }

Destructor THL7V2XMLDecoder.Destroy;
Begin
  FOptions.Free;
  Inherited;
End;

Function THL7V2XMLDecoder.SuppressErrors: Boolean;
Begin
  Result := Assigned(FOptions) And FOptions.SuppressErrors;
End;

Procedure THL7V2XMLDecoder.XMLCheck(bCondition: Boolean; Const sMethod, sMessage: String);
Begin
  If Not bCondition Then
    ErrorXML(sMethod, sMessage);
End;

Function  THL7V2XMLDecoder.LoadDom(Const sContent: TBytes) : TMxMLDocument;
Begin
  result := TMXmlParser.parse(sContent, [xpResolveNamespaces, xpDropWhitespace, xpDropComments]);
End;

Procedure THL7V2XMLDecoder.DecodeMessage(oMessage: THL7V2Message; Const sContent: TBytes; oOptions: THL7V2DecodingOptions);
Var
  oDom : TMxMLDocument;
  oRoot : TMXmlElement;
  sStructID: String;
  oNode: TMXmlElement;
Begin
  FOptions := oOptions.Link;
  oMessage.Format := hfXML;
  oDom := LoadDom(sContent);
  oRoot := oDom.docElement;
  sStructID := oRoot.name;
  XMLCheck(SuppressErrors Or (((Length(sStructID) = 7) And (sStructID[4] = '_')) Or (Length(sStructID) = 3)), 'DecodeMessage', 'Base Root Node should have an HL7 structure name, but is "' + sStructID + '"');

  oNode := oRoot.firstElement;
  XMLCheck(Assigned(oNode), 'DecodeMessage', 'Unable to find a first child element');
  XMLCheck(oNode.name = 'MSH', 'DecodeMessage', 'The First Segment is "' + oNode.name + '", but should be "MSH"');
  PreProcessMSHSegment(oMessage, oNode As TMXmlElement);
  ReadSegment(oMessage, oNode As TMXmlElement);
  oMessage.BindToMSH;
  ReadDelimiters(oMessage);
  ReadSegmentSeries(oMessage, oNode.nextElement);
  XMLCheck(SuppressErrors Or (sStructID = oMessage.StructName), 'DecodeMessage', 'Message Structure was "' + sStructID + '", but content calls for structure "' + oMessage.StructName + '"');
End;

function getFirstChildElement(element : TMXmlElement; name : String) : TMXmlElement;
var
  node : TMXmlElement;
begin
  result := nil;
  node := element.firstElement;
  while (result = nil) and (node <> nil) do
  begin
    if (node.name = name) then
      result := node as TMXmlElement;
    node := node.nextElement;
  end;
end;

Procedure THL7V2XMLDecoder.PreProcessMSHSegment(oMessage : THL7V2Message; oRoot: TMXmlElement);
Var
  oVer : TMXmlElement;
  sVer: String;
Begin
  If Assigned(FOptions) And (FOptions.VersionOverride <> '') Then
    sVer := FOptions.VersionOverride
  Else
    Begin
    oVer := getFirstChildElement(oRoot, 'MSH.12');
    XMLCheck(Assigned(oVer), 'PreProcessMSHSegment','MSH.12 not found');
    oVer := getFirstChildElement(oVer, 'VID.1');
    XMLCheck(Assigned(oVer), 'PreProcessMSHSegment','MSH.12-VID.1 not found');
    XMLCheck(oVer.first.nodeType = ntText, 'PreProcessMSHSegment','MSH.12-VID.1 found but doens''t contain text?');
    sVer := oVer.first.Text;
    End;

  If StringArrayExists(NAMES_HL7V2_VERSION, sVer) Then
    oMessage.Version := FromVersionCode(sVer)
  Else
    ErrorUnsVersion('ReadMessageVersion', 'Message Version "'+sVer+'" unknown');
End;

Procedure THL7V2XMLDecoder.ReadDelimiters(oMessage : THL7V2Message);
Var
  oMSH : THL7V2Segment;
  sField : String;
Begin
  oMSH := oMessage.Segment['MSH', 0];
  oMessage.Delimiters.FieldDelimiter := GetDelimiter('Field', oMSH.Field[1].AsString, 1);
  sField := oMSH.Field[2].AsString;
  oMessage.Delimiters.ComponentDelimiter := GetDelimiter('Component', sField, 1);
  oMessage.Delimiters.RepetitionDelimiter := GetDelimiter('Repetition', sField, 2);
  oMessage.Delimiters.EscapeCharacter := GetDelimiter('Escape', sField, 3);
  oMessage.Delimiters.SubComponentDelimiter := GetDelimiter('SubComponent', sField, 4);
End;

Procedure THL7V2XMLDecoder.ReadSegmentInner(oSegment: THL7V2Segment; oRoot: TMXmlElement);
Var
  oNode: TMXmlElement;
  sName : String;
  oField : THL7V2DataElement;
  iLast: Integer;
  iIndex: Integer;
Begin
  oNode := oRoot.firstElement;
  iLast := -1;
  While Assigned(oNode) Do
    Begin
    sName := oNode.name;
    If (Copy(sName, 1, 4) = oRoot.name + '.') And StringIsInteger32(Copy(sName, 5, $FF)) Then
      Begin
      iIndex := StringToInteger32(Copy(sName, 5, $FF));
      oField := oSegment.forceField(iIndex);
      If iLast = iIndex Then
        oField := oField.AddRepeat(False)
      Else
        iLast := iIndex;
      ReadCell(oField, oNode As TMXmlElement);
      End
    Else
      XMLCheck(Not SuppressErrors, 'ReadSegment', 'The Node "' + sName + '" is not expected as a element of the "' + oRoot.name + '" Segment');
    oNode := oNode.nextElement;
    End;
End;

Procedure THL7V2XMLDecoder.ReadSegment(oMessage: THL7V2Message; oRoot: TMXmlElement);
Var
  oSeg: THL7V2Segment;
Begin
  oSeg := THL7V2Segment.Create;
  Try
    oMessage.Segments.Add(oSeg.Link);
  Finally
    oSeg.Free;
  End;
  oSeg.Code := oRoot.name;
  oSeg.BuildFields;
  ReadSegmentInner(oSeg, oRoot);
End;

Procedure THL7V2XMLDecoder.ReadCell(oCell : THL7V2Cell; oRoot: TMXmlElement);
Var
  oNode: TMXmlElement;
  bFound: Boolean;
  sName: String;
  sIndex: String;
  sText: String;
  sValue : String;
  oField : THL7V2DataElement;
  oComp : THL7V2Component;
Begin
  bFound := False;
  oNode := oRoot.firstElement;
  While Assigned(oNode) Do
    Begin
    // we may have mixed content
    // if the name of any node has the format name.index then we accept it as a component rather than mixed content
    // once we have a component, then everything must be a component
    StringSplit(oNode.name, '.', sName, sIndex);
    bFound := bFound Or ((sName <> '') And (sIndex <> ''));
    If bFound Then
      Begin
      XMLCheck(SuppressErrors Or (sName <> ''), 'ReadField', 'The Component Name "' + oNode.name + '" at "'+oCell.FullPathName+'" is not right');
      XMLCheck(StringIsInteger32(sIndex), 'ReadField', 'The Component Index "' + sIndex + '" at "'+oCell.FullPathName+'" is not a valid number');

      // name checking
      If oCell.CellType = ctDataElement Then
        Begin
        oField := (oCell As THL7V2DataElement);
        If Assigned(oField.Definition) And Assigned(oField.Definition.RefDataElement) And Assigned(oField.Definition.RefDataElement.RefStructure) Then
          Begin
          If Not StringArrayExistsInsensitive(['varies', 'var'], oField.Definition.RefDataElement.RefStructure.DataType) Then // in this case we never check the name
            If Copy(oField.Definition.RefDataElement.RefStructure.DataType, 1, 2) = 'CE' Then
              XMLCheck(SuppressErrors Or (sName = 'CE'), 'ReadField', 'The Component Name "' + sName + '" is not right. Expected "CE" in field "' + oField.FullPathName + '"')
            Else If (oField.Definition.RefDataElement.RefStructure.DataType <> '*') and (oField.Definition.RefDataElement.RefStructure.DataType <> 'var') Then
              XMLCheck(SuppressErrors Or (sName = oField.Definition.RefDataElement.RefStructure.DataType), 'ReadField', 'The Component Name "' + sName + '" is not right. Expected "'+oField.Definition.RefDataElement.RefStructure.DataType+'" in field "' + oField.FullPathName + '"')
          End
        Else
          Begin
          If oField.SpecifiedDataType = '' Then
            oField.SpecifiedDataType := sName
          Else
            XMLCheck(SuppressErrors Or (oField.SpecifiedDataType = sName), 'ReadField', 'The Component Name "' + sName + '" is not right. Expected "'+oField.SpecifiedDataType+'" in field "' + oField.FullPathName + '"')
          End;
        End
      Else
        Begin
        oComp := oCell As THL7V2Component;

        If Assigned(oComp.Definition) And Assigned(oComp.Definition.RefStructure) Then
          Begin
          If Not StringEquals(THL7V2ModelStructure(oComp.Definition.RefStructure).DataType, 'varies') Then // in this case we never check the name
            If Copy(oComp.Definition.DataType, 1, 2) = 'CE' Then
              XMLCheck(SuppressErrors Or (sName = 'CE'), 'ReadField', 'The Component Name "' + sName + '" is not right. Expected "CE" in field "' + oComp.FullPathName + '"')
            Else
              XMLCheck(SuppressErrors Or (sName = oComp.Definition.DataType), 'ReadField', 'The Component Name "' + sName + '" is not right. Expected "'+THL7V2ModelStructure(oComp.Definition.RefStructure).DataType+'" in field "' + oComp.FullPathName + '"')
          End;
        // else nothing to check
        End;

      ReadCell(oCell.ForceChild(StringToInteger32(sIndex)), oNode As TMXmlElement);
      End;
    oNode := oNode.nextElement;
    End;

  If Not bFound Then
    Begin
    If oRoot.Children.count = 1 Then
      Begin
      XMLCheck(oRoot.first.NodeType = ntText, 'ReadField', 'found a node of type ' + CODES_TMXmlElementType[oRoot.first.NodeType] + ' attached to ' + oRoot.name + ' expecting a text node at '+oCell.FullPathName);
      oCell.RawContent := oRoot.first.text; // no escaping
      End
    Else If oRoot.Children.count > 1 Then
      Begin
      // well, we have mixed content....
      // see comments in ER7 - why not allow esacping anywhere?
      // XMLCheck(SuppressErrors or oCell.Escapable, 'ReadField', 'Encountered a mixed content field where it is not acceptable at ' + oCell.FullPathName + ')');
      oCell.Contents.Clear;
      sText := '';
      oNode := oRoot.first;
      While Assigned(oNode) Do
        Begin
        If oNode.NodeType = ntText Then
          sText := sText + {HL7EscapeHexChars(}oNode.text{)}
        Else If oNode.NodeType = ntElement Then
          Begin
          if (sText <> '') Then
            oCell.Contents.add(THL7V2ContentText.create(sText));
          sText := '';
          XMLCheck((oNode As TMXmlElement).name = 'escape', 'ReadField', 'Encountered unexpected node in mixed content: ' + (oNode As TMXmlElement).name + ' on ' + oRoot.name+ ' at '+oCell.FullPathName);
          sValue := (oNode As TMXmlElement).attribute['V'];
          oCell.Contents.add(THL7V2ContentEscape.create(sValue));
          End
        Else
          XMLCheck(SuppressErrors, 'ReadField', 'Unexpected node type ' + CODES_TMXmlElementType[oNode.NodeType] + ' on ' + oRoot.name+ ' at '+oCell.FullPathName);
        oNode := oNode.next;
        End;
      if (sText <> '') Then
        oCell.Contents.add(THL7V2ContentText.create(sText));
      End;
    End;
End;

Procedure THL7V2XMLDecoder.ReadSegmentSeries(oMessage: THL7V2Message; oNode: TMXmlElement);
Begin
  While Assigned(oNode) Do
    Begin
    If oNode.NodeType = ntElement Then
      Begin
      If Length(oNode.name) = 3 Then
        ReadSegment(oMessage, oNode As TMXmlElement)
      Else
        ReadSegmentSeries(oMessage, oNode.first);
      End;
    oNode := oNode.next;
    End;
End;

Procedure THL7V2XMLDecoder.DecodeSegment(oSegment: THL7V2Segment; Const sContent: TBytes; oOptions: THL7V2DecodingOptions);
Var
  oDom : TMxMLDocument;
  oRoot : TMXmlElement;
Begin
  FOptions := oOptions.Link;
  oDom := LoadDom(sContent);
  oRoot := oDom.docElement;
  XMLCheck(oRoot.name = oSegment.Code, 'DecodeSegment', 'Attempting to read Segment "'+oRoot.name+'" over the top of a segment '+oSegment.Code+', this is not allowed');
  ReadSegmentInner(oSegment, oRoot);
End;

Procedure THL7V2XMLDecoder.DecodeCell(oCell: THL7V2Cell; Const sContent: TBytes; oOptions: THL7V2DecodingOptions);
Var
  oDom : TMxMLDocument;
  oRoot : TMXmlElement;
Begin
  FOptions := oOptions.Link;
  oDom := LoadDom(sContent);
  oRoot := oDom.docElement;
  ReadCell(oCell, oRoot);
End;

Procedure THL7V2XMLDecoder.DecodeDataElement(oDataElement: THL7V2DataElement; Const sContent: TBytes; oOptions: THL7V2DecodingOptions);
Var
  oDom : TMxMLDocument;
  oRoot : TMXmlElement;
Begin
  FOptions := oOptions.Link;
  oDom := LoadDom(sContent);
  oRoot := oDom.docElement;
  ReadCell(oDataElement, oRoot);
End;

procedure THL7V2XMLDecoder.DecodeCellEx(oCell: THL7V2Cell; iStart: integer; const sContent: TBytes; oOptions: THL7V2DecodingOptions; aLevelOperation : THL7v2DecodingLevelOperation);
begin
  ErrorHL7Library('DecodeCellEx', 'not supported in xml');
end;

function THL7V2XMLDecoder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOptions.sizeInBytes);
end;

{ THL7V2XMLEncoder }

Function TextToXML(AStr: String): String;
Var
  i: Integer;
Begin
  Result := '';
  If Length(AStr) <= 0 Then
    Exit;
  i := 1;
  While i <= Length(AStr) Do
    Begin
    Case AStr[i] Of
      '''':
        Result := Result + '&#' + IntegerToString(Ord(AStr[i])) + ';';
      '"':
        Result := Result + '&quot;';
      '&':
        Result := Result + '&amp;';
      '<':
        Result := Result + '&lt;';
      '>':
        Result := Result + '&gt;';
      #13:
        Begin
        If i < Length(AStr) Then
          If AStr[i + 1] = #10 Then
            Inc(i);
        Result := Result + ' ';
        End;
      Else If AStr[i] > '~' Then
  Result := Result + '&#x'+IntToHex(ord(AStr[i]), 2)+';'
      Else
        Begin
        Result := Result + AStr[i];
        End;
      End;
    Inc(i);
    End;
End;

Destructor THL7V2XMLEncoder.Destroy;
Begin
  FOptions.Free;
  Inherited;
End;

Procedure THL7V2XMLEncoder.XMLCheck(bCondition: Boolean; Const sMethod, sMessage: String);
Begin
  If Not bCondition Then
    ErrorXML(sMethod, sMessage);
End;

Function THL7V2XMLEncoder.EncodeCell(oCell: THL7V2Cell; oOptions: THL7V2EncodingOptions): TBytes;
Begin
  FOptions := oOptions.Link;
  Result := StringAsBytes(FormatCell(oCell, 'XXX-N'));
End;

Function THL7V2XMLEncoder.EncodeDataElement(oDataElement: THL7V2DataElement; oOptions: THL7V2EncodingOptions): TBytes;
Begin
  FOptions := oOptions.Link;
  Result := StringAsBytes(FormatCell(oDataElement, 'XXX-N'));
End;

Function THL7V2XMLEncoder.EncodeSegment(oSegment: THL7V2Segment; oOptions: THL7V2EncodingOptions): TBytes;
Begin
  FOptions := oOptions.Link;
  Result := StringAsBytes(FormatSegment(oSegment));
End;

Function THL7V2XMLEncoder.EncodeMessage(oMessage: THL7V2Message; oOptions: THL7V2EncodingOptions): TBytes;
var
  r : String;
Begin
  FOptions := oOptions.Link;

  XMLCheck(oMessage.Version >= hv231, 'EncodeMessage', 'Unable to encode messages with Version '+oMessage.VersionCode+' in XML');
  XMLCheck(oMessage.StructName <> '', 'EncodeMessage', 'Cannot encode an XML message with no assigned structure ('+oMessage.VersionCode+'/'+oMessage.Event+'/'+oMessage.MessageType+')');
  oMessage.BuildSegmentMap(False, oOptions);

  XMLCheck(Assigned(oMessage.SegmentMap), 'EncodeMessage', 'Unable to encode to XML (no XML Segment Map) - check for XML support and message details: version (' + oMessage.VersionCode + '), message type (' + oMessage.MessageType + '), and event ID (' + oMessage.Event + ')');

  R :=
    '<?xml version="1.0" encoding="UTF-8"?>' + #10 +
    '<' + oMessage.StructName + ' xmlns="'+V2_XML_NS+'" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="'+V2_XML_NS+' '+oMessage.StructName+'.xsd">'+
    FormatSegments(oMessage.SegmentMap) +
    '</' + oMessage.StructName + '>';

  result := TEncoding.UTF8.GetBytes(R);
End;

Function THL7V2XMLEncoder.FormatSegments(oGroup: THL7V2SegmentGroup): String;
Var
  iLoop : Integer;
  iChild : Integer;
  oChild: THL7V2SegmentGroup;
Begin
  Result := '';
  For iLoop := 0 To oGroup.Children.Count - 1 Do
    Begin
    oChild := oGroup.Children[iLoop];
    If oChild.Segments.Count > 0 Then
      For iChild := 0 To oChild.Segments.Count - 1 Do
        Result := Result + FormatSegment(oChild.Segments[iChild])
    Else If oChild.Name = '' Then
      Result := Result + FormatSegments(oChild)
    Else
      Result := Result + '<' + oChild.Name + '>' + FormatSegments(oChild) + '</' + oChild.Name + '>';
    End;
End;

Function THL7V2XMLEncoder.FormatSegment(oSegment: THL7V2Segment): String;
Begin
  Result := '<' + oSegment.Code + '>' + FormatSegmentContent(oSegment) + '</' + oSegment.Code + '>';
End;

Function THL7V2XMLEncoder.FormatSegmentContent(oSegment: THL7V2Segment): String;
Var
  iLoop   : Integer;
  iRepeat : Integer;
  oField : THL7V2DataElement;
Begin
  Result := '';
  For iLoop := 0 To oSegment.Fields.Count - 1 Do
    Begin
    oField := oSegment.Fields[iLoop];
    For iRepeat := 0 To oField.repeatCount - 1  Do
      Result := Result + FormatCell(oField._Repeat[iRepeat], oSegment.Code + '.' + IntegerToString(iLoop + 1));
    End;
End;

Function THL7V2XMLEncoder.buildComplexContent(oCell : THL7V2Cell) : String;
var
  iIndex : integer;
  oContent : THL7V2Content;
Begin
  Result := '';
  For iIndex := 0 to oCell.Contents.Count - 1 do
    Begin
    oContent := oCell.Contents[iIndex];
    if oContent.ContentType = CONTENT_TYPE_NULL Then
      RaiseError('buildComplexContent', 'cannot find a null here')
    else if oContent.ContentType = CONTENT_TYPE_TEXT Then
      Result := Result + TextToXML(oContent.Text)
    else if oContent.ContentType = CONTENT_TYPE_Escape Then
      Result := Result + '<escape V="'+oContent.Text+'"/>'
    else if oContent.ContentType = CONTENT_TYPE_Binary Then
      Result := Result + THL7V2ContentBinary(oContent).XmlText
    Else
      RaiseError('buildComplexContent', 'Unknown content type');
    End;
End;

Function THL7V2XMLEncoder.FormatCell(oCell : THL7V2Cell; Const sName : String) : String;
Var
  sComp : String;
  iLoop : Integer;
  oComp : THL7V2Component;
  oField : THL7V2DataElement;
  bSimple : Boolean;
  oStruc : THL7V2ModelStructure;
Begin
  Result := '';
  sComp := '';
  bSimple := True;
  If oCell.HasContent Then
    Begin
    If oCell.HasChildren Then
      Begin
      bSimple := False;
      sComp := 'UNK';
      // work out the name for the components
      If oCell.CellType = ctDataElement Then
        Begin
        oField := oCell As THL7V2DataElement;
        If oField.SpecifiedDataType <> '' Then
        Begin
          sComp := oField.SpecifiedDataType;
          oStruc := oField.Model.Structures.GetByName(sComp);
          bSimple := (oCell.Children.count = 1) And (oStruc <> nil) And (oStruc.Components.Count < 2);
        End
        Else If Assigned(oField.Definition) And Assigned(oField.Definition.RefDataElement) And Assigned(oField.Definition.RefDataElement.RefStructure) Then
          sComp := oField.Definition.RefDataElement.RefStructure.DataType
        End
      Else
        Begin
        oComp := oCell As THL7V2Component;
        If Assigned(oComp.Definition) Then
          sComp := oComp.Definition.DataType
        Else
          sComp := '_unknown_';
        End;
      End;

    If Not bSimple Then
      For iLoop := 0 To oCell.Children.count - 1 Do
        Result := Result + FormatCell(oCell.Children[iLoop], sComp+'.'+IntegerToString(iLoop+1))
    Else if oCell.IsNullify Then
      Result := TextToXML('""')
    Else if oCell.IsSimple Then
      Result := TextToXML(oCell.RawContent)
    Else
      Result := BuildComplexContent(oCell);
    End
  Else
    Result := '';
  If Result <> '' Then
    Result := '<'+sName+'>'+Result+'</'+sName+'>';
End;

function THL7V2XMLEncoder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOptions.sizeInBytes);
end;

{ THL7V2ErrorCollector }

procedure THL7V2ErrorCollector.RecordError(AElement: THL7V2BaseObject; const sError: String);
begin
  RaiseError('RecordError', 'Need to override RecordError in '+ClassName);
end;

{ THL7V2DictionaryValidator }

function THL7V2DictionaryValidator.Validate(oMessage : THL7V2Message; aOptions : THL7V2ValidationOptions; oErrors : THL7V2ErrorCollector) : Boolean;
var
  iLoop : integer;
  bOk : Boolean;
  oOptions : THL7V2EncodingOptions;
begin
  result := False;
  // first, check segment order,
  if (voSegmentOrder in AOptions) then
    begin
    try
      oOptions := THL7V2EncodingOptions.create;
      try
        oMessage.BuildSegmentMap(false, oOptions);
      finally
        oOptions.Free;
      end;

      if not assigned(oMessage.SegmentMap) and (oMessage.Version >= hv231) then
        oErrors.RecordError(NIL, 'Segment order could not be validated as structure was not recognised')
      else
        result := true;
    except
      on e: Exception do
        oErrors.RecordError(NIL, e.message);
    end;
    end
  else
    result := true;

  for iLoop := 0 to oMessage.Segments.Count - 1 do
    begin
    bOk := ValidateSegment(oMessage.Segments[iLoop], aOptions, oErrors);
    result := bOk and result;
    end;
end;

function THL7V2DictionaryValidator.ValidateSegment(oSegment: THL7V2Segment; aOptions: THL7V2ValidationOptions; oErrors: THL7V2ErrorCollector): Boolean;
var
  iLoop : Integer;
  bOk : Boolean;
  iStart : Integer;
begin
  result := true;
  if assigned(oSegment.Definition) then
    begin
    if StringArrayExists(['MSH', 'FHS', 'BHS'], oSegment.Code) then
      // cause MSH-1 and MSH-2 are screwy - and if they were wrong we couldn't be doing this
      iStart := 2
    else
      iStart := 0;
    for iLoop := iStart to oSegment.Fields.Count - 1 do
      begin
      bOk := ValidateDataElement(oSegment.Fields[iLoop], AOptions, oErrors);
      result := bOk and result;
      end;
    end
  else if not oSegment.Matches('Z*') then
    begin
    result := False;
    oErrors.RecordError(oSegment, 'Segment "'+oSegment.Code+'" not known');
    end;
end;

function THL7V2DictionaryValidator.ValidateDataElement(oDataElement: THL7V2DataElement; aOptions: THL7V2ValidationOptions; oErrors: THL7V2ErrorCollector): Boolean;
begin
  result := ValidateCell(oDataElement, aOptions, oErrors);

  if oDataElement.HasRepeats and assigned(oDataElement.Definition) and not oDataElement.Definition.Repeatable then
    begin
    result := False;
    oErrors.RecordError(oDataElement, oDataElement.FullPathName +': Found repeats when non were expected');
    end;
end;

function THL7V2DictionaryValidator.ValidateCell(oCell: THL7V2Cell; aOptions: THL7V2ValidationOptions; oErrors: THL7V2ErrorCollector): Boolean;
var
  sDataType : String;
  bRequired : Boolean;
  oField : THL7V2DataElement;
  oComp : THL7V2Component;
  bOk : Boolean;
  sError : String;
  iLoop : integer;
  sFormat : String;
begin
  bRequired := False;
  sDataType := '';
  if oCell.CellType = ctDataElement then
    begin
    oField := oCell as THL7V2DataElement;
    if assigned (oField.Definition) then
      begin
      bRequired := oField.Definition.Required;
      if assigned(oField.Definition.RefDataElement) and assigned(oField.Definition.RefDataElement.RefStructure) then
        sDataType := oField.Definition.RefDataElement.RefStructure.DataType;
      end;
    end
  else
    begin
    bRequired := False;
    oComp := oCell as THL7V2Component;
    if assigned(oComp.Definition) then
      sDataType := oComp.Definition.DataType;
    end;

  result := True;
  if (sDataType = 'ID') and oCell.HasTable and oCell.HasContent and not oCell.TableValueValid And (oCell.Table.Items.COunt > 0) then
    begin
    result := False;
    oErrors.RecordError(oCell, oCell.FullPathName + ': Table value "'+oCell.RawContent+'" is not valid (valid values = '+oCell.Table.CodesAsString);
    end;

  if (voRequired in AOptions) and bRequired and not oCell.HasContent then
    begin
    result := False;
    oErrors.RecordError(oCell, oCell.FullPathName+': Required Element not provided');
    end;

  if (voDateFormat in aOptions) and StringArrayExists(['DT', 'TM', 'TS', 'DTM'], sDataType) and (oCell.HasContent) and
          DateFormatForType(sDataType, oCell.Model.Version, sFormat, true) then
    begin
    bOk := CheckDateFormat(sFormat, oCell.RawContent, sError);
    if not bOk then
      begin
      result := False;
      oErrors.RecordError(oCell, oCell.FullPathName + ': Date Format Error: '+sError);
      end;
    end;

  if oCell.HasChildren then
    for iLoop := 0 to oCell.Children.Count - 1 do
      begin
      bOk := ValidateCell(oCell.Children[iLoop], aOptions, oErrors);
      result := result and bOk;
      end;
end;

Constructor THL7V2ValidationError.Create;
Begin
  Inherited;
End;

Destructor THL7V2ValidationError.Destroy;
Begin
  FElement.Free;
  Inherited;
End;

Function THL7V2ValidationError.Link : THL7V2ValidationError;
Begin
  Result := THL7V2ValidationError(Inherited Link);
End;

Function THL7V2ValidationError.Clone : THL7V2ValidationError;
Begin
  Result := THL7V2ValidationError(Inherited Clone);
End;

Procedure THL7V2ValidationError.Assign(oObject : TFslObject);
Begin
  Inherited;

  ErrorMsg := THL7V2ValidationError(oObject).ErrorMsg;
  Element := THL7V2ValidationError(oObject).Element.Link;
End;

function THL7V2ValidationError.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FElement.sizeInBytes);
end;

Function THL7V2ValidationErrors.Link : THL7V2ValidationErrors;
Begin
  Result := THL7V2ValidationErrors(Inherited Link);
End;

Function THL7V2ValidationErrors.Clone : THL7V2ValidationErrors;
Begin
  Result := THL7V2ValidationErrors(Inherited Clone);
End;

Function THL7V2ValidationErrors.New : THL7V2ValidationError;
Begin
  Result := THL7V2ValidationError(Inherited New);
End;

Function THL7V2ValidationErrors.ItemClass : TFslObjectClass;
Begin
  Result := THL7V2ValidationError;
End;

Function THL7V2ValidationErrors.GetElement(Const iIndex : Integer) : THL7V2ValidationError;
Begin
  Result := THL7V2ValidationError(ObjectByIndex[iIndex]);
End;

Procedure THL7V2ValidationErrors.SetElement(Const iIndex : Integer; Const oValue : THL7V2ValidationError);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;

Function THL7V2ValidationErrors.CompareByError(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(THL7V2ValidationError(pA).ErrorMsg, THL7V2ValidationError(pB).ErrorMsg);
End;

Function THL7V2ValidationErrors.IndexByError(Const aValue : THL7V2ValidationErrorError) : Integer;
Var
  oElement : THL7V2ValidationError;
Begin
  oElement := New;
  Try
    oElement.ErrorMsg := aValue;

    If Not Find(oElement, Result, CompareByError) Then
    Begin
      Result := -1;
    End;
  Finally
    oElement.Free;
  End;
End;

Function THL7V2ValidationErrors.Get(Const aValue : Integer) : THL7V2ValidationError;
Begin
  Result := THL7V2ValidationError(Inherited Get(aValue));
End;

Function THL7V2ValidationErrors.GetByError(Const aValue : THL7V2ValidationErrorError) : THL7V2ValidationError;
Begin
  Result := Get(IndexByError(aValue));
End;

Function THL7V2ValidationErrors.ExistsByError(Const aValue : THL7V2ValidationErrorError) : Boolean;
Begin
  Result := ExistsByIndex(IndexByError(aValue));
End;

Procedure THL7V2ValidationErrors.SortedByError;
Begin
  SortedBy(CompareByError);
End;

Function THL7V2ValidationErrors.IsSortedByError : Boolean;
Begin
  Result := IsSortedBy(CompareByError);
End;

Procedure THL7V2ValidationError.SetElement(Const Value: THL7V2BaseObject);
Begin
  FElement.Free;
  FElement := Value;
End;

Function THL7V2ValidationErrors.AsCommaText: String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, Elements[iLoop].AsText, ', ');
End;

Function THL7V2ValidationError.AsText: String;
Begin
  if FElement = nil Then
    Result := FError
  Else
    Result := FError+':'+FElement.FullPathName;
End;

Function THL7V2ValidationErrors.AsText: String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, Elements[iLoop].AsText, cReturn);
End;

{ THL7V2SimpleErrorCollector }

constructor THL7V2SimpleErrorCollector.Create;
begin
  inherited;
  FErrors := THL7V2ValidationErrors.create;
end;

destructor THL7V2SimpleErrorCollector.Destroy;
begin
  FErrors.Free;
  inherited;
end;

procedure THL7V2SimpleErrorCollector.RecordError(oElement: THL7V2BaseObject; const sError: String);
var
  oError : THL7V2ValidationError;
begin
  oError := THL7V2ValidationError.create;
  try
    oError.Element := oElement.Link;
    oError.ErrorMsg := sError;
    FErrors.Add(oError.Link);
  finally
    oError.Free;
  end;
end;

procedure THL7V2SimpleErrorCollector.SetErrors(const Value: THL7V2ValidationErrors);
begin
  FErrors.Free;
  FErrors := Value;
end;

initialization
  GHL7V2EncoderFactory := nil;
  GHL7V2DecoderFactory := nil;
finalization
  GHL7V2DecoderFactory.Free;
  GHL7V2EncoderFactory.Free;
end.
