unit fhir_objects;

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
  SysUtils, Classes, Generics.Collections, {$IFNDEF VER260} System.NetEncoding, {$ENDIF}
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_xml, fsl_http;

Const
  ID_LENGTH = 64;
  SYSTEM_NOT_APPLICABLE = '%%null%%';
  SEARCH_PARAM_NAME_ID = 'search-id';
  HISTORY_PARAM_NAME_ID = 'history-id';
  SEARCH_PARAM_NAME_OFFSET = 'search-offset';
  SEARCH_PARAM_NAME_TEXT = '_text';
  SEARCH_PARAM_NAME_COUNT = '_count';
  SEARCH_PARAM_NAME_SORT = '_sort';
  SEARCH_PARAM_NAME_SUMMARY = '_summary';
  SEARCH_PARAM_NAME_FILTER = '_filter';
  SEARCH_PAGE_DEFAULT = 50;
  SEARCH_PAGE_LIMIT = 1000;
  SUMMARY_SEARCH_PAGE_LIMIT = 10000;
  SUMMARY_TEXT_SEARCH_PAGE_LIMIT = 10000;


Type
  TFHIRVersion = (fhirVersionUnknown, fhirVersionRelease1, fhirVersionRelease2, fhirVersionRelease3, fhirVersionRelease4, fhirVersionRelease5);
  TFHIRVersionSet = set of TFHIRVersion;

Const
  {$IFDEF FHIR1} 
  CURRENT_FHIR_VERSION = fhirVersionRelease1;
  COMPILED_FHIR_VERSION = fhirVersionRelease1;
  {$ELSE}
  {$IFDEF FHIR2} 
  CURRENT_FHIR_VERSION = fhirVersionRelease2;
  COMPILED_FHIR_VERSION = fhirVersionRelease2;
  {$ELSE}
  {$IFDEF FHIR3}
  CURRENT_FHIR_VERSION = fhirVersionRelease3;
  COMPILED_FHIR_VERSION = fhirVersionRelease3;
  {$ELSE}
  {$IFDEF FHIR4} 
  CURRENT_FHIR_VERSION = fhirVersionRelease4;
  COMPILED_FHIR_VERSION = fhirVersionRelease4;
  {$ELSE}
  CURRENT_FHIR_VERSION = fhirVersionUnknown;
  // not defined, so that compiles fail if $define is not defined anywhere. COMPILED_FHIR_VERSION = fhirVersionUnknown;
  {$ENDIF} {$ENDIF} {$ENDIF} {$ENDIF}

  CODES_TFHIRVersion : Array [TFHIRVersion] of String = ('', 'r1', 'r2', 'r3', 'r4', 'r5');
  CODES_FHIR_GENERATED_PUBLICATION : array [TFHIRVersion] of string = ('', '1', '2', '3', '4', '5');
  PF_CONST : array [TFHIRVersion] of string = ('', '0.0', '1.0', '3.0', '4.0', '4.2');


  FHIR_ALL_VERSIONS = [fhirVersionUnknown, fhirVersionRelease1, fhirVersionRelease2, fhirVersionRelease3, fhirVersionRelease4, fhirVersionRelease5];
  FHIR_VERSIONS : Array [TFHIRVersion] of String = ('', '0.0.82', '1.0.2', '3.0.2', '4.0.1', '4.2.0');
  SUPPORTED_VERSIONS = [fhirVersionRelease2, fhirVersionRelease3, fhirVersionRelease4];

Type
  {
    Possible command types supported by HL7Connect FHIR interfaces
  }
  TFHIRCommandType = (
    fcmdUnknown, { Unknown command}
    fcmdRead, { Read the resource}
    fcmdVersionRead, { Read a particular version of the resource}
    fcmdUpdate, { Update the resource}
    fcmdDelete, { Delete the resource}
    fcmdHistoryInstance, { get a history for the resource}

    fcmdCreate, { create a resource}
    fcmdSearch, { search a resource type}
    fcmdHistoryType,{ get updates for the resource type}

    fcmdValidate, { validate the resource}
    fcmdMetadata, { get the conformance statement for the system}
    fcmdTransaction, { Update or create a set of resources}
    fcmdHistorySystem, { get updates for the resource type}
    fcmdUpload, { Manual upload (Server extension)}

    fcmdOperation, { operation, as defined in DSTU2}

    fcmdPatch, { Patch (trial for Connectathon 11)}

    fcmdBatch, { batch as defined in DSTU2}
    fcmdWebUI, { Special web interface operations - not a valid FHIR operation}
    fcmdTask,  { access an asynchronous task }
    fcmdDeleteTask, { delete an asynchronous task }
    fcmdNull); { Internal use only - not a valid FHIR operation}

  TFHIRCommandTypeSet = set of TFHIRCommandType;

  {
    Format support.
  }
  TFHIRFormat = (
    ffUnspecified, { leave the format as received/expected, or default to XML }
    ffXml, { XML }
    ffJson,{ JSON }
    ffTurtle, { RDF using Turtle syntax }
    ffText,
    ffNDJson, { new line delimited JSON }
    ffXhtml); { XHTML - only for retrieval from the server }
  TFHIROutputStyle = (OutputStyleNormal, OutputStylePretty, OutputStyleCanonical);

  {
    Enumeration of html node types
  }
  TFHIRHtmlNodeType = (
    fhntElement, { The node is an element}
    fhntText, { The node is a text fragment}
    fhntComment, { The node is a comment}
    fhntDocument);{ The node represents a document (not used in FHIR context)}

  TFHIRAuthProvider = (apNone, apInternal, apFacebook, apGoogle, apHL7);

  TFHIRXhtmlParserPolicy = (xppAllow, xppDrop, xppReject);

  TFHIRSummaryOption = (soFull, soSummary, soText, soData, soCount);

  TFhirIssueType = (itNull, itInvalid, itStructure, itRequired, itValue, itInvariant, itSecurity, itLogin, itUnknown, itExpired, itForbidden, itSuppressed, itProcessing, itNotSupported, itDuplicate, itNotFound, itTooLong, itCodeInvalid, itExtension, itTooCostly, itBusinessRule, itConflict, itIncomplete, itTransient, itLockError, itNoStore, itException, itTimeout, itThrottled, itInformational);
  TIssueSeverity = (isNull, isFatal, isError, isWarning, isInformation);

  EFHIRException = class (EFslException)
  public
    constructor CreateLang(code : String; const lang : THTTPLanguages); overload;
    constructor CreateLang(code : String; const lang : THTTPLanguages; const Args: array of const); overload;
  end;
  EFHIRTodo = Class(EFHIRException)
  public
    Constructor Create(place : String);
  End;

  ETooCostly = class (EFHIRException);
  EUnsafeOperation = class (EFHIRException);
  EDefinitionException = class (EFHIRException);
  EDefinitionExceptionTodo = Class(EDefinitionException)
  public
    Constructor Create(place : String);
  End;
  EFHIRNarrativeException = class (EFHIRException);

  ERestfulException = class (EFHIRException)
  Private
    FContext : String;
    FStatus : word;
    FCode : TFhirIssueType;
  Public
    constructor Create(Const sContext : String; aStatus : word; code : TFhirIssueType; sMessage : String; const lang : THTTPLanguages); Overload; Virtual;
    constructor Create(Const sContext : String; aStatus : word; code : TFhirIssueType; sMessage : String; const lang : THTTPLanguages; const Args: array of const); Overload; Virtual;

    Property Status : word read FStatus write FStatus;
    Property Code : TFhirIssueType read FCode write FCode;
  End;

  EFHIRPath = class (EFHIRException)
  public
     constructor Create(problem : String); overload;
     constructor Create(path : String; offset : integer; problem : String); overload;
  end;
  EFHIRPathTodo = Class(EDefinitionException)
  public
    Constructor Create(place : String);
  End;

  EFHIRPathDefinitionCheck = class (EFHIRPath);
  EFHIRUnsupportedVersion = class (EFslException)
  public
    constructor Create(version : TFHIRVersion; action : String);
  end;

  TFhirResourceTypeV = string;

Const
  FHIR_NS = 'http://hl7.org/fhir';
  CODES_TFHIRCommandType : array [TFHIRCommandType] of String = (
    'Unknown', 'Read', 'VersionRead', 'Update', 'Delete', 'HistoryInstance', 'Create', 'Search', 'HistoryType', 'Validate', 'Metadata', 'Transaction', 'HistorySystem', 'Upload', 'Operation', 'Patch', 'Batch', 'WebUI', 'Task', 'delete task', 'Null');
  CODES_TFHIRHtmlNodeType : array [TFHIRHtmlNodeType] of String = ('Element', 'Text', 'Comment', 'Document');
  CODES_TFHIRFormat : Array [TFHIRFormat] of String = ('Unspecified', 'XML', 'JSON', 'RDF/Turtle', 'Text Representation', 'Newline delimited JSON', 'XHTML');
  EXT_ACTUAL_TFHIRFormat : Array [TFHIRFormat] of String = ('.bin', '.xml', '.json', '.ttl', '.txt', '.ndjson', '.html');
  EXT_WEB_TFHIRFormat : Array [TFHIRFormat] of String = ('.bin', '.xml', '.json', '.ttl', '.txt', '.ndjson', '.xml');
  MIMETYPES_TFHIRFormat : Array [TFHIRFormat] of String = ('', 'application/fhir+xml', 'application/fhir+json', 'text/turtle; x-dialect=fhir', 'text/fhir', 'application/x-ndjson', 'text/xhtml');
  MIMETYPES_TFHIRFormat_Version : Array [TFHIRFormat, TFHIRVersion] of String = (
    ('', '', '', '', '', ''),
    ('', 'application/xml+fhir', 'application/xml+fhir', 'application/fhir+xml', 'application/fhir+xml', 'application/fhir+xml'),
    ('', 'application/json+fhir', 'application/json+fhir', 'application/fhir+json', 'application/fhir+json', 'application/fhir+json'),
    ('','','','text/turtle; x-dialect=fhir','text/turtle; x-dialect=fhir','text/turtle; x-dialect=fhir'),
    ('','','','text/fhir','text/fhir','text/fhir'),
    ('','','','','application/x-ndjson', 'application/x-ndjson'),
    ('', 'text/xhtml', 'text/xhtml', 'text/xhtml', 'text/xhtml', 'text/xhtml')
    );

  Names_TFHIRAuthProvider : Array [TFHIRAuthProvider] of String = ('', 'Custom', 'Facebook', 'Google', 'HL7');
  USER_SCHEME_IMPLICIT = 'http://healthintersections.com.au/fhir/user/implicit';
  USER_SCHEME_PROVIDER : array [TFHIRAuthProvider] of String =
    ('', 'http://healthintersections.com.au/fhir/user/explicit', 'http://www.facebook.com', 'http://www.google.com', 'http://www.hl7.org');
  CODES_TFHIRSummaryOption : array [TFHIRSummaryOption] of String = ('Full', 'Summary', 'Text', 'Data', 'Count');
  CODES_TFhirIssueType : array [TFhirIssueType] of String = ('null', 'invalid', 'structure', 'required', 'value', 'invariant', 'security', 'login', 'unknown', 'expired', 'forbidden', 'suppressed', 'processing', 'not-supported', 'duplicate', 'not-found', 'too-long', 'code-invalid', 'extension', 'too-costly', 'business-rule', 'conflict', 'incomplete', 'transient', 'lock-error', 'no-store', 'exception', 'timeout', 'throttled', 'informational');
  CODES_TIssueSeverity : array [TIssueSeverity] of String = ('null', 'fatal', 'error', 'warning', 'information');

  NON_STD_COMMANDS = [fcmdUnknown, fcmdWebUI, fcmdTask, fcmdDeleteTask, fcmdNull];
type

  TFHIRObject = class;
  TFHIRObjectList = class;
  TFHIRPropertyList = class;
  TFHIRSelection = class;
  TFHIRLocatedNode = class;
  TFHIRSelectionList = class;

  TFHIRProperty = class (TFslObject)
  Private
    FOwner : TFHIRObject; // noown
    FName : String;
    FType : String;
    FIsList : boolean;
    FList : TFHIRObjectList;
    FClass : TClass;
    FEnumName : String;
    function GetHasValue: Boolean;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oObject : TFHIRObject); Overload;
    constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oList : TFHIRObjectList); Overload;
    constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; sValue : String); Overload;
    constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; Value : TBytes); Overload;
    constructor CreateEnum(oOwner : TFHIRObject; Const sName : String;    bList: boolean; cClass : TClass; enumName : String; sValue : String); Overload;
    constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass); Overload;
    destructor Destroy; Override;

    Function Link : TFHIRProperty; overload;

    class function create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oList : TFslList<TFslObject>) : TFHIRProperty; Overload;

    {$IFNDEF FPC}
    class function create<T : TFslObject>(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oList : TFslList<T>) : TFHIRProperty; Overload;
    {$ENDIF}

    Property hasValue : Boolean read GetHasValue;
    Property Name : String read FName;
    Property Type_ : String read FType;
    Property Class_ : TClass read FClass;
    Property IsList : boolean read FIsList;
    Property Values : TFHIRObjectList read FList;
    Property EnumName : String read FEnumName;
    procedure forceValues;
  End;


  TFHIRPropertyListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFHIRPropertyList;
    function GetCurrent : TFHIRProperty;
  public
    constructor Create(list : TFHIRPropertyList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFHIRProperty read GetCurrent;
  end;

  TFHIRPropertyList = class (TFslObjectList)
  private
    Function GetProperty(iIndex : Integer) : TFHIRProperty;
    Function GetPropertyByName(name : String) : TFHIRProperty;
  public
    function Link : TFHIRPropertyList; overload;
    function GetEnumerator : TFHIRPropertyListEnumerator;
    Property Properties[iIndex : Integer] : TFHIRProperty read GetProperty; default;
    Property ByName[name : String] : TFHIRProperty read GetPropertyByName;
  End;

  TFHIRPropertyIterator = class (TFslObject)
  private
    FFocus : TFHIRObject;
    FProperties : TFHIRPropertyList;
    FCursor : Integer;
    Function GetCurrent : TFHIRProperty;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(oFocus : TFHIRObject; bInheritedProperties, bPrimitiveValues : Boolean); overload;
    destructor Destroy; Override;
    Procedure Next;
    Procedure Reset;
    Function More : Boolean;
    Property Current : TFHIRProperty read GetCurrent;
  End;

  {$M+}

  { TFHIRNamedValue }

  TFHIRNamedValue = class (TFslObject)
  private
    FName: String;
    FType: String;
    FValue: TFhirObject;
    FList : TFhirObjectList;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    constructor Create(name, type_ : String; value : TFhirObject; list : TFHIRObjectList); overload;
    destructor Destroy; override;

    property name : String read FName;
    property type_ : String read FType;
    property value : TFhirObject read FValue;
    property list : TFHIRObjectList read FList;
  end;

  { TFHIRObjectLocationData }

  // we keep both parse and compose for help when editing content live.
  TFHIRObjectLocationData = class (TFslObject)
  private
    FComposeStart: TSourceLocation;
    FComposeFinish: TSourceLocation;
    FParseStart: TSourceLocation;
    FParseFinish: TSourceLocation;
    FParseFinish2: TSourceLocation;
    FParseStart2: TSourceLocation;
    FComposeFinish2: TSourceLocation;
    FComposeStart2: TSourceLocation;
  public
    constructor Create; override;
    function inSpan(loc : TSourceLocation) : boolean;
    function hasLocation1 : boolean;
    function hasLocation2 : boolean;

    property parseStart : TSourceLocation read FParseStart write FParseStart;
    property parseFinish : TSourceLocation read FParseFinish write FParseFinish;
    property composeStart : TSourceLocation read FComposeStart write FComposeStart;
    property composeFinish : TSourceLocation read FComposeFinish write FComposeFinish;

    // json primitive support
    property parseStart2 : TSourceLocation read FParseStart2 write FParseStart2;
    property parseFinish2 : TSourceLocation read FParseFinish2 write FParseFinish2;
    property composeStart2 : TSourceLocation read FComposeStart2 write FComposeStart2;
    property composeFinish2 : TSourceLocation read FComposeFinish2 write FComposeFinish2;
  end;

  { TFHIRLocatedNode }

  TFHIRLocatedNode = class (TFslObject)
  private
    FProp: TFHIRProperty;
    FValue: TFHIRObject;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor create(prop : TFHIRProperty; value : TFHIRObject);
    destructor Destroy; override;

    property prop : TFHIRProperty read FProp;
    property value : TFHIRObject read FValue;
  end;

  { TFHIRObject }
  {$M+}
  TFHIRObject = class (TFslObject)
  private
    FTags : TFslStringDictionary;
    FTag : TFslObject;
    FLocationData : TFHIRObjectLocationData;
    FCommentsStart: TFslStringList;
    FCommentsEnd: TFslStringList;
    FFormat : TFHIRFormat;
    FNoCompose: boolean;
    FTagInt: integer;
    FJsHandle: pointer;
    FJsInstance: cardinal;
    function GetCommentsStart: TFslStringList;
    function GetCommentsEnd: TFslStringList;
    procedure SetTag(const Value: TFslObject);
    procedure SetTags(name: String; const Value: String);
    function getTags(name: String): String;
    function GetLocationData : TFHIRObjectLocationData;
    function GetHasLocationData : boolean;
    function findLocation(loc : TSourceLocation; propFrom : TFHIRProperty; path : TFslList<TFHIRLocatedNode>) : boolean; overload;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); virtual;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Virtual;

    procedure deletePropertyValue(name : String; list : TFHIRObjectList; value : TFHIRObject);
    procedure replacePropertyValue(name : String; list : TFHIRObjectList; existing, new : TFHIRObject);

    function isMatchingName(given, expected : String; types : Array of String) : boolean;
    function GetFhirObjectVersion: TFHIRVersion; virtual;
    procedure listFieldsInOrder(fields : TStringList); virtual;

    function GetDateValue: TFslDateTime; virtual;
    procedure SetDateValue(Value: TFslDateTime); virtual;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Link : TFHIRObject;
    function Clone : TFHIRObject; Overload;
    procedure Assign(oSource : TFslObject); override;

    // version delegation. these 3 make* are public for internal reasons, but there's never any reason to use these outside the library
    function makeStringValue(v : String) : TFHIRObject; virtual; abstract;
    function makeCodeValue(v : String) : TFHIRObject; virtual; abstract;
    function makeIntValue(v : String) : TFHIRObject; virtual; abstract;
    function hasExtension(url : String) : boolean; virtual;
    function hasExtensions : boolean; virtual; abstract;
    function getExtensionString(url : String) : String; virtual;
    function extensionCount(url : String) : integer; virtual;
    function extensions(url : String) : TFslList<TFHIRObject>; virtual;
    procedure addExtension(url : String; value : TFHIRObject); virtual;

    procedure ListChildrenByName(name : string; list : TFHIRSelectionList);
    function getNamedChildren : TFslList<TFHIRNamedValue>;

    // property access API
    // getting access to the properties
    function createPropertyList(bPrimitiveValues : boolean) : TFHIRPropertyList;
    function createIterator(bInheritedProperties, bPrimitiveValues : Boolean) : TFHIRPropertyIterator;

    // create a class that is the correct type for the named property
    function createPropertyValue(propName : string): TFHIRObject; virtual;
    function getPropertyValue(propName : string): TFHIRProperty; virtual;
    function getTypesForProperty(propName : string): String; virtual;

    // set the value of the property. For properties with cardinality > 1, append to the list, or use insertProperty
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; virtual;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); virtual;

    // delete the value. propValue is used where cardinality > 1, to pick the correct item to delete
    procedure deleteProperty(propName : string; propValue : TFHIRObject); virtual;

    // replace the value of the property with a new value
    procedure replaceProperty(propName : string; existing : TFHIRObject; new : TFHIRObject); virtual;
    procedure reorderProperty(propName : string; source, destination : integer); virtual;
    function SerialiseUsingProperties : boolean; virtual;

    // tags...
    Property Tags[name : String] : String read getTags write SetTags;
    function HasTag(name : String): boolean; overload;
    property Tag : TFslObject read FTag write SetTag;
    property TagInt : integer read FTagInt write FTagInt;

    // javascript caching
    property jsInstance : cardinal read FJsInstance write FJsInstance;
    property jsHandle : pointer read FJsHandle write FJsHandle;

    // this is populated by the json and xml parsers if requested
    property LocationData : TFHIRObjectLocationData read GetLocationData;
    property HasLocationData : boolean read GetHasLocationData;
    function findLocation(loc : TSourceLocation) : TFslList<TFHIRLocatedNode>; overload;

    // called when content is added to the source from which this was parsed.
    // start is where the insertion was
    // finish is the content that was deleted
    // new content is ths line/col count of the content inserted
    // focus is the object that was the focus of the change (inner content for this has to be adjusted from as composed
    procedure updateLocationData(start : TSourceLocation; removed, added: TSourceRange; focus : TFHIRObject);

    function HasXmlCommentsStart : Boolean;
    function HasXmlCommentsEnd : Boolean;
    function HasComments : Boolean;
    function fhirType : String; virtual; abstract;
    function JSType : String; virtual; // same as fhirType, but with version number appended
    function getId : String; virtual; abstract;
    procedure setIdValue(id : String); virtual; abstract;
    function isPrimitive : boolean; virtual;
    function isBooleanPrimitive : boolean; virtual;
    function isResource : boolean; virtual;
    function isEnum : boolean; virtual;
    function isType : boolean; virtual;
    function hasPrimitiveValue : boolean; virtual;
    function primitiveValue : string; virtual;
    function fpValue : String; virtual;
    function isDateTime : boolean; virtual;
    property dateValue : TFslDateTime read GetDateValue write SetDateValue;
    function isMetaDataBased : boolean; virtual;
//    Function PerformQuery(path : String) : TFHIRObjectList;
    function hasType(t : String) : boolean; overload;
    function hasType(tl : Array of String) : boolean; overload;
    function describe : String; virtual;
    procedure getProperty(name : String; checkValid : boolean; list : TFslList<TFHIRObject>); virtual;
    function ToString : String; override;
    function isEmpty : boolean; virtual;
    procedure dropEmpty;
    function Equals(other : TObject) : boolean; override;
  published
    {
      comments from the XML stream. No support for comments in JSON
    }
    Property xml_commentsStart : TFslStringList read GetCommentsStart;
    Property xml_commentsEnd : TFslStringList read GetCommentsEnd;

    Property _source_format : TFHIRFormat read FFormat write FFormat;

    property noCompose : boolean read FNoCompose write FNoCompose; // used by various filtering techniques to ensure that an element is not rendered

    Property fhirObjectVersion : TFHIRVersion read GetFhirObjectVersion;
    Property id : String read Getid write SetIdValue;
  end;

  TFHIRObjectClass = class of TFHIRObject;

  {$M-}
  TFHIRObjectListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFHIRObjectList;
    function GetCurrent : TFHIRObject;
  public
    constructor Create(list : TFHIRObjectList);
    destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFHIRObject read GetCurrent;
  end;

  { TFHIRObjectList }

  TFHIRObjectList = class (TFslObjectList)
  private
    FTags : TFslStringDictionary;
    FLocationData : TFHIRObjectLocationData;
    FJsHandle: pointer;
    FJsInstance: cardinal;
    function GetLocationData : TFHIRObjectLocationData;
    function GetHasLocationData : boolean;
    Function GetItemN(index : Integer) : TFHIRObject;
    procedure SetTags(name: String; const Value: String);
    function getTags(name: String): String;
  protected
    function ItemClass : TFslObjectClass; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(item : TFHIRObject); overload;
    constructor Create(items : TFHIRObjectList); overload;
    destructor Destroy; override;
    function Link : TFHIRObjectList; Overload;
    function Clone : TFHIRObjectList; Overload;
    function GetEnumerator : TFHIRObjectListEnumerator;
    Property ObjByIndex[index : Integer] : TFHIRObject read GetItemN; default;
    Property Tags[name : String] : String read getTags write SetTags;
    function ToString : String; override;
    function new : TFHIRObject; reintroduce; overload; virtual;

    property LocationData : TFHIRObjectLocationData read GetLocationData; // this is only populated by the parsers on demand
    property HasLocationData : boolean read GetHasLocationData;

    // javascript caching
    property jsInstance : cardinal read FJsInstance write FJsInstance;
    property jsHandle : pointer read FJsHandle write FJsHandle;
  end;

  TFHIRResourceV = class (TFHIRObject)
  protected
    function GetProfileVersion: TFHIRVersion; virtual;
    procedure SetProfileVersion(Value: TFHIRVersion); virtual;

  public
    function link : TFHIRResourceV; overload;
    function isResource : boolean; override;
    function isDomainResource : boolean; virtual;

    procedure checkNoImplicitRules(place, role : String); virtual; abstract;

    property profileVersion : TFHIRVersion read GetProfileVersion write SetProfileVersion;
  end;

  TFHIRWorkerContextV = class (TFslObject)
  protected
    FLang : THTTPLanguages;

    function GetVersion: TFHIRVersion; virtual;
  public
    constructor Create; override;
    function link : TFHIRWorkerContextV; overload;

    property lang : THTTPLanguages read FLang write FLang;
    procedure loadResourceJson(rtype, id : String; json : TStream); virtual; abstract;
    Property version : TFHIRVersion read GetVersion;
    function versionString : String;
    function oid2Uri(oid : String) : String; virtual; abstract;
  end;

  TFHIRObjectText = class (TFHIRObject)
  private
    FValue : String;
  protected
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(value : String); Overload;
    constructor Create(value : TFslDateTime); Overload;
    constructor Create(value : boolean); Overload;
    constructor Create(value : integer); Overload;
    constructor Create(value : TBytes); Overload;

    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function fhirType : String; override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;
    function isPrimitive : boolean; override;
    function primitiveValue : String; override;

    property value : string read FValue write FValue;
    function isEmpty : boolean; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function Equals(other : TObject) : boolean; override;
  end;

  TFHIRSelection = class (TFhirObject)
  private
    FParent : TFHIRObject;
    FName : String;
    FValue : TFHIRObject;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(focus : TFHIRObject); overload;
    constructor Create(parent : TFHIRObject; name : String; focus : TFHIRObject); overload;
    destructor Destroy; override;
    function Link : TFHIRSelection; overload;
    property parent : TFHIRObject read FParent;
    property name : String read FName;
    property value : TFHIRObject read FValue;

    function createPropertyValue(propName : string): TFHIRObject; override;
    function getTypesForProperty(propName : string): String; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function fhirType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;
  end;

  { TFHIRSelectionList }

  TFHIRSelectionList = class (TFslList<TFHIRSelection>)
  private
    FOneBased : boolean;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(focus : TFHIRObject); overload;
    constructor Create(parent : TFHIRObject; name : String; focus : TFHIRObject); overload;
    function Link : TFHIRSelectionList; overload;
    procedure add(value : TFHIRObject); overload;
    procedure add(parent : TFHIRObject; name : String; value : TFHIRObject); overload;
    procedure addAll(value : TFHIRObjectList); overload;
    procedure addAll(value : TFslList<TFHIRObject>); overload;
    procedure addAll(parent : TFHIRObject; name : String; value : TFHIRObjectList); overload;

    function asValues : TFHIRObjectList;
    function describeAsPath : String;

    property OneBased : boolean read FOneBased write FOneBased;

    class function compareDeep(e1, e2 : TFHIRSelectionList; allowNull : boolean) : boolean;
  end;

  TValidationResult = class (TFslObject)
  private
    FSeverity : TIssueSeverity;
    FMessage  : String;
    FDisplay: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(Severity : TIssueSeverity; Message : String); overload; virtual;
    constructor Create(display : String); overload; virtual;
    Property Severity : TIssueSeverity read FSeverity write FSeverity;
    Property Message : String read FMessage write FMessage;
    Property Display : String read FDisplay write FDisplay;
    function isOk : boolean;
  end;

  TFHIRSystemString = class (TFHIRObject)
  private
    FValue : String;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function GetFhirObjectVersion: TFHIRVersion; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(v : String);
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;
    function fhirType : String; override;
    function isPrimitive : boolean; override;
    function hasPrimitiveValue : boolean; override;
    function primitiveValue : string; override;

    function ToString : String; override;
  end;

  TFHIRSystemTuple = class (TFHIRObject)
  private
    FFields : TFslMap<TFHIRObject>;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function GetFhirObjectVersion: TFHIRVersion; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TFHIRSystemTuple; overload;

    property Fields : TFslMap<TFHIRObject> read FFields;

    function getId : String; override;
    procedure setIdValue(id : String); override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;
    function fhirType : String; override;
    function isPrimitive : boolean; override;
    function hasPrimitiveValue : boolean; override;

    function ToString : String; override;

    class function fromParams(pm : THTTPParameters) : TFHIRSystemTuple;
  end;


function noList(e : TFHIRObjectList) : boolean; overload;
function compareDeep(e1, e2 : TFHIRObjectList; allowNull : boolean) : boolean; overload;
function compareDeep(e1, e2 : TFHIRObject; allowNull : boolean) : boolean; overload;
function isPrimitiveType(name : String):boolean;
function isEmptyProp(v : TFHIRObject) : boolean; overload;
function isEmptyProp(v : TFHIRObjectList) : boolean; overload;
function objectsEqual(o1, o2 : TFHIRObject) : boolean; // true if all properties are equal, recursively
function objectsEquivalent(o1, o2 : TFHIRObject) : boolean; // true if all properties that exist on both objects are equivalent, recursively

function GetFhirMessage(id : String; const lang : THTTPLanguages) : String; overload;
function GetFhirMessage(id : String; const lang : THTTPLanguages; def : String) : String; overload;

procedure LoadMessages;

var
  FHIRExeModuleName : String;

// exceptions

Const
   HTTP_OK_200 = 200;
   HTTP_OK_220 = 220;
   HTTP_CREATED = 201;
   HTTP_ACCEPTED = 202;
   HTTP_NO_CONTENT = 204;
   HTTP_REDIR_MULTIPLE_CHOICE = 300;
   HTTP_REDIR_MOVED_PERMANENT = 301;
   HTTP_REDIR_MOVED_TEMPORARY = 302;
   HTTP_REDIR_AFTER_POST = 303;
   HTTP_REDIR_NOT_MODIFIED = 304;
   HTTP_ERR_BAD_REQUEST = 400;
   HTTP_ERR_UNAUTHORIZED = 401;
   HTTP_ERR_FORBIDDEN = 403;
   HTTP_ERR_NOTFOUND = 404;
   HTTP_ERR_METHOD_ILLEGAL = 405;
   HTTP_ERR_NOT_ACCEPTABLE = 406;
   HTTP_ERR_CONFLICT = 409;
   HTTP_ERR_DELETED = 410;
   HTTP_ERR_PRECONDITION_FAILED = 412;
   HTTP_ERR_NOT_UNSUPPORTED_MEDIA_TYPE = 415;
   HTTP_ERR_BUSINESS_RULES_FAILED = 422;
   HTTP_ERR_INTERNAL = 500;


Implementation

{ TFHIRNamedValue }

constructor TFHIRNamedValue.create;
begin
  inherited create;
end;

constructor TFHIRNamedValue.Create(name, type_: String; value: TFhirObject; list : TFHIRObjectList);
begin
  Create;
  FName := name;
  FType := type_;
  FValue := value;
  FList := list;
end;

destructor TFHIRNamedValue.Destroy;
begin
  FValue.Free;
  FList.Free;
  inherited Destroy;
end;

function TFHIRNamedValue.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FType.length * sizeof(char)) + 12);
  inc(result, FValue.sizeInBytes);
  inc(result, FList.sizeInBytes);
end;

{ TFHIRObjectLocationData }

function TFHIRObjectLocationData.hasLocation2: boolean;
begin
  result := not FParseFinish2.isNull or not FComposeFinish2.isNull;
end;

function TFHIRObjectLocationData.hasLocation1: boolean;
begin
  result := not FParseFinish.isNull or not FComposeFinish.isNull;
end;

constructor TFHIRObjectLocationData.create;
begin
  inherited create;
  FComposeStart := TSourceLocation.CreateNull;
  FComposeFinish := TSourceLocation.CreateNull;
  FParseStart := TSourceLocation.CreateNull;
  FParseFinish := TSourceLocation.CreateNull;
  FParseFinish2 := TSourceLocation.CreateNull;
  FParseStart2 := TSourceLocation.CreateNull;
  FComposeFinish2 := TSourceLocation.CreateNull;
  FComposeStart2 := TSourceLocation.CreateNull;

end;

function TFHIRObjectLocationData.inSpan(loc: TSourceLocation): boolean;
begin
  result := loc.inSpan(FParseStart, FParseFinish) or loc.inSpan(FParseStart2, FParseFinish2);
end;

{ TFHIRLocatedNode }

constructor TFHIRLocatedNode.create(prop: TFHIRProperty; value: TFHIRObject);
begin
  inherited Create;
  FProp := prop;
  FValue := value;
end;

destructor TFHIRLocatedNode.Destroy;
begin
  FProp.Free;
  FValue.Free;
  inherited Destroy;
end;

function TFHIRLocatedNode.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FProp.sizeInBytes);
  inc(result, FValue.sizeInBytes);
end;

{ TFHIRObject }

constructor TFHIRObject.Create;
begin
  inherited;
  FLocationData := nil;
end;

destructor TFHIRObject.Destroy;
begin
  FLocationData.Free;
  FCommentsStart.Free;
  FCommentsEnd.Free;
  FTags.Free;
  FTag.Free;
  inherited;
end;

procedure TFHIRObject.dropEmpty;
var
  l : TFHIRPropertyList;
  p : TFHIRProperty;
  o : TFHIRObject;
begin
  l := createPropertyList(false);
  try
  for p in l do
  begin
    if p.Values <> nil then
      for o in p.Values do
      begin
        o.dropEmpty;
        if o.isEmpty then
          deleteProperty(p.Name, o);
      end;
  end;
  finally
    l.free
  end;
end;

function TFHIRObject.Link: TFHIRObject;
begin
  result := TFHIRObject(inherited Link);
end;

function TFHIRObject.Clone: TFHIRObject;
begin
  result := TFHIRObject(Inherited Clone);
end;

procedure TFHIRObject.addExtension(url: String; value: TFHIRObject);
begin
  raise EFHIRException.create('Extensions are not supported on this object');
end;

procedure TFHIRObject.Assign(oSource: TFslObject);
begin
  inherited;
  if TFHIRObject(oSource).HasXmlCommentsStart then
    xml_commentsStart.assign(TFHIRObject(oSource).xml_commentsStart)
  else if FCommentsStart <> nil then
  begin
    FCommentsStart.free;
    FCommentsStart := nil;
  end;
  if TFHIRObject(oSource).HasXmlCommentsEnd then
    xml_commentsEnd.assign(TFHIRObject(oSource).xml_commentsEnd)
  else if FCommentsEnd <> nil then
  begin
    FCommentsEnd.free;
    FCommentsEnd := nil;
  end;
end;

function TFHIRObject.createIterator(bInheritedProperties, bPrimitiveValues: Boolean): TFHIRPropertyIterator;
begin
  Result := TFHIRPropertyIterator.create(self, bInheritedProperties, bPrimitiveValues);
end;

function TFHIRObject.createPropertyList(bPrimitiveValues : boolean): TFHIRPropertyList;
begin
  result := TFHIRPropertyList.Create;
  try
    ListProperties(result, true, bPrimitiveValues);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRObject.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRException.create('The property "'+propName+'" on "'+fhirType+'" is unknown"');
end;

procedure TFHIRObject.GetChildrenByName(name: string; list: TFHIRSelectionList);
begin
  // nothing to add here
end;

function TFHIRObject.getTags(name: String): String;
begin
  if FTags = nil then
    FTags := TFslStringDictionary.create;
  if FTags.ContainsKey(name) then
    result := FTags[name]
  else
    result := '';
end;

function TFHIRObject.GetLocationData : TFHIRObjectLocationData;
begin
  if FLocationData = nil then
    FLocationData := TFHIRObjectLocationData.create;
  result := FLocationData;
end;

function TFHIRObject.GetHasLocationData : boolean;
begin
  result := FLocationData <> nil;
end;

function TFHIRObject.findLocation(loc: TSourceLocation; propFrom : TFHIRProperty; path: TFslList<TFHIRLocatedNode>): boolean;
var
  properties : TFHIRPropertyList;
  prop : TFHIRProperty;
  i : integer;
  n : String;
begin
  result := GetLocationData.inSpan(loc);
  if result then
  begin
    path.add(TFHIRLocatedNode.create(propFrom.Link, self.link));

    properties := createPropertyList(true);
    try
      for prop in properties do
      begin
        n := prop.name;
        if n <> '' then
        begin
          for i := 0 to prop.Values.count - 1 do
          begin
            if prop.values[i].findLocation(loc, prop, path) then
              exit;
          end;
        end;
      end;
    finally
      properties.Free;
    end;
  end;
end;

function TFHIRObject.findLocation(loc: TSourceLocation): TFslList<TFHIRLocatedNode>;
begin
  result := TFslList<TFHIRLocatedNode>.create;
  try
    findLocation(loc, nil, result);
    result.Link;
  finally
    result.free;
  end;
end;

function TFHIRObject.fpValue: String;
begin
  if isDateTime then
    result := '@'+primitiveValue
  else
    result := primitiveValue;
end;

procedure TFHIRObject.updateLocationData(start : TSourceLocation; removed, added: TSourceRange; focus : TFHIRObject);
var
  nc : TFHIRNamedValue;
begin
  if self = focus then
  begin
    raise exception.create('not done yet');
  end
  else if start >= LocationData.parseFinish then // if start is after this, then we don't need to do anything
  begin
    if start <= LocationData.parseStart then
      LocationData.parseStart := (LocationData.parseStart - removed) + added;
    LocationData.parseFinish := (LocationData.parseFinish - removed) + added;
    for nc in getNamedChildren do
      nc.value.updateLocationData(start, removed, added, focus);
  end;
end;

function TFHIRObject.getTypesForProperty(propName: string): String;
begin
  raise EFHIRException.create('The property "'+propName+'" is unknown or not a list property (getting types for property)"');
end;

function TFHIRObject.GetFhirObjectVersion: TFHIRVersion;
begin
  result := fhirVersionUnknown;
end;

function TFHIRObject.HasTag(name: String): boolean;
begin
  result := (FTags <> nil) and FTags.ContainsKey(name);
end;

procedure TFHIRObject.ListChildrenByName(name: string; list: TFHIRSelectionList);
begin
  if self <> nil then
    GetChildrenByName(name, list);
end;

procedure TFHIRObject.listFieldsInOrder(fields: TStringList);
begin
  // nothing here
end;

function TFHIRObject.getNamedChildren: TFslList<TFHIRNamedValue>;
var
  list : TFHIRPropertyList;
  p : TFHIRProperty;
  v : TFHIRObject;
begin
  result := TFslList<TFHIRNamedValue>.create;
  try
    list := createPropertyList(true);
    try
      for p in list do
      begin
        if p.hasValue then
          for v in p.Values do
            if p.IsList then
              result.add(TFHIRNamedValue.create(p.Name, p.Type_, v.link, p.Values.link))
            else
              result.add(TFHIRNamedValue.create(p.Name, p.Type_, v.link, nil));
      end;
    finally
      list.free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

procedure TFHIRObject.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  // nothing to add here
end;


procedure TFHIRObject.SetTag(const Value: TFslObject);
begin
  FTag.Free;
  FTag := Value;
end;

procedure TFHIRObject.SetTags(name: String; const Value: String);
begin
  if FTags = nil then
    FTags := TFslStringDictionary.create;
  FTags.AddOrSetValue(name, value);
end;

function TFHIRObject.getDateValue: TFslDateTime;
begin
  result := TFslDateTime.makeNull;
end;

procedure TFHIRObject.deleteProperty(propName: string; propValue: TFHIRObject);
begin
  raise EFHIRException.create('The property "'+propName+'" is unknown deleting the property"');
end;

procedure TFHIRObject.deletePropertyValue(name : String; list: TFHIRObjectList; value: TFHIRObject);
var
  i : integer;
begin
  i := list.IndexByReference(value);
  if (i = -1) then
    raise EFHIRException.create('Unable to find object in '+name+' to remove it');
  list.DeleteByIndex(i);
end;

function TFHIRObject.describe: String;
begin
  result := FhirType;
  if isPrimitive then
    result := result + ': '+primitiveValue;
end;

function TFHIRObject.Equals(other: TObject): boolean;
begin
  result := (other <> nil) and (other.className = className);
end;

function TFHIRObject.extensionCount(url: String): integer;
begin
  result := 0;
end;

function TFHIRObject.extensions(url: String): TFslList<TFHIRObject>;
begin
  result := TFslList<TFHIRObject>.create;
end;

function TFHIRObject.GetCommentsStart: TFslStringList;
begin
  if FCommentsStart = nil then
    FCommentsStart := TFslStringList.Create;
  result := FCommentsStart;
end;

function TFHIRObject.getExtensionString(url: String): String;
begin
  result := '';
end;

procedure TFHIRObject.getProperty(name: String; checkValid: boolean; list: TFslList<TFHIRObject>);
begin
  if checkValid then
    raise EFHIRException.create('Property '+name+' is not valid');
end;

function TFHIRObject.getPropertyValue(propName: string): TFHIRProperty;
var
  list : TFHIRPropertyList;
  p : TFHIRProperty;
begin
  result := nil;
  list := TFHIRPropertyList.Create;
  try
    ListProperties(list, true, true);
    for p in list do
      if (p.Name = propName) or (p.name.endsWith('[x]') and propName.startsWith(p.Name.subString(0, p.Name.length-3))) then
        exit(p.Link);
  finally
    list.Free;
  end;
end;

function TFHIRObject.HasXmlCommentsStart: Boolean;
begin
  result := (FCommentsStart <> nil) and (FCommentsStart.count > 0);
end;

procedure TFHIRObject.insertProperty(propName: string; propValue: TFHIRObject; index: integer);
begin
  raise EFHIRException.create('The property "'+propName+'" is unknown or not a list property (inserting value)"');
end;

function TFHIRObject.isBooleanPrimitive: boolean;
begin
  result := false;
end;

function TFHIRObject.isDateTime: boolean;
begin
  result := false;
end;

function TFHIRObject.isEmpty: boolean;
begin
  result := true;
end;

function TFHIRObject.isEnum: boolean;
begin
  result := false;
end;

function TFHIRObject.isMatchingName(given, expected: String;
  types: array of String): boolean;
var
  s : String;
begin
  if given = expected then
    result := true
  else if not given.startsWith(expected) then
    result := false
  else
  begin
    s := given.subString(expected.length);
    if (s = '[x]') then
      result := true
    else if (length(types) = 1) and (types[0] = '*') then
      result := true
    else
      result := StringArrayExistsSensitive(types, s);
  end;
end;

function TFHIRObject.isMetaDataBased: boolean;
begin
  result := false;
end;

function TFHIRObject.isPrimitive: boolean;
begin
  result := false;
end;

function TFHIRObject.isResource: boolean;
begin
  result := false;
end;

function TFHIRObject.isType: boolean;
begin
  result := false;
end;

function TFHIRObject.JSType: String;
begin
  result := fhirType;
end;

function TFHIRObject.GetCommentsEnd: TFslStringList;
begin
  if FCommentsEnd = nil then
    FCommentsEnd := TFslStringList.Create;
  result := FCommentsEnd;
end;

function TFHIRObject.HasComments: Boolean;
begin
  result := HasXmlCommentsStart or HasXmlCommentsEnd;
end;

function TFHIRObject.hasType(t: String): boolean;
begin
  result := t = fhirType;
end;

function TFHIRObject.hasExtension(url: String): boolean;
begin
  result := false;
end;

function TFHIRObject.hasPrimitiveValue: boolean;
begin
  result := false;
end;

function TFHIRObject.hasType(tl: array of String): boolean;
var
  t : String;
begin
  for t in tl do
    if hasType(t) then
      exit(true);
  exit(false);
end;

function TFHIRObject.HasXmlCommentsEnd: Boolean;
begin
  result := (FCommentsEnd <> nil) and (FCommentsEnd.count > 0);
end;

function TFHIRObject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTags.sizeInBytes);
  inc(result, FTag.sizeInBytes);
  inc(result, FLocationData.sizeInBytes);
  inc(result, FCommentsStart.sizeInBytes);
  inc(result, FCommentsEnd.sizeInBytes);
end;

{ TFHIRObjectText }

constructor TFHIRObjectText.create(value: String);
begin
  Create;
  self.value := value;
end;

constructor TFHIRObjectText.create(value: boolean);
begin
  Create;
  self.value := lowercase(BooleanToString(value));
end;

constructor TFHIRObjectText.create(value: TFslDateTime);
begin
  Create;
  self.value := value.toXML;
end;

constructor TFHIRObjectText.create(value: TBytes);
begin
  Create;
  self.value := String(EncodeBase64(value));
end;

constructor TFHIRObjectText.Create(value: integer);
begin
  Create;
  self.value := inttostr(value);
end;

function TFHIRObjectText.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRObjectText.makeStringValue: not sure how to implement this?');
end;

function TFHIRObjectText.equals(other : TObject): boolean;
begin
  result := inherited equals(other) and (FValue <> TFHIRObjectText(other).FValue);
end;

function TFHIRObjectText.fhirType: String;
begin
  result := 'string';
end;

function TFHIRObjectText.getId: String;
begin
  result := '';
end;

function TFHIRObjectText.getTypesForProperty(propName : string): String;
begin
  raise EFHIRException.create('TFHIRObjectText.makeStringValue: not sure how to implement this?');
end;

function TFHIRObjectText.hasExtensions: boolean;
begin
  result := false;
end;

function TFHIRObjectText.isEmpty: boolean;
begin
  result := inherited isEmpty and (FValue = '');
end;

function TFHIRObjectText.isPrimitive: boolean;
begin
  result := true;
end;

procedure TFHIRObjectText.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  if (bInheritedProperties) Then
    inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'string', false, nil, FValue));
end;

function TFHIRObjectText.makeCodeValue(v: String): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRObjectText.makeStringValue: not sure how to implement this?');
end;

function TFHIRObjectText.makeIntValue(v: String): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRObjectText.makeStringValue: not sure how to implement this?');
end;

function TFHIRObjectText.makeStringValue(v: String): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRObjectText.makeStringValue: not sure how to implement this?');
end;

function TFHIRObjectText.primitiveValue: String;
begin
  result := FValue;
end;

procedure TFHIRObjectText.setIdValue(id: String);
begin
end;

function TFHIRObjectText.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  raise EFHIRException.create('TFHIRObjectText.makeStringValue: not sure how to implement this?');
end;

function TFHIRObjectText.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TFHIRObjectList }

constructor TFHIRObjectList.Create(item: TFHIRObject);
begin
  Create;
  add(item);
end;

constructor TFHIRObjectList.Create(items: TFHIRObjectList);
begin
  Create;
  AddAll(items);
end;

destructor TFHIRObjectList.Destroy;
begin
  FTags.Free;
  FLocationData.Free;;
  inherited;
end;

function TFHIRObjectList.Clone: TFHIRObjectList;
begin
  result := TFHIRObjectList(Inherited Clone);
end;

function TFHIRObjectList.GetEnumerator: TFHIRObjectListEnumerator;
begin
  result := TFHIRObjectListEnumerator.Create(self.link);
end;

function TFHIRObjectList.GetLocationData: TFHIRObjectLocationData;
begin
  if FLocationData = nil then
    FLocationData := TFHIRObjectLocationData.create;
  result := FLocationData;
end;

function TFHIRObjectList.GetHasLocationData: boolean;
begin
  result := assigned(FLocationData);
end;

function TFHIRObjectList.GetItemN(index: Integer): TFHIRObject;
begin
  result := TFHIRObject(ObjectByIndex[index]);
end;

function TFHIRObjectList.getTags(name: String): String;
begin
  if FTags = nil then
    FTags := TFslStringDictionary.create;
  if FTags.ContainsKey(name) then
    result := FTags[name]
  else
    result := '';
end;

function TFHIRObjectList.ItemClass: TFslObjectClass;
begin
  result := TFHIRObject;
end;

function TFHIRObjectList.Link: TFHIRObjectList;
begin
  result := TFHIRObjectList(Inherited Link);
end;

function TFHIRObjectList.new: TFHIRObject;
begin
  result := inherited new as TFHIRObject;
end;

procedure TFHIRObjectList.SetTags(name: String; const Value: String);
begin
  if FTags = nil then
    FTags := TFslStringDictionary.create;
  FTags.AddOrSetValue(name, value);
end;

function TFHIRObjectList.ToString: String;
var
  i : integer;
begin
  result := '(';
  for i := 0 to count - 1 do
  begin
    if (i > 0) then
      result := result + ',';
    result := result + ObjByIndex[i].ToString;
  end;
  result := result + ')';
end;

function TFHIRObjectList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTags.sizeInBytes);
  inc(result, FLocationData.sizeInBytes);
end;

{ TFHIRProperty }

constructor TFHIRProperty.Create(oOwner: TFHIRObject; const sName, sType: String; bList : boolean; cClass : TClass; oObject: TFHIRObject);
begin
  Create;
  FOwner := oOwner;
  FName := sName;
  FType := sType;
  FClass := cClass;
  FIsList := bList;
  FList := TFHIRObjectList.Create;
  if (oObject <> nil) then
  begin
    assert(oObject is TFHIRObject);
    FList.Add(oObject);
  end;
end;

constructor TFHIRProperty.Create(oOwner: TFHIRObject; const sName, sType: String; bList : boolean; cClass : TClass; oList: TFHIRObjectList);
var
  i : integer;
begin
  Create;
  FOwner := oOwner;
  FName := sName;
  FType := sType;
  FClass := cClass;
  FIsList := bList;
  if oList = nil then
    FList := TFHIRObjectList.create
  else
    FList := oList;
  for I := 0 to FList.count - 1 do
    assert(FList[i] is TFHIRObject);
end;

constructor TFHIRProperty.Create(oOwner: TFHIRObject; const sName, sType: String; bList : boolean; cClass : TClass; sValue: String);
begin
  Create;
  FOwner := oOwner;
  FName := sName;
  FType := sType;
  FClass := cClass;
  FIsList := bList;
  FList := TFHIRObjectList.Create;
  if (sValue <> '') then
    FList.Add(Fowner.makeStringValue(sValue));
end;

destructor TFHIRProperty.Destroy;
begin
  FList.free;
  inherited;
end;

procedure TFHIRProperty.forceValues;
begin
  if FList = nil then
    FList := TFHIRObjectList.Create;
end;

function TFHIRProperty.GetHasValue: Boolean;
begin
  result := (FList <> nil) and (Flist.Count > 0);
end;

function TFHIRProperty.Link: TFHIRProperty;
begin
  result := TFHIRProperty(inherited link);
end;

constructor TFHIRProperty.Create(oOwner: TFHIRObject; const sName, sType: String; bList : boolean; cClass : TClass; Value: TBytes);
begin
  Create;
  FOwner := oOwner;
  FName := sName;
  FType := sType;
  FClass := cClass;
  FIsList := bList;
  FList := TFHIRObjectList.Create;
  if (length(value) > 0) then
    FList.Add(FOwner.makeStringValue(String(EncodeBase64(value))));
end;

constructor TFHIRProperty.CreateEnum(oOwner: TFHIRObject; const sName: String; bList: boolean; cClass : TClass; enumName, sValue: String);
begin
  Create;
  FOwner := oOwner;
  FName := sName;
  FType := 'code';
  FClass := cClass;
  FEnumName := enumName;
  FIsList := false;
  FList := TFHIRObjectList.Create;
  if (sValue <> '') then
    FList.Add(FOwner.makeCodeValue(sValue));
end;

constructor TFHIRProperty.Create(oOwner: TFHIRObject; const sName, sType: String; bList: boolean; cClass: TClass);
begin
  Create;
  FOwner := oOwner;
  FName := sName;
  FType := sType;
  FClass := cClass;
  FIsList := bList;
  FList := TFHIRObjectList.Create;
end;

function TFHIRProperty.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FOwner.sizeInBytes);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FType.length * sizeof(char)) + 12);
  inc(result, FList.sizeInBytes);
  inc(result, (FEnumName.length * sizeof(char)) + 12);
end;

class function TFHIRProperty.create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oList : TFslList<TFslObject>) : TFHIRProperty;
var
  o : TFslObject;
begin
  result := TFHIRProperty.create(oOwner, sName, sType, bList, cClass);
  try
    for o in oList do
      result.FList.Add(o.Link);
    result.link;
  finally
    result.free;
  end;
end;

{$IFNDEF FPC}

class function TFHIRProperty.create<T>(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oList : TFslList<T>) : TFHIRProperty;
var
  o : T;
begin
  result := TFHIRProperty.create(oOwner, sName, sType, bList, cClass);
  try
    for o in oList do
      result.FList.Add(o.Link);
    result.link;
  finally
    result.free;
  end;
end;
{$ENDIF}

{ TFHIRPropertyList }

function TFHIRPropertyList.GetEnumerator: TFHIRPropertyListEnumerator;
begin
  result := TFHIRPropertyListEnumerator.Create(self.link);
end;

function TFHIRPropertyList.GetProperty(iIndex: Integer): TFHIRProperty;
begin
  result := TFHIRProperty(ObjectByIndex[iIndex]);
end;

function TFHIRPropertyList.GetPropertyByName(name: String): TFHIRProperty;
var
  p : TFHIRProperty;
begin
  result := nil;
  for p in self do
    if (p.Name = name) then
      exit(p);
end;

function TFHIRPropertyList.Link: TFHIRPropertyList;
begin
  result := TFHIRPropertyList(inherited Link);
end;

{ TFHIRPropertyIterator }

constructor TFHIRPropertyIterator.Create(oFocus: TFHIRObject; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  Create;
  FFocus := oFocus;
  FProperties := TFHIRPropertyList.Create;
  if FFocus <> nil Then
    FFocus.ListProperties(FProperties, bInheritedProperties, bPrimitiveValues);
end;

destructor TFHIRPropertyIterator.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TFHIRPropertyIterator.GetCurrent: TFHIRProperty;
begin
  Result := FProperties[FCursor];
end;

function TFHIRPropertyIterator.More: Boolean;
begin
  result := FCursor < FProperties.Count;
end;

procedure TFHIRPropertyIterator.Next;
begin
  inc(FCursor);
end;

procedure TFHIRPropertyIterator.Reset;
begin
  FCursor := 0;
end;

function TFHIRPropertyIterator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFocus.sizeInBytes);
  inc(result, FProperties.sizeInBytes);
end;

{ TFhirPropertyListEnumerator }

Constructor TFhirPropertyListEnumerator.Create(list : TFhirPropertyList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirPropertyListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirPropertyListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirPropertyListEnumerator.GetCurrent : TFhirProperty;
begin
  Result := FList[FIndex];
end;

{ TFhirObjectListEnumerator }

Constructor TFhirObjectListEnumerator.Create(list : TFhirObjectList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirObjectListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirObjectListEnumerator.MoveNext : boolean;
begin
  Inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirObjectListEnumerator.GetCurrent : TFhirObject;
begin
  Result := FList[FIndex];
end;


function noList(e : TFHIRObjectList) : boolean;
begin
  result := (e = nil) or (e.Count = 0);
end;

function objectsEqual(o1, o2 : TFHIRObject) : boolean; // true if all properties are equal, recursively
begin
  result := compareDeep(o1, o2, false);
end;

function objectsEquivalent(o1, o2 : TFHIRObject) : boolean; // true if all properties that exist on both objects are equivalent, recursively
var
  pl1, pl2 : TFHIRPropertyList;
  p1, p2 : TFHIRProperty;
  i : integer;
begin
  if o1.isPrimitive and o2.isPrimitive then
    exit(sameText(o1.primitiveValue, o2.primitiveValue));

  result := true;
  pl1 := TFHIRPropertyList.Create;
  pl2 := TFHIRPropertyList.Create;
  try
    o1.ListProperties(pl1, true, true);
    o2.ListProperties(pl2, true, true);
    for p1 in pl1 do
    begin
      if p1.hasValue then
      begin
        p2 := pl2.GetPropertyByName(p1.Name);
        if (p2 <> nil) and (p2.hasValue) then
        begin
          if p1.Values.Count <> p2.Values.Count then
            exit(false);
          for i := 0 to p1.Values.Count - 1 do
            if not objectsEquivalent(p1.Values[i], p2.Values[i]) then
              exit(false);
        end;
      end;
    end;
  finally
    pl2.free;
    pl1.free;
  end;
end;

function compareDeep(e1, e2 : TFHIRObjectList; allowNull : boolean) : boolean;
var
  i : integer;
begin
  if noList(e1) and noList(e2) and (allowNull) then
    result := true
  else if (noList(e1)) or (noList(e2)) then
    result := false
  else if (e1.Count <> e2.Count) then
    result := false
  else
  begin
    result := true;
    for i := 0 to e1.Count - 1 do
      if (not compareDeep(e1.get(i) as TFHIRObject, e2.get(i) as TFHIRObject, allowNull)) then
        result := false;
  end;
end;

function compareDeep(e1, e2 : TFHIRObject; allowNull : boolean) : boolean;
begin
  if (e1 = nil) and (e2 = nil) and (allowNull) then
    result := true
  else if (e1 = nil) or (e2 = nil) then
    result := false
  else if (e2.isMetaDataBased) then
    result := e2.equals(e1)
  else
    result := e1.equals(e2);
end;

function TFHIRObject.primitiveValue: string;
begin
  result := '';
end;

procedure TFHIRObject.reorderProperty(propName: string; source, destination: integer);
begin
  raise EFHIRException.create('The property "'+propName+'" is unknown or not a list reordering the property"');
end;

procedure TFHIRObject.replaceProperty(propName: string; existing: TFHIRObject;
  new: TFHIRObject);
begin
  raise EFHIRException.create('The property "'+propName+'" is unknown replacing the property"');
end;

procedure TFHIRObject.replacePropertyValue(name: String; list: TFHIRObjectList; existing, new: TFHIRObject);
var
  i : integer;
begin
  i := list.IndexByReference(existing);
  if (i = -1) then
    raise EFHIRException.create('Unable to find object in '+name+' to remove it');
  list.SetItem(i, new);
end;

function TFHIRObject.SerialiseUsingProperties: boolean;
begin
  result := false;
end;

procedure TFHIRObject.SetDateValue(Value: TFslDateTime);
begin
  raise Exception.Create('This object of type '+className+' does not support date value');
end;

function TFHIRObject.setProperty(propName: string; propValue: TFHIRObject): TFHIRObject;
begin
  raise EFHIRException.create('The property "'+propName+'" is unknown or not a list property (setting property)"');
end;

function TFHIRObject.ToString: String;
begin
  if isPrimitive then
    result := fhirType+'[''' + primitiveValue + ''']'
  else
    result := fhirType;
end;

function isPrimitiveType(name : String) : boolean;
begin
  result := StringArrayExistsSensitive(['integer', 'unsignedInt', 'positiveInt', 'decimal', 'dateTime', 'date',
    'time', 'instant', 'string', 'uri', 'oid', 'uuid', 'id', 'boolean', 'code', 'markdown', 'xhtml', 'base64Binary', 'canonical', 'url'], name)
    or name.StartsWith('http://hl7.org/fhirpath/System.')
end;

{ TFHIRSelection }

constructor TFHIRSelection.Create(focus: TFHIRObject);
begin
  inherited create;
  FValue := focus;
end;

constructor TFHIRSelection.Create(parent : TFHIRObject; name : String; focus: TFHIRObject);
begin
  inherited create;
  FParent := parent;
  FName := name;
  FValue := focus;
end;

function TFHIRSelection.createPropertyValue(propName: string): TFHIRObject;
begin
  result := value.createPropertyValue(propName);
end;

destructor TFHIRSelection.Destroy;
begin
  FParent.Free;
  FValue.Free;
  inherited;
end;

function TFHIRSelection.fhirType: String;
begin
  result := value.fhirType;
end;

function TFHIRSelection.getId: String;
begin
  result := value.getId;
end;

function TFHIRSelection.getTypesForProperty(propName : string): String;
begin
  result := value.getTypesForProperty(propName);
end;

function TFHIRSelection.hasExtensions: boolean;
begin
  result := FValue.hasExtensions;
end;

function TFHIRSelection.Link: TFHIRSelection;
begin
  result := TFHIRSelection(inherited link);
end;

function TFHIRSelection.makeCodeValue(v: String): TFHIRObject;
begin
  result := value.makeCodeValue(v);
end;

function TFHIRSelection.makeIntValue(v: String): TFHIRObject;
begin
  result := value.makeIntValue(v);
end;

function TFHIRSelection.makeStringValue(v: String): TFHIRObject;
begin
  result := value.makeStringValue(v);
end;

procedure TFHIRSelection.setIdValue(id: String);
begin
  value.setIdValue(id);
end;

function TFHIRSelection.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  result := value.setProperty(propName, propValue);
end;

function TFHIRSelection.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FValue.sizeInBytes);
end;

{ TFHIRSelectionList }

constructor TFHIRSelectionList.Create(parent : TFHIRObject; name : String; focus: TFHIRObject);
begin
  inherited create;
  Add(parent, name, focus);
end;

constructor TFHIRSelectionList.Create(focus: TFHIRObject);
begin
  inherited create;
  Add(focus);
end;

constructor TFHIRSelectionList.Create;
begin
  inherited Create;
end;

function TFHIRSelectionList.Link: TFHIRSelectionList;
begin
  result := TFHIRSelectionList(inherited link);
end;

procedure TFHIRSelectionList.add(parent : TFHIRObject; name : String; value: TFHIRObject);
begin
  add(TFHIRSelection.Create(parent, name, value));
end;

procedure TFHIRSelectionList.addAll(value: TFslList<TFHIRObject>);
var
  o : TFHIRObject;
begin
  if value <> nil then
    for o in value do
      add(nil, '', o.Link);
end;

procedure TFHIRSelectionList.addAll(value: TFHIRObjectList);
begin
  addAll(nil, '', value);
end;

procedure TFHIRSelectionList.addAll(parent: TFHIRObject; name: String; value: TFHIRObjectList);
var
  o : TFHIRObject;
begin
  if value <> nil then
    for o in value do
      add(parent.Link, name, o.Link);
end;

procedure TFHIRSelectionList.add(value: TFHIRObject);
begin
  add(TFHIRSelection.Create(value));
end;

function noList(e : TFHIRSelectionList) : boolean; overload;
begin
  result := (e = nil) or (e.Count = 0);
end;


function TFHIRSelectionList.asValues: TFHIRObjectList;
var
  s : TFHIRSelection;
begin
  result := TFHIRObjectList.Create;
  try
    for s in self do
      result.Add(s.value.link);
    result.Link;
  finally
    result.Free;
  end;
end;

function TFHIRSelectionList.describeAsPath: String;
var
  sel : TFHIRSelection;
begin
  result := '';
  for sel in self do
  begin
    if result = '' then
      result := sel.name
    else
      result := result + '.'+sel.name;
  end;
end;

function TFHIRSelectionList.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

class function TFHIRSelectionList.compareDeep(e1, e2: TFHIRSelectionList; allowNull: boolean): boolean;
var
  i : integer;
begin
  if noList(e1) and noList(e2) and (allowNull) then
    result := true
  else if (noList(e1)) or (noList(e2)) then
    result := false
  else if (e1.Count <> e2.Count) then
    result := false
  else
  begin
    result := true;
    for i := 0 to e1.Count - 1 do
      if (not fhir_objects.compareDeep(e1[i].value as TFHIRObject, e2[i].value as TFHIRObject, allowNull)) then
        result := false;
  end;
end;

function isEmptyProp(v : TFHIRObject) : boolean;
begin
  result := (v = nil) or v.isEmpty;
end;

function isEmptyProp(v : TFHIRObjectList) : boolean;
var
  o : TFHIRObject;
begin
  result := (v = nil) or (v.Count = 0);
  if not result then
  begin
    result := true;
    for o in v do
      if not isEmptyProp(o) then
        exit(false);
  end;
end;

{ TFHIRWorkerContextV }

constructor TFHIRWorkerContextV.Create;
begin
  inherited;
  FLang := THTTPLanguages.create('en');
end;

function TFHIRWorkerContextV.GetVersion: TFHIRVersion;
begin
  result := fhirVersionUnknown;
end;

function TFHIRWorkerContextV.link: TFHIRWorkerContextV;
begin
  result := TFHIRWorkerContextV(inherited Link);
end;

function TFHIRWorkerContextV.versionString: String;
begin
  result := CODES_TFHIRVersion[version];
end;

{ TFHIRResourceV }

function TFHIRResourceV.GetProfileVersion: TFHIRVersion;
begin
  result := fhirVersionUnknown;
end;

function TFHIRResourceV.isDomainResource: boolean;
begin
  result := false;
end;

function TFHIRResourceV.isResource: boolean;
begin
  result := true;
end;

function TFHIRResourceV.link: TFHIRResourceV;
begin
  result := TFHIRResourceV(inherited Link);
end;

procedure TFHIRResourceV.SetProfileVersion(Value: TFHIRVersion);
begin
  // nothing
end;

{ TValidationResult }

constructor TValidationResult.Create(Severity: TIssueSeverity; Message: String);
begin
  inherited create;
  FSeverity := Severity;
  FMessage := Message;
end;

constructor TValidationResult.Create;
begin
  Inherited Create;
end;

constructor TValidationResult.Create(display: String);
begin
  inherited Create;
  FDisplay := display;
end;

function TValidationResult.isOk: boolean;
begin
  result := not (Severity in [isError, isFatal]);
end;


function TValidationResult.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FMessage.length * sizeof(char)) + 12);
  inc(result, (FDisplay.length * sizeof(char)) + 12);
end;

{ EFHIRUnsupportedVersion }

constructor EFHIRUnsupportedVersion.Create(version: TFHIRVersion; action: String);
begin
  inherited Create('Version '+FHIR_VERSIONS[version]+' not supported '+action);
end;

{ TFHIRSystemString }

constructor TFHIRSystemString.Create(v: String);
begin
  inherited Create;
  FValue := v;
end;

function TFHIRSystemString.fhirType: String;
begin
  result := 'string';
end;

procedure TFHIRSystemString.GetChildrenByName(name: string; list: TFHIRSelectionList);
begin
end;

function TFHIRSystemString.GetFhirObjectVersion: TFHIRVersion;
begin
  result := fhirVersionUnknown;
end;

function TFHIRSystemString.getId: String;
begin
  result := '';
end;

function TFHIRSystemString.hasExtensions: boolean;
begin
  result := false;
end;

function TFHIRSystemString.hasPrimitiveValue: boolean;
begin
  result := true;
end;

function TFHIRSystemString.isPrimitive: boolean;
begin
  result := true;
end;


procedure TFHIRSystemString.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
end;


function TFHIRSystemString.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFHIRSystemString.Create(v);
end;


function TFHIRSystemString.makeIntValue(v: String): TFHIRObject;
begin
  result := TFHIRSystemString.Create(v);
end;


function TFHIRSystemString.makeStringValue(v: String): TFHIRObject;
begin
  result := TFHIRSystemString.Create(v);
end;


function TFHIRSystemString.primitiveValue: string;
begin
  result := FValue;
end;


procedure TFHIRSystemString.setIdValue(id: String);
begin

end;

function TFHIRSystemString.ToString: String;
begin
  result := FValue;
end;

function TFHIRSystemString.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TFHIRSystemTuple }

constructor TFHIRSystemTuple.Create;
begin
  inherited;
  FFields := TFslMap<TFhirObject>.create('tuple');
end;

destructor TFHIRSystemTuple.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TFHIRSystemTuple.fhirType: String;
begin
  result := 'Tuple';
end;

procedure TFHIRSystemTuple.GetChildrenByName(name: string; list: TFHIRSelectionList);
begin
  if FFields.ContainsKey(name) then
    list.add(FFields[name].Link);
end;


function TFHIRSystemTuple.GetFhirObjectVersion: TFHIRVersion;
begin
  result := fhirVersionUnknown;
end;


function TFHIRSystemTuple.getId: String;
begin
  result := '';
end;


function TFHIRSystemTuple.hasExtensions: boolean;
begin
  result := false;
end;


function TFHIRSystemTuple.hasPrimitiveValue: boolean;
begin
  result := false;
end;


function TFHIRSystemTuple.isPrimitive: boolean;
begin
  result := false;
end;


function TFHIRSystemTuple.link: TFHIRSystemTuple;
begin
  result := TFHIRSystemTuple(inherited link);
end;

procedure TFHIRSystemTuple.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
end;


function TFHIRSystemTuple.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFHIRSystemString.Create(v);
end;


function TFHIRSystemTuple.makeIntValue(v: String): TFHIRObject;
begin
  result := TFHIRSystemString.Create(v);
end;


function TFHIRSystemTuple.makeStringValue(v: String): TFHIRObject;
begin
  result := TFHIRSystemString.Create(v);
end;


procedure TFHIRSystemTuple.setIdValue(id: String);
begin
end;


function TFHIRSystemTuple.ToString: String;
begin
  result := FFields.ToString;
end;

function TFHIRSystemTuple.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFields.sizeInBytes);
end;

class function TFHIRSystemTuple.fromParams(pm: THTTPParameters): TFHIRSystemTuple;
var
  this : TFHIRSystemTuple;
  i : integer;
begin
  this := TFHIRSystemTuple.Create;
  try
    for i := 0 to pm.Count - 1 do
      this.Fields.AddOrSetValue(pm.Name[i], TFHIRSystemString.Create(pm[pm.Name[i]]));
    result := this.Link;
  finally
    this.Free;
  end;
end;


Type
  TFHIRMessage = class (TFslObject)
  private
    FMessages : TFslStringDictionary;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  GMessages : TFslMap<TFHIRMessage>;

Function LoadSource : TBytes;
begin
  result := fsl_stream.FileToBytes(IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0)))+'translations.xml');
end;

procedure LoadMessages;
var
  source : TMXmlDocument;
  child, lang : TMXmlElement;
  msg : TFHIRMessage;
begin
  if FHIRExeModuleName = '##' then
    exit;

  if GMessages <> nil then
    exit;

  source := TMXmlParser.parse(LoadSource, [xpDropWhitespace, xpDropComments]);
  try
    GMessages := TFslMap<TFHIRMessage>.create('MXml Parser');
    child := source.document.firstElement;
    while child <> nil do
    begin
      msg := TFHIRMessage.Create;
      GMessages.Add(child.attribute['id'], msg);
      lang := child.firstElement;
      while lang <> nil do
      begin
        msg.FMessages.add(lang.attribute['lang'], lang.text);
        lang := lang.nextElement;
      end;
      child := child.nextElement;
    end;
  finally
    source.free;
  end;
end;

function GetFhirMessage(id : String; const lang : THTTPLanguages) : String;
begin
  result := GetFhirMessage(id, lang, '??');
end;

function GetFhirMessage(id : String; const lang : THTTPLanguages; def : String) : String;
var
  msg : TFHIRMessage;
  l : string;
begin
  result := '';
  if GMessages = nil then
    LoadMessages;
  if not GMessages.ContainsKey(id) then
    result := def
  else
  begin
    msg := GMessages[id];
    for l in lang.codes do
      if (result = '') and (msg.FMessages.ContainsKey(l)) then
        result := msg.FMessages[l];
    if result = '' then
      result := msg.FMessages['en'];
    if result = '' then
      result := '??';
    if result = '' then
      result := def;
  end;
end;

{ TFHIRMessage }

constructor TFHIRMessage.Create;
begin
  inherited;
  FMessages := TFslStringDictionary.create;
end;

destructor TFHIRMessage.Destroy;
begin
  FMessages.Free;
  inherited;
end;

function TFHIRMessage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMessages.sizeInBytes);
end;

{ EFHIRException }

constructor EFHIRException.CreateLang(code : String; const lang : THTTPLanguages);
begin
  inherited Create(GetFhirMessage(code, lang));
end;

constructor EFHIRException.CreateLang(code : String; const lang : THTTPLanguages; const Args: array of const);
begin
  inherited Create(Format(GetFhirMessage(code, lang), args));
end;


{ ERestfulException }

constructor ERestfulException.Create(const sContext: String; aStatus: word; code: TFhirIssueType; sMessage : String; const lang : THTTPLanguages; const Args: array of const);
begin
  inherited Create(Format(GetFhirMessage(sMessage, lang), args));

  FContext := sContext;
  FStatus := aStatus;
  FCode := code;
end;

constructor ERestfulException.Create(const sContext: String; aStatus: word; code: TFhirIssueType; sMessage : String; const lang : THTTPLanguages);
begin
  inherited Create(GetFhirMessage(sMessage, lang, sMessage));
  FContext := sContext;
  FStatus := aStatus;
  FCode := code;
end;

function languageMatches(spec, possible : String) : boolean;
begin
  result := spec = possible; // todo: make this better
end;

{ EFHIRPath }

constructor EFHIRPath.create(path: String; offset: integer; problem: String);
begin
  inherited create('fhir2_pathengine error "'+problem+'" at position '+inttostr(offset)+' in "'+path+'"');
end;

constructor EFHIRPath.create(problem: String);
begin
  inherited create(problem);
end;

{ EFHIRTodo }

constructor EFHIRTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

{ EDefinitionExceptionTodo }

constructor EDefinitionExceptionTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

{ EFHIRPathTodo }

constructor EFHIRPathTodo.Create(place: String);
begin
  inherited create('Not done yet @ '+place);
end;

initialization
  GMessages := nil;
finalization
  GMessages.Free;
end.
