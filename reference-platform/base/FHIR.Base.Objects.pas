unit FHIR.Base.Objects;

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

Interface

Uses
  SysUtils, Classes, Generics.Collections, {$IFNDEF VER260} System.NetEncoding, {$ENDIF}
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Collections;

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
  TFHIRVersion = (fhirVersionUnknown, fhirVersionRelease1, fhirVersionRelease2, fhirVersionRelease3, fhirVersionRelease4);
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

  CODES_TFHIRVersion : Array [TFHIRVersion] of String = ('', 'r1', 'r2', 'r3', 'r4');
  CODES_FHIR_GENERATED_PUBLICATION : array [TFHIRVersion] of string = ('', '1', '2', '3', '4');
  PF_CONST : array [TFHIRVersion] of string = ('', '0.0', '1.0', '3.0', '3.4');


  FHIR_ALL_VERSIONS = [fhirVersionUnknown, fhirVersionRelease1, fhirVersionRelease2, fhirVersionRelease3, fhirVersionRelease4];
  FHIR_VERSIONS : Array [TFHIRVersion] of String = ('', '0.0.82', '1.0.2', '3.0.1', '3.4.0');
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
    fcmdConformanceStmt, { get the conformance statement for the system}
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

  EFHIRUnsupportedVersion = class (EFslException)
  public
    constructor Create(version : TFHIRVersion; action : String);
  end;

//  TFhirTag = class (TFslName)
//  private
//    FKey : integer;
//    FDisplay : String;
//    FKind : TFhirTagKind;
//    FUri : String;
//    FCode : String;
//  public
//    function combine : String;
//
//    property Key : integer read FKey write FKey;
//    property Kind : TFhirTagKind read FKind write FKind;
//    property Uri : String read FUri write FUri;
//    property Code : String read FCode write FCode;
//    property Display : String read FDisplay write FDisplay;
//  end;
  TFhirResourceTypeV = string;

Const
  FHIR_NS = 'http://hl7.org/fhir';
  CODES_TFHIRCommandType : array [TFHIRCommandType] of String = (
    'Unknown', 'Read', 'VersionRead', 'Update', 'Delete', 'HistoryInstance', 'Create', 'Search', 'HistoryType', 'Validate', 'ConformanceStmt', 'Transaction', 'HistorySystem', 'Upload', 'Operation', 'Patch', 'Batch', 'WebUI', 'Task', 'delete task', 'Null');
  CODES_TFHIRHtmlNodeType : array [TFHIRHtmlNodeType] of String = ('Element', 'Text', 'Comment', 'Document');
  CODES_TFHIRFormat : Array [TFHIRFormat] of String = ('Unspecified', 'XML', 'JSON', 'RDF/Turtle', 'Text Representation', 'Newline delimited JSON', 'XHTML');
  EXT_ACTUAL_TFHIRFormat : Array [TFHIRFormat] of String = ('.bin', '.xml', '.json', '.ttl', '.txt', '.ndjson', '.html');
  EXT_WEB_TFHIRFormat : Array [TFHIRFormat] of String = ('.bin', '.xml', '.json', '.ttl', '.txt', '.ndjson', '.xml');
  MIMETYPES_TFHIRFormat : Array [TFHIRFormat] of String = ('', 'application/fhir+xml', 'application/fhir+json', 'text/turtle; x-dialect=fhir', 'text/fhir', 'application/x-ndjson', 'text/xhtml');
  MIMETYPES_TFHIRFormat_Version : Array [TFHIRFormat, TFHIRVersion] of String = (
    ('', '', '', '', ''),
    ('', 'application/xml+fhir', 'application/xml+fhir', 'application/fhir+xml', 'application/fhir+xml'),
    ('', 'application/json+fhir', 'application/json+fhir', 'application/fhir+json', 'application/fhir+json'),
    ('','','','text/turtle; x-dialect=fhir','text/turtle; x-dialect=fhir'),
    ('','','','text/fhir','text/fhir'),
    ('','','','','application/x-ndjson'),
    ('', 'text/xhtml', 'text/xhtml', 'text/xhtml', 'text/xhtml')
    );

  Names_TFHIRAuthProvider : Array [TFHIRAuthProvider] of String = ('', 'Custom', 'Facebook', 'Google', 'HL7');
  USER_SCHEME_IMPLICIT = 'http://healthintersections.com.au/fhir/user/implicit';
  USER_SCHEME_PROVIDER : array [TFHIRAuthProvider] of String =
    ('', 'http://healthintersections.com.au/fhir/user/explicit', 'http://www.facebook.com', 'http://www.google.com', 'http://www.hl7.org');
  CODES_TFHIRSummaryOption : array [TFHIRSummaryOption] of String = ('Full', 'Summary', 'Text', 'Data', 'Count');

type

  TFHIRObject = class;
  TFHIRObjectList = class;
  TFHIRPropertyList = class;
  TFHIRSelection = class;
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
  Public
    Constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oObject : TFHIRObject); Overload;
    Constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oList : TFHIRObjectList); Overload;
    Constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oList : TFslList<TFHIRObject>); Overload;
    Constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; sValue : String); Overload;
    Constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; Value : TBytes); Overload;
    Constructor CreateEnum(oOwner : TFHIRObject; Const sName : String;     bList: boolean; cClass : TClass; enumName : String; sValue : String); Overload;
    Destructor Destroy; Override;

    Function Link : TFHIRProperty; overload;

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
    Constructor Create(list : TFHIRPropertyList);
    Destructor Destroy; override;
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
  public
    Constructor Create(oFocus : TFHIRObject; bInheritedProperties, bPrimitiveValues : Boolean); overload;
    Destructor Destroy; Override;
    Procedure Next;
    Procedure Reset;
    Function More : Boolean;
    Property Current : TFHIRProperty read GetCurrent;
  End;

  {$M+}
  TFHIRObject = class (TFslObject)
  private
    FTags : TFslStringDictionary;
    FTag : TFslObject;
    FTagObject : TObject;
    FLocationStart : TSourceLocation;
    FLocationEnd : TSourceLocation;
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
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); virtual;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Virtual;

    procedure deletePropertyValue(name : String; list : TFHIRObjectList; value : TFHIRObject);
    procedure replacePropertyValue(name : String; list : TFHIRObjectList; existing, new : TFHIRObject);

    function GetFhirObjectVersion: TFHIRVersion; virtual;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    function Link : TFHIRObject;
    function Clone : TFHIRObject; Overload;
    procedure Assign(oSource : TFslObject); override;

    // version delegation. these 3 make* are public for internal reasons, but there's never any reason to use these outside the library
    function makeStringValue(v : String) : TFHIRObject; virtual; abstract;
    function makeCodeValue(v : String) : TFHIRObject; virtual; abstract;
    function makeIntValue(v : String) : TFHIRObject; virtual; abstract;
    function hasExtension(url : String) : boolean; virtual;
    function getExtensionString(url : String) : String; virtual;
    function extensionCount(url : String) : integer; virtual;
    function extensions(url : String) : TFslList<TFHIRObject>; virtual;
    procedure addExtension(url : String; value : TFHIRObject); virtual;

    procedure ListChildrenByName(name : string; list : TFHIRSelectionList);

    // property access API
    // getting access to the properties
    function createPropertyList(bPrimitiveValues : boolean) : TFHIRPropertyList;
    function createIterator(bInheritedProperties, bPrimitiveValues : Boolean) : TFHIRPropertyIterator;

    // create a class that is the correct type for the named property
    function createPropertyValue(propName : string): TFHIRObject; virtual; abstract;
    function getPropertyValue(propName : string): TFHIRProperty; virtual;

    // set the value of the property. For properties with cardinality > 1, append to the list, or use insertProperty
    procedure setProperty(propName : string; propValue : TFHIRObject); virtual; abstract;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); virtual;

    // delete the value. propValue is used where cardinality > 1, to pick the correct item to delete
    procedure deleteProperty(propName : string; propValue : TFHIRObject); virtual;

    // replace the value of the property with a new value
    procedure replaceProperty(propName : string; existing : TFHIRObject; new : TFHIRObject); virtual;
    procedure reorderProperty(propName : string; source, destination : integer); virtual;

    // tags...
    Property Tags[name : String] : String read getTags write SetTags;
    function HasTag(name : String): boolean; overload;
    property Tag : TFslObject read FTag write SetTag;
    property TagObject : TObject read FTagObject write FTagObject; // no ownership....
    property TagInt : integer read FTagInt write FTagInt;

    // javascript caching
    property jsInstance : cardinal read FJsInstance write FJsInstance;
    property jsHandle : pointer read FJsHandle write FJsHandle;

    // populated by some parsers when parsing
    property LocationStart : TSourceLocation read FLocationStart write FLocationStart;
    property LocationEnd : TSourceLocation read FLocationEnd write FLocationEnd;

    function HasXmlCommentsStart : Boolean;
    function HasXmlCommentsEnd : Boolean;
    function HasComments : Boolean;
    function fhirType : String; virtual; abstract;
    function getId : String; virtual; abstract;
    procedure setIdValue(id : String); virtual; abstract;
    function isPrimitive : boolean; virtual;
    function isResource : boolean; virtual;
    function isEnum : boolean; virtual;
    function isType : boolean; virtual;
    function hasPrimitiveValue : boolean; virtual;
    function primitiveValue : string; virtual;
    function isMetaDataBased : boolean; virtual;
//    Function PerformQuery(path : String) : TFHIRObjectList;
    function hasType(t : String) : boolean; overload;
    function hasType(tl : Array of String) : boolean; overload;
    function describe : String; virtual;
    procedure getProperty(name : String; checkValid : boolean; list : TFslList<TFHIRObject>); virtual;
    function ToString : String; override;
    function isEmpty : boolean; virtual;
    procedure dropEmpty;
    function equalsDeep(other : TFHIRObject) : boolean; virtual;
    function equalsShallow(other : TFHIRObject) : boolean; virtual;
  public
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

  TFHIRObjectListEnumerator = class (TFslObject)
  private
    FIndex : integer;
    FList : TFHIRObjectList;
    function GetCurrent : TFHIRObject;
  public
    Constructor Create(list : TFHIRObjectList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFHIRObject read GetCurrent;
  end;

  TFHIRObjectList = class (TFslObjectList)
  private
    FTags : TFslStringDictionary;
    FJsHandle: pointer;
    FJsInstance: cardinal;
    Function GetItemN(index : Integer) : TFHIRObject;
    procedure SetTags(name: String; const Value: String);
    function getTags(name: String): String;
  protected
    function ItemClass : TFslObjectClass; override;
  public
    Constructor Create(item : TFHIRObject); overload;
    Constructor Create(items : TFHIRObjectList); overload;
    Destructor Destroy; override;
    function Link : TFHIRObjectList; Overload;
    function Clone : TFHIRObjectList; Overload;
    function GetEnumerator : TFHIRObjectListEnumerator;
    Property ObjByIndex[index : Integer] : TFHIRObject read GetItemN; default;
    Property Tags[name : String] : String read getTags write SetTags;
    function ToString : String; override;
    function new : TFHIRObject; reintroduce; overload; virtual;

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
    function GetVersion: TFHIRVersion; virtual;
  public
    function link : TFHIRWorkerContextV; overload;

    procedure loadResourceJson(rtype, id : String; json : TStream); virtual; abstract;
    Property version : TFHIRVersion read GetVersion;
    function versionString : String;

  end;

  TFHIRObjectText = class (TFHIRObject)
  private
    FValue : String;
  protected
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
  public
    constructor create(value : String); Overload;
    constructor create(value : TDateTimeEx); Overload;
    constructor create(value : boolean); Overload;
    constructor create(value : TBytes); Overload;

    function createPropertyValue(propName : string): TFHIRObject; override;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function fhirType : String; override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;

    property value : string read FValue write FValue;
    function isEmpty : boolean; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function equalsDeep(other : TFHIRObject) : boolean; override;
    function equalsShallow(other : TFHIRObject) : boolean; override;
  end;

  TFHIRSelection = class (TFhirObject)
  private
    FParent : TFHIRObject;
    FName : String;
    FValue : TFHIRObject;
  public
    Constructor Create(focus : TFHIRObject); overload;
    Constructor Create(parent : TFHIRObject; name : String; focus : TFHIRObject); overload;
    Destructor Destroy; override;
    function Link : TFHIRSelection; overload;
    property parent : TFHIRObject read FParent;
    property name : String read FName;
    property value : TFHIRObject read FValue;

    function createPropertyValue(propName : string): TFHIRObject; override;
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function fhirType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
  end;

  TFHIRSelectionList = class (TFslList<TFHIRSelection>)
  public
    Constructor Create; overload; override;
    Constructor Create(focus : TFHIRObject); overload;
    Constructor Create(parent : TFHIRObject; name : String; focus : TFHIRObject); overload;
    function Link : TFHIRSelectionList; overload;
    procedure add(value : TFHIRObject); overload;
    procedure add(parent : TFHIRObject; name : String; value : TFHIRObject); overload;
    procedure addAll(value : TFHIRObjectList); overload;
    procedure addAll(value : TFslList<TFHIRObject>); overload;
    procedure addAll(parent : TFHIRObject; name : String; value : TFHIRObjectList); overload;

    function asValues : TFHIRObjectList;

    class function compareDeep(e1, e2 : TFHIRSelectionList; allowNull : boolean) : boolean;
  end;

  TValidationResult = class (TFslObject)
  private
    FSeverity : TIssueSeverity;
    FMessage  : String;
    FDisplay: String;
  public
    constructor Create; overload; override;
    constructor Create(Severity : TIssueSeverity; Message : String); overload; virtual;
    constructor Create(display : String); overload; virtual;
    Property Severity : TIssueSeverity read FSeverity write FSeverity;
    Property Message : String read FMessage write FMessage;
    Property Display : String read FDisplay write FDisplay;
    function isOk : boolean;
  end;

function noList(e : TFHIRObjectList) : boolean; overload;
function compareDeep(e1, e2 : TFHIRObjectList; allowNull : boolean) : boolean; overload;
function compareDeep(e1, e2 : TFHIRObject; allowNull : boolean) : boolean; overload;
function isPrimitiveType(name : String):boolean;
function isEmptyProp(v : TFHIRObject) : boolean; overload;
function isEmptyProp(v : TFHIRObjectList) : boolean; overload;

Implementation

Uses
  EncdDecd,
  FHIR.Base.Lang;

{ TFHIRObject }

constructor TFHIRObject.Create;
begin
  inherited;
  FLocationStart := nullLoc;
  FLocationEnd := nullLoc;
end;

destructor TFHIRObject.Destroy;
begin
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

function TFHIRObject.equalsDeep(other: TFHIRObject): boolean;
begin
  result := (other <> nil) and (other.className = className);
end;

function TFHIRObject.equalsShallow(other: TFHIRObject): boolean;
begin
  result := other <> nil;
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

procedure TFHIRObject.insertProperty(propName: string; propValue: TFHIRObject;
  index: integer);
begin
  raise EFHIRException.create('The property "'+propName+'" is unknown or not a list property (inserting value)"');
end;

function TFHIRObject.isEmpty: boolean;
begin
  result := true;
end;

function TFHIRObject.isEnum: boolean;
begin
  result := false;
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

constructor TFHIRObjectText.create(value: TDateTimeEx);
begin
  Create;
  self.value := value.toXML;
end;

constructor TFHIRObjectText.create(value: TBytes);
begin
  Create;
  self.value := String(EncodeBase64(@value[0], length(value)));
end;

function TFHIRObjectText.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRException.create('TFHIRObjectText.makeStringValue: not sure how to implement this?');
end;

function TFHIRObjectText.equalsDeep(other: TFHIRObject): boolean;
begin
  result := inherited equalsDeep(other) and (FValue <> TFHIRObjectText(other).FValue);
end;

function TFHIRObjectText.equalsShallow(other: TFHIRObject): boolean;
begin
  result := equalsDeep(other);
end;

function TFHIRObjectText.fhirType: String;
begin
  raise EFHIRException.create('TFHIRObjectText.makeStringValue: not sure how to implement this?');
end;

function TFHIRObjectText.getId: String;
begin
  result := '';
end;

function TFHIRObjectText.isEmpty: boolean;
begin
  result := inherited isEmpty and (FValue = '');
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

procedure TFHIRObjectText.setIdValue(id: String);
begin
end;

procedure TFHIRObjectText.setProperty(propName: string; propValue: TFHIRObject);
begin
  raise EFHIRException.create('TFHIRObjectText.makeStringValue: not sure how to implement this?');
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
    FList.Add(FOwner.makeStringValue(String(EncodeBase64(@value[0], length(value)))));
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

constructor TFHIRProperty.Create(oOwner: TFHIRObject; const sName, sType: String; bList: boolean; cClass: TClass; oList: TFslList<TFHIRObject>);
var
  i : integer;
begin
  Create;
  FOwner := oOwner;
  FName := sName;
  FType := sType;
  FClass := cClass;
  FIsList := bList;
  FList := TFHIRObjectList.create;
  for I := 0 to oList.count - 1 do
    FList.Add(oList[i].Link);
end;

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
    result := e2.equalsDeep(e1)
  else
    result := e1.equalsDeep(e2);
end;

function TFHIRObject.primitiveValue: string;
begin
  result := '';
end;

procedure TFHIRObject.reorderProperty(propName: string; source, destination: integer);
begin
  raise EFHIRException.create('The property "'+propName+'" is unknown or not a list reordering the property"');
end;

procedure TFHIRObject.replaceProperty(propName: string; existing, new: TFHIRObject);
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

function TFHIRObject.toString: String;
begin
  if isPrimitive then
    result := fhirType+'[''' + primitiveValue + ''']'
  else
    result := fhirType;
end;

function isPrimitiveType(name : String) : boolean;
begin
  result := StringArrayExistsSensitive(['integer', 'unsignedInt', 'positiveInt', 'decimal', 'dateTime', 'date',
    'time', 'instant', 'string', 'uri', 'oid', 'uuid', 'id', 'boolean', 'code', 'markdown', 'xhtml', 'base64Binary', 'canonical', 'url'], name);
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

procedure TFHIRSelection.setProperty(propName: string; propValue: TFHIRObject);
begin
  value.setProperty(propName, propValue);
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
      result.Add(s.link);
    result.Link;
  finally
    result.Free;
  end;
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
      if (not FHIR.Base.Objects.compareDeep(e1[i].value as TFHIRObject, e2[i].value as TFHIRObject, allowNull)) then
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


{ EFHIRUnsupportedVersion }

constructor EFHIRUnsupportedVersion.Create(version: TFHIRVersion; action: String);
begin
  inherited Create('Version '+FHIR_VERSIONS[version]+' not supported '+action);
end;

End.



