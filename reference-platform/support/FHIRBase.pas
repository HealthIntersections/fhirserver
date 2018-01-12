unit FHIRBase;

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


{!ignore TFHIRObject}
{!ignore TFHIRObjectList}
{!ignore TFHIRAttribute}
{!Wrapper uses Classes,MSSEWrap}

Interface

Uses
  SysUtils, Classes, Generics.Collections, {$IFNDEF VER260} System.NetEncoding, {$ENDIF}
  AdvNames, AdvExceptions, AdvObjects, AdvObjectLists, AdvBuffers, AdvGenerics, AdvStringLists,
   DateSupport, EncodeSupport, EncdDecd, DecimalSupport, ParserSupport;

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

Const
  {$IFDEF FHIR2}
  COMPILED_FHIR_VERSION = fhirVersionRelease2;
  {$ENDIF}
  {$IFDEF FHIR3}
  COMPILED_FHIR_VERSION = fhirVersionRelease3;
  {$ENDIF}
  {$IFDEF FHIR4}
  COMPILED_FHIR_VERSION = fhirVersionRelease4;
  {$ENDIF}

  CODES_TFHIRVersion : Array [TFHIRVersion] of String = ('', 'r1', 'r2', 'r3', 'r4');
  CURRENT_FHIR_VERSION = {$IFDEF FHIR1} fhirVersionRelease1 {$ENDIF} {$IFDEF FHIR2} fhirVersionRelease2 {$ENDIF}{$IFDEF FHIR3} fhirVersionRelease3 {$ENDIF} {$IFDEF FHIR4}  fhirVersionRelease4{$ENDIF} ;

Type
  {@Enum TFHIRCommandType
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

  {@Enum TFHIRFormat
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

  {@Enum TFHIRHtmlNodeType
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


//  TFhirTag = class (TAdvName)
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

Const
  FHIR_NS = 'http://hl7.org/fhir';
  CODES_TFHIRCommandType : array [TFHIRCommandType] of String = (
    'Unknown', 'Read', 'VersionRead', 'Update', 'Delete', 'HistoryInstance', 'Create', 'Search', 'HistoryType', 'Validate', 'ConformanceStmt', 'Transaction', 'HistorySystem', 'Upload', 'Operation', 'Patch', 'Batch', 'WebUI', 'Task', 'delete task', 'Null');
  CODES_TFHIRHtmlNodeType : array [TFHIRHtmlNodeType] of String = ('Element', 'Text', 'Comment', 'Document');
  CODES_TFHIRFormat : Array [TFHIRFormat] of String = ('Unspecified', 'XML', 'JSON', 'RDF/Turtle', 'Text Representation', 'Newline delimited JSON', 'XHTML');
  EXT_ACTUAL_TFHIRFormat : Array [TFHIRFormat] of String = ('.bin', '.xml', '.json', '.ttl', '.txt', '.ndjson', '.html');
  EXT_WEB_TFHIRFormat : Array [TFHIRFormat] of String = ('.bin', '.xml', '.json', '.ttl', '.txt', '.ndjson', '.xml');
  MIMETYPES_TFHIRFormat : Array [TFHIRFormat] of String = ('', 'application/fhir+xml', 'application/fhir+json', 'text/turtle; x-dialect=fhir', 'text/fhir', 'application/x-ndjson', 'text/xhtml');
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

  TFHIRProperty = class (TAdvObject)
  Private
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
    Constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oList : TAdvList<TFHIRObject>); Overload;
    Constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; sValue : String); Overload;
    Constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; Value : TBytes); Overload;
    Constructor CreateEnum(oOwner : TFHIRObject; Const sName : String; bList: boolean; enumName : String; sValue : String); Overload;
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


  TFHIRPropertyListEnumerator = class (TAdvObject)
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

  TFHIRPropertyList = class (TAdvObjectList)
  private
    Function GetProperty(iIndex : Integer) : TFHIRProperty;
    Function GetPropertyByName(name : String) : TFHIRProperty;
  public
    function Link : TFHIRPropertyList; overload;
    function GetEnumerator : TFHIRPropertyListEnumerator;
    Property Properties[iIndex : Integer] : TFHIRProperty read GetProperty; default;
    Property ByName[name : String] : TFHIRProperty read GetPropertyByName;
  End;


  TFHIRPropertyIterator = class (TAdvObject)
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
  TFHIRObject = class (TAdvObject)
  private
    FTags : TDictionary<String,String>;
    FTag : TAdvObject;
    FTagObject : TObject;
    FLocationStart : TSourceLocation;
    FLocationEnd : TSourceLocation;
    FCommentsStart: TAdvStringList;
    FCommentsEnd: TAdvStringList;
    FFormat : TFHIRFormat;
    FNoCompose: boolean;
    FTagInt: integer;
    FJsHandle: pointer;
    FJsInstance: cardinal;
    function GetCommentsStart: TAdvStringList;
    function GetCommentsEnd: TAdvStringList;
    procedure SetTag(const Value: TAdvObject);
    procedure SetTags(name: String; const Value: String);
    function getTags(name: String): String;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); virtual;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Virtual;

    procedure deletePropertyValue(name : String; list : TFHIRObjectList; value : TFHIRObject);
    procedure replacePropertyValue(name : String; list : TFHIRObjectList; existing, new : TFHIRObject);
  public
    Constructor Create; override;
    Destructor Destroy; override;
    function Link : TFHIRObject;
    function Clone : TFHIRObject; Overload;
    procedure Assign(oSource : TAdvObject); override;

    procedure ListChildrenByName(name : string; list : TFHIRSelectionList);

    // property access API
    // getting access to the properties
    function createPropertyList(bPrimitiveValues : boolean) : TFHIRPropertyList;
    function createIterator(bInheritedProperties, bPrimitiveValues : Boolean) : TFHIRPropertyIterator;

    // create a class that is the correct type for the named property
    function createPropertyValue(propName : string): TFHIRObject; virtual;
    function getPropertyValue(propName : string): TFHIRProperty; virtual;

    // set the value of the property. For properties with cardinality > 1, append to the list, or use insertProperty
    procedure setProperty(propName : string; propValue : TFHIRObject); virtual;
    procedure insertProperty(propName : string; propValue : TFHIRObject; index : integer); virtual;

    // delete the value. propValue is used where cardinality > 1, to pick the correct item to delete
    procedure deleteProperty(propName : string; propValue : TFHIRObject); virtual;

    // replace the value of the property with a new value
    procedure replaceProperty(propName : string; existing : TFHIRObject; new : TFHIRObject); virtual;
    procedure reorderProperty(propName : string; source, destination : integer); virtual;

    // tags...
    Property Tags[name : String] : String read getTags write SetTags;
    function HasTag(name : String): boolean;
    property Tag : TAdvObject read FTag write SetTag;
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
    function fhirType : String; virtual;
    function getId : String; virtual;
    function isPrimitive : boolean; virtual;
    function hasPrimitiveValue : boolean; virtual;
    function primitiveValue : string; virtual;
    function isMetaDataBased : boolean; virtual;
    Function PerformQuery(path : String) : TFHIRObjectList;
    function hasType(t : String) : boolean; overload;
    function hasType(tl : Array of String) : boolean; overload;
    function describe : String; virtual;
    procedure getProperty(name : String; checkValid : boolean; list : TAdvList<TFHIRObject>); virtual;
    function ToString : String; override;
    function isEmpty : boolean; virtual;
    procedure dropEmpty;
    function equalsDeep(other : TFHIRObject) : boolean; virtual;
    function equalsShallow(other : TFHIRObject) : boolean; virtual;
  public
    {@member comments
      comments from the XML stream. No support for comments in JSON
    }
    Property xml_commentsStart : TAdvStringList read GetCommentsStart;
    Property xml_commentsEnd : TAdvStringList read GetCommentsEnd;

    Property _source_format : TFHIRFormat read FFormat write FFormat;

    property noCompose : boolean read FNoCompose write FNoCompose; // used by various filtering techniques to ensure that an element is not rendered
  end;

  TFHIRObjectClass = class of TFHIRObject;

  TFHIRObjectListEnumerator = class (TAdvObject)
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

  TFHIRObjectList = class (TAdvObjectList)
  private
    FTags : TDictionary<String,String>;
    FJsHandle: pointer;
    FJsInstance: cardinal;
    Function GetItemN(index : Integer) : TFHIRObject;
    procedure SetTags(name: String; const Value: String);
    function getTags(name: String): String;
  protected
    function ItemClass : TAdvObjectClass; override;
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
    property value : string read FValue write FValue;
    function isEmpty : boolean; override;
    function getId : String; override;
    function equalsDeep(other : TFHIRObject) : boolean; override;
    function equalsShallow(other : TFHIRObject) : boolean; override;
  end;

  TFHIRAttributeList = class;
  TFHIRAttribute = class (TFHIRObject)
  private
    FName : String;
    FValue : String;
  protected
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
  public
    Constructor Create(Name : String; Value : String); Overload;

    function Link : TFHIRAttribute; Overload;
    function Clone : TFHIRAttribute; Overload;
    procedure Assign(oSource : TAdvObject); override;
    property Name : String read FName write FName;
    property Value : String read FValue write FValue;
    function isEmpty : boolean; override;
    function getId : String; override;
    function equalsDeep(other : TFHIRObject) : boolean; override;
    function equalsShallow(other : TFHIRObject) : boolean; override;
  end;

  TFHIRAttributeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFHIRAttributeList;
    function GetCurrent : TFHIRAttribute;
  public
    Constructor Create(list : TFHIRAttributeList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFHIRAttribute read GetCurrent;
  end;

  TFHIRAttributeList = class (TFHIRObjectList)
  private
    Function GetItemN(index : Integer) : TFHIRAttribute;
  public
    function Link : TFHIRAttributeList; Overload;
    Function IndexOf(value : TFHIRAttribute) : Integer;
    Function Item(index : Integer) : TFHIRAttribute;
    Function Count : Integer; Overload;
    Property Segments[index : Integer] : TFHIRAttribute read GetItemN; default;
    Function Get(name : String):String;
    function GetEnumerator : TFHIRAttributeListEnumerator;
    Procedure SetValue(name : String; value :String);
    Procedure Add(name : String; value :String); overload;
  End;


  TFHIRObjectFactory = class (TAdvObject)
  private
  public
  end;

  TFHIRSelection = class (TAdvObject)
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
  end;

  TFHIRSelectionList = class (TAdvList<TFHIRSelection>)
  public
    Constructor Create; overload; override;
    Constructor Create(focus : TFHIRObject); overload;
    Constructor Create(parent : TFHIRObject; name : String; focus : TFHIRObject); overload;
    function Link : TFHIRSelectionList; overload;
    procedure add(value : TFHIRObject); overload;
    procedure add(parent : TFHIRObject; name : String; value : TFHIRObject); overload;
    procedure addAll(value : TFHIRObjectList); overload;
    procedure addAll(parent : TFHIRObject; name : String; value : TFHIRObjectList); overload;

    function asValues : TFHIRObjectList;

    class function compareDeep(e1, e2 : TFHIRSelectionList; allowNull : boolean) : boolean;
  end;

  TFhirXHtmlNodeList = class;

  {@Class TFhirXHtmlNode
    An xhtml node. Has a type - is either an element, with a name and children,
    or a different type of node with text (usually text or comment)
  }
  {!.Net HL7Connect.Fhir.XhtmlNode}
  TFhirXHtmlNode = class (TFHIRObject)
  private
    FNodeType : TFHIRHtmlNodeType;
    FName : String;
    FAttributes : TFHIRAttributeList;
    FChildNodes : TFhirXHtmlNodeList;
    FContent : String;
    procedure SetNodeType(const Value: TFHIRHtmlNodeType);
    function GetChildNodes: TFhirXHtmlNodeList;
    function GetAttributes: TFHIRAttributeList;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); Override;
  public
    Constructor Create; Override;
    Constructor Create(nodeType : TFHIRHtmlNodeType) ; Overload;
    Constructor Create(name : String) ; Overload;
    Destructor Destroy; Override;
    {!script hide}
    function Link : TFhirXHtmlNode; Overload;
    function Clone : TFhirXHtmlNode; Overload;
    procedure Assign(oSource : TAdvObject); override;
    property Attributes : TFHIRAttributeList read GetAttributes;
    function allChildrenAreText : boolean;
    function isPrimitive : boolean; override;
    function primitiveValue : string; override;
    function fhirType : String; override;
    function NsDecl : String; virtual;
    {!script show}

    {@member AsPlainText
      plain text content of html
    }
    function AsPlainText : String;
    function isEmpty : boolean; override;
    function getId : String; override;

    function equalsDeep(other : TFHIRObject) : boolean; override;
    function equalsShallow(other : TFHIRObject) : boolean; override;
  published
    {@member NodeType
      The type of the node - fhntElement, fhntText, fhntComment, fhntDocument

      Note that documents are not encountered in FHIR resources
    }
    property NodeType : TFHIRHtmlNodeType read FNodeType write SetNodeType;

    {@member Name
      The name of the element, if the node is an element

      Note that namespaces are not supported in FHIR xhtml
    }
    property Name : String read FName write FName;

    {@member Content
      The content of the element if it is a text or comment node
    }
    property Content : String read FContent write FContent;

    {@member ChildNodes
      The children of the node, if it is an element
    }
    property ChildNodes : TFhirXHtmlNodeList read GetChildNodes;

    {@member AddText
      Add a text node to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddText(content : String) : TFhirXHtmlNode;

    {@member AddComment
      Add a comment node to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddComment(content : String) : TFhirXHtmlNode;

    {@member AddChild
      Add a child element to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddChild(name : String) : TFhirXHtmlNode;

    {@member AddTag
      Add a child element to the end of the list of nodes.

      If you want more control over the node children use @ChildNodes
    }
    function AddTag(name : String) : TFhirXHtmlNode;

    {@member GetAttribute
      Get an attribute by it's name

      Note that namespaces are not supported in FHIR xhtml
    }
    Function GetAttribute(name : String) : String;

    {@member SetAttribute
      Set the value of an attribute. Create it if it doesn't exist

      Note that namespaces are not supported in FHIR xhtml
    }
    function SetAttribute(name, value : String) : TFhirXHtmlNode;
  end;

  TFHIRXhtmlNodeListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFHIRXhtmlNodeList;
    function GetCurrent : TFHIRXhtmlNode;
  public
    Constructor Create(list : TFHIRXhtmlNodeList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFHIRXhtmlNode read GetCurrent;
  end;

  {@Class TFHIRXHtmlNodeList
    A list of Xhtml Nodes
  }
  {!.Net HL7Connect.Fhir.XHtmlNodeList}
  TFHIRXHtmlNodeList = class (TFHIRObjectList)
  private
    Function GetItemN(index : Integer) : TFHIRXHtmlNode;
    Procedure SetItemN(index : Integer; value : TFHIRXHtmlNode);
  public
    {!script hide}
    Function Link : TFHIRXHtmlNodeList; Overload;
    Function Clone : TFHIRXHtmlNodeList; Overload;
    function GetEnumerator : TFHIRXhtmlNodeListEnumerator;
    {!script show}
    {@member Append
      Add an Xhtml Node to the end of the list.
    }
    Function Append : TFHIRXHtmlNode;
    {@member AddItem
      Add an already existing Xhtml Node to the end of the list.
    }
    Procedure AddItem(value : TFHIRXHtmlNode);
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TFHIRXHtmlNode) : Integer;
    {@member Insert
       Insert an Xhtml node before the designated index (0 = first item)
    }
    Function Insert(index : Integer) : TFHIRXHtmlNode;
    {@member InsertItem
       Insert an existing Xhtml Node before the designated index (0 = first item)
    }
    Procedure InsertItem(index : Integer; value : TFHIRXHtmlNode);
    {@member Item
       Get the indexth Xhtml Node. (0 = first item)
    }
    Function Item(index : Integer) : TFHIRXHtmlNode;
    {@member SetItemByIndex
       Set the indexth Xhtml Node. (0 = first item)
    }
    Procedure SetItemByIndex(index : Integer; value : TFHIRXHtmlNode);
    {@member Count
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {@member remove
      Remove the indexth item. The first item is index 0.
    }
    Procedure Remove(index : Integer);
    {@member ClearItems
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Nodes[index : Integer] : TFHIRXHtmlNode read GetItemN write SetItemN; default;
  End;

  TFHIRPathOperation = (
       popNull, popEquals, popEquivalent, popNotEquals, popNotEquivalent, popLessThan, popGreater, popLessOrEqual, popGreaterOrEqual, popIs, popAs,
       popUnion, popOr, popAnd, popXor, popImplies, popTimes, popDivideBy, popPlus, popConcatenate, popMinus, popDiv, popMod, popIn, popContains, popCustom);
  TFHIRPathOperationSet = set of TFHIRPathOperation;

  TFHIRPathFunction = (
    pfNull, pfEmpty, pfNot, pfExists, pfSubsetOf, pfSupersetOf, pfIsDistinct, pfDistinct, pfCount, pfWhere, pfSelect, pfAll,
    pfRepeat, pfItem, pfAs, pfIs, pfSingle, pfFirst, pfLast, pfTail, pfSkip, pfTake, pfIif, pfToInteger, pfToDecimal, pfToString,
    pfSubstring, pfStartsWith, pfEndsWith, pfMatches, pfReplaceMatches, pfContains, pfReplace, pfLength, pfChildren, pfDescendants,
    pfMemberOf, pfTrace, pfToday, pfNow, pfResolve, pfExtension, pfAllFalse, pfAnyFalse, pfCombine, pfType, pfOfType,
    pfElementDefinition, pfSlice, pfCheckModifiers, pfConformsTo, pfHasValue, pfHtmlChecks, pfCustom);

  TFHIRPathExpressionNodeKind = (enkName, enkFunction, enkConstant, enkGroup, enkStructure); // structure is not used in FHIRPath, but is in CQL
  TFHIRCollectionStatus = (csNULL, csSINGLETON, csORDERED, csUNORDERED);

const
  CODES_TFHIRPathOperation : array [TFHIRPathOperation] of String = (
    '', '=' , '~' , '!=' , '!~' , '<' , '>' , '<=' , '>=' , 'is', 'as', '|', 'or' , 'and' , 'xor', 'implies',
     '*', '/', '+' , '&', '-', 'div', 'mod', 'in', 'contains', 'xx-custom-xx');

  CODES_TFHIRPathFunctions : array [TFHIRPathFunction] of String = (
    '', 'empty', 'not', 'exists', 'subsetOf', 'supersetOf', 'isDistinct', 'distinct', 'count', 'where', 'select', 'all',
    'repeat', '[]', 'as', 'is', 'single', 'first', 'last', 'tail', 'skip', 'take', 'iif', 'toInteger', 'toDecimal', 'toString',
    'substring', 'startsWith', 'endsWith', 'matches', 'replaceMatches', 'contains', 'replace', 'length', 'children', 'descendants',
    'memberOf', 'trace', 'today', 'now', 'resolve', 'extension', 'allFalse', 'anyFalse', 'combine', 'type', 'ofType',
    'elementDefinition', 'slice', 'checkModifiers', 'conformsTo', 'hasValue', 'htmlchecks', 'xx-custom-xx');



type
  TFHIRTypeDetails = class (TAdvObject)
  private
    id : integer;
    FTypes : TStringList;
    FCollectionStatus : TFHIRCollectionStatus;
  public
    constructor create(status : TFHIRCollectionStatus; types : array of String);
    constructor createList(status : TFHIRCollectionStatus; types : TStringList);
    destructor Destroy; override;
    function Link : TFHIRTypeDetails; overload;
    procedure addType(n : String);
    procedure addTypes(n : TStringList); overload;
    procedure addTypes(types : array of String); overload;
    function hasType(types : array of String) : boolean; overload;
    function hasType(types : TStringList) : boolean; overload;
    function hasType(typeName : String) : boolean; overload;
    procedure update(source : TFHIRTypeDetails);
    function union(right : TFHIRTypeDetails) : TFHIRTypeDetails;
    function hasNoTypes() : boolean;
    function toSingleton : TFHIRTypeDetails;
    property types : TStringList read FTypes;
    property CollectionStatus : TFHIRCollectionStatus read FCollectionStatus;
    function describe : String;
    function type_ : String;
  end;

  TFHIRPathExpressionNode = class (TAdvObject)
  private
    FTag : integer;
    FName: String;
    FConstant : string;
    FFunctionId : TFHIRPathFunction;
    FParameters : TAdvList<TFHIRPathExpressionNode>;
    FInner: TFHIRPathExpressionNode;
    FGroup: TFHIRPathExpressionNode;
    FOperation : TFHIRPathOperation;
    FProximal : boolean;
    FOpNext: TFHIRPathExpressionNode;
    FTypes : TFHIRTypeDetails;
    FOpTypes : TFHIRTypeDetails;
    FKind: TFHIRPathExpressionNodeKind;
    FUniqueId : integer;
    FSourceLocationStart : TSourceLocation;
    FSourceLocationEnd : TSourceLocation;
    FOpSourceLocationStart : TSourceLocation;
    FOpSourceLocationEnd : TSourceLocation;

    procedure SetOpNext(const Value: TFHIRPathExpressionNode);
    procedure SetInner(const Value: TFHIRPathExpressionNode);
    procedure SetGroup(const Value: TFHIRPathExpressionNode);
    procedure SetFunctionId(const Value: TFHIRPathFunction);
    procedure SetTypes(const Value: TFHIRTypeDetails);
    procedure SetOpTypes(const Value: TFHIRTypeDetails);
    procedure write(b : TStringBuilder);
  public
    Constructor Create(uniqueId : Integer);
    Destructor Destroy; override;

    function ToString : String; override;
    function Link : TFHIRPathExpressionNode; overload;
    function checkName : boolean;

    property uniqueId : integer read FUniqueId;
    property SourceLocationStart : TSourceLocation read FSourceLocationStart write FSourceLocationStart;
    property SourceLocationEnd : TSourceLocation read FSourceLocationEnd write FSourceLocationEnd;
    property OpSourceLocationStart : TSourceLocation read FOpSourceLocationStart write FOpSourceLocationStart;
    property OpSourceLocationEnd : TSourceLocation read FOPSourceLocationEnd write FOpSourceLocationEnd;

    function summary : String;
    function ParameterCount : integer;
    function Canonical : String;
    function check(out msg : String; refCount : integer): boolean;
    function location : String;
    function opLocation : String;

    property tag : integer read FTag write FTag;
    property kind : TFHIRPathExpressionNodeKind read FKind write FKind;
    property name : String read FName write FName;
    property constant : String read FConstant write FConstant;
    property FunctionId : TFHIRPathFunction read FFunctionId write SetFunctionId;
    property Parameters : TAdvList<TFHIRPathExpressionNode> read FParameters;
    property Inner : TFHIRPathExpressionNode read FInner write SetInner;
    property Group : TFHIRPathExpressionNode read FGroup write SetGroup;
    property Operation : TFHIRPathOperation read FOperation write FOperation;
    property Proximal : boolean read FProximal write FProximal;
    property OpNext : TFHIRPathExpressionNode read FOpNext write SetOpNext;
    property Types : TFHIRTypeDetails read FTypes write SetTypes;
    property OpTypes : TFHIRTypeDetails read FOpTypes write SetOpTypes;
  end;


function noList(e : TFHIRObjectList) : boolean; overload;
function compareDeep(e1, e2 : TFHIRObjectList; allowNull : boolean) : boolean; overload;
function compareDeep(e1, e2 : TFHIRObject; allowNull : boolean) : boolean; overload;
function compareDeep(div1, div2 : TFhirXHtmlNode; allowNull : boolean) : boolean; overload;
function isPrimitiveType(name : String):boolean;
function isEmptyProp(v : TFHIRObject) : boolean; overload;
function isEmptyProp(v : TFHIRObjectList) : boolean; overload;

Implementation

Uses
  StringSupport,
  FHIRUtilities,
  FHIRXhtml,
  FHIRTypes,
  FHIRResources,
  FHIRPath;


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

procedure TFHIRObject.Assign(oSource: TAdvObject);
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
    FTags := TDictionary<String, String>.create;
  if FTags.ContainsKey(name) then
    result := FTags[name]
  else
    result := '';
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

function TFHIRObject.createPropertyValue(propName: string): TFHIRObject;
begin
  raise Exception.Create('The property "'+propName+' is unknown, or cannot be made directly"');
end;

procedure TFHIRObject.setProperty(propName : string; propValue: TFHIRObject);
begin
  raise Exception.Create('The property "'+propName+'" is unknown (setting value)"');
end;

procedure TFHIRObject.SetTag(const Value: TAdvObject);
begin
  FTag.Free;
  FTag := Value;
end;

procedure TFHIRObject.SetTags(name: String; const Value: String);
begin
  if FTags = nil then
    FTags := TDictionary<String,String>.create;
  FTags.AddOrSetValue(name, value);
end;

procedure TFHIRObject.deleteProperty(propName: string; propValue: TFHIRObject);
begin
  raise Exception.Create('The property "'+propName+'" is unknown deleting the property"');
end;

procedure TFHIRObject.deletePropertyValue(name : String; list: TFHIRObjectList; value: TFHIRObject);
var
  i : integer;
begin
  i := list.IndexByReference(value);
  if (i = -1) then
    raise Exception.Create('Unable to find object in '+name+' to remove it');
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

function TFHIRObject.fhirType: String;
begin
  raise Exception.Create('"fhirType" is not overridden in '+className);
end;

function TFHIRObject.GetCommentsStart: TAdvStringList;
begin
  if FCommentsStart = nil then
    FCommentsStart := TAdvStringList.Create;
  result := FCommentsStart;
end;

function TFHIRObject.getId: String;
begin
  raise Exception.Create('"getId" is not overridden in '+className);
end;

procedure TFHIRObject.getProperty(name: String; checkValid: boolean; list: TAdvList<TFHIRObject>);
begin
  if checkValid then
    raise Exception.Create('Property '+name+' is not valid');
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
  raise Exception.Create('The property "'+propName+'" is unknown or not a list property (inserting value)"');
end;

function TFHIRObject.isEmpty: boolean;
begin
  result := true;
end;

function TFHIRObject.isMetaDataBased: boolean;
begin
  result := false;
end;

function TFHIRObject.isPrimitive: boolean;
begin
  result := false;
end;

function TFHIRObject.GetCommentsEnd: TAdvStringList;
begin
  if FCommentsEnd = nil then
    FCommentsEnd := TAdvStringList.Create;
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

{ TFHIRXHtmlNodeList }

procedure TFHIRXHtmlNodeList.AddItem(value: TFHIRXHtmlNode);
begin
  add(value.Link);
end;

function TFHIRXHtmlNodeList.Append: TFHIRXHtmlNode;
begin
  result := TFhirXHtmlNode.create;
  try
    add(result.Link);
  finally
    result.free;
  end;
end;

procedure TFHIRXHtmlNodeList.ClearItems;
begin
  Clear;
end;

function TFHIRXHtmlNodeList.Clone: TFHIRXHtmlNodeList;
begin
  result := TFHIRXHtmlNodeList(inherited Clone);
end;

function TFHIRXHtmlNodeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFHIRXHtmlNodeList.GetEnumerator: TFHIRXhtmlNodeListEnumerator;
begin
  result := TFHIRXhtmlNodeListEnumerator.Create(self.Link);
end;

function TFHIRXHtmlNodeList.GetItemN(index: Integer): TFHIRXHtmlNode;
begin
  result := TFHIRXHtmlNode(ObjectByIndex[index]);
end;

function TFHIRXHtmlNodeList.IndexOf(value: TFHIRXHtmlNode): Integer;
begin
  result := IndexByReference(value);
end;

function TFHIRXHtmlNodeList.Insert(index: Integer): TFHIRXHtmlNode;
begin
  result := TFhirXHtmlNode.create;
  try
    inherited insert(index, result.Link);
  finally
    result.free;
  end;
end;

procedure TFHIRXHtmlNodeList.InsertItem(index: Integer; value: TFHIRXHtmlNode);
begin
  Inherited Insert(index, value.Link);
end;

function TFHIRXHtmlNodeList.Item(index: Integer): TFHIRXHtmlNode;
begin
  result := TFHIRXHtmlNode(ObjectByIndex[index]);
end;

function TFHIRXHtmlNodeList.Link: TFHIRXHtmlNodeList;
begin
  result := TFHIRXHtmlNodeList(inherited Link);
end;

procedure TFHIRXHtmlNodeList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFHIRXHtmlNodeList.SetItemByIndex(index: Integer; value: TFHIRXHtmlNode);
begin
  Nodes[index] := value.Link;
end;

procedure TFHIRXHtmlNodeList.SetItemN(index: Integer; value: TFHIRXHtmlNode);
begin
  ObjectByIndex[index] := value;
end;

{ TFHIRAttributeList }
procedure TFHIRAttributeList.Add(name, value: String);
begin
  SetValue(name, value);
end;

function TFHIRAttributeList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFHIRAttributeList.Get(name: String): String;
var
  i : integer;
begin
  result := '';
  for i := 0 to Count - 1 do
    if GetItemN(i).Name = name then
      result := GetItemN(i).Value;
end;

function TFHIRAttributeList.GetEnumerator: TFHIRAttributeListEnumerator;
begin
  result := TFHIRAttributeListEnumerator.Create(self.Link);
end;

function TFHIRAttributeList.GetItemN(index: Integer): TFHIRAttribute;
begin
  result := TFHIRAttribute(ObjectByIndex[index]);
end;

function TFHIRAttributeList.IndexOf(value: TFHIRAttribute): Integer;
begin
  result := IndexByReference(value);
end;

function TFHIRAttributeList.Item(index: Integer): TFHIRAttribute;
begin
  result := TFHIRAttribute(ObjectByIndex[index]);
end;

function TFHIRAttributeList.Link: TFHIRAttributeList;
begin
  result := TFHIRAttributeList(inherited Link);
end;

procedure TFHIRAttributeList.SetValue(name, value: String);
var
  i : integer;
  b : boolean;
  attr : TFHIRAttribute;
begin
  b := false;
  for i := 0 to Count - 1 do
    if GetItemN(i).Name = name then
    begin
      b := true;
      GetItemN(i).Value := value;
    end;
  if not b then
  begin
    attr := TFHIRAttribute.create;
    try
      attr.name := name;
      attr.value := value;
      add(attr.link);
    finally
      attr.free;
    end;
  end;
end;

{ TFHIRAttribute }

procedure TFHIRAttribute.Assign(oSource: TAdvObject);
begin
  inherited;
  FName := TFHIRAttribute(oSource).FName;
  FValue := TFHIRAttribute(oSource).FValue;
end;

function TFHIRAttribute.Clone: TFHIRAttribute;
begin
  result := TFHIRAttribute(inherited Clone);
end;

constructor TFHIRAttribute.Create(Name, Value: String);
begin
  Create;
  FName := Name;
  FValue := Value;
end;

function TFHIRAttribute.equalsDeep(other: TFHIRObject): boolean;
begin
  result := inherited equalsDeep(other) and (FName = TFHIRAttribute(other).FName) and (FValue = TFHIRAttribute(other).FValue);
end;

function TFHIRAttribute.equalsShallow(other: TFHIRObject): boolean;
begin
  result := equalsDeep(other);
end;

function TFHIRAttribute.getId: String;
begin
  result := '';
end;

function TFHIRAttribute.isEmpty: boolean;
begin
  result := inherited isEmpty and (FName = '') and (FValue = '');
end;

function TFHIRAttribute.Link: TFHIRAttribute;
begin
  result := TFHIRAttribute(inherited Link);
end;

procedure TFHIRAttribute.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  if (bInheritedProperties) Then
    inherited;
  oList.add(TFHIRProperty.create(self, 'name', 'string', false, nil, FName));
  oList.add(TFHIRProperty.create(self, 'value', 'string', false, nil, FValue));
end;

{ TFhirXHtmlNode }

function TFhirXHtmlNode.AddChild(name: String): TFhirXHtmlNode;
var
  node : TFhirXHtmlNode;
begin
  node := TFhirXHtmlNode.create;
  try
    node.NodeType := fhntElement;
    node.FName := name;
    ChildNodes.add(node.Link);
    result := node;
  finally
    node.free;
  end;
end;

function TFhirXHtmlNode.AddComment(content: String): TFhirXHtmlNode;
var
  node : TFhirXHtmlNode;
begin
  node := TFhirXHtmlNode.create;
  try
    node.NodeType := fhntComment;
    node.FContent := content;
    ChildNodes.add(node.Link);
    result := node;
  finally
    node.free;
  end;
end;

function TFhirXHtmlNode.AddTag(name: String): TFhirXHtmlNode;
begin
  result := AddChild(name);
end;

function TFhirXHtmlNode.AddText(content : String): TFhirXHtmlNode;
var
  node : TFhirXHtmlNode;
begin
  if content = '' then
    result := nil
  else
  begin
    node := TFhirXHtmlNode.create;
    try
      node.NodeType := fhntText;
      node.FContent := content;
      ChildNodes.add(node.Link);
      result := node;
    finally
      node.free;
    end;
  end;
end;

function TFhirXHtmlNode.allChildrenAreText: boolean;
var
  i : integer;
begin
  result := FChildNodes.Count > 0;
  for i := 0 to FChildNodes.Count - 1 do
    result := result and (FChildNodes[i].FNodeType = fhntText);
end;

function TFhirXHtmlNode.AsPlainText: String;
var
  s : String;
  i : integer;
begin
  case NodeType of
    fhntText : result := Content;
    fhntComment : result := '';
  else // fhntElement, fhntDocumenk
    s := '';
    for i := 0 to ChildNodes.count - 1 do
      s := s + ChildNodes[i].AsPlainText;
    if (Name = 'p') or (Name = 'h2') or (Name = 'h3') or (Name = 'h4') or (Name = 'h5') or (Name = 'h6') or (name='div') then
      result := s + #13#10
    else if Name = 'li' then
      result := '* '+ s +#13#10
    else
      result := s;
  end;
end;

procedure TFhirXHtmlNode.Assign(oSource: TAdvObject);
begin
  inherited;
  NodeType := TFhirXHtmlNode(oSource).FNodeType;
  FName := TFhirXHtmlNode(oSource).FName;
  FContent := TFhirXHtmlNode(oSource).FContent;
  if TFhirXHtmlNode(oSource).Attributes <> nil Then
    Attributes.assign(TFhirXHtmlNode(oSource).Attributes);
  if TFhirXHtmlNode(oSource).FChildNodes <> nil then
    ChildNodes.assign(TFhirXHtmlNode(oSource).FChildNodes);
end;

function TFhirXHtmlNode.Clone: TFhirXHtmlNode;
begin
  result := TFhirXHtmlNode(inherited Clone);
end;

constructor TFhirXHtmlNode.Create(nodeType: TFHIRHtmlNodeType);
begin
  Create;
  NodeType := fhntElement;
end;

constructor TFhirXHtmlNode.Create(name: String);
begin
  Create;
  NodeType := fhntElement;
  FName := name;
end;

constructor TFhirXHtmlNode.Create;
begin
  inherited;
end;

destructor TFhirXHtmlNode.Destroy;
begin
   FChildNodes.Free;
  FAttributes.Free;
  inherited;
end;

function TFhirXHtmlNode.equalsDeep(other: TFHIRObject): boolean;
var
  o : TFhirXHtmlNode;
  i : integer;
begin
  result := inherited equalsDeep(other);
  if result then
  begin
    o := TFhirXHtmlNode(other);
    if FNodeType <> o.FNodeType then
      exit(false);
    if FName <> o.FName then
      exit(false);

    if (FAttributes <> nil) <> (o.FAttributes <> nil) then
      exit(false);
    if (FAttributes <> nil) then
    begin
      if Attributes.Count <> o.FAttributes.Count then
        exit(false);
      for i := 0 to FAttributes.Count - 1 do
        if FAttributes[i].Value <> o.FAttributes.Get(FAttributes[i].FName) then
          exit(false);
    end;
    if (FChildNodes <> nil) <> (o.FChildNodes <> nil) then
      exit(false);
    if (FChildNodes <> nil) then
    begin
      if FChildNodes.Count <> o.FChildNodes.Count then
        exit(false);
      for i := 0 to FChildNodes.Count - 1 do
        if not FChildNodes[i].equalsDeep(o.FChildNodes[i]) then
          exit(false);
    end;
    if FContent <> o.FContent then
      exit(false);
  end;
end;

function TFhirXHtmlNode.equalsShallow(other: TFHIRObject): boolean;
begin
  result := equalsDeep(other);
end;

function TFhirXHtmlNode.FhirType: String;
begin
  result := 'xhtml';
end;

function TFhirXHtmlNode.GetAttribute(name: String): String;
var
  i : integer;
begin
  result := '';
  for i := 0 to FAttributes.Count - 1 Do
    if FAttributes[i].Name = name then
    begin
      result := FAttributes[i].Value;
      exit;
    end;
end;

function TFhirXHtmlNode.GetAttributes: TFHIRAttributeList;
begin
  if FAttributes = nil then
    FAttributes := TFHIRAttributeList.create;
  result := FAttributes;
end;

function TFhirXHtmlNode.GetChildNodes: TFhirXHtmlNodeList;
begin
  if FChildNodes = nil then
    FChildNodes := TFhirXHtmlNodeList.create;
  result := FChildNodes;
end;

procedure TFhirXHtmlNode.GetChildrenByName(name: string; list: TFHIRSelectionList);
var
  i : integer;
begin
  inherited;
  if FAttributes <> Nil then
    for i := 0 to FAttributes.Count - 1 do
      if name = '@'+FAttributes[i].FName then
        list.add(FAttributes[i].Link);
  if FChildNodes <> Nil then
    for i := 0 to FChildNodes.Count - 1 do
      if name = FChildNodes[i].FName then
        list.add(FChildNodes[i].Link);
  if name = 'text()' then
    list.add(TFHIRObjectText.create(FContent));
end;

function TFhirXHtmlNode.getId: String;
begin
  result := '';
end;

function TFhirXHtmlNode.isEmpty: boolean;
begin
  result := inherited isEmpty and (FName = '') and FAttributes.IsEmpty and FChildNodes.IsEmpty and (FContent = '');
end;

function TFhirXHtmlNode.isPrimitive: boolean;
begin
  result := true;
end;

function TFhirXHtmlNode.Link: TFhirXHtmlNode;
begin
  result := TFhirXHtmlNode(inherited Link);
end;

procedure TFhirXHtmlNode.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  if (bInheritedProperties) Then
    inherited;
  if (bPrimitiveValues) then
  begin
    oList.add(TFHIRProperty.create(self, 'type', 'string', false, nil, CODES_TFHIRHtmlNodeType[FNodeType]));
    oList.add(TFHIRProperty.create(self, 'name', 'string', false, nil, FName));
    oList.add(TFHIRProperty.create(self, 'attribute', 'Attribute', true, nil, FAttributes.Link));
    oList.add(TFHIRProperty.create(self, 'childNode', 'Node', true, nil, FChildNodes.Link));
    oList.add(TFHIRProperty.create(self, 'content', 'string', false, nil, FContent));
  end;
end;

function TFhirXHtmlNode.NsDecl: String;
var
  attr : TFHIRAttribute;
begin
  result := '';
  if FAttributes <> nil then
   	for attr in Attributes do
 	  	if attr.Name = 'xmlns' then
 		  	exit(attr.value);
end;

function TFhirXHtmlNode.primitiveValue: string;
begin
  result := AsPlainText;
end;

function TFhirXHtmlNode.SetAttribute(name, value: String) : TFhirXHtmlNode;
var
  i : integer;
begin
  result := self;
  for i := 0 to FAttributes.Count - 1 Do
    if FAttributes[i].Name = name then
    begin
      FAttributes[i].Value := value;
      exit;
    end;
  FAttributes.add(TFHIRAttribute.create(name, value));
end;

procedure TFhirXHtmlNode.SetNodeType(const Value: TFHIRHtmlNodeType);
begin
  FNodeType := Value;
  if FNodeType = fhntElement then
  begin
    FChildNodes := TFhirXHtmlNodeList.create;
    FAttributes := TFHIRAttributeList.create;
  end;
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

function TFHIRObjectText.equalsDeep(other: TFHIRObject): boolean;
begin
  result := inherited equalsDeep(other) and (FValue <> TFHIRObjectText(other).FValue);
end;

function TFHIRObjectText.equalsShallow(other: TFHIRObject): boolean;
begin
  result := equalsDeep(other);
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
    FTags := TDictionary<String, String>.create;
  if FTags.ContainsKey(name) then
    result := FTags[name]
  else
    result := '';
end;

function TFHIRObjectList.ItemClass: TAdvObjectClass;
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
    FTags := TDictionary<String,String>.create;
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


(*

{ TFHIRSid }

function TFHIRSid.Clone: TFHIRSid;
begin
  result := TFHIRSid(Inherited Clone);
end;

function TFHIRSid.Link: TFHIRSid;
begin
  result := TFHIRSid(Inherited Link);
end;

{ TFHIRDateTime }

function TFHIRDateTime.Clone: TFHIRDateTime;
begin
  result := TFHIRDateTime(Inherited Clone);
end;

function TFHIRDateTime.Link: TFHIRDateTime;
begin
  result := TFHIRDateTime(Inherited Link);
end;

{ TFHIRDate }

function TFHIRDate.Clone: TFHIRDate;
begin
  result := TFHIRDate(Inherited Clone);
end;

function TFHIRDate.Link: TFHIRDate;
begin
  result := TFHIRDate(Inherited Link);
end;

{ TFHIRUri }

function TFHIRUri.Clone: TFHIRUri;
begin
  result := TFHIRUri(Inherited Clone);
end;

function TFHIRUri.Link: TFHIRUri;
begin
  result := TFHIRUri(Inherited Link);
end;

{ TFHIRId }

function TFHIRId.Clone: TFHIRId;
begin
  result := TFHIRId(Inherited Clone);
end;

function TFHIRId.Link: TFHIRId;
begin
  result := TFHIRId(Inherited Link);
end;

{ TFHIROid }

function TFHIROid.Clone: TFHIROid;
begin
  result := TFHIROid(Inherited Clone);
end;

function TFHIROid.Link: TFHIROid;
begin
  result := TFHIROid(Inherited Link);
end;

{ TFHIRUuid }

function TFHIRUuid.Clone: TFHIRUuid;
begin
  result := TFHIRUuid(Inherited Clone);
end;

function TFHIRUuid.Link: TFHIRUuid;
begin
  result := TFHIRUuid(Inherited Link);
end;

{ TFHIRBuffer }

procedure TFHIRBuffer.Assign(oSource: TAdvObject);
begin
  inherited;
  FBuffer.Assign(TFHIRBuffer(oSource).FBuffer);
end;

function TFHIRBuffer.Clone: TFHIRBuffer;
begin
  result := TFHIRBuffer(Inherited Clone);
end;

constructor TFHIRBuffer.Create;
begin
  inherited;
  FBuffer := TAdvBuffer.Create;
end;

constructor TFHIRBuffer.Create(buffer: TAdvBuffer);
begin
  Create;
  FBuffer.Assign(buffer);
end;

destructor TFHIRBuffer.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

function TFHIRBuffer.GetText: String;
begin
  result := FBuffer.AsText;
end;

function TFHIRBuffer.Link: TFHIRBuffer;
begin
  result := TFHIRBuffer(Inherited Link);
end;

procedure TFHIRBuffer.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  if (bInheritedProperties) Then
    inherited;
  oList.add(TFHIRProperty.create(self, 'content', 'string', FBuffer.asBase64));
end;

procedure TFHIRBuffer.LoadFromFile(filename: String);
begin
  FBuffer.LoadFromFileName(filename);
end;

procedure TFHIRBuffer.SaveToFile(filename: String);
begin
  FBuffer.SaveToFileName(filename);
end;

procedure TFHIRBuffer.SetBuffer(const Value: TAdvBuffer);
begin
  FBuffer.Free;
  FBuffer := Value;
end;

procedure TFHIRBuffer.SetText(const Value: String);
begin
  FBuffer.AsText := value;
end;
*)


{ TFHIRProperty }

constructor TFHIRProperty.Create(oOwner: TFHIRObject; const sName, sType: String; bList : boolean; cClass : TClass; oObject: TFHIRObject);
begin
  Create;
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
  FName := sName;
  FType := sType;
  FClass := cClass;
  FIsList := bList;
  FList := TFHIRObjectList.Create;
  if (sValue <> '') then
    FList.Add(TFhirString.Create(sValue));
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
  FName := sName;
  FType := sType;
  FClass := cClass;
  FIsList := bList;
  FList := TFHIRObjectList.Create;
  if (length(value) > 0) then
    FList.Add(TFhirString.Create(String(EncodeBase64(@value[0], length(value)))));
end;

constructor TFHIRProperty.CreateEnum(oOwner: TFHIRObject; const sName: String; bList: boolean; enumName, sValue: String);
begin
  Create;
  FName := sName;
  FType := 'code';
  FClass := TFHIREnum;
  FEnumName := enumName;
  FIsList := false;
  FList := TFHIRObjectList.Create;
  if (sValue <> '') then
    FList.Add(TFhirCode.Create(sValue));
end;

constructor TFHIRProperty.Create(oOwner: TFHIRObject; const sName, sType: String; bList: boolean; cClass: TClass; oList: TAdvList<TFHIRObject>);
var
  i : integer;
begin
  Create;
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

{ TFhirAttributeListEnumerator }

Constructor TFhirAttributeListEnumerator.Create(list : TFhirAttributeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirAttributeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirAttributeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirAttributeListEnumerator.GetCurrent : TFhirAttribute;
begin
  Result := FList[FIndex];
end;


{ TFhirXhtmlNodeListEnumerator }

Constructor TFhirXhtmlNodeListEnumerator.Create(list : TFhirXhtmlNodeList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirXhtmlNodeListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirXhtmlNodeListEnumerator.MoveNext : boolean;
begin
  inc(FIndex);
  Result := FIndex < FList.count;
end;

function TFhirXhtmlNodeListEnumerator.GetCurrent : TFhirXhtmlNode;
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

function compareDeep(div1, div2 : TFhirXHtmlNode; allowNull : boolean) : boolean;
begin
  if (div1 = nil) and (div2 = nil) and (allowNull) then
    result := true
  else if (div1 = nil) or (div2 = nil) then
    result := false
  else
  begin
    result := false; //div1.equalsDeep(div2);
    raise Exception.Create('Not done yet');
  end;
end;

function TFHIRObject.PerformQuery(path: String): TFHIRObjectList;
var
  qry : TFHIRPathEngine;
  list : TFHIRSelectionList;
begin
  qry := TFHIRPathEngine.create(nil);
  try
    list := qry.evaluate(nil, self, path);
    try
      result := list.asValues;
    finally
      list.Free;
    end;
  finally
    qry.free;
  end;
end;

function TFHIRObject.primitiveValue: string;
begin
  result := '';
end;

procedure TFHIRObject.reorderProperty(propName: string; source, destination: integer);
begin
  raise Exception.Create('The property "'+propName+'" is unknown or not a list reordering the property"');
end;

procedure TFHIRObject.replaceProperty(propName: string; existing, new: TFHIRObject);
begin
  raise Exception.Create('The property "'+propName+'" is unknown replacing the property"');
end;

procedure TFHIRObject.replacePropertyValue(name: String; list: TFHIRObjectList; existing, new: TFHIRObject);
var
  i : integer;
begin
  i := list.IndexByReference(existing);
  if (i = -1) then
    raise Exception.Create('Unable to find object in '+name+' to remove it');
  list.SetItem(i, new);
end;

function TFHIRObject.toString: String;
begin
  if isPrimitive then
    result := fhirType+'[''' + primitiveValue + ''']'
  else
    result := fhirType;
end;

{ TFHIRPathExpressionNode }

function TFHIRPathExpressionNode.Canonical: String;
var
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    write(b);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

function TFHIRPathExpressionNode.check(out msg: String; refCount : integer): boolean;
var
  n : TFHIRPathExpressionNode;
begin
  msg := '';
  if refCount <> AdvObjectReferenceCount then
    msg := 'Reference Count mistmatch'
  else
    case kind of
    enkName:
      if Name = '' then
        msg := 'No Name provided @ '+location;
    enkFunction:
      begin
        if (FFunctionId = pfNull) and (FName <> 'null') then
          msg := 'No Function id provided @ '+location;
        for n in parameters do
          if not n.check(msg, 0) then
            break;
      end;
    enkConstant:
      if FConstant = '' then
        msg := 'No Constant provided @ '+location;
    enkGroup:
      if FGroup = nil then
        msg := 'No Group provided @ '+location
      else
        FGroup.check(msg, 0);
  end;
  if (msg = '') and (FInner <> nil) then
    FInner.check(msg, 0);
  if (msg = '') then
    begin
    if FOperation = popNull then
    begin
      if FOpNext <> nil then
        msg := 'Next provided when it shouldn''t be @ '+location
    end
    else
      if FOpNext = nil then
        msg := 'No Next provided @ '+location
      else
        FOpNext.check(msg, 0);
    end;
  result := msg = '';
end;

function TFHIRPathExpressionNode.checkName: boolean;
begin
  if (name.StartsWith('$')) then
    result := StringArrayExistsSensitive(['$this', '$resource'], name)
  else
    result := true;
end;

constructor TFHIRPathExpressionNode.Create;
begin
  inherited Create;
  FUniqueId := uniqueId
end;

destructor TFHIRPathExpressionNode.Destroy;
begin
  FParameters.free;
  FOpNext.Free;
  FInner.Free;
  FGroup.Free;
  FTypes.Free;
  FOpTypes.Free;
  inherited;
end;

function TFHIRPathExpressionNode.Link: TFHIRPathExpressionNode;
begin
  result := TFHIRPathExpressionNode(inherited Link);
end;

function TFHIRPathExpressionNode.location: String;
begin
  result := inttostr(SourceLocationStart.line)+', '+inttostr(SourceLocationStart.col);
end;

function TFHIRPathExpressionNode.opLocation: String;
begin
  result := inttostr(OpSourceLocationStart.line)+', '+inttostr(OpSourceLocationStart.col);
end;

function TFHIRPathExpressionNode.ParameterCount: integer;
begin
  if FParameters = nil then
    result := 0
  else
    result := FParameters.Count;
end;

procedure TFHIRPathExpressionNode.SetFunctionId(const Value: TFHIRPathFunction);
begin
  FFunctionId := Value;
  if FParameters = nil then
    FParameters := TAdvList<TFHIRPathExpressionNode>.create;
end;

procedure TFHIRPathExpressionNode.SetOpNext(const Value: TFHIRPathExpressionNode);
begin
  FOpNext.Free;
  FOpNext := Value;
end;

procedure TFHIRPathExpressionNode.SetTypes(const Value: TFHIRTypeDetails);
begin
  FTypes.Free;
  FTypes := Value;
end;

function TFHIRPathExpressionNode.summary: String;
begin
  case FKind of
    enkName: result := inttostr(uniqueId)+': '+FName;
    enkFunction: result := inttostr(uniqueId)+': '+CODES_TFHIRPathFunctions[FFunctionId]+'()';
    enkConstant: result := inttostr(uniqueId)+': "'+FConstant+'"';
    enkGroup: result := inttostr(uniqueId)+': (Group)';
  end;
end;

function TFHIRPathExpressionNode.toString: String;
var
  b : TStringBuilder;
  first : boolean;
  n : TFHIRPathExpressionNode;
begin
  b := TStringBuilder.create();
  try
		case kind of
		enkName:
      begin
        b.append(name);
      end;
		enkFunction:
      begin
        if (FunctionId = pfItem) then
          b.append('[')
        else
        begin
          b.append(name);
          b.append('(');
        end;
        first := true;
        for n in parameters do
        begin
          if (first) then
            first := false
          else
            b.append(', ');
          b.append(n.toString());
        end;
        if (FunctionId = pfItem) then
          b.append(']')
        else
          b.append(')');
      end;
		enkConstant:
      begin
    	  b.append(jsonEscape(constant, true));
			end;
		enkGroup:
      begin
  			b.append('(');
	  		b.append(group.toString());
		  	b.append(')');
		  end;
    end;

		if (inner <> nil) then
    begin
			b.append('.');
			b.append(inner.toString());
		end;
		if (operation <> popNull) then
    begin
			b.append(' ');
			b.append(CODES_TFHIRPathOperation[operation]);
			b.append(' ');
			b.append(opNext.toString());
		end;

		result := b.toString();
  finally
    b.free;
  end;
end;

procedure TFHIRPathExpressionNode.write(b: TStringBuilder);
var
  f : boolean;
  n : TFHIRPathExpressionNode;
begin
  case fKind of
    enkName:
      b.Append(FName);
    enkConstant:
      b.Append(FConstant);
    enkFunction:
      begin
        b.Append(CODES_TFHIRPathFunctions[FFunctionId]);
        b.Append('(');
        f := true;
        for n in Parameters do
        begin
          if f then
            f := false
          else
            b.Append(', ');
          n.write(b);
        end;
        b.Append(')');
      end;
    enkGroup:
      begin
        b.Append('(');
        FGroup.write(b);
        b.Append(')');
      end;
  end;
  if inner <> nil then
  begin
    b.Append('.');
    inner.write(b);
  end;
  if Operation <> popNull then
  begin
    b.Append(' ');
    b.Append(CODES_TFHIRPathOperation[Operation]);
    b.Append(' ');
    OpNext.write(b);
  end;
end;

procedure TFHIRPathExpressionNode.SetOpTypes(const Value: TFHIRTypeDetails);
begin
  FOpTypes.Free;
  FOpTypes := Value;
end;

procedure TFHIRPathExpressionNode.SetInner(const Value: TFHIRPathExpressionNode);
begin
  FInner.free;
  FInner := Value;
end;

procedure TFHIRPathExpressionNode.SetGroup(const Value: TFHIRPathExpressionNode);
begin
  FGroup.Free;
  FGroup := Value;
end;


{ TFHIRTypeDetails }

var
  gc : integer = 0;

constructor TFHIRTypeDetails.createList(status: TFHIRCollectionStatus; types: TStringList);
begin
  inherited Create;
  FTypes := TStringList.create;
  FTypes.Sorted := true;
  FCollectionStatus := status;
  addTypes(types);
  inc(gc);
  id := gc;
end;

constructor TFHIRTypeDetails.create(status: TFHIRCollectionStatus; types: array of String);
begin
  inherited Create;
  FTypes := TStringList.create;
  FTypes.Sorted := true;
  FCollectionStatus := status;
  addTypes(types);
  inc(gc);
  id := gc;
end;

destructor TFHIRTypeDetails.Destroy;
begin
  FTypes.Free;
  inherited;
end;

procedure TFHIRTypeDetails.addType(n: String);
begin
  if (n <> '') then
    if not hasType(n) then
      FTypes.add(n);
end;

procedure TFHIRTypeDetails.addTypes(n: TStringList);
var
  t : String;
begin
  for t in n do
    addType(t);
end;

procedure TFHIRTypeDetails.addTypes(types: array of String);
var
  t : String;
begin
  for t in types do
    addType(t);
end;

function TFHIRTypeDetails.describe: String;
begin
  result := FTypes.commaText;
end;

function TFHIRTypeDetails.hasNoTypes: boolean;
begin
  result := FTypes.count = 0;
end;

function TFHIRTypeDetails.hasType(types: TStringList): boolean;
var
  t : String;
begin
  result := false;
  for t in types do
    if hasType(t) then
      exit(true);
end;

function TFHIRTypeDetails.hasType(typeName: String): boolean;
begin
  result := FTypes.indexOf(typeName) > -1;
end;

function TFHIRTypeDetails.Link: TFHIRTypeDetails;
begin
  result := TFHIRTypeDetails(inherited Link);
end;

function TFHIRTypeDetails.hasType(types: array of String): boolean;
var
  t : String;
begin
  result := false;
  for t in types do
    if hasType(t) then
      exit(true);
end;

function TFHIRTypeDetails.toSingleton: TFHIRTypeDetails;
begin
  result := TfhirTypeDetails.createList(csSINGLETON, FTypes);
end;

function TfhirTypeDetails.type_: String;
begin

end;

function TFHIRTypeDetails.union(right: TFHIRTypeDetails): TFHIRTypeDetails;
begin
  result := TFHIRTypeDetails.createList(csNULL, FTypes);
  if (right.FcollectionStatus in [csUNORDERED, csUNORDERED]) then
    result.FcollectionStatus := csUNORDERED
  else
    result.FcollectionStatus := csORDERED;
  result.addTypes(types);
  result.addTypes(right.types);
end;

procedure TFHIRTypeDetails.update(source: TFHIRTypeDetails);
begin
  addTypes(source.types);
  if (FcollectionStatus = csNULL) then
    FcollectionStatus := source.collectionStatus
  else if (source.FcollectionStatus = csUNORDERED) then
    FcollectionStatus := source.collectionStatus
  else
    FcollectionStatus := csORDERED;
end;


function isPrimitiveType(name : String) : boolean;
begin
  result := StringArrayExistsSensitive(['integer', 'unsignedInt', 'positiveInt', 'decimal', 'dateTime', 'date',
    'time', 'instant', 'string', 'uri', 'oid', 'uuid', 'id', 'boolean', 'code', 'markdown', 'xhtml', 'base64Binary'], name);
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

destructor TFHIRSelection.Destroy;
begin
  FParent.Free;
  FValue.Free;
  inherited;
end;

function TFHIRSelection.Link: TFHIRSelection;
begin
  result := TFHIRSelection(inherited link);
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
      if (not FHIRBase.compareDeep(e1[i].value as TFHIRObject, e2[i].value as TFHIRObject, allowNull)) then
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

End.
