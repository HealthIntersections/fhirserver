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
  CODES_FHIR_GENERATED_PUBLICATION : array [TFHIRVersion] of string = ('', '1', '2', '3', '4');
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

  TFHIRProperty = class (TAdvObject)
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
    Constructor Create(oOwner : TFHIRObject; Const sName, sType : String; bList : boolean; cClass : TClass; oList : TAdvList<TFHIRObject>); Overload;
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

    // version delegation
    function makeStringValue(v : String) : TFHIRObject; virtual;
    function makeCodeValue(v : String) : TFHIRObject; virtual;
    function GetVersion: TFHIRVersion; virtual;
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
//    Function PerformQuery(path : String) : TFHIRObjectList;
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

    Property version : TFHIRVersion read GetVersion;
    Property id : String read Getid;
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

  TFHIRResourceV = class (TFHIRObject)
  public
    function link : TFHIRResourceV; overload;
  end;

  TFHIRWorkerContextV = class (TAdvObject)
  protected
    function GetVersion: TFHIRVersion; virtual;
  public
    function link : TFHIRWorkerContextV; overload;

    Property version : TFHIRVersion read GetVersion;
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
    procedure addAll(value : TAdvList<TFHIRObject>); overload;
    procedure addAll(parent : TFHIRObject; name : String; value : TFHIRObjectList); overload;

    function asValues : TFHIRObjectList;

    class function compareDeep(e1, e2 : TFHIRSelectionList; allowNull : boolean) : boolean;
  end;


function noList(e : TFHIRObjectList) : boolean; overload;
function compareDeep(e1, e2 : TFHIRObjectList; allowNull : boolean) : boolean; overload;
function compareDeep(e1, e2 : TFHIRObject; allowNull : boolean) : boolean; overload;
function isPrimitiveType(name : String):boolean;
function isEmptyProp(v : TFHIRObject) : boolean; overload;
function isEmptyProp(v : TFHIRObjectList) : boolean; overload;

Implementation

Uses
  StringSupport;

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

function TFHIRObject.GetVersion: TFHIRVersion;
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

function TFHIRObject.makeCodeValue(v: String): TFHIRObject;
begin
  raise Exception.Create('Must override makeCodeValue in '+className);
end;

function TFHIRObject.makeStringValue(v: String): TFHIRObject;
begin
  raise Exception.Create('Must override makeCodeValue in '+className);
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

constructor TFHIRProperty.Create(oOwner: TFHIRObject; const sName, sType: String; bList: boolean; cClass: TClass; oList: TAdvList<TFHIRObject>);
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

procedure TFHIRSelectionList.addAll(value: TAdvList<TFHIRObject>);
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

{ TFHIRWorkerContextV }

function TFHIRWorkerContextV.GetVersion: TFHIRVersion;
begin
  result := fhirVersionUnknown;
end;

function TFHIRWorkerContextV.link: TFHIRWorkerContextV;
begin
  result := TFHIRWorkerContextV(inherited Link);
end;

{ TFHIRResourceV }

function TFHIRResourceV.link: TFHIRResourceV;
begin
  result := TFHIRResourceV(inherited Link);
end;

End.


