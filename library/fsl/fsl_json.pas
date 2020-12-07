Unit fsl_json;

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

{$I fhir.inc}

Interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections;

Function JSONString(const value : String) : String;
Function UnJSONString(const value : String) : String;

Type
  TJsonObject = class;
  TJsonArray = class;
  TJsonPointerMatch = class;

  TJsonNodeKind = (jnkNull, jnkBoolean, jnkString, jnkNumber, jnkObject, jnkArray);

  { TJsonNode }

  TJsonNode = class abstract (TFslObject)
  private
    FPath: String;
    function setStart(loc : TSourceLocation) : TJsonNode;
  protected
    function nodeType : String; virtual;
    function compare(other : TJsonNode) : boolean; overload; virtual; abstract;
    function evaluatePointer(path : String) : TJsonNode; virtual;
    function findLocation(loc: TSourceLocation; name : String; path : TFslList<TJsonPointerMatch>) : boolean; overload; virtual;
  public
    LocationStart : TSourceLocation;
    LocationInner : TSourceLocation; // where inner content starts
    LocationEnd : TSourceLocation;

    constructor Create(path : String); overload;
    constructor Create(path : String; locStart, locEnd : TSourceLocation); overload;
    Function Link : TJsonNode; Overload;
    function kind : TJsonNodeKind; virtual; abstract;
    property path : String read FPath write FPath;

    class function compare(n1, n2 : TJsonNode) : boolean; overload;

    function findLocation(loc : TSourceLocation) : TFslList<TJsonPointerMatch>; overload;
    function describePath(path : TFslList<TJsonPointerMatch>) : string;
  end;
  TJsonNodeClass = class of TJsonNode;

  TJsonArrayEnumerator = class (TFslObject)
  private
    FArray : TJsonArray;
    cursor : integer;
    function GetCurrent: TJsonNode;
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; Override;
    function MoveNext() : boolean;
    Property Current : TJsonNode read GetCurrent;
  end;

  { TJsonArray }

  TJsonArray = class (TJsonNode)
  private
    FItems : TFslObjectList;
    function GetCount: integer;
    function GetItem(i: integer): TJsonNode;
    function GetObj(i: integer): TJsonObject;
    function GetValue(i: integer): String;
    procedure SetItem(i: integer; const Value: TJsonNode);
    procedure SetObj(i: integer; const Value: TJsonObject);
    procedure SetValue(i: integer; const Value: String);
  protected
    function nodeType : String; override;
    function compare(other : TJsonNode) : boolean; override;
    function evaluatePointer(path : String) : TJsonNode; override;
    function findLocation(loc: TSourceLocation; name : String; path : TFslList<TJsonPointerMatch>) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Function Link : TJsonArray; Overload;
    function kind : TJsonNodeKind; override;

    Property Count : integer read GetCount;
    Property Item[i : integer] : TJsonNode read GetItem write SetItem;
    Property Obj[i : integer] : TJsonObject read GetObj write SetObj; default;
    Property Value[i : integer] : String read GetValue write SetValue;

    function add(value : String): TJsonArray; overload;
    function add(value : TJsonNode): TJsonArray; overload;
    function addObject : TJsonObject; overload;

    procedure remove(index : integer);
    procedure move(index, delta : integer);
    procedure clear;

    function asObjects : TFslList<TJsonObject>;
    function GetEnumerator : TJsonArrayEnumerator; // can only use this when the array members are objects
  end;

  TJsonNull = class (TJsonNode)
  protected
    function nodeType : String; override;
  public
    function kind : TJsonNodeKind; override;
    function compare(other : TJsonNode) : boolean; override;
  end;

  TJsonBoolean = class (TJsonNode)
  private
    FValue: boolean;
  protected
    function nodeType : String; override;
    function compare(other : TJsonNode) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(path : String; value : boolean); overload;
    constructor Create(path : String; locStart, locEnd : TSourceLocation; value : boolean); overload;
    Function Link : TJsonBoolean; Overload;
    function kind : TJsonNodeKind; override;
    property value : boolean read FValue write FValue;
  end;

  TJsonString = class (TJsonNode)
  private
    FValue: String;
  protected
    function nodeType : String; override;
    function compare(other : TJsonNode) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(path : String; value : string); overload;
    constructor Create(path : String; locStart, locEnd : TSourceLocation; value : string); overload;
    Function Link : TJsonString; Overload;
    function kind : TJsonNodeKind; override;
    property value : String read FValue write FValue;
  end;

  TJsonNumber = class (TJsonNode)
  private
    FValue: String;
  protected
    function nodeType : String; override;
    function compare(other : TJsonNode) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(path : String; value : string); overload;
    constructor Create(path : String; locStart, locEnd : TSourceLocation; value : string); overload;
    Function Link : TJsonNumber; Overload;
    function kind : TJsonNodeKind; override;
    property value : String read FValue write FValue;
  end;

  { TJsonObject }

  TJsonObject = class (TJsonNode)
  private
    FName : String;
    FProperties : TFslMap<TJsonNode>;
    function GetString(name: String): String;
    function GetNumber(name: String): String;
    function GetArray(name: String): TJsonArray;
    function GetObject(name: String): TJsonObject;
    procedure SetString(name: String; const Value: String);
    procedure SetNumber(name: String; const Value: String);
    function GetBool(name: String): boolean;
    procedure SetBool(name: String; const Value: boolean);
    function GetForcedObject(name: String): TJsonObject;
    procedure SetArray(name: String; const Value: TJsonArray);
    procedure SetObject(name: String; const Value: TJsonObject);
    function GetForcedArray(name: String): TJsonArray;
    function GetNode(name: String): TJsonNode;
    procedure setNode(name: String; const Value: TJsonNode);
    function GetInteger(name: String): Integer;

    procedure SetInteger(name: String; const Value: Integer);
  protected
    function nodeType : String; override;
    function compare(other : TJsonNode) : boolean; override;
    function evaluatePointer(path : String) : TJsonNode; override;
    function findLocation(loc: TSourceLocation; name : String; path : TFslList<TJsonPointerMatch>) : boolean; override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    Function Link : TJsonObject; Overload;
    function kind : TJsonNodeKind; override;

    Function has(name : String) : Boolean;
    Function isNull(name : String) : Boolean;

    property node[name : String] : TJsonNode read GetNode write setNode;
    Property str[name : String] : String read GetString write SetString; default;
    Property num[name : String] : String read GetNumber write SetNumber;
    Property int[name : String] : Integer read GetInteger write SetInteger;
    Property bool[name : String] : boolean read GetBool write SetBool;
    Property arr[name : String] : TJsonArray read GetArray write SetArray;
    Property obj[name : String] : TJsonObject read GetObject write SetObject;

//    // legacy, until the FHIR code is regenerated
    Property vStr[name : String] : String read GetString write SetString;
    Property vBool[name : String] : boolean read GetBool write SetBool;
    Property vArr[name : String] : TJsonArray read GetArray write SetArray;
    Property vObj[name : String] : TJsonObject read GetObject write SetObject;

    Property forceObj[name : String] : TJsonObject read GetForcedObject;
    Property forceArr[name : String] : TJsonArray read GetForcedArray;
    procedure clear(name : String = '');

    function str2(n1, n2 : String) : String;
    Property name : String read FName write FName;
    Property properties : TFslMap<TJsonNode> read FProperties;
  end;

  TJsonPointerTerminalState = (tsNotFound, tsFound, tsAtEnd);

  TJsonPointerMatch = class (TFslObject)
  private
    FName: String;
    FNode: TJsonNode;
    procedure SetNode(const Value: TJsonNode);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(name : String; node : TJsonNode);
    destructor Destroy; Override;
    property name : String read FName write FName;
    property node : TJsonNode read FNode write SetNode;
  end;

  TJsonPointerQuery = class (TFslObject)
  private
    FMatches : TFslList<TJsonPointerMatch>;
    FTerminalState: TJsonPointerTerminalState;
    function GetLast: TJsonNode;
    function GetLastName: String;
    function GetSecondLast: TJsonNode;

    function unescape(s : String) : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; Override;
    procedure execute(focus : TJsonNode; path : string; terminalExtensions : boolean);

    property terminalState : TJsonPointerTerminalState read FTerminalState;
    property last : TJsonNode read GetLast;
    property lastName : String read GetLastName;
    property secondLast : TJsonNode read GetSecondLast;
  end;

  { TJSONWriter }

  TJSONWriter = class (TFslTextFormatter)
  private
    FBuilder : TFslStringBuilder;
  protected
    procedure doValue(name, value : String); virtual; abstract;
    function GetSourceLocation: TSourceLocation; virtual; abstract;

    Function JSONString(const value : String) : String;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; Override;
    Function Link: TJSONWriter; overload;
    function canInject : boolean; virtual;

    property sourceLocation : TSourceLocation read GetSourceLocation;
    Procedure Start(obj : boolean = true); virtual; abstract;
    Procedure Finish(obj : boolean = true); virtual;  abstract;

    Procedure Value(Const name : String; Const avalue : String); overload;
    Procedure ValueNumber(Const name : String; Const avalue : String); overload;
    Procedure ValueBoolean(Const name : String; avalue : Boolean); overload;
    Procedure Value(Const name : String; avalue : Boolean); overload;
    Procedure Value(Const name : String; avalue : Integer); overload;
    Procedure Value(Const name : String; avalue : Int64); overload;
    Procedure Value(Const name : String; avalue : Double); overload;
    Procedure ValueDate(Const name : String; aValue : TDateTime); overload;
    Procedure ValueNull(Const name : String);
    Procedure ValueBytes(Const name : String; bytes : TBytes); virtual;

    Procedure ValueObject(Const name : String); Overload; virtual;
    Procedure ValueObject; Overload; virtual;
    Procedure FinishObject; virtual;  abstract;

    Procedure ValueArray(Const name : String); virtual;
    Procedure FinishArray; virtual;  abstract;

    Procedure ValueInArray(Const value : String); overload; virtual;  abstract;
    Procedure ValueNumberInArray(Const value : String); overload;
    Procedure ValueInArray(value : Boolean); overload;
    Procedure ValueInArray(value : Integer); overload;
    Procedure ValueInArray(value : Int64); overload;
    Procedure ValueInArray(value : Double); overload;
    Procedure ValueDateInArray(aValue : TDateTime); overload;
    Procedure ValueNullInArray; virtual;

    Procedure WriteObject(name : String; obj : TJsonObject); overload;
    Procedure WriteObjectInner(obj : TJsonObject);
    Procedure WriteArray(name : String; arr : TJsonArray); overload;

    function ToString : String; override;

    class Function writeObject(obj : TJsonObject; pretty : boolean = false) : TBytes; overload;
    class Function writeArray(arr : TJsonArray; pretty : boolean = false) : TBytes; overload;
    class Function writeObjectStr(obj : TJsonObject; pretty : boolean = false) : String; overload;
    class Function writeArrayStr(arr : TJsonArray; pretty : boolean = false) : String; overload;
    class Procedure writeObject(stream : TStream; obj : TJsonObject; pretty : boolean = false); overload;
    class Procedure writeObject(stream : TFslStream; obj : TJsonObject; pretty : boolean = false); overload;
    class Procedure writeArray(stream : TStream; arr : TJsonArray; pretty : boolean = false); overload;
    class Procedure writeArray(stream : TFslStream; arr : TJsonArray; pretty : boolean = false); overload;
  end;

  TJsonWriterDirect = class (TJSONWriter)
  private
    FName : String;
    FCache : String;
    FProperty : Boolean;
    Function UseName : String;
    Function UseCache : String;
    procedure DoName(const name : String);
  protected
    procedure doValue(name, value : String); override;
    function sizeInBytesV : cardinal; override;
  Public
    Function Link: TJsonWriterDirect; overload;
    function canInject : boolean; override;

    function GetSourceLocation : TSourceLocation; override;
    Procedure Start(obj : boolean); override;
    Procedure Finish(obj : boolean); override;

    Procedure ValueBytes(Const name : String; bytes : TBytes); override;
    Procedure ValueObject(Const name : String); Overload; override;
    Procedure ValueObject; Overload; override;
    Procedure FinishObject; override;
    Procedure ValueArray(Const name : String); override;
    Procedure FinishArray; override;
    Procedure ValueInArray(Const value : String); overload; override;
    procedure ValueNullInArray; override;
  End;

  TCanonicalJsonNodeType = (jntProperty, jntObject, jntArray);

  TCanonicalJsonNode = class (TFslObject)
  private
    FType : TCanonicalJsonNodeType;
    FName : String;
    FValue : String;
    FChildren : TFslList<TCanonicalJsonNode>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(aType : TCanonicalJsonNodeType);
    destructor Destroy; override;
    function link : TCanonicalJsonNode; overload;
  end;

  // this one resorts all attributes to alphametical order
  TJsonWriterCanonical = class (TJSONWriter)
  private
    FObject : TCanonicalJsonNode;
    FStack : TFslList<TCanonicalJsonNode>;
    procedure commitObject(node : TCanonicalJsonNode);
    procedure commitArray(node : TCanonicalJsonNode);
  protected
    procedure doValue(name, value : String); override;
    function sizeInBytesV : cardinal; override;
  public
    Function Link: TJsonWriterCanonical; overload;
    function GetSourceLocation : TSourceLocation; override;
    Procedure Start(obj : boolean); override;
    Procedure Finish(obj : boolean); override;
    Procedure ValueObject(Const name : String); Overload; override;
    Procedure ValueObject; Overload; override;
    Procedure FinishObject; override;
    Procedure ValueArray(Const name : String); override;
    Procedure FinishArray; override;
    Procedure ValueInArray(Const value : String); overload; override;
    procedure ValueNullInArray; override;
  end;

  TJSONLexType = (jltOpen, jltClose, jltString, jltNumber, jltColon, jltComma, jltOpenArray, jltCloseArray, jltEof, jltNull, jltBoolean);

  TJSONLexer = class (TFslTextExtractor)
  Private
    FLoose : boolean;
    FPeek : String;
    FValue: TStringBuilder;
    FLexType: TJSONLexType;
    FStates : TStringList;
    FLastLocationBWS : TSourceLocation;
    FLastLocationAWS : TSourceLocation;
    FPrevLocation : TSourceLocation;
    FLocation : TSourceLocation;
    Function getNextChar : Char;
    Function peekNextChar : Char;
    Procedure Push(ch : Char);
    procedure ParseWord(sWord : String; ch : Char; aType : TJSONLexType);
    Procedure JsonError(sMsg : String);
    Function Path : String;
    function GetValue: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oStream : TFslStream; loose : boolean = false); Overload;
    destructor Destroy; Override;
    Procedure Start;
    Property LexType : TJSONLexType read FLexType;
    Property Value : String read GetValue;
    Procedure Next;
    Function Consume(aType : TJsonLexType):String;
  End;

  TJsonParserItemType = (jpitObject, jpitString, jpitNumber, jpitBoolean, jpitArray, jpitEnd, jpitEof, jpitNull);

  TJSONParser = class (TFslObject)
  Private
    FLex : TJSONLexer;
    FNameStart : TSourceLocation;
    FNameEnd : TSourceLocation;
    FValueStart : TSourceLocation;
    FValueEnd : TSourceLocation;
    FItemName: String;
    FItemValue: String;
    FItemType: TJsonParserItemType;
    FTimeToAbort : cardinal;
    Procedure ParseProperty;
    Procedure SkipInner;
    function GetItemValue: String;
    function GetItemNull: boolean;
    procedure readObject(obj : TJsonObject; root : boolean);
    procedure readArray(arr : TJsonArray; root : boolean);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(oStream : TStream; loose : boolean); Overload;
    constructor Create(oStream : TFslStream; loose : boolean);  Overload;
    destructor Destroy; Override;
    Property ItemType : TJsonParserItemType read FItemType;
    Property ItemName : String read FItemName;
    Property ItemValue : String read GetItemValue;
    Property ItemNull : boolean read GetItemNull;
    Procedure Next;
    Procedure Skip;
    Procedure JsonError(sMsg : String);
    Procedure CheckState(aState : TJsonParserItemType);
    function readNode : TJsonNode;
    // loose: allow comments and comma deviations
    class Function Parse(stream : TFslStream; timeToAbort : cardinal = 0; loose : boolean = false): TJsonObject; overload;
    class Function Parse(stream : TStream; timeToAbort : cardinal = 0; loose : boolean = false): TJsonObject; overload;
    class Function Parse(b : TBytes; timeToAbort : cardinal = 0; loose : boolean = false): TJsonObject; overload;
    class Function Parse(s : String; timeToAbort : cardinal = 0; loose : boolean = false): TJsonObject; overload;
    class Function ParseNode(stream : TFslStream; timeToAbort : cardinal = 0; loose : boolean = false): TJsonNode; overload;
    class Function ParseNode(stream : TStream; timeToAbort : cardinal = 0; loose : boolean = false): TJsonNode; overload;
    class Function ParseNode(b : TBytes; timeToAbort : cardinal = 0; loose : boolean = false): TJsonNode; overload;
    class Function ParseNode(s : String; timeToAbort : cardinal = 0; loose : boolean = false): TJsonNode; overload;
    class Function ParseFile(fn : String; loose : boolean = false) : TJsonObject; overload;
  End;

  TJsonPatchEngine = class (TFslObject)
  private
    FPatch: TJsonArray;
    FTarget: TJsonNode;
    procedure SetPatch(const Value: TJsonArray);
    procedure SetTarget(const Value: TJsonNode);
    procedure applyPatchOperation(patchOp : TJsonObject);
    procedure applyAdd(patchOp : TJsonObject; path : String);
    procedure applyAddInner(path : String; value : TJsonNode);
    procedure applyRemove(patchOp : TJsonObject; path : String);
    procedure applyReplace(patchOp : TJsonObject; path : String);
    procedure applyMove(patchOp : TJsonObject; path : String);
    procedure applyCopy(patchOp : TJsonObject; path : String);
    procedure applyTest(patchOp : TJsonObject; path : String);

    class procedure runtest(test : TJsonObject);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property patch : TJsonArray read FPatch write SetPatch;
    property target : TJsonNode read FTarget write SetTarget;

    procedure execute;

    class function applyPatch(target : TJsonObject; patch : TJsonArray) : TJsonObject;

    // source for tests: https://github.com/json-patch/json-patch-tests/blob/master/spec_tests.json
    class procedure tests(fileName : String);
  end;

Const
  Codes_TJsonParserItemType : Array[TJsonParserItemType] of String = ('Object', 'String', 'Number', 'Boolean', 'Array', 'End', 'EOF', 'Null');
  Codes_TJSONLexType : Array[TJSONLexType] of String = ('Open', 'Close', 'String', 'Number', 'Colon', 'Comma', 'OpenArray', 'CloseArray', 'Eof', 'Null', 'Boolean');

function JsonBoolToString(b : boolean) : String;
function JsonStringToBool(s : String; def : boolean = false) : boolean;

type
 TJWT = class (TFslObject)
  private
    FHeader : TJsonObject;
    FPayLoad : TJsonObject;
    FOriginalSource: String;

    procedure setHeader(const Value: TJsonObject);
    procedure setPayload(const Value: TJsonObject);

    function GetaddressCountry: string;
    function GetaddressFormatted: string;
    function GetaddressLocality: string;
    function GetaddressPostCode: string;
    function GetaddressRegion: string;
    function GetaddressStreet: string;
    function Getaudience: string;
    function Getbirthdate: string;
    function Getemail: string;
    function GetemailVerified: boolean;
    function Getexpires: TDateTime;
    function GetfamilyName: string;
    function Getgender: string;
    function GetgivenName: string;
    function Getid: string;
    function GetissuedAt: TDateTime;
    function Getissuer: string;
    function Getlocale: string;
    function GetmiddleName: string;
    function Getname: string;
    function GetnickName: string;
    function GetnotBefore: TDateTime;
    function Getphone: string;
    function Getphone_verified: boolean;
    function Getpicture: string;
    function GetpreferredName: string;
    function Getprofile: string;
    function Getsubject: string;
    function GettimeZone: string;
    function GetupdatedAt: TDateTime;
    function Getwebsite: string;
    procedure SetaddressCountry(Value: string);
    procedure SetaddressFormatted(Value: string);
    procedure SetaddressLocality(Value: string);
    procedure SetaddressPostCode(Value: string);
    procedure SetaddressRegion(Value: string);
    procedure SetaddressStreet(Value: string);
    procedure Setaudience(Value: string);
    procedure Setbirthdate(Value: string);
    procedure Setemail(Value: string);
    procedure SetemailVerified(Value: boolean);
    procedure Setexpires(Value: TDateTime);
    procedure SetfamilyName(Value: string);
    procedure Setgender(Value: string);
    procedure SetgivenName(Value: string);
    procedure Setid(Value: string);
    procedure SetissuedAt(Value: TDateTime);
    procedure Setissuer(Value: string);
    procedure Setlocale(Value: string);
    procedure SetmiddleName(Value: string);
    procedure Setname(Value: string);
    procedure SetnickName(Value: string);
    procedure SetnotBefore(Value: TDateTime);
    procedure Setphone(Value: string);
    procedure Setphone_verified(Value: boolean);
    procedure Setpicture(Value: string);
    procedure SetpreferredName(Value: string);
    procedure Setprofile(Value: string);
    procedure Setsubject(Value: string);
    procedure SettimeZone(Value: string);
    Procedure SetUpdatedAt(Value: TDateTime);
    procedure Setwebsite(Value: string);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    constructor Create(header, payload : TJsonObject); overload;

    function Link : TJWT; overload;

    destructor Destroy; override;

    property originalSource : String read FOriginalSource write FOriginalSource;

    // the header is provided to get/set extra properties beyond those used in packing/unpacking.
    // you don't need to do anything with it if you don't use extra properties
    Property header : TJsonObject read FHeader write setHeader;
    Property payload : TJsonObject read FPayload write setPayload;

    // information in the JWT
    // from JWT itself
    property issuer : string read Getissuer write Setissuer; // 'iss'
    property subject : string read Getsubject write Setsubject;  // 'sub' Identifier for the End-User at the Issuer.
    property audience : string read Getaudience write Setaudience; // 'aud'
    property expires : TDateTime read Getexpires write Setexpires; // 'exp'
    property notBefore : TDateTime read GetnotBefore write SetnotBefore; // 'nbf'
    property issuedAt : TDateTime read GetissuedAt write SetissuedAt; // 'ist'
    property id : string read Getid write Setid; // 'jti'

    function desc : String;

    // from openid:
    property name : string  read Getname write Setname; // 'name' End-User's full name in displayable form including all name parts, possibly including titles and suffixes, ordered according to the End-User's locale and preferences.
    property givenName : string read GetgivenName write SetgivenName; // 'given_name' Given name(s) or first name(s) of the End-User. Note that in some cultures, people can have multiple given names; all can be present, with the names being separated by space characters.
    property familyName : string read GetfamilyName  write SetfamilyName; // 'family_name' Surname(s) or last name(s) of the End-User. Note that in some cultures, people can have multiple family names or no family name; all can be present, with the names being separated by space characters.
    property middleName : string read GetmiddleName  write SetmiddleName; // 'middle_name' Middle name(s) of the End-User. Note that in some cultures, people can have multiple middle names; all can be present, with the names being separated by space characters. Also note that in some cultures, middle names are not used.
    property nickName : string read GetnickName  write SetnickName;  // 'nickname' Casual name of the End-User that may or may not be the same as the given_name. For instance, a nickname value of Mike might be returned alongside a given_name value of Michael.
    property preferredName : string read GetpreferredName write SetpreferredName;  // 'preferred_username' Shorthand name by which the End-User wishes to be referred to at the RP, such as janedoe or j.doe. This value MAY be any valid JSON string including special characters such as @, /, or whitespace. The RP MUST NOT rely upon this value being unique, as discussed in Section 5.7.
    property profile : string read Getprofile write Setprofile; // 'profile' URL of the End-User's profile page. The contents of this Web page SHOULD be about the End-User.
    property picture : string read Getpicture write Setpicture; // 'picture' URL of the End-User's profile picture. This URL MUST refer to an image file (for example, a PNG, JPEG, or GIF image file), rather than to a Web page containing an image. Note that this URL SHOULD specifically reference a profile photo of the End-User suitable for displaying when describing the End-User, rather than an arbitrary photo taken by the End-User.
    property website : string read Getwebsite write Setwebsite; // 'website' URL of the End-User's Web page or blog. This Web page SHOULD contain information published by the End-User or an organization that the End-User is affiliated with.
    property email : string read Getemail write Setemail; // 'email' End-User's preferred e-mail address. Its value MUST conform to the RFC 5322 [RFC5322] addr-spec syntax. The RP MUST NOT rely upon this value being unique, as discussed in Section 5.7.
    property emailVerified : boolean  read GetemailVerified write SetemailVerified; // 'email_verified' True if the End-User's e-mail address has been verified; otherwise false. When this Claim Value is true, this means that the OP took affirmative steps to ensure that this e-mail address was controlled by the End-User at the time the verification was performed. The means by which an e-mail address is verified is context-specific, and dependent upon the trust framework or contractual agreements within which the parties are operating.
    property gender : string read Getgender  write Setgender; // 'gender' End-User's gender. Values defined by this specification are female and male. Other values MAY be used when neither of the defined values are applicable.
    property birthdate : string read Getbirthdate write Setbirthdate; // 'birthdate' End-User's birthday, represented as an ISO 8601:2004 [ISO8601 2004] YYYY-MM-DD format. The year MAY be 0000, indicating that it is omitted. To represent only the year, YYYY format is allowed. Note that depending on the underlying platform's date related function, providing just year can result in varying month and day, so the implementers need to take this factor into account to correctly process the dates.
    property timeZone : string read GettimeZone  write SettimeZone;  // 'zoneinfo' String from zoneinfo [zoneinfo] time zone database representing the End-User's time zone. For example, Europe/Paris or America/Los_Angeles.
    property locale : string read Getlocale  write Setlocale;  // 'locale' End-User's locale, represented as a BCP47 [RFC5646] language tag. This is typically an ISO 639-1 Alpha-2 [ISO639b] language code in lowercase and an ISO 3166-1 Alpha-2 [ISO3166b1] country code in uppercase, separated by a dash. For example, en-US or fr-CA. As a compatibility note, some implementations have used an underscore as the separator rather than a dash, for example, en_US; Relying Parties MAY choose to accept this locale syntax as well.
    property phone : string read Getphone write Setphone; // 'phone_number' End-User's preferred telephone number. E.164 [E.164] is RECOMMENDED as the format of this Claim, for example, +1 (425) 555-1212 or +56 (2) 687 2400. If the phone number contains an extension, it is RECOMMENDED that the extension be represented using the RFC 3966 [RFC3966] extension syntax, for example, +1 (604) 555-1234;ext=5678.
    property phone_verified : boolean  read Getphone_verified  write Setphone_verified; // 'phone_number_verified' True if the End-User's phone number has been verified; otherwise false. When this Claim Value is true, this means that the OP took affirmative steps to ensure that this phone number was controlled by the End-User at the time the verification was performed. The means by which a phone number is verified is context-specific, and dependent upon the trust framework or contractual agreements within which the parties are operating. When true, the phone_number Claim MUST be in E.164 format and any extensions MUST be represented in RFC 3966 format.
    property updatedAt : TDateTime read GetupdatedAt  write SetupdatedAt; // 'updated_at' Time the End-User's information was last updated. Its value is a JSON number representing the number of seconds from 1970-01-01T0:0:0Z as measured in UTC until the date/time.' +                                  '
    // 'address' object   End-User's preferred postal address. The value of the address member is a JSON [RFC4627] structure containing some or all of the members defined in Section 5.1.1.
    property addressFormatted : string read GetaddressFormatted write SetaddressFormatted; // 'address.formatted'  Full mailing address, formatted for display or use on a mailing label. This field MAY contain multiple lines, separated by newlines. Newlines can be represented either as a carriage return/line feed pair ("\r\n") or as a single line feed character ("\n").
    property addressStreet : string read GetaddressStreet write SetaddressStreet; // 'address.street_address'  Full street address component, which MAY include house number, street name, Post Office Box, and multi-line extended street address information. This field MAY contain multiple lines, separated by newlines. Newlines can be represented either as a carriage return/line feed pair ("\r\n") or as a single line feed character ("\n").
    property addressLocality : string read GetaddressLocality write SetaddressLocality; // 'address.locality'  City or locality component.
    property addressRegion : string read GetaddressRegion write SetaddressRegion; // 'address.region'  State, province, prefecture, or region component.
    property addressPostCode : string read GetaddressPostCode write SetaddressPostCode; // 'address.postal_code'  Zip code or postal code component.
    property addressCountry : string read GetaddressCountry write SetaddressCountry; // 'address.country'  Country name component.

    function userName : String;
  end;

Implementation

{ TJSONWriter }

function TJSONWriter.canInject: boolean;
begin
  result := false;
end;

constructor TJSONWriter.Create;
Begin
  Inherited ;
  FBuilder := TFslStringBuilder.Create;
End;

destructor TJSONWriter.Destroy;
Begin
  FBuilder.Free;
  Inherited;
End;

function TJSONWriter.ToString: String;
begin
  if (Stream <> nil) and (Stream is TFslStringStream) then
    result := string(TFslStringStream(Stream).Data)
  else
    result := inherited toString;
end;


function TJSONWriter.JSONString(const value: String): String;
var
  i : integer;
Begin
  FBuilder.Clear;
  FBuilder.Append('"');
  for i := 1 to length(value) do
    case value[i] of
      '"':FBuilder.Append('\"');
      '\':FBuilder.Append('\\');
      #13:FBuilder.Append('\r');
      #10:FBuilder.Append('\n');
      #09:FBuilder.Append('\t');
    else if ord(value[i]) < 32 Then
      FBuilder.Append('\u'+inttohex(ord(value[i]), 4))
    else
      FBuilder.Append(value[i]);
    End;
  FBuilder.Append('"');
  result := FBuilder.AsString;
End;

Function JSONString(const value : String) : String;
var
  i : integer;
Begin
  result := '';
  for i := 1 to length(value) do
    case value[i] of
      '"':result := result + '\"';
      '\':result := result + '\\';
      #13:result := result + '\r';
      #10:result := result + '\n';
      #09:result := result + '\t';
    else if ord(value[i]) < 32 Then
      result := result + '\u'+inttohex(ord(value[i]), 4)
    else
      result := result + value[i];
    End;
End;

Function UnJSONString(const value : String) : String;
var
  i : integer;
  b : TStringBuilder;
  hex : String;
Begin
  b := TStringBuilder.Create;
  try
    i := 1;
    while i <= length(value) do
    begin
      if (i < length(value)) and (value[i] = '\') Then
      begin
        inc(i);
        case value[i] of
          '"':  b.append('"');
          '''': b.append('''');
          '\':  b.append('\');
          '/':  b.append('/');
          'n':  b.append(#10);
          'r':  b.append(#13);
          't':  b.append(#09);
          'u':  begin
                hex := value.Substring(i, 4);
                b.append(chr(StrToInt('$'+hex)));
                inc(i, 4);
                end;
        else
          raise EJsonException.create('illegal escape: \'+value[i]);
        End;
      End
      Else
        b.append(value[i]);
      inc(i);
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
End;

procedure TJSONWriter.Value(const name : String; const avalue: String);
begin
  if name = '' then
    valueInArray(avalue)
  else if avalue = '' then
    ValueNull(name)
  Else
    doValue(name, JSONString(avalue));
end;

procedure TJSONWriter.Value(const name : String; avalue: Boolean);
begin
  if name = '' then
    valueInArray(avalue)
  else if avalue then
    doValue(name, 'true')
  else
    doValue(name, 'false');
end;

procedure TJSONWriter.ValueNull(const name : String);
begin
  if name = '' then
    ValueNullInArray
  else
    doValue(name, 'null');
end;

procedure TJSONWriter.Value(const name : String; avalue: Int64);
begin
  if name = '' then
    valueInArray(avalue)
  else
    doValue(name, inttostr(avalue));
end;

procedure TJSONWriter.Value(const name : String; avalue: Double);
begin
  if name = '' then
    valueInArray(avalue)
  else
    doValue(name, FloatToStr(avalue));
end;

procedure TJSONWriter.ValueArray(const name: String);
begin
  raise EJsonException.Create('Need to override '+className+'.ValueArray');
end;

procedure TJSONWriter.Value(const name : String; avalue: Integer);
begin
  if name = '' then
    valueInArray(avalue)
  else
    doValue(name, inttostr(avalue));
end;


function TJSONWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FBuilder.sizeInBytes);
end;

class procedure TJSONWriter.writeObject(stream: TFslStream; obj: TJsonObject; pretty : boolean = false);
var
  this : TJsonWriterDirect;
begin
  this := TJsonWriterDirect.Create;
  try
    this.HasWhitespace := pretty;
    this.Stream := stream.Link;
    this.Start(true);
    this.writeObjectInner(obj);
    this.Finish(true);
  finally
    this.Free;
  end;
end;

procedure TJSONWriter.WriteArray(name: String; arr: TJsonArray);
var
  i : integer;
  v : TJsonNode;
begin
  ValueArray(name);
  for i := 0 to arr.FItems.Count - 1 do
  begin
    v := arr.Fitems[i] as TJsonNode;
    if v is TJsonArray then
      WriteArray('', v as TJsonArray)
    else if v is TJsonNull then
      ValueNull('')
    else if v is TJsonString then
      Value('', (v as TJsonString).FValue)
    else if v is TJsonNumber then
      Value('', (v as TJsonNumber).FValue)
    else // TJsonObject
      WriteObject('', v as TJsonObject);
  end;
  FinishArray;
end;

class function TJSONWriter.writeArrayStr(arr: TJsonArray; pretty: boolean): String;
begin
  result := TEncoding.UTF8.GetString(writeArray(arr, pretty));
end;

procedure TJSONWriter.WriteObjectInner(obj: TJsonObject);
var
  n : String;
  v : TJsonNode;
begin
  for n in obj.properties.AsAddedKeys do
  begin
    v := obj.properties[n] as TJsonNode;
    if v is TJsonArray then
      WriteArray(n, v as TJsonArray)
    else if v is TJsonNull then
      ValueNull(n)
    else if v is TJsonBoolean then
      Value(n, TJsonBoolean(v).FValue)
    else if v is TJsonString then
      Value(n, (v as TJsonString).FValue)
    else if v is TJsonNumber then
      ValueNumber(n, (v as TJsonNumber).FValue)
    else if v is  TJsonObject then
      WriteObject(n, v as TJsonObject)
    else
      raise EJsonException.Create('Unexpected object type '+v.nodeType);
  end;
end;

procedure TJSONWriter.WriteObject(name : String; obj: TJsonObject);
begin
  ValueObject(name);
  WriteObjectInner(obj);
  FinishObject;
end;

procedure TJSONWriter.ValueNumberInArray(const value: String);
begin
  ValueInArray(value);
end;

procedure TJSONWriter.ValueObject;
begin
  raise EJsonException.Create('Need to override '+className+'.ValueObject');
end;

class procedure TJSONWriter.writeArray(stream: TStream; arr: TJsonArray;
  pretty: boolean);
var
  s : TFslVCLStream;
begin
  s := TFslVCLStream.Create;
  try
    s.Stream := stream;
    writeArray(s, arr, pretty);
  finally
    s.Free;
  end;
end;

class procedure TJSONWriter.writeArray(stream: TFslStream; arr: TJsonArray;
  pretty: boolean);
var
  this : TJsonWriterDirect;
begin
  this := TJsonWriterDirect.Create;
  try
    this.HasWhitespace := pretty;
    this.Stream := stream.link;
    this.Start(false);
    this.WriteArray('', arr);
    this.Finish(false);
  finally
    this.Free;
  end;
end;

class function TJSONWriter.writeArray(arr: TJsonArray; pretty: boolean): TBytes;
var
  mem : TBytesStream;
begin
  mem := TBytesStream.Create;
  try
    writeArray(mem, arr, pretty);
    result := mem.Bytes;
    SetLength(result, mem.size);
  finally
    mem.Free
  end;
end;

procedure TJSONWriter.ValueObject(const name: String);
begin
  raise EJsonException.Create('Need to override '+className+'.ValueObject');
end;

procedure TJSONWriter.ValueInArray(value: Boolean);
begin
  if value then
    valueInArray('true')
  else
    valueInArray('false');
end;

procedure TJSONWriter.ValueNullInArray;
begin
  raise EJsonException.Create('Need to override '+className+'.ValueNullInArray');
end;

procedure TJSONWriter.ValueNumber(const name: String; const avalue: String);
begin
  if name = '' then
    valueNumberInArray(avalue)
  else if avalue = '' then
    ValueNull(name)
  Else
    DoValue(name, aValue);
end;

procedure TJSONWriter.ValueInArray(value: Int64);
begin
  valueInArray(inttostr(value));
end;

procedure TJSONWriter.ValueInArray(value: Double);
begin
  valueInArray(FloatToStr(value));
end;

procedure TJSONWriter.ValueInArray(value: Integer);
begin
  valueInArray(inttostr(value));
end;

class function TJSONWriter.writeObjectStr(obj: TJsonObject; pretty: boolean): String;
begin
  result := TEncoding.UTF8.GetString(writeObject(obj, pretty));
end;

class function TJSONWriter.writeObject(obj: TJsonObject; pretty: boolean): TBytes;
var
  mem : TBytesStream;
begin
  mem := TBytesStream.Create;
  try
    writeObject(mem, obj, pretty);
    result := mem.Bytes;
    SetLength(result, mem.size);
  finally
    mem.Free
  end;
end;

class procedure TJSONWriter.writeObject(stream: TStream; obj: TJsonObject; pretty: boolean);
var
  s : TFslVCLStream;
begin
  s := TFslVCLStream.Create;
  try
    s.Stream := stream;
    writeObject(s, obj, pretty);
  finally
    s.Free;
  end;
end;


procedure TJSONWriter.ValueBoolean(const name: String; avalue: Boolean);
begin
  if name = '' then
    valueInArray(avalue)
  else if avalue then
    doValue(name, 'true')
  else
    doValue(name, 'false');
end;


procedure TJSONWriter.ValueBytes(const name: String; bytes: TBytes);
begin

end;

procedure TJSONWriter.ValueDate(const name : String; aValue: TDateTime);
begin
  if aValue = 0 then
    ValueNull(Name)
  Else
    Value(name, FormatDateTime('yyyymmddhhnnss.zzz', aValue));
end;

function TJSONWriter.Link: TJSONWriter;
begin
  result := TJSONWriter(Inherited Link);
end;

procedure TJSONWriter.ValueDateInArray(aValue: TDateTime);
begin
  if aValue = 0 then
    ValueNullInArray
  Else
    ValueInArray(FormatDateTime('yyyymmddhhnnss.zzz', aValue));
end;

{ TJsonWriterDirect }

procedure TJsonWriterDirect.Start;
begin
  if not HasStream then
    Stream := TFslStringStream.create;
  if obj then
    ProduceLine('{');
  LevelDown;
end;

procedure TJsonWriterDirect.Finish;
begin
  if FCache <> '' Then
    ProduceLine(UseCache);
  LevelUp;
  Assert(Level = 0);
  if obj then
    ProduceLine('}');
end;

function TJsonWriterDirect.canInject: boolean;
begin
  result := true;
end;

procedure TJsonWriterDirect.DoName(const name : String);
begin
  if FCache <> '' Then
  begin
    ProduceLine(UseCache+',');
    FProperty := false;
  end
  else if FProperty then
  begin
    FProperty := false;
    ProduceLine(',')
  end;
  FName := JSONString(name)+' : ';
end;

procedure TJsonWriterDirect.FinishArray;
begin
  if FCache <> '' Then
    ProduceLine(UseCache);
  LevelUp;
  Assert(Level >= 0);
  FCache := ']';
end;

procedure TJsonWriterDirect.ValueArray(const name : String);
begin
  if name <> '' then
  begin
    DoName(name);
    ProduceLine(UseName + '[');
  end
  else
    ProduceLine('[');
  LevelDown;
end;

procedure TJsonWriterDirect.ValueObject(const name : String);
begin
  if (name = '') then
    ValueObject
  else
  begin
    DoName(name);
    ProduceLine(UseName+ '{');
    LevelDown;
  end;
end;

procedure TJsonWriterDirect.ValueObject;
begin
  if FCache <> '' Then
  begin
    ProduceLine(UseCache+',');
    FProperty := false;
  end
  else if FProperty then
  begin
    FProperty := false;
    ProduceLine(',')
  end;
  ProduceLine(UseName+ '{');
  LevelDown;
end;

procedure TJsonWriterDirect.ValueBytes(const name: String; bytes: TBytes);
begin
  if name = '' then
    raise EJsonException.Create('Injecting bytes not supported in an array');
  DoName(Name);
  produce(UseName);
  ProduceBytes(bytes);
  FProperty := true;
end;

procedure TJsonWriterDirect.doValue(name, value : String);
begin
  DoName(Name);
  FCache := UseName + value;
end;

procedure TJsonWriterDirect.ValueInArray(const value: String);
begin
  if FCache <> '' Then
  begin
    ProduceLine(UseCache+',');
    FProperty := false;
  end
  else if FProperty then
  begin
    FProperty := false;
    ProduceLine(',')
  end;
  if value = '' then
    ValueNullInArray
  Else
    FCache := JSONString(value);
end;

procedure TJsonWriterDirect.valueNullInArray;
begin
  if FCache <> '' Then
    ProduceLine(UseCache+',');
  FCache := 'null';
end;

procedure TJsonWriterDirect.FinishObject;
begin
  if FCache <> '' Then
    ProduceLine(UseCache);
  LevelUp;
  Assert(Level >= 0);
  FCache := '}';
end;

function TJsonWriterDirect.Link: TJsonWriterDirect;
begin
  result := TJsonWriterDirect(inherited Link);
end;

function TJsonWriterDirect.GetSourceLocation: TSourceLocation;
begin
  result := Location;
  if FCache <> '' then
    result := AdjustLocation(result, BeforeWhitespace + FCache + AfterWhitespace);
end;

function TJsonWriterDirect.UseName: String;
begin
  result := FName;
  FName := '';
end;

function TJsonWriterDirect.UseCache: String;
begin
  result := FCache;
  FCache := '';
end;

function TJsonWriterDirect.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FCache.length * sizeof(char)) + 12);
end;

{ TJSONLexer }

procedure TJSONLexer.Start;
var
  ch : char;
begin
  ch := getNextChar();
  if ch = char($EF) then
  begin
    // skip BOM
    getNextChar();
    getNextChar();
  end
  else
    push(ch);
  Next;
end;

procedure TJSONLexer.ParseWord(sWord : String; ch : Char; aType : TJSONLexType);
Begin
  FLexType := aType;
  FValue.Append(ch);
  While More and (FValue.Length < length(sWord)) and (FValue.ToString = copy(sWord, 1, FValue.Length)) Do
    FValue.Append(getNextChar);
  if FValue.ToString <> sWord Then
    JsonError('Syntax error in json reading special word '+sWord);
End;

procedure TJSONLexer.Next;
var
  ch : Char;
  hex : String;
  exp : boolean;
begin
  FLastLocationBWS := FPrevLocation;
  repeat
    ch := getNextChar;
    if (FLoose and (ch = '/') and (peekNextChar = '/')) then
    begin
      repeat
        ch := getNextChar()
      until CharInSet(ch, [#13, #10]);
    end;
  Until Not More Or not (CharInSet(ch, [' ', #13, #10, #9]) or (FLoose and (ch = ',')));
  FLastLocationAWS := FPrevLocation;

  FValue.Clear;
  If (CharInSet(ch, [#0, ' ', #13, #10, #9]) and Not More) Then
    FLexType := jltEof
  Else case ch of
    '{' : FLexType := jltOpen;
    '}' : FLexType := jltClose;
    '"' :
      Begin
      FLexType := jltString;
      repeat
        ch := getNextChar;
        if (ch = '\') Then
        Begin
          if not More then
            JsonError('premature termination of json stream during a string');
          ch := getNextChar;
          case ch of
            '"':FValue.append('"');
            '''':FValue.append('''');
            '\':FValue.append('\');
            '/':FValue.append('/');
            'n':FValue.append(#10);
            'r':FValue.append(#13);
            't':FValue.append(#09);
            'u':
              begin
              setLength(hex, 4);
              hex[1] := getNextChar;
              hex[2] := getNextChar;
              hex[3] := getNextChar;
              hex[4] := getNextChar;
              FValue.append(chr(StrToInt('$'+hex)));
              end
          Else
            JsonError('not supported: \'+ch);
          End;
          ch := #0;
        End
        Else if (ch <> '"') then
          FValue.append(ch);
      until not More or (ch = '"');
      if ch <> '"' Then
        JsonError('premature termination of json stream during a string');
      End;
    ':' : FLexType := jltColon;
    ',' : FLexType := jltComma;
    '[' : FLexType := jltOpenArray;
    ']' : FLexType := jltCloseArray;
    't' : ParseWord('true', ch, jltBoolean);
    'f' : ParseWord('false', ch, jltBoolean);
    'n' : ParseWord('null', ch, jltNull);
    '0'..'9', '-' :
      Begin
      FLexType := jltNumber;
      FValue.Clear;
      exp := false;
      while More and (CharInSet(ch, ['0'..'9', '.', '+', '-']) or (not exp and (CharInSet(ch, ['0'..'9', '.', '-', 'e', 'E'])))) do
      Begin
        FValue.append(ch);
        if (ch = 'e') or (ch = 'E') then
        exp := true;
        ch := getNextChar;
      End;
      push(ch);
      End;
  Else
    JsonError('Unexpected char "'+ch+'" in json stream');
  End;
end;

function TJSONLexer.getNextChar: Char;
begin
  if FPeek <> '' Then
  Begin
    result := FPeek[1];
    FLocation.incCol;
    FPrevLocation := FLocation;
    Delete(FPeek, 1, 1);
  End
  Else
  begin
    result := ConsumeCharacter;
    FPrevLocation := FLocation;
    if result = #10 then
      FLocation.incLine
    else
      FLocation.incCol;
  end;
end;

function TJSONLexer.GetValue: String;
begin
  result := string(FValue.ToString);
end;

function TJSONLexer.Consume(aType: TJsonLexType): String;
begin
  if FLexType <> aType Then
    JsonError('JSON syntax error - found '+Codes_TJSONLexType[FLexType]+' expecting '+Codes_TJSONLexType[aType]);
  result := Value;
  Next;
end;

procedure TJSONLexer.Push(ch: Char);
begin
  if (FLocation.col > 0) then
    dec(FLocation.col);
  insert(ch, FPeek, 1);
end;

constructor TJSONLexer.Create(oStream: TFslStream; loose : boolean);
begin
  Inherited Create(oStream);
  FLoose := loose;
  FLocation := TSourceLocation.Create;
  FPrevLocation := FLocation;
  FStates := TStringList.Create;
  FValue := TStringBuilder.create;
end;

destructor TJSONLexer.Destroy;
begin
  FStates.Free;
  FValue.free;
  inherited;
end;

procedure TJSONLexer.JsonError(sMsg: String);
begin
  Raise EJsonParserException.Create('Error parsing JSON source: '+sMsg+' at Line '+inttostr(Line)+' (path=['+Path+'])', FLocation);
end;

function TJSONLexer.Path: String;
var
  i : integer;
begin
  if FStates.count = 0 then
    result := Value
  else
  begin
    result := '';
    for i := FStates.count-1 downto 1 do
      result := result + '/'+FStates[i];
    result := result + Value;
  end;
end;

function TJSONLexer.peekNextChar: Char;
begin
  if FPeek <> '' Then
    result := FPeek[1]
  Else
  begin
    result := ConsumeCharacter;
    if result = #10 then
      FLocation.incLine
    else
      FLocation.incCol;
    push(result);
  end;
end;

function TJSONLexer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPeek.length * sizeof(char)) + 12);
  inc(result, FStates.sizeInBytes);
end;

{ TJSONParser }

constructor TJSONParser.Create(oStream: TStream; loose : boolean);
var
  oVCLStream : TFslVclStream;
begin
  inherited Create;
  oVCLStream := TFslVCLStream.Create;
  Try
    oVCLStream.Stream := oStream;
    FLex := TJSONLexer.Create(oVCLStream.Link, loose);
  Finally
    oVCLStream.Free;
  End;
  FLex.Start;
end;

procedure TJSONParser.CheckState(aState: TJsonParserItemType);
begin
  if FItemType <> aState Then
    JsonError('Unexpected state. Expected '+Codes_TJsonParserItemType[aState]+', but found '+Codes_TJsonParserItemType[FItemType]);
end;

constructor TJSONParser.Create(oStream: TFslStream; loose : boolean);
begin
  inherited Create;
  FLex := TJSONLexer.Create(oStream.Link, loose);
  FLex.Start;
end;

function TJSONParser.GetItemNull: boolean;
begin
  result := false;
end;

function TJSONParser.GetItemValue: String;
begin
  if not (FItemType in [jpitBoolean, jpitString, jpitNumber]) Then
    FLex.JSONError('Attempt to read a simple value, but state is '+Codes_TJsonParserItemType[FItemType]);
  result := FItemValue;
end;

procedure TJSONParser.JsonError(sMsg: String);
begin
  FLex.JsonError(sMsg);
end;

procedure TJSONParser.Next;
begin
//  if (FTimeToAbort > 0) and (FTimeToAbort < GetTickCount) then
//    abort;

  case FItemType of
    jpitObject :
      Begin
      FLex.Consume(jltOpen);
      FLex.FStates.InsertObject(0, ItemName, nil);
      if FLex.LexType = jltClose then
      begin
        FItemType := jpitEnd;
        FLex.Next;
      end
      else
        ParseProperty;
      End;
    jpitNull, jpitString, jpitNumber, jpitEnd, jpitBoolean :
      Begin
      if FItemType = jpitEnd Then
        FLex.FStates.Delete(0);
      if FLex.LexType = jltComma then
      Begin
        FLex.Next;
        ParseProperty;
      End
      Else if FLex.Floose and (FLex.LexType = jltString) then
      begin
        ParseProperty;
      end
      Else if FLex.LexType = jltClose Then
      Begin
        FItemType := jpitEnd;
        FLex.Next;
      End
      Else if FLex.LexType = jltCloseArray Then
      Begin
        FItemType := jpitEnd;
        FLex.Next;
      End
      Else if FLEx.LexType = jltEof then
        FItemType := jpitEof
      Else
        FLex.JsonError('JSON Syntax Error');
      End;
    jpitArray :
      Begin
      FLex.next;
      FLex.FStates.InsertObject(0, ItemName+'[]', Self);
      if FLex.LexType = jltCloseArray Then
      Begin
        FItemType := jpitEnd;
        FLex.Next;
      End
      else
        ParseProperty;
      End;
    jpitEof :
        FLex.JsonError('JSON Syntax Error - attempt to read past end of json stream');
  else
    FLex.JsonError('error (a): '+Codes_TJsonParserItemType[ItemType]);
  End;
end;

function TJSONParser.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FLex.sizeInBytes);
  inc(result, (FItemName.length * sizeof(char)) + 12);
  inc(result, (FItemValue.length * sizeof(char)) + 12);
end;

class function TJSONParser.Parse(stream: TFslStream; timeToAbort : cardinal = 0; loose : boolean = false): TJsonObject;
var
  p : TJSONParser;
begin
  p := TJSONParser.Create(stream, loose);
  try
    p.FtimeToAbort := timeToAbort;
    result := TJsonObject.Create('$');
    try
      result.LocationStart := p.FLex.FLastLocationBWS;
      if p.FLex.LexType = jltOpen Then
      begin
        p.FLex.Next;
        p.FLex.FStates.InsertObject(0, '', nil);
      End
      Else
        p.FLex.JsonError('Unexpected content at start of JSON: '+Codes_TJSONLexType[p.FLex.LexType]);

      p.readObject(result, true);
      result.Link;
    finally
      result.Free;
    end;
  finally
    p.Free;
  end;
end;

class function TJSONParser.Parse(stream: TStream; timeToAbort : cardinal = 0; loose : boolean = false): TJsonObject;
var
  p : TJSONParser;
begin
  p := TJSONParser.Create(stream, loose);
  try
    p.FtimeToAbort := timeToAbort;
    result := TJsonObject.Create('$');
    try
      result.LocationStart := p.FLex.FLastLocationBWS;
      if p.FLex.LexType = jltOpen Then
      begin
        p.FLex.Next;
        p.FLex.FStates.InsertObject(0, '', nil);
      End
      Else
        p.FLex.JsonError('Unexpected content at start of JSON: '+Codes_TJSONLexType[p.FLex.LexType]);
      p.ParseProperty;
      p.readObject(result, true);
      result.Link;
    finally
      result.Free;
    end;
  finally
    p.Free;
  end;
end;

class function TJSONParser.Parse(s: String; timeToAbort : cardinal = 0; loose : boolean = false): TJsonObject;
begin
  result := Parse(TEncoding.UTF8.GetBytes(s), timeToAbort);
end;

class function TJSONParser.ParseFile(fn: String; loose : boolean = false): TJsonObject;
var
  f : TFileStream;
begin
  f := TFileStream.Create(fn, fmOpenRead + fmShareDenyWrite);
  try
    result := parse(f, 0, loose);
  finally
    f.Free;
  end;
end;

class function TJSONParser.ParseNode(stream: TFslStream; timeToAbort : cardinal = 0; loose : boolean = false): TJsonNode;
var
  p : TJSONParser;
begin
  p := TJSONParser.Create(stream, loose);
  try
    p.FtimeToAbort := timeToAbort;
    result := p.readNode;
  finally
    p.Free;
  end;
end;

class function TJSONParser.ParseNode(stream: TStream; timeToAbort : cardinal = 0; loose : boolean = false): TJsonNode;
var
  p : TJSONParser;
begin
  p := TJSONParser.Create(stream, loose);
  try
    p.FtimeToAbort := timeToAbort;
    result := p.readNode;
  finally
    p.Free;
  end;
end;

class function TJSONParser.ParseNode(s: String; timeToAbort : cardinal = 0; loose : boolean = false): TJsonNode;
begin
  result := ParseNode(TEncoding.UTF8.GetBytes(s), timeToAbort, loose);
end;

class function TJSONParser.ParseNode(b: TBytes; timeToAbort : cardinal = 0; loose : boolean = false): TJsonNode;
var
  s : TBytesStream;
begin
  s := TBytesStream.Create(b);
  try
    result := ParseNode(s, timeToAbort, loose);
  finally
    s.Free;
  end;
end;

procedure TJSONParser.ParseProperty;
Begin
  If FLex.FStates.Objects[0] = nil Then
  Begin
    FNameStart := FLex.FLastLocationAWS;
    FItemName := FLex.Consume(jltString);
    FNameEnd := FLex.FLocation;
    FItemValue := '';
    FLex.Consume(jltColon);
  End;
  case FLex.LexType of
    jltNull :
      Begin
      FItemType := jpitNull;
      FItemValue := FLex.Value;
      FValueStart := FLex.FLastLocationAWS;
      FValueEnd := FLex.FLocation;
      FLex.Next;
      end;
    jltString :
      Begin
      FItemType := jpitString;
      FItemValue := FLex.Value;
      FValueStart := FLex.FLastLocationAWS;
      FValueEnd := FLex.FLocation;
      FLex.Next;
      End;
    jltBoolean :
      Begin
      FItemType := jpitBoolean;
      FItemValue := FLex.Value;
      FValueStart := FLex.FLastLocationAWS;
      FValueEnd := FLex.FLocation;
      FLex.Next;
      End;
    jltNumber :
      Begin
      FItemType := jpitNumber;
      FItemValue := FLex.Value;
      FValueStart := FLex.FLastLocationAWS;
      FValueEnd := FLex.FLocation;
      FLex.Next;
      End;
    jltOpen :
      Begin
      FItemType := jpitObject;
      End;
    jltOpenArray :
      Begin
      FItemType := jpitArray;
      End;
    jltCloseArray :
      begin
      FItemType := jpitEnd;
      End;
    // jltClose, , jltColon, jltComma, jltOpenArray,       !
  else
    FLex.JsonError('error (b): '+Codes_TJSONLexType[FLex.LexType]);
  End;
End;


procedure TJSONParser.readArray(arr: TJsonArray; root : boolean);
var
  obj : TJsonObject;
  child : TJsonArray;
  i : integer;
begin
  i := 0;
  while not ((ItemType = jpitEnd) or (root and (ItemType = jpitEof))) do
  begin
    case ItemType of
      jpitObject:
        begin
          obj := TJsonObject.Create(arr.path+'['+inttostr(i)+']');
          arr.FItems.Add(obj);
          obj.LocationStart := FLex.FPrevLocation;
          Next;
          readObject(obj, false);
        end;
      jpitString:
        arr.FItems.Add(TJsonString.Create(arr.path+'['+inttostr(i)+']', FValueStart, FValueEnd, ItemValue));
      jpitNumber:
        arr.FItems.Add(TJsonNumber.Create(arr.path+'['+inttostr(i)+']', FValueStart, FValueEnd, ItemValue));
      jpitNull :
        arr.FItems.Add(TJsonNull.Create(arr.path+'['+inttostr(i)+']', FValueStart, FValueEnd));
      jpitArray:
        begin
        child := TJsonArray.Create(arr.path+'['+inttostr(i)+']');
        arr.FItems.Add(child);
        child.LocationStart := FLex.FPrevLocation;
        Next;
        readArray(child, false);
        end;
      jpitEof : raise EJsonParserException.Create('Unexpected End of File', FLex.FLocation);
    end;
    arr.LocationEnd := FLex.FLocation;
    Next;
    inc(i);
  end;
end;

function TJSONParser.readNode: TJsonNode;
begin
  case FLex.LexType of
    jltOpen :
      begin
        result := TJsonObject.Create('$');
        try
          result.LocationStart := FLex.FLastLocationBWS;
          Next;
          readObject(result as TJsonObject, true);
          result.link;
        finally
          result.Free;
        end;
      end;
    jltString : raise EJsonTodo.Create('Not implemented yet');
    jltNumber : raise EJsonTodo.Create('Not implemented yet');
    jltOpenArray :
      begin
        FItemType := jpitArray;
        result := TJsonArray.Create('$');
        try
          FLex.FStates.InsertObject(0, '', result);
          result.LocationStart := FLex.FLastLocationBWS;
          Next;
          readArray(result as TJsonArray, true);
          result.link;
        finally
          result.Free;
        end;
      end;
    jltNull : raise EJsonTodo.Create('Not implemented yet');
    jltBoolean : raise EJsonTodo.Create('Not implemented yet');
  else
    raise EJsonParserException.Create('Unexpected Token '+Codes_TJSONLexType[FLex.LexType]+' at start of Json Stream', FLex.FLocation);
  end;
end;

procedure TJSONParser.readObject(obj: TJsonObject; root : boolean);
var
  child : TJsonObject;
  arr : TJsonArray;
  ns : TSourceLocation;
begin
  obj.LocationInner := FLex.FLocation;

  while not ((ItemType = jpitEnd) or (root and (ItemType = jpitEof))) do
  begin
    ns := FNameStart; // we rule that the value 'starts' in a location sense where the name starts, not where the value starts
    if obj.FProperties.ContainsKey(itemName) then
    begin
      if FLex.FLoose then
        obj.FProperties.remove(itemName)
      else
        raise EJsonParserException.Create('DuplicateKey: '+itemName+' at '+obj.path, FLex.FLocation);
    end;

    case ItemType of
      jpitObject:
        begin
          child := TJsonObject.Create(obj.path+'.'+ItemName);
          obj.FProperties.Add(ItemName, child);
          child.LocationStart := FNameStart;
          Next;
          readObject(child, false);
        end;
      jpitBoolean :
        obj.FProperties.Add(ItemName, TJsonBoolean.Create(obj.path+'.'+ItemName, FValueStart, FValueEnd, StrToBool(ItemValue)).setStart(ns));
      jpitString:
        obj.FProperties.Add(ItemName, TJsonString.Create(obj.path+'.'+ItemName, FValueStart, FValueEnd, ItemValue).setStart(ns));
      jpitNumber:
        obj.FProperties.Add(ItemName, TJsonNumber.Create(obj.path+'.'+ItemName, FValueStart, FValueEnd, ItemValue).setStart(ns));
      jpitNull:
        obj.FProperties.Add(ItemName, TJsonNull.Create(obj.path+'.'+ItemName, FValueStart, FValueEnd).setStart(ns));
      jpitArray:
        begin
        arr := TJsonArray.Create(obj.path+'.'+ItemName);
        obj.FProperties.Add(ItemName, arr);
        arr.LocationStart := ns;
        Next;
        readArray(arr, false);
        end;
      jpitEof : raise EJsonParserException.Create('Unexpected End of File', FLex.FLocation);
    end;
    obj.LocationEnd := FLex.FLocation;
    next;
  end;
end;

procedure TJSONParser.Skip;
begin
  if ItemType in [jpitString, jpitNumber] then
    Next
  Else
    SkipInner;
end;

procedure TJSONParser.SkipInner;
begin
  Next;
  While ItemType <> jpitEnd do
  Begin
    Case ItemType of
      jpitObject : SkipInner;
      jpitString : Next;
      jpitNumber : Next;
      jpitArray : SkipInner;
    End;
  End;
  Next;
end;



destructor TJSONParser.Destroy;
begin
  FLex.free;
  inherited;
end;


class function TJSONParser.Parse(b: TBytes; timeToAbort : cardinal = 0; loose : boolean = false): TJsonObject;
var
  s : TBytesStream;
begin
  s := TBytesStream.Create(b);
  try
    result := Parse(s, timeToAbort, loose);
  finally
    s.Free;
  end;
end;

{ TJsonNode }

constructor TJsonNode.Create(path: String);
begin
  Create;
  self.path := path;
end;

class function TJsonNode.compare(n1, n2: TJsonNode): boolean;
begin
  if (n1 = nil)  and (n2 = nil) then
    exit(true);
  if (n1 = nil) or (n2 = nil) then
    exit(false);

  result := n1.compare(n2);
end;

function TJsonNode.findLocation(loc: TSourceLocation): TFslList<TJsonPointerMatch>;
begin
  result := TFslList<TJsonPointerMatch>.create;
  try
    findLocation(loc, '$', result);
    result.Link;
  finally
    result.Free;
  end;
end;

function TJsonNode.describePath(path: TFslList<TJsonPointerMatch>): string;
var
  p : TJsonPointerMatch;
begin
  result := '';
  for p in path do
  begin
    if result = '' then
      result := p.name
    else
      result := result + '.' + p.name;
  end;
end;

constructor TJsonNode.Create(path: String; locStart, locEnd: TSourceLocation);
begin
  Create;
  self.path := path;
  LocationStart := locStart;
  LocationEnd := locEnd;
end;

function TJsonNode.evaluatePointer(path: String): TJsonNode;
begin
  result := nil;
end;

function TJsonNode.findLocation(loc: TSourceLocation; name : String; path: TFslList<TJsonPointerMatch>) : boolean;
begin
  result := loc.inSpan(LocationStart, LocationEnd);
  if result then
    path.add(TJsonPointerMatch.create(name, self.link));
end;

function TJsonNode.Link: TJsonNode;
begin
  result := TJsonNode(Inherited Link);
end;

function TJsonNode.nodeType: String;
begin
  result := copy(className, 6, $FF);
end;


function TJsonNode.setStart(loc: TSourceLocation): TJsonNode;
begin
  LocationStart := loc;
  result := self;
end;

{ TJsonArray }

function TJsonArray.add(value: String): TJsonArray;
begin
  FItems.Add(TJsonString.Create(path+'/'+inttostr(FItems.count), value));
  result := self;
end;

function TJsonArray.add(value: TJsonNode): TJsonArray;
begin
  FItems.Add(value);
  result := self;
end;

function TJsonArray.addObject: TJsonObject;
begin
  result := TJsonObject.Create;
  add(result);
end;

function TJsonArray.asObjects: TFslList<TJsonObject>;
var
  i : integer;
begin
  result := TFslList<TJsonObject>.create;
  try
    for I := 0 to count - 1 do
      result.Add(obj[i].Link);
    result.link;
  finally
    result.Free;
  end;
end;

procedure TJsonArray.clear;
begin
  FItems.Clear;
end;

function TJsonArray.compare(other: TJsonNode): boolean;
var
  o : TJsonArray;
  i : integer;
begin
  if not (other is TJsonArray) then
    exit(false);

  o := other as TJsonArray;
  if Count <> o.Count then
    exit(false);

  for i := 0 to Count -1 do
    if not Item[i].compare(o.Item[i]) then
      exit(false);

  result := true;
end;

constructor TJsonArray.Create;
begin
  inherited Create;
  FItems := TFslObjectList.Create;
end;

destructor TJsonArray.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TJsonArray.evaluatePointer(path: String): TJsonNode;
begin
  if StringIsInteger32(path) then
    result := GetItem(StrToInt(path))
  else
    result := nil;
end;

function TJsonArray.findLocation(loc: TSourceLocation; name: String; path: TFslList<TJsonPointerMatch>): boolean;
var
  i : integer;
begin
  Result := inherited findLocation(loc, name, path);
  if result then
  begin
    for i := 0 to FItems.count - 1 do
    begin
      if (FItems[i] as TJsonNode).findLocation(loc, '['+inttostr(i)+']', path) then
        exit;
    end;
  end;
end;

function TJsonArray.GetCount: integer;
begin
  if self = nil then
    result := 0
  else
    result := FItems.Count;
end;

function TJsonArray.GetEnumerator: TJsonArrayEnumerator;
begin
  result := TJsonArrayEnumerator.Create;
  result.FArray := self.Link;
  result.cursor := -1;
end;

function TJsonArray.GetItem(i: integer): TJsonNode;
begin
  if (self = nil) or (i >= Count) then
    result := nil
  else
    result := FItems[i] as TJsonNode;
end;

function TJsonArray.GetObj(i: integer): TJsonObject;
begin
  if (self = nil) or (i >= Count) then
    result := nil
  else if FItems[i] is TJsonObject then
    result := FItems[i] as TJsonObject
  else if FItems[i] is TJsonNull then
    result := nil
  else
    raise EJsonException.Create('Found a property of type '+TJsonNode(FItems[i]).nodeType+' looking for an object at '+path+'['+inttostr(i)+']');
end;

function TJsonArray.GetValue(i: integer): String;
begin
  if (self = nil) or (i >= Count)  then
    result := ''
  else if FItems[i] is TJsonString then
    result := (FItems[i] as TJsonString).FValue
  else if FItems[i] is TJsonNumber then
    result := (FItems[i] as TJsonNumber).FValue
  else if FItems[i] is TJsonNull then
    result := ''
  else
    raise EJsonException.Create('Found a '+nodeType+' expecting a string property at '+path);
end;

function TJsonArray.kind: TJsonNodeKind;
begin
  result := jnkArray;
end;

function TJsonArray.Link: TJsonArray;
begin
  result := TJsonArray(Inherited Link);
end;

procedure TJsonArray.move(index, delta: integer);
begin
  FItems.Move(index, index+delta);
end;

function TJsonArray.nodeType: String;
begin
  result := 'array';
end;

procedure TJsonArray.remove(index: integer);
begin
  FItems.DeleteByIndex(index);
end;

procedure TJsonArray.SetItem(i: integer; const Value: TJsonNode);
begin
  FItems[i] := Value;
end;

procedure TJsonArray.SetObj(i: integer; const Value: TJsonObject);
begin
  FItems[i] := Value;
end;

procedure TJsonArray.SetValue(i: integer; const Value: String);
begin
  FItems[i] := TJsonString.Create(Path+'['+inttostr(i)+']', Value);
end;

function TJsonArray.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FItems.sizeInBytes);
end;

{ TJsonString }

constructor TJsonString.Create(path, value: string);
begin
  Create(path);
  self.value := value;
end;

function TJsonString.compare(other: TJsonNode): boolean;
begin
  if not (other is TJsonString) then
    result := false
  else
    result := FValue = (other as TJsonString).FValue;
end;

constructor TJsonString.Create(path: String; locStart, locEnd: TSourceLocation; value: string);
begin
  Create(path);
  self.value := value;
  LocationStart := locStart;
  LocationEnd := locEnd;
end;

function TJsonString.kind: TJsonNodeKind;
begin
  result := jnkString;
end;

function TJsonString.Link: TJsonString;
begin
  result := TJsonString(Inherited Link);
end;

function TJsonString.nodeType: String;
begin
  result := 'string';
end;

function TJsonString.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TJsonNumber }

constructor TJsonNumber.Create(path, value: string);
begin
  Create(path);
  self.value := value;
end;

function TJsonNumber.compare(other: TJsonNode): boolean;
begin
  if not (other is TJsonNumber) then
    result := false
  else
    result := FValue = (other as TJsonNumber).FValue;
end;

constructor TJsonNumber.Create(path: String; locStart, locEnd: TSourceLocation; value: string);
begin
  Create(path);
  self.value := value;
  LocationStart := locStart;
  LocationEnd := locEnd;
end;

function TJsonNumber.kind: TJsonNodeKind;
begin
  result := jnkNumber;
end;

function TJsonNumber.Link: TJsonNumber;
begin
  result := TJsonNumber(Inherited Link);
end;

function TJsonNumber.nodeType: String;
begin
  result := 'number';
end;

function TJsonNumber.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TJsonObject }

procedure TJsonObject.clear(name: String);
begin
  if name = '' then
    FProperties.Clear
  else
    FProperties.Remove(name);
end;

function TJsonObject.compare(other: TJsonNode): boolean;
var
  o : TJsonObject;
  s : String;
begin
  if not (other is TJsonObject) then
    exit(false);

  o := other as TJsonObject;
  if properties.Count <> o.properties.Count then
    exit(false);

  for s in properties.Keys do
    if not TJsonNode.compare(properties[s], o.properties[s]) then
      exit(false);

  result := true;
end;

constructor TJsonObject.Create;
begin
  inherited Create;
  FProperties := TFslMap<TJsonNode>.Create('Json Properties');
  FProperties.trackOrder;
end;

destructor TJsonObject.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TJsonObject.evaluatePointer(path: String): TJsonNode;
begin
  if has(path) then
    result := properties[path]
  else
    result := nil;
end;

function TJsonObject.findLocation(loc: TSourceLocation; name: String; path: TFslList<TJsonPointerMatch>): boolean;
var
  s : String;
begin
  result := inherited findLocation(loc, name, path);
  if result then
  begin
    for s in FProperties.Keys do
      if FProperties[s].findLocation(loc, s, path) then
        exit;
  end;
end;

function TJsonObject.GetArray(name: String): TJsonArray;
var
  node : TJsonNode;
begin
  if has(name) then
  begin
    node := FProperties[name];
    if node is TJsonArray then
      result := TJsonArray(node)
    else if node is TJsonNull then
      result := nil
    else
      raise EJsonException.Create('Found a property of '+node.nodeType+' looking for an array at '+path+'.'+name);
  end
  else
    result := nil;
end;

function TJsonObject.GetBool(name: String): boolean;
var
  node : TJsonNode;
begin
  if has(name) then
  begin
    node := FProperties[name];
    if node is TJsonNull then
      result := false
    else if node is TJsonBoolean then
      result := (node as TJsonBoolean).FValue
    else
      raise EJsonException.Create('Found a property of type '+node.nodeType+' looking for a boolean at '+path+'.'+name);
  end
  else
    result := false;
end;

function TJsonObject.GetForcedArray(name: String): TJsonArray;
begin
  if not properties.containsKey(name) or not (properties[name] is TJsonArray) then
    arr[name] := TJsonArray.Create;
  result := arr[name];
end;

function TJsonObject.GetForcedObject(name: String): TJsonObject;
begin
  if not properties.containsKey(name) or not (properties[name] is TJsonObject) then
    obj[name] := TJsonObject.Create;
  result := obj[name];
end;

function TJsonObject.GetInteger(name: String): Integer;
var
  n : TJsonNode;
  s : String;
begin
  n := GetNode(name);
  result := 0;
  if (n <> nil) then
  begin
    if (n is TJsonBoolean) then
    begin
      if (n as TJsonBoolean).value then
        result := 1
      else
        result := 0;
    end
    else if (n is TJsonNumber) then
    begin
      s := (n as TJsonNumber).FValue;
      result := StrToIntDef(s, 0);
    end
    else if (n is TJsonString) then
    begin
      s := (n as TJsonString).FValue;
      result := StrToIntDef(s, 0);
    end;
  end;
end;

function TJsonObject.GetNode(name: String): TJsonNode;
begin
  if has(name) then
    result := FProperties[name]
  else
    result := nil;
end;

function TJsonObject.GetNumber(name: String): String;
var
  node : TJsonNode;
begin
  if self = nil then
    result := ''
  else
  begin
    if has(name) then
    begin
      node := FProperties[name];
      if (node is TJsonString) and StringIsInteger32(TJsonString(node).FValue) then
        result := TJsonString(node).FValue
      else if node is TJsonNumber then
        result := TJsonNumber(node).FValue
      else if node is TJsonNull then
        result := ''
      else if node is TJsonBoolean then
        if (node as TJsonBoolean).FValue then
          result := '1'
        else
          result := '0'
      else
        raise EJsonException.Create('Found a property of type '+node.nodeType+' looking for a string at '+FPath+'.'+name);
    end
    else
      result := '';
  end;
end;

function TJsonObject.GetObject(name: String): TJsonObject;
var
  node : TJsonNode;
begin
  if has(name) then
  begin
    node := FProperties[name];
    if node is TJsonObject then
      result := TJsonObject(node)
    else if node is TJsonNull then
      result := nil
    else
      raise EJsonException.Create('Found a property of type '+node.nodeType+' looking for an object at '+FPath+'.'+name);
  end
  else
    result := nil;
end;

function TJsonObject.GetString(name: String): String;
var
  node : TJsonNode;
begin
  if self = nil then
    result := ''
  else
  begin
    if has(name) then
    begin
      node := FProperties[name];
      if node is TJsonString then
        result := TJsonString(node).FValue
      else if node is TJsonNumber then
        result := TJsonNumber(node).FValue
      else if node is TJsonNull then
        result := ''
      else if node is TJsonBoolean then
        if (node as TJsonBoolean).FValue then
          result := 'true'
        else
          result := 'false'
      else
        raise EJsonException.Create('Found a property of type '+node.nodeType+' looking for a string at '+FPath+'.'+name);
    end
    else
      result := '';
  end;
end;

function TJsonObject.has(name: String): Boolean;
begin
  result := FProperties.containsKey(name);
end;

function TJsonObject.isNull(name: String): Boolean;
begin
  result := has(name) and (FProperties[name] is TJsonNull);
end;

function TJsonObject.kind: TJsonNodeKind;
begin
  result := jnkObject;
end;

function TJsonObject.Link: TJsonObject;
begin
  result := TJsonObject(Inherited Link);
end;

function TJsonObject.nodeType: String;
begin
  result := 'object';
end;

procedure TJsonObject.SetArray(name: String; const Value: TJsonArray);
begin
  properties.AddOrSetValue(name, value);
end;

procedure TJsonObject.SetBool(name: String; const Value: boolean);
var
  v : TJsonBoolean;
begin
  v := TJsonBoolean.Create(path+'/'+name, Value);
  try
    properties.AddOrSetValue(name, v.Link);
  finally
    v.Free;
  end;
end;

procedure TJsonObject.SetInteger(name: String; const Value: Integer);
var
  v : TJsonNumber;
begin
  v := TJsonNumber.Create(path+'/'+name, IntToStr(Value));
  try
    properties.AddOrSetValue(name, v.Link);
  finally
    v.Free;
  end;
end;

procedure TJsonObject.setNode(name: String; const Value: TJsonNode);
begin
  properties.AddOrSetValue(name, value);
end;

procedure TJsonObject.SetObject(name: String; const Value: TJsonObject);
begin
  properties.AddOrSetValue(name, value);
end;

procedure TJsonObject.SetString(name: String; const Value: String);
var
  v : TJsonString;
begin
  v := TJsonString.Create(path+'/'+name, Value);
  try
    properties.AddOrSetValue(name, v.Link);
  finally
    v.Free;
  end;
end;

function TJsonObject.str2(n1, n2: String): String;
begin
  if has(n1) then
    result := str[n1]
  else
    result := str[n2];
end;

procedure TJsonObject.SetNumber(name: String; const Value: String);
var
  v : TJsonNumber;
begin
  v := TJsonNumber.Create(path+'/'+name, Value);
  try
    properties.AddOrSetValue(name, v.Link);
  finally
    v.Free;
  end;
end;

function TJsonObject.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FProperties.sizeInBytes);
end;

{ TJsonBoolean }

constructor TJsonBoolean.Create(path: String; value: boolean);
begin
  create('path');
  FValue := value;
end;

function TJsonBoolean.compare(other: TJsonNode): boolean;
begin
  if not (other is TJsonBoolean) then
    result := false
  else
    result := FValue = (other as TJsonBoolean).FValue;
end;

constructor TJsonBoolean.Create(path: String; locStart, locEnd: TSourceLocation; value: boolean);
begin
  create('path');
  FValue := value;
  LocationStart := locStart;
  LocationEnd := locEnd;
end;

function TJsonBoolean.kind: TJsonNodeKind;
begin
  result := jnkBoolean;
end;

function TJsonBoolean.Link: TJsonBoolean;
begin
  result := TJsonBoolean(inherited Link);
end;

function TJsonBoolean.nodeType: String;
begin
  result := 'boolean';
end;

function JsonBoolToString(b : boolean) : String;
begin
  if b then
    result := 'true'
  else
    result := 'false';

end;

function JsonStringToBool(s : String; def : boolean = false) : boolean;
begin
  if SameText(s, 'true') then
    result := true
  else if SameText(s, 'false') then
    result := false
  else if SameText(s, '0') then
    result := false
  else if SameText(s, 'no') then
    result := false
  else if SameText(s, '') then
    result := false
  else
    result := def;
end;

function TJsonBoolean.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TJsonArrayEnumerator }

function TJsonArrayEnumerator.GetCurrent: TJsonNode;
begin
  result := FArray.GetItem(cursor);
end;

destructor TJsonArrayEnumerator.Destroy;
begin
  FArray.Free;
  inherited;
end;

function TJsonArrayEnumerator.MoveNext: boolean;
begin
  inc(cursor);
  result := cursor < FArray.GetCount;
end;


function TJsonArrayEnumerator.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FArray.sizeInBytes);
end;

{ TJsonPatchEngine }

function TJsonPatchEngine.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPatch.sizeInBytes);
  inc(result, FTarget.sizeInBytes);
end;

class function TJsonPatchEngine.applyPatch(target: TJsonObject; patch: TJsonArray) : TJsonObject;
var
  this : TJsonPatchEngine;
begin
  this := TJsonPatchEngine.Create;
  try
    this.target := target.Link;
    this.patch := patch.Link;
    this.execute;
    result := this.target.link as TJsonObject;
  finally
    this.free;
  end;
end;

constructor TJsonPatchEngine.Create;
begin
  inherited;
end;

destructor TJsonPatchEngine.Destroy;
begin
  FPatch.free;
  FTarget.Free;
  inherited;
end;

procedure TJsonPatchEngine.SetPatch(const Value: TJsonArray);
begin
  FPatch.free;
  FPatch := Value;
end;

procedure TJsonPatchEngine.SetTarget(const Value: TJsonNode);
begin
  FTarget.Free;
  FTarget := Value;
end;

class procedure TJsonPatchEngine.runtest(test: TJsonObject);
var
  cmt : string;
  outcome : TJsonObject;
  ok : boolean;
begin
  cmt := test['comment'];
  writeln('test: '+cmt);
  if test.has('error') then
  begin
    ok := true;
    try
      applyPatch(test.obj['doc'], test.arr['patch']).Free;
      ok := false;
    except
    end;
    if not ok then
      raise EJsonException.Create('Test failed: '+cmt);
  end
  else
  begin
    outcome := applyPatch(test.obj['doc'], test.arr['patch']);
    try
      if not TJsonNode.compare(outcome, test.obj['expected']) then
        raise EJsonException.Create('Test failed: '+cmt);
    finally
      outcome.Free;
    end;
  end;
end;

class procedure TJsonPatchEngine.tests(fileName: String);
var
  tests : TJsonArray;
  test : TJsonNode;
begin
  tests := TJSONParser.ParseNode(FileToBytes(filename)) as TJsonArray;
  try
    for test in tests do
      runtest(test as TJsonObject);
  finally
    tests.free;
  end;
end;

procedure TJsonPatchEngine.execute;
var
  op : TJsonNode;
begin
  assert(target <> nil);
  assert(patch <> nil);
  for op in patch do
    if op is TJsonObject then
      applyPatchOperation(op as TJsonObject)
    else
      raise EJsonException.Create('Unexpected JSON node type looking for operation: '+op.nodeType);
end;

procedure TJsonPatchEngine.applyPatchOperation(patchOp: TJsonObject);
var
  op, path : String;
begin
  op := patchOp['op'];
  path := patchOp['path'];
  if path = '' then
    raise EJsonException.Create('No patch path parameter found');

  if op = '' then
    raise EJsonException.Create('No patch op parameter found')
  else if op = 'add' then
    applyAdd(patchOp, path)
  else if op = 'remove' then
    applyRemove(patchOp, path)
  else if op = 'replace' then
    applyReplace(patchOp, path)
  else if op = 'move' then
    applyMove(patchOp, path)
  else if op = 'copy' then
    applyCopy(patchOp, path)
  else if op = 'test' then
    applyTest(patchOp, path)
  else
    raise EJsonException.Create('Unknown patch operation "'+op+'"');
end;

procedure TJsonPatchEngine.applyAdd(patchOp: TJsonObject; path : String);
var
  value : TJsonNode;
begin
  value := patchOp.properties['value'];
  if value = nil then
    raise EJsonException.Create('No patch value parameter found in add');
  applyAddInner(path, value);
end;

procedure TJsonPatchEngine.applyAddInner(path : String; value : TJsonNode);
var
  query : TJsonPointerQuery;
begin
  query := TJsonPointerQuery.create;
  try
    query.execute(target, path, true);
    case query.terminalState of
      tsNotFound :
        if (query.secondLast is TJsonObject) then
          (query.secondLast as TJsonObject).properties.add(query.lastName, value.Link)
        else
          raise EJsonException.Create('Unexpected target type '+query.last.nodeType);
      tsFound :
        begin
        if (query.secondLast is TJsonArray) and StringIsInteger32(query.lastName) then
          (query.secondLast as TJsonArray).FItems.Insert((query.secondLast as TJsonArray).FItems.IndexByReference(query.last), value.link)
        else if query.secondlast is TJsonObject then
          (query.secondlast as TJsonObject).FProperties.AddOrSetValue(query.lastName, value.link)
        else
          raise EJsonException.Create('Unexpected target type '+query.last.nodeType);
        end;
      tsAtEnd :
        begin
        if (query.secondlast is TJsonArray) then
          (query.secondlast as TJsonArray).add(value.Link)
        else
          raise EJsonException.Create('Attempt to append content to a non-array ('+query.last.nodeType+')');
        end;
    end;
  finally
    query.free;
  end;
end;

procedure TJsonPatchEngine.applyRemove(patchOp: TJsonObject; path : String);
var
  query : TJsonPointerQuery;
begin
  query := TJsonPointerQuery.create;
  try
    query.execute(target, path, false);
    if (query.secondLast is TJsonArray) and StringIsInteger32(query.lastName) then
      (query.secondLast as TJsonArray).FItems.DeleteByIndex((query.secondLast as TJsonArray).FItems.IndexByReference(query.last))
    else if query.secondLast is TJsonObject then
      (query.secondLast as TJsonObject).FProperties.Remove(query.lastName)
    else
      raise EJsonException.Create('Unexpected target type '+query.last.nodeType);
  finally
    query.free;
  end;
end;

procedure TJsonPatchEngine.applyReplace(patchOp: TJsonObject; path : String);
var
  value : TJsonNode;
  query : TJsonPointerQuery;
begin
  value := patchOp.properties['value'];
  if value = nil then
    raise EJsonException.Create('No patch value parameter found in add');
  query := TJsonPointerQuery.create;
  try
    query.execute(target, path, false);
    if (query.secondLast is TJsonArray) and StringIsInteger32(query.lastName) then
      (query.secondLast as TJsonArray).FItems[(query.secondLast as TJsonArray).FItems.IndexByReference(query.last)] := value.link
    else if query.secondLast is TJsonObject then
      (query.secondLast as TJsonObject).FProperties[query.lastName] := value.link
    else
      raise EJsonException.Create('Unexpected target type '+query.last.nodeType);
  finally
    query.free;
  end;
end;

procedure TJsonPatchEngine.applyTest(patchOp: TJsonObject; path : String);
var
  query : TJsonPointerQuery;
  value : TJsonNode;
begin
  value := patchOp.properties['value'];
  if value = nil then
    raise EJsonException.Create('No patch value parameter found in add');

  query := TJsonPointerQuery.create;
  try
    query.execute(target, path, false);
    if not TJsonNode.compare(query.last, value) then
      raise EJsonException.Create('Test Failed because nodes are not equal');
  finally
    query.free;
  end;
end;

procedure TJsonPatchEngine.applyCopy(patchOp: TJsonObject; path : String);
var
  from : string;
  qFrom : TJsonPointerQuery;
begin
  from := patchOp['from'];
  if from = '' then
    raise EJsonException.Create('No patch from parameter found');

  qFrom := TJsonPointerQuery.create;
  try
    qFrom.execute(target, from, false);
    applyAddInner(path, qFrom.last);
  finally
    qFrom.free;
  end;
end;

procedure TJsonPatchEngine.applyMove(patchOp: TJsonObject; path : String);
var
  from : string;
  qFrom : TJsonPointerQuery;
  focus : TJsonNode;
begin
  from := patchOp['from'];
  if from = '' then
    raise EJsonException.Create('No patch from parameter found');

  qFrom := TJsonPointerQuery.create;
  try
    qFrom.execute(target, from, false);
    focus := qFrom.last.Link;
    try
      if (qFrom.secondLast is TJsonArray) and StringIsInteger32(qFrom.lastName) then
        (qFrom.secondLast as TJsonArray).FItems.DeleteByIndex((qFrom.secondLast as TJsonArray).FItems.IndexByReference(qFrom.last))
      else if qFrom.secondLast is TJsonObject then
        (qFrom.secondLast as TJsonObject).FProperties.Remove(qFrom.lastName)
      else
        raise EJsonException.Create('Unexpected target type '+qFrom.last.nodeType);

      applyAddInner(path, focus);
    finally
      focus.Free
    end;

  finally
    qFrom.free;
  end;
end;


{ TJsonPointerMatch }

constructor TJsonPointerMatch.Create(name: String; node: TJsonNode);
begin
  inherited create;
  self.Name := name;
  self.Node := node;
end;

destructor TJsonPointerMatch.Destroy;
begin
  FNode.Free;
  inherited;
end;

procedure TJsonPointerMatch.SetNode(const Value: TJsonNode);
begin
  FNode.Free;
  FNode := Value;
end;

function TJsonPointerMatch.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FNode.sizeInBytes);
end;

{ TJsonNull }

function TJsonNull.compare(other: TJsonNode): boolean;
begin
  result := other is TJsonNull;
end;

function TJsonNull.kind: TJsonNodeKind;
begin
  result := jnkNull;
end;

function TJsonNull.nodeType: String;
begin
  result := 'null'
end;

{ TJsonPointerQuery }

constructor TJsonPointerQuery.Create;
begin
  inherited Create;
  FMatches := TFslList<TJsonPointerMatch>.create;
end;

destructor TJsonPointerQuery.Destroy;
begin
  FMatches.Free;
  inherited;
end;

procedure TJsonPointerQuery.execute(focus: TJsonNode; path: String; terminalExtensions: boolean);
var
  i : integer;
  pl : TArray<String>;
  p : String;
begin
  if (path.Trim = '') then
    raise EJsonException.Create('Path cannot be blank');

  FMatches.Add(TJsonPointerMatch.Create('$', focus.Link));
  FTerminalState := tsFound;
  pl  := path.Split(['/']);
  for i := 1 to length(pl) - 1 do
  begin
    p := unescape(pl[i].Trim);
    if terminalExtensions and (i = length(pl) - 1) then
    begin
      if (p = '-') and (focus is TJsonArray) then
      begin
        FTerminalState := tsAtEnd;
        FMatches.Add(TJsonPointerMatch.Create(p, nil));
      end
      else
      begin
        focus := focus.evaluatePointer(p);
        if focus = nil then
        begin
          if (last is TJsonArray) and StringIsInteger32(p) and (StrToInt(p) = (last as TJsonArray).count) then
            FTerminalState := tsAtEnd
          else
            FTerminalState := tsNotFound;

          FMatches.Add(TJsonPointerMatch.Create(p, nil));
        end
        else
          FMatches.Add(TJsonPointerMatch.Create(p, focus.Link));
      end;
    end
    else
    begin
      focus := focus.evaluatePointer(p);
      if focus = nil then
        raise EJsonException.Create('Pointer could not be resolved: "'+p+'"')
      else
        FMatches.Add(TJsonPointerMatch.Create(p, focus.Link));
    end;
  end;
end;

function TJsonPointerQuery.GetLast: TJsonNode;
begin
  result := FMatches[FMatches.Count - 1].FNode;
end;

function TJsonPointerQuery.GetLastName: String;
begin
  result := FMatches[FMatches.Count - 1].FName;
end;

function TJsonPointerQuery.GetSecondLast: TJsonNode;
begin
  result := FMatches[FMatches.Count - 2].FNode;

end;

function TJsonPointerQuery.unescape(s: String): String;
begin
  result := s.Replace('~1', '/').Replace('~0', '~');
end;

function TJsonPointerQuery.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMatches.sizeInBytes);
end;

{ TJsonWriterCanonical }

procedure TJsonWriterCanonical.commitArray(node: TCanonicalJsonNode);
var
  c : TCanonicalJsonNode;
  first : boolean;
begin
  Produce('[');
  first := true;
  for c in node.FChildren do
  begin
    if first then
      first := false
    else
      produce(',');
    case c.FType of
      jntProperty : Produce(c.FValue);
      jntObject :
        commitObject(c);
      jntArray :
        commitArray(c);
    end;
  end;
  Produce(']');
end;

procedure TJsonWriterCanonical.commitObject(node: TCanonicalJsonNode);
var
  ts : TStringList;
  c : TCanonicalJsonNode;
  s : String;
  first : boolean;
begin
  // no pretty print: this is nly for canonical JSON anyway
  Produce('{');
  ts := TStringList.Create;
  try
    for c in node.FChildren do
      ts.Add(c.FName);
    ts.Sort;
    first := true;
    for s in ts do
    begin
      for c in node.FChildren do
        if c.FName = s then
        begin
          if first then
            first := false
          else
            produce(',');
          case c.FType of
            jntProperty : Produce('"'+c.FName+'":'+c.FValue);
            jntObject :
              begin
              Produce('"'+c.FName+'":');
              commitObject(c);
              end;
            jntArray :
              begin
              Produce('"'+c.FName+'":');
              commitArray(c);
              end;
          end;
        end;
    end;
  finally
    ts.Free;
  end;
  Produce('}');
end;

procedure TJsonWriterCanonical.doValue(name, value: String);
var
  node : TCanonicalJsonNode;
begin
  node := TCanonicalJsonNode.Create(jntProperty);
  try
    node.FName := name;
    node.FValue := value;
    FStack[FStack.Count - 1].FChildren.add(node.Link);
  finally
    node.Free;
  end;
end;

procedure TJsonWriterCanonical.Finish;
begin
  commitObject(FObject);
  FStack.Free;
  FObject.Free;
end;

procedure TJsonWriterCanonical.FinishArray;
begin
  FStack.Delete(FStack.Count - 1);
end;

procedure TJsonWriterCanonical.FinishObject;
begin
  FStack.Delete(FStack.Count - 1);
end;

function TJsonWriterCanonical.Link: TJsonWriterCanonical;
begin
  result := TJsonWriterCanonical(inherited Link);
end;

function TJsonWriterCanonical.GetSourceLocation: TSourceLocation;
begin
  result := TSourceLocation.CreateNull;
end;

procedure TJsonWriterCanonical.Start;
begin
  FObject := TCanonicalJsonNode.Create(jntObject);
  FStack := TFslList<TCanonicalJsonNode>.create;
  FStack.Add(FObject.Link);
end;

procedure TJsonWriterCanonical.ValueArray(const name: String);
var
  node : TCanonicalJsonNode;
begin
  node := TCanonicalJsonNode.Create(jntArray);
  try
    node.FName := name;
    FStack[FStack.Count - 1].FChildren.add(node.Link);
    FStack.Add(node.link);
  finally
    node.Free;
  end;
end;

procedure TJsonWriterCanonical.ValueInArray(const value: String);
var
  node : TCanonicalJsonNode;
begin
  node := TCanonicalJsonNode.Create(jntProperty);
  try
    node.FValue := value;
    FStack[FStack.Count - 1].FChildren.add(node.Link);
  finally
    node.Free;
  end;
end;

procedure TJsonWriterCanonical.valueNullInArray;
var
  node : TCanonicalJsonNode;
begin
  node := TCanonicalJsonNode.Create(jntProperty);
  try
    node.FValue := 'null';
    FStack[FStack.Count - 1].FChildren.add(node.Link);
  finally
    node.Free;
  end;
end;

procedure TJsonWriterCanonical.ValueObject;
var
  node : TCanonicalJsonNode;
begin
  node := TCanonicalJsonNode.Create(jntObject);
  try
    FStack[FStack.Count - 1].FChildren.add(node.Link);
    FStack.Add(node.link);
  finally
    node.Free;
  end;
end;

procedure TJsonWriterCanonical.ValueObject(const name: String);
var
  node : TCanonicalJsonNode;
begin
  node := TCanonicalJsonNode.Create(jntObject);
  try
    node.FName := name;
    FStack[FStack.Count - 1].FChildren.add(node.Link);
    FStack.Add(node.link);
  finally
    node.Free;
  end;
end;

function TJsonWriterCanonical.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FObject.sizeInBytes);
  inc(result, FStack.sizeInBytes);
end;

{ TCanonicalJsonNode }

constructor TCanonicalJsonNode.Create(aType : TCanonicalJsonNodeType);
begin
  inherited Create;
  FType := aType;
  FChildren := TFslList<TCanonicalJsonNode>.create;
end;

destructor TCanonicalJsonNode.Destroy;
begin
  FChildren.Free;
  inherited;
end;

function TCanonicalJsonNode.link: TCanonicalJsonNode;
begin
  result := TCanonicalJsonNode(inherited Link);
end;


function TCanonicalJsonNode.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FValue.length * sizeof(char)) + 12);
  inc(result, FChildren.sizeInBytes);
end;

{ TJWT }

constructor TJWT.create(header, payload: TJsonObject);
begin
  Create;
  self.Header := header;
  self.Payload := payload;
end;

function TJWT.desc: String;
begin
  if preferredName <> '' then
    result := preferredName
  else if name <> '' then
    result := name
  else if id <> '' then
    result := id
  else
    result := '??';
end;

constructor TJWT.create;
begin
  inherited create;
  FHeader := TJsonObject.Create('header');
  FPayload := TJsonObject.Create('payload');
end;

destructor TJWT.Destroy;
begin
  FHeader.free;
  FPayload.Free;
  inherited;
end;

procedure TJWT.setHeader(const Value: TJsonObject);
begin
  assert(value <> nil);
  FHeader.Free;
  FHeader := value;
end;

procedure TJWT.setPayload(const Value: TJsonObject);
begin
  assert(value <> nil);
  FPayload.Free;
  FPayload := value;
end;

function TJWT.Getissuer : String;
begin
  result := payload['iss'];
end;

procedure TJWT.Setissuer(value : String);
begin
  payload['iss'] := value;
end;

function TJWT.Getsubject : String;
begin
  result := payload['sub'];
end;

procedure TJWT.Setsubject(value : String);
begin
  payload['sub'] := value;
end;

function TJWT.Getaudience : String;
begin
  result := payload['aud'];
end;

procedure TJWT.Setaudience(value : String);
begin
  payload['aud'] := value;
end;

function TJWT.Getexpires : TDateTime;
begin
  result := UnixToDateTime(trunc(StrToFloat(payload.num['exp'])));
end;

procedure TJWT.Setexpires(value : TDateTime);
begin
  payload.num['exp'] := IntToStr(DateTimeToUnix(value));
end;

function TJWT.GetnotBefore : TDateTime;
begin
  result := UnixToDateTime(StrToIntDef(payload['nbf'], 0));
end;

procedure TJWT.SetnotBefore(value : TDateTime);
begin
  payload['nbf'] := IntToStr(DateTimeToUnix(value));
end;

function TJWT.GetissuedAt : TDateTime;
begin
  result := UnixToDateTime(StrToIntDef(payload['iat'], 0));
end;

procedure TJWT.SetissuedAt(value : TDateTime);
begin
  payload['iat'] := IntToStr(DateTimeToUnix(value));
end;

function TJWT.Getid : String;
begin
  result := payload['jti'];
end;

procedure TJWT.Setid(value : String);
begin
  if payload = nil then
    payload := TJsonObject.Create('payload');
  payload['jti'] := value;
end;


function TJWT.Getname : string;
begin
  result := payload['name'];
end;

procedure TJWT.Setname(value : string);
begin
  payload['name'] := value;
end;

function TJWT.GetgivenName : string;
begin
  result := payload['given_name'];
end;

procedure TJWT.SetgivenName(value : string);
begin
  payload['given_name'] := value;
end;

function TJWT.GetfamilyName : string;
begin
  result := payload['family_name'];
end;

procedure TJWT.SetfamilyName(value : string);
begin
  payload['family_name'] := value;
end;

function TJWT.GetmiddleName : string;
begin
  result := payload['middle_name'];
end;

procedure TJWT.SetmiddleName(value : string);
begin
  payload['middle_name'] := value;
end;

function TJWT.GetnickName : string;
begin
  result := payload['nickname'];
end;

procedure TJWT.SetnickName(value : string);
begin
  payload['nickname'] := value;
end;

function TJWT.GetpreferredName : string;
begin
  result := payload['preferred_username'];
end;

procedure TJWT.SetpreferredName(value : string);
begin
    payload['preferred_username'] := value;
  end;

function TJWT.Getprofile : string;
begin
  result := payload['profile'];
end;

procedure TJWT.Setprofile(value : string);
begin
  payload['profile'] := value;
end;

function TJWT.Getpicture : string;
begin
  result := payload['picture'];
end;

procedure TJWT.Setpicture(value : string);
begin
  payload['picture'] := value;
end;

function TJWT.Getwebsite : string;
begin
  result := payload['website'];
end;

function TJWT.Link: TJWT;
begin
  result := TJWT(inherited Link);
end;

procedure TJWT.Setwebsite(value : string);
begin
  payload['website'] := value;
end;

function TJWT.userName: String;
begin
  if name <> '' then
    result := name
  else if email <> '' then
    result := email
  else
    result := subject;
end;

function TJWT.Getemail : string;
begin
  result := payload['email'];
end;

procedure TJWT.Setemail(value : string);
begin
  payload['email'] := value;
end;

function TJWT.GetemailVerified : boolean ;
begin
  result := payload.bool['email_verified'];
end;

procedure TJWT.SetemailVerified(value : boolean );
begin
  payload.bool['email_verified'] := value;
end;

function TJWT.Getgender : string;
begin
  result := payload['gender'];
end;

procedure TJWT.Setgender(value : string);
begin
  payload['gender'] := value;
end;

function TJWT.Getbirthdate : string;
begin
  result := payload['birthdate'];
end;

procedure TJWT.Setbirthdate(value : string);
begin
  payload['birthdate'] := value;
end;

function TJWT.GettimeZone : string;
begin
  result := payload['zoneinfo'];
end;

procedure TJWT.SettimeZone(value : string);
begin
  payload['zoneinfo'] := value;
end;

function TJWT.Getlocale : string;
begin
  result := payload['locale'];
end;

procedure TJWT.Setlocale(value : string);
begin
  payload['locale'] := value;
end;

function TJWT.Getphone : string;
begin
  result := payload['phone_number'];
end;

procedure TJWT.Setphone(value : string);
begin
  payload['phone_number'] := value;
end;

function TJWT.Getphone_verified : boolean ;
begin
  result := payload.bool['phone_number_verified'];
end;

procedure TJWT.Setphone_verified(value : boolean );
begin
  payload.bool['phone_number_verified'] := value;
end;

function TJWT.GetupdatedAt : TDateTime;
begin
  result := UnixToDateTime(StrToIntDef(payload['updated_at'], 0));
end;

procedure TJWT.SetupdatedAt(value : TDateTime);
begin
  payload['updated_at'] := IntToStr(DateTimeToUnix(value));
end;

function TJWT.GetaddressFormatted : string;
begin
  result := payload.forceObj['address']['formatted'];
end;

procedure TJWT.SetaddressFormatted(value : string);
begin
  payload.forceObj['address']['formatted'] := value;
end;

function TJWT.GetaddressStreet : string;
begin
  result := payload.forceObj['address']['street_address'];
end;

procedure TJWT.SetaddressStreet(value : string);
begin
  payload.forceObj['address']['street_address'] := value;
end;

function TJWT.GetaddressLocality : string;
begin
  result := payload.forceObj['address']['locality'];
end;

procedure TJWT.SetaddressLocality(value : string);
begin
  payload.forceObj['address']['locality'] := value;
end;

function TJWT.GetaddressRegion : string;
begin
  result := payload.forceObj['address']['region'];
end;

procedure TJWT.SetaddressRegion(value : string);
begin
  payload.forceObj['address']['region'] := value;
end;

function TJWT.GetaddressPostCode : string;
begin
  result := payload.forceObj['address']['postal_code'];
end;

procedure TJWT.SetaddressPostCode(value : string);
begin
  payload.forceObj['address']['postal_code'] := value;
end;

function TJWT.GetaddressCountry : string;
begin
  result := payload.forceObj['address']['country'];
end;

procedure TJWT.SetaddressCountry(value : string);
begin
  payload.forceObj['address']['country'] := value;
end;

function TJWT.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FHeader.sizeInBytes);
  inc(result, FPayLoad.sizeInBytes);
  inc(result, (FOriginalSource.length * sizeof(char)) + 12);
end;

End.

