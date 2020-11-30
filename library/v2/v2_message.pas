unit v2_message;

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

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_stream, fsl_utilities,
  fhir_objects, fhir_pathengine;

type
  TV2ContentKind = (ckString, ckNull, ckBinary, ckEscape);
  TV2Location = class;

  // inherit from TFHIRObject so we can use FHIRPath on the objects
  // the underlying framework insists that this has a version.
  TV2Object = class (TFHIRObject)
  private
    FId : String;
  protected
    function GetFhirObjectVersion: TFHIRVersion; override;
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function sizeInBytesV : cardinal; override;
  public
    property id : String read Fid write FId;
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function hasExtensions : boolean; override;

    function SerialiseUsingProperties : boolean; override;

    function createPropertyValue(propName : string): TFHIRObject; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
  end;

  TV2Content = class (TV2Object)
  private
    FKind: TV2ContentKind;
    FValue: String;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(kind : TV2ContentKind; value : String);
    function link : TV2Content; overload;

    property kind : TV2ContentKind read FKind write FKind;
    property value : String read FValue write FValue;

    function isEmpty : boolean; override;
    function fhirType : String; override;
  end;

  TV2Cell = class (TV2Object)
  private
    FContentList: TFSLList<TV2Content>;
    FComponentList: TFSLList<TV2Cell>;
    function GetText: String;
    procedure SetText(const Value: String);
    function GetComponent(index: integer): TV2Cell;
    function GetContent(index: integer): TV2Content;
    function GetSimple: String;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TV2Cell; overload;

    property contentList : TFSLList<TV2Content> read FContentList;
    property content[index : integer] : TV2Content read GetContent; // 1 based
    property componentList : TFSLList<TV2Cell> read FComponentList;
    property component[index : integer] : TV2Cell read GetComponent; // 1 based

    function isEmpty : boolean; override;
    property text : String read GetText write SetText;
    property simple : String read GetSimple;
    function fhirType : String; override;
  end;

  TV2Field = class (TV2Object)
  private
    FElementList: TFSLList<TV2Cell>;
    function GetElement(index: integer): TV2Cell;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TV2Field; overload;

    function isEmpty : boolean; override;

    property elementList : TFSLList<TV2Cell> read FElementList;
    property element[index : integer] : TV2Cell read GetElement; // 1 based
    function fhirType : String; override;
  end;

  TV2Segment = class (TV2Object)
  private
    FFieldList: TFSLList<TV2Field>;
    FCode: String;
    function GetField(index: integer): TV2Field;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; overload; override;
    constructor Create(code : String); overload;
    destructor Destroy; override;
    function link : TV2Segment; overload;

    property code : String read FCode write FCode;
    property fieldList : TFSLList<TV2Field> read FFieldList;
    property field[index : integer] : TV2Field read GetField; // 1 based
    function element(index : integer) : TV2Cell;
    function isEmpty : boolean; override;
    function fhirType : String; override;
  end;

  { TV2Message }

  TV2Message = class (TV2Object)
  private
    FSegmentList: TFSLList<TV2Segment>;
    function GetSegment(index: integer): TV2Segment;
  protected
    Procedure GetChildrenByName(name : string; list : TFHIRSelectionList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties, bPrimitiveValues : Boolean); override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function link : TV2Message; overload;

    property segmentList : TFSLList<TV2Segment> read FSegmentList;
    property segment[index : integer] : TV2Segment read GetSegment; // 1 based
    function isEmpty : boolean; override;
    function fhirType : String; override;

    function findLocation(loc : TSourceLocation): TV2Location;
  end;

  TV2ParserOption = (v2doPreventOddBinaryLengths, v2doMessageHeaderOnly, v2doIgnoreXmlErrors, v2Validating, v2LocationData);
  TV2ParserOptions = set of TV2ParserOption;

  TV2Parser = class (TFslObject)
  private
    FSource : TBytes;
    FLine : integer;
    FOptions : TV2ParserOptions;
    FEscapeCharacter: Char;
    FRepetitionDelimiter: Char;
    FFieldDelimiter: Char;
    FSubComponentDelimiter: Char;
    FComponentDelimiter: Char;

    procedure ReadDelimiters;
    Function isDelimiterEscape(ch : Char) : Boolean;
    Function getDelimiterEscapeChar(ch : Char) : Char;
    Function cleanPacket(packet : TBytes) : TBytes;

    procedure generateIds(cell : TV2Cell); overload;
    procedure generateIds(fld : TV2Field); overload;
    procedure generateIds(seg : TV2Segment); overload;
    procedure generateIds(msg : TV2Message); overload;

    Procedure readBinary(cell : TV2Cell; Const cnt : String; var iCursor : integer);
    Procedure readEscape(cell : TV2Cell; Const cnt : String; var iCursor : integer);
    procedure parseContent(cell: TV2Cell; const cnt: String; bNoEscape: Boolean);
    procedure parseCell(cell : TV2Cell; const cnt : String; bNoEscape : Boolean; cBreak, cSubBreak : char);
    procedure parseField(ofield: TV2Field; var iCursor: Integer; iCursorStart : integer);
    procedure parseSegmentInner(oSegment: TV2Segment; var iCursor: Integer; iStart : integer);
    procedure parseSegment(oMessage : TV2Message; var iCursor : Integer);
    Procedure DecodeMessage(msg : TV2Message; options : TV2ParserOptions); Overload; Virtual;
  public
    class function parse(msg : TBytes; options : TV2ParserOptions = []) : TV2Message; overload;
    class function parse(msg : String; options : TV2ParserOptions = []) : TV2Message; overload;
    class function parse(msg : TStream; options : TV2ParserOptions = []) : TV2Message; overload;
    class function parse(msg : TFslStream; options : TV2ParserOptions = []) : TV2Message; overload;
    class function parse(msg : TFslBuffer; options : TV2ParserOptions = []) : TV2Message; overload;
  end;

  TV2ComposerOption = (v2coEscapeExtendedCharacters, v2coExtraFieldDelimiter);
  TV2ComposerOptions = set of TV2ComposerOption;

  TV2Composer = class (TFslObject)
  private
    FOptions: TV2ComposerOptions;
    FEscapeCharacter: AnsiChar;
    FRepetitionDelimiter: AnsiChar;
    FFieldDelimiter: AnsiChar;
    FSubComponentDelimiter: AnsiChar;
    FComponentDelimiter: AnsiChar;

    procedure init;
    function escape(b : TStringBuilder; src : String) : AnsiString;
    procedure composeBinary(b : TStringBuilder; cnt : TV2Content);
    procedure composeEscape(b : TStringBuilder; cnt : TV2Content);
    procedure composeContent(b : TStringBuilder; cnt : TV2Content);
    procedure composeCell(b : TStringBuilder; cell : TV2Cell; ch : AnsiChar);
    procedure composeField(b : TStringBuilder; fld : TV2Field);
    procedure composeSegment(b : TStringBuilder; seg : TV2Segment);
    procedure composeMessage(b : TStringBuilder; msg : TV2Message);
  public
    class function composeString(msg : TV2Message; options : TV2ComposerOptions = []) : String; overload;
    class function composeBytes(msg : TV2Message; options : TV2ComposerOptions = []) : TBytes; overload;
    class function composeString(obj : TV2Object; options : TV2ComposerOptions = []) : String; overload;
    class function composeBytes(obj : TV2Object; options : TV2ComposerOptions = []) : TBytes; overload;
    class procedure compose(msg : TV2Message; dst : TStream; options : TV2ComposerOptions = []); overload;
    class procedure compose(msg : TV2Message; dst : TFslStream; options : TV2ComposerOptions = []); overload;
    class procedure compose(msg : TV2Message; dst : TFslBuffer; options : TV2ComposerOptions = []); overload;
  end;

  TV2FHIRPathExtensions = class (TFHIRPathEngineExtension)
  public
    function resolveConstant(context : TFHIRPathExecutionContext; s : String; var obj : TFHIRObject) : boolean; override;
    function isValidFunction(name : String) : boolean; override;
    function functionApplies(context : TFHIRPathExecutionContext; focus: TFHIRSelectionList; name : String): boolean; override;
    function execute(context : TFHIRPathExecutionContext; focus: TFHIRObject; name : String; params : TFslList<TFHIRPathExpressionNodeV>; engine : TFHIRPathEngineV): TFHIRSelectionList; override;
  end;

  { TV2Location }

  TV2Location = class (TFslObject)
  private
    FComponent: TV2Cell;
    FComponentIndex: integer;
    FElement: TV2Cell;
    FElementIndex: integer;
    FField: TV2Field;
    FFieldIndex: integer;
    FSegment: TV2Segment;
    FSegmentIndex: integer;
    FSubComponent: TV2Cell;
    FSubComponentIndex: integer;
    procedure SetComponent(AValue: TV2Cell);
    procedure SetElement(AValue: TV2Cell);
    procedure SetField(AValue: TV2Field);
    procedure SetSegment(AValue: TV2Segment);
    procedure SetSubComponent(AValue: TV2Cell);

  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;

    property Segment : TV2Segment read FSegment write SetSegment;
    property SegmentIndex : integer read FSegmentIndex write FSegmentIndex;

    property Field : TV2Field read FField write SetField;
    property FieldIndex : integer read FFieldIndex write FFieldIndex;

    property Element : TV2Cell read FElement write SetElement;
    property ElementIndex : integer read FElementIndex write FElementIndex;

    property Component : TV2Cell read FComponent write SetComponent;
    property ComponentIndex : integer read FComponentIndex write FComponentIndex;

    property SubComponent : TV2Cell read FSubComponent write SetSubComponent;
    property SubComponentIndex : integer read FSubComponentIndex write FSubComponentIndex;
  end;

const
  CODES_TV2ContentKind : Array [TV2ContentKind] of String = ('string', 'null', 'binary', 'escape');

implementation

{ TV2Location }

destructor TV2Location.Destroy;
begin
  FComponent.Free;
  FElement.Free;
  FField.Free;
  FSegment.Free;
  FSubComponent.Free;
  inherited Destroy;
end;

procedure TV2Location.SetComponent(AValue: TV2Cell);
begin
  FComponent.Free;
  FComponent:=AValue;
end;

procedure TV2Location.SetElement(AValue: TV2Cell);
begin
  FElement.Free;
  FElement:=AValue;
end;

procedure TV2Location.SetField(AValue: TV2Field);
begin
  FField.Free;
  FField:=AValue;
end;

procedure TV2Location.SetSegment(AValue: TV2Segment);
begin
  FSegment.Free;
  FSegment:=AValue;
end;

procedure TV2Location.SetSubComponent(AValue: TV2Cell);
begin
  FSubComponent.Free;
  FSubComponent:=AValue;
end;

function TV2Location.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FComponent.sizeInBytes);
  inc(result, FElement.sizeInBytes);
  inc(result, FField.sizeInBytes);
  inc(result, FSegment.sizeInBytes);
  inc(result, FSubComponent.sizeInBytes);
end;

{ TV2Object }

function TV2Object.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFSLException.Create('Not supported');
end;

procedure TV2Object.GetChildrenByName(name: string; list: TFHIRSelectionList);
begin
  inherited;
  if (name = 'id') Then
    if id <> '' then
      list.add(self.link, 'id', TFHIRObjectText.create(id))
    else
      list.add(self.link, 'id', nil)
end;

function TV2Object.GetFhirObjectVersion: TFHIRVersion;
begin
  result := fhirVersionRelease4;
end;

function TV2Object.getId: String;
begin
  result := id;
end;

function TV2Object.hasExtensions: boolean;
begin
  result := false;
end;

procedure TV2Object.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  if bInheritedProperties then
    inherited;
  oList.add(TFHIRProperty.create(self, 'id', 'id', false, TFHIRObjectText, TFHIRObjectText.Create(FId)));
end;

function TV2Object.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFHIRObjectText.Create(v);
end;

function TV2Object.makeIntValue(v: String): TFHIRObject;
begin
  result := TFHIRObjectText.Create(v);
end;

function TV2Object.makeStringValue(v: String): TFHIRObject;
begin
  result := TFHIRObjectText.Create(v);
end;

procedure TV2Object.setIdValue(id: String);
begin
  self.id := id;
end;

function TV2Object.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  raise EFSLException.Create('Not supported');
end;

function TV2Object.SerialiseUsingProperties: boolean;
begin
  result := true;
end;

function TV2Object.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FId.length * sizeof(char)) + 12);
end;

{ TV2Content }

constructor TV2Content.Create(kind: TV2ContentKind; value: String);
begin
  inherited Create;
  FKind := kind;
  FValue := value;
end;

function TV2Content.fhirType: String;
begin
  result := 'Content';
end;

procedure TV2Content.GetChildrenByName(name: string; list: TFHIRSelectionList);
begin
  inherited;
  if (name = 'kind') Then
     list.add(self.link, 'kind', TFHIRObjectText.Create(CODES_TV2ContentKind[kind]));
  if (name = 'value') Then
    if id <> '' then
      list.add(self.link, 'value', TFHIRObjectText.create(value))
    else
      list.add(self.link, 'value', nil)
end;

function TV2Content.isEmpty: boolean;
begin
  result := value <> '';
end;

function TV2Content.link: TV2Content;
begin
  result := TV2Content(inherited link);
end;

procedure TV2Content.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
begin
  if bInheritedProperties then
    inherited;
  oList.add(TFHIRProperty.create(self, 'value', 'string', false, TFHIRObjectText, TFHIRObjectText.Create(FValue)));
  oList.add(TFHIRProperty.create(self, 'kind', 'code', false, TFHIRObjectText, TFHIRObjectText.Create(CODES_TV2ContentKind[FKind])));
end;

function TV2Content.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FValue.length * sizeof(char)) + 12);
end;

{ TV2Cell }

constructor TV2Cell.Create;
begin
  inherited;
  FContentList := TFSLList<TV2Content>.create;
  FComponentList := TFSLList<TV2Cell>.create;
end;

destructor TV2Cell.Destroy;
begin
  FComponentList.Free;
  FContentList.free;
  inherited;
end;

function TV2Cell.fhirType: String;
begin
  result := 'Cell';
end;

procedure TV2Cell.GetChildrenByName(name: string; list: TFHIRSelectionList);
var
  cnt : TV2Content;
  cmp : TV2Cell;
begin
  inherited;
  if (name = 'content') Then
  begin
    list.OneBased :=  true;
    for cnt in contentList do
       list.add(cnt.link);
  end;
  if (name = 'component') Then
  begin
    list.OneBased :=  true;
    for cmp in componentList do
      list.add(cmp.link);
  end;
end;

function TV2Cell.GetComponent(index: integer): TV2Cell;
begin
  result := FComponentList[index-1];
end;

function TV2Cell.GetContent(index: integer): TV2Content;
begin
  result := FContentList[index-1];
end;

function TV2Cell.GetSimple: String;
begin
  if (componentList <> nil) and (componentList.Count > 1) then
    result := componentList[0].simple
  else
    result := text;
end;

function TV2Cell.GetText: String;
var
  cnt : TV2Content;
begin
  if componentList.Count > 0 then
    result := componentList[0].text
  else
  begin
    result := '';
    for cnt in contentList do
      result := result + cnt.value;
  end;
end;

function TV2Cell.isEmpty: boolean;
var
  comp : TV2Cell;
begin
  result := true;
  if componentList.Count = 0 then
    result := contentList.Empty
  else
    for comp in componentList do
      if not comp.isEmpty then
        exit(false);
end;

function TV2Cell.link: TV2Cell;
begin
  result := TV2Cell(inherited Link);
end;

procedure TV2Cell.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
var
  p : TFHIRProperty;
  v : TFslObject;
begin
  if bInheritedProperties then
    inherited;
  p := TFHIRProperty.create(self, 'content', 'Content', true, TV2Content);
  for v in FContentList do
    p.Values.add(v.link);
  p := TFHIRProperty.create(self, 'component', 'Cell', true, TV2Cell);
  for v in FComponentList do
    p.Values.add(v.link);
end;

procedure TV2Cell.SetText(const Value: String);
begin
  if componentList.Count > 0 then
    componentList[0].text := value
  else
  begin
    contentList.Clear;
    contentList.Add(TV2Content.Create(ckString, value));
  end;
end;

function TV2Cell.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FContentList.sizeInBytes);
  inc(result, FComponentList.sizeInBytes);
end;

{ TV2Field }

constructor TV2Field.Create;
begin
  inherited;
  FElementList := TFSLList<TV2Cell>.create;
end;

destructor TV2Field.Destroy;
begin
  FElementList.Free;
  inherited;
end;

function TV2Field.fhirType: String;
begin
  result := 'Field';
end;

procedure TV2Field.GetChildrenByName(name: string; list: TFHIRSelectionList);
var
  elem : TV2Cell;
begin
  inherited;
  if (name = 'element') Then
  begin
    list.OneBased :=  true;
    for elem in elementList do
      list.add(elem.link);
  end;
end;

function TV2Field.GetElement(index: integer): TV2Cell;
begin
  result := FElementList[index-1];
end;

function TV2Field.isEmpty: boolean;
var
  elem : TV2Cell;
begin
  result := true;
  for elem in elementList do
    if not elem.isEmpty then
      exit(false);
end;

function TV2Field.link: TV2Field;
begin
  result := TV2Field(inherited Link);
end;

procedure TV2Field.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
var
  p : TFHIRProperty;
  v : TFslObject;
begin
  if bInheritedProperties then
    inherited;
  p := TFHIRProperty.create(self, 'element', 'Cell', true, TV2Cell);
  for v in FElementList do
    p.Values.add(v.link);
end;

function TV2Field.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FElementList.sizeInBytes);
end;

{ TV2Segment }

constructor TV2Segment.Create;
begin
  inherited;
  FFieldList := TFSLList<TV2Field>.create;
end;

constructor TV2Segment.Create(code: String);
begin
  Create;
  FCode := code;
end;

destructor TV2Segment.Destroy;
begin
  FFieldList.Free;
  inherited;
end;

function TV2Segment.element(index: integer): TV2Cell;
begin
  while FFieldList.Count < index do
    FFieldList.Add(TV2Field.Create);

  if FFieldList[index-1].elementList.Count > 1 then
    raise EFslException.Create('Repeats encountered at '+inttostr(index))
  else
    result := FFieldList[index-1].elementList[0];
end;

function TV2Segment.fhirType: String;
begin
  result := 'Segment';
end;

procedure TV2Segment.GetChildrenByName(name: string; list: TFHIRSelectionList);
var
  fld : TV2Field;
begin
  inherited;
  if (name = 'code') Then
    list.add(self.link, 'code', TFHIRObjectText.create(code));
  if (name = 'field') Then
  begin
    list.OneBased :=  true;
    for fld in fieldList do
      list.add(fld.link);
  end;
end;

function TV2Segment.GetField(index: integer): TV2Field;
begin
  result := FFieldList[index - 1];
end;

function TV2Segment.isEmpty: boolean;
begin
  result := FFieldList.Empty;
end;

function TV2Segment.link: TV2Segment;
begin
  result := TV2Segment(inherited link);
end;

procedure TV2Segment.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
var
  p : TFHIRProperty;
  v : TFslObject;
begin
  if bInheritedProperties then
    inherited;
  p := TFHIRProperty.create(self, 'field', 'Field', true, TV2Field);
  for v in FFieldList do
    p.Values.add(v.link);
end;

function TV2Segment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFieldList.sizeInBytes);
  inc(result, (FCode.length * sizeof(char)) + 12);
end;

{ TV2Message }

constructor TV2Message.Create;
begin
  inherited;
  FSegmentList := TFSLList<TV2Segment>.create;
end;

destructor TV2Message.Destroy;
begin
  FSegmentList.Free;
  inherited;
end;

function TV2Message.fhirType: String;
begin
  result := 'Message';
end;

function TV2Message.findLocation(loc: TSourceLocation): TV2Location;
var
  seg : TV2Segment;
  field : TV2Field;
  cell : TV2Cell;
begin
  result := TV2Location.create;
  try
    for seg in FSegmentList do
    begin
      if (seg.LocationData.InSpan(loc)) then
      begin
        result.Segment := seg.link;
        break;
      end;
    end;
    if (result.Segment <> nil) then
    begin
      for field in result.segment.FFieldList do
      begin
        if (field.LocationData.InSpan(loc)) then
        begin
          result.Field := field.link;
          break;
        end;
      end;
    end;
    if (result.Field <> nil) then
    begin
      for cell in result.Field.FElementList do
      begin
        if (cell.LocationData.InSpan(loc)) then
        begin
          result.Element := cell.link;
          break;
        end;
      end;
    end;
    if (result.Element <> nil) then
    begin
      for cell in result.Element.FComponentList do
      begin
        if (cell.LocationData.InSpan(loc)) then
        begin
          result.Component := cell.link;
          break;
        end;
      end;
    end;
    if (result.Component <> nil) then
    begin
      for cell in result.Component.FComponentList do
      begin
        if (cell.LocationData.InSpan(loc)) then
        begin
          result.SubComponent := cell.link;
          break;
        end;
      end;
    end;

    result.link;
  finally
    result.free;
  end;
end;

procedure TV2Message.GetChildrenByName(name: string; list: TFHIRSelectionList);
var
  seg : TV2Segment;
begin
  inherited;
  if (name = 'segment') Then
  begin
    list.OneBased := true;
    for seg in segmentList do
      list.add(seg.link);
  end;
end;

function TV2Message.GetSegment(index: integer): TV2Segment;
begin
  result := FSegmentList[index-1];
end;

function TV2Message.isEmpty: boolean;
begin
  result := segmentList.Empty;
end;

function TV2Message.link: TV2Message;
begin
  result := TV2Message(inherited link);
end;

procedure TV2Message.ListProperties(oList: TFHIRPropertyList; bInheritedProperties, bPrimitiveValues: Boolean);
var
  p : TFHIRProperty;
  v : TFslObject;
begin
  if bInheritedProperties then
    inherited;
  p := TFHIRProperty.create(self, 'segment', 'Segment', true, TV2Segment);
  for v in FSegmentList do
    p.Values.add(v.link);
end;

function TV2Message.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSegmentList.sizeInBytes);
end;

{ TV2Parser }

Function TV2Parser.cleanPacket(packet : TBytes) : TBytes;
begin
  // some systems are somewhat careless about how they wrap a packet
  // some systems prepend a #13 to the message
  // some fail to append a #13.
  // some even send HL7 segment delimiters as  #13#10 instead of #13
  result := BytesReplace(packet, Bytes([13, 10]), Bytes([13]));
  if length(result) > 0 then
  begin
    while result[0] = TByte(13) do
    begin
      result := copy(result, 1, length(result) -1);
      inc(FLine);
    end;
    while result[length(result)-1] = TByte(13) do
      SetLength(result, length(result) - 1);
    result := BytesAdd(result, TByte(13));
  end;
end;

class function TV2Parser.parse(msg: TBytes; options: TV2ParserOptions): TV2Message;
var
  this : TV2Parser;
begin
  this := TV2Parser.Create;
  try
    result := TV2Message.Create;
    try
      this.FSource := this.cleanPacket(msg);
      this.FOptions := options;
      this.decodeMessage(result, options);
      result.link;
    finally
      result.Free;
    end;
  finally
    this.free;
  end;
end;

class function TV2Parser.parse(msg: String; options: TV2ParserOptions): TV2Message;
begin
  result := parse(TEncoding.UTF8.GetBytes(msg), options);
end;

class function TV2Parser.parse(msg: TStream; options: TV2ParserOptions): TV2Message;
begin
  result := parse(StreamToBytes(msg), options);
end;

class function TV2Parser.parse(msg: TFslStream; options: TV2ParserOptions): TV2Message;
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

Function GetDelimiter(sName, sStr : String; iIndex : Integer) : Char;
Begin
  If Length(sStr) >= iIndex Then
    Result := SStr[iIndex]
  Else
  Begin
//    Result := #0;
    raise EER7Exception(sName+' Delimiter not found in MSH-2: "'+sStr+'"');
  End;
End;

const
  DEFAULT_DELIMITER_FIELD = '|';
  DEFAULT_DELIMITER_COMPONENT = '^';
  DEFAULT_DELIMITER_SUBCOMPONENT = '&';
  DEFAULT_DELIMITER_REPETITION = '~';
  DEFAULT_CHARACTER_ESCAPE = '\';

procedure TV2Parser.ReadDelimiters;
var
  sLeft : String;
  sRight : String;
begin
  // basics: this checks that the MSH appears to be valid, records the delimiters,
  // and checks the version
  if Not SameBytes(copy(FSource, 0, 3), 'MSH') then
    raise EER7Exception.create('Packet "' + BytesAsMime(copy(FSource, 0, 12)) + '"does not appear to be valid HL7/ER7: starting MSH not found');
  if not BytesContains(FSource, Tbyte(13)) then
    raise EER7Exception.create('Packet does not appear to be valid HL7/ER7: Segment Delimiter not found');

  FFieldDelimiter := Char(FSource[3]);
  StringSplit(BytesAsString(FSource), FFieldDelimiter, sLeft, sRight);
  StringSplit(sRight, FFieldDelimiter, sLeft, sRight);

  FComponentDelimiter := GetDelimiter('Component', sLeft, 1);
  FRepetitionDelimiter := GetDelimiter('Repetition', sLeft, 2);
  FEscapeCharacter := GetDelimiter('Escape', sLeft, 3);
  if Length(sLeft) > 3 then
    FSubComponentDelimiter := GetDelimiter('SubComponent', sLeft, 4)
  Else
    FSubComponentDelimiter := DEFAULT_DELIMITER_SUBCOMPONENT;

  if FComponentDelimiter = FFieldDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FComponentDelimiter+'" is used for both Component and Field');
  if FSubComponentDelimiter = FFieldDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FSubComponentDelimiter+'" is used for both SubComponent and Field');
  if FSubComponentDelimiter = FComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FSubComponentDelimiter+'" is used for both SubComponent and Component');
  if FRepetitionDelimiter = FFieldDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FRepetitionDelimiter+'" is used for both Repetition and Field');
  if FRepetitionDelimiter = FComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FRepetitionDelimiter+'" is used for both Repetition and Component');
  if FRepetitionDelimiter = FSubComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FRepetitionDelimiter+'" is used for both Repetition and SubComponent');
  if FEscapeCharacter = FFieldDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and Field');
  if FEscapeCharacter = FComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and Component');
  if FEscapeCharacter = FSubComponentDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and SubComponent');
  if FEscapeCharacter = FRepetitionDelimiter then
    raise EER7Exception.create('Delimiter Error: "'+FEscapeCharacter+'" is used for both Escape and Repetition');
end;

procedure TV2Parser.DecodeMessage(msg : TV2Message; options: TV2ParserOptions);
var
  iCursor : Integer;
begin
  // 1. initial check of MSH: version and delimiters
  ReadDelimiters;

  // 2. Decode MSH
  iCursor := 0;
  ParseSegment(msg, iCursor);

  // 4. decode remaining segmentList
  while (iCursor < length(FSource)) do
    ParseSegment(msg, iCursor);

  generateIds(msg);
end;

class function TV2Parser.parse(msg: TFslBuffer; options: TV2ParserOptions): TV2Message;
begin
  result := parse(msg.AsBytes);
end;

procedure TV2Parser.ParseSegment(oMessage: TV2Message; var iCursor: Integer);
var
  sSegCode : String;
  oSegment : TV2Segment;
  loc : TSourceLocation;
  iStart : integer;
begin
  loc := TSourceLocation.Create(FLine, 0);
  iStart :=  iCursor;
  if (iCursor + 3 > length(FSource)) then //we are either missing a #13 or have an invalid segment code
    raise EER7Exception.create('Remaining length in message too short for a valid segment.' + #13 +
                         'Remainder of packet: ' + StringReplace(copy(BytesAsString(FSource), iCursor, length(FSource)), #13, '<CR>'));

  //recognise segment
  sSegCode := BytesAsString(copy(FSource, iCursor, 3));
  inc(iCursor, 3);

  if not (CharInSet(sSegCode[1], ['A'..'Z']) and CharInSet(sSegCode[2], ['A'..'Z', '0'..'9']) and CharInSet(sSegCode[3], ['A'..'Z', '0'..'9'])) then
    raise EER7Exception.create('Segment code too short or contains invalid content: ' + String(sSegCode));

  if not StringArrayExists(['MSH', 'FHS', 'BHS'], String(sSegCode)) and (FSource[iCursor] <> 13) then
      inc(iCursor); // special case: field is it's own delimiter for first MSH field

  oSegment := TV2Segment.Create;
  try
    if v2LocationData in FOptions then
      oSegment.LocationData.parseStart := loc;
    oSegment.Code := sSegCode;
    oMessage.SegmentList.Add(oSegment.Link); // do this to provide a model before setting the code
    parseSegmentInner(oSegment, iCursor, iStart);
    if v2LocationData in FOptions then
      oSegment.LocationData.parseFinish := TSourceLocation.Create(FLine, iCursor - iStart);
  finally
    oSegment.Free;
  end;
  inc(FLine);
end;

procedure TV2Parser.parseSegmentInner(oSegment: TV2Segment; var iCursor: Integer; iStart : integer);
var
  oField : TV2Field;
  first : boolean;
begin
  while FSource[iCursor] <> 13 do
  begin
    oField := TV2Field.create;
    try
      if v2LocationData in FOptions then
        oField.LocationData.ParseStart := TSourceLocation.Create(FLine, iCursor - iStart);
      oSegment.FieldList.Add(oField.Link);
      ParseField(oField, iCursor, iStart);
      if v2LocationData in FOptions then
        oField.LocationData.ParseFinish := TSourceLocation.Create(FLine, iCursor - iStart - 1);
    finally
      oField.Free;
    end;
  end;

  first := true;
  while (iCursor < Length(FSource)) and (FSource[iCursor] = 13) do
  begin
    if not first and (v2Validating in FOptions) then
      raise EParserException.create('Unexpected line break', TSourceLocation.Create(FLine+2, 0))
    else
    begin
      inc(iCursor);
      first := false;
    end;
  end;
end;

Function BytesSlice(Const sValue : TBytes; iBegin, iEnd : Integer) : TBytes;
Begin
  Result := Copy(sValue, iBegin, iEnd - iBegin + 1);
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

procedure TV2Parser.parseField(ofield: TV2Field; var iCursor : Integer; iCursorStart : integer);
var
  cell : Tv2Cell;
  iStart : Integer;
  bEscape : Boolean;
//  LContent: String;
begin
  while (iCursor < length(FSource)) and not (FSource[iCursor] in [13, ord(FFieldDelimiter)]) do
  begin
    iStart := iCursor;
    bEscape := False;
    while (iCursor < length(FSource)) and not (FSource[iCursor] in [13, ord(FFieldDelimiter), ord(FRepetitionDelimiter)]) do
    Begin
      bEscape := bEscape or (FSource[iCursor] = ord(FEscapeCharacter));
      inc(iCursor);
    End;
    cell := TV2Cell.Create;
    try
      if v2LocationData in FOptions then
        cell.LocationData.ParseStart := TSourceLocation.Create(FLine, iStart - iCursorStart);
      if v2LocationData in FOptions then
        cell.LocationData.ParseFinish := TSourceLocation.Create(FLine, iCursor - iCursorStart);
      parseCell(cell, BytesAsString(BytesSlice(FSource, iStart, iCursor-1)), not bEscape, FComponentDelimiter, FSubComponentDelimiter);
      ofield.elementList.Add(cell.link);
    finally
      cell.Free;
    end;
    if (iCursor < length(FSource)) and (FSource[iCursor] = ord(FRepetitionDelimiter)) then
      inc(iCursor);
  end;
  if (iCursor < length(FSource)) and (FSource[iCursor] <> 13) then
    inc(iCursor);
end;

procedure TV2Parser.parseCell(cell : TV2Cell; const cnt : String; bNoEscape : Boolean; cBreak, cSubBreak : char);
var
  child : TV2Cell;
  sLeft : String;
  sRight : String;
  i : integer;
begin
  // if children are defined or a seperator is found, then we will break content up into children
  if (pos(cBreak, cnt) > 0) or ((cSubBreak <> #0) and (pos(cSubBreak, cnt) > 0)) then
  begin
    i := 0;
    sRight := cnt;
    while sRight <> '' do
    begin
      StringSplit(sRight, cBreak, sLeft, sRight);
      child := TV2Cell.Create;
      try
        if v2LocationData in FOptions then
          child.LocationData.ParseStart := TSourceLocation.Create(cell.LocationData.ParseStart.line, cell.LocationData.ParseStart.col + i);
        inc(i, sLeft.length);
        if v2LocationData in FOptions then
          child.LocationData.ParseFinish := TSourceLocation.Create(cell.LocationData.ParseStart.line, cell.LocationData.ParseStart.col + i);
        cell.componentList.Add(child.link);
        parseCell(child, sLeft, bNoEscape, cSubBreak, #0);
        inc(i);
      finally
        child.Free;
      end;
    end;
  end
  else
  begin
    if pos(#0, cnt) > 0 Then
      parseContent(cell, StringStrip(cnt, [#0]), bNoEscape)
    else
      parseContent(cell, cnt, bNoEscape);
  end;
end;

Procedure TV2Parser.parseContent(cell : TV2Cell; Const cnt : String; bNoEscape : Boolean);
var
  buf : TStringBuilder;
  iCursor : Integer;
  Procedure Commit();
  Begin
    if buf.Length > 0 Then
      Begin
      cell.ContentList.Add(TV2Content.create(ckString, buf.ToString));
      buf.Clear;
      End;
  End;
Begin
  cell.contentList.Clear;
  if (cnt = FEscapeCharacter) Then
    cell.ContentList.Add(TV2Content.create(ckString, cnt))
  Else If (cnt = '""') then
    cell.ContentList.add(TV2Content.create(ckNull, '""'))
  Else If bNoEscape or (pos(FEscapeCharacter, cnt) = 0) Then
  Begin
    If cnt <> '' Then
      cell.ContentList.Add(TV2Content.create(ckString, cnt));
  End
  Else
  Begin
    buf := TStringBuilder.Create;
    try
      iCursor := 1;
      While (iCursor <= Length(cnt)) Do
        Begin
        if (iCursor < Length(cnt)) and (cnt[iCursor] = FEscapeCharacter) Then
          Begin
          inc(iCursor);
          if (isDelimiterEscape(cnt[iCursor])) Then
          Begin
            buf.append(getDelimiterEscapeChar(cnt[iCursor]));
            inc(iCursor);
            if cnt[iCursor] <> FEscapeCharacter Then
              dec(iCursor); // cause it will be inc next. this is really an error, an improperly terminated escape sequence, but we do not report it
          End
          Else If (cnt[iCursor] = 'X') Then
          Begin
            Commit;
            readBinary(cell, cnt, iCursor);
          End
          Else
            Begin
            Commit;
            readEscape(cell, cnt, iCursor);
            End;
          End
        Else
          buf.append(cnt[iCursor]);
        inc(iCursor);
        End;
      Commit;
    finally
      buf.Free;
    end;
  End;
End;

Procedure TV2Parser.ReadBinary(cell : TV2Cell; Const cnt : String; var iCursor : integer);
var
  sBuffer : String;
  iStart : Integer;
Begin
  inc(iCursor); // skip the X at the start
  iStart := iCursor;
  While (iCursor <= Length(cnt)) And Not (cnt[iCursor] = FEscapeCharacter) Do
    Inc(iCursor);
  If (iCursor > length(cnt)) Then
    raise EER7Exception.Create('unterminated binary escape in '+cnt);
  sBuffer := copy(cnt, iStart, iCursor - iStart);
  if Length(sBuffer) mod 2 = 1 then
    If v2doPreventOddBinaryLengths in FOptions Then
      sBuffer := '0'+sBuffer
    Else
      raise EER7Exception.Create('Odd length of binary escape in "'+cnt+'"');

  cell.ContentList.add(TV2Content.create(ckBinary, HexDecode(sBuffer)));
End;

Procedure TV2Parser.ReadEscape(cell : TV2Cell; Const cnt : String; var iCursor : integer);
var
  iStart : Integer;
Begin
  iStart := iCursor;
  While (iCursor <= Length(cnt)) And Not (cnt[iCursor] = FEscapeCharacter) Do
    Inc(iCursor);
  If (iCursor > length(cnt)) Then
  Begin
    // very often turkeys sending us the escape character do not escape.
    // rather than raising the error, we are going to treat the escape as an escape
    iCursor := iStart - 1;
    cell.ContentList.add(TV2Content.create(ckString, FEscapeCharacter));
  End
  Else
    cell.ContentList.add(TV2Content.create(ckEscape, copy(cnt, iStart, iCursor - iStart)));
End;

Function TV2Parser.isDelimiterEscape(ch : Char) : Boolean;
Begin
  Result := (ch = 'F') or (ch = 'S') or (ch = 'E') or (ch = 'T') or (ch = 'R');
End;

procedure TV2Parser.generateIds(msg: TV2Message);
var
  st : TStringList;
  seg : TV2Segment;
  i, t : integer;
begin
  st := TStringList.Create;
  try
    st.Sorted := true;
    for seg in msg.segmentList do
    begin
      if st.Find(seg.code, i) then
        st.Objects[i] := TObject(integer(st.Objects[i])+1)
      else
        i := st.AddObject(seg.code, nil);
      t := integer(st.Objects[i]);
      if t = 0 then
        seg.id := seg.code
      else
        seg.id := seg.code+':'+inttostr(t);
      generateIds(seg);
    end;
  finally
    st.Free;
  end;
end;

procedure TV2Parser.generateIds(seg: TV2Segment);
var
  i : integer;
begin
  for i := 0 to seg.fieldList.Count - 1 do
  begin
    seg.fieldList[i].id := seg.id+'-'+inttostr(i+1);
    generateIds(seg.fieldList[i]);
  end;
end;

procedure TV2Parser.generateIds(fld: TV2Field);
var
  i : integer;
begin
  for i := 0 to fld.elementList.Count - 1 do
  begin
    fld.elementList[i].id := fld.id+':'+inttostr(i);
    generateIds(fld.elementList[i]);
  end;
end;

procedure TV2Parser.generateIds(cell: TV2Cell);
var
  i : integer;
begin
  for i := 0 to cell.componentList.Count - 1 do
  begin
    cell.componentList[i].id := cell.id+'-'+inttostr(i+1);
    generateIds(cell.componentList[i]);
  end;
  for i := 0 to cell.contentList.Count - 1 do
    cell.contentList[i].id := cell.id+'['+inttostr(i)+']';
end;

Function TV2Parser.getDelimiterEscapeChar(ch : Char) : Char;
Begin
  if (ch = 'E') Then
    Result := FEscapeCharacter
  else if (ch = 'F') Then
    Result := FFieldDelimiter
  else if (ch = 'S') Then
    Result := FComponentDelimiter
  else if (ch = 'T') Then
    Result := FSubComponentDelimiter
  else if (ch = 'R') Then
    Result := FRepetitionDelimiter
  else
    raise EER7Exception.Create('Illegal escape char '+ch);
End;

{ TV2Composer }

class procedure TV2Composer.compose(msg: TV2Message; dst: TStream; options: TV2ComposerOptions);
var
  b : TBytes;
begin
  b := composeBytes(msg, options);
  dst.Write(b[0], length(b));
end;

class procedure TV2Composer.compose(msg: TV2Message; dst: TFslStream; options: TV2ComposerOptions);
var
  b : TBytes;
begin
  b := composeBytes(msg, options);
  dst.Write(b[0], length(b));
end;

class procedure TV2Composer.compose(msg: TV2Message; dst: TFslBuffer; options: TV2ComposerOptions);
begin
  dst.AsBytes := composeBytes(msg, options);
end;

class function TV2Composer.composeBytes(msg: TV2Message; options: TV2ComposerOptions): TBytes;
begin
  result := TEncoding.UTF8.GetBytes(composeString(msg, options));
end;

class function TV2Composer.composeString(obj: TV2Object; options: TV2ComposerOptions): String;
var
  this : TV2Composer;
  b : TStringBuilder;
begin
  this := TV2Composer.Create;
  try
    this.FOptions := options;
    this.init;
    b := TStringBuilder.Create;
    try
      if obj is TV2Message then
        this.composeMessage(b, obj as TV2Message)
      else if obj is TV2Segment then
        this.composeSegment(b, obj as TV2Segment)
      else if obj is TV2Field then
        this.composeField(b, obj as TV2Field)
      else if obj is TV2Cell then
        this.composeCell(b, obj as TV2Cell, '^')
      else if obj is TV2Content then
        this.composeContent(b, obj as TV2Content);
      result := b.ToString;
    finally
      b.Free;
    end;
  finally
    this.free;
  end;
end;

procedure TV2Composer.composeCell(b: TStringBuilder; cell: TV2Cell; ch: AnsiChar);
var
  first : boolean;
  comp : TV2Cell;
  cnt : TV2Content;
begin
  first := true;
  if not cell.componentList.Empty then
  begin
    for comp in cell.componentList do
    begin
      if first then
        first := false
      else
        b.Append(ch);
      composeCell(b, comp, FSubComponentDelimiter);
    end;
  end
  else
  begin
    for cnt in cell.contentList do
    begin
      case cnt.kind of
        ckString: escape(b, cnt.value);
        ckNull: escape(b, cnt.value);// there's no need to escape, but it solves the AnsiChar warning
        ckBinary: composeBinary(b, cnt);
        ckEscape: composeEscape(b, cnt);
      end;
    end;
  end;
end;

procedure TV2Composer.composeContent(b: TStringBuilder; cnt: TV2Content);
begin
  case cnt.kind of
    ckString: b.Append(cnt.FValue);
    ckNull: b.Append('""');
    ckBinary: composeBinary(b, cnt);
    ckEscape: composeEscape(b, cnt);
  end;
end;

procedure TV2Composer.composeField(b: TStringBuilder; fld: TV2Field);
var
  first : boolean;
  cell : TV2Cell;
begin
  first := true;
  for cell in fld.elementList do
  begin
    if first then
      first := false
    else
      b.Append(FRepetitionDelimiter);
    composeCell(b, cell, FComponentDelimiter);
  end;
end;

procedure TV2Composer.composeMessage(b: TStringBuilder; msg: TV2Message);
var
  seg : TV2Segment;
begin
  for seg in msg.segmentList do
    composeSegment(b, seg);
end;

procedure TV2Composer.composeSegment(b: TStringBuilder; seg: TV2Segment);
var
  iLoop : integer;
  iStart : Integer;
  iEnd : integer;
begin
  b.Append(seg.code);
  iStart := 0;
  if StringArrayExists(['MSH', 'FHS', 'BHS'], String(seg.Code)) then
  begin
    b.Append(FFieldDelimiter);
    b.Append(FComponentDelimiter);
    b.Append(FRepetitionDelimiter);
    b.Append(FEscapeCharacter);
    b.Append(FSubComponentDelimiter);
    iStart := 2;
  end;
  iEnd := seg.fieldList.Count - 1;
  While (iEnd > iStart) and seg.FieldList[iEnd].isEmpty Do
    dec(iEnd);
  for iLoop := iStart to iEnd do
  begin
    b.Append(FFieldDelimiter);
    composeField(b, seg.fieldList[iloop]);
  end;
  if v2coExtraFieldDelimiter in FOptions then
    b.Append(FFieldDelimiter);
  b.Append(#13);
end;

class function TV2Composer.composeString(msg: TV2Message; options: TV2ComposerOptions): String;
var
  this : TV2Composer;
  b : TStringBuilder;
begin
  this := TV2Composer.Create;
  try
    this.FOptions := options;
    this.init;
    b := TStringBuilder.Create;
    try
      this.composeMessage(b, msg);
      result := b.ToString;
    finally
      b.Free;
    end;
  finally
    this.free;
  end;
end;

function TV2Composer.escape(b: TStringBuilder; src: String): AnsiString;
var
  ch : char;
begin
  for ch in src do
  begin
    if ansichar(ch) in [FFieldDelimiter, FEscapeCharacter, FComponentDelimiter, FRepetitionDelimiter, FSubComponentDelimiter] then
    begin
      b.Append(FEscapeCharacter);
      if ansichar(ch) = FFieldDelimiter then
        b.Append('F')
      else if ansichar(ch) = FComponentDelimiter then
        b.Append('S')
      else if ansichar(ch) = FSubComponentDelimiter then
        b.Append('T')
      else if ansichar(ch) = FEscapeCharacter then
        b.Append('E')
      else // if ansichar(ch) = FRepetitionDelimiter then
        b.Append('R');
      b.Append(FEscapeCharacter);
    end
    else if CharInSet(ch, [#10, #13, #9]) or (ord(ch) >= 128)  or ((ord(ch) >= 128) and (v2coEscapeExtendedCharacters in FOptions)) then
    begin
      b.Append(FEscapeCharacter);
      b.Append('X');
      b.Append(IntToHex(Ord(ch), 2));
      b.Append(FEscapeCharacter);
    end
    else
      b.Append(ch);
  end;
end;

procedure TV2Composer.composeEscape(b: TStringBuilder; cnt: TV2Content);
begin
  b.Append(FEscapeCharacter);
  b.Append(cnt.value);
  b.Append(FEscapeCharacter);
end;

procedure TV2Composer.composeBinary(b: TStringBuilder; cnt: TV2Content);
begin
  b.Append(FEscapeCharacter);
  b.Append('X');
  b.Append(HexEncode(cnt.value));
  b.Append(FEscapeCharacter);
end;

class function TV2Composer.composeBytes(obj: TV2Object; options: TV2ComposerOptions): TBytes;
begin
  result := TEncoding.UTF8.GetBytes(composeString(obj, options));
end;

procedure TV2Composer.init;
begin
  FEscapeCharacter := '\';
  FRepetitionDelimiter := '~';
  FFieldDelimiter := '|';
  FSubComponentDelimiter := '&';
  FComponentDelimiter := '^';
end;

{ TV2FHIRPathExtensions }

function TV2FHIRPathExtensions.isValidFunction(name: String): boolean;
begin
  result := (name = 'text') or (name = 'element') or (name = 'simple') ;
end;

function TV2FHIRPathExtensions.resolveConstant(context: TFHIRPathExecutionContext; s: String; var obj: TFHIRObject): boolean;
begin
  result := false;
end;

function TV2FHIRPathExtensions.functionApplies(context: TFHIRPathExecutionContext; focus: TFHIRSelectionList; name: String): boolean;
var
  item : TFHIRSelection;
begin
  result := false;
  for item in focus do
  begin
    if (item.value is TV2Cell) and (name = 'text') then
      exit(true);
    if (item.value is TV2Cell) and (name = 'simple') then
      exit(true);
    if (item.value is TV2Segment) and (name = 'element') then
      exit(true);
  end;
end;

function TV2FHIRPathExtensions.execute(context: TFHIRPathExecutionContext; focus: TFHIRObject; name: String; params: TFslList<TFHIRPathExpressionNodeV>; engine: TFHIRPathEngineV): TFHIRSelectionList;
var
  s : String;
  i : integer;
  sel : TFHIRSelectionList;
begin
  result := TFHIRSelectionList.Create;
  try
    if (focus is TV2Cell) and (name = 'text') then
      result.add(TFHIRObjectText.Create(TV2Cell(focus).text));
    if (focus is TV2Cell) and (name = 'simple') then
      result.add(TFHIRObjectText.Create(TV2Cell(focus).simple));
    if (focus is TV2Segment) and (name = 'element') then
    begin
      sel := engine.evaluate(context.appInfo, context.resource, params[0]);
      try
        s := engine.convertToString(sel);
        if StringIsInteger16(s) then
        begin
          i := StrToInt(s);
          result.add(TV2Segment(focus).element(i).link);
        end;
      finally
        sel.free;
      end;
    end;

    result.Link;
  finally
    result.Free;
  end;
end;

end.
