Unit FHIRParserBase;

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

{$WARN SYMBOL_DEPRECATED OFF}

Interface

uses
  {$IFDEF MACOS} OSXUtils, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Math, EncdDecd, Generics.Collections, System.Character, {$IFNDEF VER260} System.NetEncoding, {$ENDIF}
  DateSupport, StringSupport, DecimalSupport, EncodeSupport, BytesSupport, TextUtilities, GuidSupport,
  AdvBuffers, AdvStringLists,  AdvStringMatches, AdvVCLStreams, AdvStringBuilders, AdvGenerics, AdvStreams,
  ParserSupport, MXML, XmlBuilder, MXmlBuilder, AdvXmlBuilders, AdvJSON, TurtleParser,
  FHIRBase, FHIRSupport, FHIRLang, FHIRXhtml;

const
  ATOM_NS = 'http://www.w3.org/2005/Atom';
  GDATA_NS = 'http://schemas.google.com/g/2005';
  FHIR_JS =
    '<script type="text/javascript" src="/js/json2.js"></script>'+#13#10+
    '<script type="text/javascript" src="/js/statuspage.js"></script>'+#13#10+
    '<script type="text/javascript" src="/js/jquery-1.6.2.min.js"></script>'+#13#10+
    '<script type="text/javascript" src="/js/jquery-ui-1.8.16.custom.min.js"></script>'+#13#10+
    '<link rel="stylesheet" href="/css/jquery.ui.all.css">'+#13#10+
    '<script src="/js/jquery-1.6.2.js"></script>'+#13#10+
    '<script src="/js/jquery.ui.core.js"></script>'+#13#10+
    '<script src="/js/jquery.ui.widget.js"></script>'+#13#10+
    '<script src="/js/jquery.ui.mouse.js"></script>'+#13#10+
    '<script src="/js/jquery.ui.resizable.js"></script>'+#13#10+
    '<script src="/js/jquery.ui.draggable.js"></script>'+#13#10+
    '<script type="text/javascript" src="/js/jtip.js"></script>'+#13#10+
    '<script type="text/javascript" src="/js/jcookie.js"></script>'+#13#10+
    '<script type="text/javascript" src="/js/hl7connect.js"></script>'+#13#10+
    '<script type="text/javascript" src="/js/fhir-gw.js"></script>'+#13#10;
  MAP_ATTR_NAME = 'B88BF977DA9543B8A5915C84A70F03F7';

  {$IFDEF FHIR3}
  FHIR_SPEC_URL = 'http://hl7-fhir.github.io/';
  {$ELSE}
  FHIR_SPEC_URL = 'http://hl7.org/fhir';
  {$ENDIF}
  FHIR_TTL_URI_BASE = 'http://hl7.org/fhir/';

Type
  TFHIRParser = class abstract (TFHIRObject)
  private
    FAllowUnknownContent: Boolean;
    Fresource: TFhirResourceV;
    FSource: TStream;
    FLang: String;
    FParserPolicy : TFHIRXhtmlParserPolicy;
    FKeepLineNumbers : boolean;
    FTimeLimit: Cardinal;
    FTimeToAbort : Cardinal;
    FIgnoreHtml: Boolean;
    FWorker : TFHIRWorkerContextV;
    procedure SetResource(const Value: TFhirResourceV);
    procedure start;
    procedure checkTimeOut;
  protected
    procedure checkDateFormat(s : string);
    Function toTDateTimeEx(s : String) : TDateTimeEx;
    function toTBytes(s : String) : TBytes;
    function StringArrayToCommaString(Const aNames : Array Of String) : String;
    function GetFormat: TFHIRFormat; virtual; abstract;
  public
    Constructor Create(worker : TFHIRWorkerContextV; lang : String); Virtual;
    Destructor Destroy; Override;
    property source : TStream read FSource write FSource;
    procedure Parse; Virtual; abstract;
    property resource : TFhirResourceV read Fresource write SetResource;

    function ParseDT(rootName : String; type_ : TClass) : TFHIRObject; Virtual; abstract;

    procedure ParseFile(filename : String); overload;
    Property AllowUnknownContent : Boolean read FAllowUnknownContent write FAllowUnknownContent;
    Property Lang : String read FLang write FLang;
    property ParserPolicy : TFHIRXhtmlParserPolicy read FParserPolicy write FParserPolicy;
    property KeepLineNumbers : boolean read FKeepLineNumbers write FKeepLineNumbers;
    property timeLimit : Cardinal read FTimeLimit write FTimeLimit;
    property Format : TFHIRFormat read GetFormat;
    property IgnoreHtml : Boolean read FIgnoreHtml write FIgnoreHtml;
  end;

  TFHIRParserClass = class of TFHIRParser;

  TFHIRXmlParserBase = class (TFHIRParser)
  Private
    FElement: TMXmlElement;
    FComments : TAdvStringList;

    Function LoadXml(stream : TStream) : TMXmlDocument;
    Function PathForElement(element : TMXmlElement) : String;
    procedure SetFhirElement(const Value: TMXmlElement);

  Protected
    Function GetAttribute(element : TMXmlElement; const name : String) : String;
    function FirstChild(element : TMXmlElement) : TMXmlElement;
    function NextSibling(element : TMXmlElement) : TMXmlElement;
    procedure TakeCommentsStart(element : TFHIRObject);
    procedure TakeCommentsEnd(element : TFHIRObject);
    procedure closeOutElement(result : TFHIRObject; element : TMXmlElement);
    procedure GetObjectLocation(obj : TFHIRObject; element : TMXmlElement);

    Function ParseXHtmlNode(element : TMXmlElement; path : String) : TFhirXHtmlNode; overload;

    Procedure UnknownContent(element : TMXmlElement; path : String);

    Procedure XmlError(const sPath, sMessage : String);

    Function ParseDomainResourceV(element: TMXmlElement; path : String) : TFhirResourceV;
    Function ParseResourceV(element : TMXmlElement; path : String) : TFhirResourceV; Virtual;
    function ParseDataTypeV(element : TMXmlElement; name : String; type_ : TClass) : TFHIRObject; virtual;

    Procedure checkOtherAttributes(value : TMXmlElement; path : String);
    function GetFormat: TFHIRFormat; override;
  Public
    Destructor Destroy; Override;
    procedure Parse; Override;
    function ParseDT(rootName : String; type_ : TClass) : TFHIRObject; override;

    property Element : TMXmlElement read FElement write SeTFhirElement;
  End;


  TJsonObjectHandler = procedure (jsn : TJsonObject; ctxt : TFHIRObjectList) of object;
  TJsonObjectPrimitiveHandler = procedure (value : String; jsn : TJsonObject; ctxt : TFHIRObjectList) of object;
  TJsonObjectEnumHandler = procedure (path, value : String; jsn : TJsonObject; ctxt : TFHIRObjectList; Const aSystems, aNames : Array Of String) of object;

  TFHIRJsonParserBase = class (TFHIRParser)
  Protected
    Function ParseXHtmlNode(path, value : String) : TFhirXHtmlNode;

    Function ParseResourceV(jsn : TJsonObject) : TFhirResourceV; Virtual;
    function ParseDataTypeV(jsn : TJsonObject; name : String; type_ : TClass) : TFHIRObject; virtual;
    procedure ParseComments(base : TFHIRObject; jsn : TJsonObject);

    procedure iterateArray(arr : TJsonArray; ctxt : TFHIRObjectList; handler : TJsonObjectHandler);
    procedure iteratePrimitiveArray(arr1, arr2 : TJsonArray; ctxt : TFHIRObjectList; handler : TJsonObjectPrimitiveHandler);
    procedure iterateEnumArray(arr1, arr2 : TJsonArray; path : String; ctxt : TFHIRObjectList; handler : TJsonObjectEnumHandler; Const aSystems, aNames : Array Of String);

    // handlers
    procedure parseDomainResource(jsn : TJsonObject; ctxt : TFHIRObjectList);
    function GetFormat: TFHIRFormat; override;
  Public
    procedure Parse; Overload; Override;
    procedure Parse(obj : TJsonObject); Reintroduce; Overload; Virtual;
    function ParseDT(rootName : String; type_ : TClass) : TFHIRObject; override;
  End;

  {$IFNDEF FHIR2}
  TFHIRTurtleParserBase = class (TFHIRParser)
  protected
    Fdoc : TTurtleDocument;

    function GetFormat: TFHIRFormat; override;
    function rdfsType(obj : TTurtleComplex) : string;
    function ParseResourceV(obj : TTurtleComplex) : TFHIRResourceV; virtual;
    function ParseXHtmlNode(literal : String) : TFhirXHtmlNode;
  public
    procedure Parse; Overload; Override;
    function ParseDT(rootName : String; type_ : TClass) : TFHIRObject; override;
  end;
  {$ENDIF}

  TFHIRTextParser = class (TFHIRParser)
  protected
    function GetFormat: TFHIRFormat; override;
  Public
    procedure Parse; Overload; Override;
  End;

  TFHIRComposer = {abstract} class (TAdvObject)
  private
    FLang: String;
    FSummaryOption: TFHIRSummaryOption;
    FNoHeader: Boolean;
    FElements : TStringList;
    FLogId: string;
  protected
    FWorker : TFHIRWorkerContextV;
    FStyle : TFHIROutputStyle;

    function isPretty : boolean;
    function isCanonical : boolean;
    function doCompose(name : String) : boolean;

    Procedure ComposeResourceV(xml : TXmlBuilder; oResource : TFhirResourceV); overload; virtual;
    procedure ComposeXHtmlNode(xml : TXmlBuilder; name : String; node: TFhirXHtmlNode); overload;

    function ResourceMediaType: String; virtual;

    function asString(value : TDateTimeEx):String; overload;
    function asString(value : TBytes):String; overload;
    function asString(value : string):String; overload;
    function asString(value : boolean):String; overload;
    procedure ComposeItems(stream : TStream; name : String; items : TFHIRObjectList); Virtual;
    procedure ComposeItem(stream : TStream; name : String; item : TFHIRObject); Virtual;
  public
    Constructor Create(worker : TFHIRWorkerContextV; style : TFHIROutputStyle; lang : String); Virtual;
    Destructor Destroy; override;
    Procedure Compose(stream : TStream; oResource : TFhirResourceV); Overload; Virtual;

    function Compose(oResource : TFhirResourceV) : String; Overload;
    function Compose(name : String; items : TFHIRObjectList): String; Overload;
    function Compose(name : String; item : TFHIRObject): String; Overload;

    Function MimeType : String; virtual;
    function Extension : String; virtual;
    Property Lang : String read FLang write FLang;
    Property SummaryOption : TFHIRSummaryOption read FSummaryOption write FSummaryOption;
    property NoHeader : Boolean read FNoHeader write FNoHeader;
    property ElementToCompose : TStringList read FElements;
    property LogId : string read FLogId write FLogId;
  End;

  TFHIRComposerClass = class of TFHIRComposer;

  TFHIRXmlComposerBase = class (TFHIRComposer)
  private
    FComment: String;
  Protected
    procedure commentsStart(xml : TXmlBuilder; value : TFHIRObject);
    procedure commentsEnd(xml : TXmlBuilder; value : TFHIRObject);
    Procedure Attribute(xml : TXmlBuilder; name, value : String);
    Procedure Text(xml : TXmlBuilder; name, value : String);
    procedure closeOutElement(xml : TXmlBuilder; value : TFHIRObject);
    Procedure ComposeDomainResource(xml : TXmlBuilder; name : String; value : TFhirResourceV);
    Procedure ComposeInnerResource(xml : TXmlBuilder; name : String; holder : TFhirResourceV; value : TFhirResourceV); overload;
    Procedure ComposeInnerResource(xml : TXmlBuilder; name : String; holder : TFHIRObject; value : TFhirResourceV); overload;
    procedure ComposeItems(stream : TStream; name : String; items : TFHIRObjectList); override;
    procedure ComposeItem(stream : TStream; name : String; item : TFHIRObject); override;
  Public
    Procedure Compose(stream : TStream; oResource : TFhirResourceV); Override;
    Procedure Compose(node : TMXmlElement; oResource : TFhirResourceV); Overload;
    Function MimeType : String; Override;
    function Extension : String; Override;
    Property Comment : String read FComment write FComment;
    procedure ComposeBase(xml : TXmlBuilder; name : String; base : TFHIRObject); virtual;
  End;

  TFHIRJsonComposerBase = class (TFHIRComposer)
  private
    FComments : Boolean;
  Protected
    Procedure PropNull(json : TJSONWriter; name : String); overload;
    Procedure Prop(json : TJSONWriter; name, value : String); overload;
    Procedure PropNumber(json : TJSONWriter; name, value : String); overload;
    Procedure Prop(json : TJSONWriter; name : String; value : boolean); overload;
    Procedure ComposeXHtmlNode(json : TJSONWriter; name : String; value : TFhirXHtmlNode); overload;

    Procedure composeComments(json : TJSONWriter; base : TFHIRObject);
    procedure ComposeDomainResource(json : TJSONWriter; name : String; oResource : TFhirResourceV); overload; virtual;
    procedure ComposeInnerResource(json : TJSONWriter; name : String; holder : TFHIRObject; oResource : TFhirResourceV); overload; virtual;
    procedure ComposeInnerResource(json : TJSONWriter; name : String; holder : TFhirResourceV; oResource : TFhirResourceV); overload; virtual;
    Procedure ComposeResourceV(json : TJSONWriter; oResource : TFhirResourceV); overload; virtual;
//    Procedure ComposeResourceV(xml : TXmlBuilder; oResource : TFhirResourceV); overload;
    procedure ComposeItems(stream : TStream; name : String; items : TFHIRObjectList); override;
    procedure ComposeItem(stream : TStream; name : String; item : TFHIRObject); override;
  Public
    Procedure Compose(stream : TStream; oResource : TFhirResourceV); Override;
    Procedure Compose(stream : TAdvStream; oResource : TFhirResourceV); overload;
    Procedure Compose(json: TJSONWriter; oResource : TFhirResourceV); Overload;
    Function MimeType : String; Override;
    function Extension : String; Override;
    Property Comments : Boolean read FComments write FComments;
    procedure ComposeBase(json: TJSONWriter; name : String; base : TFHIRObject); virtual;
  End;

  {$IFNDEF FHIR2}
  TFHIRTurtleComposerBase = class (TFHIRComposer)
  private
    FURL: String;
    FTtl : TTurtleDocument;
  protected
    procedure ComposeXHtmlNode(parent : TTurtleComplex; parentType, name : String; value: TFhirXHtmlNode; useType : boolean; index : integer); overload;

    function dateXsdType(value : TDateTimeEx) : string;
    Procedure ComposeResource(xml : TXmlBuilder; oResource : TFhirResourceV); overload;
    procedure ComposeItems(stream : TStream; name : String; items : TFHIRObjectList); override;
    procedure ComposeItem(stream : TStream; name : String; item : TFHIRObject); override;

    Procedure ComposeResourceV(parent :  TTurtleComplex; oResource : TFhirResourceV); overload; virtual;
    Procedure ComposeInnerResource(this : TTurtleComplex; parentType, name : String; elem : TFhirResourceV; useType : boolean; index : integer); overload;
  public
    Procedure Compose(stream : TStream; oResource : TFhirResourceV); Override;
    Function MimeType : String; Override;
    property URL : String read FURL write FURL;
    function Extension : String; Override;
  end;
  {$ENDIF}

  TFHIRTextComposer = class (TFHIRComposer)
  private
  protected
    function ResourceMediaType: String; override;
  public
    Procedure ComposeResource(xml : TXmlBuilder; oResource : TFhirResourceV); overload;
    Procedure Compose(stream : TStream; oResource : TFhirResourceV); Override;
    Function MimeType : String; Override;
    function Extension : String; Override;
  end;

Implementation

uses
  RegularExpressions;

Function TFHIRXmlParserBase.LoadXml(stream : TStream) : TMXmlDocument;
begin
  start;
  result := TMXmlParser.parse(stream, [xpResolveNamespaces]);
end;

procedure TFHIRXmlParserBase.Parse;
var
  xml : TMXmlDocument;
  root : TMXmlElement;
begin
  xml := nil;
  try
    FComments := TAdvStringList.create;
    try
      if (Element = nil) then
      begin
        xml := LoadXml(Source);
        root := xml.document;
      end
      else
        root := element;

      if root.namespaceURI = FHIR_NS Then
        resource := ParseResourceV(root, '')
      else
        XmlError('/', StringFormat(GetFhirMessage('MSG_WRONG_NS', lang), [root.namespaceURI]))
    finally
      FComments.Free;
    end;
  finally
    xml.free;
  end;
end;

function TFHIRXmlParserBase.ParseResourceV(element: TMXmlElement; path : String): TFhirResourceV;
begin
  raise exception.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;


{ TFHIRJsonParserBase }


procedure TFHIRJsonParserBase.Parse;
var
  obj : TJsonObject;
begin
  start;
  obj := TJSONParser.Parse(source, timeLimit);
  try
    resource := ParseResourceV(obj);
  finally
    obj.Free;
  end;
end;


function TFHIRJsonParserBase.ParseDT(rootName: String; type_: TClass): TFHIRObject;
var
  obj : TJsonObject;
begin
  start;
  obj := TJSONParser.Parse(source, timelimit);
  try
    result := ParseDataTypeV(obj, rootName, type_);
  finally
    obj.Free;
  end;
end;

function TFHIRJsonParserBase.ParseResourceV(jsn : TJsonObject): TFhirResourceV;
begin
  raise exception.create('don''t use TFHIRJsonParserBase directly - use TFHIRJsonParser');
end;

function TFHIRXmlParserBase.PathForElement(element: TMXmlElement): String;
begin
  result := '';
  while element <> nil Do
  Begin
    insert(element.localName+'/', result, 1);
    element := element.parent;
  End;
  result := copy(result, 1, length(result)-1);
end;

procedure TFHIRXmlParserBase.UnknownContent(element: TMXmlElement; path : String);
begin
  if Not AllowUnknownContent Then
    XmlError(PathForElement(element), StringFormat(GetFhirMessage('MSG_UNKNOWN_CONTENT', lang), [element.Name, path]));
end;

procedure TFHIRXmlParserBase.XmlError(const sPath, sMessage: String);
begin

  Raise Exception.Create(StringFormat(GetFhirMessage('MSG_ERROR_PARSING', lang), [sMessage+' @ '+sPath]));
end;

function TFHIRJsonParserBase.ParseXHtmlNode(path, value : String): TFhirXHtmlNode;
begin
  if FIgnoreHtml then
    result := nil
  else
    result := TFHIRXhtmlParser.parse(lang, FParserPolicy, [], value);
end;



function TFHIRXmlParserBase.ParseXHtmlNode(element: TMXmlElement; path : String): TFhirXHtmlNode;
begin
  if not AllowUnknownContent and (element.namespaceURI <> XHTML_NS) Then
    XmlError(PathForElement(element), StringFormat(GetFhirMessage('MSG_WRONG_NS', lang), [element.namespaceURI]));
  if FIgnoreHtml then
    result := nil
  else
    result := TFHIRXhtmlParser.Parse(lang, FParserPolicy, [], element, path, FHIR_NS);
end;

procedure TFHIRJsonParserBase.Parse(obj: TJsonObject);
begin
  start;
  resource := ParseResourceV(obj);
end;

procedure TFHIRJsonParserBase.ParseComments(base: TFHIRObject; jsn : TJsonObject);
begin
  checkTimeOut;
end;

function TFHIRJsonParserBase.ParseDataTypeV(jsn: TJsonObject; name: String; type_: TClass): TFHIRObject;
begin
  raise exception.create('don''t use TFHIRJsonParserBase directly - use TFHIRJsonParser');
end;

procedure TFHIRJsonParserBase.parseDomainResource(jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseResourceV(jsn));
end;

function TFHIRJsonParserBase.GetFormat: TFHIRFormat;
begin
  result := ffJson;
end;

procedure TFHIRJsonParserBase.iterateArray(arr : TJsonArray; ctxt : TFHIRObjectList; handler : TJsonObjectHandler);
var
  i : integer;
begin
  if arr <> nil then
  begin
    for i := 0 to arr.Count - 1 do
      if (arr.Obj[i] <> nil) then
        handler(arr.Obj[i], ctxt);
  end;
end;

procedure TFHIRJsonParserBase.iteratePrimitiveArray(arr1, arr2 : TJsonArray; ctxt : TFHIRObjectList; handler : TJsonObjectPrimitiveHandler);
var
  i : integer;
begin
  if (arr1 <> nil) or (arr2 <> nil) then
  begin
    for i := 0 to max(arr1.Count, arr2.Count) - 1 do
      handler(arr1.Value[i], arr2.Obj[i], ctxt);
  end;
end;

procedure TFHIRJsonParserBase.iterateEnumArray(arr1, arr2 : TJsonArray; path : String; ctxt : TFHIRObjectList; handler : TJsonObjectEnumHandler; Const aSystems, aNames : Array Of String);
var
  i : integer;
begin
  if (arr1 <> nil) or (arr2 <> nil) then
  begin
    for i := 0 to max(arr1.Count, arr2.Count) - 1 do
      handler(path+'['+inttostr(i+1)+']', arr1.Value[i], arr2.Obj[i], ctxt, aSystems, aNames);
  end;
end;


{ TFHIRXmlComposerBase }

procedure TFHIRXmlComposerBase.Compose(stream: TStream; oResource: TFhirResourceV);
var
  xml : TXmlBuilder;
begin
  xml := TAdvXmlBuilder.Create;
  try
    xml.IsPretty := isPretty;
    xml.NoHeader := NoHeader;
    if isCanonical then
      TAdvXmlBuilder(xml).CanonicalEntities := true;
    xml.CurrentNamespaces.DefaultNS := FHIR_NS;
    xml.Start;
    if not isCanonical and (FComment <> '') then
      xml.Comment(FComment);
    ComposeResourceV(xml, oResource);
    xml.Finish;
    xml.Build(stream);
  finally
    xml.Free;
  end;
end;

function canonicalise(s : string) : string;
var
  i, j : integer;
  lastWS : Boolean;
begin
  lastWS := false;
  SetLength(result, length(s));
  i := 1;
  j := 1;
  while i <= length(s) do
  begin
    if not IsWhiteSpace(s[i]) then
    begin
      lastWS := false;
      result[j] := s[i];
      inc(j);
    end
    else if not lastWS then
    begin
      lastWS := true;
      result[j] := ' ';
      inc(j);
    end;
    inc(i);
  end;
  SetLength(result, j-1);
end;

procedure TFHIRXmlComposerBase.Attribute(xml : TXmlBuilder; name, value: String);
begin
  if value <> '' Then
    if isCanonical then
      xml.AddAttribute(name, canonicalise(value))
    else
      xml.AddAttribute(name, value);
end;

procedure TFHIRXmlComposerBase.Compose(node: TMXmlElement; oResource: TFhirResourceV);
var
  xml : TXmlBuilder;
begin
  xml := TMXmlBuilder.Create;
  try
    TMXmlBuilder(xml).Start(node);
    xml.CurrentNamespaces.DefaultNS := FHIR_NS;
    if FComment <> '' then
      xml.Comment(FComment);
    ComposeResourceV(xml, oResource);
    xml.Finish;
  finally
    xml.Free;
  end;
end;

procedure TFHIRXmlComposerBase.ComposeBase(xml: TXmlBuilder; name: String; base: TFHIRObject);
begin

end;

procedure TFHIRXmlComposerBase.Text(xml : TXmlBuilder; name, value: String);
begin
  if value <> '' Then
    xml.TagText(name, value);
end;


function TFHIRXmlComposerBase.MimeType: String;
begin
  {$IFDEF FHIR2}
  result := 'application/xml+fhir; charset=UTF-8';
  {$ELSE}
  result := 'application/fhir+xml; charset=UTF-8';
  {$ENDIF} 
end;

procedure TFHIRXmlComposerBase.commentsStart(xml: TXmlBuilder; value: TFHIRObject);
var
  i : integer;
begin
  if isCanonical then
    exit;

  if value = nil then
    exit;

  if not value.HasXmlCommentsStart then
    exit;

  for i := 0 to value.Xml_commentsStart.count - 1 do
    xml.Comment(value.Xml_commentsStart[i]);
end;

procedure TFHIRXmlComposerBase.commentsEnd(xml: TXmlBuilder; value: TFHIRObject);
var
  i : integer;
begin
  if isCanonical then
    exit;

  if not value.HasXmlCommentsEnd then
    exit;

  for i := 0 to value.Xml_commentsEnd.count - 1 do
    xml.Comment(value.Xml_commentsEnd[i]);
end;

procedure TFHIRXmlComposerBase.closeOutElement(xml: TXmlBuilder; value: TFHIRObject);
begin
  commentsEnd(xml, value);
end;

procedure TFHIRXmlComposerBase.composeDomainResource(xml: TXmlBuilder; name: String; value: TFhirResourceV);
begin
  xml.open(name);
  ComposeResourceV(xml, value);
  xml.close(name);
end;


procedure TFHIRXmlComposerBase.ComposeInnerResource(xml: TXmlBuilder; name: String; holder: TFhirResourceV; value: TFhirResourceV);
begin
  if value <> nil then
  begin
    xml.open(name);
    ComposeResourceV(xml, value);
    xml.close(name);
  end;
end;

{ TFHIRJsonComposerBase }


procedure TFHIRJsonComposerBase.Compose(stream: TStream; oResource: TFhirResourceV);
var
  oStream : TAdvVCLStream;
  json : TJSONWriter;
begin
  if isCanonical then
    json := TJsonWriterCanonical.create
  else
    json := TJsonWriterDirect.create;
  try
    oStream := TAdvVCLStream.Create;
    json.Stream := oStream;
    oStream.Stream := stream;
    json.HasWhitespace := isPretty;
    json.Start;
    ComposeResourceV(json, oResource);
    json.Finish;
  finally
    json.free;
  end;
end;

procedure TFHIRJsonComposerBase.Prop(json : TJSONWriter; name, value: String);
begin
  if value <> '' Then
    if isCanonical then
      json.Value(name, canonicalise(value))
    else
      json.Value(name, value);
end;

procedure TFHIRJsonComposerBase.PropNumber(json : TJSONWriter; name, value: String);
begin
  if value <> '' Then
    json.ValueNumber(name, value);
end;

procedure TFHIRJsonComposerBase.Compose(json : TJSONWriter; oResource: TFhirResourceV);
begin
  json := json.Link;
  ComposeResourceV(json, oResource);
end;

procedure TFHIRJsonComposerBase.Compose(stream: TAdvStream; oResource: TFhirResourceV);
var
  v : TVCLStream;
begin
  v :=  TVCLStream.Create(stream.Link);
  try
    Compose(v, oResource);
  finally
    v.Free;
  end;
end;

procedure TFHIRJsonComposerBase.ComposeBase(json: TJSONWriter; name: String; base: TFHIRObject);
begin
  json.ValueObject(name);
  json.Value('test', 'value');
  json.FinishObject;
end;

procedure TFHIRJsonComposerBase.ComposeResourceV(json : TJSONWriter; oResource: TFhirResourceV);
begin
  raise exception.create('don''t use TFHIRJsonComposerBase directly - use TFHIRJsonComposer');
end;

{Procedure TFHIRJsonComposerBase.ComposeResourceV(xml : TXmlBuilder; oResource : TFhirResourceV);
var
  s : TBytesStream;
begin
  s := TBytesStream.Create();
  try
    compose(s, oResource);
    xml.Text(TEncoding.UTF8.getString(s.bytes, 0, s.size));
  finally
    s.free;
  end;
end;
}

procedure TFHIRJsonComposerBase.ComposeXHtmlNode(json : TJSONWriter; name: String; value: TFhirXHtmlNode);
begin
  if value = nil then
    exit;

  json.value(name, TFHIRXhtmlParser.Compose(value, isCanonical));
end;


function TFHIRJsonComposerBase.Extension: String;
begin
  result := '.json';
end;

function TFHIRJsonComposerBase.MimeType: String;
begin
 {$IFDEF FHIR2}
 result := 'application/json+fhir; charset=UTF-8';
 {$ELSE}
 result := 'application/fhir+json';
 {$ENDIF}
end;




procedure TFHIRJsonComposerBase.composeDomainResource(json: TJSONWriter; name: String; oResource: TFhirResourceV);
begin
  json.ValueObject('');
  ComposeResourceV(json, oResource);
  json.FinishObject;
end;

procedure TFHIRJsonComposerBase.ComposeInnerResource(json: TJSONWriter; name: String; holder: TFhirResourceV; oResource: TFhirResourceV);
begin
  if oResource <> nil then
  begin
    json.ValueObject(name);
    ComposeResourceV(json, oResource);
    json.FinishObject;
  end;
end;

procedure TFHIRJsonComposerBase.ComposeItem(stream: TStream; name: String; item: TFHIRObject);
var
  oStream : TAdvVCLStream;
  json : TJSONWriter;
begin
  json := TJsonWriterDirect.create;
  try
    oStream := TAdvVCLStream.Create;
    json.Stream := oStream;
    oStream.Stream := stream;
    json.HasWhitespace := isPretty;
    json.Start;
    ComposeBase(json, name, item);
    json.Finish;
  finally
    json.free;
  end;
end;

procedure TFHIRJsonComposerBase.ComposeItems(stream: TStream; name: String; items: TFHIRObjectList);
var
  oStream : TAdvVCLStream;
  json : TJSONWriter;
  base : TFHIRObject;
begin
  json := TJsonWriterDirect.create;
  try
    oStream := TAdvVCLStream.Create;
    json.Stream := oStream;
    oStream.Stream := stream;
    json.HasWhitespace := isPretty;
    json.Start;
    json.ValueArray(name);
    for base in items do
      ComposeBase(json, '', base);
    json.FinishArray;
    json.Finish;
  finally
    json.free;
  end;
end;

procedure TFHIRJsonComposerBase.composeInnerResource(json: TJSONWriter; name: String; holder : TFHIRObject; oResource: TFhirResourceV);
var
  blob : TAdvBuffer;
  bytes : TBytes;
begin
  if (holder <> nil) and (holder.Tag <> nil) and json.canInject then
  begin
    blob := holder.Tag as TAdvBuffer;
    bytes := blob.AsBytes;
    json.ValueBytes(name, bytes);
  end
  else if oResource <> nil then
  begin
    json.ValueObject(name);
    ComposeResourceV(json, oResource);
    json.FinishObject;
  end;
end;

procedure TFHIRJsonComposerBase.composeComments(json: TJSONWriter; base: TFHIRObject);
begin
  if not FComments then
    exit;
end;

procedure TFHIRJsonComposerBase.Prop(json: TJSONWriter; name: String; value: boolean);
begin
  json.Value(name, value);
end;

procedure TFHIRJsonComposerBase.PropNull(json: TJSONWriter; name: String);
begin
  json.ValueNull(name);
end;


{ TFHIRParser }

procedure TFHIRParser.checkDateFormat(s: string);
var
  ok : boolean;
begin
  ok := false;
  if (length(s) = 4) and StringIsCardinal16(s) then
    ok := true
  else if (length(s) = 7) and (s[5] = '-') and
          StringIsCardinal16(copy(s, 1, 4)) and StringIsCardinal16(copy(s, 5, 2)) then
    ok := true
  else if (length(s) = 10) and (s[5] = '-') and (s[8] = '-') and
          StringIsCardinal16(copy(s, 1, 4)) and StringIsCardinal16(copy(s, 6, 2)) and StringIsCardinal16(copy(s, 9, 2)) then
    ok := true
  else if (length(s) > 11) and (s[5] = '-') and (s[8] = '-') and (s[11] = 'T') and
          StringIsCardinal16(copy(s, 1, 4)) and StringIsCardinal16(copy(s, 6, 2)) and StringIsCardinal16(copy(s, 9, 2)) then
  begin
    if (length(s) = 16) and (s[14] = '-') and StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) then
      ok := true
    else if (length(s) = 19) and (s[14] = '-') and (s[17] = '-') and
          StringIsCardinal16(copy(s, 12, 2)) and StringIsCardinal16(copy(s, 15, 2)) and StringIsCardinal16(copy(s, 18, 2)) then
      ok := true;
  end;
  if not ok then
    raise exception.create('The Date value '+s+' is not in the correct format (Xml Date Format required)');
end;

procedure TFHIRParser.checkTimeOut;
begin
  if (FTimeToAbort > 0) and (FTimeToAbort < GetTickCount) then
    abort;
end;

constructor TFHIRParser.Create(worker : TFHIRWorkerContextV; lang: String);
begin
  Inherited Create;
  FLang := lang;
  FWorker := worker;
end;

destructor TFHIRParser.Destroy;
begin
  Fresource.Free;
  Fworker.Free;
  inherited;
end;


procedure TFHIRParser.ParseFile(filename: String);
var
  f : TFileStream;
begin
  f := TFIleStream.Create(filename, fmOpenRead + fmShareDenyWrite);
  try
    source := f;
    parse;
  finally
    f.Free;
  end;
end;

procedure TFHIRParser.SetResource(const Value: TFhirResourceV);
begin
  Fresource.Free;
  Fresource := Value;
end;

procedure TFHIRParser.start;
begin
  if FTimeLimit = 0 then
    FTimeToAbort := 0
  else
    FTimeToAbort := GetTickCount + FTimeLimit;
end;

procedure TFHIRXmlParserBase.SeTFhirElement(const Value: TMXmlElement);
begin
  FElement.Free;
  FElement := Value;
end;

{atom }

procedure TFHIRComposer.Compose(stream: TStream; oResource: TFhirResourceV);
begin
  raise Exception.Create('Error: call to TFHIRComposer.Compose(stream: TStream; oResource: TFhirResource; isPretty: Boolean; links: TFhirBundleLinkList)');
end;


function TFHIRComposer.Compose(name : String; items : TFHIRObjectList): String;
var
  stream : TBytesStream;
begin
  stream := TBytesStream.create;
  try
    composeItems(stream, name, items);
    result := TEncoding.UTF8.GetString(copy(stream.Bytes, 0, stream.position));
  finally
    stream.Free;
  end;
end;

function TFHIRComposer.asString(value: string): String;
begin
  result := value;
end;

function TFHIRComposer.asString(value: boolean): String;
begin
  if value then
    result := 'true'
  else
    result := 'false';
end;

function TFHIRComposer.Compose(name : String; item : TFHIRObject): String;
var
  stream : TBytesStream;
begin
  stream := TBytesStream.create;
  try
    composeItem(stream, name, item);
    result := TEncoding.UTF8.GetString(copy(stream.Bytes, 0, stream.position));
  finally
    stream.Free;
  end;
end;


procedure TFHIRComposer.ComposeItem(stream: TStream; name: String; item: TFHIRObject);
begin
  raise Exception.Create('ComposeExpression is Not supported for '+className);
end;

procedure TFHIRComposer.ComposeItems(stream: TStream; name: String; items: TFHIRObjectList);
begin
  raise Exception.Create('ComposeExpression is Not supported for '+className);
end;

procedure TFHIRComposer.ComposeResourceV(xml : TXmlBuilder; oResource: TFhirResourceV);
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRComposer.ComposeXHtmlNode(xml: TXmlBuilder; name: String; node: TFhirXHtmlNode);
begin
  TFHIRXhtmlParser.Compose(node, xml);
end;

procedure TFHIRXmlComposerBase.ComposeInnerResource(xml: TXmlBuilder; name: String; holder : TFHIRObject; value: TFhirResourceV);
var
  blob : TAdvBuffer;
begin
  if (holder <> nil) and (holder.Tag <> nil) then
  begin
    blob := holder.Tag as TAdvBuffer;
    xml.open(name);
    xml.inject(blob.AsBytes);
    xml.close(name);
  end
  else if value <> nil then
  begin
    xml.open(name);
    ComposeResourceV(xml, value);
    xml.close(name);
  end;
end;

procedure TFHIRXmlComposerBase.ComposeItem(stream: TStream; name: String; item: TFHIRObject);
var
  xml : TXmlBuilder;
begin
  xml := TAdvXmlBuilder.Create;
  try
    xml.IsPretty := isPretty;
    xml.NoHeader := NoHeader;
    xml.CurrentNamespaces.DefaultNS := FHIR_NS;
    xml.Start;
    ComposeBase(xml, name, item);
    xml.Finish;
    xml.Build(stream);
  finally
    xml.Free;
  end;
end;

procedure TFHIRXmlComposerBase.ComposeItems(stream: TStream; name: String; items: TFHIRObjectList);
var
  xml : TXmlBuilder;
  item : TFHIRObject;
begin
  xml := TAdvXmlBuilder.Create;
  try
    xml.IsPretty := isPretty;
    xml.NoHeader := NoHeader;
    xml.CurrentNamespaces.DefaultNS := FHIR_NS;
    xml.Start;
    if items <> nil then
      xml.addattribute('count', inttostr(items.count))
    else
      xml.addattribute('count', 'nil');
    xml.Open(name);
    for item in items do
      ComposeBase(xml, item.fhirtype, item);
    xml.Close(name);
    xml.Finish;
    xml.Build(stream);
  finally
    xml.Free;
  end;
end;

function TFHIRXmlComposerBase.Extension: String;
begin
  result := '.xml';
end;

function TFHIRComposer.MimeType: String;
begin
  result := '??';
end;

function paramForScheme(scheme : String): String;
begin
  if scheme = 'http://hl7.org/fhir/tag' then
    result := '_tag'
  else if scheme = 'http://hl7.org/fhir/tag/profile' then
    result := '_profile'
  else if scheme = 'http://hl7.org/fhir/tag/security' then
    result := '_security'
  else
    result := '_othertag';
end;


function TFHIRComposer.ResourceMediaType: String;
begin
  {$IFDEF FHIR2}
  result := 'application/xml+fhir; charset=UTF-8';
  {$ELSE}
  result := 'application/fhir+xml';
  {$ENDIF}
end;

function URLTail(s : String):String;
var
  i : integer;
begin
  i := LastDelimiter('/', s);
  result := copy(s, i+1, $FFFF);
  i := Pos('?', result);
  if i > 0 then
    result := copy(result, 1, i-1);
end;

function TFHIRXmlParserBase.GetAttribute(element: TMXmlElement; const name : String): String;
begin
  result := element.attribute[name].Trim;
end;

function TFHIRXmlParserBase.GetFormat: TFHIRFormat;
begin
  result := ffXml;
end;

procedure TFHIRXmlParserBase.GetObjectLocation(obj: TFHIRObject; element: TMXmlElement);
begin
  obj.LocationStart := element.Start;
  obj.LocationEnd := element.Stop;
end;

function TFHIRXmlParserBase.FirstChild(element: TMXmlElement): TMXmlElement;
Var
  node : TMXmlElement;
Begin
  result := Nil;
  node := element.first;
  While Assigned(node) And not Assigned(result) Do
  Begin
    If node.nodeType = ntElement Then
      result := node as TMXmlElement
    else if node.nodeType = ntComment then
      FComments.add(node.text);
    node := node.next;
  End;
end;

function TFHIRXmlParserBase.NextSibling(element: TMXmlElement): TMXmlElement;
Var
  node : TMXmlElement;
Begin
  result := Nil;
  node := element.next;
  While Assigned(node) And not Assigned(result) Do
  Begin
    If node.nodeType = ntElement Then
      result := node as TMXmlElement
    else if node.nodeType = ntComment then
      FComments.add(node.text);
    node := node.next;
  End;
end;


procedure TFHIRXmlParserBase.TakeCommentsStart(element: TFHIRObject);
begin
  checkTimeOut;
  if FComments.count > 0 then
  begin
    element.xml_commentsStart.assign(FComments);
    FComments.Clear;
  end;
end;

procedure TFHIRXmlParserBase.TakeCommentsEnd(element: TFHIRObject);
begin
  if FComments.count > 0 then
  begin
    element.xml_commentsEnd.assign(FComments);
    FComments.Clear;
  end;
end;

function TFHIRComposer.Compose(oResource: TFhirResourceV): String;
var
  stream : TBytesStream;
begin
  stream := TBytesStream.create;
  try
    compose(stream, oResource);
    result := TEncoding.UTF8.GetString(copy(stream.Bytes, 0, stream.position));
  finally
    stream.Free;
  end;
end;

function TFHIRComposer.asString(value: TBytes): String;
begin
  result := String(EncodeBase64(@value[0], length(value))).replace(#13#10, '');
end;

procedure TFHIRXmlParserBase.checkOtherAttributes(value: TMXmlElement; path : String);
var
  name : String;
begin
  if not AllowUnknownContent then
  begin
    for name in value.attributes.Keys do
    begin
      if (name <> 'id') and // always ok
         (name <> 'value') and // value is ok (todo: only on primitives)
         ((name <> 'url')) and // url is ok on extensions which appear with various names
         (name <> 'xmlns') and // namespaces are ok
         (not name.StartsWith('xmlns:')) then // namespaces are ok
        XmlError(path+'/@'+name, StringFormat(GetFhirMessage('MSG_UNKNOWN_CONTENT', lang), [name, path]));
    end;
  end;
end;

procedure TFHIRXmlParserBase.closeOutElement(result: TFHIRObject; element: TMXmlElement);
begin
  TakeCommentsEnd(result);
end;

destructor TFHIRXmlParserBase.Destroy;
begin
  FElement.Free;
  inherited;
end;

function TFHIRXmlParserBase.ParseDataTypeV(element: TMXmlElement; name: String; type_: TClass): TFHIRObject;
begin
  raise exception.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;

function TFHIRXmlParserBase.ParseDomainResourceV(element: TMXmlElement; path : String): TFhirResourceV;
var
  child : TMXmlElement;
begin
  child := FirstChild(element);
  result := ParseResourceV(child, path);
  try
    child := NextSibling(child);
    if (child <> nil) then
      UnknownContent(child, path);
    result.link;
  finally
    result.free;
  end;
end;

function TFHIRXmlParserBase.ParseDT(rootName: String; type_: TClass): TFHIRObject;
var
  xml : TMXmlDocument;
  root : TMXmlElement;
begin
  start;
  xml := nil;
  try
    FComments := TAdvStringList.create;
    try
      if (Element = nil) then
      begin
        xml := LoadXml(Source);
        root := xml.document;
      end
      else
        root := element;

      if root.namespaceURI <> FHIR_NS Then
        XmlError('/', StringFormat(GetFhirMessage('MSG_WRONG_NS', lang), [root.namespaceURI]));

      result := ParseDataTypeV(root, rootName, type_);
    finally
      FComments.Free;
    end;
  finally
    xml.Free;
  end;
end;

function TFHIRParser.toTBytes(s: String): TBytes;
begin
  result := DecodeBase64(AnsiString(s));
end;

function TFHIRParser.toTDateTimeEx(s: String): TDateTimeEx;
begin
  if s = '' then
    result := TDateTimeEx.makeNull
  else
    result := TDateTimeEx.fromXml(s);
end;

function TFHIRComposer.asString(value: TDateTimeEx): String;
begin
  if value.null then
    result := ''
  else
    result := value.ToXML;
end;

constructor TFHIRComposer.Create(worker: TFHIRWorkerContextV; Style : TFHIROutputStyle; lang: String);
begin
  inherited Create;
  FWorker := worker;
  FLang := lang;
  FStyle := Style;
  FElements := TStringList.create;
end;

destructor TFHIRComposer.Destroy;
begin
  Fworker.free;
  FElements.Free;
  inherited;
end;

function TFHIRComposer.doCompose(name : String): boolean;
begin
  result := (FElements.count = 0) or (FElements.IndexOf(name) > -1);
end;

function TFHIRComposer.Extension: String;
begin
  result := '??';
end;

function TFHIRComposer.isCanonical: boolean;
begin
  result := FStyle = OutputStyleCanonical;
end;

function TFHIRComposer.isPretty: boolean;
begin
  result := FStyle = OutputStylePretty;
end;

function TFHIRParser.StringArrayToCommaString(const aNames: array of String): String;
var
  i : integer;
begin
  result := '(';
  for i := 0 to Length(aNames) - 1 do
    if i = 0 then
      result := result + '"'+aNames[i]+'"'
    else
      result := result + ', "'+aNames[i]+'"';
  result := result + ')';
end;


{$IFNDEF FHIR2}
{ TFHIRTurtleComposerBase }

procedure TFHIRTurtleComposerBase.Compose(stream: TStream; oResource: TFhirResourceV);
var
  base : TTurtleComplex;
begin
  Fttl := TTurtleDocument.create;
  try
    Fttl.prefix('fhir', 'http://hl7.org/fhir/');
    Fttl.prefix('rdfs', 'http://www.w3.org/2000/01/rdf-schema#');
    Fttl.prefix('owl' ,'http://www.w3.org/2002/07/owl#');
    Fttl.prefix('xsd', 'http://www.w3.org/2001/XMLSchema#');

    if (url = '') then
      url := oResource.Tags['rdf-url'];

    base := TTurtleComplex.Create(nullLoc);
    try
      base.addUriPredicate('a', 'fhir:'+oResource.fhirType);
      base.addUriPredicate('fhir:nodeRole', 'fhir:treeRoot');
      if URL <> '' then
        Fttl.addObject(url, base.link)
      else
        Fttl.addObject('_', base.link);
      composeResourceV(base, oResource);
    finally
      base.free;
    end;

    if URL <> '' then
    begin
      // Protege Ontology Link
      base := TTurtleComplex.Create(nullLoc);
      try
        base.addUriPredicate('a', 'owl:ontology');
        base.addUriPredicate('owl:imports', 'fhir:fhir.ttl');
        base.addUriPredicate('owl:versionIRI', url+'.ttl');
        Fttl.addObject(url+'.ttl', base.link);
      finally
        base.free;
      end;
    end;

    TTurtleComposer.compose(Fttl, stream);
  finally
    Fttl.Free;
  end;
end;

procedure TFHIRTurtleComposerBase.ComposeInnerResource(this: TTurtleComplex; parentType, name: String; elem: TFhirResourceV; useType: boolean; index: integer);
var
  base : TTurtleComplex;
  url : String;
begin
  if (elem = nil) then
    exit;
  url := NewGuidURN;
  this.addUriPredicate('fhir:'+parentType+'.'+name, url);
  base := TTurtleComplex.Create(nullLoc);
  try
    base.addUriPredicate('a', 'fhir:'+elem.fhirType);
    Fttl.addObject(url, base.link);
    composeResourceV(base, elem);
  finally
    base.free;
  end;
end;

procedure TFHIRTurtleComposerBase.ComposeItem(stream: TStream; name: String; item: TFHIRObject);
begin
  raise Exception.Create('not implemented yet');
end;

procedure TFHIRTurtleComposerBase.ComposeItems(stream: TStream; name: String; items: TFHIRObjectList);
begin
  raise Exception.Create('not implemented yet');
end;

procedure TFHIRTurtleComposerBase.ComposeResourceV(parent :  TTurtleComplex; oResource: TFhirResourceV);
begin
  raise Exception.Create('do not instantiate TFHIRTurtleComposerBase directly');
end;

procedure TFHIRTurtleComposerBase.ComposeXHtmlNode(parent: TTurtleComplex; parentType, name: String; value: TFhirXHtmlNode; useType : boolean; index: integer);
begin
  if (value = nil) then
    exit;
  parent.addPredicate('fhir:'+parentType+'.'+name, ttlLiteral(TFHIRXhtmlParser.compose(value)));
end;

function TFHIRTurtleComposerBase.dateXsdType(value: TDateTimeEx): string;
begin
  case value.Precision of
    dtpYear : result := 'xsd:gYear';
    dtpMonth : result := 'xsd:gYearMonth';
    dtpDay : result := 'xsd:date';
  else
    result := 'xsd:dateTime';
  end;
end;

function TFHIRTurtleComposerBase.Extension: String;
begin
  result := '.ttl';
end;

procedure TFHIRTurtleComposerBase.ComposeResource(xml: TXmlBuilder; oResource: TFhirResourceV);
begin
  raise Exception.Create('not implemented yet');
end;

function TFHIRTurtleComposerBase.MimeType: String;
begin
  result := 'text/turtle';
end;
{$ENDIF}

{ TFHIRTextParser }

function TFHIRTextParser.GetFormat: TFHIRFormat;
begin
  result := ffText;
end;

procedure TFHIRTextParser.Parse;
var
  s : String;
begin
  s := StreamToString(source, TEncoding.UTF8);
    raise Exception.Create('Unable to process text content - unrecognised');
end;


{ TFHIRTextComposer }

procedure TFHIRTextComposer.Compose(stream: TStream; oResource: TFhirResourceV);
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TFHIRTextComposer.ComposeResource(xml: TXmlBuilder; oResource: TFhirResourceV);
begin
  raise Exception.Create('Not Done Yet');
end;

function TFHIRTextComposer.Extension: String;
begin
  result := '.txt';
end;

function TFHIRTextComposer.MimeType: String;
begin
  result := 'text/fhir';
end;

function TFHIRTextComposer.ResourceMediaType: String;
begin
  result := 'text/fhir';
end;

{$IFNDEF FHIR2}

{ TFHIRTurtleParserBase }

function TFHIRTurtleParserBase.GetFormat: TFHIRFormat;
begin
  result := ffTurtle;
end;

procedure TFHIRTurtleParserBase.Parse;
var
  p, pred : TTurtlePredicate;
  s : String;
begin
  start;
  Fdoc := TTurtleParser.parse(StreamToString(source, TENcoding.UTF8));
  try
    p := nil;
    for pred in Fdoc.Objects do
      for s in pred.Value.predicates.Keys do
        if (s = FHIR_TTL_URI_BASE + 'nodeRole') and pred.Value.predicates[s].hasValue(FHIR_TTL_URI_BASE + 'treeRoot') then
        begin
          if p = nil then
            p := pred
          else
            raise Exception.Create('Multiple tree node start points found');
        end;
    if (p = nil) then
      raise Exception.Create('No tree node start point found in Turtle Format');

    resource := ParseResourceV(p.Value);
    resource.Tags['rdf-url'] := p.URL.uri;
  finally
    Fdoc.Free;
  end;
end;

function TFHIRTurtleParserBase.ParseDT(rootName: String; type_: TClass): TFHIRObject;
begin
  raise Exception.Create('not supported');
end;

function TFHIRTurtleParserBase.ParseResourceV(obj: TTurtleComplex): TFHIRResourceV;
begin
  raise Exception.Create('Need to override ParseResourceV() in '+className);
end;

function TFHIRTurtleParserBase.ParseXHtmlNode(literal: String): TFhirXHtmlNode;
begin
  if literal = '' then
    result := nil
  else
    result := TFHIRXhtmlParser.parse(lang, FParserPolicy, [xopTrimWhitspace], literal);
end;

function TFHIRTurtleParserBase.rdfsType(obj: TTurtleComplex): string;
var
  t : TTurtleObject;
begin
  t := obj.predicates['http://www.w3.org/2000/01/rdf-schema#type'];
  if t = nil then
    raise Exception.Create('Unable to determine type: rdfs#type not found');
  if not (t is TTurtleURL) then
    raise Exception.Create('Unable to determine type: rdfs#type not a URL');
  result := TTurtleURL(t).uri.Substring(20);
end;
{$ENDIF}


End.
