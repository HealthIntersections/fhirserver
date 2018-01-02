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
  FHIRBase, FHIRResources, FHIRTypes, FHIRConstants, FHIRContext, FHIRSupport, FHIRTags, FHIRLang, FHIRXhtml;

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
    Fresource: TFhirResource;
    FSource: TStream;
    FLang: String;
    FParserPolicy : TFHIRXhtmlParserPolicy;
    FKeepLineNumbers : boolean;
    FTimeLimit: Cardinal;
    FTimeToAbort : Cardinal;
    FWorker : TFHIRWorkerContext;
    FIgnoreHtml: Boolean;
    procedure SetResource(const Value: TFhirResource);
    procedure start;
    procedure checkTimeOut;
  protected
    procedure checkDateFormat(s : string);
    Function toTDateTimeEx(s : String) : TDateTimeEx;
    function toTBytes(s : String) : TBytes;
    function StringArrayToCommaString(Const aNames : Array Of String) : String;
    function GetFormat: TFHIRFormat; virtual; abstract;
  public
    Constructor Create(worker : TFHIRWorkerContext; lang : String); Virtual;
    Destructor Destroy; Override;
    property source : TStream read FSource write FSource;
    procedure Parse; Virtual; abstract;
    function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Virtual; abstract;
    property resource : TFhirResource read Fresource write SetResource;


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

    Function ParseDomainResource(element: TMXmlElement; path : String) : TFhirResource;
    Function ParseInnerResource(element : TMXmlElement; path : String) : TFhirResource; Virtual;
    Function ParseResource(element : TMXmlElement; path : String) : TFhirResource; Virtual;
//    function parseBinary(element : TMXmlElement; path : String) : TFhirBinary;
    Procedure checkOtherAttributes(value : TMXmlElement; path : String);
    function ParseDataType(element : TMXmlElement; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;
    function GetFormat: TFHIRFormat; override;
  Public
    Destructor Destroy; Override;
    procedure Parse; Override;
    function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Override;
    property Element : TMXmlElement read FElement write SeTFhirElement;
    class function ParseFragment(worker : TFHIRWorkerContext; fragment, lang : String) : TFHIRObject; overload;
    class function ParseFile(worker : TFHIRWorkerContext; lang : String; filename : String) : TFHIRResource; overload;
  End;


  TJsonObjectHandler = procedure (jsn : TJsonObject; ctxt : TFHIRObjectList) of object;
  TJsonObjectPrimitiveHandler = procedure (value : String; jsn : TJsonObject; ctxt : TFHIRObjectList) of object;
  TJsonObjectEnumHandler = procedure (path, value : String; jsn : TJsonObject; ctxt : TFHIRObjectList; Const aSystems, aNames : Array Of String) of object;

  TFHIRJsonParserBase = class (TFHIRParser)
  Protected
    Function ParseXHtmlNode(path, value : String) : TFhirXHtmlNode;

    Function ParseResource(jsn : TJsonObject) : TFhirResource; Virtual;
    procedure ParseComments(base : TFHIRObject; jsn : TJsonObject);
    function ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;

    procedure iterateArray(arr : TJsonArray; ctxt : TFHIRObjectList; handler : TJsonObjectHandler);
    procedure iteratePrimitiveArray(arr1, arr2 : TJsonArray; ctxt : TFHIRObjectList; handler : TJsonObjectPrimitiveHandler);
    procedure iterateEnumArray(arr1, arr2 : TJsonArray; path : String; ctxt : TFHIRObjectList; handler : TJsonObjectEnumHandler; Const aSystems, aNames : Array Of String);

    // handlers
    procedure parseDomainResource(jsn : TJsonObject; ctxt : TFHIRObjectList);
    procedure ParseInnerResource(jsn : TJsonObject; ctxt : TFHIRObjectList);  overload;
    function ParseInnerResource(jsn: TJsonObject) : TFhirResource; overload;
    function GetFormat: TFHIRFormat; override;
  Public
    procedure Parse; Overload; Override;
    procedure Parse(obj : TJsonObject); Reintroduce; Overload; Virtual;
    function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Override;
    class function ParseFragment(worker : TFHIRWorkerContext; fragment, type_, lang : String) : TFHIRObject; overload;
    class function ParseFile(worker : TFHIRWorkerContext; lang : String; filename : String) : TFHIRResource; overload;
  End;

  {$IFNDEF FHIR2}
  TFHIRTurtleParserBase = class (TFHIRParser)
  private
    Fdoc : TTurtleDocument;
  protected
    function GetFormat: TFHIRFormat; override;
    function rdfsType(obj : TTurtleComplex) : string;
    function ParseInnerResource(obj : TTurtleObject) : TFHIRResource;
    function ParseResource(obj : TTurtleComplex) : TFHIRResource; virtual;
    function ParseDataType(obj : TTurtleComplex; name : String; type_ : TFHIRTypeClass) : TFHIRType; virtual;
    function ParseXHtmlNode(literal : String) : TFhirXHtmlNode;
  public
    procedure Parse; Overload; Override;
    function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Override;
    class function ParseFile(worker : TFHIRWorkerContext; lang : String; filename : String) : TFHIRResource; overload;
  end;
  {$ENDIF}

  TFHIRTextParser = class (TFHIRParser)
  protected
    function GetFormat: TFHIRFormat; override;
  Public
    procedure Parse; Overload; Override;
    function ParseDT(rootName : String; type_ : TFHIRTypeClass) : TFHIRType; Override;
    class function ParseFile(worker : TFHIRWorkerContext; lang : String; filename : String) : TFHIRResource; overload;
  End;

  TFHIRComposer = {abstract} class (TFHIRObject)
  private
    FLang: String;
    FSummaryOption: TFHIRSummaryOption;
    FNoHeader: Boolean;
    FElements : TStringList;
    FLogId: string;
  protected
    FWorker : TFHIRWorkerContext;
    FStyle : TFHIROutputStyle;

    function isPretty : boolean;
    function isCanonical : boolean;
    function doCompose(name : String) : boolean;

    Procedure ComposeResource(xml : TXmlBuilder; oResource : TFhirResource; links : TFhirBundleLinkList = nil); overload; virtual;
//    Procedure ComposeBinary(xml : TXmlBuilder; binary : TFhirBinary);
    procedure ComposeXHtmlNode(xml : TXmlBuilder; name : String; node: TFhirXHtmlNode); overload;
//    procedure ComposeXHtmlNode(s : TAdvStringBuilder; node: TFhirXHtmlNode; indent, relativeReferenceAdjustment : integer); overload;

    function ResourceMediaType: String; virtual;

    function asString(value : TDateTimeEx):String; overload;
    function asString(value : TBytes):String; overload;
    function asString(value : string):String; overload;
    function asString(value : boolean):String; overload;
    procedure ComposeExpression(stream : TStream; expr : TFHIRPathExpressionNode; items : TFHIRObjectList; types : TAdvStringSet); Virtual;
    procedure ComposeItems(stream : TStream; name : String; items : TFHIRObjectList); Virtual;
    procedure ComposeItem(stream : TStream; name : String; item : TFHIRObject); Virtual;
  public
    Constructor Create(worker : TFHIRWorkerContext; style : TFHIROutputStyle; lang : String); Virtual;
    Destructor Destroy; override;
    Procedure Compose(stream : TStream; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Overload; Virtual;
//    Procedure Compose(stream : TStream; ResourceType : TFhirResourceType; statedType, id, ver : String; oTags : TFHIRCodingList); Overload; Virtual; Abstract;

    function Compose(oResource : TFhirResource; links : TFhirBundleLinkList = nil) : String; Overload;
    function Compose(expr : TFHIRPathExpressionNode; items : TFHIRObjectList; types : TAdvStringSet): String; Overload;
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
//    xml : TXmlBuilder;
    procedure commentsStart(xml : TXmlBuilder; value : TFHIRObject);
    procedure commentsEnd(xml : TXmlBuilder; value : TFHIRObject);
    Procedure Attribute(xml : TXmlBuilder; name, value : String);
    Procedure Text(xml : TXmlBuilder; name, value : String);
    procedure closeOutElement(xml : TXmlBuilder; value : TFHIRObject);
    Procedure ComposeDomainResource(xml : TXmlBuilder; name : String; value : TFhirDomainResource);
    Procedure ComposeInnerResource(xml : TXmlBuilder; name : String; holder : TFhirDomainResource; value : TFhirResource); overload;
    Procedure ComposeInnerResource(xml : TXmlBuilder; name : String; holder : TFHIRElement; value : TFhirResource); overload;
    procedure ComposeExpression(stream : TStream; expr : TFHIRPathExpressionNode; items : TFHIRObjectList; types : TAdvStringSet); overload; override;
    procedure ComposeExpression(xml : TXmlBuilder; expr : TFHIRPathExpressionNode); reintroduce; overload; virtual;
    procedure ComposeBase(xml : TXmlBuilder; name : String; base : TFHIRObject); virtual;
    procedure ComposeItems(stream : TStream; name : String; items : TFHIRObjectList); override;
    procedure ComposeItem(stream : TStream; name : String; item : TFHIRObject); override;
  Public
    Procedure Compose(stream : TStream; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Override;
    Procedure Compose(node : TMXmlElement; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Overload;
//    Procedure Compose(stream : TStream; ResourceType : TFhirResourceType; oTags : TFHIRCodingList); Override;
//    Procedure ComposeXHtmlNode(xml : TXmlBuilder; name : String; value : TFhirXHtmlNode); overload;
    Function MimeType : String; Override;
    function Extension : String; Override;
    Property Comment : String read FComment write FComment;
    class procedure composeFile(worker : TFHIRWorkerContext; r : TFHIRResource; lang : String; filename : String; style : TFHIROutputStyle); overload;
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
//    Procedure ComposeExtensions(json : TJSONWriter; extensions : TFhirExtensionList);
//    Procedure ComposeModifierExtensions(json : TJSONWriter; extensions : TFhirExtensionList);

    Procedure composeComments(json : TJSONWriter; base : TFHIRObject);
    procedure ComposeDomainResource(json : TJSONWriter; name : String; oResource : TFhirDomainResource); overload; virtual;
    procedure ComposeInnerResource(json : TJSONWriter; name : String; holder : TFHIRElement; oResource : TFhirResource); overload; virtual;
    procedure ComposeInnerResource(json : TJSONWriter; name : String; holder : TFhirDomainResource; oResource : TFhirResource); overload; virtual;
    Procedure ComposeResource(json : TJSONWriter; oResource : TFhirResource; links : TFhirBundleLinkList); overload; virtual;
    Procedure ComposeResource(xml : TXmlBuilder; oResource : TFhirResource; links : TFhirBundleLinkList); overload; override;
//    Procedure ComposeExtension(json : TJSONWriter; name : String; extension : TFhirExtension; noObj : boolean = false); virtual;
//    Procedure ComposeBinary(json : TJSONWriter; binary : TFhirBinary);
    procedure ComposeExpression(stream : TStream; expr : TFHIRPathExpressionNode; items : TFHIRObjectList; types : TAdvStringSet); overload; override;
    procedure ComposeExpression(json: TJSONWriter; expr : TFHIRPathExpressionNode); reintroduce; overload; virtual;
    procedure ComposeBase(json: TJSONWriter; name : String; base : TFHIRObject); virtual;
    procedure ComposeItems(stream : TStream; name : String; items : TFHIRObjectList); override;
    procedure ComposeItem(stream : TStream; name : String; item : TFHIRObject); override;
  Public
    Procedure Compose(stream : TStream; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Override;
    Procedure Compose(stream : TAdvStream; oResource : TFhirResource; links : TFhirBundleLinkList = nil); overload;
    Procedure Compose(json: TJSONWriter; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Overload;
//    Procedure Compose(stream : TStream; ResourceType : TFhirResourceType; statedType, id, ver : String; oTags : TFHIRCodingList); Override;
    class procedure composeFile(worker : TFHIRWorkerContext; r : TFHIRResource; lang : String; filename : String; style : TFHIROutputStyle); overload;
    Function MimeType : String; Override;
    function Extension : String; Override;
    Property Comments : Boolean read FComments write FComments;
  End;

  TFHIRNDJsonComposer = class (TFHIRComposer)
  public
    Procedure Compose(stream : TStream; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Override;
  end;

  {$IFNDEF FHIR2}
  TFHIRTurtleComposerBase = class (TFHIRComposer)
  private
    FURL: String;
    FTtl : TTurtleDocument;
  protected
    procedure ComposeXHtmlNode(parent : TTurtleComplex; parentType, name : String; value: TFhirXHtmlNode; useType : boolean; index : integer); overload;

    function dateXsdType(value : TDateTimeEx) : string;
    Procedure ComposeResource(xml : TXmlBuilder; oResource : TFhirResource; links : TFhirBundleLinkList); overload; override;
    procedure ComposeExpression(stream : TStream; expr : TFHIRPathExpressionNode; items : TFHIRObjectList; types : TAdvStringSet); overload; override;
    procedure ComposeItems(stream : TStream; name : String; items : TFHIRObjectList); override;
    procedure ComposeItem(stream : TStream; name : String; item : TFHIRObject); override;

    Procedure ComposeResource(parent :  TTurtleComplex; oResource : TFhirResource); overload; virtual;
    Procedure ComposeInnerResource(this : TTurtleComplex; parentType, name : String; elem : TFhirResource; useType : boolean; index : integer); overload;
  public
    Procedure Compose(stream : TStream; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Override;
    Function MimeType : String; Override;
    property URL : String read FURL write FURL;
    function Extension : String; Override;
  end;
  {$ENDIF}

  TFHIRXhtmlComposerGetLink = procedure (resource : TFhirResource; base, statedType, id, ver : String; var link, text : String) of object;

  TFHIRXhtmlComposer = class (TFHIRComposer)
  private
    FBaseURL: String;
    FSession: TFhirSession;
    FTags : TFHIRTagList;
    FrelativeReferenceAdjustment: integer;
    FOnGetLink: TFHIRXhtmlComposerGetLink;
    FOperationName : String;
    FVersion: String;

    procedure SetSession(const Value: TFhirSession);
    function PresentTags(aType : TFhirResourceType; target : String; tags : TFHIRTagList; c : integer):String; overload;
    function PresentTags(aType : TFhirResourceType; target : String; meta: TFhirMeta; c : integer):String; overload;
    procedure SetTags(const Value: TFHIRTagList);
    function PatchToWeb(url: String): String;
//    xml : TXmlBuilder;
//    procedure ComposeNode(node : TFhirXHtmlNode);
    Procedure ComposeBundle(stream : TStream; bundle : TFHIRBundle);


  protected
    function ResourceMediaType: String; override;
  public
    Constructor Create(worker: TFHIRWorkerContext; Style : TFHIROutputStyle; lang, BaseURL : String); reintroduce; overload;
    Destructor Destroy; override;
    property BaseURL : String read FBaseURL write FBaseURL;
    Property Session : TFhirSession read FSession write SetSession;
    Property Version : String read FVersion write FVersion;
    property Tags : TFHIRTagList read FTags write SetTags;
    Procedure ComposeResource(xml : TXmlBuilder; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Override;
    Procedure Compose(stream : TStream; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Override;
    Function MimeType : String; Override;
    function Extension : String; Override;

    Property relativeReferenceAdjustment : integer read FrelativeReferenceAdjustment write FrelativeReferenceAdjustment;
    Property OnGetLink : TFHIRXhtmlComposerGetLink read FOnGetLink write FOnGetLink;
    Property OperationName : String read FOperationName write FOperationName;

    class function ResourceLinks(a, lang, base : String; count : integer; bTable, bPrefixLinks, canRead : boolean): String;
    class function PageLinks : String;
    class function Header(Session : TFhirSession; base, lang, version : String) : String;
    class function Footer(base, lang, logId : String; tail : boolean = true) : string;
  end;

  TFHIRTextComposer = class (TFHIRComposer)
  private
    function render(op : TFHIROperationOutcome) : String;
  protected
    function ResourceMediaType: String; override;
  public
    Procedure ComposeResource(xml : TXmlBuilder; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Override;
    Procedure Compose(stream : TStream; oResource : TFhirResource; links : TFhirBundleLinkList = nil); Override;
    Function MimeType : String; Override;
    function Extension : String; Override;
  end;

Implementation

uses
  RegularExpressions,
  FHIRParser,
  FHIRUtilities,
  FHIRProfileUtilities,
  FHIRMetaModel;

Function TFHIRXmlParserBase.LoadXml(stream : TStream) : TMXmlDocument;
begin
  start;
  result := TMXmlParser.parse(stream, [xpResolveNamespaces]);
end;

procedure TFHIRXmlParserBase.Parse;
var
  xml : TMXmlDocument;
  root : TMXmlElement;
  sd : TFHIRStructureDefinition;
  e : TFHIRMMElement;
  x : TFHIRMMXmlParser;
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
        resource := ParseResource(root, '')
      else
      begin
        // well, ok, we'll look to see if it's a logical model....
        if FWorker = nil then
          sd := nil
        else
          sd := FWorker.getStructure(root.namespaceURI, root.localName);
        if sd = nil then
          XmlError('/', StringFormat(GetFhirMessage('MSG_WRONG_NS', lang), [root.namespaceURI]))
        else
        begin
          x := TFHIRMMXmlParser.create(FWorker.Link);
          try
            e := x.parse(root, sd);
            try
              resource := TFHIRCustomResource.create(e.link);
            finally
              e.free;
            end;
          finally
            x.free;
          end;
        end;
      end;

    finally
      FComments.Free;
    end;
  finally
    xml.free;
  end;
end;

function TFHIRXmlParserBase.ParseResource(element: TMXmlElement; path : String): TFhirResource;
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
    resource := ParseResource(obj);
  finally
    obj.Free;
  end;
end;


function TFHIRJsonParserBase.ParseResource(jsn : TJsonObject): TFhirResource;
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


//function TFHIRJsonParserBase.parseBinary(jsn : TJsonObject): TFhirBinary;
//begin
//  result := TFhirBinary.create;
//  try
//    if jsn.has('contentType') then
//      result.ContentType:= jsn['contentType'];
//    if jsn.has('content') then
//      result.content.AsBytes := DecodeBase64(AnsiString(jsn['content']));
//    if jsn.has('id') then
//      result.xmlId:= jsn['id'];
//    result.link;
//  finally
//    result.free;
//  end;
//end;

procedure TFHIRJsonParserBase.Parse(obj: TJsonObject);
begin
  start;
  resource := ParseResource(obj);
end;

procedure TFHIRJsonParserBase.ParseComments(base: TFHIRObject; jsn : TJsonObject);
begin
  checkTimeOut;
  if jsn.has('_xml_comments_start') then
    base.xml_commentsStart.AsText:= jsn['_xml_comments_start'];
  if jsn.has('_xml_comments_end') then
    base.xml_commentsEnd.AsText:= jsn['_xml_comments_end'];
end;

procedure TFHIRJsonParserBase.parseDomainResource(jsn : TJsonObject; ctxt : TFHIRObjectList);
begin
  ctxt.add(ParseResource(jsn));
end;

function TFHIRJsonParserBase.ParseDataType(jsn : TJsonObject; name : String; type_ : TFHIRTypeClass) : TFHIRType;
begin
  raise exception.create('don''t use TFHIRJsonParserBase directly - use TFHIRJsonParser');
end;

function TFHIRJsonParserBase.ParseDT(rootName: String; type_: TFHIRTypeClass): TFHIRType;
var
  obj : TJsonObject;
begin
  start;
  obj := TJSONParser.Parse(source, timelimit);
  try
    result := ParseDataType(obj, rootName, type_);
  finally
    obj.Free;
  end;
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

procedure TFHIRXmlComposerBase.Compose(stream: TStream; oResource: TFhirResource; links : TFhirBundleLinkList);
var
  xml : TXmlBuilder;
  mx : TFHIRMMXmlParser;
begin
  if (oResource <> nil) and (oResource.resourceType = frtCustom) then
  begin
    mx := TFHIRMMXmlParser.create(FWorker.Link);
    try
      mx.compose(TFHIRCustomResource(oResource).Root, stream, isPretty, '');
    finally
      mx.free;
    end;
  end
  else
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
      ComposeResource(xml, oResource, links);
      xml.Finish;
      xml.Build(stream);
    finally
      xml.Free;
    end;
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

procedure TFHIRXmlComposerBase.Compose(node: TMXmlElement; oResource: TFhirResource; links : TFhirBundleLinkList);
var
  xml : TXmlBuilder;
begin
  xml := TMXmlBuilder.Create;
  try
    TMXmlBuilder(xml).Start(node);
    xml.CurrentNamespaces.DefaultNS := FHIR_NS;
    if FComment <> '' then
      xml.Comment(FComment);
    ComposeResource(xml, oResource, links);
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

procedure TFHIRXmlComposerBase.composeDomainResource(xml: TXmlBuilder; name: String; value: TFhirDomainResource);
begin
  xml.open(name);
  ComposeResource(xml, value, nil);
  xml.close(name);
end;

procedure TFHIRXmlComposerBase.ComposeExpression(xml: TXmlBuilder; expr: TFHIRPathExpressionNode);
var
  p : TFHIRPathExpressionNode;
begin
  if expr.Proximal then
  begin
    xml.AddAttribute('value', 'true');
    xml.Tag('proximal');
  end;

  case expr.kind of
    enkName :
      begin
        xml.AddAttribute('value', expr.name);
        xml.Tag('name');
      end;
    enkFunction :
      begin
        xml.AddAttribute('value', CODES_TFHIRPathFunctions[expr.FunctionId]);
        xml.Tag('function');
        for p in expr.Parameters do
        begin
          xml.open('parameter');
          ComposeExpression(xml, p);
          xml.close('parameter');
        end;
      end;
    enkConstant :
      begin
        xml.AddAttribute('value', expr.constant);
        xml.Tag('constant');
      end;
    enkGroup :
      begin
        xml.Open('group');
        ComposeExpression(xml, expr.Group);
        xml.Close('group');
      end;
  end;
  if expr.Types <> nil then
  begin
    xml.AddAttribute('value', expr.types.ToString);
    xml.Tag('types');
  end;
  if expr.Inner <> nil then
  begin
    xml.open('inner');
    ComposeExpression(xml, expr.Inner);
    xml.close('inner');
  end;
  if expr.Operation <> popNull then
  begin
    xml.AddAttribute('kind', CODES_TFHIRPathOperation[expr.Operation]);
    xml.open('operation');
    ComposeExpression(xml, expr.OpNext);
    xml.close('operation');
  end;
  if expr.OpTypes <> nil then
  begin
    xml.AddAttribute('value', expr.optypes.ToString);
    xml.Tag('op-types');
  end;
end;

class procedure TFHIRXmlComposerBase.composeFile(worker : TFHIRWorkerContext; r: TFHIRResource; lang, filename: String; style : TFHIROutputStyle);
var
  x: TFHIRXmlComposer;
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmCreate);
  try
    x := TFHIRXmlComposer.Create(worker.link, style, lang);
    try
      x.Compose(f, r);
    finally
      x.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TFHIRXmlComposerBase.ComposeInnerResource(xml: TXmlBuilder; name: String; holder: TFhirDomainResource; value: TFhirResource);
begin
  if value <> nil then
  begin
    xml.open(name);
    ComposeResource(xml, value, nil);
    xml.close(name);
  end;
end;

//procedure TFHIRXmlComposerBase.Compose(stream: TStream; ResourceType : TFhirResourceType; oTags: TFHIRCodingList; isPretty: Boolean);
//var
//  xml : TXmlBuilder;
//  i : integer;
//begin
//  xml := TAdvXmlBuilder.Create;
//  try
//    xml.IsPretty := isPretty;
//    xml.CurrentNamespaces.DefaultNS := FHIR_NS;
//    xml.Start;
//    if FComment <> '' then
//      xml.Comment(FComment);
//    xml.Open('TagList');
//    for i := 0 to oTags.Count - 1 do
//    begin
//    // todo-bundle
////      xml.AddAttribute('scheme', oTags[i].scheme);
////      xml.AddAttribute('term', oTags[i].term);
////      if oTags[i].label_ <> '' then
////        xml.AddAttribute('label', oTags[i].label_);
////      xml.Tag('category');
//    end;
//
//    xml.Close('TagList');
//    xml.Finish;
//    xml.Build(stream);
//  finally
//    xml.Free;
//  end;
//end;

{ TFHIRJsonComposerBase }


procedure TFHIRJsonComposerBase.Compose(stream: TStream; oResource: TFhirResource; links : TFhirBundleLinkList);
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
    ComposeResource(json, oResource, links);
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

procedure TFHIRJsonComposerBase.Compose(json : TJSONWriter; oResource: TFhirResource; links : TFhirBundleLinkList);
begin
  json := json.Link;
  ComposeResource(json, oResource, links);
end;

procedure TFHIRJsonComposerBase.Compose(stream: TAdvStream; oResource: TFhirResource; links: TFhirBundleLinkList);
var
  v : TVCLStream;
begin
  v :=  TVCLStream.Create(stream.Link);
  try
    Compose(v, oResource, links);
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

procedure TFHIRJsonComposerBase.ComposeResource(json : TJSONWriter; oResource: TFhirResource; links : TFhirBundleLinkList);
begin
  raise exception.create('don''t use TFHIRJsonComposerBase directly - use TFHIRJsonComposer');
end;

Procedure TFHIRJsonComposerBase.ComposeResource(xml : TXmlBuilder; oResource : TFhirResource; links : TFhirBundleLinkList);
var
  s : TBytesStream;
begin
  s := TBytesStream.Create();
  try
    compose(s, oResource, links);
    xml.Text(TEncoding.UTF8.getString(s.bytes, 0, s.size));
  finally
    s.free;
  end;
end;

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


function tail(s : String):String;
begin
  result := copy(s, LastDelimiter('/', s)+1, $FF);
end;



procedure TFHIRJsonComposerBase.composeDomainResource(json: TJSONWriter; name: String; oResource: TFhirDomainResource);
begin
  json.ValueObject('');
  ComposeResource(json, oResource, nil);
  json.FinishObject;
end;

procedure TFHIRJsonComposerBase.ComposeExpression(json: TJSONWriter; expr: TFHIRPathExpressionNode);
var
  p : TFHIRPathExpressionNode;
begin
  if expr.Proximal then
    json.value('proximal', true);

  case expr.kind of
    enkName: json.value('name', expr.name);
    enkFunction:
      begin
        json.value('function', CODES_TFHIRPathFunctions[expr.FunctionId]);
        json.ValueArray('parameters');
        for p in expr.Parameters do
        begin
          json.ValueObject('');
          ComposeExpression(json, p);
          json.FinishObject;
        end;
        json.FinishArray();
      end;
    enkConstant: json.value('constant', expr.constant);
    enkGroup:
      begin
      json.valueObject('group');
      ComposeExpression(json, expr.Group);
      json.FinishObject;
      end;
  end;
  if expr.Types <> nil then
    json.value('types', expr.types.ToString);
  if expr.Inner <> nil then
  begin
    json.ValueObject('inner');
    ComposeExpression(json, expr.Inner);
    json.FinishObject;
  end;
  if expr.Operation <> popNull then
  begin
    json.ValueObject('operation');
    json.value('kind', CODES_TFHIRPathOperation[expr.Operation]);
    ComposeExpression(json, expr.OpNext);
    json.FinishObject;
  end;
  if expr.OpTypes <> nil then
    json.value('op-types', expr.optypes.ToString);
end;

class procedure TFHIRJsonComposerBase.composeFile(worker: TFHIRWorkerContext; r: TFHIRResource; lang, filename: String; style: TFHIROutputStyle);
var
  j: TFHIRJsonComposer;
  f : TFileStream;
begin
  f := TFileStream.Create(filename, fmCreate);
  try
    j := TFHIRJsonComposer.Create(worker.link, style, lang);
    try
      j.Compose(f, r);
    finally
      j.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TFHIRJsonComposerBase.ComposeInnerResource(json: TJSONWriter; name: String; holder: TFhirDomainResource; oResource: TFhirResource);
begin
  if oResource <> nil then
  begin
    json.ValueObject(name);
    ComposeResource(json, oResource, nil);
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

procedure TFHIRJsonComposerBase.ComposeExpression(stream: TStream; expr : TFHIRPathExpressionNode; items: TFHIRObjectList; types : TAdvStringSet);
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
    json.ValueArray('outcome');
    for base in items do
      ComposeBase(json, '', base);
    json.FinishArray;
    json.Value('canonical', expr.Canonical);
    json.ValueObject('tree');
    composeExpression(json, expr);
    json.FinishObject;
    if (types <> nil) then
      json.Value('types', types.ToString);
    json.Finish;
  finally
    json.free;
  end;
end;

procedure TFHIRJsonComposerBase.composeInnerResource(json: TJSONWriter; name: String; holder : TFHIRElement; oResource: TFhirResource);
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
    ComposeResource(json, oResource, nil);
    json.FinishObject;
  end;
end;

//procedure TFHIRJsonComposerBase.ComposeBinary(json: TJSONWriter; binary: TFhirBinary);
//begin
//  Prop(json, 'id', binary.xmlId);
//  Prop(json, 'contentType', binary.ContentType);
//  Prop(json, 'content', StringReplace(string(EncodeBase64(binary.Content.Data, binary.Content.Size)), #13#10, ''));
//end;

procedure TFHIRJsonComposerBase.composeComments(json: TJSONWriter; base: TFHIRObject);
begin
  if not FComments then
    exit;

  if base.HasXmlCommentsStart then
    json.Value('_xml_comments_start', base.xml_commentsStart.AsText);
  if base.HasXmlCommentsEnd then
    json.Value('_xml_comments_end', base.xml_commentsEnd.AsText);
end;

//procedure TFHIRJsonComposerBase.Compose(stream: TStream; ResourceType : TFhirResourceType; oTags: TFHIRCodingList; isPretty: Boolean);
//var
//  oStream : TAdvVCLStream;
//  json : TJSONWriter;
//  i : integer;
//begin
//  json := TJsonWriterDirect.create;
//  try
//    oStream := TAdvVCLStream.Create;
//    json.Stream := oStream;
//    oStream.Stream := stream;
//    json.Start;
//    json.Value('resourceType', 'TagList');
//    json.ValueArray('category');
//    for i := 0 to oTags.Count - 1 do
//    begin
//      json.ValueObject;
//      // todo-bundle
////      Prop(json, 'scheme', oTags[i].scheme);
////      Prop(json, 'term', oTags[i].term);
////      if oTags[i].label_ <> '' then
////        Prop(json, 'label', oTags[i].label_);
//      json.FinishObject;
//    end;
//    json.FinishArray;
//    json.Finish;
//  finally
//    json.free;
//  end;
//end;
//
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

constructor TFHIRParser.Create(worker : TFHIRWorkerContext; lang: String);
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


class function TFHIRXmlParserBase.ParseFile(worker : TFHIRWorkerContext; lang : String; filename: String): TFHIRResource;
var
  x : TFHIRXmlParser;
begin
  x := TFHIRXmlParser.Create(worker.link, lang);
  try
    x.ParseFile(filename);
    result := x.resource.Link;
  finally
    x.Free;
  end;
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

class function TFHIRJsonParserBase.ParseFile(worker: TFHIRWorkerContext; lang, filename: String): TFHIRResource;
var
  j : TFHIRJsonParser;
begin
  j := TFHIRJsonParser.Create(worker.link, lang );
  try
    j.ParseFile(filename);
    result := j.resource.Link;
  finally
    j.Free;
  end;
end;

class function TFHIRJsonParserBase.ParseFragment(worker: TFHIRWorkerContext; fragment, type_, lang: String): TFHIRObject;
var
  ss : TBytesStream;
  p : TFHIRJsonParser;
  jsn : TJsonObject;
begin
  ss := TBytesStream.Create(TEncoding.UTF8.getBytes(fragment));
  try
    jsn := TJSONParser.Parse(ss);
    try
      p := TFHIRJsonParser.Create(worker.link, lang);
      try
        result := p.ParseFragment(jsn, type_);
      finally
        p.Free;
      end;
    finally
      jsn.Free;
    end;
  finally
    ss.Free;
  end;
end;

procedure TFHIRJsonParserBase.ParseInnerResource(jsn: TJsonObject; ctxt: TFHIRObjectList);
begin
  ctxt.add(ParseResource(jsn));
end;

function TFHIRJsonParserBase.ParseInnerResource(jsn: TJsonObject) : TFhirResource;
begin
  result := ParseResource(jsn);
end;

class function TFHIRXmlParserBase.ParseFragment(worker: TFHIRWorkerContext; fragment, lang: String): TFHIRObject;
var
  ss : TBytesStream;
  p : TFHIRXmlParser;
  xml : TMXmlDocument;
begin
  result := nil;
  ss := TBytesStream.Create(TEncoding.UTF8.getBytes(fragment));
  try
    p := TFHIRXmlParser.Create(worker.link, lang);
    try
      p.source := ss;
      xml := p.LoadXml(ss);
      try
        if xml.document.namespaceURI <> FHIR_NS Then
          raise Exception.Create('Unknown namespace');
        result := p.ParseFragment(xml.document);
      finally
        xml.Free;
      end;
    finally
      p.free;
    end;
  finally
    ss.Free;
  end;

end;

procedure TFHIRParser.SetResource(const Value: TFhirResource);
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

function TFHIRXmlParserBase.ParseDataType(element: TMXmlElement; name: String; type_: TFHIRTypeClass): TFHIRType;
begin
  raise exception.create('don''t use TFHIRXmlParserBase directly - use TFHIRXmlParser');
end;

function TFHIRXmlParserBase.ParseDT(rootName: String; type_: TFHIRTypeClass): TFHIRType;
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

      result := ParseDataType(root, rootName, type_);
    finally
      FComments.Free;
    end;
  finally
    xml.Free;
  end;
end;


function TFHIRXmlParserBase.ParseInnerResource(element: TMXmlElement; path: String): TFhirResource;
var
  child : TMXmlElement;
begin
  child := FirstChild(element);
  result := ParseResource(child, path);
  try
    child := NextSibling(child);
    if (child <> nil) then
      UnknownContent(child, path);
    result.link;
  finally
    result.free;
  end;
end;

{atom }

procedure TFHIRComposer.Compose(stream: TStream; oResource: TFhirResource; links: TFhirBundleLinkList);
begin
  raise Exception.Create('Error: call to TFHIRComposer.Compose(stream: TStream; oResource: TFhirResource; isPretty: Boolean; links: TFhirBundleLinkList)');
end;


function TFHIRComposer.Compose(expr : TFHIRPathExpressionNode; items: TFHIRObjectList; types : TAdvStringSet): String;
var
  stream : TBytesStream;
begin
  stream := TBytesStream.create;
  try
    composeExpression(stream, expr, items, types);
    result := TEncoding.UTF8.GetString(copy(stream.Bytes, 0, stream.position));
  finally
    stream.Free;
  end;
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


procedure TFHIRComposer.ComposeExpression(stream: TStream; expr : TFHIRPathExpressionNode; items: TFHIRObjectList; types : TAdvStringSet);
begin
  raise Exception.Create('ComposeExpression is Not supported for '+className);
end;

procedure TFHIRComposer.ComposeItem(stream: TStream; name: String; item: TFHIRObject);
begin
  raise Exception.Create('ComposeExpression is Not supported for '+className);
end;

procedure TFHIRComposer.ComposeItems(stream: TStream; name: String; items: TFHIRObjectList);
begin
  raise Exception.Create('ComposeExpression is Not supported for '+className);
end;

procedure TFHIRComposer.ComposeResource(xml : TXmlBuilder; oResource: TFhirResource; links : TFhirBundleLinkList);
begin
  raise exception.create('don''t use '+className+' directly - use TFHIRXmlComposer');
end;

procedure TFHIRComposer.ComposeXHtmlNode(xml: TXmlBuilder; name: String; node: TFhirXHtmlNode);
begin
  TFHIRXhtmlParser.Compose(node, xml);
end;

procedure TFHIRXmlComposerBase.ComposeInnerResource(xml: TXmlBuilder; name: String; holder : TFHIRElement; value: TFhirResource);
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
    ComposeResource(xml, value, nil);
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

procedure TFHIRXmlComposerBase.ComposeExpression(stream: TStream; expr : TFHIRPathExpressionNode; items: TFHIRObjectList; types : TAdvStringSet);
var
  xml : TXmlBuilder;
  base : TFHIRObject;
begin
  xml := TAdvXmlBuilder.Create;
  try
    xml.IsPretty := isPretty;
    xml.NoHeader := NoHeader;
    xml.CurrentNamespaces.DefaultNS := FHIR_NS;
    xml.Start;
    xml.Open('Expression');
    if items <> nil then
      xml.addattribute('count', inttostr(items.count))
    else
      xml.addattribute('count', 'nil');
    xml.Open('outcome');
    if items <> nil then
      for base in items do
        if (base = nil) then
          xml.tag('Null')
        else
          ComposeBase(xml, base.FhirType, base);
    xml.Close('outcome');
    xml.TagText('canonical', expr.Canonical);
    xml.Open('tree');
    composeExpression(xml, expr);
    xml.Close('tree');
    if (types <> nil) then
    begin
      xml.AddAttribute('value', types.ToString);
      xml.Tag('types');
    end;
    xml.Close('Expression');
    xml.Finish;
    xml.Build(stream);
  finally
    xml.Free;
  end;
end;

function TFHIRComposer.MimeType: String;
begin
  result := '??';
end;

{ TFHIRXhtmlComposer }

procedure TFHIRXhtmlComposer.Compose(stream: TStream; oResource: TFhirResource; links : TFhirBundleLinkList);
var
  s : TAdvStringBuilder;
  ss : TBytesStream;
  xml : TFHIRXmlComposer;
  c : integer;
  title : String;
  link, text : String;
  id : String;
  ver : String;
  statedType : String;
begin
  if (oResource is TFhirBundle) then
  begin
    composeBundle(stream, oResource as TFhirBundle);
    exit;
  end;

  id := oResource.id;
  if (oResource.meta <> nil) then
    ver := oResource.meta.versionId;
  statedType := CODES_TFhirResourceType[oResource.resourceType]; // todo: resolve this

  if (id = '') and (ver = '') then
  begin
    if FOperationName <> '' then
      title := 'Results from '+FOperationName
    else
      title := FormatTextToXml(GetFhirMessage(CODES_TFhirResourceType[oResource.resourceType], lang), xmlText)
  end
  else if (ver = '') then
    title := FormatTextToXml(GetFhirMessage('NAME_RESOURCE', lang)+' "'+id + '" ('+CODES_TFhirResourceType[oResource.ResourceType]+') ', xmlText)
  else
    title := FormatTextToXml(GetFhirMessage('NAME_RESOURCE', lang)+' "'+id+'" '+GetFhirMessage('NAME_VERSION', lang)+' "'+ver + '" ('+CODES_TFhirResourceType[oResource.ResourceType]+') ', xmlText);

  c := 0;
  s := TAdvStringBuilder.create;
  try
    s.append(
'<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
'<!DOCTYPE HTML'+#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+#13#10+
''+#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
'<head>'+#13#10+
'    <title>'+title+'</title>'+#13#10+
PageLinks+
FHIR_JS+
'</head>'+#13#10+
''+#13#10+
'<body>'+#13#10+
''+#13#10+
Header(Session, FBaseURL, lang, version)+
'<h2>'+title+'</h2>'+#13#10);

    if oResource is TFhirBinary then
    begin
      if StringStartsWith(TFhirBinary(oResource).ContentType, 'image/') then
        s.append('<img src="'+CODES_TFhirResourceType[oResource.ResourceType]+'/'+id+'">'+#13#10)
      else
        s.append('<pre class="xml">'+#13#10+'('+GetFhirMessage('NAME_BINARY', lang)+')'+#13#10+'</pre>'+#13#10);
    end
    else
    begin
      inc(c);
      if assigned(FTags) then
        if ver <> '' then
          s.append('<p><a href="./_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>: '+PresentTags(oResource.resourceType, FBaseURL+CODES_TFhirResourceType[oResource.ResourceType]+'/'+id+'/_history/'+ver+'/_tags', Ftags, c)+'</p>'+#13#10)
        else if id <> '' then
          s.append('<p><a href="./_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>: '+PresentTags(oResource.resourceType, FBaseURL+CODES_TFhirResourceType[oResource.ResourceType]+'/'+id+'/_tags', Ftags, c)+'</p>'+#13#10);
      if id <> '' then
      begin
        if assigned(FOnGetLink) then
          FOnGetLink(oResource, BaseURL, statedType, id, ver, link, text)
        else
          link := '';
        if link <> '' then
          s.append('<p><a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. <a href="'+link+'">'+FormatTextToHTML(text)+'</a>'+#13#10)
        else
          s.append('<p><a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+#13#10);

        if (links <> nil) and (links.Matches['z-edit-src'] <> '') then
          s.append('. Edit this as <a href="'+patchToWeb(links.Matches['z-edit-src'])+'?srcformat=xml">XML</a> or <a href="'+patchToWeb(links.Matches['z-edit-src'])+'?srcformat=json">JSON</a>');
        {$IFDEF FHIR_QUESTIONNAIRERESPONSE}
        if (links <> nil) and (links.Matches['edit-form'] <> '') then
          if (oResource is TFHIRQuestionnaireResponse) then
          begin
            if (TFHIRQuestionnaireResponse(oResource).questionnaire <> nil) then
              s.append('. <a href="'+patchToWeb(links.Matches['edit-form'])+'">Edit this Resource</a> (or <a href="'+TFHIRQuestionnaireResponse(oResource).questionnaire.reference+'">see the questionnaire</a>)')
          end
          else
            s.append('. <a href="'+patchToWeb(links.Matches['edit-form'])+'">Edit this Resource</a> (or <a href="'+links.Matches['edit-form']+'">see resources underlying that</a>)');
        {$ENDIF}
        if (links <> nil) and (links.Matches['edit-post'] <> '') then
          s.append('. Submit edited content by POST to '+links.Matches['edit-post']);
        if not (oResource.resourceType in [frtProvenance, frtAuditEvent]) then
          s.append('. <a href="'+FBaseURL+'Provenance?target='+oResource.fhirType+'/'+oResource.id+'">provenance for this resource</a>');
        s.append('</p>'#13#10);
      end;

      if (oResource is TFhirDomainResource) and (TFHIRDomainResource(oResource).text <> nil) then
        TFHIRXhtmlParser.Compose(TFHIRDomainResource(oResource).text.div_, s, false, 0, relativeReferenceAdjustment);
      s.append('<hr/>'+#13#10);
      xml := TFHIRXmlComposer.create(FWorker.link, OutputStylePretty, lang);
      ss := TBytesStream.create();
      try
        xml.Compose(ss, oResource, links);
        s.append('<pre class="xml">'+#13#10+FormatXMLToHTML(TEncoding.UTF8.getString(ss.bytes, 0, ss.size))+#13#10+'</pre>'+#13#10);
      finally
        ss.free;
        xml.free;
      end;
    end;
    s.append(
'<p><br/>'+
Footer(FBaseURL, lang, logid)
    );
    s.WriteToStream(stream);
  finally
    s.free;
  end;
end;

function TFHIRXhtmlComposer.PatchToWeb(url : String) : String;
begin
  result := FBaseURL+'_web/'+url.substring(FBaseURL.length);
end;

function TFHIRXhtmlComposer.PresentTags(aType: TFhirResourceType; target: String; tags: TFHIRTagList; c: integer): String;
begin
//  PresentTags()
end;

//procedure TFHIRXhtmlComposer.Compose(stream: TStream; oFeed: TFHIRAtomFeed; isPretty: Boolean);
//var
//  s : TAdvStringBuilder;
//  i : integer;
//  a : string;
//  e : TFHIRAtomEntry;
//  ss : TBytesStream;
//  xml : TFHIRXmlComposer;
//  link, text : String;
//  u : string;
//begin
//  a := oFeed.authorUri;
//  s := TAdvStringBuilder.create;
//  try
//    s.append(
//'<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
//'<!DOCTYPE HTML'+#13#10+
//'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+#13#10+
//''+#13#10+
//'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
//'<head>'+#13#10+
//'    <title>'+FormatTextToXml(oFeed.title)+'</title>'+#13#10+
//PageLinks+
//FHIR_JS+#13#10+
//'</head>'+#13#10+
//''+#13#10+
//'<body>'+#13#10+
//''+#13#10+
//Header(Session, FBaseURL, lang)+
//'<h1>'+FormatTextToXml(oFeed.title)+'</h1>'+#13#10);
//
//  u := ofeed.links['self'];
//  if not u.contains('?') then
//    u := u + '?'
//  else
//    u := u + '&';
//  s.append('<p><a href="'+u+'_format=xml"><img src="/rss.png"> XML</a> '+GetFhirMessage('OR', lang)+' <a href="'+u+'_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'</p>'+#13#10);
//
//    if (ofeed.isSearch) then
//    begin
//      s.append('<p>'+GetFhirMessage('NAME_LINKS', lang)+':&nbsp;');
//      if (ofeed.links['first'] <> '') then
//        s.append('<a href="'+ofeed.links.getrel('first')+'">'+GetFhirMessage('NAME_FIRST', lang)+'</a>&nbsp;')
//      else
//        s.append('<span style="color: grey">'+GetFhirMessage('NAME_FIRST', lang)+'</span>&nbsp;');
//      if (ofeed.links['previous'] <> '') then
//        s.append('<a href="'+ofeed.links.getrel('previous')+'">'+GetFhirMessage('NAME_PREVIOUS', lang)+'</a>&nbsp;')
//      else
//        s.append('<span style="color: grey">'+GetFhirMessage('NAME_PREVIOUS', lang)+'</span>&nbsp;');
//      if (ofeed.links['next'] <> '') then
//        s.append('<a href="'+ofeed.links.getrel('next')+'">'+GetFhirMessage('NAME_NEXT', lang)+'</a>&nbsp;')
//      else
//        s.append('<span style="color: grey">'+GetFhirMessage('NAME_NEXT', lang)+'</span>&nbsp;');
//      if (ofeed.links['last'] <> '') then
//        s.append('<a href="'+ofeed.links.getrel('last')+'">'+GetFhirMessage('NAME_LAST', lang)+'</a>&nbsp;')
//      else
//        s.append('<span style="color: grey">'+GetFhirMessage('NAME_LAST', lang)+'</span>&nbsp;');
//      if oFeed.SearchTotal <> 0 then
//        s.append(' ('+inttostr(oFeed.SearchTotal)+' '+GetFhirMessage('FOUND', lang)+'). ');
//      s.append('<span style="color: grey">'+GetFhirMessage('NAME_SEARCH', lang)+': '+ofeed.links.getrel('self')+'</span>&nbsp;</p>');
//      s.append('<p>SQL: <span style="color: maroon">'+FormatTextToXML(oFeed.sql)+'</span></p>');
//    end;
//
//    for i := 0 to oFeed.entries.Count - 1 do
//    begin
//      e := oFeed.entries[i];
//      s.append('<h2>'+FormatTextToXml(e.title)+'</h2>'+#13#10);
//      if (e.categories <> nil) and (e.Resource <> nil) then
//        s.append('<p><a href="'+e.id+'/_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>: '+PresentTags(e.resource.ResourceType, e.links.GetRel('self')+'/_tags', e.categories, i+1        )+'</p>'+#13#10);
//
//      u := e.Links.rel['self'];
//      if (u <> '')  then
//      begin
//        s.append('<p><a href="'+e.Links.rel['self']+'">'+GetFhirMessage('THIS_RESOURCE', lang)+'</a> ');
//      if not (e.resource is TFhirBinary) then
//        begin
//        s.append(
//          ', <a href="'+e.Links.rel['self']+'?_format=xml">XML</a> '+GetFhirMessage('OR', lang)+' '+
//        '<a href="'+e.Links.rel['self']+'?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang));
//        s.append(
//          ', '+GetFhirMessage('OR', lang)+' <a href="'+e.id+'/_history">'+GetFhirMessage('NAME_HISTORY', lang)+'</a>.');
//
//        if (e.links <> nil) and (e.links.GetRel('z-edit-src') <> '') then
//          s.append(' Edit this as <a href="'+patchToWeb(e.links.GetRel('z-edit-src'))+'?srcformat=xml">XML</a> or <a href="'+patchToWeb(e.links.GetRel('z-edit-src'))+'?srcformat=json">JSON</a>.');
//
//        {$IFNDEF FHIR_DSTU}
//        if e.links.GetRel('edit-form') <> '' then
//          if (e.resource is TFHIRQuestionnaireResponse) then
//          begin
//            if (TFHIRQuestionnaireResponse(e.resource).questionnaire <> nil) then
//              s.append(' <a href="'+patchToWeb(e.links.GetRel('edit-form'))+'">Edit this Resource</a> (or <a href="'+TFHIRQuestionnaireResponse(e.resource).questionnaire.reference+'">see the questionnaire</a>)')
//          end
//          else
//            s.append(' <a href="'+patchToWeb(e.links.GetRel('edit-form'))+'">Edit this Resource</a> (or just see <a href="'+e.links.GetRel('edit-form')+'">the Questionnaire</a>)');
//        if e.links.GetRel('edit-post') <> '' then
//          s.append(' Submit edited content by POST to '+e.links.GetRel('edit-post'));
//        {$ENDIF}
//
//        if assigned(FOnGetLink) then
//        begin
//          FOnGetLink(e.resource, BaseURL, '', tail(e.id), tail(e.Links.rel['self']), link, text);
//          if (link <> '') then
//            s.append(' <a href="'+link+'">'+FormatTextToHTML(text)+'</a>');
//        end;
//        s.append('</br> Updated: '+e.updated.AsXML+'; Author: '+Author(e, a)+'</p>'+#13#10);
//        end;
//      end;
//
//      if e.deleted then
//        s.append('<p>'+GetFhirMessage('MSG_DELETED', lang)+'</p>')
//      else if e.resource = nil then
//        s.append('<p>(--)</p>')
//      else if e.resource is TFhirBinary then
//      begin
//        if StringStartsWith(TFhirBinary(e.resource).ContentType, 'image/') then
//          s.append('<img src="'+CODES_TFhirResourceType[e.resource.resourcetype]+'/'+e.id+'">'+#13#10)
//        else
//          s.append('<pre class="xml">'+#13#10+'('+GetFhirMessage('NAME_BINARY', lang)+')'+#13#10+'</pre>'+#13#10);
//      end
//      else
//      begin
//        xml := TFHIRXmlComposer.create(lang);
//        ss := TBytesStream.create('');
//        try
//          if (e.resource.text <> nil) and (e.resource.text.div_ <> nil) then
//            ComposeXHtmlNode(s, e.resource.text.div_, 2, relativeReferenceAdjustment);
//          xml.Compose(ss, '', e.id, tail(e.links.rel['self']), e.resource, true, e.links);
//          s.append('<hr/>'+#13#10+'<pre class="xml">'+#13#10+FormatXMLToHTML(ss.dataString)+#13#10+'</pre>'+#13#10);
//        finally
//          ss.free;
//          xml.free;
//        end;
//      end;
//    end;
//    s.append(
//'<p><br/>'
//+footer(FBaseUrl, lang)
//    );
//    s.WriteToStream(stream);
//  finally
//    s.free;
//  end;
//end;
//
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


const
  TYPE_TITLE : Array[TFhirBundleTypeEnum] of String = ('', 'Document', 'Message', 'Transaction', 'Transaction Response', 'Batch', 'Batch Response', 'History Record', 'Search Results', 'Resource Collection');

{
procedure TFHIRXhtmlComposer.Compose(stream: TStream; oMeta: TFhirMeta; ResourceType : TFhirResourceType; id, ver : String; isPretty: Boolean; links: TFhirBundleLinkList);
var
  s : TAdvStringBuilder;
  i : integer;
begin
  s := TAdvStringBuilder.create;
  try
    s.append(
'<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
'<!DOCTYPE HTML'+#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+#13#10+
''+#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
'<head>'+#13#10);
  if ResourceType = frtNull then
    s.append('    <title>'+FormatTextToXml(GetFhirMessage('SYSTEM_TAGS', lang))+'</title>'+#13#10)
  else if id = '' then
    s.append('    <title>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_TYPE_TAGS', lang), [CODES_TFhirResourceType[ResourceType]]))+'</title>'+#13#10)
  else if ver = '' then
    s.append('    <title>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_TAGS', lang), [CODES_TFhirResourceType[ResourceType], id]))+'</title>'+#13#10)
  else
    s.append('    <title>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_VER_TAGS', lang), [CODES_TFhirResourceType[ResourceType], id, ver]))+'</title>'+#13#10);

    s.append(
PageLinks+#13#10+
FHIR_JS+#13#10+
'</head>'+#13#10+
''+#13#10+
'<body>'+#13#10+
''+#13#10+
Header(Session, FBaseURL, Lang));

  if ResourceType = frtNull then
    s.append('    <h2>'+FormatTextToXml(GetFhirMessage('SYSTEM_TAGS', lang))+'</title>'+#13#10+
     '<p></p><p>'+GetFhirMessage('NAME_LINKS', lang)+': <a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. '+
     'Or: <a href="'+FBaseUrl+'"/>Home Page</a> </p>')
  else if id = '' then
    s.append('    <h2>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_TYPE_TAGS', lang), [CODES_TFhirResourceType[ResourceType]]))+'</h2>'+#13#10+
     '<p></p><p>'+GetFhirMessage('NAME_LINKS', lang)+': <a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. '+
     'Or: '+ResourceLinks(ResourceType, lang, FBaseURL, 0, false, false, false)+' </p>')
  else if ver = '' then
    s.append('    <h2>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_TAGS', lang), [CODES_TFhirResourceType[ResourceType], id]))+'</h2>'+#13#10+
     '<p></p><p>'+GetFhirMessage('NAME_LINKS', lang)+': <a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. '+
     'Or: <a href="../'+id+'">This Resource</a> </p>')
  else
    s.append('    <h2>'+FormatTextToXml(StringFormat(GetFhirMessage('RESOURCE_VER_TAGS', lang), [CODES_TFhirResourceType[ResourceType], id, ver]))+'</h2>'+#13#10+
     '<p></p><p>'+GetFhirMessage('NAME_LINKS', lang)+': <a href="?_format=xml">XML</a> or <a href="?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'. '+
     'Or: <a href="../'+ver+'">This Resource Version</a> </p>');

   s.append('<p></p>'+#13#10);
   if (oMeta.profileList.Count + oMeta.tagList.Count + oMeta.securityList.Count = 0) then
     s.append('<p>'+GetFhirMessage('NO_TAGS', lang)+'</p>'+#13#10)
   else
   begin
     s.append('<table>'+#13#10);
     s.append(' <tr><td><b>Type</b></td><td></td><td><b>identity</b></td><td></td><td><b>Label</b></td></tr>'+#13#10);
     for i := 0 to oMeta.profileList.Count - 1 do
       s.append(' <tr><td>Profile</td><td></td><td>'+oMeta.profileList[i].value+'</td><td></td><td></td></tr>'+#13#10);
     for i := 0 to oMeta.tagList.Count - 1 do
       s.append(' <tr><td>Tag</td><td></td><td>'+oMeta.tagList[i].system+'::'+oMeta.tagList[i].code+'</td><td></td><td>'+oMeta.tagList[i].display+'</td></tr>'+#13#10);
     for i := 0 to oMeta.securityList.Count - 1 do
       s.append(' <tr><td>Security Label</td><td></td><td>'+oMeta.securityList[i].system+'::'+oMeta.securityList[i].code+'</td><td></td><td>'+oMeta.securityList[i].display+'</td></tr>'+#13#10);
     s.append('</table>'+#13#10);
   end;
   s.append('<p></p>'+#13#10);

    s.append(
'<p><br/>'+Footer(FBaseURL, lang)
    );
    s.WriteToStream(stream);
  finally
    s.free;
  end;
end;
}

procedure TFHIRXhtmlComposer.ComposeBundle(stream: TStream; bundle: TFHIRBundle);
var
  s : TAdvStringBuilder;
  i : integer;
  e : TFhirBundleEntry;
  ss : TBytesStream;
  xml : TFHIRXmlComposer;
  r : TFhirResource;
  t, link, text, sl, ul : String;
  a : TArray<String>;
begin
  s := TAdvStringBuilder.create;
  try
    s.append(
'<?xml version="1.0" encoding="UTF-8"?>'+#13#10+
'<!DOCTYPE HTML'+#13#10+
'       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'+#13#10+
''+#13#10+
'<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'+#13#10+
'<head>'+#13#10+
'    <title>'+TYPE_TITLE[bundle.type_]+'</title>'+#13#10+
PageLinks+
FHIR_JS+#13#10+
'</head>'+#13#10+
''+#13#10+
'<body>'+#13#10+
''+#13#10+
Header(Session, FBaseURL, lang, FVersion)+
'<h1>'+TYPE_TITLE[bundle.type_]+'</h1>'+#13#10);

  ul := bundle.link_List.Matches['self'];
  if pos('?', ul) = 0 then
    ul := ul + '?'
  else
    ul := ul + '&';
  s.append('<p><a href="'+ul+'_format=xml">XML</a> '+GetFhirMessage('OR', lang)+' <a href="'+ul+'_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang)+'</p>'+#13#10);

    if (bundle.type_ in [BundleTypeSearchset, BundleTypeHistory])  then
    begin
      s.append('<p>'+GetFhirMessage('NAME_LINKS', lang)+':&nbsp;');
      if (bundle.link_List.Matches['first'] <> '') then
        s.append('<a href="'+bundle.link_List.Matches['first']+'">'+GetFhirMessage('NAME_FIRST', lang)+'</a>&nbsp;')
      else
        s.append('<span style="color: grey">'+GetFhirMessage('NAME_FIRST', lang)+'</span>&nbsp;');
      if (bundle.link_List.Matches['previous'] <> '') then
        s.append('<a href="'+bundle.link_List.Matches['previous']+'">'+GetFhirMessage('NAME_PREVIOUS', lang)+'</a>&nbsp;')
      else
        s.append('<span style="color: grey">'+GetFhirMessage('NAME_PREVIOUS', lang)+'</span>&nbsp;');
      if (bundle.link_List.Matches['next'] <> '') then
        s.append('<a href="'+bundle.link_List.Matches['next']+'">'+GetFhirMessage('NAME_NEXT', lang)+'</a>&nbsp;')
      else
        s.append('<span style="color: grey">'+GetFhirMessage('NAME_NEXT', lang)+'</span>&nbsp;');
      if (bundle.link_List.Matches['last'] <> '') then
        s.append('<a href="'+bundle.link_List.Matches['last']+'">'+GetFhirMessage('NAME_LAST', lang)+'</a>&nbsp;')
      else
        s.append('<span style="color: grey">'+GetFhirMessage('NAME_LAST', lang)+'</span>&nbsp;');
      if bundle.Total <> '' then
        s.append(' ('+bundle.Total+' '+GetFhirMessage('FOUND', lang)+'). ');
      s.append('<span style="color: grey">'+GetFhirMessage('NAME_SEARCH', lang)+': '+bundle.link_List.Matches['self']+'</span>&nbsp;</p>');
      if bundle.tags['sql'] <> '' then
        s.append('<p>SQL (for debugging): <span style="color: maroon">'+FormatTextToXML(bundle.tags['sql'], xmlText)+'</span></p>');
    end;

    for i := 0 to bundle.entryList.Count - 1 do
    begin
      e := bundle.entryList[i];
      r := e.resource;
      if (r = nil) then
      begin
        if (e.request <> nil) and (e.request.method = HttpVerbDELETE) then
        begin
          a := e.request.url.Split(['/']);
          s.append('<h2>'+a[length(a)-2]+' '+a[length(a)-1]+' deleted</h2>'+#13#10);
          s.append('<p>'+FormatTextToXML(e.tags['opdesc'], xmlText));
          if e.link_List.Matches['audit'] <> '' then

            s.append(' (<a href="'+BaseURL+e.link_List.Matches['audit']+'">Audit</a>)');
          s.append('</p>'+#13#10);
        end
        else
          s.append('<h2>nil?</h2>'+#13#10)
      end
      else
      begin
      t := GetFhirMessage(CODES_TFHIRResourceType[r.ResourceType], lang)+' "'+r.id+'"';
      if (r.id = '') then
        sl := ''
      else
      begin
        sl := AppendForwardSlash(BaseURL)+ CODES_TFHIRResourceType[r.ResourceType]+'/'+r.id;
        if (r.meta <> nil) and (r.meta.versionId <> '') then
        begin
          t := t +' '+GetFhirMessage('NAME_VERSION', lang)+' "'+r.meta.versionId+'"';
          sl := sl + '/_history/'+r.meta.versionId;
        end;
      end;

      s.append('<h2>'+FormatTextToXml(t, xmlText)+'</h2>'+#13#10);
      if e.tags['opdesc'] <>'' then
      begin
        s.append('<p>'+FormatTextToXML(e.tags['opdesc'], xmlText));
        if e.link_List.Matches['audit'] <> '' then
          s.append(' (<a href="'+BaseURL+e.link_List.Matches['audit']+'">Audit</a>)');
        s.append('</p>'+#13#10);
      end;
      if (r.meta <> nil) then
        s.append('<p><a href="'+e.id+'/_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>: '+PresentTags(r.ResourceType, sl+'/_tags', r.meta, i+1)+'</p>'+#13#10);

      if e.search <> nil then
      begin
        s.Append('<p>Search Information: Mode = '+CODES_TFhirSearchEntryModeEnum[e.search.mode]);
        if e.search.scoreElement <> nil then
          s.Append(', score = '+e.search.score);
        if e.search.hasExtension('http://hl7.org/fhir/StructureDefinition/patient-mpi-match') then
          s.Append(', mpi says '+e.search.getExtensionString('http://hl7.org/fhir/StructureDefinition/patient-mpi-match'));
        s.Append('</p>');
      end;

      if (sl <> '')  then
      begin
        s.append('<p><a href="'+sl+'">'+GetFhirMessage('THIS_RESOURCE', lang)+'</a> ');
      if not (r is TFhirBinary) then
        begin
        s.append(
          ', <a href="'+sl+'?_format=xml">XML</a> '+GetFhirMessage('OR', lang)+' '+
        '<a href="'+sl+'?_format=json">JSON</a> '+GetFhirMessage('NAME_REPRESENTATION', lang));
        s.append(
          ', '+GetFhirMessage('OR', lang)+' <a href="'+e.id+'/_history">'+GetFhirMessage('NAME_HISTORY', lang)+'</a>.');

        if (e.tags['z-edit-src'] <> '') then
          s.append(' Edit this as <a href="'+patchToWeb(e.tags['z-edit-src'])+'?srcformat=xml">XML</a> or <a href="'+patchToWeb(e.tags['z-edit-src'])+'?srcformat=json">JSON</a>.');

        {$IFDEF FHIR_QUESTIONNAIRERESPONSE}
        if e.tags['edit-form'] <> '' then
          if (r is TFHIRQuestionnaireResponse) then
          begin
            if (TFHIRQuestionnaireResponse(r).questionnaire <> nil) then
              s.append(' <a href="'+patchToWeb(e.tags['edit-form'])+'">Edit this Resource</a> (or <a href="'+TFHIRQuestionnaireResponse(r).questionnaire.reference+'">see the questionnaire</a>)')
          end
          else
            s.append(' <a href="'+patchToWeb(e.tags['edit-form'])+'">Edit this Resource</a> (or just see <a href="'+e.tags['edit-form']+'">the Questionnaire</a>)');
        if e.tags['edit-post'] <> '' then
          s.append(' Submit edited content by POST to '+e.tags['edit-post']);
        {$ENDIF}

        if assigned(FOnGetLink) then
        begin
          FOnGetLink(r, BaseURL, '', tail(e.id), tail(sl), link, text);
          if (link <> '') then
            s.append(' <a href="'+link+'">'+FormatTextToHTML(text)+'</a>');
        end;
        if not (r.resourceType in [frtProvenance, frtAuditEvent]) then
          s.append('. <a href="'+FBaseURL+'Provenance/target='+r.fhirType+'/'+r.id+'">provenance for this resource</a>');
        s.append('</br> Updated: '+e.tags['updated']+' by '+e.tags['author']+'</p>'+#13#10);
        end;
      end;

//      if e.deleted then
//        s.append('<p>'+GetFhirMessage('MSG_DELETED', lang)+'</p>')
//      else if r = nil then
//        s.append('<p>(--)</p>')
//      else
      if r is TFhirBinary then
      begin
        if StringStartsWith(TFhirBinary(r).ContentType, 'image/', true) then
          s.append('<img src="'+CODES_TFhirResourceType[r.resourcetype]+'/'+e.id+'">'+#13#10)
        else
          s.append('<pre class="xml">'+#13#10+'('+GetFhirMessage('NAME_BINARY', lang)+')'+#13#10+'</pre>'+#13#10);
      end
      else
      begin
        xml := TFHIRXmlComposer.create(Fworker.link, OutputStylePretty, lang);
        ss := TBytesStream.create();
        try
          if (r is TFhirDomainResource) and (TFhirDomainResource(r).text <> nil) and (TFhirDomainResource(r).text.div_ <> nil) then
            TFHIRXhtmlParser.Compose(TFhirDomainResource(r).text.div_, s, false, 2, relativeReferenceAdjustment);
          xml.Compose(ss, r, nil);
          s.append('<hr/>'+#13#10+'<pre class="xml">'+#13#10+FormatXMLToHTML(TENcoding.UTF8.getString(ss.bytes, 0, ss.size))+#13#10+'</pre>'+#13#10);
        finally
          ss.free;
          xml.free;
        end;
      end;
      end;
    end;
    s.append(
'<p><br/>'
+footer(FBaseUrl, lang, logid)
    );
    s.WriteToStream(stream);
  finally
    s.free;
  end;
end;

procedure TFHIRXhtmlComposer.ComposeResource(xml: TXmlBuilder; oResource: TFhirResource; links : TFhirBundleLinkList);
var
  oHtml : TFhirXHtmlNode;
  oDoc : TFhirXHtmlNode;
  oHead : TFhirXHtmlNode;
  oWork : TFhirXHtmlNode;
begin
  oHtml := TFhirXHtmlNode.create;
  try
    oHtml.NodeType := fhntDocument;
    oHtml.AddComment('Generated by Server automatically');
    oDoc := oHtml.AddChild('html');
    oHead := oDoc.AddChild('head');
    oWork := oHead.AddChild('title');
    oWork.AddText('test title');
    oWork := oHead.AddChild('link');
    oWork.SetAttribute('rel', 'Stylesheet');
    oWork.SetAttribute('href', '/css/fhir.css');
    oWork.SetAttribute('type', 'text/css');
    oWork.SetAttribute('media', 'screen');
    oWork := oDoc.AddChild('body');
    if (oResource is TFhirDomainResource) and (TFhirDomainResource(oResource).text <> nil) And (TFhirDomainResource(oResource).text.div_ <> nil) Then
    begin
      oWork.Attributes.addAll(TFhirDomainResource(oResource).text.div_.Attributes);
      oWork.ChildNodes.AddAll(TFhirDomainResource(oResource).text.div_.ChildNodes);
    end;
    TFHIRXhtmlParser.compose(oHtml, xml);
  finally
    oHtml.Free;
  end;
end;

constructor TFHIRXhtmlComposer.Create(worker: TFHIRWorkerContext; Style : TFHIROutputStyle; lang, BaseURL: String);
begin
  Create(worker, Style, lang);
  FBaseURL := BaseURL;
end;


destructor TFHIRXhtmlComposer.Destroy;
begin
  FSession.free;
  FTags.Free;
  inherited;
end;

function TFHIRXhtmlComposer.Extension: String;
begin
  result := '.html';
end;

class function TFHIRXhtmlComposer.Footer(base, lang, logId : String; tail : boolean = true): string;
begin
  result :=
    '</div>'+#13#10+
    ''+#13#10+
    ''+#13#10+
    '				</div>  <!-- /inner-wrapper -->'+#13#10+
    '            </div>  <!-- /row -->'+#13#10+
    '        </div>  <!-- /container -->'+#13#10+
    '    </div>  <!-- /segment-content -->'+#13#10+
    ''+#13#10+
    ''+#13#10+
    '	<div id="segment-footer" class="segment">  <!-- segment-footer -->'+#13#10+
    '		<div class="container">  <!-- container -->'+#13#10+
    '			<div class="inner-wrapper">'+#13#10+
    '				<p>'+#13#10+
    '        <a href="'+base+'" style="color: gold">'+GetFhirMessage('SERVER_HOME', lang)+'</a>.&nbsp;|&nbsp;FHIR &copy; HL7.org 2011+. &nbsp;|&nbsp; FHIR '+GetFhirMessage('NAME_VERSION', lang)+' <a href="'+FHIR_SPEC_URL+'" style="color: gold">'+FHIR_GENERATED_VERSION+'</a>'+#13#10+
    '        | Request-id: '+logId+
    '        </span>'+#13#10+
    '        </p>'+#13#10+
    '			</div>  <!-- /inner-wrapper -->'+#13#10+
    '		</div>  <!-- /container -->'+#13#10+
    '	</div>  <!-- /segment-footer -->'+#13#10+
    ''+#13#10+
    ''+#13#10+
    '	<div id="segment-post-footer" class="segment hidden">  <!-- segment-post-footer -->'+#13#10+
    '		<div class="container">  <!-- container -->'+#13#10+
    '		</div>  <!-- /container -->'+#13#10+
    '	</div>  <!-- /segment-post-footer -->'+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10+
    '      <!-- JS and analytics only. -->'+#13#10+
    '      <!-- Bootstrap core JavaScript'+#13#10+
    '================================================== -->'+#13#10+
    '  <!-- Placed at the end of the document so the pages load faster -->'+#13#10+
    '<script src="/assets/js/jquery.js"/>'+#13#10+
    '<script src="/dist/js/bootstrap.min.js"/>'+#13#10+
    '<script src="/assets/js/respond.min.js"/>'+#13#10+
    ''+#13#10+
    '<script src="/assets/js/fhir.js"/>'+#13#10+
    ''+#13#10+
    '  <!-- Analytics Below'+#13#10+
    '================================================== -->'+#13#10+
    ''+#13#10+
    ''+#13#10+
    ''+#13#10;
if tail then
  result := result +
    '</body>'+#13#10+
    '</html>'+#13#10;
end;

class function TFHIRXhtmlComposer.Header(Session : TFhirSession; base, lang, version: String): String;
var
   id : TFHIRCompartmentId;
   f : boolean;
begin
  result :=
    '	<div id="segment-navbar" class="segment">  <!-- segment-breadcrumb -->'+#13#10+
    '		<div id="stripe"> </div>'+#13#10+
    '		<div class="container">  <!-- container -->'+#13#10+
    '		<div style="background-color: #ad1f2f; padding: 6px; color: white;">  <!-- container -->'+#13#10;


  result := result +
    '  <a href="http://www.hl7.org/fhir" style="color: gold" title="'+GetFhirMessage('MSG_HOME_PAGE_TITLE', lang)+'"><img border="0" src="/icon-fhir-16.png" style="vertical-align: text-bottom"/> <b>FHIR</b></a>'#13#10+
    ''#13#10+
    '  &copy; HL7.org'#13#10+
    '  &nbsp;|&nbsp;'#13#10+
    '  <a href="/" style="color: gold">'+GetFhirMessage('SERVER_HOME', lang)+'</a> '+
    '  &nbsp;|&nbsp;'#13#10+
    '  <a href="http://www.healthintersections.com.au" style="color: gold">Health Intersections</a> '+GetFhirMessage('NAME_SERVER', lang)+' v'+version+#13#10+
    '  &nbsp;|&nbsp;'#13#10+
    '  <a href="'+FHIR_SPEC_URL+'" style="color: gold">FHIR '+GetFhirMessage('NAME_VERSION', lang)+' '+FHIR_GENERATED_VERSION+'</a>'#13#10;

  if (session <> nil)  then
  begin
    result := result +'&nbsp;|&nbsp;';
    if session.canGetUser then
      result := result +'User: '+FormatTextToXml(Session.SessionName, xmlText)
    else
      result := result +'User: [n/a]';
    if session.UserEvidence <> userAnonymous then
      result := result +'&nbsp; <a href="'+base+'/logout" title="Log Out"><img src="/logout.png"></a>';
    if session.Compartments.Count > 0 then
    begin
      if session.Compartments.Count = 1 then
        result := result+'  &nbsp;'#13#10+'</div><div style="background-color: #e5e600; padding: 6px; color: black;"> This session limited to '+CODES_TFhirResourceType[session.Compartments[0].Enum]+' '
      else
        result := result+'  &nbsp;'#13#10+'</div><div style="background-color: #e5e600; padding: 6px; color: black;"> . This session limited to the following compartments: ';
      f := true;
      for id in session.Compartments do
      begin
        if f then
          f := false
        else
          result := result +', ';
        result := result + id.ToString;
      end;
    end;
  end;

  result := result +
    '  &nbsp;'#13#10+
    '		</div>  <!-- /container -->'+#13#10+
    '		</div>  <!-- /container -->'+#13#10+
    '</div>'#13#10+
    ''#13#10;
//    if FFacebookLike and (FOauthUrl <> '') then
//      result := result + '<iframe src="https://www.facebook.com/plugins/like.php?href='+FOauthUrl+'" scrolling="no" frameborder="0" style="border:none; width:450px; height:30px"></iframe>'#13#10;

  result := result +
    '	<!-- /segment-breadcrumb -->'+#13#10+
    ''+#13#10+
    '	<div id="segment-content" class="segment">  <!-- segment-content -->'+#13#10+
    '	<div class="container">  <!-- container -->'+#13#10+
    '            <div class="row">'+#13#10+
    '            	<div class="inner-wrapper">'+#13#10+
    ' <div id="div-cnt" class="col-9">'+#13#10+
    ''+#13#10+
    ''+#13#10;
end;

function TFHIRXhtmlComposer.MimeType: String;
begin
  result := 'text/html; charset=UTF-8';
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

class function TFHIRXhtmlComposer.PageLinks: String;
begin
  result :=
    '  <meta charset="utf-8"/>'+#13#10+
    '  <meta charset="utf-8" http-equiv="X-UA-Compatible" content="IE=edge" />'+#13#10+
    '  <meta content="width=device-width, initial-scale=1.0" name="viewport"/>'+#13#10+
    '  <meta content="http://hl7.org/fhir" name="author"/>'+#13#10+
    ''+#13#10+
    '  <link rel="stylesheet" href="/fhir.css"/>'+#13#10+
    ''+#13#10+
    ''+#13#10+
    '    <!-- Bootstrap core CSS -->'+#13#10+
    '  <link rel="stylesheet" href="/dist/css/bootstrap.css"/>'+#13#10+
    '  <link rel="stylesheet" href="/assets/css/bootstrap-fhir.css"/>'+#13#10+
    ''+#13#10+
    '    <!-- Project extras -->'+#13#10+
    '  <link rel="stylesheet" href="/assets/css/project.css"/>'+#13#10+
    '  <link rel="stylesheet" href="/assets/css/pygments-manni.css"/>'+#13#10+
    ''+#13#10+
    '    <!-- FHIR Server stuff -->'+#13#10+
    '  <link rel="stylesheet" href="/css/tags.css"/>'+#13#10+
    ''+#13#10+
    '    <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->'+#13#10+
    '    <!-- [if lt IE 9]>'+#13#10+
    '  <script src="/assets/js/html5shiv.js"></script>'+#13#10+
    '  <script src="/assets/js/respond.min.js"></script>'+#13#10+
    '  <![endif] -->'+#13#10+
    ''+#13#10+
    '    <!-- Favicons -->'+#13#10+
    '  <link sizes="144x144" rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-144-precomposed.png"/>'+#13#10+
    '  <link sizes="114x114" rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-114-precomposed.png"/>'+#13#10+
    '  <link sizes="72x72" rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-72-precomposed.png"/>'+#13#10+
    '  <link rel="apple-touch-icon-precomposed" href="/assets/ico/apple-touch-icon-57-precomposed.png"/>'+#13#10+
    '  <link rel="shortcut icon" href="/assets/ico/favicon.png"/>'+#13#10;
end;

function TFHIRXhtmlComposer.PresentTags(aType : TFhirResourceType; target : String; meta: TFhirMeta; c : integer): String;
var
  i : integer;
//  lbl : string;
//  clss, typ : string;
begin
  if tags.count = 0 then
    result := '(no tags)'
  else
  begin
    result := '';
    for i := 0 to tags.count - 1 do
    begin
{ todo-bundle
      lbl := tags[i].label_;
      if lbl = '' then
        lbl := URLTail(tags[i].term);
      if (length(lbl) > 20) then
        lbl := Copy(lbl, 1, 20)+'..';

      if tags[i].scheme = TAG_FHIR_SCHEME_PROFILE then
      begin
        clss := 'tag-profile';
        typ := 'Profile: ';
      end
      else if tags[i].scheme = TAG_FHIR_SCHEME_SECURITY then
    begin
        clss := 'tag-security';
        typ := 'Security: ';
      end
      else
        clss := 'tag';

      if aType = frtNull then
        result := result + '<a href="'+FBaseUrl+'_search?'+paramForScheme(tags[i].scheme)+'='+EncodeMIME(tags[i].term)+'" class="'+clss+'" title="'+typ+tags[i].term+'">'+lbl+'</a>'
      else
      begin
        result := result + '<a href="'+FBaseUrl+CODES_TFhirResourceType[aType]+'/_search?'+paramForScheme(tags[i].scheme)+'='+EncodeMIME(tags[i].term)+'" class="'+clss+'" title="'+typ+tags[i].term+'">'+lbl+'</a>';
        if (target <> '') then
          result := result + '<a href="javascript:deleteTag('''+target+'/_delete'', '''+tags[i].scheme+''', '''+tags[i].term+''')" class="tag-delete" title="Delete '+tags[i].term+'">-</a>'
      end;
      result := result + '&nbsp;';
    }
    end;
  end;
  if target <> '' then
    result := result +'&nbsp; <a id="tb'+inttostr(c)+'" class="tag" title="Add a tag" href="javascript:addTag(''tb'+inttostr(c)+''', '''+FBaseUrl+''', '''+target+''')">+</a>';
end;

class function TFHIRXhtmlComposer.ResourceLinks(a : String; lang, base : String; count : integer; bTable, bPrefixLinks : boolean; canRead : boolean): String;
var
  bef, aft, pfx, pfxp : String;
begin
  if bPrefixLinks then
  begin
    pfx := base+'/'+a+'/';
    pfxp := base+'/'+'StructureDefinition/'
  end
  else
  begin
    pfxp := '../StructureDefinition/';
    pfx := '';
  end;

  if bTable then
  begin
    bef := '<td>';
    aft := '</td>';
  end
  else
  begin
    bef := '&nbsp;';
    aft := '';
  end;
  result := bef + a + aft;
  if not bTable then
    result := result + ':';
  if count > -1 then
    result := result + bef + inttostr(count) + aft;
  if a = 'Binary' then
    result := result + bef + 'n/a' + aft
  else
    result := result + bef + '<a class="button" href="'+pfxp+a+'">'+GetFhirMessage('NAME_PROFILE', lang)+'</a>' + aft;
  if canRead then
  begin
    result := result + bef + '<a class="button" href="'+pfx+'_history">'+GetFhirMessage('NAME_UPDATES', lang)+'</a>' + aft;
    if a = 'Binary' then
      result := result + bef + 'n/a' + aft
    else
      result := result + bef + '<a class="button" href="'+pfx+'_search">'+GetFhirMessage('NAME_SEARCH', lang)+'</a>' + aft;
    if bTable then
      result := result + bef + '<a class="tag" href="'+pfx+'_tags">'+GetFhirMessage('NAME_TAGS', lang)+'</a>' + aft;
  end
  else if bTable then
    result := result + bef + aft + bef + aft + bef + aft
  else
    result := result + bef + aft + bef + aft;
end;

function TFHIRXhtmlComposer.ResourceMediaType: String;
begin
  result := 'text/html; charset=UTF-8';
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

function TFHIRComposer.Compose(oResource: TFhirResource; links: TFhirBundleLinkList): String;
var
  stream : TBytesStream;
begin
  stream := TBytesStream.create;
  try
    compose(stream, oResource, links);
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

function TFHIRXmlParserBase.ParseDomainResource(element: TMXmlElement; path : String): TFhirResource;
var
  child : TMXmlElement;
begin
  child := FirstChild(element);
  result := ParseResource(child, path);
  try
    child := NextSibling(child);
    if (child <> nil) then
      UnknownContent(child, path);
    result.link;
  finally
    result.free;
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

constructor TFHIRComposer.Create(worker: TFHIRWorkerContext; Style : TFHIROutputStyle; lang: String);
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

procedure TFHIRXhtmlComposer.SetSession(const Value: TFhirSession);
begin
  FSession.free;
  FSession := Value;
end;


procedure TFHIRXhtmlComposer.SetTags(const Value: TFHIRTagList);
begin
  FTags.free;
  FTags := Value;
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

procedure TFHIRTurtleComposerBase.Compose(stream: TStream; oResource: TFhirResource; links: TFhirBundleLinkList);
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
      base.addUriPredicate('a', 'fhir:'+CODES_TFHIRResourceType[oResource.ResourceType]);
      base.addUriPredicate('fhir:nodeRole', 'fhir:treeRoot');
      if URL <> '' then
        Fttl.addObject(url, base.link)
      else
        Fttl.addObject('_', base.link);
      composeResource(base, oResource);
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

procedure TFHIRTurtleComposerBase.ComposeExpression(stream: TStream; expr: TFHIRPathExpressionNode; items: TFHIRObjectList; types: TAdvStringSet);
begin
  raise Exception.Create('not implemented yet');
end;

procedure TFHIRTurtleComposerBase.ComposeInnerResource(this: TTurtleComplex; parentType, name: String; elem: TFhirResource; useType: boolean; index: integer);
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
    base.addUriPredicate('a', 'fhir:'+CODES_TFHIRResourceType[elem.ResourceType]);
    Fttl.addObject(url, base.link);
    composeResource(base, elem);
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

procedure TFHIRTurtleComposerBase.ComposeResource(parent :  TTurtleComplex; oResource: TFhirResource);
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

procedure TFHIRTurtleComposerBase.ComposeResource(xml: TXmlBuilder; oResource: TFhirResource; links: TFhirBundleLinkList);
begin
  raise Exception.Create('not implemented yet');
end;

function TFHIRTurtleComposerBase.MimeType: String;
begin
  result := 'text/turtle';
end;
{$ENDIF}

{ TFHIRTextParser }

function TFHIRTextParser.ParseDT(rootName: String; type_: TFHIRTypeClass): TFHIRType;
begin
  raise Exception.Create('Not supported');
end;

class function TFHIRTextParser.ParseFile(worker: TFHIRWorkerContext; lang, filename: String): TFHIRResource;
var
  t : TFHIRTextParser;
begin
  t := TFHIRTextParser.Create(worker.link, lang);
  try
    t.ParseFile(filename);
    result := t.resource.Link;
  finally
    t.Free;
  end;
end;

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

procedure TFHIRTextComposer.Compose(stream: TStream; oResource: TFhirResource; links: TFhirBundleLinkList);
begin
  case oResource.ResourceType of
    frtOperationOutcome : StringToStream(render(oResource as TFHIROperationOutcome), stream, TEncoding.UTF8);
  else
    raise Exception.Create('Text format not supported for '+oResource.fhirtype);
  end;
end;

procedure TFHIRTextComposer.ComposeResource(xml: TXmlBuilder; oResource: TFhirResource; links: TFhirBundleLinkList);
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

function TFHIRTextComposer.render(op: TFHIROperationOutcome): String;
var
  b : TStringBuilder;
  iss : TFhirOperationOutcomeIssue;
begin
  b := TStringBuilder.create;
  try
    if op.issueList.Count = 0 then
      b.Append('All Ok (no issues)')
    else
      for iss in op.issueList do
      begin
        b.Append(CODES_TFhirIssueSeverityEnum[iss.severity]);
        b.Append(': ');
        b.Append(gen(iss.details));
        {$IFDEF FHIR2}
        if (iss.locationList.Count > 0) then
        {$ELSE}
        if (iss.expressionList.Count > 0) then
        {$ENDIF}
        begin
          b.Append(' @ ');
          {$IFDEF FHIR2}
          b.Append(iss.locationList[0].Value);
          {$ELSE}
          b.Append(iss.expressionList[0].Value);
          {$ENDIF}
        end;
        b.Append(#13#10);
      end;
    result := b.toString;
  finally
    b.free;
  end;
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

    resource := ParseResource(p.Value);
    resource.Tags['rdf-url'] := p.URL.uri;
  finally
    Fdoc.Free;
  end;
end;

function TFHIRTurtleParserBase.ParseDataType(obj: TTurtleComplex; name: String; type_: TFHIRTypeClass): TFHIRType;
begin
  raise Exception.Create('Must be overriden in '+className);
end;

function TFHIRTurtleParserBase.ParseDT(rootName: String; type_: TFHIRTypeClass): TFHIRType;
begin
  raise Exception.Create('not supported');
end;

class function TFHIRTurtleParserBase.ParseFile(worker: TFHIRWorkerContext; lang, filename: String): TFHIRResource;
var
  j : TFHIRTurtleParser;
begin
  j := TFHIRTurtleParser.Create(worker.link, lang);
  try
    j.ParseFile(filename);
    result := j.resource.Link;
  finally
    j.Free;
  end;
end;

function TFHIRTurtleParserBase.ParseInnerResource(obj: TTurtleObject): TFHIRResource;
var
  c : TTurtleComplex;
begin
  if obj = nil then
    result := nil
  else
  begin
    if obj is TTurtleComplex then
      c := obj as TTurtleComplex
    else if (obj is TTurtleURL) then
    begin
      c := FDoc.getObject(TTurtleURL(obj).uri);
      if c = nil then
        raise Exception.Create('Unable to resolve internal resource reference in RDF - to '+TTurtleURL(obj).uri)
    end
    else
      raise Exception.Create('Unable to process internal resource reference in RDF');
    result := ParseResource(c);
  end;
end;

function TFHIRTurtleParserBase.ParseResource(obj: TTurtleComplex): TFHIRResource;
begin
  raise Exception.Create('Need to override ParseResource() in '+className);
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

{ TFHIRNDJsonComposer }

procedure TFHIRNDJsonComposer.Compose(stream: TStream; oResource: TFhirResource; links: TFhirBundleLinkList);
var
  oStream : TAdvVCLStream;
  json : TFHIRJsonComposer;
  be : TFhirBundleEntry;
  first : boolean;
  ch : char;
begin
  ch := #10;
  if oResource.ResourceType = frtBundle then
  begin
    first := true;
    for be in TFHIRBundle(oResource).entryList do
    begin
      if first then
        first := false
      else
        stream.Write(ch, 1);
      if be.resource <> nil then
      begin
        json := TFHIRJsonComposer.Create(FWorker.link, OutputStyleNormal, lang);
        try
          json.Compose(stream, be.resource, links);
        finally
          json.Free;
        end;
      end
      else if be.tag is TAdvBuffer then
      begin
        TAdvBuffer(be.tag).SaveToStream(stream);
      end;
    end;
  end
  else
  begin
    json := TFHIRJsonComposer.Create(FWorker.link, OutputStyleNormal, lang);
    try
      json.Compose(stream, oResource, links);
    finally
      json.Free;
    end;
  end;
end;

End.
