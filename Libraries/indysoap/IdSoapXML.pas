{
IndySOAP: IdSoapXML

Abstract XML interface to multiple XML implementations

}

{
 note: IndySoap uses a namespace aware DOM while reading XML, but namespaces
       are handled outside this unit when writing a DOM. So at times the DOM
       that is available to a progam is not namespace aware (when hooking the
       events OnSendMessageDom and OnSendExceptionDom). This may be reviewed
       in the future

}

Unit IdSoapXML;

Interface

{$I IdSoapDefines.inc}

Uses
  Windows,
  ActiveX,
  ComObj,
  Classes,
  Contnrs,
  IdSoapComponent,
  IdSoapDebug,
  IdSoapMsXml,
  IdSoapClasses,
{$IFDEF UNICODE}
  Variants,
  System.Types,
  Xml.Internal.XmlRulesUtils,
  Xml.xmldom,
  Xml.xmldoc,
  Xml.xmlintf,
  Xml.XMLConst,
  Xml.adomxmldom,
{$ELSE}
  IdSoapOpenXml,
  IdSoapOpenXmlUcl,
{$ENDIF}
  IdSoapUtilities;

{$IFDEF UNICODE}
Type
  TdomElement = IDOMElement;
  TdomAttr = IDOMAttr;
  TdomNode = IDomNode;
  TDomImplementation = IDomImplementation;
  TdomDocument = IDOMDocument;
{$ELSE}
Const
  ELEMENT_NODE = IdSoapOpenXml.ntElement_Node;
{$ENDIF}

Type

  TIdSoapXmlProvider = (
     xpOpenXML,  // OpenXML provider built into IndySoap
     xpMsXml,    // MsXML 4.1 - must be installed
     xpCustom);  // has known issues supporting characters in the 128 - 256 range

  TIdSoapXmlElement = Class;

  TIdSoapXmlDom = Class (TIdBaseObject)
  Protected
    FRoot : TIdSoapXMLElement;
    FBuilding : Boolean;
    Procedure DoRead(ASource : TStream); Virtual; abstract;
    Procedure Clear; Virtual;
  Public
    Constructor Create;
    destructor Destroy; Override;
    Property Root : TIdSoapXMLElement Read FRoot;

    Procedure Read(ASource : TStream); Overload;

    // Source will be interpreted as a stream of bytes. (which may be UTF encoded)
    // D2009: Actual unicode strings are not yet handled
    Procedure Read(ASource : AnsiString); Overload;

    Procedure StartBuild(AName, ANS : String); Virtual; abstract;
    Procedure writeUTF16(ADest : TStream; ANoXMLDec : Boolean = False); Virtual; abstract;
    Procedure writeUTF8(ADest : TStream; ANoXMLDec : Boolean = False); Virtual; abstract;

    Function ImportElement(AElem : TIdSoapXmlElement) : TIdSoapXmlElement; Virtual; abstract;
  End;

  TIdSoapXmlElement = Class (TIdBaseObject)
  Private
    FDom : TIdSoapXmlDom;
    FChildren : TObjectList;
    FParentNode : TIdSoapXmlElement;
    FSibling : TIdSoapXmlElement;
    FKnowNilAttribute : Boolean;
    FNilAttribute : Boolean;
    FKnowXSIType : Boolean;
    FXSIType : String;
    FKnowHasID : Boolean;
    FHasID : Boolean;
    FLastFindName : String;
    FLastFind : Integer;
    FLastIndex : Integer;
    Function GetChildCount  : Integer;
    Function GetChild(AIndex : Integer):TIdSoapXmlElement;
    Function GetFirstChild  : TIdSoapXmlElement;
    Procedure PrivAppendChild(AChild : TIdSoapXmlElement);
    Procedure PrivInsertChild(AIndex : Integer; AChild : TIdSoapXmlElement);
    Procedure PrivRemoveChild(AElement : TIdSoapXmlElement);
    Function getIndexOf: Integer;
  Protected
    Function GetAttributeCount : Integer;             Virtual; abstract;
    Function GetName : WideString;                    Virtual; abstract;
    Function GetNamespace : WideString;               Virtual; abstract;
    Function GetNodeName : WideString;                Virtual; abstract;
    Function GetHasText : Boolean;                    Virtual; abstract;
    Function GetAsXML : AnsiString;                   Virtual; abstract;
    Function GetTextContentA:String;                  Virtual; abstract;
    Procedure SetTextContentA(AValue : String);       Virtual; abstract;
    Function GetTextContentW:WideString;              Virtual; abstract;
    Procedure SetTextContentW(AValue : WideString);   Virtual; abstract;
  Public
    Constructor Create(ADom : TIdSoapXmlDom; AParent : TIdSoapXmlElement);
    destructor Destroy; Override;

    Property DOM : TIdSoapXmlDom Read FDom;

    // DOM navigation
    Property ParentNode  : TIdSoapXmlElement Read FParentNode;
    Property ChildCount  : Integer           Read GetChildCount;
    Property Child[AIndex : Integer]: TIdSoapXmlElement Read GetChild;
    Property indexOf : Integer Read getIndexOf;

    Property FirstChild  : TIdSoapXmlElement Read GetFirstChild;
    Property NextSibling : TIdSoapXmlElement Read FSibling;
    Function FirstElement(ANamespace, AName : WideString) : TIdSoapXmlElement;
    Function NextElement(ANamespace, AName : WideString) : TIdSoapXmlElement;
    Function NthChildElement(ANamespace, AName : WideString; AIndex : Integer) : TIdSoapXmlElement;
    Function ChildElementCount(ANamespace, AName : WideString) : Integer;
    Function FindElementAnyNS(AName : WideString) : TIdSoapXmlElement;
    Function AppendChild(AName, ANS : WideString):TIdSoapxmlelement;                          Virtual; abstract;
    Function InsertChild(AIndex : Integer; AName, ANS : WideString):TIdSoapxmlelement;        Virtual; abstract;
    Procedure removeChild(AElement : TIdSoapXmlElement);                                 Virtual; abstract;
    Procedure ClearChildren();                                                           Virtual; abstract;
    procedure appendComment(source : String);                                            Virtual; abstract;
    Property Element[ANamespace, AName : WideString] : TIdSoapXmlElement Read FirstElement; Default;

    // attributes
    Property AttributeCount : Integer Read GetAttributeCount;
    Function hasAttribute(Const ANamespace, AName : WideString): Boolean;                Virtual; abstract;
    Function getAttribute(Const ANamespace, AName : WideString): WideString;             Virtual; abstract;
    Function getAttributeName(i : Integer; Var VNamespace, VName : WideString) :Boolean; Virtual; abstract;
    Procedure setAttribute(ANS, AName, AValue : WideString); Virtual; abstract;

    // names and namespaces
    Function ResolveXMLNamespaceCode(ANamespace, ALocation : WideString) : String;       Virtual; abstract;
    Property Name : WideString Read GetName;
    Property Namespace : WideString Read GetNamespace;
    Property NodeName : WideString Read GetNodeName;

    // text content. Code is xml encoded contents
    Property HasText : Boolean Read GetHasText;
    Property TextContentA : String Read GetTextContentA Write SetTextContentA;
    Property TextContentW : WideString Read GetTextContentW Write SetTextContentW;

    // utilities
    Procedure GrabChildren(AElem : TIdSoapXmlElement; AOtherDOM : Boolean);              Virtual; abstract;
    Function Path : String;
    Function HasNilAttribute : Boolean;
    Function GetSchemaInstanceAttribute(AAttributeName: String): String;
    Function GetXSIType: String;
    Function HasID : Boolean;

    // xml content
    Property AsXML : AnsiString Read GetAsXML;
    Procedure BuildChildFromXML(ASrc : WideString);                                      Virtual; abstract;
  End;

  {$IFDEF UNICODE}
  TIdSoapDelphiXmlElement = Class;

  TIdSoapDelphiXmlDom = Class (TIdSoapXmlDom)
  Private
    dom : TXMLDocument;
    doc : IXMLDocument;
    function CloneNodeToDoc(const SourceNode: IXMLNode; Deep: Boolean = True): IXMLNode;
    procedure CopyChildNodes(SrcNode, DestNode: IXMLNode);
    Procedure IterateChildren(AElem : TIdSoapDelphiXmlElement);
  Protected
    Procedure DoRead(ASource : TStream); Override;
    Procedure Clear; Override;
  Public
    Procedure StartBuild(AName, ANS : String); Override;
    Procedure writeUTF16(ADest : TStream; ANoXMLDec : Boolean = False); Override;
    Procedure writeUTF8(ADest : TStream; ANoXMLDec : Boolean = False); Override;
    Function ImportElement(AElem : TIdSoapXmlElement) : TIdSoapXmlElement; Override;
  End;

  TIdSoapDelphiXmlElement = Class (TIdSoapXmlElement)
  Private
    FElement : IXMLNode;
  Protected
    Function GetAttributeCount : Integer;             Override;
    Function GetName : WideString;                    Override;
    Function GetNamespace : WideString;               Override;
    Function GetNodeName : WideString;                Override;
    Function GetHasText : Boolean;                    Override;
    Function GetAsXML : AnsiString;                   Override;
    Function GetTextContentA:String;                  Override;
    Procedure SetTextContentA(AValue : String);       Override;
    Function GetTextContentW:WideString;              Override;
    Procedure SetTextContentW(AValue : WideString);   Override;
  Public
    Constructor Create(ADom : TIdSoapXmlDom; AParent : TIdSoapXmlElement; aNode : IXMLNode);

    Function AppendChild(AName, ANS : WideString):TIdSoapxmlelement;                          Override;
    Function InsertChild(AIndex : Integer; AName, ANS : WideString):TIdSoapxmlelement;        Override;
    Procedure removeChild(AElement : TIdSoapXmlElement);                                 Override;
    Procedure ClearChildren();                                                           Override;
    procedure appendComment(source : String);                                            Override;
    Function hasAttribute(Const ANamespace, AName : WideString): Boolean;                Override;
    Function getAttribute(Const ANamespace, AName : WideString): WideString;             Override;
    Function getAttributeName(i : Integer; Var VNamespace, VName : WideString) :Boolean; Override;
    Procedure setAttribute(ANS, AName, AValue : WideString);                                  Override;
    Function ResolveXMLNamespaceCode(ANamespace, ALocation : WideString) : String;       Override;
    Procedure BuildChildFromXML(ASrc : WideString);                                      Override;
    Procedure GrabChildren(AElem : TIdSoapXmlElement; AOtherDOM : Boolean);              Override;
  End;
  {$ELSE}
{=== open XML =================================================================}
Type
  // XE3 aliasing for D5 - D7
  Tox4DomImplementation = TDomImplementation;
  Tox4domDocument = TDomDocument;
  Tox4domElement = TDomElement;
  Tox4domNode = TdomNode;
  TIdSoapOpenXmlElement = Class;

  TIdSoapOpenXmlDom = Class (TIdSoapXmlDom)
  Private
    FDom : Tox4DomImplementation;
    FDoc : Tox4domDocument;
    FDomErr : String;
    {$IFNDEF UNICODE}
    Procedure DOMReadError(ASender: TObject; AError: TdomError; Var VGo: Boolean);
    {$ENDIF}
    Procedure IterateChildren(AElem : TIdSoapOpenXmlElement);
  Protected
    Procedure DoRead(ASource : TStream); Override;
    Procedure Clear; Override;
  Public
    Procedure StartBuild(AName, ANS : String); Override;
    Procedure writeUTF16(ADest : TStream; ANoXMLDec : Boolean = False); Override;
    Procedure writeUTF8(ADest : TStream; ANoXMLDec : Boolean = False); Override;
    Function ImportElement(AElem : TIdSoapXmlElement) : TIdSoapXmlElement; Override;
  End;

  TIdSoapOpenXmlElement = Class (TIdSoapXmlElement)
  Private
    FElement : Tox4domElement;
  Protected
    Function GetAttributeCount : Integer;             Override;
    Function GetName : WideString;                    Override;
    Function GetNamespace : WideString;               Override;
    Function GetNodeName : WideString;                Override;
    Function GetHasText : Boolean;                    Override;
    Function GetAsXML : AnsiString;                   Override;
    Function GetTextContentA:String;                  Override;
    Procedure SetTextContentA(AValue : String);       Override;
    Function GetTextContentW:WideString;              Override;
    Procedure SetTextContentW(AValue : WideString);   Override;
  Public
    Constructor Create(ADom : TIdSoapXmlDom; AParent : TIdSoapXmlElement; AElement : Tox4domElement);

    Function AppendChild(AName, ANs : WideString):TIdSoapxmlelement;                          Override;
    Function InsertChild(AIndex : Integer; AName, ANs : WideString):TIdSoapxmlelement;        Override;
    Procedure removeChild(AElement : TIdSoapXmlElement);                                 Override;
    Procedure ClearChildren();                                                           Override;
    procedure appendComment(source : String);                                            Override;
    Function hasAttribute(Const ANamespace, AName : WideString): Boolean;                Override;
    Function getAttribute(Const ANamespace, AName : WideString): WideString;             Override;
    Function getAttributeName(i : Integer; Var VNamespace, VName : WideString) :Boolean; Override;
    Procedure setAttribute(ANs, AName, AValue : WideString);                                  Override;
    Function ResolveXMLNamespaceCode(ANamespace, ALocation : WideString) : String;       Override;
    Procedure BuildChildFromXML(ASrc : WideString);                                      Override;
    Procedure GrabChildren(AElem : TIdSoapXmlElement; AOtherDOM : Boolean);              Override;

    // expose raw XML for direct access through TIdSoapRawXML
    Property XMLElement : Tox4domElement Read FElement;
  End;
  {$ENDIF}

{$IFNDEF DESIGNTIME}
Var
  GMsXmlProgId_DOM : String;
  GMsXmlProgId_FTDOM : String;
  GMsXmlProgId_SCHEMA : String;
  GMsXmlProgId_XSLT : String;
  GMsXmlProgId_XSLP : String;
  GMsXmlProgId_SAX : String;


Procedure DetermineMsXmlProgId;
Function LoadMsXMLDom : IXMLDomDocument2;
Function LoadMsXMLDomV(isFree : boolean = false) : Variant;

{=== MSXML =================================================================}

Type
  TIdSoapMSXmlElement = Class;

  TIdSoapMSXmlDom = Class (TIdSoapXmlDom)
  Private
    FDom : IXMLDomDocument2;
    FSchemas: IXMLDOMSchemaCollection;
    FParseError : IXMLDOMParseError;
    Procedure IterateChildren(AElem : TIdSoapMSXmlElement);
  Protected
    Procedure Clear; Override;
    Procedure DoRead(ASource : TStream); Override;
  Public
    Procedure StartBuild(AName, ANS : String); Override;
    Procedure writeUTF16(ADest : TStream; ANoXMLDec : Boolean = False); Override;
    Procedure writeUTF8(ADest : TStream; ANoXMLDec : Boolean = False); Override;
    Function ImportElement(AElem : TIdSoapXmlElement) : TIdSoapXmlElement; Override;

    Property schemas : IXMLDOMSchemaCollection read FSchemas write FSchemas;
    Property ParseError : IXMLDOMParseError read FParseError write FParseError;
    Property Impl : IXMLDomDocument2 read FDom;
  End;

  TIdSoapMSXmlElement = Class (TIdSoapXmlElement)
  Private
    FElem : IXMLDOMElement;
  Protected
    Function GetAttributeCount : Integer;             Override;
    Function GetName : WideString;                    Override;
    Function GetNamespace : WideString;               Override;
    Function GetNodeName : WideString;                Override;
    Function GetHasText : Boolean;                    Override;
    Function GetAsXML : AnsiString;                   Override;
    Function GetTextContentA:String;                  Override;
    Procedure SetTextContentA(AValue : String);       Override;
    Function GetTextContentW:WideString;              Override;
    Procedure SetTextContentW(AValue : WideString);   Override;
  Public
    Constructor Create(ADom : TIdSoapXmlDom; AParent : TIdSoapXmlElement; AElem : IXMLDOMElement);
    Function AppendChild(AName, ANS : WideString):TIdSoapxmlelement;                          Override;
    procedure appendComment(source : String);                                            Override;
    Function InsertChild(AIndex : Integer; AName, ANS : WideString):TIdSoapxmlelement;        Override;
    Procedure ClearChildren();                                                           Override;
    Procedure removeChild(AElement : TIdSoapXmlElement);                                 Override;
    Function hasAttribute(Const ANamespace, AName : WideString): Boolean;                Override;
    Function getAttribute(Const ANamespace, AName : WideString): WideString;             Override;
    Function getAttributeName(i : Integer; Var VNamespace, VName : WideString) :Boolean; Override;
    Procedure setAttribute(ANS, AName, AValue : WideString);                                  Override;
    Function ResolveXMLNamespaceCode(ANamespace, ALocation : WideString) : String;       Override;
    Procedure BuildChildFromXML(ASrc : WideString);                                      Override;
    Procedure GrabChildren(AElem : TIdSoapXmlElement; AOtherDOM : Boolean);              Override;

    // expose raw XML for direct access through TIdSoapRawXML
    Property XMLElement : IXMLDOMElement Read FElem;
  End;
{$ENDIF DESIGNTIME}


{=== Custom ======================================================================}
Type
  TIdSoapCustomElement = Class;

  TIdSoapCustomDom = Class (TIdSoapXmlDom)
  Private
    FSrc : PChar;
    FLength : Integer;
    FCursor : Integer;
    FIsUTF8 : Boolean;
    Procedure CheckForBOM;
    Function ReadToken(ASkipWhitespace : Boolean): String;
    Function ReadToNextChar(ACh : Char): String;
    Procedure ReadAttribute(AName : String; AOwner: TIdSoapCustomElement);
    Function ReadElement(AParent : TIdSoapXMLElement; ADefaultNamespace : String) : TIdSoapCustomElement;
  Protected
    Procedure DoRead(ASource : TStream); Override;
  Public
    Procedure StartBuild(AName, ANS : String); Override;
    Procedure StripWhitespace;
    Procedure writeUTF16(ADest : TStream; ANoXMLDec : Boolean = False); Override;
    Procedure writeUTF8(ADest : TStream; ANoXMLDec : Boolean = False); Override;
    Procedure WritePretty(ADest : TStream);
    Function ImportElement(AElem : TIdSoapXmlElement) : TIdSoapXmlElement; Override;
  End;

  TIdSoapCustomAttribute = Class (TIdBaseObject)
  Private
    FNs : String;
    FName : String;
    FContent : String;
  End;

  TIdSoapCustomElement = Class (TIdSoapXmlElement)
  Private
    FNodeName : String;
    FNs : String;
    FName : String;
    FContent : String;
    FContentLength : integer;
    FXMLNs : TObjectList;
    FAttr : TObjectList;
    Procedure WriteToString(Var VCnt : String; Var VLen : Integer; iIndent : Integer);
    Function ResolveNamespaces(ADefaultNamespace : String) : String;
    Procedure StartLoadContent;
    Procedure AddToContent(const sContent:String);
    Procedure CloseLoadContent;
    Procedure StripWhitespace;
  Protected
    Function GetAttributeCount : Integer;             Override;
    Function GetName : WideString;                    Override;
    Function GetNamespace : WideString;               Override;
    Function GetNodeName : WideString;                Override;
    Function GetHasText : Boolean;                    Override;
    Function GetAsXML : AnsiString;                   Override;
    Function GetTextContentA:String;                  Override;
    Procedure SetTextContentA(AValue : String);       Override;
    Function GetTextContentW:WideString;              Override;
    Procedure SetTextContentW(AValue : WideString);   Override;
  Public
    Constructor Create(ADom : TIdSoapXmlDom; AParent : TIdSoapXmlElement; AName : widestring);
    destructor Destroy; Override;

    Function AppendChild(AName, ANS : WideString):TIdSoapxmlelement;                          Override;
    procedure appendComment(source : String);                                            Override;
    Function InsertChild(AIndex : Integer; AName, ANS : WideString):TIdSoapxmlelement;        Override;
    Procedure removeChild(AElement : TIdSoapXmlElement);                                 Override;
    Procedure ClearChildren();                                                           Override;
    Function hasAttribute(Const ANamespace, AName : WideString): Boolean;                Override;
    Function getAttribute(Const ANamespace, AName : WideString): WideString;             Override;
    Function getAttributeName(i : Integer; Var VNamespace, VName : WideString) :Boolean; Override;
    Procedure setAttribute(ANS, AName, AValue : WideString);                                  Override;
    Function ResolveXMLNamespaceCode(ANamespace, ALocation : WideString) : String;       Override;
    Procedure BuildChildFromXML(ASrc : WideString);                                      Override;
    Procedure GrabChildren(AElem : TIdSoapXmlElement; AOtherDOM : Boolean);              Override;
  End;

  TIdViewMessageDomEvent = Procedure (ASender : TIdSoapComponent; ADom : TIdSoapXmlDom) Of Object;

Function IdSoapDomFactory(AXmlProvider : TIdSoapXmlProvider = xpOpenXML) : TIdSoapXmlDom;
{$IFDEF UNICODE}
function IsXmlName(const S: String): boolean;
{$ENDIF}
Function XMLToText(AStr: String): String;

Implementation

Uses
  IdSoapConsts,
  IdSoapExceptions,
  IdSoapNamespaces,
  IdSoapResourceStrings,
  SysUtils
  {$IFDEF VCL6ORABOVE}
  , Variants
  {$ENDIF};

Const
  ASSERT_UNIT = 'IdSoapXML';

{ TIdSoapXmlDom }

Procedure TIdSoapXmlDom.Clear;
Begin
  FreeAndNil(FRoot);
End;

Constructor TIdSoapXmlDom.Create;
Begin
  Inherited Create;
  FBuilding := True;
End;

Destructor TIdSoapXmlDom.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlDom.destroy';
Begin
  Clear;
  Inherited;
End;

Procedure TIdSoapXmlDom.Read(ASource : TStream);
Begin
  Clear;
  DoRead(ASource);
  FBuilding := False;
End;

Procedure TIdSoapXmlDom.Read(ASource: AnsiString);
Var
  LStream : TIdMemoryStream;
Begin
  LStream := TIdMemoryStream.CreateString(ASource);
  Try
    Read(LStream);
  Finally
    FreeAndNil(LStream);
  End;
End;

{ TIdSoapXmlElement }

Constructor TIdSoapXmlElement.Create(ADom : TIdSoapXmlDom; AParent: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.create';
Begin
  Inherited Create;
  Assert(ADom.TestValid(TIdSoapXmlDom), ASSERT_LOCATION+': Dom is not valid');
  Assert((AParent = Nil) Or (AParent.TestValid(TIdSoapXmlElement)), ASSERT_LOCATION+': parent is not valid');
  FDom := ADom;
  FParentNode := AParent;
  FChildren := TObjectList.Create(True);
  FSibling := Nil;
  FKnowNilAttribute := False;
  FNilAttribute := False;
  FKnowXSIType := False;
  FXSIType := '';
  FKnowHasID := False;
  FHasID := False;
  FLastFind := -1;
  FLastFindName := '';
  FLastIndex := 0;
End;

Destructor TIdSoapXmlElement.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.destroy';
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  FreeAndNil(FChildren);
  Inherited;
End;

Function TIdSoapXmlElement.GetChildCount  : Integer;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.GetChildCount';
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FChildren.Count;
End;

Function TIdSoapXmlElement.GetFirstChild  : TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.GetFirstChild';
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  If FChildren.Count > 0 Then
    Begin
    Result := FChildren[0] As TIdSoapXmlElement;
    End
  Else
    Begin
    Result := Nil;
    End;
End;

Function TIdSoapXmlElement.FirstElement(ANamespace, AName : WideString) : TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.FirstElement';
Var
  i : Integer;
  LChild : TIdSoapXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
//  assert(ANamespace <> '', ASSERT_LOCATION+': namespace is blank');
  Assert(AName <> '', ASSERT_LOCATION+': name is blank');
  Result := Nil;
  If FChildren.Count > 0 Then
    Begin
    For i := (Max(FLastFind, 0)) To (FLastFind + FChildren.Count) Do
      Begin
      LChild := FChildren[i Mod FChildren.Count] As TIdSoapXmlElement;
      If AnsiSameText(LChild.Namespace, ANamespace) And AnsiSameText(LChild.Name, AName) Then
        Begin
        FLastFind := i Mod FChildren.Count;
        FLastIndex := 1;
        Result := LChild;
        Break;
        End;
      End;
    End;
End;

Function TIdSoapXmlElement.NextElement(ANamespace, AName : WideString) : TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.NextElement';
Var
  LSib : TIdSoapXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is blank');
  Result := Nil;
  LSib := Self.FSibling;
  While Assigned(LSib) And Not Assigned(Result) Do
    Begin
    If AnsiSameText(LSib.Namespace, ANamespace) And AnsiSameText(LSib.Name, AName) Then
      Begin
      Result := LSib;
      End;
    LSib := LSib.FSibling; 
    End;
End;


Function TIdSoapXmlElement.FindElementAnyNS(AName : WideString) : TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.FindElementAnyNS';
Var
  i : Integer;
  LChild : TIdSoapXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  Result := Nil;
  For i := 0 To FChildren.Count - 1 Do
    Begin
    LChild := FChildren[i] As TIdSoapXmlElement;
    If (AnsiSameText(LChild.Name, AName) Or AnsiSameText(LChild.nodeName, AName)) Then
      Begin
      Result := LChild;
      Break;
      End;
    End;
End;

Procedure TIdSoapXmlElement.PrivAppendChild(AChild: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.PrivAppendChild';
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AChild.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': child is not valid');
  If FChildren.Count > 0 Then
    Begin
    (FChildren[FChildren.Count - 1] As TIdSoapXmlElement).FSibling := AChild;
    End;
  FChildren.Add(AChild);
End;

Procedure TIdSoapXmlElement.PrivInsertChild(AIndex: Integer; AChild: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.PrivInsertChild';
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AChild.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': child is not valid');

  If AIndex > 0 Then
    Begin
    (FChildren[AIndex - 1] As TIdSoapXmlElement).FSibling := AChild;
    End;
  AChild.FSibling := (FChildren[AIndex] As TIdSoapXmlElement);
  FChildren.Insert(AIndex, AChild);
End;

Procedure TIdSoapXmlElement.PrivRemoveChild(AElement: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.PrivRemoveChild';
Var
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AElement.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': child is not valid');
  i := FChildren.IndexOf(AElement);
  If i >= 0 Then
    Begin
    If i > 0 Then
      Begin
      If i < FChildren.Count -1 Then
        Begin
        (FChildren[i - 1] As TIdSoapXmlElement).FSibling := (FChildren[i + 1] As TIdSoapXmlElement);
        End
      Else
        Begin
        (FChildren[i - 1] As TIdSoapXmlElement).FSibling := Nil;
        End;
      FChildren.Delete(i);
      End;
    End;
End;

Function TIdSoapXmlElement.Path: String;
Begin
  If Assigned(FParentNode) Then
    Begin
    Result := FParentNode.Path+'\'+NodeName;
    End
  Else
    Begin
    Result := '\'+NodeName;
    End;
End;

Function TIdSoapXmlElement.HasNilAttribute: Boolean;
Begin
  If Not FKnowNilAttribute Then
    Begin
    FNilAttribute :=
      AnsiSameText(GetSchemaInstanceAttribute(ID_SOAP_XSI_ATTR_NIL),  'true') Or { do not localize }
      AnsiSameText(GetSchemaInstanceAttribute(ID_SOAP_XSI_ATTR_NULL),  'true') Or { do not localize }
      AnsiSameText(GetSchemaInstanceAttribute('Nil'),  'true') Or { do not localize }
      AnsiSameText(GetSchemaInstanceAttribute('NIL'),  'true') Or { do not localize }
      AnsiSameText(GetSchemaInstanceAttribute('Null'),  'true') Or { do not localize }
      AnsiSameText(GetSchemaInstanceAttribute('NULL'),  'true'); { do not localize }
    FKnowNilAttribute := True;
    End;
  Result := FNilAttribute;
End;

Function TIdSoapXmlElement.GetSchemaInstanceAttribute(AAttributeName : String):String;
Begin
  Result := getAttribute(ID_SOAP_NS_SCHEMA_INST_2001, AAttributeName);
  If Result = '' Then
    Begin
    Result := getAttribute(ID_SOAP_NS_SCHEMA_INST_1999, AAttributeName)
    End;
End;

Function TIdSoapXmlElement.GetXSIType: String;
Begin
  If Not FKnowXSIType Then
    Begin
    FXSIType := GetSchemaInstanceAttribute(ID_SOAP_NAME_SCHEMA_TYPE);
    FKnowXSIType := True;
    End;
  Result := FXSIType;
End;

Function TIdSoapXmlElement.HasID: Boolean;
Begin
  If Not FKnowHasID Then
    Begin
    FHasID := hasAttribute('', ID_SOAP_NAME_XML_ID);
    FKnowHasID := True;
    End;
  Result := FHasID;
End;

Function TIdSoapXmlElement.GetChild(AIndex: Integer): TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.GetChild';
Begin
  Assert((AIndex >= 0) And (AIndex < FChildren.Count), ASSERT_LOCATION+': Index is out of range ('+inttostr(AIndex)+'/'+inttostr(FChildren.Count)+')');
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FChildren[AIndex] As TIdSoapXMLElement;
End;

Function TIdSoapXmlElement.getIndexOf: Integer;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.getIndexOf';
Var
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  If FParentNode = Nil Then
    Begin
    Result := 0;
    End
  Else
    Begin
    Result := -1;
    For i := 0 To FParentNode.FChildren.count - 1 Do
      Begin
      If FParentNode.Child[i] = Self Then
        Begin
        Result := i;
        Exit;
        End;
      End;
    Assert(Result <> -1, ASSERT_LOCATION+': self not found in parent');
    End;
End;

Function TIdSoapXmlElement.NthChildElement(ANamespace, AName: WideString; AIndex: Integer): TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.NthChildElement';
Var
  i, c : Integer;
  LChild : TIdSoapXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := Nil;
  If (ANamespace+#1+AName = FLastFindName) And (AIndex > FLastIndex) Then
    Begin
    c := FLastIndex+1;
    i := FLastFind+1;
    End
  Else
    Begin
    c := 0;
    i := 0;
    End;

  While Not Assigned(Result) And (I < FChildren.Count) Do
    Begin
    LChild := FChildren[i] As TIdSoapXmlElement;
    If AnsiSameText(LChild.Namespace, ANamespace) And AnsiSameText(LChild.Name, AName) Then
      Begin
      If c = AIndex Then
        Begin
        FLastFindName := ANamespace+#1+AName;
        FLastFind := i;
        FLastIndex := c;
        Result := LChild;
        End;
      Inc(c);
      End;
    Inc(i);
    End;
  If Not Assigned(Result) Then
    FLastFindName := '';
End;

Function TIdSoapXmlElement.ChildElementCount(ANamespace, AName: WideString ) : Integer;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapXmlElement.NthChildElement';
Var
  i : Integer;
  LChild : TIdSoapXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := 0;

  For i := 0 To FChildren.Count - 1 Do
    Begin
    LChild := FChildren[i] As TIdSoapXmlElement;
    If AnsiSameText(LChild.Namespace, ANamespace) And AnsiSameText(LChild.Name, AName) Then
      Begin
      Inc(Result);
      End;
    End;
End;

{$IFNDEF UNICODE}
{ TIdSoapOpenXmlDom }

Procedure TIdSoapOpenXmlDom.Clear;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlDom.Clear';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlDom), ASSERT_LOCATION+': self is not valid');
  Inherited;
  FreeAndNil(FDom);
End;

{$IFNDEF UNICODE}
Procedure TIdSoapOpenXmlDom.DOMReadError(ASender: TObject; AError: TdomError; Var VGo: Boolean);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlDom.DOMReadError';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlDom), ASSERT_LOCATION+': self is not valid');
  Assert(Assigned(AError), ASSERT_LOCATION+': Error is nil');
  VGo := False;
  If Assigned(AError.location.relatedNode) Then
    Begin
    FDomErr := AError.code+': '+AError.Message+' (at '+AError.location.relatedNode.localName+')';        { do not localize }
    End
  Else
    Begin
    FDomErr := AError.code+': '+AError.Message;
    End;
End;
{$ENDIF}

Function TIdSoapOpenXmlDom.ImportElement(AElem: TIdSoapXmlElement): TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlDom.ImportElement';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlDom), ASSERT_LOCATION+': self is not valid');
  // check that XML provider is the right type
  IdRequire(AElem.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': Element is not valid');
  Result := TIdSoapOpenXmlElement.Create(Self, Nil, FDoc.ImportNode((AElem As TIdSoapOpenXmlElement).FElement, True) As TdomElement);
End;

Procedure TIdSoapOpenXmlDom.IterateChildren(AElem: TIdSoapOpenXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlDom.IterateChildren';
Var
  LNode : Tox4domNode;
  LChild : TIdSoapOpenXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlDom), ASSERT_LOCATION+': self is not valid');
  Assert(AElem.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': Elem is not valid');
  LNode := AElem.FElement.firstChild;
  While Assigned(LNode) Do
    Begin
    If LNode Is TdomElement Then
      Begin
      LChild := TIdSoapOpenXmlElement.Create(Self, AElem, LNode As TdomElement);
      AElem.PrivAppendChild(LChild);
      IterateChildren(LChild);
      End;
    LNode := LNode.nextSibling;
    End;
End;

Procedure TIdSoapOpenXmlDom.DoRead(ASource: TStream);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlDom.Read';
Var
  LParser: TXmlToDomParser;
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlDom), ASSERT_LOCATION+': self is not valid');
  FDom := TDomImplementation.Create(Nil);
  FDom.OnError := DOMReadError;
  FDomErr := '';
  LParser := TXmlToDomParser.Create(Nil);
  Try
    LParser.DOMImpl := FDom;
    LParser.DocBuilder.BuildNamespaceTree := True;
    Try
      LParser.StreamToDom(ASource);
    Except
      On e:Exception Do
        Begin
        If FDomErr <> '' Then
          Begin
          e.Message := e.Message + ' '+FDomErr;
          End;
        Raise;
        End;
    End;
  Finally
    FreeAndNil(LParser);
  End;
  (FDom.documents.item[0] As TdomDocument).resolveEntityReferences(erReplace);
  FDoc := (FDom.documents.item[0] As TdomDocument);
  FRoot := TIdSoapOpenXmlElement.Create(Self, Nil, FDoc.documentElement);
  IterateChildren(FRoot As TIdSoapOpenXmlElement);
End;

Procedure TIdSoapOpenXmlDom.StartBuild(AName, ANS: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlDom.StartBuild';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlDom), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': Name is not valid');
  FDom := TDomImplementation.Create(Nil);
  FDoc := FDom.createDocument(AName, Nil);
  FRoot := TIdSoapOpenXmlElement.Create(Self, Nil, FDoc.documentElement);
End;

Procedure TIdSoapOpenXmlDom.writeUTF16(ADest: TStream; ANoXMLDec : Boolean = False);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlDom.writeUTF16';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlDom), ASSERT_LOCATION+': self is not valid');
  FDoc.writeCodeAsUTF16(ADest);
End;

Procedure TIdSoapOpenXmlDom.writeUTF8(ADest: TStream; ANoXMLDec : Boolean = False);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlDom.writeUTF8';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlDom), ASSERT_LOCATION+': self is not valid');
  FDoc.writeCodeAsUTF8(ADest);
End;

{ TIdSoapOpenXmlElement }

Constructor TIdSoapOpenXmlElement.Create(ADom : TIdSoapXmlDom; AParent : TIdSoapXmlElement; AElement : TdomElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.create';
Begin
  Inherited Create(ADom, AParent);
  Assert(IdSoapTestNodeValid(AElement, TdomElement), ASSERT_LOCATION+': Element is not valid');
  FElement := AElement;
End;

Function TIdSoapOpenXmlElement.GetAttributeCount : Integer;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.GetAttributeCount';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FElement.attributes.Length;
End;

Function TIdSoapOpenXmlElement.GetName : WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.GetName';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FElement.localName;
End;

Function TIdSoapOpenXmlElement.GetNamespace : WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.GetNamespace';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FElement.namespaceURI;
End;

Function TIdSoapOpenXmlElement.GetNodeName : WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.GetNodeName';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FElement.nodeName;
End;

Function TIdSoapOpenXmlElement.GetHasText : Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.GetHasText';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := (FElement.childNodes.Length = 0) Or ((FElement.childNodes.Length = 1) And (FElement.firstChild Is TdomText));
End;

Function TIdSoapOpenXmlElement.GetTextContentA:String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.GetTextContent';
Begin
  Result := GetTextContentW;
End;

Function TIdSoapOpenXmlElement.GetTextContentW:WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.GetTextContent';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(hasText, ASSERT_LOCATION+': attempt to get text content when HasText is false');
  If FElement.childNodes.Length = 0 Then
    Begin
    Result := '';
    End
  Else
    Begin
    Result := FElement.firstChild.nodeValue;
    End;
End;

Function TIdSoapOpenXmlElement.GetAsXML : AnsiString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.GetAsXML';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FElement.code;
End;

Procedure TIdSoapOpenXmlElement.SetTextContentA(AValue : String);
Begin
  SetTextContentW(AValue);
End;

Procedure TIdSoapOpenXmlElement.SetTextContentW(AValue : WideString);
Const
  ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.SetTextContent';
Var
  LText : TdomText;
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  // no check on AValue
  Assert(FElement.childNodes.Length = 0, ASSERT_LOCATION+': attempt to set TextContent when children already exist');
  LText := (FDom As TIdSoapOpenXmlDom).FDoc.createTextNode(AValue);
  FElement.appendChild(LText);
End;

Function TIdSoapOpenXmlElement.AppendChild(AName, ANs : WideString):TIdSoapxmlelement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.AppendChild';
Var
  LElem : TdomElement;
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');

  FLastFindName := '';
  LElem := (FDom As TIdSoapOpenXmlDom).FDoc.createElement(AName);
  FElement.appendChild(LElem);
  Result := TIdSoapOpenXmlElement.Create(FDom, Self, LElem);
  PrivAppendChild(Result);
End;

Procedure TIdSoapOpenXmlElement.removeChild(AElement : TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.removeChild';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  If AElement = Nil Then
    Exit;

  FLastFindName := '';
  Assert(AElement.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': element is not valid');
  FElement.removeChild((AElement As TIdSoapOpenXmlElement).FElement);
  PrivRemoveChild(AElement);
End;

Function TIdSoapOpenXmlElement.hasAttribute(Const ANamespace, AName : WideString): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.hasAttribute';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  // no check on namespace
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  Assert(Not FDom.FBuilding Or (ANamespace = ''), ASSERT_LOCATION+': Attempt to use namespace while building'); // see note at top of unit
  If FDom.FBuilding Then
    Begin
    Result := FElement.hasAttribute(AName);
    End
  Else
    Begin
    Result := FElement.hasAttributeNS(ANamespace, AName);
    If Not Result Then
      Begin
      Result := FElement.hasAttributeNS('', AName);
      End;
    End;
End;

Function TIdSoapOpenXmlElement.getAttribute(Const ANamespace, AName : WideString): WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.getAttribute';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  // no check on namespace
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  Assert(Not FDom.FBuilding Or (ANamespace = ''), ASSERT_LOCATION+': Attempt to use namespace while building'); // see note at top of unit
  If FDom.FBuilding Then
    Begin
    Result := FElement.getAttribute(AName);
    End
  Else
    Begin
    If FElement.hasAttributeNS(ANamespace, AName) Then
      Result := FElement.getAttributeNS(ANamespace, AName)
    Else
      Result := FElement.getAttributeNS('', AName)
    End;
End;

Function TIdSoapOpenXmlElement.getAttributeName(i : Integer; Var VNamespace, VName : WideString) :Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.getAttributeName';
Var
  LAttr : TdomAttr;
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  // no check on i
  LAttr := FElement.attributes.item[i] As TdomAttr;
  Result := Assigned(LAttr);
  If Result Then
    Begin
    VNamespace := LAttr.namespaceURI;
    VName := LAttr.localName;
    End;
End;

Procedure TIdSoapOpenXmlElement.setAttribute(ANs, AName, AValue : WideString);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.setAttribute';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  // no check on AValue
  FElement.setAttribute(AName, AValue);
End;

Function TIdSoapOpenXmlElement.ResolveXMLNamespaceCode(ANamespace, ALocation : WideString) : String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.ResolveXMLNamespaceCode';
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': namespace is not valid');
  Assert(ALocation <> '', ASSERT_LOCATION+': location is not valid');
  Result := IdSoapNamespaces.ResolveXMLNamespaceCode(FElement, ANamespace, ALocation);
End;

Procedure TIdSoapOpenXmlElement.BuildChildFromXML(ASrc : WideString);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.BuildChildFromXML';
Var
  LRoot : TdomDocumentFragment;
  LParser : TXmlToDomParser;
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  // no check on ASrc

  // this code courtesy of Ernst van der Pols, NLSoftware@hetnet.nl
  // import fragment
  LRoot := (FDom As TIdSoapOpenXmlDom).FDoc.CreateDocumentFragment;
  LParser := TXmlToDomParser.Create(Nil);
  Try
    Try
      LParser.DocStringToDom(ASrc,'','',LRoot);
      FElement.appendChild(LRoot);
    Except
      On ex: Exception Do
        Begin
        (FDom As TIdSoapOpenXmlDom).FDoc.FreeAllNodes(TdomNode(LRoot));
        TextContentW := 'Invalid XML details: '+ASrc;
        End;
    End;
  Finally
    LParser.Free;
  End;
End;

Procedure TIdSoapOpenXmlElement.GrabChildren(AElem: TIdSoapXmlElement; AOtherDOM : Boolean);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.GrabChildren';
Var
  LSrc, LDest : TdomElement;
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  // check provider is the right type
  IdRequire(AElem.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': Elem is not valid');

  LSrc := (AELem As TIdSoapOpenXmlElement).FElement;
  LDest := FElement;

  While Assigned(LSrc.FirstChild) Do
    Begin
    If AOtherDom Then
      Begin
      LDest.AppendChild((FDom As TIdSoapOpenXMLDom).FDoc.importNode(LSrc.FirstChild, True));
      LSrc.removeChild(LSrc.FirstChild);
      End
    Else
      Begin
      LDest.AppendChild(LSrc.FirstChild);
      End;
    End;
  // now the attributes
  For i := 0 To LSrc.attributes.Length - 1 Do
    Begin
    LDest.setAttribute(LSrc.attributes.item[i].nodeName, LSrc.attributes.item[i].textContent);
    End;
End;

Function TIdSoapOpenXmlElement.InsertChild(AIndex: Integer; AName, ANs: WideString): TIdSoapxmlelement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.InsertChild';
Var
  LElem : TdomElement;
  LPrev : TIdSoapOpenXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  Assert((AIndex >= 0) And (AIndex < FChildren.Count), ASSERT_LOCATION+': insert location not valid ('+inttostr(AIndex)+'/'+inttostr(ChildCount)+')');

  FLastFindName := '';
  LPrev := FChildren[AIndex] As TIdSoapOpenXmlElement;
  LElem := (FDom As TIdSoapOpenXmlDom).FDoc.createElement(AName);
  FElement.insertBefore(LElem, LPrev.FElement);
  Result := TIdSoapOpenXmlElement.Create(FDom, Self, LElem);
  PrivInsertChild(AIndex, Result);
End;

procedure TIdSoapOpenXmlElement.ClearChildren;
begin
  FElement.clear;
end;

procedure TIdSoapOpenXmlElement.appendComment(source: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.appendComment';
Var
  LElem : TdomNode;
Begin
  Assert(Self.TestValid(TIdSoapOpenXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(source <> '', ASSERT_LOCATION+': source is not valid');

  FLastFindName := '';
  LElem := (FDom As TIdSoapOpenXmlDom).FDoc.createComment(source);
  FElement.appendChild(LElem);
end;

{$ENDIF}

{$IFNDEF DESIGNTIME}

Procedure DetermineMsXmlProgId;
  Function TryLoad(sId : String) : Boolean;
  Var
    ClassID: TCLSID;
    iTest : IDispatch;
    Res : HResult;
  Begin
    Result := false;
    if Succeeded(CLSIDFromProgID(PWideChar(WideString('MSXML2.DOMDocument'+sId)), ClassID)) Then
    Begin
      Res := CoCreateInstance(ClassID, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IDispatch, iTest);
      result := Succeeded(Res);
      If result then
      Begin
        iTest := nil;
        GMsXmlProgId_DOM := 'MSXML2.DOMDocument'+sId;
        GMsXmlProgId_FTDOM := 'MSXML2.FreeThreadedDOMDocument'+sId;
        GMsXmlProgId_SCHEMA := 'MSXML2.XMLSchemaCache'+sId;
        GMsXmlProgId_XSLT := 'MSXML2.XSLTemplate'+sId;
        GMsXmlProgId_XSLP := 'MSXML2.XSLProcessor'+sId;
        GMsXmlProgId_SAX := 'MSXML2.SAXXMLReader'+sId;
      End;
    End;
  End;
Begin
  CoInitializeEx(nil, COINIT_MULTITHREADED);
  Try
   If not TryLoad('.6.0') And not TryLoad('.5.0')
       And not TryLoad('.4.0') And not TryLoad('') Then
        GMsXmlProgId_DOM := '';
  Finally
    CoUninitialize;
  End;
End;

Function LoadMsXMLDom : IXMLDomDocument2;
Var
  LVariant: Variant;
Begin
  LVariant := LoadMsXMLDomV;
  Result := IUnknown(TVarData(LVariant).VDispatch) as IXMLDomDocument2;
End;

Function LoadMsXMLDomV(isFree : boolean = false) : Variant;
Begin
  if GMsXmlProgId_DOM = '' Then
    Raise Exception.Create('Unable to load Microsoft XML Services');
  if isFree then
    Result := CreateOleObject(GMsXmlProgId_FTDOM)
  else
    Result := CreateOleObject(GMsXmlProgId_DOM);
End;



{ TIdSoapMSXmlDom }

Procedure TIdSoapMSXmlDom.Clear;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlDom.Clear';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlDom), ASSERT_LOCATION+': self is not valid');
  Inherited;
  FDom := Nil;
End;

Function TIdSoapMSXmlDom.ImportElement(AElem: TIdSoapXmlElement): TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlDom.ImportElement';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlDom), ASSERT_LOCATION+': self is not valid');
  // check that XML provider is the right type
  IdRequire(AElem.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': Element is not valid');

  Result := TIdSoapMSXmlElement.Create(Self, Nil, (AElem As TIdSoapMsXmlElement).FElem.cloneNode(True) As IXMLDOMElement);
End;

Procedure TIdSoapMSXmlDom.IterateChildren(AElem: TIdSoapMSXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlDom.IterateChildren';
Var
  LNode : IXMLDOMNode;
  LChild : TIdSoapMSXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlDom), ASSERT_LOCATION+': self is not valid');
  Assert(AElem.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': Elem is not valid');
  LNode := AElem.FElem.firstChild;
  While Assigned(LNode) Do
    Begin
    If LNode.nodeType = NODE_ELEMENT Then
      Begin
      LChild := TIdSoapMSXmlElement.Create(Self, AElem, LNode As IXMLDOMElement);
      AElem.PrivAppendChild(LChild);
      IterateChildren(LChild);
      End;
    LNode := LNode.nextSibling;
    End;
End;

Procedure TIdSoapMSXmlDom.DoRead(ASource: TStream);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlDom.Read';
Var
  LAdapter : Variant;
  ok : boolean;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlDom), ASSERT_LOCATION+': self is not valid');
  FDom := LoadMsXmlDom;
  FDom.preserveWhiteSpace := True;
  FDom.setProperty('NewParser', True);
  FDom.schemas := FSchemas;
  if Fschemas <> nil then
    FDom.validateOnParse := true
  else
    FDom.validateOnParse := False;
  LAdapter := TStreamAdapter.Create(ASource) As IStream;
  ok := FDom.load(LAdapter);
  if schemas = nil then
  begin
    Assert(ok, ASSERT_LOCATION+': xml load failed: '+FDom.parseError.reason);
    Assert(Assigned(FDom.documentElement), ASSERT_LOCATION+': document could not be parsed');
  end
  else
    ParseError := FDom.ParseError;
  if Assigned(FDom.documentElement) then
  begin
    FDom.documentElement.normalize;
    FRoot := TIdSoapMSXmlElement.Create(Self, Nil, FDom.documentElement);
    IterateChildren(FRoot As TIdSoapMSXmlElement);
  end
  else
    FRoot := nil;
End;

Procedure TIdSoapMSXmlDom.StartBuild(AName, ANS: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlDom.StartBuild';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlDom), ASSERT_LOCATION+': self is not valid');
  FDom := LoadMsXmlDom;
  FDom.documentElement := FDom.createElement(AName);
  FRoot := TIdSoapMSXmlElement.Create(Self, Nil, FDom.documentElement);
End;

Procedure TIdSoapMSXmlDom.writeUTF16(ADest: TStream; ANoXMLDec : Boolean = False);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlDom.writeUTF16';
Var
  LAdapter : Variant;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlDom), ASSERT_LOCATION+': self is not valid');
  // set up for utf-16....
  LAdapter := TStreamAdapter.Create(ADest) As IStream;
  FDom.save(LAdapter);
End;

Procedure TIdSoapMSXmlDom.writeUTF8(ADest: TStream; ANoXMLDec : Boolean = False);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlDom.writeUTF8';
Var
  LAdapter : Variant;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlDom), ASSERT_LOCATION+': self is not valid');
  LAdapter := TStreamAdapter.Create(ADest) As IStream;
  FDom.save(LAdapter);
End;

{ TIdSoapMSXmlElement }

Constructor TIdSoapMSXmlElement.Create(ADom: TIdSoapXmlDom; AParent: TIdSoapXmlElement; AElem : IXMLDOMElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.create';
Begin
  Inherited Create(ADom, AParent);
  FElem := AElem;
End;


Function TIdSoapMSXmlElement.AppendChild(AName, ANs: WideString): TIdSoapxmlelement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.AppendChild';
Var
  LElem : IXMLDOMElement;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  FLastFindName := '';
  LElem := (FDom As TIdSoapMSXmlDom).FDom.createElement(AName);
  FElem.appendChild(LElem);
  Result := TIdSoapMSXmlElement.Create(FDom, Self, LElem);
  PrivAppendChild(Result);
End;

Function TIdSoapMSXmlElement.InsertChild(AIndex: Integer; AName, ANs: WideString): TIdSoapxmlelement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.InsertChild';
Var
  LElem : IXMLDOMElement;
  LPrev : TIdSoapMSXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  Assert((AIndex >= 0) And (AIndex < FChildren.Count), ASSERT_LOCATION+': insert location not valid ('+inttostr(AIndex)+'/'+inttostr(ChildCount)+')');

  FLastFindName := '';
  LPrev := FChildren[AIndex] As TIdSoapMSXmlElement;
  LElem := (FDom As TIdSoapMSXmlDom).FDom.createElement(AName);
  FElem.insertBefore(LElem, LPrev.FElem);
  Result := TIdSoapMSXmlElement.Create(FDom, Self, LElem);
  PrivInsertChild(AIndex, Result);
End;

procedure TIdSoapMSXmlElement.appendComment(source: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.appendComment';
Var
  LElem : IXMLDOMComment;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(source <> '', ASSERT_LOCATION+': source is not valid');
  LElem := (FDom As TIdSoapMSXmlDom).FDom.createComment(source);
  FElem.appendChild(LElem);
end;

Procedure TIdSoapMSXmlElement.BuildChildFromXML(ASrc: WideString);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.BuildChildFromXML';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
 {TODO}
End;

Function TIdSoapMSXmlElement.GetAsXML: AnsiString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.GetAsXML';
{$IFDEF UNICODE}
var
  b : TBytes;
{$ENDIF}
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  {$IFDEF UNICODE}
  b := TEncoding.UTF8.GetBytes(FElem.xml);
  SetLength(b, length(b));
  move(b[0], result[1], length(b));
  {$ELSE}
  Result := FElem.xml;
  {$ENDIF}
End;

Function TIdSoapMSXmlElement.getAttribute(Const ANamespace, AName: WideString): WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.getAttribute';
Var
  LAttr : IXMLDOMNamedNodeMap;
  LNode : IXMLDOMAttribute;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  LAttr := FElem.attributes;
  LNode := LAttr.getQualifiedItem(AName, ANamespace) As IXMLDOMAttribute;
  If Assigned(Lnode) Then
    Begin
    Result := LNode.text;
    End
  Else
    Begin
    LNode := LAttr.getNamedItem(AName) As IXMLDOMAttribute;
    If Assigned(Lnode) Then
      Begin
      Result := LNode.text;
      End;
    End;
End;

Function TIdSoapMSXmlElement.GetAttributeCount: Integer;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.GetAttributeCount';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FElem.attributes.Length;
End;

Function TIdSoapMSXmlElement.getAttributeName(i: Integer; Var VNamespace, VName: WideString): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.getAttributeName';
Var
  LAttr : IXMLDOMNamedNodeMap;
  LNode : IXMLDOMNode;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  LAttr := FElem.attributes;
  LNode := LAttr.item[i];
  Result := Assigned(LNode);
  If Result Then
    Begin
    VNamespace := LNode.namespaceURI;
    VName := LNode.baseName;
    End;
End;

Function TIdSoapMSXmlElement.GetHasText: Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.GetHasText';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := (FElem.childNodes.Length = 0) Or ((FElem.childNodes.Length = 1) And (FElem.firstChild.NodeType = NODE_TEXT));
End;

Function TIdSoapMSXmlElement.GetName: WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.GetName';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FElem.baseName;
End;

Function TIdSoapMSXmlElement.GetNamespace: WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.GetNamespace';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FElem.namespaceURI;
End;

Function TIdSoapMSXmlElement.GetNodeName: WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.GetNodeName';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := FElem.nodeName;
End;

Function TIdSoapMSXmlElement.GetTextContentA: String;
Begin
  Result := GetTextContentW;
End;

Function TIdSoapMSXmlElement.GetTextContentW: WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.GetTextContent';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(hasText, ASSERT_LOCATION+': attempt to get text content when HasText is false');
  If FElem.childNodes.Length = 0 Then
    Begin
    Result := '';
    End
  Else
    Begin
    Result := FElem.firstChild.text;
    End;
End;

Function TIdSoapMSXmlElement.hasAttribute(Const ANamespace, AName: WideString): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.hasAttribute';
Var
  LAttr : IXMLDOMNamedNodeMap;
  LNode : IXMLDOMNode;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  LAttr := FElem.attributes;
  LNode := LAttr.getQualifiedItem(AName, ANamespace);
  Result := Assigned(LNode);
End;

Procedure TIdSoapMSXmlElement.removeChild(AElement: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.removeChild';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  If AElement = Nil Then
    Exit;

  FLastFindName := '';
  FElem.removeChild((AElement As TIdSoapMSXmlElement).FElem);
  PrivRemoveChild(AElement);
End;

Function ResolveMSXMLNamespaceCode(AElement : IXMLDOMElement; ANamespace, ALocation : String):String;
Const ASSERT_LOCATION = 'IdSoapXML.ResolveMSXMLNamespaceCode';
Var
  i : Integer;
  LAttr : IXMLDOMAttribute;
Begin
  Assert(ANamespace <> '', ASSERT_LOCATION+': namespace is blank ('+ALocation+')');
  Assert(ALocation <> '', ASSERT_LOCATION+': Location is blank ('+ALocation+')');

  Result := '';
  For i := 0 To AElement.attributes.Length - 1 Do
    Begin
    LAttr := AElement.Attributes.item[i] As IXMLDOMAttribute;
    If (LAttr.prefix = 'xmlns') And
       (LAttr.baseName = ANameSpace) Then
      Begin
      Result := LAttr.text;
      Break;
      End;
    End;
  If Result = '' Then
    Begin
    If Assigned(AElement.parentNode) Then
      Begin
      Result := ResolveMSXMLNamespaceCode(AElement.parentNode As IXMLDOMElement, ANamespace, ALocation);
      End
    Else
      Begin
      Raise EIdSoapNamespaceProblem.CreateFmt(RS_ERR_SOAP_UNRESOLVABLE_NAMESPACE, [ANamespace, ALocation]);
      End;
    End;
End;


Function TIdSoapMSXmlElement.ResolveXMLNamespaceCode(ANamespace, ALocation: WideString): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.ResolveXMLNamespaceCode';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := ResolveMSXMLNamespaceCode(FElem, ANamespace, ALocation);
End;

Procedure TIdSoapMSXmlElement.setAttribute(ANs, AName, AValue: WideString);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.setAttribute';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  FElem.setAttribute(AName, AValue);
End;

Procedure TIdSoapMSXmlElement.SetTextContentA(AValue: String);
Begin
  SetTextContentW(AValue);
End;

Procedure TIdSoapMSXmlElement.SetTextContentW(AValue: WideString);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.SetTextContent';
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  FElem.text := AValue;
End;

Procedure TIdSoapMSXmlElement.GrabChildren(AElem: TIdSoapXmlElement; AOtherDOM : Boolean);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.GrabChildren';
Var
  LSrc, LDest : IXMLDOMElement;
  i : Integer;
Begin
  Assert(Self.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': self is not valid');
  // check provider is the right type
  IdRequire(AElem.TestValid(TIdSoapMSXmlElement), ASSERT_LOCATION+': Elem is not valid');

  LSrc := (AELem As TIdSoapMSXmlElement).FElem;
  LDest := FElem;

  While Assigned(LSrc.FirstChild) Do
    Begin
    If AOtherDom Then
      Begin
      LDest.AppendChild(LSrc.FirstChild.cloneNode(True));
      LSrc.removeChild(LSrc.FirstChild);
      End
    Else
      Begin
      LDest.AppendChild(LSrc.FirstChild);
      End;
    End;
  // now the attributes
  For i := 0 To LSrc.attributes.Length - 1 Do
    Begin
    LDest.setAttribute(LSrc.attributes.item[i].nodeName, LSrc.attributes.item[i].text);
    End;
End;

procedure TIdSoapMSXmlElement.ClearChildren;
begin
  FElem.childNodes.reset;
end;
{$ENDIF}

Function IdSoapDomFactory(AXmlProvider : TIdSoapXmlProvider = xpOpenXML) : TIdSoapXmlDom;
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapDomFactory';
Begin
  Assert(IdEnumIsValid(TypeInfo(TIdSoapXmlProvider), ord(AXmlProvider)), ASSERT_LOCATION+': XML provider type is invalid');
  If AXmlProvider = xpMsXml Then
    Begin
    {$IFNDEF DESIGNTIME} // work around unable to compile design time project with OLEServer
    Result := TIdSoapMSXmlDom.Create;
    {$ENDIF}
    End
  Else If AXmlProvider = xpCustom Then
    Begin
    Result := TIdSoapCustomDom.Create;
    End
  Else  If AXmlProvider = xpOpenXML Then
    Begin
    {$IFDEF UNICODE}
    result := TIdSoapDelphiXmlDom.Create;
    {$ELSE}
    Result := TIdSoapOpenXmlDom.Create;
    {$ENDIF}
    End
  Else
    Begin
    Raise EIdSoapBadParameterValue.Create(ASSERT_LOCATION+': unknown soap Provider "'+inttostr(ord(AXMLProvider))+'"');
    End;
End;


{== String Utilities for writing XML directly =================================}

Function XMLCheck(ACondition : Boolean; AMsg : String) : Boolean;
Begin
  Result := True;
  If Not ACondition Then
    Begin
    Raise EIdSoapXmlParseError.Create(AMsg);
    End;
End;

Procedure StringAppendStart(Var VStr: String; Var VLen: Integer);
Begin
  VLen := Length(VStr);
  SetLength(VStr, Length(VStr) + 4096);
End;

Procedure StringAppend(Var VStr: String; AStrToAdd: String; Var VLen: Integer);
Begin
  If (AStrToAdd = '') Then
    Begin
    Exit;
    End;
  If VLen + Length(AStrToAdd) > Length(VStr) Then
    SetLength(VStr, Length(VStr) + max(4096, Length(AStrToAdd)));
  move(AStrToAdd[1], VStr[VLen + 1], Length(AStrToAdd){$IFDEF UNICODE}*2{$ENDIF});
  Inc(VLen, Length(AStrToAdd));
End;

Procedure StringAppendClose(Var VStr: String; ALen: Integer);
Begin
  SetLength(VStr, ALen);
End;

Function TextToXML(AStr : String):String;
Const ASSERT_LOCATION = 'TextToXML';
Var
  i, LLen: Integer;
  s : String;
Begin
  i := 1;
  Result := '';
  StringAppendStart(Result, LLen);
  While i <= Length(AStr) Do
    Begin
    Case AStr[i] Of
      '''':s := '&#' + IntToStr(Ord(AStr[i])) + ';';
      '"': s := '&quot;';
      '&': s := '&amp;';
      '<': s := '&lt;';
      '>': s := '&gt;';
      #13:
        Begin
        s := #10;
        If (i < Length(AStr)) And (AStr[i + 1] = #10) Then
          Begin
          Inc(i);
          End;
        End;
    Else
        Begin
        If (AStr[i] >= ' ') And (AStr[i]<='~') Then
          Begin
          s :=  AStr[i]
          End
        Else
          Begin
          // s := '&#' + IntToStr(Ord(AStr[i])) + ';';
          {$IFDEF UNICODE}
          s :=  AStr[i]
          {$ELSE}
          // s := UTF16BEToUTF8Str(cp1252ToUTF16Str(AStr[i]), False);
          s := AStr[i];
          {$ENDIF}
          End;
        End;
    End;
    StringAppend(Result, s, LLen);
    Inc(i);
    End;
  StringAppendClose(Result, LLen);
End;

Function XMLToText(AStr: String): String;
Const ASSERT_LOCATION = 'XMLToText';
Var
  i, j, LLen: Integer;
  s : String;
Begin
  i := 1;
  Result := '';
  StringAppendStart(Result, LLen);
  While i <= Length(AStr) Do
    Begin
    If AStr[i] = '&' Then
      Begin
      Inc(i);
      j := i;
      Repeat
        Inc(i);
        Assert(i <= Length(AStr), ASSERT_LOCATION+': illegal XML source "'+AStr+'" - unterminated Entity');
      Until AStr[i] = ';';
      s := Copy(AStr, j, i-j);
      If s[1] = '#' Then
        Begin
        StringAppend(Result, chr(IdStrToIntWithError(Copy(s, 2, Length(s)), 'Entity in XML source "'+AStr+'"')), LLen);
        End
      Else If s = 'quot' Then
        Begin
        StringAppend(Result, '"', LLen);
        End
      Else If s = 'amp' Then
        Begin
        StringAppend(Result, '&', LLen);
        End
      Else If s = 'lt' Then
        Begin
        StringAppend(Result, '<', LLen);
        End
      Else If s = 'gt' Then
        Begin
        StringAppend(Result, '>', LLen);
        End
      Else If s = 'apos' Then
        Begin
        StringAppend(Result, '''', LLen);
        End
      Else
        Begin
        Assert(False, ASSERT_LOCATION+': illegal XML source "'+AStr+'" - unknown Entity +"'+s+'"');
        End;
      End
    Else
      Begin
      
      If (AStr[i] = #13) Then
        Begin
        StringAppend(Result, #10, LLen);
        If (i < Length(AStr)) And (AStr[i+1] = #10) Then
          Begin
          Inc(i);
          End;
        End
      Else
        Begin
        StringAppend(Result, AStr[i], LLen);
        End;
      End;
    Inc(i);
    End;
  StringAppendClose(Result, LLen);
End;

{$IFNDEF UNICODE}
Function IsXmlNameChar(Const ACh : Char): Boolean;
Begin
  Result := ACh In ['_', ':', '-', '.', 'A'..'Z', 'a'..'z', '0'..'9'];
End;

Function IsXmlWhiteSpace(Const ACh : Char): Boolean;
Begin
  Result := ACh In [#9,#10,#13,' '];
End;
{$ENDIF}

{ TIdSoapCustomElement }

Constructor TIdSoapCustomElement.Create(ADom : TIdSoapXmlDom; AParent : TIdSoapXmlElement; AName : widestring);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.create';
Begin
  Inherited Create(ADom, AParent);
  FNodeName := AName;
  FName := AName;
  FContent := '';
  FAttr := TObjectList.Create(True);
  FXMLNs := TObjectList.Create(False);
End;

Destructor TIdSoapCustomElement.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.destroy';
Begin
  FreeAndNil(FXMLNs);
  FreeAndNil(FAttr);
  Inherited;
End;

Function TIdSoapCustomElement.AppendChild(AName, ANs: WideString): TIdSoapxmlelement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.AppendChild';
Begin
  Assert(Self.TestValid(TIdSoapCustomElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');

  FLastFindName := '';
  Result := TIdSoapCustomElement.Create(FDom, Self, AName);
  PrivAppendChild(Result);
End;

procedure TIdSoapCustomElement.appendComment(source: String);
begin
// studiously ignore this
end;

Function TIdSoapCustomElement.InsertChild(AIndex: Integer; AName, ANs: WideString): TIdSoapxmlelement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.InsertChild';
Begin
  Assert(Self.TestValid(TIdSoapCustomElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');
  Assert((AIndex >= 0) And (AIndex < FChildren.Count), ASSERT_LOCATION+': insert location not valid ('+inttostr(AIndex)+'/'+inttostr(ChildCount)+')');

  FLastFindName := '';
  Result := TIdSoapCustomElement.Create(FDom, Self, AName);
  PrivInsertChild(AIndex, Result);
End;

Procedure TIdSoapCustomElement.BuildChildFromXML(ASrc: WideString);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.BuildChildFromXML';
Begin
  Raise Exception.Create('not supported');
End;

Function TIdSoapCustomElement.GetAsXML: AnsiString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.GetAsXML';
Var
  LCnt : String;
  LLen : Integer;
{$IFDEF UNICODE}
  b : TBytes;
{$ENDIF}
Begin
  LCnt := '';
  StringAppendStart(LCnt, LLen);
  WriteToString(LCnt, LLen, -1);
  StringAppendClose(LCnt, LLen);

  {$IFDEF UNICODE}
  b := TEncoding.UTF8.GetBytes(LCnt);
  SetLength(result, length(b));
  move(b[0], result[1], length(b));
  {$ELSE}
  Result := LCnt;
  {$ENDIF}
End;

Function TIdSoapCustomElement.getAttribute(Const ANamespace, AName: WideString): WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.getAttribute';
Var
  i : Integer;
  LAttr : TIdSoapCustomAttribute;
Begin
  Result := '';
  For i := 0 To FAttr.count -1 Do
    Begin
    LAttr := FAttr[i] As TIdSoapCustomAttribute;
    If (LAttr.FNs = ANamespace) And (LAttr.FName = AName) Then
      Begin
      Result := LAttr.FContent;
      Exit;
      End;
    End;
  For i := 0 To FAttr.count -1 Do
    Begin
    LAttr := FAttr[i] As TIdSoapCustomAttribute;
    If (LAttr.FNs = '') And (LAttr.FName = AName) Then
      Begin
      Result := LAttr.FContent;
      Exit;
      End;
    End;
End;

Function TIdSoapCustomElement.GetAttributeCount: Integer;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.GetAttributeCount';
Begin
  Result := FAttr.count;
End;

Function TIdSoapCustomElement.getAttributeName(i: Integer; Var VNamespace, VName: WideString): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.getAttributeName';
Var
  LAttr : TIdSoapCustomAttribute;
Begin
  LAttr := FAttr[i] As TIdSoapCustomAttribute;
  Result := True;
  VNamespace := LAttr.FNs;
  VName := LAttr.FName;
End;

Function TIdSoapCustomElement.GetHasText: Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.GetHasText';
Begin
  Result := (FContent <> '') And (FChildren.Count = 0);
End;

Function TIdSoapCustomElement.GetName: WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.GetName';
Begin
  Result := FName;
End;

Function TIdSoapCustomElement.GetNamespace: WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.GetNamespace';
Begin
  Result := FNs;
End;

Function TIdSoapCustomElement.GetNodeName: WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.GetNodeName';
Begin
  Result := FNodeName;
End;

Function TIdSoapCustomElement.GetTextContentA: String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.GetTextContentA';
Begin
  Result := FContent;
End;

Function TIdSoapCustomElement.GetTextContentW: WideString;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.GetTextContentW';
Begin
  {$IFDEF UNICODE}
  Result := FContent;
  {$ELSE}
  Result := Iso8859_1ToUTF16Str(FContent);
  {$ENDIF}
End;

Procedure TIdSoapCustomElement.GrabChildren(AElem: TIdSoapXmlElement; AOtherDOM: Boolean);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.GrabChildren';
Begin
  Raise Exception.Create('not supported');
End;

Function TIdSoapCustomElement.hasAttribute(Const ANamespace, AName: WideString): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.hasAttribute';
Var
  i : Integer;
  LAttr : TIdSoapCustomAttribute;
Begin
  Result := False;
  For i := 0 To FAttr.count -1 Do
    Begin
    LAttr := FAttr[i] As TIdSoapCustomAttribute;
    If (LAttr.FNs = ANamespace) And (LAttr.FName = AName) Then
      Begin
      Result := True;
      Exit;
      End;
    End;
End;

Procedure TIdSoapCustomElement.removeChild(AElement: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.removeChild';
Begin
  If AElement = Nil Then
    Exit;

  FLastFindName := '';
  FChildren.Remove(AElement);
End;

Function TIdSoapCustomElement.ResolveXMLNamespaceCode(ANamespace, ALocation: WideString): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.ResolveXMLNamespaceCode';
Var
  i : Integer;
  LAttr : TIdSoapCustomAttribute;
Begin
  If (ANamespace = 'xml') Or (ANamespace = 'xmlns') Then
    Begin
    Result := ANamespace;
    End
  Else
    Begin
    Result := '';
    For i := 0 To FXMLNs.count -1 Do
      Begin
      LAttr := FXMLNs[i] As TIdSoapCustomAttribute;
      If (LAttr.FName = ANamespace) Then
        Begin
        Result := LAttr.FContent;
        Break;
        End
      End;
    End;
  If Result = '' Then
    Begin
    If Assigned(FParentNode) Then
      Begin
      Result := FParentNode.ResolveXMLNamespaceCode(ANamespace, ALocation);
      End
    Else
      Begin
      XMLCheck(False, ASSERT_LOCATION+': Error reading XML document: the namespace prefix "'+ANamespace+'" found at "'+ALocation+'" could not be resolved');
      End;
    End;
End;

Procedure TIdSoapCustomElement.setAttribute(ANs, AName, AValue: WideString);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.setAttribute';
Var
  LAttr : TIdSoapCustomAttribute;
Begin
  LAttr := TIdSoapCustomAttribute.Create;
  LAttr.FName := AName;
  LAttr.FContent := AValue;
  FAttr.Add(LAttr);
End;

Procedure TIdSoapCustomElement.SetTextContentA(AValue: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.SetTextContentA';
Begin
  FContent := AValue;
End;

Procedure TIdSoapCustomElement.SetTextContentW(AValue: WideString);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.SetTextContentW';
Begin
  {$IFDEF UNICODE}
  FContent := AValue;
  {$ELSE}
  FContent := UTF16BEToUTF8Str(AValue, False);
  {$ENDIF}
End;

Function CalcIndent(iIndent : Integer) : String;
Begin
  if iIndent = -1 Then
    result := ''
  Else
    Result := StringMultiply(' ', iIndent*2);
End;

Function IncIndent(iIndent : Integer) : Integer;
Begin
  if iIndent = -1 Then
    Result := -1
  Else
    Result := iIndent + 1;
End;


Procedure TIdSoapCustomElement.WriteToString(Var VCnt: String; Var VLen: Integer; iIndent : Integer);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomElement.WriteToString';
Var
  i : Integer;
  LAttr : TIdSoapCustomAttribute;
Begin
  StringAppend(VCnt, CalcIndent(iIndent)+'<'+FNodeName, VLen);
  For i := 0 To FAttr.count - 1 Do
    Begin
    LAttr := FAttr[i] As TIdSoapCustomAttribute;
    StringAppend(VCnt, ' ', VLen);
    StringAppend(VCnt, LAttr.FName, VLen);
    StringAppend(VCnt, '="', VLen);
    StringAppend(VCnt, TextToXML(LAttr.FContent), VLen);
    StringAppend(VCnt, '"', VLen);
    End;
  If (FContent = '') And (FChildren.count = 0) Then
    Begin
    StringAppend(VCnt, '/>', VLen);
    If (iIndent > -1) Then
       StringAppend(VCnt, #13#10, VLen);
    End
  Else
    Begin
    StringAppend(VCnt, '>', VLen);
    If FContent <> '' Then
      Begin
      StringAppend(VCnt, TextToXML(FContent), VLen)
      End;
    If (FChildren.Count > 0) And (iIndent > -1) Then
       StringAppend(VCnt, #13#10, VLen);
    For i := 0 To FChildren.Count - 1 Do
      Begin
      (FChildren[i] As TIdSoapCustomElement).WriteToString(VCnt, VLen, IncIndent(iIndent));
      End;
    if FChildren.Count > 0 Then
      StringAppend(VCnt, CalcIndent(iIndent), VLen);
    StringAppend(VCnt, '</', VLen);
    StringAppend(VCnt, FName, VLen);
    StringAppend(VCnt, '>', VLen);
    If (iIndent > -1) Then
       StringAppend(VCnt, #13#10, VLen);
    End;
End;

Procedure TIdSoapCustomElement.StartLoadContent;
Begin
  StringAppendStart(FContent, FContentLength);
End;

Procedure TIdSoapCustomElement.AddToContent(const sContent:String);
Begin
  StringAppend(FContent, sContent, FContentLength);
End;

Procedure TIdSoapCustomElement.CloseLoadContent;
Begin
  StringAppendClose(FContent, FContentLength);
End;

Function TIdSoapCustomElement.ResolveNamespaces(ADefaultNamespace: String): String;
Var
  LAttr : TIdSoapCustomAttribute;
  sl, sr : String;
  i : Integer;
Begin
  Result := ADefaultNamespace;
  For i := 0 To FAttr.count - 1 Do
    Begin
    LAttr := FAttr[i] As TIdSoapCustomAttribute;
    If (LAttr.FNs = '') And (LAttr.FName = 'xmlns') Then
      Begin
      Result := LAttr.FContent;
      End
    Else If (LAttr.FNs) = 'xmlns' Then
      Begin
      FXMLNs.Add(LAttr);
      End;
    End;
  If Pos(':', FNodeName) = 0 Then
    Begin
    FNs := Result;
    FName := FNodeName;
    End
  Else
    Begin
    SplitNamespace(FNodeName, sl, sr);
    FNs := ResolveXMLNamespaceCode(sl, 'XML Element "'+Path+'"');
    FName := sr;
    End;
  For i := 0 To FAttr.count - 1 Do
    Begin
    LAttr := FAttr[i] As TIdSoapCustomAttribute;
    If (LAttr.FNs <> '') And (LAttr.FNs <> 'xml') And (LAttr.FNs <> 'xmlns') Then
      Begin
      LAttr.FNs := ResolveXMLNamespaceCode(LAttr.FNs, 'Attribute "'+LAttr.FName+'" on '+Path);
      End;
    End;
End;

procedure TIdSoapCustomElement.ClearChildren;
begin
  FChildren.Clear;
end;

procedure TIdSoapCustomElement.StripWhitespace;
Var
  iLoop : Integer;
begin
  FContent := Trim(FContent);
  For iLoop := 0 to FChildren.Count - 1 Do
  Begin
    TIdSoapCustomElement(FChildren[iLoop]).StripWhitespace;
  End;
end;

{ TIdSoapCustomDom }

Function TIdSoapCustomDom.ImportElement(AElem: TIdSoapXmlElement): TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomDom.ImportElement';
Begin
  Raise EIdSoapXmlParseError.Create('not supported');
End;

Function TIdSoapCustomDom.ReadToken(ASkipWhitespace : Boolean): String;
Const ASSERT_LOCATION = 'TIdSoapCustomDom.ReadToken';
Var
  LCh : Char;
  LStart : Integer;
Begin
  If ASkipWhitespace Then
    Begin
    While (FCursor < FLength) And IsXmlWhiteSpace(FSrc[FCursor]) Do
      Begin
      Inc(FCursor);
      End;
    End;
  XMLCheck(FCursor < FLength, ASSERT_LOCATION+': read of end of stream');
  LCh := FSrc[FCursor];
  Inc(FCursor);
  If isXmlNameChar(LCh) Then
    Begin
    LStart := FCursor - 1;
    While (FCursor < FLength) And isXmlNameChar(FSrc[FCursor]) Do
      Begin
      Inc(FCursor);
      End;
    XMLCheck(FCursor < FLength, ASSERT_LOCATION+': read of end of stream');
    result := copy(FSrc, LStart+1, FCursor - LStart);
    End
  Else
    Begin
    Result := LCh;
    Case LCh Of
      '<':Begin
          XMLCheck(FCursor < FLength, ASSERT_LOCATION+': read of end of stream');
          LCh := FSrc[FCursor];
          Inc(FCursor);
          If (LCh = '?') or (LCh = '/') Then
            Begin
            Result := '<' + LCh;
            End
          Else If (LCh = '!') Then
            Begin
            // comment or CData Section. just drop it for now
            Inc(FCursor);
            If FSrc[FCursor] = '-' Then
              Begin
              Repeat
                Inc(FCursor)
              Until (FCursor > FLength - 3) Or ( (FSrc[FCursor-2] = '-') And (FSrc[FCursor-1] = '-') And (FSrc[FCursor] = '>') );
              End
            Else
              Begin
              Repeat
                Inc(FCursor)
              Until (FCursor > FLength - 3) Or ( (FSrc[FCursor-2] = ']') And (FSrc[FCursor-1] = ']') And (FSrc[FCursor] = '>') );
              End;
            Result := ReadToken(False);
            End
          Else
            Dec(FCursor);
          End;
      '/':Begin
          XMLCheck(FCursor < FLength, ASSERT_LOCATION+': read of end of stream');
          LCh := FSrc[FCursor];
          Inc(FCursor);
          If LCh = '>' Then
            Begin
            Result := '/>';
            End
          Else
            Begin
            Dec(FCursor);
            End;
          End;
    Else
      // don't care
    End;
    End;
End;

Function TIdSoapCustomDom.ReadToNextChar(ACh : Char): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomDom.ReadToNextChar';
Var
  LStart : Integer;
Begin
  LStart := FCursor;
  While (FCursor < FLength) And (FSrc[FCursor] <> ACh) Do
    Begin
    Inc(FCursor);
    End;
  XMLCheck(FCursor < FLength, ASSERT_LOCATION+': read of end of stream');
  result := copy(FSrc, LStart+1, FCursor - LStart);
End;


Procedure TIdSoapCustomDom.ReadAttribute(AName : String; AOwner: TIdSoapCustomElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomDom.ReadAttribute';
Var
  LAttr : TIdSoapCustomAttribute;
  s : String;
  ns : String;
Begin
  LAttr := TIdSoapCustomAttribute.Create;
  AOwner.FAttr.Add(LAttr);
  SplitNamespace(AName, ns, AName);
  LAttr.FNs := ns;
  LAttr.FName := AName;
  s := ReadToken(True);
  XMLCheck(s = '=', ASSERT_LOCATION+': Found "'+s+'" looking for "=" in attribute "'+AName+'" on '+AOwner.FNodeName);
  s := ReadToken(True);
  XMLCheck((s = '"') Or (s = ''''), ASSERT_LOCATION+': Found "'+s+'" looking for " or '' at start of attribute "'+AName+'" on '+AOwner.FNodeName);
{$IFNDEF UNICODE}
  If FIsUTF8 Then
    Begin
    LAttr.FContent := IdSoapUTF8ToAnsi(XmlToText(ReadToNextChar(s[1])));
    End
  Else
{$ENDIF}
    Begin
    LAttr.FContent := XmlToText(ReadToNextChar(s[1]));
    End;
  s := ReadToken(False);
  XMLCheck((s = '"') Or (s = ''''), ASSERT_LOCATION+': Found "'+s+'" looking for " or '' at start of attribute "'+AName+'" on '+AOwner.FNodeName);
End;

Function TIdSoapCustomDom.ReadElement(AParent : TIdSoapXMLElement; ADefaultNamespace : String) : TIdSoapCustomElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomDom.ReadElement';
Var
  LNew, LSib : TIdSoapCustomElement;
  s : String;
Begin
  LSib := Nil;
  Result := TIdSoapCustomElement.Create(Self, AParent, '');
  Result.FNodeName := ReadToken(True);
  s := ReadToken(True);
  While (s <> '/>') And (s <> '>') Do
    Begin
    ReadAttribute(s, Result);
    s := ReadToken(True);
    End;
  ADefaultNamespace := Result.ResolveNamespaces(ADefaultNamespace);
  If s = '>' Then
    Begin
    Result.StartLoadContent;
    // there's actual content to read
    While s <> '</' Do
      Begin
      {$IFNDEF UNICODE}
      If FIsUTF8 Then
        Begin
        Result.AddToContent(IdSoapUTF8ToAnsi(XmlToText(ReadToNextChar('<'))));
        End
      Else
      {$ENDIF}
        Begin
        Result.AddToContent(XmlToText(ReadToNextChar('<')));
        End;
      s := ReadToken(False);
      If s = '<' Then
        Begin
        LNew := readElement(Result, ADefaultNamespace);
        If Assigned(LSib) Then
          Begin
          LSib.FSibling := LNew;
          End;
        LSib := LNew;
        Result.FChildren.Add(LNew);
        End;
      End;
    Result.CloseLoadContent;
    s := ReadToken(True);
    XMLCheck(s = Result.FNodeName, ASSERT_LOCATION+': nodename mismatch (start "'+Result.FNodeName+'"/ end"'+s+'")');
    s := Readtoken(True);
    XMLCheck(s = '>', ASSERT_LOCATION+': node "'+Result.FNodeName+'" terminator not terminated properly');
    End;
End;

Procedure TIdSoapCustomDom.DoRead(ASource: TStream);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomDom.Read';
Var
  s : String;
Begin
  If ASource.Size - ASource.Position = 0 Then
    Begin
    Raise EIdSoapXmlParseError.Create('Source was empty');
    End;

  s := IdSoapReadStreamToString(ASource, '');
  FLength := length(s);
  GetMem(FSrc, FLength{$IFDEF UNICODE} * 2{$ENDIF});
  Try
    move(s[1], FSrc^, FLength{$IFDEF UNICODE} * 2{$ENDIF});

    FCursor := 0;
    CheckForBOM;
    s := ReadToken(True);
    XMLCheck(s[1] = '<', ASSERT_LOCATION+': Unable to read Document - starts with "'+s+'"');
    If s = '<?' Then
      Begin
      Repeat
        s := readToken(True);
        If s = 'encoding' Then
          Begin
          s := readToken(True);
          XMLCheck(s = '=', ASSERT_LOCATION+': found "'+s+'" looking for "=" reading encoding');
          s := readToken(True);
          XMLCheck((s = '"') Or (s=''''), ASSERT_LOCATION+': found "'+s+'" looking for " or '' reading encoding');
          s := readToken(True);
          If AnsiSameText(s, 'ISO-8859-1') Then
            Begin
            FIsUTF8 := False;
            End
          Else If AnsiSameText(s, 'UTF-8') Then
            Begin
            FIsUTF8 := True;
            End
          Else
            Begin
            Raise EIdSoapException.Create('The IndySoap Custom XML parser does not support the encoding "'+s+'"');
            End;
          End;
      Until (s = '>') Or (s = '?>');
      s := ReadToken(True);
      End;
    XMLCheck(s = '<', ASSERT_LOCATION+': Unable to read Soap Packet - starts with "'+s+'"');
    FRoot := ReadElement(Nil, '');
  Finally
    FreeMem(FSrc);
  End;
End;

Procedure TIdSoapCustomDom.StartBuild(AName, ANS: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomDom.StartBuild';
Begin
  FRoot:= TIdSoapCustomElement.Create(Self, Nil, AName);
End;

Procedure TIdSoapCustomDom.writeUTF16(ADest: TStream; ANoXMLDec : Boolean = False);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomDom.writeUTF16';
Begin
  Raise Exception.Create('UTF-16 is not supported');
End;

Procedure TIdSoapCustomDom.writeUTF8(ADest: TStream; ANoXMLDec : Boolean = False);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomDom.writeUTF8';
Var
  LCnt : String;
  LLen : Integer;
  {$IFDEF UNICODE}
  enc : TEncoding;
  b : TBytes;
  {$ENDIF}
Begin
  LCnt := '';
  StringAppendStart(LCnt, LLen);
  If Not ANoXMLDec Then
    Begin
    {$IFDEF UNICODE}
    StringAppend(LCnt, '<?xml version="1.0" encoding="UTF-8" ?>', LLen);
    {$ELSE}
    StringAppend(LCnt, '<?xml version="1.0" encoding="ISO-8859-1" ?>', LLen);
    {$ENDIF}
    End;
  (FRoot As TIdSoapCustomElement).WriteToString(LCnt, LLen, -1);
  StringAppendClose(LCnt, LLen);
  {$IFDEF UNICODE}
  enc := TUTF8Encoding.Create;
  try
    b := enc.GetBytes(LCnt);
    aDest.Write(b[0], Length(b));
  finally
    enc.Free;
  end;
  {$ELSE}
  ADest.Write(LCnt[1], Length(LCnt));
  {$ENDIF}
End;

Procedure TIdSoapCustomDom.CheckForBOM;
Begin
  If Copy(FSrc, FCursor+1, 3) = #239#187#191 Then
    Inc(FCursor, 3);
End;

procedure TIdSoapCustomDom.StripWhitespace;
begin
  TIdSoapCustomElement(FRoot).StripWhitespace;
end;

procedure TIdSoapCustomDom.WritePretty(ADest: TStream);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapCustomDom.writePretty';
Var
  LCnt : String;
  LLen : Integer;
  {$IFDEF UNICODE}
  b : TBytes;
  {$ENDIF}
Begin
  LCnt := '';
  StringAppendStart(LCnt, LLen);
  StringAppend(LCnt, '<?xml version="1.0" encoding="UTF-8" ?>'+#13#10, LLen);
  (FRoot As TIdSoapCustomElement).WriteToString(LCnt, LLen, 0);
  StringAppendClose(LCnt, LLen);
  {$IFDEF UNICODE}
  b := TEncoding.UTF8.GetBytes(LCnt);
  ADest.Write(b[0], Length(b));
  {$ELSE}
  ADest.Write(LCnt[1], Length(LCnt));
  {$ENDIF}
end;


{$IFDEF UNICODE}
function IsXmlName(const S: String): boolean;
var
  i: integer;
begin
  Result:= true;
  if Length(S) = 0 then begin Result:= false; exit; end;
  if not ( {$IFDEF UNICODE}IsXmlNameChar{$ELSE}IsXmlLetter{$ENDIF}(PWideChar(S)^)
           or (PWideChar(S)^ = '_')
           or (PWideChar(S)^ = ':')   )
    then begin Result:= false; exit; end;
  for i:= 2 to length(S) do
    if not IsXmlNameChar((PChar(S)+i-1)^)
      then begin Result:= false; exit; end;
end;

{ TIdSoapDelphiXmlDom }

procedure TIdSoapDelphiXmlDom.Clear;
begin
  inherited;
  doc := nil;
end;

procedure TIdSoapDelphiXmlDom.DoRead(ASource: TStream);
begin
  dom := TXMLDocument.Create(nil);
  dom.DOMVendor := OpenXML4Factory;
  dom.ParseOptions := [poPreserveWhiteSpace];
  dom.Options := [{doNodeAutoCreate, doNodeAutoIndent, doAttrNull,  doAutoPrefix, doAutoSave} doNamespaceDecl];
  doc := dom;
  doc.LoadFromStream(aSource);
  FRoot := TIdSoapDelphiXmlElement.create(self, nil, doc.DocumentElement);
  IterateChildren(FRoot as TIdSoapDelphiXmlElement);
end;

function TIdSoapDelphiXmlDom.CloneNodeToDoc(const SourceNode: IXMLNode; Deep: Boolean = True): IXMLNode;
var
  I: Integer;
begin
    case SourceNode.nodeType of
      ntElement:
        begin
          Result := doc.CreateElement(SourceNode.NodeName, SourceNode.NamespaceURI);
          if Deep then
            for I := 0 to SourceNode.ChildNodes.Count - 1 do
              Result.ChildNodes.Add(CloneNodeToDoc(SourceNode.ChildNodes[I], Deep));
        end;
      ntAttribute:
        begin
          Result := doc.CreateNode(SourceNode.NodeName, ntAttribute, SourceNode.NamespaceURI);
          Result.NodeValue := SourceNode.NodeValue;
        end;
      ntText, ntCData, ntComment:
          if SourceNode.NodeValue = null then
            Result := doc.CreateNode('', SourceNode.NodeType)
          else
            Result := doc.CreateNode(SourceNode.NodeValue, SourceNode.NodeType);
      ntEntityRef:
          Result := doc.createNode(SourceNode.nodeName, SourceNode.NodeType);
      ntProcessingInstr:
          Result := doc.CreateNode(SourceNode.NodeName, ntProcessingInstr, SourceNode.NodeValue);
      ntDocFragment:
        begin
          Result := doc.CreateNode('', ntDocFragment);
          if Deep then
            for I := 0 to SourceNode.ChildNodes.Count - 1 do
              Result.ChildNodes.Add(CloneNodeToDoc(SourceNode.ChildNodes[I], Deep));
        end;
      else
       {ntReserved, ntEntity, ntDocument, ntDocType:}
        XMLDocError(SInvalidNodeType);
    end;
end;

procedure TIdSoapDelphiXmlDom.CopyChildNodes(SrcNode, DestNode: IXMLNode);
var
  I: Integer;
  SrcChild, DestChild: IXMLNode;
begin
  for I := 0 to SrcNode.ChildNodes.Count - 1 do
  begin
    SrcChild := SrcNode.ChildNodes[I];
    DestChild := CloneNodeToDoc(SrcChild, False);
    { Note this fails on documents with DOCTYPE nodes }
    DestNode.ChildNodes.Add(DestChild);
    if SrcChild.HasChildNodes or (srcChild.nodeType = ntElement) then
      CopyChildNodes(SrcChild, DestChild);
  end;
  if SrcNode.nodeType = ntElement then
    for I := 0 to SrcNode.AttributeNodes.Count - 1 do
      DestNode.AttributeNodes.Add(CloneNodeToDoc(SrcNode.AttributeNodes[I], False));
end;

function TIdSoapDelphiXmlDom.ImportElement(AElem: TIdSoapXmlElement): TIdSoapXmlElement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDelphiXmlDom.ImportElement';
var
  node : IXMLNode;
Begin
  Assert(Self.TestValid(TIdSoapDelphiXmlDom), ASSERT_LOCATION+': self is not valid');
  // check that XML provider is the right type
  IdRequire(AElem.TestValid(TIdSoapDelphiXmlElement), ASSERT_LOCATION+': Element is not valid');

  node := doc.CreateElement(AElem.Name, AElem.Namespace);
  CopyChildNodes(TIdSoapDelphiXmlElement(AElem).FElement, node);
  Result := TIdSoapDelphiXmlElement.Create(Self, nil, node);
  IterateChildren(TIdSoapDelphiXmlElement(result));
end;


procedure TIdSoapDelphiXmlDom.IterateChildren(AElem: TIdSoapDelphiXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlDom.IterateChildren';
Var
  LNode : IXMLNode;
  i : integer;
  LChild : TIdSoapDelphiXmlElement;
Begin
  Assert(Self.TestValid(TIdSoapDelphiXmlDom), ASSERT_LOCATION+': self is not valid');
  Assert(AElem.TestValid(TIdSoapDelphiXmlElement), ASSERT_LOCATION+': Elem is not valid');
  for i := 0 to AElem.FElement.ChildNodes.Count - 1 do
  begin
    LNode := AElem.FElement.ChildNodes[i];
    if Lnode.NodeType = ntElement then
    begin
      LChild := TIdSoapDelphiXmlElement.Create(Self, AElem, LNode);
      AElem.PrivAppendChild(LChild);
      IterateChildren(LChild);
    end;
  end;

end;

procedure TIdSoapDelphiXmlDom.StartBuild(AName, ANS: String);
begin
  dom := TXMLDocument.Create(nil);
  dom.DOMVendor := OpenXML4Factory;
  dom.Options := [{doNodeAutoCreate, doNodeAutoIndent, doAttrNull,
    doAutoPrefix, doNamespaceDecl, doAutoSave}];
  dom.Active := true;
  doc := dom;
  FRoot := TIdSoapDelphiXmlElement.Create(self, nil, doc.AddChild(AName, ANS));
end;


procedure TIdSoapDelphiXmlDom.writeUTF16(ADest: TStream; ANoXMLDec: Boolean);
begin
  doc.Encoding := 'UTF-16LE';
  doc.SaveToStream(aDest);
end;

procedure TIdSoapDelphiXmlDom.writeUTF8(ADest: TStream; ANoXMLDec: Boolean);
begin
  doc.Encoding := 'UTF-8';
  doc.SaveToStream(aDest);
end;


{ TIdSoapDelphiXmlElement }

constructor TIdSoapDelphiXmlElement.Create(ADom: TIdSoapXmlDom; AParent: TIdSoapXmlElement; aNode: IXMLNode);
begin
  inherited Create(ADom, AParent);
  FElement := aNode;
end;

function TIdSoapDelphiXmlElement.AppendChild(AName, Ans: WideString): TIdSoapxmlelement;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.AppendChild';
Var
  LElem : IXMLNode;
Begin
  Assert(Self.TestValid(TIdSoapDelphiXmlElement), ASSERT_LOCATION+': self is not valid');
  Assert(AName <> '', ASSERT_LOCATION+': name is not valid');

  FLastFindName := '';
  LElem := FElement.addChild(AName, Ans);
  Result := TIdSoapDelphiXmlElement.Create(FDom, Self, LElem);
  PrivAppendChild(Result);
end;

procedure TIdSoapDelphiXmlElement.appendComment(source: String);
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TIdSoapDelphiXmlElement.BuildChildFromXML(ASrc: WideString);
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TIdSoapDelphiXmlElement.ClearChildren;
begin
  raise Exception.Create('Not Done Yet');
end;

function TIdSoapDelphiXmlElement.GetAsXML: AnsiString;
var
  b : TBytes;
  s : String;
Begin
  s := IdTrimBOM(FElement.xml);

  b := TEncoding.UTF8.GetBytes(s);
  SetLength(result, length(b));
  move(b[0], result[1], length(b));
end;

function TIdSoapDelphiXmlElement.getAttribute(const ANamespace, AName: WideString): WideString;
begin
  if FElement.HasAttribute(AName, ANamespace) then
    result := FElement.GetAttributeNS(AName, ANamespace)
  else
    result := '';
end;

function TIdSoapDelphiXmlElement.GetAttributeCount: Integer;
begin
  result := FElement.AttributeNodes.Count;
end;

function TIdSoapDelphiXmlElement.getAttributeName(i: Integer; var VNamespace, VName: WideString): Boolean;
var
  n : IXmlNode;
begin
  result := (i >= 0) and (i < FElement.AttributeNodes.Count);
  if result then
  begin
    n := FElement.AttributeNodes[i];
    VNamespace := n.NamespaceURI;
    VName := n.LocalName;
  end;
end;

function TIdSoapDelphiXmlElement.GetHasText: Boolean;
begin
  result := FElement.IsTextElement;
end;

function TIdSoapDelphiXmlElement.GetName: WideString;
begin
  result := FElement.LocalName;
end;

function TIdSoapDelphiXmlElement.GetNamespace: WideString;
begin
  result := FElement.NamespaceURI;
end;

function TIdSoapDelphiXmlElement.GetNodeName: WideString;
begin
  result := FElement.nodeName;
end;

function TIdSoapDelphiXmlElement.GetTextContentA: String;
begin
//  if FElement.ChildNodes.Count = 0 then
    result := FElement.Text
//  else if (FElement.ChildNodes.Count = 1) and (FElement.ChildNodes[0].IsTextElement) then
//    result := FElement.ChildNodes[0].Text
//  else
//    raise EIdSoapRequirementFail.Create('Attempt to read complex as text ('+FElement.NodeName+': '+inttostr(FElement.ChildNodes.Count)+' children');
end;

function TIdSoapDelphiXmlElement.GetTextContentW: WideString;
begin
  result := GetTextContentA;
end;

procedure TIdSoapDelphiXmlElement.GrabChildren(AElem: TIdSoapXmlElement; AOtherDOM: Boolean);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDelphiXmlElement.GrabChildren';
Begin
  Assert(Self.TestValid(TIdSoapDelphiXmlElement), ASSERT_LOCATION+': self is not valid');
  // check that XML provider is the right type
  IdRequire(AElem.TestValid(TIdSoapDelphiXmlElement), ASSERT_LOCATION+': Element is not valid');

  TIdSoapDelphiXmlDom(FDom).CopyChildNodes(TIdSoapDelphiXmlElement(AElem).FElement, FElement);
  // todo: TIdSoapDelphiXmlDom(FDom).IterateChildren(self);
end;

function TIdSoapDelphiXmlElement.hasAttribute(const ANamespace, AName: WideString): Boolean;
begin
  result := FElement.HasAttribute(aName, ANamespace);
end;

function TIdSoapDelphiXmlElement.InsertChild(AIndex: Integer; AName, ANs: WideString): TIdSoapxmlelement;
begin
  raise Exception.Create('Not Done Yet');
end;

procedure TIdSoapDelphiXmlElement.removeChild(AElement: TIdSoapXmlElement);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapOpenXmlElement.removeChild';
Begin
  Assert(Self.TestValid(TIdSoapDelphiXmlElement), ASSERT_LOCATION+': self is not valid');
  If AElement = Nil Then
    Exit;

  FLastFindName := '';
  Assert(AElement.TestValid(TIdSoapDelphiXmlElement), ASSERT_LOCATION+': element is not valid');
  FElement.ChildNodes.Remove((AElement As TIdSoapDelphiXmlElement).FElement);
  PrivRemoveChild(AElement);
end;

Function ResolveDelphiXMLNamespaceCode(AElement : IXMLNode; ANamespace, ALocation : String):String;
Const ASSERT_LOCATION = 'IdSoapXML.ResolveDelphiXMLNamespaceCode';
Var
  i : Integer;
  LAttr : IXMLNode;
Begin
  Assert(AElement <> nil, ASSERT_LOCATION+': self is not valid');
  Assert(ANamespace <> '', ASSERT_LOCATION+': namespace is blank ('+ALocation+')');
  Assert(ALocation <> '', ASSERT_LOCATION+': Location is blank ('+ALocation+')');

  Result := '';
  For i := 0 To AElement.AttributeNodes.Count - 1 Do
    Begin
    LAttr := AElement.AttributeNodes[i];
    If (LAttr.Prefix = 'xmlns') And
       (LAttr.LocalName = ANameSpace) Then
      Begin
      Result := LAttr.Text;
      Break;
      End;
    End;
  If Result = '' Then
    Begin
    If Assigned(AElement.parentNode) Then
      Begin
      Result := ResolveDelphiXMLNamespaceCode(AElement.parentNode As IXMLNode, ANamespace, ALocation);
      End
    Else
      Begin
      Raise EIdSoapNamespaceProblem.CreateFmt(RS_ERR_SOAP_UNRESOLVABLE_NAMESPACE, [ANamespace, ALocation]);
      End;
    End;
End;

function TIdSoapDelphiXmlElement.ResolveXMLNamespaceCode(ANamespace,  ALocation: WideString): String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapMSXmlElement.ResolveXMLNamespaceCode';
Begin
  Assert(Self.TestValid(TIdSoapDelphiXmlElement), ASSERT_LOCATION+': self is not valid');
  Result := ResolveDelphiXMLNamespaceCode(FElement, ANamespace, ALocation);
end;

procedure TIdSoapDelphiXmlElement.setAttribute(ANs, AName, AValue: WideString);
begin
  FElement.SetAttributeNS(AName, Ans, AValue);
end;

procedure TIdSoapDelphiXmlElement.SetTextContentA(AValue: String);
Const
  ASSERT_LOCATION = ASSERT_UNIT+'.TIdSoapDelphiXmlElement.SetTextContent';
Begin
  Assert(Self.TestValid(TIdSoapDelphiXmlElement), ASSERT_LOCATION+': self is not valid');
  // no check on AValue
  Assert(FElement.childNodes.Count = 0, ASSERT_LOCATION+': attempt to set TextContent when children already exist');
  FElement.Text := AValue;
end;

procedure TIdSoapDelphiXmlElement.SetTextContentW(AValue: WideString);
begin
  SetTextContentA(AValue);
end;

{$ENDIF}

{$IFNDEF DESIGNTIME} // work around unable to compile design time project with OLEServer
Initialization
  DetermineMsXmlProgId;
{$ENDIF}
End.


