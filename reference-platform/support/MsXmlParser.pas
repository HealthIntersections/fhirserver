Unit MsXmlParser;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

Interface

Uses
  Windows, SysUtils, Classes, ComObj, Generics.Collections,
  AdvObjects, Advmemories, AdvBuffers, AdvStreams, AdvStringLists, AdvGenerics,
  XmlBuilder, MsXml, ParserSupport;

const
  MAP_ATTR_NAME = 'B88BF977DA9543B8A5915C84A70F03F7';

Type
  TTextAction = (ttAsIs, ttTrim, ttTrimPad);

  TMsXmlSaxHandler = class (TinterfacedObject, IVBSAXContentHandler, IVBSAXErrorHandler)
  private
    FLocator : IVBSAXLocator;
    FLocation : TSourceLocation;
    FExceptionMessage : String;
  protected
    FXmlComments : TAdvStringList;
    procedure startElement(sourceLocation : TSourceLocation; uri, localname : string; attrs : IVBSAXAttributes); overload; virtual;
    procedure endElement(sourceLocation : TSourceLocation); overload; virtual;
    procedure text(chars : String; sourceLocation : TSourceLocation); virtual;
  public
    Constructor create;
    destructor Destroy; override;

    property ExceptionMessage : String read FExceptionMessage;
    { SAX }
   // IDispatch
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    procedure _Set_documentLocator(const locator: IVBSAXLocator); safecall;
    procedure Set_documentLocator(const locator: IVBSAXLocator); safecall;
    procedure startDocument; safecall;
    procedure endDocument; safecall;
    procedure startPrefixMapping(var prefix, uri: widestring); safecall;
    procedure endPrefixMapping(var prefix: WideString); safecall;
    procedure startElement(var uri, localname, qname : widestring; const attrs: IVBSAXAttributes); overload; safecall;
    procedure endElement(var uri, localname, qname : WideString); overload; safecall;
    procedure characters(var chars: WideString); safecall;
    procedure ignorableWhitespace(var text: WideString); safecall;
    procedure processingInstruction(var target, data: WideString); safecall;
    procedure skippedEntity(var name: wideString); safecall;
    procedure error(const oLocator: IVBSAXLocator; var strErrorMessage: WideString; nErrorCode: Integer); safecall;
    procedure fatalError(const oLocator: IVBSAXLocator; var strErrorMessage: WideString; nErrorCode: Integer); safecall;
    procedure ignorableWarning(const oLocator: IVBSAXLocator; var strErrorMessage: WideString; nErrorCode: Integer); safecall;
  end;

  TLocatingSaxToDomParser = class (TMsXmlSaxHandler)
  private
    FStack : TList<IXMLDOMElement>;
    FDom : IXMLDOMDocument2;
    FLastStart : TSourceLocation;
    FLocations : TAdvList<TSourceLocationObject>;
    FTimeToAbort : Cardinal;
  public
    constructor create(locations : TAdvList<TSourceLocationObject>; timeToAbort : cardinal);
    destructor Destroy; override;
    property DOm : IXMLDOMDocument2 read FDom;
    procedure startElement(sourceLocation : TSourceLocation; uri, localname : string; attrs : IVBSAXAttributes); override;
    procedure endElement(sourceLocation : TSourceLocation); overload; override;
    procedure text(chars : String; sourceLocation : TSourceLocation); override;
  end;


  TMsXmlParser = class (TAdvObject)
  Private
  Public
    Class Function Parse(Const sFilename : String) : IXMLDomDocument2; Overload;
    Class Function Parse(Const oSource : TStream) : IXMLDomDocument2; Overload;
    Class Function Parse(Const oSource : TAdvStream) : IXMLDomDocument2; Overload;
    Class Function Parse(Const oSource : TAdvBuffer) : IXMLDomDocument2; Overload;
    Class Function Parse(Const bytes : TBytes) : IXMLDomDocument2; Overload;
    Class Function ParseString(Const sSource : String) : IXMLDomDocument2; Overload;

    Class Function Parse(Const sFilename : String; locations : TAdvList<TSourceLocationObject>) : IXMLDomDocument2; Overload;
    Class Function Parse(Const oSource : TStream; locations : TAdvList<TSourceLocationObject>) : IXMLDomDocument2; Overload;
    Class Function Parse(Const oSource : TAdvStream; locations : TAdvList<TSourceLocationObject>) : IXMLDomDocument2; Overload;
    Class Function Parse(Const oSource : TAdvBuffer; locations : TAdvList<TSourceLocationObject>) : IXMLDomDocument2; Overload;
    Class Function Parse(Const bytes : TBytes; locations : TAdvList<TSourceLocationObject>) : IXMLDomDocument2; Overload;
    Class Function ParseString(Const sSource : String; locations : TAdvList<TSourceLocationObject>) : IXMLDomDocument2; Overload;

    Class Function GetAttribute(oElement : IXMLDOMElement; Const sName : String) : String; overload;
    Class Function GetAttribute(oElement : IXMLDOMElement; Const sNamespace, sName : String) : String; overload;
    Class Function FirstChild(oElement : IXMLDOMNode) : IXMLDOMElement;
    Class Function NextSibling(oElement : IXMLDOMElement) : IXMLDOMElement;
    Class Function NamedChild(oElement : IXMLDOMElement; name : String) : IXMLDOMElement;
    Class Function TextContent(oElement : IXMLDOMElement; aTextAction : TTextAction) : String;
    Class Procedure getNamedChildrenWithWildcard(oElement : IXMLDOMElement; name : string; children : TInterfaceList);

    Class Procedure ParseByHandler(Const sFilename : String; handler : TMsXmlSaxHandler); Overload;
    Class Procedure ParseByHandler(Const oSource : TStream; handler : TMsXmlSaxHandler); Overload;
    Class Procedure ParseByHandler(Const oSource : TAdvStream; handler : TMsXmlSaxHandler); Overload;
    Class Procedure ParseByHandler(Const oSource : TAdvBuffer; handler : TMsXmlSaxHandler); Overload;
  End;

Procedure DetermineMsXmlProgId;
Function LoadMsXMLDom : IXMLDomDocument2;
Function LoadMsXMLDomV(isFree : boolean = false) : Variant;

Var
  GMsXmlProgId_DOM : String;
  GMsXmlProgId_FTDOM : String;
  GMsXmlProgId_SCHEMA : String;
  GMsXmlProgId_XSLT : String;
  GMsXmlProgId_XSLP : String;
  GMsXmlProgId_SAX : String;

Implementation

Uses
  ActiveX,
  AdvWinInetClients,
  MXmlBuilder,
  StringSupport,
  AdvVclStreams;

Procedure DetermineMsXmlProgId;
  Function TryLoad(sId : String) : Boolean;
  Var
    ClassID: TCLSID;
    iTest : IDispatch;
    Res : HResult;
  Begin
    Result := false;
    if Succeeded(CLSIDFromProgID(PWideChar(String('MSXML2.DOMDocument'+sId)), ClassID)) Then
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


{ TMsXmlParser }

Class function TMsXmlParser.Parse(const sFilename: String): IXMLDomDocument2;
begin
  result := parse(sFileName, nil);
end;

Class function TMsXmlParser.Parse(const sFilename: String; locations : TAdvList<TSourceLocationObject>): IXMLDomDocument2;
var
  oFile : TFileStream;
  oWeb : TAdvWinInetClient;
begin
  if StringStartsWith(sFilename, 'http:') or StringStartsWith(sFilename, 'https:') or StringStartsWith(sFilename, 'ftp:')  Then
  Begin
    oWeb := TAdvWinInetClient.Create;
    Try
//      oWeb.SetAddress(sFilename);
      oWeb.RequestMethod := 'GET';
      oWeb.Request := TAdvBuffer.Create;
      oWeb.Response := TAdvBuffer.Create;
      oWeb.Execute;
      if oWeb.ResponseCode <> '200' Then
        Raise Exception.Create('HTTP Error '+oWeb.ResponseCode);
      result := Parse(oWeb.Response, locations);
    Finally
      oWeb.Free;
    End;
  End
  Else
  Begin
    oFile := TFileStream.Create(sFilename, fmOpenRead + fmShareDenyWrite);
    Try
      Result := Parse(oFile, locations);
    Finally
      oFile.Free;
    End;
  End;
end;


Class function TMsXmlParser.Parse(const oSource: TStream): IXMLDomDocument2;
begin
  result := parse(oSource, nil);
end;

Class function TMsXmlParser.Parse(const oSource: TStream; locations : TAdvList<TSourceLocationObject>): IXMLDomDocument2;
Var
  iDom : IXMLDomDocument2;
  vAdapter : Variant;
  sError : String;
  ms : TMsXmlParser;
  sax : TLocatingSaxToDomParser;
begin
  if (locations = nil) then
  begin
    CoInitializeEx(nil, COINIT_MULTITHREADED);
    iDom := LoadMsXMLDom;
    iDom.validateOnParse := False;
    iDom.preserveWhiteSpace := True;
    iDom.resolveExternals := False;
    iDom.setProperty('NewParser', True);
    iDom.setProperty('ProhibitDTD', false);
    vAdapter := TStreamAdapter.Create(oSource) As IStream;
    if not iDom.load(vAdapter) Then
    Begin
      sError := iDom.parseError.reason + ' at line '+IntToStr(iDom.parseError.line)+' row '+IntToStr(iDom.parseError.linepos);
      if iDom.parseError.url <> '' Then
        sError := sError + '. url="'+ iDom.parseError.url+'"';
      sError := sError + '. source = '+ iDom.parseError.srcText+'"';
      raise Exception.Create(sError);
    End;
    Result := iDom;
  end
  else
  begin
    ms := TMsXmlParser.Create;
    try
      sax := TLocatingSaxToDomParser.create(locations.Link, 0); // no try...finally..., this is interfaced
      iDom := sax.DOM;
      ms.ParseByHandler(oSource, sax);
      result := iDom;
    finally
      ms.Free;
    end;
  end;
end;


class function TMsXmlParser.Parse(const oSource: TAdvStream): IXMLDomDocument2;
begin
  result := parse(oSource, nil);
end;

class function TMsXmlParser.Parse(const oSource: TAdvStream; locations : TAdvList<TSourceLocationObject>): IXMLDomDocument2;
Var
  oWrapper : TVCLStream;
begin
  oWrapper := TVCLStream.Create;
  Try
    oWrapper.Stream := oSource.Link;
    Result := Parse(oWrapper, locations);
  Finally
    oWrapper.Free;
  End;
end;

Class Function TMsXmlParser.GetAttribute(oElement : IXMLDOMElement; Const sName : String) : String;
Var
  LAttr : IXMLDOMNamedNodeMap;
  LNode : IXMLDOMAttribute;
Begin
  LAttr := oElement.attributes;
  LNode := LAttr.getQualifiedItem(sName, '') As IXMLDOMAttribute;
  If Assigned(Lnode) Then
    Result := LNode.text
  Else
  Begin
    LNode := LAttr.getNamedItem(sName) As IXMLDOMAttribute;
    If Assigned(Lnode) Then
      Result := LNode.text;
  End;
End;

Class Function TMsXmlParser.GetAttribute(oElement : IXMLDOMElement; Const sNamespace, sName : String) : String;
Var
  LAttr : IXMLDOMNamedNodeMap;
  LNode : IXMLDOMAttribute;
Begin
  LAttr := oElement.attributes;
  LNode := LAttr.getQualifiedItem(sName, sNamespace) As IXMLDOMAttribute;
  If Assigned(Lnode) Then
    Result := LNode.text
  else
    Result := '';
End;


class procedure TMsXmlParser.getNamedChildrenWithWildcard(oElement: IXMLDOMElement; name: string; children: TInterfaceList);
var
  c : IXMLDOMElement;
  n : string;
begin
  c := FirstChild(oElement);
  while (c <> nil) do
  begin
    if c.baseName <> '' then
      n := c.baseName
    else
      n := c.NodeName;
    if (name = n) or (name.endsWith('[x]') and n.startsWith(name.substring(0, name.length-3))) then
        children.add(c);
    c := NextSibling(c);
  end;
end;

Class Function TMsXmlParser.FirstChild(oElement : IXMLDOMNode) : IXMLDOMElement;
Var
  oNode : IXMLDOMNode;
Begin
  result := Nil;
  oNode := oElement.firstChild;
  While Assigned(oNode) And not Assigned(result) Do
  Begin
    If oNode.nodeType = NODE_ELEMENT Then
      result := oNode as IXMLDOMElement;
    oNode := oNode.nextSibling;
  End;
End;


class function TMsXmlParser.NamedChild(oElement: IXMLDOMElement; name: String): IXMLDOMElement;
var
  n : IXMLDOMElement;
begin
  result := nil;
  n := FirstChild(oElement);
  while (n <> nil) do
  begin
    if n.nodeName = name then
      exit(n);
    n := NextSibling(n);
  end;
end;

Class Function TMsXmlParser.NextSibling(oElement : IXMLDOMElement) : IXMLDOMElement;
Var
  oNode : IXMLDOMNode;
Begin
  result := Nil;
  oNode := oElement.nextSibling;
  While Assigned(oNode) And not Assigned(result) Do
  Begin
    If oNode.nodeType = NODE_ELEMENT Then
      result := oNode as IXMLDOMElement;
    oNode := oNode.nextSibling;
  End;
End;



class procedure TMsXmlParser.ParseByHandler(const oSource: TStream; handler: TMsXmlSaxHandler);
var
  v : variant;
  sax : IVBSAXXMLReader ;
begin
  v := CreateOleObject(GMsXmlProgId_SAX);
  sax := IUnknown(TVarData(v).VDispatch) as IVBSAXXMLReader ;

  sax.PutFeature('prohibit-dtd', false);
  sax.contentHandler := handler;
  sax.errorHandler := handler;

  v := TStreamAdapter.Create(oSource) As IStream;
  sax.parse(v);
  if handler.ExceptionMessage <> '' then
    raise Exception.create(handler.ExceptionMessage);
end;

class procedure TMsXmlParser.ParseByHandler(const sFilename: String; handler: TMsXmlSaxHandler);
var
  oFile : TFileStream;
  oWeb : TAdvWinInetClient;
begin
  if StringStartsWith(sFilename, 'http:') or StringStartsWith(sFilename, 'https:') or StringStartsWith(sFilename, 'ftp:')  Then
  Begin
    oWeb := TAdvWinInetClient.Create;
    Try
//      oWeb.SetAddress(sFilename);
      oWeb.RequestMethod := 'GET';
      oWeb.Request := TAdvBuffer.Create;
      oWeb.Response := TAdvBuffer.Create;
      oWeb.Execute;
      if oWeb.ResponseCode <> '200' Then
        Raise Exception.Create('HTTP Error '+oWeb.ResponseCode);
      ParseByHandler(oWeb.Response, handler);
    Finally
      oWeb.Free;
    End;
  End
  Else
  Begin
    oFile := TFileStream.Create(sFilename, fmOpenRead + fmShareDenyWrite);
    Try
      ParseByHandler(oFile, handler);
    Finally
      oFile.Free;
    End;
  End;

end;

class procedure TMsXmlParser.ParseByHandler(const oSource: TAdvBuffer; handler: TMsXmlSaxHandler);
var
  oMem : TAdvMemoryStream;
begin
  oMem := TAdvMemoryStream.Create;
  try
    oMem.Buffer := oSource.Link;
    ParseByHandler(oMem, handler);
  Finally
    oMem.Free;
  End;
end;

class function TMsXmlParser.ParseString(const sSource: String): IXMLDomDocument2;
begin
  result := parseString(sSource, nil);
end;

class function TMsXmlParser.ParseString(const sSource: String; locations : TAdvList<TSourceLocationObject>): IXMLDomDocument2;
var
  oMem : TBytesStream;
begin
  oMem := TBytesStream.Create(TEncoding.UTF8.GetBytes(sSource));
  try
    result := Parse(oMem, locations);
  Finally
    oMem.Free;
  End;
end;

class procedure TMsXmlParser.ParseByHandler(const oSource: TAdvStream; handler: TMsXmlSaxHandler);
Var
  oWrapper : TVCLStream;
begin
  oWrapper := TVCLStream.Create;
  Try
    oWrapper.Stream := oSource.Link;
    ParseByHandler(oWrapper, handler);
  Finally
    oWrapper.Free;
  End;

end;

Function Trim(Const sValue : WideString; bWhitespaceWithMeaning : Boolean):WideString;
Begin
  result := StringTrimWhitespace(sValue);
  If bWhitespaceWithMeaning And (Result = '') Then
    result := ' ';
End;


class function TMsXmlParser.TextContent(oElement: IXMLDOMElement; aTextAction: TTextAction): String;
Var
  oNode : IXMLDOMNode;
Begin
  result := '';
  if oElement <> nil Then
  Begin
    oNode := oElement.firstChild;
    While Assigned(oNode) Do
    Begin
      If (oNode.nodeType = NODE_TEXT) Then
        result := result + oNode.text;
      oNode := oNode.nextSibling;
    End;
    if (aTextAction <> ttAsIs) Then
      Result := Trim(result, aTextAction = ttTrimPad);
  End;
end;

class function TMsXmlParser.Parse(const oSource: TAdvBuffer): IXMLDomDocument2;
begin
  result := parse(oSource, nil);
end;

class function TMsXmlParser.Parse(const oSource: TAdvBuffer; locations : TAdvList<TSourceLocationObject>): IXMLDomDocument2;
var
  oMem : TAdvMemoryStream;
begin
  oMem := TAdvMemoryStream.Create;
  try
    oMem.Buffer := oSource.Link;
    result := Parse(oMem, locations);
  Finally
    oMem.Free;
  End;
end;


class function TMsXmlParser.Parse(const bytes: TBytes): IXMLDomDocument2;
begin
  result := parse(bytes, nil);
end;

class function TMsXmlParser.Parse(const bytes: TBytes; locations: TAdvList<TSourceLocationObject>): IXMLDomDocument2;
var
  b : TBytesStream;
begin
  b := TBytesStream.Create(bytes);
  try
    result := parse(b, locations);
  finally
    b.Free;
  end;
end;

{ TMsXmlSaxHandler }

procedure TMsXmlSaxHandler.characters(var chars: WideString);
begin
  FLocation.Line := FLocator.lineNumber;
  FLocation.col := FLocator.columnNumber;
  text(chars, FLocation);
end;

constructor TMsXmlSaxHandler.create;
begin
  inherited;
  FXmlComments := TAdvStringList.create;
end;

destructor TMsXmlSaxHandler.destroy;
begin
  FXmlComments.Free;
  inherited;
end;

procedure TMsXmlSaxHandler.endDocument;
begin
  // nothing
end;

procedure TMsXmlSaxHandler.endElement(var uri, localname, qname: WideString);
begin
  FLocation.Line := FLocator.lineNumber;
  FLocation.col := FLocator.columnNumber;
  endElement(FLocation);
end;

procedure TMsXmlSaxHandler.endElement(sourceLocation : TSourceLocation);
begin
  // nothing - override in descendent
end;

procedure TMsXmlSaxHandler.endPrefixMapping(var prefix: WideString);
begin
  // nothing
end;

procedure TMsXmlSaxHandler.error(const oLocator: IVBSAXLocator; var strErrorMessage: WideString; nErrorCode: Integer);
begin
  FExceptionMessage := strErrorMessage+' at line '+inttostr(oLocator.lineNumber);
end;

procedure TMsXmlSaxHandler.fatalError(const oLocator: IVBSAXLocator; var strErrorMessage: WideString; nErrorCode: Integer);
begin
  FExceptionMessage := strErrorMessage+' at line '+inttostr(oLocator.lineNumber);
end;

function TMsXmlSaxHandler.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TMsXmlSaxHandler.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TMsXmlSaxHandler.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TMsXmlSaxHandler.ignorableWarning(const oLocator: IVBSAXLocator;
  var strErrorMessage: WideString; nErrorCode: Integer);
begin
  raise Exception.Create('todo');
end;

procedure TMsXmlSaxHandler.ignorableWhitespace(var text: WideString);
begin
  // nothing
end;

function TMsXmlSaxHandler.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

procedure TMsXmlSaxHandler.processingInstruction(var target, data: WideString);
begin
  // nothing
end;

procedure TMsXmlSaxHandler.Set_documentLocator(const locator: IVBSAXLocator);
begin
  FLocator := locator;
end;

procedure TMsXmlSaxHandler.skippedEntity(var name: wideString);
begin
  // ignore
end;

procedure TMsXmlSaxHandler.startDocument;
begin
  // ignore
end;

procedure TMsXmlSaxHandler.startElement(sourceLocation : TSourceLocation; uri, localname: string; attrs: IVBSAXAttributes);
begin
  // override in descendants
end;

procedure TMsXmlSaxHandler.startElement(var uri, localname, qname: widestring; const attrs: IVBSAXAttributes);
begin
  FLocation.Line := FLocator.lineNumber;
  FLocation.col := FLocator.columnNumber;
  startElement(FLocation, uri, localname, attrs);
end;

procedure TMsXmlSaxHandler.startPrefixMapping(var prefix, uri: widestring);
begin
  // ignore
end;

procedure TMsXmlSaxHandler.text(chars: String; sourceLocation : TSourceLocation);
begin
  // for descendants
end;

procedure TMsXmlSaxHandler._Set_documentLocator(const locator: IVBSAXLocator);
begin
  Set_documentLocator(locator);
end;

{ TLocatingSaxToDomParser }

constructor TLocatingSaxToDomParser.create(locations : TAdvList<TSourceLocationObject>; timeToAbort : cardinal);
begin
  FStack := TList<IXMLDOMElement>.create;
  FDom := CoDOMDocument.Create;
  FLocations := locations;
  FTimeToAbort := timeToAbort;
end;


destructor TLocatingSaxToDomParser.destroy;
begin
  FStack.Free;
  FDom := nil;
  FLocations.Free;
  inherited;
end;

procedure TLocatingSaxToDomParser.startElement(sourceLocation: TSourceLocation; uri,localname: string; attrs: IVBSAXAttributes);
var
  ts : cardinal;
  focus : IXMLDOMElement;
  loc : TSourceLocationObject;
  i : integer;
begin
  ts := GetTickCount;
  if (FTimeToAbort > 0) and (FTimeToAbort < ts) then
    abort;

  focus := FDom.createNode(NODE_ELEMENT, localname, uri) as IXMLDOMElement;
  if FStack.Count = 0 then
    FDom.documentElement := focus
  else
    FStack[FStack.Count-1].appendChild(focus);
  FStack.Add(focus);

  loc := TSourceLocationObject.Create;
  focus.setAttribute(MAP_ATTR_NAME, inttostr(FLocations.Add(loc)));

  // SAX reports that the element 'starts' at the end of the element.
  // which is where we want to end it. So we store the last location
  // we saw anything at, and use that instead

  if isNullLoc(FLastStart) then
    loc.locationStart := sourceLocation
  else
    loc.locationStart := FLastStart;
  loc.locationEnd := nullLoc;

  for i := 0 to attrs.length - 1 do
    focus.setAttribute(attrs.getQName(i), attrs.getValue(i));
  FLastStart := nullLoc;
end;

procedure TLocatingSaxToDomParser.text(chars: String; sourceLocation: TSourceLocation);
var
  sl : TSourceLocationObject;
begin
 // we consider that an element 'ends' where the text or next element
  // starts. That's not strictly true, but gives a better output
  if FStack.Count > 0 then
  begin
    sl := FLocations[StrToInt(FStack[FStack.Count-1].getAttribute(MAP_ATTR_NAME))];
    if isNullLoc(sl.LocationEnd) then
      sl.LocationEnd := sourceLocation;
    FStack[FStack.Count-1].appendChild(FDom.createTextNode(chars));
  end;
  FLastStart := sourceLocation;
end;

procedure TLocatingSaxToDomParser.endElement(sourceLocation: TSourceLocation);
var
  sl : TSourceLocationObject;
begin
  // we consider that an element 'ends' where the text or next element
  // starts. That's not strictly true, but gives a better output
    sl := FLocations[StrToInt(FStack[FStack.Count-1].getAttribute(MAP_ATTR_NAME))];
  if isNullLoc(sl.LocationEnd) then
    sl.LocationEnd := sourceLocation;
  FStack.Delete(FStack.Count-1);
  FLastStart := sourceLocation;
end;


Initialization
  DetermineMsXmlProgId;
End.


