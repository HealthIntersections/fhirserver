unit MsXmlBuilder;

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
  SysUtils, Classes,
  StringSupport, EncodeSupport, TextUtilities,
  AdvStreams, AdvVCLStreams,  AdvObjects, AdvGenerics,
  MXML, XmlBuilder, ParserSupport;

Type
  TMsXmlBuilder = class (TXmlBuilder)
  private
    FExternal : Boolean;
    FStack : TAdvList<TMXmlElement>;
    FDoc : TMXmlDocument;
    FAttributes : TAdvMap<TMXmlAttribute>;
    FSourceLocation : TSourceLocation;
    Function Pad(offset : integer = 0) : String;
    function ReadTextLength(s : string):String;
    function ReadTextLengthWithEscapes(pfx, s, sfx : string):String;
//    procedure WriteMsXml(iElement: IXMLDomElement);
//    procedure WriteMsXmlNode(iDoc: IXMLDOMNode);
//    procedure WriteXmlDocument(iDoc: IXMLDocument);
//    procedure WriteXmlNode(iDoc: IXMLNode; first: boolean);
  Public
    Constructor Create; Override;
    Destructor Destroy; override;
    Procedure Start(oNode : TMXmlElement); overload;
    Procedure Start(); overload; override;
    Procedure StartFragment; override;
    Procedure Finish; override;
    Procedure Build(oStream: TStream);  Overload; override;
    Procedure Build(oStream: TAdvStream);  Overload; override;
    Function Build : String;  Overload; override;

    Function SourceLocation : TSourceLocation; override;
    Procedure Comment(Const sContent : String); override;
    Procedure AddAttribute(Const sName, sValue : String); override;
    Procedure AddAttributeNS(Const sNamespace, sName, sValue : String); override;
    function Tag(Const sName : String) : TSourceLocation; override;
    function Open(Const sName : String) : TSourceLocation; override;
    Procedure Close(Const sName : String); override;
    function Text(Const sValue : String) : TSourceLocation; override;
    function Entity(Const sValue : String) : TSourceLocation; override;
    function TagText(Const sName, sValue : String) : TSourceLocation; override;
    Procedure WriteXml(iElement : TMXmlElement); override;
    procedure ProcessingInstruction(sName, sText : String); override;
    procedure DocType(sText : String); override;

    procedure inject(const bytes : TBytes); override;
  End;

Implementation

Uses
  MsXmlParser,
  ActiveX,
  ComObj;


{ TMsXmlBuilder }

function TMsXmlBuilder.SourceLocation: TSourceLocation;
begin
  result.line := 0;
  result.col := 0;
end;

procedure TMsXmlBuilder.Start(oNode : TMXmlElement);
begin
  if oNode = nil Then
  Begin
    FDoc := TMXmlDocument.create;
    if CharEncoding <> '' Then
      FDoc.addC(TMXmlElement.createProcessingInstruction(CharEncoding));
    FStack.Add(FDoc.Link);
  End
  else
    FStack.Add(oNode.Link);
  FSourceLocation.line := 0;
  FSourceLocation.col := 0;
end;

procedure TMsXmlBuilder.Build(oStream: TAdvStream);
var
  oVCL : TVCLStream;
Begin
  oVCL := TVCLStream.Create;
  Try
    oVCL.Stream := oStream.Link;
    Build(oVCL);
  Finally
    oVCL.Free;
  End;
End;

procedure TMsXmlBuilder.Finish;
Begin
  if FStack.Count > 1 Then
    RaiseError('Close', 'Document is not finished');
  FDoc.Free;
End;

procedure TMsXmlBuilder.inject(const bytes: TBytes);
begin
  raise Exception.Create('Inject is not supported on the MSXml Builder');
end;

procedure TMsXmlBuilder.Build(oStream: TStream);
Var
  vAdapter : Variant;
  b : TBytes;
begin
  assert(FAttributes = nil);
  assert(not FExternal);
  b := TEncoding.UTF8.GetBytes(FStack[0].ToXml(true));
  oStream.Write(b[0], length(b));
end;


{function HasElements(oElem : IXMLDOMElement) : Boolean;
var
  oChild : IXMLDOMNode;
Begin
  Result := False;
  oChild := oElem.firstChild;
  While Not result and (oChild <> nil) Do
  Begin
    result := oChild.nodeType = NODE_ELEMENT;
    oChild := oChild.nextSibling;
  End;
End;
}
function TMsXmlBuilder.Open(const sName: String) : TSourceLocation;
var
  oElem : TMXmlElement;
  oParent : TMXmlElement;
  iLoop : integer;
  len : integer;
begin
  oElem := nil;
  try
    if CurrentNamespaces.DefaultNS <> '' Then
    begin
      oElem := TMXmlElement.Create(ntElement, sName, CurrentNamespaces.DefaultNS);
      len := length(sName)+3
    end
    Else
    begin
      oElem := TMXmlElement.Create(ntElement, sName);
      len := length(sName);
    end;
    oParent := FStack.Last;
    if IsPretty and (oParent.NodeType = ntElement) Then
      oParent.addC(TMXmlElement.createText(ReadTextLength(#13#10+pad)));
    oParent.addC(oElem);
    inc(FSourceLocation.col, len+2);
    for iLoop := 0 to FAttributes.Count - 1 Do
      oElem.attributes.addAll(FAttributes);
    FAttributes.Clear;
    FStack.Add(oElem.Link);
    result.line := FSourceLocation.line;
    result.col := FSourceLocation.col;
  finally
    oElem.Free;
  end;
end;

procedure TMsXmlBuilder.Close(const sName: String);
begin
  if IsPretty Then
  Begin
    If FStack.Last.HasChildren Then
      FStack.Last.addC(TMXmlElement.createText(readTextLength(#13#10+pad(-1))));
  End;
  FStack.Delete(FStack.Count - 1)
end;


procedure TMsXmlBuilder.AddAttribute(const sName, sValue: String);
begin
  FAttributes.AddOrSetValue(sName, TMXmlAttribute.Create(sValue));
  ReadTextLengthWithEscapes(sName+'="', sValue, '"');
end;

function TMsXmlBuilder.Text(const sValue: String) : TSourceLocation;
begin
  FStack.Last.addC(TMXmlElement.createText(ReadTextLengthWithEscapes('', sValue, '')));
  result.line := FSourceLocation.line;
  result.col := FSourceLocation.col;
end;

procedure TMsXmlBuilder.WriteXml(iElement: TMXmlElement);
begin
  raise Exception.Create('TMsXmlBuilder.WriteXml not Done Yet');

end;

function TMsXmlBuilder.Entity(const sValue: String) : TSourceLocation;
begin
  FStack.Last.addC(TMXmlElement.createText('&'+sValue+';'));
  inc(FSourceLocation.col, length(sValue)+2);
  result.line := FSourceLocation.line;
  result.col := FSourceLocation.col;
end;

function TMsXmlBuilder.Tag(const sName: String) : TSourceLocation;
begin
  Open(sName);
  Close(sName);
  result.line := FSourceLocation.line;
  result.col := FSourceLocation.col;
end;

function TMsXmlBuilder.TagText(const sName, sValue: String) : TSourceLocation;
begin
  result := Open(sName);
  if (sValue <> '') Then
    Text(sValue);
  Close(sName);
end;

function TMsXmlBuilder.Pad(offset : integer = 0): String;
var
  iLoop : integer;
begin
  Setlength(result, ((FStack.Count - 1) + offset) * 2);
  For iLoop := 1 to Length(Result) Do
    result[iLoop] := ' ';
end;


procedure TMsXmlBuilder.ProcessingInstruction(sName, sText: String);
begin
  raise Exception.Create('Not supported yet');
end;

function TMsXmlBuilder.ReadTextLength(s: string): String;
var
  i : integer;
begin
  i := 1;
  while i <= length(s) do
  begin
    if CharInSet(s[i], [#10, #13]) then
    begin
      inc(FSourceLocation.line);
      FSourceLocation.col := 0;
      if (i < length(s)) and (s[i+1] <> s[i]) and CharInSet(s[i+1], [#10, #13]) then
        inc(i);
    end
    else
      inc(FSourceLocation.col);
    inc(i);
  end;
end;

function TMsXmlBuilder.ReadTextLengthWithEscapes(pfx, s, sfx: string): String;
begin
  ReadTextLength(pfx);
  ReadTextLength(EncodeXml(s, xmlText));
  ReadTextLength(sfx);
end;

Procedure TMsXmlBuilder.Comment(Const sContent : String);
begin
  if IsPretty and (FStack.Last.nodeType = ntElement) Then
    FStack.Last.addC(TMXmlElement.createText(ReadTextLength(#13#10+pad)));
  FStack.Last.addC(TMXmlElement.createComment(sContent));
  ReadTextLength('<!--'+sContent+'-->');
End;


function TMsXmlBuilder.Build: String;
var
  oStream : TStringStream;
begin
  oStream := TStringStream.Create('');
  Try
    Build(oStream);
    Result := oStream.DataString;
  Finally
    oStream.Free;
  End;
end;

constructor TMsXmlBuilder.Create;
begin
  inherited;
  CurrentNamespaces.DefaultNS := 'urn:hl7-org:v3';
  CharEncoding := 'UTF-8';
  FStack := TAdvList<TMXmlElement>.Create;
  FAttributes := TAdvMap<TMXmlAttribute>.Create;
end;

destructor TMsXmlBuilder.Destroy;
begin
  FStack.Free;
  FStack := nil;
  FAttributes.Free;
  FAttributes := nil;
  inherited;
end;

procedure TMsXmlBuilder.DocType(sText: String);
begin
  raise Exception.Create('Not supported yet');
end;

{procedure TMsXmlBuilder.WriteMsXml(iElement: IXMLDomElement);
var
  i : integer;
  oElem : IXMLDOMElement;
begin
  oElem := FStack[FStack.Count - 1] as IXMLDOMElement;
  For i := 0 to iELement.childNodes.length - 1 do
    if (iElement.childNodes[i].nodeType <> NODE_TEXT) or not StringIsWhitespace(iElement.childNodes[i].text) Then
      oElem.appendChild(iElement.childNodes[i].CloneNode(true));
end;
}

procedure TMsXmlBuilder.AddAttributeNS(const sNamespace, sName, sValue: String);
var
  attr : TMXmlAttribute;
begin
  attr := TMXmlAttribute.Create(sValue);
  try
    attr.NamespaceURI := sNamespace;
    attr.LocalName := sName;
    FAttributes.AddOrSetValue(sName, attr.link);
  finally
    attr.free;
  end;
  ReadTextLengthWithEscapes(sName+'="', sValue, '"');
end;

procedure TMsXmlBuilder.Start;
begin
  Start(nil);
end;

procedure TMsXmlBuilder.StartFragment;
begin
  Start(nil);
end;

{procedure TMsXmlBuilder.WriteXml(iElement: IXMLNode; first : boolean);
begin
  raise Exception.Create('Not supported yet');
end;

procedure TMsXmlBuilder.WriteXmlDocument(iDoc: IXMLDocument);
begin
  raise Exception.Create('Not supported yet');
end;

procedure TMsXmlBuilder.WriteMsXmlNode(iDoc: IXMLDOMNode);
begin
  raise Exception.Create('Not supported yet');
end;

procedure TMsXmlBuilder.WriteXmlNode(iDoc: IXMLNode; first : boolean);
begin
  raise Exception.Create('Not supported yet');
end;

}
End.
