unit XMLBuilder;

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

interface

uses
  SysUtils, Classes,
  AdvStreams, AdvStringMatches, AdvObjectLists, AdvObjects,
  ParserSupport, MXML{, XmlIntf};

type
  TXmlCanonicalisationMethod = (xcmCanonicalise, xcmComments, xcmTrimWhitespace, {xcmPrefixRewrite, } xcmQNameAware);
  TXmlCanonicalisationMethodSet = set of TXmlCanonicalisationMethod;

  TXmlBuilderNamespaceList = class (TAdvStringMatch)
  private
    FDefaultNS : String;
    FNew: TStringList;
    FDefaultSet: boolean;
    procedure SetDefaultNS(const Value: String);
  public
    Constructor Create; override;
    Destructor Destroy; override;

    function link : TXmlBuilderNamespaceList; overload;
    function clone : TXmlBuilderNamespaceList; overload;

    Procedure Assign(oObject : TAdvObject); Override;

    Property DefaultNS : String read FDefaultNS write SetDefaultNS;
    Property DefaultSet : boolean read FDefaultSet write FDefaultSet;
    Property new : TStringList read FNew;
  end;

  TXmlBuilder = {abstract} class (TAdvObject)
  private
    function GetCurrentNamespaces: TXmlBuilderNamespaceList;
    function getNSAbbrev(iElement: TMXMLElement): String;
  Protected
    FNoHeader: Boolean;
    FCanonicalise: TXmlCanonicalisationMethodSet;
    FIsPretty: Boolean;
    FCharEncoding: String;
    FNamespaces : TAdvObjectList;
  Public
    constructor Create; override;
    destructor Destroy; override;

    procedure Start; overload; virtual; abstract;
    procedure StartFragment; overload; virtual; abstract;
    procedure Finish; overload; virtual; abstract;
    procedure Build(oStream: TStream);   overload; virtual; abstract;
    procedure Build(oStream: TAdvStream);   overload; virtual; abstract;
    function Build : String;   overload; virtual; abstract;

    function SourceLocation : TSourceLocation;  overload; virtual; abstract;
    procedure Comment(Const sContent : String); overload; virtual; abstract;
    procedure AddAttribute(Const sName, sValue : String); overload; virtual; abstract;
    procedure AddAttributeNS(Const sNamespace, sName, sValue : String); overload; virtual; abstract;
    function Tag(Const sName : String) : TSourceLocation; overload; virtual; abstract;
    function Open(Const sName : String) : TSourceLocation; overload; virtual; abstract;
    procedure Close(Const sName : String); overload; virtual; abstract;
    function Text(Const sValue : String) : TSourceLocation; overload; virtual; abstract;
    function Entity(Const sValue : String) : TSourceLocation; overload; virtual; abstract;
    function TagText(Const sName, sValue : String) : TSourceLocation; overload; virtual; abstract;
    procedure ProcessingInstruction(sName, sText : String); overload; virtual; abstract;
    procedure DocType(sText : String); overload; virtual; abstract;

    Procedure WriteXml(iElement : TMXMLElement);
{
    Procedure WriteXml(iElement : XmlIntf.IXMLNode; first : boolean); overload; virtual; abstract;
    Procedure WriteXmlNode(iNode : XmlIntf.IXMLNode; first : boolean); overload; virtual; abstract;
    Procedure WriteXmlDocument(iDoc : XmlIntf.IXMLDocument); overload; virtual; abstract;
}

    procedure inject(Const aBytes : TBytes); Overload; virtual; abstract; // not supported on all implementations

    property IsPretty : boolean read FIsPretty Write FIsPretty;
    property CharEncoding : String read FCharEncoding write FCharEncoding;
    property CurrentNamespaces : TXmlBuilderNamespaceList Read GetCurrentNamespaces;
    property NoHeader : Boolean read FNoHeader write FNoHeader;
    property Canonicalise : TXmlCanonicalisationMethodSet read FCanonicalise write FCanonicalise;


    // consumer has to call this manually if it wants to change namespaes, before it starts playing with attributes or namesapces.
    // it must call pop if it calls push
    procedure NSPush;
    Procedure NSPop;
  End;

implementation


{ TXmlBuilderNamespaceList }

procedure TXmlBuilderNamespaceList.Assign(oObject: TAdvObject);
begin
  inherited;

  DefaultNS := TXmlBuilderNamespaceList(oObject).DefaultNS;
  FDefaultSet := false;
end;

function TXmlBuilderNamespaceList.clone: TXmlBuilderNamespaceList;
begin
  result := TXmlBuilderNamespaceList(Inherited Clone);
end;

constructor TXmlBuilderNamespaceList.Create;
begin
  inherited;

end;

destructor TXmlBuilderNamespaceList.Destroy;
begin

  inherited;
end;

function TXmlBuilderNamespaceList.link: TXmlBuilderNamespaceList;
begin
  result := TXmlBuilderNamespaceList(Inherited Link);
end;

procedure TXmlBuilderNamespaceList.SetDefaultNS(const Value: String);
begin
  if FDefaultNS <> Value then
    FDefaultSet := true;
  FDefaultNS := Value;
end;

{ TXmlBuilder }

constructor TXmlBuilder.Create;
begin
  inherited;
  FNamespaces := TAdvObjectList.create;
  FNamespaces.Add(TXmlBuilderNamespaceList.Create());
end;

destructor TXmlBuilder.Destroy;
begin
  FNamespaces.Free;
  inherited;
end;

function TXmlBuilder.GetCurrentNamespaces: TXmlBuilderNamespaceList;
begin
  result := FNamespaces[FNamespaces.Count - 1] as TXmlBuilderNamespaceList
end;

procedure TXmlBuilder.NSPop;
begin
  FNamespaces.DeleteByIndex(FNamespaces.Count - 1);
end;

procedure TXmlBuilder.NSPush;
begin
  FNamespaces.Add(CurrentNamespaces.clone);
end;

function TXmlBuilder.getNSAbbrev(iElement: TMXMLElement) : String;
var
  a : TMXmlAttribute;
  i : integer;
begin
  for a in iELement.Attributes.Values do
    if a.Value = iElement.NamespaceURI then
    begin
      if (a.Name = 'xmlns') then
        exit('')
      else if a.Name.StartsWith('xmlns:') then
        exit(a.Name.Substring(6)+':');
    end;

  if CurrentNamespaces.DefaultNS = iElement.NamespaceURI then
    exit('');

  for i := 0 to CurrentNamespaces.Count - 1 do
  begin
    if CurrentNamespaces.Values[i] = iElement.NamespaceURI then
      exit(CurrentNamespaces.Keys[i]+':');
  end;
  if iElement.attribute['xmlns'] = '' then
  begin
    AddAttribute('xmlns', iElement.NamespaceURI);
    CurrentNamespaces.DefaultNS := iElement.NamespaceURI;
    exit('');
  end
  else
  begin
    AddAttribute('xmlns:ns', iElement.NamespaceURI);
    CurrentNamespaces.Add('ns', iElement.NamespaceURI);
    exit('ns:');
  end
end;

procedure TXmlBuilder.WriteXml(iElement: TMXMLElement);
var
  n : string;
  a : TMXmlAttribute;
  c : TMXmlElement;
begin
  n := iElement.name;
  if n = '' then
    n := getNSAbbrev(iElement)+iElement.LocalName;
  for a in iElement.Attributes.Values do
    if a.NamespaceURI <> '' then
      AddAttributeNS(a.NamespaceURI, a.Name, a.Value)
    else
      AddAttribute(a.Name, a.Value);
  Open(n);
  for c in iElement.Children do
    case c.NodeType of
      ntElement: WriteXml(c);
      ntText: Text(c.Text);
      ntComment: Comment(c.Text);
      ntDocument: raise Exception.Create('Illegal XML - document inside element');
      ntAttribute: raise Exception.Create('Illegal XML - attribute in element chilren');
      ntProcessingInstruction: ProcessingInstruction(c.Name, c.Text);
      ntDocumentDeclaration: raise Exception.Create('Illegal DTD not supported');
      ntCData: raise Exception.Create('Illegal CDATA not supported');
    end;
  Close(n);
end;

end.
