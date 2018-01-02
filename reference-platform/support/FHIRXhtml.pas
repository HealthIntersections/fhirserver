unit FHIRXhtml;
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

interface

uses
  SysUtils, Classes, System.Character,
  StringSupport, AdvStringBuilders, TextUtilities,
  MXML, XmlBuilder,
  FHIRBase;

const
  XHTML_NS = 'http://www.w3.org/1999/xhtml';

Type
  TFHIRXhtmlParserOption = (xopTrimWhitspace, xopValidatorMode);
  TFHIRXhtmlParserOptions = set of TFHIRXhtmlParserOption;

  TFHIRXhtmlParser = class
  private
	  class Function checkNS(options: TFHIRXhtmlParserOptions; focus : TFhirXHtmlNode; node : TMXmlElement; defaultNS : String)  : String;
    class procedure doCompose(node: TFhirXHtmlNode; xml : TXmlBuilder);
    class function doParse(lang: String; policy: TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; node: TMXmlElement; path, defaultNS: String): TFhirXHtmlNode; static;
  public
    class Function parse(lang : String; policy : TFHIRXhtmlParserPolicy; options : TFHIRXhtmlParserOptions; node : TMXmlElement; path : String; defaultNS : String) : TFhirXHtmlNode; overload;
    class Function parse(lang : String; policy : TFHIRXhtmlParserPolicy; options : TFHIRXhtmlParserOptions; content : String) : TFhirXHtmlNode; Overload;

    class procedure compose(node: TFhirXHtmlNode; xml : TXmlBuilder); overload;
    class procedure compose(node: TFhirXHtmlNode; s : TAdvStringBuilder; canonicalise : boolean; indent : integer = 0; relativeReferenceAdjustment : integer = 0); overload;
    class function  compose(node: TFhirXHtmlNode; canonicalise : boolean = false) : String; overload;

    class Function attributeIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name, attr, value : String) : boolean;
    class Function elementIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name : String) : boolean;
  end;


implementation

{ TFHIRXhtmlParser }

class Function TFHIRXhtmlParser.parse(lang : String; policy : TFHIRXhtmlParserPolicy; options : TFHIRXhtmlParserOptions; content : String) : TFhirXHtmlNode;
var
  doc : TMXmlDocument;
begin
  doc := TMXmlParser.Parse(content, [xpResolveNamespaces]);
  try
    result := parse(lang, policy, options, doc.document, '', '');
  finally
    doc.Free;
  end;
end;

class function TFHIRXhtmlParser.parse(lang: String; policy: TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; node: TMXmlElement; path, defaultNS: String): TFhirXHtmlNode;
begin
  result := doParse(lang, policy, options, node, path, defaultNS);
  if (result.NsDecl = '') and not (xopValidatorMode in options) then
    result.Attributes.Add('xmlns', XHTML_NS);
end;

class function TFHIRXhtmlParser.doParse(lang: String; policy: TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; node: TMXmlElement; path, defaultNS: String): TFhirXHtmlNode;
var
  attr : TMXmlAttribute;
  n : String;
  child : TMXmlElement;
begin
  result := TFhirXHtmlNode.create(fhntElement);
  try
    result.Name := node.localName;
    defaultNS := checkNS(options, result, node, defaultNS);
    path := path + '/h:'+result.Name;
    for n in node.Attributes.Keys do
    begin
      attr := node.attributes[n];
      if not n.startsWith('xmlns') and attributeIsOk(policy, options, result.Name, n, attr.Value) then
        result.Attributes.add(n, attr.value);
    end;
    child := node.First;
    while (child <> nil) do
    begin
      if (child.NodeType = ntText) then
        result.addText(child.text)
      else if (child.NodeType = ntComment) then
        result.addComment(child.text)
      else if (child.NodeType = ntElement) then
      begin
        if (elementIsOk(policy, options, child.localName)) then
          result.ChildNodes.add(doParse(lang, policy, options, child as TMXmlElement, path, defaultNS));
      end
      else
        raise Exception.create('Unhandled XHTML feature: '+inttostr(ord(child.NodeType))+path);
      child := child.Next;
    end;
    result.link;
  finally
    result.free;
  end;
end;


class Function TFHIRXhtmlParser.elementIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name : String) : boolean;
begin
  if (xopValidatorMode in options) then
    exit(true);
	result := (policy = xppAllow) or StringArrayExistsInsensitive(['p', 'br', 'div', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'a', 'span', 'b', 'em', 'i', 'strong',
    'small', 'big', 'tt', 'small', 'dfn', 'q', 'var', 'abbr', 'acronym', 'cite', 'blockquote', 'hr', 'address', 'bdo', 'kbd', 'q', 'sub', 'sup',
    'ul', 'ol', 'li', 'dl', 'dt', 'dd', 'pre', 'table', 'caption', 'colgroup', 'col', 'thead', 'tr', 'tfoot', 'tbody', 'th', 'td',
    'code', 'samp', 'img', 'map', 'area'], name);
  if (not result) and (policy = xppReject) then
  	raise Exception.create('Illegal HTML element '+name);
end;


class Function TFHIRXhtmlParser.attributeIsOk(policy : TFHIRXhtmlParserPolicy; options: TFHIRXhtmlParserOptions; name, attr, value : String) : boolean;
begin
  if (xopValidatorMode in options) then
    exit(true);
  if (attr = 'xmlns') or (attr.StartsWith('xmlns:')) then
    exit(true);

	result := (policy = xppAllow) or
         StringArrayExistsInsensitive(['title', 'style', 'class', 'id', 'lang', 'xml:lang', 'dir', 'accesskey', 'tabindex',
                    // tables
                   'span', 'width', 'align', 'valign', 'char', 'charoff', 'abbr', 'axis', 'headers', 'scope', 'rowspan', 'colspan'], attr) or
         StringArrayExistsInsensitive(['a.href', 'a.name', 'img.src', 'img.border', 'div.xmlns', 'blockquote.cite', 'q.cite',
             'a.charset', 'a.type', 'a.name', 'a.href', 'a.hreflang', 'a.rel', 'a.rev', 'a.shape', 'a.coords', 'img.src',
             'img.alt', 'img.longdesc', 'img.height', 'img.width', 'img.usemap', 'img.ismap', 'map.name', 'area.shape',
             'area.coords', 'area.href', 'area.nohref', 'area.alt', 'table.summary', 'table.width', 'table.border',
             'table.frame', 'table.rules', 'table.cellspacing', 'table.cellpadding'], name+'.'+attr);

  if result and (name+'.'+attr = 'img.src') then
    result := (policy = xppAllow) or (StringStartsWith(value, '#') or StringStartsWith(value, 'data:') or StringStartsWith(value, 'http:') or StringStartsWith(value, 'https:'));

  if (not result) and (policy = xppReject) then
  	raise Exception.create('Illegal Attribute name '+name+'.'+attr);
end;

class Function TFHIRXhtmlParser.checkNS(options: TFHIRXhtmlParserOptions; focus : TFhirXHtmlNode; node : TMXmlElement; defaultNS : String)  : String;
var
  ns : String;
begin
  if not (xopValidatorMode in options) then
    exit('');

  ns := node.NamespaceURI;
  if (ns = '') then
   	exit('');
  if (ns <> defaultNS) then
  begin
	  focus.Attributes.add('xmlns', ns);
   	exit(ns);
	end;
  exit(defaultNS);
end;

class function TFHIRXhtmlParser.compose(node: TFhirXHtmlNode; canonicalise : boolean): String;
var
  b : TAdvStringBuilder;
begin
  b := TAdvStringBuilder.Create;
  try
    compose(node, b, canonicalise);
    result := b.AsString;
  finally
    b.Free;
  end;
end;

function isRelativeReference(s : string) : boolean;
begin
  if s.StartsWith('http') then
    result := false
  else if s.StartsWith('https') then
    result := false
  else if s.StartsWith('/') then
    result := false
  else
    result := true;
end;

function FixRelativeReference(s : string; indent : integer) : String;
var
  i : integer;
begin
  result := '';
  for i := 1 to indent do
    result := result + '../';
  result := result + s;
end;

function normaliseWhitespace(s : String) : String;
var
  w : boolean;
  b : TStringBuilder;
  c : Char;
begin
  w := false;
  b := TStringBuilder.Create;
  try
    for c in s do
    begin
      if not c.isWhitespace or (c = #$a0) then
      begin
        b.Append(c);
        w := false;
      end
      else if not w then
      begin
        b.append(' ');
        w := true;
      end;
      // else
        // ignore


    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;


class procedure TFHIRXhtmlParser.compose(node: TFhirXHtmlNode; s: TAdvStringBuilder; canonicalise : boolean; indent, relativeReferenceAdjustment: integer);
var
  i : Integer;
  function canonical(s: String) : String;
  begin
    if canonicalise then
      result := normaliseWhitespace(s)
    else
      result := s;
  end;
begin
  if node = nil then
    exit;
  case node.NodeType of
    fhntText : s.append(FormatTexttoXml(canonical(node.Content), xmlText));
    fhntComment : s.append('<!-- '+FormatTexttoXml(node.Content, xmlText)+' -->');
    fhntElement :
      begin
      s.append('<'+node.name);
      for i := 0 to node.Attributes.count - 1 do
        if (node.name = 'a') and (node.Attributes[i].Name = 'href') and isRelativeReference(node.Attributes[i].Value) then
          s.append(' '+node.Attributes[i].Name+'="'+FixRelativeReference(node.Attributes[i].Value, relativeReferenceAdjustment)+'"')
        else
          s.append(' '+node.Attributes[i].Name+'="'+FormatTexttoXml(node.Attributes[i].Value, xmlAttribute)+'"');
      if node.ChildNodes.Count > 0 then
      begin
        s.append('>');
        for i := 0 to node.ChildNodes.count - 1 do
          compose(node.ChildNodes[i], s, canonicalise, indent+2, relativeReferenceAdjustment);
        s.append('</'+node.name+'>');
      end
      else
        s.append('/>');
      end;
    fhntDocument:
      for i := 0 to node.ChildNodes.count - 1 do
        compose(node.ChildNodes[i], s, canonicalise, indent, relativeReferenceAdjustment);
  else
    raise exception.create('not supported');
  End;
end;

class procedure TFHIRXhtmlParser.docompose(node: TFhirXHtmlNode; xml: TXmlBuilder);
var
  i : Integer;
begin
  case node.NodeType of
    fhntText : xml.Text(normaliseWhitespace(node.Content));
    fhntComment : xml.Comment(node.Content);
    fhntElement :
      begin
      for i := 0 to node.Attributes.count - 1 do
        xml.AddAttribute(node.Attributes[i].Name, node.Attributes[i].Value);
      xml.Open(node.name);
      for i := 0 to node.ChildNodes.count - 1 do
        docompose(node.ChildNodes[i], xml);
      xml.Close(node.Name);
      end;
    fhntDocument:
      for i := 0 to node.ChildNodes.count - 1 do
        docompose(node.ChildNodes[i], xml);
  else
    raise exception.create('not supported');
  end;
end;

class procedure TFHIRXhtmlParser.compose(node: TFhirXHtmlNode; xml: TXmlBuilder);
begin
  if node = nil then
    exit;

  xml.NSPush;
  xml.CurrentNamespaces.DefaultNS := XHTML_NS;
  doCompose(node, xml);
  xml.NSPop;
end;

end.

