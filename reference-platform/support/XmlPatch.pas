unit XmlPatch;

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
  SysUtils,
  StringSupport, AdvObjects, AdvGenerics,
  MXML;

type
  TXmlPatchEngine = class (TAdvObject)
  private
    class procedure remove(doc : TMXmlDocument; sel : String; target : TMXmlElement);
    class procedure add(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
    class procedure replace(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
    class procedure addChildNodes(doc : TMXmlDocument; source, target : TMXmlElement; pos : String);
  public
    class procedure execute(doc : TMXmlDocument; target : TMXmlElement; patch : TMXmlElement);
  end;

implementation

{ TXmlPatchEngine }

class procedure TXmlPatchEngine.execute(doc : TMXmlDocument; target : TMXmlElement; patch: TMXmlElement);
begin
  if doc = nil then
    raise Exception.Create('No Target Document Root Found');
  if target = nil then
    raise Exception.Create('No Target Element Found');
  if patch = nil then
    raise Exception.Create('No Patch Operations Found');
  patch := patch.firstElement;
  if patch = nil then
    raise Exception.Create('No Patch Operations Found');

  doc.NamespaceAbbreviations.AddOrSetValue('f', 'http://hl7.org/fhir');
  doc.NamespaceAbbreviations.AddOrSetValue('h', 'http://www.w3.org/1999/xhtml');

  while (patch <> nil) do
  begin
    if (patch.localName = 'remove') then
      remove(doc, patch.attribute['sel'], target)
    else if (patch.localName = 'add') then
      add(doc, patch, target)
    else if (patch.localName = 'replace') then
      replace(doc, patch, target)
    else
      raise Exception.Create('Unknown Patch Operation "'+patch.localName+'"');
    patch := patch.nextElement;
  end;
end;

procedure checkEndsWithAttribute(var path, attrName : String);
var
  l, r : String;
begin
  StringSplitRight(path, '/', l, r);
  if (r.StartsWith('@')) then
  begin
    path := l;
    attrName := r.Substring(1);
  end
  else
    attrName := '';
end;

class procedure TXmlPatchEngine.add(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
var
  matches : TAdvList<TMXmlNode>;
  elem : TMXmlElement;
  sel, typ, pos : String;
begin
  sel := op.attribute['sel'];
  typ := op.attribute['type'];
  pos := op.attribute['pos'];

  matches := doc.select(sel, target);
  try
    if matches.count = 0 then
      raise Exception.Create('No match found for '+sel+' performing addition');
    if matches.count > 1 then
      raise Exception.Create('The xpath '+sel+' matched multiple nodes performing addition');

    if typ = '' then
      addChildNodes(doc, op, matches[0] as TMXmlElement, pos)
    else if typ.StartsWith('@') then
    begin
      elem := matches[0] as TMXmlElement;
      elem.attribute[typ.Substring(1)] := op.text;
    end
    else if typ.StartsWith('namespace::') then
    begin
      elem := matches[0] as TMXmlElement;
      elem.attribute['xmlns:'+typ.Substring(11)] := op.text;
    end
    else
      raise Exception.Create('Unknown value for type: '+typ);
  finally
    matches.Free;
  end;
end;

class procedure TXmlPatchEngine.replace(doc : TMXmlDocument; op : TMXmlElement; target : TMXmlElement);
var
  matches : TAdvList<TMXmlNode>;
  n, ce : TMXmlElement;
  sel : String;
begin
  sel := op.attribute['sel'];

  matches := doc.select(sel, target);
  try
    if matches.count = 0 then
      raise Exception.Create('No match found for '+sel+' performing replace');
    if matches.count > 1 then
      raise Exception.Create('The xpath '+sel+' matched multiple nodes performing replace');

    case TMXmlNamedNode(matches[0]).nodeType of
      ntElement :
        begin
          n := op.first;
          ce := TMXmlElement.CreateNSN(ntElement, TMXmlElement(n).Name, n.namespaceURI, n.localName);
          try
            ce.Attributes.addAll(n.Attributes);
            addChildNodes(doc, n, ce, '');
            TMXmlElement(matches[0]).parent.Children.replace(TMXmlElement(matches[0]), ce.Link);
            ce.Parent := matches[0].parent;
          finally
            ce.Free;
          end;
        end;
      ntText,
      ntComment : TMXmlElement(matches[0]).text := op.text;
      ntAttribute : TMXmlAttribute(matches[0]).value := op.text;
    else
      raise Exception.Create('Unsupported Node Type for replace');
    end;
  finally
    matches.Free;
  end;
end;

class procedure TXmlPatchEngine.remove(doc : TMXmlDocument; sel: String; target: TMXmlElement);
var
  matches : TAdvList<TMXmlNode>;
  elem : TMXmlElement;
  attrName : String;
begin
  checkEndsWithAttribute(sel, attrName);

  matches := doc.select(sel, target);
  try
    if matches.count = 0 then
      raise Exception.Create('Nothing to delete found for xpath '+sel);
    if matches.count > 1 then
      raise Exception.Create('The xpath '+sel+' matched multiple nodes');

    if attrName <> '' then
    begin
      elem := matches[0] as TMXmlElement;
      elem.Attributes.Remove(attrName)
    end
    else
      matches[0].parent.Children.remove(matches[0] as TMXmlElement);
  finally
    matches.Free;
  end;
end;

class procedure TXmlPatchEngine.addChildNodes(doc : TMXmlDocument; source, target: TMXmlElement; pos : String);
var
  n, c : TMXmlElement;
  ce, elem : TMXmlElement;
begin
  n := source.first;
  while n <> nil do
  begin
    case n.nodeType of
      ntElement :
        begin
        ce := TMXmlElement.createNSN(ntElement, n.Name, n.namespaceURI, n.localName);
        try
          elem := (n as TMXmlElement);
          ce.Attributes.addAll(elem.attributes);
          addChildNodes(doc, n, ce, '');
          c := ce.Link;
        finally
          ce.Free;
        end;
        end;
      ntText :
        c := TMXmlElement.createText(n.text);
      ntComment :
        c := TMXmlElement.createComment(n.text);
    else
      raise Exception.Create('Node type not supported '+inttostr(ord(n.nodeType)));
    end;
    if pos = '' then
    begin
      target.Children.add(c);
      c.Parent := target;
    end
    else if (pos = 'before') then
    begin
      target.parent.Children.insert(target.parent.Children.IndexOf(target), c);
      c.Parent := target.Parent;
    end
    else
      raise Exception.Create('Pos "'+pos+'" not supported');
    n := n.next;
  end;
end;

end.
