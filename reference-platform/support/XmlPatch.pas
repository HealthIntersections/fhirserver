unit XmlPatch;

interface

uses
  SysUtils,
  AdvObjects, StringSupport,
  MsXml, MsXmlParser;

type
  TXmlPatchEngine = class (TAdvObject)
  private
    class procedure remove(sel : String; targetRoot : IXMLDOMNode);
    class procedure add(doc : IXMLDOMDocument2; op : IXMLDOMElement; targetRoot : IXMLDOMNode);
    class procedure replace(doc : IXMLDOMDocument2; op : IXMLDOMElement; targetRoot : IXMLDOMNode);
    class procedure addChildNodes(doc : IXMLDOMDocument2; source, target : IXMLDomNode; pos : String);
  public
    class procedure execute(doc : IXMLDOMDocument2; targetRoot : IXMLDOMNode; patch : IXMLDOMElement);
  end;

implementation

{ TXmlPatchEngine }

class procedure TXmlPatchEngine.execute(doc : IXMLDOMDocument2; targetRoot : IXMLDOMNode; patch: IXMLDOMElement);
begin
  if doc = nil then
    raise Exception.Create('No Target Document Root Found');
  if targetRoot = nil then
    raise Exception.Create('No Target Document Found');
  if patch = nil then
    raise Exception.Create('No Patch Operations Found');
  patch := TMsXmlParser.FirstChild(patch);
  if patch = nil then
    raise Exception.Create('No Patch Operations Found');

  doc.setProperty('SelectionNamespaces', 'xmlns:f="http://hl7.org/fhir" xmlns:h="http://www.w3.org/1999/xhtml"');

  while (patch <> nil) do
  begin
    if (patch.baseName = 'remove') then
      remove(patch.getAttribute('sel'), targetRoot)
    else if (patch.baseName = 'add') then
      add(doc, patch, targetRoot)
    else if (patch.baseName = 'replace') then
      replace(doc, patch, targetRoot)
    else
      raise Exception.Create('Unknown Patch Operation "'+patch.baseName+'"');
    patch := TMsXmlParser.NextSibling(patch);
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

class procedure TXmlPatchEngine.add(doc : IXMLDOMDocument2; op : IXMLDOMElement; targetRoot : IXMLDOMNode);
var
  matches : IXMLDOMNodeList;
  elem : IXMLDOMElement;
  sel, typ, pos : String;
begin
  sel := op.getAttribute('sel');
  if op.getAttributeNode('type') <> nil then
    typ := op.getAttribute('type');
  if op.getAttributeNode('pos') <> nil then
    pos := op.getAttribute('pos');

  matches := targetRoot.selectNodes(sel);
  if matches.length = 0 then
    raise Exception.Create('No match found for '+sel+' performing addition');
  if matches.length > 1 then
    raise Exception.Create('The xpath '+sel+' matched multiple nodes performing addition');

  if typ = '' then
    addChildNodes(doc, op, matches.item[0], pos)
  else if typ.StartsWith('@') then
  begin
    elem := matches.item[0] as IXmlDomElement;
    elem.setAttribute(typ.Substring(1), op.text);
  end
  else if typ.StartsWith('namespace::') then
  begin
    elem := matches.item[0] as IXmlDomElement;
    elem.setAttribute('xmlns:'+typ.Substring(11), op.text);
  end
  else
    raise Exception.Create('Unknown value for type: '+typ);
end;

class procedure TXmlPatchEngine.replace(doc : IXMLDOMDocument2; op : IXMLDOMElement; targetRoot : IXMLDOMNode);
var
  matches : IXMLDOMNodeList;
  n, ce, elem : IXMLDOMElement;
  sel : String;
  i : integer;
begin
  sel := op.getAttribute('sel');

  matches := targetRoot.selectNodes(sel);
  if matches.length = 0 then
    raise Exception.Create('No match found for '+sel+' performing replace');
  if matches.length > 1 then
    raise Exception.Create('The xpath '+sel+' matched multiple nodes performing replace');

  case matches.item[0].nodeType of
    NODE_ELEMENT :
      begin
        n := TMsXmlParser.FirstChild(op);
        ce := doc.createNode(NODE_ELEMENT, n.baseName, n.namespaceURI) as IXMLDOMElement;
        for i := 0 to n.attributes.length - 1 do
          ce.setAttribute(n.attributes.item[i].nodeName, n.attributes.item[i].nodeValue);
        addChildNodes(doc, n, ce, '');
        matches.item[0].parentNode.replaceChild(ce, matches.item[0]);
      end;
    NODE_TEXT,
    NODE_COMMENT,
    NODE_ATTRIBUTE : matches.item[0].text := op.text;
  else
    raise Exception.Create('Unsupported Node Type for replace');
  end;
end;

class procedure TXmlPatchEngine.remove(sel: String; targetRoot: IXMLDOMNode);
var
  matches : IXMLDOMNodeList;
  elem : IXMLDOMElement;
  attrName : String;
begin
  checkEndsWithAttribute(sel, attrName);

  matches := targetRoot.selectNodes(sel);
  if matches.length = 0 then
    raise Exception.Create('Nothing to delete found for xpath '+sel);
  if matches.length > 1 then
    raise Exception.Create('The xpath '+sel+' matched multiple nodes');
  if attrName <> '' then
  begin
    elem := matches.item[0] as IXmlDomElement;
    elem.removeAttribute(attrName)
  end
  else
    matches.item[0].parentNode.removeChild(matches.item[0]);
end;

class procedure TXmlPatchEngine.addChildNodes(doc : IXMLDOMDocument2; source, target: IXMLDomNode; pos : String);
var
  n, c : IXmlDomNode;
  ce, elem : IXMLDOMElement;
  i : integer;
begin
  n := source.firstChild;
  while n <> nil do
  begin
    case n.nodeType of
      NODE_ELEMENT :
        begin
        ce := doc.createNode(NODE_ELEMENT, n.baseName, n.namespaceURI) as IXMLDOMElement;
        elem := (n as IXMLDOMElement);
        for i := 0 to elem.attributes.length - 1 do
          ce.setAttribute(elem.attributes.item[i].nodeName, elem.attributes.item[i].nodeValue);
        addChildNodes(doc, n, ce, '');
        c := ce;
        end;
      NODE_TEXT :
        c := doc.createTextNode(n.text);
      NODE_COMMENT :
        c := doc.createComment(n.text);
    else
      raise Exception.Create('Node type not supported '+inttostr(n.nodeType));
    end;
    if pos = '' then
      target.appendChild(c)
    else if (pos = 'before') then
      target.parentNode.insertBefore(c, target)
    else
      raise Exception.Create('Pos "'+pos+'" not supported');
    n := n.nextSibling;
  end;
end;

end.
