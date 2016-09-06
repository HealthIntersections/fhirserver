unit XmlTests;

{$DEFINE DIFF}

interface

Uses
  Windows, SysUtils, Classes, Soap.EncdDecd,
  StringSupport, GuidSupport, BytesSupport, EncodeSupport, TextUtilities, ShellApi,
  AdvObjects,
  MsXml, MsXmlParser, XmlPatch,
  DUnitX.TestFramework;

Type
  [TextFixture]
  TXmlTests = Class (TObject)
  Private
  Published
  End;

  XmlPatchTestCaseAttribute = class (CustomTestCaseSourceAttribute)
  protected
    function GetCaseInfoArray : TestCaseInfoArray; override;
  end;

  [TextFixture]
  TXmlPatchTests = Class (TObject)
  Private
    tests : IXMLDOMDocument2;
    engine : TXmlPatchEngine;
  Published
    [SetupFixture] procedure setup;
    [TearDownFixture] procedure teardown;

    [XmlPatchTestCase]
    procedure PatchTest(Name : String);
  End;

function CheckXMLIsSame(filename1, filename2 : String; var msg : string) : boolean;

var
  showdiff : boolean = true;

implementation

uses
  IdGlobalProtocols;

function PadString(const AStr: String; AWidth: Integer; APadChar: Char; APadLeft: Boolean): String;
var
  i: Integer;
begin
  if Length(AStr) >= AWidth then
    Result := AStr
  else
  begin
    SetLength(Result, AWidth - length(AStr));
    for i := 1 to length(result) - length(AStr) do
      result[i] := APadChar;
    if APadLeft then
      result := result+AStr
    else
      result := AStr+result;
  end;
end;


function MakeXmlPretty(ASrc: String): String;
var
  i: Integer;
  l: Integer;
  LLevelIsSimple: Boolean;
  b : TStringBuilder;
begin
  Result := '';
  b := TStringBuilder.Create;
  try
    l := -1;
    LLevelIsSimple := False;
    for i := 1 to length(ASrc) do
      begin
      if ASrc[i] = '<' then
        begin
        if (i < length(ASrc)) and (ASrc[i + 1] = '?') then
          begin
          b.Append(ASrc[i]);
          end
        else if (i < length(ASrc)) and (ASrc[i + 1] = '/') then
          begin
          if not LLevelIsSimple then
            begin
            b.append(#13#10 + PadString('', l * 2, ' ', False) + ASrc[i]);
            end
          else
            begin
            b.append(ASrc[i]);
            end;
          dec(l);
          LLevelIsSimple := False;
          end
        else if (i < length(ASrc)) and (ASrc[i + 1] = '!') then
        begin
          b.append(#13#10 + PadString('', l * 2, ' ', False) + ASrc[i])
        end
        else
          begin
          inc(l);
          LLevelIsSimple := True;
          if l <> 0 then
            begin
            b.append(#13#10 + PadString('', l * 2, ' ', False) + ASrc[i]);
            end
          else
            begin
            If (i = 1) Or ((i > 1) And ((ASrc[i - 1] = #13) Or (ASrc[i-1] =  #10))) Then
              begin
              b.append(ASrc[i]);
              end
            else
              begin
              b.append(#13#10 + ASrc[i]);
              end;
            end;
          end;
        end
      else
        begin
        If (ASrc[i] = '>') And ( (ASrc[i-1] = '/') Or (ASrc[i-1] = '?') Or (ASrc[i-1] = '-')) Then
          begin
          b.append(ASrc[i]);
          If (ASrc[i-1] = '?') or (ASrc[i-1] = '-') Then
            begin
            b.append(#13#10);
            end;
          if ASrc[i-1] = '/' then
            begin
            dec(l);
            LLevelIsSimple := False;
            end;
          end
        Else If Not ((ASrc[i] = #9) Or (ASrc[i] = #10) Or (ASrc[i] = #13)) Then
          begin
          if LLevelIsSimple or (ASrc[1] <> ' ') then
            begin
            b.append(ASrc[i]);
            end;
          end;
        end
      end;
    Result := b.ToString;
  finally
    b.free;
  end;
end;

function normalise(text : String) : String;
begin
  result := text.Trim.replace(#13, ' ').replace(#10, ' ').replace(#9, ' ');
  while result.Contains('  ') do
    result := result.Replace('  ', ' ');
end;

function unbase64(text : String) : TBytes;
begin
  result := DecodeBase64(AnsiString(text));
end;

function CompareAttributes(path : String; src, tgt : IXMLDOMNamedNodeMap) : string;
var
  i : integer;
  sa, ta : IXMLDOMNode;
  sn : String;
  b1, b2 : TBytes;
begin
  for i := 0 to src.length - 1 do
  begin
    sa := src.item[i];
    sn := sa.nodeName;
    if not ((sn = 'xmlns') or sn.StartsWith('xmlns:')) then
    begin
      ta := tgt.getNamedItem(sn);
      if (ta = nil) then
        exit('Attributes differ at '+path+': missing attribute '+sn);
      if normalise(sa.text) <> normalise(ta.text) then
      begin
        b1 := unBase64(sa.text);
        b2 := unBase64(ta.text);
        if not sameBytes(b1, b2) then
          exit('Attributes differ at '+path+': value '+normalise(sa.text) +'/'+ normalise(ta.text));
      end;
    end;
  end;
  result := '';
end;

function skipBlankText(node : IXMLDOMNode) : IXMLDOMNode;
begin
  while (node <> nil) and (((node.nodeType = NODE_TEXT) and StringIsWhitespace(node.text)) or (node.nodeType = NODE_COMMENT)) do
    node := node.nextSibling;
  result := node;
end;

function CompareElements(path : String; e1, e2 : IXMLDOMElement) : String;
var
  c1, c2 : IXMLDOMNode;
  s : String;
begin
  if e1.namespaceURI <> e2.namespaceURI then
    exit('Namespaces differ at '+path+': '+e1.namespaceURI+'/'+e2.namespaceURI);
  if e1.baseName <> e2.baseName then
    exit('Names differ at '+path+': '+e1.baseName+'/'+e2.baseName);
  path := path + '/'+e1.baseName;
  s := compareAttributes(path, e1.attributes, e2.attributes);
  if (s <> '') then
    exit(s);
  s := compareAttributes(path, e2.attributes, e1.attributes);
  if (s <> '') then
    exit(s);

  c1 := e1.firstChild;
  c2 := e2.firstChild;
  c1 := skipBlankText(c1);
  c2 := skipBlankText(c2);
  while (c1 <> nil) and (c2 <> nil) do
  begin
    if (c1.nodeType <> c2.nodeType) then
      exit('node type mismatch in children of '+path+': '+inttostr(e1.nodeType)+'/'+inttostr(e2.nodeType));
    if (c1.nodeType = NODE_TEXT) then
    begin
      if normalise(c1.text) <> normalise(c2.text) then
        exit('Text differs at '+path+': '+normalise(c1.text) +'/'+ normalise(c2.text));
    end
    else if (c1.nodeType = NODE_ELEMENT) then
    begin
      s := CompareElements(path, c1 as IXMLDOMElement, c2 as IXMLDOMElement);
      if (s <> '') then
        exit(s);
    end;

    c1 := skipBlankText(c1.nextSibling);
    c2 := skipBlankText(c2.nextSibling);
  end;
  if (c1 <> nil) then
    exit('node mismatch - more nodes in source in children of '+path);
  if (c2 <> nil) then
    exit('node mismatch - more nodes in target in children of '+path);
  result := '';
end;

function CompareXml(filename1, filename2 : String; var msg : string) : boolean;
begin
  msg := CompareElements('', TMsXmlParser.Parse(filename1).documentElement, TMsXmlParser.Parse(filename2).documentElement);
  result := msg = '';
end;

function CheckXMLIsSame(filename1, filename2 : String; var msg : string) : boolean;
{$IFDEF DIFF}
var
  x1, x2, f1, f2, cmd : String;
{$ENDIF}
begin
  result := compareXML(filename1, filename2, msg);
{$IFDEF DIFF}
  if not result and showdiff then
  begin
    showdiff := false;
    x1 := FileToString(filename1 {$IFDEF UNICODE}, TEncoding.UTF8 {$ENDIF});
    x2 := FileToString(filename2 {$IFDEF UNICODE}, TEncoding.UTF8 {$ENDIF});
    x1 := MakeXmlPretty(x1);
    x2 := MakeXmlPretty(x2);
    x1 := x1.Replace('&#39;', '''').Replace('&quot;', '"');
    x2 := x2.Replace('&#39;', '''').Replace('&quot;', '"');

    f1 := MakeTempFilename +'-source.xml';
    f2 := MakeTempFilename +'-dest.xml';
    StringToFile(x1, f1{$IFDEF UNICODE}, TEncoding.UTF8{$ENDIF});
    StringToFile(x2, f2{$IFDEF UNICODE}, TEncoding.UTF8{$ENDIF});
    cmd := f1+' '+f2;
    ShellExecute(0, 'open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar(cmd), PChar(ExtractFilePath(f1)), SW_MAXIMIZE);
  end;
{$ENDIF}
end;


{ TXmlTests }


{ XmlPatchTestCaseAttribute }

function XmlPatchTestCaseAttribute.GetCaseInfoArray: TestCaseInfoArray;
var
  tests : IXMLDOMDocument2;
  test : IXMLDOMElement;
  i : integer;
  s : String;
begin
  tests := TMsXmlParser.Parse('C:\work\fhirserver\tests\xml-patch-tests.xml');
  test := TMsXmlParser.FirstChild(tests.documentElement);
  i := 0;
  while test <> nil do
  begin
    if test.nodeName = 'case' then
    begin
      s := test.getAttribute('name');
      SetLength(result, i+1);
      result[i].Name := s;
      SetLength(result[i].Values, 1);
      result[i].Values[0] := s;
      inc(i);
    end;
    test := TMsXmlParser.NextSibling(test);
  end;
end;

{ TXmlPatchTests }

procedure TXmlPatchTests.PatchTest(Name: String);
var
  test, target, patch, error, patched, outcome : IXMLDOMElement;
  s : String;
  ok : boolean;
begin
  test := TMsXmlParser.FirstChild(tests.documentElement);
  while test <> nil do
  begin
    if (test.nodeName = 'case') and (name = test.getAttribute('name')) then
    begin
      target := TMsXmlParser.NamedChild(test, 'target');
      patch := TMsXmlParser.NamedChild(test, 'patch');
      error := TMsXmlParser.NamedChild(test, 'error');
      patched := TMsXmlParser.NamedChild(test, 'patched');
      if patched <> nil then
        patched := TMsXmlParser.FirstChild(patched);

      if (error <> nil) then
        Assert.WillRaiseWithMessage(
          procedure begin
            engine.execute(tests, target, patch);
          end, Exception, error.text)
      else
      begin
        engine.execute(tests, target, patch);
        StringToFile(TMsXmlParser.FirstChild(target).xml, 'c:\temp\outcome.xml', TEncoding.UTF8);
        StringToFile(patched.xml, 'c:\temp\patched.xml', TEncoding.UTF8);
        ok := CheckXMLIsSame('c:\temp\patched.xml', 'c:\temp\outcome.xml', s);
        Assert.IsTrue(ok, s);
      end;
    end;
    test := TMsXmlParser.NextSibling(test);
  end;
end;

procedure TXmlPatchTests.setup;
begin
  tests := TMsXmlParser.Parse('C:\work\fhirserver\tests\xml-patch-tests.xml');
  engine := TXmlPatchEngine.Create;
end;

procedure TXmlPatchTests.teardown;
begin
  engine.Free;
end;

initialization
  TDUnitX.RegisterTestFixture(TXmlTests);
  TDUnitX.RegisterTestFixture(TXmlPatchTests);
end.
