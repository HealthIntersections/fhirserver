unit fsl_comparisons;

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

{$I fhir.inc}

interface

Uses
  SysUtils,
  IdGlobalProtocols,
  fsl_utilities, fsl_xml, fsl_json, fsl_turtle, fsl_stream
  {$IFDEF WINDOWS}, fsl_shell{$ENDIF};

function CheckXMLIsSame(filename1, filename2 : String; var msg : string) : boolean;
function CheckJsonIsSame(filename1, filename2 : String; var msg : string) : boolean;
function CheckTurtleIsSame(src1, src2 : String; var msg : string) : boolean;
function CheckTextIsSame(src1, src2 : String; var msg : string) : boolean;

var
  showdiff : boolean = true;

implementation

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

function CompareAttributes(path : String; src, tgt : TMXmlElement) : string;
var
//  i : integer;
//  sa, ta : IXMLDOMNode;
//  sn : String;
  b1, b2 : TBytes;
  a, b : TMXmlAttribute;
begin
  result := '';
  for a in src.Attributes do
  begin
    if not ((a.name = 'xmlns') or a.name.StartsWith('xmlns:')) then
    begin
      if not tgt.getAttrByName(a.name, b) then
        exit('Attributes differ at '+path+': missing attribute '+a.name);
      if normalise(a.value) <> normalise(b.value) then
      begin
        b1 := unBase64(a.value);
        b2 := unBase64(b.value);
        if not sameBytes(b1, b2) then
          exit('Attributes differ at '+path+': value '+normalise(a.value) +'/'+ normalise(b.value));
      end;
    end;
  end;
  for a in tgt.Attributes do
  begin
    if not ((a.name = 'xmlns') or a.name.StartsWith('xmlns:')) then
    begin
      if not src.getAttrByName(a.name, b) then
        exit('Attributes differ at '+path+': missing attribute '+a.name);
      if normalise(a.value) <> normalise(b.value) then
      begin
        b1 := unBase64(a.value);
        b2 := unBase64(b.value);
        if not sameBytes(b1, b2) then
          exit('Attributes differ at '+path+': value '+normalise(a.value) +'/'+ normalise(b.value));
      end;
    end;
  end;
end;

function skipBlankText(node : TMXmlElement) : TMXmlElement;
begin
  while (node <> nil) and (((node.nodeType = ntText) and StringIsWhitespace(node.text)) or (node.nodeType = ntComment)) do
    node := node.next;
  result := node;
end;

function CompareElements(path : String; e1, e2 : TMXmlElement) : String;
var
  c1, c2 : TMXmlElement;
  s : String;
begin
  if e1.namespaceURI <> e2.namespaceURI then
    exit('Namespaces differ at '+path+': '+e1.namespaceURI+'/'+e2.namespaceURI);
  if e1.localName <> e2.localName then
    exit('Names differ at '+path+': '+e1.localName+'/'+e2.localName);
  path := path + '/'+e1.localName;
  s := compareAttributes(path, e1, e2);
  if (s <> '') then
    exit(s);

  c1 := e1.first;
  c2 := e2.first;
  c1 := skipBlankText(c1);
  c2 := skipBlankText(c2);
  while (c1 <> nil) and (c2 <> nil) do
  begin
    if (c1.nodeType <> c2.nodeType) then
      exit('node type mismatch in children of '+path+': '+inttostr(ord(e1.nodeType))+'/'+inttostr(ord(e2.nodeType)));
    if (c1.nodeType = ntText) then
    begin
      if normalise(c1.text) <> normalise(c2.text) then
        exit('Text differs at '+path+': '+normalise(c1.text) +'/'+ normalise(c2.text));
    end
    else if (c1.nodeType = ntElement) then
    begin
      s := CompareElements(path, c1, c2);
      if (s <> '') then
        exit(s);
    end;

    c1 := skipBlankText(c1.next);
    c2 := skipBlankText(c2.next);
  end;
  if (c1 <> nil) then
    exit('node mismatch - more nodes in source in children of '+path);
  if (c2 <> nil) then
    exit('node mismatch - more nodes in target in children of '+path);
  result := '';
end;

function CompareXml(filename1, filename2 : String; var msg : string) : boolean;
var
  src, tgt : TMXmlDocument;
begin
  src := TMXmlParser.ParseFile(filename1, [xpResolveNamespaces]);
  try
    tgt := TMXmlParser.ParseFile(filename2, [xpResolveNamespaces]);
    try
      msg := CompareElements('', src.document, tgt.document);
      result := msg = '';
    finally
      tgt.Free;
    end;
  finally
    src.free;
  end
end;

function CheckXMLIsSame(filename1, filename2 : String; var msg : string) : boolean;
var
  x1, x2, f1, f2, cmd : String;
begin
  result := compareXML(filename1, filename2, msg);
  if not result and showdiff then
  begin
    showdiff := false;
    x1 := FileToString(filename1, TEncoding.UTF8);
    x2 := FileToString(filename2, TEncoding.UTF8);
    x1 := MakeXmlPretty(x1);
    x2 := MakeXmlPretty(x2);
    x1 := x1.Replace('&#39;', '''').Replace('&quot;', '"');
    x2 := x2.Replace('&#39;', '''').Replace('&quot;', '"');

    f1 := MakeTempFilename +'-source.xml';
    f2 := MakeTempFilename +'-dest.xml';
    StringToFile(x1, f1, TEncoding.UTF8);
    StringToFile(x2, f2, TEncoding.UTF8);
    cmd := f1+' '+f2;
    {$IFDEF WINDOWS}
    ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', cmd, true);
    {$ELSE}
    raise Exception.create('to do');
    {$ENDIF}
  end;
end;

function CompareObjectsJson(path : String; o1, o2 : TJsonObject) : String; forward;

function CompareNodes(path : String; n1, n2 : TJsonNode) : String;
var
  s, s1, s2 : String;
  i : integer;
begin
  if n1.ClassName <> n2.ClassName then
    exit('properties differ at '+path+': type '+n1.ClassName+'/'+n2.ClassName)
  else if (n1 is TJsonBoolean) then
  begin
    if ((n1 as TJsonBoolean).value <> (n2 as TJsonBoolean).value) then
      exit('boolean property values differ at '+path+': type '+booltoStr((n1 as TJsonBoolean).value)+'/'+boolToStr((n2 as TJsonBoolean).value))
  end
  else if (n1 is TJsonString) then
  begin
    s1 := (n1 as TJsonString).value;
    s2 := (n2 as TJsonString).value;
    if not (s1.contains('<div') and s2.contains('<div'))  then
      if s1 <> s2 then
        if not SameBytes(unbase64(s1), unbase64(s2)) then
          exit('string property values differ at '+path+': type '+s1+'/'+s2)
  end
  else if (n1 is TJsonNumber) then
  begin
    if ((n1 as TJsonNumber).value <> (n2 as TJsonNumber).value) then
      exit('number property values differ at '+path+': type '+(n1 as TJsonNumber).value+'/'+(n2 as TJsonNumber).value)
  end
  else if (n1 is TJsonObject) then
  begin
    s := CompareObjectsJson(path, (n1 as TJsonObject), (n2 as TJsonObject));
    if s <> '' then
      exit(s)
  end
  else if (n1 is TJsonArray) then
  begin
    if ((n1 as TJsonArray).Count <> (n2 as TJsonArray).Count) then
      exit('array properties differ at '+path+': count '+inttostr((n1 as TJsonArray).Count)+'/'+inttostr((n2 as TJsonArray).Count))
    else
      for I := 0 to (n1 as TJsonArray).Count - 1 do
      begin
        s := compareNodes(path+'['+inttostr(i)+']', (n1 as TJsonArray).Item[i], (n2 as TJsonArray).Item[i]);
        if s <> '' then
          exit(s)
      end;
  end
  else if (n1 is TJsonNull) then
  begin
    // nothing to compare
  end
  else
    exit('unhandled property '+n1.className);
end;

function CompareObjectsJson(path : String; o1, o2 : TJsonObject) : String;
var
  n : String;
  n1: TJsonNode;
  s : string;
begin
  for n in o1.properties.Keys do
    if (n <> 'fhir_comments') then
    begin
      n1 := o1.properties[n];
      if o2.properties.ContainsKey(n) then
      begin
        s := compareNodes(path+'.'+n, n1, o2.properties[n]);
        if (s <> '') then
          exit(s);
      end
      else
        exit('properties differ at '+path+': missing property '+n);
    end;
  for n in o2.properties.Keys do
    if (n <> 'fhir_comments') then
      if not o1.properties.ContainsKey(n) then
        exit('properties differ at '+path+': missing property '+n);
end;

function CompareJson(filename1, filename2 : String; var msg : string) : boolean;
var
  j1, j2 : TJsonObject;
begin
  j1 := TJSONParser.ParseFile(filename1);
  try
    j2 := TJSONParser.ParseFile(filename2);
    try
      msg := CompareObjectsJson('$', j1, j2);
      result := msg = '';
    finally
      j2.free;
    end;
  finally
    j1.Free;
  end;
end;

function CheckJsonIsSame(filename1, filename2 : String; var msg : string) : boolean;
var
  j1, j2 : TJsonObject;
  f1, f2, cmd : String;
begin
  result := compareJson(filename1, filename2, msg);
  if not result and showdiff then
  begin
    showdiff := false;
    j1 := TJSONParser.ParseFile(filename1);
    j2 := TJSONParser.ParseFile(filename2);
    try

      f1 := MakeTempFilename +'-source.xml';
      f2 := MakeTempFilename +'-dest.xml';
      StringToFile(TJsonWriter.writeObjectStr(j1, true), f1, TEncoding.UTF8);
      StringToFile(TJsonWriter.writeObjectStr(j2, true), f2, TEncoding.UTF8);
      cmd := f1+' '+f2;
      {$IFDEF WINDOWS}
      ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar(cmd), true);
      {$ELSE}
      raise Exception.create('to do');
      {$ENDIF}
    finally
      j2.Free;
      j1.Free;
    end;
  end;
end;

function compareObjectsTurtle(path : String; t1, t2 : TTurtleObject) : String;
var
  c1, c2 : TTurtleComplex;
  l1, l2 : TTurtleList;
  o1, o2 : TTurtleObject;
  key, s : String;
  i : integer;
begin
  result := '';
  if (t1.ClassName <> t2.ClassName) then
    result := 'Objects at '+path+' have different types ("'+t1.ClassName+'"/"'+t2.ClassName+'")'
  else if t1 is TTurtleLiteral then
  begin
    if TTurtleLiteral(t1).value <> TTurtleLiteral(t2).value then
      result := 'Objects at '+path+' have different values ("'+TTurtleLiteral(t1).value+'"/"'+TTurtleLiteral(t2).value+'")'
    else if TTurtleLiteral(t1).type_ <> TTurtleLiteral(t2).type_ then
      result := 'Objects at '+path+' have different types ("'+TTurtleLiteral(t1).type_+'"/"'+TTurtleLiteral(t2).type_+'")'
  end
  else if t1 is TTurtleURL then
  begin
    if TTurtleURL(t1).uri <> TTurtleURL(t2).uri then
      result := 'Objects at '+path+' have different uris ("'+TTurtleURL(t1).uri+'"/"'+TTurtleURL(t2).uri+'")'
  end
  else if t1 is TTurtleComplex then
  begin
    c1 := TTurtleComplex(t1);
    c2 := TTurtleComplex(t2);
    if c1.predicates.Count <> c2.predicates.Count then
      result := 'Objects at '+path+' have different property counts ("'+inttostr(c1.predicates.Count)+'"/"'+inttostr(c2.predicates.Count)+'")'
    else
    begin
      for key in c1.predicates.Keys do
      begin
        o1 := c1.predicates[key];
        if not c2.predicates.TryGetValue(key, o2) then
          exit('Object at '+path+' has no property for "'+key+'" ("'+c1.predicates.SortedKeys.CommaText+'" vs "'+c2.predicates.SortedKeys.CommaText+'")')
        else
        begin
          s := compareObjectsTurtle(path+' / '+key, o1, o2);
          if s <> '' then
            exit(s);
        end;
      end;
    end;
  end
  else if t1 is TTurtleList then
  begin
    l1 := TTurtleList(t1);
    l2 := TTurtleList(t2);
    if l1.List.Count <> l2.list.Count then
      result := 'Objects at '+path+' have different property counts ("'+inttostr(l1.list.Count)+'"/"'+inttostr(l2.list.Count)+'")'
    else
    begin
      for i := 0 to l1.List.count - 1 do
      begin
        s := compareObjectsTurtle(path+' # '+inttostr(i), l1.List[i], l2.List[i]);
        if s <> '' then
          exit(s);
      end;
    end;
  end
end;

function compareTurtle(t1, t2 : TTurtleDocument; var msg : string) : boolean;
var
  i : integer;
  p1, p2 : TTurtlePredicate;
begin
  if t1.objects.Count <> t2.objects.Count then
    msg := 'Object Counts differ ('+inttostr(t1.objects.Count)+'/'+inttostr(t2.objects.Count)+')'
  else
  begin
    for i := 0 to t1.objects.Count - 1 do
    begin
      p1 := t1.objects[i];
      p2 := t2.objects[i];
      if (p1.URL.uri <> p2.URL.uri) then
        msg := 'URL mismatch: "'+p1.URL.uri+'"/"'+p2.URL.uri+'"'
      else
        msg := compareObjectsTurtle(p1.URL.uri, p1.Value, p2.Value);
    end;
  end;
  result := msg = '';
end;

function CheckTurtleIsSame(src1, src2 : String; var msg : string) : boolean;
var
  t1, t2 : TTurtleDocument;
  f1, f2, cmd : String;
begin
  result := false;
  try
    t1 := TTurtleParser.parse(src1);
    try
      t2 := TTurtleParser.parse(src2);
      try
        result := compareTurtle(t1, t2, msg);
        if not result then
        begin
          f1 := MakeTempFilename +'-source.xml';
          f2 := MakeTempFilename +'-dest.xml';
      StringToFile(src1, f1, TEncoding.UTF8);
      StringToFile(src2, f2, TEncoding.UTF8);

//          TRDFGenerator.saveToFile(t1, f1);
//          TRDFGenerator.saveToFile(t2, f2);
          cmd := f1+' '+f2;
          {$IFDEF WINDOWS}
          ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar(cmd), true);
          {$ELSE}
          raise Exception.create('to do');
          {$ENDIF}
        end;
      finally
        t2.free;
      end;
    finally
      t1.free;
    end;
  except
    on e : Exception do
    begin
      msg := e.Message;
      f1 := MakeTempFilename +'-source.xml';
      f2 := MakeTempFilename +'-dest.xml';
      StringToFile(src1, f1, TEncoding.UTF8);
      StringToFile(src2, f2, TEncoding.UTF8);
      cmd := f1+' '+f2;
      {$IFDEF WINDOWS}
      ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar(cmd), true);
      {$ELSE}
      raise Exception.create('to do');
      {$ENDIF}
    end;
  end;
end;

function compareText(s1, s2 : string; var msg : string) : boolean;
var
  i : integer;
begin
  msg := '';
  for i := 1 to IntegerMin(s1.length, s2.length) do
  begin
    if (s1[i] <> s2[i]) then
    begin
      msg := 'Strings differ at character '+intToStr(i)+': "'+s1[i] +'" vs "'+s2[i]+'"';
      exit(false);
    end;
  end;
  if (s1.length <> s2.length) then
    msg := 'Strings differ in length: '+inttostr(s1.length)+' vs '+inttostr(s2.length)+' but match to the end of the shortest';
  result := msg = '';
end;

function CheckTextIsSame(src1, src2 : String; var msg : string) : boolean;
var
  f1, f2, cmd : String;
begin
  result := compareText(src1, src2, msg);
  if not result then
  begin
    f1 := MakeTempFilename +'-source.txt';
    f2 := MakeTempFilename +'-dest.txt';
    StringToFile(src1, f1, TEncoding.UTF8);
    StringToFile(src2, f2, TEncoding.UTF8);
    cmd := f1+' '+f2;
    {$IFDEF WINDOWS}
    ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar(cmd), true);
    {$ELSE}
    raise Exception.create('to do');
    {$ENDIF}
  end;
end;

end.
