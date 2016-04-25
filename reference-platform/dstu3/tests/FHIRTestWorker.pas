unit FHIRTestWorker;

{.$.DEFINE DIFF}

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

{$IFNDEF FHIR3}
This is the dstu3 version of the FHIR code
{$ENDIF}


interface

uses
  SysUtils, Classes, Windows, WinAPI.ShellAPI, Soap.EncdDecd,
  StringSupport,
  FHIRBase, FHIRTypes, FHIRResources, FHIRConstants, FHIRParser,
  FHIRProfileUtilities, FHIRPath,
  MsXml, MsXmlParser, AdvJson,
  DUnitX.TestFramework;

var
  GBasePath : String;

Type
  TTestingWorkerContext = class (TWorkerContext)
  public
    function expand(vs : TFhirValueSet) : TFHIRValueSet; override;
    function supportsSystem(system : string) : boolean; override;
    function validateCode(system, code, display : String) : TValidationResult; overload; override;
    function validateCode(system, code, version : String; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCoding; vs : TFhirValueSet) : TValidationResult; overload; override;
    function validateCode(code : TFHIRCodeableConcept; vs : TFhirValueSet) : TValidationResult; overload; override;

    class function Use : TWorkerContext;
    class procedure closeUp;
  end;

function CheckXMLIsSame(filename1, filename2 : String; var msg : string) : boolean;
function CheckJsonIsSame(filename1, filename2 : String; var msg : string) : boolean;

implementation

uses
  TextUtilities, IdGlobalProtocols, BytesSupport;

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
  if not result then
  begin
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

function CompareObjects(path : String; o1, o2 : TJsonObject) : String; forward;

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
    s := CompareObjects(path, (n1 as TJsonObject), (n2 as TJsonObject));
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

function CompareObjects(path : String; o1, o2 : TJsonObject) : String;
var
  n : String;
  n1, n2 : TJsonNode;
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
      msg := CompareObjects('$', j1, j2);
      result := msg = '';
    finally
      j2.free;
    end;
  finally
    j1.Free;
  end;
end;

function CheckJsonIsSame(filename1, filename2 : String; var msg : string) : boolean;
{$IFDEF DIFF}
var
  j1, j2 : TJsonObject;
  f1, f2, cmd : String;
{$ENDIF}
begin
  result := compareJson(filename1, filename2, msg);
{$IFDEF DIFF}
  if not result then
  begin
    j1 := TJSONParser.ParseFile(filename1);
    j2 := TJSONParser.ParseFile(filename2);
    try

      f1 := MakeTempFilename +'-source.xml';
      f2 := MakeTempFilename +'-dest.xml';
      StringToFile(TJsonWriter.writeObjectStr(j1, true), f1, TEncoding.UTF8);
      StringToFile(TJsonWriter.writeObjectStr(j2, true), f2, TEncoding.UTF8);
      cmd := f1+' '+f2;
      ShellExecute(0, 'open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar(cmd), PChar(ExtractFilePath(f1)), SW_MAXIMIZE);
    finally
      j2.Free;
      j1.Free;
    end;
  end;
{$ENDIF}
end;


{ TTestingWorkerContext }
var
  GWorkerContext : TWorkerContext;

class procedure TTestingWorkerContext.closeUp;
begin
  GWorkerContext.Free;
end;

function TTestingWorkerContext.expand(vs: TFhirValueSet): TFHIRValueSet;
begin
  raise EFHIRPath.create('Not done yet');
end;

function TTestingWorkerContext.supportsSystem(system: string): boolean;
begin
  raise EFHIRPath.create('Not done yet');
end;


class function TTestingWorkerContext.Use: TWorkerContext;
begin
  if GWorkerContext = nil then
  begin
    GWorkerContext := TTestingWorkerContext.create;
//    GWorkerContext.LoadFromDefinitions(IncludeTrailingBackslash(GBasePath)+'build\\publish\\validation-min.xml.zip');
    GWorkerContext.LoadFromFile(IncludeTrailingBackslash(GBasePath)+'build\\publish\\profiles-types.xml');
    GWorkerContext.LoadFromFile(IncludeTrailingBackslash(GBasePath)+'build\\publish\\profiles-resources.xml');
  end;
  result := GWorkerContext.link;
end;

function TTestingWorkerContext.validateCode(system, code, version: String; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPath.create('Not done yet');
end;

function TTestingWorkerContext.validateCode(system, code, display: String): TValidationResult;
begin
  raise EFHIRPath.create('Not done yet');
end;

function TTestingWorkerContext.validateCode(code: TFHIRCodeableConcept; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPath.create('Not done yet');
end;

function TTestingWorkerContext.validateCode(code: TFHIRCoding; vs: TFhirValueSet): TValidationResult;
begin
  raise EFHIRPath.create('Not done yet');
end;

end.

