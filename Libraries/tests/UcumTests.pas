{! 1 !}


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


Unit UcumTests;

Interface

Uses
  Sysutils,
  DecimalSupport,
  MsXml,
  UcumServices,
  MsXmlParser,
  AdvObjects;

Type
  EUcumTestFail = class (Exception);

  {$M+}
  TUcumTests = Class(TAdvObject) // but can be used with DUnit
  private
    FFolder : String;
    FUcum : TUcumServices;
    FTestDoc : IXMLDOMDocument2;

    Function TestValidationCase(id, unit_ : String; isValid : Boolean; var vMsg : String) : Boolean;
    procedure TestDisplaynameCase(id, unit_, display : String);
    procedure TestConversionCase(id, value, srcUnit, dstUnit, outcome: String);
    procedure TestMultiplicationCase(id, v1, u1, v2, u2, vRes, uRes : String);


    procedure CheckValidation(oElement : IXmlDomElement);
    procedure CheckDisplayNameGeneration(oElement : IXmlDomElement);
    procedure CheckConversions(oElement : IXmlDomElement);
    procedure CheckMultiplication(oElement : IXmlDomElement);
  Published
    Procedure Setup;
    Procedure TearDown;

    Procedure TestValidation;
    Procedure TestDisplay;
    Procedure TestConversion;
    Procedure TestMultiplication;

    Class procedure runTests(folder : String); // folder needs to contain 3 files: ucum-essence.xml, and ucum-tests.xml (both from http://unitsofmeasure.org)
  End;

Implementation

Uses
  AdvBuffers,
  StringSupport;

{ TUcumTests }

Procedure TUcumTests.TestValidation;
var
  iElem : IXMLDOMElement;
  idocElement: MsXML.IXMLDOMNode;
Begin
  idocElement:=FTestDoc.documentElement;
  iElem := TMsXmlParser.FirstChild(idocElement);
  while iElem <> nil Do
  Begin
    if iElem.nodeName = 'validation' Then
      CheckValidation(iElem);
    iElem := TMsXmlParser.NextSibling(iElem);
  End;
End;

Procedure TUcumTests.TestDisplay;
var
  iElem : IXMLDOMElement;
  idocElement: MsXML.IXMLDOMNode;
Begin
  idocElement:=FTestDoc.documentElement;
  iElem := TMsXmlParser.FirstChild(idocElement);
  while iElem <> nil Do
  Begin
    if iElem.nodeName = 'displayNameGeneration' Then
      CheckDisplayNameGeneration(iElem);
    iElem := TMsXmlParser.NextSibling(iElem);
  End;
End;

procedure TUcumTests.TearDown;
begin
  FTestDoc := nil;
  FUcum.Free;
end;

Procedure TUcumTests.TestConversion;
var
  iElem : IXMLDOMElement;
  idocElement: MsXML.IXMLDOMNode;
Begin
  idocElement:=FTestDoc.documentElement;
  iElem := TMsXmlParser.FirstChild(idocElement);
  while iElem <> nil Do
  Begin
    if iElem.nodeName = 'conversion' Then
      CheckConversions(iElem);
    iElem := TMsXmlParser.NextSibling(iElem);
  End;
End;

Procedure TUcumTests.TestMultiplication;
var
  iElem : IXMLDOMElement;
  idocElement: MsXML.IXMLDOMNode;
Begin
  idocElement:=FTestDoc.documentElement;
  iElem := TMsXmlParser.FirstChild(idocElement);
  while iElem <> nil Do
  Begin
    if iElem.nodeName = 'multiplication' Then
      CheckMultiplication(iElem);
    iElem := TMsXmlParser.NextSibling(iElem);
  End;
End;



procedure TUcumTests.CheckValidation(oElement : IXmlDomElement);
var
  oChild : IXMLDOMElement;
  bOk : Boolean;
  Msg, id : String;
Begin
  bOk := true;
  Msg := '';
  oChild := TMsXmlParser.FirstChild(oElement);
  while (oChild <> nil) Do
  Begin
    id := TMsXmlParser.GetAttribute(oChild, 'id');
    bOk := TestValidationCase(id,  TMsXmlParser.GetAttribute(oChild, 'unit'), StringToBoolean(TMsXmlParser.GetAttribute(oChild, 'valid')), Msg) and bOk;
    if not bOk then
      raise exception.Create('Error running case '+id+': '+Msg);
    oChild := TMsXmlParser.NextSibling(oChild);
  End;
End;

class procedure TUcumTests.runTests;
var
  this : TUcumTests;
begin
  this := TUcumTests.create;
  try
    this.FFolder:= folder;
    this.SetUp;
    try
      this.TestValidation;
      this.TestDisplay;
      this.TestConversion;
      this.TestMultiplication;
    finally
      this.TearDown;
    end;
  finally
    this.Free;
  end;
end;

procedure TUcumTests.Setup;
var
  oParser : TMsXmlParser;
begin
  FUcum := TUcumServices.Create;
  FUcum.Import(IncludeTrailingPathDelimiter(Ffolder)+'ucum-essence.xml');

  oParser := TMsXmlParser.Create;
  try
    FTestDoc := oParser.Parse(IncludeTrailingPathDelimiter(Ffolder)+'ucum-tests.xml');
  finally
    oParser.Free;
  End;
end;

procedure TUcumTests.CheckDisplayNameGeneration(oElement : IXmlDomElement);
var
  oChild : IXMLDOMElement;
Begin
  oChild := TMsXmlParser.FirstChild(oElement);
  while (oChild <> nil) Do
  Begin
    TestDisplaynameCase(TMsXmlParser.GetAttribute(oChild, 'id'),  TMsXmlParser.GetAttribute(oChild, 'unit'), TMsXmlParser.GetAttribute(oChild, 'display'));
    oChild := TMsXmlParser.NextSibling(oChild);
  End;
End;

procedure TUcumTests.CheckConversions(oElement : IXmlDomElement);
var
  oChild : IXMLDOMElement;
Begin
  oChild := TMsXmlParser.FirstChild(oElement);
  while (oChild <> nil) Do
  Begin
    TestConversionCase(TMsXmlParser.GetAttribute(oChild, 'id'),  TMsXmlParser.GetAttribute(oChild, 'value'), TMsXmlParser.GetAttribute(oChild, 'srcUnit'),
                            TMsXmlParser.GetAttribute(oChild, 'dstUnit'), TMsXmlParser.GetAttribute(oChild, 'outcome'));
    oChild := TMsXmlParser.NextSibling(oChild);
  End;
End;

procedure TUcumTests.CheckMultiplication(oElement : IXmlDomElement);
var
  oChild : IXMLDOMElement;
Begin
  oChild := TMsXmlParser.FirstChild(oElement);
  while (oChild <> nil) Do
  Begin
    TestMultiplicationCase(TMsXmlParser.GetAttribute(oChild, 'id'),
      TMsXmlParser.GetAttribute(oChild, 'v1'), TMsXmlParser.GetAttribute(oChild, 'u1'),
      TMsXmlParser.GetAttribute(oChild, 'v2'), TMsXmlParser.GetAttribute(oChild, 'u2'),
      TMsXmlParser.GetAttribute(oChild, 'vRes'), TMsXmlParser.GetAttribute(oChild, 'uRes'));
    oChild := TMsXmlParser.NextSibling(oChild);
  End;
End;


procedure TUcumTests.TestConversionCase(id, value, srcUnit, dstUnit, outcome: String);
var
  v, r, o : TSmartDecimal;
begin
  v := TSmartDecimal.valueOf(value);
  o := TSmartDecimal.valueOf(outcome);
  r := FUcum.convert(v, srcUnit, dstunit);
  if r.Compares(o) <> 0 then
     RaiseError('TestConversionCase', 'case '+id+': conversion of '+value+' '+srcUnit+' to '+dstUnit+' failed. Expected '+outcome+' but got '+r.AsString);
end;

procedure TUcumTests.TestDisplaynameCase(id, unit_, display: String);
var
  s : String;
begin
  s := FUcum.analyse(unit_);
  if s <> display Then
    raise EUcumTestFail.create(id+' failed. expected "'+display+'", got "'+s+'"');
end;

procedure TUcumTests.TestMultiplicationCase(id, v1, u1, v2, u2, vRes,uRes: String);
var
  o1, o2, o3 : TUcumPair;
begin
  o1 := TUcumPair.Create(TSmartDecimal.valueOf(v1), u1);
  Try
    o2 := TUcumPair.Create(TSmartDecimal.valueOf(v2), u2);
    Try
      o3 := FUcum.multiply(o1, o2);
      Try
        if o3.UnitCode <> uRes Then
          RaiseError('TestMultiplicationCase', 'Error in multiplication: got units '+o3.unitCode +' expecting '+URes);
        if (o3.Value.AsString <> vRes) Then
          RaiseError('TestMultiplicationCase', 'Error in multiplication: got value '+o3.Value.AsString +' expecting '+vRes);
      Finally
        o3.Free;
      End;
    Finally
      o2.Free;
    End;
  Finally
    o1.Free;
  End;
end;

Function TUcumTests.TestValidationCase(id, unit_: String; isValid: Boolean; var vMsg : String) : Boolean;
var
  sMsg : String;
  bOk : Boolean;
begin
  sMsg := FUcum.validate(Unit_);
  bOk := sMsg = '';
  if bOk = isValid Then
    result := true
  Else
  Begin
    result := false;
    vMsg := vMsg + '['+id+'] '+unit_+' failed: '+sMsg+#13#10;
  End;
end;

End.
