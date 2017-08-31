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
  MXml,
  UcumServices,
  AdvObjects;

Type
  EUcumTestFail = class (Exception);

  {$M+}
  TUcumTests = Class(TAdvObject) // but can be used with DUnit
  private
    FFolder : String;
    FUcum : TUcumServices;
    FTestDoc : TMXmlDocument;

    Function TestValidationCase(id, unit_ : String; isValid : Boolean; var vMsg : String) : Boolean;
    procedure TestDisplaynameCase(id, unit_, display : String);
    procedure TestConversionCase(id, value, srcUnit, dstUnit, outcome: String);
    procedure TestMultiplicationCase(id, v1, u1, v2, u2, vRes, uRes : String);


    procedure CheckValidation(oElement : TMXmlElement);
    procedure CheckDisplayNameGeneration(oElement : TMXmlElement);
    procedure CheckConversions(oElement : TMXmlElement);
    procedure CheckMultiplication(oElement : TMXmlElement);
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
  iElem : TMXmlElement;
Begin
  iElem := FTestDoc.document.firstElement;
  while iElem <> nil Do
  Begin
    if iElem.Name = 'validation' Then
      CheckValidation(iElem);
    iElem := iElem.nextElement;
  End;
End;

Procedure TUcumTests.TestDisplay;
var
  iElem : TMXmlElement;
Begin
  iElem := FTestDoc.document.firstElement;
  while iElem <> nil Do
  Begin
    if iElem.name = 'displayNameGeneration' Then
      CheckDisplayNameGeneration(iElem);
    iElem := iElem.nextElement;
  End;
End;

procedure TUcumTests.TearDown;
begin
  FTestDoc := nil;
  FUcum.Free;
end;

Procedure TUcumTests.TestConversion;
var
  iElem : TMXmlElement;
Begin
  iElem := FTestDoc.document.firstElement;
  while iElem <> nil Do
  Begin
    if iElem.name = 'conversion' Then
      CheckConversions(iElem);
    iElem := iElem.nextElement;
  End;
End;

Procedure TUcumTests.TestMultiplication;
var
  iElem : TMXmlElement;
Begin
  iElem := FTestDoc.document.firstElement;
  while iElem <> nil Do
  Begin
    if iElem.name = 'multiplication' Then
      CheckMultiplication(iElem);
    iElem := iElem.nextElement;
  End;
End;



procedure TUcumTests.CheckValidation(oElement : TMXmlElement);
var
  oChild : TMXmlElement;
  bOk : Boolean;
  Msg, id : String;
Begin
  bOk := true;
  Msg := '';
  oChild := oElement.firstElement;
  while (oChild <> nil) Do
  Begin
    id := oChild.attribute['id'];
    bOk := TestValidationCase(id,  oChild.attribute['unit'], StringToBoolean(oChild.attribute['valid']), Msg) and bOk;
    if not bOk then
      raise exception.Create('Error running case '+id+': '+Msg);
    oChild := oChild.nextElement;
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
begin
  FUcum := TUcumServices.Create;
  FUcum.Import(IncludeTrailingPathDelimiter(Ffolder)+'ucum-essence.xml');
  FTestDoc := TMXmlParser.parseFile(IncludeTrailingPathDelimiter(Ffolder)+'ucum-tests.xml', [xpResolveNamespaces]);
end;

procedure TUcumTests.CheckDisplayNameGeneration(oElement : TMXmlElement);
var
  oChild : TMXmlElement;
Begin
  oChild := oElement.firstElement;
  while (oChild <> nil) Do
  Begin
    TestDisplaynameCase(oChild.attribute['id'], oChild.attribute['unit'], oChild.attribute['display']);
    oChild := oChild.nextElement;
  End;
End;

procedure TUcumTests.CheckConversions(oElement : TMXmlElement);
var
  oChild : TMXmlElement;
Begin
  oChild := oElement.firstElement;
  while (oChild <> nil) Do
  Begin
    TestConversionCase(oChild.attribute['id'], oChild.attribute['value'], oChild.attribute['srcUnit'],
                            oChild.attribute['dstUnit'], oChild.attribute['outcome']);
    oChild := oChild.nextElement;
  End;
End;

procedure TUcumTests.CheckMultiplication(oElement : TMXmlElement);
var
  oChild : TMXmlElement;
Begin
  oChild := oElement.firstElement;
  while (oChild <> nil) Do
  Begin
    TestMultiplicationCase(oChild.attribute['id'],
      oChild.attribute['v1'], oChild.attribute['u1'],
      oChild.attribute['v2'], oChild.attribute['u2'],
      oChild.attribute['vRes'], oChild.attribute['uRes']);
    oChild := oChild.nextElement;
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
