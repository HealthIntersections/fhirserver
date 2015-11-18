{! 1 !}
{0.00-001  09 Nov 04 08:47  [21370]  User: Grahame Grieve    fix for new path}

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
Begin
  iElem := TMsXmlParser.FirstChild(FTestDoc.documentElement);
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
Begin
  iElem := TMsXmlParser.FirstChild(FTestDoc.documentElement);
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
Begin
  iElem := TMsXmlParser.FirstChild(FTestDoc.documentElement);
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
Begin
  iElem := TMsXmlParser.FirstChild(FTestDoc.documentElement);
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
  Msg : String;
Begin
  bOk := true;
  Msg := '';
  oChild := TMsXmlParser.FirstChild(oElement);
  while (oChild <> nil) Do
  Begin
    bOk := TestValidationCase(TMsXmlParser.GetAttribute(oChild, 'id'),  TMsXmlParser.GetAttribute(oChild, 'unit'), StringToBoolean(TMsXmlParser.GetAttribute(oChild, 'valid')), Msg) and bOk;
    oChild := TMsXmlParser.NextSibling(oChild);
  End;
  if not bOk then
    raise exception.Create(Msg);
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
  ctxt : TSmartDecimalContext;
  v, r, o : TSmartDecimal;
begin
  ctxt := TSmartDecimalContext.Create;
  Try
    v := ctxt.value(value);
    o := ctxt.value(outcome);
    r := FUcum.convert(v, srcUnit, dstunit);
    if r.Compares(o) <> 0 then
       Error('TestConversionCase', 'case '+id+': conversion of '+value+' '+srcUnit+' to '+dstUnit+' failed. Expected '+outcome+' but got '+r.AsString);
    assert(r.hasContext(v));
  Finally
    ctxt.free;
  End;
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
  context : TSmartDecimalContext;
begin
  context := TSmartDecimalContext.create;
  try
    o1 := TUcumPair.Create(context.value(v1).link, u1);
    Try
      o2 := TUcumPair.Create(context.value(v2).link, u2);
      Try
        o3 := FUcum.multiply(o1, o2);
        Try
          if o3.UnitCode <> uRes Then
            Error('TestMultiplicationCase', 'Error in multiplication: got units '+o3.unitCode +' expecting '+URes);
          if (o3.Value.AsString <> vRes) Then
            Error('TestMultiplicationCase', 'Error in multiplication: got value '+o3.Value.AsString +' expecting '+vRes);
        Finally
          o3.Free;
        End;
      Finally
        o2.Free;
      End;
    Finally
      o1.Free;
    End;
  finally
    context.Free;
  end;
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
