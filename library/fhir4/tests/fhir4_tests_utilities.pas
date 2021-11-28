unit fhir4_tests_utilities;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

uses
  SysUtils, Classes,
  fsl_testing, fsl_stream, fsl_utilities,
  fsl_http, fsl_crypto,
  fhir_objects, fhir4_parser, fhir_healthcard,
  fhir4_types, fhir4_resources, fhir4_utilities, fhir4_factory;

type
  TFHIRUtilityTests4 = Class (TFslTestCase)
  public
    Procedure TestBundleSigningXml;
    Procedure TestBundleSigningJson;
  published
    Procedure TestZipPartCreation;
    Procedure TestReferenceAnalysis;
    procedure TestHealthcardReading;
  end;

procedure registerTests;

implementation

{ TFHIRUtilityTests4 }

procedure TFHIRUtilityTests4.TestBundleSigningXml;
var
  bnd : TFhirBundle;
begin
  bnd := fileToResource(TestSettings.fhirTestFile(['R4', 'examples', 'document-example-dischargesummary.xml'])) as TFHIRBundle;
  try
    bnd.signRef(SignatureTypeAuthor, 'Practitioner/example', ffXml, 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem');
    ResourceToFile(bnd, filePath(['[tmp]', 'signed.xml']), ffXml, OutputStylePretty);
    assertTrue(bnd <> nil);
  finally
    bnd.Free;
  end;
end;

const
  josh_qr = 'shc:/5676295953265460346029254077280433602870286471674522280928655924334129593806214433653759320654745627373533393705416159214105673033036745326436074403040553062075400629385261366031222909524320603460292437404460573601064135214071'+
  '7074243505417773456837082823204375700558094052422030632034395876297144654162763835296453673250670541247766545825333535561053710312355309412732232522234004304352744574110966554235226727546423450306316204110531236907354343110770573927456429312'+
  '6304107315566346623245563415907081240455407372352713430574200377430543070040426280525413200085776075830052200592540362803722642637300714073412264230055753573553242547540740061076673210309687227363022237152004271673965612742246225365266270360'+
  '61715044724222281177125654223250253011314332596452770408636006703705376731567321553466054242597277374054120034456103037122086923543136714464452252777729632943250542674222631240325569736041385060573811435340720850082627521140624075502776105255'+
  '75443852743004407661605456770368303000330507760507752039716655630360075356773172317257306671253253430039582166656656286570257372227258342044750504596136454211731176343841103966704540735605735273403663386508406063343871390771006552255837670660'+
  '11732504570522032010230566540566003736687237233562605305256445281009435559323643577720455037283031656342002541500525077509424450563574706806700060562664303466074261222976285305570055591000503553523865601230640868296262272761507260772040455629'+
  '7265503345665457447275107554222163102058015510413231362230065643354137407727617765355024720609691007094472644272723752637042316709507773643352320305123438077212540735454238206573526350053059003208293709546821247236';

procedure TFHIRUtilityTests4.TestHealthcardReading;
var
  jws : String;
  utils : THealthcareCardUtilities;
  card : THealthcareCard;
begin
  utils := THealthcareCardUtilities.Create;
  try
    utils.Factory := TFHIRFactoryR4.Create;
    utils.JWKList := TJWKList.Create;

    jws := utils.readQR(josh_qr);
    StringToFile(jws, filePath(['[tmp]', 'jws.txt']), TEncoding.UTF8);

    card := utils.verify(jws);
    try
      assertTrue(card.isValid, 'Card isn''t valid: '+card.validationMessage);
    finally
      card.Free;
    end;
  finally
    utils.Free;
  end;
end;

procedure TFHIRUtilityTests4.TestBundleSigningJson;
var
  bnd : TFhirBundle;
begin
  bnd := fileToResource(TestSettings.fhirTestFile(['R4', 'examples', 'document-example-dischargesummary.xml'])) as TFHIRBundle;
  try
    bnd.signRef(SignatureTypeAuthor, 'Practitioner/example', ffJson, 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem');
    ResourceToFile(bnd, filePath(['[tmp]', 'signed.json']), ffJson, OutputStylePretty);
    assertTrue(bnd <> nil);
  finally
    bnd.Free;
  end;
end;

procedure TFHIRUtilityTests4.TestReferenceAnalysis;
var
  ref : TFhirReference;
begin
  ref := TFhirReference.Create;
  try
    ref.reference := 'http://hl7.org/fhir/Patient/example';
    assertTrue(not ref.isRelative);
    assertTrue(ref.getType = 'Patient');
    assertTrue(ref.getId = 'example');
  finally
    ref.Free;
  end;
  ref := TFhirReference.Create;
  try
    ref.reference := 'http://hl7.org/fhir/Patient/example/history/2';
    assertTrue(not ref.isRelative);
    assertTrue(ref.getType = 'Patient');
    assertTrue(ref.getId = 'example');
  finally
    ref.Free;
  end;
  ref := TFhirReference.Create;
  try
    ref.reference := 'Patient/example';
    assertTrue(ref.isRelative);
    assertTrue(ref.getType = 'Patient');
    assertTrue(ref.getId = 'example');
  finally
    ref.Free;
  end;
end;

procedure TFHIRUtilityTests4.TestZipPartCreation;
var
  att : TFhirAttachment;
  p : TFslZipPart;
begin
  att := TFHIRAttachment.create;
  try
    att.title := 'test';
    att.data := TEncoding.UTF8.GetBytes('Some test text');
    att.contentType := 'text/plain';
    p := att.asZipPart(0);
    try
      assertTrue(p.Name = 'test.txt');
      assertTrue(p.Size > 0);
      assertTrue(p.Comment = 'text/plain');
    finally
      p.Free;
    end;
  finally
    att.Free;
  end;
end;

procedure registerTests;
begin
  RegisterTest('R4', TFHIRUtilityTests4.Suite);
end;

end.
