unit fhir_icao;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$i fhir.inc}

interface

uses
  SysUtils, Classes, Graphics, {$IFDEF DELPHI} Vcl.Imaging.pngimage, Vcl.Imaging.GIFImg, {$ENDIF}
  ZXing.ScanManager, ZXing.BarCodeFormat, ZXing.ReadResult,
  fsl_base, fsl_utilities, fsl_json, fsl_crypto,
  fhir_objects, fhir_factory, fhir_common, fhir_healthcard, fhir_uris;

type
  TICAOCardImporter = class (TFslObject)
  private
    FFactory: TFHIRFactory;
    FIssuer: string;
    FJWK: TJWK;
    FLog : TFslStringBuilder;
    FStore: TX509CertificateStore;
    FMustVerify: boolean;

    function makePatient(pid : TJsonObject) : TFHIRResourceV;
    function makeImmunization(cvxCode : String; vd : TJsonObject) : TFHIRResourceV;
    procedure addEntry(bundle : TFHIRBundleW; i : integer; res : TFHIRResourceV);

    procedure checkSignature(sig, data : TJsonObject);
    procedure checkheader(hdr : TJsonObject);

    procedure SetFactory(const Value: TFHIRFactory);

    function makeBundle : TFHIRBundleW;
    function processVaccineCode(ve: TJsonObject): String;

    procedure SetJWK(const Value: TJWK);
    function GetHtmlReport: String;
    function displayCvx(code: String): String;
    procedure SetStore(const Value: TX509CertificateStore);
  public
    constructor Create; override;
    destructor Destroy; override;

    property factory : TFHIRFactory read FFactory write SetFactory;
    property issuer : string read FIssuer write FIssuer;
    property store : TX509CertificateStore read FStore write SetStore;
    property mustVerify : boolean read FMustVerify write FMustVerify;
    property jwk : TJWK read FJWK write SetJWK;

    procedure log(s : String);
    property htmlReport : String read GetHtmlReport;
    function import(json : TJsonObject) : THealthcareCard; overload;
    function import(source : String) : THealthcareCard; overload;
    function import(image: TBitmap): THealthcareCard; overload;
    function import(image : TBytes) : THealthcareCard; overload;
  end;

implementation

{ TICAOCardImporter }

function TICAOCardImporter.import(json: TJsonObject): THealthcareCard;
var
  bundle : TFHIRBundleW;
  data, sig, hdr, msg : TJsonObject;
  ve, vd : TJsonNode;
  i : integer;
  cvxCode : String;
  util : THealthcareCardUtilities;
begin
  Flog.Append('<p>Processing Content</p>'#13#10);
  data := json.objReq['data'];
  hdr := data.objReq['hdr'];
  msg := data.objReq['msg'];
  sig := json.objReq['sig'];

  checkheader(hdr);

  Flog.Append('<p>Build Smart Health card for VDS id = '+encodeXml(msg['uvci'])+'</p>'#13#10);

  bundle := makeBundle;
  try
    addEntry(bundle, 0, makePatient(msg.objReq['pid']));
    i := 1;
    if (msg.arr['ve'] = nil) or (msg.arr['ve'].Count = 0) then
      raise EFHIRException.Create('Unable to find ve in VDS');
    for ve in msg.arr['ve'] do
    begin
      cvxCode := processVaccineCode(ve as TJsonObject);
      for vd in (ve  as TJsonObject).forceArr['vd'] do
      begin
        addEntry(bundle, i, makeImmunization(cvxCode, vd as TJsonObject));
        inc(i);
      end;
    end;

    checkSignature(sig, data);

    Flog.Append('<p>Build Smart Health Card</p>'#13#10);

    result := factory.makeHealthcareCard;
    try
      result.bundle := bundle.Resource.link;
      result.issueDate := TFslDateTime.makeUTC;
      result.issuer := issuer;
      result.types := [ctHealthCard, ctCovidCard, ctImmunizationCard];
      result.id := msg['uvci'];
      util := THealthcareCardUtilities.create;
      try
        util.Factory := FFactory.link;
        Flog.Append('<p>Sign with Certificate '+FJwk.id+' ('+FJwk.thumbprint+')</p>'#13#10);
        util.sign(result, FJwk);
        Flog.Append('<p>Build QR Code</p>'#13#10);
        result.image := util.generateImage(result);
      finally
        util.Free;
      end;
      result.link;
    finally
      result.Free;
    end;
  finally
    bundle.free;
  end;
end;

function TICAOCardImporter.makePatient(pid: TJsonObject): TFHIRResourceV;
var
  pat : TFhirPatientW;
  fn, fam, giv : String;
  n : TArray<string>;
  i : integer;
begin
  pat := FFactory.wrapPatient(FFactory.makeResource('Patient'));
  try
    pat.dob := pid['dob'];
    pat.identifier[URI_AUSTRALIAN_PASSPORT_NUMBER] := pid['i'];
    // Australian ICAO cards are not conformant with the spec, which say that a comma comes after family name; they have a double space
    fn := pid['n'];
    if fn.Contains('  ') then
      StringSplit(fn, '  ', fam, giv)
    else
      StringSplit(fn, ',', fam, giv);
    pat.family := fam;
    n := giv.split([' ']);
    for i := 0 to length(n)-1 do
      if (n[i] <> '') and (n[i] <> ',') then
        pat.addGiven(n[i]);
    pat.active := true;
    Flog.Append('<p>The card is for '+encodeXml(giv)+' '+encodeXml(fam)+', dob = '+encodeXml(pat.dob)+'. Passport # = '+encodeXml(pid['i'])+'</p>'#13#10);
    // we ignore gender.
    result := pat.Resource.link;
  finally
    pat.Free;
  end;
end;

function TICAOCardImporter.processVaccineCode(ve : TJsonObject) : String;
var
  nam : String;
begin
  // VDS appear to use XM68M6 for all vaccines.
  // the ICD-11 definition of XM68M6 is 'COVID-19 vaccines' (https://icd.who.int/dev11/f/en#/http%3A%2F%2Fid.who.int%2Ficd%2Fentity%2F894585096)
  // the ICAO documentation uses XM68M6 for all vaccine types.
  //  the cominirty code is XM8NQ0 and the AstraZenaca code is XM4YL8
  // so... we don't use the code.

  nam := ve['nam'];
  if (nam = 'AstraZeneca Vaxzevria') or (nam.ToLower.Contains('astra')) then
    result := '210'
  else if (nam = 'Pfizer Comirnaty') or (nam.ToLower.Contains('pfizer')) then
    result := '208'
  else
    raise EFHIRException.Create('Unknown vaccine code '+ve['des']+'/"'+nam+'"');
end;

function TICAOCardImporter.displayCvx(code : String) : String;
begin
  if code = '210' then
    result := 'AstraZeneca Vaxzevria'
  else if code = '208' then
    result := 'Pfizer Comirnaty'
  else
    result := '??';
end;

function TICAOCardImporter.makeImmunization(cvxCode : String; vd: TJsonObject): TFHIRResourceV;
var
  imm : TFHIRImmunizationW;
begin
  // we don't do anything with 'ctr'?
  imm := FFactory.wrapImmunization(FFactory.makeResource('Immunization'));
  try
    imm.status := 'completed';
    imm.cvxCode := cvxCode;
    imm.performerDisplay := vd['adm'];
    imm.lotNumber := vd['lot'];
    imm.patient := 'resource:0';
    imm.date := TFslDateTime.fromXML(vd['dvc']);
    Flog.Append('<p>Vaccinated with CVX#'+cvxCode+' ('+displayCvx(cvxCode)+') on '+encodeXml(vd['dvc'])+' by '+encodeXml(vd['adm'])+', lot# = '+encodeXml(imm.lotNumber)+'</p>'#13#10);
    result := imm.Resource.link;
  finally
    imm.Free;
  end;
end;

procedure TICAOCardImporter.checkheader(hdr: TJsonObject);
begin
  if hdr['t'] <> 'icao.vacc' then
    raise EFHIRException.Create('Unsupported card type = only type: icao.vacc cards are supported');
  Flog.Append('<p>Importing an ICAO VDS, version "'+encodeXml(hdr['v'])+'" issued by '+encodeXml(hdr['is'])+#13#10);
  if hdr['v'] <> '1' then
    raise EFHIRException.Create('Unsupported card version = only v: 1 cards are supported');
  if hdr['is'] <> 'AUS' then
    raise EFHIRException.Create('Unsupported card issuer. Only Australian cards are supported at this time (please send us a sample!)');
end;

procedure TICAOCardImporter.checkSignature(sig, data: TJsonObject);
var
  cert, vl, src : TBytes;
  x, ca : TX509Certificate;
  jwk : TJWK;
  s : String;
begin
  if sig['alg'] <> 'ES256' then
    raise EFHIRException.Create('Unsupported signature algorithm - only ES256 is supported');
  if not sig.has('cer') then
    raise EFHIRException.Create('Certificate not found');
  if not sig.has('sigvl') then
    raise EFHIRException.Create('Signature not found');

  cert := unBase64URL(sig['cer']);
  vl := unBase64URL(sig['sigvl']);
  src := TJsonWriterCanonical.canonicaliseObject(data);

  x := TX509Certificate.create(cert);
  try
    Flog.Append('<p>Check Certificate # '+encodeXml(x.SerialNumber)+'</p>'#13#10);
    Flog.Append('<p>Issuer = '+encodeXml(x.Issuer.AsString)+'</p>'#13#10);
    Flog.Append('<p>Subject = '+encodeXml(x.Subject.AsString)+'</p>'#13#10);
    Flog.Append('<p>Expires = '+FormatDateTime('c', x.ValidToInGMT)+'</p>'#13#10);
    Flog.Append('<p>Algorithm = '+encodeXml(x.SignatureAlgorithmAsString)+'</p>'#13#10);
    Flog.Append('<p>Authority Key Id = '+encodeXml(x.AuthorityKeyIdentifier)+'</p>'#13#10);

    ca := FStore.ByKeyId[x.AuthorityKeyIdentifier];
    if ca <> nil then
    begin
      Flog.Append('<p>Found Authority Certificate '+ca.SerialNumber+', verifying against it</p>'#13#10);
      TX509CertificateVerifier.verifyCert(x, [ca]);
    end
    else
    begin
      Flog.Append('<p>No matching certificate found for Authority Key Id</p>'#13#10);
      if FMustVerify then
        raise EFHIRException.Create('Cannot verify certificate - no match for key "'+x.AuthorityKeyIdentifier+'"');
    end;
    // todo: how do we verify that the certificate is a real one issued by the Australian passport office?

    if now > x.ValidToInGMT then
      raise EFHIRException.Create('Australian certificate is no longer valid - expired '+FormatDateTime('c', x.ValidToInGMT));
    if x.SignatureAlgorithmAsString <> 'sha256WithRSAEncryption' then
      raise EFHIRException.Create('Australian certificate is not valid - wrong algorithm type (must be sha256WithRSAEncryption)');


    jwk := TJWK.loadFromX509(x, false);
    try
      Flog.Append('<p>Check Signature ('+Base64URL(vl)+'/'+Base64URL(src)+'</p>'#13#10);
      s := TJWTUtils.Verify_Hmac_ES256(src, vl, jwk);
      if s <> '' then
        raise EFHIRException.Create('The Covid Passport Signature is not valid');
    finally
      jwk.Free;
    end;
  finally
    x.Free;
  end;
end;

constructor TICAOCardImporter.Create;
begin
  inherited;
  FLog := TFslStringBuilder.create;
end;

destructor TICAOCardImporter.Destroy;
begin
  FStore.Free;
  FLog.Free;
  FJWK.Free;
  FFactory.Free;
  inherited;
end;

function TICAOCardImporter.GetHtmlReport: String;
begin
  result := FLog.ToString;
end;

procedure TICAOCardImporter.SetFactory(const Value: TFHIRFactory);
begin
  FFactory.Free;
  FFactory := Value;
end;

procedure TICAOCardImporter.SetJWK(const Value: TJWK);
begin
  FJWK.Free;
  FJWK := Value;
end;

procedure TICAOCardImporter.SetStore(const Value: TX509CertificateStore);
begin
  FStore.Free;
  FStore := Value;
end;

function TICAOCardImporter.import(source: String): THealthcareCard;
var
  json : TJsonObject;
begin
  Flog.Append('<p>Reading JSON ('+inttostr(source.Length)+'bytes)</p>'#13#10);
  json := TJSONParser.Parse(source);
  try
    result := import(json);
  finally
    json.free;
  end;
end;

function TICAOCardImporter.import(image: TBytes): THealthcareCard;
var
  picture : TPicture;
  bitmap : TBitmap;
  stream : TBytesStream;
begin
  Flog.Append('<p>Importing from an image format ('+inttostr(length(image))+'bytes)</p>'#13#10);
  picture := TPicture.create;
  try
    stream := TBytesStream.Create(image);
    try
      picture.LoadFromStream(stream);
      bitmap := TBitmap.Create;
      try
        bitmap.Width := Picture.Width;
        bitmap.Height := Picture.Height;
        bitmap.Canvas.Draw(0, 0, Picture.Graphic);
        result := import(bitmap);
      finally
        bitmap.Free;
      end;
    finally
      stream.free;
    end;
  finally
    picture.Free;
  end;
end;

procedure TICAOCardImporter.log(s: String);
begin
  Flog.Append('<p>'+encodeXml(s)+'</p>'#13#10);
end;

function TICAOCardImporter.import(image: TBitmap): THealthcareCard;
var
  scanner : TScanManager;
  bc : TReadResult;
begin
  Flog.Append('<p>Scanning image for QR code ('+inttostr(image.Width)+'x'+inttostr(image.Height)+')</p>'#13#10);
  scanner := TScanManager.create(TBarcodeFormat.Auto, nil);
  try
    bc := scanner.Scan(image);
    try
      if (bc = nil) then
      begin
        raise EFHIRException.Create('Unable to read a barcode from the image supplied');
      end;
      result := import(bc.text);
    finally
      bc.free;
    end;
  finally
    scanner.free;
  end;
end;

function TICAOCardImporter.makeBundle: TFHIRBundleW;
begin
  result := FFactory.makeBundle(nil);
  result.type_ := btCollection;
end;

procedure TICAOCardImporter.addEntry(bundle : TFHIRBundleW; i: integer; res : TFHIRResourceV);
var
  be : TFhirBundleEntryW;
begin
  be := bundle.addEntry;
  try
    be.url := 'resource:'+inttostr(i);
    be.resource := res;
  finally
    be.free;
  end;
end;


end.
