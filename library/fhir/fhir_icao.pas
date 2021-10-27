unit fhir_icao;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_json, fsl_crypto,
  fhir_objects, fhir_factory, fhir_common, fhir_healthcard, fhir_uris;

const
  AUS_KNOWN_THUMBPRINT = 'B9A66AC5A2D60D8766B38EEC181B3F13F3375BD32C3E66A6F9E0975290AB92F9';

type
  TICAOCardImporter = class (TFslObject)
  private
    FFactory: TFHIRFactory;
    FIssuer: string;
    FJWK: TJWK;

    function makePatient(pid : TJsonObject) : TFHIRResourceV;
    function makeImmunization(cvxCode : String; vd : TJsonObject) : TFHIRResourceV;
    procedure addEntry(bundle : TFHIRBundleW; i : integer; res : TFHIRResourceV);

    procedure checkSignature(sig, data : TJsonObject);
    procedure checkheader(hdr : TJsonObject);

    procedure SetFactory(const Value: TFHIRFactory);

    function makeBundle : TFHIRBundleW;
    function processVaccineCode(ve: TJsonObject): String;

    procedure SetJWK(const Value: TJWK);
  public
    destructor Destroy; override;

    property factory : TFHIRFactory read FFactory write SetFactory;
    property issuer : string read FIssuer write FIssuer;
    property jwk : TJWK read FJWK write SetJWK;

    function import(json : TJsonObject) : THealthcareCard; overload;
    function import(source : String) : THealthcareCard; overload;
    function import(image : TBytes) : THealthcareCard; overload;
  end;

implementation

{ TICAOCardImporter }

function TICAOCardImporter.import(json: TJsonObject): THealthcareCard;
var
  bundle : TFHIRBundleW;
  data, sig, hdr, msg, ve : TJsonObject;
  vd : TJsonNode;
  i : integer;
  cvxCode : String;
  util : THealthcareCardUtilities;
begin
  data := json.objReq['data'];
  hdr := data.objReq['hdr'];
  msg := data.objReq['msg'];
  sig := json.objReq['sig'];

  checkheader(hdr);
  checkSignature(sig, data);

  bundle := makeBundle;
  try
    addEntry(bundle, 0, makePatient(msg.objReq['pid']));
    i := 1;
    if (msg.arr['ve'] = nil) or (msg.arr['ve'].Count = 0) then
      raise EFHIRException.Create('Unable to find ve in VDS');
    ve := msg.arr['ve'].Obj[0];
    cvxCode := processVaccineCode(ve);
    for vd in ve.forceArr['vd'] do
    begin
      addEntry(bundle, i, makeImmunization(cvxCode, vd as TJsonObject));
      inc(i);
    end;

    result := factory.makeHealthcareCard;
    try
      result.bundle := bundle.Resource.link;
      result.issueDate := TFslDateTime.makeUTC; // or is in the signature?
      result.issuer := issuer;
      result.types := [ctHealthCard, ctCovidCard, ctImmunizationCard];
      result.id := msg['uvci'];
      util := THealthcareCardUtilities.create;
      try
        util.Factory := FFactory.link;
        util.sign(result, FJwk);
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
  n : TArray<string>;
  i : integer;
begin
  pat := FFactory.wrapPatient(FFactory.makeResource('Patient'));
  try
    pat.dob := pid['dob'];
    pat.identifier[URI_AUSTRALIAN_PASSPORT_NUMBER] := pid['i'];
    n := pid['n'].split([' ']); // note that Australia ICAO cards are not conformant with the spec, which say that a comma comes after family name; they have a double space
    if length(n) > 0 then
      pat.family := n[0];
    for i := 1 to length(n)-1 do
      if (n[i] <> '') and (n[i] <> ',') then
        pat.addGiven(n[i]);
    pat.active := true;
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
    raise Exception.Create('Unknown vaccine code '+ve['des']+'/"'+nam+'"');
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

    result := imm.Resource.link;
  finally
    imm.Free;
  end;
end;

procedure TICAOCardImporter.checkheader(hdr: TJsonObject);
begin
  if hdr['t'] <> 'icao.vacc' then
    raise EFHIRException.Create('Unsupported card type = only type: icao.vacc cards are supported');
  if hdr['v'] <> '1' then
    raise EFHIRException.Create('Unsupported card version = only v: 1 cards are supported');
  if hdr['is'] <> 'AUS' then
    raise EFHIRException.Create('Unsupported card issuer. Only Australian cards are supported at this time (please send us a sample!)');
end;

procedure TICAOCardImporter.checkSignature(sig, data: TJsonObject);
var
  cert, vl, src : TBytes;
  x : TX509Certificate;
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
  BytesToFile(src, 'c:\temp\canonical.json');

  x := TX509Certificate.create(cert);
  try
    if x.ThumbprintAsSHA256 <> AUS_KNOWN_THUMBPRINT then
      raise EFHIRException.Create('Wrong certificate for Australia: expected a thumbprint of '+AUS_KNOWN_THUMBPRINT+' but found '+x.ThumbprintAsSHA256);
    if now > x.ValidToInGMT then
      raise EFHIRException.Create('Australian certificate is no longer valid - expired '+FormatDateTime('c', x.ValidToInGMT));
    if x.SignatureAlgorithmAsString <> 'sha256WithRSAEncryption' then
      raise EFHIRException.Create('Australian certificate is not valid - wrong algorithm type (must be sha256WithRSAEncryption)');

    jwk := TJWK.Create(x, false);
    try
     s := TJWTUtils.Verify_Hmac_ES256(src, vl, jwk);
     if s <> '' then
       raise EFHIRException.Create(s);
    finally
      jwk.Free;
    end;
  finally
    x.Free;
  end;
end;

destructor TICAOCardImporter.Destroy;
begin
  FJWK.Free;
  FFactory.Free;
  inherited;
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

function TICAOCardImporter.import(source: String): THealthcareCard;
var
  json : TJsonObject;
begin
  json := TJSONParser.Parse(source);
  try
    result := import(json);
  finally
    json.free;
  end;
end;

function TICAOCardImporter.import(image: TBytes): THealthcareCard;
begin
  raise Exception.Create('Not done yet');
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
