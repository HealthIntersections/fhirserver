unit fhir_icao;

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_json, fsl_crypto,
  fhir_objects,fhir_factory, fhir_common, fhir_uris;

type
  TICAOCardImporter = class (TFslObject)
  private
    FFactory: TFHIRFactory;
    FIssuer: string;

    function makePatient(pid : TJsonObject) : TFHIRResourceV;
    function makeImmunization(cvxCode : String; vd : TJsonObject) : TFHIRResourceV;
    function addEntry(bundle : TFHIRBundleW; i : integer) : TFhirBundleEntryW;

    procedure checkSignature(sig, data : TJsonObject);
    procedure checkheader(hdr : TJsonObject);

    procedure SetFactory(const Value: TFHIRFactory);

    function childReq(json : TJsonObject; name : String) : TJsonObject;
    function makeBundle : TFHIRBundleW;
    function processVaccineCode(ve: TJsonObject): String;
  public
    destructor Destroy; override;

    property factory : TFHIRFactory read FFactory write SetFactory;
    property issuer : string read FIssuer write FIssuer;

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
begin
  data := childReq(json, 'data');
  hdr := childReq(data, 'hdr');
  msg := childReq(data, 'msg');
  sig := childReq(json, 'sig');

  checkheader(hdr);
  checkSignature(sig, data);

  bundle := makeBundle;
  try
    addEntry(bundle, 0).resource := makePatient(childReq(msg, 'pid'));
    i := 1;
    if (msg.arr['ve'] = nil) or (msg.arr['ve'].Count = 0) then
      raise EFHIRException.Create('Unable to find ve in VDS');
    ve := msg.arr['ve'].Obj[0];
    cvxCode := processVaccineCode(ve);
    for vd in ve.forceArr['vd'] do
    begin
      addEntry(bundle, i).resource := makeImmunization(cvxCode, vd as TJsonObject);
      inc(i);
    end;

    result := factory.makeHealthcareCard;
    try
      result.bundle := bundle.Resource.link;
      result.issueDate := TFslDateTime.makeUTC; // or is in the signature?
      result.issuer := issuer;
      result.types := [ctHealthCard, ctCovidCard, ctImmunizationCard];
      result.link;
    finally
      result.free;
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
  // Australian ICAO VDS appear to use XM68M6 for AstraZeneca and XY64M3 for Cominirty
  // the ICD-11 definition of XM68M6 is 'COVID-19 vaccines' (https://icd.who.int/dev11/f/en#/http%3A%2F%2Fid.who.int%2Ficd%2Fentity%2F894585096)
  // the ICAO documentation uses XM68M6 for all vaccine types.
  // XY64M3 is not valid?
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
    imm.lotNumber := vd['adm'];
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
begin
  if sig['alg'] <> 'ES256' then
    raise EFHIRException.Create('Unsupported signature algorithm - only ES256 is supported');
  if not sig.has('cer') then
    raise EFHIRException.Create('Certificate not found');
  if not sig.has('sigvl') then
    raise EFHIRException.Create('Signature not found');

  cert := unBase64URL(sig['cer']);
  vl := unBase64URL(sig['sigvl']);
  src := TJsonWriterCanonical.writeObject(data);

  raise Exception.Create('Todo: actual validation');
end;

destructor TICAOCardImporter.Destroy;
begin
  FFactory.Free;
  inherited;
end;

procedure TICAOCardImporter.SetFactory(const Value: TFHIRFactory);
begin
  FFactory.Free;
  FFactory := Value;
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

function TICAOCardImporter.childReq(json: TJsonObject; name: String): TJsonObject;
begin
  result := json.obj[name];
  if result = nil then
    raise EFHIRException.Create('Unable to find '+name+' in VDS');
end;

function TICAOCardImporter.makeBundle: TFHIRBundleW;
begin
  result := FFactory.makeBundle(nil);
  result.type_ := btCollection;
end;

function TICAOCardImporter.addEntry(bundle : TFHIRBundleW; i: integer): TFhirBundleEntryW;
begin
  result := bundle.addEntry;
  result.url := 'resource:'+inttostr(i);
end;


end.
