unit healthcard_generator;

{$i fhir.inc}

interface

uses
  SysUtils, Classes, DateUtils,
  fsl_base, fsl_utilities, fsl_http, fsl_json, fsl_crypto,
  fhir_objects, fhir_common, fhir_healthcard,
  fhir4_types, fhir4_resources, fhir4_json, fhir4_utilities, fhir4_factory,
  session, storage;

type

  TCardMaker = class (TFslObject)
  private
    FManager: TFHIROperationEngine;
    FRequest : TFHIRRequest;
    FCovid: boolean;
    FPatientId: String;
    FLinks: TFslStringDictionary;
    FIssues : TStringList;
    procedure linkResource(index : integer; ref : String);
  public
    constructor Create(manager: TFHIROperationEngine; request : TFHIRRequest; issues : TStringList);
    destructor Destroy; override;

    property links : TFslStringDictionary read FLinks;
    property covid : boolean read FCovid write FCovid;
    property patientId : String read FPatientId write FPatientId;

    function findPatient : TFHIRPatient;
    function makePatient(patient : TFHIRPatient) : TFHIRPatient;
  end;

  TVciImmunizationCardMaker = class (TCardMaker)
  private
    function isIncludedCoding(c : TFHIRCoding) : boolean;
  public
    function findImmunizations : TFslList<TFHIRImmunization>;
    function isRelevantImmunization(imm : TFhirImmunization) : boolean;

    function makeImmunization(imm : TFhirImmunization) : TFhirImmunization;

    function makeBundle : TFHIRBundle;
  end;

  THealthCardGenerator = class (TFslObject)
  private
    FIssuerUrl : String;
    FJwk : TJWK;

    FManager : TFHIROperationEngine;
    FRequest : TFHIRRequest;
    FPatientId: String;
    FParams: TFhirParameters;
    FSince : TFslDateTime;

//    FHealthCardJWS: string;
    FCards: TFslList<THealthcareCard>;
    FIssues : TStringList;
    procedure SetParams(const Value: TFhirParameters);

    function signCard(bundle : TFHIRBundle; types : TCredentialTypeSet) : THealthcareCard;

    procedure makeImmunizationCard(covid : boolean);
    function processParams : TCredentialTypeSet;
  public
    constructor Create(manager: TFHIROperationEngine; request : TFHIRRequest; key : TJWK);
    destructor Destroy; override;

    property patientId : String read FPatientId write FPatientId;
    property params : TFhirParameters read FParams write SetParams;

    procedure process;

    property cards : TFslList<THealthcareCard> read FCards;
    property issues : TStringList read FIssues;
    property IssuerURL : String read FIssuerUrl write FIssuerUrl;
  end;

implementation

{ THealthCardGenerator }

constructor THealthCardGenerator.Create(manager: TFHIROperationEngine; request : TFHIRRequest; key : TJWK);
begin
  inherited Create;
  FManager := manager;
  FCards := TFslList<THealthcareCard>.create;
  FIssues := TStringList.create;
  FJwk := key;
  FRequest := request;
end;

destructor THealthCardGenerator.Destroy;
begin
  FRequest.Free;
  FJwk.Free;
  FIssues.free;
  FCards.free;
  FParams.Free;
  FManager.free;
  inherited;
end;

procedure THealthCardGenerator.makeImmunizationCard(covid : boolean);
var
  engine : TVciImmunizationCardMaker;
  bnd : TFhirBundle;
  card : THealthcareCard;
begin
  engine := TVciImmunizationCardMaker.Create(FManager.link, FRequest.link, FIssues);
  try
    engine.covid := covid;
    engine.patientId := FPatientId;
    bnd := engine.makeBundle;
    try
      if bnd <> nil then
      begin
        if covid then
          card := signCard(bnd, [ctCovidCard, ctImmunizationCard])
        else
          card := signCard(bnd, [ctCovidCard]);
        card.links.assign(engine.links);
      end;
    finally
      bnd.Free;
    end;
  finally
    engine.Free;
  end;
end;

function THealthCardGenerator.processParams : TCredentialTypeSet;
var
  p : TFhirParametersParameter;
begin
  result := [];
  for p in params.parameterList do
  begin
    if p.name = 'credentialType' then
    begin
      if p.value.primitiveValue = 'https://smarthealth.cards#health-card' then
        result := result + [ctHealthCard]
      else if p.value.primitiveValue = 'https://smarthealth.cards#covid19' then
        result := result + [ctCovidCard]
      else if p.value.primitiveValue = 'https://smarthealth.cards#immunization' then
        result := result + [ctImmunizationCard]
      else if p.value.primitiveValue = 'https://smarthealth.cards#laboratory' then
        result := result + [ctLabCard]
      else
        raise EFslException.Create('Unknown credentialType value "'+p.value.primitiveValue+'"');
    end
    else if p.name = '_since' then
    begin
      if not TFslDateTime.isValidXmlDate(p.value.primitiveValue) then
        raise EFslException.Create('Unknown _since value "'+p.value.primitiveValue+'"');
      FSince := TFslDateTime.fromXML(p.value.primitiveValue)
    end
    else
      raise EFslException.Create('Unknown parameter "'+p.name+'"');
  end;

  if result = [] then
    raise EFslException.Create('no credentialType parameter found');
end;

procedure THealthCardGenerator.process;
var
  cts : TCredentialTypeSet;
  covid : boolean;
begin
  if PatientId = '' then raise EFslException.Create('No Patient ID found');
  if params = nil then raise EFslException.Create('No Parameters found');
  cts := processParams;
  covid := ctCovidCard in cts;
  if (ctHealthCard in cts) or not (ctLabCard in cts) then
    makeImmunizationCard(covid);
end;

procedure THealthCardGenerator.SetParams(const Value: TFhirParameters);
begin
  FParams.Free;
  FParams := Value;
end;

function THealthCardGenerator.signCard(bundle: TFHIRBundle; types : TCredentialTypeSet) : THealthcareCard;
var
  util : THealthcareCardUtilities;
begin
  result := TFHIRHealthcareCardR4.Create;
  try
    result.Bundle := bundle.Link;
    result.issueDate := TFslDateTime.makeUTC;
    result.issuer := FIssuerUrl;
    result.types := types;
    util := THealthcareCardUtilities.create;
    try
      util.Factory := TFHIRFactoryR4.Create;
      util.sign(result, FJwk);
    finally
      util.Free;
    end;
    FCards.add(result.link);
  finally
    result.Free;
  end;
end;

{ TCardMaker }

constructor TCardMaker.Create(manager: TFHIROperationEngine; request : TFHIRRequest; issues : TStringList);
begin
  inherited Create;
  FManager := manager;
  FRequest := request;
  FIssues := issues;
  FLinks := TFslStringDictionary.create;
end;

destructor TCardMaker.Destroy;
begin
  FLinks.Free;
  FRequest.Free;
  FManager.Free;
  inherited;
end;

function TCardMaker.findPatient: TFHIRPatient;
var
  ns : boolean;
begin
  result := FManager.GetResourceById(FRequest, 'Patient', FRequest.id, '', ns) as TFHIRPatient;
end;

procedure TCardMaker.linkResource(index: integer; ref: String);
begin
  links.add('resource:'+inttostr(index), ref);
end;

function TCardMaker.makePatient(patient: TFHIRPatient): TFHIRPatient;
var
  n, ns : TFhirHumanName;
begin
  result := TFHIRPatient.Create;
  try
    if patient.hasNameList then
    begin
      n := result.nameList.Append;
      n.family := patient.nameList[0].family;
      if patient.nameList[0].hasGivenList then
        n.givenList.add(patient.nameList[0].givenList[0].clone);
    end;
    result.birthDate := patient.birthDate;
    result.link;
  finally
    result.Free;
  end;
end;

{ TVciImmunizationCardMaker }

function TVciImmunizationCardMaker.findImmunizations: TFslList<TFHIRImmunization>;
var
  bw : TFhirBundleW;
  bnd : TFhirBundle;
  be : TFhirBundleEntry;
begin
  result := TFslList<TFHIRImmunization>.create;
  try
    bw := FManager.DoSearch(FRequest, 'Immunization', 'patient='+FPatientId+'&_sort=date');
    try
      bnd := bw.Resource as TFhirBundle;
      for be in bnd.entryList do
        if (be.resource <> nil) and (be.resource is TFhirImmunization) then
          result.Add((be.resource as TFhirImmunization).link);
    finally
      bw.Free;
    end;
    result.Link;
  finally
    result.Free;
  end;
end;

function TVciImmunizationCardMaker.isIncludedCoding(c: TFHIRCoding): boolean;
begin
  result := StringArrayExists(['http://hl7.org/fhir/sid/cvx', 'https://www.gs1.org/gtin', 'http://snomed.info/sct',
      'http://id.who.int/icd/release/11/mms','https://www.humanservices.gov.au/organisations/health-professionals/enablers/air-vaccine-code-formats','http://www.whocc.no/atc'], c.system)
end;

function TVciImmunizationCardMaker.isRelevantImmunization(imm: TFhirImmunization): boolean;
var
  c : TFHIRCoding;
begin
  result := false;
  if imm.occurrence is TFhirDateTime then
    if imm.vaccineCode <> nil then
      for c in imm.vaccineCode.codingList do
        if isIncludedCoding(c) then
          exit(true);
end;

function TVciImmunizationCardMaker.makeBundle: TFHIRBundle;
var
  bundle : TFhirBundle;
  pat : TFhirPatient;
  immList : TFslList<TFhirImmunization>;
  imm : TFhirImmunization;
  i : integer;
begin
  bundle := TFhirBundle.Create(BundleTypeCollection);
  try
    pat := findPatient;
    try
      linkResource(0, 'Patient/'+pat.id);
      bundle.entryList.Append.resource := makePatient(pat);
    finally
      pat.Free;
    end;
    i := 1;
    immList := findImmunizations;
    try
      for imm in immList do
        if isRelevantImmunization(imm) then
        begin
          bundle.entryList.Append.resource := makeImmunization(imm);
          linkResource(i, 'Immunization/'+imm.id);
          inc(i);
        end;
    finally
      immList.Free;
    end;
    if bundle.entryList.Count >= 2 then
      result := bundle.Link
    else
    begin
      FIssues.Add('No Immunizations found');
      result := nil;
    end;
  finally
    bundle.Free;
  end;
end;

function TVciImmunizationCardMaker.makeImmunization(imm: TFhirImmunization): TFhirImmunization;
var
  c, t : TFHIRCoding;
begin
  result := TFhirImmunization.create;
  try
    result.status := imm.status;
    result.vaccineCode := TFhirCodeableConcept.Create;
    for c in imm.vaccineCode.codingList do
      if isIncludedCoding(c) then
      begin
        t := result.vaccineCode.codingList.append;
        t.code := c.code;
        t.system := c.system;
      end;
    result.patient := TFhirReference.Create('resource:0');
    result.occurrence := imm.occurrence.Link;
    if (imm.manufacturer <> nil) and (imm.manufacturer.identifier <> nil) then
    begin
      result.manufacturer := TFhirReference.Create;
      result.manufacturer.identifier := TFhirIdentifier.Create;
      result.manufacturer.identifier.system := imm.manufacturer.identifier.system;
      result.manufacturer.identifier.value := imm.manufacturer.identifier.value;
    end;
    result.lotNumber := imm.lotNumber;
    result.isSubpotentElement := imm.isSubpotentElement.link;
    result.Link;
  finally
    result.Free;
  end;
end;

end.
