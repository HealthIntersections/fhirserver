unit healthcard_generator;

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
  SysUtils, Classes, DateUtils, Graphics, {$IFDEF FPC} FPImage, FPWritePNG, {$ELSE} Vcl.Imaging.pngimage, {$ENDIF}
  fsl_base, fsl_utilities, fsl_http, fsl_json, fsl_crypto, fhir_qrcode,
  fhir_objects, fhir_common, fhir_healthcard, fhir_utilities, fhir_uris,
  fhir4_types, fhir4_resources, fhir4_json, fhir4_utilities, fhir4_factory,
  session, storage, server_context;

type

  TCardMaker = class (TFslObject)
  private
    FManager: TFHIROperationEngine;
    FRequest : TFHIRRequest;
    FCovid: boolean;
    FPatientId: String;
    FLinks: TFslStringDictionary;
    FIssues : TStringList;
    FDisplays: boolean;
    procedure linkResource(index : integer; ref : String);
    function serverContext : TFHIRServerContext;
  public
    constructor Create(manager: TFHIROperationEngine; request : TFHIRRequest; issues : TStringList);
    destructor Destroy; override;

    property links : TFslStringDictionary read FLinks;
    property covid : boolean read FCovid write FCovid;
    property patientId : String read FPatientId write FPatientId;
    property displays : boolean read FDisplays write FDisplays;

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

  TVciLaboratoryCardMaker = class (TCardMaker)
  private
    function loincCode(obs : TFHIRObservation) : String;
    function DoSortObservations(sender : TObject; const L, R: TFHIRObservation): Integer;
  public
    function findObservations : TFslList<TFHIRObservation>;
    function isRelevantObservation(obs : TFHIRObservation) : boolean;

    function makeObservation(obs : TFHIRObservation) : TFHIRObservation;

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
    procedure makeLaboratoryCard(covid : boolean);

    function processParams : TCredentialTypeSet;
    procedure SetManager(const Value: TFHIROperationEngine);
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

  TParameterCheckType = (f_name, f_date, f_numeric, f_text);

  THealthCardFormGenerator = class (TFslObject)
  private
    FIssuerUrl : String;
    FJwk : TJWK;

    FVpr2: String;
    FVpr3: String;
    FVpr1: String;
    FIhi: String;
    FVacc2: String;
    FVacc3: String;
    FId: String;
    FVacc1: String;
    FVdate2: String;
    FFamily: String;
    FVdate3: String;
    Fissues: TStringList;
    FVdate1: String;
    FExp: String;
    FVlot2: String;
    FDob: String;
    FVlot3: String;
    FGiven: String;
    FVlot1: String;
    procedure checkParam(value, name: String; required: boolean; minLength, maxLength: integer; checkType: TParameterCheckType);
    function makePatient : TFHIRPatient;
    function makeImmunization(date, vacc, lot, pr : String) : TFhirImmunization;
    function makeBundle : TFHIRBundle;
  public
    constructor Create(key : TJWK);
    destructor Destroy; override;

    function checkInput(vars : THTTPParameters) : boolean;
    function generateCard : THealthcareCard;

    property IssuerURL : String read FIssuerUrl write FIssuerUrl;
    property issues : TStringList read Fissues;

    property family : String read FFamily write FFamily;
    property given : String read FGiven write FGiven;
    property dob : String read FDob write FDob;
    property ihi : String read FIhi write FIhi;
    property vdate1 : String read FVdate1 write FVdate1;
    property vacc1 : String read FVacc1 write FVacc1;
    property vlot1 : String read FVlot1 write FVlot1;
    property vpr1 : String read FVpr1 write FVpr1;
    property vdate2 : String read FVdate2 write FVdate2;
    property vacc2 : String read FVacc2 write FVacc2;
    property vlot2 : String read FVlot2 write FVlot2;
    property vpr2 : String read FVpr2 write FVpr2;
    property vdate3 : String read FVdate3 write FVdate3;
    property vacc3 : String read FVacc3 write FVacc3;
    property vlot3 : String read FVlot3 write FVlot3;
    property vpr3 : String read FVpr3 write FVpr3;
    property id : String read FId write FId;
    property exp : String read FExp write FExp;
  end;
implementation

{ THealthCardGenerator }

constructor THealthCardGenerator.Create(manager: TFHIROperationEngine; request : TFHIRRequest; key : TJWK);
begin
  inherited Create;
  FManager := manager;
  FCards := TFslList<THealthcareCard>.Create;
  FIssues := TStringList.Create;
  FJwk := key;
  key.checkThumbprintIsSHA256Hash;
  FRequest := request;
end;

destructor THealthCardGenerator.Destroy;
begin
  FRequest.free;
  FJwk.free;
  FIssues.free;
  FCards.free;
  FParams.free;
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
    engine.displays := params.bool['displays'];
    bnd := engine.makeBundle;
    try
      if bnd <> nil then
      begin
        if covid then
          card := signCard(bnd, [ctCovidCard, ctImmunizationCard])
        else
          card := signCard(bnd, [ctImmunizationCard]);
        card.links.assign(engine.links);
      end;
    finally
      bnd.free;
    end;
  finally
    engine.free;
  end;
end;

procedure THealthCardGenerator.makeLaboratoryCard(covid: boolean);
var
  engine : TVciLaboratoryCardMaker;
  bnd : TFhirBundle;
  card : THealthcareCard;
begin
  engine := TVciLaboratoryCardMaker.Create(FManager.link, FRequest.link, FIssues);
  try
    engine.covid := covid;
    engine.patientId := FPatientId;
    bnd := engine.makeBundle;
    try
      if bnd <> nil then
      begin
        if covid then
          card := signCard(bnd, [ctCovidCard, ctLabCard])
        else
          card := signCard(bnd, [ctLabCard]);
        card.links.assign(engine.links);
      end;
    finally
      bnd.free;
    end;
  finally
    engine.free;
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
    else if not StringArrayExistsSensitive(['images', 'displays'], p.name) then
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
  if (ctHealthCard in cts) or not (ctImmunizationCard in cts) then
    makeLaboratoryCard(covid);
end;

procedure THealthCardGenerator.SetManager(const Value: TFHIROperationEngine);
begin
  FManager.free;
  FManager := Value;
end;

procedure THealthCardGenerator.SetParams(const Value: TFhirParameters);
begin
  FParams.free;
  FParams := Value;
end;

function THealthCardGenerator.signCard(bundle: TFHIRBundle; types : TCredentialTypeSet) : THealthcareCard;
var
  key : integer;
  util : THealthcareCardUtilities;
begin
  result := TFHIRHealthcareCardR4.Create;
  try
    result.Bundle := bundle.Link;
    result.issueDate := TFslDateTime.makeUTC;
    key := FManager.Storage.issueHealthCardKey;
    result.id := inttostr(key);
    result.issuer := ExcludeTrailingSlash(FIssuerUrl);
    result.types := types;
    util := THealthcareCardUtilities.Create;
    try
      util.Factory := TFHIRFactoryR4.Create;
      util.sign(result, FJwk);
      FManager.storage.logHealthCard(key, shcSrcFromResources, result.issueDate, IntToStr(result.issueDate.toNbf), util.hash(result), PatientId, TEncoding.UTF8.GetBytes(bundle.asJson));
      result.image := util.generateImage(result);
    finally
      util.free;
    end;
    FCards.add(result.link);
  finally
    result.free;
  end;
end;

{ TCardMaker }

constructor TCardMaker.Create(manager: TFHIROperationEngine; request : TFHIRRequest; issues : TStringList);
begin
  inherited Create;
  FManager := manager;
  FRequest := request;
  FIssues := issues;
  FLinks := TFslStringDictionary.Create;
end;

destructor TCardMaker.Destroy;
begin
  FLinks.free;
  FRequest.free;
  FManager.free;
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
  n : TFhirHumanName;
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
    result.free;
  end;
end;

function TCardMaker.serverContext: TFHIRServerContext;
begin
  result := (FManager.ServerContextObject as TFHIRServerContext)
end;

{ TVciImmunizationCardMaker }

function TVciImmunizationCardMaker.findImmunizations: TFslList<TFHIRImmunization>;
var
  bw : TFhirBundleW;
  bnd : TFhirBundle;
  be : TFhirBundleEntry;
begin
  result := TFslList<TFHIRImmunization>.Create;
  try
    bw := FManager.DoSearch(FRequest, 'Immunization', 'patient='+FPatientId+'&_sort=date');
    try
      bnd := bw.Resource as TFhirBundle;
      for be in bnd.entryList do
        if (be.resource <> nil) and (be.resource is TFhirImmunization) then
          result.Add((be.resource as TFhirImmunization).link);
    finally
      bw.free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function TVciImmunizationCardMaker.isIncludedCoding(c: TFHIRCoding): boolean;
begin
  result := StringArrayExists([URI_CVX, URI_GTIN, URI_SNOMED,
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
      bundle.entryList.Append('resource:0').resource := makePatient(pat);
    finally
      pat.free;
    end;
    i := 1;
    immList := findImmunizations;
    try
      for imm in immList do
        if isRelevantImmunization(imm) then
        begin
          bundle.entryList.Append('resource:'+inttostr(i)).resource := makeImmunization(imm);
          linkResource(i, 'Immunization/'+imm.id);
          inc(i);
        end;
    finally
      immList.free;
    end;
    if bundle.entryList.Count >= 2 then
      result := bundle.Link
    else
    begin
      FIssues.Add('No Immunizations found');
      result := nil;
    end;
  finally
    bundle.free;
  end;
end;

function TVciImmunizationCardMaker.makeImmunization(imm: TFhirImmunization): TFhirImmunization;
var
  c, t : TFHIRCoding;
  i : integer;
  actor, a : TFhirReference;
  p : TResourceWithReference;
  s : String;
begin
  result := TFhirImmunization.Create;
  try
    result.status := imm.status;
    result.vaccineCode := TFhirCodeableConcept.Create;
    for c in imm.vaccineCode.codingList do
      if isIncludedCoding(c) then
      begin
        t := result.vaccineCode.codingList.append;
        t.code := c.code;
        t.system := c.system;
        if displays then
        begin
          t.display := c.display;
          if (t.display = '') then
            t.display := serverContext.TerminologyServer.getDisplayForCode(FRequest.langList, t.system, c.version, t.code);
        end;
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
    for i := 0 to imm.performerList.Count - 1 do
    begin
      actor := imm.performerList[i].actor;
      if actor <> nil then
      begin
        if actor.display <> '' then
        begin
          a := TFhirReference.Create;
          result.performerList.Append.actor := a;
          a.display := actor.display;
          break;
        end
        else if actor.reference <> '' then
        begin
          p := FManager.LookupReference(FRequest, actor.reference);
          if (p <> nil) then
          begin
            s := serverContext.Factory.describe(p.Resource);
            if (s <> '') then
            begin
              a := TFhirReference.Create;
              result.performerList.Append.actor := a;
              a.display := s;
              break;
            end;
          end;
        end;
      end;
    end;
    result.isSubpotentElement := imm.isSubpotentElement.link;
    result.Link;
  finally
    result.free;
  end;
end;

{ TVciLaboratoryCardMaker }

function TVciLaboratoryCardMaker.DoSortObservations(sender: TObject; const L, R: TFHIRObservation): Integer;
var
  ld, rd : TFslDateTime;
begin
  ld := (l.effective as TFhirDateTime).value;
  rd := (r.effective as TFhirDateTime).value;
  result := ld.compare(rd);
end;

function TVciLaboratoryCardMaker.loincCode(obs: TFHIRObservation): String;
var
  c : TFHIRCoding;
begin
  result := '';
  for c in obs.code.codingList do
    if c.system = URI_LOINC then
      exit(c.code);
end;

function TVciLaboratoryCardMaker.findObservations: TFslList<TFHIRObservation>;
var
  bw : TFhirBundleW;
  bnd : TFhirBundle;
  be : TFhirBundleEntry;
begin
  result := TFslList<TFHIRObservation>.Create;
  try
    bw := FManager.DoSearch(FRequest, 'Observation', 'patient='+FPatientId+'&_sort=date');
    try
      bnd := bw.Resource as TFhirBundle;
      for be in bnd.entryList do
        if (be.resource <> nil) and (be.resource is TFHIRObservation) then
          result.Add((be.resource as TFHIRObservation).link);
    finally
      bw.free;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

function TVciLaboratoryCardMaker.isRelevantObservation(obs: TFHIRObservation): boolean;
var
  code : String;
  c : TFHIRCodingW;
  ctxt : TFHIRServerContext;
begin
  // it's relevant if it's a LOINC observation for a genetic material, antibodies, or antigens
  code := loincCode(obs);
  if (code = '') then
    exit(false);

  if (obs.subject = nil) or (obs.subject.reference = '') or (obs.effectiveElement = nil) then
    exit(false);
  if (obs.value = nil) or not StringArrayExistsSensitive(['CodeableConcept', 'Quantity', 'string'], obs.value.fhirType) then
    exit(false);

  ctxt := serverContext;
  c := ctxt.Factory.wrapCoding(ctxt.Factory.makeCoding(URI_LOINC, code));
  try
    if covid then
      result := ctxt.TerminologyServer.codeInValueSet(c, 'http://test.fhir.org/r4/ValueSet/Covid19Labs')
    else
      result := ctxt.TerminologyServer.codeInValueSet(c, 'http://test.fhir.org/r4/ValueSet/InfectiousDiseaseLabsLabs');
  finally
    c.free;
  end;
end;

function TVciLaboratoryCardMaker.makeBundle: TFHIRBundle;
var
  bundle : TFhirBundle;
  pat : TFhirPatient;
  obsList : TFslList<TFhirObservation>;
  map : TFslMap<TFhirObservation>;
  obs : TFhirObservation;
  i : integer;
begin
  bundle := TFhirBundle.Create(BundleTypeCollection);
  try
    pat := findPatient;
    try
      linkResource(0, 'Patient/'+pat.id);
      bundle.entryList.Append('resource:0').resource := makePatient(pat);
    finally
      pat.free;
    end;
    obsList := findObservations;
    try
      map := TFslMap<TFhirObservation>.Create;
      try
        for obs in obsList do
          if isRelevantObservation(obs) then
            map.AddOrSetValue(loincCode(obs), obs.Link);
        obsList.Clear;
        for obs in map.Values do
          obsList.Add(obs.link);
      finally
        map.free;
      end;
      obsList.SortE(DoSortObservations);
      i := 1;
      for obs in obsList do
      begin
        bundle.entryList.Append('resource:'+inttostr(i)).resource := makeObservation(obs);
        linkResource(i, 'Observation/'+obs.id);
        inc(i);
      end;
    finally
      obsList.free;
    end;
    if bundle.entryList.Count >= 2 then
      result := bundle.Link
    else
    begin
      FIssues.Add('No Observations found');
      result := nil;
    end;
  finally
    bundle.free;
  end;
end;

function TVciLaboratoryCardMaker.makeObservation(obs: TFHIRObservation): TFHIRObservation;
var
  p : TResourceWithReference;
  s : String;
  i : integer;
  rs, rt : TFhirObservationReferenceRange;
begin
  result := TFHIRObservation.Create;
  try
    result.status := obs.status;
    result.code := TFhirCodeableConcept.Create(URI_LOINC, loincCode(obs));
    result.subject := TFhirReference.Create('resource:0');
    result.effective := obs.effective.Link;

    for i := 0 to obs.performerList.Count - 1 do
    begin
      if obs.performerList[0].display <> '' then
      begin
        result.performerList.Append.display := obs.performerList[0].display;
        break;
      end
      else if obs.performerList[0].reference <> '' then
      begin
        p := FManager.LookupReference(FRequest, obs.performerList[0].reference);
        if (p <> nil) then
        begin
          s := serverContext.Factory.describe(p.Resource);
          if (s <> '') then
          begin
            result.performerList.Append.display := s;
            break;
          end;
        end;
      end;
    end;

    result.value := obs.value.Link;
    for rs in obs.referenceRangeList do
    begin
      rt := result.referenceRangeList.Append;
      rt.low := rs.low.Link;
      rt.high := rs.high.Link;
      rt.text := rs.text;
      if (rs.type_ <> nil) and (rs.type_.text <> '') then
      begin
        rt.type_ := TFhirCodeableConcept.Create;
        rt.text := rs.type_.text;
      end;
    end;
    result.Link;
  finally
    result.free;
  end;
end;

{ THealthCardFormGenerator }

constructor THealthCardFormGenerator.Create(key: TJWK);
begin
  inherited Create;
  FJwk := key;
  FIssues := TStringList.Create;
end;

destructor THealthCardFormGenerator.Destroy;
begin
  FIssues.free;
  FJwk.free;
  inherited;
end;

function THealthCardFormGenerator.generateCard: THealthcareCard;
var
  key : integer;
  util : THealthcareCardUtilities;
begin
  result := TFHIRHealthcareCardR4.Create;
  try
    result.Bundle := makeBundle;
    result.issueDate := TFslDateTime.makeUTC;
    // result.id := inttostr(key);
    result.issuer := ExcludeTrailingSlash(FIssuerUrl);
    result.types := [ctCovidCard, ctImmunizationCard];
    util := THealthcareCardUtilities.Create;
    try
      util.Factory := TFHIRFactoryR4.Create;
      util.sign(result, FJwk);
      // FManager.storage.logHealthCard(key, shcSrcFromResources, result.issueDate, IntToStr(result.issueDate.toNbf), util.hash(result), PatientId, TEncoding.UTF8.GetBytes(bundle.asJson));
      result.image := util.generateImage(result);
    finally
      util.free;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function THealthCardFormGenerator.makeBundle: TFHIRBundle;
var
  bundle : TFhirBundle;
  imm : TFhirImmunization;
  i : integer;
begin
  bundle := TFhirBundle.Create(BundleTypeCollection);
  try
    bundle.entryList.Append('resource:0').resource := makePatient();
    bundle.entryList.Append('resource:1').resource := makeImmunization(vdate1, vacc1, vlot1, vpr1);
    imm := makeImmunization(vdate2, vacc2, vlot2, vpr2);
    if (imm <> nil) then
    begin
      bundle.entryList.Append('resource:2').resource := imm;
      imm := makeImmunization(vdate3, vacc3, vlot3, vpr3);
      if (imm <> nil) then
        bundle.entryList.Append('resource:3').resource := imm;
    end;
    result := bundle.Link;
  finally
    bundle.free;
  end;

end;

function THealthCardFormGenerator.makeImmunization(date, vacc, lot, pr : String): TFhirImmunization;
var
  c : TFHIRCoding;
  prf : TFhirImmunizationPerformer;
begin
  if (date = '') and (vacc = '') then
    result := nil
  else
  begin
    result := TFhirImmunization.Create;
    try
      result.status := ImmunizationStatusCompleted;
      result.vaccineCode := TFhirCodeableConcept.Create;
      c := result.vaccineCode.codingList.append;
      c.system := 'http://hl7.org/fhir/sid/cvx';
      c.code := vacc;
      result.patient := TFhirReference.Create('resource:0');
      result.lotNumber := lot;
      if (pr <> '') then
      begin
        prf := result.performerList.Append;
        prf.actor := TFhirReference.Create;
        prf.actor.display := pr;
      end;
      if TFslDateTime.isValidDate('yyyy-mm-dd', date) then
        result.occurrence := TFhirDateTime.Create(TFslDateTime.fromFormat('yyyy-mm-dd', date))
      else
        result.occurrence := TFhirDateTime.Create(TFslDateTime.fromFormat('dd-mm-yyyy', date));
      result.Link;
    finally
      result.free;
    end;
  end;
end;

function THealthCardFormGenerator.makePatient(): TFHIRPatient;
var
  n : TFhirHumanName;
  s : String;
begin
  result := TFHIRPatient.Create;
  try
    n := result.nameList.Append;
    n.family := family;
    for s in given.Split([' ']) do
       n.givenList.add(s);
    if TFslDateTime.isValiddate('yyyy-mm-dd', dob) then
      result.birthDate := TFslDateTime.fromFormat('yyyy-mm-dd', dob)
    else
      result.birthDate := TFslDateTime.fromFormat('dd-mm-yyyy', dob);
    result.link;
  finally
    result.free;
  end;

end;

function THealthCardFormGenerator.checkInput(vars: THTTPParameters): boolean;
begin
  family := vars['family'];
  given := vars['given'];
  dob := vars['dob'];
  ihi := vars['ihi'];
  vdate1 := vars['vdate1'];
  vacc1 := vars['vacc1'];
  vlot1 := vars['vlot1'];
  vpr1 := vars['vpr1'];
  vdate2 := vars['vdate2'];
  vacc2 := vars['vacc2'];
  vlot2 := vars['vlot2'];
  vpr2 := vars['vpr2'];
  vdate3 := vars['vdate3'];
  vacc3 := vars['vacc3'];
  vlot3 := vars['vlot3'];
  vpr3 := vars['vpr3'];
  id := vars['id'];
  exp := vars['exp'];

  checkParam(family, 'family', true, 1, 40, f_name);
  checkParam(given, 'given', true, 1, 40, f_name);
  checkParam(dob, 'dob', true, 10, 10, f_date);
  checkParam(ihi, 'given', false, 16, 16, f_numeric);

  // if (vdate1 <> '') or (vacc1 <> '') or (vlot1 <> '') or (vpr1 <> '') then have to have at least a first
//    begin
  checkParam(vdate1, 'vdate1', true, 10, 10, f_date);
  checkParam(vacc1, 'vacc1', true, 3, 3, f_numeric);
  checkParam(vlot1, 'vlot1', false, 1, 15, f_text);
  checkParam(vpr1, 'vpr1', false, 1, 40, f_text);
  if (vdate2 <> '') or (vacc2 <> '') or (vlot2 <> '') or (vpr2 <> '') then
  begin
    checkParam(vdate2, 'vdate2', true, 10, 10, f_date);
    checkParam(vacc2, 'vacc2', true, 3, 3, f_numeric);
    checkParam(vlot2, 'vlot2', false, 1, 15, f_text);
    checkParam(vpr2, 'vpr2', false, 1, 40, f_text);
    if (vdate3 <> '') or (vacc3 <> '') or (vlot3 <> '') or (vpr3 <> '') then
    begin
      checkParam(vdate3, 'vdate3', true, 10, 10, f_date);
      checkParam(vacc3, 'vacc3', true, 3, 3, f_numeric);
      checkParam(vlot3, 'vlot3', false, 1, 15, f_text);
      checkParam(vpr3, 'vpr3', false, 1, 40, f_text);
    end;
  end;
  result := Fissues.Count = 0;
end;

procedure THealthCardFormGenerator.checkParam(value, name : String; required : boolean; minLength, maxLength : integer; checkType : TParameterCheckType);
var
  ch : char;
begin
  if value = '' then
  begin
    if required then
      issues.Add('Parameter '+name+' needs to have a value')
  end
  else
  begin
    if value.length < minLength then
      issues.Add('<li>Parameter '+name+' minimum length is '+inttostr(minLength)+' ("'+EncodeXML(value)+'")</li>');
    if value.length > maxLength then
      issues.Add('<li>Parameter '+name+' maximum length is '+inttostr(maxLength)+' ("'+EncodeXML(value)+'")</li>');
    case checkType of
      f_name:
        for ch in value do
        begin
          if not CharInSet(ch, ['a'..'z', 'A'..'Z', '-', '`', '''', ' ']) then
          begin
            issues.Add('Parameter '+name+' illegal char "'+EncodeXML(ch)+'"</li>');
            break
          end;
        end;
      f_date:
        if not TFslDateTime.isValiddate('yyyy-mm-dd', value) and not TFslDateTime.isValiddate('dd-mm-yyyy', value) then
          issues.Add('<li>Parameter '+name+' illegal date "'+EncodeXML(value)+'"</li>');
      f_numeric:
        for ch in value do
        begin
          if not CharInSet(ch, ['0'..'9']) then
          begin
            issues.Add('<li>Parameter '+name+' illegal char "'+EncodeXML(ch)+'"</li>');
            break
          end;
        end;
      f_text:
        for ch in value do
        begin
          if (ord(ch) < 32) or (ord(ch) > 126) then
          begin
            issues.Add('<li>Parameter '+name+' illegal char "'+EncodeXML(ch)+'"</li>');
            break
          end;
        end;
    end;
  end;
end;

end.
