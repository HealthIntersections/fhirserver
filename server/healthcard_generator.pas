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
  fsl_base, fsl_utilities, fsl_http, fsl_json, fsl_crypto, fsl_qrcode,
  fhir_objects, fhir_common, fhir_healthcard, fhir_utilities,
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
  key.checkThumbprintIsSHA256Hash;
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
      bnd.Free;
    end;
  finally
    engine.Free;
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
    result.issuer := ExcludeTrailingSlash(FIssuerUrl);
    result.types := types;
    util := THealthcareCardUtilities.create;
    try
      util.Factory := TFHIRFactoryR4.Create;
      util.sign(result, FJwk);
      result.image := util.generateImage(result);
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
    result.Free;
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
      bundle.entryList.Append('resource:0').resource := makePatient(pat);
    finally
      pat.Free;
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
  i : integer;
  actor, a : TFhirReference;
  p : TResourceWithReference;
  s : String;
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
        if displays then
        begin
          t.display := c.display;
          if (t.display = '') then
            t.display := serverContext.TerminologyServer.getDisplayForCode(FRequest.Lang, t.system, c.version, t.code);
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
          a := TFhirReference.create;
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
              a := TFhirReference.create;
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
    result.Free;
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
    if c.system = 'http://loinc.org' then
      exit(c.code);
end;

function TVciLaboratoryCardMaker.findObservations: TFslList<TFHIRObservation>;
var
  bw : TFhirBundleW;
  bnd : TFhirBundle;
  be : TFhirBundleEntry;
begin
  result := TFslList<TFHIRObservation>.create;
  try
    bw := FManager.DoSearch(FRequest, 'Observation', 'patient='+FPatientId+'&_sort=date');
    try
      bnd := bw.Resource as TFhirBundle;
      for be in bnd.entryList do
        if (be.resource <> nil) and (be.resource is TFHIRObservation) then
          result.Add((be.resource as TFHIRObservation).link);
    finally
      bw.Free;
    end;
    result.Link;
  finally
    result.Free;
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
  c := ctxt.Factory.wrapCoding(ctxt.Factory.makeCoding('http://loinc.org', code));
  try
    if covid then
      result := ctxt.TerminologyServer.codeInValueSet(c, 'http://test.fhir.org/r4/ValueSet/Covid19Labs')
    else
      result := ctxt.TerminologyServer.codeInValueSet(c, 'http://test.fhir.org/r4/ValueSet/InfectiousDiseaseLabsLabs');
  finally
    c.Free;
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
      pat.Free;
    end;
    obsList := findObservations;
    try
      map := TFslMap<TFhirObservation>.create;
      try
        for obs in obsList do
          if isRelevantObservation(obs) then
            map.AddOrSetValue(loincCode(obs), obs.Link);
        obsList.Clear;
        for obs in map.Values do
          obsList.Add(obs.link);
      finally
        map.Free;
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
      obsList.Free;
    end;
    if bundle.entryList.Count >= 2 then
      result := bundle.Link
    else
    begin
      FIssues.Add('No Observations found');
      result := nil;
    end;
  finally
    bundle.Free;
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
    result.code := TFhirCodeableConcept.Create('http://loinc.org', loincCode(obs));
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
    result.Free;
  end;
end;

end.
