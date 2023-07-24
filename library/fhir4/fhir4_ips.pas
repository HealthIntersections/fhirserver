unit fhir4_ips;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_json, fsl_stream, fsl_http, fsl_utilities,
  fhir4_resources, fhir4_resources_clinical, fhir4_types, fhir4_utilities;

const
  ROOT = 'http://healthintersections.com.au/IPS/';

type
  { TIPSGenerator }

  TIPSGenerator = class (TFslObject)
  private
    FParams: THTTPParameters;

    procedure SetParams(AValue: THTTPParameters);

    function makeCodeableConcept(systemUri, code, display : String) : TFhirCodeableConcept;
    procedure addToBundle(bnd : TFhirBundle; resource : TFHIRResource);
    function addSection(comp : TFhirComposition; title, systemUri, code : String) : TFHIRCompositionSection;

    function makeBundle : TFhirBundle;
    function makeComposition : TFHIRComposition;
    function makePatient : TFhirPatient;
    function makeFuncStatusCondition(sect : TFHIRCompositionSection; paramName, systemUri, code, display : String) : TFHIRCondition;
  public
    destructor Destroy; Override;

    property params : THTTPParameters read FParams write SetParams;

    function generate() : TFhirBundle;
  end;


  { TIPSWrapper }

  TIPSWrapper = class (TFslObject)
  private
    FManifest : TJsonObject;
    FContent : TFslMap<TFslBuffer>;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function fromStream(stream : TStream) : TIPSWrapper; overload;
    class function fromStream(stream : TFslStream) : TIPSWrapper; overload;
    procedure saveToStream(stream : TStream); overload;
    procedure saveToStream(stream : TFslStream); overload;
  end;

implementation

{ TIPSGenerator }
                 
destructor TIPSGenerator.Destroy;
begin
  inherited Destroy;
end;

procedure TIPSGenerator.SetParams(AValue: THTTPParameters);
begin
  if FParams=AValue then Exit;
  FParams:=AValue;
end;


function TIPSGenerator.makeCodeableConcept(systemUri, code, display : String) : TFhirCodeableConcept;
begin
  result := TFHIRCodeableConcept.create(systemUri, code);
  result.codingList[0].display := display;
end;

procedure TIPSGenerator.addToBundle(bnd : TFhirBundle; resource : TFHIRResource);
var
  e : TFHIRBundleEntry;
begin
  if (resource <> nil) then
  begin
    e := bnd.entryList.Append;
    e.fullUrl := URLPath([ROOT, resource.fhirType, resource.id]);
    e.resource := resource.Link;
  end;
end;

function TIPSGenerator.addSection(comp : TFhirComposition; title, systemUri, code : String) : TFHIRCompositionSection;
begin
  result := comp.sectionList.Append;
  try
    result.title := title;
    result.code := makeCodeableConcept(systemUri, code, '');
    result.link;
  finally
    result.free;
  end;
end;

function TIPSGenerator.makeBundle: TFhirBundle;
begin
  result := TFhirBundle.create;
  try
    result.id := newGuidId;
    result.identifier := TFhirIdentifier.create;
    result.identifier.system := 'urn:ietf:rfc:3986';
    result.identifier.value := 'urn:uuid:'+result.id;
    result.type_ := BundleTypeDocument;
    result.timestamp := TFslDateTime.makeUTC;
    result.link;
  finally
    result.free;
  end;
end;

function TIPSGenerator.makeComposition: TFHIRComposition;
var
  ref : TFHIRReference;
begin
  result := TFHIRComposition.create;
  try
    result.id := newGuidId;
    result.status := CompositionStatusFinal;
    result.type_ := makeCodeableConcept('http://loinc.org', '60591-5', '');
    result.subject := TFhirReference.create;
    result.subject.reference := 'Patient/p1';
    result.date := TFslDateTime.makeToday;
    if (params.has('author')) then
      result.authorList.Append.display := params['author']
    else
      result.authorList.Append.reference := 'Patient/p1';
    result.title := 'Patient Passport (IPS)';
    result.link;
  finally
    result.free;
  end;
end;

function TIPSGenerator.makePatient: TFhirPatient;
var
  id : TFhirIdentifier;
  cp : TFhirContactPoint;
  nok : TFHIRPatientContact;
begin
  result := TFhirPatient.create;
  try
    result.id := 'p1';
    if params.has('name') then
      result.nameList.Append.text := params.Value['name'];
    if params.has('dob') then
      result.birthDate := TFslDateTime.fromXML(params['dob']);
    if params.has('id') then
    begin
      id := result.identifierList.Append;
      id.value := params['id'];
      id.type_ := makeCodeableConcept('http://terminology.hl7.org/CodeSystem/v2-0203', 'NIIP', 'National Insurance Payor Identifier');
    end;
    if params.has('country') then
      result.addressList.append.country := params['country'];
    if params.has('email') then
    begin
      cp := result.telecomList.Append;
      cp.system := ContactPointSystemEmail;
      cp.value := params['email'];
    end;
    if params.has('mobile') then   
    begin
      cp := result.telecomList.Append;
      cp.system := ContactPointSystemPhone;
      cp.use := ContactPointUseMobile;
      cp.value := params['mobile'];
    end;
    if params.has('phone') then    
    begin
      cp := result.telecomList.Append;
      cp.system := ContactPointSystemPhone;
      cp.value := params['phone'];
    end;

    if (params.has('nok')) then
    begin
      nok := result.contactList.Append;
      nok.name := TFhirHumanName.create;
      nok.name.text := params['nok'];
      if params.has('nokemail') then
      begin
        cp := nok.telecomList.Append;
        cp.system := ContactPointSystemEmail;
        cp.value := params['nokemail'];
      end;
      if params.has('nokmobile') then
      begin
        cp := nok.telecomList.Append;
        cp.system := ContactPointSystemPhone;
        cp.use := ContactPointUseMobile;
        cp.value := params['nokmobile'];
      end;
      if params.has('nokphone') then
      begin
        cp := nok.telecomList.Append;
        cp.system := ContactPointSystemPhone;
        cp.value := params['nokphone'];
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TIPSGenerator.makeFuncStatusCondition(sect: TFHIRCompositionSection; paramName, systemUri, code, display: String): TFHIRCondition;
begin
  if params[paramName] <> 'true' then
    result := nil
  else
  begin
    result := TFHIRCondition.create;
    try
      result.id := newGuidId;
      result.clinicalStatus := makeCodeableConcept('http://terminology.hl7.org/CodeSystem/condition-clinical', 'actice', 'Active');
      result.code := makeCodeableConcept(systemUri, code, display);
      result.subject := TFHIRReference.create('Patient/p1');
      sect.entryList.Append.reference := 'Condition/'+result.id;
      result.link;
    finally
      result.free;
    end;
  end;
end;

function TIPSGenerator.generate : TFhirBundle;
var
  bnd : TFHIRBundle;
  comp : TFHIRComposition;
  sect : TFHIRCompositionSection;
  cp : TFHIRCarePlan;
begin
  bnd := makeBundle;
  try
    comp := makeComposition;
    addToBundle(bnd, comp);
    addToBundle(bnd, makePatient);

    // functional concerns / needs
    sect := addSection(comp, 'Functional Concerns', 'http://loinc.org', '47420-5');
    addToBundle(bnd, makeFuncStatusCondition(sect, 'concern-visual', 'http://snomed.info/sct', '397540003', 'Visual impairment'));

    // consent things
    // sect := addSection(comp, 'Care Directives', 'http://loinc.org', '42348-3');
    // cp :=

    result := bnd.Link;
  finally
    bnd.free;
  end;
end;

{ TIPSWrapper }

constructor TIPSWrapper.Create;
begin
  inherited Create;
end;

destructor TIPSWrapper.Destroy;
begin
  inherited Destroy;
end;

class function TIPSWrapper.fromStream(stream: TStream): TIPSWrapper;
begin

end;

class function TIPSWrapper.fromStream(stream: TFslStream): TIPSWrapper;
begin

end;

procedure TIPSWrapper.saveToStream(stream: TStream);
begin

end;

procedure TIPSWrapper.saveToStream(stream: TFslStream);
begin

end;

end.

