unit fhir4_ips;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_json, fsl_stream, fsl_http, fsl_utilities, fsl_logging,
  fhir_xhtml, fhir_objects, fhir_parser,
  fhir4_resources, fhir4_resources_clinical, fhir4_types, fhir4_utilities, fhir4_json, fhir4_xml;

const
  ROOT = 'http://healthintersections.com.au/IPS/';

type
  { TIPSGenerator }

  TIPSGenerator = class (TFslObject)
  private
    FFormatChoice : String;
    FFile: TFslbuffer;
    FParams: THTTPParameters;
    FLastId : integer;
    FPatDesc : String;
    procedure SetFile(AValue: TFslbuffer);
    procedure SetParams(AValue: THTTPParameters);

    function nextId(pfx : String) : String;
    function makeCodeableConcept(systemUri, code, display : String) : TFhirCodeableConcept;
    function makeAttachment(mimeType, title : String; content : TFslBuffer) : TFhirAttachment; overload;
    function makeAttachment(mimeType, title : String; ref : String) : TFhirAttachment; overload;
    function makeDiv(ext : boolean; out x : TFhirXHtmlNode) : TFHIRNarrative;
    procedure addToBundle(bnd : TFhirBundle; resource : TFHIRResource);
    function addSection(comp : TFhirComposition; title, systemUri, code : String; out x : TFhirXHtmlNode) : TFHIRCompositionSection;

    function makeBundle : TFhirBundle;
    function makeComposition : TFHIRComposition;
    function makePatient : TFhirPatient;
    function makeFuncStatusCondition(sect : TFHIRCompositionSection; ul : TFhirXHtmlNode; paramName, text, systemUri, code, display : String) : TFHIRCondition;
    function makeFuncStatusTextCondition(sect : TFHIRCompositionSection; x : TFhirXHtmlNode; paramName : string) : TFHIRCondition;
    function makeCareAdvocate(sect : TFHIRCompositionSection; ul : TFhirXHtmlNode) : TFHIRRelatedPerson;
    function makeConsent(sect : TFHIRCompositionSection; ul : TFhirXHtmlNode; fwds : boolean; paramName, textYes, textNo, systemUri, code, display : String) : TFHIRConsent;
    function makeDocRef(sect : TFHIRCompositionSection; ul : TFhirXHtmlNode) : TFHIRDocumentReference;
  public
    destructor Destroy; Override;

    property params : THTTPParameters read FParams write SetParams;
    property attachment : TFslbuffer read FFile write SetFile;

    function generateBundle : TFhirBundle;
    function generateBinary : TFslBuffer;
  end;


  { TIPSWrapper }
  TIPSVersionKind = (ivkOriginal, ivkTransformed, ivkAnnotated);
  TIPSAttachmentKind = (iakBrand, iakAuthored, iakAttachment, iakStylesheet);

  TIPSWrapper = class (TFslObject)
  private
    FManifest : TJsonObject;
    FContent : TFslMap<TFslBuffer>;
    function writeManifest : TBytes;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure startBuild(name : String);
    procedure addIPS(ips : TFhirBundle; kind : TIPSVersionKind; agent : String);
    procedure addAttachment(attachment : TFslBuffer; fn : String; kind : TIPSAttachmentKind);

    class function fromStream(stream : TStream) : TIPSWrapper; overload;
    class function fromStream(stream : TFslStream) : TIPSWrapper; overload;
    procedure saveToStream(stream : TStream); overload;
    procedure saveToStream(stream : TFslStream); overload;
    function saveToBytes : TBytes;
  end;

const
  CODES_TIPSVersionKind : array [TIPSVersionKind] of String = ('original', 'transformed', 'annotated');
  CODES_TIPSAttachmentKind : array [TIPSAttachmentKind] of String = ('brand', 'authored', 'attachment', 'stylesheet');

implementation

{ TIPSGenerator }
                 
destructor TIPSGenerator.Destroy;
begin
  FParams.free;
  FFile.free;
  inherited Destroy;
end;

procedure TIPSGenerator.SetParams(AValue: THTTPParameters);
begin
  FParams.free;
  FParams:=AValue;
end;

procedure TIPSGenerator.SetFile(AValue: TFslbuffer);
begin
  FFile.free;
  FFile:=AValue;
end;

function TIPSGenerator.nextId(pfx : String): String;
begin
  inc(FLastId);
  result := pfx+inttostr(FLastId);
end;


function TIPSGenerator.makeCodeableConcept(systemUri, code, display : String) : TFhirCodeableConcept;
begin
  result := TFHIRCodeableConcept.Create(systemUri, code);
  result.codingList[0].display := display;
end;

function TIPSGenerator.makeAttachment(mimeType, title: String; content: TFslBuffer): TFhirAttachment;
begin
  result := TFhirAttachment.Create;
  try
    result.contentType := mimeType;
    result.title := title;
    result.data := content.AsBytes;
    result.link;
  finally
    result.free;
  end;
end;

function TIPSGenerator.makeAttachment(mimeType, title: String; ref: String): TFhirAttachment;
begin
  result := TFhirAttachment.Create;
  try
    result.contentType := mimeType;
    result.title := title;
    result.url := ref;
    result.link;
  finally
    result.free;
  end;
end;

function TIPSGenerator.makeDiv(ext : boolean; out x : TFhirXHtmlNode): TFHIRNarrative;
begin
  result := TFHIRNarrative.Create;
  try
    if ext then
      result.status := NarrativeStatusExtensions
    else
      result.status := NarrativeStatusGenerated;
    result.div_ := TFhirXHtmlNode.Create('div');
    result.div_.attribute('xmlns', 'http://www.w3.org/1999/xhtml');
    x := result.div_;
    result.link;
  finally
    result.free;
  end;
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

function TIPSGenerator.addSection(comp : TFhirComposition; title, systemUri, code : String; out x : TFhirXHtmlNode) : TFHIRCompositionSection;
begin
  result := comp.sectionList.Append;
  try
    result.title := title;
    result.code := makeCodeableConcept(systemUri, code, '');
    result.text := makeDiv(false, x);
    result.link;
  finally
    result.free;
  end;
end;

function TIPSGenerator.makeBundle: TFhirBundle;
begin
  result := TFhirBundle.Create;
  try
    result.id := newGuidId;
    result.identifier := TFhirIdentifier.Create;
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
  result := TFHIRComposition.Create;
  try
    result.id := nextId('cmp');
    result.status := CompositionStatusFinal;
    result.type_ := makeCodeableConcept('http://loinc.org', '60591-5', '');
    result.subject := TFhirReference.Create;
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
  x : TFhirXHtmlNode;
  ext : TFHIRExtension;
  sg, cg, dg, tg, sp, cdp, dp, tp : String;
begin
  if (params.has('gender')) then
  begin
    if params['gender'] = 'f' then
    begin
      sg := 'http://snomed.info/sct';
      cg := '446141000124107';
      dg := 'Female gender identity';
      tg := 'Female';
    end
    else if params['gender'] = 'm' then
    begin
      sg := 'http://snomed.info/sct';
      cg := '446151000124109';
      dg := 'Male gender identity';
      tg := 'Male';
    end
    else if params['gender'] = 'n' then
    begin
      sg := 'http://snomed.info/sct';
      cg := '33791000087105';
      dg := 'Non-binary gender identity';
      tg := 'Non-binary';
    end
    else if params['gender'] = 'u' then
    begin
      sg := 'http://terminology.hl7.org/CodeSystem/data-absent-reason';
      cg := 'asked-declined';
      dg := 'Asked But Declined';
      tg := '';
    end
    else
      raise EFslException.Create('Unknown value for gender: '+params['gender']);
  end;
  if (params.has('pronouns')) then
  begin
    if params['pronouns'] = 'f' then
    begin
      sp := 'http://loinc.org';
      cdp := 'LA29519-8';
      dp := 'she/her/her/hers/herself';
      tp := 'she/her';
    end
    else if params['pronouns'] = 'm' then
    begin
      sp := 'http://loinc.org';
      cdp := 'LA29518-0';
      dp := 'he/him/his/his/himself';
      tp := 'he/him';
    end
    else if params['pronouns'] = 'o' then
    begin
      sp := 'http://loinc.org';
      cdp := 'LA29520-6';
      dp := 'they/them/their/theirs/themselves';
      tp := 'they/them';
    end
    else
      raise EFslException.Create('Unknown value for pronouns: '+params['pronouns']);
  end;


  result := TFhirPatient.Create;
  try
    result.id := 'p1';
    result.text := makeDiv((tp <> '') or (tg <> ''), x);
    if params.has('name') then
    begin
      result.nameList.Append.text := params['name'];
      x.tx('Patient: '+params['name']);
      FPatDesc := params['name'];
    end;
    if params.has('dob') then
    begin
      result.birthDate := TFslDateTime.fromXML(params['dob']);
      x.sep(', ');
      x.tx('born '+params['dob']);
      FPatDesc := FPatDesc + ', '+params['dob'];
    end;
    ;
    if (tg <> '') then
    begin
      x.sep(', ');
      x.tx(' ('+tg+' gender');
      if (tp <> '') then
      begin
        x.tx(', '+tp);
        FPatDesc := FPatDesc + '('+tg+':'+tp+')';
      end
      else
        FPatDesc := FPatDesc + '('+tg+')';
      x.tx(')');
      ext := result.addExtension('http://hl7.org/fhir/StructureDefinition/individual-genderIdentity');
      ext.addExtension('value', makeCodeableConcept(sg,cg,dg));
    end;
    if (tp <> '') then
    begin
      if (tg = '') then
      begin
        x.sep(', ');
        x.tx(' ('+tp+')');
        FPatDesc := FPatDesc + '('+tp+')';
      end;
      
      ext := result.addExtension('http://hl7.org/fhir/StructureDefinition/individual-pronouns');
      ext.addExtension('value', makeCodeableConcept(sp,cdp,dp));
    end;
    if params.has('id') then
    begin
      id := result.identifierList.Append;
      id.value := params['id'];
      id.type_ := makeCodeableConcept('http://terminology.hl7.org/CodeSystem/v2-0203', 'NIIP', 'National Insurance Payor Identifier (Payor)');
      x.sep('. ');
      x.tx('National ID '+params['id']);
      FPatDesc := FPatDesc + ', National ID '+params['id'];
      if params.has('country') then
        x.tx(' for '+params['country']);
    end;
    if params.has('country') then
      result.addressList.append.country := params['country'];
    if (x.ChildNodes.Count > 0) then
      x.br;
    if params.has('culture') then
    begin
      result.addExtension('http://healthintersections.com.au/fhir/StructureDefinition/patient-cultural-background', TFHIRString.Create(params['culture']));
      x.tx('Cultural background: '+params['culture']);
      x.br;
    end;

    if params.has('email') or params.has('mobile') or params.has('phone') then
    begin
      x.tx('Contacts: ');
      if params.has('email') then
      begin
        cp := result.telecomList.Append;
        cp.system := ContactPointSystemEmail;
        cp.value := params['email'];
        x.tx('email: ');
        x.ah('mailto:'+params['email']).tx(params['email']);
      end;
      if params.has('mobile') then
      begin
        if params.has('email') then
          x.tx(', ');
        cp := result.telecomList.Append;
        cp.system := ContactPointSystemPhone;
        cp.use := ContactPointUseMobile;
        cp.value := params['mobile'];
        x.tx('mobile: ');
        x.ah('tel:'+params['mobile']).tx(params['mobile']);
      end;
      if params.has('phone') then
      begin
        if params.has('email') or params.has('mobile') then
          x.tx(', ');
        cp := result.telecomList.Append;
        cp.system := ContactPointSystemPhone;
        cp.value := params['phone'];
        x.tx('phone: ');
        x.ah('tel:'+params['phone']).tx(params['phone']);
      end;
    end;

    if (params.has('nok')) then
    begin
      x.br;
      x.tx('Next of Kin: '+params['nok']);
      nok := result.contactList.Append;
      nok.name := TFhirHumanName.Create;
      nok.name.text := params['nok'];
      if params.has('nokemail') or params.has('nokmobile') or params.has('nokphone') then
      begin
        x.tx(', contacts: ');
        if params.has('nokemail') then
        begin
          cp := nok.telecomList.Append;
          cp.system := ContactPointSystemEmail;
          cp.value := params['nokemail']; 
          x.tx('email: ');
          x.ah('mailto:'+params['nokemail']).tx(params['nokemail']);
        end;
        if params.has('nokmobile') then
        begin                      
          if params.has('nokemail') then
            x.tx(', ');
          cp := nok.telecomList.Append;
          cp.system := ContactPointSystemPhone;
          cp.use := ContactPointUseMobile;
          cp.value := params['nokmobile']; 
        x.tx('mobile: ');
        x.ah('tel:'+params['nokmobile']).tx(params['nokmobile']);
        end;
        if params.has('nokphone') then
        begin                          
          if params.has('email') or params.has('mobile') then
            x.tx(', ');
          cp := nok.telecomList.Append;
          cp.system := ContactPointSystemPhone;
          cp.value := params['nokphone'];  
        x.tx('phone: ');
        x.ah('tel:'+params['nokphone']).tx(params['nokphone']);
        end;
      end;
    end;
    result.link;
  finally
    result.free;
  end;
end;

function TIPSGenerator.makeFuncStatusCondition(sect: TFHIRCompositionSection; ul : TFhirXHtmlNode; paramName, text, systemUri, code, display: String): TFHIRCondition;
var
  rx : TFhirXHtmlNode;
begin
  if params[paramName] <> 'true' then
    result := nil
  else
  begin
    result := TFHIRCondition.Create;
    try
      result.id := nextId('cnd');  
      result.text := makeDiv(false, rx);
      result.clinicalStatus := makeCodeableConcept('http://terminology.hl7.org/CodeSystem/condition-clinical', 'active', 'Active');
      result.code := makeCodeableConcept(systemUri, code, display);
      result.code.text := text;
      result.subject := TFHIRReference.Create('Patient/p1');
      sect.entryList.Append.reference := 'Condition/'+result.id;
      ul.li.tx(text);
      rx.p.tx('Condition for '+FPatDesc);
      rx.p.tx(text);
      result.link;
    finally
      result.free;
    end;
  end;
end;

function TIPSGenerator.makeFuncStatusTextCondition(sect: TFHIRCompositionSection; x: TFhirXHtmlNode; paramName: string): TFHIRCondition;
var
  rx : TFhirXHtmlNode;
begin
  if params[paramName] = '' then
    result := nil
  else
  begin
    result := TFHIRCondition.Create;
    try
      result.id := nextId('cnd'); 
      result.text := makeDiv(false, rx);
      result.clinicalStatus := makeCodeableConcept('http://terminology.hl7.org/CodeSystem/condition-clinical', 'active', 'Active');
      result.code := TFhirCodeableConcept.Create;
      result.code.text := params[paramName];
      result.subject := TFHIRReference.Create('Patient/p1');
      sect.entryList.Append.reference := 'Condition/'+result.id;
      x.p.tx(params[paramName]);
      rx.p.tx('Condition for '+FPatDesc);
      rx.p.tx(params[paramName]);
      result.link;
    finally
      result.free;
    end;
  end;
end;

function TIPSGenerator.makeCareAdvocate(sect: TFHIRCompositionSection; ul: TFhirXHtmlNode): TFHIRRelatedPerson;
var
  li: TFhirXHtmlNode;
var
  rx : TFhirXHtmlNode;
  cp : TFHIRContactPoint;
begin    
  if params['ca'] = '' then
    result := nil
  else
  begin
    li := ul.li;
    result := TFHIRRelatedPerson.Create;
    try
      result.id := nextId('rp');
      result.text := makeDiv(false, rx);
      result.patient := TFHIRReference.Create('Patient/p1');
      li.tx('Care Advocate:');
      rx.p.tx('Care Advocate for '+FPatDesc+':');
      result.nameList.Append.text := params['name'];
      li.tx(params['ca']);
      rx.tx(params['ca']);
      if params.has('caemail') or params.has('camobile') or params.has('caphone') then
      begin
        li.tx('. Contacts: ');
        rx.tx('. Contacts: ');
        if params.has('caemail') then
        begin
          cp := result.telecomList.Append;
          cp.system := ContactPointSystemEmail;
          cp.value := params['caemail'];
          li.tx('email: ');
          li.ah('mailto:'+params['caemail']).tx(params['caemail']);
          rx.tx('email: ');
          rx.ah('mailto:'+params['caemail']).tx(params['caemail']);
        end;
        if params.has('camobile') then
        begin
          if params.has('caemail') then
          begin
            li.tx(', ');
            rx.tx(', ');
          end;
          cp := result.telecomList.Append;
          cp.system := ContactPointSystemPhone;
          cp.use := ContactPointUseMobile;
          cp.value := params['camobile'];
          li.tx('mobile: ');
          li.ah('tel:'+params['camobile']).tx(params['camobile']);
          rx.tx('mobile: ');
          rx.ah('tel:'+params['camobile']).tx(params['camobile']);
        end;
        if params.has('caphone') then
        begin
          if params.has('caemail') or params.has('camobile') then
          begin
            li.tx(', ');
            rx.tx(', ');
          end;
          cp := result.telecomList.Append;
          cp.system := ContactPointSystemPhone;
          cp.value := params['caphone'];
          li.tx('phone: ');
          li.ah('tel:'+params['caphone']).tx(params['caphone']);
          rx.tx('phone: ');
          rx.ah('tel:'+params['caphone']).tx(params['caphone']);
        end;
      end;
      if params['calegal'] = 'true' then
      begin
        result.relationshipList.Add(makeCodeableConcept('http://terminology.hl7.org/CodeSystem/v3-RoleCode', 'HPOWATT', 'healthcare power of attorney'));
        rx.tx(' (legal power of attorney)');
        li.tx(' (legal power of attorney)');
      end
      else
        result.relationshipList.Add(makeCodeableConcept('http://terminology.hl7.org/CodeSystem/v3-RoleCode', 'NOK', 'next of kin'));

      sect.entryList.Append.reference := 'RelatedPerson/'+result.id;
      result.link;
    finally
      result.free;
    end;
  end;
end;

function TIPSGenerator.makeConsent(sect : TFHIRCompositionSection; ul : TFhirXHtmlNode; fwds : boolean; paramName, textYes, textNo, systemUri, code, display : String) : TFHIRConsent;
var
  li: TFhirXHtmlNode;
var
  rx : TFhirXHtmlNode;
  cp : TFHIRContactPoint;
begin
  if (params[paramName] = '') then
    result := nil
  else
  begin
    li := ul.li;
    result := TFHIRConsent.Create;
    try
      result.id := nextId('cnst');
      result.text := makeDiv(false, rx);
      li.tx('Consent: ');
      rx.p.tx('Consent for '+FPatDesc+': ');
      result.patient := TFHIRReference.Create('Patient/p1');
      result.status := ConsentStateCodesActive;
      result.scope := makeCodeableConcept('http://terminology.hl7.org/CodeSystem/consentscope', 'adr', 'Advanced Care Directive');
      result.categoryList.Add(makeCodeableConcept('http://terminology.hl7.org/CodeSystem/consentcategorycodes', 'acd', 'Advance Directive'));
      result.policyRule := TFhirCodeableConcept.Create;
      result.policyRule.text := 'Unknown Policy';
      result.provision := TFHIRConsentProvision.Create;
      if (params[paramName] = 'false') xor fwds then
      begin
        result.provision.type_ := ConsentProvisionTypePermit;
        li.tx(textYes);
        rx.tx(textYes);
      end
      else
      begin
        result.provision.type_ := ConsentProvisionTypeDeny;
        li.tx(textNo);
        rx.tx(textNo);
      end;
      result.provision.codeList.add(makeCodeableConcept(systemUri, code, display));

      sect.entryList.Append.reference := 'Consent/'+result.id;
      result.link;
    finally
      result.free;
    end;
  end;
end;

function TIPSGenerator.makeDocRef(sect: TFHIRCompositionSection; ul: TFhirXHtmlNode): TFHIRDocumentReference;
var
  li: TFhirXHtmlNode;
var
  rx : TFhirXHtmlNode;
  cp : TFHIRContactPoint;
begin
  if attachment = nil then
    result := nil
  else
  begin
    li := ul.li;
    result := TFHIRDocumentReference.Create;
    try
      result.id := nextId('dr');
      result.text := makeDiv(false, rx);
      result.status := DocumentReferenceStatusCurrent;
      result.subject := TFHIRReference.Create('Patient/p1');
      result.type_ := makeCodeableConcept('http://loinc.org', '75320-2', 'Advance directive');
      li.tx('Advance Care Directive:');
      rx.p.tx('Advance Care Directive '+FPatDesc+':');
      if FFormatChoice = 'z' then
        result.contentList.Append.attachment := makeAttachment(attachment.Format, 'Advance directive', 'adr.pdf')
      else
        result.contentList.Append.attachment := makeAttachment(attachment.Format, 'Advance directive', attachment);

      sect.entryList.Append.reference := 'DocumentReference/'+result.id;
      result.link;
    finally
      result.free;
    end;
  end;
end;

function TIPSGenerator.generateBundle : TFhirBundle;
var
  bnd : TFHIRBundle;
  comp : TFHIRComposition;
  sect : TFHIRCompositionSection;
  cp : TFHIRCarePlan;
  x, ul : TFhirXHtmlNode;
begin
  FFormatChoice := params['format'];

  bnd := makeBundle;
  try
    comp := makeComposition;
    addToBundle(bnd, comp);
    addToBundle(bnd, makePatient);

    // functional concerns / needs
    sect := addSection(comp, 'Functional Concerns', 'http://loinc.org', '47420-5', x);
    ul := x.ul;
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'concern-visual', 'Patient has concerns around Vision', 'http://snomed.info/sct', '397540003', 'Visual impairment'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'concern-audio', 'Patient has concerns around Hearing / Listening', 'http://snomed.info/sct', '15188001', 'Hearing impaired'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'concern-cognition', 'Patient has concerns around Cognition / thinking / understanding / information processing', 'http://snomed.info/sct', '386806002', 'Impaired cognition'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'concern-speaking', 'Patient has concerns around Speaking / communicating / Conversation / Verbal interaction', 'http://snomed.info/sct', '29164008', 'Speech impairment'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'concern-mobility', 'Patient has concerns around Mobility / moving myself around', 'http://snomed.info/sct', '82971005', 'Impaired mobility'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'concern-gender', 'Patient has concerns around Use of gender specific areas', 'http://snomed.info/sct', '93461009', 'Gender dysphoria'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'concern-memory', 'Patient has concerns around Memory', 'http://snomed.info/sct', '386807006', 'Memory impairment'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'concern-trauma', 'Patient has concerns around dealing with Past Trauma', 'http://snomed.info/sct', '161472001', 'History of psychological trauma'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'concern-focus', 'Patient has concerns around Staying focused / Concentration', 'http://snomed.info/sct', '1144748009', 'Impaired concentration'));


    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'eating', 'Patient may need help with Eating / Drinking', 'http://snomed.info/sct', '110292000', 'Difficulty eating'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'toileting', 'Patient may need help with Toileting', 'http://snomed.info/sct', '284911003', 'Difficulty using toilet'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'bed-exit', 'Patient may need help with Getting out of bed', 'http://snomed.info/sct', '301666002', 'Difficulty getting on and off a bed'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'bed-in', 'Patient may need help with Moving in bed', 'http://snomed.info/sct', '301685004', 'Difficulty moving in bed'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'orientation', 'Patient may need help with Getting orientated in a new environment', 'http://snomed.info/sct', '72440003', ' Disorientated in place'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'dressing', 'Patient may need help with Dressing', 'http://snomed.info/sct', '284977008', 'Difficulty dressing'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'bathing', 'Patient may need help with Bathing / Cleaning', 'http://snomed.info/sct', '284807005', 'Difficulty bathing self'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'meds', 'Patient may need help with Taking my medications', 'http://snomed.info/sct', '715037005', 'Difficulty taking medication'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'reading', 'Patient may need help with Reading Documentation', 'http://snomed.info/sct', '309253009', 'Difficulty reading'));


    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'dog', 'Patient has a Guide Dog', 'http://snomed.info/sct', '105506000', 'Dependence on seeing eye dog'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'wheelchair', 'Patient has a Wheelchair', 'http://snomed.info/sct', '105503008', 'Dependence on wheelchair'));
    addToBundle(bnd, makeFuncStatusCondition(sect, ul, 'comm-device', 'Patient has a Communication Device', 'http://snomed.info/sct', '719369003', 'Uses communication device'));

    addToBundle(bnd, makeFuncStatusTextCondition(sect, x, 'text'));

    // advance directive things
    sect := addSection(comp, 'Care Directives', 'http://loinc.org', '42348-3', x);
    ul := x.ul;
    addToBundle(bnd, makeCareAdvocate(sect, ul));
    addToBundle(bnd, makeConsent(sect, ul, false, 'dnr', 'Please resuscitate if necessary',
          'Patient wishes to not be resuscitated (DNR)', 'http://snomed.info/sct', '439569004', 'Resuscitation'));
    addToBundle(bnd, makeConsent(sect, ul, true, 'donor', 'Patient agrees to be an organ donor',
          'Patient does not agree to be an organ donor', 'http://snomed.info/sct', '1148553005', 'Post-mortem organ donation'));
    addToBundle(bnd, makeConsent(sect, ul, true, 'bld', 'Patient agrees to accept a blood tranfusion if necessary',
          'Patient does not accept a blood transfusion', 'http://snomed.info/sct', '116859006', 'Blood transfusion'));
    addToBundle(bnd, makeDocRef(sect, ul));

    result := bnd.Link;
  finally
    bnd.free;
  end;
end;

function TIPSGenerator.generateBinary: TFslBuffer;
var
  bnd : TFhirBundle;
  comp : TFHIRComposer;
  wrap : TIPSWrapper;
begin
  bnd := generateBundle;
  try
    result := TFslBuffer.Create;
    try
      if FFormatChoice = 'j' then
      begin
        comp := TFHIRJsonComposer.Create(nil, OutputStylePretty, nil);
        try
          result.AsText := comp.Compose(bnd);
        finally
          comp.free;
        end;
        result.Format := 'application/fhir+json';
      end
      else if FFormatChoice = 'x' then
      begin
        comp := TFHIRXmlComposer.Create(nil, OutputStylePretty, nil);
        try
          result.AsText := comp.Compose(bnd);
        finally
          comp.free;
        end;
        result.Format := 'application/fhir+xml';
      end
      else
      begin
        wrap := TIPSWrapper.Create;
        try
          wrap.startBuild('Patient Authored Passport (IPS)');
          wrap.addIPS(bnd, ivkOriginal, 'Health Intersections Website');
          if attachment <> nil then
            wrap.addAttachment(attachment, 'adr.pdf', iakAttachment);
          result.AsBytes := wrap.saveToBytes;
        finally
          wrap.free;
        end;
        result.Format := 'application/health-document';
      end;
      result.link;
    finally
      result.free;
    end;
  finally
    bnd.free;
  end;
end;


{ TIPSWrapper }

function TIPSWrapper.writeManifest: TBytes;
begin
  result := TJsonWriter.writeObject(FManifest, true);
end;

constructor TIPSWrapper.Create;
begin
  inherited Create;
  FContent := TFslMap<TFslBuffer>.Create;
end;

destructor TIPSWrapper.Destroy;
begin
  FContent.free;
  FManifest.free;
  inherited Destroy;
end;

procedure TIPSWrapper.startBuild(name : String);
begin
  FManifest := TJsonObject.Create;
  FManifest.str['format'] := 'ips-wrapper/1';
  FManifest.str['description'] := name;
end;

procedure TIPSWrapper.addIPS(ips: TFhirBundle; kind : TIPSVersionKind; agent : String);
var
  comp : TFHIRComposer;
  id : String;
  e : TJsonObject;
  bin : TFslBuffer;
begin
  id := 'ips-'+CODES_TIPSVersionKind[kind]+'.json';
  if (FContent.ContainsKey(id)) then
    id := newGuidId+'.json';
  e := FManifest.forceArr['ips-versions'].addObject;
  e.str['source'] := id;
  e.str['kind'] := CODES_TIPSVersionKind[kind];
  e.str['mimetype'] := 'application/fhir+json';
  bin := TFslBuffer.Create;
  try
    comp := TFHIRJsonComposer.Create(nil, OutputStylePretty, nil);
    try
      bin.AsText := comp.Compose(ips);
    finally
      comp.free;
    end;
    FContent.Add(id, bin.link);
  finally
    bin.free;
  end;
end;

procedure TIPSWrapper.addAttachment(attachment : TFslBuffer; fn : String; kind : TIPSAttachmentKind);
var
  e : TJsonObject;
begin    
  e := FManifest.forceArr['attachments'].addObject;
  e.str['source'] := fn;
  e.str['kind'] := CODES_TIPSAttachmentKind[kind];
  if attachment.format <> '' then
    e.str['mimetype'] := attachment.format;
  FContent.Add(fn, attachment.link);
end;

class function TIPSWrapper.fromStream(stream: TStream): TIPSWrapper;
begin
  raise Exception.Create('Not done yet');
end;

class function TIPSWrapper.fromStream(stream: TFslStream): TIPSWrapper;
begin
  raise Exception.Create('Not done yet');
end;

procedure TIPSWrapper.saveToStream(stream: TStream);
begin
  raise Exception.Create('Not done yet');
end;

procedure TIPSWrapper.saveToStream(stream: TFslStream);
begin
  raise Exception.Create('Not done yet');
end;


procedure Decode(const sSource: TBytes);
var
  buffer : TFslBuffer;
  zip : TFslZipPartList;
  reader : TFslZipReader;
  ss : TFslStringStream;
  i : integer;
Begin
  ss := TFslStringStream.Create;
  try
    ss.Bytes := sSource;
    zip := TFslZipPartList.Create;
    reader := TFslZipReader.Create;
    try
      reader.Stream := ss.Link;
      reader.Parts := zip.Link;
      reader.ReadZip;
      for i := 0 to zip.Count - 1 do
        Logging.log(zip[i].Name);
    finally
      reader.free;
    end;
  finally
    ss.free;
  end;
end;

function TIPSWrapper.saveToBytes: TBytes;
var
  ss : TFslStringStream;
  zip : TFslZipPartList;
  writer : TFslZipWriter;
  name : String;
begin
  zip := TFslZipPartList.Create;
  try
    zip.Add('manifest.json', WriteManifest);
    for name in FContent.Keys do
      zip.Add(name, FContent[name].AsBytes);
    writer := TFslZipWriter.Create;
    try
      writer.Parts := zip.Link;
      ss := TFslStringStream.Create;
      try
        writer.Stream := ss.Link;
        writer.WriteZip;
        result := ss.Bytes;
      finally
        ss.free;
      end;
    finally
      writer.free;
    end;
  finally
    zip.free;
  end;
 // decode(result);
end;


end.

