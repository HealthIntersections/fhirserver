unit ServerPostHandlers;

interface

uses
  SysUtils, Generics.Collections,
  StringSupport, DateSupport, EncodeSupport,
  AdvObjects,
  ParseMap,
  FHIRBase, FHIRTypes, FHIRResources, FHIRSupport, FHIRClient, FHIRUtilities,
  FHIRServerContext;

Type
  TFHIRServerPostHandler = class (TAdvObject)
  private
    FParams: TParseMap;
    FSecure: boolean;
    FContext : TFHIRServerContext;
    FSession : TFhirSession;

    procedure SetParams(const Value: TParseMap);
    procedure SetContext(const Value: TFHIRServerContext);
    procedure SetSession(const Value: TFHIRSession);
  protected
    function buildCodeableConcept(system, code : String) : TFhirCodeableConcept;
    function buildReference(param : String) : TFhirReference;
    function buildPeriod(startParam, endParam : String) : TFHIRPeriod;

    procedure processIdentifier(list : TFhirIdentifierList; systemParam, valueParam, typeParam : String);
    procedure processReference(list : TFhirReferenceList; param : String);
  public
    destructor Destroy; override;

    property params : TParseMap read FParams write SetParams;
    property secure : boolean read FSecure write FSecure;
    property context : TFHIRServerContext read FContext write SetContext;
    property session : TFHIRSession read FSession write SetSession;

    function execute : TDictionary<String, String>; virtual;
  end;

  {$IFNDEF FHIR2}
  TFHIRServerCoveragePostHandler = class (TFHIRServerPostHandler)
  private
  public
    function execute : TDictionary<String, String>; override;
  end;
  {$ENDIF}

implementation

{ TFHIRServerPostHandler }

function TFHIRServerPostHandler.buildCodeableConcept(system, code: String): TFhirCodeableConcept;
var
  c : TFhirCoding;
begin
  if code = '' then
    exit(nil);

  result := TFhirCodeableConcept.Create;
  try
    c := result.codingList.Append;
    c.system := system;
    c.code := code;
    c.display := context.TerminologyServer.getDisplayForCode('en', system, '', code);
    result.Link;
  finally
    result.Free;
  end;
end;



function TFHIRServerPostHandler.buildPeriod(startParam, endParam: String): TFHIRPeriod;
begin
  if (params.GetVar('startParam') = '') and  (params.GetVar('endParam') = '') then
    exit(nil)
  else
  begin
    result := TFhirPeriod.Create;
    try
      if (params.GetVar('startParam') <> '') then
        result.start := TDateTimeEx.fromXML(params.GetVar('startParam'));
      if (params.GetVar('endParam') <> '') then
        result.end_ := TDateTimeEx.fromXML(params.GetVar('endParam'));
      result.Link;
    finally
      result.Free;
    end;
  end;
end;

function TFHIRServerPostHandler.buildReference(param: String): TFhirReference;
begin
  if (params.GetVar('param') = '') then
    exit(nil)
  else
  begin
    result := TFhirReference.Create;
    try
      result.reference := params.GetVar('param');
      // todo: look up display
      result.Link;
    finally
      result.Free;
    end;
  end;
end;

destructor TFHIRServerPostHandler.Destroy;
begin
  FSession.Free;
  FContext.Free;
  FParams.Free;
  inherited;
end;

function TFHIRServerPostHandler.execute: TDictionary<String, String>;
begin
 raise Exception.Create('Must override '+className+'.execute');
end;

procedure TFHIRServerPostHandler.processIdentifier(list: TFhirIdentifierList; systemParam, valueParam, typeParam: String);
var
  s, v, t : String;
  id : TFhirIdentifier;
begin
  s := params.GetVar(systemParam);
  v := params.GetVar(valueParam);
  t := params.GetVar(typeParam);
  if (s <> '') or (t <> '') or (v <> '') then
  begin
    id := TFhirIdentifier.Create;
    try
      id.system := s;
      id.value := v;
      if t <> '' then
        if StringArrayExistsSensitive(['UDI', 'SNO', 'SB', 'PLAC', 'FILL'], t) then
          id.type_ := buildCodeableConcept('http://hl7.org/fhir/identifier-type', t)
        else
          id.type_ := buildCodeableConcept('http://hl7.org/fhir/v2/0203', t);
      list.Add(id.Link);
    finally
      id.Free;
    end;
  end;
end;

procedure TFHIRServerPostHandler.processReference(list: TFhirReferenceList; param: String);
var
  ref : TFhirReference;
begin
  ref := buildReference(param);
  try
    if ref <> nil then
      list.Add(ref.Link);
  finally
    ref.Free;
  end;
end;

procedure TFHIRServerPostHandler.SetContext(const Value: TFHIRServerContext);
begin
  FContext.Free;
  FContext := Value;
end;

procedure TFHIRServerPostHandler.SetParams(const Value: TParseMap);
begin
  FParams.Free;
  FParams := Value;
end;

procedure TFHIRServerPostHandler.SetSession(const Value: TFHIRSession);
begin
  FSession.Free;
  FSession := Value;
end;

  {$IFNDEF FHIR2}

{ TFHIRServerCoveragePostHandler }

function TFHIRServerCoveragePostHandler.execute: TDictionary<String, String>;
var
  coverage : TFhirCoverage;
  prov : TFhirProvenance;
  l : TFhirLocation;
  p : TFhirPractitioner;
  client : TFhirClient;
  id : String;
  i : integer;
  function forceGroup : TFhirCoverageGrouping;
  begin
    if coverage.grouping = nil then
      coverage.grouping := TFhirCoverageGrouping.Create;
    result := coverage.grouping;
  end;
  procedure processExtension(id : String);
  var
    n, v, t : String;
    ext : TFHIRExtension;
  begin
    n := params.getvar('ext.'+id+'.name');
    v := params.getvar('ext.'+id+'.value');
    t := params.getvar('ext.'+id+'.type');
    if (n <> '') and (v <> '') then
    begin
      ext := coverage.extensionList.Append;
      ext.url := 'http://www.healthintersections.com.au/fhir/StructureDefinition/coverage-adhoc-'+EncodeMIME(n);
      if t = 'integer' then
        ext.value := TFhirInteger.Create(v)
      else if t = 'decimal' then
        ext.value := TFhirDecimal.Create(v)
      else if t = 'boolean' then
        ext.value := TFhirBoolean.Create(StrToBool(v))
      else if t = 'Coding/CodeableConcept' then
        ext.value := TFhirCoding.fromEdit(v)
      else if t = 'Quantity' then
        ext.value := TFhirQuantity.fromEdit(v)
      else if t = 'Identifier' then
        ext.value := TFhirIdentifier.fromEdit(v)
      else if t = 'Reference' then
        ext.value := TFhirReference.create(v)
      else if t = 'Period' then
        ext.value := TFhirPeriod.fromEdit(v)
//      else if t = 'Range' then
//        ext.value := TFhirRange.fromEdit(v)
      else
        ext.value := TFhirString.Create(v)
    end;
  end;
begin
  if params.GetVar('provenance.name') = '' then
    raise Exception.Create('Please provide a name');
  if params.GetVar('provenance.country') = '' then
    raise Exception.Create('Please provide a country');

  prov := TFhirProvenance.Create;
  try
    coverage := TFhirCoverage.Create;
    try
      // build the coverage from the source
      processIdentifier(coverage.identifierList, 'id.system.1',' id.value.1', 'id.type.1');
      processIdentifier(coverage.identifierList, 'id.system.2',' id.value.2', 'id.type.2');
      processIdentifier(coverage.identifierList, 'id.system.3',' id.value.3', 'id.type.3');
      coverage.status := TFhirFmStatusEnum(StringArrayIndexOfSensitive(['', 'active', 'cancelled', 'draft', 'entered-in-error'], params.GetVar('status')));
      if params.GetVar('type') <> '' then
        if params.GetVar('type') = 'pay' then
          coverage.type_ := buildCodeableConcept('http://hl7.org/fhir/coverage-selfpay', params.GetVar('type'))
        else
          coverage.type_ := buildCodeableConcept('http://hl7.org/fhir/v3/ActCode', params.GetVar('type'));
      coverage.policyHolder := buildReference('policy');
      coverage.subscriber := buildReference('subscriber');
      coverage.subscriberId := params.GetVar('subscriberId');
      coverage.beneficiary := buildReference('beneficiary');
      coverage.relationship := buildCodeableConcept('http://hl7.org/fhir/policyholder-relationship', params.GetVar('relationship'));
      coverage.period := buildPeriod('start', 'end');
      processReference(coverage.payorList, 'payor');

      if params.GetVar('group.code') <> '' then
        forceGroup.group := params.GetVar('group.code');
      if params.GetVar('group.display') <> '' then
        forceGroup.groupDisplay := params.GetVar('group.display');
      if params.GetVar('subgroup.code') <> '' then
        forceGroup.subGroup := params.GetVar('subgroup.code');
      if params.GetVar('subgroup.display') <> '' then
        forceGroup.subGroupDisplay := params.GetVar('subgroup.display');
      if params.GetVar('plan.code') <> '' then
        forceGroup.plan := params.GetVar('plan.code');
      if params.GetVar('plan.display') <> '' then
        forceGroup.planDisplay := params.GetVar('plan.display');
      if params.GetVar('subplan.code') <> '' then
        forceGroup.subPlan := params.GetVar('subplan.code');
      if params.GetVar('subplan.display') <> '' then
        forceGroup.subPlanDisplay := params.GetVar('subplan.display');
      if params.GetVar('class.code') <> '' then
        forceGroup.class_ := params.GetVar('class.code');
      if params.GetVar('class.display') <> '' then
        forceGroup.classDisplay := params.GetVar('class.display');
      if params.GetVar('subclass.code') <> '' then
        forceGroup.subclass := params.GetVar('subclass.code');
      if params.GetVar('subclass.display') <> '' then
        forceGroup.subclassDisplay := params.GetVar('subclass.display');

      coverage.dependent := params.GetVar('dependent');
      coverage.sequence := params.GetVar('sequence');
      coverage.order := params.GetVar('order');
      coverage.network := params.GetVar('network');

      processExtension('a');
      processExtension('b');
      processExtension('c');
      processExtension('d');
      processExtension('e');
      for i := 1 to 12 do
        processExtension(inttostr(i));

      prov.recorded := TDateTimeEx.makeLocal(dtpSec);
      p := TFhirPractitioner.Create;
      prov.containedList.Add(p);
      p.id := 'p1';
      p.nameList.Add(TFhirHumanName.create);
      p.nameList[0].text := params.GetVar('provenance.name');
      prov.agentList.Append;
      prov.agentList[0].roleList.add(buildCodeableConcept('http://hl7.org/fhir/v3/ParticipationType', 'AUT'));
      prov.agentList[0].who := TFhirReference.Create('#p1');
      l := TFhirLocation.Create;
      prov.containedList.Add(l);
      l.id := 'l1';
      prov.location := TFhirReference.Create('#l1');
      l.name := params.GetVar('provenance.country');
      coverage.meta := TFhirMeta.create;
      coverage.meta.extensionList.AddExtension('http://www.healthintersections.com.au/fhir/StructureDefinition/source', params.GetVar('provenance.name')+' @ '+params.GetVar('provenance.country'));

      // post the coverage
      client := context.Storage.createClient('en', context, FContext.ValidatorContext.Link, FSession.Link);
      try
        client.provenance := prov.Link;
        client.createResource(coverage, id);
      finally
        client.Free;
      end;

      result := TDictionary<String,String>.create;
      result.Add('rid', id);
    finally
      coverage.Free;
    end;
  finally
    prov.Free;
  end;
end;
{$ENDIF}


end.
