unit post_handlers;

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

interface

uses
  SysUtils, Generics.Collections,
  fsl_base, fsl_utilities,
  fsl_http,
  fhir_objects, 
  server_context, session;

Type
  TFHIRServerPostHandler = class (TFslObject)
  private
    FParams: THTTPParameters;
    FSecure: boolean;
    FContext : TFHIRServerContext;
    FSession : TFhirSession;

    procedure SetParams(const Value: THTTPParameters);
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

    property params : THTTPParameters read FParams write SetParams;
    property secure : boolean read FSecure write FSecure;
    property context : TFHIRServerContext read FContext write SetContext;
    property session : TFHIRSession read FSession write SetSession;

    function execute : TFslStringDictionary; virtual; abstract;
  end;

  !{$IFDEF FHIR3}
  TFHIRServerCoveragePostHandler = class (TFHIRServerPostHandler)
  private
  public
    function execute : TFslStringDictionary; override;
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
    c.display := context.TerminologyServer.getDisplayForCode(THTTPLanguages.create('en'), system, '', code);
    result.Link;
  finally
    result.Free;
  end;
end;



function TFHIRServerPostHandler.buildPeriod(startParam, endParam: String): TFHIRPeriod;
begin
  if (params['startParam') = '') and  (params['endParam') = '') then
    exit(nil)
  else
  begin
    result := TFhirPeriod.Create;
    try
      if (params['startParam') <> '') then
        result.start := TFslDateTime.fromXML(params['startParam'));
      if (params['endParam') <> '') then
        result.end_ := TFslDateTime.fromXML(params['endParam'));
      result.Link;
    finally
      result.Free;
    end;
  end;
end;

function TFHIRServerPostHandler.buildReference(param: String): TFhirReference;
begin
  if (params['param') = '') then
    exit(nil)
  else
  begin
    result := TFhirReference.Create;
    try
      result.reference := params['param');
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


procedure TFHIRServerPostHandler.processIdentifier(list: TFhirIdentifierList; systemParam, valueParam, typeParam: String);
var
  s, v, t : String;
  id : TFhirIdentifier;
begin
  s := params[systemParam);
  v := params[valueParam);
  t := params[typeParam);
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

procedure TFHIRServerPostHandler.SetParams(const Value: THTTPParameters);
begin
  FParams.Free;
  FParams := Value;
end;

procedure TFHIRServerPostHandler.SetSession(const Value: TFHIRSession);
begin
  FSession.Free;
  FSession := Value;
end;

!{$IFDEF FHIR3}

{ TFHIRServerCoveragePostHandler }

function TFHIRServerCoveragePostHandler.execute: TFslStringDictionary;
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
  if params['provenance.name') = '' then
    raise EFHIRException.create('Please provide a name');
  if params['provenance.country') = '' then
    raise EFHIRException.create('Please provide a country');

  prov := TFhirProvenance.Create;
  try
    coverage := TFhirCoverage.Create;
    try
      // build the coverage from the source
      processIdentifier(coverage.identifierList, 'id.system.1',' id.value.1', 'id.type.1');
      processIdentifier(coverage.identifierList, 'id.system.2',' id.value.2', 'id.type.2');
      processIdentifier(coverage.identifierList, 'id.system.3',' id.value.3', 'id.type.3');
      coverage.status := TFhirFmStatusEnum(StringArrayIndexOfSensitive(['', 'active', 'cancelled', 'draft', 'entered-in-error'], params['status')));
      if params['type') <> '' then
        if params['type') = 'pay' then
          coverage.type_ := buildCodeableConcept('http://hl7.org/fhir/coverage-selfpay', params['type'))
        else
          coverage.type_ := buildCodeableConcept('http://hl7.org/fhir/v3/ActCode', params['type'));
      coverage.policyHolder := buildReference('policy');
      coverage.subscriber := buildReference('subscriber');
      coverage.subscriberId := params['subscriberId');
      coverage.beneficiary := buildReference('beneficiary');
      coverage.relationship := buildCodeableConcept('http://hl7.org/fhir/policyholder-relationship', params['relationship'));
      coverage.period := buildPeriod('start', 'end');
      processReference(coverage.payorList, 'payor');

      if params['group.code') <> '' then
        forceGroup.group := params['group.code');
      if params['group.display') <> '' then
        forceGroup.groupDisplay := params['group.display');
      if params['subgroup.code') <> '' then
        forceGroup.subGroup := params['subgroup.code');
      if params['subgroup.display') <> '' then
        forceGroup.subGroupDisplay := params['subgroup.display');
      if params['plan.code') <> '' then
        forceGroup.plan := params['plan.code');
      if params['plan.display') <> '' then
        forceGroup.planDisplay := params['plan.display');
      if params['subplan.code') <> '' then
        forceGroup.subPlan := params['subplan.code');
      if params['subplan.display') <> '' then
        forceGroup.subPlanDisplay := params['subplan.display');
      if params['class.code') <> '' then
        forceGroup.class_ := params['class.code');
      if params['class.display') <> '' then
        forceGroup.classDisplay := params['class.display');
      if params['subclass.code') <> '' then
        forceGroup.subclass := params['subclass.code');
      if params['subclass.display') <> '' then
        forceGroup.subclassDisplay := params['subclass.display');

      coverage.dependent := params['dependent');
      coverage.sequence := params['sequence');
      coverage.order := params['order');
      coverage.network := params['network');

      processExtension('a');
      processExtension('b');
      processExtension('c');
      processExtension('d');
      processExtension('e');
      for i := 1 to 12 do
        processExtension(inttostr(i));

      prov.recorded := TFslDateTime.makeLocal(dtpSec);
      p := TFhirPractitioner.Create;
      prov.containedList.Add(p);
      p.id := 'p1';
      p.nameList.Add(TFhirHumanName.create);
      p.nameList[0].text := params['provenance.name');
      prov.agentList.Append;
      prov.agentList[0].roleList.add(buildCodeableConcept('http://hl7.org/fhir/v3/ParticipationType', 'AUT'));
      prov.agentList[0].who := TFhirReference.Create('#p1');
      l := TFhirLocation.Create;
      prov.containedList.Add(l);
      l.id := 'l1';
      prov.location := TFhirReference.Create('#l1');
      l.name := params['provenance.country');
      coverage.meta := TFhirMeta.create;
      coverage.meta.extensionList.AddExtension('http://www.healthintersections.com.au/fhir/StructureDefinition/source', params['provenance.name')+' @ '+params['provenance.country'));

      // post the coverage
      client := context.Storage.createClient(THTTPLanguages.create('en'), context, FContext.ValidatorContext.Link, FSession.Link);
      try
        client.provenance := prov.Link;
        client.createResource(coverage, id);
      finally
        client.Free;
      end;

      result := TFslStringDictionary.create;
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
