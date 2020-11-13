unit simple_consent_engine;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{$I fhir.inc}


{
This unit implements a simple consent engine that implements that following aspects of the Consent resource:

- active consent agreemnts only
  - ordered by date descending, and
- policy applies to patient
- base policy, opt-in, opt-out
- provisions:
   - type
   - period
   - author/source and information recipient
   - action
   - data period

This engine uses the latest matching consent statement
}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  fsl_base, fsl_utilities, fsl_threads,
  fhir_objects, fhir_factory, fhir_common, fhir_client,
  consent_engine, session;

type
  TConsentComparer = class (TFslComparer<TFhirConsentW>)
  private
  public
    function compare(const Left, Right: TFhirConsentW): Integer; override;
  end;

  TConsentCache = class (TFslObject)
  private
    FLock : TFslLock;
    FIdMap : TDictionary<String, String>; // records patients for consents, for updates that change patient (rare, but allowed)
    FCache : TFslMap<TFslList<TFhirConsentW>>;
    procedure dropConsentById(patId, consentId : string);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure seeConsent(consent : TFhirConsentW);
    procedure dropConsent(id : string);

    function getConsents(patientId : String) : TFslList<TFhirConsentW>;
  end;

  TFHIRSimpleConsentEngineOperation = class abstract (TFHIRConsentEngineOperation)
  public
    function addResource(resource : TFHIRResourceV) : TFHIRResourceV; override;
    procedure addToAudit(event : TFHIRResourceV); override;
  end;

  TFHIRSimpleConsentEngine = class abstract (TFHIRConsentEngine)
  private
    FCache : TConsentCache;
  public
    constructor Create(factory : TFHIRFactory); override;
    destructor Destroy; override;

    procedure initialise(client : TFhirClientV); override;
    procedure seeResource(resource : TFHIRResourceV); override;
    function startOperation(request : TFHIRRequest; client : TFHIRClientV) : TFHIRConsentEngineOperation; override;
  end;


implementation

{ TFHIRSimpleConsentEngineOperation }

function TFHIRSimpleConsentEngineOperation.addResource(resource: TFHIRResourceV): TFHIRResourceV;
begin
  result := nil;
end;

procedure TFHIRSimpleConsentEngineOperation.addToAudit(event: TFHIRResourceV);
begin
  inherited;

end;

{ TFHIRSimpleConsentEngine }

constructor TFHIRSimpleConsentEngine.Create(factory : TFHIRFactory);
begin
  inherited;
  FCache := TConsentCache.create;
end;

destructor TFHIRSimpleConsentEngine.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TFHIRSimpleConsentEngine.initialise(client: TFhirClientV);
begin
  inherited;

end;

procedure TFHIRSimpleConsentEngine.seeResource(resource: TFHIRResourceV);
var
  consent : TFhirConsentW;
begin
  if resource.fhirType = 'Consent' then
  begin
    consent := FFactory.wrapConsent(resource.link);
    try
      if consent.active then
        FCache.seeConsent(consent)
      else
        FCache.dropConsent(consent.id);
    finally
      consent.Free;
    end;
  end;
end;

function TFHIRSimpleConsentEngine.startOperation(request: TFHIRRequest;
  client: TFHIRClientV): TFHIRConsentEngineOperation;
begin
  result := nil;
end;

{ TConsentCache }

constructor TConsentCache.Create;
begin
  inherited;
  FLock := TFslLock.Create('consentStore');
  FCache := TFslMap<TFslList<TFhirConsentW>>.create('cache');
end;

destructor TConsentCache.Destroy;
begin
  FCache.Free;
  FLock.Free;
  inherited;
end;

procedure TConsentCache.dropConsent(id : string);
var
  patId : String;
begin
  FLock.Lock;
  try
    if FIdMap.TryGetValue(id, patId) then
      dropConsentById(patId, id);
  finally
    FLock.Unlock;
  end;
end;

procedure TConsentCache.dropConsentById(patId, consentId: string);
var
  list : TFslList<TFhirConsentW>;
begin
  Assert(FLock.LockedToMe);

  if FCache.TryGetValue(patId, list) then
    list.RemoveAll(
      function(item : TFhirConsentW):boolean
      begin
        result := item.Resource.id = consentId;
      end);
end;

procedure TConsentCache.seeConsent(consent: TFhirConsentW);
var
  id, oldId : String;
  list : TFslList<TFhirConsentW>;
begin
  id := consent.patient;
  if (id.StartsWith('Patient/')) then
  begin
    id := id.Substring(8);
    FLock.Lock;
    try
      if FIdMap.TryGetValue(consent.Resource.id, oldId) then
        dropConsentById(oldId, consent.Resource.id);
      FIdMap.AddOrSetValue(consent.Resource.id, id);
      if not FCache.TryGetValue(id, list) then
      begin
        list := TFslList<TFhirConsentW>.create();
        FCache.add(id, list);
      end;
      list.RemoveAll(
        function(item : TFhirConsentW):boolean
        begin
          result := item.Resource.id = consent.Resource.id;
        end);
      list.Add(consent.link);
   finally
      FLock.Unlock;
    end;
  end;
end;

function TConsentCache.getConsents(patientId: String): TFslList<TFhirConsentW>;
var
  list : TFslList<TFhirConsentW>;
begin
  result := TFslList<TFhirConsentW>.create;
  try
    FLock.Lock;
    try
      if FCache.TryGetValue(patientId, list) then
        result.AddAll(list);
    finally
      FLock.Unlock;
    end;
    result.link;
  finally
    result.Free;
  end;
end;

function TConsentComparer.Compare(const Left, Right: TFhirConsentW): Integer;
begin
  result := right.dateTime.compare(left.dateTime); // order reversal - because order in descending order
end;


end.
