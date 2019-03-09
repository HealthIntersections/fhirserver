unit FHIR.Server.SimpleConsentEngine;


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
  SysUtils, Classes, System.Generics.Defaults, System.Generics.Collections,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Threads,
  FHIR.Base.Objects, FHIR.Base.Factory, FHIR.Base.Common, FHIR.Client.Base,
  FHIR.Server.ConsentEngine, FHIR.Server.Session;

type
  TConsentCache = class (TFslObject, IComparer<TFhirConsentW>)
  private
    FLock : TFslLock;
    FIdMap : TDictionary<String, String>; // records patients for consents, for updates that change patient (rare, but allowed)
    FCache : TFslMap<TFslList<TFhirConsentW>>;
    function compare(const Left, Right: TFhirConsentW): Integer;
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

constructor TFHIRSimpleConsentEngine.Create;
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
        FCache.dropConsent(consent);
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
  FCache := TFslMap<TFslList<TFhirConsentW>>.create;;
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
        list := TFslList<TFhirConsentW>.create(self);
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

function TConsentCache.Compare(const Left, Right: TFhirConsentW): Integer;
begin
  result := right.dateTime.compare(left.dateTime); // order reversal - because order in descending order
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

end.
