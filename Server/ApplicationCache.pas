unit ApplicationCache;

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
interface

uses
  SysUtils,
  kCritSct,
  AdvObjects, AdvGenerics,
  FHIRResources, FHIRUtilities;

type
  TEndorsement = class (TAdvObject)
  private
    FObservation: TFHIRObservation;
    FOrganization: TFHIROrganization;
    procedure SetObservation(const Value: TFHIRObservation);
    procedure SetOrganization(const Value: TFHIROrganization);

  public
    destructor Destroy; override;
    function link : TEndorsement;
    property Observation : TFHIRObservation read FObservation write SetObservation;
    property Organization : TFHIROrganization read FOrganization write SetOrganization;
  end;

  TApplicationCache = class (TAdvObject)
  private
    FLock : TCriticalSection;

    FById : TAdvMap<TFHIRDevice>;
    FByJWT : TAdvMap<TFHIRDevice>;
    FEndorsements : TAdvMap<TFhirObservation>;
    FOrgs : TAdvMap<TFhirOrganization>;
    procedure seeApplication(app : TFHIRDevice);
    procedure seeObservation(obs : TFHIRObservation);
    procedure seeOrganization(org : TFhirOrganization);
  public
    constructor Create; override;
    destructor Destroy; override;

    function link : TApplicationCache; overload;
    procedure seeResource(res : TFhirResource);
    procedure checkResource(res : TFhirResource);

    function recogniseJWT(jwt : String; endorsements : TAdvList<TEndorsement>): TFHIRDevice;
  end;

implementation

const
  MAGIC_OBS = 'http://healthintersections.com.au/fhir/codes/obs';
  EXT_JWT = 'http://www.healthintersections.com.au/fhir/StructureDefinition/JWT';

{ TApplicationCache }

procedure TApplicationCache.checkResource(res: TFhirResource);
begin

end;

constructor TApplicationCache.Create;
begin
  inherited;
  FLock := TCriticalSection.create('app-cache');
  FById := TAdvMap<TFHIRDevice>.create;
  FByJWT := TAdvMap<TFHIRDevice>.create;
  FEndorsements := TAdvMap<TFhirObservation>.create;
  FOrgs := TAdvMap<TFhirOrganization>.create;
end;

destructor TApplicationCache.Destroy;
begin
  FEndorsements.Free;
  FOrgs.Free;
  FById.Free;
  FByJWT.Free;
  FLock.Free;
  inherited;
end;

function TApplicationCache.link: TApplicationCache;
begin
  result := TApplicationCache(inherited link);
end;

function TApplicationCache.recogniseJWT(jwt : String; endorsements: TAdvList<TEndorsement>): TFHIRDevice;
var
  obs : TFHIRObservation;
  item : TEndorsement;
begin
  FLock.Enter;
  try
    if FByJWT.TryGetValue(jwt, result) then
    begin
      result.Link;
      for obs in FEndorsements.Values do
        if (obs.subject <> nil) and (obs.subject.reference = 'Device/'+result.id) then
        begin
          item := TEndorsement.Create;
          try
            item.Observation := obs.Link;
            if (obs.performerList.Count > 0) and (obs.performerList[0].reference.startsWith('Organization/')) then
              item.Organization := FOrgs[obs.performerList[0].reference.subString(13)].Link;
            endorsements.Add(item.link);
          finally
            item.Free;
          end;
        end;
    end;
  finally
    FLock.Leave;
  end;
end;


procedure TApplicationCache.seeApplication(app: TFHIRDevice);
begin
  if app.type_.hasCode(MAGIC_OBS, 'app') and app.hasExtension(EXT_JWT) then
  begin
    FLock.Enter;
    try
      FById.AddOrSetValue(app.id, app.Link);
      FByJWT.AddOrSetValue(app.getExtensionString(EXT_JWT), app.Link);
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TApplicationCache.seeObservation(obs: TFHIRObservation);
begin
  {$IFDEF FHIR2}
  if (obs.category <> nil) and obs.category.hasCode(MAGIC_OBS, 'endorsement') then
  {$ELSE}
  if obs.categoryList.hasCode[MAGIC_OBS, 'endorsement'] then
  {$ENDIF}
  begin
    FLock.Enter;
    try
      FEndorsements.AddOrSetValue(obs.id, obs.Link);
    finally
      FLock.Leave;
    end;
  end;
end;

procedure TApplicationCache.seeOrganization(org: TFhirOrganization);
begin
  FLock.Enter;
  try
    FOrgs.AddOrSetValue(org.id, org.Link);
  finally
    FLock.Leave;
  end;
end;

procedure TApplicationCache.seeResource(res: TFhirResource);
begin
  if res.ResourceType = frtObservation then
    seeObservation(res as TFhirObservation)
  else if res.ResourceType = frtDevice then
    seeApplication(res as TFhirDevice)
  else if res.ResourceType = frtOrganization then
    seeOrganization(res as TFhirOrganization);
end;

{ TEndorsement }

destructor TEndorsement.Destroy;
begin
  FObservation.Free;
  FOrganization.Free;
  inherited;
end;

function TEndorsement.link: TEndorsement;
begin
  result := TEndorsement(inherited Link);
end;

procedure TEndorsement.SetObservation(const Value: TFHIRObservation);
begin
  FObservation.Free;
  FObservation := Value;
end;

procedure TEndorsement.SetOrganization(const Value: TFHIROrganization);
begin
  FOrganization.Free;
  FOrganization := Value;
end;
end.

