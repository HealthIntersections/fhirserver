unit consent_engine;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

uses
  SysUtils,
  fsl_base,
  fhir_objects, fhir_factory,  fhir_client,
  session;

type
  TConsentOperationStatus = (
    cosReject, // the requested operation cannot proceed, and an operation outcome suitable for the user is available
    cosProceed, // the requested operation is allowed to proceed, but the engine will review each resource before sending to the client
    cosAuthorised); // the engine has nothing to say about the operation  (same as proceed, but the host application need not consult the engine - can use more efficient counting/caching methods)

  TFHIRConsentEngineOperation = class abstract (TFslObject)
  private
    FStatus: TConsentOperationStatus;
    FOutcome: TFHIRResourceV;
    procedure SetOutcome(const Value: TFHIRResourceV);
  public
    destructor Destroy; override;

    // find out the status of the operation
    property status : TConsentOperationStatus read FStatus write FStatus;

    // if the operation is rejected, the details to return to the client as an explanation (may be explicit or not)
    property outcome : TFHIRResourceV read FOutcome write SetOutcome;

    // check whether to add the resource to the response going out to the client.
    //   result = null: don't send the resource to the client.
    //   result != null: send the resource to the client.
    // the routine may modify the resource to prepare it for sending to the client
    function addResource(resource : TFHIRResourceV) : TFHIRResourceV; virtual; abstract;

    // adds consent related information to the audit record for the operation
    procedure addToAudit(event : TFHIRResourceV); virtual; abstract;
  end;

  TFHIRConsentEngine = class abstract (TFslObject)
  protected
    FFactory : TFHIRFactory;
  public
    Constructor Create(factory : TFHIRFactory); virtual;
    Destructor Destroy; override;

    // initialises the engine, and provides it with a FHIR API Access to a data store
    // that includes patient consent statements. The engine is trusted to access the
    // store as part of the system
    procedure initialise(client : TFhirClientV); virtual; abstract;

    // notify the engine that a new resource has been added to the store
    // called immediately after the transaction to commit the resource
    // completes (may be in a different thread). The engine can use this
    // to help maintain it's internal cache
    procedure seeResource(resource : TFHIRResourceV); virtual; abstract;

    // notify the engine that an existing resource has been dropped from the store
    // called immediately after the transaction to commit the resource
    // completes (may be in a different thread). The engine can use this
    // to help maintain it's internal cache
    procedure dropResource(resource : TFHIRResourceV); virtual; abstract;

    // call this when any operation is started. return true if the
    // request should be processed. call this *before* user authorization
    // has been checked
    //
    // if the operation is not appropriate, fill out the response, and
    // return false
    function startOperation(request : TFHIRRequest; client : TFHIRClientV) : TFHIRConsentEngineOperation; virtual; abstract;
  end;

  TFHIRNullConsentEngineOperation = class abstract (TFHIRConsentEngineOperation)
  public
    function addResource(resource : TFHIRResourceV) : TFHIRResourceV; override;
    procedure addToAudit(event : TFHIRResourceV); override;
  end;

  TFHIRNullConsentEngine = class abstract (TFHIRConsentEngine)
  public
    procedure initialise(client : TFhirClientV); override;
    procedure seeResource(resource : TFHIRResourceV); override;
    procedure dropResource(resource : TFHIRResourceV); override;
    function startOperation(request : TFHIRRequest; client : TFHIRClientV) : TFHIRConsentEngineOperation; override;
  end;

implementation

{ TFHIRConsentEngineOperation }

destructor TFHIRConsentEngineOperation.Destroy;
begin
  FOutcome.Free;
  inherited;
end;

procedure TFHIRConsentEngineOperation.SetOutcome(const Value: TFHIRResourceV);
begin
  FOutcome.Free;
  FOutcome := Value;
end;

{ TFHIRNullConsentEngineOperation }

function TFHIRNullConsentEngineOperation.addResource(resource: TFHIRResourceV): TFHIRResourceV;
begin
  result := resource.link;
end;

procedure TFHIRNullConsentEngineOperation.addToAudit(event: TFHIRResourceV);
begin
  // nothing
end;

{ TFHIRNullConsentEngine }

procedure TFHIRNullConsentEngine.dropResource(resource: TFHIRResourceV);
begin
  // nothing
end;

procedure TFHIRNullConsentEngine.initialise(client: TFhirClientV);
begin
  // nothing
end;

procedure TFHIRNullConsentEngine.seeResource(resource: TFHIRResourceV);
begin
  // nothing
end;

function TFHIRNullConsentEngine.startOperation(request: TFHIRRequest; client: TFHIRClientV): TFHIRConsentEngineOperation;
begin
  result := TFHIRNullConsentEngineOperation.Create;
end;

{ TFHIRConsentEngine }

constructor TFHIRConsentEngine.Create(factory: TFHIRFactory);
begin
  inherited create;
  FFactory := factory;
end;

destructor TFHIRConsentEngine.Destroy;
begin
  FFactory.Free;
  inherited;
end;

end.
