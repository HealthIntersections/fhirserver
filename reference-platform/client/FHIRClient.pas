unit FHIRClient;

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


interface

uses
  SysUtils,
  DateSupport, StringSupport,
  AdvObjects, AdvBuffers,
  FHIRBase, FHIRClientBase, FHIRClientHTTP, FHIRClientThreaded,
  {$IFDEF FHIR2} FHIRClient2, {$ENDIF}
  {$IFDEF FHIR3} FHIRClient3, {$ENDIF}
  {$IFDEF FHIR4} FHIRClient4, {$ENDIF}
  FHIRContext, FHIRConstants, FHIRResources;

Type
  EFHIRClientException = FHIRClientBase.EFHIRClientException;
  THTTPHeaders = FHIRClientBase.THTTPHeaders;
  TFHIRClientLogger = FHIRClientBase.TFHIRClientLogger;
  TFHIRHTTPCommunicator = FHIRClientHTTP.TFHIRHTTPCommunicator;
  TFhirThreadedCommunicator = FHIRClientThreaded.TFhirThreadedCommunicator;
  {$IFDEF FHIR2}
  TFhirClient = FHIRClient2.TFhirClient2;
  {$ENDIF}
  {$IFDEF FHIR3}
  TFhirClient = FHIRClient3.TFhirClient3;
  {$ENDIF}
  {$IFDEF FHIR4}
  TFhirClient = FHIRClient4.TFhirClient4;
  {$ENDIF}

  TFhirClients = class
  public
    class function makeHTTP(worker : TFHIRWorkerContext; url : String; json : boolean) : TFhirClient; overload;
    class function makeHTTP(worker : TFHIRWorkerContext; url : String; json : boolean; timeout : cardinal) : TFhirClient; overload;
    class function makeHTTP(worker : TFHIRWorkerContext; url : String; json : boolean; timeout : cardinal; proxy : String) : TFhirClient; overload;
    class function makeHTTP(worker : TFHIRWorkerContext; url : String; mimeType : String) : TFhirClient; overload;
    class function makeIndy(worker : TFHIRWorkerContext; url : String; json : boolean) : TFhirClient; overload;
    class function makeIndy(worker : TFHIRWorkerContext; url : String; json : boolean; timeout : cardinal) : TFhirClient; overload;

    class function makeThreaded(worker : TFHIRWorkerContext; internal : TFhirClient; event : TThreadManagementEvent) : TFhirClient; overload;
  end;

  TFHIRClientAsyncContextState = (asyncReady, asyncInitialQuery, asyncInitialWait, asyncDelay, asyncPing, asyncPingWait, asyncReadyToDownload, async);

  TFHIRClientAsyncTask = class (TAdvObject)
  private
    FClient : TFhirClientV;
    FFolder: String;
    FTypes: TFhirResourceTypeSet;
    FQuery: string;
    FFormat : TFHIRFormat;
    FSince: TDateTimeEx;
    FLog : String;
    FStart : TDateTime;
    FStatus : TFHIRClientAsyncContextState;
    FTaskLocation : String;
    procedure SetTypes(const Value: TFhirResourceTypeSet);
    procedure log(s : String);
    procedure makeInitialRequest;
  public
    Constructor create(client : TFhirClientV);
    Destructor Destroy; override;

    // setup
    property query : string read FQuery write FQuery;
    property format : TFHIRFormat read FFormat write FFormat;
    property folder : String read FFolder write FFolder;
    property since : TDateTimeEx read FSince write FSince;
    property types : TFhirResourceTypeSet read FTypes write SetTypes;

    // operation
    function logText : String;
    function status : String;
    procedure next;
    function Finished : boolean;
  end;


implementation


{ TFhirClients }

class function TFhirClients.makeHTTP(worker: TFHIRWorkerContext; url: String; json: boolean; timeout : cardinal): TFhirClient;
begin
  result := makeHTTP(worker, url, json, timeout, '');
end;

class function TFhirClients.makeHTTP(worker: TFHIRWorkerContext; url: String; json: boolean; timeout : cardinal; proxy : String): TFhirClient;
var
  http : TFHIRHTTPCommunicator;
begin
  http := TFHIRHTTPCommunicator.Create(url);
  try
    http.timeout := timeout;
    http.proxy := proxy;
    result := TFhirClient.create(worker, 'en', http.link);
    try
      if json then
        result.format := ffJson
      else
        result.format := ffXml;
      result.link;
    finally
      result.Free;
    end;
  finally
    http.free;
  end;
end;

class function TFhirClients.makeHTTP(worker: TFHIRWorkerContext; url: String; json: boolean): TFhirClient;
begin
  result := makeHTTP(worker, url, json, 0);
end;

class function TFhirClients.makeHTTP(worker: TFHIRWorkerContext; url, mimeType: String): TFhirClient;
begin
  result := makeHTTP(worker, url, mimeType.contains('json'));
end;

class function TFhirClients.makeIndy(worker: TFHIRWorkerContext; url: String; json: boolean; timeout: cardinal): TFhirClient;
begin
  result := makeHTTP(worker, url, json, timeout);
  TFHIRHTTPCommunicator(result.Communicator).UseIndy := true;
end;

class function TFhirClients.makeThreaded(worker: TFHIRWorkerContext; internal: TFhirClient; event: TThreadManagementEvent): TFhirClient;
begin
  result := TFhirClient.create(worker, 'en', TFhirThreadedCommunicator.Create(internal, event));
end;

class function TFhirClients.makeIndy(worker: TFHIRWorkerContext; url: String; json: boolean): TFhirClient;
begin
  result := makeIndy(worker, url, json, 0);
end;

{ TFHIRClientAsyncTask }

constructor TFHIRClientAsyncTask.create(client: TFhirClientV);
begin
  inherited Create;
  FClient := client;
  FLog := '';
  FStart := now;
  FStatus := asyncReady;
  log('Initialised at '+FormatDateTime('c', now));
end;

destructor TFHIRClientAsyncTask.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TFHIRClientAsyncTask.Finished: boolean;
begin
  result := false;
end;

procedure TFHIRClientAsyncTask.log(s: String);
begin
  s := DescribePeriod(now - FStart) + ' '+ s + #13#10;
  Flog := s + Flog;
end;

function TFHIRClientAsyncTask.logText : String;
begin
  result := Flog;
end;

procedure TFHIRClientAsyncTask.makeInitialRequest;
var
  headers : THTTPHeaders;
  p, t : String;
  a : TFhirResourceType;
  buf : TAdvBuffer;
begin
  headers.accept := 'application/fhir+json';
  headers.prefer := 'respond-async';
  p := FQuery+'?_outputFormat=application/fhir+ndjson';
  if since.notNull then
    p := p + '&_since='+since.toXML;
  t := '';
  for a := low(TFhirResourceType) to high(TFhirResourceType) do
    if a in types then
      CommaAdd(t, CODES_TFHIRResourceType[a]);
  if t <> '' then
    p := '&_type='+t;
  log('Make request of '+p+' with headers '+headers.asString);
  buf := FClient.custom(p, headers);
  try
    if FClient.LastStatus = 202 then
    begin
      FTaskLocation := FClient.LastHeaders.ContentLocation;
      FStatus := asyncDelay;
      log('Accepted. Task Location = '+FTaskLocation);
    end
    else
      raise Exception.Create('Error?');
  finally
    buf.Free;
  end;
end;

procedure TFHIRClientAsyncTask.next;
begin
  case FStatus of
    asyncReady: makeInitialRequest;
    asyncInitialQuery: raise Exception.Create('Not done yet');
    asyncInitialWait: raise Exception.Create('Not done yet');
    asyncDelay: raise Exception.Create('Not done yet');
    asyncPing: raise Exception.Create('Not done yet');
    asyncPingWait: raise Exception.Create('Not done yet');
    asyncReadyToDownload: raise Exception.Create('Not done yet');
    async: raise Exception.Create('Not done yet');
  end;
end;

procedure TFHIRClientAsyncTask.SetTypes(const Value: TFhirResourceTypeSet);
begin
  FTypes := Value;
end;

function TFHIRClientAsyncTask.status: String;
begin
  case FStatus of
    asyncReady: result := 'Ready to make Request';
    asyncInitialQuery: result := 'Not done yet';
    asyncInitialWait: result := 'Not done yet';
    asyncDelay: result := 'Not done yet';
    asyncPing: result := 'Not done yet';
    asyncPingWait: result := 'Not done yet';
    asyncReadyToDownload: result := 'Not done yet';
    async: result := 'Not done yet';
  end;
  result := 'Status to be determined';
end;

end.

