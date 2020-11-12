unit FHIR.Version.Client;

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
  fsl_base, fsl_utilities, fsl_stream,
  fhir_objects, fhir_client, fhir_client_http, fhir_client_threaded, fhir_parser, fhir_factory,
  {$IFDEF FHIR2}
  fhir2_client, fhir2_context;
  {$ENDIF}
  {$IFDEF FHIR3}
  fhir3_client, fhir3_context;
  {$ENDIF}
  {$IFDEF FHIR4}
  fhir4_client, fhir4_context;
  {$ENDIF}


Type
  EFHIRClientException = fhir_client.EFHIRClientException;
  THTTPHeaders = fhir_client.THTTPHeaders;
  TFHIRClientLogger = fhir_client.TFHIRClientLogger;
  TFHIRHTTPCommunicator = fhir_client_http.TFHIRHTTPCommunicator;
  TFhirThreadedCommunicator = fhir_client_threaded.TFhirThreadedCommunicator;
  {$IFDEF FHIR2}
  TFhirClient = fhir2_client.TFhirClient2;
  {$ENDIF}
  {$IFDEF FHIR3}
  TFhirClient = fhir3_client.TFhirClient3;
  {$ENDIF}
  {$IFDEF FHIR4}
  TFhirClient = fhir4_client.TFhirClient4;
  {$ENDIF}

  TFhirClients = class
  public
    class function makeHTTP(worker : TFHIRWorkerContext; url : String; json : boolean) : TFhirClient; overload;
    class function makeHTTP(worker : TFHIRWorkerContext; url : String; json : boolean; timeout : cardinal) : TFhirClient; overload;
    class function makeHTTP(worker : TFHIRWorkerContext; url : String; json : boolean; timeout : cardinal; proxy : String) : TFhirClient; overload;
    class function makeHTTP(worker : TFHIRWorkerContext; url : String; fmt : TFHIRFormat; timeout : cardinal; proxy : String) : TFhirClient; overload;
    class function makeHTTP(worker : TFHIRWorkerContext; url : String; mimeType : String) : TFhirClient; overload;
    class function makeIndy(worker : TFHIRWorkerContext; url : String; json : boolean) : TFhirClient; overload;
    class function makeIndy(worker : TFHIRWorkerContext; url : String; json : boolean; timeout : cardinal) : TFhirClient; overload;

    class function makeThreaded(worker : TFHIRWorkerContext; internal : TFhirClient; event : TThreadManagementEvent) : TFhirClient; overload;
  end;


implementation


{ TFhirClients }

class function TFhirClients.makeHTTP(worker: TFHIRWorkerContext; url: String; json: boolean; timeout : cardinal): TFhirClient;
begin
  result := makeHTTP(worker, url, json, timeout, '');
end;

class function TFhirClients.makeHTTP(worker: TFHIRWorkerContext; url: String; json: boolean; timeout : cardinal; proxy : String): TFhirClient;
begin
  if json then
    result := makeHTTP(worker, url, ffJson, timeout, proxy)
  else
    result := makeHTTP(worker, url, ffXml, timeout, proxy);
end;

class function TFhirClients.makeHTTP(worker: TFHIRWorkerContext; url: String; json: boolean): TFhirClient;
begin
  result := makeHTTP(worker, url, json, 0);
end;

class function TFhirClients.makeHTTP(worker: TFHIRWorkerContext; url, mimeType: String): TFhirClient;
begin
  result := makeHTTP(worker, url, mimeType.contains('json'));
end;

class function TFhirClients.makeHTTP(worker: TFHIRWorkerContext; url: String;
  fmt: TFHIRFormat; timeout: cardinal; proxy: String): TFhirClient;
var
  http : TFHIRHTTPCommunicator;
begin
  http := TFHIRHTTPCommunicator.Create(url);
  try
    http.timeout := timeout;
    http.proxy := proxy;
    if fmt = ffUnspecified then
      fmt := ffJson;

    result := TFhirClient.create(worker, worker.lang, http.link);
    try
      result.format := fmt;
      result.link;
    finally
      result.Free;
    end;
  finally
    http.free;
  end;
end;

class function TFhirClients.makeIndy(worker: TFHIRWorkerContext; url: String; json: boolean; timeout: cardinal): TFhirClient;
begin
  result := makeHTTP(worker, url, json, timeout);
  TFHIRHTTPCommunicator(result.Communicator).UseIndy := true;
end;

class function TFhirClients.makeThreaded(worker: TFHIRWorkerContext; internal: TFhirClient; event: TThreadManagementEvent): TFhirClient;
begin
  result := TFhirClient.create(worker, worker.lang, TFhirThreadedCommunicator.Create(internal, event));
end;

class function TFhirClients.makeIndy(worker: TFHIRWorkerContext; url: String; json: boolean): TFhirClient;
begin
  result := makeIndy(worker, url, json, 0);
end;

end.

