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
  FHIR.Support.DateTime, FHIR.Support.Strings,
  FHIR.Support.Objects, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Client.Base, FHIR.Client.HTTP, FHIR.Client.Threaded, FHIR.Base.Parser,
  {$IFDEF FHIR2} FHIR.R2.Client, {$ENDIF}
  {$IFDEF FHIR3} FHIR.R3.Client, {$ENDIF}
  {$IFDEF FHIR4} FHIR.R4.Client, {$ENDIF}
  FHIR.Version.Context, FHIR.Version.Constants, FHIR.Version.Resources;


Type
  EFHIRClientException = FHIR.Client.Base.EFHIRClientException;
  THTTPHeaders = FHIR.Client.Base.THTTPHeaders;
  TFHIRClientLogger = FHIR.Client.Base.TFHIRClientLogger;
  TFHIRHTTPCommunicator = FHIR.Client.HTTP.TFHIRHTTPCommunicator;
  TFhirThreadedCommunicator = FHIR.Client.Threaded.TFhirThreadedCommunicator;
  {$IFDEF FHIR2}
  TFhirClient = FHIR.R2.Client.TFhirClient2;
  {$ENDIF}
  {$IFDEF FHIR3}
  TFhirClient = FHIR.R3.Client.TFhirClient3;
  {$ENDIF}
  {$IFDEF FHIR4}
  TFhirClient = FHIR.R4.Client.TFhirClient4;
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
    result := TFhirClient.create(worker, 'en', http.link);
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
  result := TFhirClient.create(worker, 'en', TFhirThreadedCommunicator.Create(internal, event));
end;

class function TFhirClients.makeIndy(worker: TFHIRWorkerContext; url: String; json: boolean): TFhirClient;
begin
  result := makeIndy(worker, url, json, 0);
end;

end.

