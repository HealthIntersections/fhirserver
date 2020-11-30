unit FHIR.XVersion.Client;

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

{
This is an R4 client that can actually talk to any server / any version.
}

uses
  SysUtils, Classes,
  fsl_stream, fsl_json,
  fhir_objects, fhir_parser,
  fhir_client,
  fhir4_types, fhir4_resources;

type
  TFhirXVersionClient = class (TFhirClientV)
  public
    constructor Create(worker : TFHIRWorkerContextV; const lang : THTTPLanguages; communicator : TFHIRClientCommunicator);
    destructor Destroy; override;
    function link : TFhirClientV; overload;

    function makeParser(fmt : TFHIRFormat) : TFHIRParser; override;
    function makeComposer(fmt : TFHIRFormat; style : TFHIROutputStyle) : TFHIRComposer; override;

    function version : TFHIRVersion; override;

    // version independent API
    function conformance(summary : boolean) : TFHIRResource;
    function transactionV(bundle : TFHIRResource) : TFHIRResource;
    function createResourceV(resource : TFHIRResource; var id : String) : TFHIRResource;
    function readResourceV(atype : TFhirResourceType; id : String) : TFHIRResource;
    function vreadResourceV(atype : TFhirResourceType; id, vid : String) : TFHIRResource;
    function updateResourceV(resource : TFHIRResource) : TFHIRResource; overload;
    function patchResourceV(atype : TFhirResourceType; id : String; params : TFHIRResource) : TFHIRResource; overload;
    function patchResourceV(atype : TFhirResourceType; id : String; patch : TJsonArray) : TFHIRResource; overload;
    procedure deleteResourceV(atype : TFhirResourceType; id : String);
    function searchV(allRecords : boolean; params : TStringList) : TFHIRResource; overload;
    function searchV(allRecords : boolean; params : string) : TFHIRResource; overload;
    function searchV(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRResource; overload;
    function searchV(atype : TFhirResourceType; allRecords : boolean; params : string) : TFHIRResource; overload;
    function searchPostV(atype : TFhirResourceType; allRecords : boolean; params : TStringList; resource : TFHIRResource) : TFHIRResource;
    function searchAgainV(link : String) : TFHIRResource; overload;
    function operationV(atype : TFhirResourceType; opName : String; params : TFHIRResource) : TFHIRResource; overload;
    function operationV(atype : TFhirResourceType; id, opName : String; params : TFHIRResource) : TFHIRResource; overload;
    function historyTypeV(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRResource;
    function historyinstanceV(atype : TFhirResourceType; id : String; allRecords : boolean; params : TStringList) : TFHIRResource;

    // special case that gives direct access to the communicator...
    function customGet(path : String; headers : THTTPHeaders) : TFslBuffer; overload;
    function customPost(path : String; headers : THTTPHeaders; body : TFslBuffer) : TFslBuffer; overload;
    procedure terminate; overload; // only works for some communicators
  end;


implementation

end.
