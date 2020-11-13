unit fhir2_client;

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
  SysUtils, Classes,
  fhir_objects,  fhir_parser, fhir_client, fhir_common,
  fhir2_parser, fhir2_constants, fhir2_utilities, fhir2_context, fhir2_common,
  fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_other;

Type
  TFhirClient2 = class (TFhirClientV)
  protected
    function opWrapper : TFhirOperationOutcomeWClass; override;
    function getResourceVersionId(res : TFHIRResourceV) : string; override;
    function getBundleClass : TFHIRBundleWClass; override;
  public
    function link : TFhirClient2; overload;
    function version : TFHIRVersion; override;
    function makeParser(fmt : TFHIRFormat) : TFHIRParser; override;
    function makeComposer(fmt : TFHIRFormat; style : TFHIROutputStyle) : TFHIRComposer; override;

    function conformance(summary : boolean) : TFhirCapabilityStatement;
    function transaction(bundle : TFHIRBundle) : TFHIRBundle;
    function createResource(resource : TFhirResource; var id : String) : TFHIRResource;
    function readResource(atype : TFhirResourceType; id : String) : TFHIRResource;
    function vreadResource(atype : TFhirResourceType; id, vid : String) : TFHIRResource;
    function updateResource(resource : TFhirResource) : TFHIRResource; overload;
    procedure deleteResource(atype : TFhirResourceType; id : String);
    function search(allRecords : boolean; params : TStringList) : TFHIRBundle; overload;
    function search(allRecords : boolean; params : string) : TFHIRBundle; overload;
    function search(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle; overload;
    function search(atype : TFhirResourceType; allRecords : boolean; params : string) : TFHIRBundle; overload;
    function searchPost(atype : TFhirResourceType; allRecords : boolean; params : TStringList; resource : TFhirResource) : TFHIRBundle;
    function searchAgain(link : String) : TFHIRBundle; overload;
    function operation(atype : TFhirResourceType; opName : String; params : TFhirParameters) : TFHIRResource; overload;
    function operation(atype : TFhirResourceType; id, opName : String; params : TFhirParameters) : TFHIRResource; overload;
    function historyType(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle;
    function historyInstance(atype : TFhirResourceType; id : String; allRecords : boolean; params : TStringList) : TFHIRBundle;
  end;


implementation

function TFhirClient2.makeParser(fmt : TFHIRFormat) : TFHIRParser;
begin
  result := TFHIRParsers2.parser(Worker.Link as TFHIRWorkerContext, fmt, Lang);
end;

function TFhirClient2.makeComposer(fmt : TFHIRFormat; style : TFHIROutputStyle) : TFHIRComposer;
begin
  result := TFHIRParsers2.composer(Worker.Link as TFHIRWorkerContext, fmt, Lang, style);
end;

function TFhirClient2.opWrapper : TFhirOperationOutcomeWClass;
begin
  result := TFhirOperationOutcome2;
end;

function TFhirClient2.version : TFHIRVersion;
begin
  result := fhirVersionRelease2;
end;

function TFhirClient2.conformance(summary : boolean) : TFhirCapabilityStatement;
begin
  result := conformanceV(summary) as TFhirCapabilityStatement;
end;

function TFhirClient2.transaction(bundle : TFHIRBundle) : TFHIRBundle;
begin
  result := transactionV(bundle) as TFhirBundle;
end;

function TFhirClient2.createResource(resource : TFhirResource; var id : String) : TFHIRResource;
begin
  result := createResourceV(resource, id) as TFhirResource;
end;

function TFhirClient2.readResource(atype : TFhirResourceType; id : String) : TFHIRResource;
begin
  result := readResourceV(CODES_TFHIRResourceType[aType], id) as TFhirResource;
end;

function TFhirClient2.vreadResource(atype : TFhirResourceType; id, vid : String) : TFHIRResource;
begin
  result := VreadResourceV(CODES_TFHIRResourceType[aType], id, vid) as TFhirResource;
end;

function TFhirClient2.updateResource(resource : TFhirResource) : TFHIRResource;
begin
  result := updateResourceV(resource) as TFhirResource;
end;

procedure TFhirClient2.deleteResource(atype : TFhirResourceType; id : String);
begin
  deleteResourceV(CODES_TFHIRResourceType[aType], id);
end;

function TFhirClient2.getBundleClass: TFHIRBundleWClass;
begin
  result := TFHIRBundle2;
end;

function TFhirClient2.getResourceVersionId(res: TFHIRResourceV): string;
var
  resource : TFhirResource;
begin
  resource := res as TFHIRResource;
  if (resource.meta <> nil) then
    result := resource.meta.versionId
  else
    result := '';
end;

function TFhirClient2.search(allRecords : boolean; params : TStringList) : TFHIRBundle;
begin
  result := searchV(allRecords, params) as TFhirBundle;
end;

function TFhirClient2.search(allRecords : boolean; params : string) : TFHIRBundle;
begin
  result := searchV(allRecords, params) as TFHIRBundle;
end;

function TFhirClient2.search(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle;
begin
  result := searchV(CODES_TFHIRResourceType[aType], allRecords, params) as TFhirBundle;
end;

function TFhirClient2.search(atype : TFhirResourceType; allRecords : boolean; params : string) : TFHIRBundle;
begin
  result := searchV(CODES_TFHIRResourceType[aType], allRecords, params) as TFhirBundle;
end;

function TFhirClient2.searchPost(atype : TFhirResourceType; allRecords : boolean; params : TStringList; resource : TFhirResource) : TFHIRBundle;
begin
  result := searchPostV(CODES_TFHIRResourceType[aType], allRecords, params, resource) as TFhirBundle;
end;

function TFhirClient2.searchAgain(link : String) : TFHIRBundle;
begin
  result := searchAgainV(link) as TFhirBundle;
end;

function TFhirClient2.operation(atype : TFhirResourceType; opName : String; params : TFhirParameters) : TFHIRResource;
begin
  result := operationV(CODES_TFHIRResourceType[aType], opName, params) as TFhirResource;
end;

function TFhirClient2.operation(atype : TFhirResourceType; id, opName : String; params : TFhirParameters) : TFHIRResource;
begin
  result := operationV(CODES_TFHIRResourceType[aType], id, opName, params) as TFhirResource;
end;

function TFhirClient2.historyType(atype : TFhirResourceType; allRecords : boolean; params : TStringList) : TFHIRBundle;
begin
  result := historyTypeV(CODES_TFHIRResourceType[aType], allRecords, params) as TFhirBundle;
end;

function TFhirClient2.historyInstance(atype : TFhirResourceType; id : String; allRecords : boolean; params : TStringList) : TFHIRBundle;
begin
  result := historyInstanceV(CODES_TFHIRResourceType[aType], id, allRecords, params) as TFhirBundle;
end;

function TFhirClient2.link: TFhirClient2;
begin
  result := TFhirClient2(inherited link);
end;

end.

