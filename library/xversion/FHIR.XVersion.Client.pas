unit FHIR.XVersion.Client;

interface

{
This is an R4 client that can actually talk to any server / any version.
}

uses
  SysUtils, Classes,
  FHIR.Support.Stream, FHIR.Support.Json,
  FHIR.Base.Objects, FHIR.Base.Parser,
  FHIR.Client.Base,
  FHIR.R4.Types, FHIR.R4.Resources;

type
  TFhirXVersionClient = class (TFhirClientV)
  public
    constructor Create(worker : TFHIRWorkerContextV; lang : String; communicator : TFHIRClientCommunicator);
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
