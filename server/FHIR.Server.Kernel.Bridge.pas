unit FHIR.Server.Kernel.Bridge;

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

{
This unit shows an example of how to integrate the FHIR Server into another
application server. It instantiates a FHIR Server, and provides storage
to allow the FHIR server to expose application functionality.

This example FHIR server uses some CSV files to provide somewhat meaningful
services. The intent is that you replace the CSV routines with functionality
from your own application

There are 3 different classes that you must subclass to implement a server:

TFHIRUserProvider
  - provides user information. At a minimum, you must provide user information about anonymous sessions.
  - if you want to support OAuth (smart on fhir) additional user work is required. OAuth users are defined by the application

TFHIRStorageService
  - provides persistent storage services for the server as a whole
  - as well FetchResourceCounts, you must implement createOperationContext/Yield, which provide you Operation Engine

TFHIROperationEngine
  - responds to the actual service calls from clients
  - note that this runs in the contexts of an Indy HTTP server thread
  - you override any of the execute* methods to provide the functionality you want
  - you also must override the transaction methods, though you are not required to
  - do anything with them

}

interface

Uses
  SysUtils, StrUtils, Classes, IniFiles,
  fsl_base, fsl_utilities, fsl_collections, fsl_threads,
  fsl_stream, fsl_json,
  fsl_http,
  ftx_ucum_services,
  fhir_objects,  fhir_validator, fhir_factory, fhir_pathengine, fhir_utilities, fhir_common, fsl_scim,

  // change which version is implemented by changing these imports
  fhir3_types, fhir3_resources, fhir3_constants, fhir3_utilities, fhir3_factory, fhir3_pathengine,
  fhir3_validator, fhir3_indexinfo, FHIR.Server.ValidatorR3,

  fhir_indexing,
  FHIR.Server.Factory, FHIR.Server.Indexing, FHIR.Server.Subscriptions, FHIR.Server.Session, FHIR.Server.UserMgr,
  FHIR.Server.Context, FHIR.Server.Storage, FHIR.Server.Kernel.Base;

Const
  SYSTEM_ID = 'http://example.org/mrn-id';

Type
  TCSVData = class (TFslObject)
  private
    FRows : TFslList<TFslStringList>;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure load(path : String; names : array of String);
    procedure save(path : String);

    function GetById(id : String) : TFslStringList;
    function addRow(data : TFslStringList) : integer;
  end;

 TExampleServerFactory = class (TFHIRServerFactory)
  public
    function makeIndexes : TFHIRIndexBuilder; override;
    function makeValidator: TFHIRValidatorV; override;
    function makeIndexer : TFHIRIndexManager; override;
    function makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager; override;
    function makeEngine(validatorContext : TFHIRWorkerContextWithFactory; ucum : TUcumServiceImplementation) : TFHIRPathEngineV; override;

    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); override;
  end;

  TExampleFhirServerStorage = class;

  TExampleServerData = class (TFslObject)
  private
    FLock : TFslLock;
    FPath : String;
    FPatients : TCSVData;
    FObservations : TCSVData;
  public
    constructor Create(path : String);
    destructor Destroy; override;
    function link : TExampleServerData;
    procedure save;
  end;

  TExampleFHIROperationEngine = class (TFHIROperationEngine)
  private
    FData : TExampleServerData;

    function dataFromPatient(pat : TFHIRPatient) : TFslStringList;
    function patientFromData(data : TFslStringList) : TFHIRPatient;

    function patientCreate(request: TFHIRRequest; response : TFHIRResponse) : String;
    procedure patientUpdate(request: TFHIRRequest; response : TFHIRResponse);
    function patientRead(request: TFHIRRequest; response : TFHIRResponse) : boolean;
  protected
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    procedure processGraphQL(graphql: String; request : TFHIRRequest; response : TFHIRResponse); override;

    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    function ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String; override;
    function ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; override;
  public
    constructor Create(storage : TExampleFhirServerStorage; serverContext : TFHIRServerContext; Data : TExampleServerData; const lang : THTTPLanguages);

    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; override;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResourceV; override;
    function getResourceByUrl(aType : string; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResourceV; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    function patientIds(request : TFHIRRequest; res : TFHIRResourceV) : TArray<String>; override;
  end;

  TExampleFhirServerStorage = class (TFHIRStorageService)
  private
    FData : TExampleServerData;
    FServerContext : TFHIRServerContext; // not linked
  protected
    function GetTotalResourceCount: integer; override;
  public
    constructor Create(factory : TFHIRFactory); override;
    destructor Destroy; override;
    function Link : TExampleFhirServerStorage; overload;

    // no OAuth Support

    // server total counts:
    procedure FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList); override;

    procedure RecordFhirSession(session: TFhirSession); override;
    procedure CloseFhirSession(key: integer); override;
    procedure QueueResource(session : TFHIRSession; r: TFhirResourceV); overload; override;
    procedure QueueResource(session : TFHIRSession; r: TFhirResourceV; dateTime: TFslDateTime); overload; override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;

    function ProfilesAsOptionList : String; override;

    procedure ProcessSubscriptions; override;
    procedure ProcessObservations; override;
    procedure RunValidation; override;

    function createOperationContext(const lang : THTTPLanguages) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); override;

    procedure Sweep; override;
    function RetrieveSession(key : integer; var UserKey, Provider : integer; var Id, Name, Email : String) : boolean; override;
    procedure ProcessEmails; override;
    function FetchResource(key : integer) : TFHIRResourceV; override;
    function getClientInfo(id : String) : TRegisteredClientInformation; override;
    function getClientName(id : String) : string; override;
    function storeClient(client : TRegisteredClientInformation; sessionKey : integer) : String; override;
    procedure fetchClients(list : TFslList<TRegisteredClientInformation>); override;
    function loadPackages : TFslMap<TLoadedPackageInformation>; override;
    function fetchLoadedPackage(id : String) : TBytes; override;
    procedure recordPackageLoaded(id, ver : String; count : integer; blob : TBytes); override;

    Procedure SetUpRecording(session : TFhirSession); override;
    procedure RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception); override;
    procedure FinishRecording(); override;

  end;

  TExampleFHIRUserProvider = class (TFHIRUserProvider)
  public
    Function loadUser(key : integer) : TSCIMUser; overload; override;
    Function loadUser(id : String; var key : integer) : TSCIMUser; overload; override;
    function CheckLogin(username, password : String; var key : integer) : boolean; override;
    function CheckId(id : String; var username, hash : String) : boolean; override;
    function loadOrCreateUser(id, name, email : String; var key : integer) : TSCIMUser; override;
    function allowInsecure : boolean; override;
  end;

  TFHIRServiceBridgeServer = class (TFHIRServiceBase)
  private
    FData : TExampleServerData;
  protected
    function initialise : boolean; override;
    function setup : boolean; override;
    procedure closeDown; override;
    procedure registerEndPoints; override;
    function WantActive : boolean; override;
    function WantThreads : boolean; override;
  public
    destructor Destroy; override;
    function command(cmd : String) : boolean; override;
  end;

implementation

{ TCSVData }

constructor TCSVData.Create;
begin
  inherited create;
  FRows := TFslList<TFslStringList>.create;
end;

destructor TCSVData.Destroy;
begin
  FRows.Free;
  inherited;
end;

procedure TCSVData.load(path: String; names : array of String);
var
  csv : TFslCSVExtractor;
  f : TFslFile;
  ts : TFslStringList;
  s : String;
begin
  if FileExists(path) then
  begin
    f := TFslFile.Create(path, fmOpenRead);
    try
      csv := TFslCSVExtractor.Create(f.Link, TEncoding.UTF8);
      try
        while csv.More do
        begin
         ts := TFslStringList.Create;
         FRows.Add(ts);
         csv.ConsumeEntries(ts);
         csv.ConsumeLine;
        end;
      finally
        csv.Free;
      end;
    finally
      f.free;
    end;
  end
  else
  begin
    ts := TFslStringList.Create;
    FRows.Add(ts);
    for s in names do
      ts.Add(s);
  end;
end;

procedure TCSVData.save(path: String);
var
  csv : TFslCSVFormatter;
  f : TFslFile;
  ts : TFslStringList;
begin
  f := TFslFile.Create(path, fmCreate);
  try
    csv := TFslCSVFormatter.Create;
    try
      csv.Stream := f.Link;
      csv.Encoding := TEncoding.UTF8;
      for ts in FRows do
      begin
        csv.ProduceEntryStringList(ts);
        csv.ProduceNewLine;
      end;
    finally
      csv.Free;
    end;
  finally
    f.free;
  end;
end;

function TCSVData.addRow(data: TFslStringList): integer;
begin
  result := FRows.Count;
  FRows.Add(data.Link);
end;

function TCSVData.GetById(id: String): TFslStringList;
var
  index : integer;
begin
  index := StrToIntDef(id, -1);
  if (index > 0) and (index < FRows.Count) then
    result := FRows[index].Link
  else
    result := nil;
end;

{ TExampleServerData }

constructor TExampleServerData.Create(path: String);
begin
  inherited Create;
  FLock := TFslLock.Create;
  FPath := path;
  FPatients := TCSVData.Create;
  FObservations := TCSVData.Create;
  FPatients.load(fsl_utilities.Path([path, 'patients.csv']), ['Version', 'LastModified', 'MRN', 'Surname', 'First', 'Middle', 'Gender', 'BirthDate', 'Active']);
  FObservations.load(fsl_utilities.Path([path, 'observations.csv']), ['Version', 'LastModified']);
end;

destructor TExampleServerData.Destroy;
begin
  FPatients.free;
  FObservations.free;
  FLock.Free;
  inherited;
end;

function TExampleServerData.link: TExampleServerData;
begin
  result := TExampleServerData(inherited link);
end;

procedure TExampleServerData.save;
begin
  FPatients.save(fsl_utilities.Path([Fpath, 'patients.csv']));
  FObservations.save(fsl_utilities.Path([Fpath, 'observations.csv']));
end;

{ TExampleFHIROperationEngine }

constructor TExampleFHIROperationEngine.create(storage : TExampleFhirServerStorage; serverContext : TFHIRServerContext; Data: TExampleServerData; const lang : THTTPLanguages);
begin
  inherited Create(storage, serverContext, lang);
  FData := data;
end;

procedure TExampleFHIROperationEngine.StartTransaction;
begin
  FData.FLock.Lock();
end;

procedure TExampleFHIROperationEngine.RollbackTransaction;
begin
  FData.FLock.Unlock;
end;

procedure TExampleFHIROperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; httpCode: Integer; name, message: String; patients : TArray<String>);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFHIROperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; opName: String; httpCode: Integer; name, message: String; patients : TArray<String>);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFHIROperationEngine.CommitTransaction;
begin
  FData.FLock.Unlock;
end;

function TExampleFHIROperationEngine.ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
begin
  if request.ResourceName = 'Patient' then
    result := patientCreate(request, response)
  else
    raise EFHIRException.create('The resource "'+request.ResourceName+'" is not supported by this server');
end;

function TExampleFHIROperationEngine.ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean;
begin
  if request.ResourceName = 'Patient' then
    result := patientRead(request, response)
  else
    raise EFHIRException.create('The resource "'+request.ResourceName+'" is not supported by this server');
end;


function TExampleFHIROperationEngine.ExecuteUpdate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): Boolean;
begin
  result := true;
  if request.ResourceName = 'Patient' then
    patientUpdate(request, response)
  else
    raise EFHIRException.create('The resource "'+request.ResourceName+'" is not supported by this server');
end;

function TExampleFHIROperationEngine.GetResourceById(request: TFHIRRequest; aType, id, base: String; var needSecure: boolean): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFHIROperationEngine.getResourceByUrl(aType: String; url, version: string; allowNil: boolean; var needSecure: boolean): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFHIROperationEngine.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFHIROperationEngine.patientFromData(data: TFslStringList): TFHIRPatient;
begin
  result := TFHIRPatient.Create;
  try
    result.meta := TFHIRMeta.Create;
    result.meta.versionId := data[0];
    result.meta.lastUpdated := TFslDateTime.fromHL7(data[1]);
    with result.identifierList.Append do
    begin
      system := SYSTEM_ID;
      value := data[2];
    end;
    with result.nameList.Append do
    begin
      family := data[3];
      givenList.add(data[4]);
      if (data[5] <> '') then
        givenList.add(data[5]);
    end;
    result.gender := TFhirAdministrativeGenderEnum(StrToIntDef(data[6], 0));
    if (data[7] <> '') then
      result.birthDate := TFslDateTime.fromHL7(data[7]);
    result.active := data[8] = '1';
    result.Link;
  finally
    result.Free;
  end;
end;

function TExampleFHIROperationEngine.patientIds(request: TFHIRRequest; res: TFHIRResourceV): TArray<String>;
begin
  result := nil;
end;

function TExampleFHIROperationEngine.dataFromPatient(pat: TFHIRPatient): TFslStringList;
var
  id : TFhirIdentifier;
begin
  result := TFslStringList.Create;
  try
    result.add(pat.meta.versionId);
    result.add(pat.meta.lastUpdated.ToHL7);

    id := pat.identifierList.BySystem(SYSTEM_ID);
    if (id = nil) then
      raise ERestfulException.Create('TExampleFHIROperationEngine.dataFromPatient', 422, itRequired, 'A MRN is required', lang);
    result.Add(id.value);

    if (pat.nameList.IsEmpty) then
      raise ERestfulException.Create('TExampleFHIROperationEngine.dataFromPatient', 422, itRequired, 'A name is required', lang);
    if (pat.nameList[0].family = '') then
      raise ERestfulException.Create('TExampleFHIROperationEngine.dataFromPatient', 422, itRequired, 'A family name is required', lang);
    if (pat.nameList[0].givenList.isEmpty) then
      raise ERestfulException.Create('TExampleFHIROperationEngine.dataFromPatient', 422, itRequired, 'A given name is required', lang);
    result.Add(pat.nameList[0].family);
    result.Add(pat.nameList[0].givenList[0].value);
    if (pat.nameList[0].givenList.Count > 1) then
      result.Add(pat.nameList[0].givenList[1].value);
    result.add(inttostr(ord(pat.gender)));
    if pat.birthDate.notNull then
      result.add(pat.birthDate.ToHL7);
    if (pat.activeElement = nil) or pat.active then
      result.add('1')
    else
      result.add('0');
    result.Link;
  finally
    result.Free;
  end;

end;

function TExampleFHIROperationEngine.patientCreate(request: TFHIRRequest; response: TFHIRResponse) : String;
var
  data : TFslStringList;
  r : TFHIRPatient;
begin
  r := request.Resource as TFHIRPatient;
  if r.meta = nil then
    r.meta := TFHIRMeta.Create;
  r.meta.versionId := '1';
  r.meta.lastUpdated := TFslDateTime.makeUTC;

  data := dataFromPatient(r);
  try
    response.Resource := patientFromData(data);
    result := inttostr(FData.FPatients.addRow(data));
    response.id := result;
    response.Resource.id := result;
    response.versionId := '1';
    response.HTTPCode := 201;
    response.Message := 'Created';
    response.Location := request.baseUrl+request.ResourceName+'/'+result+'/_history/1';
    response.LastModifiedDate := TFHIRPatient(response.Resource).meta.lastUpdated.DateTime;
  finally
    data.Free;
  end;
end;

procedure TExampleFHIROperationEngine.patientUpdate(request: TFHIRRequest; response: TFHIRResponse);
var
  odata, ndata : TFslStringList;
  r : TFHIRPatient;
begin
  r := request.Resource as TFHIRPatient;
  odata := FData.FPatients.GetById(request.Id);
  if odata = nil then
    raise ERestfulException.Create('TExampleFHIROperationEngine.patientUpdate', 422, itBusinessRule, 'Cannot update a patient that does not already exist', lang);
  try
    if r.meta = nil then
      r.meta := TFHIRMeta.Create;
    r.meta.versionId := inttostr(StrToInt(odata[0])+1);
    r.meta.lastUpdated := TFslDateTime.makeUTC;

    ndata := dataFromPatient(request.Resource as TFHIRPatient);
    try
      // can't change MRN
      if odata[2] <> ndata[2] then
        raise ERestfulException.Create('TExampleFHIROperationEngine.patientUpdate', 422, itBusinessRule, 'Cannot change a patient''s MRN', lang);

      response.Resource := patientFromData(ndata);
      response.Id := request.Id;
      response.versionId := ndata[0];
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Location := request.baseUrl+request.ResourceName+'/'+request.id+'/_history/'+response.versionId;
      response.LastModifiedDate := TFHIRPatient(response.Resource).meta.lastUpdated.DateTime;
    finally
      ndata.Free;
    end;
  finally
    odata.Free;
  end;
end;

procedure TExampleFHIROperationEngine.processGraphQL(graphql: String; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFHIROperationEngine.patientRead(request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  data : TFslStringList;
begin
  result := false;
  data := FData.FPatients.GetById(request.Id);
  if data <> nil then
  begin
    try
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Resource := patientFromData(data);
      result := true;
    finally
      data.Free;
    end;
  end
  else
  begin
    response.HTTPCode := 404;
    response.Message := 'Not Found';
    response.Resource := BuildOperationOutcome(lang, 'not found', IssueTypeUnknown);
  end;
end;

{ TExampleFhirServerStorage }

constructor TExampleFhirServerStorage.Create(factory : TFHIRFactory);
begin
  inherited Create(factory);
end;

destructor TExampleFhirServerStorage.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TExampleFhirServerStorage.fetchClients(list: TFslList<TRegisteredClientInformation>);
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFhirServerStorage.fetchLoadedPackage(id: String): TBytes;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception);
begin
end;

procedure TExampleFhirServerStorage.RecordFhirSession(session: TFhirSession);
begin
  // this server doesn't track sessions
end;

procedure TExampleFhirServerStorage.recordPackageLoaded(id, ver: String; count: integer; blob: TBytes);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.CloseFhirSession(key: integer);
begin
  // this server doesn't track sessions
end;

function TExampleFhirServerStorage.createOperationContext(const lang : THTTPLanguages): TFHIROperationEngine;
begin
  result := TExampleFHIROperationEngine.create(self.Link, FServerContext.link, FData, lang);
end;


function TExampleFhirServerStorage.FetchResource(key: integer): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList);
begin
  FData.FLock.Lock();
  try
    counts.AddObject('Patient', TObject(FData.FPatients.FRows.Count));
  finally
      FData.FLock.Unlock;
  end;
end;

procedure TExampleFhirServerStorage.FinishRecording;
begin
  inherited;
end;

function TExampleFhirServerStorage.getClientInfo(id: String): TRegisteredClientInformation;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFhirServerStorage.getClientName(id: String): string;
begin
  raise EFslException.Create('Not Implemented');
end;

function TExampleFhirServerStorage.GetTotalResourceCount: integer;
begin
  result := 1;
end;


function TExampleFhirServerStorage.Link: TExampleFhirServerStorage;
begin
  result := TExampleFhirServerStorage(inherited link);
end;

function TExampleFhirServerStorage.loadPackages: TFslMap<TLoadedPackageInformation>;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.ProcessEmails;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.ProcessObservations;
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.ProcessSubscriptions;
begin
  // nothing in this server
end;

function TExampleFhirServerStorage.ProfilesAsOptionList: String;
begin
  result := '';
end;

procedure TExampleFhirServerStorage.QueueResource(session : TFHIRSession; r: TFhirResourceV);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.QueueResource(session : TFHIRSession; r: TFhirResourceV; dateTime: TFslDateTime);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.RegisterAuditEvent(session: TFhirSession; ip: String);
begin
  // nothing in this server
end;

function TExampleFhirServerStorage.RetrieveSession(key: integer; var UserKey, Provider: integer; var Id, Name, Email: String): boolean;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.RunValidation;
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.SetupRecording(session: TFhirSession);
begin
end;

function TExampleFhirServerStorage.storeClient(client: TRegisteredClientInformation; sessionKey: integer): String;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.Sweep;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.Yield(op: TFHIROperationEngine; exception: Exception);
begin
  op.Free;
end;

{ TExampleFHIRUserProvider }

function TExampleFHIRUserProvider.allowInsecure: boolean;
begin
  result := true;
end;

function TExampleFHIRUserProvider.CheckId(id: String; var username, hash: String): boolean;
begin
  if (id = 'user') then
  begin
    result := false;
    userName := 'Registered User';
    hash := inttostr(HashStringToCode32('Password'));
  end
  else
    result := false;
end;

function TExampleFHIRUserProvider.CheckLogin(username, password: String; var key : integer): boolean;
begin
  result := (username = 'user') and (HashStringToCode32('Password') = HashStringToCode32(password));
  if result then
    Key := 1;
end;

function TExampleFHIRUserProvider.loadOrCreateUser(id, name, email: String; var key: integer): TSCIMUser;
begin
  key := 1;
  result := loadUser(key);
end;

function TExampleFHIRUserProvider.loadUser(key: integer): TSCIMUser;
begin
  result := TSCIMUser.Create(TJsonObject.Create);
  result.userName := 'Registered User';
  result.formattedName := 'Registered User';
end;

function TExampleFHIRUserProvider.loadUser(id: String; var key: integer): TSCIMUser;
begin
  key := 1;
  result := LoadUser(key);
end;


{ TExampleServerFactory }

function TExampleServerFactory.makeEngine(validatorContext: TFHIRWorkerContextWithFactory; ucum: TUcumServiceImplementation): TFHIRPathEngineV;
begin
  raise EFslException.Create('Not supported in this server');
end;

function TExampleServerFactory.makeIndexer: TFHIRIndexManager;
begin
  raise EFslException.Create('Not supported in this server');
end;

function TExampleServerFactory.makeIndexes: TFHIRIndexBuilder;
begin
  result := TFHIRIndexBuilderR3.create;
end;

function TExampleServerFactory.makeSubscriptionManager(ServerContext: TFslObject): TSubscriptionManager;
begin
  raise EFslException.Create('Not supported in this server');
end;

function TExampleServerFactory.makeValidator: TFHIRValidatorV;
begin
  result := TFHIRValidator3.Create(TFHIRServerWorkerContextR3.Create(TFHIRFactoryR3.create));
end;

procedure TExampleServerFactory.setTerminologyServer(validatorContext: TFHIRWorkerContextWithFactory; server: TFslObject);
begin
  raise EFslException.Create('Not supported in this server');
end;


{ TFHIRServiceBridgeServer }

destructor TFHIRServiceBridgeServer.Destroy;
begin
  FData.Free;
  inherited;
end;

function TFHIRServiceBridgeServer.initialise: boolean;
begin
  result := true;
end;

function TFHIRServiceBridgeServer.setup: boolean;
var
  s : String;
begin
  s := ini.kernel['data-path'];
  if (s = '') then
    s := SystemTemp;
  FData := TExampleServerData.Create(s);
  result := true;
end;

procedure TFHIRServiceBridgeServer.closeDown;
begin
  if (FData <> nil) then
    FData.save;
end;

function TFHIRServiceBridgeServer.command(cmd: String): boolean;
begin
  result := false;
end;

procedure TFHIRServiceBridgeServer.registerEndPoints;
var
  store : TExampleFhirServerStorage;
  ctxt : TFHIRServerContext;
begin
  store := TExampleFhirServerStorage.create(TFHIRFactoryR3.create);
  try
    store.FData := FData.link;
    ctxt := TFHIRServerContext.Create(store.Link, TExampleServerFactory.create);
    try
      store.FServerContext := ctxt;
      ctxt.Globals := Settings.Link;
      ctxt.userProvider := TExampleFHIRUserProvider.Create;
      WebServer.registerEndPoint('example', ini.kernel['path'], ctxt.Link, Ini);
    finally
      ctxt.Free;
    end;
  finally
    store.Free;
  end;
end;

function TFHIRServiceBridgeServer.WantActive: boolean;
begin
  result := true;
end;

function TFHIRServiceBridgeServer.WantThreads: boolean;
begin
  result := false;
end;

end.


