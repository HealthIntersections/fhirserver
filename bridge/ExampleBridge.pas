{
This unit shows an example of how to integrate the FHIR Server into another
application server. It instantiates a FHIR Server, and provides storage
to allow the FHIR server to expose application functionality.

This example FHIR server uses some CSV files to provide somewhat meaningful
services. The intent is that you replace hte CSV stuff with functionality
from your own application

To change FHIR version: change the define in the project options, and change the
paths to the reference implementation in the .dpr

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

Finally, you actually to set the server up, using the boilerplate shown below in
TExampleFhirServer.Start
}

unit ExampleBridge;


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

Uses
  SysUtils, Classes, IniFiles,

  FHIR.Support.Base, FHIR.Support.Threads, FHIR.Support.Utilities,
  FHIR.Support.Collections, FHIR.Support.Json, FHIR.Support.Stream,

  FHIR.Base.Objects, FHIR.Base.Utilities, FHIR.Base.Lang, FHIR.Base.Factory, FHIR.Base.Scim,
  FHIR.R3.Types, FHIR.R3.Resources, FHIR.R3.Constants, FHIR.R3.Utilities, FHIR.R3.Factory,
  FHIR.Tools.Indexing,
  FHIR.Server.Session, FHIR.Server.UserMgr, FHIR.Server.Context, FHIR.Server.Storage, FHIR.Server.Web, FHIR.Server.Utilities, FHIR.Server.WebSource,
  FHIR.Server.Factory, FHIR.Server.Indexing, FHIR.Server.Subscriptions, FHIR.Server.Ini;

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

    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); override;
  end;

  TExampleServerData = class (TObject)
  private
    FLock : TFslLock;
    FPath : String;
    FPatients : TCSVData;
    FObservations : TCSVData;
  public
    constructor Create(path : String);
    destructor Destroy; override;
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
    Constructor create(Data : TExampleServerData; lang : String);

    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; override;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResourceV; override;
    function getResourceByUrl(aType : string; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResourceV; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirResourceV; httpCode : Integer; name, message : String); overload; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirResourceV; opName : String; httpCode : Integer; name, message : String); overload; override;

  end;

  TExampleFhirServerStorage = class (TFHIRStorageService)
  private
    FData : TExampleServerData;
  protected
    function GetTotalResourceCount: integer; override;
  public
    Constructor create(Data : TExampleServerData);
    Destructor Destroy; override;

    // no OAuth Support

    // server total counts:
    procedure FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList); override;

    procedure RecordFhirSession(session: TFhirSession); override;
    procedure CloseFhirSession(key: integer); override;
    procedure QueueResource(r: TFhirResourceV); overload; override;
    procedure QueueResource(r: TFhirResourceV; dateTime: TDateTimeEx); overload; override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;

    function ProfilesAsOptionList : String; override;

    procedure ProcessSubscriptions; override;
    procedure ProcessObservations; override;
    procedure RunValidation; override;

    function createOperationContext(lang : String) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); override;

    procedure Sweep; override;
    function RetrieveSession(key : integer; var UserKey, Provider : integer; var Id, Name, Email : String) : boolean; override;
    procedure ProcessEmails; override;
    function FetchResource(key : integer) : TFHIRResourceV; override;
    function getClientInfo(id : String) : TRegisteredClientInformation; override;
    function getClientName(id : String) : string; override;
    function storeClient(client : TRegisteredClientInformation; sessionKey : integer) : String; override;
    procedure fetchClients(list : TFslList<TRegisteredClientInformation>); override;
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

  TExampleFhirServer = class
  private
    FData : TExampleServerData;
    FIni : TFHIRServerIniFile;
    FSettings : TFHIRServerSettings;
    FIPMask: String;
    FPort: word;
    FIPClient: String;
    FSystemName : String;
    FSystemUID: String;

    FWebServer : TFhirWebServer;
  public
    Constructor Create;
    Destructor Destroy; Override;

    Property Port: word read FPort write FPort;
    Property IPClient : String read FIPClient write FIPCLient;
    Property IPMask : String read FIPMask write FIPMask;
    Property SystemName : String read FSystemName write FSystemName;
    Property SystemUID : String read FSystemUID write FSystemUID;

    Procedure Start;
    Procedure Stop;
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
  FPatients.load(FHIR.Support.Utilities.Path([path, 'patients.csv']), ['Version', 'LastModified', 'MRN', 'Surname', 'First', 'Middle', 'Gender', 'BirthDate', 'Active']);
  FObservations.load(FHIR.Support.Utilities.Path([path, 'observations.csv']), ['Version', 'LastModified']);
end;

destructor TExampleServerData.Destroy;
begin
  FPatients.save(FHIR.Support.Utilities.Path([Fpath, 'patients.csv']));
  FObservations.save(FHIR.Support.Utilities.Path([Fpath, 'observations.csv']));
  FPatients.free;
  FObservations.free;
  FLock.Free;
  inherited;
end;





{ TExampleFhirServer }

constructor TExampleFhirServer.Create;
begin
  inherited Create;
  FIni := TFHIRServerIniFile.Create('example.ini');
  FSettings := TFHIRServerSettings.create;
end;

destructor TExampleFhirServer.Destroy;
begin
  FIni.Free;
  inherited;
end;

procedure TExampleFhirServer.Start;
var
  ctxt : TFHIRServerContext;
  store : TExampleFhirServerStorage;
  s : String;
  details : TFHIRServerIniComplex;
  ep : TFhirWebServerEndpoint;
begin
  FData := TExampleServerData.Create('c:\temp');

  FWebServer := TFhirWebServer.create(FSettings.Link, FSystemname);
  FWebServer.loadConfiguration(FIni);
  FWebServer.SourceProvider := TFHIRWebServerSourceFolderProvider.Create(ProcessPath(ExtractFilePath(FIni.FileName), FIni.web['folder']));
  FWebServer.OWinSecurityPlain := true;

  store := TExampleFhirServerStorage.create(FData);
  try
    ctxt := TFHIRServerContext.Create(store.Link, TExampleServerFactory.create);
    try
      ctxt.Globals := FSettings.Link;
      ctxt.userProvider := TExampleFHIRUserProvider.Create;
      ep := FWebServer.registerEndPoint(s, details['path'], ctxt.Link, FIni);
    finally
      ctxt.Free;
    end;
  finally
    store.Free;
  end;
  FWebServer.Start(true);
end;

procedure TExampleFhirServer.Stop;
begin
  FWebServer.Stop;
  FWebServer.Free;
  FData.Free;
end;

{ TExampleFHIROperationEngine }

constructor TExampleFHIROperationEngine.create(Data: TExampleServerData; lang: String);
begin
  inherited Create(nil, lang);
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

procedure TExampleFHIROperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirResourceV; httpCode: Integer; name, message: String);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFHIROperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirResourceV; opName: String; httpCode: Integer; name, message: String);
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
    result.meta.lastUpdated := TDateTimeEx.fromHL7(data[1]);
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
      result.birthDate := TDateTimeEx.fromHL7(data[7]);
    result.active := data[8] = '1';
    result.Link;
  finally
    result.Free;
  end;
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
  r.meta.lastUpdated := TDateTimeEx.makeUTC;

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
    r.meta.lastUpdated := TDateTimeEx.makeUTC;

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

constructor TExampleFhirServerStorage.Create;
begin
  inherited Create(TFHIRFactoryR3.create);
  FData := Data;
end;

destructor TExampleFhirServerStorage.Destroy;
begin

  inherited;
end;

procedure TExampleFhirServerStorage.fetchClients(list: TFslList<TRegisteredClientInformation>);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TExampleFhirServerStorage.RecordFhirSession(session: TFhirSession);
begin
  // this server doesn't track sessions
end;

procedure TExampleFhirServerStorage.CloseFhirSession(key: integer);
begin
  // this server doesn't track sessions
end;

function TExampleFhirServerStorage.createOperationContext(lang: String): TFHIROperationEngine;
begin
  result := TExampleFHIROperationEngine.create(FData, lang);
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

procedure TExampleFhirServerStorage.QueueResource(r: TFhirResourceV);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.QueueResource(r: TFhirResourceV; dateTime: TDateTimeEx);
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

function TExampleServerFactory.makeIndexer: TFHIRIndexManager;
begin
  raise Exception.Create('Not supported in this server');
end;

function TExampleServerFactory.makeIndexes: TFHIRIndexBuilder;
begin
  raise Exception.Create('Not supported in this server');
end;

function TExampleServerFactory.makeSubscriptionManager(ServerContext: TFslObject): TSubscriptionManager;
begin
  raise Exception.Create('Not supported in this server');
end;

function TExampleServerFactory.makeValidator: TFHIRValidatorV;
begin
  raise Exception.Create('Not supported in this server');
end;

procedure TExampleServerFactory.setTerminologyServer(validatorContext: TFHIRWorkerContextWithFactory; server: TFslObject);
begin
  raise Exception.Create('Not supported in this server');
end;

end.
