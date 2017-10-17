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

  kCritSct, DateSupport, FileSupport,
  AdvObjects, AdvGenerics, AdvStringLists, AdvCSVFormatters, AdvCSVExtractors, AdvFiles, AdvJson, HashSupport,

  FHIRTypes, FHIRResources, FHIRConstants, FHIRSupport, FHIRUtilities, SCIMObjects,
  FHIRUserProvider, FHIRServerContext, FHIRStorageService, FHIRRestServer, ServerUtilities, WebSourceProvider;

Const
  SYSTEM_ID = 'http://example.org/mrn-id';

Type
  TCSVData = class (TAdvObject)
  private
    FRows : TAdvList<TAdvStringList>;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure load(path : String; names : array of String);
    procedure save(path : String);

    function GetById(id : String) : TAdvStringList;
    function addRow(data : TAdvStringList) : integer;
  end;

  TExampleServerData = class (TObject)
  private
    FLock : TCriticalSection;
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

    function dataFromPatient(pat : TFHIRPatient) : TAdvStringList;
    function patientFromData(data : TAdvStringList) : TFHIRPatient;

    function patientCreate(request: TFHIRRequest; response : TFHIRResponse) : String;
    procedure patientUpdate(request: TFHIRRequest; response : TFHIRResponse);
    function patientRead(request: TFHIRRequest; response : TFHIRResponse) : boolean;
  protected
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;

    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    function ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String; override;
    function ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; override;
  public
    Constructor create(Data : TExampleServerData; lang : String);
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
    function FetchResourceCounts(comps : TAdvList<TFHIRCompartmentId>) : TStringList; override;

    procedure RecordFhirSession(session: TFhirSession); override;
    procedure CloseFhirSession(key: integer); override;
    procedure QueueResource(r: TFhirResource); overload; override;
    procedure QueueResource(r: TFhirResource; dateTime: TDateTimeEx); overload; override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;

    function ProfilesAsOptionList : String; override;

    procedure ProcessSubscriptions; override;
    procedure ProcessObservations; override;
    procedure RunValidation; override;

    function createOperationContext(lang : String) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); override;
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
  FRows := TAdvList<TAdvStringList>.create;
end;

destructor TCSVData.Destroy;
begin
  FRows.Free;
  inherited;
end;

procedure TCSVData.load(path: String; names : array of String);
var
  csv : TAdvCSVExtractor;
  f : TAdvFile;
  ts : TAdvStringList;
  s : String;
begin
  if FileExists(path) then
  begin
    f := TAdvFile.Create(path, fmOpenRead);
    try
      csv := TAdvCSVExtractor.Create(f.Link, TEncoding.UTF8);
      try
        while csv.More do
        begin
         ts := TAdvStringList.Create;
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
    ts := TAdvStringList.Create;
    FRows.Add(ts);
    for s in names do
      ts.Add(s);
  end;
end;

procedure TCSVData.save(path: String);
var
  csv : TAdvCSVFormatter;
  f : TAdvFile;
  ts : TAdvStringList;
begin
  f := TAdvFile.Create(path, fmCreate);
  try
    csv := TAdvCSVFormatter.Create;
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

function TCSVData.addRow(data: TAdvStringList): integer;
begin
  result := FRows.Count;
  FRows.Add(data.Link);
end;

function TCSVData.GetById(id: String): TAdvStringList;
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
  FLock := TCriticalSection.Create;
  FPath := path;
  FPatients := TCSVData.Create;
  FObservations := TCSVData.Create;
  FPatients.load(FileSupport.Path([path, 'patients.csv']), ['Version', 'LastModified', 'MRN', 'Surname', 'First', 'Middle', 'Gender', 'BirthDate', 'Active']);
  FObservations.load(FileSupport.Path([path, 'observations.csv']), ['Version', 'LastModified']);
end;

destructor TExampleServerData.Destroy;
begin
  FPatients.save(FileSupport.Path([Fpath, 'patients.csv']));
  FObservations.save(FileSupport.Path([Fpath, 'observations.csv']));
  FPatients.free;
  FObservations.free;
  FLock.Free;
  inherited;
end;





{ TExampleFhirServer }

constructor TExampleFhirServer.Create;
begin
  inherited Create;
  FIni := TFHIRServerIniFile.Create('');
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
begin
  FData := TExampleServerData.Create('c:\temp');
  FIni.WriteInteger('web', 'http', FPort);
  FIni.WriteString('web', 'host', 'local.fhir.org');
  FIni.WriteString('web', 'base', '/');
  FIni.WriteString('web', 'clients', 'c:\temp\auth.ini');
  FIni.WriteString('admin', 'email', 'grahame@hl7.org');

//  todo: figure out the FTerminologyServer := TTerminologyServer.create(FDB.Link);
//  FTerminologyServer.load(FIni);

  store := TExampleFhirServerStorage.create(FData);
  try
    ctxt := TFHIRServerContext.Create(store.Link);
    try
//      ctxt.TerminologyServer := FterminologyServer.Link;
      ctxt.ownername := FSystemName;
      FWebServer := TFhirWebServer.create(FIni.Link, FSystemname, nil {FTerminologyServer}, ctxt.link);
      FWebServer.SourceProvider := TFHIRWebServerSourceFolderProvider.Create(ProcessPath(ExtractFilePath(FIni.FileName), FIni.ReadString(voVersioningNotApplicable, 'fhir', 'web', '')));
      ctxt.UserProvider := TExampleFHIRUserProvider.Create;
      ctxt.userProvider.OnProcessFile := FWebServer.ReturnProcessedFile;
      FWebServer.AuthServer.UserProvider := ctxt.userProvider.Link;
      FWebServer.OWinSecurityPlain := true;
      FWebServer.Start(true);
    finally
      ctxt.free;
    end;
  finally
    store.free;
  end;
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

procedure TExampleFHIROperationEngine.CommitTransaction;
begin
  FData.FLock.Unlock;
end;

function TExampleFHIROperationEngine.ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String;
begin
  case request.ResourceEnum of
    frtPatient: result := patientCreate(request, response);
  else
    raise Exception.Create('The resource "'+request.ResourceName+'" is not supported by this server');
  end;

end;

function TExampleFHIROperationEngine.ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean;
begin
  case request.ResourceEnum of
    frtPatient: result := patientRead(request, response);
  else
    raise Exception.Create('The resource "'+request.ResourceName+'" is not supported by this server');
  end;
end;


function TExampleFHIROperationEngine.ExecuteUpdate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): Boolean;
begin
  result := true;
  case request.ResourceEnum of
    frtPatient: patientUpdate(request, response);
  else
    raise Exception.Create('The resource "'+request.ResourceName+'" is not supported by this server');
  end;
end;

function TExampleFHIROperationEngine.patientFromData(data: TAdvStringList): TFHIRPatient;
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

function TExampleFHIROperationEngine.dataFromPatient(pat: TFHIRPatient): TAdvStringList;
var
  id : TFhirIdentifier;
begin
  result := TAdvStringList.Create;
  try
    result.add(pat.meta.versionId);
    result.add(pat.meta.lastUpdated.ToHL7);

    id := pat.identifierList.BySystem(SYSTEM_ID);
    if (id = nil) then
      raise ERestfulException.Create('TExampleFHIROperationEngine', 'dataFromPatient', 'A MRN is required', 422, IssueTypeRequired);
    result.Add(id.value);

    if (pat.nameList.IsEmpty) then
      raise ERestfulException.Create('TExampleFHIROperationEngine', 'dataFromPatient', 'A name is required', 422, IssueTypeRequired);
    if (pat.nameList[0].family = '') then
      raise ERestfulException.Create('TExampleFHIROperationEngine', 'dataFromPatient', 'A family name is required', 422, IssueTypeRequired);
    if (pat.nameList[0].givenList.isEmpty) then
      raise ERestfulException.Create('TExampleFHIROperationEngine', 'dataFromPatient', 'A given name is required', 422, IssueTypeRequired);
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
  data : TAdvStringList;
begin
  if request.Resource.meta = nil then
    request.Resource.meta := TFHIRMeta.Create;
  request.Resource.meta.versionId := '1';
  request.Resource.meta.lastUpdated := TDateTimeEx.makeUTC;

  data := dataFromPatient(request.Resource as TFHIRPatient);
  try
    response.Resource := patientFromData(data);
    result := inttostr(FData.FPatients.addRow(data));
    response.id := result;
    response.Resource.id := result;
    response.versionId := '1';
    response.HTTPCode := 201;
    response.Message := 'Created';
    response.Location := request.baseUrl+request.ResourceName+'/'+result+'/_history/1';
    response.LastModifiedDate := response.Resource.meta.lastUpdated.DateTime;
  finally
    data.Free;
  end;
end;

procedure TExampleFHIROperationEngine.patientUpdate(request: TFHIRRequest; response: TFHIRResponse);
var
  odata, ndata : TAdvStringList;
begin
  odata := FData.FPatients.GetById(request.Id);
  if odata = nil then
    raise ERestfulException.Create('TExampleFHIROperationEngine', 'patientUpdate', 'Cannot update a patient that does not already exist', 422, IssueTypeBusinessRule);
  try
    if request.Resource.meta = nil then
      request.Resource.meta := TFHIRMeta.Create;
    request.Resource.meta.versionId := inttostr(StrToInt(odata[0])+1);
    request.Resource.meta.lastUpdated := TDateTimeEx.makeUTC;

    ndata := dataFromPatient(request.Resource as TFHIRPatient);
    try
      // can't change MRN
      if odata[2] <> ndata[2] then
        raise ERestfulException.Create('TExampleFHIROperationEngine', 'patientUpdate', 'Cannot change a patient''s MRN', 422, IssueTypeBusinessRule);

      response.Resource := patientFromData(ndata);
      response.Id := request.Id;
      response.versionId := ndata[0];
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Location := request.baseUrl+request.ResourceName+'/'+request.id+'/_history/'+response.versionId;
      response.LastModifiedDate := response.Resource.meta.lastUpdated.DateTime;
    finally
      ndata.Free;
    end;
  finally
    odata.Free;
  end;
end;

function TExampleFHIROperationEngine.patientRead(request: TFHIRRequest; response: TFHIRResponse) : boolean;
var
  data : TAdvStringList;
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
  inherited Create;
  FData := Data;
end;

destructor TExampleFhirServerStorage.Destroy;
begin

  inherited;
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


function TExampleFhirServerStorage.FetchResourceCounts(comps : TAdvList<TFHIRCompartmentId>): TStringList;
begin
  FData.FLock.Lock();
  try
    result := TStringList.Create;
    result.AddObject('Patient', TObject(FData.FPatients.FRows.Count));
  finally
      FData.FLock.Unlock;
  end;
end;

function TExampleFhirServerStorage.GetTotalResourceCount: integer;
begin
  result := 1;
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

procedure TExampleFhirServerStorage.QueueResource(r: TFhirResource);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.QueueResource(r: TFhirResource; dateTime: TDateTimeEx);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.RegisterAuditEvent(session: TFhirSession; ip: String);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.RunValidation;
begin
  // nothing in this server
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


end.
