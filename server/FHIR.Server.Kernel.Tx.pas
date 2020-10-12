unit FHIR.Server.Kernel.Tx;

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
  SysUtils, Classes, IOUtils,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Logging, FHIR.Support.Json,
  FHIR.Ucum.Services, FHIR.Web.Parsers,
  FHIR.Base.Objects, FHIR.Base.Lang, FHIR.Base.Factory, FHIR.Base.PathEngine, FHIR.Base.Parser, FHIR.Base.Common, FHIR.Base.Utilities,
  {$IFNDEF NO_JS}FHIR.Javascript.Base, {$ENDIF}
  FHIR.Npm.Cache, FHIR.Npm.Package,

  FHIR.R2.Factory, FHIR.R3.Factory, FHIR.R4.Factory, FHIR.R5.Factory,
  FHIR.R2.IndexInfo, FHIR.R3.IndexInfo, FHIR.R4.IndexInfo, FHIR.R5.IndexInfo,
  FHIR.R2.Validator, FHIR.R3.Validator, FHIR.R4.Validator, FHIR.R5.Validator,
  FHIR.Server.ValidatorR2, FHIR.Server.ValidatorR3, FHIR.Server.ValidatorR4, FHIR.Server.ValidatorR5,

  FHIR.Tools.Indexing,
  FHIR.Database.Manager,
  FHIR.Base.Scim,
  FHIR.Tx.Manager, FHIR.Tx.Server,
  FHIR.Server.Storage, FHIR.Server.Context, FHIR.Server.Session, FHIR.Server.UserMgr, FHIR.Server.Ini,
  FHIR.Server.Indexing, FHIR.Server.Factory, FHIR.Server.Subscriptions, FHIR.Server.Web,
  FHIR.Server.Kernel.Base;

type
  TTerminologyServerFactory = class (TFHIRServerFactory)
  private
    FVersion : TFHIRVersion;

  public
    constructor Create(version : TFHIRVersion);
    function makeIndexes : TFHIRIndexBuilder; override;
    function makeValidator: TFHIRValidatorV; override;
    function makeIndexer : TFHIRIndexManager; override;
    function makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager; override;
    function makeEngine(validatorContext : TFHIRWorkerContextWithFactory; ucum : TUcumServiceImplementation) : TFHIRPathEngineV; override;

    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); override;
  end;

  TTerminologyServerData = class (TFslObject)
  private
    FCodeSystems : TFslMap<TFHIRCodeSystemW>;
    FValueSets : TFslMap<TFHIRValueSetW>;
    FNamingSystems : TFslMap<TFHIRNamingSystemW>;
    FConceptMaps : TFslMap<TFHIRConceptMapW>;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TTerminologyServerOperationEngine = class (TFHIROperationEngine)
  private
    FData : TTerminologyServerData;

  protected
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    procedure processGraphQL(graphql: String; request : TFHIRRequest; response : TFHIRResponse); override;

    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse); override;
    function ExecuteOperation(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String; override;
  public
    constructor Create(Data : TTerminologyServerData; const lang : THTTPLanguages);

    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; override;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResourceV; override;
    function getResourceByUrl(aType : string; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResourceV; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    function patientIds(request : TFHIRRequest; res : TFHIRResourceV) : TArray<String>; override;
  end;

  TTerminologyFhirServerStorage = class (TFHIRStorageService)
  private
    FData : TTerminologyServerData;
    FCache : TFHIRPackageManager;
    FServerContext : TFHIRServerContext; // free from owner
    function loadfromUTG(factory : TFHIRFactory; folder : String) : integer;
    procedure loadResource(res: TFHIRResourceV);
  protected
    function GetTotalResourceCount: integer; override;
  public
    constructor Create(factory : TFHIRFactory); Override;
    destructor Destroy; override;
    function link : TTerminologyFhirServerStorage; overload;

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

    procedure SetupRecording(session : TFhirSession); override;
    procedure RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception); override;
    procedure FinishRecording(); override;

    procedure loadUTGFolder(factory : TFHIRFactory; folder : String);
    procedure loadPackage(factory : TFHIRFactory; pid : String);

  end;

  TTerminologyFHIRUserProvider = class (TFHIRUserProvider)
  public
    Function loadUser(key : integer) : TSCIMUser; overload; override;
    Function loadUser(id : String; var key : integer) : TSCIMUser; overload; override;
    function CheckLogin(username, password : String; var key : integer) : boolean; override;
    function CheckId(id : String; var username, hash : String) : boolean; override;
    function loadOrCreateUser(id, name, email : String; var key : integer) : TSCIMUser; override;
    function allowInsecure : boolean; override;
  end;

  TFHIRServiceTxServer = class (TFHIRServiceDataStore)
  private
    FStores : TFslMap<TTerminologyFhirServerStorage>;

    procedure registerEndPoint(code, path : String; db : TFslDbManager; factory : TFHIRFactory; packages : TStringList; UTGFolder : String);
  protected
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

{ TTerminologyServerFactory }

function TTerminologyServerFactory.makeIndexes: TFHIRIndexBuilder;
begin
  case FVersion of
    fhirVersionRelease2 : result := TFHIRIndexBuilderR2.create;
    fhirVersionRelease3 : result := TFHIRIndexBuilderR3.create;
    fhirVersionRelease4 : result := TFHIRIndexBuilderR4.create;
    fhirVersionRelease5 : result := TFHIRIndexBuilderR5.create;
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating index information');
  end;
end;

function TTerminologyServerFactory.makeValidator: TFHIRValidatorV;
begin
  case FVersion of
    fhirVersionRelease2 : result := TFHIRValidator2.Create(TFHIRServerWorkerContextR2.Create(TFHIRFactoryR2.create));
    fhirVersionRelease3 : result := TFHIRValidator3.Create(TFHIRServerWorkerContextR3.Create(TFHIRFactoryR3.create));
    fhirVersionRelease4 : result := TFHIRValidator4.Create(TFHIRServerWorkerContextR4.Create(TFHIRFactoryR4.create));
    fhirVersionRelease5 : result := TFHIRValidator5.Create(TFHIRServerWorkerContextR5.Create(TFHIRFactoryR5.create));
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating Validator');
  end;
end;

function TTerminologyServerFactory.makeIndexer: TFHIRIndexManager;
begin
  raise EFslException.Create('Not supported in this server');
end;

function TTerminologyServerFactory.makeSubscriptionManager(ServerContext: TFslObject): TSubscriptionManager;
begin
  raise EFslException.Create('Not supported in this server');
end;

constructor TTerminologyServerFactory.Create(version: TFHIRVersion);
begin
  inherited Create;
  FVersion := version;
end;

function TTerminologyServerFactory.makeEngine(validatorContext: TFHIRWorkerContextWithFactory; ucum: TUcumServiceImplementation): TFHIRPathEngineV;
begin
  raise EFslException.Create('Not supported in this server');
end;

procedure TTerminologyServerFactory.setTerminologyServer(validatorContext: TFHIRWorkerContextWithFactory; server: TFslObject);
begin
  case FVersion of
    fhirVersionRelease2 : TFHIRServerWorkerContextR2(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease3 : TFHIRServerWorkerContextR3(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease4 : TFHIRServerWorkerContextR4(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease5 : TFHIRServerWorkerContextR5(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Setting Terminology Server');
  end;
end;


{ TTerminologyServerData }

constructor TTerminologyServerData.Create;
begin
  inherited create;
  FCodeSystems := TFslMap<TFHIRCodeSystemW>.create('FHIR Tx Kernel');
  FValueSets := TFslMap<TFHIRValueSetW>.create('FHIR Tx Kernel');
  FNamingSystems := TFslMap<TFHIRNamingSystemW>.create('FHIR Tx Kernel');
  FConceptMaps := TFslMap<TFHIRConceptMapW>.create('FHIR Tx Kernel');
end;

destructor TTerminologyServerData.Destroy;
begin
  FConceptMaps.Free;
  FNamingSystems.Free;
  FValueSets.Free;
  FCodeSystems.Free;

  inherited;
end;

{ TTerminologyServerOperationEngine }

constructor TTerminologyServerOperationEngine.Create(Data : TTerminologyServerData; const lang : THTTPLanguages);
begin
  inherited Create(nil, lang);
  FData := data;
end;

procedure TTerminologyServerOperationEngine.StartTransaction;
begin
  raise ENotImplemented.Create('Transactions are not implemented in this server');
end;

procedure TTerminologyServerOperationEngine.CommitTransaction;
begin
  raise ENotImplemented.Create('Transactions are not implemented in this server');
end;

procedure TTerminologyServerOperationEngine.RollbackTransaction;
begin
  raise ENotImplemented.Create('Transactions are not implemented in this server');
end;

function TTerminologyServerOperationEngine.patientIds(request: TFHIRRequest; res: TFHIRResourceV): TArray<String>;
begin
  setLength(result, 0);
end;

procedure TTerminologyServerOperationEngine.processGraphQL(graphql: String; request: TFHIRRequest; response: TFHIRResponse);
begin
end;

function TTerminologyServerOperationEngine.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
begin
  raise ETodo.Create('Not done yet');
end;

function TTerminologyServerOperationEngine.GetResourceById(request: TFHIRRequest; aType, id, base: String; var needSecure: boolean): TFHIRResourceV;
begin
  raise ETodo.Create('Not done yet');
end;

function TTerminologyServerOperationEngine.getResourceByUrl(aType, url, version: string; allowNil: boolean; var needSecure: boolean): TFHIRResourceV;
begin
  raise ETodo.Create('Not done yet');
end;

function TTerminologyServerOperationEngine.ExecuteOperation(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): String;
begin
  raise ETodo.Create('Not done yet');
end;

function TTerminologyServerOperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse; ignoreHeaders: boolean): boolean;
begin
  raise ETodo.Create('Not done yet');
end;

procedure TTerminologyServerOperationEngine.ExecuteSearch(request: TFHIRRequest;response: TFHIRResponse);
begin
  raise ETodo.Create('Not done yet');
end;

procedure TTerminologyServerOperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; httpCode: Integer; name, message: String; patients: TArray<String>);
begin
  // todo... what?
end;

procedure TTerminologyServerOperationEngine.AuditRest(session: TFhirSession; intreqid, extreqid, ip, resourceName, id, ver: String; verkey: integer; op: TFHIRCommandType; provenance: TFhirProvenanceW; opName: String; httpCode: Integer; name, message: String; patients: TArray<String>);
begin
  // todo... what?
end;

{ TTerminologyFhirServerStorage }

constructor TTerminologyFhirServerStorage.Create(factory : TFHIRFactory);
begin
  inherited Create(factory);
  FData := TTerminologyServerData.create;
end;

destructor TTerminologyFhirServerStorage.Destroy;
begin
  FData.Free;
  FCache.Free;
  inherited;
end;

procedure TTerminologyFhirServerStorage.fetchClients(list: TFslList<TRegisteredClientInformation>);
begin
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyFhirServerStorage.fetchLoadedPackage(id: String): TBytes;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.RecordExchange(req: TFHIRRequest; resp: TFHIRResponse; e: exception);
begin
end;

procedure TTerminologyFhirServerStorage.RecordFhirSession(session: TFhirSession);
begin
  // this server doesn't track sessions
end;

procedure TTerminologyFhirServerStorage.recordPackageLoaded(id, ver: String; count: integer; blob: TBytes);
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.CloseFhirSession(key: integer);
begin
  // this server doesn't track sessions
end;

function TTerminologyFhirServerStorage.createOperationContext(const lang : THTTPLanguages): TFHIROperationEngine;
begin
  result := TTerminologyServerOperationEngine.create(FData, lang);
end;

function TTerminologyFhirServerStorage.FetchResource(key: integer): TFHIRResourceV;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.FetchResourceCounts(compList : TFslList<TFHIRCompartmentId>; counts : TStringList);
begin
  counts.AddObject('CodeSystem', TObject(FData.FCodeSystems.Count));
  counts.AddObject('ValueSet', TObject(FData.FValueSets.Count));
  counts.AddObject('NamingSystem', TObject(FData.FNamingSystems.Count));
  counts.AddObject('ConceptMap', TObject(FData.FConceptMaps.Count));
end;

procedure TTerminologyFhirServerStorage.FinishRecording;
begin
  inherited;
end;

function TTerminologyFhirServerStorage.getClientInfo(id: String): TRegisteredClientInformation;
begin
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyFhirServerStorage.getClientName(id: String): string;
begin
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyFhirServerStorage.GetTotalResourceCount: integer;
begin
  result := FData.FCodeSystems.Count + FData.FValueSets.Count + FData.FNamingSystems.Count + FData.FConceptMaps.Count;
end;

function TTerminologyFhirServerStorage.link: TTerminologyFhirServerStorage;
begin
  result := TTerminologyFhirServerStorage(inherited link);
end;

procedure TTerminologyFhirServerStorage.loadResource(res : TFHIRResourceV);
begin
  if res.fhirType = 'CodeSystem' then
  begin
    FData.FCodeSystems.Add(inttostr(FData.FCodeSystems.Count+1), factory.wrapCodeSystem(res.link));
    FServerContext.ValidatorContext.seeResource(res);
  end
  else if res.fhirType = 'ConceptMap' then
  begin
    FData.FConceptMaps.Add(inttostr(FData.FConceptMaps.Count+1), factory.wrapConceptMap(res.link));
    FServerContext.ValidatorContext.seeResource(res);
  end
  else if res.fhirType = 'NamingSystem' then
  begin
    FData.FNamingSystems.Add(inttostr(FData.FNamingSystems.Count+1), factory.wrapNamingSystem(res.link));
    FServerContext.ValidatorContext.seeResource(res);
  end
  else if res.fhirType = 'ValueSet' then
  begin
    FData.FValueSets.Add(inttostr(FData.FValueSets.Count+1), factory.wrapValueSet(res.link));
    FServerContext.ValidatorContext.seeResource(res);
  end;
end;

procedure TTerminologyFhirServerStorage.loadPackage(factory : TFHIRFactory; pid: String);
var
  npm : TNpmPackage;
  s : String;
  res : TFHIRResourceV;
  i : integer;
  p : TFHIRParser;
begin
  Logging.start('Load package '+pid);

  if (FCache = nil) then
    FCache := TFHIRPackageManager.Create(false);
  i := 0;

  p := factory.makeParser(FServerContext.ValidatorContext.link, ffJson, THTTPLanguages.Create('en'));
  try
    npm := FCache.loadPackage(pid);
    try
      for s in npm.listResources(['CodeSystem', 'ValueSet', 'NamingSystem', 'ConceptMap']) do
      begin
        inc(i);
        if (i mod 100 = 0) then
          Logging.continue('.');
        res := p.parseResource(npm.loadBytes(s));
        try
          loadResource(res);
        finally
          res.Free;
        end;
      end;
    finally
      npm.Free;
    end;
  finally
    p.Free;
  end;
  Logging.finish(' '+inttostr(i)+' resources');
end;

function TTerminologyFhirServerStorage.loadPackages: TFslMap<TLoadedPackageInformation>;
begin
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyFhirServerStorage.loadfromUTG(factory : TFHIRFactory; folder : String) : integer;
var
  filename : String;
  procedure load(fn : String);
  var
    res : TFHIRResourceV;
  begin
    inc(result);
    res := factory.makeParser(FServerContext.ValidatorContext.link, ffXml, THTTPLanguages.Create('en')).parseResource(FileToBytes(fn));
    try
      loadResource(res);
    finally
      res.Free;
    end;
  end;
begin
  Logging.continue('.');
  result := 0;
  for filename in TDirectory.GetFiles(folder, '*.xml') do
    load(filename);
  if FolderExists(path([folder, 'codeSystems'])) then
    for filename in TDirectory.GetFiles(path([folder, 'codeSystems']), '*.xml') do
      load(filename);
  if  FolderExists(path([folder, 'valueSets'])) then
    for filename in TDirectory.GetFiles(path([folder, 'valueSets']), '*.xml') do
      load(filename);
end;

procedure TTerminologyFhirServerStorage.loadUTGFolder(factory : TFHIRFactory; folder : String);
var
  count : integer;
begin
  if FolderExists(path([folder, 'input'])) then
    folder := path([folder, 'input']);
  if FolderExists(path([folder, 'sourceOfTruth'])) then
    folder := path([folder, 'sourceOfTruth']);

  Logging.start('Load UTG Folder '+folder);
  count := 0;
  count := count + loadFromUTG(factory, path([folder, 'cimi']));
  count := count + loadFromUTG(factory, path([folder, 'v2']));
  count := count + loadFromUTG(factory, path([folder, 'v3']));
  count := count + loadFromUTG(factory, path([folder, 'external']));
  count := count + loadFromUTG(factory, path([folder, 'fhir']));
  count := count + loadFromUTG(factory, path([folder, 'unified']));
  Logging.finish(inttostr(count)+' resources loaded');
end;

procedure TTerminologyFhirServerStorage.ProcessEmails;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.ProcessObservations;
begin
  // nothing in this server
end;

procedure TTerminologyFhirServerStorage.ProcessSubscriptions;
begin
  // nothing in this server
end;

function TTerminologyFhirServerStorage.ProfilesAsOptionList: String;
begin
  result := '';
end;

procedure TTerminologyFhirServerStorage.QueueResource(session : TFHIRSession; r: TFhirResourceV);
begin
  // nothing in this server
end;

procedure TTerminologyFhirServerStorage.QueueResource(session : TFHIRSession; r: TFhirResourceV; dateTime: TFslDateTime);
begin
  // nothing in this server
end;

procedure TTerminologyFhirServerStorage.RegisterAuditEvent(session: TFhirSession; ip: String);
begin
  // nothing in this server
end;

function TTerminologyFhirServerStorage.RetrieveSession(key: integer; var UserKey, Provider: integer; var Id, Name, Email: String): boolean;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.RunValidation;
begin
  // nothing in this server
end;

procedure TTerminologyFhirServerStorage.SetupRecording(session: TFhirSession);
begin
end;

function TTerminologyFhirServerStorage.storeClient(client: TRegisteredClientInformation; sessionKey: integer): String;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.Sweep;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.Yield(op: TFHIROperationEngine; exception: Exception);
begin
  op.Free;
end;

{ TTerminologyFHIRUserProvider }

function TTerminologyFHIRUserProvider.allowInsecure: boolean;
begin
  result := true;
end;

function TTerminologyFHIRUserProvider.CheckId(id: String; var username, hash: String): boolean;
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

function TTerminologyFHIRUserProvider.CheckLogin(username, password: String; var key : integer): boolean;
begin
  result := (username = 'user') and (HashStringToCode32('Password') = HashStringToCode32(password));
  if result then
    Key := 1;
end;

function TTerminologyFHIRUserProvider.loadOrCreateUser(id, name, email: String; var key: integer): TSCIMUser;
begin
  key := 1;
  result := loadUser(key);
end;

function TTerminologyFHIRUserProvider.loadUser(key: integer): TSCIMUser;
begin
  result := TSCIMUser.Create(TJsonObject.Create);
  result.userName := 'Registered User';
  result.formattedName := 'Registered User';
end;

function TTerminologyFHIRUserProvider.loadUser(id: String; var key: integer): TSCIMUser;
begin
  key := 1;
  result := LoadUser(key);
end;

{ TFHIRServiceTxServer }

destructor TFHIRServiceTxServer.Destroy;
begin
  FStores.Free;
  inherited;
end;

function TFHIRServiceTxServer.setup: boolean;
begin
  FStores := TFslMap<TTerminologyFhirServerStorage>.create('Tx.Stores');
  result := inherited setup;
end;

procedure TFHIRServiceTxServer.registerEndPoint(code, path : String; db : TFslDbManager; factory : TFHIRFactory; packages : TStringList; UTGFolder : String);
var
  s : String;
  store : TTerminologyFhirServerStorage;
begin
  if UTGFolder <> '' then
    Logging.log('Load Terminology EndPoint for '+factory.versionString+'. UTG = "'+UTGFolder+'", Packages = '+packages.CommaText)
  else
    Logging.log('Load Terminology EndPoint for '+factory.versionString+'. Packages = '+packages.CommaText);

  store := TTerminologyFhirServerStorage.Create(factory.link);
  try
    store.FServerContext := TFHIRServerContext.Create(store.Link, TTerminologyServerFactory.create(factory.version));
    store.FServerContext.Globals := Settings.Link;
    store.FServerContext.TerminologyServer := TTerminologyServer.Create(db.link, factory.Link, Terminologies.link);
    store.FServerContext.userProvider := TTerminologyFHIRUserProvider.Create;

    store.loadPackage(factory, factory.corePackage);
    if UTGFolder <> '' then
      store.loadUTGFolder(factory, UTGFolder)
    else
      store.loadPackage(factory, factory.txPackage);
    store.loadPackage(factory, factory.txSupportPackage);
    for s in packages do
      store.loadPackage(factory, s);

    WebServer.registerEndPoint('r4', 'path', store.FServerContext.Link, ini);
    FStores.Add(code, store.link);
  finally
    store.Free;
  end;
end;

procedure TFHIRServiceTxServer.registerEndPoints;
var
  s : String;
  details : TFHIRServerIniComplex;
  factory : TFHIRFactory;
  list : TStringList;
begin
  for s in Ini.endpoints.sortedKeys do
  begin
    details := Ini.endpoints[s];
    Logging.log('Initialise endpoint '+s+' at '+details['path']+' for '+details['version']);

    if details['version'] = 'r2' then
    begin
      factory := TFHIRFactoryR2.Create;
    end
    else if details['version'] = 'r3' then
    begin
      factory := TFHIRFactoryR3.Create;
    end
    else if details['version'] = 'r4' then
    begin
      factory := TFHIRFactoryR4.Create;
    end
    else if details['version'] = 'r5' then
    begin
      factory := TFHIRFactoryR5.Create;
    end
    else
      raise EFslException.Create('Cannot load end-point '+s+' version '+details['version']);
    try
      list := TStringList.create;
      try
        list.CommaText := ini.kernel['packages-'+details['version']];
        registerEndPoint(s, details['path'], Databases[details['database']].Link, factory.link,
           list, ini.kernel['utg-folder']);

      finally
        list.Free;
      end;
    finally
      factory.Free;
    end;
  end;
  WebServer.IsTerminologyServerOnly := true;
end;

procedure TFHIRServiceTxServer.closeDown;
begin
  if FStores <> nil then
    FStores.Clear;
  inherited;
end;

function TFHIRServiceTxServer.WantActive: boolean;
begin
  result := true;
end;

function TFHIRServiceTxServer.WantThreads: boolean;
begin
  result := false;
end;

function TFHIRServiceTxServer.command(cmd: String): boolean;
begin
  result := false;
end;

end.


