unit FHIR.Server.TxKernel;

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
  Classes, SysUtils,

  FHIR.Support.Base, FHIR.Support.Threads, FHIR.Support.Utilities, FHIR.Support.Logging,
  FHIR.Support.Collections, FHIR.Support.Json, FHIR.Support.Stream, FHIR.Web.Parsers,
  FHIR.Database.Manager,
  FHIR.Base.Objects, FHIR.Base.Utilities, FHIR.Base.Lang, FHIR.Base.Factory, FHIR.Base.Scim, FHIR.Ucum.Services, FHIR.Base.PathEngine, FHIR.Base.Common,
  FHIR.Cache.PackageManager, FHIR.Cache.NpmPackage,
  FHIR.R4.Resources.Base, FHIR.R4.Resources.Canonical, FHIR.R4.Json, FHIR.R4.Factory, FHIR.R4.Validator, FHIR.R4.Context, FHIR.R4.IndexInfo,
  FHIR.Tools.Indexing,
  FHIR.Tx.Manager, FHIR.Tx.Server,
  FHIR.Server.Session, FHIR.Server.UserMgr, FHIR.Server.Context, FHIR.Server.Storage, FHIR.Server.Web, FHIR.Server.Utilities, FHIR.Server.WebSource,
  FHIR.Server.Factory, FHIR.Server.Indexing, FHIR.Server.Subscriptions, FHIR.Server.Ini,
  FHIR.Server.ValidatorR4, FHIR.Server.IndexingR4;


type
  { TTerminologyServerFactory }
  TTerminologyServerFactory = class (TFHIRServerFactory)
  public
    function makeIndexes : TFHIRIndexBuilder; override;
    function makeValidator: TFHIRValidatorV; override;
    function makeIndexer : TFHIRIndexManager; override;
    function makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager; override;
    function makeEngine(validatorContext : TFHIRWorkerContextWithFactory; ucum : TUcumServiceImplementation) : TFHIRPathEngineV; override;

    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); override;
  end;

  TTerminologyServerData = class (TFslObject)
  private
    FCodeSystems : TFslMap<TFHIRCodeSystem>;
    FValueSets : TFslMap<TFHIRValueSet>;
    FNamingSystems : TFslMap<TFHIRNamingSystem>;
    FConceptMaps : TFslMap<TFHIRConceptMap>;
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
    FJson : TFHIRJsonParser;
    FServerContext : TFHIRServerContext; // free from owner
  protected
    function GetTotalResourceCount: integer; override;
  public
    constructor Create(factory : TFHIRFactory); Override;
    destructor Destroy; override;

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

    procedure loadUTGFolder;
    procedure loadPackage(pid : String);

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

  TTerminologyServerKernel = class (TFslObject)
  private
    FIniFile : String;
    FPackages: TStringList;
    FPort: word;
    FUTGFolder: String;
    FVersion: String;

    FWebServer : TFhirWebServer;
    FSettings : TFHIRServerSettings;
    FTerminologies : TCommonTerminologies;
    FDatabases : TFslMap<TFslDBManager>;
    FStore : TTerminologyFhirServerStorage;
    procedure loadTerminologies(ini : TFHIRServerIniFile);
    procedure ConnectToDatabases(ini : TFHIRServerIniFile);
  public
    constructor Create(iniName : String);
    destructor Destroy; override;

    property Version : String read FVersion write FVersion;
    property UTGFolder : String read FUTGFolder write FUTGFolder; // else load UTG out of the normal package
    property packages : TStringList read FPackages;
    property port : word read FPort write FPort;
    property iniFile : String read FIniFile write FIniFile;

    Procedure Start;
    Procedure Stop;
  end;

implementation


{ TTerminologyServerFactory }

function TTerminologyServerFactory.makeIndexes: TFHIRIndexBuilder;
begin
  result := TFHIRIndexBuilderR4.Create;
end;

function TTerminologyServerFactory.makeValidator: TFHIRValidatorV;
begin
  result := TFHIRValidator4.Create(TFHIRServerWorkerContextR4.Create(TFHIRFactoryR4.create));
end;

function TTerminologyServerFactory.makeIndexer: TFHIRIndexManager;
begin
  raise EFslException.Create('Not supported in this server');
end;

function TTerminologyServerFactory.makeSubscriptionManager(ServerContext: TFslObject): TSubscriptionManager;
begin
  raise EFslException.Create('Not supported in this server');
end;

function TTerminologyServerFactory.makeEngine(validatorContext: TFHIRWorkerContextWithFactory; ucum: TUcumServiceImplementation): TFHIRPathEngineV;
begin
  raise EFslException.Create('Not supported in this server');
end;

procedure TTerminologyServerFactory.setTerminologyServer(validatorContext: TFHIRWorkerContextWithFactory; server: TFslObject);
begin
  TFHIRServerWorkerContextR4(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
end;

{ TTerminologyServerData }

constructor TTerminologyServerData.Create;
begin
  inherited create;
  FCodeSystems := TFslMap<TFHIRCodeSystem>.create('FHIR Tx Kernel');
  FValueSets := TFslMap<TFHIRValueSet>.create('FHIR Tx Kernel');
  FNamingSystems := TFslMap<TFHIRNamingSystem>.create('FHIR Tx Kernel');
  FConceptMaps := TFslMap<TFHIRConceptMap>.create('FHIR Tx Kernel');
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
  FJson.Free;
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

procedure TTerminologyFhirServerStorage.loadPackage(pid: String);
var
  npm : TNpmPackage;
  s : String;
  res : TFHIRResource;
  i : integer;
begin
  logts('Load package '+pid);

  if (FCache = nil) then
    FCache := TFHIRPackageManager.Create(false);
  if FJson = nil then
    FJson := TFHIRJsonParser.Create(FServerContext.ValidatorContext.link, THTTPLanguages.Create('en'));
  i := 0;

  npm := FCache.loadPackage(pid);
  try
    for s in npm.listResources(['CodeSystem', 'ValueSet', 'NamingSystem', 'ConceptMap']) do
    begin
      inc(i);
      if (i mod 100 = 0) then
        logtd('.');
      res := FJson.parseResource(npm.loadBytes(s)) as TFhirResource;
      try
        case res.ResourceType of
          frtCodeSystem:
            begin
            FData.FCodeSystems.Add(inttostr(FData.FCodeSystems.Count+1), res.link as TFhirCodeSystem);
            FServerContext.ValidatorContext.seeResource(res);
            end;
          frtConceptMap:
            begin
            FData.FConceptMaps.Add(inttostr(FData.FConceptMaps.Count+1), res.link as TFhirConceptMap);
            FServerContext.ValidatorContext.seeResource(res);
            end;
          frtNamingSystem:
            begin
            FData.FNamingSystems.Add(inttostr(FData.FNamingSystems.Count+1), res.link as TFhirNamingSystem);
            FServerContext.ValidatorContext.seeResource(res);
            end;
          frtValueSet:
            begin
            FData.FValueSets.Add(inttostr(FData.FValueSets.Count+1), res.link as TFhirValueSet);
            FServerContext.ValidatorContext.seeResource(res);
            end;
        else
          ; // we ignore it
        end;

      finally
        res.Free;
      end;
    end;
  finally
    npm.Free;
  end;
  logtf(inttostr(i)+' resources');
end;

function TTerminologyFhirServerStorage.loadPackages: TFslMap<TLoadedPackageInformation>;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.loadUTGFolder;
begin
  raise ETodo.Create('Not done yet');
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


{ TTerminologyServerKernel }

constructor TTerminologyServerKernel.Create(iniName : String);
begin
  inherited Create;
  FPackages := TStringList.create;
  FSettings := TFHIRServerSettings.create;
  FIniFile := iniName;
  FDatabases := TFslMap<TFslDBManager>.create('fhir.svc');
end;

destructor TTerminologyServerKernel.Destroy;
begin
  FWebServer.Free;
  FStore.FServerContext.Free;
  FStore.Free;
  FTerminologies.Free;
  FSettings.Free;
  FDatabases.free;
  FPackages.Free;
  inherited;
end;

procedure TTerminologyServerKernel.Start;
var
  s : String;
//  details : TFHIRServerIniComplex;
  ep : TFhirWebServerEndpoint;
  ini : TFHIRServerIniFile;
begin
  logt('Run as Terminology Server');

  ini := TFHIRServerIniFile.Create(FIniFile);
  try
    FWebServer := TFhirWebServer.create(FSettings.Link, nil, 'FHIR Terminology Server');
    FWebServer.loadConfiguration(ini);
    if (FolderExists(ProcessPath(ExtractFilePath(ini.FileName), ini.web['folder']))) then
      FWebServer.SourceProvider := TFHIRWebServerSourceFolderProvider.Create(ProcessPath(ExtractFilePath(ini.FileName), ini.web['folder']))
    else
      FWebServer.SourceProvider := TFHIRWebServerSourceZipProvider(Path([ExtractFilePath(paramstr(0)), 'websource.zip']));

    FStore := TTerminologyFhirServerStorage.Create(TFHIRFactoryR4.create);
    ConnectToDatabases(ini);
    loadTerminologies(ini);

    FStore.FServerContext := TFHIRServerContext.Create(FStore.Link, TTerminologyServerFactory.create);
    FStore.FServerContext.Globals := FSettings.Link;
    FStore.FServerContext.TerminologyServer := TTerminologyServer.Create(nil {no closures - for now}, FStore.FServerContext.factory.Link, FTerminologies.link);
    FStore.FServerContext.userProvider := TTerminologyFHIRUserProvider.Create;

    FStore.loadPackage('hl7.fhir.r4.core');
    if FUTGFolder <> '' then
      FStore.loadUTGFolder
    else
      FStore.loadPackage('hl7.terminology.r4');
    FStore.loadPackage('fhir.tx.support.r4');
    for s in FPackages do
      FStore.loadPackage(s);

    ep := FWebServer.registerEndPoint('r4', 'path', FStore.FServerContext.Link, ini);
    FWebServer.Start(true, false);
  finally
    ini.Free;
  end;
end;

procedure TTerminologyServerKernel.Stop;
begin
  FWebServer.Stop;
end;

procedure TTerminologyServerKernel.loadTerminologies(ini : TFHIRServerIniFile);
begin
  FTerminologies := TCommonTerminologies.Create(FSettings.link);
  FTerminologies.load(ini, FDatabases, false);
end;

Procedure TTerminologyServerKernel.ConnectToDatabases(ini : TFHIRServerIniFile);
var
  s : String;
  details : TFHIRServerIniComplex;
begin
  for s in ini.databases.keys do
  begin
    details := ini.databases[s];
    FDatabases.Add(s, connectToDatabase(s, details));
  end;
end;


end.

