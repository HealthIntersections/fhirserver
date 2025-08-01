unit endpoint_txsvr;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
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

{$i fhir.inc}

interface

uses
  SysUtils, Classes, {$IFDEF DELPHI} IOUtils, {$ENDIF}
  fsl_base, fsl_utilities, fsl_logging, fsl_json, fsl_stream, fsl_fpc, fsl_scim, fsl_http, fsl_npm_cache,
  fsl_npm, fsl_htmlgen, fsl_threads, fsl_i18n, fsl_lang,
  fdb_manager, fdb_fts,
  ftx_ucum_services,
  fhir_objects,  fhir_factory, fhir_pathengine, fhir_parser, fhir_common, fhir_utilities,

  ftx_service, ftx_sct_services,

  fhir3_factory, fhir4_factory, fhir4b_factory, fhir5_factory,
  fhir3_indexinfo, fhir4_indexinfo, fhir4b_indexinfo, fhir5_indexinfo,
  fhir3_context, fhir4_context, fhir4b_context, fhir5_context,
  fhir3_pathengine, fhir4_pathengine, fhir4b_pathengine, fhir5_pathengine,
  fhir3_validator, fhir4_validator, fhir4b_validator, fhir5_validator,
  validator_r3, validator_r4, validator_r4b, validator_r5,

  fhir_indexing, search_base, database_installer,
  tx_manager, tx_server, tx_operations, operations,
  storage, server_context, session, user_manager, server_config, bundlebuilder,
  utilities, security, indexing, server_factory, subscriptions, 
  telnet_server, kernel_thread, server_stats, server_constants,
  web_server, web_base, endpoint, endpoint_storage;

const
  TX_SEARCH_PAGE_DEFAULT = 10;
  TX_SEARCH_PAGE_LIMIT = 200;

type
  TTerminologyServerEndPoint = class;

  { TTerminologyServerFactory }

  TTerminologyServerFactory = class (TFHIRServerFactory)
  protected
    FLanguages : TIETFLanguageDefinitions;
    FVersion : TFHIRVersion;
  public
    constructor Create(Languages : TIETFLanguageDefinitions; version : TFHIRVersion);
    destructor Destroy; override;
    function makeIndexes : TFHIRIndexBuilder; override;
    function makeValidator(pc : TFHIRPackageManager): TFHIRValidatorV; override;
    function makeIndexer : TFHIRIndexManager; override;
    function makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager; override;
    function makeEngine(validatorContext : TFHIRWorkerContextWithFactory; ucum : TUcumServiceImplementation) : TFHIRPathEngineV; override;

    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); override;
  end;

  { TTerminologyServerData }

  TTerminologyServerData = class (TFslObject)
  private
    FLock : TFslLock;
    FPackages : TStringList;
    FCodeSystems : TFslMap<TFHIRResourceProxyV>;
    FTextIndex: TFDBFullTextSearch;
    FValueSets : TFslMap<TFHIRResourceProxyV>;
    FNamingSystems : TFslMap<TFHIRResourceProxyV>;
    FConceptMaps : TFslMap<TFHIRResourceProxyV>;
    FLoadingComplete : boolean;
    FTotalToLoad : integer; 
    FTerminologyServer : TTerminologyServer;
    FImplicitValueSets : TFslList<TFhirValueSetW>;
    FLoaded : integer;
    procedure addCodesToIndex(cmp: TFDBFullTextSearchCompartment; vurl : String; codes: TFhirCodeSystemConceptListW);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    constructor Create(TextIndex : TFDBFullTextSearch);
    destructor Destroy; override;
    function link : TTerminologyServerData; overload;

    procedure clear;

    property TerminologyServer : TTerminologyServer read FTerminologyServer;
    property CodeSystems : TFslMap<TFHIRResourceProxyV> read FCodeSystems;
    property ValueSets : TFslMap<TFHIRResourceProxyV> read FValueSets;
    property NamingSystems : TFslMap<TFHIRResourceProxyV> read FNamingSystems;
    property ConceptMaps : TFslMap<TFHIRResourceProxyV> read FConceptMaps;
    property TextIndex : TFDBFullTextSearch read FTextIndex;
  end;

  { TTerminologyServerDataLoadThread }

  TTerminologyServerDataLoadThread = class (TFslThread)
  private
    FEndPoint : TTerminologyServerEndPoint;
    procedure process(name : String; list : TFslMap<TFHIRResourceProxyV>);
  public
    constructor create(ep : TTerminologyServerEndPoint);
    procedure execute; override;
  end;

  TTerminologyFhirServerStorage = class;

  { TTerminologyServerOperationEngine }

  TTerminologyServerOperationEngine = class (TFHIROperationEngine)
  private
    FData : TTerminologyServerData;

    function compareDate(base, min, max : TFslDateTime; value : String; prefix : TFHIRSearchParamPrefix) : boolean;
    function matchesNative(resource : TFHIRMetadataResourceW; sp : TSearchParameter) : boolean;
    function matchesFHIRPath(resource : TFhirResourceV; sp : TSearchParameter) : boolean;
    function matchesObject(obj : TFhirObject; sp : TSearchParameter) : boolean;
    function tokenMatchesCodeableConcept(obj: TFhirObject; sp: TSearchParameter): boolean;
    function tokenMatchesCoding(obj: TFhirObject; sp: TSearchParameter): boolean; overload;
    function tokenMatchesCoding(c: TFhirCodingW; sp: TSearchParameter): boolean; overload;
    function tokenMatchesIdentifier(obj: TFhirObject; sp: TSearchParameter): boolean; overload;
    function makeWrapper(rn : String; p : TFHIRResourceProxyV) : TFHIRMetadataResourceW;
    procedure checkForImplicitValueSet(list : TFslMetadataResourceList; url : String);
  protected
    function context : TFHIRServerContext;
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    procedure processGraphQL(graphql: String; request : TFHIRRequest; response : TFHIRResponse); override;

    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse); override;
    function ExecuteTransaction(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String; override ;

    function Repository : TTerminologyFhirServerStorage; // private - hint busting
  public
    constructor Create(Storage : TFHIRStorageService; ServerContext : TFHIRServerContext; langList : THTTPLanguageList; Data : TTerminologyServerData);
    destructor Destroy; override;

    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; override;
    function GetResourceById(request: TFHIRRequest; aType : String; id, base : String; var needSecure : boolean) : TFHIRResourceV; override;
    function getResourceByUrl(aType : string; url, version : string; allowNil : boolean; var needSecure : boolean): TFHIRResourceV; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    procedure AuditRest(session : TFhirSession; intreqid, extreqid, ip, resourceName : string; id, ver : String; verkey : integer; op : TFHIRCommandType; provenance : TFhirProvenanceW; opName : String; httpCode : Integer; name, message : String; patients : TArray<String>); overload; override;
    function patientIds(request : TFHIRRequest; res : TFHIRResourceV) : TArray<String>; override;
    function FindResource(aType, sId : String; options : TFindResourceOptions; var resourceKey, versionKey : integer; request: TFHIRRequest; response: TFHIRResponse; sessionCompartments : TFslList<TFHIRCompartmentId>): boolean; override;
  end;

  { TTerminologyFhirServerStorage }

  TTerminologyFhirServerStorage = class (TFHIRStorageService)
  private
    FData : TTerminologyServerData;
    FServerContext : TFHIRServerContext; // free from owner
    procedure loadResource(res: TFHIRResourceProxyV; ignoreEmptyCodeSystems : boolean);
    procedure loadBytes(factory: TFHIRFactory; name: String; cnt: TBytes);
    procedure loadFromZip(factory: TFHIRFactory; cnt: TBytes);
  protected
    function GetTotalResourceCount: integer; override;
    function SupportsTransactions : boolean; override;
  public
    constructor Create(factory : TFHIRFactory); Override;
    destructor Destroy; override;

    function link : TTerminologyFhirServerStorage; overload;
    procedure UnLoad; override;

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

    function createOperationContext(langList : THTTPLanguageList) : TFHIROperationEngine; override;
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

    procedure loadPackage(pid : String; ignoreEmptyCodeSystems : boolean);
    procedure loadFile(factory : TFHIRFactory; name : String);

    function cacheSize(magic : integer) : UInt64; override;
    function issueHealthCardKey : integer; override;
    procedure logHealthCard(key : integer; source : TSmartHealthCardSource; date : TFslDateTime; nbf, hash, patientId : String; details : TBytes); override;
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

  { TTerminologyServerWebServer }

  TTerminologyServerWebServer = class (TStorageWebEndpoint)
  private
    FEndPoint : TTerminologyServerEndPoint;
    function factory : TFHIRFactory;
    function data : TTerminologyServerData;
    function terminologies : TCommonTerminologies;
  protected

    Function BuildFhirHomePage(compList : TFslList<TFHIRCompartmentId>; logId : String; langList : THTTPLanguageList; host, rawHost, sBaseURL: String; Session: TFHIRSession; secure: boolean): String; override;
    Function BuildFhirUploadPage(langList : THTTPLanguageList; host, sBaseURL: String; aType: String; Session: TFHIRSession): String; override;
    Function BuildFhirAuthenticationPage(langList : THTTPLanguageList; host, path, logId, Msg: String; secure: boolean; params : String): String; override;
    function HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime; override;
    procedure GetWebUILink(resource: TFhirResourceV; base, statedType, id, ver: String; var link, text: String); override;
    Function ProcessZip(langList : THTTPLanguageList; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerConfigFile; Context: TOperationContext; var cursor: integer): TFHIRBundleW; override;
    function DoSearch(Session: TFHIRSession; rtype: string; langList : THTTPLanguageList; params: String): TFHIRBundleW; override;
    procedure PopulateConformance(sender: TObject; conf: TFhirCapabilityStatementW; secure : boolean; baseUrl : String; caps : Array of String); override;

    function AutoCache : boolean; override;
  public
    destructor Destroy; override;
    function link : TTerminologyServerWebServer; overload;
    function description : String; override;
    function logId : string; override;
  end;


  { TTerminologyServerEndPoint }

  TTerminologyServerEndPoint = class (TStorageEndPoint)
  private
    FStore : TTerminologyFhirServerStorage;
    UTGFolder : String;
    FWeb : TTerminologyServerWebServer;
    FLoadThread : TTerminologyServerDataLoadThread;
    function version : TFHIRVersion;
  public
    constructor Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; pcm : TFHIRPackageManager; i18n : TI18nSupport);
    destructor Destroy; override;
    function summary : String; override;
    function makeWebEndPoint(common : TFHIRWebServerCommon) : TFhirWebServerEndpoint; override;

    procedure Load; override;
    procedure Unload; override;
    procedure InstallDatabase(params : TCommandLineParameters); override;
    procedure UninstallDatabase; override;
    procedure LoadPackages(installer : boolean; plist : String); override;
    procedure updateAdminPassword(pw : String); override;
    procedure internalThread(callback : TFhirServerMaintenanceThreadTaskCallBack); override;
    function cacheSize(magic : integer) : UInt64; override;
    procedure clearCache; override;
    procedure SweepCaches; override;
    procedure SetCacheStatus(status : boolean); override;
    procedure getCacheInfo(ci: TCacheInformation); override;    
    procedure recordStats(rec : TStatusRecord); override;
    procedure Started; override;
    procedure Stopping; override;
  end;

function makeTxFactory(version : TFHIRVersion) : TFHIRFactory;
function makeTxServerFactory(Languages: TIETFLanguageDefinitions; version : TFHIRVersion) : TFHIRServerFactory;

implementation

{ TTerminologyServerFactory }

function TTerminologyServerFactory.makeIndexes: TFHIRIndexBuilder;
begin
  case FVersion of
    fhirVersionRelease3 : result := TFHIRIndexBuilderR3.create;
    fhirVersionRelease4 : result := TFHIRIndexBuilderR4.create;
    fhirVersionRelease4B : result := TFHIRIndexBuilderR4B.create;
    fhirVersionRelease5 : result := TFHIRIndexBuilderR5.create;
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating index information');
  end;
end;

function TTerminologyServerFactory.makeValidator(pc : TFHIRPackageManager): TFHIRValidatorV;
begin
  case FVersion of
    fhirVersionRelease3 : result := TFHIRValidator3.Create(TFHIRServerWorkerContextR3.Create(FLanguages.link, TFHIRFactoryR3.create, pc.link));
    fhirVersionRelease4 : result := TFHIRValidator4.Create(TFHIRServerWorkerContextR4.Create(FLanguages.link, TFHIRFactoryR4.create, pc.link));
    fhirVersionRelease4B : result := TFHIRValidator4B.Create(TFHIRServerWorkerContextR4B.Create(FLanguages.link, TFHIRFactoryR4B.create, pc.link));
    fhirVersionRelease5 : result := TFHIRValidator5.Create(TFHIRServerWorkerContextR5.Create(FLanguages.link, TFHIRFactoryR5.create, pc.link));
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating Validator');
  end;
end;

function TTerminologyServerFactory.makeIndexer: TFHIRIndexManager;
begin
  result := nil;
  raise EFslException.Create('Not supported in this server');
end;

function TTerminologyServerFactory.makeSubscriptionManager(ServerContext: TFslObject): TSubscriptionManager;
begin
  result := nil;
  raise EFslException.Create('Not supported in this server');
end;

constructor TTerminologyServerFactory.Create(Languages: TIETFLanguageDefinitions; version: TFHIRVersion);
begin
  inherited Create;
  FLanguages := Languages;
  FVersion := version;
end;

destructor TTerminologyServerFactory.Destroy;
begin
  FLanguages.free;
  inherited Destroy;
end;

function TTerminologyServerFactory.makeEngine(validatorContext: TFHIRWorkerContextWithFactory; ucum: TUcumServiceImplementation): TFHIRPathEngineV;
begin
  case FVersion of
    fhirVersionRelease3 : result := TFHIRPathEngine3.Create(validatorContext as TFHIRWorkerContext3, ucum);
    fhirVersionRelease4 : result := TFHIRPathEngine4.Create(validatorContext as TFHIRWorkerContext4, ucum);
    fhirVersionRelease4B : result := TFHIRPathEngine4B.Create(validatorContext as TFHIRWorkerContext4B, ucum);
    fhirVersionRelease5 : result := TFHIRPathEngine5.Create(validatorContext as TFHIRWorkerContext5, ucum);
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Creating FHIRPathEngine');
  end;
end;

procedure TTerminologyServerFactory.setTerminologyServer(validatorContext: TFHIRWorkerContextWithFactory; server: TFslObject);
begin
  case FVersion of
    fhirVersionRelease3 : TFHIRServerWorkerContextR3(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease4 : TFHIRServerWorkerContextR4(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease4B : TFHIRServerWorkerContextR4B(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
    fhirVersionRelease5 : TFHIRServerWorkerContextR5(ValidatorContext).TerminologyServer := (server as TTerminologyServer);
  else
    raise EFHIRUnsupportedVersion.Create(FVersion, 'Setting Terminology Server');
  end;
end;


{ TTerminologyServerData }

constructor TTerminologyServerData.Create(TextIndex : TFDBFullTextSearch);
begin
  inherited create;
  FLock := TFslLock.create('tx.storage');
  FCodeSystems := TFslMap<TFHIRResourceProxyV>.create('FHIR Tx Kernel');
  FCodeSystems.defaultValue := nil;
  FValueSets := TFslMap<TFHIRResourceProxyV>.create('FHIR Tx Kernel');
  FValueSets.defaultValue := nil;
  FNamingSystems := TFslMap<TFHIRResourceProxyV>.create('FHIR Tx Kernel');
  FNamingSystems.defaultValue := nil;
  FConceptMaps := TFslMap<TFHIRResourceProxyV>.create('FHIR Tx Kernel');
  FConceptMaps.defaultValue := nil;
  FPackages := TStringList.create;
  FTextIndex := TextIndex;
  FImplicitValueSets := TFslList<TFhirValueSetW>.create;
end;

destructor TTerminologyServerData.Destroy;
begin
  FPackages.free;
  FConceptMaps.free;
  FNamingSystems.free;
  FValueSets.free;
  FCodeSystems.free;
  FTextIndex.free;
  FTerminologyServer.Free;
  FImplicitValueSets.Free;
  FLock.free;

  inherited;
end;

function TTerminologyServerData.link: TTerminologyServerData;
begin
  result := TTerminologyServerData(inherited link);
end;

procedure TTerminologyServerData.clear;
begin
  FCodeSystems.clear;
  FValueSets.clear;
  FNamingSystems.clear;
  FConceptMaps.clear;
end;

procedure TTerminologyServerData.addCodesToIndex(cmp : TFDBFullTextSearchCompartment; vurl : String; codes : TFhirCodeSystemConceptListW);
var
  cc : TFHIRCodeSystemConceptW;
begin
  for cc in codes do
  begin
    FTextIndex.addText(cmp, vurl+'#'+cc.code, cc.code+': '+cc.display, cc.definition);
    addCodesToIndex(cmp, vurl, cc.conceptList);
  end;
end;

function TTerminologyServerData.sizeInBytesV(magic: integer): cardinal;
begin
  result := inherited sizeInBytesV(magic) + FPackages.sizeInBytes(magic) + FCodeSystems.sizeInBytes(magic) + FValueSets.sizeInBytes(magic) + FNamingSystems.sizeInBytes(magic) + FConceptMaps.sizeInBytes(magic);
end;

{ TTerminologyServerDataLoadThread }

procedure TTerminologyServerDataLoadThread.process(name : String; list : TFslMap<TFHIRResourceProxyV>);
var
  p : TFHIRResourceProxyV;
  t : TFHIRResourceV;
  i, c : integer;
begin
  SetThreadStatus(name+': 0%');
  c := list.Count;
  i := 0;
  for p in list.values do
  begin
    if Stopped then
      exit;
    SetThreadStatus(name+': '+inttostr(((100 * i) div c))+'% ('+p.url+')');
    inc(FEndPoint.FStore.FData.FLoaded);
    try
      t := p.resourceV; // prompt to load
      if (name = 'CodeSystems') and (FEndPoint <> nil) and (FEndPoint.FStore <> nil) and (FEndPoint.FStore.FServerContext <> nil) then
        FEndPoint.FStore.FServerContext.ValidatorContext.loadCodeSystem(p);
    except
      on e : Exception do
        Logging.log('Error loading '+p.url+'|'+p.version+': '+e.message);
    end;
    inc(i);
    SetThreadStatus(name+': '+inttostr(((100 * i) div c))+'%');
    sleep(15);   // if this goes too low, bang on OSX
  end;
end;

constructor TTerminologyServerDataLoadThread.create(ep: TTerminologyServerEndPoint);
begin
  FEndPoint := ep;
  inherited create;
end;

procedure TTerminologyServerDataLoadThread.execute;
var
  version : String;
begin
  GThreadDoingConstruction := true;
  try
    version := FEndPoint.config.prop['version'].value;
    SetThreadName(version+' Loader');

    FEndPoint.FStore.FData.FTotalToLoad :=
      FEndPoint.FStore.FData.CodeSystems.count + FEndPoint.FStore.FData.ValueSets.Count+
      FEndPoint.FStore.FData.NamingSystems.count + FEndPoint.FStore.FData.ConceptMaps.Count;

    process('CodeSystems', FEndPoint.FStore.FData.CodeSystems);
    if not Stopped then
      process('ValueSets', FEndPoint.FStore.FData.ValueSets);
    if not Stopped then
      process('NamingSystems', FEndPoint.FStore.FData.NamingSystems);
    if not Stopped then
      process('ConceptMaps', FEndPoint.FStore.FData.ConceptMaps);
    if (FEndPoint <> nil) and (FEndPoint.FStore <> nil) and (FEndPoint.FStore.FData <> nil) then
      FEndPoint.FStore.FData.FLoadingComplete := true;
  finally
    GThreadDoingConstruction := false;
  end;
end;

{ TTerminologyServerOperationEngine }

constructor TTerminologyServerOperationEngine.Create(Storage : TFHIRStorageService; ServerContext : TFHIRServerContext; langList : THTTPLanguageList; Data : TTerminologyServerData);
begin
  inherited Create(Storage, ServerContext, langList);
  FData := data;
end;

destructor TTerminologyServerOperationEngine.Destroy;
begin
  FData.free;
  inherited;
end;

procedure TTerminologyServerOperationEngine.StartTransaction;
begin
  // Transactions are not implemented in this server
end;

procedure TTerminologyServerOperationEngine.CommitTransaction;
begin
  // Transactions are not implemented in this server
end;

function TTerminologyServerOperationEngine.compareDate(base, min, max: TFslDateTime; value: String; prefix: TFHIRSearchParamPrefix): boolean;
var
  v, vmin, vmax : TFslDateTime;
begin
  v := TFslDateTime.fromXML(value);
  vmin := v.Min;
  vmax := v.Max;
  result := false;
  case prefix of
    sppNull: result := v.equal(base);
    sppNotEqual: result := not v.Equal(base);
    sppGreaterThan: result := max.after(vmax, false);
    sppLessThan: result := min.before(vmin, false);
    sppGreaterOrEquals: result := not min.before(vmin, false);
    sppLesserOrEquals: result := not max.after(vmax, false);
    sppStartsAfter: result := min.after(vmax, false);
    sppEndsBefore: result := max.before(vmin, false);
    sppAproximately:
      begin
        min := base.lessPrecision.Min;
        max := base.lessPrecision.Max;
        vmin := v.lessPrecision.Min;
        vmax := v.lessPrecision.Max;
        result := min.between(vmin, vmax, true) or max.between(vmin, vmax, true);
      end;
  end;
end;

function TTerminologyServerOperationEngine.context: TFHIRServerContext;
begin
  result := (FServerContext as TFHIRServerContext);
end;

function TTerminologyServerOperationEngine.Repository: TTerminologyFhirServerStorage;
begin
  result := FStorage as TTerminologyFhirServerStorage;
end;

procedure TTerminologyServerOperationEngine.RollbackTransaction;
begin
  // Transactions are not implemented in this server
end;

function TTerminologyServerOperationEngine.patientIds(request: TFHIRRequest; res: TFHIRResourceV): TArray<String>;
begin
  setLength(result, 0);
end;

procedure TTerminologyServerOperationEngine.processGraphQL(graphql: String; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyServerOperationEngine.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
begin
  result := nil;
  raise ETodo.Create('Not done yet');
end;

function TTerminologyServerOperationEngine.GetResourceById(request: TFHIRRequest; aType, id, base: String; var needSecure: boolean): TFHIRResourceV;
begin
  needSecure := false;
  if aType = 'CodeSystem' then
    result := FData.CodeSystems[id].ResourceV.Link
  else if aType = 'ValueSet' then
    result := FData.ValueSets[id].ResourceV.Link
  else if aType = 'NamingSystem' then
    result := FData.NamingSystems[id].ResourceV.Link
  else if aType = 'ConceptMap' then
    result := FData.ConceptMaps[id].ResourceV.Link
  else
    result := nil;
end;

function TTerminologyServerOperationEngine.getResourceByUrl(aType, url, version: string; allowNil: boolean; var needSecure: boolean): TFHIRResourceV;
var
  res : TFHIRResourceProxyV;
begin
  result := nil;
  needSecure := false;
  if (aType = '') or (aType = 'CodeSystem') then
    for res in FData.CodeSystems.Values do
      if res.url = url then
        exit(res.ResourceV.link);
  if (aType = '') or (aType = 'ConceptMap') then
    for res in FData.ConceptMaps.Values do
      if res.url = url then
        exit(res.ResourceV.link);
  if (aType = '') or (aType = 'NamingSystem') then
    for res in FData.NamingSystems.Values do
      if res.url = url then
        exit(res.ResourceV.link);
  if (aType = '') or (aType = 'ValueSet') then
    for res in FData.ValueSets.Values do
      if res.url = url then
        exit(res.ResourceV.link);
end;

function TTerminologyServerOperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse; ignoreHeaders: boolean): boolean;
var
  res : TFHIRResourceV;
  vs : TFhirValueSetW;
begin
  result := false;
  res := nil;
  if request.ResourceName = 'CodeSystem' then
  begin
    if (FData.CodeSystems.ContainsKey(request.id)) then
      res := FData.CodeSystems[request.Id].resourceV.Link
  end
  else if request.ResourceName = 'ValueSet' then
  begin
    if (FData.ValueSets.ContainsKey(request.id)) then
      res := FData.ValueSets[request.Id].resourceV.Link;
    if res = nil then
    begin
      FData.FLock.lock('ExecuteRead');
      try
        for vs in FData.FImplicitValueSets do
          if (res = nil) and (vs.id = request.id) then
            res := vs.Resource.link;
      finally
        FData.FLock.Unlock;
      end;
    end;
  end
  else if request.ResourceName = 'NamingSystem' then
  begin
    if (FData.NamingSystems.ContainsKey(request.id)) then
      res := FData.NamingSystems[request.Id].resourceV.Link
  end
  else if request.ResourceName = 'ConceptMap' then
  begin
    if (FData.ConceptMaps.ContainsKey(request.id)) then
      res := FData.ConceptMaps[request.Id].resourceV.Link
  end;

  try
    if res <> nil then
    begin
      response.HTTPCode := 200;
      response.Message := 'OK';
      response.Resource := res.Link;
      result := true;
    end
    else
    begin
      response.HTTPCode := 404;
      response.Message := 'Not Found';
      response.Resource := factory.BuildOperationOutcome(langList, 'not found', itUnknown);
    end;
  finally
    res.free;
  end;
end;

function hasScope(request : TFHIRRequest; name : String) : boolean;
begin
  result := (request.ResourceName = name) or ((request.ResourceName = '') and request.Parameters['_type'].Contains(name));
end;

function onlyHasElements(ts : TStringList; names : array of String) : boolean;
var
  s : String;
begin
  result := true;
  for s in ts do
    if not StringArrayExists(names, s) then
      exit(false);
end;

function TTerminologyServerOperationEngine.makeWrapper(rn : String; p : TFHIRResourceProxyV) : TFHIRMetadataResourceW;
begin
  result := factory.wrapResource(factory.makeResource(rn)) as TFHIRMetadataResourceW;
  result.id := p.id;
  result.url := p.url;
  result.version := p.version;
end;

procedure TTerminologyServerOperationEngine.checkForImplicitValueSet(list: TFslMetadataResourceList; url: String);
var
  vs : TFhirValueSetW;
  ok : boolean;
begin
  ok := false;
  FData.FLock.Lock('checkForImplicitValueSet');
  try
    for vs in FData.FImplicitValueSets do
      ok := ok or (vs.url = url);
    if not ok and (FData.TerminologyServer.isKnownValueSet(url, vs)) then
    begin
      list.add(vs);
      vs.id := NewGuidId;
      FData.FImplicitValueSets.add(vs.link);
    end;
  finally
    FData.FLock.Unlock;
  end;
end;

procedure TTerminologyServerOperationEngine.ExecuteSearch(request: TFHIRRequest; response: TFHIRResponse);
var
  search : TFslList<TSearchParameter>;
  sp : TSearchParameter;
  spp : TSearchParser;
  list, filtered : TFslMetadataResourceList;
  res : TFhirMetadataResourceW;
  bundle : TFHIRBundleBuilder;
  op : TFHIROperationOutcomeW;
  base : String;
  isMatch, defCount : boolean;
  i, t, offset, count : integer;
  be : TFhirBundleEntryW;
  p : TFHIRResourceProxyV;
  useProxy : boolean;
  start : UInt64;
  url : string;
  vs : TFhirValueSetW;
begin
  if FEngine = nil then
    FEngine := context.ServerFactory.makeEngine(context.ValidatorContext.Link, TUcumServiceImplementation.Create(context.TerminologyServer.CommonTerminologies.Ucum.link));

  offset := 0;
  count := TX_SEARCH_PAGE_DEFAULT;
  defCount := true;
  for i := 0 to request.Parameters.Count - 1 do
    if request.Parameters.Name[i] = SEARCH_PARAM_NAME_OFFSET then
      offset := StrToIntDef(request.Parameters.Value[request.Parameters.Name[i]], 0)
    else if request.Parameters.Name[i] = '_count' then
    begin
      count := StrToIntDef(request.Parameters.Value[request.Parameters.Name[i]], 0);
      defCount := false;
    end;
  if (count < 2) then
    count := TX_SEARCH_PAGE_DEFAULT
  else if (Count > TX_SEARCH_PAGE_LIMIT) then
    count := TX_SEARCH_PAGE_LIMIT;
  if offset < 0 then
    offset := 0;

  start := GetTickCount64;
  if (request.Parameters.Count = 0) and (response.Format = ffXhtml) and not request.hasCompartments then
    BuildSearchForm(request, response)
  else
  begin
    TypeNotFound(request, response);
    spp := TSearchParser.create([scpCount, scpElements, scpSummary]);
    try
      spp.allowedParams.add('url');
      spp.allowedParams.add('version');
      spp.allowedParams.add('content-mode');
      spp.allowedParams.add('date');
      spp.allowedParams.add('description');
      spp.allowedParams.add('supplements');
      spp.allowedParams.add('identifier');
      spp.allowedParams.add('jurisdiction');
      spp.allowedParams.add('name');
      spp.allowedParams.add('publisher');
      spp.allowedParams.add('status');
      spp.allowedParams.add('system');
      spp.allowedParams.add('title');
      search := spp.parse(TFHIRServerContext(FServerContext).Indexes, request.ResourceName, request.Parameters);
      try
        base := spp.buildUrl(request.resourceName, search);
        OnCreateBuilder(request, response, btSearchset, bundle);
        op := factory.wrapOperationOutcome(factory.makeResource('OperationOutcome'));
        try
          bundle.setLastUpdated(TFslDateTime.makeUTC);
          bundle.addLink('self', base);
          bundle.setId(FhirGUIDToString(CreateGUID));

          list := TFslMetadataResourceList.create;
          try
            useProxy := false;
            if (spp.elements.count > 0) and (search.count = 1) then
            begin
              if onlyHasElements(spp.elements, ['id', 'url' ,'version']) then
              begin
                useProxy := true;
                if (defCount) then
                  count := 100000;
              end
            end;

            if not useProxy and not FData.FLoadingComplete then
              raise EWebServerException.create(500,
                TFHIRServerContext(FServerContext).i18nSupport.translate('TX_SERVER_NOT_READY', request.LangList,
                  [inttostr((100 * FData.FLoaded) div FData.FTotalToLoad), inttostr(FData.FLoaded), inttostr(FData.FTotalToLoad)]),
                'TX_SERVER_NOT_READY', CODES_TFhirIssueType[itTransient]);
            if (hasScope(request, 'CodeSystem')) then
            begin
              for p in FData.CodeSystems.Values do
              begin
                deadCheck(start);
                if useProxy then
                  list.add(makeWrapper('CodeSystem', p))
                else
                  list.add(p.resourceW.link as TFhirMetadataResourceW);
              end;
            end;
            if (hasScope(request, 'ValueSet')) then
            begin
              FData.FLock.Lock('ExecuteSearch');
              try
                for vs in FData.FImplicitValueSets do
                  list.Add(vs.link);
              finally
                FData.FLock.Unlock;
              end;
              if (spp.hasParam('url', url)) then
                checkForImplicitValueSet(list, url);
              for p in FData.ValueSets.Values do
              begin
                deadCheck(start);
                if useProxy then
                  list.add(makeWrapper('ValueSet', p))
                else
                  list.add(p.resourceW.link as TFhirMetadataResourceW);
              end;
            end;
            if (hasScope(request, 'ConceptMap')) then
            begin
              for p in FData.ConceptMaps.Values do
              begin
                deadCheck(start);
                if useProxy then
                  list.add(makeWrapper('ConceptMap', p))
                else
                  list.add(p.resourceW.link as TFhirMetadataResourceW);
              end;
            end;
            if (hasScope(request, 'NamingSystem')) then
            begin
              for p in FData.NamingSystems.Values do
              begin
                deadCheck(start);
                if useProxy then
                  list.add(makeWrapper('NamingSystem', p))
                else
                  list.add(p.resourceW.link as TFhirMetadataResourceW);
              end;
            end;

            filtered := TFslMetadataResourceList.create;
            try
              for res in list do
              begin
                deadCheck(start);
                isMatch := true;
                for sp in search do
                begin
                  if (sp.Control = scpNull) then
                  begin
                    if isMatch and not matchesNative(res, sp) then
                      isMatch := false;
                  end;
                end;
                if isMatch then
                  filtered.add(res.link);
              end;

              bundle.setTotal(filtered.count);

              if (offset > 0) or (Count < filtered.count) then
              begin
                bundle.addLink('first', base+'&'+SEARCH_PARAM_NAME_OFFSET+'=0&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
                if offset - count >= 0 then
                  bundle.addLink('previous', base+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset - count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
                if offset + count < list.count then
                  bundle.addLink('next', base+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr(offset + count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
                if count < list.count then
                  bundle.addLink('last', base+'&'+SEARCH_PARAM_NAME_OFFSET+'='+inttostr((filtered.count div count) * count)+'&'+SEARCH_PARAM_NAME_COUNT+'='+inttostr(Count));
              end;

              i := 0;
              t := 0;
              for res in filtered do
              begin
                deadCheck(start);
                inc(i);
                if (i > offset) then
                begin
                  be := bundle.makeEntry;
                  try
                    bundle.addEntry(be, false);
                    be.Url := URLPath([context.FormalURLPlain, res.fhirType, res.id]);
                    be.resource := res.Resource.Link;
                  finally
                    be.free;
                  end;
                  inc(t);
                  if (t = count) then
                    break;
                end;
              end;
            finally
              filtered.free;
            end;
          finally
            list.free;
          end;
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.Body := '';
          response.resource := bundle.getBundle;
        finally
          op.free;
          bundle.free;
        end;
      finally
        search.free;
      end;
    finally
      spp.free;
    end;
  end;
end;

function TTerminologyServerOperationEngine.ExecuteTransaction(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse) : String;
var
  req, resp : TFHIRBundleW;
  src, dest : TFHIRBundleEntryW;
  url : String;
  dummy : integer;
  oplist : TStringList;
  s : String;
  i : integer;
  tt : TFslTimeTracker;
  start : UInt64;
begin
  start := GetTickCount64;

  opList := TStringList.create;
  try
    // since we're not making any changes, this is pretty straight forward
    try
      if check(response, request.Resource.fhirType = 'Bundle', 400, langList, 'A bundle is required for a Transaction operation', itInvalid) then
      begin
        context.groupId := NewGuidId;
        req := factory.wrapBundle(request.resource.Link);
        resp := factory.wrapBundle(factory.makeResource('Bundle'));
        tt := TFslTimeTracker.create;
        try
          resp.type_ := btBatchResponse;
          resp.id := NewGuidId;
          for src in req.entries.forEnum do
          begin
            deadCheck(start);
            dest := resp.addEntry;
            try
              try
                if (resp.type_ = btBatch) and (src.requestMethod = '') then
                  raise EFHIRException.create('No request details');
                if (src.requestMethod = '') then
                begin
                  src.requestMethod := 'GET';
                  src.requestUrl := src.resource.fhirType+'/'+src.resource.id;
                end;
                request.reset;
                url := request.preAnalyse(src.requestUrl);
                request.analyse(src.requestMethod, url, dummy, nil);
                request.IfNoneMatch := src.requestifNoneMatch;
                if src.requestIfModifiedSince.notNull then
                  request.IfModifiedSince := src.requestIfModifiedSince.UTC.DateTime;
                request.IfMatch := src.requestIfMatch;
                request.IfNoneExist := src.requestIfNoneExist;
                request.resource := src.resource.link;
                s := Execute(context, request, response, tt);
                if s.contains('#') then
                  s := s.substring(0, s.indexof('#'));
                i := oplist.IndexOf(s);
                if i = -1 then
                  oplist.Add(s)
                else
                  oplist.Objects[i] := TObject(integer(oplist.Objects[i])+1);
                dest.responseStatus := inttostr(response.HTTPCode);
                dest.responseLocation := response.Location;
                dest.responseEtag := 'W/'+response.versionId;
                dest.responseDate := TFslDateTime.makeUTC(response.lastModifiedDate);
                dest.resource := response.resource.link;
              except
                on e : ERestfulException do
                begin
                  if req.type_ = btTransaction then
                    raise;
                  dest.responseStatus := inttostr(e.Status);
                  dest.resource := Factory.BuildOperationOutcome(request.langList, e);
                end;
                on e : Exception do
                begin
                  if req.type_ = btTransaction then
                    raise;
                  dest.responseStatus := '500';
                  dest.resource := Factory.BuildOperationOutcome(request.langList, e);
                end;
              end;
            finally
              dest.free;
            end;
          end;
          response.HTTPCode := 200;
          response.Message := 'OK';
          response.resource := resp.Resource.Link;
        finally
          req.free;
          resp.free;
          tt.free;
        end;
      end;
      AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, '', '', '', 0, fcmdBatch, request.Provenance, response.httpCode, '', response.message, []);
    except
      on e: exception do
      begin
        AuditRest(request.session, request.internalRequestId, request.externalRequestId, request.ip, '', '', '', 0, fcmdBatch, request.Provenance, 500, '', e.message, []);
        recordStack(e);
        raise;
      end;
    end;
    result := '';
    for i := 0 to oplist.Count - 1 do
      CommaAdd(result, opList[i]+'*'+inttostr(integer(opList.Objects[i])+1));
    result := ' Transaction ('+result+')';
  finally
    oplist.free;
  end;
end;

function TTerminologyServerOperationEngine.FindResource(aType, sId: String; options: TFindResourceOptions; var resourceKey, versionKey: integer; request: TFHIRRequest; response: TFHIRResponse; sessionCompartments: TFslList<TFHIRCompartmentId>): boolean;
begin
  resourceKey := 0;
  versionKey := 0;
  if aType = 'CodeSystem' then
    result := FData.CodeSystems.ContainsKey(sId)
  else if aType = 'ConceptMap' then
    result := FData.ConceptMaps.ContainsKey(sId)
  else if aType = 'NamingSystem' then
    result := FData.NamingSystems.ContainsKey(sId)
  else if aType = 'ValueSet' then
    result := FData.ValueSets.ContainsKey(sId)
  else
    result := false;
end;

function TTerminologyServerOperationEngine.matchesNative(resource : TFHIRMetadataResourceW; sp : TSearchParameter) : boolean;
  function tokenMatches(mask, value : String) : boolean;
  begin
    result := mask = value;
  end;
  function stringMatches(mask, value : String) : boolean;
  begin
    result := value.toLower.contains(mask.toLower);
  end;
  function dateMatches(mask, value : String) : boolean;
  begin
    result := mask = value;
  end;
var
  cs : TFHIRCodeSystemW;
begin
  if sp.index.Name = 'url' then
    result := tokenMatches(sp.value, resource.url)
  else if sp.index.Name = 'version' then
    result := tokenMatches(sp.value, resource.version)
  else if sp.index.Name = 'date' then
    result := dateMatches(sp.value, resource.date.toXML)
  else if sp.index.Name = 'description' then
    result := stringMatches(sp.value, resource.description)
  else if sp.index.Name = 'name' then
    result := stringMatches(sp.value, resource.name)
  else if sp.index.Name = 'publisher' then
    result := stringMatches(sp.value, resource.publisher)
  else if sp.index.Name = 'status' then
    result := tokenMatches(sp.value, CODES_TPublicationStatus[resource.status])
  else if sp.index.Name = 'system' then
    result := tokenMatches(sp.value, resource.url)
  else if sp.index.Name = 'title' then
    result := stringMatches(sp.value, resource.title)
  else if sp.index.Name = 'identifier' then
    result := false // todo
  else if sp.index.Name = 'jurisdiction' then
    result := false // todo
  else if (resource is TFHIRCodeSystemW) then
  begin
    cs := resource as TFHIRCodeSystemW;
    if sp.index.Name = 'content-mode' then
      result := tokenMatches(sp.value, CODES_TFhirCodeSystemContentMode[cs.content])
    else if sp.index.Name = 'supplements' then
      result := tokenMatches(sp.value, cs.supplements)
    else
      result := false;
  end
  else
    result := false;
end;

// this is not used - proved too slow for operational use
function TTerminologyServerOperationEngine.matchesFHIRPath(resource: TFhirResourceV; sp: TSearchParameter): boolean;
var
  selection : TFHIRSelectionList;
  so : TFHIRSelection;
  parser : TFHIRPathEngineV;
begin
  if sp.index.expression = nil then
  begin
    parser := factory.makePathEngine(context.ValidatorContext.link, TUcumServiceImplementation.Create(context.TerminologyServer.CommonTerminologies.Ucum.Link));
    try
      sp.index.expression := parser.parseV(sp.index.Path);
    finally
      parser.free;
    end;
  end;

  selection := FEngine.evaluate(resource, resource, resource, sp.index.expression);
  try
    if sp.modifier = spmMissing then
    begin
      if sp.value = 'true' then
        result := selection.Empty
      else if sp.value = 'false' then
        result := not selection.Empty
      else
        raise EFHIRException.create('Error Processing search parameter (:missing, value = '+sp.value+')');
    end
    else if selection.Empty then
      result := false
    else
    begin
      result := false;
      for so in selection do
        result := result or matchesObject(so.value, sp);
    end;
  finally
    selection.free;
  end;
end;

function TTerminologyServerOperationEngine.tokenMatchesCoding(obj: TFhirObject; sp: TSearchParameter): boolean;
var
  c : TFhirCodingW;
begin
  c := factory.wrapCoding(obj.link);
  try
    result := tokenMatchesCoding(c, sp);
  finally
    c.free;
  end;
end;

function TTerminologyServerOperationEngine.tokenMatchesCoding(c : TFhirCodingW; sp: TSearchParameter): boolean;
begin
  case sp.modifier of
    spmNull, spmExact: result := ((sp.namespace = '') or (sp.namespace = c.systemUri)) and
                ((sp.value = '') or (sp.value = c.code));
    spmText: result := c.display.ToLower.Contains(sp.value.ToLower);
//    spmIn: ; todo: we're a terminology server so we should really implement these!
//    spmBelow: ;
//    spmAbove: ;
//    spmNotIn: ;
  end;
end;

function TTerminologyServerOperationEngine.tokenMatchesCodeableConcept(obj: TFhirObject; sp: TSearchParameter): boolean;
var
  cc : TFhirCodeableConceptW;
  cl : TFslList<TFhirCodingW>;
  c : TFhirCodingW;
begin
  cc := factory.wrapCodeableConcept(obj.link);
  try
    if sp.modifier = spmText then
      result := cc.text.ToLower.Contains(sp.value.ToLower)
    else
    begin
      result := false;
      cl := cc.codings;
      try
        for c in cl do
          if tokenMatchesCoding(c, sp) then
            exit(true);
      finally
        cl.free;
      end;
    end;
  finally
    cc.free;
  end;
end;

function TTerminologyServerOperationEngine.tokenMatchesIdentifier(obj: TFhirObject; sp: TSearchParameter): boolean;
var
  id : TFhirIdentifierW;
begin
  id := factory.wrapIdentifier(obj.link);
  try
    case sp.modifier of
      spmNull, spmExact : result := ((sp.namespace = '') or (sp.namespace = id.systemUri)) and
                ((sp.value = '') or (sp.value = id.value));
      spmText : result := id.type_.text.ToLower.Contains(sp.value.ToLower);
      spmType : result := tokenMatchesCodeableConcept(id.type_.Element, sp);
    else
      result := false;
    end;
  finally
    id.free;
  end;
end;


function TTerminologyServerOperationEngine.matchesObject(obj: TFhirObject; sp: TSearchParameter): boolean;
var
  date : TFslDateTime;
begin
  result := false;
  case sp.index.SearchType of
    sptNull: raise EFHIRException.create('param.type = null');
    sptNumber: raise EFHIRTodo.create('TTerminologyServerOperationEngine.matchesObject');
//      if obj.isPrimitive then
//        result := compareNumber(obj.primitiveValue, sp.value, sp.prefix)
//      else
//        result := false;
    sptDate:
      begin
      date := obj.dateValue;
      if date.notNull then
        result := compareDate(date, date.Min, date.Max, sp.value, sp.prefix)
      else
        result := false;
      end;
    sptString:
      if not obj.isPrimitive then
        result := false
      else if sp.modifier = spmNull then
        result := RemoveAccents(obj.primitiveValue.ToLower).StartsWith(RemoveAccents(sp.value.ToLower))
      else if sp.modifier = spmContains then
        result := RemoveAccents(obj.primitiveValue.ToLower).contains(RemoveAccents(sp.value.ToLower))
      else if sp.modifier = spmExact then
        result := obj.primitiveValue = sp.value
      else
        raise EFHIRException.create('Modifier is not supported');
    sptToken:
      if obj.fhirType = 'Coding' then
        result := tokenMatchesCoding(obj, sp)
      else if obj.fhirType = 'CodeableConcept' then
        result := tokenMatchesCodeableConcept(obj, sp)
      else if obj.fhirType = 'Identifier' then
        result := tokenMatchesIdentifier(obj, sp)
      else if not obj.isPrimitive then
        result := false
      else if sp.modifier = spmNull then
        result := obj.primitiveValue = sp.value
      else if sp.modifier = spmContains then
        result := obj.primitiveValue.contains(sp.value)
      else if sp.modifier = spmExact then
        result := obj.primitiveValue = sp.value
      else
        raise EFHIRException.create('Modifier is not supported');
    sptReference: raise EFHIRTodo.create('TTerminologyServerOperationEngine.matchesObjectB');
    sptComposite: raise EFHIRTodo.create('TTerminologyServerOperationEngine.matchesObjectC');
    sptQuantity: raise EFHIRTodo.create('TTerminologyServerOperationEngine.matchesObjectD');
    sptUri:
      if not obj.isPrimitive then
        result := false
      else if sp.modifier = spmNull then
        result := obj.primitiveValue = sp.value
      else if sp.modifier = spmAbove then
        result := sp.value.StartsWith(obj.primitiveValue)
      else if sp.modifier = spmBelow then
        result := obj.primitiveValue.StartsWith(sp.value)
      else if sp.modifier = spmExact then
        raise EFHIRException.create('Modifier is not supported');
  end;
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
  FData := TTerminologyServerData.create(TFDBFullTextSearchFactory.makeSQLiteTextSearch(factory.versionName))         ;
end;

destructor TTerminologyFhirServerStorage.Destroy;
begin
  FData.free;
  inherited;
end;

procedure TTerminologyFhirServerStorage.fetchClients(list: TFslList<TRegisteredClientInformation>);
begin
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyFhirServerStorage.fetchLoadedPackage(id: String): TBytes;
begin
  result := nil;
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

function TTerminologyFhirServerStorage.cacheSize(magic: integer): UInt64;
begin
  result := inherited cacheSize(magic) + FData.sizeInBytes(magic) + FServerContext.sizeInBytes(magic);
end;

procedure TTerminologyFhirServerStorage.CloseFhirSession(key: integer);
begin
  // this server doesn't track sessions
end;

function TTerminologyFhirServerStorage.createOperationContext(langList : THTTPLanguageList): TFHIROperationEngine;
begin
  result := TTerminologyServerOperationEngine.create(self.link, FServerContext {no link}, langList, FData.link);
  result.Operations.add(TFhirExpandValueSetOperation.create(FServerContext.Factory.link, FServerContext.TerminologyServer.Link, FServerContext.TerminologyServer.CommonTerminologies.Languages.link));
  result.Operations.add(TFhirLookupCodeSystemOperation.create(FServerContext.Factory.link, FServerContext.TerminologyServer.Link, FServerContext.TerminologyServer.CommonTerminologies.Languages.link));
  result.Operations.add(TFhirValueSetValidationOperation.create(FServerContext.Factory.link, FServerContext.TerminologyServer.Link, FServerContext.TerminologyServer.CommonTerminologies.Languages.link));
  result.Operations.add(TFhirValueSetBatchValidationOperation.create(FServerContext.Factory.link, FServerContext.TerminologyServer.Link, FServerContext.TerminologyServer.CommonTerminologies.Languages.link));
  result.Operations.add(TFhirConceptMapTranslationOperation.create(FServerContext.Factory.link, FServerContext.TerminologyServer.Link, FServerContext.TerminologyServer.CommonTerminologies.Languages.link));
  result.Operations.add(TFhirConceptMapClosureOperation.create(FServerContext.Factory.link, FServerContext.TerminologyServer.Link, FServerContext.TerminologyServer.CommonTerminologies.Languages.link));
  result.Operations.add(TFhirVersionsOperation.create(Factory.link, FServerContext.TerminologyServer.CommonTerminologies.Languages.link));
  {$IFDEF DEV_FEATURES}
  result.Operations.add(TFhirFeatureNegotiation.create(Factory.link, FServerContext.TerminologyServer.CommonTerminologies.Languages.link));
  {$ENDIF}
end;

function TTerminologyFhirServerStorage.FetchResource(key: integer): TFHIRResourceV;
begin
  result := nil;
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
  result := nil;
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyFhirServerStorage.getClientName(id: String): string;
begin
  result := '';
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyFhirServerStorage.GetTotalResourceCount: integer;
begin
  result := FData.FCodeSystems.Count + FData.FValueSets.Count + FData.FNamingSystems.Count + FData.FConceptMaps.Count;
end;

function TTerminologyFhirServerStorage.issueHealthCardKey: integer;
begin
  result := 0;
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyFhirServerStorage.link: TTerminologyFhirServerStorage;
begin
  result := TTerminologyFhirServerStorage(inherited link);
end;

procedure TTerminologyFhirServerStorage.UnLoad;
begin
  inherited UnLoad;
  FData.clear;
end;

procedure TTerminologyFhirServerStorage.loadResource(res : TFHIRResourceProxyV; ignoreEmptyCodeSystems : boolean);
begin
  if res.fhirType = 'CodeSystem' then
  begin
    if (not ignoreEmptyCodeSystems or StringArrayExists(['fragment', 'complete', 'supplement'], res.content)) then
    begin
      if FData.FCodeSystems.ContainsKey(res.id) then
        res.id := inttostr(FData.FCodeSystems.Count+1);
      FData.FCodeSystems.Add(res.id, res.link);
      FServerContext.ValidatorContext.seeResource(res);
      // todo; index supplements
    end;
  end
  else if res.fhirType = 'ConceptMap' then
  begin
    if FData.FConceptMaps.ContainsKey(res.id) then
      res.id := inttostr(FData.FConceptMaps.Count+1);
    FData.FConceptMaps.Add(res.id, res.link);
    FServerContext.ValidatorContext.seeResource(res);
  end
  else if res.fhirType = 'NamingSystem' then
  begin
    if FData.FNamingSystems.ContainsKey(res.id) then
      res.id := inttostr(FData.FNamingSystems.Count+1);
    FData.FNamingSystems.Add(res.id, res.link);
    FServerContext.ValidatorContext.seeResource(res);
  end
  else if res.fhirType = 'ValueSet' then
  begin
    if FData.FValueSets.ContainsKey(res.id) then
      res.id := inttostr(FData.FValueSets.Count+1);
    FData.FValueSets.Add(res.id, res.link);
    FServerContext.ValidatorContext.seeResource(res);
  end;
end;

procedure TTerminologyFhirServerStorage.loadPackage(pid: String; ignoreEmptyCodeSystems : boolean);
var
  npm : TNpmPackage;
  s : String;
  res : TFHIRResourceProxyV;
  i : integer;
  list : TFslList<TNpmPackageResource>;
  pi : TNpmPackageResource;
  fileToLoad: string;
begin
  i := 0;

  fileToLoad := filePath([FServerContext.pcm.folder,pid]) ;

  if (FileExists(fileToLoad)) then
    try
      npm := FServerContext.pcm.loadPackageFromCache(fileToLoad)
      except
        on e : Exception do
          Logging.log('Error loading package from file '+fileToLoad+': '+e.message);
      end
  else
    npm := FServerContext.pcm.loadPackage(pid);

  try
    Logging.start('Load package '+npm.name+'#'+npm.version);
    try
      FData.FPackages.Add(npm.name+'#'+npm.version);
      list := npm.listResourceInfo('package');
      try
        if (list <> nil) then
        begin
          for pi in list do
          begin
            if StringArrayExists(['CodeSystem', 'ValueSet', 'NamingSystem', 'ConceptMap'], pi.ResourceType) then
            begin
              inc(i);
              if (i mod 100 = 0) then
                Logging.continue('.');
              res := factory.makeProxy(npm.name+'#'+npm.version, pi.Link, FServerContext.ValidatorContext.Link, FData.FLock.link);
              try
                res.ignoreHtml := true;
                loadResource(res, ignoreEmptyCodeSystems);
              finally
                res.free;
              end;
            end;
          end;
        end;
      finally
        list.free;
      end;
    finally
      Logging.finish(' '+inttostr(i)+' resources');
    end;
  finally
    npm.free;
  end;
end;

function TTerminologyFhirServerStorage.loadPackages: TFslMap<TLoadedPackageInformation>;
begin
  result := nil;
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.loadFromZip(factory: TFHIRFactory; cnt : TBytes);
var
  zip : TFslZipReader;
  i : integer;
begin
  zip := TFslZipReader.Create;
  try
    zip.Stream := TFslMemoryStream.create(cnt);
    zip.ReadZip;
    for i := 0 to zip.Parts.Count - 1 do
    begin
      Logging.continue('.');
      loadBytes(factory, zip.Parts[i].Name, zip.Parts[i].AsBytes);
    end;
  finally
    zip.free;
  end;
end;

procedure TTerminologyFhirServerStorage.loadBytes(factory: TFHIRFactory; name: String; cnt : TBytes);
var
  fmt : TFHIRFormat;
  p : TFHIRParser;
  res : TFHIRResourceV;
  pr : TFHIRResourceProxyV;
begin
  if name.EndsWith('.zip') then
    loadFromZip(factory, cnt)
  else
  begin
    if name.EndsWith('.json') then
      fmt := ffJson
    else if name.EndsWith('.xml') then
      fmt := ffXml
    else
      fmt := DetectFormat(cnt);

    if fmt = ffUnspecified then
      raise EFslException.Create('Resource in "'+name+'" could not be parsed (format unrecognised)');
    p := factory.makeParser(FServerContext.ValidatorContext.Link, fmt, nil);
    try
      res := p.parseResource(cnt);
      try
        pr := factory.makeProxy(name, res.link);
        try
          loadResource(pr, false);
        finally
          pr.free;
        end;
      finally
        res.free;
      end;
    finally
      p.free;
    end;
  end;
end;

procedure TTerminologyFhirServerStorage.loadFile(factory: TFHIRFactory; name: String);
var
  cnt : TBytes;
begin
  if (not FileExists(name)) then
    raise EFslException.Create('Resource in "'+name+'" could not be parsed (not found)');

  Logging.start('Load File '+name);
  cnt := FileToBytes(name);
  loadBytes(factory, name, cnt);
  Logging.finish(' - done');
end;

procedure TTerminologyFhirServerStorage.logHealthCard(key: integer; source: TSmartHealthCardSource; date: TFslDateTime; nbf, hash, patientId: String; details: TBytes);
begin
  raise EFslException.Create('Not Implemented');
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
  result := false;
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.RunValidation;
begin
  // nothing in this server
end;

procedure TTerminologyFhirServerStorage.SetUpRecording(session: TFhirSession);
begin
end;

function TTerminologyFhirServerStorage.storeClient(client: TRegisteredClientInformation; sessionKey: integer): String;
begin
  result := '';
  raise EFslException.Create('Not Implemented');
end;

function TTerminologyFhirServerStorage.SupportsTransactions: boolean;
begin
  result := true;
end;

procedure TTerminologyFhirServerStorage.Sweep;
begin
  raise EFslException.Create('Not Implemented');
end;

procedure TTerminologyFhirServerStorage.Yield(op: TFHIROperationEngine; exception: Exception);
begin
  op.free;
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
var
  ts : TStringList;
  s : String;
begin
  result := TSCIMUser.Create(TJsonObject.Create);
  result.userName := 'Registered User';
  result.formattedName := 'Registered User';
  ts := TFHIRSecurityRights.allScopesAsUris;
  try
    for s in ts do
      result.addEntitlement(s);
  finally
    ts.free;
  end;
end;

function TTerminologyFHIRUserProvider.loadUser(id: String; var key: integer): TSCIMUser;
begin
  key := 1;
  result := LoadUser(key);
end;


{ TTerminologyServerEndPoint }

function TTerminologyServerEndPoint.cacheSize(magic : integer): UInt64;
begin
  result := inherited cacheSize(magic) + FStore.cacheSize(magic);
end;

procedure TTerminologyServerEndPoint.clearCache;
begin
  inherited;
  ServerContext.ClientCacheManager.clearCache;
end;

procedure TTerminologyServerEndPoint.SweepCaches;
begin
  inherited SweepCaches;
  ServerContext.ClientCacheManager.sweep;
end;

constructor TTerminologyServerEndPoint.Create(config : TFHIRServerConfigSection; settings : TFHIRServerSettings; db : TFDBManager; common : TCommonTerminologies; pcm : TFHIRPackageManager; i18n : TI18nSupport);
begin
  inherited Create(config, settings, db, common, pcm, i18n);
  pcm.Caching := false;
end;

destructor TTerminologyServerEndPoint.Destroy;
begin
  FStore.free;
  inherited;
end;

procedure TTerminologyServerEndPoint.getCacheInfo(ci: TCacheInformation);
begin
  inherited;
  ci.add('Terminology Data', FStore.FData.sizeInBytes(ci.magic));
end;

procedure TTerminologyServerEndPoint.recordStats(rec: TStatusRecord);
begin
  inherited recordStats(rec);
  FServerContext.TerminologyServer.recordStats(rec);
end;

procedure TTerminologyServerEndPoint.Started;
begin
  inherited Started;
  FLoadThread.Start;
end;

procedure TTerminologyServerEndPoint.Stopping;
begin
  inherited Stopping;
  if FLoadThread <> nil then
    FLoadThread.StopAndWait(500);
end;

function TTerminologyServerEndPoint.summary: String;
begin
  result := 'Terminology Server using '+describeDatabase(Config);
end;

function makeTxFactory(version : TFHIRVersion): TFHIRFactory;
begin
  case version of
    fhirVersionRelease3 : result := TFHIRFactoryR3.create;
    fhirVersionRelease4 : result := TFHIRFactoryR4.create;
    fhirVersionRelease4B : result := TFHIRFactoryR4B.create;
    fhirVersionRelease5 : result := TFHIRFactoryR5.create;
  else
    raise EFslException.Create('Unsupported Version');
  end;
end;

function makeTxServerFactory(Languages: TIETFLanguageDefinitions; version : TFHIRVersion): TFHIRServerFactory;
begin
  result := TTerminologyServerFactory.Create(Languages, version);
end;

function TTerminologyServerEndPoint.makeWebEndPoint(common: TFHIRWebServerCommon): TFhirWebServerEndpoint;
begin
  inherited makeWebEndPoint(common);
  FWeb := TTerminologyServerWebServer.Create(Config.name, Config['path'].value, common, self);
  FWeb.FEndPoint := self;
  WebEndPoint := FWeb;
  result := FWeb;
  FServerContext.FormalURLPlain := 'http://'+Common.host+nonDefPort(Common.statedPort, 80)+WebEndPoint.PathNoSlash;
  FServerContext.FormalURLSecure := 'https://'+Common.host+nonDefPort(Common.statedSSLPort, 443)+WebEndPoint.PathNoSlash;
end;

procedure TTerminologyServerEndPoint.SetCacheStatus(status: boolean);
begin
  inherited;
end;

procedure TTerminologyServerEndPoint.Load;
var
  pid : String;
begin
  FStore := TTerminologyFhirServerStorage.Create(makeTxFactory(version));
  FServerContext := TFHIRServerContext.Create(Config.name, FStore.link, makeTxServerFactory(i18n.Languages.link, version), FPcm.link);
  FStore.FServerContext := FServerContext;
  FServerContext.Globals := Settings.Link;
  FServerContext.userProvider := TTerminologyFHIRUserProvider.Create;
  FServerContext.i18nSupport := i18n.link;
  FServerContext.TerminologyServer := TTerminologyServer.Create(Database.link, FStore.FFactory.Link, Terminologies.link, FServerContext.i18nSupport.link);

  FServerContext.TerminologyServer.Loading := true;
  FStore.loadPackage(FStore.factory.corePackage, false);

  for pid in Config['packages'].values do
    if not pid.StartsWith('hl7.terminology') or (UTGFolder = '') then
      FStore.loadPackage(pid, true);

  FServerContext.TerminologyServer.Loading := false;

  FStore.Initialise;
  FStore.FData.FTerminologyServer := FServerContext.TerminologyServer.link;
  FStore.FData.FLoadingComplete := false;
  FLoadThread := TTerminologyServerDataLoadThread.create(self);
end;

procedure TTerminologyServerEndPoint.Unload;
begin
  if FLoadThread <> nil then
    FLoadThread.StopAndWait(500);
  if FServerContext <> nil then
  begin
    FServerContext.Unload;
    FServerContext.free;
  end;
  FServerContext := nil;
  FStore.free;
  FStore := nil;
end;

procedure TTerminologyServerEndPoint.internalThread(callback : TFhirServerMaintenanceThreadTaskCallBack);
begin
  try
    callback(self, 'Google Commit', 0);
    FWeb.Common.Google.commit;
    callback(self, 'Checking Async Tasks', 25);
    FWeb.CheckAsyncTasks;
    //callback(self, 'Sweeping Client Cache', 50);
    //FServerContext.ClientCacheManager.sweep;
    //callback(self, 'Build Terminology Indexes', 75);
    //FServerContext.TerminologyServer.BuildIndexes(false);
  except
    on e : exception do
      Logging.log('Error in internal thread for '+Config.name+': '+e.Message);
  end;
end;

procedure TTerminologyServerEndPoint.InstallDatabase(
  params: TCommandLineParameters);
var
  conn : TFDBConnection;
  dbi : TFHIRDatabaseInstaller;
begin
  conn := Database.getConnection('install');
  try
    dbi := TFHIRDatabaseInstaller.Create(conn, makeTxFactory(version), makeTxServerFactory(i18n.Languages.link, version));
    try
      dbi.InstallTerminologyServer;
    finally
      dbi.free;
    end;
    conn.Release;
  except
    on e : exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

procedure TTerminologyServerEndPoint.UninstallDatabase;
var
  dbi : TFHIRDatabaseInstaller;
  conn : TFDBConnection;
begin
  conn := Database.GetConnection('uninstall');
  try
    dbi := TFHIRDatabaseInstaller.create(conn, nil, nil);
    try
      dbi.Uninstall;
    finally
      dbi.free;
    end;
    conn.Release;
  except
    on e : Exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

procedure TTerminologyServerEndPoint.LoadPackages(installer : boolean; plist: String);
begin
  raise EFslException.Create('This operation does not apply to this endpoint');
end;

procedure TTerminologyServerEndPoint.updateAdminPassword(pw: String);
begin
  raise EFslException.Create('This operation does not apply to this endpoint');
end;

function TTerminologyServerEndPoint.version: TFHIRVersion;
var
  v : String;
begin
  v := Config['version'].value;
  if (v = 'r2') then
    result := fhirVersionRelease2
  else if (v = 'r3') then
    result := fhirVersionRelease3
  else if (v = 'r4') then
    result := fhirVersionRelease4
  else if (v = 'r4b') then
    result := fhirVersionRelease4B
  else if (v = 'r5') then
    result := fhirVersionRelease5
  else
    raise EFslException.Create('Unknown version "'+v+'"');
end;

{ TTerminologyServerWebServer }

destructor TTerminologyServerWebServer.Destroy;
begin
  // nothing
  inherited;
end;

function TTerminologyServerWebServer.link: TTerminologyServerWebServer;
begin
  result := TTerminologyServerWebServer(inherited link);
end;

function TTerminologyServerWebServer.logId: string;
begin
  result := factory.versionName;
end;

function TTerminologyServerWebServer.AutoCache: boolean;
begin
  result := true;
end;

function TTerminologyServerWebServer.BuildFhirAuthenticationPage(langList : THTTPLanguageList; host, path, logId, Msg: String; secure: boolean; params: String): String;
begin
  result := '';
  raise EFslException.Create('Authentication is not supported for the terminology server');
end;

function TTerminologyServerWebServer.BuildFhirHomePage(compList: TFslList<TFHIRCompartmentId>; logId: String; langList : THTTPLanguageList; host, rawHost, sBaseURL: String; Session: TFHIRSession; secure: boolean): String;
var
  h : THtmlPublisher;
  s : String;
  sct : TSnomedServices;
  p : TCodeSystemProviderFactory;
  pl : TFslList<TCodeSystemProviderFactory>;
begin
  h := THtmlPublisher.Create;
  try
    h.Heading(2, 'Terminology Server');
    h.StartParagraph;
    h.AddTextPlain('This server implements the ');
    h.URL('FHIR Terminology Service', urlPath([factory.specURL, 'terminology-service.html']));
    h.AddTextPlain('.');
    h.EndParagraph;

    h.Line;
    h.StartList();

    h.StartListItem;
    h.AddTextPlain(inttostr(data.CodeSystems.Count)+' CodeSystem Resources');
    h.EndListItem;

    h.StartListItem;
    h.AddTextPlain(inttostr(data.ValueSets.Count)+' ValueSet Resources');
    h.EndListItem;

    h.StartListItem;
    h.AddTextPlain(inttostr(data.ConceptMaps.Count)+' ConceptMap Resources');
    h.EndListItem;

    h.StartListItem;
    h.AddTextPlain(inttostr(data.NamingSystems.Count)+' NamingSystem Resources');
    h.EndListItem;

    h.EndList();
    h.Line;

    h.AddParagraph('The following packages are loaded:');
    h.StartList();
    for s in data.FPackages do
      h.AddListItem(s);
    h.EndList();

    h.Line;
    h.AddParagraph('In addition, the server supports the following code systems:');
    h.StartTable(true);
    h.StartTableRow;
    h.AddTableCell('Code System', true);
    h.AddTableCell('Version', true);
    h.AddTableCell('Uri', true);
    h.EndTableRow;

    pl := TFslList<TCodeSystemProviderFactory>.create;
    try
      for s in terminologies.ProviderClasses.SortedKeys do
      begin
        p := terminologies.ProviderClasses[s];
        if not pl.Contains(p) then
        begin
          pl.add(p.Link);
          h.StartTableRow;
          h.AddTableCell(p.description);
          h.AddTableCell(p.versionDesc);
          h.AddTableCell(p.systemUri);
          h.EndTableRow;
        end;
      end;
      h.EndTable();
    finally
      pl.free;
    end;

    result := processContent('template-fhir.html', secure, 'Terminology Service Home Page', h.output);
  finally
    h.free;
  end;
end;

function TTerminologyServerWebServer.BuildFhirUploadPage(
  langList: THTTPLanguageList; host, sBaseURL: String; aType: String;
  Session: TFHIRSession): String;
begin
  result := '';
  raise EFslException.Create('Uploads are not supported for the terminology server');
end;

function TTerminologyServerWebServer.data: TTerminologyServerData;
begin
  result := FEndPoint.FStore.FData;
end;

function TTerminologyServerWebServer.description: String;
begin
  result := 'Terminology server for v'+factory.versionString;
end;

function TTerminologyServerWebServer.DoSearch(Session: TFHIRSession; rtype: string; langList : THTTPLanguageList; params: String): TFHIRBundleW;
begin
  result := nil;
  raise EFslException.Create('Not done yet');
end;

procedure TTerminologyServerWebServer.PopulateConformance(sender: TObject; conf: TFhirCapabilityStatementW; secure: boolean; baseUrl: String; caps: array of String);
var
  rr : TFhirCapabilityStatementRestResourceW;
begin
  inherited PopulateConformance(sender, conf, secure, baseUrl, caps);
  rr := conf.addResource('CodeSystem');
  try
    rr.addInteraction('read', 'Read a code system');
    rr.addInteraction('search-type', 'Search the code systems. Not that there a few major code systems that are not available through this API (SCT, LOINC etc)');
    rr.addOperation('validate-code', 'http://hl7.org/fhir/OperationDefinition/CodeSystem-validate-code', '');
    rr.addOperation('lookup', 'http://hl7.org/fhir/OperationDefinition/CodeSystem-lookup', '');
  finally
    rr.free;
  end;

  rr := conf.addResource('ValueSet');
  try
    rr.addInteraction('read', 'Read a ValueSet');
    rr.addInteraction('search-type', 'Search the value sets');
    rr.addOperation('validate-code', 'http://hl7.org/fhir/OperationDefinition/ValueSet-validate-code', '');
    rr.addOperation('expand', 'http://hl7.org/fhir/OperationDefinition/ValueSet-expand', '');
  finally
    rr.free;
  end;

  conf.addInstantiates('http://hl7.org/fhir/CapabilityStatement/terminology-server');
  conf.addTxFeature(TX_TESTS_VERSION);
end;

function TTerminologyServerWebServer.factory: TFHIRFactory;
begin
  result := FEndPoint.FStore.Factory;
end;

procedure TTerminologyServerWebServer.GetWebUILink(resource: TFhirResourceV; base, statedType, id, ver: String; var link, text: String);
begin
  link := '';
  text := '';
end;

function TTerminologyServerWebServer.HandleWebUIRequest(request: TFHIRRequest; response: TFHIRResponse; secure: boolean): TDateTime;
begin
  result := 0;
  raise EFslException.Create('The WebUI is not supported for the terminology server');
end;

function TTerminologyServerWebServer.ProcessZip(langList : THTTPLanguageList; oStream: TStream; name, base: String; init: boolean; ini: TFHIRServerConfigFile; Context: TOperationContext; var cursor: integer): TFHIRBundleW;
begin
  result := nil;
  raise EFslException.Create('Uploads are not supported for the terminology server');
end;

function TTerminologyServerWebServer.terminologies: TCommonTerminologies;
begin
  result := FEndPoint.FServerContext.TerminologyServer.CommonTerminologies;
end;

end.
