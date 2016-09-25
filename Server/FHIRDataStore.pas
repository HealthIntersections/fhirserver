unit FHIRDataStore;

{
  Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

uses
  SysUtils, Classes, IniFiles, Generics.Collections,
  kCritSct, DateSupport, kDate, DateAndTime, StringSupport, GuidSupport, OidSupport,
  ParseMap, TextUtilities,
  AdvNames, AdvObjects, AdvStringMatches, AdvExclusiveCriticalSections,
  AdvStringBuilders, AdvGenerics, AdvExceptions, AdvBuffers,
  KDBManager, KDBDialects,
  FHIRResources, FHIRBase, FHIRTypes, FHIRParser, FHIRParserBase, FHIRConstants,
  FHIRTags, FHIRValueSetExpander, FHIRValidator, FHIRIndexManagers, FHIRSupport,
  FHIRUtilities, FHIRSubscriptionManager, FHIRSecurity, FHIRLang, FHIRProfileUtilities, FHIRPath,
  ServerUtilities, ServerValidator, TerminologyServices, TerminologyServer, SCIMObjects, SCIMServer, DBInstaller;

const
  OAUTH_LOGIN_PREFIX = 'os9z4tw9HdmR-';
  OAUTH_SESSION_PREFIX = 'b35b7vX3KTAe-';
  IMPL_COOKIE_PREFIX = 'implicit-';

Type
  TQuestionnaireCache = class(TAdvObject)
  private
    FLock: TCriticalSection;
    FQuestionnaires: TAdvMap<TFhirQuestionnaire>;
    FForms: TAdvStringMatch;
    FValueSetDependencies: TDictionary<String, TList<string>>;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    procedure putQuestionnaire(rtype: TFHIRResourceType; id: String; q: TFhirQuestionnaire; dependencies: TList<String>);
    procedure putForm(rtype: TFHIRResourceType; id: String; form: String; dependencies: TList<String>);

    function getQuestionnaire(rtype: TFHIRResourceType; id: String) : TFhirQuestionnaire;
    function getForm(rtype: TFHIRResourceType; id: String): String;

    procedure clear(rtype: TFHIRResourceType; id: String); overload;
    procedure clearVS(id: string);
    procedure clear; overload;
  end;

  TFHIRDataStore = class(TAdvObject)
  private
    FDB: TKDBManager;
    FSCIMServer: TSCIMServer;
    FTerminologyServer: TTerminologyServer;
    // folder in which the FHIR specification itself is found
    FSessions: TStringList;
    FTags: TFHIRTagList;
    FTagsByKey: TAdvMap<TFHIRTag>;
    FLock: TCriticalSection;
    FLastSessionKey: integer;
    FLastSearchKey: integer;
    FLastVersionKey: integer;
    FLastTagVersionKey: integer;
    FLastTagKey: integer;
    FLastResourceKey: integer;
    FLastEntryKey: integer;
    FLastCompartmentKey: integer;
    FValidatorContext : TFHIRServerWorkerContext;
    FValidator: TFHIRValidator;
    FResConfig: TAdvMap<TFHIRResourceConfig>;
    FSupportTransaction: Boolean;
    FDoAudit: Boolean;
    FSupportSystemHistory: Boolean;
    FBases: TStringList;
    FTotalResourceCount: integer;
    FFormalURLPlain: String;
    FFormalURLSecure: String;
    FFormalURLPlainOpen: String;
    FFormalURLSecureOpen: String;
    FFormalURLSecureClosed: String;
    FOwnerName: String;
    FSubscriptionManager: TSubscriptionManager;
    FQuestionnaireCache: TQuestionnaireCache;
    FAppFolder : String;
    {$IFDEF FHIR3}
    FMaps : TAdvMap<TFHIRStructureMap>;
    {$ENDIF}
    FNamingSystems : TAdvMap<TFHIRNamingSystem>;
    FClaimQueue: TFHIRClaimList;
    FValidate: Boolean;
    FAudits: TFhirResourceList;
    FNextSearchSweep: TDateTime;
    FSystemId: String;
    FIndexes : TFHIRIndexInformation;
    FForLoad : boolean;

    procedure LoadExistingResources(conn: TKDBConnection);
    procedure RecordFhirSession(session: TFhirSession);
    procedure CloseFhirSession(key: integer);
    function GetSessionByKey(userkey : integer) : TFhirSession;
    procedure checkDefinitions;

    procedure DoExecuteOperation(request: TFHIRRequest; response: TFHIRResponse; bWantSession: Boolean);
    function DoExecuteSearch(typekey: integer; compartmentId, compartments: String; params: TParseMap; conn: TKDBConnection): String;
    function getTypeForKey(key: integer): String;
    procedure doRegisterTag(tag: TFHIRTag; conn: TKDBConnection);
    procedure checkRegisterTag(tag: TFHIRTag; conn: TKDBConnection);
    procedure RegisterAuditEvent(session: TFhirSession; ip: String);
    procedure RunValidateResource(i : integer; rtype, id : String; bufJson, bufXml : TAdvBuffer; b : TStringBuilder);

    procedure loadCustomResources(guides : TAdvStringSet);
  public
    constructor Create(DB: TKDBManager; AppFolder: String; TerminologyServer: TTerminologyServer; ini: TIniFile; SCIMServer: TSCIMServer; loadStore : boolean);
    Destructor Destroy; Override;
    Function Link: TFHIRDataStore; virtual;
    procedure CloseAll;
    procedure SaveResource(res: TFhirResource; dateTime: TDateAndTime; origin : TFHIRRequestOrigin);
    function GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean;
    function GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean;
    Function CreateImplicitSession(clientInfo: String; server: Boolean) : TFhirSession;
    Procedure EndSession(sCookie, ip: String);
    function RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
    procedure MarkSessionChecked(sCookie, sName: String);
    function isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean;
    function ProfilesAsOptionList: String;
    function NextVersionKey: integer;
    function NextTagVersionKey: integer;
    function NextSearchKey: integer;
    function NextResourceKeySetId(aType: String; id: string) : integer;
    function NextResourceKeyGetId(aType: String; var id: string): integer;
    function NextEntryKey: integer;
    function NextCompartmentKey: integer;
    Function GetNextKey(keytype: TKeyType; aType: String; var id: string): integer;
    procedure RegisterTag(tag: TFHIRTag; conn: TKDBConnection); overload;
    procedure RegisterTag(tag: TFHIRTag); overload;
    procedure SeeResource(key, vkey: integer; id: string; needsSecure, created : boolean; resource: TFhirResource; conn: TKDBConnection; reload: Boolean; session: TFhirSession);
    procedure DropResource(key, vkey: integer; id, resource: string; indexer: TFhirIndexManager);
    procedure RegisterConsentRecord(session: TFhirSession);
    function KeyForTag(category : TFHIRTagCategory; system, code: String): integer;
    Property Validator: TFHIRValidator read FValidator;
    function GetTagByKey(key: integer): TFHIRTag;
    Property DB: TKDBManager read FDB;
    Property ResConfig: TAdvMap<TFHIRResourceConfig> read FResConfig;
    Property SupportTransaction: Boolean read FSupportTransaction;
    Property DoAudit: Boolean read FDoAudit;
    Property SupportSystemHistory: Boolean read FSupportSystemHistory;
    Property Bases: TStringList read FBases;
    Property TotalResourceCount: integer read FTotalResourceCount;
    Property TerminologyServer: TTerminologyServer read FTerminologyServer;
    procedure Sweep;
    property FormalURLPlain: String read FFormalURLPlain write FFormalURLPlain;
    property FormalURLSecure: String read FFormalURLSecure
      write FFormalURLSecure;
    property FormalURLPlainOpen: String read FFormalURLPlainOpen
      write FFormalURLPlainOpen;
    property FormalURLSecureOpen: String read FFormalURLSecureOpen
      write FFormalURLSecureOpen;
    property FormalURLSecureClosed: String read FFormalURLSecureClosed
      write FFormalURLSecureClosed;
    function ResourceTypeKeyForName(name: String): integer;
    procedure ProcessSubscriptions;
    function GenerateClaimResponse(claim: TFhirClaim): TFhirClaimResponse;
    {$IFDEF FHIR3}
    function getMaps : TAdvMap<TFHIRStructureMap>;
    {$ENDIF}
    function oid2Uri(oid : String) : String;

    Property OwnerName: String read FOwnerName write FOwnerName;
    Property ValidatorContext : TFHIRServerWorkerContext read FValidatorContext;
    function ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer;
      allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet;
    function LookupCode(system, code: String): String;
    property QuestionnaireCache: TQuestionnaireCache read FQuestionnaireCache;
    Property Validate: Boolean read FValidate write FValidate;
    procedure QueueResource(r: TFhirResource); overload;
    procedure QueueResource(r: TFhirResource; dateTime: TDateAndTime); overload;
    procedure RunValidation;
    property SystemId: String read FSystemId;

    function DumpSessions : String;

    property SubscriptionManager : TSubscriptionManager read FSubscriptionManager;
    property Indexes : TFHIRIndexInformation read FIndexes;
    property ForLoad : boolean read FForLoad write FForLoad;
  end;

implementation

uses
  FHIRLog, SystemService,
  FHIROperation, SearchProcessor;

function chooseFile(fReal, fDev : String) : String;
begin
  if FileExists(fDev) then
    result := fDev
  else
    result := fReal;
end;

{ TFHIRRepository }

procedure TFHIRDataStore.CloseAll;
var
  i: integer;
  session: TFhirSession;
begin
  FLock.Lock('close all');
  try
    for i := FSessions.Count - 1 downto 0 do
    begin
      session := TFhirSession(FSessions.Objects[i]);
      session.free;
      FSessions.Delete(i);
    end;
  finally
    FLock.Unlock;
  end;
end;

constructor TFHIRDataStore.Create(DB: TKDBManager; AppFolder: String; TerminologyServer: TTerminologyServer; ini: TIniFile; SCIMServer: TSCIMServer; loadStore : boolean);
var
  i : integer;
  conn: TKDBConnection;
  a: TFHIRResourceType;
  rn, fn : String;
  implGuides : TAdvStringSet;
  cfg : TFHIRResourceConfig;
begin
  inherited Create;
  LoadMessages; // load while thread safe
  FIndexes := TFHIRIndexInformation.create;
  FBases := TStringList.Create;
  FBases.add('http://localhost/');
  FAppFolder := AppFolder;
  FResConfig := TAdvMap<TFHIRResourceConfig>.create;
  for a := low(TFHIRResourceType) to high(TFHIRResourceType) do
  begin
    cfg := TFHIRResourceConfig.Create;
    cfg.name := CODES_TFHIRResourceType[a];
    cfg.Supported := false;
    FResConfig.Add(cfg.name,  cfg);
  end;
  FDB := DB;
  FSessions := TStringList.Create;
  FTags := TFHIRTagList.Create;
  FLock := TCriticalSection.Create('fhir-store');
  FSCIMServer := SCIMServer;
  FAudits := TFhirResourceList.Create;

  FQuestionnaireCache := TQuestionnaireCache.Create;
  FClaimQueue := TFHIRClaimList.Create;
  {$IFDEF FHIR3}
  FMaps := TAdvMap<TFHIRStructureMap>.create;
  {$ENDIF}
  FNamingSystems := TAdvMap<TFHIRNamingSystem>.create;

  FSubscriptionManager := TSubscriptionManager.Create(ValidatorContext.link, FIndexes.Compartments.Link);
  FSubscriptionManager.dataBase := FDB.Link;
  FSubscriptionManager.Base := 'http://localhost/';
  FSubscriptionManager.SMTPHost := ini.ReadString('email', 'Host', '');
  FSubscriptionManager.SMTPPort := ini.ReadString('email', 'Port', '');
  FSubscriptionManager.SMTPUsername := ini.ReadString('email', 'Username', '');
  FSubscriptionManager.SMTPPassword := ini.ReadString('email', 'Password', '');
  FSubscriptionManager.SMTPUseTLS := ini.ReadBool('email', 'Secure', false);
  FSubscriptionManager.SMTPSender := ini.ReadString('email', 'Sender', '');
  FSubscriptionManager.SMSAccount := ini.ReadString('sms', 'account', '');
  FSubscriptionManager.SMSToken := ini.ReadString('sms', 'token', '');
  FSubscriptionManager.SMSFrom := ini.ReadString('sms', 'from', '');
  FSubscriptionManager.OnExecuteOperation := DoExecuteOperation;
  FSubscriptionManager.OnExecuteSearch := DoExecuteSearch;
  FSubscriptionManager.OnGetSessionEvent := GetSessionByKey;

  implGuides := TAdvStringSet.create;
  try
    conn := FDB.GetConnection('setup');
    try
      FLastSessionKey := conn.CountSQL('Select max(SessionKey) from Sessions');
      FLastVersionKey := conn.CountSQL
        ('Select Max(ResourceVersionKey) from Versions');
      FLastTagVersionKey :=
        conn.CountSQL('Select Max(ResourceTagKey) from VersionTags');
      FLastSearchKey := conn.CountSQL('Select Max(SearchKey) from Searches');
      FLastTagKey := conn.CountSQL('Select Max(TagKey) from Tags');
      FLastResourceKey := conn.CountSQL('select Max(ResourceKey) from Ids');
      FLastEntryKey := conn.CountSQL('select max(EntryKey) from indexEntries');
      FLastCompartmentKey :=
        conn.CountSQL('select max(ResourceCompartmentKey) from Compartments');
      conn.execSQL('Update Sessions set Closed = ' +
        DBGetDate(conn.Owner.Platform) + ' where Closed = null');

      conn.SQL := 'Select TagKey, Kind, Uri, Code, Display from Tags';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
      begin
        FTags.addTag(conn.ColIntegerByName['TagKey'], TFHIRTagCategory(conn.ColIntegerByName['Kind']), conn.ColStringByName['Uri'], conn.ColStringByName['Code'], conn.ColStringByName['Display']).ConfirmedStored := true;
      end;
      conn.terminate;

      conn.SQL := 'Select * from Config';
      conn.Prepare;
      conn.Execute;
      while conn.FetchNext do
        if conn.ColIntegerByName['ConfigKey'] = 1 then
          FSupportTransaction := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 2 then
          FBases.add(AppendForwardSlash(conn.ColStringByName['Value']))
        else if conn.ColIntegerByName['ConfigKey'] = 3 then
          FSupportSystemHistory := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 4 then
          FDoAudit := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 6 then
          FSystemId := conn.ColStringByName['Value']
        else if conn.ColIntegerByName['ConfigKey'] = 7 then
          FResConfig[''].cmdSearch := conn.ColStringByName['Value'] = '1'
        else if conn.ColIntegerByName['ConfigKey'] = 8 then
        begin
          if conn.ColStringByName['Value'] <> FHIR_GENERATED_VERSION then
            raise Exception.Create('Database FHIR Version mismatch. The database contains DSTU'+conn.ColStringByName['Value']+' resources, but this server is based on DSTU'+FHIR_GENERATED_VERSION)
        end
        else if conn.ColIntegerByName['ConfigKey'] <> 5 then
          raise Exception.Create('Unknown Configuration Item '+conn.ColStringByName['ConfigKey']);

      conn.terminate;
      conn.SQL := 'Select * from Types';
      conn.Prepare;
      conn.Execute;
      While conn.FetchNext do
      begin
        rn := conn.ColStringByName['ResourceName'];
        if conn.ColStringByName['ImplementationGuide'] <> '' then
          implGuides.add(conn.ColStringByName['ImplementationGuide']);

        if FResConfig.ContainsKey(rn) then
          cfg := FResConfig[rn]
        else
        begin
          cfg := TFHIRResourceConfig.Create;
          cfg.name := rn;
          FResConfig.Add(cfg.name, cfg);
        end;
        cfg.key := conn.ColIntegerByName['ResourceTypeKey'];
        cfg.Supported := conn.ColStringByName['Supported'] = '1';
        cfg.IdGuids := conn.ColStringByName['IdGuids'] = '1';
        cfg.IdClient := conn.ColStringByName['IdClient'] = '1';
        cfg.IdServer := conn.ColStringByName['IdServer'] = '1';
        cfg.cmdUpdate := conn.ColStringByName['cmdUpdate'] = '1';
        cfg.cmdDelete := conn.ColStringByName['cmdDelete'] = '1';
        cfg.cmdValidate := conn.ColStringByName['cmdValidate'] = '1';
        cfg.cmdHistoryInstance := conn.ColStringByName['cmdHistoryInstance'] = '1';
        cfg.cmdHistoryType := conn.ColStringByName['cmdHistoryType'] = '1';
        cfg.cmdSearch := conn.ColStringByName['cmdSearch'] = '1';
        cfg.cmdCreate := conn.ColStringByName['cmdCreate'] = '1';
        cfg.cmdOperation := conn.ColStringByName['cmdOperation'] = '1';
        cfg.versionUpdates := conn.ColStringByName['versionUpdates'] = '1';
        cfg.LastResourceId := conn.ColIntegerByName['LastId'];
      end;
      conn.terminate;
      conn.SQL :=
        'select ResourceTypeKey, max(CASE WHEN ISNUMERIC(RTRIM(Id) + ''.0e0'') = 1 THEN CAST(Id AS bigINT) ELSE 0 end) as MaxId from Ids group by ResourceTypeKey';
      conn.Prepare;
      conn.Execute;
      While conn.FetchNext do
      begin
        rn := getTypeForKey(conn.ColIntegerByName['ResourceTypeKey']);
        if StringIsInteger32(conn.ColStringByName['MaxId']) and (conn.ColIntegerByName['MaxId'] > FResConfig[rn].LastResourceId) then
          raise Exception.Create('Error in database - LastResourceId (' +
            inttostr(FResConfig[rn].LastResourceId) + ') < MaxId (' +
            inttostr(conn.ColIntegerByName['MaxId']) + ') found for ' +
            rn);
      end;
      conn.terminate;

      FTagsByKey := TAdvMap<TFHIRTag>.Create;
      for i := 0 to FTags.Count - 1 do
        FTagsByKey.add(inttostr(FTags[i].key), FTags[i].Link);

      FIndexes.ReconcileIndexes(conn);

      FValidatorContext := TFHIRServerWorkerContext.Create;
      FValidator := TFHIRValidator.Create(FValidatorContext.link);

      if TerminologyServer <> nil then
      begin
        // the expander is tied to what's on the system
        FTerminologyServer := TerminologyServer.Link;
        FValidatorContext.TerminologyServer := TerminologyServer.Link;

        // the order here is important: specification resources must be loaded prior to stored resources
        {$IFDEF FHIR3}
        fn := ChooseFile(IncludeTrailingPathDelimiter(FAppFolder) + 'definitions.json.zip', 'C:\work\org.hl7.fhir\build\publish\definitions.json.zip');
        {$ELSE}
        fn := ChooseFile(IncludeTrailingPathDelimiter(FAppFolder) + 'validation.json.zip', 'C:\work\org.hl7.fhir.dstu2\build\publish\validation.json.zip');
        {$ENDIF}

        logt('Load Validation Pack from ' + fn);
        FValidatorContext.LoadFromDefinitions(fn);
        if loadStore then
        begin
          logt('Load Custom Resources');
          LoadCustomResources(implGuides);
          logt('Load Store');
          LoadExistingResources(conn);
          logt('Check Definitions');
  //        checkDefinitions();
        end;
        logt('Load Subscription Queue');
        FSubscriptionManager.LoadQueue(conn);
      end;
      conn.Release;
    except
      on e: Exception do
      begin
        conn.Error(e);
        recordStack(e);
        raise;
      end;
    end;
  finally
    implGuides.free;
  end;
end;

function TFHIRDataStore.CreateImplicitSession(clientInfo: String;
  server: Boolean): TFhirSession;
var
  session: TFhirSession;
  dummy: Boolean;
  new: Boolean;
  se: TFhirAuditEvent;
  C: TFHIRCoding;
  p: TFhirAuditEventParticipant;
  key : integer;
begin
  new := false;
  FLock.Lock('CreateImplicitSession');
  try
    if not GetSession(IMPL_COOKIE_PREFIX + clientInfo, result, dummy) then
    begin
      new := true;
      session := TFhirSession.Create(FValidatorContext.link, false);
      try
        inc(FLastSessionKey);
        session.key := FLastSessionKey;
        session.id := '';
        session.name := clientInfo;
        session.expires := UniversalDateTime + DATETIME_SECOND_ONE * 60 * 60;
        // 1 hour
        session.Cookie := '';
        session.provider := apNone;
        session.originalUrl := '';
        session.email := '';
        session.anonymous := true;
        session.userkey := 0;
        FSessions.AddObject(IMPL_COOKIE_PREFIX + clientInfo, session.Link);
        result := session.Link as TFhirSession;
      finally
        session.free;
      end;
    end;
  finally
    FLock.Unlock;
  end;
  if new then
  begin
    if server then
      session.User := FSCIMServer.loadUser(SCIM_SYSTEM_USER, key)
    else
      session.User := FSCIMServer.loadUser(SCIM_ANONYMOUS_USER, key);
    session.name := session.User.username + ' (' + clientInfo + ')';
    session.UserKey := key;
    session.scopes := TFHIRSecurityRights.allScopes;
    // though they'll only actually get what the user allows
    RecordFhirSession(result);
    se := TFhirAuditEvent.Create;
    try
      se.event := TFhirAuditEventEvent.Create;
      se.event.type_ := TFHIRCoding.Create;
      C := se.event.type_;
      C.code := '110114';
      C.system := 'http://nema.org/dicom/dcid';
      C.Display := 'User Authentication';
      C := se.event.subtypeList.append;
      C.code := '110122';
      C.system := 'http://nema.org/dicom/dcid';
      C.Display := 'Login';
      se.event.action := AuditEventActionE;
      se.event.outcome := AuditEventOutcome0;
      se.event.dateTime := NowUTC;
      se.source := TFhirAuditEventSource.Create;
      se.source.site := FOwnerName;
      se.source.identifier := TFhirIdentifier.Create;
      se.source.identifier.system := 'urn:ietf:rfc:3986';
      se.source.identifier.value := SystemId;

      C := se.source.type_List.append;
      C.code := '3';
      C.Display := 'Web Server';
      C.system := 'http://hl7.org/fhir/security-source-type';

      // participant - the web browser / user proxy
      p := se.participantList.append;
      p.network := TFhirAuditEventParticipantNetwork.Create;
      p.network.address := clientInfo;
      p.network.type_ := NetworkType2;

      QueueResource(se, se.event.dateTime);
    finally
      se.free;
    end;
  end;
end;

procedure TFHIRDataStore.RecordFhirSession(session: TFhirSession);
var
  conn: TKDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL :=
      'insert into Sessions (SessionKey, UserKey, Created, Provider, Id, Name, Email, Expiry) values (:sk, :uk, :d, :p, :i, :n, :e, :ex)';
    conn.Prepare;
    conn.BindInteger('sk', session.key);
    conn.BindInteger('uk', StrToInt(session.User.id));
    conn.BindTimeStamp('d', DateTimeToTS(now));
    conn.BindInteger('p', integer(session.provider));
    conn.BindString('i', session.id);
    conn.BindString('n', session.name);
    conn.BindString('e', session.email);
    conn.BindTimeStamp('ex', DateTimeToTS(session.expires));
    conn.Execute;
    conn.terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;

end;

destructor TFHIRDataStore.Destroy;
begin
  FAudits.free;
  FBases.free;
  FTagsByKey.free;
  FSessions.free;
  FTags.free;
  FSubscriptionManager.free;
  FQuestionnaireCache.free;
  {$IFDEF FHIR3}
  FMaps.Free;
  {$ENDIF}
  FNamingSystems.Free;
  FClaimQueue.free;
  FLock.free;
  FIndexes.free;
  FSCIMServer.free;
  FValidator.free;
  FValidatorContext.Free;
  FTerminologyServer.free;
  FDB.Free;
  FResConfig.free;
  inherited;
end;

procedure TFHIRDataStore.DoExecuteOperation(request: TFHIRRequest; response: TFHIRResponse; bWantSession: Boolean);
var
  storage: TFhirOperationManager;
  context : TOperationContext;
begin
  if bWantSession then
    request.session := CreateImplicitSession('server', true);
  context := TOperationContext.create;
  try
    storage := TFhirOperationManager.Create('en', self.Link);
    try
      storage.OwnerName := OwnerName;
      storage.Connection := FDB.GetConnection('fhir');
      storage.Connection.StartTransact;
      try
        storage.Execute(context, request, response);
        storage.Connection.Commit;
        storage.Connection.Release;
      except
        on e: Exception do
        begin
          storage.Connection.Rollback;
          storage.Connection.Error(e);
          recordStack(e);
          raise;
        end;
      end;
    finally
      storage.free;
    end;
  finally
    context.Free;
  end;
end;

function TFHIRDataStore.DoExecuteSearch(typekey: integer;
  compartmentId, compartments: String; params: TParseMap;
  conn: TKDBConnection): String;
var
  sp: TSearchProcessor;
  spaces: TFHIRIndexSpaces;
begin
  spaces := TFHIRIndexSpaces.Create(conn);
  try
    sp := TSearchProcessor.Create(ResConfig.Link);
    try
      sp.typekey := typekey;
      sp.type_ := getTypeForKey(typekey);
      sp.compartmentId := compartmentId;
      sp.compartments := compartments;
      sp.baseURL := FFormalURLPlainOpen; // todo: what?
      sp.lang := 'en';
      sp.params := params;
      sp.indexes := FIndexes.Link;
      sp.repository := self.Link;
      sp.countAllowed := false;
      sp.build;
      result := sp.filter;
    finally
      sp.free;
    end;
  finally
    spaces.free;
  end;
end;

procedure TFHIRDataStore.EndSession(sCookie, ip: String);
var
  i: integer;
  session: TFhirSession;
  se: TFhirAuditEvent;
  C: TFHIRCoding;
  p: TFhirAuditEventParticipant;
  key: integer;
begin
  key := 0;
  FLock.Lock('EndSession');
  try
    i := FSessions.IndexOf(sCookie);
    if i > -1 then
    begin
      session := TFhirSession(FSessions.Objects[i]);
      try
        se := TFhirAuditEvent.Create;
        try
          se.event := TFhirAuditEventEvent.Create;
          se.event.type_ := TFHIRCoding.Create;
          C := se.event.type_;
          C.code := '110114';
          C.system := 'http://nema.org/dicom/dcid';
          C.Display := 'User Authentication';
          C := se.event.subtypeList.append;
          C.code := '110123';
          C.system := 'http://nema.org/dicom/dcid';
          C.Display := 'Logout';
          se.event.action := AuditEventActionE;
          se.event.outcome := AuditEventOutcome0;
          se.event.dateTime := NowUTC;
          se.source := TFhirAuditEventSource.Create;
          se.source.site := FOwnerName;
          se.source.identifier := TFhirIdentifier.Create;
          se.source.identifier.system := 'urn:ietf:rfc:3986';
          se.source.identifier.value := SystemId;
          C := se.source.type_List.append;
          C.code := '3';
          C.Display := 'Web Server';
          C.system := 'http://hl7.org/fhir/security-source-type';

          // participant - the web browser / user proxy
          p := se.participantList.append;
          p.userId := TFhirIdentifier.Create;
          p.userId.system := SystemId;
          p.userId.value := inttostr(session.key);
          p.altId := session.id;
          p.name := session.name;
          if (ip <> '') then
          begin
            p.network := TFhirAuditEventParticipantNetwork.Create;
            p.network.address := ip;
            p.network.type_ := NetworkType2;
            p.requestor := true;
          end;

          QueueResource(se, se.event.dateTime);
        finally
          se.free;
        end;
        key := session.key;
        FSessions.Delete(i);
      finally
        session.free;
      end;
    end;
  finally
    FLock.Unlock;
  end;
  if key > 0 then
    CloseFhirSession(key);
end;

function TFHIRDataStore.ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList) : TFHIRValueSet;
var
  profile : TFhirExpansionProfile;
begin
  profile := TFhirExpansionProfile.Create;
  try
    profile.limitedExpansion := allowIncomplete;
    if (vs <> nil) then
      result := FTerminologyServer.ExpandVS(vs, '', profile, '', dependencies, limit, count, offset)
    else
    begin
      if FTerminologyServer.isKnownValueSet(ref.reference, vs) then
        result := FTerminologyServer.ExpandVS(vs, ref.reference, profile, '', dependencies, limit, count, offset)
      else
      begin
        vs := FTerminologyServer.getValueSetByUrl(ref.reference);
        if vs = nil then
          vs := FTerminologyServer.getValueSetByid(ref.reference);
        if vs = nil then
          result := nil
        else
          result := FTerminologyServer.ExpandVS(vs, ref.reference, profile, '',
            dependencies, limit, count, offset)
      end;
    end;
  finally
    profile.free;
  end;
end;

procedure TFHIRDataStore.CloseFhirSession(key: integer);
var
  conn: TKDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL := 'Update Sessions set closed = :d where SessionKey = ' +
      inttostr(key);
    conn.Prepare;
    conn.BindTimeStamp('d', DateTimeToTS(UniversalDateTime));
    conn.Execute;
    conn.terminate;
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;

end;

function TFHIRDataStore.GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean;
var
  key, i: integer;
begin
  key := 0;
  FLock.Lock('GetSession');
  try
    i := FSessions.IndexOf(sCookie);
    result := i > -1;
    if result then
    begin
      session := TFhirSession(FSessions.Objects[i]);
      session.useCount := session.useCount + 1;
      if session.expires > UniversalDateTime then
      begin
        session.Link;
        check := (session.provider in [apFacebook, apGoogle]) and
          (session.NextTokenCheck < UniversalDateTime);
      end
      else
      begin
        result := false;
        try
          key := session.key;
          FSessions.Delete(i);
        finally
          session.free;
        end;
      end;
    end;
  finally
    FLock.Unlock;
  end;
  if key > 0 then
    CloseFhirSession(key);
end;

function TFHIRDataStore.GetSessionByKey(userkey: integer): TFhirSession;
var
  c, i, key: integer;
begin
  c := -1;
  key := 0;
  result := nil;
  FLock.Lock('GetSession');
  try
    for i := 0 to FSessions.Count - 1 do
      if TFhirSession(FSessions.Objects[i]).UserKey = userkey then
        c := i;
    if (c <> -1) then
    begin
      result := FSessions.Objects[c] as TFhirSession;
      result.useCount := result.useCount + 1;
      if (result.expires > UniversalDateTime) and not ((result.provider in [apFacebook, apGoogle]) and (result.NextTokenCheck < UniversalDateTime)) then
        result.Link
      else
      begin
        key := result.Key;
        FSessions.Delete(c);
        result.Free;
        result := nil;
      end;
    end;
  finally
    FLock.Unlock;
  end;
  if c > 0 then
    CloseFhirSession(c);
  if result = nil then
  begin
    result := TFhirSession.Create(FValidatorContext.Link, true);
    try
      result.innerToken := NewGuidURN;
      result.outerToken := NewGuidURN;
      result.id := NewGuidURN;
      result.UserKey := userkey;
      result.User := FSCIMServer.loadUser(userkey);
      result.name := result.User.formattedName;
      result.expires := LocalDateTime + DATETIME_SECOND_ONE * 500;
      result.Cookie := NewGuidURN;
      result.provider := apInternal;
      result.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
      result.scopes := TFHIRSecurityRights.allScopes;
      FLock.Lock('RegisterSession2');
      try
        inc(FLastSessionKey);
        result.key := FLastSessionKey;
        FSessions.AddObject(result.Cookie, result.Link);
      finally
        FLock.Unlock;
      end;
      RegisterAuditEvent(result, 'Subscription.Hook');
      result.Link;
    finally
      result.Free;
    end;
  end;
  RecordFhirSession(result);
end;

function TFHIRDataStore.GetSessionByToken(outerToken: String;
  var session: TFhirSession): Boolean;
var
  i: integer;
begin
  result := false;
  session := nil;
  FLock.Lock('GetSessionByToken');
  try
    for i := 0 to FSessions.Count - 1 do
      if (TFhirSession(FSessions.Objects[i]).outerToken = outerToken) or
        (TFhirSession(FSessions.Objects[i]).JWTPacked = outerToken) then
      begin
        result := true;
        session := TFhirSession(FSessions.Objects[i]).Link;
        session.useCount := session.useCount + 1;
        break;
      end;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.GetTagByKey(key: integer): TFHIRTag;
begin
  FLock.Lock('GetTagByKey');
  try
    if FTagsByKey.TryGetValue(inttostr(key), result) then
      result := result.Link
    else
      result := nil;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.getTypeForKey(key: integer): String;
var
  a: TFHIRResourceConfig;
begin
  FLock.Lock('getTypeForKey');
  try
    result := '';
    for a in FResConfig.Values do
      if a.key = key then
      begin
        result := a.Name;
        exit;
      end;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean;
var
  id, hash, username, password: String;
  i, key: integer;
  se: TFhirAuditEvent;
  C: TFHIRCoding;
  p: TFhirAuditEventParticipant;
begin
  result := false;
  session := nil;
  FLock.Lock('GetSessionByToken');
  try
    for i := 0 to FSessions.Count - 1 do
      if (TFhirSession(FSessions.Objects[i]).innerToken = token) and
        (TFhirSession(FSessions.Objects[i]).outerToken = '$BEARER') then
      begin
        result := true;
        session := TFhirSession(FSessions.Objects[i]).Link;
        session.useCount := session.useCount + 1;
        break;
      end;
  finally
    FLock.Unlock;
  end;
  if (not result) then
  begin
    StringSplit(token, '.', id, hash);
    result := StringIsInteger32(id) and FSCIMServer.CheckId(id, username,
      password);
    if (result and (password = hash)) then
    begin
      session := TFhirSession.Create(FValidatorContext.Link, true);
      try
        session.innerToken := token;
        session.outerToken := '$BEARER';
        session.id := id;
        session.User := FSCIMServer.loadUser(username, key);
        session.UserKey := key;
        session.name := session.User.bestName;
        session.expires := LocalDateTime + DATETIME_SECOND_ONE * 0.25;
        session.provider := apInternal;
        session.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
        session.scopes := TFHIRSecurityRights.allScopes;
        if (session.User.emails.Count > 0) then
          session.email := session.User.emails[0].value;
        // session.scopes := ;
        FLock.Lock('CreateImplicitSession');
        try
          inc(FLastSessionKey);
          session.key := FLastSessionKey;
          FSessions.AddObject(token, session.Link);
          session.Link;
        finally
          FLock.Unlock;
        end;
      finally
        session.free;
      end;
      RecordFhirSession(session);
      se := TFhirAuditEvent.Create;
      try
        se.event := TFhirAuditEventEvent.Create;
        se.event.type_ := TFHIRCoding.Create;
        C := se.event.type_;
        C.code := '110114';
        C.system := 'http://nema.org/dicom/dcid';
        C.Display := 'User Authentication';
        C := se.event.subtypeList.append;
        C.code := '110122';
        C.system := 'http://nema.org/dicom/dcid';
        C.Display := 'Login';
        se.event.action := AuditEventActionE;
        se.event.outcome := AuditEventOutcome0;
        se.event.dateTime := NowUTC;
        se.source := TFhirAuditEventSource.Create;
        se.source.site := FOwnerName;
        se.source.identifier := TFhirIdentifier.Create;
        se.source.identifier.system := 'urn:ietf:rfc:3986';
        se.source.identifier.value := SystemId;
        C := se.source.type_List.append;
        C.code := '3';
        C.Display := 'Web Server';
        C.system := 'http://hl7.org/fhir/security-source-type';

        // participant - the web browser / user proxy
        p := se.participantList.append;
        p.userId := TFhirIdentifier.Create;
        p.userId.system := SystemId;
        p.userId.value := inttostr(session.key);
        p.network := TFhirAuditEventParticipantNetwork.Create;
        p.network.address := clientInfo;
        p.network.type_ := NetworkType2;
        QueueResource(se, se.event.dateTime);
      finally
        se.free;
      end;
    end
    else
      result := false;
  end;
end;

function TFHIRDataStore.KeyForTag(category : TFHIRTagCategory; system, code: String): integer;
var
  p: TFHIRTag;
begin
  FLock.Lock('KeyForTag');
  try
    p := FTags.findTag(category, system, code);
    if (p = nil) then
      result := 0
    else
      result := p.key;
  finally
    FLock.Unlock;
  end;

end;

procedure TFHIRDataStore.MarkSessionChecked(sCookie, sName: String);
var
  i: integer;
  session: TFhirSession;
begin
  FLock.Lock('MarkSessionChecked');
  try
    i := FSessions.IndexOf(sCookie);
    if i > -1 then
    begin
      session := TFhirSession(FSessions.Objects[i]);
      session.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
      session.name := sName;
    end;
  finally
    FLock.Unlock;
  end;

end;

function TFHIRDataStore.NextTagVersionKey: integer;
begin
  FLock.Lock('NextTagVersionKey');
  try
    inc(FLastTagVersionKey);
    result := FLastTagVersionKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.NextVersionKey: integer;
begin
  FLock.Lock('NextVersionKey');
  try
    inc(FLastVersionKey);
    result := FLastVersionKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.oid2Uri(oid: String): String;
var
  ns : TFHIRNamingSystem;
begin
  result := '';
  FLock.Lock;
  try
		result := UriForKnownOid(oid);
		if (result = '') then
    begin
  		for ns in FNamingSystems.Values do
      begin
        if ns.hasOid(oid) then
        begin
          result := ns.getUri;
          if (result <> '') then
            exit;
        end;
      end;
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRDataStore.RegisterConsentRecord(session: TFhirSession);
{$IFNDEF FHIR2}
var
  pc: TFhirConsent;
begin
  if session.PatientList.Count = 1 then
  begin
    pc := TFhirConsent.Create;
    try
      pc.status := ConsentStatusActive;
      with pc.categoryList.Append.codingList.append do
      begin
        system := 'http://hl7.org/fhir/consentcategorycodes';
        code := 'smart-on-fhir';
      end;
      pc.dateTime := NowUTC;
      pc.period := TFHIRPeriod.Create;
      pc.period.start := pc.dateTime.Link;
      pc.period.end_ := TDateAndTime.CreateUTC(session.expires);
      pc.patient := TFHIRReference.Create;
      pc.patient.reference := 'Patient/'+session.PatientList[0];
      // todo: do we have a reference for the consentor?
      // todo: do we have an identity for the organization?
  //    for
  //
  //    with pc.except_List.Append do
  //    begin
  //      type_ := ConsentExceptTypePermit;
  //      action := TFHIRCodeableConcept.Create;
  //      action.codingList.add(TFHIRCoding.Create('http://hl7.org/fhir/consentaction', 'read')));
  //    end;
  //  finally
  //
  //  end;
    finally
      pc.Free;
    end;
  end;
{$ELSE}
var
  ct: TFhirContract;
  s: String;
begin
  ct := TFhirContract.Create;
  try
    ct.issued := NowUTC;
    ct.applies := TFHIRPeriod.Create;
    ct.applies.start := ct.issued.Link;
    ct.applies.end_ := TDateAndTime.CreateUTC(session.expires);
    // need to figure out who this is...   ct.subjectList.Append.reference := '
    ct.type_ := TFhirCodeableConcept.Create;
    with ct.type_.codingList.append do
    begin
      code := 'disclosure';
      system := 'http://hl7.org/fhir/contracttypecodes';
    end;
    ct.subtypeList.append.text := 'Smart on FHIR Authorization';
    with ct.actionReasonList.append.codingList.append do
    begin
      code := 'PATRQT';
      system := 'http://hl7.org/fhir/v3/ActReason';
      Display := 'patient requested';
    end;
    with ct.actorList.append do
    begin
      roleList.append.text := 'Server Host';
      {$IFNDEF FHIR2}
      entity := TFhirReference.Create;
      entity.reference := 'Device/this-server';
      {$ENDIF}
    end;
    for s in session.scopes.Split([' ']) do
      with ct.actionList.append.codingList.append do
      begin
        code := UriForScope(s);
        system := 'urn:ietf:rfc:3986';
      end;
    QueueResource(ct, ct.issued);
  finally
    ct.free;
  end;
{$ENDIF}
end;

procedure TFHIRDataStore.RegisterAuditEvent(session: TFhirSession; ip: String);
var
  se: TFhirAuditEvent;
  C: TFHIRCoding;
  p: TFhirAuditEventParticipant;
begin
  se := TFhirAuditEvent.Create;
  try
    se.event := TFhirAuditEventEvent.Create;
    se.event.type_ := TFHIRCoding.Create;
    C := se.event.type_;
    C.code := '110114';
    C.system := 'http://nema.org/dicom/dcid';
    C.Display := 'User Authentication';
    C := se.event.subtypeList.append;
    C.code := '110122';
    C.system := 'http://nema.org/dicom/dcid';
    C.Display := 'Login';
    se.event.action := AuditEventActionE;
    se.event.outcome := AuditEventOutcome0;
    se.event.dateTime := NowUTC;
    se.source := TFhirAuditEventSource.Create;
    se.source.site := FOwnerName;
    se.source.identifier := TFhirIdentifier.Create;
    se.source.identifier.system := 'urn:ietf:rfc:3986';
    se.source.identifier.value := SystemId;
    C := se.source.type_List.append;
    C.code := '3';
    C.Display := 'Web Server';
    C.system := 'http://hl7.org/fhir/security-source-type';

    // participant - the web browser / user proxy
    p := se.participantList.append;
    p.userId := TFhirIdentifier.Create;
    p.userId.system := SystemId;
    p.userId.value := inttostr(session.key);
    p.altId := session.id;
    p.name := session.name;
    if (ip <> '') then
    begin
      p.network := TFhirAuditEventParticipantNetwork.Create;
      p.network.address := ip;
      p.network.type_ := NetworkType2;
      p.requestor := true;
    end;

    QueueResource(se, se.event.dateTime);
  finally
    se.free;
  end;
end;

function TFHIRDataStore.RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
var
  session: TFhirSession;
  key : integer;
begin
  session := TFhirSession.Create(FValidatorContext.Link, true);
  try
    session.innerToken := innerToken;
    session.outerToken := outerToken;
    session.id := id;
    session.name := name;
    session.expires := LocalDateTime + DATETIME_SECOND_ONE * StrToInt(expires);
    session.Cookie := OAUTH_SESSION_PREFIX +
      copy(GUIDToString(CreateGuid), 2, 36);
    session.provider := provider;
    session.originalUrl := original;
    session.email := email;
    session.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
    if provider = apInternal then
      session.User := FSCIMServer.loadUser(id, key)
    else
      session.User := FSCIMServer.loadOrCreateUser(USER_SCHEME_PROVIDER[provider] + '#' + id, name, email, key);
    session.UserKey := key;
    if session.name = '' then
      session.name := session.User.bestName;
    if (session.email = '') and (session.User.emails.Count > 0) then
      session.email := session.User.emails[0].value;

    session.scopes := rights;
    // empty, mostly - user will assign them later when they submit their choice

    FLock.Lock('RegisterSession');
    try
      inc(FLastSessionKey);
      session.key := FLastSessionKey;
      FSessions.AddObject(session.Cookie, session.Link);
    finally
      FLock.Unlock;
    end;

    RegisterAuditEvent(session, ip);

    result := session.Link as TFhirSession;
  finally
    session.free;
  end;
  RecordFhirSession(result);
end;

procedure TFHIRDataStore.RegisterTag(tag: TFHIRTag; conn: TKDBConnection);
var
  C: TFHIRTag;
begin
  FLock.Lock('RegisterTag');
  try
    C := FTags.findTag(tag.Category, tag.system, tag.code);
    if C <> nil then
    begin
      tag.key := C.key;
      if tag.Display = '' then
        tag.Display := C.Display;
      checkRegisterTag(tag, conn); // this is required because of a mis-match between the cached tags and the commit scope of doRegisterTag
    end
    else
    begin
      inc(FLastTagKey);
      tag.key := FLastTagKey;
      doRegisterTag(tag, conn);
      FTags.add(tag.Link);
      FTagsByKey.add(inttostr(FLastTagKey), tag.Link);
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRDataStore.doRegisterTag(tag: TFHIRTag; conn: TKDBConnection);
begin
  conn.SQL :=
    'insert into Tags (Tagkey, Kind, Uri, Code, Display) values (:k, :tk, :s, :c, :d)';
  conn.Prepare;
  conn.BindInteger('k', tag.key);
  conn.BindInteger('tk', ord(tag.Category));
  conn.BindString('s', tag.system);
  conn.BindString('c', tag.code);
  conn.BindString('d', tag.Display);
  conn.Execute;
  conn.terminate;
  tag.TransactionId := conn.transactionId;
end;

procedure TFHIRDataStore.checkDefinitions;
var
  s, sx : string;
  c, t : integer;
  fpe : TFHIRExpressionEngine;
  sd : TFhirStructureDefinition;
  ed: TFhirElementDefinition;
  inv : TFhirElementDefinitionConstraint;
  td : TFHIRTypeDetails;
  expr : TFHIRExpressionNode;
begin
  s := '';
  c := 0;
  t := 0;
  fpe:= TFHIRExpressionEngine.create(FValidatorContext.Link);
  try
    for sd in FValidatorContext.Profiles.ProfilesByURL.Values do
      {$IFDEF FHIR2}
      if sd.constrainedType = '' then
      {$ENDIF}

      if sd.snapshot <> nil then
      begin
        for ed in sd.snapshot.elementList do
          for inv in ed.constraintList do
          begin
            sx := {$IFDEF FHIR3} inv.expression {$ELSE} inv.getExtensionString('http://hl7.org/fhir/StructureDefinition/structuredefinition-expression') {$ENDIF};
            if (sx <> '') and not sx.contains('$parent') then
            begin
              inc(t);
              try
                expr := fpe.parse(sx);
                try
                  if sd.kind = StructureDefinitionKindResource then
                    td := fpe.check(nil, sd.id, ed.path, '', expr, false)
                  else
                    td := fpe.check(nil, 'DomainResource', ed.path, '', expr, false);
                  try
                    if (td.hasNoTypes) then
                      s := s + inv.key+' @ '+ed.path+' ('+sd.name+'): no possible result from '+sx + #13#10
                    else
                      inc(c);
                  finally
                    td.free;
                  end;
                finally
                  expr.Free;

                end;
              except
                on e : Exception do
                  s := s + inv.key+' @ '+ed.path+' ('+sd.name+'): exception "'+e.message+'" ('+sx+')' + #13#10;
              end;
            end;
          end;
        end;
  finally
    fpe.Free;
  end;
end;

procedure TFHIRDataStore.checkRegisterTag(tag: TFHIRTag; conn: TKDBConnection);
begin
  if tag.ConfirmedStored then
    exit;

  if conn.CountSQL('select Count(*) from Tags where TagKey = '+inttostr(tag.key)) = 0 then
    doRegisterTag(tag, conn)
  else if conn.transactionId <> tag.TransactionId then
    tag.ConfirmedStored := true;
end;

procedure TFHIRDataStore.RegisterTag(tag: TFHIRTag);
var
  conn: TKDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    doRegisterTag(tag, conn);
    conn.Release;
  except
    on e: Exception do
    begin
      conn.Error(e);
      recordStack(e);
      raise;
    end;
  end;
end;

function TFHIRDataStore.ResourceTypeKeyForName(name: String): integer;
begin
  FLock.Lock('ResourceTypeKeyForName');
  try
    result := FResConfig[name].key;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRDataStore.RunValidateResource(i : integer; rtype, id: String; bufJson, bufXml: TAdvBuffer; b : TStringBuilder);
var
  ctxt : TFHIRValidatorContext;
  issue : TFHIROperationOutcomeIssue;
begin
  try
    ctxt := TFHIRValidatorContext.Create;
    try
      FValidator.validate(ctxt, bufXml, ffXml);
      FValidator.validate(ctxt, bufJson, ffJson);
      if (ctxt.Errors.Count = 0) then
        writeln(inttostr(i)+': '+rtype+'/'+id+': passed validation')
      else
      begin
        writeln(inttostr(i)+': '+rtype+'/'+id+': failed validation');
        b.Append(inttostr(i)+': '+'http://local.healthintersections.com.au:960/open/'+rtype+'/'+id+' : failed validation'+#13#10);
        for issue in ctxt.Errors do
          if (issue.severity in [IssueSeverityFatal, IssueSeverityError]) then
            b.Append('  '+issue.Summary+#13#10);
      end;
    finally
      ctxt.Free;
    end;
  except
    on e:exception do
    begin
      recordStack(e);
      writeln(inttostr(i)+': '+rtype+'/'+id+': exception validating: '+e.message);
      b.Append(inttostr(i)+': '+'http://fhir2.healthintersections.com.au/open/'+rtype+'/'+id+' : exception validating: '+e.message+#13#10);
    end;
  end;
end;

procedure TFHIRDataStore.RunValidation;
var
  conn : TKDBConnection;
  bufJ, bufX : TAdvBuffer;
  b : TStringBuilder;
  i : integer;
begin
  b := TStringBuilder.Create;
  try
    conn := FDB.GetConnection('Run Validation');
    try
      conn.SQL := 'select ResourceTypeKey, Ids.Id, JsonContent, XmlContent from Ids, Versions where Ids.MostRecent = Versions.ResourceVersionKey';
      conn.Prepare;
      try
        conn.Execute;
        i := 0;
        while conn.FetchNext do
        begin
          bufJ := TAdvBuffer.create;
          bufX := TAdvBuffer.create;
          try
            bufJ.asBytes := conn.ColBlobByName['JsonContent'];
            bufX.asBytes := conn.ColBlobByName['XmlContent'];
            inc(i);
//            if (i = 57) then
            RunValidateResource(i, getTypeForKey(conn.ColIntegerByName['ResourceTypeKey']), conn.ColStringByName['Id'], bufJ, bufX, b);
          finally
            bufJ.free;
            bufX.free;
          end;
        end;
      finally
        conn.terminate;
      end;
      conn.release;
    except
      on e:exception do
      begin
        conn.error(e);
        raise;
      end;
    end;
    bufJ := TAdvBuffer.Create;
    try
      bufJ.AsUnicode := b.ToString;
      bufJ.SaveToFileName('c:\temp\validation.txt');
    finally
      bufJ.free;
    end;
  finally
    b.Free;
  end;
end;

procedure TFHIRDataStore.Sweep;
var
  key, i: integer;
  session: TFhirSession;
  d: TDateTime;
  list: TFhirResourceList;
  storage: TFhirOperationManager;
  claim: TFhirClaim;
  resp: TFhirClaimResponse;
  conn: TKDBConnection;
begin
  key := 0;
  list := nil;
  claim := nil;
  d := UniversalDateTime;
  TerminologyServer.BackgroundThreadStatus := 'Sweeping Sessions';
  FLock.Lock('sweep2');
  try
    for i := FSessions.Count - 1 downto 0 do
    begin
      session := TFhirSession(FSessions.Objects[i]);
      if session.expires < d then
      begin
        try
          key := session.key;
          FSessions.Delete(i);
        finally
          session.free;
        end;
      end;
    end;
    if FAudits.Count > 0 then
    begin
      list := FAudits;
      FAudits := TFhirResourceList.Create;
    end;
    if (list = nil) and (FClaimQueue.Count > 0) then
    begin
      claim := FClaimQueue[0].Link;
      FClaimQueue.DeleteByIndex(0);
    end;
  finally
    FLock.Unlock;
  end;
  TerminologyServer.BackgroundThreadStatus := 'Sweeping Search';
  if FNextSearchSweep < d then
  begin
    conn := FDB.GetConnection('Sweep.search');
    try
      conn.SQL :=
        'Delete from SearchEntries where SearchKey in (select SearchKey from Searches where Date < :d)';
      conn.Prepare;
      conn.BindTimeStamp('d', DateTimeToTS(d - 0.3));
      conn.Execute;
      conn.terminate;

      conn.SQL := 'Delete from Searches where Date < :d';
      conn.Prepare;
      conn.BindTimeStamp('d', DateTimeToTS(d - 0.3));
      conn.Execute;
      conn.terminate;

      conn.Release;
    except
      on e: Exception do
      begin
        conn.Error(e);
        recordStack(e);
        raise;
      end;
    end;
    FNextSearchSweep := d + 10 * MINUTE_LENGTH;
  end;

  TerminologyServer.BackgroundThreadStatus := 'Sweeping - Closing';
  try
    if key > 0 then
      CloseFhirSession(key);
    if list <> nil then
    begin
      TerminologyServer.BackgroundThreadStatus := 'Sweeping - audits';
      storage := TFhirOperationManager.Create('en', self.Link);
      try
        storage.OwnerName := OwnerName;
        storage.Connection := FDB.GetConnection('fhir.sweep');
        try
          storage.storeResources(list, roSweep, false);
          storage.Connection.Release;
        except
          on e: Exception do
          begin
            storage.Connection.Error(e);
            recordStack(e);
            raise;
          end;
        end;
      finally
        storage.free;
      end;
    end;
    if (claim <> nil) then
    begin
      TerminologyServer.BackgroundThreadStatus := 'Sweeping - claims';
      resp := GenerateClaimResponse(claim);
      try
        QueueResource(resp, resp.created);
      finally
        resp.free;
      end;
    end;
  finally
    list.free;
  end;
end;

procedure TFHIRDataStore.SeeResource(key, vkey: integer; id: string; needsSecure, created : boolean; resource: TFhirResource; conn: TKDBConnection; reload: Boolean; session: TFhirSession);
begin
  if (resource.ResourceType in [frtValueSet, frtConceptMap, frtStructureDefinition, frtQuestionnaire, frtSubscription]) and (needsSecure or ((resource.meta <> nil) and not resource.meta.securityList.IsEmpty)) then
    raise ERestfulException.Create('TFHIRDataStore', 'SeeResource', 'Resources of type '+CODES_TFHIRResourceType[resource.ResourceType]+' are not allowed to have a security label on them', 400, IssueTypeBusinessRule);

  FLock.Lock('SeeResource');
  try
    if resource.ResourceType in [frtValueSet, frtConceptMap {$IFDEF FHIR3}, frtCodeSystem {$ENDIF}] then
      TerminologyServer.SeeTerminologyResource(resource)
    else if resource.ResourceType = frtStructureDefinition then
      FValidatorContext.seeResource(resource as TFhirStructureDefinition)
    else if resource.ResourceType = frtQuestionnaire then
      FValidatorContext.seeResource(resource as TFhirQuestionnaire);
    FSubscriptionManager.SeeResource(key, vkey, id, created, resource, conn, reload, session);
    FQuestionnaireCache.clear(resource.ResourceType, id);
    if resource.ResourceType = frtValueSet then
      FQuestionnaireCache.clearVS(TFHIRValueSet(resource).url);
    if resource.ResourceType = frtClaim then
      FClaimQueue.add(resource.Link);
    {$IFDEF FHIR3}
    if resource.ResourceType = frtStructureMap then
      FMaps.AddOrSetValue(TFHIRStructureMap(resource).url, TFHIRStructureMap(resource).Link);
    {$ENDIF}
    if resource.ResourceType = frtNamingSystem then
      FNamingSystems.AddOrSetValue(inttostr(key), TFHIRNamingSystem(resource).Link);
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRDataStore.DropResource(key, vkey: integer; id: string; resource: String; indexer: TFhirIndexManager);
var
  i: integer;
  aType : TFhirResourceType;
begin
  i := StringArrayIndexOfSensitive(CODES_TFhirResourceType, resource);
  if i > -1 then
  begin
    aType := TFhirResourceType(i);
    FLock.Lock('DropResource');
    try
      if aType in [frtValueSet, frtConceptMap] then
        TerminologyServer.DropTerminologyResource(aType, id)
      else if aType = frtStructureDefinition then
        FValidatorContext.Profiles.DropProfile(aType, id);
      FSubscriptionManager.DropResource(key, vkey);
      FQuestionnaireCache.clear(aType, id);
      for i := FClaimQueue.Count - 1 downto 0 do
        if FClaimQueue[i].id = id then
          FClaimQueue.DeleteByIndex(i);
    finally
      FLock.Unlock;
    end;
  end;
end;

function TFHIRDataStore.DumpSessions: String;
var
  i: integer;
  session: TFhirSession;
  b : TStringBuilder;
begin
  b := TStringBuilder.Create;
  try
    b.Append('<table>'#13#10);
    b.Append('<tr>');
    b.Append('<td>Session Key</td>');
    b.Append('<td>user Identity</td>');
    b.Append('<td>UserKey</td>');
    b.Append('<td>Name</td>');
    b.Append('<td>Created</td>');
    b.Append('<td>Expires</td>');
    b.Append('<td>Check Time</td>');
    b.Append('<td>Use Count</td>');
    b.Append('<td>Scopes</td>');
    b.Append('<td>Component</td>');
    b.Append('</tr>'#13#10);

    FLock.Lock('DumpSessions');
    try
      for i := FSessions.Count - 1 downto 0 do
      begin
        session := TFhirSession(FSessions.Objects[i]);
        session.describe(b);
        b.Append(#13#10);
      end;
    finally
      FLock.Unlock;
    end;
    b.Append('</table>'#13#10);
    result := b.ToString;
  finally
    b.Free;
  end;
end;

procedure TFHIRDataStore.SaveResource(res: TFhirResource; dateTime: TDateAndTime; origin : TFHIRRequestOrigin);
var
  request: TFHIRRequest;
  response: TFHIRResponse;
begin
  request := TFHIRRequest.Create(ValidatorContext.Link, origin, FIndexes.Compartments.Link);
  try
    request.ResourceName := res.fhirType;
    request.CommandType := fcmdCreate;
    request.resource := res.Link;
    request.lastModifiedDate := dateTime.AsUTCDateTime;
    request.session := nil;
    response := TFHIRResponse.Create;
    try
      DoExecuteOperation(request, response, false);
    finally
      response.free;
    end;
  finally
    request.free;
  end;
end;

procedure TFHIRDataStore.ProcessSubscriptions;
begin
  FSubscriptionManager.Process;
end;

function TFHIRDataStore.ProfilesAsOptionList: String;
var
  i: integer;
  builder: TAdvStringBuilder;
  Profiles: TAdvStringMatch;
begin
  builder := TAdvStringBuilder.Create;
  try
    Profiles := FValidatorContext.Profiles.getLinks(false);
    try
      for i := 0 to Profiles.Count - 1 do
      begin
        builder.append('<option value="');
        builder.append(Profiles.KeyByIndex[i]);
        builder.append('">');
        if Profiles.ValueByIndex[i] = '' then
        begin
          builder.append('@');
          builder.append(Profiles.KeyByIndex[i]);
          builder.append('</option>');
          builder.append(#13#10)
        end
        else
        begin
          builder.append(Profiles.ValueByIndex[i]);
          builder.append('</option>');
          builder.append(#13#10);
        end;
      end;
    finally
      Profiles.free;
    end;
    result := builder.AsString;
  finally
    builder.free;
  end;
end;

procedure TFHIRDataStore.QueueResource(r: TFhirResource; dateTime: TDateAndTime);
begin
  QueueResource(r);
end;

procedure TFHIRDataStore.QueueResource(r: TFhirResource);
begin
  FLock.Lock;
  try
    FAudits.add(r.Link);
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.NextSearchKey: integer;
begin
  FLock.Lock('NextSearchKey');
  try
    inc(FLastSearchKey);
    result := FLastSearchKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.NextResourceKeyGetId(aType: String; var id: string): integer;
begin
  FLock.Lock('NextResourceKey');
  try
    inc(FLastResourceKey);
    result := FLastResourceKey;
    inc(FResConfig[aType].LastResourceId);
    id := inttostr(FResConfig[aType].LastResourceId);
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.NextResourceKeySetId(aType: String; id: string): integer;
var
  i: integer;
begin
  FLock.Lock('NextResourceKey');
  try
    inc(FLastResourceKey);
    result := FLastResourceKey;
    if IsNumericString(id) and StringIsInteger32(id) then
    begin
      i := StrToInt(id);
      if (i > FResConfig[aType].LastResourceId) then
        FResConfig[aType].LastResourceId := i;
    end;
  finally
    FLock.Unlock;
  end;

end;

function TFHIRDataStore.NextEntryKey: integer;
begin
  FLock.Lock('NextEntryKey');
  try
    inc(FLastEntryKey);
    result := FLastEntryKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.NextCompartmentKey: integer;
begin
  FLock.Lock('NextCompartmentKey');
  try
    inc(FLastCompartmentKey);
    result := FLastCompartmentKey;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.GenerateClaimResponse(claim: TFhirClaim)
  : TFhirClaimResponse;
var
  resp: TFhirClaimResponse;
begin
  resp := TFhirClaimResponse.Create;
  try
    resp.created := NowUTC;
    with resp.identifierList.append do
    begin
      system := FBases[0] + '/claimresponses';
      value := claim.id;
    end;
    resp.request := TFhirReference.Create;
    TFhirReference(resp.request).reference := 'Claim/' + claim.id;
//    resp.outcome := RemittanceOutcomeComplete;
    resp.disposition := 'Automatic Response';
//    resp.paymentAmount := {$IFDEF FHIR2}TFHIRQuantity{$ELSE}TFhirMoney{$ENDIF}.Create;
//    resp.paymentAmount.value := '0';
//    resp.paymentAmount.unit_ := '$';
//    resp.paymentAmount.system := 'urn:iso:std:4217';
//    resp.paymentAmount.code := 'USD';
    result := resp.Link;
  finally
    resp.free;
  end;
end;

{$IFDEF FHIR3}
function TFHIRDataStore.getMaps: TAdvMap<TFHIRStructureMap>;
var
  s : String;
begin
  FLock.Lock;
  try
    result := TAdvMap<TFHIRStructureMap>.create;
    for s in FMaps.Keys do
      result.Add(s, FMaps[s].Link);
  finally
    FLock.Unlock;
  end;
end;
{$ENDIF}

function TFHIRDataStore.GetNextKey(keytype: TKeyType; aType: string; var id: string): integer;
begin
  case keytype of
    ktResource:
      result := NextResourceKeyGetId(aType, id);
    ktEntries:
      result := NextEntryKey;
    ktCompartment:
      result := NextCompartmentKey;
  else
    raise Exception.Create('not done');
  end;
end;

function TFHIRDataStore.Link: TFHIRDataStore;
begin
  result := TFHIRDataStore(Inherited Link);
end;


procedure TFHIRDataStore.loadCustomResources(guides: TAdvStringSet);
var
  storage: TFhirOperationManager;
  s : String;
  names : TStringList;
begin
  names := TStringList.create;
  try
    storage := TFhirOperationManager.Create('en', self.Link);
    try
      storage.OwnerName := OwnerName;
      storage.Connection := FDB.GetConnection('fhir');
      try
        for s in guides do
          if not storage.loadCustomResources(nil, s, true, names) then
            raise Exception.Create('Error Loading Custom resources');
        storage.Connection.Release;
      except
        on e : exception do
        begin
          storage.Connection.Error(e);
          raise;
        end;
      end;
    finally
      storage.free;
    end;
  finally
    names.Free;
  end;
end;

procedure TFHIRDataStore.LoadExistingResources(conn: TKDBConnection);
var
  parser: TFHIRParser;
  mem: TBytes;
  i: integer;
  cback: TKDBConnection;
begin
  FTerminologyServer.Loading := true;
  conn.SQL :=
    'select Ids.ResourceKey, Versions.ResourceVersionKey, Ids.Id, Secure, JsonContent from Ids, Types, Versions where '
    + 'Versions.ResourceVersionKey = Ids.MostRecent and ' +
    'Ids.ResourceTypeKey = Types.ResourceTypeKey and ' +
    '(Types.ResourceName = ''ValueSet'' or Types.ResourceName = ''CodeSystem'' or Types.ResourceName = ''ConceptMap'' or '+
    'Types.ResourceName = ''StructureDefinition'' or Types.ResourceName = ''Questionnaire'' or Types.ResourceName = ''StructureMap'' or Types.ResourceName = ''Subscription'') and Versions.Status < 2';
  conn.Prepare;
  try
    cback := FDB.GetConnection('load2');
    try
      i := 0;
      conn.Execute;
      while conn.FetchNext do
      begin
        inc(i);
        mem := conn.ColBlobByName['JsonContent'];

        parser := MakeParser(Validator.Context, 'en', ffJson, mem, xppDrop);
        try
          SeeResource(conn.ColIntegerByName['ResourceKey'],
            conn.ColIntegerByName['ResourceVersionKey'],
            conn.ColStringByName['Id'],
            conn.ColIntegerByName['Secure'] = 1,
            false, parser.resource, cback, true, nil);
        finally
          parser.free;
        end;
      end;
      cback.Release;
    except
      on e: Exception do
      begin
        cback.Error(e);
        recordStack(e);
        raise;
      end;
    end;
  finally
    conn.terminate;
  end;
  FTotalResourceCount := i;
  FTerminologyServer.Loading := false;
end;

function TFHIRDataStore.LookupCode(system, code: String): String;
var
  prov: TCodeSystemProvider;
begin
  try
    prov := FTerminologyServer.getProvider(system);
    try
      if prov <> nil then
        result := prov.getDisplay(code, '');
    finally
      prov.free;
    end;
  except
    result := '';
  end;
end;

{ TQuestionnaireCache }

constructor TQuestionnaireCache.Create;
begin
  inherited;
  FLock := TCriticalSection.Create('TQuestionnaireCache');
  FQuestionnaires := TAdvMap<TFhirQuestionnaire>.Create;
  FForms := TAdvStringMatch.Create;
  FForms.Forced := true;
  FValueSetDependencies := TDictionary < String, TList < string >>.Create;
end;

destructor TQuestionnaireCache.Destroy;
begin
  FValueSetDependencies.free;
  FForms.free;
  FQuestionnaires.free;
  FLock.free;
  inherited;
end;

procedure TQuestionnaireCache.clear;
begin
  FLock.Lock('clear');
  try
    FQuestionnaires.clear;
    FForms.clear;
    FValueSetDependencies.clear;
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.clearVS(id: string);
var
  s: String;
  l: TList<String>;
begin
  FLock.Lock('clear(id)');
  try
    if FValueSetDependencies.TryGetValue(id, l) then
    begin
      for s in l do
      begin
        if FQuestionnaires.ContainsKey(s) then
          FQuestionnaires.Remove(s);
        if FForms.ExistsByKey(s) then
          FForms.DeleteByKey(s);
      end;
      FValueSetDependencies.Remove(s);
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.clear(rtype: TFHIRResourceType; id: String);
var
  s: String;
begin
  s := CODES_TFHIRResourceType[rtype] + '/' + id;
  FLock.Lock('clear(id)');
  try
    if FQuestionnaires.ContainsKey(s) then
      FQuestionnaires.Remove(s);
    if FForms.ExistsByKey(s) then
      FForms.DeleteByKey(s);
  finally
    FLock.Unlock;
  end;
end;

function TQuestionnaireCache.getForm(rtype: TFHIRResourceType;
  id: String): String;
begin
  FLock.Lock('getForm');
  try
    result := FForms[CODES_TFHIRResourceType[rtype] + '/' + id];
  finally
    FLock.Unlock;
  end;

end;

function TQuestionnaireCache.getQuestionnaire(rtype: TFHIRResourceType;
  id: String): TFhirQuestionnaire;
begin
  FLock.Lock('getQuestionnaire');
  try
    result := FQuestionnaires[CODES_TFHIRResourceType[rtype] + '/' + id]
      .Link as TFhirQuestionnaire;
    // comes off linked - must happen inside the lock
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.putForm(rtype: TFHIRResourceType;
  id, form: String; dependencies: TList<String>);
var
  s: String;
  l: TList<String>;
begin
  FLock.Lock('putForm');
  try
    FForms[CODES_TFHIRResourceType[rtype] + '/' + id] := form;
    for s in dependencies do
    begin
      if not FValueSetDependencies.TryGetValue(id, l) then
      begin
        l := TList<String>.Create;
        FValueSetDependencies.add(s, l);
      end;
      if not l.Contains(id) then
        l.add(id);
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.putQuestionnaire(rtype: TFHIRResourceType;
  id: String; q: TFhirQuestionnaire; dependencies: TList<String>);
var
  s: String;
  l: TList<String>;
begin
  FLock.Lock('putQuestionnaire');
  try
    FQuestionnaires[CODES_TFHIRResourceType[rtype] + '/' + id] := q.Link;
    for s in dependencies do
    begin
      if not FValueSetDependencies.TryGetValue(id, l) then
      begin
        l := TList<String>.Create;
        FValueSetDependencies.add(s, l);
      end;
      if not l.Contains(id) then
        l.add(id);
    end;
  finally
    FLock.Unlock;
  end;
end;

end.
