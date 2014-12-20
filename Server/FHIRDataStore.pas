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
  kCritSct, DateSupport, kDate, DateAndTime, StringSupport, GuidSupport, ParseMap,
  AdvNames, AdvIntegerObjectMatches, AdvObjects, AdvStringObjectMatches, AdvStringMatches, AdvExclusiveCriticalSections, AdvStringBuilders,
  KDBManager, KDBDialects,
  FHIRAtomFeed, FHIRResources, FHIRBase, FHIRTypes, FHIRComponents, FHIRParser, FHIRParserBase, FHIRConstants,
  FHIRTags, FHIRValueSetExpander, FHIRValidator, FHIRIndexManagers, FHIRSupport, FHIRUtilities,
  {$IFNDEF FHIR-DSTU} FHIRSubscriptionManager, {$ENDIF}
  TerminologyServices, TerminologyServer, SCIMObjects, SCIMServer, ProfileManager;

const
  OAUTH_LOGIN_PREFIX = 'os9z4tw9HdmR-';
  OAUTH_SESSION_PREFIX = 'b35b7vX3KTAe-';
  IMPL_COOKIE_PREFIX = 'implicit-';
  SECURITY_BASE_URI = 'http://www.healthintersections.com.au/scim/entitlement/';

Type
  TFhirTag = class (TAdvName)
  private
    FKey : integer;
    FLabel : String;
    FScheme: String;
    FTerm: String;
    function combine : String;
  public
    property Key : integer read FKey write FKey;
    property Scheme : String read FScheme write FScheme;
    property Term : String read FTerm write FTerm;
    property Label_ : String read FLabel write FLabel;
  end;

  TFHIRResourceConfig = record
    key : integer;
    Supported : Boolean;
    IdGuids : Boolean;
    IdClient : Boolean;
    IdServer : Boolean;
    cmdUpdate : Boolean;
    cmdDelete : Boolean;
    cmdValidate : Boolean;
    cmdHistoryInstance : Boolean;
    cmdHistoryType : Boolean;
    cmdSearch : Boolean;
    cmdCreate : Boolean;
    cmdOperation : Boolean;
    versionUpdates : Boolean;
  end;

  TConfigArray = Array [TFHIRResourceType] of TFHIRResourceConfig;

  TQuestionnaireCache = class (TAdvObject)
  private
    FLock : TCriticalSection;
    FQuestionnaires : TAdvStringObjectMatch;
    FForms : TAdvStringMatch;
    FValueSetDependencies : TDictionary<String, TList<string>>;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;

    procedure putQuestionnaire(rtype : TFhirResourceType; id : String; q : TFhirQuestionnaire; dependencies : TList<String>);
    procedure putForm(rtype : TFhirResourceType; id : String; form : String; dependencies : TList<String>);

    function getQuestionnaire(rtype : TFhirResourceType; id : String) : TFhirQuestionnaire;
    function getForm(rtype : TFhirResourceType; id : String) : String;

    procedure clear(rtype : TFhirResourceType; id : String); overload;
    procedure clearVS(id : string);
    procedure clear; overload;
  end;

  TFHIRDataStore = class (TAdvObject)
  private
    FDB : TKDBManager;
    FSCIMServer : TSCIMServer;
    FTerminologyServer : TTerminologyServer;
    FSourceFolder : String; // folder in which the FHIR specification itself is found
    FSessions : TStringList;
    FTags : TAdvNameList;
    FTagsByKey : TAdvIntegerObjectMatch;
    FLock : TCriticalSection;
    FLastSessionKey : integer;
    FLastSearchKey : integer;
    FLastVersionKey : integer;
    FLastTagVersionKey : integer;
    FLastTagKey : integer;
    FLastResourceKey : Integer;
    FLastEntryKey : Integer;
    FLastCompartmentKey : Integer;
    FProfiles : TProfileManager;
    FValidator : TFHIRValidator;
    FResConfig : TConfigArray;
    FSupportTransaction : Boolean;
    FDoAudit : Boolean;
    FSupportSystemHistory : Boolean;
    FBases : TStringList;
    FTotalResourceCount: integer;
    FFormalURLPlain: String;
    FFormalURLSecure: String;
    FFormalURLPlainOpen: String;
    FFormalURLSecureOpen: String;
    FFormalURLSecureClosed: String;
    FOwnerName: String;
    {$IFNDEF FHIR-DSTU}
    FSubscriptionManager : TSubscriptionManager;
    {$ENDIF}
    FQuestionnaireCache : TQuestionnaireCache;
    FValidate: boolean;
    FAudits : TFhirResourceList;

    procedure LoadExistingResources(conn : TKDBConnection);
    procedure SaveSecurityEvent(se : TFhirSecurityEvent);
    procedure RecordFhirSession(session: TFhirSession);
    procedure CloseFhirSession(key: integer);

    procedure DoExecuteOperation(request : TFHIRRequest; response : TFHIRResponse; bWantSession : boolean);
    {$IFNDEF FHIR-DSTU}
    function DoExecuteSearch (typekey : integer; compartmentId, compartments : String; params : TParseMap; conn : TKDBConnection): String;
    function getTypeForKey(key : integer) : TFhirResourceType;
    {$ENDIF}
    procedure asssignAllowedRights(list : TStringList; user : TSCIMUser; choice : String);
  public
    constructor Create(DB : TKDBManager; SourceFolder, WebFolder : String; terminologyServer : TTerminologyServer; ini : TIniFile; SCIMServer :  TSCIMServer);
    Destructor Destroy; Override;
    Function Link : TFHIRDataStore; virtual;
    procedure CloseAll;
    function GetSession(sCookie : String; var session : TFhirSession; var check : boolean) : boolean;
    function GetSessionByToken(outerToken : String; var session : TFhirSession) : boolean;
    Function CreateImplicitSession(clientInfo : String) : TFhirSession;
    Procedure EndSession(sCookie, ip : String);
    function RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
    procedure MarkSessionChecked(sCookie, sName : String);
    function ProfilesAsOptionList : String;
    function NextVersionKey : Integer;
    function NextTagVersionKey : Integer;
    function NextSearchKey : Integer;
    function NextResourceKey : Integer;
    function NextEntryKey : Integer;
    function NextCompartmentKey : Integer;
    Function GetNextKey(keytype : TKeyType): Integer;
    procedure RegisterTag(tag : TFHIRAtomCategory; conn : TKDBConnection); overload;
    procedure registerTag(tag: TFhirTag); overload;
    procedure SeeResource(key, vkey : Integer; id : string; resource : TFHIRResource; conn : TKDBConnection; reload : boolean; session : TFhirSession);
    procedure DropResource(key, vkey : Integer; id : string; aType : TFhirResourceType; indexer : TFhirIndexManager);
    function KeyForTag(scheme, term : String) : Integer;
    Property Validator : TFHIRValidator read FValidator;
    function GetTagByKey(key : integer): TFhirTag;
    Property DB : TKDBManager read FDB;
    Property ResConfig : TConfigArray read FResConfig;
    Property SupportTransaction : Boolean read FSupportTransaction;
    Property DoAudit : Boolean read FDoAudit;
    Property SupportSystemHistory : Boolean read FSupportSystemHistory;
    Property Bases : TStringList read FBases;
    Property TotalResourceCount : integer read FTotalResourceCount;
    Property TerminologyServer : TTerminologyServer read FTerminologyServer;
    procedure Sweep;
    property FormalURLPlain : String read FFormalURLPlain write FFormalURLPlain;
    property FormalURLSecure : String read FFormalURLSecure write FFormalURLSecure;
    property FormalURLPlainOpen : String read FFormalURLPlainOpen write FFormalURLPlainOpen;
    property FormalURLSecureOpen : String read FFormalURLSecureOpen write FFormalURLSecureOpen;
    property FormalURLSecureClosed : String read FFormalURLSecureClosed write FFormalURLSecureClosed;
    function ResourceTypeKeyForName(name : String) : integer;
    procedure ProcessSubscriptions;
    function DefaultRights : String;
    Property OwnerName : String read FOwnerName write FOwnerName;
    Property Profiles : TProfileManager read FProfiles;
    function ExpandVS(vs : TFHIRValueSet; ref : TFhirReference; limit : integer; allowIncomplete : Boolean; dependencies : TStringList) : TFhirValueSet;
    function LookupCode(system, code : String) : String;
    property QuestionnaireCache : TQuestionnaireCache read FQuestionnaireCache;
    Property Validate : boolean read FValidate write FValidate;
    procedure QueueResource(r : TFHIRResource);
  end;


implementation

uses
  FHIROperation, SearchProcessor;

function TagCombine(scheme, term : String): String;
begin
  result := scheme+#1+term;
end;

{ TFHIRRepository }


procedure TFHIRDataStore.asssignAllowedRights(list: TStringList; user: TSCIMUser; choice: String);
var
  chosen : TStringList;
  s : String;
begin
  list.Clear;
  chosen := TStringList.create;
  try
    chosen.CommaText := choice;
    for s in chosen do
      if user.hasEntitlement(SECURITY_BASE_URI+s) or User.hasEntitlement(SCIM_ADMIN) then
        list.Add(s);
  finally
    chosen.Free;
  end;
end;

procedure TFHIRDataStore.CloseAll;
var
  i : integer;
  session : TFhirSession;
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

constructor TFHIRDataStore.Create(DB : TKDBManager; SourceFolder, WebFolder : String; terminologyServer : TTerminologyServer; ini : TIniFile; SCIMServer :  TSCIMServer);
var
  i : integer;
  conn : TKDBConnection;
  tag : TFhirTag;
  a : TFHIRResourceType;
begin
  inherited Create;
  FBases := TStringList.create;
  FBases.add('http://localhost/');
  for a := low(TFHIRResourceType) to high(TFHIRResourceType) do
    FResConfig[a].Supported := false;
  FDb := db;
  FSourceFolder := SourceFolder;
  FSessions := TStringList.create;
  FTags := TAdvNameList.Create;
  FLock := TCriticalSection.Create('fhir-store');
  FSCIMServer := SCIMServer;
  FSCIMServer.DefaultRights.Add(SECURITY_BASE_URI+'user');
  FAudits := TFhirResourceList.create;

  FQuestionnaireCache := TQuestionnaireCache.create;

  {$IFNDEF FHIR-DSTU}
  FSubscriptionManager := TSubscriptionManager.Create;
  FSubscriptionManager.dataBase := FDB;
  FSubscriptionManager.SMTPHost := ini.ReadString('email', 'Host', '');
  FSubscriptionManager.SMTPPort := ini.ReadString('email', 'Port' ,'');
  FSubscriptionManager.SMTPUsername := ini.readString('email', 'Username' ,'');
  FSubscriptionManager.SMTPPassword := ini.readString('email', 'Password' ,'');
  FSubscriptionManager.SMTPUseTLS := ini.ReadBool('email', 'Secure', false);
  FSubscriptionManager.SMTPSender := ini.readString('email', 'Sender' ,'');
  FSubscriptionManager.SMSAccount := ini.readString('sms', 'account' ,'');
  FSubscriptionManager.SMSToken := ini.readString('sms', 'token' ,'');
  FSubscriptionManager.SMSFrom := ini.readString('sms', 'from' ,'');
  FSubscriptionManager.OnExecuteOperation := DoExecuteOperation;
  FSubscriptionManager.OnExecuteSearch := DoExecuteSearch;
  {$ENDIF}
  conn := FDB.GetConnection('setup');
  try
    FLastSessionKey := conn.CountSQL('Select max(SessionKey) from Sessions');
    FLastVersionKey := conn.CountSQL('Select Max(ResourceVersionKey) from Versions');
    FLastTagVersionKey := conn.CountSQL('Select Max(ResourceTagKey) from VersionTags');
    FLastSearchKey := conn.CountSQL('Select Max(SearchKey) from Searches');
    FLastTagKey := conn.CountSQL('Select Max(TagKey) from Tags');
    FLastResourceKey := conn.CountSQL('select Max(ResourceKey) from Ids');
    FLastEntryKey := conn.CountSQL('select max(EntryKey) from indexEntries');
    FLastCompartmentKey := conn.CountSQL('select max(ResourceCompartmentKey) from Compartments');
    conn.execSQL('Update Sessions set Closed = '+DBGetDate(conn.Owner.Platform)+' where Closed = null');

    conn.SQL := 'Select TagKey, SchemeUri, TermURI, Label from Tags';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
    begin
      tag := TFhirTag.create;
      try
        tag.Term := conn.ColStringByName['TermURI'];
        tag.Scheme := conn.ColStringByName['SchemeURI'];
        tag.Label_ := conn.ColStringByName['Label'];
        tag.Key := conn.ColIntegerByName['TagKey'];
        tag.Name := tag.combine;
        FTags.add(tag.Link);
      finally
        tag.free;
      end;
    end;
    conn.terminate;


    conn.SQL := 'Select * from Config';
    conn.Prepare;
    conn.Execute;
    while conn.FetchNext do
      if conn.ColIntegerByName['ConfigKey'] = 1 then
        FSupportTransaction := conn.ColStringByName['Value'] = '1'
      else if conn.ColIntegerByName['ConfigKey'] = 2 then
        FBases.add(conn.ColStringByName['Value'])
      else if conn.ColIntegerByName['ConfigKey'] = 3 then
        FSupportSystemHistory := conn.ColStringByName['Value'] = '1'
      else if conn.ColIntegerByName['ConfigKey'] = 4 then
        FDoAudit := conn.ColStringByName['Value'] = '1';
    conn.Terminate;
    conn.SQL := 'Select * from Types';
    conn.Prepare;
    conn.Execute;
    While conn.FetchNext do
    begin
      a := TFHIRResourceType(StringArrayIndexOfSensitive(CODES_TFHIRResourceType, conn.ColStringByName['ResourceName']));
      FResConfig[a].Key := conn.ColIntegerByName['ResourceTypeKey'];
      FResConfig[a].Supported := conn.ColStringByName['Supported'] = '1';
      FResConfig[a].IdGuids := conn.ColStringByName['IdGuids'] = '1';
      FResConfig[a].IdClient := conn.ColStringByName['IdClient'] = '1';
      FResConfig[a].IdServer := conn.ColStringByName['IdServer'] = '1';
      FResConfig[a].cmdUpdate := conn.ColStringByName['cmdUpdate'] = '1';
      FResConfig[a].cmdDelete := conn.ColStringByName['cmdDelete'] = '1';
      FResConfig[a].cmdValidate := conn.ColStringByName['cmdValidate'] = '1';
      FResConfig[a].cmdHistoryInstance := conn.ColStringByName['cmdHistoryInstance'] = '1';
      FResConfig[a].cmdHistoryType := conn.ColStringByName['cmdHistoryType'] = '1';
      FResConfig[a].cmdSearch := conn.ColStringByName['cmdSearch'] = '1';
      FResConfig[a].cmdCreate := conn.ColStringByName['cmdCreate'] = '1';
      FResConfig[a].cmdOperation := conn.ColStringByName['cmdOperation'] = '1';
      FResConfig[a].versionUpdates := conn.ColStringByName['versionUpdates'] = '1';
    end;
    conn.Terminate;

    FTags.SortedByName;
    FTagsByKey := TAdvIntegerObjectMatch.create;
    for i := 0 to FTags.count - 1 do
      FTagsBykey.Add(TFhirTag(FTags[i]).Key, FTags[i].Link);

    // the expander is tied to what's on the system
    FTerminologyServer := terminologyServer;
    FProfiles := TProfileManager.create;
    FValidator := TFHIRValidator.create;
    FValidator.SchematronSource := WebFolder;
    FValidator.TerminologyServer := terminologyServer.Link;
    FValidator.Profiles := Profiles.Link;
    // the order here is important: specification resources must be loaded prior to stored resources
    FValidator.LoadFromDefinitions(IncludeTrailingPathDelimiter(FSourceFolder)+'validation.zip');
    LoadExistingResources(Conn);
    {$IFNDEF FHIR-DSTU}
    FSubscriptionManager.LoadQueue(Conn);
    {$ENDIF}
    conn.Release;
  except
    on e:exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;

function TFHIRDataStore.CreateImplicitSession(clientInfo: String): TFhirSession;
var
  session : TFhirSession;
  dummy : boolean;
  new : boolean;
  se : TFhirSecurityEvent;
  C : TFhirCoding;
  p : TFhirSecurityEventParticipant;
begin
  new := false;
  FLock.Lock('CreateImplicitSession');
  try
    if not GetSession(IMPL_COOKIE_PREFIX+clientInfo, result, dummy) then
    begin
      new := true;
      session := TFhirSession.create;
      try
        inc(FLastSessionKey);
        session.Key := FLastSessionKey;
        session.Id := '';
        session.Name := ClientInfo;
        session.Expires := UniversalDateTime + DATETIME_SECOND_ONE * 60*60; // 1 hour
        session.Cookie := '';
        session.Provider := apNone;
        session.originalUrl := '';
        session.email := '';
        session.anonymous := true;
        session.User := FSCIMServer.loadUser(SCIM_ANONYMOUS);
        asssignAllowedRights(session.rights, session.user, 'user,read,write');
        session.Name := Session.User.username +' ('+clientInfo+')';

        se := TFhirSecurityEvent.create;
        try
          se.event := TFhirSecurityEventEvent.create;
          se.event.type_ := TFhirCodeableConcept.create;
          c := se.event.type_.codingList.Append;
          c.code := '110114';
          c.system := 'http://nema.org/dicom/dcid';
          c.display := 'User Authentication';
          c := se.event.subtypeList.append.codingList.Append;
          c.code := '110122';
          c.system := 'http://nema.org/dicom/dcid';
          c.display := 'Login';
          se.event.action := SecurityEventActionE;
          se.event.outcome := SecurityEventOutcome0;
          se.event.dateTime := NowUTC;
          se.source := TFhirSecurityEventSource.create;
          se.source.site := 'Cloud';
          se.source.identifier := FOwnerName;
          c := se.source.type_List.Append;
          c.code := '3';
          c.display := 'Web Server';
          c.system := 'http://hl7.org/fhir/security-source-type';

          // participant - the web browser / user proxy
          p := se.participantList.Append;
          p.userId := clientInfo;
          p.network := TFhirSecurityEventParticipantNetwork.create;
          p.network.identifier := clientInfo;
          p.network.type_ := NetworkType2;

          SaveSecurityEvent(se);
        finally
          se.free;
        end;

        FSessions.AddObject(IMPL_COOKIE_PREFIX+clientInfo, session.Link);
        result := session.Link as TFhirSession;
      finally
        session.free;
      end;
    end;
  finally
    FLock.UnLock;
  end;
  if new then
    RecordFhirSession(result);
end;

procedure TFHIRDataStore.RecordFhirSession(session: TFhirSession);
var
  conn : TKDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL := 'insert into Sessions (SessionKey, Provider, Id, Name, Email, Expiry) values (:sk, :p, :i, :n, :e, :ex)';
    conn.Prepare;
    conn.BindInteger('sk', Session.Key);
    conn.BindInteger('p', Integer(Session.Provider));
    conn.BindString('i', session.Id);
    conn.BindString('n', session.Name);
    conn.BindString('e', session.Email);
    conn.BindTimeStamp('ex', DateTimeToTS(session.Expires));
    conn.Execute;
    conn.Terminate;
    conn.Release;
  except
    on e:exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;

end;


function TFHIRDataStore.DefaultRights: String;
begin
  result := 'read,write,user';
end;

destructor TFHIRDataStore.Destroy;
begin
  FAudits.Free;
  FBases.free;
  FProfiles.free;
  FTagsByKey.free;
  FSessions.free;
  FTags.Free;
  {$IFNDEF FHIR-DSTU}
  FSubscriptionManager.Free;
  {$ENDIF}
  FQuestionnaireCache.Free;
  FLock.Free;
  FSCIMServer.Free;
  FValidator.Free;
  FTerminologyServer.Free;
  inherited;
end;

procedure TFHIRDataStore.DoExecuteOperation(request: TFHIRRequest; response: TFHIRResponse; bWantSession : boolean);
var
  storage : TFhirOperationManager;
begin
  if bWantSession then
    request.Session := CreateImplicitSession('server');
  storage := TFhirOperationManager.create('en', self.Link);
  try
    storage.OwnerName := OwnerName;
    storage.Connection := FDB.GetConnection('fhir');
    storage.Connection.StartTransact;
    try
      storage.Execute(request, response, false);
      storage.Connection.Commit;
      storage.Connection.Release;
    except
      on e : exception do
      begin
        storage.Connection.Rollback;
        storage.Connection.Error(e);
        raise;
      end;
    end;
  finally
    storage.Free;
  end;
end;

{$IFNDEF FHIR-DSTU}
function TFHIRDataStore.DoExecuteSearch(typekey: integer; compartmentId, compartments: String; params: TParseMap; conn : TKDBConnection): String;
var
  sp : TSearchProcessor;
  spaces : TFHIRIndexSpaces;
begin
  spaces := TFHIRIndexSpaces.Create(conn);
  try
    sp := TSearchProcessor.create;
    try
      sp.typekey := typekey;
      sp.type_ := getTypeForKey(typeKey);
      sp.compartmentId := compartmentId;
      sp.compartments := compartments;
      sp.baseURL := FFormalURLPlainOpen; // todo: what?
      sp.lang := 'en';
      sp.params := params;
      sp.indexer := TFhirIndexManager.Create(spaces);
      sp.Indexer.TerminologyServer := TerminologyServer.Link;
      sp.Indexer.Bases := Bases;
      sp.Indexer.KeyEvent := GetNextKey;
      sp.repository := self.Link;
      sp.build;
      result := sp.filter;
    finally
      sp.free;
    end;
  finally
    spaces.Free;
  end;
end;
{$ENDIF}

procedure TFHIRDataStore.EndSession(sCookie, ip: String);
var
  i : integer;
  session : TFhirSession;
  se : TFhirSecurityEvent;
  C : TFhirCoding;
  p : TFhirSecurityEventParticipant;
  key : integer;
begin
  key := 0;
  FLock.Lock('EndSession');
  try
    i := FSessions.IndexOf(sCookie);
    if i > -1 then
    begin
      session := TFhirSession(FSessions.Objects[i]);
      try
        se := TFhirSecurityEvent.create;
        try
          se.event := TFhirSecurityEventEvent.create;
          se.event.type_ := TFhirCodeableConcept.create;
          c := se.event.type_.codingList.Append;
          c.code := '110114';
          c.system := 'http://nema.org/dicom/dcid';
          c.display := 'User Authentication';
          c := se.event.subtypeList.append.codingList.Append;
          c.code := '110123';
          c.system := 'http://nema.org/dicom/dcid';
          c.display := 'Logout';
          se.event.action := SecurityEventActionE;
          se.event.outcome := SecurityEventOutcome0;
          se.event.dateTime := NowUTC;
          se.source := TFhirSecurityEventSource.create;
          se.source.site := 'Cloud';
          se.source.identifier := ''+FOwnerName+'';
          c := se.source.type_List.Append;
          c.code := '3';
          c.display := 'Web Server';
          c.system := 'http://hl7.org/fhir/security-source-type';

          // participant - the web browser / user proxy
          p := se.participantList.Append;
          p.userId := inttostr(session.Key);
          p.altId := session.Id;
          p.name := session.Name;
          if (ip <> '') then
          begin
            p.network := TFhirSecurityEventParticipantNetwork.create;
            p.network.identifier := ip;
            p.network.type_ := NetworkType2;
            p.requestor := true;
          end;

          SaveSecurityEvent(se);
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

function TFHIRDataStore.ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit : integer; allowIncomplete : Boolean; dependencies : TStringList): TFhirValueSet;
begin
  if (vs <> nil) then
    result := FTerminologyServer.expandVS(vs, '', '',  dependencies, limit, allowIncomplete)
  else
  begin
    if FTerminologyServer.isKnownValueSet(ref.reference, vs) then
      result := FTerminologyServer.expandVS(vs, ref.reference, '', dependencies, limit, allowIncomplete)
    else
    begin
      vs := FTerminologyServer.getValueSetByUrl(ref.reference);
      if vs = nil then
        vs := FTerminologyServer.getValueSetByIdentifier(ref.reference);
      if vs = nil then
        result := nil
      else
        result := FTerminologyServer.expandVS(vs, ref.reference, '', dependencies, limit, allowIncomplete)
    end;
  end;
end;

procedure TFHIRDataStore.CloseFhirSession(key : integer);
var
  conn : TKDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL := 'Update Sessions set closed = :d where SessionKey = '+inttostr(key);
    conn.Prepare;
    conn.BindTimeStamp('d', DateTimeToTS(UniversalDateTime));
    conn.Execute;
    conn.Terminate;
    conn.Release;
  except
    on e:exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;

end;


function TFHIRDataStore.GetSession(sCookie: String; var session: TFhirSession; var check : boolean): boolean;
var
  key, i : integer;
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
      if session.Expires > UniversalDateTime then
      begin
        session.link;
        check := (session.Provider in [apFacebook, apGoogle]) and (session.NextTokenCheck < UniversalDateTime);
      end
      else
      begin
        result := false;
        try
          Key := Session.key;
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

function TFHIRDataStore.GetSessionByToken(outerToken : String; var session: TFhirSession): boolean;
var
  i : integer;
begin
  result := false;
  session := nil;
  FLock.Lock('GetSessionByToken');
  try
    for i := 0 to FSessions.count - 1 do
      if (TFhirSession(FSessions.Objects[i]).Outertoken = outerToken) or (TFhirSession(FSessions.Objects[i]).JWTPacked = outerToken) then
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

function TFHIRDataStore.GetTagByKey(key: integer): TFhirTag;
begin
  FLock.Lock('GetTagByKey');
  try
    result := TFhirTag(FTagsByKey.GetValueByKey(key));
  finally
    FLock.Unlock;
  end;
end;

{$IFNDEF FHIR-DSTU}
function TFHIRDataStore.getTypeForKey(key: integer): TFhirResourceType;
var
  a : TFHIRResourceType;
begin
  result := frtNull;
  for a := Low(FResConfig) to High(FResConfig) do
    if FResConfig[a].key = key then
    begin
      result := a;
      exit;
    end;
end;
{$ENDIF}

function TFHIRDataStore.KeyForTag(scheme, term: String): Integer;
var
  i : integer;
  p : String;
  s : string;
begin
  FLock.Lock('KeyForTag');
  try
    p := TagCombine(scheme, term);
    i := FTags.IndexByName(p);
    if i = -1 then
    begin
      for i := 0 to FTags.count - 1 do
        s := s + FTags[i].Name+#13#10;
      writeln(s);
      result := -1; // nothing will match
    end
    else
      result := TFhirTag(FTags[i]).Key;
  finally
    FLock.Unlock;
  end;

end;

procedure TFHIRDataStore.MarkSessionChecked(sCookie, sName: String);
var
  i : integer;
  session : TFhirSession;
begin
  FLock.Lock('MarkSessionChecked');
  try
    i := FSessions.IndexOf(sCookie);
    if i > -1 then
    begin
      session := TFhirSession(FSessions.Objects[i]);
      session.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
      session.Name := sName;
    end;
  finally
    FLock.Unlock;
  end;

end;

function TFHIRDataStore.NextTagVersionKey: Integer;
begin
  FLock.Lock('NextTagVersionKey');
  try
    inc(FLastTagVersionKey);
    result := FLastTagVersionKey;
  finally
    FLock.UnLock;
  end;
end;

function TFHIRDataStore.NextVersionKey: Integer;
begin
  FLock.Lock('NextVersionKey');
  try
    inc(FLastVersionKey);
    result := FLastVersionKey;
  finally
    FLock.UnLock;
  end;
end;

function TFHIRDataStore.RegisterSession(provider : TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
var
  session : TFhirSession;
  se : TFhirSecurityEvent;
  C : TFhirCoding;
  p : TFhirSecurityEventParticipant;
begin
  session := TFhirSession.create;
  try
    session.InnerToken := innerToken;
    session.OuterToken := outerToken;
    session.Id := id;
    session.Name := name;
    session.Expires := LocalDateTime + DATETIME_SECOND_ONE * StrToInt(expires);
    session.Cookie := OAUTH_SESSION_PREFIX + copy(GUIDToString(CreateGuid), 2, 36);
    session.Provider := provider;
    session.originalUrl := original;
    session.email := email;
    session.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
    if provider = apInternal then
      session.User := FSCIMServer.loadUser(id)
    else
      session.User := FSCIMServer.loadOrCreateUser(USER_SCHEME_PROVIDER[provider]+'#'+id, name, email);
    if session.name = '' then
      session.name := session.user.bestName;
    if (session.email = '') and (session.user.emails.count > 0) then
      session.email := session.user.emails[0].value;

    asssignAllowedRights(session.rights, session.user, rights);

    FLock.Lock('RegisterSession');
    try
      inc(FLastSessionKey);
      session.Key := FLastSessionKey;
      FSessions.AddObject(session.Cookie, session.Link);
    finally
      FLock.Unlock;
    end;

    se := TFhirSecurityEvent.create;
    try
      se.event := TFhirSecurityEventEvent.create;
      se.event.type_ := TFhirCodeableConcept.create;
      c := se.event.type_.codingList.Append;
      c.code := '110114';
      c.system := 'http://nema.org/dicom/dcid';
      c.display := 'User Authentication';
      c := se.event.subtypeList.append.codingList.Append;
      c.code := '110122';
      c.system := 'http://nema.org/dicom/dcid';
      c.display := 'Login';
      se.event.action := SecurityEventActionE;
      se.event.outcome := SecurityEventOutcome0;
      se.event.dateTime := NowUTC;
      se.source := TFhirSecurityEventSource.create;
      se.source.site := 'Cloud';
      se.source.identifier := ''+FOwnerName+'';
      c := se.source.type_List.Append;
      c.code := '3';
      c.display := 'Web Server';
      c.system := 'http://hl7.org/fhir/security-source-type';

      // participant - the web browser / user proxy
      p := se.participantList.Append;
      p.userId := inttostr(session.Key);
      p.altId := session.Id;
      p.name := session.Name;
      if (ip <> '') then
      begin
        p.network := TFhirSecurityEventParticipantNetwork.create;
        p.network.identifier := ip;
        p.network.type_ := NetworkType2;
        p.requestor := true;
      end;

      SaveSecurityEvent(se);
    finally
      se.free;
    end;

    result := session.Link as TFhirSession;
  finally
    session.free;
  end;
  RecordFhirSession(result);
end;


procedure TFHIRDataStore.RegisterTag(tag : TFHIRAtomCategory; conn : TKDBConnection);
var
  i : integer;
  t : TFhirTag;
begin
   {$IFDEF FHIR-DSTU}
  FLock.Lock('RegisterTag');
  try
    i := FTags.IndexByName(TagCombine(Tag.scheme, Tag.term));
    if i > -1 then
    begin
      tag.TagKey := TFhirTag(FTags[i]).Key;
      if tag.label_ = '' then
        tag.label_ := TFhirTag(FTags[i]).Label_;
    end
    else
    begin
      t := TFhirTag.create;
      try
        t.Scheme := tag.scheme;
        t.Term := tag.term;
        t.Label_ := tag.label_;
        t.Name := t.combine;
        inc(FLastTagKey);
        t.key := FLastTagKey;
        registerTag(t);
        tag.TagKey := t.Key;
        FTags.add(t.Link);
        FTagsByKey.Add(t.Key, t.Link);
      finally
        t.free;
      end;
    end;
  finally
    FLock.UnLock;
  end;
  {$ELSE}
  raise Exception.Create('TODO');
  {$ENDIF}
end;


procedure TFHIRDataStore.registerTag(tag: TFhirTag);
var
  conn : TKDBConnection;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL := 'insert into Tags (TagKey, SchemeUri, TermUri, Label) values (:k, :s, :u, :l)';
    conn.Prepare;
    conn.BindInteger('k', tag.Key);
    conn.BindString('s', tag.Scheme);
    conn.BindString('u', tag.Term);
    conn.BindString('l', tag.Label_);
    conn.Execute;
    conn.terminate;
    conn.Release;
  except
    on e:exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;


function TFHIRDataStore.ResourceTypeKeyForName(name: String): integer;
var
  i : integer;
begin
  i := StringArrayIndexOfSensitive(CODES_TFhirResourceType, name);
  if i < 1 then
    raise Exception.Create('Unknown Resource Type '''+name+'''');
  result := FResConfig[TFhirResourceType(i)].key;
end;

procedure TFHIRDataStore.Sweep;
var
  key, i : integer;
  session : TFhirSession;
  d : TDateTime;
  list : TFhirResourceList;
  storage : TFhirOperationManager;
begin
  key := 0;
  list := nil;
  d := UniversalDateTime;
  FLock.Lock('sweep2');
  try
    for i := FSessions.Count - 1 downto 0 do
    begin
      session := TFhirSession(FSessions.Objects[i]);
      if session.Expires < d then
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
  finally
    FLock.Unlock;
  end;
  if list <> nil then
    try
      storage := TFhirOperationManager.create('en', self.Link);
      try
        storage.OwnerName := OwnerName;
        storage.Connection := FDB.GetConnection('fhir');
        try
          storage.storeResources(list);
          storage.Connection.Release;
        except
          on e : exception do
          begin
            storage.Connection.Error(e);
            raise;
          end;
        end;
      finally
        storage.Free;
      end;
    finally
      list.Free;
    end;
  if key > 0 then
    CloseFhirSession(key);
end;


procedure TFHIRDataStore.SeeResource(key, vkey : Integer; id : string; resource : TFHIRResource; conn : TKDBConnection; reload : boolean; session : TFhirSession);
begin
  FLock.Lock('SeeResource');
  try
    if resource.ResourceType in [frtValueSet, frtConceptMap] then
      TerminologyServer.SeeTerminologyResource(Codes_TFHIRResourceType[resource.ResourceType]+'/'+id, key, resource)
    else if resource.ResourceType = frtProfile then
      FProfiles.seeProfile(Codes_TFHIRResourceType[resource.ResourceType]+'/'+id, key, resource as TFhirProfile);
    {$IFNDEF FHIR-DSTU}
    FSubscriptionManager.SeeResource(key, vkey, id, resource, conn, reload, session);
    FQuestionnaireCache.clear(resource.ResourceType, id);
    if resource.ResourceType = frtValueSet then
      FQuestionnaireCache.clearVS(TFhirValueSet(resource).identifier);
    {$ENDIF}
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRDataStore.DropResource(key, vkey : Integer; id : string; aType : TFhirResourceType; indexer : TFhirIndexManager);
begin
  FLock.Lock('DropResource');
  try
    if aType in [frtValueSet, frtConceptMap] then
      TerminologyServer.DropTerminologyResource(key, Codes_TFHIRResourceType[aType]+'/'+id, aType)
    else if aType = frtProfile then
      FProfiles.DropProfile({$IFNDEF FHIR-DSTU}Codes_TFHIRResourceType[aType]+'/'+id, {$ENDIF}key, Codes_TFHIRResourceType[aType]+'/'+id, aType);
    {$IFNDEF FHIR-DSTU}
    FSubscriptionManager.DropResource(key, vkey);
    FQuestionnaireCache.clear(aType, id);
    {$ENDIF}
  finally
    FLock.Unlock;
  end;
end;


procedure TFHIRDataStore.SaveSecurityEvent(se: TFhirSecurityEvent);
var
  request : TFHIRRequest;
  response : TFHIRResponse;
begin
  request := TFHIRRequest.create;
  try
    request.ResourceType := frtSecurityEvent;
    request.CommandType := fcmdCreate;
    request.Resource := se.link;
    request.lastModifiedDate := se.event.dateTime.AsUTCDateTime;
    request.Session := nil;
    response := TFHIRResponse.create;
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
  {$IFNDEF FHIR-DSTU}
  FSubscriptionManager.Process;
  {$ENDIF}
end;

function TFHIRDataStore.ProfilesAsOptionList: String;
var
  i : integer;
  builder : TAdvStringBuilder;
  profiles : TAdvStringMatch;
begin
  builder := TAdvStringBuilder.Create;
  try
    profiles := FProfiles.getLinks({$IFDEF FHIR-DSTU}false, {$ENDIF} false);
    try
      for i := 0 to profiles.Count - 1 do
      begin
        builder.Append('<option value="');
        builder.Append(profiles.KeyByIndex[i]);
        builder.Append('">');
        if profiles.ValueByIndex[i] = '' then
        begin
          builder.Append('@');
          builder.Append(profiles.KeyByIndex[i]);
          builder.Append('</option>');
          builder.Append(#13#10)
        end
        else
        begin
          builder.Append(profiles.ValueByIndex[i]);
          builder.Append('</option>');
          builder.Append(#13#10);
        end;
      end;
    finally
      profiles.Free;
    end;
    result := builder.AsString;
  finally
    builder.Free;
  end;
end;

procedure TFHIRDataStore.QueueResource(r: TFHIRResource);
begin
  FLock.Lock;
  try
    FAudits.add(r.link);
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.NextSearchKey: Integer;
begin
  FLock.Lock('NextSearchKey');
  try
    inc(FLastSearchKey);
    result := FLastSearchKey;
  finally
    FLock.UnLock;
  end;
end;

function TFHIRDataStore.NextResourceKey: Integer;
begin
  FLock.Lock('NextResourceKey');
  try
    inc(FLastResourceKey);
    result := FLastResourceKey;
  finally
    FLock.UnLock;
  end;
end;

function TFHIRDataStore.NextEntryKey : Integer;
begin
  FLock.Lock('NextEntryKey');
  try
    inc(FLastEntryKey);
    result := FLastEntryKey;
  finally
    FLock.UnLock;
  end;
end;

function TFHIRDataStore.NextCompartmentKey : Integer;
begin
  FLock.Lock('NextCompartmentKey');
  try
    inc(FLastCompartmentKey);
    result := FLastCompartmentKey;
  finally
    FLock.UnLock;
  end;
end;


function TFHIRDataStore.GetNextKey(keytype: TKeyType): Integer;
begin
  case keyType of
    ktResource : result := NextResourceKey;
    ktEntries : result := NextEntryKey;
    ktCompartment : result := NextCompartmentKey;
  else
    raise exception.create('not done');
  end;
end;

function TFHIRDataStore.Link: TFHIRDataStore;
begin
  result := TFHIRDataStore(Inherited Link);
end;

procedure TFHIRDataStore.LoadExistingResources(conn : TKDBConnection);
var
  parser : TFHIRParser;
  mem : TBytes;
  i : integer;
  cback : TKDBConnection;
begin
  FTerminologyServer.Loading := true;
  conn.SQL := 'select Ids.ResourceKey, Versions.ResourceVersionKey, Ids.Id, Content from Ids, Types, Versions where '+
    'Versions.ResourceVersionKey = Ids.MostRecent and '+
    'Ids.ResourceTypeKey = Types.ResourceTypeKey and '+
    '(Types.ResourceName = ''ValueSet'' or Types.ResourceName = ''ConceptMap'' or Types.ResourceName = ''Profile'' or Types.ResourceName = ''User''or Types.ResourceName = ''Subscription'') and not Versions.Deleted = 1';
  conn.Prepare;
  try
    cback := FDB.GetConnection('load2');
    try
      i := 0;
      conn.execute;
      while conn.FetchNext do
      begin
        inc(i);
        mem := ZDecompressBytes(conn.ColBlobByName['Content']);

        parser := MakeParser('en', ffXml, mem, xppDrop);
        try
          SeeResource(conn.colIntegerByName['ResourceKey'], conn.colIntegerByName['ResourceVersionKey'], conn.colStringByName['Id'], parser.resource, cback, true, nil);
        finally
          parser.free;
        end;
      end;
      cback.Release;
    except
      on e : Exception do
      begin
        cback.Error(e);
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
  prov : TCodeSystemProvider;
begin
  try
    prov := FTerminologyServer.getProvider(system);
    try
      if prov <> nil then
        result := prov.getDisplay(code);
    finally
      prov.free;
    end;
  except
    result := '';
  end;
end;

{ TFhirTag }

function TFhirTag.combine: String;
begin
  result := TagCombine(scheme, term);
end;

{ TQuestionnaireCache }

constructor TQuestionnaireCache.Create;
begin
  inherited;
  FLock := TCriticalSection.Create('TQuestionnaireCache');
  FQuestionnaires := TAdvStringObjectMatch.Create;
  FQuestionnaires.Forced := true;
  FForms := TAdvStringMatch.Create;
  FForms.Forced := true;
  FValueSetDependencies := TDictionary<String, TList<string>>.create;
end;

destructor TQuestionnaireCache.Destroy;
begin
  FValueSetDependencies.Free;
  FForms.Free;
  FQuestionnaires.Free;
  FLock.Free;
  inherited;
end;

procedure TQuestionnaireCache.clear;
begin
  FLock.Lock('clear');
  try
    FQuestionnaires.Clear;
    FForms.Clear;
    FValueSetDependencies.Clear;
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.clearVS(id : string);
var
  s : String;
  l : TList<String>;
begin
  FLock.Lock('clear(id)');
  try
    if FValueSetDependencies.TryGetValue(id, l) then
    begin
      for s in l do
      begin
        if FQuestionnaires.ExistsByKey(s) then
          FQuestionnaires.DeleteByKey(s);
        if FForms.ExistsByKey(s) then
          FForms.DeleteByKey(s);
      end;
      FValueSetDependencies.Remove(s);
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.clear(rtype : TFhirResourceType; id: String);
var
  s  : String;
begin
  s := Codes_TFHIRResourceType[rType]+'/'+id;
  FLock.Lock('clear(id)');
  try
    if FQuestionnaires.ExistsByKey(s) then
      FQuestionnaires.DeleteByKey(s);
    if FForms.ExistsByKey(s) then
      FForms.DeleteByKey(s);
  finally
    FLock.Unlock;
  end;
end;

function TQuestionnaireCache.getForm(rtype : TFhirResourceType; id: String): String;
begin
  FLock.Lock('getForm');
  try
    result := FForms[Codes_TFHIRResourceType[rType]+'/'+id];
  finally
    FLock.Unlock;
  end;

end;

function TQuestionnaireCache.getQuestionnaire(rtype : TFhirResourceType; id: String): TFhirQuestionnaire;
begin
  FLock.Lock('getQuestionnaire');
  try
    result := FQuestionnaires[Codes_TFHIRResourceType[rType]+'/'+id].Link as TFhirQuestionnaire; // comes off linked - must happen inside the lock
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.putForm(rtype : TFhirResourceType; id, form: String; dependencies : TList<String>);
var
  s : String;
  l : TList<String>;
begin
  FLock.Lock('putForm');
  try
    FForms[Codes_TFHIRResourceType[rType]+'/'+id] := form;
    for s in dependencies do
    begin
      if not FValueSetDependencies.TryGetValue(id, l) then
      begin
        l := TList<String>.create;
        FValueSetDependencies.Add(s, l);
      end;
      if not l.Contains(id) then
        l.Add(id);
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TQuestionnaireCache.putQuestionnaire(rtype : TFhirResourceType; id : String; q: TFhirQuestionnaire; dependencies : TList<String>);
var
  s : String;
  l : TList<String>;
begin
  FLock.Lock('putQuestionnaire');
  try
    FQuestionnaires[Codes_TFHIRResourceType[rType]+'/'+id] := q.Link;
    for s in dependencies do
    begin
      if not FValueSetDependencies.TryGetValue(id, l) then
      begin
        l := TList<String>.create;
        FValueSetDependencies.Add(s, l);
      end;
      if not l.Contains(id) then
        l.Add(id);
    end;
  finally
    FLock.Unlock;
  end;
end;

end.
