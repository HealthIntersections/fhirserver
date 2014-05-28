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
  SysUtils, Classes, IniFiles,
  kCritSct, DateSupport, kDate, DateAndTime, StringSupport, GuidSupport, ParseMap,
  AdvNames, AdvIntegerObjectMatches, AdvObjects, AdvStringObjectMatches, AdvStringMatches, AdvExclusiveCriticalSections,
  KDBManager, KDBDialects,
  FHIRAtomFeed, FHIRResources, FHIRBase, FHIRTypes, FHIRComponents, FHIRParser, FHIRParserBase, FHIRConstants,
  FHIRTags, FHIRValueSetExpander, FHIRValidator, FHIRIndexManagers, FHIRSupport, FHIRUtilities,
  {$IFNDEF FHIR-DSTU} FHIRSubscriptionManager, {$ENDIF}
  TerminologyServer;

const
  OAUTH_LOGIN_PREFIX = 'os9z4tw9HdmR-';
  OAUTH_SESSION_PREFIX = 'b35b7vX3KTAe-';
  IMPL_COOKIE_PREFIX = 'implicit-';


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
    versionUpdates : Boolean;
  end;

  TConfigArray = Array [TFHIRResourceType] of TFHIRResourceConfig;

  TFHIRDataStore = class (TAdvObject)
  private
    FDB : TKDBManager;
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
    FProfiles : TAdvStringMatch;
    FUserIds : TAdvStringObjectMatch;
    FUserLogins : TAdvStringObjectMatch;
    FValidator : TFHIRValidator;
    FResConfig : TConfigArray;
    FSupportTransaction : Boolean;
    FDoAudit : Boolean;
    FSupportSystemHistory : Boolean;
    FBases : TStringList;
    FTotalResourceCount: integer;
    FFormalURL: String;
    {$IFNDEF FHIR-DSTU}
    FSubscriptionManager : TSubscriptionManager;
    {$ENDIF}

    procedure LoadExistingResources(conn : TKDBConnection);
    procedure SaveSecurityEvent(se : TFhirSecurityEvent);
    procedure RecordFhirSession(session: TFhirSession);
    procedure CloseFhirSession(key: integer);

    procedure DoExecuteOperation(request : TFHIRRequest; response : TFHIRResponse; bWantSession : boolean);
    function DoExecuteSearch (typekey : integer; compartmentId, compartments : String; params : TParseMap; conn : TKDBConnection): String;
    function getTypeForKey(key : integer) : TFhirResourceType;
  public
    constructor Create(DB : TKDBManager; SourceFolder, WebFolder : String; terminologyServer : TTerminologyServer; ini : TIniFile);
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
    function GetFhirUser(namespace, name :String) : TFHIRUser;
    function GetFhirUserStructure(namespace, name :String) : TFHIRUserStructure;
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
    property FormalURL : String read FFormalURL write FFormalURL;
    function ResourceTypeKeyForName(name : String) : integer;
    procedure ProcessSubscriptions;
    function DefaultRights : String;
  end;


implementation

uses
  FHIROperation, SearchProcessor;

function TagCombine(scheme, term : String): String;
begin
  result := scheme+#1+term;
end;
{ TFHIRRepository }


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

constructor TFHIRDataStore.Create(DB : TKDBManager; SourceFolder, WebFolder : String; terminologyServer : TTerminologyServer; ini : TIniFile);
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
  {$IFNDEF FHIR-DSTU}
  FSubscriptionManager := TSubscriptionManager.Create;
  FSubscriptionManager.dataBase := FDB;
  FSubscriptionManager.SMTPHost := ini.ReadString('email', 'Host', '');
  FSubscriptionManager.SMTPPort := ini.ReadString('email', 'Port' ,'');
  FSubscriptionManager.SMTPUsername := ini.readString('email', 'Username' ,'');
  FSubscriptionManager.SMTPPassword := ini.readString('email', 'Password' ,'');
  FSubscriptionManager.SMTPUseTLS := ini.ReadBool('email', 'Secure', false);
  FSubscriptionManager.SMTPSender := ini.readString('email', 'Sender' ,'');
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
      FResConfig[a].versionUpdates := conn.ColStringByName['versionUpdates'] = '1';
    end;
    conn.Terminate;

    FTags.SortedByName;
    FTagsByKey := TAdvIntegerObjectMatch.create;
    for i := 0 to FTags.count - 1 do
      FTagsBykey.Add(TFhirTag(FTags[i]).Key, FTags[i].Link);

    // the expander is tied to what's on the system
    FTerminologyServer := terminologyServer;
    FProfiles := TAdvStringMatch.create;
    FProfiles.Forced := true;
    FUserIds := TAdvStringObjectMatch.create;
    FUserIds.PreventDuplicates;
    FUserIds.Forced := true;
    FUserLogins := TAdvStringObjectMatch.create;
    FUserLogins.PreventDuplicates;
    FUserLogins.Forced := true;
    FValidator := TFHIRValidator.create;
    FValidator.SchematronSource := WebFolder;
    FValidator.TerminologyServer := terminologyServer.Link;
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
        session.User := GetFhirUserStructure(USER_SCHEME_IMPLICIT, clientInfo);
        if Session.user <> nil then
          session.Name := Session.User.Resource.Name +' ('+clientInfo+')';

        se := TFhirSecurityEvent.create;
        try
          se.event := TFhirSecurityEventEvent.create;
          se.event.type_ := TFhirCodeableConcept.create;
          c := se.event.type_.codingList.Append;
          c.codeST := '110114';
          c.systemST := 'http://nema.org/dicom/dcid';
          c.displayST := 'User Authentication';
          c := se.event.subtypeList.append.codingList.Append;
          c.codeST := '110122';
          c.systemST := 'http://nema.org/dicom/dcid';
          c.displayST := 'Login';
          se.event.actionST := SecurityEventActionE;
          se.event.outcomeST := SecurityEventOutcome0;
          se.event.dateTimeST := NowUTC;
          se.source := TFhirSecurityEventSource.create;
          se.source.siteST := 'Cloud';
          se.source.identifierST := 'HL7Connect';
          c := se.source.type_List.Append;
          c.codeST := '3';
          c.displayST := 'Web Server';
          c.systemST := 'http://hl7.org/fhir/security-source-type';

          // participant - the web browser / user proxy
          p := se.participantList.Append;
          p.userIdST := clientInfo;
          p.network := TFhirSecurityEventParticipantNetwork.create;
          p.network.identifierST := clientInfo;
          p.network.type_ST := NetworkType2;

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
  FBases.free;
  FUserIds.free;
  FUserLogins.free;
  FProfiles.free;
  FTagsByKey.free;
  FSessions.free;
  FTags.Free;
  {$IFNDEF FHIR-DSTU}
  FSubscriptionManager.Free;
  {$ENDIF}
  FLock.Free;
  FValidator.Free;
  FTerminologyServer.Free;
  inherited;
end;

procedure TFHIRDataStore.DoExecuteOperation(request: TFHIRRequest; response: TFHIRResponse; bWantSession : boolean);
var
  storage : TFhirOperation;
begin
  if bWantSession then
    request.Session := CreateImplicitSession('server');
  storage := TFhirOperation.create('en', self.Link);
  try
    storage.Connection := FDB.GetConnection('fhir');
    storage.PreCheck(request, response);
    storage.Connection.StartTransact;
    try
      storage.Execute(request, response);
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
      sp.baseURL := FFormalURL; // todo: what?
      sp.lang := 'en';
      sp.params := params;
      sp.indexer := TFhirIndexManager.Create(spaces);
      sp.Indexer.Ucum := TerminologyServer.Ucum.Link;
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
          c.codeST := '110114';
          c.systemST := 'http://nema.org/dicom/dcid';
          c.displayST := 'User Authentication';
          c := se.event.subtypeList.append.codingList.Append;
          c.codeST := '110123';
          c.systemST := 'http://nema.org/dicom/dcid';
          c.displayST := 'Logout';
          se.event.actionST := SecurityEventActionE;
          se.event.outcomeST := SecurityEventOutcome0;
          se.event.dateTimeST := NowUTC;
          se.source := TFhirSecurityEventSource.create;
          se.source.siteST := 'Cloud';
          se.source.identifierST := 'HL7Connect';
          c := se.source.type_List.Append;
          c.codeST := '3';
          c.displayST := 'Web Server';
          c.systemST := 'http://hl7.org/fhir/security-source-type';

          // participant - the web browser / user proxy
          p := se.participantList.Append;
          p.userIdST := inttostr(session.Key);
          p.altIdST := session.Id;
          p.nameST := session.Name;
          if (ip <> '') then
          begin
            p.network := TFhirSecurityEventParticipantNetwork.create;
            p.network.identifierST := ip;
            p.network.type_ST := NetworkType2;
            p.requestorST := true;
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
  key, i : integer;
begin
  result := false;
  session := nil;
  FLock.Lock('GetSessionByToken');
  try
    for i := 0 to FSessions.count - 1 do
      if TFhirSession(FSessions.Objects[i]).Outertoken = outerToken then
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
    session.User := GetFhirUserStructure(USER_SCHEME_PROVIDER[provider], id);
    session.rights.commaText := rights;

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
      c.codeST := '110114';
      c.systemST := 'http://nema.org/dicom/dcid';
      c.displayST := 'User Authentication';
      c := se.event.subtypeList.append.codingList.Append;
      c.codeST := '110122';
      c.systemST := 'http://nema.org/dicom/dcid';
      c.displayST := 'Login';
      se.event.actionST := SecurityEventActionE;
      se.event.outcomeST := SecurityEventOutcome0;
      se.event.dateTimeST := NowUTC;
      se.source := TFhirSecurityEventSource.create;
      se.source.siteST := 'Cloud';
      se.source.identifierST := 'HL7Connect';
      c := se.source.type_List.Append;
      c.codeST := '3';
      c.displayST := 'Web Server';
      c.systemST := 'http://hl7.org/fhir/security-source-type';

      // participant - the web browser / user proxy
      p := se.participantList.Append;
      p.userIdST := inttostr(session.Key);
      p.altIdST := session.Id;
      p.nameST := session.Name;
      if (ip <> '') then
      begin
        p.network := TFhirSecurityEventParticipantNetwork.create;
        p.network.identifierST := ip;
        p.network.type_ST := NetworkType2;
        p.requestorST := true;
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
begin
  key := 0;
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
  finally
    FLock.Unlock;
  end;
  if key > 0 then
    CloseFhirSession(key);
end;


procedure TFHIRDataStore.SeeResource(key, vkey : Integer; id : string; resource : TFHIRResource; conn : TKDBConnection; reload : boolean; session : TFhirSession);
var
  profile : TFhirProfile;
begin
  FLock.Lock('SeeResource');
  try
    if resource.ResourceType in [frtValueSet, frtConceptMap] then
      TerminologyServer.SeeTerminologyResource(Codes_TFHIRResourceType[resource.ResourceType]+'/'+id, key, resource)
    else if resource.ResourceType = frtProfile then
    begin
      profile := resource as TFhirProfile;
      FProfiles.matches[id] := profile.nameST;
    end;
{    else if resource.ResourceType = frtUser then
    begin
      user := resource as TFhirUser;
      FUserIds.matches[id] := user.link;
      FUserLogins.matches[user.providerST+#1+user.loginST] := user.link;
    end;}
    {$IFNDEF FHIR-DSTU}
    FSubscriptionManager.SeeResource(key, vkey, id, resource, conn, reload, session);
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
    begin
      FProfiles.DeleteByKey(id);
    end;
{    else if aType = frtUser then
    begin
      user := FUserIds.Matches[id] as TFHIRUser;
      FUserLogins.DeleteByKey(user.providerST+#1+user.loginST);
      FUserIds.DeleteByKey(id);
    end;}
    {$IFNDEF FHIR-DSTU}
    FSubscriptionManager.DropResource(key, vkey);
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
    request.lastModifiedDate := se.event.dateTimeST.AsUTCDateTime;
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
  sn, sv : string;
begin
  result := '';
  FLock.Enter;
  try
    for i := 0 to FProfiles.Count - 1 do
    begin
      sn := FProfiles.KeyByIndex[i];
      sv := FProfiles.ValueByIndex[i];
      result := result + '<option value="'+sn+'">';
      if sv = '' then
        result := result + '@'+sn+'</option>'+#13#10
      else
        result := result + sv+'</option>'+#13#10;
    end;
  finally
    FLock.Leave;
  end;
end;

function TFHIRDataStore.GetFhirUser(namespace, name: String): TFHIRUser;
begin
  FLock.Enter;
  try
    result := FUserLogins.Matches[namespace + #1 + name] as TFhirUser;
  finally
    FLock.Leave;
  end;
end;

function TFHIRDataStore.GetFhirUserStructure(namespace, name: String): TFHIRUserStructure;
var
  user : TFHIRUser;
  conn : TKDBConnection;
begin
  FLock.Enter;
  try
    user := FUserLogins.Matches[namespace + #1 + name] as TFhirUser;
    if user = nil then
      result := nil
    else
    begin
      result := TFhirUserStructure.create;
      try
        result.Resource := user.link;
        // todo: read other patient compartments
        conn := FDB.GetConnection('fhir');
        try
          conn.sql := 'select Id from Ids where MostRecent in (select ResourceVersionKey from VersionTags where TagKey in (select TagKey from Tags where Scheme = '''+FHIR_TAG_SCHEME+''' and Uri = ''http://fhir.org/connectathon4/patient-compartment-user/'+SQLWrapString(user.login)+'''))';
          conn.Prepare;
          conn.Execute;
          while conn.FetchNext do
            result.TaggedCompartments.Add(Conn.ColStringByName['Id']);
          conn.terminate;
          conn.Release;
        except
          on e:exception do
          begin
            conn.Error(e);
            raise;
          end;
        end;
        result.link;
      finally
        result.free;
      end;
    end;
  finally
    FLock.Leave;
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
end;


{ TFhirTag }

function TFhirTag.combine: String;
begin
  result := TagCombine(scheme, term);
end;

end.
