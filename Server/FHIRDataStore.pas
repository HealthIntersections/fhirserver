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
  SysUtils, Classes,
  kCritSct, DateSupport, kDate, DateAndTime, StringSupport, GuidSupport,
  AdvNames, AdvIntegerObjectMatches, AdvObjects, AdvStringObjectMatches, AdvStringMatches,
  KDBManager, KDBDialects,
  FHIRAtomFeed, FHIRResources, FHIRBase, FHIRTypes, FHIRComponents, FHIRParser, FHIRParserBase, FHIRConstants,
  FHIRTags, FHIRValueSetExpander, FHIRValidator, FHIRIndexManagers, FHIRSupport, FHIRUtilities;

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
    FSourceFolder : String; // fodler in which the FHIR specification itself is found
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
    FValuesetExpander : TFHIRValueSetExpander;
    FValueSetTracker : TAdvStringObjectMatch;
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

    procedure Sweep;
    procedure LoadExistingResources;
    procedure SaveSecurityEvent(se : TFhirSecurityEvent);
    procedure RecordFhirSession(session: TFhirSession);
    procedure CloseFhirSession(key: integer);

  public
    constructor Create(DB : TKDBManager; SourceFolder, WebFolder : String);
    Destructor Destroy; Override;
    Function Link : TFHIRDataStore; virtual;
    procedure CloseAll;
    function GetSession(sCookie : String; var session : TFhirSession; var check : boolean) : boolean;
    Function CreateImplicitSession(clientInfo : String) : TFhirSession;
    Procedure EndSession(sCookie, ip : String);
    function RegisterSession(provider: TFHIRAuthProvider; token, id, name, email, original, expires, ip: String): TFhirSession;
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
    procedure SeeResource(key : Integer; id : string; resource : TFHIRResource);
    procedure DelistValueSet(key : Integer);
    procedure DropResource(key : Integer; id : string; aType : TFhirResourceType);
    function KeyForTag(scheme, term : String) : Integer;
    Property Validator : TFHIRValidator read FValidator;
    function GetTagByKey(key : integer): TFhirTag;
    function expandVS(vs : TFHIRValueSet) : TFHIRValueSet;
    Property DB : TKDBManager read FDB;
    Property ResConfig : TConfigArray read FResConfig;
    Property SupportTransaction : Boolean read FSupportTransaction;
    Property DoAudit : Boolean read FDoAudit;
    Property SupportSystemHistory : Boolean read FSupportSystemHistory;
    Property Bases : TStringList read FBases;
    Property TotalResourceCount : integer read FTotalResourceCount;
    Property ValuesetExpander : TFHIRValueSetExpander read FValuesetExpander;
  end;


implementation

uses
  FHIROperation;

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

constructor TFHIRDataStore.Create(DB : TKDBManager; SourceFolder, WebFolder : String);
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

    conn.Release;
  except
    on e:exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
  FTags.SortedByName;
  FTagsByKey := TAdvIntegerObjectMatch.create;
  for i := 0 to FTags.count - 1 do
    FTagsBykey.Add(TFhirTag(FTags[i]).Key, FTags[i].Link);

  // the expander is tied to what's on the system
  FValuesetExpander := TFHIRValueSetExpander.create;
  FValuesetExpander.ValueSets := TAdvStringObjectMatch.create;
  FValuesetExpander.CodeSystems := TAdvStringObjectMatch.create;
  FValueSetTracker := TAdvStringObjectMatch.create;
  FProfiles := TAdvStringMatch.create;
  FProfiles.Forced := true;
  FUserIds := TAdvStringObjectMatch.create;
  FUserIds.Forced := true;
  FUserLogins := TAdvStringObjectMatch.create;
  FUserLogins.Forced := true;
  LoadExistingResources;
  FValidator := TFHIRValidator.create;
  FValidator.SchematronSource := WebFolder;
  FValidator.LoadFromDefinitions(IncludeTrailingPathDelimiter(FSourceFolder)+'validation.zip');
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


destructor TFHIRDataStore.Destroy;
begin
  FBases.free;
  FUserIds.free;
  FUserLogins.free;
  FProfiles.free;
  FValueSetTracker.Free;
  FValuesetExpander.Free;
  FTagsByKey.free;
  FSessions.free;
  FTags.Free;
  FLock.Free;
  FValidator.Free;
  inherited;
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

function TFHIRDataStore.GetTagByKey(key: integer): TFhirTag;
begin
  FLock.Lock('GetTagByKey');
  try
    result := TFhirTag(FTagsByKey.GetValueByKey(key));
  finally
    FLock.Unlock;
  end;
end;

function TFHIRDataStore.KeyForTag(scheme, term: String): Integer;
var
  i : integer;
begin
  FLock.Lock('KeyForTag');
  try
    i := FTags.IndexByName(TagCombine(scheme, term));
    if i = -1 then
      result := -1 // nothing will match
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

function TFHIRDataStore.RegisterSession(provider : TFHIRAuthProvider; token, id, name, email, original, expires, ip: String): TFhirSession;
var
  session : TFhirSession;
  se : TFhirSecurityEvent;
  C : TFhirCoding;
  p : TFhirSecurityEventParticipant;
begin
  session := TFhirSession.create;
  try
    inc(FLastSessionKey);
    session.Key := FLastSessionKey;
    session.Token := token;
    session.Id := id;
    session.Name := name;
    session.Expires := UniversalDateTime + DATETIME_SECOND_ONE * StrToInt(expires);
    session.Cookie := OAUTH_SESSION_PREFIX + copy(GUIDToString(CreateGuid), 2, 36);
    session.Provider := provider;
    session.originalUrl := original;
    session.email := email;
    session.NextTokenCheck := UniversalDateTime + 5 * DATETIME_MINUTE_ONE;
    session.User := GetFhirUserStructure(USER_SCHEME_PROVIDER[provider], id);

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

    FLock.Lock('RegisterSession');
    try
      FSessions.AddObject(session.Cookie, session.Link);
    finally
      FLock.Unlock;
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


procedure TFHIRDataStore.DelistValueSet(key: Integer);
var
  vs : TFHIRValueSet;
begin
  FLock.Lock('DelistValueSet');
  try
    if FValueSetTracker.ExistsByKey(inttostr(key)) then
    begin
      vs := FValueSetTracker.Matches[inttostr(key)] as TFhirValueSet;
      if (vs.define <> nil) then
        FValuesetExpander.CodeSystems.DeleteByKey(vs.define.systemST);
      if (vs.identifierST <> '') then
      FValuesetExpander.ValueSets.DeleteByKey(vs.identifierST);
      FValueSetTracker.DeleteByKey(inttostr(key));
    end;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRDataStore.SeeResource(key : Integer; id : string; resource : TFHIRResource);
var
  vs : TFHIRValueSet;
  profile : TFhirProfile;
  user : TFHIRUser;
begin
  FLock.Lock('SeeResource');
  try
    if resource.ResourceType = frtValueSet then
    begin
      vs := resource as TFHIRValueSet;
      FValueSetTracker.add(inttostr(key), vs.link);
      if (vs.define <> nil) then
        FValuesetExpander.CodeSystems.add(vs.define.systemST, vs.link);
      if (vs.identifierST <> '') then
        FValuesetExpander.ValueSets.add(vs.identifierST, vs.link);
    end
    else if resource.ResourceType = frtProfile then
    begin
      profile := resource as TFhirProfile;
      FProfiles.matches[id] := profile.nameST;
    end
{    else if resource.ResourceType = frtUser then
    begin
      user := resource as TFhirUser;
      FUserIds.matches[id] := user.link;
      FUserLogins.matches[user.providerST+#1+user.loginST] := user.link;
    end;}
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRDataStore.DropResource(key : Integer; id : string; aType : TFhirResourceType);
var
  vs : TFHIRValueSet;
  user : TFHIRUser;
begin
  FLock.Lock('DropResource');
  try
    if (aType = frtValueSet) and (FValueSetTracker.ExistsByKey(inttostr(key)))  then
    begin
      vs := FValueSetTracker.Matches[inttostr(key)] as TFHIRValueSet;
      if (vs.define <> nil) then
        FValuesetExpander.CodeSystems.DeleteByKey(vs.define.systemST);
      if (vs.identifierST <> '') then
        FValuesetExpander.ValueSets.DeleteByKey(vs.identifierST);
      FValueSetTracker.DeleteByKey(inttostr(key));
    end
    else if aType = frtProfile then
    begin
      FProfiles.DeleteByKey(id);
    end
{    else if aType = frtUser then
    begin
      user := FUserIds.Matches[id] as TFHIRUser;
      FUserLogins.DeleteByKey(user.providerST+#1+user.loginST);
      FUserIds.DeleteByKey(id);
    end;}
  finally
    FLock.Unlock;
  end;
end;


function TFHIRDataStore.expandVS(vs: TFHIRValueSet): TFHIRValueSet;
begin
  FLock.Lock('expandVS');
  try
    result := FValuesetExpander.expand(vs);
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRDataStore.SaveSecurityEvent(se: TFhirSecurityEvent);
var
  request : TFHIRRequest;
  response : TFHIRResponse;
  storage : TFhirOperation;
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
      storage := TFhirOperation.create('en', self.Link);
      try
        storage.Request := request.Link;
        storage.Response := response.link;
        storage.Connection := FDB.GetConnection('fhir');
        storage.PreCheck;
        storage.Connection.StartTransact;
        try
          storage.Execute;
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
    finally
      response.free;
    end;
  finally
    request.free;
  end;
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
var
  user : TFHIRUser;
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

procedure TFHIRDataStore.LoadExistingResources;
var
  conn : TKDBConnection;
  parser : TFHIRParser;
  stream : TStream;
  mem : TMemoryStream;
  i : integer;
begin
  conn := FDB.GetConnection('fhir');
  try
    conn.SQL := 'select Ids.ResourceKey, Ids.Id, Content from Ids, Types, Versions where '+
      'Versions.ResourceVersionKey = Ids.MostRecent and '+
      'Ids.ResourceTypeKey = Types.ResourceTypeKey and '+
      '(Types.ResourceName = ''ValueSet'' or Types.ResourceName = ''Profile'' or Types.ResourceName = ''User'') and not Versions.Deleted = 1';
    conn.Prepare;
    try
      i := 0;
      conn.execute;
      while conn.FetchNext do
      begin
        inc(i);
        mem := conn.ColMemoryByName['Content'];
        mem.position := 0;
        mem.SaveToFile('c:\temp\text.xml');
        mem.position := 0;

        parser := MakeParser('en', ffXml, mem);
        try
          SeeResource(conn.colIntegerByName['ResourceKey'], conn.colStringByName['Id'], parser.resource);
        finally
          parser.free;
        end;
      end;
    finally
      conn.terminate;
    end;
    conn.Release;
    FTotalResourceCount := i;
  except
    on e:exception do
    begin
      conn.Error(e);
      raise;
    end;
  end;
end;



{ TFhirTag }

function TFhirTag.combine: String;
begin
  result := TagCombine(scheme, term);
end;

end.
