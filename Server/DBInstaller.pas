unit DBInstaller;

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
  SysUtils, Classes, GuidSupport,
  AdvObjects, AdvExceptions,
  KDBManager, KDBDialects, TextUtilities,
  FHIRBase, FHIRResources, FHIRConstants, FHIRIndexManagers, FHIRUtilities,
  SCIMServer;

const
//  ServerDBVersion = 3;
//  ServerDBVersion = 4; // added secure flag in versions table
//  ServerDBVersion = 5; // added scores to search entries table
//  ServerDBVersion = 6; // added reverse to search entries table
  ServerDBVersion = 7; // changed compartment table. breaking change

  // config table keys
  CK_Transactions = 1;   // whether transactions and batches are allowed or not
  CK_Bases = 2;          // (multiple) list of base urls the server accepts ids for (in addition to itself)
  CK_SystemHistory = 3;  // whether to support history at the system level or not
  CK_KeepAuditTrail = 4; // whether to populate the an AuditEvent history
  CK_ServerVersion = 5;  // version of the database (database upgrade tracking)
  CK_UniqueUri = 6; // Unique uri created when the database is installed
  CK_GlobalSearch = 7; // whether search across all types is allowed
  CK_FHIRVersion = 8; // the version of FHIR for which the database is configured


Type
  TFHIRDatabaseInstaller = class (TAdvObject)
  private
    FConn : TKDBConnection;
    FDoAudit: boolean;
    FTransactions: boolean;
    FBases: TStringList;
    FSupportSystemHistory: boolean;
    FTextIndexing: boolean;
    FTxPath : String;
    procedure CreateResourceCompartments;
    procedure CreateResourceConfig;
    procedure CreateResourceIndexEntries;
    procedure CreateResourceIndexes;
    procedure CreateResources;
    procedure CreateResourceSearchEntries;
    procedure CreateResourceSearches;
    procedure CreateResourceSessions;
    procedure CreateResourceSpaces;
    procedure CreateResourceTags;
    procedure CreateResourceTypes;
    procedure CreateResourceVersions;
    procedure CreateResourceVersionsTags;
    procedure CreateSubscriptionQueue;
    procedure CreateNotificationQueue;
    procedure CreateWebSocketsQueue;
    procedure CreateUsers;
    procedure CreateUserIndexes;
    procedure CreateOAuthLogins;
    procedure CreateClosures;
    procedure CreateConcepts;
    procedure CreateValueSets;
    procedure CreateValueSetMembers;
    procedure CreateClosureEntries;
    procedure DefineIndexes;
    procedure DefineResourceSpaces;
    procedure DoPostTransactionInstall;
    procedure DoPostTransactionUnInstall;
    procedure CreateCodeSystems;
    procedure runScript(s : String);
  public
    Constructor create(conn : TKDBConnection; txpath : String);
    Destructor Destroy; override;
    Property Transactions : boolean read FTransactions write FTransactions;
    Property SupportSystemHistory : boolean read FSupportSystemHistory write FSupportSystemHistory;
    Property DoAudit : boolean read FDoAudit write FDoAudit;
    Property  Bases : TStringList read FBases;
    procedure Install(scim : TSCIMServer);
    Procedure Uninstall;
    Procedure Upgrade(version : integer);
    Property TextIndexing : boolean read FTextIndexing write FTextIndexing;

  end;

implementation

Function ForeignKeySql(Conn: TKDBConnection; Const sSlaveTable, sSlaveField, sMasterTable, sMasterField, sIndexName : String; b2 : Boolean = false) : String;
Begin
  if b2 Then
    Result := 'ALTER TABLE '+sSlaveTable+' ADD CONSTRAINT '+sIndexName+' '+
            'FOREIGN KEY ( '+sSlaveField+' ) REFERENCES '+sMasterTable+' ( '+sMasterField+' )'
  Else
    Result := 'ALTER TABLE '+sSlaveTable+' ADD CONSTRAINT '+sIndexName +' '+
            'FOREIGN KEY ( '+sSlaveField+' ) REFERENCES '+sMasterTable+' ( '+sMasterField+' )';
End;


constructor TFHIRDatabaseInstaller.create(conn: TKDBConnection; txpath : String);
begin
  inherited Create;
  FBases := TStringList.Create;
  FDoAudit := true;
  FTransactions := true;
  FSupportSystemHistory := true;
  FConn := conn;
  FTxPath := txpath;
end;

procedure TFHIRDatabaseInstaller.CreateResourceSessions;
begin
  FConn.ExecSQL('CREATE TABLE Sessions( '+#13#10+
       ' SessionKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' UserKey int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Provider int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Id nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Name nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Email nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Created '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Expiry '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Closed '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+',  '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Sessions', 'SessionKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_Sessions_Id ON Sessions (Provider, Id, Name)');
  FConn.ExecSQL('Create INDEX SK_Sessions_Name ON Sessions (Name, Created)');
  FConn.ExecSQL('Create INDEX SK_Sessions_User ON Sessions (UserKey, Created)');
  FConn.ExecSQL('Create INDEX SK_Sessions_Email ON Sessions (Email)');
  FConn.ExecSQL(ForeignKeySql(FConn, 'Sessions', 'UserKey', 'Users', 'UserKey', 'FK_Session_UserKey'));
  FConn.execSQL('insert into Sessions (SessionKey, UserKey, Created, Provider, Expiry, Name) values (0, 1, '+DBGetDate(FConn.Owner.Platform)+', 0, '+DBGetDate(FConn.Owner.Platform)+', ''System'')');
end;

procedure TFHIRDatabaseInstaller.CreateResourceTags;
begin
  FConn.ExecSQL('CREATE TABLE Tags( '+#13#10+
       ' TagKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Kind int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Uri nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Code nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Display nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Tags', 'TagKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create Unique INDEX SK_Tags_Uri ON Tags (Kind, Uri, Code)');
  // pre-registering common tags
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (1,  1, ''http://hl7.org/fhir/v3/Confidentiality'', ''U'', ''Confidentiality = none'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (2,  1, ''http://hl7.org/fhir/v3/Confidentiality'', ''L'', ''Confidentiality = Low'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (3,  1, ''http://hl7.org/fhir/v3/Confidentiality'', ''M'', ''Confidentiality = Moderate'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (4,  1, ''http://hl7.org/fhir/v3/Confidentiality'', ''N'', ''Confidentiality = Normal'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (5,  1, ''http://hl7.org/fhir/v3/Confidentiality'', ''R'', ''Confidentiality = Restricted'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (6,  1, ''http://hl7.org/fhir/v3/Confidentiality'', ''V'', ''Confidentiality = Very Restricted'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (7,  1, ''http://hl7.org/fhir/v3/ActCode'', ''CEL'', ''Celebrity / VIP'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (8,  1, ''http://hl7.org/fhir/v3/ActCode'', ''EMP'', ''Employee / Staff member'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (9,  1, ''http://hl7.org/fhir/v3/ActCode'', ''TABOO'', ''Patient/Carer Only'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (10, 1, ''http://hl7.org/fhir/v3/ActCode'', ''DEMO'', ''Contact/Employment Confidential'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (11, 1, ''http://hl7.org/fhir/v3/ActCode'', ''DIA'', ''Diagnosis is/would be Confidential'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, Kind, Uri, Code, Display) values (12, 1, ''http://hl7.org/fhir/v3/ActCode'', ''ORCON'', ''Author only'')');
end;

procedure TFHIRDatabaseInstaller.CreateResourceTypes;
var
  a : TFHIRResourceType;
Begin
  FConn.ExecSQL('CREATE TABLE Types( '+#13#10+
       ' ResourceTypeKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' ResourceName nchar(32) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Supported int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' LastId int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' IdGuids int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' IdClient int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' IdServer int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdRead int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdVersionRead int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdCreate int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdOperation int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdUpdate int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdDelete int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdValidate int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdHistoryInstance int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdHistoryType int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' cmdSearch int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' versionUpdates int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+

       PrimaryKeyType(FConn.owner.Platform, 'PK_Types', 'ResourceTypeKey')+') '+CreateTableInfo(FConn.owner.platform));
  for a := Low(TFHIRResourceType) to High(TFHIRResourceType) do
    if (a = frtBinary) then
      FConn.ExecSql('insert into Types (ResourceTypeKey, ResourceName, Supported, LastId, IdGuids, IdClient, IdServer, cmdRead, cmdUpdate, cmdVersionRead, cmdDelete, cmdValidate, cmdHistoryInstance, cmdHistoryType, cmdSearch, cmdCreate, cmdOperation, versionUpdates) values '+
      '('+inttostr(ord(a)+1)+', '''+CODES_TFHIRResourceType[a]+''', 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0)')
    else if (a = frtAuditEvent) then
      FConn.ExecSql('insert into Types (ResourceTypeKey, ResourceName, Supported, LastId, IdGuids, IdClient, IdServer, cmdRead, cmdUpdate, cmdVersionRead, cmdDelete, cmdValidate, cmdHistoryInstance, cmdHistoryType, cmdSearch, cmdCreate, cmdOperation, versionUpdates) values '+
      '('+inttostr(ord(a)+1)+', '''+CODES_TFHIRResourceType[a]+''', 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0)')
    else
      FConn.ExecSql('insert into Types (ResourceTypeKey, ResourceName, Supported, LastId, IdGuids, IdClient, IdServer, cmdRead, cmdUpdate, cmdVersionRead, cmdDelete, cmdValidate, cmdHistoryInstance, cmdHistoryType, cmdSearch, cmdCreate, cmdOperation, versionUpdates) values '+
      '('+inttostr(ord(a)+1)+', '''+CODES_TFHIRResourceType[a]+''', 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)');
End;

FUnction BooleanToInt(b : boolean) : String;
begin
  if b then
    result := '1'
  else
    result := '0';
end;

procedure TFHIRDatabaseInstaller.CreateResourceConfig;
var
  i : integer;
Begin
  FConn.ExecSQL('CREATE TABLE Config( '+#13#10+
       ' ConfigKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Value nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_Config_ConfigKey ON Config (ConfigKey)');
  FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (1, '''+BooleanToInt(FTransactions)+''')');
  if FBases.Count = 0 then
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (2, ''http://hl7.org/fhir'')')
  else for i := 0 to FBases.Count - 1 do
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (2, '''+FBases[i]+''')');

  FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (3, '''+BooleanToInt(FSupportSystemHistory)+''')');
  FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (4, '''+BooleanToInt(FDoAudit)+''')');
  FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (5, '''+inttostr(ServerDBVersion)+''')');
  FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (6, '''+NewGuidURN+''')');
  FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (7, ''1'')');
  FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (8, '''+FHIR_GENERATED_VERSION+''')');
End;

procedure TFHIRDatabaseInstaller.CreateClosureEntries;
begin
  FConn.ExecSQL('CREATE TABLE ClosureEntries ( '+#13#10+
       ' ClosureEntryKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ClosureKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SubsumesKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SubsumedKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' IndexedVersion int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_ClosureEntries', 'ClosureEntryKey')+') '+CreateTableInfo(FConn.owner.platform));

  FConn.ExecSQL('Create INDEX SK_ClosureEntries_Subsumes ON ClosureEntries (ClosureKey, SubsumesKey)');
  FConn.ExecSQL('Create INDEX SK_ClosureEntries_Subsumed ON ClosureEntries (ClosureKey, SubsumedKey)');
  FConn.ExecSQL('Create INDEX SK_ClosureEntries_IndexedVersion ON ClosureEntries (IndexedVersion)');

  FConn.ExecSQL(ForeignKeySql(FConn, 'ClosureEntries', 'ClosureKey',  'Closures', 'ClosureKey', 'FK_ClosureEntries_ConceptKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'ClosureEntries', 'SubsumesKey', 'Concepts', 'ConceptKey', 'FK_ClosureEntries_SubsumesKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'ClosureEntries', 'SubsumedKey', 'Concepts', 'ConceptKey', 'FK_ClosureEntries_SubsumedKey'));
end;

procedure TFHIRDatabaseInstaller.CreateClosures;
begin
  FConn.ExecSQL('CREATE TABLE Closures ( '+#13#10+
       ' ClosureKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Name nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+
       ' Version int '+ColCanBeNull(FConn.owner.platform, False)+', '+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Closures', 'ClosureKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_Closure_Name ON Closures (Name)');
end;

procedure TFHIRDatabaseInstaller.CreateCodeSystems;
begin
  runScript('tx_db.sql');
  runScript('us-state-codes.sql');
  runScript('area-codes.sql');
  runScript('country-codes.sql');
end;

procedure TFHIRDatabaseInstaller.CreateConcepts;
begin
  FConn.ExecSQL('CREATE TABLE Concepts ( '+#13#10+
       ' ConceptKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' URL nchar(100) '+ColCanBeNull(FConn.owner.platform, False)+', '+
       ' Code nchar(100) '+ColCanBeNull(FConn.owner.platform, False)+', '+
       ' NeedsIndexing int '+ColCanBeNull(FConn.owner.platform, False)+', '+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Concepts', 'ConceptKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_Concepts_Name ON Concepts (URL, Code)');
end;

procedure TFHIRDatabaseInstaller.CreateNotificationQueue;
begin
  FConn.ExecSQL('CREATE TABLE NotificationQueue ( '+#13#10+
       ' NotificationQueueKey '+   DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SubscriptionKey '+        DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceVersionKey '+ DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Operation                  int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Entered '+       DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' LastTry '+       DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       ' ErrorCount int                                          '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       ' Abandoned '+     DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       ' Handled '+       DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_NotificationQueue', 'NotificationQueueKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'NotificationQueue', 'SubscriptionKey','Ids', 'ResourceKey', 'FK_NotificationQ_SubVerKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'NotificationQueue', 'ResourceVersionKey',    'Versions', 'ResourceVersionKey', 'FK_NotificationQ_ResVerKey'));
  FConn.ExecSQL('Create INDEX SK_NotificationQueue_Reload ON NotificationQueue (Handled)');
end;

procedure TFHIRDatabaseInstaller.CreateOAuthLogins;
begin
  FConn.ExecSQL('CREATE TABLE OAuthLogins( '+#13#10+
       ' Id nchar('+inttostr(ID_LENGTH)+') NOT NULL, '+#13#10+
       ' Client nchar(48) NOT NULL, '+#13#10+
       ' Scope nchar(255) NOT NULL, '+#13#10+
       ' Redirect nchar(255) NOT NULL, '+#13#10+
       ' ClientState nchar(255) NOT NULL, '+#13#10+
       ' Patient nchar(64) NULL, '+#13#10+
       ' Status int NOT NULL, '+#13#10+
       ' DateAdded '+DBDateTimeType(FConn.owner.platform)+' NOT NULL, '+#13#10+
       ' DateSignedIn '+DBDateTimeType(FConn.owner.platform)+' NULL, '+#13#10+
       ' DateChosen '+DBDateTimeType(FConn.owner.platform)+' NULL, '+#13#10+
       ' DateTokenAccessed '+DBDateTimeType(FConn.owner.platform)+' NULL, '+#13#10+
       ' SessionKey '+DBKeyType(FConn.owner.platform)+' NULL, '+#13#10+
       ' Rights '+DBBlobType(FConn.owner.platform)+' Null, '+#13#10+
       ' Jwt '+DBBlobType(FConn.owner.platform)+' Null, '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_OAuthLogins', 'Id')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'OAuthLogins', 'SessionKey', 'Sessions', 'SessionKey', 'FK_OUathLogins_SessionKey'));
end;


procedure TFHIRDatabaseInstaller.CreateResourceCompartments;
begin
  FConn.ExecSQL('CREATE TABLE Compartments( '+#13#10+
       ' ResourceCompartmentKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+ // internal primary key
       ' ResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+ // id of resource that is in a compartment
       ' TypeKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+ // resource type key for the compartment type
       ' Id nchar('+inttostr(ID_LENGTH)+') '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+                    // field two of composite id for compartment - compartment id
       ' CompartmentKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+   // key for the resource that creates this compartment
       PrimaryKeyType(FConn.owner.Platform, 'PK_Compartments', 'ResourceCompartmentKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Compartments', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_CompartmentResource_ResKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Compartments', 'TypeKey', 'Types', 'ResourceTypeKey', 'FK_CompartmentResource_TypeKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Compartments', 'CompartmentKey', 'Ids', 'ResourceKey', 'FK_Compartment_ResKey'));
  FConn.ExecSQL('Create INDEX SK_Comps_Res ON Compartments (ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_Comps_CompKey ON Compartments (TypeKey, CompartmentKey)');
  FConn.ExecSQL('Create INDEX SK_Comps_CompId ON Compartments (TypeKey, Id)');
end;


procedure TFHIRDatabaseInstaller.CreateResources;
Begin
  FConn.ExecSQL('CREATE TABLE Ids( '+#13#10+
       ' ResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceTypeKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Id nchar('+inttostr(ID_LENGTH)+') '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' originalId nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' MasterResourceKey int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' MostRecent '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Deleted int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Ids', 'ResourceKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Ids', 'ResourceTypeKey', 'Types', 'ResourceTypeKey', 'FK_ResType_TypeKey'));
  FConn.ExecSQL('Create Unique INDEX SK_Ids_Id ON Ids (ResourceTypeKey, Id)');
  FConn.ExecSQL('Create INDEX SK_Ids_TypeMaster ON Ids (ResourceTypeKey, MasterResourceKey)');
  FConn.ExecSQL('Create INDEX SK_Ids_DelTypeMaster ON Ids (ResourceTypeKey, Deleted, MasterResourceKey)');
End;

procedure TFHIRDatabaseInstaller.CreateResourceVersions;
Begin
  FConn.ExecSQL('CREATE TABLE Versions( '+#13#10+
       ' ResourceVersionKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' StatedDate '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' TransactionDate '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' VersionId nchar('+inttostr(ID_LENGTH)+') '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Status int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Format int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Secure int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SessionKey int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' AuditKey int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Tags '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' XmlContent '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' XmlSummary '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' JsonContent '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' JsonSummary '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Versions', 'ResourceVersionKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Versions', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_ResKey_IdKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Versions', 'AuditKey', 'Ids', 'ResourceKey', 'FK_ResKey_AuditKey'));
  FConn.ExecSQL('Create INDEX SK_Versions_ResDate ON Versions (ResourceKey, TransactionDate)');
  FConn.ExecSQL('Create UNIQUE INDEX SK_Versions_ResVer ON Versions (ResourceKey, VersionId)');
  FConn.ExecSQL(ForeignKeySql(FConn, 'Ids', 'MostRecent', 'Versions', 'ResourceVersionKey', 'FK_ResCurrent_VersionKey'));
End;

procedure TFHIRDatabaseInstaller.CreateResourceVersionsTags;
Begin
  FConn.ExecSQL('CREATE TABLE VersionTags( '+#13#10+
       ' ResourceTagKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceVersionKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' TagKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Display nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_VersionTags', 'ResourceTagKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create UNIQUE INDEX SK_VersionTags_ResTag1 ON VersionTags (ResourceVersionKey, TagKey)');
  FConn.ExecSQL('Create UNIQUE INDEX SK_VersionTags_ResTag2 ON VersionTags (TagKey, ResourceVersionKey)');
  FConn.ExecSQL(ForeignKeySql(FConn, 'VersionTags', 'ResourceVersionKey', 'Versions', 'ResourceVersionKey', 'FK_VerTag_VerKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'VersionTags', 'TagKey', 'Tags', 'TagKey', 'FK_VerTag_TagKey'));
End;

procedure TFHIRDatabaseInstaller.CreateResourceIndexes;
Begin
  FConn.ExecSQL('CREATE TABLE Indexes( '+#13#10+
       ' IndexKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Name nchar(250) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Indexes', 'IndexKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create UNIQUE INDEX SK_IndexesName ON Indexes (Name)');
End;

procedure TFHIRDatabaseInstaller.CreateResourceSpaces;
Begin
  FConn.ExecSQL('CREATE TABLE Spaces( '+#13#10+
       ' SpaceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Space nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Spaces', 'SpaceKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create UNIQUE INDEX SK_SpacesSpace ON Spaces (Space)');
End;

procedure TFHIRDatabaseInstaller.CreateResourceSearches;
Begin
  FConn.ExecSQL('CREATE TABLE Searches( '+#13#10+
       ' SearchKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Id nchar(36) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SessionKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       ' Count int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Summary int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Reverse int '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       ' Date '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Type int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Title '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Base '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Link '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' SqlCode '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Searches', 'SearchKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create UNIQUE INDEX SK_SearchesSearchkey ON Searches (SearchKey)');
  FConn.ExecSQL('Create UNIQUE INDEX SK_SearchesId ON Searches (Id)');
  FConn.ExecSQL('Create INDEX SK_SearchesDate ON Searches (Date)');
End;

procedure TFHIRDatabaseInstaller.CreateResourceSearchEntries;
Begin
  FConn.ExecSQL('CREATE TABLE SearchEntries( '+#13#10+
       ' SearchKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceVersionKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SortValue nchar(128) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Score1 int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Score2 int '+ColCanBeNull(FConn.owner.platform, True)+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create UNIQUE INDEX SK_SearchesSearchEntries ON SearchEntries (SearchKey, SortValue, ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_SearchesResourceKey ON SearchEntries (ResourceKey)');
  FConn.ExecSQL(ForeignKeySql(FConn, 'SearchEntries', 'SearchKey', 'Searches', 'SearchKey', 'FK_Search_Search'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'SearchEntries', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_Search_ResKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'SearchEntries', 'ResourceVersionKey', 'Versions', 'ResourceVersionKey', 'FK_Search_ResVerKey'));
End;

procedure TFHIRDatabaseInstaller.CreateSubscriptionQueue;
begin
  FConn.ExecSQL('CREATE TABLE SubscriptionQueue ( '+#13#10+
       ' SubscriptionQueueKey '+   DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceKey '+        DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceVersionKey '+ DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Operation                  int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Entered '+       DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Handled '+       DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_SubscriptionQueue', 'SubscriptionQueueKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'SubscriptionQueue', 'ResourceKey',        'Ids', 'ResourceKey',             'FK_SubscriptionQ_ResKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'SubscriptionQueue', 'ResourceVersionKey', 'Versions', 'ResourceVersionKey', 'FK_SubscriptionQ_ResVerKey'));
  FConn.ExecSQL('Create INDEX SK_SubscriptionsQueue_Reload ON SubscriptionQueue (Handled)');
end;

procedure TFHIRDatabaseInstaller.CreateResourceIndexEntries;
Begin
  FConn.ExecSQL('CREATE TABLE IndexEntries( '+#13#10+
       ' EntryKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' IndexKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Parent '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' MasterResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' SpaceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Value nchar(210) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Value2 nchar(210) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Flag '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, false)+', '+#13#10+
       ' Target '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Concept '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Extension nchar(5) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Xhtml '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_IndexEntries', 'EntryKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'IndexKey', 'Indexes', 'IndexKey', 'FK_IndexEntry_IndexKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_IndexEntry_ResKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'SpaceKey', 'Spaces', 'SpaceKey', 'FK_IndexEntry_SpaceKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'Target', 'Ids', 'ResourceKey', 'FK_IndexEntry_TargetKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'Concept', 'Concepts', 'ConceptKey', 'FK_IndexEntry_ConceptKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'Parent', 'IndexEntries', 'EntryKey', 'FK_IndexEntry_ParentKey'));
  FConn.ExecSQL('Create INDEX SK_IndexEntriesValueType ON IndexEntries (Flag, Value, ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_IndexEntriesValueType2 ON IndexEntries (Flag, Value2, ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_IndexEntriesValueSpaceType ON IndexEntries (SpaceKey, Flag, Value, ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_IndexEntriesValueSpaceType2 ON IndexEntries (SpaceKey, Flag, Value2, ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_IndexEntriesResKey ON IndexEntries (ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_IndexEntriesTargetKey ON IndexEntries (Target)');
  FConn.ExecSQL('Create INDEX SK_IndexEntriesMasterResKey ON IndexEntries (MasterResourceKey)');
End;

procedure TFHIRDatabaseInstaller.DefineResourceSpaces;
begin
  FConn.ExecSQL('insert into Spaces select ResourceTypeKey as SpaceKey, ResourceName as Space from Types');
end;

procedure TFHIRDatabaseInstaller.CreateUsers;
Begin
  FConn.ExecSQL('CREATE TABLE Users( '+#13#10+
       ' UserKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' UserName nchar(255)'+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Password nchar(255) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Status int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Content '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Users', 'UserKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create Unique INDEX SK_Users_StatusName ON Users (Status, UserName)');
  FConn.ExecSQL('Create Unique INDEX SK_Users_StatusKey ON Users (Status, UserKey)');
End;

procedure TFHIRDatabaseInstaller.CreateValueSetMembers;
begin
  FConn.ExecSQL('CREATE TABLE ValueSetMembers ( '+#13#10+
       ' ValueSetMemberKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ValueSetKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ConceptKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_ValueSetMembers', 'ValueSetMemberKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_ValueSetMembers_Members ON ValueSetMembers (ValueSetKey, ConceptKey)');
  FConn.ExecSQL('Create INDEX SK_ValueSetMembers_Owners ON ValueSetMembers (ConceptKey, ValueSetKey)');
  FConn.ExecSQL(ForeignKeySql(FConn, 'ValueSetMembers', 'ValueSetKey', 'ValueSets', 'ValueSetKey', 'FK_ValueSetMembers_ValueSetKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'ValueSetMembers', 'ConceptKey', 'Concepts', 'ConceptKey', 'FK_ValueSetMembers_ConceptKey'));
end;

procedure TFHIRDatabaseInstaller.CreateValueSets;
begin
  FConn.ExecSQL('CREATE TABLE ValueSets ( '+#13#10+
       ' ValueSetKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' URL nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+
       ' NeedsIndexing int '+ColCanBeNull(FConn.owner.platform, False)+', '+
       ' Error nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+
       PrimaryKeyType(FConn.owner.Platform, 'PK_ValueSets', 'ValueSetKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_ValueSets_URI ON ValueSets (URL)');
  FConn.ExecSQL('Create INDEX SK_ValueSets_NeedsIndexing ON ValueSets (NeedsIndexing)');
end;

procedure TFHIRDatabaseInstaller.CreateWebSocketsQueue;
begin
  FConn.ExecSQL('CREATE TABLE WebSocketsQueue ( '+#13#10+
       ' WebSocketsQueueKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SubscriptionId     nchar(64)                           '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Handled            int                                 '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Content            '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_WebSocketsQueue', 'WebSocketsQueueKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_WebSocketsQueue_SubscriptionId ON WebSocketsQueue (SubscriptionId, Handled)');
end;

procedure TFHIRDatabaseInstaller.CreateUserIndexes;
Begin
  FConn.ExecSQL('CREATE TABLE UserIndexes( '+#13#10+
       ' UserIndexKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' UserKey      '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' IndexName    nchar(20)'+                           ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Parent       '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       ' Value        nchar(255) '+                         ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       ' SortBy       '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_UserIndexes', 'UserIndexKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'UserIndexes', 'UserKey', 'Users', 'UserKey', 'FK_UserIndexes_UserKey'));
  FConn.ExecSQL('Create INDEX SK_UserIndexes_IndexNameValue ON UserIndexes (IndexName, Value)');
  FConn.ExecSQL('Create INDEX SK_UserIndexes_ParentIndexNameValue ON UserIndexes (Parent, IndexName, Value)');
End;


destructor TFHIRDatabaseInstaller.Destroy;
begin
  FBases.Free;
  inherited;
end;

procedure TFHIRDatabaseInstaller.DoPostTransactionInstall;
begin
  if TextIndexing then
  begin
    FConn.ExecSQL('CREATE FULLTEXT CATALOG FHIR as DEFAULT');
    FConn.ExecSQL('Create FULLTEXT INDEX on IndexEntries (Xhtml TYPE COLUMN Extension) KEY INDEX PK_IndexEntries');
  end;
end;

procedure TFHIRDatabaseInstaller.DoPostTransactionUnInstall;
begin
  try
    FConn.ExecSQL('DROP FULLTEXT CATALOG FHIR');
  except
  end;
end;

procedure TFHIRDatabaseInstaller.DefineIndexes;
var
  m : TFHIRIndexInformation;
  i, k : integer;
  names : TStringList;
begin
  k := 1;

//  // general indexes
//  FConn.Sql := 'insert into Indexes (IndexKey, Name) values (:k, :d)';
//  FConn.prepare;
//  FConn.bindInteger('k', k);
//  FConn.bindString('d', NARRATIVE_INDEX_NAME);
//  FConn.execute;
//  inc(k);
//  FConn.terminate;

  m := TFHIRIndexInformation.create();
  names := TStringList.Create;
  try
    for i := 0 to m.Indexes.count - 1 do
    begin
      if names.IndexOf(m.indexes[i].Name.ToLower) = -1 Then
      begin
        FConn.Sql := 'insert into Indexes (IndexKey, Name) values (:k, :d)';
        FConn.prepare;
        FConn.bindInteger('k', k);
        FConn.bindString('d', m.indexes[i].Name);
        FConn.execute;
        inc(k);
        FConn.terminate;
        names.add(m.indexes[i].Name.ToLower);
      end;
    end;
    for i := 0 to m.Composites.count - 1 do
    begin
      if names.IndexOf(m.Composites[i].Name.ToLower) = -1 Then
      begin
        FConn.Sql := 'insert into Indexes (IndexKey, Name) values (:k, :d)';
        FConn.prepare;
        FConn.bindInteger('k', k);
        FConn.bindString('d', m.Composites[i].Name);
        FConn.execute;
        inc(k);
        FConn.terminate;
        names.add(m.Composites[i].Name.ToLower);
      end;
    end;
  finally
    names.free;
    m.free;
  end;
end;


procedure TFHIRDatabaseInstaller.Install(scim : TSCIMServer);
begin
  FConn.StartTransact;
  try
    CreateUsers;
    CreateUserIndexes;
    scim.defineSystem(FConn);
    CreateResourceSessions;
    CreateOAuthLogins;

    CreateCodeSystems;

    CreateClosures;
    CreateConcepts;
    CreateValueSets;
    CreateValueSetMembers;
    CreateClosureEntries;

    CreateResourceTags;
    CreateResourceTypes;
    CreateResourceConfig;
    CreateResources;
    CreateResourceCompartments;
    CreateResourceVersions;
    CreateResourceVersionsTags;
    CreateResourceIndexes;
    CreateResourceSpaces;
    CreateResourceIndexEntries;
    CreateResourceSearches;
    CreateResourceSearchEntries;
    CreateSubscriptionQueue;
    CreateNotificationQueue;
    CreateWebSocketsQueue;

    DefineResourceSpaces;
    DefineIndexes;
    FConn.Commit;
  except
    on e:exception do
    begin
      FConn.Rollback;
      recordStack(e);
      raise;
    end;
  end;
  DoPostTransactionInstall;
end;

procedure TFHIRDatabaseInstaller.runScript(s: String);
var
  lines : TStringList;
  sql, l : String;
begin
  lines := TStringList.create;
  try
    lines.Text := FileToString(IncludeTrailingPathDelimiter(Ftxpath)+s, TEncoding.ANSI);
    sql := '';
    for l in lines do
      if l.Trim = 'GO' then
      begin
        Fconn.ExecSQL(sql);
        sql := '';
      end
      else
        sql := sql + l+#13#10;
    if (sql.trim <> '') then
      Fconn.ExecSQL(sql);
  finally
    lines.free;
  end;
end;

procedure TFHIRDatabaseInstaller.Uninstall;
var
  meta : TKDBMetaData;
begin
  meta := FConn.FetchMetaData;
  try
    FConn.StartTransact;
    try
      if meta.hasTable('Ids') then
        if FConn.owner.platform = kdbMySQL then
          FConn.execsql('ALTER TABLE Ids DROP FOREIGN KEY FK_ResCurrent_VersionKey')
        else
          FConn.execsql('ALTER TABLE Ids DROP CONSTRAINT FK_ResCurrent_VersionKey');

      if meta.hasTable('WebSocketsQueue') then
        FConn.DropTable('WebSocketsQueue');
      if meta.hasTable('NotificationQueue') then
        FConn.DropTable('NotificationQueue');
      if meta.hasTable('SubscriptionQueue') then
        FConn.DropTable('SubscriptionQueue');
      if meta.hasTable('SearchEntries') then
        FConn.DropTable('SearchEntries');
      if meta.hasTable('Searches') then
        FConn.DropTable('Searches');
      if meta.hasTable('IndexEntries') then
        FConn.DropTable('IndexEntries');
      if meta.hasTable('Indexes') then
        FConn.DropTable('Indexes');
      if meta.hasTable('Spaces') then
        FConn.DropTable('Spaces');

      if meta.hasTable('VersionTags') then
        FConn.DropTable('VersionTags');
      if meta.hasTable('Versions') then
        FConn.DropTable('Versions');
      if meta.hasTable('Compartments') then
        FConn.DropTable('Compartments');
      if meta.hasTable('Ids') then
        FConn.DropTable('Ids');
      if meta.hasTable('Config') then
        FConn.DropTable('Config');
      if meta.hasTable('Types') then
        FConn.DropTable('Types');
      if meta.hasTable('Tags') then
        FConn.DropTable('Tags');
      if meta.hasTable('OAuthLogins') then
        FConn.DropTable('OAuthLogins');
      if meta.hasTable('Sessions') then
        FConn.DropTable('Sessions');
      if meta.hasTable('UserIndexes') then
        FConn.DropTable('UserIndexes');
      if meta.hasTable('Users') then
        FConn.DropTable('Users');

      if meta.hasTable('ClosureEntries') then
        FConn.DropTable('ClosureEntries');
      if meta.hasTable('ValueSetMembers') then
        FConn.DropTable('ValueSetMembers');
      if meta.hasTable('ValueSets') then
        FConn.DropTable('ValueSets');
      if meta.hasTable('Closures') then
        FConn.DropTable('Closures');
      if meta.hasTable('Concepts') then
        FConn.DropTable('Concepts');


      if meta.hasTable('UniiDesc') then
        FConn.DropTable('UniiDesc');
      if meta.hasTable('Unii') then
        FConn.DropTable('Unii');
      if meta.hasTable('cvx') then
        FConn.DropTable('cvx');
      if meta.hasTable('USStateCodes') then
        FConn.DropTable('USStateCodes');
      if meta.hasTable('AreaCodes') then
        FConn.DropTable('AreaCodes');
      if meta.hasTable('CountryCodes') then
        FConn.DropTable('CountryCodes');

      FConn.Commit;
    except
      on e:exception do
      begin
        FConn.Rollback;
        recordStack(e);
        raise;
      end;
    end;
  finally
    meta.free;
  end;
  try
    DoPostTransactionUnInstall;
  finally
    // nothing
  end;
end;


procedure TFHIRDatabaseInstaller.Upgrade(version : integer);
var
  m : TKDBMetaData;
  t : TKDBTable;
begin
  if version > ServerDBVersion then
    raise Exception.Create('Database Version mismatch (found='+inttostr(version)+', can handle 1-'+inttostr(ServerDBVersion)+'): you must re-install the database or change which version of the server you are running');

  if version < 7 then
    raise Exception.Create('data base must be recreated (-mount or -remount)');  // 7 was a breaking change
//  // 1- > 2 upgrade - add Patient to OAuthLogins. Also, clean up changes to Closures
//  if version < 2 then
//  begin
//    Fconn.ExecSQL('ALTER TABLE OAuthLogins ADD Patient char(64) NULL');
//
//    m := Fconn.FetchMetaData;
//    try
//      t := m.getTable('Closures');
//      if not t.hasColumn('Version') then
//      begin
//        Fconn.ExecSQL('ALTER TABLE Closures ADD Version int NULL');
//        Fconn.ExecSQL('update Closures set Version = 1');
//      end;
//      t := m.getTable('ClosureEntries');
//      if t.hasColumn('NeedsIndexing') then
//      begin
//        Fconn.ExecSQL('sp_RENAME ''ClosureEntries.NeedsIndexing'' , ''IndexedVersion'', ''COLUMN''');
//        Fconn.ExecSQL('update ClosureEntries set IndexedVersion = 1');
//      end;
//    finally
//      m.Free;
//    end;
//  end;
//  if version < 3 then
//    Fconn.ExecSQL('insert into Config (ConfigKey, Value) values (8, '''+FHIR_GENERATED_VERSION+''')');
//  if version < 4 then
//  begin
//    Fconn.ExecSQL('ALTER TABLE Versions ADD Secure int NULL');
//    Fconn.ExecSQL('update Versions set Secure = 0');
//  end;
//  if version < 5 then
//  begin
//    Fconn.ExecSQL('ALTER TABLE SearchEntries ADD Score1 int NULL');
//    Fconn.ExecSQL('ALTER TABLE SearchEntries ADD Score2 int NULL');
//  end;
//  if version < 6 then
//    Fconn.ExecSQL('ALTER TABLE Searches ADD Reverse int NULL');
//  Fconn.ExecSQL('update Config set value = '+inttostr(ServerDBVersion)+' where ConfigKey = 5');
end;

end.

