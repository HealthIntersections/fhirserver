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
  SysUtils, Classes,
  AdvObjects,
  KDBManager, KDBDialects,
  FHIRResources, FHIRConstants, FHIRIndexManagers,
  SCIMServer;

Type
  TFHIRDatabaseInstaller = class (TAdvObject)
  private
    FConn : TKDBConnection;
    FDoAudit: boolean;
    FTransactions: boolean;
    FBases: TStringList;
    FSupportSystemHistory: boolean;
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
    procedure CreateUsers;
    procedure CreateUserIndexes;
    procedure CreateOAuthLogins;
    procedure DefineIndexes;
    procedure DefineResourceSpaces;
    procedure DoPostTransactionInstall;
    procedure DoPostTransactionUnInstall;
  public
    Constructor create(conn : TKDBConnection);
    Destructor Destroy; override;
    Property Transactions : boolean read FTransactions write FTransactions;
    Property SupportSystemHistory : boolean read FSupportSystemHistory write FSupportSystemHistory;
    Property DoAudit : boolean read FDoAudit write FDoAudit;
    Property  Bases : TStringList read FBases;
    procedure Install;
    Procedure Uninstall;

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


constructor TFHIRDatabaseInstaller.create(conn: TKDBConnection);
begin
  inherited Create;
  FBases := TStringList.Create;
  FDoAudit := true;
  FTransactions := true;
  FSupportSystemHistory := true;
  FConn := conn;
end;

procedure TFHIRDatabaseInstaller.CreateResourceSessions;
begin
  FConn.ExecSQL('CREATE TABLE Sessions( '+#13#10+
       ' SessionKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Provider int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Id nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Name nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Email nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Expiry '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Closed '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+',  '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Sessions', 'SessionKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_Sessions_Id ON Sessions (Provider, Id, Name)');
  FConn.ExecSQL('Create INDEX SK_Sessions_Name ON Sessions (Name)');
  FConn.ExecSQL('Create INDEX SK_Sessions_Email ON Sessions (Email)');
  FConn.execSQL('insert into Sessions (SessionKey, Provider, Expiry, Name) values (0, 0, '+DBGetDate(FConn.Owner.Platform)+', ''System'')');
end;

procedure TFHIRDatabaseInstaller.CreateResourceTags;
begin
  FConn.ExecSQL('CREATE TABLE Tags( '+#13#10+
       ' TagKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' SchemeUri nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' TermUri nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Label nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Tags', 'TagKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_Tags_Uri ON Tags (SchemeUri, TermUri)');
  // pre-registering common tags
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (1, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/Confidentiality#U'', ''Confidentiality = none'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (2, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/Confidentiality#L'', ''Confidentiality = Low'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (3, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/Confidentiality#M'', ''Confidentiality = Moderate'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (4, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/Confidentiality#N'', ''Confidentiality = Normal'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (5, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/Confidentiality#R'', ''Confidentiality = Restricted'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (6, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/Confidentiality#V'', ''Confidentiality = Very Restricted'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (7, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/ActCode#CEL'', ''Celebrity / VIP'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (8, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/ActCode#EMP'', ''Employee / Staff member'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (9, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/ActCode#TABOO'', ''Patient/Carer Only'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (10, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/ActCode#DEMO'', ''Contact/Employment Confidential'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (11, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/ActCode#DIA'', ''Diagnosis is/would be Confidential'')');
  FConn.ExecSQL('Insert into Tags (Tagkey, SchemeUri, TermUri, Label) values (12, ''http://hl7.org/fhir/tag/security'', ''http://hl7.org/fhir/v3/ActCode#ORCON'', ''Author only'')');
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
    else if (a = frtSecurityEvent) then
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
  i: Integer;
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

End;

procedure TFHIRDatabaseInstaller.CreateNotificationQueue;
begin
  FConn.ExecSQL('CREATE TABLE NotificationQueue ( '+#13#10+
       ' NotificationQueueKey '+   DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SubscriptionKey '+        DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceVersionKey '+ DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
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
       ' Id nchar(36) NOT NULL, '+#13#10+
       ' Client nchar(12) NOT NULL, '+#13#10+
       ' Scope nchar(255) NOT NULL, '+#13#10+
       ' Redirect nchar(255) NOT NULL, '+#13#10+
       ' ClientState nchar(255) NOT NULL, '+#13#10+
       ' Status int NOT NULL, '+#13#10+
       ' DateAdded '+DBDateTimeType(FConn.owner.platform)+' NOT NULL, '+#13#10+
       ' DateSignedIn '+DBDateTimeType(FConn.owner.platform)+' NOT NULL, '+#13#10+
       ' DateChosen '+DBDateTimeType(FConn.owner.platform)+' NOT NULL, '+#13#10+
       ' DateTokenAccessed '+DBDateTimeType(FConn.owner.platform)+' NOT NULL, '+#13#10+
       ' SessionKey '+DBKeyType(FConn.owner.platform)+' NULL '+#13#10+
       ' Rights '+DBBlobType(FConn.owner.platform)+' Null, '+#13#10+
       ' Jwt '+DBBlobType(FConn.owner.platform)+' Null, '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_OAuthLogins', 'Id')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'OAuthLogins', 'SessionKey', 'Sessions', 'SessionKey', 'FK_OUathLogins_SessionKey'));
end;


procedure TFHIRDatabaseInstaller.CreateResourceCompartments;
begin
  FConn.ExecSQL('CREATE TABLE Compartments( '+#13#10+
       ' ResourceCompartmentKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' MasterResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' CompartmentType '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Id nchar(36) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' CompartmentKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Compartments', 'ResourceCompartmentKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Compartments', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_CompartmentResource_ResKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Compartments', 'CompartmentKey', 'Ids', 'ResourceKey', 'FK_Compartment_ResKey'));
  FConn.ExecSQL('Create INDEX SK_Comps_Res ON Compartments (ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_Comps_ResMstr ON Compartments (MasterResourceKey)');
  FConn.ExecSQL('Create INDEX SK_Comps_Comp ON Compartments (CompartmentType, CompartmentKey)');
end;


procedure TFHIRDatabaseInstaller.CreateResources;
Begin
  FConn.ExecSQL('CREATE TABLE Ids( '+#13#10+
       ' ResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceTypeKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Id nchar(36) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' originalId nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' MasterResourceKey int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' MostRecent '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Ids', 'ResourceKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Ids', 'ResourceTypeKey', 'Types', 'ResourceTypeKey', 'FK_ResType_TypeKey'));
  FConn.ExecSQL('Create Unique INDEX SK_Ids_Id ON Ids (ResourceTypeKey, Id)');
  FConn.ExecSQL('Create INDEX SK_Ids_TypeMaster ON Ids (ResourceTypeKey, MasterResourceKey)');
End;

procedure TFHIRDatabaseInstaller.CreateResourceVersions;
Begin
  FConn.ExecSQL('CREATE TABLE Versions( '+#13#10+
       ' ResourceVersionKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' StatedDate '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' TransactionDate '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' VersionId nchar(36) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Deleted int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Format int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SessionKey int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' TextSummary nchar(255) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Tags '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Content '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Summary '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Versions', 'ResourceVersionKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Versions', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_ResKey_IdKey'));
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
       ' Label nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
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
       ' Count int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Summary int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
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
       ' SortValue nchar(50) '+ColCanBeNull(FConn.owner.platform, True)+') '+CreateTableInfo(FConn.owner.platform));
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
       ' MasterResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' SpaceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Value nchar(128) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Value2 nchar(128) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Target '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Extension nchar(5) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Xhtml '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_IndexEntries', 'EntryKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'IndexKey', 'Indexes', 'IndexKey', 'FK_IndexEntry_IndexKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_IndexEntry_ResKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'SpaceKey', 'Spaces', 'SpaceKey', 'FK_IndexEntry_SpaceKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'IndexEntries', 'Target', 'Ids', 'ResourceKey', 'FK_IndexEntry_TargetKey'));
  FConn.ExecSQL('Create INDEX SK_IndexEntriesValueType ON IndexEntries (Value, ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_IndexEntriesValueType2 ON IndexEntries (Value2, ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_IndexEntriesValueSpaceType ON IndexEntries (SpaceKey, Value, ResourceKey)');
  FConn.ExecSQL('Create INDEX SK_IndexEntriesValueSpaceType2 ON IndexEntries (SpaceKey, Value2, ResourceKey)');
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
  FConn.ExecSQL('CREATE FULLTEXT CATALOG FHIR as DEFAULT');
  FConn.ExecSQL('Create FULLTEXT INDEX on IndexEntries (Xhtml TYPE COLUMN Extension) KEY INDEX PK_IndexEntries');
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
  m : TFHIRIndexManager;
  i, k : integer;
  names : TStringList;
begin
  k := 1;

  // general indexes
  FConn.Sql := 'insert into Indexes (IndexKey, Name) values (:k, :d)';
  FConn.prepare;
  FConn.bindInteger('k', k);
  FConn.bindString('d', NARRATIVE_INDEX_NAME);
  FConn.execute;
  inc(k);
  FConn.terminate;

  m := TFHIRIndexManager.create(nil);
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
  finally
    names.free;
    m.free;
  end;
end;


procedure TFHIRDatabaseInstaller.Install;
begin
  FConn.StartTransact;
  try
    CreateUsers;
    CreateUserIndexes;
    CreateResourceSessions;
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

    DefineResourceSpaces;
    DefineIndexes;
    FConn.Commit;
  except
    FConn.Rollback;
    raise;
  end;
  DoPostTransactionInstall;
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
      if meta.hasTable('UserIndexes') then
        FConn.DropTable('UserIndexes');
      if meta.hasTable('Users') then
        FConn.DropTable('Users');
      if meta.hasTable('Sessions') then
        FConn.DropTable('Sessions');
      FConn.Commit;
    except
      FConn.Rollback;
      raise;
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

end.

