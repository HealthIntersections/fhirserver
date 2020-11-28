unit database_installer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Classes, fsl_utilities,
  fsl_base,
  fdb_manager, fdb_dialects, fsl_stream,
  fhir_objects, fhir_factory, fhir_utilities,
  indexing, server_factory, server_version,
  scim_server;

const
  // config table keys
  CK_Transactions = 1;   // whether transactions and batches are allowed or not
  CK_Bases = 2;          // (multiple) list of base urls the server accepts ids for (in addition to itself)
  CK_SystemHistory = 3;  // whether to support history at the system level or not
  CK_KeepAuditTrail = 4; // whether to populate the an AuditEvent history
  CK_ServerVersion = 5;  // version of the database (database upgrade tracking)
  CK_UniqueUri = 6; // Unique uri created when the database is installed
  CK_GlobalSearch = 7; // whether search across all types is allowed
  CK_FHIRVersion = 8; // the version of FHIR for which the database is configured


type
  TFHIRInstallerSecurityMode = (ismUnstated, ismOpenAccess, ismClosedAccess, ismReadOnly, ismTerminologyServer);

function readInstallerSecurityMode(s : String) : TFHIRInstallerSecurityMode;

Type

  { TFHIRDatabaseInstaller }

  TFHIRDatabaseInstaller = class (TFslObject)
  private
    Fcallback : TInstallerCallback;
    FConn : TFDBConnection;
    FDoAudit: boolean;
    FTransactions: boolean;
    FBases: TStringList;
    FSupportSystemHistory: boolean;
    FFactory : TFHIRFactory;
    FServerFactory : TFHIRServerFactory;
    FDefaultRights : String;

    procedure CreateResourceCompartments;
    procedure CreateResourceConfig(res : boolean);
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
    procedure CreateObservations;
    procedure CreateObservationCodes;
    procedure CreateObservationQueue;
    procedure CreateAuthorizations;
    procedure CreateAuthorizationSessions;
    procedure CreateConnections;
    procedure CreateAsyncTasks;
    procedure CreateClientRegistrations;
    procedure CreatePseudoData;
    procedure CreatePackagesTables;
    procedure CreatePackagePermissionsTable;
    procedure CreateLoadedPackagesTable;
//    procedure CreateTwilioTable;
//    procedure runScript(s : String);
  public
    constructor Create(conn : TFDBConnection; factory : TFHIRFactory; serverFactory : TFHIRServerFactory);
    destructor Destroy; override;
    Property Transactions : boolean read FTransactions write FTransactions;
    Property SupportSystemHistory : boolean read FSupportSystemHistory write FSupportSystemHistory;
    Property DoAudit : boolean read FDoAudit write FDoAudit;
    Property Bases : TStringList read FBases;
    Property DefaultRights : String read FDefaultRights write FDefaultRights;

    procedure installPackageServer;
    procedure InstallTerminologyServer;
    procedure Install(scim : TSCIMServer);
    Procedure Uninstall;

    Procedure Upgrade(version : integer);
    property callback : TInstallerCallback read Fcallback write Fcallback;
  end;

implementation

Function ForeignKeySql(Conn: TFDBConnection; Const sSlaveTable, sSlaveField, sMasterTable, sMasterField, sIndexName : String) : String;
Begin
  if conn.Owner.Platform = kdbSQLite then
    result := ''
  else
    Result := 'ALTER TABLE '+sSlaveTable+' ADD CONSTRAINT '+sIndexName+' '+ 'FOREIGN KEY ( '+sSlaveField+' ) REFERENCES '+sMasterTable+' ( '+sMasterField+' )';
End;

Function InlineForeignKeySql(Conn: TFDBConnection; Const sSlaveTable, sSlaveField, sMasterTable, sMasterField, sIndexName : String) : String;
Begin
  if conn.Owner.Platform <> kdbSQLite then
    result := ''
  else
    result := 'FOREIGN KEY('+sSlaveField+') REFERENCES '+sMasterTable+'('+sMasterField+')';
End;


constructor TFHIRDatabaseInstaller.Create(conn: TFDBConnection;
  factory: TFHIRFactory; serverFactory: TFHIRServerFactory);
begin
  inherited Create;
  FFactory := factory;
  FServerFactory := serverFactory;
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
       ' UserKey int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Provider int '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Id nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Name nchar(200) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Email nchar(200) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Created '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Expiry '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' Closed '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+',  '+#13#10+
       InlineForeignKeySql(FConn, 'Sessions', 'UserKey', 'Users', 'UserKey', 'FK_Session_UserKey')+
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
//  this is dsiabled because of a case sensitivity issue... FConn.ExecSQL('Create Unique INDEX SK_Tags_Uri ON Tags (Kind, Uri, Code)');
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
  a : String;
  i : integer;
Begin
  FConn.ExecSQL('CREATE TABLE Types( '+#13#10+
       ' ResourceTypeKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+',  '+#13#10+
       ' ResourceName nchar(64) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ImplementationGuide nchar(64) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
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
  i := 1;
  for a in FFactory.ResourceNames do
  begin
    if (a = 'Binary') then
      FConn.ExecSql('insert into Types (ResourceTypeKey, ResourceName, Supported, LastId, IdGuids, IdClient, IdServer, cmdRead, cmdUpdate, cmdVersionRead, cmdDelete, cmdValidate, cmdHistoryInstance, cmdHistoryType, cmdSearch, cmdCreate, cmdOperation, versionUpdates) values '+
      '('+inttostr(i)+', '''+a+''', 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0)')
    else if (a = 'AuditEvent') then
      FConn.ExecSql('insert into Types (ResourceTypeKey, ResourceName, Supported, LastId, IdGuids, IdClient, IdServer, cmdRead, cmdUpdate, cmdVersionRead, cmdDelete, cmdValidate, cmdHistoryInstance, cmdHistoryType, cmdSearch, cmdCreate, cmdOperation, versionUpdates) values '+
      '('+inttostr(i)+', '''+a+''', 1, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0)')
    else if (a <> 'Custom') then
      FConn.ExecSql('insert into Types (ResourceTypeKey, ResourceName, Supported, LastId, IdGuids, IdClient, IdServer, cmdRead, cmdUpdate, cmdVersionRead, cmdDelete, cmdValidate, cmdHistoryInstance, cmdHistoryType, cmdSearch, cmdCreate, cmdOperation, versionUpdates) values '+
      '('+inttostr(i)+', '''+a+''', 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)');
    inc(i);
  end;
End;

FUnction BooleanToInt(b : boolean) : String;
begin
  if b then
    result := '1'
  else
    result := '0';
end;

procedure TFHIRDatabaseInstaller.CreateResourceConfig(res : boolean);
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

  FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (5, '''+inttostr(ServerDBVersion)+''')');
  if (res) then
  begin
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (3, '''+BooleanToInt(FSupportSystemHistory)+''')');
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (4, '''+BooleanToInt(FDoAudit)+''')');
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (6, '''+NewGuidURN+''')');
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (7, ''1'')');
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (8, '''+FFactory.versionString+''')');
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (9, '''+BooleanToInt(true)+''')');
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (10, '''+FDefaultRights+''')');
  end;
End;

procedure TFHIRDatabaseInstaller.CreateAuthorizations;
begin
  FConn.ExecSQL('CREATE TABLE Authorizations( '+#13#10+
       ' AuthorizationKey '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  // internal primary key
       ' Uuid             char(36)                              '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  // GUID used externally
       ' PatientKey       '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // id of resource changed
       ' PatientId        char(64)                              '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // id of resource changed
       ' ConsentKey       '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // id of resource changed
       ' SessionKey       '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // id of resource changed
       ' Status           int                                   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+      // whether added or deleted
       ' Expiry           '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+        // observation.effectiveTime Stated (null = range)
       ' JWT              '+DBBlobType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // id of resource changed
       InlineForeignKeySql(FConn, 'Authorizations', 'PatientKey', 'Ids', 'ResourceKey', 'FK_Authorizations_PatKey')+
       InlineForeignKeySql(FConn, 'Authorizations', 'ConsentKey', 'Ids', 'ResourceKey', 'FK_Authorizations_ConsKey')+
       InlineForeignKeySql(FConn, 'Authorizations', 'SessionKey', 'Sessions', 'SessionKey', 'FK_Authorizations_SessionKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Authorizations', 'AuthorizationKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Authorizations', 'PatientKey', 'Ids', 'ResourceKey', 'FK_Authorizations_PatKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Authorizations', 'ConsentKey', 'Ids', 'ResourceKey', 'FK_Authorizations_ConsKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Authorizations', 'SessionKey', 'Sessions', 'SessionKey', 'FK_Authorizations_SessionKey'));
  FConn.ExecSQL('Create INDEX SK_Authorizations_Uuid ON Authorizations (Uuid)');
end;

procedure TFHIRDatabaseInstaller.CreateAuthorizationSessions;
begin
  FConn.ExecSQL('CREATE TABLE AuthorizationSessions( '+#13#10+
       ' AuthorizationSessionKey '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' AuthorizationKey        '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' DateTime                '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Message                 nchar(255)                               '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' SessionKey              '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+   //
       InlineForeignKeySql(FConn, 'AuthorizationSessions', 'AuthorizationKey', 'Authorizations', 'AuthorizationKey', 'FK_AuthorizationSessions_AuthKey')+
       InlineForeignKeySql(FConn, 'AuthorizationSessions', 'SessionKey', 'Sessions', 'SessionKey', 'FK_AuthorizationSessions_SessKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_AuthorizationSessions', 'AuthorizationSessionKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'AuthorizationSessions', 'AuthorizationKey', 'Authorizations', 'AuthorizationKey', 'FK_AuthorizationSessions_AuthKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'AuthorizationSessions', 'SessionKey', 'Sessions', 'SessionKey', 'FK_AuthorizationSessions_SessKey'));
  FConn.ExecSQL('Create INDEX SK_Authorizations_AKey ON AuthorizationSessions (AuthorizationKey)');
end;

procedure TFHIRDatabaseInstaller.CreateClientRegistrations;
begin
  FConn.ExecSQL('CREATE TABLE ClientRegistrations ( '+#13#10+
       ' ClientKey         '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' DateRegistered    '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' SessionRegistered '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+    //
       ' SoftwareId        nchar(128)                               '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+    //
       ' SoftwareVersion   nchar(128)                               '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' Uri               nchar(255)                               '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' LogoUri           nchar(255)                               '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' Name              nchar(255)                               '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' PatientContext    int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Mode              int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Secret            nchar(36)                                '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' JwksUri           nchar(255)                               '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' Issuer            nchar(255)                               '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' SoftwareStatement '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' PublicKey         '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' Scopes            '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' Redirects         '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       InlineForeignKeySql(FConn, 'ClientRegistrations', 'SessionRegistered', 'Sessions', 'SessionKey', 'FK_ClientRegistrations_SessionKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_ClientRegistrations', 'ClientKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'ClientRegistrations', 'SessionRegistered', 'Sessions', 'SessionKey', 'FK_ClientRegistrations_SessionKey'));
  FConn.ExecSQL('Insert into ClientRegistrations (ClientKey, DateRegistered, Name, Mode, PatientContext) values (1, '+DBGetDate(FConn.Owner.Platform)+', ''Web Interface'', 0, 0)');
end;

procedure TFHIRDatabaseInstaller.CreatePackagePermissionsTable;
begin
  FConn.ExecSQL('CREATE TABLE PackagePermissions ( '+#13#10+
       ' PackagePermissionKey  '+DBKeyType(FConn.owner.platform)+'  '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' ManualToken           nchar(64)                            '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Email                 nchar(128)                           '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Mask                  nchar(64)                                '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+    //
       PrimaryKeyType(FConn.owner.Platform, 'PK_PackagePermissions', 'PackagePermissionKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_PackagePermissions_Token ON PackagePermissions (ManualToken)');
end;

procedure TFHIRDatabaseInstaller.CreatePackagesTables;
begin
  FConn.ExecSQL('CREATE TABLE Packages ( '+#13#10+
       ' PackageKey      '+DBKeyType(FConn.owner.platform)+'  '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Id              nchar(64)                            '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Canonical       nchar(128)                           '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' DownloadCount   int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' ManualToken     nchar(64)                                '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+    //
       ' CurrentVersion  '+DBKeyType(FConn.owner.platform)+'  '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Packages', 'PackageKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_Packages_Id ON Packages (Id, PackageKey)');
  FConn.ExecSQL('Create INDEX SK_Packages_Canonical ON Packages (Canonical, PackageKey)');

  FConn.ExecSQL('CREATE TABLE PackageVersions ( '+#13#10+
       ' PackageVersionKey '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' GUID              nchar(128)                               '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' PubDate           '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Indexed           '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Id                nchar(64)                                '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Version           nchar(64)                                '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Kind              int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' DownloadCount     int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' ManualToken       nchar(64)                                '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+    //
       ' Canonical         nchar(255)                               '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' FhirVersions      nchar(255)                               '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Description       '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, True) +', '+#13#10+    //
       ' Content           '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_PackageVersions', 'PackageVersionKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_PackageVersions_Id ON PackageVersions (Id, Version, PackageVersionKey)');
  FConn.ExecSQL('Create INDEX SK_PackageVersions_Canonical ON PackageVersions (Canonical, PackageVersionKey)');
  FConn.ExecSQL('Create INDEX SK_PackageVersions_PubDate ON PackageVersions (Id, PubDate, PackageVersionKey)');
  FConn.ExecSQL('Create INDEX SK_PackageVersions_Indexed ON PackageVersions (Indexed, PackageVersionKey)');
  FConn.ExecSQL('Create INDEX SK_PackageVersions_GUID ON PackageVersions (GUID)');

  FConn.ExecSQL('CREATE TABLE PackageFHIRVersions ( '+#13#10+
       ' PackageVersionKey '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Version            nchar(128)                               '+ColCanBeNull(FConn.owner.platform, False)+')'+#13#10);
  FConn.ExecSQL('Create INDEX SK_PackageFHIRVersions ON PackageFHIRVersions (PackageVersionKey)');


  FConn.ExecSQL('CREATE TABLE PackageDependencies ( '+#13#10+
       ' PackageVersionKey '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Dependency            nchar(128)                               '+ColCanBeNull(FConn.owner.platform, False)+')'+#13#10);
  FConn.ExecSQL('Create INDEX SK_PackageDependencies ON PackageDependencies (PackageVersionKey)');

end;

procedure TFHIRDatabaseInstaller.CreatePseudoData;
begin
  FConn.ExecSQL('CREATE TABLE PseudoData ( '+#13#10+
       ' PseudoDataKey  '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' ResourceType   int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Id             nchar(64)                                '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+    //
       ' Given          nchar(32)                                '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+    //
       ' Family         nchar(32)                                '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' Line           nchar(255)                               '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' City           nchar(32)                                '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' State          nchar(10)                                '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' PostCode       int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Telecom        nchar(20)                                '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       ' Photo          '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, True )+', '+#13#10+  //
       PrimaryKeyType(FConn.owner.Platform, 'PK_PseudoData', 'PseudoDataKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_PseudoData ON PseudoData (ResourceType, Id)');
end;


procedure TFHIRDatabaseInstaller.CreateClosureEntries;
begin
  FConn.ExecSQL('CREATE TABLE ClosureEntries ( '+#13#10+
       ' ClosureEntryKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' ClosureKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SubsumesKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' SubsumedKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' IndexedVersion int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       InlineForeignKeySql(FConn, 'ClosureEntries', 'ClosureKey',  'Closures', 'ClosureKey', 'FK_ClosureEntries_ConceptKey')+
       InlineForeignKeySql(FConn, 'ClosureEntries', 'SubsumesKey', 'Concepts', 'ConceptKey', 'FK_ClosureEntries_SubsumesKey')+
       InlineForeignKeySql(FConn, 'ClosureEntries', 'SubsumedKey', 'Concepts', 'ConceptKey', 'FK_ClosureEntries_SubsumedKey')+
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

procedure TFHIRDatabaseInstaller.CreateConnections;
begin
  FConn.ExecSQL('CREATE TABLE Connections( '+#13#10+
       ' ConnectionKey   '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' SourceUrl          nchar(255)                               '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Expiry          '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
       ' Status          int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Consent         char(64)                                 '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+   //
       ' AuthToken       char(64)                                 '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+   //
       ' Patient         char(64)                                 '+ColCanBeNull(FConn.owner.platform, True)+ ', '+#13#10+   //
       ' JWT             '+DBBlobType(FConn.owner.platform)+'                                    '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+   //
       PrimaryKeyType(FConn.owner.Platform, 'PK_Connections', 'ConnectionKey')+') '+CreateTableInfo(FConn.owner.platform));
end;



procedure TFHIRDatabaseInstaller.CreateLoadedPackagesTable;
begin
  FConn.ExecSQL('CREATE TABLE LoadedPackages ( '+#13#10+
       ' LoadedPackageKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Id                 nchar(127) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Version            nchar(50) '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' DateLoaded '+      DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Resources          int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Content '+         DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       PrimaryKeyType(FConn.owner.Platform, 'PK_LoadedPackageKey', 'LoadedPackageKey')+') '+CreateTableInfo(FConn.owner.platform));
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
       InlineForeignKeySql(FConn, 'NotificationQueue', 'SubscriptionKey','Ids', 'ResourceKey', 'FK_NotificationQ_SubVerKey')+
       InlineForeignKeySql(FConn, 'NotificationQueue', 'ResourceVersionKey',    'Versions', 'ResourceVersionKey', 'FK_NotificationQ_ResVerKey')+
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
       ' Scope nchar(1024) NOT NULL, '+#13#10+
       ' Redirect nchar(255) NOT NULL, '+#13#10+
       ' Patient nchar(64) NULL, '+#13#10+
       ' Status int NOT NULL, '+#13#10+
       ' DateAdded '+DBDateTimeType(FConn.owner.platform)+' NOT NULL, '+#13#10+
       ' DateSignedIn '+DBDateTimeType(FConn.owner.platform)+' NULL, '+#13#10+
       ' DateChosen '+DBDateTimeType(FConn.owner.platform)+' NULL, '+#13#10+
       ' DateTokenAccessed '+DBDateTimeType(FConn.owner.platform)+' NULL, '+#13#10+
       ' SessionKey '+DBKeyType(FConn.owner.platform)+' NULL, '+#13#10+
       ' Launch nchar(255) NULL, '+#13#10+
       ' ClientState '+DBBlobType(FConn.owner.platform)+' NOT NULL, '+#13#10+
       ' Rights '+DBBlobType(FConn.owner.platform)+' Null, '+#13#10+
       ' Jwt '+DBBlobType(FConn.owner.platform)+' Null, '+#13#10+
       InlineForeignKeySql(FConn, 'OAuthLogins', 'SessionKey', 'Sessions', 'SessionKey', 'FK_OUathLogins_SessionKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_OAuthLogins', 'Id')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'OAuthLogins', 'SessionKey', 'Sessions', 'SessionKey', 'FK_OUathLogins_SessionKey'));
end;


procedure TFHIRDatabaseInstaller.CreateObservations;
begin
  FConn.ExecSQL('CREATE TABLE Observations( '+#13#10+
       ' ObservationKey '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  // internal primary key
       ' ResourceKey    '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // id of resource this came from
       ' SubjectKey     '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+      // id of resource this observation is about
       ' DateTime       '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+        // observation.effectiveTime Stated (null = range)
       ' DateTimeMin    '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // observation.effectiveTime Min
       ' DateTimeMax    '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // observation.effectiveTime Max
       ' Value          '+DBFloatType(FConn.owner.platform)+'    '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+                 // stated value (if available)
       ' ValueUnit      '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+             // stated units (if available)
       ' Canonical      '+DBFloatType(FConn.owner.platform)+'    '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+             // canonical value (if units)
       ' CanonicalUnit  '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+         // canonical units (if canonical value)
       ' ValueConcept   '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+          // if observation is a concept (or a data missing value)
       ' IsComponent    int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+          // if observation is a concept (or a data missing value)
       ' CodeList       nchar(30)                                '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+          // roll up of observations as an optimization for grouping
       InlineForeignKeySql(FConn, 'Observations', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_Observations_ResKey')+
       InlineForeignKeySql(FConn, 'Observations', 'SubjectKey', 'Ids', 'ResourceKey', 'FK_Observations_SubjKey')+
       InlineForeignKeySql(FConn, 'Observations', 'ValueUnit', 'Concepts', 'ConceptKey', 'FK_Observations_ValueUnitKey')+
       InlineForeignKeySql(FConn, 'Observations', 'CanonicalUnit', 'Concepts', 'ConceptKey', 'FK_Observations_CanonicalUnitKey')+
       InlineForeignKeySql(FConn, 'Observations', 'ValueConcept', 'Concepts', 'ConceptKey', 'FK_Observations_ValueConceptKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Observations', 'ObservationKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Observations', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_Observations_ResKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Observations', 'SubjectKey', 'Ids', 'ResourceKey', 'FK_Observations_SubjKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Observations', 'ValueUnit', 'Concepts', 'ConceptKey', 'FK_Observations_ValueUnitKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Observations', 'CanonicalUnit', 'Concepts', 'ConceptKey', 'FK_Observations_CanonicalUnitKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Observations', 'ValueConcept', 'Concepts', 'ConceptKey', 'FK_Observations_ValueConceptKey'));
  FConn.ExecSQL('Create INDEX SK_Obs_Dt1 ON Observations (SubjectKey, IsComponent, DateTimeMin)');
  FConn.ExecSQL('Create INDEX SK_Obs_Dt2 ON Observations (SubjectKey, IsComponent, DateTimeMax)');
  FConn.ExecSQL('Create INDEX SK_Obs_Dt3 ON Observations (DateTimeMin)');
  FConn.ExecSQL('Create INDEX SK_Obs_Dt4 ON Observations (DateTimeMax)');
  FConn.ExecSQL('Create INDEX SK_Obs_CL ON Observations (CodeList)');
end;

procedure TFHIRDatabaseInstaller.CreateObservationCodes;
begin
  FConn.ExecSQL('CREATE TABLE ObservationCodes( '+#13#10+
       ' ObservationCodeKey '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // internal primary key
       ' ObservationKey     '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // primary key from Observations Table
       ' ConceptKey         '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // primary key from concept table
       ' Source             int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // 1 = Observation.category, 2 = Observation.code, 3= Observation.component.code
       InlineForeignKeySql(FConn, 'ObservationCodes', 'ObservationKey', 'Observations', 'ObservationKey', 'FK_ObservationCodes_ObsKey')+
       InlineForeignKeySql(FConn, 'ObservationCodes', 'ConceptKey', 'Concepts', 'ConceptKey', 'FK_ObservationCodes_ConceptKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_ObservationCodes', 'ObservationCodeKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'ObservationCodes', 'ObservationKey', 'Observations', 'ObservationKey', 'FK_ObservationCodes_ObsKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'ObservationCodes', 'ConceptKey', 'Concepts', 'ConceptKey', 'FK_ObservationCodes_ConceptKey'));
  FConn.ExecSQL('Create INDEX SK_ObsC_Dt1 ON ObservationCodes (ObservationKey, Source, ConceptKey)');
  FConn.ExecSQL('Create INDEX SK_ObsC_Dt2 ON ObservationCodes (Source, ConceptKey, ObservationKey)');
end;


procedure TFHIRDatabaseInstaller.CreateObservationQueue;
begin
  FConn.ExecSQL('CREATE TABLE ObservationQueue( '+#13#10+
       ' ObservationQueueKey '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  // internal primary key
       ' ResourceKey    '+DBKeyType(FConn.owner.platform)+'   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+     // id of resource changed
       ' Status         int                                   '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+      // whether added or deleted
       InlineForeignKeySql(FConn, 'Observations', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_ObservationQueue_ResKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_ObservationQueue', 'ObservationQueueKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Observations', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_ObservationQueue_ResKey'));
end;

procedure TFHIRDatabaseInstaller.CreateResourceCompartments;
begin
  FConn.ExecSQL('CREATE TABLE Compartments( '+#13#10+
       ' ResourceCompartmentKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+ // internal primary key
       ' ResourceKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+ // id of resource that is in a compartment
       ' TypeKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+ // resource type key for the compartment type
       ' Id nchar('+inttostr(ID_LENGTH)+') '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+                    // field two of composite id for compartment - compartment id
       ' CompartmentKey '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+   // key for the resource that creates this compartment
       InlineForeignKeySql(FConn, 'Compartments', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_CompartmentResource_ResKey')+
       InlineForeignKeySql(FConn, 'Compartments', 'TypeKey', 'Types', 'ResourceTypeKey', 'FK_CompartmentResource_TypeKey')+
       InlineForeignKeySql(FConn, 'Compartments', 'CompartmentKey', 'Ids', 'ResourceKey', 'FK_Compartment_ResKey')+
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
       ' ForTesting int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Deleted int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       InlineForeignKeySql(FConn, 'Ids', 'ResourceTypeKey', 'Types', 'ResourceTypeKey', 'FK_ResType_TypeKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_Ids', 'ResourceKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'Ids', 'ResourceTypeKey', 'Types', 'ResourceTypeKey', 'FK_ResType_TypeKey'));
  FConn.ExecSQL('Create Unique INDEX SK_Ids_Id ON Ids (ResourceTypeKey, Id)');
  FConn.ExecSQL('Create INDEX SK_Ids_Master ON Ids (MasterResourceKey)');
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
       ' ForTesting int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Tags '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' XmlContent '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' XmlSummary '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' JsonContent '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' JsonSummary '+DBBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       InlineForeignKeySql(FConn, 'Versions', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_ResKey_IdKey')+
       InlineForeignKeySql(FConn, 'Versions', 'AuditKey', 'Ids', 'ResourceKey', 'FK_ResKey_AuditKey')+
//       InlineForeignKeySql(FConn, 'Ids', 'MostRecent', 'Versions', 'ResourceVersionKey', 'FK_ResCurrent_VersionKey')+
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
       InlineForeignKeySql(FConn, 'VersionTags', 'ResourceVersionKey', 'Versions', 'ResourceVersionKey', 'FK_VerTag_VerKey')+
       InlineForeignKeySql(FConn, 'VersionTags', 'TagKey', 'Tags', 'TagKey', 'FK_VerTag_TagKey')+
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
       ' Score2 int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       InlineForeignKeySql(FConn, 'SearchEntries', 'SearchKey', 'Searches', 'SearchKey', 'FK_Search_Search')+
       InlineForeignKeySql(FConn, 'SearchEntries', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_Search_ResKey')+
       InlineForeignKeySql(FConn, 'SearchEntries', 'ResourceVersionKey', 'Versions', 'ResourceVersionKey', 'FK_Search_ResVerKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_SearchEntries', 'SearchKey, ResourceKey, ResourceVersionKey')+') '+CreateTableInfo(FConn.owner.platform));
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
       ' ResourcePreviousKey '+ DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+
       ' Operation                  int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Entered '+       DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' Handled '+       DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       InlineForeignKeySql(FConn, 'SubscriptionQueue', 'ResourceKey',        'Ids', 'ResourceKey',             'FK_SubscriptionQ_ResKey')+
       InlineForeignKeySql(FConn, 'SubscriptionQueue', 'ResourceVersionKey', 'Versions', 'ResourceVersionKey', 'FK_SubscriptionQ_ResVerKey')+
       InlineForeignKeySql(FConn, 'SubscriptionQueue', 'ResourcePreviousKey','Versions', 'ResourceVersionKey', 'FK_SubscriptionQ_ResPrevKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_SubscriptionQueue', 'SubscriptionQueueKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'SubscriptionQueue', 'ResourceKey',        'Ids', 'ResourceKey',             'FK_SubscriptionQ_ResKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'SubscriptionQueue', 'ResourceVersionKey', 'Versions', 'ResourceVersionKey', 'FK_SubscriptionQ_ResVerKey'));
  FConn.ExecSQL(ForeignKeySql(FConn, 'SubscriptionQueue', 'ResourcePreviousKey','Versions', 'ResourceVersionKey', 'FK_SubscriptionQ_ResPrevKey'));
  FConn.ExecSQL('Create INDEX SK_SubscriptionsQueue_Reload ON SubscriptionQueue (Handled)');
end;

//procedure TFHIRDatabaseInstaller.CreateTwilioTable;
//begin
//  FConn.ExecSQL('CREATE TABLE Twilio ( '+#13#10+
//       ' TwilioKey      '+DBKeyType(FConn.owner.platform)+'       '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
//       ' AccountId       nchar(255)                               '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
//       ' Status          int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
//       ' SourceNum       nchar(255)                               '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+    //
//       ' CreatedDate    '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+    //
//       ' DownloadedDate '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+    //
//       ' MsgBody            '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
//       PrimaryKeyType(FConn.owner.Platform, 'PK_Twilio', 'TwilioKey')+') '+CreateTableInfo(FConn.owner.platform));
//  FConn.ExecSQL('Create INDEX SK_Twilio_Account ON Twilio (AccountId, Status, DownloadedDate)');
//end;
//
procedure TFHIRDatabaseInstaller.CreateAsyncTasks;
begin
  FConn.ExecSQL('CREATE TABLE AsyncTasks ( '+#13#10+
       ' TaskKey     '+DBKeyType(FConn.owner.platform)+'      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Id          nchar(36)                                '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' SourceUrl   nchar(255)                               '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Format      int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Status      int                                      '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+  //
       ' Message     nchar(255)                               '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+  //
       ' Request     nchar(255)                               '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+  //
       ' Created     '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+    //
       ' Finished    '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+    //
       ' Expires     '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+    //
       ' TransactionTime '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+    //
       ' Deleted     '+DBDateTimeType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+    //
       ' Secure      int                                      '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+   //
       ' Count       int                                      '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+   //
       ' Downloads   int                                      '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+   //
       ' Names       '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+   //
       ' Outcome     '+DBBlobType(FConn.owner.platform)+'     '+ColCanBeNull(FConn.owner.platform, true)+', '+#13#10+   //
       PrimaryKeyType(FConn.owner.Platform, 'PK_AsyncTasks', 'TaskKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL('Create INDEX SK_AsyncTasksId ON AsyncTasks (Id)');
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
       ' SrcTesting int '+ColCanBeNull(FConn.owner.platform, False)+', '+#13#10+
       ' TgtTesting int '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Concept '+DBKeyType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Extension nchar(5) '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       ' Xhtml '+DBTextBlobType(FConn.owner.platform)+' '+ColCanBeNull(FConn.owner.platform, True)+', '+#13#10+
       InlineForeignKeySql(FConn, 'IndexEntries', 'IndexKey', 'Indexes', 'IndexKey', 'FK_IndexEntry_IndexKey')+
       InlineForeignKeySql(FConn, 'IndexEntries', 'ResourceKey', 'Ids', 'ResourceKey', 'FK_IndexEntry_ResKey')+
       InlineForeignKeySql(FConn, 'IndexEntries', 'SpaceKey', 'Spaces', 'SpaceKey', 'FK_IndexEntry_SpaceKey')+
       InlineForeignKeySql(FConn, 'IndexEntries', 'Target', 'Ids', 'ResourceKey', 'FK_IndexEntry_TargetKey')+
       InlineForeignKeySql(FConn, 'IndexEntries', 'Concept', 'Concepts', 'ConceptKey', 'FK_IndexEntry_ConceptKey')+
       InlineForeignKeySql(FConn, 'IndexEntries', 'Parent', 'IndexEntries', 'EntryKey', 'FK_IndexEntry_ParentKey')+
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
       InlineForeignKeySql(FConn, 'ValueSetMembers', 'ValueSetKey', 'ValueSets', 'ValueSetKey', 'FK_ValueSetMembers_ValueSetKey')+
       InlineForeignKeySql(FConn, 'ValueSetMembers', 'ConceptKey', 'Concepts', 'ConceptKey', 'FK_ValueSetMembers_ConceptKey')+
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
       ' URL nchar(255) '+ColCanBeNull(FConn.owner.platform, False)+', '+
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
       InlineForeignKeySql(FConn, 'UserIndexes', 'UserKey', 'Users', 'UserKey', 'FK_UserIndexes_UserKey')+
       PrimaryKeyType(FConn.owner.Platform, 'PK_UserIndexes', 'UserIndexKey')+') '+CreateTableInfo(FConn.owner.platform));
  FConn.ExecSQL(ForeignKeySql(FConn, 'UserIndexes', 'UserKey', 'Users', 'UserKey', 'FK_UserIndexes_UserKey'));
  FConn.ExecSQL('Create INDEX SK_UserIndexes_IndexNameValue ON UserIndexes (IndexName, Value)');
  FConn.ExecSQL('Create INDEX SK_UserIndexes_ParentIndexNameValue ON UserIndexes (Parent, IndexName, Value)');
End;


destructor TFHIRDatabaseInstaller.Destroy;
begin
  FServerFactory.free;
  FBases.Free;
  FFactory.Free;
  inherited;
end;

procedure TFHIRDatabaseInstaller.installPackageServer;
begin
  FConn.StartTransact;
  try
    if assigned(CallBack) then Callback(1, 'Create Config');
    CreateResourceConfig(false);
    if assigned(CallBack) then Callback(1, 'Create Package Tables');
    CreatePackagesTables;
    if assigned(CallBack) then Callback(2, 'Create Package Permissions Table');
    CreatePackagePermissionsTable;
    if assigned(CallBack) then Callback(4, 'Commit');
    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (100, ''package||Installed '+TFslDateTime.makeLocal.toString+''')');
    FConn.Commit;
  except
    on e:exception do
    begin
      FConn.Rollback;
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRDatabaseInstaller.InstallTerminologyServer;
begin
  FConn.StartTransact;
  try
    if assigned(CallBack) then Callback(1, 'Create Config');
    CreateResourceConfig(false);
    if assigned(CallBack) then Callback(1, 'Create Closure Table');
    CreateClosures;
    if assigned(CallBack) then Callback(1, 'Create Concept Table');
    CreateConcepts;
    if assigned(CallBack) then Callback(1, 'Create ValueSet Table');
    CreateValueSets;
    if assigned(CallBack) then Callback(1, 'Create ValueSetMember Table');
    CreateValueSetMembers;
    if assigned(CallBack) then Callback(1, 'Create ClosureEntry Table');
    CreateClosureEntries;
    if assigned(CallBack) then Callback(4, 'Commit');

    FConn.ExecSQL('Insert into Config (ConfigKey, Value) values (100, ''terminology||Installed '+TFslDateTime.makeLocal.toString+''')');
    FConn.Commit;
  except
    on e:exception do
    begin
      FConn.Rollback;
      recordStack(e);
      raise;
    end;
  end;
end;

procedure TFHIRDatabaseInstaller.DoPostTransactionInstall;
begin
  try
    if FConn.owner.platform = kdbMySQL then
    begin
//      FConn.ExecSQL('ALTER TABLE IndexEntries MODIFY Xhtml LONGTEXT;');
      FConn.ExecSQL('ALTER TABLE IndexEntries ADD FULLTEXT INDEX `PK_IndexEntries` (Xhtml);');
    end;
    if FConn.owner.platform = kdbSQLServer then
    begin
      FConn.ExecSQL('CREATE FULLTEXT CATALOG FHIR as DEFAULT');
      FConn.ExecSQL('Create FULLTEXT INDEX on IndexEntries (Xhtml TYPE COLUMN Extension) KEY INDEX PK_IndexEntries');
    end;

  except
    // well, we ignore this; it fails on SQLServer Express, in which case _text search parameter won't work
  end;
end;

procedure TFHIRDatabaseInstaller.DoPostTransactionUnInstall;
begin
  try
    if FConn.owner.platform = kdbSQLServer  then
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

  m := TFHIRIndexInformation.create(FFactory.link, FServerFactory.link);
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
    if assigned(CallBack) then Callback(40, 'Create Users');
    CreateUsers;
    if assigned(CallBack) then Callback(41, 'Create UserIndexes');
    CreateUserIndexes;
    scim.defineSystem(FConn);
    if assigned(CallBack) then Callback(42, 'Create ResourceSessions');
    CreateResourceSessions;
    if assigned(CallBack) then Callback(43, 'Create OAuthLogins');
    CreateOAuthLogins;

    if assigned(CallBack) then Callback(45, 'Create Closures');
    CreateClosures;
    if assigned(CallBack) then Callback(46, 'Create Concepts');
    CreateConcepts;
    if assigned(CallBack) then Callback(47, 'Create ValueSets');
    CreateValueSets;
    if assigned(CallBack) then Callback(48, 'Create ValueSetMembers');
    CreateValueSetMembers;
    if assigned(CallBack) then Callback(49, 'Create ClosureEntries');
    CreateClosureEntries;

    if assigned(CallBack) then Callback(50, 'Create ResourceTags');
    CreateResourceTags;
    if assigned(CallBack) then Callback(51, 'Create ResourceTypes');
    CreateResourceTypes;
    if assigned(CallBack) then Callback(52, 'Create ResourceConfig');
    CreateResourceConfig(true);
    if assigned(CallBack) then Callback(53, 'Create Resources');
    CreateResources;
    if assigned(CallBack) then Callback(54, 'Create ResourceCompartments');
    CreateResourceCompartments;
    if assigned(CallBack) then Callback(55, 'Create ResourceVersions');
    CreateResourceVersions;
    if assigned(CallBack) then Callback(56, 'Create ResourceVersionsTags');
    CreateResourceVersionsTags;
    if assigned(CallBack) then Callback(57, 'Create ResourceIndexes');
    CreateResourceIndexes;
    if assigned(CallBack) then Callback(58, 'Create ResourceSpaces');
    CreateResourceSpaces;
    if assigned(CallBack) then Callback(59, 'Create ResourceIndexEntries');
    CreateResourceIndexEntries;
    if assigned(CallBack) then Callback(60, 'Create ResourceSearches');
    CreateResourceSearches;
    if assigned(CallBack) then Callback(61, 'Create ResourceSearchEntries');
    CreateResourceSearchEntries;
    if assigned(CallBack) then Callback(62, 'Create SubscriptionQueue');
    CreateSubscriptionQueue;
    if assigned(CallBack) then Callback(63, 'Create NotificationQueue');
    CreateNotificationQueue;
    if assigned(CallBack) then Callback(64, 'Create WebSocketsQueue');
    CreateWebSocketsQueue;

    if assigned(CallBack) then Callback(65, 'Create ResourceSpaces');
    DefineResourceSpaces;
    if assigned(CallBack) then Callback(66, 'Create Indexes');
    DefineIndexes;
    if assigned(CallBack) then Callback(67, 'Create Observations');
    CreateObservations;
    if assigned(CallBack) then Callback(68, 'Create ObservationCodes');
    CreateObservationCodes;
    if assigned(CallBack) then Callback(69, 'Create ObservationQueue');
    CreateObservationQueue;
    if assigned(CallBack) then Callback(70, 'Create Authorizations');
    CreateAuthorizations;
    if assigned(CallBack) then Callback(71, 'Create AuthorizationSessions');
    CreateAuthorizationSessions;
    if assigned(CallBack) then Callback(72, 'Create Connections');
    CreateConnections;
    if assigned(CallBack) then Callback(74, 'Create AsyncTasks');
    CreateAsyncTasks;
    if assigned(CallBack) then Callback(75, 'Create ClientRegistrations');
    CreateClientRegistrations;
    if assigned(CallBack) then Callback(75, 'Create PseudoData');
    CreatePseudoData;
    if assigned(CallBack) then Callback(76, 'Create Package Tables');
    CreatePackagesTables;
    if assigned(CallBack) then Callback(77, 'Create Package Permissions Table');
    CreatePackagePermissionsTable;
    if assigned(CallBack) then Callback(79, 'Commit');
    CreateLoadedPackagesTable;
    if assigned(CallBack) then Callback(80, 'Commit');

    FConn.Commit;
  except
    on e:exception do
    begin
      FConn.Rollback;
      recordStack(e);
      raise;
    end;
  end;
  if assigned(CallBack) then Callback(68, 'Do Post Install');
  DoPostTransactionInstall;
end;

//procedure TFHIRDatabaseInstaller.runScript(s: String);
//var
//  lines : TStringList;
//  sql, l : String;
//begin
//  lines := TStringList.create;
//  try
//    lines.Text := replaceColumnWrappingChars(FileToString(IncludeTrailingPathDelimiter(Ftxpath)+s, TEncoding.ANSI), FConn.Owner.Platform);
//    sql := '';
//    for l in lines do
//      if l.Trim = 'GO' then
//      begin
//        Fconn.ExecSQL(sql);
//        sql := '';
//      end
//      else
//        sql := sql + l+#13#10;
//    if (sql.trim <> '') then
//      Fconn.ExecSQL(sql);
//  finally
//    lines.free;
//  end;
//end;

procedure TFHIRDatabaseInstaller.Uninstall;
var
  meta : TFDBMetaData;
  step : integer;
  procedure drop(name : String);
  begin
    inc(step);
    if assigned(CallBack) then Callback(step, 'Check Delete '+name);
    if meta.hasTable(name, true) then
      FConn.DropTable(name);
  end;
begin
  if assigned(CallBack) then Callback(5, 'Check for existing tables');
  step := 5;
  meta := FConn.FetchMetaData;
  try
    FConn.StartTransact;
    try
      if meta.hasTable('Ids') then
      try
        if FConn.owner.platform = kdbMySQL then
          FConn.execsql('ALTER TABLE Ids DROP FOREIGN KEY FK_ResCurrent_VersionKey')
        else if FConn.owner.platform = kdbSQLServer then
          FConn.execsql('ALTER TABLE Ids DROP CONSTRAINT FK_ResCurrent_VersionKey');
      except
      end;

      drop('Connections');
      drop('AuthorizationSessions');
      drop('Authorizations');
      drop('ObservationCodes');
      drop('Observations');
      drop('ObservationQueue');
      drop('WebSocketsQueue');
      drop('NotificationQueue');
      drop('SubscriptionQueue');
      drop('SearchEntries');
      drop('Searches');
      drop('IndexEntries');
      drop('Indexes');
      drop('Spaces');

      drop('LoadedPackages');

      drop('PackageFHIRVersions');
      drop('PackageDependencies');
      drop('PackageVersions');
      drop('Packages');
      drop('PackagePermissions');

      drop('VersionTags');
      drop('Versions');
      drop('Compartments');
      drop('Ids');
      drop('Config');
      drop('Types');
      drop('Tags');
      drop('OAuthLogins');
      drop('ClientRegistrations');
      drop('Sessions');
      drop('UserIndexes');
      drop('Users');

      drop('ClosureEntries');
      drop('ValueSetMembers');
      drop('ValueSets');
      drop('Closures');
      drop('Concepts');

      drop('UniiDesc');
      drop('Unii');
      drop('AsyncTasks');
      drop('PseudoData');
      drop('Twilio');

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
    if assigned(CallBack) then Callback(35, 'Post Uninstall check');
    DoPostTransactionUnInstall;
  finally
    // nothing
  end;
end;


procedure TFHIRDatabaseInstaller.Upgrade(version : integer);
begin
  FConn.StartTransact;
  try
    if version > ServerDBVersion then
      raise EDBException.create('Database Version mismatch (found='+inttostr(version)+', can handle 12-'+inttostr(ServerDBVersion)+'): you must re-install the database or change which version of the server you are running');
    if (version < ServerDBVersionEarliestSupported) then
      raise EDBException.create('Database must be rebuilt');
    if (version < 13) then
    begin
      Fconn.ExecSQL('ALTER TABLE dbo.Observations ADD  IsComponent int NULL');
      Fconn.ExecSQL('ALTER TABLE dbo.Observations ADD  CodeList nchar(30) NULL');
    end;

    if (version < 14) then
      CreateAuthorizations
    else
    begin
      if (version < 15) then
      begin
        Fconn.ExecSQL('Drop index SK_Authorizations_Hash on Authorizations');
        Fconn.ExecSQL('ALTER TABLE dbo.Authorizations Drop Column Hash');
        Fconn.ExecSQL('ALTER TABLE dbo.Authorizations ADD  Uuid char(36) NULL');
      end;
      if (version < 16) then
        Fconn.ExecSQL('ALTER TABLE dbo.Authorizations ADD  PatientId char(64) NULL');
    end;
    if (version < 17) then
    begin
     CreateAuthorizationSessions;
     CreateConnections;
    end;
    if (version < 18) then
      CreateAsyncTasks;
    if (version < 19) then
      CreateClientRegistrations;
    if (version < 20) then
      CreatePseudoData;
    if (version < 21) then
      Fconn.ExecSQL('ALTER TABLE dbo.ClientRegistrations ADD PatientContext int NULL');
    if (version < 22) then
    begin
      Fconn.ExecSQL('ALTER TABLE dbo.AsyncTasks ADD Request char(255) NULL');
      Fconn.ExecSQL('ALTER TABLE dbo.AsyncTasks ADD TransactionTime '+DBDateTimeType(FConn.Owner.Platform)+' NULL');
    end;
    if (version < 23) then
    begin
      Fconn.ExecSQL('ALTER TABLE dbo.AsyncTasks ADD Secure int NULL');
    end;
    if (version < 24) then
      CreatePackagesTables;
    if (version <= 26) then
    begin
      Fconn.ExecSQL('ALTER TABLE dbo.PackageVersions ADD DownloadCount int NULL');
      Fconn.ExecSQL('Update PackageVersions set DownloadCount = 0');
      Fconn.ExecSQL('ALTER TABLE dbo.Packages ADD DownloadCount int NULL');
      Fconn.ExecSQL('Update Packages set DownloadCount = 0');
    end;
    if (version <= 27) then
    begin
      Fconn.ExecSQL('ALTER TABLE dbo.PackageVersions ADD ManualToken nchar(64) NULL');
      Fconn.ExecSQL('ALTER TABLE dbo.Packages ADD ManualToken nchar(64) NULL');
      CreatePackagePermissionsTable;
    end;
    if (version <= 28) then
    begin
      CreateLoadedPackagesTable;
    end;
    if (version <= 29) then
    begin
      Fconn.ExecSQL('ALTER TABLE dbo.OAuthLogins ADD Launch nchar(255) NULL');
    end;
    if (version <= 30) then
    begin
      Fconn.ExecSQL('ALTER TABLE dbo.OAuthLogins ALTER COLUMN Scope nchar(1024) NOT NULL');
    end;

    Fconn.ExecSQL('update Config set value = '+inttostr(ServerDBVersion)+' where ConfigKey = 5');
    FConn.commit;
  except
    FConn.rollback;
    raise;
  end;
end;

function readInstallerSecurityMode(s : String) : TFHIRInstallerSecurityMode;
begin
  s := s.ToLower;
  if s = 'open' then
    result := ismOpenAccess
  else if s = 'closed' then
    result := ismClosedAccess
  else if s = 'read-only' then
    result := ismReadOnly
  else if s = 'terminology' then
    result := ismTerminologyServer
  else
    result := ismUnstated;
end;

end.

