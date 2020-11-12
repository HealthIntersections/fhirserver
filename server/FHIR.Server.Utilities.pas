Unit FHIR.Server.Utilities;

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
  {$IFDEF WINDOWS} Windows, {$ENDIF}
  SysUtils, Classes, IniFiles, Generics.Collections,
  IdCustomHTTPServer,
  fsl_utilities, fsl_base, fsl_logging, fsl_threads, fsl_http,
  FHIR.Database.Manager, FHIR.Database.ODBC, FHIR.Database.Dialects, FHIR.Database.SQLite,
  fhir_objects,  fhir_utilities, fhir_factory, fhir_common,
  FHIR.Server.Ini, FHIR.Server.Session;

var
  GCounterWebConnections : integer = 0;
  GCounterWebRequests : integer = 0;
  GCounterFHIRRequests : integer = 0;

type
  TProcessFileEvent = procedure (request : TIdHTTPRequestInfo; response : TIdHTTPResponseInfo; session : TFhirSession; path : String; secure : boolean; variables: TFslMap<TFHIRObject> = nil) of Object;

  TFHIRResourceConfig = class (TFslObject)
  public
    name : String;
    key: integer;
    Supported: Boolean;
    IdGuids: Boolean;
    IdClient: Boolean;
    IdServer: Boolean;
    cmdUpdate: Boolean;
    cmdDelete: Boolean;
    cmdValidate: Boolean;
    cmdHistoryInstance: Boolean;
    cmdHistoryType: Boolean;
    cmdSearch: Boolean;
    cmdCreate: Boolean;
    cmdOperation: Boolean;
    cmdVRead : boolean;
    versionUpdates: Boolean;

    lastResourceId : integer;
    storedResourceId : integer;

    constructor Create; override;
  end;

  TFHIRServerWorker = class (TFslObject)
  private
    FServerContext : TFslObject; // no link
  public
    constructor Create(ServerContext : TFslObject);
    destructor Destroy; override;

    Property ServerContext : TFslObject read FServerContext;
  end;

  TFHIRServerSettings = class (TFslObject)
  private
    FLock : TFslLock;

    FBases: TStringList;
    FOwnerName: String;
    FForLoad : boolean;
    FRunNumber: integer;
    FRequestId : integer;

    FMaintenanceThreadStatus : String;
    FSubscriptionThreadStatus : String;
    FEmailThreadStatus : String;

    FSMTPPort: String;
    FSMTPPassword: String;
    FSMTPHost: String;
    FSMTPSender: String;
    FSMTPUsername: String;
    FDirectPort: String;
    FDirectPassword: String;
    FDirectHost: String;
    FDirectSender: String;
    FDirectUsername: String;
    FDirectPopHost : String;
    FDirectPopPort : String;
    FSMTPUseTLS: boolean;
    FSMSFrom: String;
    FSMSToken: String;
    FSMSAccount: String;

    function GetMaintenanceThreadStatus: String;
    procedure SetMaintenanceThreadStatus(const Value: String);
    function GetSubscriptionThreadStatus: String;
    procedure SetSubscriptionThreadStatus(const Value: String);
    function GetEmailThreadStatus: String;
    procedure SetEmailThreadStatus(const Value: String);
  public
    constructor Create; override;
    destructor Destroy; override;
    Function Link : TFHIRServerSettings; overload;

    procedure load(ini : TFHIRServerIniFile);

    Property Bases: TStringList read FBases;
    Property OwnerName: String read FOwnerName;// write FOwnerName;
    property ForLoad : boolean read FForLoad write FForLoad;
    Property RunNumber : integer read FRunNumber;
    function nextRequestId : string;

    Property SMTPHost : String read FSMTPHost;// write FSMTPHost;
    Property SMTPPort : String read FSMTPPort;// write FSMTPPort;
    Property SMTPUsername : String read FSMTPUsername;// write FSMTPUsername;
    Property SMTPPassword : String read FSMTPPassword;// write FSMTPPassword;
    Property SMTPSender : String read FSMTPSender;// write FSMTPSender;
    Property SMTPUseTLS : boolean read FSMTPUseTLS;// write FSMTPUseTLS;
    Property DirectHost : String read FDirectHost;// write FDirectHost;
    Property DirectPort : String read FDirectPort;// write FDirectPort;
    Property DirectUsername : String read FDirectUsername;// write FDirectUsername;
    Property DirectPassword : String read FDirectPassword;// write FDirectPassword;
    Property DirectSender : String read FDirectSender;// write FDirectSender;
    Property SMSAccount : String read FSMSAccount;// write FSMSAccount;
    Property SMSToken : String read FSMSToken;// write FSMSToken;
    Property SMSFrom : String read FSMSFrom;// write FSMSFrom;
    property DirectPopHost : String read FDirectPopHost;// write FDirectPopHost;
    property DirectPopPort : String read FDirectPopPort;// write FDirectPopPort;

    Property MaintenanceThreadStatus : String read GetMaintenanceThreadStatus write SetMaintenanceThreadStatus;
    Property SubscriptionThreadStatus : String read GetSubscriptionThreadStatus write SetSubscriptionThreadStatus;
    Property EmailThreadStatus : String read GetEmailThreadStatus write SetEmailThreadStatus;
  end;

function buildCompartmentsSQL(resconfig : TFslMap<TFHIRResourceConfig>; compartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>) : String;
function LoadBinaryResource(factory : TFHIRFactory; const lang : THTTPLanguages; b: TBytes): TFhirResourceV;
function connectToDatabase(s : String; details : TFHIRServerIniComplex) : TFslDBManager;

implementation

function LoadBinaryResource(factory : TFHIRFactory; const lang : THTTPLanguages; b: TBytes): TFhirResourceV;
var
  s : TBytes;
  i, j : integer;
  ct : AnsiString;
begin
  s := ZDecompressBytes(b);
  move(s[0], i, 4);
  setLength(ct, i);
  move(s[4], ct[1], i);
  move(s[4+i], j, 4);

  result := factory.makeBinary(copy(s, 8+i, j), String(ct));
end;

function buildCompartmentsSQL(resconfig : TFslMap<TFHIRResourceConfig>; compartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>) : String;
var
  first : boolean;
  c : TFHIRCompartmentId;
begin
  result := '';
  if (compartment <> nil) then
    if compartment.Id = '*' then
      result := ' and Ids.ResourceKey in (select ResourceKey from Compartments where TypeKey = '+inttostr(ResConfig[compartment.ResourceType].key)+' and Id is not null)'
    else
      result := ' and Ids.ResourceKey in (select ResourceKey from Compartments where TypeKey = '+inttostr(ResConfig[compartment.ResourceType].key)+' and Id = '''+compartment.Id+''')';

  if (sessionCompartments <> nil) and (sessionCompartments.Count > 0) then
  begin
    result := result +' and Ids.ResourceKey in (select ResourceKey from Compartments where ';
    first := true;
    for c in sessionCompartments do
    begin
      if first then
        first := false
      else
        result := result + ' or ';
      result := result + 'TypeKey = '+inttostr(ResConfig[c.ResourceType].key)+' and Id = '''+c.id+'''';
    end;
    result := result + ')';
  end;
end;

{ TFHIRResourceConfig }

constructor TFHIRResourceConfig.Create;
begin
  inherited;
  Supported := true;
  IdGuids := false;
  IdClient := true;
  IdServer := true;
  cmdUpdate := true;
  cmdDelete := true;
  cmdValidate := true;
  cmdHistoryInstance := true;
  cmdHistoryType := true;
  cmdSearch := true;
  cmdCreate := true;
  cmdOperation := true;
  versionUpdates := false;
  lastResourceId  := 0;
end;

{ TFHIRServerWorker }

constructor TFHIRServerWorker.Create(ServerContext: TFslObject);
begin
  inherited Create;
  FServerContext := ServerContext;
end;

destructor TFHIRServerWorker.Destroy;
begin
  inherited;
end;

{ TFHIRServerSettings }

constructor TFHIRServerSettings.Create;
begin
  inherited;
  FBases := TStringList.Create;
  FBases.add('http://localhost/');
  FLock := TFslLock.Create('Settings');
end;

destructor TFHIRServerSettings.Destroy;
begin
  FBases.free;
  FLock.Free;
  inherited;
end;

function TFHIRServerSettings.Link: TFHIRServerSettings;
begin
  result := TFHIRServerSettings(inherited Link);
end;

procedure TFHIRServerSettings.load(ini: TFHIRServerIniFile);
begin
   // FBases - set in kernel
   // FForLoad - set in kernel
  FRunNumber := ini.runNumber + 1;
  ini.runNumber := FRunNumber;
  FRequestId := 0;

  FMaintenanceThreadStatus := 'Not started';
  FSubscriptionThreadStatus := 'Not started';
  FEmailThreadStatus := 'Not started';

  FOwnerName := ini.admin['ownername'];

  FSMTPPort := ini.destinations['email']['port'];
  FSMTPPassword := ini.destinations['email']['password'];
  FSMTPHost := ini.destinations['email']['host'];
  FSMTPSender := ini.destinations['email']['sender'];
  FSMTPUsername := ini.destinations['email']['username'];
  FSMTPUseTLS := ini.destinations['email']['secure'] = 'true';

  FDirectPort := ini.destinations['direct']['port'];
  FDirectPassword := ini.destinations['direct']['password'];
  FDirectHost := ini.destinations['direct']['host'];
  FDirectSender := ini.destinations['direct']['sender'];
  FDirectUsername := ini.destinations['direct']['username'];
  FDirectPopHost  := ini.destinations['direct']['pop-host'];
  FDirectPopPort  := ini.destinations['direct']['pop-port'];

  FSMSFrom := ini.destinations['sms']['from'];
  FSMSToken := ini.destinations['sms']['token'];
  FSMSAccount := ini.destinations['sms']['account'];
end;

function TFHIRServerSettings.nextRequestId: string;
var
  v : integer;
begin
  v := InterlockedIncrement(FRequestId);
  result := inttostr(FRunNumber)+'-'+inttostr(v);
end;


procedure TFHIRServerSettings.SetMaintenanceThreadStatus(const Value: String);
begin
  FLock.Lock;
  try
    FMaintenanceThreadStatus := Value;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRServerSettings.SetSubscriptionThreadStatus(const Value: String);
begin
  FLock.Lock;
  try
    FSubscriptionThreadStatus := Value;
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRServerSettings.SetEmailThreadStatus(const Value: String);
begin
  FLock.Lock;
  try
    FEmailThreadStatus := Value;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRServerSettings.GetMaintenanceThreadStatus: String;
begin
  FLock.Lock;
  try
    result := FMaintenanceThreadStatus;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRServerSettings.GetSubscriptionThreadStatus: String;
begin
  FLock.Lock;
  try
    result := FSubscriptionThreadStatus;
  finally
    FLock.Unlock;
  end;
end;

function TFHIRServerSettings.GetEmailThreadStatus: String;
begin
  FLock.Lock;
  try
    result := FEmailThreadStatus;
  finally
    FLock.Unlock;
  end;
end;

function connectToDatabase(s : String; details : TFHIRServerIniComplex) : TFslDBManager;
var
  dbn, ddr : String;
begin
  dbn := details['database'];
  ddr := details['driver'];
  if sameText(details['type'], 'mssql') then
  begin
    Logging.log('Connect to '+s+' ('+details['type']+'://'+details['server']+'/'+dbn+')');
    if ddr = '' then
      ddr := 'SQL Server Native Client 11.0';
    result := TFslDBOdbcManager.create(s, kdbSQLServer, 100, 0, ddr, details['server'], dbn, details['username'], details['password']);
  end
  else if sameText(details['type'], 'mysql') then
  begin
    Logging.log('Connect to '+s+' ('+details['type']+'://'+details['server']+'/'+dbn+')');
    result := TFslDBOdbcManager.create(s, kdbMySql, 100, 0, ddr, details['server'], dbn, details['username'], details['password']);
  end
  else if sameText(details['type'], 'SQLite') then
  begin
    Logging.log('Connect to '+s+' ('+details['type']+':'+dbn+')');
    result := TFslDBSQLiteManager.create(s, dbn, details['auto-create'] = 'true');
  end
  else
    raise ELibraryException.Create('Unknown database type '+details['type']);
end;

end.


