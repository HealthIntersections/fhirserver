Unit utilities;

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
  IdSMTP, IdMessage, IdOpenSSLIOHandlerClient, IdExplicitTLSClientServerBase, IdGlobal, IdOpenSSLVersion,
//  IdMultipartFormData, IdHeaderList, IdCustomHTTPServer, IdHTTPServer, IdTCPServer, IdContext, IdHTTP, IdCookie, IdZLibCompressorBase, IdSSL,
//  IdCompressorZLib, IdZLib, IdSchedulerOfThreadPool, IdGlobalProtocols, fsl_websocket,
//  IdOpenSSLIOHandlerServer, IdOpenSSLX509,

  IdCustomHTTPServer,
  fsl_utilities, fsl_base, fsl_logging, fsl_threads, fsl_http, fsl_twilio,
  fdb_manager, fdb_odbc, fdb_dialects, fdb_sqlite3,
  fhir_objects,  fhir_utilities, fhir_factory, fhir_common, fhir_parser,
  server_config, session;

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
    FIni : TFHIRServerConfigFile;

    FBases: TStringList;
    FOwnerName: String;
    FForLoad : boolean;
    FRunNumber: integer;
    FRequestId : integer;

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
    FHostSms: String; // for status update messages

  public
    constructor Create; override;
    destructor Destroy; override;
    Function Link : TFHIRServerSettings; overload;

    procedure load(ini : TFHIRServerConfigFile);

    property Ini : TFHIRServerConfigFile read FIni;
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
    property HostSms : String read FHostSms write FHostSms;

  end;

function buildCompartmentsSQL(resconfig : TFslMap<TFHIRResourceConfig>; compartment : TFHIRCompartmentId; sessionCompartments : TFslList<TFHIRCompartmentId>) : String;
function LoadBinaryResource(factory : TFHIRFactory; const lang : THTTPLanguages; b: TBytes): TFhirResourceV;
function connectToDatabase(details : TFHIRServerConfigSection) : TFDBManager;
function describeDatabase(details : TFHIRServerConfigSection) : String;
function checkDatabaseInstall(cfg : TFHIRServerConfigSection) : String;
procedure sendEmail(settings : TFHIRServerSettings; dest, subj, body: String);
procedure sendSMS(settings : TFHIRServerSettings; Dest, Msg: String);


implementation

function LoadBinaryResource(factory : TFHIRFactory; const lang : THTTPLanguages; b: TBytes): TFhirResourceV;
var
//  s : TBytes;
//  i, j : integer;
//  ct : AnsiString;
  p : TFHIRParser;
begin
//  s := ZDecompressBytes(b);
//  move(s[0], i, 4);
//  setLength(ct, i);
//  move(s[4], ct[1], i);
//  move(s[4+i], j, 4);
//
//  result := factory.makeBinary(copy(s, 8+i, j), String(ct));
  p := factory.makeParser(nil, ffXml, lang);
  try
    result := p.parseResource(b);
  finally
    p.Free;
  end;
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
  FIni.Free;
  FBases.free;
  FLock.Free;
  inherited;
end;

function TFHIRServerSettings.Link: TFHIRServerSettings;
begin
  result := TFHIRServerSettings(inherited Link);
end;

procedure TFHIRServerSettings.load(ini: TFHIRServerConfigFile);
begin
   // FBases - set in kernel
   // FForLoad - set in kernel
  FIni := ini.link;
  FRunNumber := ini['service'].prop['runNumber'].readAsInt(0) + 1;
  ini['service'].prop['runNumber'].value := inttostr(FRunNumber);
  ini.Save;
  FRequestId := 0;

  FOwnerName := ini.admin['ownername'].value;
  FHostSms := ini.admin['owner-sms'].value;

  FSMTPPort := ini['destinations'].section['email']['port'].value;
  FSMTPPassword := ini['destinations'].section['email']['password'].value;
  FSMTPHost := ini['destinations'].section['email']['host'].value;
  FSMTPSender := ini['destinations'].section['email']['sender'].value;
  FSMTPUsername := ini['destinations'].section['email']['username'].value;
  FSMTPUseTLS := ini['destinations'].section['email']['secure'].readAsBool(false);

  FDirectPort := ini['destinations'].section['direct']['port'].value;
  FDirectPassword := ini['destinations'].section['direct']['password'].value;
  FDirectHost := ini['destinations'].section['direct']['host'].value;
  FDirectSender := ini['destinations'].section['direct']['sender'].value;
  FDirectUsername := ini['destinations'].section['direct']['username'].value;
  FDirectPopHost  := ini['destinations'].section['direct']['pop-host'].value;
  FDirectPopPort  := ini['destinations'].section['direct']['pop-port'].value;

  FSMSFrom := ini['destinations'].section['sms']['from'].value;
  FSMSToken := ini['destinations'].section['sms']['token'].value;
  FSMSAccount := ini['destinations'].section['sms']['account'].value;
end;

function TFHIRServerSettings.nextRequestId: string;
var
  v : integer;
begin
  v := InterlockedIncrement(FRequestId);
  result := inttostr(FRunNumber)+'-'+inttostr(v);
end;

function connectToDatabase(details : TFHIRServerConfigSection) : TFDBManager;
var
  dbn, ddr : String;
begin
  dbn := details['db-database'].value;
  ddr := details['db-driver'].value;
  if sameText(details['db-type'].value, 'mssql') then
  begin
    Logging.log('Connect to '+details.name+' ('+details['db-type'].value+'://'+details['db-server'].value+'/'+dbn+')');
    if ddr = '' then
      ddr := 'SQL Server Native Client 11.0';
    result := TFDBOdbcManager.create(details.name, kdbSQLServer, 100, 0, ddr, details['db-server'].value, dbn, details['db-username'].value, details['db-password'].value);
  end
  else if sameText(details['type'].value, 'mysql') then
  begin
    Logging.log('Connect to '+details.name+' ('+details['db-type'].value+'://'+details['db-server'].value+'/'+dbn+')');
    result := TFDBOdbcManager.create(details.name, kdbMySql, 100, 0, ddr, details['db-server'].value, dbn, details['db-username'].value, details['db-password'].value);
  end
  else if sameText(details['db-type'].value, 'SQLite') then
  begin
    Logging.log('Connect to '+details.name+' ('+details['db-type'].value+':'+dbn+')');
    result := TFDBSQLiteManager.create(details.name, dbn, details['db-auto-create'].value = 'true');
  end
  else
    raise ELibraryException.Create('Unknown database type '+details['db-type'].value);
end;

function describeDatabase(details: TFHIRServerConfigSection): String;
begin
  if sameText(details['db-type'].value, 'mssql') then
    result := details['db-type'].value+'://'+details['db-server'].value+'/'+details['db-database'].value
  else if sameText(details['type'].value, 'mysql') then
    result := details['db-type'].value+'://'+details['db-server'].value+'/'+details['db-database'].value
  else if sameText(details['db-type'].value, 'SQLite') then
    result := details['db-type'].value+':'+details['db-database'].value
  else
    result := 'Unknown database type '+details['db-type'].value
end;

function checkDatabaseInstall(cfg : TFHIRServerConfigSection) : String;
var
  db : TFDBManager;
  conn : TFDBConnection;
  meta : TFDBMetaData;
  t, m, s : String;
begin
  try
    db := connectToDatabase(cfg);
    try
      conn := db.GetConnection('check');
      try
        meta := conn.FetchMetaData;
        try
          if not meta.HasTable('Config') then
            result := 'Not Installed'
          else
          begin
            s := conn.Lookup('Config', 'ConfigKey', '100', 'Value', '');
            if (s = '') then
              result := 'Needs Reinstalling'
            else
            begin
              StringSplit(s, '|', t, s);
              StringSplit(s, '|', m, s);
              if (t <> cfg['type'].value) then
                result := 'Type Mismatch - Database is for '+t+': reinstall'
              else if (m <> cfg['mode'].value) then
                result := 'Mode Mismatch - Database is for '+m+': reinstall'
              else
                result := 'OK ('+s+')';
            end;
          end;
        finally
          meta.free;
        end;
        conn.release;
      except
        on e : Exception do
        begin
          conn.Error(e);
          raise;
        end;
      end;
    finally
      db.free;
    end;
  except
    on e: Exception do
      result := 'Error: '+e.message;
  end;
end;

procedure sendEmail(settings : TFHIRServerSettings; dest, subj, body: String);
var
  sender : TIdSMTP;
  msg : TIdMessage;
  ssl : TIdOpenSSLIOHandlerClient;
begin
  sender := TIdSMTP.Create(Nil);
  try
    sender.Host := settings.SMTPHost;
    sender.port := StrToInt(settings.SMTPPort);
    sender.Username := settings.SMTPUsername;
    sender.Password := settings.SMTPPassword;
    if settings.SMTPUseTLS then
    begin
      ssl := TIdOpenSSLIOHandlerClient.create;
      sender.IOHandler := ssl;
      sender.UseTLS := utUseExplicitTLS;
      ssl.Destination := settings.SMTPHost+':'+settings.SMTPPort;
      ssl.Host := settings.SMTPHost;
      ssl.MaxLineAction := maException;
      ssl.Port := StrToInt(settings.SMTPPort);
      ssl.Options.TLSVersionMinimum := TIdOpenSSLVersion.TLSv1_3;
      ssl.Options.VerifyServerCertificate := false;
    end;
    sender.Connect;
    msg := TIdMessage.Create(Nil);
    try
      msg.Subject := subj;
      msg.Recipients.Add.Address := dest;
      msg.From.Text := settings.SMTPSender;
      msg.Body.Text := body;
      Logging.log('Send '+msg.MsgId+' to '+dest);
      sender.Send(msg);
    Finally
      msg.Free;
    End;
    sender.Disconnect;
  Finally
    sender.IOHandler.free;
    sender.Free;
  End;
end;

procedure sendSMS(settings : TFHIRServerSettings; Dest,Msg: String);
var
  client: TTwilioClient;
begin
  if Dest <> '' then
  begin
    try
      client := TTwilioClient.Create;
      try
        client.Account := settings.SMSAccount;
        if (client.Account <> '') and (settings.HostSms <> '') then
        begin
          client.Token := settings.SMSToken;
          client.From := settings.SMSFrom;
          client.dest := settings.HostSms;
          client.Body := Msg;
          client.send;
        end;
      finally
        client.Free;
      end;
    except
    end;
  end;
end;


end.



