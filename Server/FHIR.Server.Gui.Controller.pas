unit FHIR.Server.Gui.Controller;

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
  Windows,
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Logging, FHIR.Support.Threads,
  FHIR.Base.Objects,
  FHIR.R4.Factory,
  FHIR.Database.Manager, FHIR.Database.SQLite,
  FHIR.Scim.Server,
  FHIR.Server.Ini, FHIR.Server.Kernel.Tx, FHIR.Server.DBInstaller;

type
  TFHIRServerStatus = (ssNotRunning, ssStarting, ssRunning, ssStopping);

  TFHIRServerController = class;

  TFHIRServerControllerThread = class (TFslThread)
  private
    FController : TFHIRServerController;
    FStopped : boolean;
  protected
    procedure execute; override;
  public
    constructor Create(controller : TFHIRServerController);
  end;

  TFHIRServerController = class (TLogListener)
  private
    FLock : TFslLock;
    FThread : TFHIRServerControllerThread;
    FIni : TFHIRServerIniFile;
    FOnStatusChange: TNotifyEvent;
    FPendingStatus : TFHIRServerStatus;
    FStatus: TFHIRServerStatus;
    FAddress: String;
    FOnLog: TLogEvent;
    FMessagesIn : TStringList;
    FMessages : TStringList;

    procedure setStatus(st : TFHIRServerStatus);
    procedure log(const msg : String); override;
    procedure makeDB;
  public
    constructor Create(ini : TFHIRServerIniFile);
    destructor Destroy; override;

    property OnStatusChange : TNotifyEvent read FOnStatusChange write FOnStatusChange;
    property OnLog : TLogEvent read FOnLog write FOnLog;

    property Status : TFHIRServerStatus read FStatus;
    property Address : String read FAddress;

    procedure Initialise;
    procedure Ping; // give controller access to the primary thread

    procedure checkOk;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TFHIRServerController }

constructor TFHIRServerController.Create(ini: TFHIRServerIniFile);
begin
  inherited Create;
  FLock := TFslLock.create;
  FThread := nil;
  FIni := ini;
  FMessagesIn := TStringList.create;
  FMessages := TStringList.create;
  Logging.addListener(self);
  Logging.LogToConsole := false;
end;

destructor TFHIRServerController.Destroy;
begin
  Logging.removeListener(self);
  FMessages.Free;
  FMessagesIn.Free;
  FIni.Free;
  FLock.Free;
  inherited;
end;

procedure TFHIRServerController.Initialise;
begin
  OnStatusChange(self);
end;

procedure TFHIRServerController.log(const msg : String);
begin
  FLock.Lock;
  try
    FMessagesIn.Add(msg);
  finally
    FLock.Unlock;
  end;
end;

procedure TFHIRServerController.Ping;
var
  st : boolean;
  s : String;
begin
  st := false;
  FLock.Lock;
  try
    if FPendingStatus <> FStatus then
    begin
      FStatus := FPendingStatus;
      st := true;
    end;
    if FStatus = ssNotRunning then
    begin
      if FThread <> nil then
      begin
        FThread.CloseOut;
        FThread.Free;
        FThread := nil;
      end;
    end;
    FMessages.Assign(FMessagesIn);
    FMessagesIn.Clear;
  finally
    FLock.Unlock;
  end;
  if (st) then
    FOnStatusChange(self);
  for s in FMessages do
    OnLog(s);
  FMessages.Clear;
end;

procedure TFHIRServerController.setStatus(st: TFHIRServerStatus);
begin
  FLock.Lock;
  try
    FPendingStatus := st;
  finally
    FLock.Unlock;
  end;
end;

function localFile(s : String) : String;
begin
  result := Path([ExtractFilePath(ParamStr(0)), s]);
end;

function makeUcum : TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.Create('ucum');
  result.value['type'] := 'ucum';
  result.value['source'] := localFile('ucum-essence.xml');
end;

function makeLoinc : TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.Create('loinc');
  result.value['type'] := 'loinc';
  result.value['source'] := localFile('loinc-2.65.cache');
end;

function makeLang : TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.Create('lang');
  result.value['type'] := 'lang';
  result.value['source'] := localFile('lang.txt');
end;

function makeR4 : TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.Create('r4');
  result.value['path'] := 'r4';
  result.value['version'] := 'r4';
  result.value['database'] := 'utg';
end;

function makeUtgDB : TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.Create('sqlite');
  result.value['type'] := 'sqlite';
  result.value['database'] := localFile('fhirserver.db');
end;

procedure TFHIRServerController.makeDB;
var
  db : TFslDBManager;
  dbi : TFHIRDatabaseInstaller;
  scim : TSCIMServer;
  salt, un, pw, em, dr : String;
  conn : TFslDBConnection;
  details : TFHIRServerIniComplex;
begin
  // check that user account details are provided
  salt := FIni.admin['scim-salt'];
  if (salt = '') then
    salt := NewGuidId;
  dr := 'openid,fhirUser,profile,user/*.*';
  un := 'xx';
  pw := 'xx';
  em := 'none@nowhere.org';

  db := TFslDBSQLiteManager.create('db', localFile('fhirserver.db'), true);
  try
    Logging.log('Ínstall database');
    scim := TSCIMServer.Create(db.Link, salt, 'localhost', dr, true);
    try
      conn := db.GetConnection('setup');
      try
        dbi := TFHIRDatabaseInstaller.create(conn, TFHIRFactoryR4.create, TTerminologyServerFactory.create(fhirVersionRelease4));
        try
          dbi.Bases.Add('http://healthintersections.com.au/fhir/argonaut');
          dbi.Bases.Add('http://hl7.org/fhir');
          dbi.Install(scim);
        finally
          dbi.free;
        end;
        scim.DefineAnonymousUser(conn);
        scim.DefineAdminUser(conn, un, pw, em);
        conn.Release;
      except
         on e:exception do
         begin
           Logging.log('Error: '+e.Message);
           conn.Error(e);
           recordStack(e);
           raise;
         end;
      end;
    finally
      scim.Free;
    end;
  finally
    db.free;
  end;
end;

procedure TFHIRServerController.checkOk;
begin
  if not StringIsInteger16(FIni.web['http']) then
    raise EFslException.create('Port must be a Positive Integer between 0 and 65535');
  if not folderExists(FIni.kernel['utg-folder']) then
    raise EFslException.create('UTG Folder "'+FIni.kernel['utg-folder']+'" not found');

  // we're going to run r4, on /r4, on the nominated port
  // we're going to trample all over the ini file to make sure it's set up correct
  FIni.kernel['mode'] := 'tx';
  FIni.kernel['tx-versions'] := 'r4';
  FIni.kernel['packages-r4'] := '';
  FIni.databases.Clear;
  FIni.databases.Add('utg', makeUtgDB);
  FIni.terminologies.clear;
  FIni.terminologies.Add('ucum', makeUcum);
  FIni.terminologies.Add('loinc', makeLoinc);
  FIni.terminologies.Add('lang', makeLang);
  FIni.endpoints.clear;
  FIni.endpoints.Add('r4', makeR4);
  Fini.web['host'] := 'localhost';
  Fini.admin['email'] := 'grahame@hl7.org';
  Fini.web['clients'] := IncludeTrailingPathDelimiter(SystemTemp) + 'auth.ini';

  if not FileExists(localFile('fhirserver.db')) then
    makeDB;
end;

procedure TFHIRServerController.Start;
begin
  if FThread <> nil then
    raise Exception.Create('Thread already exists');
  FThread := TFHIRServerControllerThread.Create(self);
  FThread.Open;
end;

procedure TFHIRServerController.Stop;
begin

end;

{ TFHIRServerControllerThread }

constructor TFHIRServerControllerThread.Create(controller: TFHIRServerController);
begin
  FController := controller;
  inherited create;
end;

procedure TFHIRServerControllerThread.execute;
var
  svc : TFHIRServiceTxServer;
begin
  FStopped := false;
  FController.setStatus(ssStarting);
  try
    try
      svc := TFHIRServiceTxServer.Create('utg', 'UTG Server', 'Starting Server, UTG = '+FController.FIni.kernel['utg-folder'], FController.FIni.Link);
      try
        svc.DebugMode := true;
        if svc.CanStart then
        begin
          svc.postStart;
          FController.setStatus(ssRunning);
          while not FStopped do
          begin
            Sleep(50);
          end;
        end;
        FController.setStatus(ssStopping);
      finally
        svc.Free;
      end;
    except
    end;
  finally
    FController.setStatus(ssNotRunning);
  end;
end;

end.
