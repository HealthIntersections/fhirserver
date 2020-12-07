unit gui_controller;

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
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_logging, fsl_threads,
  fhir_objects,
  fsl_npm_cache,
  fhir4_factory,
  fdb_manager, fdb_sqlite3,
  scim_server,
  server_config, web_base, web_server, database_installer;

type
  TFHIRServerStatus = (ssNotRunning, ssStarting, ssRunning, ssStopping);

  TFHIRServerController = class;

  TFHIRServerControllerThread = class (TFslThread)
  private
    FController : TFHIRServerController;
    FStopped : boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(controller : TFHIRServerController);
  end;

  TFHIRServerController = class (TLogListener)
  private
    FLock : TFslLock;
    FThread : TFHIRServerControllerThread;
    FIni : TFHIRServerConfigFile;
    FOnStatusChange: TNotifyEvent;
    FPendingStatus : TFHIRServerStatus;
    FStatus: TFHIRServerStatus;
    FAddress: String;
    FOnLog: TLogEvent;
    FMessagesIn : TStringList;
    FMessages : TStringList;
    FStats : TFHIRWebServerStats;

//    procedure setStatus(st : TFHIRServerStatus);
    procedure makeDB;
  protected
    procedure log(const msg : String); override;
    function transient : boolean; override;
    procedure logStart(s : String); override;
    procedure logContinue(s : String); override;
    procedure logFinish(s : String); override;
  public
    constructor Create(ini : TFHIRServerConfigFile);
    destructor Destroy; override;

    property OnStatusChange : TNotifyEvent read FOnStatusChange write FOnStatusChange;
    property OnLog : TLogEvent read FOnLog write FOnLog;

    property Status : TFHIRServerStatus read FStatus;
    property Address : String read FAddress;
    property Stats : TFHIRWebServerStats read FStats;

    procedure Initialise;
    procedure Ping; // give controller access to the primary thread

    procedure checkOk;
    procedure Start;
    procedure Stop;
  end;

implementation

{ TFHIRServerController }


constructor TFHIRServerController.Create(ini: TFHIRServerConfigFile);
begin
  inherited Create;
  FLock := TFslLock.create;
  FThread := nil;
  FIni := ini;
  FMessagesIn := TStringList.create;
  FMessages := TStringList.create;
  Logging.addListener(self);
  Logging.LogToConsole := false;
  MustBeUserMode := true;
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

function TFHIRServerController.transient: boolean;
begin
  result := true;
end;

procedure TFHIRServerController.logStart(s: String);
begin
  log(s);
end;

procedure TFHIRServerController.logContinue(s: String);
begin
  log('~'+s);
end;

procedure TFHIRServerController.logFinish(s: String);
begin
  log('~'+s);
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
        FThread.StopAndWait(50);
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

//procedure TFHIRServerController.setStatus(st: TFHIRServerStatus);
//begin
//  FLock.Lock;
//  try
//    FPendingStatus := st;
//  finally
//    FLock.Unlock;
//  end;
//end;
//
function localFile(s : String) : String;
begin
  result := Path([ExtractFilePath(ParamStr(0)), s]);
end;

function makeUcum : TFHIRServerConfigSection;
begin
//  result := TFHIRServerIniComplex.Create('ucum');
//  result.value['type'] := 'ucum';
//  result.value['source'] := localFile('ucum-essence.xml');
  result := nil;
end;

function makeLoinc : TFHIRServerConfigSection;
begin
//  result := TFHIRServerIniComplex.Create('loinc');
//  result.value['type'] := 'loinc';
//  result.value['source'] := localFile('loinc-2.65.cache');
  result := nil;
end;

function makeLang : TFHIRServerConfigSection;
begin
//  result := TFHIRServerIniComplex.Create('lang');
//  result.value['type'] := 'lang';
//  result.value['source'] := localFile('lang.txt');
  result := nil;
end;

function makeR4 : TFHIRServerConfigSection;
begin
//  result := TFHIRServerIniComplex.Create('r4');
//  result.value['path'] := '/r4';
//  result.value['version'] := 'r4';
//  result.value['database'] := 'utg';
  result := nil;
end;

function makeUtgDB : TFHIRServerConfigSection;
begin
//  result := TFHIRServerIniComplex.Create('sqlite');
//  result.value['type'] := 'sqlite';
//  result.value['database'] := localFile('fhir-server-gui.db');
  result := nil;
end;

procedure TFHIRServerController.makeDB;
//var
//  db : TFDBManager;
//  dbi : TFHIRDatabaseInstaller;
//  scim : TSCIMServer;
//  salt, un, pw, em, dr : String;
//  conn : TFDBConnection;
//  details : TFHIRServerConfigSection;
begin
//  // check that user account details are provided
//  salt := FIni.admin['scim-salt'].value;
//  if (salt = '') then
//    salt := NewGuidId;
//  dr := 'openid,fhirUser,profile,user/*.*';
//  un := 'xx';
//  pw := 'xx';
//  em := 'none@nowhere.org';
//
//  db := TFDBSQLiteManager.create('db', localFile('fhir-server-gui.db'), true);
//  try
//    Logging.log('Ínstall database');
//    scim := TSCIMServer.Create(db.Link, salt, 'localhost', dr, true);
//    try
//      conn := db.GetConnection('setup');
//      try
//        dbi := TFHIRDatabaseInstaller.create(conn, TFHIRFactoryR4.create, TTerminologyServerFactory.create(fhirVersionRelease4));
//        try
//          dbi.Bases.Add('http://healthintersections.com.au/fhir/argonaut');
//          dbi.Bases.Add('http://hl7.org/fhir');
//          dbi.Install(scim);
//        finally
//          dbi.free;
//        end;
//        scim.DefineAnonymousUser(conn);
//        scim.DefineAdminUser(conn, un, pw, em);
//        conn.Release;
//      except
//         on e:exception do
//         begin
//           Logging.log('Error: '+e.Message);
//           conn.Error(e);
//           recordStack(e);
//           raise;
//         end;
//      end;
//    finally
//      scim.Free;
//    end;
//  finally
//    db.free;
//  end;
end;

procedure TFHIRServerController.checkOk;
begin
  if not StringIsInteger16(FIni.web['http'].value) then
    raise EFslException.create('Port must be a Positive Integer between 0 and 65535');
//  if not folderExists(FIni.kernel['utg-folder']) then
//    raise EFslException.create('UTG Folder "'+FIni.kernel['utg-folder']+'" not found');

  // we're going to run r4, on /r4, on the nominated port
  // we're going to trample all over the ini file to make sure it's set up correct
//  FIni.kernel['mode'] := 'tx';
//  FIni.kernel['tx-versions'] := 'r4';
//  FIni.kernel['packages-r4'] := '';
//  FIni.terminologies.clear;
//  FIni.terminologies.Add('ucum', makeUcum);
//  FIni.terminologies.Add('loinc', makeLoinc);
//  FIni.terminologies.Add('lang', makeLang);
//  FIni.endpoints.clear;
//  FIni.endpoints.Add('r4', makeR4);
//  Fini.web['host'] := 'localhost';
//  Fini.admin['email'] := 'grahame@hl7.org';
//  Fini.web['clients'] := IncludeTrailingPathDelimiter(SystemTemp) + 'auth.ini';

  if not FileExists(localFile('fhir-server-gui.db')) then
    makeDB;
end;

procedure TFHIRServerController.Start;
begin
  if FThread <> nil then
    raise Exception.Create('Thread already exists');
  FThread := TFHIRServerControllerThread.Create(self);
  FThread.Start;
end;

procedure TFHIRServerController.Stop;
begin
  FThread.FStopped := true;
end;

{ TFHIRServerControllerThread }

constructor TFHIRServerControllerThread.Create(controller: TFHIRServerController);
begin
  FController := controller;
  inherited create;
end;

procedure TFHIRServerControllerThread.Execute;
//var
//  svc : TFHIRServiceTxServer;
//  s : String;
begin
//  SetThreadName('Gui Controller');
//  FStopped := false;
//  s := 'Starting';
//  FController.setStatus(ssStarting);
//  try
//    try
//      svc := TFHIRServiceTxServer.Create('utg', 'UTG Server', 'Starting Server, UTG = '+FController.FIni.service['utg-folder'].value, FController.FIni.Link);
//      try
//        svc.DebugMode := true;
//        if svc.CanStart then
//        begin
//          svc.postStart;
//          FController.FStats := svc.WebServer.Stats.link;
//          FController.setStatus(ssRunning);
//          while not FStopped do
//          begin
//            sleep(50);
//          end;
//        end;
//        s := 'Stopping';
//        Logging.log('Stopping');
//        FController.FStats.Free;
//        FController.FStats := nil;
//        FController.setStatus(ssStopping);
//        svc.Stop('User Command');
//        svc.DoStop;
//      finally
//        svc.Free;
//      end;
//      Logging.log('Stopped');
//    except
//      on e : exception do
//      begin
//        Logging.log('Exception '+s+': '+e.message);
//      end;
//    end;
//  finally
//    FController.setStatus(ssNotRunning);
//  end;
//  closeThread;
end;

end.
