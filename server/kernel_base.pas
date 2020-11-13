unit kernel_base;

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
  SysUtils, StrUtils, Classes, IniFiles,
  fsl_base, fsl_utilities, fsl_logging,
  {$IFDEF WINDOWS} Windows, fsl_service_win, {$ELSE} fsl_service, {$ENDIF}
  fdb_manager,
  tx_manager,
  server_ini, telnet_server, utilities, web_source,
  {$IFNDEF NO_JS}server_javascript, {$ENDIF}
  webserver;

type

  TFHIRServiceBase = class (TSystemService)
  private
    Fcallback: TInstallerCallback;
    FLoadStore : boolean;
    FTelnet : TFHIRTelnetServer;
    FIni : TFHIRServerIniFile;
    FSettings : TFHIRServerSettings;
    FWebServer : TFhirWebServer;
    FProgress : integer;
    FProgressName : string;

    procedure StartWebServer;
    procedure StopWebServer;
  protected
    FStartTime : cardinal;

    procedure cb(i: integer; s: WideString);
    procedure fetchProgress(sender: TObject; progess: integer);
    procedure fetchProgress2(sender: TObject; pct: integer; done: boolean; desc: String);
    procedure finishProgress(status: String);
    procedure resetProgress(name: String);

    function initialise : boolean; virtual; // called while starting service;
    function setup : boolean; virtual; // called once service is stated to have started
    procedure registerEndPoints; virtual;  // this is where subclasses actually add all their functionality
    {$IFNDEF NO_JS}
    procedure registerJs(sender: TObject; js: TJsHost); virtual;
    {$ENDIF}
    procedure closeDown; virtual;
    function WantActive : boolean; virtual;
    function WantThreads : boolean; virtual;
  public
    constructor Create(const ASystemName, ADisplayName, Welcome : String; ini : TFHIRServerIniFile);
    destructor Destroy; override;

    function CanStart : boolean; Override;
    procedure postStart; override;
    procedure DoStop; Override;
    procedure dump; override;

    property callback : TInstallerCallback read Fcallback write Fcallback;
    property loadStore : boolean read FLoadStore write FLoadStore;
    property Telnet : TFHIRTelnetServer read FTelnet;
    property Ini : TFHIRServerIniFile read FIni;
    property Settings : TFHIRServerSettings read FSettings;
    property WebServer : TFhirWebServer read FWebServer;

    function command(cmd : String) : boolean; virtual;
  end;

  TFHIRServiceDataStore = class (TFHIRServiceBase)
  private
    FDatabases : TFslMap<TFslDBManager>;
    FTerminologies : TCommonTerminologies;
    procedure ConnectToDatabases;
    procedure CloseDatabases;
    procedure LoadTerminologies;
    procedure UnloadTerminologies;
  protected
    function initialise : boolean; override;
    function setup : boolean; override;
    procedure closeDown; override;
  public
    destructor Destroy; override;
    property Databases : TFslMap<TFslDBManager> read FDatabases;
    property Terminologies : TCommonTerminologies read FTerminologies;
  end;

implementation

constructor TFHIRServiceBase.Create(const ASystemName, ADisplayName, Welcome: String; ini: TFHIRServerIniFile);
begin
  FStartTime := GetTickCount;
  inherited create(ASystemName, ADisplayName);
  FTelnet := TFHIRTelnetServer.Create(44123, Welcome);
  FIni := ini;
  FTelnet.Password := FIni.web['telnet-password'];

  FSettings := TFHIRServerSettings.Create;
  FSettings.ForLoad := not hasCommandLineParam('noload');
  FSettings.load(FIni);
end;

destructor TFHIRServiceBase.Destroy;
begin
  FIni.Free;
  FSettings.Free;
  FTelnet.Free;
  inherited;
end;

function TFHIRServiceBase.CanStart: boolean;
begin
  Logging.log('Run Number '+inttostr(Settings.RunNumber));
  try
    result := initialise;
  except
    on e : Exception do
    begin
      Logging.log('Error starting: '+E.ClassName+ ': ' + E.Message+#13#10#13#10+ExceptionStack(e));
      raise;
    end;
  end;
end;

procedure TFHIRServiceBase.postStart;
begin
  if setup then
    StartWebServer();
  getReport('|', true);
  Logging.log('started ('+inttostr((GetTickCount - FStartTime) div 1000)+'secs)');
  Logging.Starting := false;
end;

procedure TFHIRServiceBase.DoStop;
begin
  try
    Logging.log('stopping: '+StopReason);
    Logging.log('stop web server');
    StopWebServer;
    Logging.log('closing');
    closeDown;
    Logging.log('stopped');
  except
    on e : Exception do
      Logging.log('Exception stopping ('+E.ClassName + '): ' + E.Message+#13#10#13#10+ExceptionStack(e));
  end;
end;

procedure TFHIRServiceBase.StartWebServer;
begin
  FWebServer := TFhirWebServer.create(Settings.Link, Telnet.Link, DisplayName);
  {$IFNDEF NO_JS}
  FWebServer.OnRegisterJs := registerJs;
  {$ENDIF}
  FWebServer.loadConfiguration(Ini);
  if FolderExists('c:\work\fhirserver\server\webn') then
  begin
    Logging.log('Web source from '+Ini.web['folder']);
    FWebServer.SourceProvider := TFHIRWebServerSourceFolderProvider.Create('c:\work\fhiserver\server\web')
  end
  else if FileExists(partnerFile('fhirserver.web')) then
  begin
    Logging.log('Web source from '+partnerFile('fhirserver.web'));
    FWebServer.SourceProvider := TFHIRWebServerSourceZipProvider.Create(partnerFile('fhirserver.web'))
  end
  else
    raise Exception.Create('Unable to find web source');

  registerEndPoints;

  FWebServer.Start(WantActive, WantThreads);
end;

procedure TFHIRServiceBase.StopWebServer;
begin
  if FWebServer <> nil then
  begin
    FWebServer.Stop;
    FWebServer.free;
  end;
end;

procedure TFHIRServiceBase.dump;
begin
  // nothing?
end;

function TFHIRServiceBase.command(cmd: String): boolean;
begin
  result := false;
end;

function TFHIRServiceBase.WantActive: boolean;
begin
  result := false;
end;

function TFHIRServiceBase.WantThreads: boolean;
begin
  result := false;
end;

function TFHIRServiceBase.initialise: boolean;
begin
  result := false;
end;

function TFHIRServiceBase.setup: boolean;
begin
  result := false;
end;

procedure TFHIRServiceBase.registerEndPoints;
begin
  // nothing
end;

procedure TFHIRServiceBase.closeDown;
begin
  // nothing
end;

{$IFNDEF NO_JS}
procedure TFHIRServiceBase.registerJs(sender: TObject; js: TJsHost);
begin
  // nothing
end;
{$ENDIF}

procedure TFHIRServiceBase.cb(i: integer; s: WideString);
begin
  if Assigned(Fcallback) then
    Fcallback(i, s);
end;

procedure TFHIRServiceBase.resetProgress(name: String);
begin
  FProgress := 0;
  FProgressName := name;
  Logging.start(name+' ');
end;

procedure TFHIRServiceBase.fetchProgress(sender: TObject; progess: integer);
begin
  if Assigned(Fcallback) then
    callback(progess, FProgressName)
  else
  begin
    if progess > FProgress then
    begin
      Logging.continue('.');
      FProgress := FProgress + 5;
    end;
  end;
end;

procedure TFHIRServiceBase.fetchProgress2(sender: TObject; pct: integer; done: boolean; desc: String);
begin
  if Assigned(Fcallback) then
    callback(pct, FProgressName)
  else
  begin
    if pct > FProgress then
    begin
      Logging.continue('.');
      FProgress := FProgress + 5;
    end;
  end;
end;

procedure TFHIRServiceBase.finishProgress(status: String);
begin
  if not Assigned(Fcallback) then
    Logging.finish(' '+status);
end;


{ TFHIRServiceDataStore }

destructor TFHIRServiceDataStore.Destroy;
begin
  FDatabases.Free;
  inherited;
end;

function TFHIRServiceDataStore.initialise: boolean;
begin
  FDatabases := TFslMap<TFslDBManager>.create('fhir.svc');
  ConnectToDatabases;
  result := true;
end;

function TFHIRServiceDataStore.setup: boolean;
begin
  if FTerminologies = nil then
    LoadTerminologies;
  result := true;
end;

procedure TFHIRServiceDataStore.closeDown;
begin
  UnloadTerminologies;
  CloseDatabases;
end;

Procedure TFHIRServiceDataStore.ConnectToDatabases();
var
  s : String;
  details : TFHIRServerIniComplex;
begin
  Logging.log('Load Databases. Config file = '+Ini.FileName);
  for s in Ini.databases.keys do
  begin
    details := Ini.databases[s];
    FDatabases.Add(s, connectToDatabase(s, details));
  end;
  Logging.log('Databases Loaded');
end;

procedure TFHIRServiceDataStore.CloseDatabases;
begin
  if FDatabases <> nil then
    FDatabases.Clear;
end;


procedure TFHIRServiceDataStore.LoadTerminologies;
begin
  FTerminologies := TCommonTerminologies.Create(Settings.link);
  FTerminologies.load(Ini, FDatabases, false);
end;

procedure TFHIRServiceDataStore.UnloadTerminologies;
begin
  FTerminologies.Free;
  FTerminologies := nil;
end;


end.
