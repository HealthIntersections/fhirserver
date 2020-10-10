unit FHIR.Server.Gui.Controller;

interface

uses
  Windows,
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Logging, FHIR.Support.Threads,
  FHIR.Server.Ini, FHIR.Server.Kernel.Tx;

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

  TFHIRServerController = class (TFslObject)
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
    procedure doLog(msg : String);
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
  logevent := doLog;
  consolelog := false;
end;

destructor TFHIRServerController.Destroy;
begin
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

procedure TFHIRServerController.doLog(msg : String);
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
  result.value['source'] := localFile('loinc-2.68.cache');
end;

function makeLang : TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.Create('r4');
  result.value['path'] := 'r4';
  result.value['version'] := 'r4';
  result.value['database'] := 'utg';
end;

function makeR4 : TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.Create('lang');
  result.value['type'] := 'lang';
  result.value['source'] := localFile('lang.txt');
end;

function makeUtgDB : TFHIRServerIniComplex;
begin
  result := TFHIRServerIniComplex.Create('utg');
  result.value['type'] := 'sqlite';
  result.value['database'] := localFile('fhirserver.db');
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
  Fini.web['admin'] := 'grahame@hl7.org';
  Fini.web['web'] := IncludeTrailingPathDelimiter(SystemTemp) + 'auth.ini';
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
  FController.setStatus(ssNotRunning);
  FController.FThread := nil;
  Free;
end;

end.
