unit FHIR.Tools.ValidationWrapper;

interface

uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  SysUtils, Classes,
  FHIR.Support.Objects, FHIR.Support.System,
  FHIR.Cache.PackageManager;

type
  TOutputLineEvent = procedure (Sender : TObject; line : String) of Object;
  TArg<T> = reference to procedure(const Arg: T);

  TFHIRValidationWrapper = class (TFslObject)
  private
    // context
    FCache : TFHIRPackageManager;
    FJarPath : String;

    // settings
    FSource: String;
    FNative: boolean;
    FPackages: TStringList;
    FOthers: TStringList;
    FVersion: String;
    FOnOutput: TOutputLineEvent;
    FTxServer : String;
    Fmap: String;
    FDest: String;
    FProfile: String;

    procedure executeCommand(cmd : String; CallBack: TArg<PAnsiChar>);
  public
    Constructor Create(Cache : TFHIRPackageManager);
    Destructor Destroy; override;
    function link : TFHIRValidationWrapper;

    function validateCmd: string;
    function transformCmd: string;

    procedure validate;
    procedure transform;

    property version : String read FVersion write FVersion;
    property Packages : TStringList read FPackages;
    property Others : TStringList read FOthers;
    property native : boolean read FNative write FNative;
    property source : String read FSource write FSource;
    property map : String read Fmap write FMap;
    property profile : String read FProfile write FProfile;
    property dest : String read FDest write FDest;
    Property txServer : String read FTxServer write FTxServer;

    property OnOutput : TOutputLineEvent read FOnOutput write FOnOutput;
  end;

implementation

{ TFHIRValidationWrapper }

constructor TFHIRValidationWrapper.Create(Cache: TFHIRPackageManager);
var
  ts : TStringList;
begin
  inherited create;
  FCache := cache;
  FPackages := TStringList.create;
  FOthers := TStringList.create;

  ts := TStringList.Create;
  try
    FCache.ListPackageVersions('hl7.FHIR.Version.Validator', ts);
    if ts.Count > 0 then
      FJarPath := path([FCache.Folder, 'hl7.FHIR.Version.Validator-'+ts[ts.Count-1], 'bin', 'org.hl7.fhir.validator.jar']);
  finally
    ts.Free;
  end;
end;

destructor TFHIRValidationWrapper.Destroy;
begin
  FPackages.Free;
  FOthers.Free;
  FCache.free;
  inherited;
end;

  // OSX: see http://www.fmxexpress.com/read-and-interact-with-a-command-line-pipe-in-delphi-xe7-firemonkey-on-mac-osx/ ?
procedure TFHIRValidationWrapper.validate;
begin
  executeCommand(validateCmd,
    procedure(const Line: PAnsiChar)
    begin
      FOnOutput(self, line);
    end);
end;

{$IFDEF MSWINDOWS}
procedure TFHIRValidationWrapper.executeCommand(cmd : String; CallBack: TArg<PAnsiChar>);
const
  CReadBuffer = 2400;
var
  saSecurity: TSecurityAttributes;
  hRead: THandle;
  hWrite: THandle;
  suiStartup: TStartupInfo;
  piProcess: TProcessInformation;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dRead: DWORD;
  dRunning: DWORD;
  dAvailable: DWORD;
begin
  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := true;
  saSecurity.lpSecurityDescriptor := nil;
  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
    try
      FillChar(suiStartup, SizeOf(TStartupInfo), #0);
      suiStartup.cb := SizeOf(TStartupInfo);
      suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;
      if CreateProcess(nil, PChar(cmd), @saSecurity, @saSecurity, true, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup,
        piProcess) then
        try
          repeat
            dRunning := WaitForSingleObject(piProcess.hProcess, 100);
            PeekNamedPipe(hRead, nil, 0, nil, @dAvailable, nil);
            if (dAvailable > 0) then
              repeat
                dRead := 0;
                ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
                pBuffer[dRead] := #0;
                OemToCharA(pBuffer, dBuffer);
                CallBack(dBuffer);
              until (dRead < CReadBuffer);
          until (dRunning <> WAIT_TIMEOUT);
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
    finally
      CloseHandle(hRead);
      CloseHandle(hWrite);
    end;
end;
{$ELSE}
procedure TFHIRValidationWrapper.executeCommand(cmd : String; CallBack: TArg<PAnsiChar>);
begin
  raise Exception.Create('not done yet');
end;
{$ENDIF}


function TFHIRValidationWrapper.link: TFHIRValidationWrapper;
begin
  result := TFHIRValidationWrapper(inherited link);
end;

procedure TFHIRValidationWrapper.transform;
begin
  executeCommand(validateCmd,
    procedure(const Line: PAnsiChar)
    begin
      FOnOutput(self, line);
    end);
end;

function TFHIRValidationWrapper.validateCmd: string;
var
  i : integer;
  s : String;
begin
  if FJarPath = '' then
    exit('Error: validator not installed');

  result := 'java -jar "'+FJarPath+'"';
  if FSource <> '' then
    result := result+' '+source;
  result := result + ' -defn hl7.fhir.core-'+FVersion;

  if FProfile <> '' then
    result := result+' -profile '+FProfile;

  if FNative then
    result := result +' -native';

  if FTxServer <> '' then
    result := result + ' -tx '+FtxServer;

  for s in FPackages do
    result := result +' -ig '+s;

  for s in FOthers do
    result := result +' -ig '+s;

  if FDest <> '' then
    result := result+' -output '+FDest;
end;

function TFHIRValidationWrapper.transformCmd: string;
var
  i : integer;
  s : String;
begin
  if FJarPath = '' then
    exit('Error: validator not installed');
  if FMap = '' then
    exit('Error: map not specified');

  result := 'java -jar "'+FJarPath+'"';
  if FSource <> '' then
    result := result+' '+source;

  if FMap <> '' then
    result := result+' -transform '+FMap+' -defn hl7.fhir.core-'+FVersion;

  if FTxServer <> '' then
    result := result + ' -tx '+FtxServer;

  for s in FPackages do
    result := result +' -ig '+s;

  for s in FOthers do
    result := result +' -ig '+s;

  if FDest <> '' then
    result := result+' -output '+FDest;
end;

end.
