unit ftk_engine_igpub;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_threads, fsl_fetcher, fsl_stream;

type
  TIgPublisherBuildEngineLineEvent = procedure(line : String) of Object;

  { TIgPublisherBuildEngine }

  TIgPublisherBuildEngine = class (TFslThread)
  private
    FDevParams: String;
    FFolder : String;
    FJavaCmd: String;
    FOnEmitLine: TIgPublisherBuildEngineLineEvent;
    FTxSrvr : String;
    FUrl: String;
    FVersion : String;
    FJarName : String;
    FWantStop : boolean;
    FStartingDate : TDateTime;

    procedure checkJava;
    procedure checkInstallIgPublisher;
    function GetSuccess: boolean;
    procedure runBuild;
    procedure procEmitLine(sender : TFslExternalProcessThread; line : String);
    procedure doFetcherProgress(sender : TObject; progress : integer);
  public
    destructor Destroy; override;
    property javaCmd : String read FJavaCmd write FJavaCmd;
    property devParams : String read FDevParams write FDevParams;
    property folder : String read FFolder write FFolder;
    property version : String read FVersion write FVersion;
    property url : String read FUrl write FUrl;
    property txSrvr : String read FTxSrvr write FTxSrvr;

    procedure execute; override;
    procedure Terminate;

    property success : boolean read GetSuccess;
    property OnEmitLine : TIgPublisherBuildEngineLineEvent read FOnEmitLine write FOnEmitLine;
  end;

implementation

{ TIgPublisherBuildEngine }

destructor TIgPublisherBuildEngine.Destroy;
begin
  inherited Destroy;
end;

procedure TIgPublisherBuildEngine.checkJava;
var
  p : TFslExternalProcessThread;
  s : String;
  ok : boolean;
begin
  FOnEmitLine('Check Java Version');

  p := TFslExternalProcessThread.create;
  try
    p.command := FJavaCmd;
    p.parameters.Add('-version');
    p.folder := GetTempDir;
    p.Start;
    while p.Running do
      sleep(50);

    ok := false;
    for s in p.lines do
    begin
      if s.ToLower.StartsWith('java version') then
      begin
        ok := true;
        FOnEmitLine(s);
      end;
    end;
    if not ok then
      raise Exception.create('Java is not installed or configured correctly');
  finally
    p.free;
  end;
end;

procedure TIgPublisherBuildEngine.checkInstallIgPublisher;
var
  folder : String;
  fetcher : TInternetFetcher;
begin
  folder := FilePath(['[tmp]', 'ig-pub']);
  if not FolderExists(folder) then
    ForceFolder(folder);
  FJarName := FilePath([folder, 'ig-publisher-'+version+'.jar']);
  if not FileExists(FJarName) then
  begin
    FOnEmitLine('Downloading jar from '+url);
    try
      fetcher := TInternetFetcher.create;
      try
        fetcher.URL := url;
        fetcher.OnProgress := doFetcherProgress;
        fetcher.Fetch;
        fetcher.Buffer.SaveToFileName(FJarName);
        if not FileExists(FJarName) then
          raise Exception.create('Unable to download jar from '+url)
        else
          FOnEmitLine('Downloaded ('+DescribeBytes(fetcher.Buffer.Size)+' to '+FJarName+')');
      finally
        fetcher.Free;
      end;
    except
      on e : EAbort do
        FOnEmitLine('Cancelled');
    end;
  end;
end;

function TIgPublisherBuildEngine.GetSuccess: boolean;
begin
  if FileExists(FilePath([FFolder, 'output', 'qa.json'])) then
    result := FileGetModified(FilePath([FFolder, 'output', 'qa.json'])) <> FStartingDate
  else
    result := false;
end;

procedure TIgPublisherBuildEngine.runBuild;
var
  p : TFslExternalProcessThread;
  s : String;
begin
  FOnEmitLine('Run Build: java -jar '+FJarName+' -ig '+FFolder+' -tx '+FTxSrvr);

  p := TFslExternalProcessThread.create;
  try
    p.command := FJavaCmd;
    if (url = '#dev') then
    begin
      for s in FDevParams.Split([' ']) do
        p.parameters.add(s);
    end
    else
    begin
      p.parameters.add('-jar');
      p.parameters.add(FJarName);
    end;
    p.parameters.add('-ig');
    p.parameters.add(FFolder);
    p.parameters.add('-tx');
    p.parameters.add(FTxSrvr);
    p.folder := GetTempDir;
    p.OnEmitLine := procEmitLine;
    p.Start;
    while p.Running do
      sleep(50);
  finally
    p.free;
  end;
end;

procedure TIgPublisherBuildEngine.procEmitLine(sender: TFslExternalProcessThread; line: String);
begin
  FOnEmitLine(line);
end;

procedure TIgPublisherBuildEngine.doFetcherProgress(sender: TObject; progress: integer);
begin
  if FWantStop then
    abort;
end;

procedure TIgPublisherBuildEngine.execute;
begin
  try
    if FileExists(FilePath([FFolder, 'output', 'qa.json'])) then
      FStartingDate := FileGetModified(FilePath([FFolder, 'output', 'qa.json']))
    else
      FStartingDate := 0;
    if FileExists(FilePath([FFolder, 'ig-publisher.kill'])) then
      DeleteFile(FilePath([FFolder, 'ig-publisher.kill']));
    if not FWantStop then
      checkJava;
    if not FWantStop and (url <> '#dev') then
      checkInstallIgPublisher;
    if not FWantStop then
      runBuild;
  except
    on e : Exception do
      FOnEmitLine(e.message);
  end;
end;

procedure TIgPublisherBuildEngine.Terminate;
begin
  // java creates a double process (at least on windows) and we can't kill the actual operational process.
  // instead we ask it to die by creating this file
  StringToFile(TFslDateTime.makeLocal.toXML, FilePath([FFolder, 'ig-publisher.kill']), TEncoding.ASCII);
  FWantStop := true;
end;

end.

