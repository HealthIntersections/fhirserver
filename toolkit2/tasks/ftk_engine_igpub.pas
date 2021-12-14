unit ftk_engine_igpub;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_threads, fsl_fetcher, fsl_stream;

type
  TIgPublisherBuildEngineLineEvent = procedure(line : String; repl : boolean) of Object;

  { TIgPublisherBuildBaseEngine }

  TIgPublisherBuildBaseEngine = class (TFslThread)
  private
    FFolder : String;
    FStartingDate : TDateTime;
    FOnEmitLine: TIgPublisherBuildEngineLineEvent;
    FWantStop : boolean;
    procedure procEmitLine(sender : TFslExternalProcessThread; line : String; repl : boolean);
    procedure doFetcherProgress(sender : TObject; progress : integer);
  protected
    function GetSuccess: boolean; virtual;
  public
    function trackBuildTime : boolean; virtual;
    procedure terminate; virtual;

    property folder : String read FFolder write FFolder;
    property success : boolean read GetSuccess;
    property OnEmitLine : TIgPublisherBuildEngineLineEvent read FOnEmitLine write FOnEmitLine;
  end;

  { TIgPublisherUpdateEngine }

  TIgPublisherUpdateEngine = class (TIgPublisherBuildBaseEngine)
  public
    procedure execute; override;
  end;

  { TIgPublisherJekyllEngine }

  TIgPublisherJekyllEngine = class (TIgPublisherBuildBaseEngine)
  private
    Fcommand: String;
  public
    property command : String read Fcommand write Fcommand;
    procedure execute; override;
  end;

  { TIgPublisherCleanEngine }

  TIgPublisherCleanEngine = class (TIgPublisherBuildBaseEngine)
  public
    procedure execute; override;
  end;

  { TIgPublisherBuildEngine }

  TIgPublisherBuildEngine = class (TIgPublisherBuildBaseEngine)
  private
    FDevParams: String;
    FJavaCmd: String;
    FTxSrvr : String;
    FUrl: String;
    FVersion : String;
    FJarName : String;

    procedure checkJava;
    procedure checkInstallIgPublisher;
    procedure runBuild;
  protected
    function GetSuccess: boolean; override;
  public
    function trackBuildTime : boolean; override;
    property javaCmd : String read FJavaCmd write FJavaCmd;
    property devParams : String read FDevParams write FDevParams;
    property version : String read FVersion write FVersion;
    property url : String read FUrl write FUrl;
    property txSrvr : String read FTxSrvr write FTxSrvr;

    procedure execute; override;
    procedure terminate; override;

  end;

implementation

{ TIgPublisherJekyllEngine }


procedure TIgPublisherJekyllEngine.execute;
var
  p : TFslExternalProcessThread;
  fn : String;
begin
  FOnEmitLine('Run jekyll Directly: '+command, false);
  fn := tempFile('jekyll-command.bat');
  StringToFile(command+#13#10, fn, TEncoding.ASCII);

  p := TFslExternalProcessThread.create;
  try
    p.command := 'cmd';
    p.parameters.Add('/c');
    p.parameters.Add(fn);
    p.folder := FFolder;
    p.OnEmitLine := procEmitLine;
    p.Start;
    while p.Running do
      sleep(50);
  finally
    p.free;
  end;
end;

{ TIgPublisherCleanEngine }

procedure TIgPublisherCleanEngine.execute;
var
  p : TFslExternalProcessThread;
begin
  FOnEmitLine('clean files', false);

  p := TFslExternalProcessThread.create;
  try
    p.command := 'cmd';
    p.parameters.add('/c');
    p.parameters.add('clean.bat');
    p.folder := FFolder;
    p.OnEmitLine := procEmitLine;
    p.Start;
    while p.Running do
      sleep(50);
  finally
    p.free;
  end;
end;

{ TIgPublisherBuildBaseEngine }

procedure TIgPublisherBuildBaseEngine.procEmitLine(sender: TFslExternalProcessThread; line: String; repl : boolean);
begin
  FOnEmitLine(line, repl);
end;

procedure TIgPublisherBuildBaseEngine.doFetcherProgress(sender: TObject; progress: integer);
begin
  if FWantStop then
    abort;
end;

function TIgPublisherBuildBaseEngine.GetSuccess: boolean;
begin
  result := false;
end;

function TIgPublisherBuildBaseEngine.trackBuildTime: boolean;
begin
  result := false;
end;

procedure TIgPublisherBuildBaseEngine.terminate;
begin
  // nothing right now
end;

{ TIgPublisherUpdateEngine }

procedure TIgPublisherUpdateEngine.execute;
var
  p : TFslExternalProcessThread;
begin
  FOnEmitLine('git pull', false);

  p := TFslExternalProcessThread.create;
  try
    p.command := 'git';
    p.parameters.Add('pull');
    p.parameters.Add('--progress');
    p.folder := FFolder;
    p.OnEmitLine := procEmitLine;
    p.Start;
    while p.Running do
      sleep(50);
  finally
    p.free;
  end;
end;

{ TIgPublisherBuildEngine }

procedure TIgPublisherBuildEngine.checkJava;
var
  p : TFslExternalProcessThread;
  s : String;
  ok : boolean;
begin
  FOnEmitLine('Check Java Version', false);

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
        FOnEmitLine(s, false);
      end;
    end;
    if not ok then
      raise EFslException.create('Java is not installed or configured correctly');
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
    FOnEmitLine('Downloading jar from '+url, false);
    try
      fetcher := TInternetFetcher.create;
      try
        fetcher.URL := url;
        fetcher.OnProgress := doFetcherProgress;
        fetcher.Fetch;
        fetcher.Buffer.SaveToFileName(FJarName);
        if not FileExists(FJarName) then
          raise EFslException.create('Unable to download jar from '+url)
        else
          FOnEmitLine('Downloaded ('+DescribeBytes(fetcher.Buffer.Size)+' to '+FJarName+')', false);
      finally
        fetcher.Free;
      end;
    except
      on e : EAbort do
        FOnEmitLine('Cancelled', false);
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

function TIgPublisherBuildEngine.trackBuildTime: boolean;
begin
  Result := true;
end;

procedure TIgPublisherBuildEngine.runBuild;
var
  p : TFslExternalProcessThread;
  s : String;
begin
  FOnEmitLine('Run Build: java -jar '+FJarName+' -ig '+FFolder+' -tx '+FTxSrvr, false);

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
      p.parameters.add('-Dfile.encoding=UTF-8');
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
      FOnEmitLine(e.message, false);
  end;
end;

procedure TIgPublisherBuildEngine.terminate;
begin
  // java creates a double process (at least on windows) and we can't kill the actual operational process.
  // instead we ask it to die by creating this file
  StringToFile(TFslDateTime.makeLocal.toXML, FilePath([FFolder, 'ig-publisher.kill']), TEncoding.ASCII);
  FWantStop := true;
end;

end.

