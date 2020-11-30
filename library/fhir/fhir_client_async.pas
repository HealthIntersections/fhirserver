unit fhir_client_async;

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

{$i fhir.inc}

interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream, fsl_json,
  fhir_objects,  fhir_client, fhir_client_http, fhir_client_threaded, fhir_parser, fhir_utilities, fhir_factory, fhir_common;

const
  WAIT_CYCLE_LENGTH = 10;
  MAX_RETRY_COUNT = 5;

type
  TFHIRClientAsyncContextState = (
    asyncReady,
    asyncInitialQueryInProgress,
    asyncWaiting,
    asyncDoPing,
    asyncPinging,
    asyncFailed,
    asyncDownload,
    asyncDownloading,
    asyncFinished);

  TDownloadFile = class (TFslObject)
  private
    FUrl : String;
    FResourceType : String;
    FSize : integer;
    FCount: integer;
    FError: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(resourceType, url : String);

    function Link : TDownloadFile; overload;

    property url : String read FUrl write FUrl;
    property resourceType : String read FResourceType write FResourceType;
    property count : integer read FCount write FCount;
    property size : integer read FSize write FSize;
    property error : String read FError write FError;
  end;

  TFHIRClientAsyncTask = class (TFslObject)
  private
    FClient : TFhirClientV;
    FFiles : TFslList<TDownloadFile>;
    FFolder: String;
    FTypes: TStringList;
    FQuery: string;
    FFormat : TFHIRFormat;
    FSince: TFslDateTime;
    FLog : String;
    FStart : TDateTime;
    FStatus : TFHIRClientAsyncContextState;
    FTaskLocation : String;
    FLastStatus : TDateTime;
    FError : String;
    FFactory : TFHIRFactory;

    function getNextFile : TDownloadFile;
    function waitingCount : integer;


    procedure log(s : String);
    procedure makeInitialRequest;
    procedure checkDelay;
    procedure doPing;
    procedure doDownload;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create(client : TFhirClientV; factory : TFHIRFactory);
    destructor Destroy; override;

    // setup
    property query : string read FQuery write FQuery;
    property format : TFHIRFormat read FFormat write FFormat;
    property folder : String read FFolder write FFolder;
    property since : TFslDateTime read FSince write FSince;
    property types : TStringList read FTypes;

    // operation
    function logText : String;
    function status : String;
    procedure next;
    function Finished : boolean;
    function Summary : String;
  end;


implementation

{ TFHIRClientAsyncTask }

function TFHIRClientAsyncTask.waitingCount: integer;
var
  t : TDownloadFile;
begin
  result := 0;
  for t in FFiles do
    if (t.size = 0) and (t.count < MAX_RETRY_COUNT) then
      inc(result);
end;

procedure TFHIRClientAsyncTask.checkDelay;
begin
  if now - FLastStatus > WAIT_CYCLE_LENGTH * DATETIME_SECOND_ONE then
  begin
    FStatus := asyncDoPing;
    FLastStatus := now;
  end;
end;

constructor TFHIRClientAsyncTask.create(client: TFhirClientV; factory : TFHIRFactory);
begin
  inherited Create;
  FClient := client;
  FLog := '';
  FStart := now;
  FFactory := factory;
  FStatus := asyncReady;
  FFiles := TFslList<TDownloadFile>.create;
  FTypes := TStringList.create;
  log('Initialised at '+FormatDateTime('c', now));
end;

destructor TFHIRClientAsyncTask.Destroy;
begin
  FFiles.Free;
  FTypes.Free;
  FClient.Free;
  FFactory.Free;
  inherited;
end;

procedure TFHIRClientAsyncTask.doDownload;
var
  df : TDownloadFile;
  headers : THTTPHeaders;
  buf : TFslBuffer;
begin
  FStatus := asyncDownloading;
  df := getNextFile;
  if df = nil then
    exit;
  try
    df.count := df.count + 1;
    buf := FClient.customGet(df.url, headers);
    try
      buf.SaveToFileName(Path([FFolder, df.resourceType+'.json']));
    finally
      buf.free;
    end;
    FError := '';
    df.size := buf.Size;
    if waitingCount = 0 then
      FStatus := asyncFinished
    else
      FStatus := asyncDownload;
  except
    on e : Exception do
    begin
      df.error := e.Message;
      FError := e.Message;
    end;
  end;
end;

procedure TFHIRClientAsyncTask.doPing;
var
  headers : THTTPHeaders;
  buf : TFslBuffer;
  p : TFHIRParser;
  r : TFHIRResourceV;
  w : TFhirOperationOutcomeW;
  json, o : TJsonObject;
  i : TJsonNode;
begin
  FStatus := asyncPinging;
  headers.accept := 'application/fhir+json';
  headers.prefer := '';
  buf := FClient.customGet(FTaskLocation, headers);
  try
    case FClient.LastStatus of
      202 :
        begin
          FStatus := asyncWaiting;
          FLastStatus := now;
          log('Still Waiting...: '+headers.progress);
        end;
      200 :
        begin
          log('Ready to download');
          json := TJSONParser.Parse(buf.AsBytes);
          try
            // check secure
            // store the transaction time somewhere
            FFiles.clear;
            for i in json.forceArr['output'] do
            begin
              o := i as TJsonObject;
              FFiles.Add(TDownloadFile.Create(o.str['type'], o.str['url']{.Substring(FClient.address.Length+1))}));
            end;
            FStatus := asyncDownload;
          finally
            json.Free;
          end;
        end
      else if FClient.LastStatus >= 500 then
      begin
        FStatus := asyncFailed;
        FError := FClient.LastStatusMsg;
        if buf.Size > 0 then
          try
            p := FClient.makeParser(ffJson);
            try
              r := p.parseResource(buf.AsBytes);
              try
                w := FFactory.wrapOperationOutcome(r.link);
                try
                  FError := w.text;
                finally
                  w.free;
                end;
              finally
                r.free;
              end;
            finally
              p.Free;
            end;
          except
          end;
        raise EFHIRException.create(FError);
      end
      else
      begin
        FStatus := asyncFailed;
        FError := 'Unexpected response : '+inttostr(FClient.LastStatus)+ ' '+FClient.LastStatusMsg;
        raise EFHIRException.create(FError);
      end;
    end;
  finally
    buf.Free;
  end;
end;

function TFHIRClientAsyncTask.Finished: boolean;
begin
  result := FStatus = asyncFinished;
end;

function TFHIRClientAsyncTask.getNextFile: TDownloadFile;
var
  min : integer;
  t : TDownloadFile;
begin
  min := MAXINT;
  for t in FFiles do
    if (min > t.count) and (t.size = 0) then
      min := t.count;
  result := nil;
  for t in FFiles do
    if t.FCount = min then
      exit(t);
  FStatus := asyncFinished;
end;

procedure TFHIRClientAsyncTask.log(s: String);
begin
  s := DescribePeriod(now - FStart) + ' '+ s + #13#10;
  Flog := s + Flog;
end;

function TFHIRClientAsyncTask.logText : String;
begin
  result := Flog;
end;

procedure TFHIRClientAsyncTask.makeInitialRequest;
var
  headers : THTTPHeaders;
  p, t, v : String;
  buf : TFslBuffer;
begin
  FStatus := asyncInitialQueryInProgress;
  headers.accept := 'application/fhir+json';
  headers.prefer := 'respond-async';
  p := FQuery+'?_outputFormat=application/x-ndjson';
  if since.notNull then
    p := p + '&_since='+since.toXML;
  t := '';
  for v in FTypes do
      CommaAdd(t, v);
  if t <> '' then
    p := '&_type='+t;
  log('Make request of '+p+' with headers '+headers.asString);
  buf := FClient.customGet(p, headers);
  try
    if FClient.LastStatus = 202 then
    begin
      FTaskLocation := FClient.LastHeaders.ContentLocation{.subString(FClient.Address.length)};
      FStatus := asyncWaiting;
      FLastStatus := now;
      log('Accepted. Task Location = '+FTaskLocation);
    end
    else
    begin
      FStatus := asyncFailed;
      FError := 'Unexpected response : '+inttostr(FClient.LastStatus)+ ' '+FClient.LastStatusMsg;
      raise EFHIRException.create(FError);
    end;
  finally
    buf.Free;
  end;
end;

procedure TFHIRClientAsyncTask.next;
begin
  case FStatus of
    asyncReady: makeInitialRequest;
    asyncInitialQueryInProgress: ; // nothing
    asyncWaiting: checkDelay;
    asyncDoPing: doPing;
    asyncPinging: ; // nothing
    asyncDownload : doDownload;
    asyncDownloading: ; // nothing
    asyncFailed: raise EFHIRException.create('Error: '+FError);
  end;
end;

function TFHIRClientAsyncTask.status: String;
begin
  case FStatus of
    asyncReady: result := 'Ready to make Request';
    asyncInitialQueryInProgress: result := 'Initial Query in Progress';
    asyncWaiting: result := 'Waiting for '+inttostr(WAIT_CYCLE_LENGTH - trunc((now - FLastStatus) * DATETIME_DAY_SECONDS))+' secs';
    asyncDoPing: result := 'Ready to check on Progess';
    asyncPinging: result := 'Checking on Progess';
    asyncFailed: result := 'Error: '+FError;
    asyncDownload: result := 'Ready to download '+inttostr(waitingCount)+' files';
    asyncDownloading: result := 'Downloading ('+inttostr(waitingCount)+' files to go)';
  end;
end;

function TFHIRClientAsyncTask.Summary: String;
var
  i : Int64;
  t : TDownloadFile;
begin
  i := 0;
  for t in FFiles do
    inc(i, t.size);
  result := 'Downloaded '+inttostr(FFiles.Count)+' count for '+DescribeBytes(i)+' bytes';
end;

function TFHIRClientAsyncTask.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FClient.sizeInBytes);
  inc(result, FFiles.sizeInBytes);
  inc(result, (FFolder.length * sizeof(char)) + 12);
  inc(result, FTypes.sizeInBytes);
  inc(result, (FQuery.length * sizeof(char)) + 12);
  inc(result, (FLog.length * sizeof(char)) + 12);
  inc(result, (FTaskLocation.length * sizeof(char)) + 12);
  inc(result, (FError.length * sizeof(char)) + 12);
  inc(result, FFactory.sizeInBytes);
end;

{ TDownloadFile }

constructor TDownloadFile.Create(resourceType, url : String);
begin
  inherited Create;
  FResourceType := resourceType;
  FUrl := url;
end;

function TDownloadFile.Link: TDownloadFile;
begin
  result := TDownloadFile(inherited Link);
end;

function TDownloadFile.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FUrl.length * sizeof(char)) + 12);
  inc(result, (FResourceType.length * sizeof(char)) + 12);
  inc(result, (FError.length * sizeof(char)) + 12);
end;

end.
