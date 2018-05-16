unit FHIR.Client.Async;

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



interface

uses
  SysUtils,
  FHIR.Support.DateTime, FHIR.Support.Strings,
  FHIR.Support.Objects, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Client.Base, FHIR.Client.HTTP, FHIR.Client.Threaded, FHIR.Base.Parser,
  {$IFDEF FHIR2} FHIR.R2.Client, {$ENDIF}
  {$IFDEF FHIR3} FHIR.R3.Client, {$ENDIF}
  {$IFDEF FHIR4} FHIR.R4.Client, {$ENDIF}
  FHIR.Tools.Context, FHIR.Tools.Constants, FHIR.Tools.Resources;

const
  WAIT_CYCLE_LENGTH = 10;

type
  TFHIRClientAsyncContextState = (
    asyncReady,
    asyncInitialQueryInProgress,
    asyncWaiting,
    asyncDoPing,
    asyncPinging,
    asyncFailed);

  TFHIRClientAsyncTask = class (TFslObject)
  private
    FClient : TFhirClientV;
    FFolder: String;
    FTypes: TFhirResourceTypeSet;
    FQuery: string;
    FFormat : TFHIRFormat;
    FSince: TDateTimeEx;
    FLog : String;
    FStart : TDateTime;
    FStatus : TFHIRClientAsyncContextState;
    FTaskLocation : String;
    FLastStatus : TDateTime;
    FError : String;

    procedure SetTypes(const Value: TFhirResourceTypeSet);
    procedure log(s : String);
    procedure makeInitialRequest;
    procedure checkDelay;
    procedure doPing;
  public
    Constructor create(client : TFhirClientV);
    Destructor Destroy; override;

    // setup
    property query : string read FQuery write FQuery;
    property format : TFHIRFormat read FFormat write FFormat;
    property folder : String read FFolder write FFolder;
    property since : TDateTimeEx read FSince write FSince;
    property types : TFhirResourceTypeSet read FTypes write SetTypes;

    // operation
    function logText : String;
    function status : String;
    procedure next;
    function Finished : boolean;
  end;


implementation

uses
  FHIR.R3.Constants,
  FHIR.Tools.Utilities;

{ TFHIRClientAsyncTask }

procedure TFHIRClientAsyncTask.checkDelay;
begin
  if now - FLastStatus > WAIT_CYCLE_LENGTH * DATETIME_SECOND_ONE then
  begin
    FStatus := asyncDoPing;
    FLastStatus := now;
  end;
end;

constructor TFHIRClientAsyncTask.create(client: TFhirClientV);
begin
  inherited Create;
  FClient := client;
  FLog := '';
  FStart := now;
  FStatus := asyncReady;
  log('Initialised at '+FormatDateTime('c', now));
end;

destructor TFHIRClientAsyncTask.Destroy;
begin
  FClient.Free;
  inherited;
end;

procedure TFHIRClientAsyncTask.doPing;
var
  headers : THTTPHeaders;
  buf : TFslBuffer;
  p : TFHIRParser;
begin
  FStatus := asyncPinging;
  headers.accept := 'application/fhir+json';
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
          !
          raise Exception.Create('not done yet');
        end
      else if FClient.LastStatus >= 500 then
      begin
        FStatus := asyncFailed;
        FError := FClient.LastStatusMsg;
        if buf.Size > 0 then
          try
            p := FClient.makeParser(ffJson);
            try
              FError := (p.parseResource(buf.AsBytes) as TFhirOperationOutcome).asExceptionMessage;
            finally
              p.Free;
            end;
          except
          end;
        raise Exception.Create(FError);
      end
      else
      begin
        FStatus := asyncFailed;
        FError := 'Unexpected response : '+inttostr(FClient.LastStatus)+ ' '+FClient.LastStatusMsg;
        raise Exception.Create(FError);
      end;
    end;
  finally
    buf.Free;
  end;
end;

function TFHIRClientAsyncTask.Finished: boolean;
begin
  result := false;
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
  p, t : String;
  a : TFhirResourceType;
  buf : TFslBuffer;
begin
  FStatus := asyncInitialQueryInProgress;
  headers.accept := 'application/fhir+json';
  headers.prefer := 'respond-async';
  p := FQuery+'?_outputFormat=application/fhir+ndjson';
  if since.notNull then
    p := p + '&_since='+since.toXML;
  t := '';
  for a := low(TFhirResourceType) to high(TFhirResourceType) do
    if a in types then
      CommaAdd(t, CODES_TFHIRResourceType[a]);
  if t <> '' then
    p := '&_type='+t;
  log('Make request of '+p+' with headers '+headers.asString);
  buf := FClient.customGet(p, headers);
  try
    if FClient.LastStatus = 202 then
    begin
      FTaskLocation := FClient.LastHeaders.ContentLocation.subString(FClient.Address.length);
      FStatus := asyncWaiting;
      FLastStatus := now;
      log('Accepted. Task Location = '+FTaskLocation);
    end
    else
    begin
      FStatus := asyncFailed;
      FError := 'Unexpected response : '+inttostr(FClient.LastStatus)+ ' '+FClient.LastStatusMsg;
      raise Exception.Create(FError);
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
    asyncFailed: raise Exception.Create('Error: '+FError);
  end;
end;

procedure TFHIRClientAsyncTask.SetTypes(const Value: TFhirResourceTypeSet);
begin
  FTypes := Value;
end;

function TFHIRClientAsyncTask.status: String;
begin
  case FStatus of
    asyncReady: result := 'Ready to make Request';
    asyncInitialQueryInProgress: result := 'Initial Query in Progress';
    asyncWaiting: result := 'Waiting for '+inttostr(WAIT_CYCLE_LENGTH - trunc((now - FLastStatus) * DATETIME_DAY_SECONDS))+' secs';
    asyncDoPing: result := 'Read to checking on Progess';
    asyncPinging: result := 'Checking on Progess';
    asyncFailed: result := 'Error: '+FError;
  end;
end;


end.
