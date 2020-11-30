unit fhir_client_threaded;

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
  fsl_base, fsl_stream, fsl_utilities, fsl_threads, fsl_json,
  fhir_objects, fhir_client;

type

  TFhirThreadedClientPackage = class (TFslObject)
  private
    FCommand: TFHIRCommandType;
    FSummary: boolean;
    FError: String;
    FResult: TFhirResourceV;
    FDone: Boolean;
    FThread: TThread;
    FResourceType: TFhirResourceTypeV;
    FAllRecords: boolean;
    Fparams : TStringList;
    FparamString : String;
    FUrl: String;
    FId: String;
    FResource: TFhirResourceV;
    FName: String;
    FLastURL: String;
    FLastStatus : integer;
    FHeaders : THTTPHeaders;
    FBuffer : TFslBuffer;
    FVId: String;
    procedure SetResult(const Value: TFhirResourceV);
    procedure SetResource(const Value: TFhirResourceV);
    procedure SetBuffer(const Value: TFslBuffer);
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    function Link : TFhirThreadedClientPackage; overload;

    property Thread : TThread read FThread write FThread;
    property Done : Boolean read FDone write FDone;

    property command : TFHIRCommandType read FCommand write FCommand;
    property summary : boolean read FSummary write FSummary;
    property resourceType : TFhirResourceTypeV read FResourceType write FResourceType;
    property allRecords : boolean read FAllRecords write FAllRecords;
    property params : TStringList read FParams write FParams;
    property paramString : string read FParamString write FParamString;
    property url : String read FUrl write FUrl;
    property id : String read FId write FId;
    property vid : String read FVId write FVId;
    property name : String read FName write FName;
    property resource : TFhirResourceV read FResource write SetResource;
    property buffer : TFslBuffer read FBuffer write SetBuffer;
    property headers : THTTPHeaders read FHeaders;

    property result : TFhirResourceV read FResult write SetResult;
    property error : String read FError write FError;
    property lastUrl : String read FLastURL write FLastURL;
    property lastStatus : integer read FLastStatus write FLastStatus;
  end;

  TFhirThreadedClientThread = class (TThread)
  private
    FPackage : TFHIRThreadedClientPackage;
    FClient : TFhirClientV;
  protected
    procedure Execute; override;
  public
    constructor Create(client: TFhirClientV; pack : TFhirThreadedClientPackage);
    destructor Destroy; override;
  end;

  TFhirFacadeCommunicator = class abstract (TFHIRClientCommunicator)
  protected
    FInternal : TFhirClientV;
    procedure process(Package : TFhirThreadedClientPackage); virtual; abstract;
  public
    constructor Create(internal: TFhirClientV);
    destructor Destroy; override;

    function address : String; override;
    function conformanceV(summary : boolean) : TFHIRResourceV; override;
    function transactionV(bundle : TFHIRResourceV) : TFHIRResourceV; override;
    function createResourceV(resource : TFHIRResourceV; var id : String) : TFHIRResourceV; override;
    function readResourceV(atype : TFhirResourceTypeV; id : String) : TFHIRResourceV; override;
    function vreadResourceV(atype : TFhirResourceTypeV; id, vid : String) : TFHIRResourceV; override;
    function updateResourceV(resource : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; patch : TJsonArray) : TFHIRResourceV; overload; override;
    procedure deleteResourceV(atype : TFHIRResourceTypeV; id : String); override;
    function searchV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; overload; override;
    function searchPostV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList; resource : TFHIRResourceV) : TFHIRResourceV; override;
    function searchAgainV(link : String) : TFHIRResourceV; overload; override;
    function operationV(atype : TFHIRResourceTypeV; opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function operationV(atype : TFHIRResourceTypeV; id, opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; override;
    function historyTypeV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; override;
    function historyInstanceV(atype : TFHIRResourceTypeV; id : String; allRecords : boolean; params : string) : TFHIRResourceV; override;

    // special case that gives direct access to the communicator...
    function customGet(path : String; headers : THTTPHeaders) : TFslBuffer; override;
    function customPost(path : String; headers : THTTPHeaders; body : TFslBuffer) : TFslBuffer; override;
  end;

  TFhirThreadedCommunicator = class (TFhirFacadeCommunicator)
  private
    FEvent : TThreadManagementEvent;
  protected
    procedure process(Package : TFhirThreadedClientPackage); override;
  public
    constructor Create(internal: TFhirClientV; event: TThreadManagementEvent);
    function link: TFhirThreadedCommunicator;

    procedure terminate; override; // only works for some communicators
  end;

implementation

{ TFhirThreadedClientPackage }

destructor TFhirThreadedClientPackage.Destroy;
begin
  FBuffer.Free;
  FResource.Free;
  FResult.Free;
  inherited;
end;

function TFhirThreadedClientPackage.Link: TFhirThreadedClientPackage;
begin
  result := TFhirThreadedClientPackage(inherited Link);
end;

procedure TFhirThreadedClientPackage.SetBuffer(const Value: TFslBuffer);
begin
  FBuffer.Free;
  FBuffer := Value;
end;

procedure TFhirThreadedClientPackage.SetResource(const Value: TFhirResourceV);
begin
  FResource.Free;
  FResource := Value;
end;

procedure TFhirThreadedClientPackage.SetResult(const Value: TFhirResourceV);
begin
  FResult.Free;
  FResult := Value;
end;

function TFhirThreadedClientPackage.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FError.length * sizeof(char)) + 12);
  inc(result, FResult.sizeInBytes);
  inc(result, Fparams.sizeInBytes);
  inc(result, (FparamString.length * sizeof(char)) + 12);
  inc(result, (FUrl.length * sizeof(char)) + 12);
  inc(result, (FId.length * sizeof(char)) + 12);
  inc(result, FResource.sizeInBytes);
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FLastURL.length * sizeof(char)) + 12);
  inc(result, FHeaders.sizeInBytes);
  inc(result, FBuffer.sizeInBytes);
  inc(result, (FVId.length * sizeof(char)) + 12);
end;

{ TFhirThreadedClientThread }

constructor TFhirThreadedClientThread.Create(client: TFhirClientV; pack: TFhirThreadedClientPackage);
begin
  FClient := client;
  FPackage := pack;
  FreeOnTerminate := true;
  inherited create(false);
end;

destructor TFhirThreadedClientThread.Destroy;
begin
  FClient.Free;
  FPackage.Free;
  inherited;
end;

procedure TFhirThreadedClientThread.execute;
var
  id : String;
begin
  SetThreadName('FHIR.Client.Thread');
  SetThreadStatus('Working');
  try
    try
      case FPackage.command of
        fcmdMetadata: FPackage.result := FClient.conformanceV(FPackage.summary);
        fcmdTransaction : FPackage.result := FCLient.transactionV(FPackage.resource as TFHIRResourceV);
        fcmdRead : FPackage.result := FClient.readResourceV(FPackage.ResourceType, FPackage.id);
        fcmdVersionRead : FPackage.result := FClient.vreadResourceV(FPackage.ResourceType, FPackage.id, FPackage.vid);
        fcmdCreate :
          begin
            FPackage.result := FClient.createResourceV(FPackage.resource, id);
            FPackage.id := id;
          end;
        fcmdSearch :
          if FPackage.FUrl <> '' then
            FPackage.result := FClient.searchAgainV(FPackage.url)
          else if FPackage.resourceType = '' then
            if FPackage.params <> nil then
              FPackage.result := FClient.searchV(FPackage.allRecords, FPackage.params)
            else
              FPackage.result := FClient.searchV(FPackage.allRecords, FPackage.paramString)
          else
            if FPackage.params <> nil then
              FPackage.result := FClient.searchV(FPackage.resourceType, FPackage.allRecords, FPackage.params)
            else
              FPackage.result := FClient.searchV(FPackage.resourceType, FPackage.allRecords, FPackage.paramString);
        fcmdUpdate : FPackage.result := FClient.updateResourceV(FPackage.Resource);
        fcmdOperation :
          if FPackage.id <> '' then
            FPackage.result := FClient.operationV(FPackage.resourceType, FPackage.id, FPackage.name, FPackage.resource)
          else
            FPackage.result := FClient.operationV(FPackage.resourceType, FPackage.name, FPackage.resource);
        fcmdTask :
          if FPackage.FBuffer <> nil then
            FPackage.Buffer := FClient.customPost(FPackage.paramString, FPackage.FHeaders, FPackage.Buffer)
          else
            FPackage.FBuffer := FClient.customGet(FPackage.paramString, FPackage.FHeaders);
      else
        raise EFHIRTodo.create('TFhirThreadedClientThread.execute');
      end;
    except
      on e : exception do
        FPackage.error := e.Message;
    end;
    FPackage.FLastURL := FClient.LastURL;
    FPackage.FLastStatus := FClient.LastStatus;
    FPackage.FHeaders := FClient.LastHeaders;
  finally
    FPackage.FDone := true;
  end;
  closeThread();
end;

{ TFhirFacadeCommunicator }

constructor TFhirFacadeCommunicator.Create(internal: TFhirClientV);
begin
  Inherited Create;
  FInternal := internal;
end;

destructor TFhirFacadeCommunicator.Destroy;
begin
  FInternal.free;
  inherited;
end;

function TFhirFacadeCommunicator.address: String;
begin
  result := FInternal.address;
end;

function TFhirFacadeCommunicator.conformanceV(summary: boolean): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdMetadata;
    pack.summary := summary;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

function TFhirFacadeCommunicator.createResourceV(resource: TFhirResourceV; var id: String): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdCreate;
    pack.resource := resource.Link;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link;
    id := pack.id;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

function TFhirFacadeCommunicator.customGet(path: String; headers: THTTPHeaders): TFslBuffer;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdTask;
    pack.resourceType := '';
    pack.paramString := path;
    pack.Fheaders := Headers;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    FHeaders := FInternal.LastHeaders;
    result := pack.Fbuffer.link;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

function TFhirFacadeCommunicator.customPost(path: String; headers: THTTPHeaders; body : TFslBuffer): TFslBuffer;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdTask;
    pack.resourceType := '';
    pack.paramString := path;
    pack.Fheaders := Headers;
    pack.buffer := body.Link;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    FHeaders := FInternal.LastHeaders;
    result := pack.Fbuffer.link;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

procedure TFhirFacadeCommunicator.deleteResourceV(atype: TFhirResourceTypeV; id: String);
begin
 raise EFHIRTodo.create('TFhirThreadedCommunicator.deleteResourceV');
end;

function TFhirFacadeCommunicator.historyTypeV(atype: TFhirResourceTypeV; allRecords: boolean; params : string): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFhirThreadedCommunicator.historyTypeV');
end;

function TFhirFacadeCommunicator.historyInstanceV(atype: TFhirResourceTypeV; id : String; allRecords: boolean; params : string): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFhirThreadedCommunicator.historyInstanceV');
end;

function TFhirFacadeCommunicator.operationV(atype: TFhirResourceTypeV; id, opName: String; params: TFHIRResourceV): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdOperation;
    pack.resourceType := aType;
    pack.id := id;
    pack.Name := opName;
    pack.resource := params.Link;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

function TFhirFacadeCommunicator.patchResourceV(atype: TFhirResourceTypeV; id: String; patch: TJsonArray): TFHIRResourceV;
begin
  raise Exception.Create('not done yet');
end;

function TFhirFacadeCommunicator.patchResourceV(atype: TFhirResourceTypeV; id: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('not done yet');
end;

function TFhirFacadeCommunicator.operationV(atype: TFhirResourceTypeV; opName: String; params: TFHIRResourceV): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdOperation;
    pack.resourceType := aType;
    pack.Name := opName;
    pack.resource := params.Link;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

function TFhirFacadeCommunicator.readResourceV(atype: TFhirResourceTypeV; id: String): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdRead;
    pack.resourceType := aType;
    pack.id := id;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

function TFhirFacadeCommunicator.vreadResourceV(atype: TFhirResourceTypeV; id, vid: String): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdRead;
    pack.resourceType := aType;
    pack.id := id;
    pack.vid := vid;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

function TFhirFacadeCommunicator.searchV(atype: TFhirResourceTypeV; allRecords: boolean; params : string): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdSearch;
    pack.resourceType := aType;
    pack.allRecords := allRecords;
    pack.paramString := params;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link as TFHIRResourceV;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;


function TFhirFacadeCommunicator.searchAgainV(link: String): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdSearch;
    pack.url := link;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link as TFHIRResourceV;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

function TFhirFacadeCommunicator.searchPostV(atype: TFhirResourceTypeV; allRecords: boolean; params : TStringList; resource: TFhirResourceV): TFHIRResourceV;
begin
  raise EFHIRTodo.create('TFhirThreadedCommunicator.searchPostV');
end;

function TFhirFacadeCommunicator.transactionV(bundle: TFHIRResourceV): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdTransaction;
    pack.resource := bundle.link;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link as TFHIRResourceV;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

function TFhirFacadeCommunicator.updateResourceV(resource: TFhirResourceV): TFHIRResourceV;
var
  pack : TFhirThreadedClientPackage;
begin
  pack := TFhirThreadedClientPackage.create;
  try
    pack.command := fcmdUpdate;
    pack.resource := resource.link;
    pack.Thread := TFhirThreadedClientThread.create(FInternal.link, pack.Link);
    process(pack);
    result := pack.result.link;
    FHeaders := FInternal.LastHeaders;
    FClient.LastUrl := pack.lastUrl;
    FClient.LastStatus := pack.lastStatus;
  finally
    pack.free;
  end;
end;

{ TFhirThreadedCommunicator }

constructor TFhirThreadedCommunicator.Create(internal: TFhirClientV; event: TThreadManagementEvent);
begin
  Inherited Create(internal);
  FEvent := event;
end;

function TFhirThreadedCommunicator.link: TFhirThreadedCommunicator;
begin
  result := TFhirThreadedCommunicator(inherited Link);
end;

procedure TFhirThreadedCommunicator.terminate;
begin
  raise EFHIRTodo.create('TFhirThreadedCommunicator.terminate');
end;

procedure TFhirThreadedCommunicator.process(Package : TFhirThreadedClientPackage);
var
  stop : boolean;
begin
  repeat
    stop := false;
    FEvent(FClient, stop);
    if stop then
    begin
      try
        Package.Thread.Terminate;
        FInternal.terminate;
      except
      end;
      abort;
    end;
  until Package.Done;
  if Package.error <> '' then
    raise EFHIRException.create(Package.error);
end;

end.
