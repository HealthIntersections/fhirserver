unit FHIR.Client.Base;

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
  SysUtils, Classes,
  FHIR.Support.Strings,
  FHIR.Support.Objects, FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Lang, FHIR.Base.Parser,
  FHIR.Client.SmartUtilities, FHIR.CdsHooks.Utilities;

Type
  TFhirOperationOutcomeW = class (TFslObject)
  protected
    FRes : TFHIRResourceV;
  public
    constructor Create(res : TFHIRResourceV);
    destructor Destroy; override;

    function link : TFhirOperationOutcomeW; overload;

    function hasText : boolean; virtual;
    function text : String; virtual;
    function code :  TExceptionType; virtual;
  end;
  TFhirOperationOutcomeWClass = class of TFhirOperationOutcomeW;

  EFHIRClientException = class (Exception)
  private
    FIssue : TFhirOperationOutcomeW;
  public
    constructor Create(message : String; issue : TFhirOperationOutcomeW);
    destructor Destroy; override;

    property issue : TFhirOperationOutcomeW read FIssue;
  end;

  THTTPHeaders = record
    contentType : String;
    accept : String;
    prefer : String;
    location : String;
    contentLocation : String;
    lastOperationId : String; // some servers return an id that links to their own internal log for debugging

    function asString : string;
  end;

  TFHIRClientLogger = class (TFslObject)
  public
    function Link : TFHIRClientLogger; overload;
    procedure logExchange(verb, url, status, requestHeaders, responseHeaders : String; request, response : TStream);  virtual;
  end;

  TNullLogger = class (TFHIRClientLogger)
  public
    procedure logExchange(verb, url, status, requestHeaders, responseHeaders : String; request, response : TStream);  override;
  end;

  // client architecture:
  //
  // the base client is TFhirClientV. There is a subclass for each version.
  //
  // when constructing, the client takes a parameter for the actual communicator that does all the work
  TFhirClientV = class;

  TBundleHandler = class (TFslObject)
  private
    FResource : TFhirResourceV;
  public
    constructor Create(bnd : TFHIRResourceV);
    destructor Destroy; override;
    property resource : TFHIRResourceV read FResource;
    function next : String; overload;
    function next(bnd : TFHIRResourceV) : String; overload; virtual;
    procedure addEntries(bnd : TFHIRResourceV); virtual;
    procedure clearLinks; virtual;
  end;

  TBundleHandlerClass = class of TBundleHandler;

  // never use this directly - always use a TFHIRClientV descendent
  TFHIRClientCommunicator = class (TFslObject)
  protected
    FClient : TFHIRClientV; // not linked
    FBundleHandler : TBundleHandlerClass;
    FHeaders : THTTPHeaders;
    function address : String; virtual; // result from the communicator
    function getResourceVersionId(res : TFHIRResourceV) : string;
    procedure notify(msg : String);
    function ProvenanceString : string;
    function opWrapper : TFhirOperationOutcomeWClass;
  public

    // version independent API
    function conformanceV(summary : boolean) : TFHIRResourceV; virtual;
    function transactionV(bundle : TFHIRResourceV) : TFHIRResourceV; virtual;
    function createResourceV(resource : TFHIRResourceV; var id : String) : TFHIRResourceV; virtual;
    function readResourceV(atype : TFhirResourceTypeV; id : String) : TFHIRResourceV; virtual;
    function updateResourceV(resource : TFHIRResourceV) : TFHIRResourceV; overload; virtual;
    procedure deleteResourceV(atype : TFHIRResourceTypeV; id : String); virtual;
    function searchV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; overload; virtual;
    function searchPostV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList; resource : TFHIRResourceV) : TFHIRResourceV; virtual;
    function searchAgainV(link : String) : TFHIRResourceV; overload; virtual;
    function operationV(atype : TFHIRResourceTypeV; opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; virtual;
    function operationV(atype : TFHIRResourceTypeV; id, opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; virtual;
    function historyTypeV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; virtual;
    function cdshook(id: String; request: TCDSHookRequest): TCDSHookResponse; virtual;

    // special case that gives direct access to the communicator...
    function custom(path : String; headers : THTTPHeaders) : TFslBuffer; virtual;
    procedure terminate; virtual; // only works for some communicators
  end;

  TFhirHTTPClientStatusEvent = procedure (client : TObject; details : String) of Object;

  TFhirClientV = {abstract} class (TFslObject)
  private
    FCommunicator : TFHIRClientCommunicator;
    FWorker : TFHIRWorkerContextV;
    FLogger : TFHIRClientLogger;
    FLastURL: String;
    FLastStatus : integer;
    FProvenance: TFHIRResourceV;
    FProvenanceString : String;
    FVersionSpecific: boolean;
    FFormat : TFHIRFormat;
    FLang : string;
    FSmartToken: TSmartOnFhirAccessToken;
    procedure SetProvenance(const Value: TFHIRResourceV);
    procedure SetSmartToken(const Value: TSmartOnFhirAccessToken);
    function encodeParams(params: TStringList): String;
    function GetHeaders: THTTPHeaders;
  protected
    procedure SetLogger(const Value: TFHIRClientLogger); virtual;
    function opWrapper : TFhirOperationOutcomeWClass; virtual;
    procedure SetFormat(fmt : TFhirFormat); virtual;
    function getResourceVersionId(res : TFHIRResourceV) : string; virtual;
    function getBundleHandler : TBundleHandlerClass; virtual;
  public
    constructor create(worker : TFHIRWorkerContextV; lang : String; communicator : TFHIRClientCommunicator);
    destructor Destroy; override;
    function link : TFhirClientV; overload;

    function makeParser(fmt : TFHIRFormat) : TFHIRParser; virtual;
    function makeComposer(fmt : TFHIRFormat; style : TFHIROutputStyle) : TFHIRComposer; virtual;

    property lang : string read FLang;
    property Worker : TFHIRWorkerContextV read FWorker;
    function version : TFHIRVersion; virtual;
    function address : String; // result from the communicator
    property Communicator : TFHIRClientCommunicator read FCommunicator;

    property format : TFHIRFormat read FFormat write SetFormat;
    property versionSpecific : boolean read FVersionSpecific write FVersionSpecific;
    property LastURL : String read FLastURL write FLastURL;
    property LastHeaders : THTTPHeaders read GetHeaders;
    property LastStatus : integer read FLastStatus write FLastStatus;
    property Logger : TFHIRClientLogger read FLogger write SetLogger;
    property provenance : TFHIRResourceV read FProvenance write SetProvenance;
    property smartToken : TSmartOnFhirAccessToken read FSmartToken write SetSmartToken;

    // version independent API
    function conformanceV(summary : boolean) : TFHIRResourceV;
    function transactionV(bundle : TFHIRResourceV) : TFHIRResourceV;
    function createResourceV(resource : TFHIRResourceV; var id : String) : TFHIRResourceV;
    function readResourceV(atype : TFhirResourceTypeV; id : String) : TFHIRResourceV;
    function updateResourceV(resource : TFHIRResourceV) : TFHIRResourceV; overload;
    procedure deleteResourceV(atype : TFHIRResourceTypeV; id : String);
    function searchV(allRecords : boolean; params : TStringList) : TFHIRResourceV; overload;
    function searchV(allRecords : boolean; params : string) : TFHIRResourceV; overload;
    function searchV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList) : TFHIRResourceV; overload;
    function searchV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; overload;
    function searchPostV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList; resource : TFHIRResourceV) : TFHIRResourceV;
    function searchAgainV(link : String) : TFHIRResourceV; overload;
    function operationV(atype : TFHIRResourceTypeV; opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload;
    function operationV(atype : TFHIRResourceTypeV; id, opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload;
    function historyTypeV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList) : TFHIRResourceV;

    // special case that gives direct access to the communicator...
    function custom(path : String; headers : THTTPHeaders) : TFslBuffer; virtual;
    function cdshook(id: String; request: TCDSHookRequest): TCDSHookResponse; virtual;

    procedure terminate; virtual; // only works for some communicators
  end;

implementation

{ TFhirOperationOutcomeW }

function TFhirOperationOutcomeW.code: TExceptionType;
begin
  raise Exception.Create('Must override code in '+className);
end;

constructor TFhirOperationOutcomeW.create(res: TFHIRResourceV);
begin
  inherited create;
  FRes := res;
end;

destructor TFhirOperationOutcomeW.Destroy;
begin
  FRes.Free;
  inherited;
end;

function TFhirOperationOutcomeW.hasText: boolean;
begin
  raise Exception.Create('Must override hasText in '+className);
end;

function TFhirOperationOutcomeW.link: TFhirOperationOutcomeW;
begin
  result := TFhirOperationOutcomeW(inherited Link);
end;

function TFhirOperationOutcomeW.text: String;
begin
  raise Exception.Create('Must override text in '+className);
end;

{ EFHIRClientException }

constructor EFHIRClientException.create(message: String; issue: TFhirOperationOutcomeW);
begin
  inherited create(message);
  FIssue := issue;
  end;

destructor EFHIRClientException.destroy;
begin
  FIssue.Free;
  inherited;
end;

{ THTTPHeaders }

function THTTPHeaders.asString: string;
begin
  result := '';
  if (contentType <> '') then
    CommaAdd(result, 'ContentType: '+contentType);
  if (accept <> '') then
    CommaAdd(result, 'Accept: '+accept);
  if (prefer <> '') then
    CommaAdd(result, 'Prefer: '+prefer);
  if (location <> '') then
    CommaAdd(result, 'Location: '+location);
  if (lastOperationId <> '') then
    CommaAdd(result, 'X-Last-Operation-Id: '+lastOperationId);
end;


{ TFHIRClientLogger }

function TFHIRClientLogger.Link: TFHIRClientLogger;
begin
  result := TFHIRClientLogger(inherited Link);
end;

procedure TFHIRClientLogger.logExchange(verb, url, status, requestHeaders,
  responseHeaders: String; request, response: TStream);
begin
  raise Exception.Create('Must override logExchange in '+className);
end;

{ TNullLogger }

procedure TNullLogger.logExchange(verb, url, status, requestHeaders, responseHeaders: String; request, response: TStream);
begin
  // nothing
end;

{ TFHIRClientCommunicator }

function TFHIRClientCommunicator.address: String;
begin
  raise Exception.Create('Must override address in '+className);
end;

function TFHIRClientCommunicator.cdshook(id: String; request: TCDSHookRequest): TCDSHookResponse;
begin
  raise Exception.Create('Must override cdshook in '+className);
end;

function TFHIRClientCommunicator.conformanceV(summary: boolean): TFHIRResourceV;
begin
  raise Exception.Create('Must override conformanceV in '+className);
end;

function TFHIRClientCommunicator.createResourceV(resource: TFHIRResourceV; var id: String): TFHIRResourceV;
begin
  raise Exception.Create('Must override createResourceV in '+className);
end;

function TFHIRClientCommunicator.custom(path: String; headers: THTTPHeaders): TFslBuffer;
begin
  raise Exception.Create('Must override custom in '+className);
end;

procedure TFHIRClientCommunicator.deleteResourceV(atype: TFHIRResourceTypeV; id: String);
begin
  raise Exception.Create('Must override deleteResourceV in '+className);
end;

function TFHIRClientCommunicator.getResourceVersionId(res : TFHIRResourceV) : string;
begin
  result := FClient.getResourceVersionId(res);
end;

function TFHIRClientCommunicator.historyTypeV(atype: TFHIRResourceTypeV; allRecords: boolean; params: string): TFHIRResourceV;
begin
  raise Exception.Create('Must override historyTypeV in '+className);
end;

procedure TFHIRClientCommunicator.notify(msg: String);
begin
 // nothing
end;

function TFHIRClientCommunicator.operationV(atype: TFHIRResourceTypeV; opName: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('Must override operationV in '+className);
end;

function TFHIRClientCommunicator.operationV(atype: TFHIRResourceTypeV; id, opName: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('Must override operationV in '+className);
end;

function TFHIRClientCommunicator.opWrapper: TFhirOperationOutcomeWClass;
begin
  result := FClient.opWrapper;
end;

function TFHIRClientCommunicator.ProvenanceString: string;
begin
  result := FClient.FProvenanceString;
end;

function TFHIRClientCommunicator.readResourceV(atype: TFhirResourceTypeV; id: String): TFHIRResourceV;
begin
  raise Exception.Create('Must override readResourceV in '+className);
end;

function TFHIRClientCommunicator.searchAgainV(link: String): TFHIRResourceV;
begin
  raise Exception.Create('Must override searchAgainV in '+className);
end;

function TFHIRClientCommunicator.searchPostV(atype: TFHIRResourceTypeV; allRecords: boolean; params: TStringList; resource: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('Must override searchPostV in '+className);
end;

function TFHIRClientCommunicator.searchV(atype: TFHIRResourceTypeV; allRecords: boolean; params: string): TFHIRResourceV;
begin
  raise Exception.Create('Must override searchV in '+className);
end;

procedure TFHIRClientCommunicator.terminate;
begin
  raise Exception.Create('Must override terminate in '+className);
end;

function TFHIRClientCommunicator.transactionV(bundle: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('Must override transactionV in '+className);
end;

function TFHIRClientCommunicator.updateResourceV(resource: TFHIRResourceV): TFHIRResourceV;
begin
  raise Exception.Create('Must override updateResourceV in '+className);
end;

{ TFhirClientV }

constructor TFhirClientV.create(worker : TFHIRWorkerContextV; lang : String; communicator : TFHIRClientCommunicator);
begin
  inherited Create;
  FWorker := worker;
  FLang := lang;
  FCommunicator := communicator;
  communicator.FClient := self;
  communicator.FBundleHandler := getBundleHandler;
  FLogger := TNullLogger.Create;
end;

destructor TFhirClientV.Destroy;
begin
  FSmartToken.Free;
  FWorker.Free;
  FCommunicator.Free;
  FLogger.Free;
  FProvenance.Free;
  inherited;
end;

function TFhirClientV.getBundleHandler: TBundleHandlerClass;
begin
  raise Exception.Create('Must override getBundleHandler in '+className);
end;

function TFhirClientV.GetHeaders: THTTPHeaders;
begin
  result := FCommunicator.FHeaders;
end;

function TFhirClientV.getResourceVersionId(res : TFHIRResourceV) : string;
begin
  raise Exception.Create('Must override getResourceValue in '+className);
end;

function TFhirClientV.link: TFhirClientV;
begin
  result := TFhirClientV(inherited Link);
end;

procedure TFhirClientV.SetLogger(const Value: TFHIRClientLogger);
begin
  FLogger.Free;
  FLogger := Value;
end;

procedure TFhirClientV.SetProvenance(const Value: TFHIRResourceV);
var
  c : TFHIRComposer;
begin
  FProvenance.Free;
  FProvenance := Value;
  if FProvenance <> nil then
  begin
    c := makeComposer(ffJson, OutputStyleNormal);
    try
      FProvenanceString := c.Compose(value);
    finally
      c.Free;
    end;
  end
  else
    FProvenanceString := '';
end;

function TFhirClientV.makeParser(fmt : TFHIRFormat) : TFHIRParser;
begin
  raise Exception.Create('Must override makeParser in '+className);
end;

function TFhirClientV.makeComposer(fmt : TFHIRFormat; style : TFHIROutputStyle) : TFHIRComposer;
begin
  raise Exception.Create('Must override makeComposer in '+className);
end;

function TFhirClientV.opWrapper : TFhirOperationOutcomeWClass;
begin
  raise Exception.Create('Must override opWrapper in '+className);
end;

function TFhirClientV.version : TFHIRVersion;
begin
  raise Exception.Create('Must override version in '+className);
end;

procedure TFhirClientV.SetFormat(fmt : TFhirFormat);
begin
  if not (fmt in [ffXml, ffJson, ffTurtle]) then
    raise Exception.Create('Unsupported format in client: '+CODES_TFHIRFormat[fmt]);

  FFormat := fmt;
end;

function TFhirClientV.address : String;
begin
  result := FCommunicator.address;
end;

function TFhirClientV.cdshook(id: String; request: TCDSHookRequest): TCDSHookResponse;
begin
  result := FCommunicator.cdshook(id, request);
end;

function TFhirClientV.conformanceV(summary : boolean) : TFHIRResourceV;
begin
  result := FCommunicator.conformanceV(summary);
end;

function TFhirClientV.transactionV(bundle : TFHIRResourceV) : TFHIRResourceV;
begin
  result := FCommunicator.transactionV(bundle);
end;

function TFhirClientV.createResourceV(resource : TFHIRResourceV; var id : String) : TFHIRResourceV;
begin
  result := FCommunicator.createResourceV(resource, id);
end;

function TFhirClientV.readResourceV(atype : TFhirResourceTypeV; id : String) : TFHIRResourceV;
begin
  result := FCommunicator.readResourceV(aType, id);
end;

function TFhirClientV.updateResourceV(resource : TFHIRResourceV) : TFHIRResourceV;
begin
  result := FCommunicator.updateResourceV(resource);
end;

procedure TFhirClientV.deleteResourceV(atype : TFHIRResourceTypeV; id : String);
begin
  FCommunicator.deleteResourceV(atype, id);
end;

function TFhirClientV.searchV(allRecords : boolean; params : TStringList) : TFHIRResourceV;
begin
  result := searchV(allrecords, encodeParams(params));
end;

function TFhirClientV.searchV(allRecords : boolean; params : string) : TFHIRResourceV;
begin
  result := FCommunicator.searchV('', allRecords, params);
end;

function TFhirClientV.searchV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList) : TFHIRResourceV;
begin
  result := searchV(atype, allrecords, encodeParams(params));
end;

function TFhirClientV.searchV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV;
begin
  result := FCommunicator.searchV(aType, allRecords, params);
end;

function TFhirClientV.searchPostV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList; resource : TFHIRResourceV) : TFHIRResourceV;
begin
  result := FCommunicator.searchPostV(aType, allRecords, params, resource);
end;

function TFhirClientV.searchAgainV(link : String) : TFHIRResourceV;
begin
  result := FCommunicator.searchAgainV(link);
end;

function TFhirClientV.operationV(atype : TFHIRResourceTypeV; opName : String; params : TFHIRResourceV) : TFHIRResourceV;
begin
  result := FCommunicator.operationV(aType, opName, params);
end;

function TFhirClientV.operationV(atype : TFHIRResourceTypeV; id, opName : String; params : TFHIRResourceV) : TFHIRResourceV;
begin
  result := FCommunicator.operationV(aType, id, opName, params);
end;

function TFhirClientV.historyTypeV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList) : TFHIRResourceV;
begin
  result := FCommunicator.historyTypeV(aType, allRecords, encodeParams(params));
end;

function TFhirClientV.custom(path : String; headers : THTTPHeaders) : TFslBuffer;
begin
  result := FCommunicator.custom(path, headers);
end;

procedure TFhirClientV.terminate;  // only works for some communicators
begin
  FCommunicator.terminate;
end;

procedure TFhirClientV.SetSmartToken(const Value: TSmartOnFhirAccessToken);
begin
  FSmartToken.Free;
  FSmartToken := Value;
  // todo: set the header for the access token
end;

function TFhirClientV.encodeParams(params : TStringList) : String;
var
  i : integer;
  s : String;
begin
  result := '';
  for i := 0 to params.Count - 1 do
  begin
    s := params.Names[i];
    result := result + s+'='+EncodeMIME(params.ValueFromIndex[i])+'&';
  end;
end;

{ TBundleHandler }

procedure TBundleHandler.addEntries(bnd: TFHIRResourceV);
begin
  raise Exception.Create('Must override addEntries in '+ClassName);
end;

procedure TBundleHandler.clearLinks;
begin
  raise Exception.Create('Must override clearLinks in '+ClassName);
end;

constructor TBundleHandler.Create(bnd: TFHIRResourceV);
begin
  inherited Create;
  FResource := bnd;
end;

destructor TBundleHandler.Destroy;
begin
  FResource.Free;
  inherited;
end;

function TBundleHandler.next(bnd: TFHIRResourceV): String;
begin
  raise Exception.Create('Must override next in '+ClassName);
end;

function TBundleHandler.next: String;
begin
  result := next(FResource);
end;


end.


