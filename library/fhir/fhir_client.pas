unit fhir_client;

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
  fsl_base, fsl_utilities, fsl_logging,
  fsl_stream, fsl_json, fsl_http,
  fhir_objects,  fhir_parser, fhir_common;

type
  TSmartAppLaunchMode = (salmNone, salmOAuthClient, salmBackendClient);

const
  CODES_TSmartAppLaunchMode : array [TSmartAppLaunchMode] of string = ('No Security', 'OAuth Client', 'Backend Services');

Type
  EFHIRClientException = class (EFHIRException)
  private
    FIssue : TFhirOperationOutcomeW;
    FCode : integer;
  public
    constructor Create(code : integer; message : String; issue : TFhirOperationOutcomeW); overload;
    constructor Create(code : integer; message : String); overload;
    destructor Destroy; override;

    property errorCode : Integer read FCode;
    property issue : TFhirOperationOutcomeW read FIssue;
  end;

  THTTPHeaders = record
    contentType : String;
    accept : String;
    prefer : String;
    location : String;
    contentLocation : String;
    Timestamp : TDateTime;
    lastOperationId : String; // some servers return an id that links to their own internal log for debugging
    progress : String; // X-Progress fgrom bulk data
    function asString : string;

    procedure addToHeaders(list : TStringList);
    function sizeInBytes(magic : integer) : cardinal;
  end;

  { TFslHTTPBuffer }

  TFslHTTPBuffer = class (TFslBuffer)
  private
    FTimestamp : TDateTime;
    FMimeType : String;
  public
    function link : TFslHTTPBuffer; overload;
    property timestamp : TDateTime read FTimestamp write FTimestamp;
    property mimeType : String read FMimeType write FMimeType;
  end;

  { TFHIRServerDetails }

  TFHIRServerDetails = class (TFslObject)
  private
    FClientId: String;
    FClientSecret: String;
    FFormat: TFHIRFormat;
    FJson: boolean;
    FName: String;
    FRedirect: String;
    FSmartMode: TSmartAppLaunchMode;
    FURL: String;
    FSmartConfig : TJsonObject;
    FVersion: TFHIRVersion;
    FXml: boolean;
    procedure SetSmartConfig(AValue: TJsonObject);
  public
    destructor Destroy; override;

    function link : TFHIRServerDetails; overload;
    function clone : TFHIRServerDetails; overload;
    procedure assign(other : TFslObject); override;

    property name : String read FName write FName;
    property URL : String read FURL write FURL;

    property version : TFHIRVersion read FVersion write FVersion;
    property xml: boolean read FXml write FXml;
    property json : boolean read FJson write FJson;
    property format : TFHIRFormat read FFormat write FFormat;
    property smartConfig : TJsonObject read FSmartConfig write SetSmartConfig;
    property smartMode : TSmartAppLaunchMode read FSmartMode write FSmartMode;
    property ClientId : String read FClientId write FClientId;
    property ClientSecret : String read FClientSecret write FClientSecret;
    property Redirect : String read FRedirect write FRedirect;
  end;

  TFHIRClientType = (fctCrossPlatform {indy}, fctWinInet);

  TClientAccessToken = class (TFslObject)
  private
    FidToken: TJWT;
    Fscopes: String;
    FaccessToken: String;
    Fexpires: TDateTime;
    FPatient: String;
    procedure SetidToken(const Value: TJWT);
  protected
    function sizeInBytesV(magic : integer) : cardinal; override;
  public
    destructor Destroy; override;

    function link : TClientAccessToken; overload;

    // the bearer access token to add to the HTTP calls. If you assign the
    // access token to a TFhirHTTPClient, it will do this
    property accessToken : String read FaccessToken write FaccessToken;

    // when this token expires (or 0 if expiry is unknown) (in local time)
    property expires : TDateTime read Fexpires write Fexpires;

    // the scopes that were actualyl granted (may be more or less than originally asked for)
    property scopes : String read Fscopes write Fscopes;

    // if an openID token was returned, the details in it (note: unverified)
    property idToken : TJWT read FidToken write SetidToken;

    // patient id specified in the token (if one is)
    property patient : String read FPatient write FPatient;

    // convenient username for the user, if any appropriate information is available
    function username : String;
  end;

  TFHIRClientLogger = class (TFslObject)
  public
    function Link : TFHIRClientLogger; overload;
    procedure logExchange(verb, url, status, requestHeaders, responseHeaders : String; request, response : TBytes);  virtual; abstract;
  end;

  TNullLogger = class (TFHIRClientLogger)
  public
    procedure logExchange(verb, url, status, requestHeaders, responseHeaders : String; request, response : TBytes);  override;
  end;

  { TTextFileLogger }

  TTextFileLogger = class(TFHIRClientLogger)
  private
    FLog: TLogger;
    function toChars(b: TBytes): string;
  public
    constructor Create(filename : String = '');
    destructor Destroy; override;

    procedure logExchange(verb, url, status, requestHeaders, responseHeaders: String; request, response: TBytes); override;
  end;

  // client architecture:
  //
  // the base client is TFhirClientV. There is a subclass for each version.
  //
  // when constructing, the client takes a parameter for the actual communicator that does all the work
  TFhirClientV = class;
  TThreadManagementEvent = procedure (sender : TFhirClientV; var stop : boolean) of object;

  // never use this directly - always use a TFHIRClientV descendent
  TFHIRClientCommunicator = class (TFslObject)
  protected
    FClient : TFHIRClientV; // not linked
    FHeaders : THTTPHeaders;
    function address : String; virtual;  abstract; // result from the communicator
    function getResourceVersionId(res : TFHIRResourceV) : string;
    procedure notify(msg : String);
    function ProvenanceString : string;
    function opWrapper : TFhirOperationOutcomeWClass;
    function sizeInBytesV(magic : integer) : cardinal; override;
  public

    // version independent API
    function conformanceV(summary : boolean) : TFHIRResourceV; virtual; abstract;
    function conformanceModeV(mode : string) : TFHIRResourceV; virtual; abstract;
    function transactionV(bundle : TFHIRResourceV) : TFHIRResourceV; virtual; abstract;
    function createResourceV(resource : TFHIRResourceV; var id : String) : TFHIRResourceV; virtual; abstract;
    function readResourceV(atype : TFhirResourceTypeV; id : String) : TFHIRResourceV; virtual; abstract;
    function vreadResourceV(atype : TFhirResourceTypeV; id, vid : String) : TFHIRResourceV; virtual; abstract;
    function updateResourceV(resource : TFHIRResourceV) : TFHIRResourceV; overload; virtual; abstract;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; params : TFHIRResourceV) : TFHIRResourceV; overload; virtual; abstract;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; patch : TJsonArray) : TFHIRResourceV; overload; virtual; abstract;
    procedure deleteResourceV(atype : TFHIRResourceTypeV; id : String); virtual; abstract;
    function searchV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; overload; virtual; abstract;
    function searchPostV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList; resource : TFHIRResourceV) : TFHIRResourceV; virtual; abstract;
    function searchAgainV(link : String) : TFHIRResourceV; overload; virtual; abstract;
    function operationV(atype : TFHIRResourceTypeV; opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; virtual; abstract;
    function operationV(atype : TFHIRResourceTypeV; id, opName : String; params : TFHIRResourceV) : TFHIRResourceV; overload; virtual; abstract;
    function historyTypeV(atype : TFHIRResourceTypeV; allRecords : boolean; params : string) : TFHIRResourceV; virtual; abstract;
    function historyInstanceV(atype : TFHIRResourceTypeV; id : string; allRecords : boolean; params : string) : TFHIRResourceV; virtual; abstract;

    // special case that gives direct access to the communicator...
    function customGet(path : String; headers : THTTPHeaders) : TFslHTTPBuffer; virtual; abstract;
    function customPost(path : String; headers : THTTPHeaders; body : TFslHTTPBuffer) : TFslHTTPBuffer; virtual; abstract;
    procedure terminate; virtual;  abstract; // only works for some communicators
  end;

  TFhirClientProgressEvent = procedure (client : TObject; details : String; pct : integer; done : boolean) of Object;

  { TFhirClientV }

  TFhirClientV = class abstract (TFslObject)
  private
    FCommunicator : TFHIRClientCommunicator;
    FWorker : TFHIRWorkerContextV;
    FLogger : TFHIRClientLogger;
    FLastURL: String;
    FLastStatus : integer;
    FProvenance: TFhirProvenanceW;
    FProvenanceString : String;
    FVersionSpecific: boolean;
    FFormat : TFHIRFormat;
    FLangList : THTTPLanguageList;
    FSmartToken: TClientAccessToken;
    FLastStatusMsg: String;
    FOnProgress : TFhirClientProgressEvent;
    FBundleFactory : TFHIRBundleWClass;
    procedure SetProvenance(const Value: TFhirProvenanceW);
    procedure SetSmartToken(const Value: TClientAccessToken);
    function encodeParams(params: TStringList): String;
    function GetHeaders: THTTPHeaders;
  protected
    procedure SetLogger(const Value: TFHIRClientLogger); virtual;
    function opWrapper : TFhirOperationOutcomeWClass; virtual; abstract;
    procedure SetFormat(fmt : TFhirFormat); virtual;
    function getResourceVersionId(res : TFHIRResourceV) : string; virtual;
    function getBundleClass : TFHIRBundleWClass; virtual; abstract;
  public
    constructor Create(worker : TFHIRWorkerContextV; langList : THTTPLanguageList; communicator : TFHIRClientCommunicator);
    destructor Destroy; override;
    function link : TFhirClientV; overload;

    function makeParser(fmt : TFHIRFormat) : TFHIRParser; virtual; abstract;
    function makeComposer(fmt : TFHIRFormat; style : TFHIROutputStyle) : TFHIRComposer; virtual; abstract;

    Property LangList : THTTPLanguageList read FLangList;
    property Worker : TFHIRWorkerContextV read FWorker;
    function version : TFHIRVersion; virtual; abstract;
    function address : String; // result from the communicator
    property Communicator : TFHIRClientCommunicator read FCommunicator;
    property BundleFactory : TFHIRBundleWClass read FBundleFactory;

    property format : TFHIRFormat read FFormat write SetFormat;
    property versionSpecific : boolean read FVersionSpecific write FVersionSpecific;
    property LastURL : String read FLastURL write FLastURL;
    property LastHeaders : THTTPHeaders read GetHeaders;
    property LastStatus : integer read FLastStatus write FLastStatus;
    property LastStatusMsg : String read FLastStatusMsg write FLastStatusMsg;
    property Logger : TFHIRClientLogger read FLogger write SetLogger;
    property provenance : TFhirProvenanceW read FProvenance write SetProvenance;
    property smartToken : TClientAccessToken read FSmartToken write SetSmartToken;
    property OnProgress : TFhirClientProgressEvent read FOnProgress write FOnProgress;

    // version independent API
    function conformanceV(summary : boolean) : TFHIRResourceV;
    function conformanceModeV(mode : string) : TFHIRResourceV;
    function transactionV(bundle : TFHIRResourceV) : TFHIRResourceV;
    function createResourceV(resource : TFHIRResourceV; var id : String) : TFHIRResourceV;
    function readResourceV(atype : TFhirResourceTypeV; id : String) : TFHIRResourceV;
    function vreadResourceV(atype : TFhirResourceTypeV; id, vid : String) : TFHIRResourceV;
    function updateResourceV(resource : TFHIRResourceV) : TFHIRResourceV; overload;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; params : TFHIRResourceV) : TFHIRResourceV; overload;
    function patchResourceV(atype : TFhirResourceTypeV; id : String; patch : TJsonArray) : TFHIRResourceV; overload;
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
    function historyinstanceV(atype : TFHIRResourceTypeV; id : String; allRecords : boolean; params : TStringList) : TFHIRResourceV;

    // special case that gives direct access to the communicator...
    function customGet(path : String; headers : THTTPHeaders) : TFslHTTPBuffer; virtual;
    function customPost(path : String; headers : THTTPHeaders; body : TFslHTTPBuffer) : TFslHTTPBuffer; virtual;

    procedure terminate; virtual; // only works for some communicators
  end;

implementation

{ TTextFileLogger }

constructor TTextFileLogger.Create(filename: String);
begin
  inherited Create;
  if Filename <> '' then
    FLog := TLogger.Create(Filename)
  else
    FLog := TLogger.Create(filePath(['[tmp]', PathTitle(ParamStr(0))+'.fhir.log']))
end;

destructor TTextFileLogger.Destroy;
begin
  FLog.free;
  inherited Destroy;
end;

function TTextFileLogger.toChars(b: TBytes): string;
begin
  result := TEncoding.ANSI.GetString(b);
end;

procedure TTextFileLogger.logExchange(verb, url, status, requestHeaders, responseHeaders: String; request, response: TBytes);
begin
  FLog.WriteToLog('=================================='#13#10);
  FLog.WriteToLog(verb + ' ' + url + ' HTTP/1.0'#13#10);
  FLog.WriteToLog(requestHeaders + #13#10);
  if request <> nil then
    FLog.WriteToLog(toChars(request) + #13#10);
  FLog.WriteToLog('----------------------------------'#13#10);
  FLog.WriteToLog(status + ' HTTP/1.0'#13#10);
  FLog.WriteToLog(responseHeaders + #13#10);
  if response <> nil then
    FLog.WriteToLog(toChars(response) + #13#10);
end;

{ TFslHTTPBuffer }

function TFslHTTPBuffer.link: TFslHTTPBuffer;
begin
  result := TFslHTTPBuffer(inherited link);
end;

{ TFHIRServerDetails }

destructor TFHIRServerDetails.Destroy;
begin
  FSmartConfig.free;
  inherited Destroy;
end;

function TFHIRServerDetails.link: TFHIRServerDetails;
begin
  result := TFHIRServerDetails(inherited Link);
end;

function TFHIRServerDetails.clone: TFHIRServerDetails;
begin
  result := TFHIRServerDetails(inherited Clone);
end;

procedure TFHIRServerDetails.assign(other: TFslObject);
var
  o : TFHIRServerDetails;
begin
  inherited assign(other);
  o := other as TFHIRServerDetails;
  Format := o.Format;
  Json := o.Json;
  Name := o.Name;
  URL := o.URL;
  if o.smartConfig <> nil then
    SmartConfig := TJSONParser.Parse(TJSONWriter.WriteObject(o.smartConfig));
  Version := o.Version;
  Xml := o.Xml;
  SmartMode := o.SmartMode;
  ClientId := o.ClientId;
  ClientSecret := o.ClientSecret;
  Redirect := o.Redirect;
end;

procedure TFHIRServerDetails.SetSmartConfig(AValue: TJsonObject);
begin
  FSmartConfig.free;
  FSmartConfig := AValue;
end;

{ EFHIRClientException }

constructor EFHIRClientException.Create(code : integer; message: String; issue: TFhirOperationOutcomeW);
begin
  inherited Create(message);
  FCode := code;
  FIssue := issue;
end;

constructor EFHIRClientException.Create(code : integer; message: String);
begin
  inherited Create(message);
  FCode := code;
end;

destructor EFHIRClientException.Destroy;
begin
  FIssue.free;
  inherited;
end;

{ THTTPHeaders }

procedure THTTPHeaders.addToHeaders(list: TStringList);
begin
  if (contentType <> '') then
    list.add('ContentType: '+contentType);
  if (accept <> '') then
    list.add('Accept: '+accept);
  if (prefer <> '') then
    list.add('Prefer: '+prefer);
  if (location <> '') then
    list.add('Location: '+location);
  if (lastOperationId <> '') then
    list.add('X-Last-Operation-Id: '+lastOperationId);
end;

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


function THTTPHeaders.sizeInBytes(magic : integer): cardinal;
begin
  result := sizeof(self);
  inc(result, (contentType.Length * sizeof(char))+12);
  inc(result, (accept.Length * sizeof(char))+12);
  inc(result, (prefer.Length * sizeof(char))+12);
  inc(result, (location.Length * sizeof(char))+12);
  inc(result, (contentLocation.Length * sizeof(char))+12);
  inc(result, (lastOperationId.Length * sizeof(char))+12);
  inc(result, (progress.Length * sizeof(char))+12);
end;

{ TFHIRClientLogger }

function TFHIRClientLogger.Link: TFHIRClientLogger;
begin
  result := TFHIRClientLogger(inherited Link);
end;

{ TNullLogger }

procedure TNullLogger.logExchange(verb, url, status, requestHeaders, responseHeaders: String; request, response: TBytes);
begin
  // nothing
end;

{ TFHIRClientCommunicator }

function TFHIRClientCommunicator.opWrapper: TFhirOperationOutcomeWClass;
begin
  result := FClient.opWrapper;
end;

function TFHIRClientCommunicator.ProvenanceString: string;
begin
  result := FClient.FProvenanceString;
end;

procedure TFHIRClientCommunicator.notify(msg: String);
begin
 // nothing
end;

function TFHIRClientCommunicator.getResourceVersionId(res : TFHIRResourceV) : string;
begin
  result := FClient.getResourceVersionId(res);
end;

function TFHIRClientCommunicator.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FClient.sizeInBytes(magic));
end;

{ TFhirClientV }

constructor TFhirClientV.Create(worker: TFHIRWorkerContextV; langList : THTTPLanguageList; communicator: TFHIRClientCommunicator);
begin
  inherited Create;
  FWorker := worker;
  FLangList := langList;
  FCommunicator := communicator;
  communicator.FClient := self;
  FLogger := TNullLogger.Create;
  FBundleFactory := getBundleClass;
end;

destructor TFhirClientV.Destroy;
begin
  FLangList.free;
  FSmartToken.free;
  FWorker.free;
  FCommunicator.free;
  FLogger.free;
  FProvenance.free;
  inherited;
end;

function TFhirClientV.GetHeaders: THTTPHeaders;
begin
  result := FCommunicator.FHeaders;
end;

function TFhirClientV.getResourceVersionId(res : TFHIRResourceV) : string;
begin
  raise EFHIRException.Create('Must override getResourceValue in '+className);
end;

function TFhirClientV.link: TFhirClientV;
begin
  result := TFhirClientV(inherited Link);
end;

procedure TFhirClientV.SetLogger(const Value: TFHIRClientLogger);
begin
  FLogger.free;
  FLogger := Value;
end;

procedure TFhirClientV.SetProvenance(const Value: TFhirProvenanceW);
var
  c : TFHIRComposer;
begin
  FProvenance.free;
  FProvenance := Value;
  if FProvenance <> nil then
  begin
    c := makeComposer(ffJson, OutputStyleNormal);
    try
      FProvenanceString := c.Compose(value.Resource);
    finally
      c.free;
    end;
  end
  else
    FProvenanceString := '';
end;

procedure TFhirClientV.SetFormat(fmt : TFhirFormat);
begin
  if not (fmt in [ffXml, ffJson, ffTurtle]) then
    raise EFHIRException.Create('Unsupported format in client: '+CODES_TFHIRFormat[fmt]);

  FFormat := fmt;
end;

function TFhirClientV.address : String;
begin
  result := FCommunicator.address;
end;

function TFhirClientV.conformanceV(summary : boolean) : TFHIRResourceV;
begin
  result := FCommunicator.conformanceV(summary);
end;

function TFhirClientV.conformanceModeV(mode : string) : TFHIRResourceV;
begin
  result := FCommunicator.conformanceModeV('terminology');
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

function TFhirClientV.vreadResourceV(atype : TFhirResourceTypeV; id, vid : String) : TFHIRResourceV;
begin
  result := FCommunicator.vreadResourceV(aType, id, vid);
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

function TFhirClientV.patchResourceV(atype: TFhirResourceTypeV; id: String; patch: TJsonArray): TFHIRResourceV;
begin
  result := FCommunicator.patchResourceV(atype, id, patch);
end;

function TFhirClientV.patchResourceV(atype: TFhirResourceTypeV; id: String; params: TFHIRResourceV): TFHIRResourceV;
begin
  result := FCommunicator.patchResourceV(atype, id, params);
end;

function TFhirClientV.historyTypeV(atype : TFHIRResourceTypeV; allRecords : boolean; params : TStringList) : TFHIRResourceV;
begin
  result := FCommunicator.historyTypeV(aType, allRecords, encodeParams(params));
end;

function TFhirClientV.historyinstanceV(atype: TFHIRResourceTypeV; id: String;
  allRecords: boolean; params: TStringList): TFHIRResourceV;
begin
  result := FCommunicator.historyinstanceV(aType, id, allRecords, encodeParams(params));
end;

function TFhirClientV.customGet(path : String; headers : THTTPHeaders) : TFslHTTPBuffer;
begin
  result := FCommunicator.customGet(path, headers);
end;

function TFhirClientV.customPost(path : String; headers : THTTPHeaders; body : TFslHTTPBuffer) : TFslHTTPBuffer;
begin
  result := FCommunicator.customPost(path, headers, body);
end;

procedure TFhirClientV.terminate;  // only works for some communicators
begin
  FCommunicator.terminate;
end;

procedure TFhirClientV.SetSmartToken(const Value: TClientAccessToken);
begin
  FSmartToken.free;
  FSmartToken := Value;
  // todo: set the header for the access token
end;

function TFhirClientV.encodeParams(params : TStringList) : String;
var
  i : integer;
  s, v : String;
begin
  result := '';
  if params <> nil then
  begin
    for i := 0 to params.Count - 1 do
    begin
      s := params.Names[i];
      v := params.ValueFromIndex[i];
      result := result + s+'='+EncodeMIME(v)+'&';
    end;
  end;
end;

{ TClientAccessToken }

destructor TClientAccessToken.Destroy;
begin
  FidToken.free;
  inherited;
end;

function TClientAccessToken.link: TClientAccessToken;
begin
  result := TClientAccessToken(inherited Link);
end;

procedure TClientAccessToken.SetidToken(const Value: TJWT);
begin
  FidToken.free;
  FidToken := Value;
end;

function TClientAccessToken.username: String;
begin
  if FidToken = nil then
    result := '??'
  else
    result := idtoken.name
end;

function TClientAccessToken.sizeInBytesV(magic : integer) : cardinal;
begin
  result := inherited sizeInBytesV(magic);
  inc(result, FidToken.sizeInBytes(magic));
  inc(result, (Fscopes.length * sizeof(char)) + 12);
  inc(result, (FaccessToken.length * sizeof(char)) + 12);
  inc(result, (FPatient.length * sizeof(char)) + 12);
end;

end.


