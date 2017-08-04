unit FHIRStorageService;

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
  SysUtils, Classes, System.Generics.Collections,
  KCritSct, StringSupport,
  AdvObjects, AdvGenerics, AdvStringMatches, AdvNames, ThreadSupport,
  KDBDialects, DateSupport,

  FHIRBase, FHIRSupport, FHIRTypes, FHIRResources, FHIRConstants, FHIRUtilities, FHIRLang,
  FHIRValidator, ServerValidator, FHIRSubscriptionManager, ServerUtilities;


Type

  TPopulateConformanceEvent = procedure (sender : TObject; conf : TFhirCapabilityStatement) of object;

  TOperationContext = class (TAdvObject)
  private
    FUpload : boolean;
    FCallback : TInstallerCallback;
    FMessage : String;
  public
    constructor Create; overload; override;
    constructor Create(upload : boolean; callback : TInstallerCallback; message : String); overload;

    property upload : boolean read FUpload write FUpload;
    property callback : TInstallerCallback read FCallback write FCallback;
    property message : String read FMessage write FMessage;

    procedure progress(i : integer);
  end;

  TFHIRStorageService = class;

  TMatchingResource = class (TAdvName)
  public
    key : integer;
    version : integer;
  end;

  TMatchingResourceList = class (TAdvNameList)
  private
    function GetEntry(iIndex: Integer): TMatchingResource;
  public
    Property entries[iIndex : Integer] : TMatchingResource Read GetEntry; Default;
  end;


  TFHIROperationEngine = class (TAdvObject)
  private
    FOnPopulateConformance : TPopulateConformanceEvent;
    FLang : String;
  protected
    procedure StartTransaction; virtual;
    procedure CommitTransaction; virtual;
    procedure RollbackTransaction; virtual;

    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; virtual;
    function  ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; virtual;
    function  ExecutePatch(request: TFHIRRequest; response : TFHIRResponse) : Boolean; virtual;
    procedure ExecuteVersionRead(request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteDelete(request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteHistory(request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteSearch(request: TFHIRRequest; response : TFHIRResponse); virtual;
    Function  ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String; virtual;
    procedure ExecuteConformanceStmt(request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteUpload(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); virtual;
    function  ExecuteValidation(request: TFHIRRequest; response : TFHIRResponse; opDesc : String) : boolean; virtual;
    procedure ExecuteTransaction(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteBatch(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); virtual;
    procedure ExecuteOperation(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse); virtual;

    procedure NoMatch(request: TFHIRRequest; response: TFHIRResponse);
    procedure NotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure VersionNotFound(request: TFHIRRequest; response : TFHIRResponse);
    procedure TypeNotFound(request: TFHIRRequest; response : TFHIRResponse);

  public
    constructor create(lang : String);

    Property OnPopulateConformance : TPopulateConformanceEvent read FOnPopulateConformance write FOnPopulateConformance;
    property lang : String read FLang write FLang;

    function check(response : TFHIRResponse; test : boolean; code : Integer; lang, message : String; issueCode : TFhirIssueTypeEnum) : Boolean; virtual;
    Function Execute(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String;  virtual;
    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; virtual;
    function getResourcesByParam(aType : TFhirResourceType; name, value : string; var needSecure : boolean): TAdvList<TFHIRResource>; virtual;
    function FindResource(aType, sId : String; bAllowDeleted : boolean; var resourceKey, versionKey : integer; request: TFHIRRequest; response: TFHIRResponse; compartments : String): boolean; virtual;
    function GetResourceByKey(key : integer; var needSecure : boolean): TFHIRResource; virtual;
    function ResolveSearchId(resourceName, compartmentId, compartments : String; baseURL, params : String) : TMatchingResourceList; virtual;
  end;

  TFHIRStorageService = class (TAdvObject)
  protected
    function GetTotalResourceCount: integer; virtual;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    function Link : TFHIRStorageService; overload;

    // OAuth Support
    procedure recordOAuthLogin(id, client_id, scope, redirect_uri, state : String); virtual;
    function fetchOAuthDetails(key, status : integer; var client_id, name, redirect, state, scope : String) : boolean; virtual;
    procedure recordOAuthChoice(id : String; scopes, jwt, patient : String); virtual;
    function hasOAuthSession(id : String; status : integer) : boolean; virtual;
    function hasOAuthSessionByKey(key, status : integer) : boolean; virtual;
    procedure updateOAuthSession(id : String; state, key : integer; var client_id : String); virtual;
    procedure RegisterConsentRecord(session: TFhirSession); virtual;
    function FetchAuthorization(hash : String; var PatientId : String; var ConsentKey, SessionKey : Integer; var Expiry : TDateTime; var jwt : String) : boolean; virtual;

    // server total counts:
    function FetchResourceCounts(comps : String) : TStringList; virtual; // comps = comma delimited list of patient compartments
    Property TotalResourceCount: integer read GetTotalResourceCount;


    procedure Sweep; virtual;
    procedure RecordFhirSession(session: TFhirSession); virtual;
    procedure CloseFhirSession(key: integer); virtual;
    procedure QueueResource(r: TFhirResource); overload; virtual;
    procedure QueueResource(r: TFhirResource; dateTime: TDateTimeEx); overload; virtual;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); virtual;
    function RetrieveSession(key : integer; var UserKey, Provider : integer; var Id, Name, Email : String) : boolean; virtual;

    function ProfilesAsOptionList : String; virtual;

    procedure ProcessSubscriptions; virtual;
    procedure ProcessEmails; virtual;
    procedure ProcessObservations; virtual;
    procedure RunValidation; virtual;


    function createOperationContext(lang : String) : TFHIROperationEngine; virtual;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); virtual;
    function ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; lang : String; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet; virtual;
    function LookupCode(system, version, code: String): String; virtual;
    function FetchResource(key : integer) : TFHIRResource; virtual;
  end;


implementation

{ TFHIRStorageService }

procedure TFHIRStorageService.CloseFhirSession(key: integer);
begin
  raise Exception.Create('The function "CloseFhirSession(key: integer)" must be overridden in '+className);
end;

constructor TFHIRStorageService.Create;
begin
  inherited;
end;

function TFHIRStorageService.createOperationContext(lang: String): TFHIROperationEngine;
begin
  raise Exception.Create('The function "createOperationContext(lang: String): TFHIROperationEngine" must be overridden in '+className);
end;

destructor TFHIRStorageService.Destroy;
begin
  inherited;
end;

function TFHIRStorageService.ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; lang : String; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet;
begin
  raise Exception.Create('Expanding valuesets is not implemented in this server');
end;


function TFHIRStorageService.FetchAuthorization(hash: string; var PatientId : string; var ConsentKey, SessionKey: Integer; var Expiry: TDateTime; var jwt: String): boolean;
begin
  raise Exception.Create('This server does not support OAuth');
end;

function TFHIRStorageService.fetchOAuthDetails(key, status: integer; var client_id, name, redirect, state, scope: String): boolean;
begin
  raise Exception.Create('This server does not support OAuth');
end;

function TFHIRStorageService.FetchResource(key: integer): TFHIRResource;
begin
  raise Exception.Create('The function "FetchResource()" must be overridden in '+className);
end;

function TFHIRStorageService.FetchResourceCounts(comps : String): TStringList;
begin
  raise Exception.Create('The function "FetchResourceCounts(comps : String): TStringList" must be overridden in '+className);
end;

function TFHIRStorageService.GetTotalResourceCount: integer;
begin
  raise Exception.Create('The function "GetTotalResourceCount: integer" must be overridden in '+className);
end;

function TFHIRStorageService.hasOAuthSession(id: String; status : integer): boolean;
begin
  raise Exception.Create('This server does not support OAuth');
end;

function TFHIRStorageService.hasOAuthSessionByKey(key, status: integer): boolean;
begin
  raise Exception.Create('This server does not support OAuth');
end;

function TFHIRStorageService.Link: TFHIRStorageService;
begin
  result := TFHIRStorageService(inherited Link);
end;

function TFHIRStorageService.LookupCode(system, version, code: String): String;
begin
  raise Exception.Create('Looking up codes is not implemented in this server');
end;

procedure TFHIRStorageService.ProcessEmails;
begin
  raise Exception.Create('The function "ProcessEmails" must be overridden in '+className);
end;

procedure TFHIRStorageService.ProcessObservations;
begin
  raise Exception.Create('The function "ProcessObservations" must be overridden in '+className);
end;

procedure TFHIRStorageService.ProcessSubscriptions;
begin
  raise Exception.Create('The function "ProcessSubscriptions" must be overridden in '+className);
end;

function TFHIRStorageService.ProfilesAsOptionList: String;
begin
  raise Exception.Create('The function "ProfilesAsOptionList: String" must be overridden in '+className);
end;

procedure TFHIRStorageService.QueueResource(r: TFhirResource);
begin
  raise Exception.Create('The function "QueueResource(r: TFhirResource)" must be overridden in '+className);
end;

procedure TFHIRStorageService.QueueResource(r: TFhirResource; dateTime: TDateTimeEx);
begin
  raise Exception.Create('The function "QueueResource(r: TFhirResource; dateTime: TDateTimeEx)" must be overridden in '+className);
end;

procedure TFHIRStorageService.RecordFhirSession(session: TFhirSession);
begin
  raise Exception.Create('The function "RecordFhirSession(session: TFhirSession)" must be overridden in '+className);
end;

procedure TFHIRStorageService.recordOAuthChoice(id, scopes, jwt, patient: String);
begin
  raise Exception.Create('This server does not support OAuth');
end;

procedure TFHIRStorageService.recordOAuthLogin(id, client_id, scope, redirect_uri, state: String);
begin
  raise Exception.Create('This server does not support OAuth');
end;

procedure TFHIRStorageService.RegisterAuditEvent(session: TFhirSession; ip: String);
begin
  raise Exception.Create('The function "RegisterAuditEvent(session: TFhirSession; ip: String)" must be overridden in '+className);
end;

procedure TFHIRStorageService.RegisterConsentRecord(session: TFhirSession);
begin
  raise Exception.Create('This server does not support OAuth');
end;

function TFHIRStorageService.RetrieveSession(key: integer; var UserKey, Provider: integer; var Id, Name, Email: String): boolean;
begin
  raise Exception.Create('The function "RegisterAuditEvent(session: TFhirSession; ip: String)" must be overridden in '+className);
end;

procedure TFHIRStorageService.RunValidation;
begin
  raise Exception.Create('The function "RunValidation" must be overridden in '+className);
end;

procedure TFHIRStorageService.Sweep;
begin
  raise Exception.Create('The function "Sweep" must be overridden in '+className);
end;

procedure TFHIRStorageService.updateOAuthSession(id : String; state, key: integer; var client_id : String);
begin
  raise Exception.Create('This server does not support OAuth');
end;

procedure TFHIRStorageService.Yield(op: TFHIROperationEngine; exception : Exception);
begin
  raise Exception.Create('The function "Yield(op: TFHIROperationEngine; exception : Exception)" must be overridden in '+className);
end;

{ TFHIROperationEngine }


{ TOperationContext }

constructor TOperationContext.Create;
begin
  inherited Create;
end;

constructor TOperationContext.Create(upload: boolean; callback: TInstallerCallback; message : String);
begin
  Create;
  FUpload := upload;
  FCallback := callback;
  FMessage := message;
end;

procedure TOperationContext.progress(i: integer);
begin
  if assigned(FCallback) then
    FCallback(i, FMessage);
end;


{ TFHIROperationEngine }

procedure TFHIROperationEngine.CommitTransaction;
begin
  raise Exception.Create('The function "CommitTransaction" must be overridden in '+className);
end;

constructor TFHIROperationEngine.create(lang: String);
begin
  inherited create;
  FLang := lang;
end;

procedure TFHIROperationEngine.NoMatch(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_MATCH', lang), [request.ResourceName+'?'+request.parameters.source]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

procedure TFHIROperationEngine.NotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.ResourceName+':'+request.Id]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

procedure TFHIROperationEngine.VersionNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_NO_EXIST', lang), [request.ResourceName+':'+request.Id+'/_history/'+request.subId]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;


procedure TFHIROperationEngine.TypeNotFound(request: TFHIRRequest; response: TFHIRResponse);
begin
  response.HTTPCode := 404;
  response.Message := StringFormat(GetFhirMessage('MSG_UNKNOWN_TYPE', lang), [request.ResourceName]);
  response.Body := response.Message;
  response.Resource := BuildOperationOutcome(lang, response.Message);
end;

function TFHIROperationEngine.check(response: TFHIRResponse; test: boolean; code : Integer; lang, message: String; issueCode : TFhirIssueTypeEnum): Boolean;
begin
  result := test;
  if not test and (response <> nil) then
  begin
    response.HTTPCode := code;
    response.Message := message;
    response.ContentType := 'text/plain';
    response.Body := message;
    response.Resource := BuildOperationOutcome(lang, message, issueCode);
  end;
end;



function TFHIROperationEngine.Execute(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): String;
begin
  StartTransaction;
  try
    result := Request.Id;
    case request.CommandType of
      fcmdRead : ExecuteRead(request, response, false);
      fcmdUpdate : ExecuteUpdate(context, request, response);
      fcmdVersionRead : ExecuteVersionRead(request, response);
      fcmdDelete : ExecuteDelete(request, response);
      fcmdHistoryInstance, fcmdHistoryType, fcmdHistorySystem : ExecuteHistory(request, response);
      fcmdSearch : ExecuteSearch(request, response);
      fcmdCreate : result := ExecuteCreate(context, request, response, request.NewIdStatus, 0);
      fcmdConformanceStmt : ExecuteConformanceStmt(request, response);
      fcmdTransaction : ExecuteTransaction(context, request, response);
      fcmdBatch : ExecuteBatch(context, request, response);
      fcmdOperation : ExecuteOperation(context, request, response);
      fcmdUpload : ExecuteUpload(context, request, response);
      fcmdPatch : ExecutePatch(request, response);
      fcmdValidate : ExecuteValidation(request, response, 'Validation')
    else
      Raise Exception.Create(GetFhirMessage('MSG_UNKNOWN_OPERATION', lang));
    End;

    CommitTransaction;
  except
    on e:exception do
    begin
      RollbackTransaction;
      raise;
    end;
  end;
end;

procedure TFHIROperationEngine.ExecuteBatch(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('This server does not implement the "Batch" function');
end;

procedure TFHIROperationEngine.ExecuteConformanceStmt(request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('This server does not implement the "ConformanceStmt" function');
end;

function TFHIROperationEngine.ExecuteCreate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse; idState: TCreateIdState; iAssignedKey: Integer): String;
begin
  raise Exception.Create('This server does not implement the "Create" function');
end;

procedure TFHIROperationEngine.ExecuteDelete(request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('This server does not implement the "Delete" function');
end;

procedure TFHIROperationEngine.ExecuteHistory(request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('This server does not implement the "History" function');
end;

procedure TFHIROperationEngine.ExecuteOperation(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('This server does not implement the "Operation" function');
end;

function TFHIROperationEngine.ExecutePatch(request: TFHIRRequest; response: TFHIRResponse): Boolean;
begin
  raise Exception.Create('This server does not implement the "Patch" function');
end;

function TFHIROperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse; ignoreHeaders : boolean) : boolean;
begin
  raise Exception.Create('This server does not implement the "Read" function');
end;

procedure TFHIROperationEngine.ExecuteSearch(request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('This server does not implement the "Search" function');
end;

procedure TFHIROperationEngine.ExecuteTransaction(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('This server does not implement the "Transaction" function');
end;

function TFHIROperationEngine.ExecuteUpdate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): Boolean;
begin
  raise Exception.Create('This server does not implement the "Update" function');
end;

procedure TFHIROperationEngine.ExecuteUpload(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('This server does not implement the "Upload" function');
end;

function TFHIROperationEngine.ExecuteValidation(request: TFHIRRequest; response: TFHIRResponse; opDesc: String): boolean;
begin
  raise Exception.Create('This server does not implement the "Validation" function');
end;

procedure TFHIROperationEngine.ExecuteVersionRead(request: TFHIRRequest; response: TFHIRResponse);
begin
  raise Exception.Create('This server does not implement the "VersionRead" function');
end;

function TFHIROperationEngine.FindResource(aType, sId: String; bAllowDeleted: boolean; var resourceKey, versionKey: integer; request: TFHIRRequest; response: TFHIRResponse; compartments: String): boolean;
begin
  raise Exception.Create('This server does not implement the "FindResource" function');
end;

function TFHIROperationEngine.GetResourceByKey(key: integer; var needSecure: boolean): TFHIRResource;
begin
  raise Exception.Create('This server does not implement the "GetResourceByKey" function');
end;

function TFHIROperationEngine.getResourcesByParam(aType: TFhirResourceType; name, value: string; var needSecure: boolean): TAdvList<TFHIRResource>;
begin
  raise Exception.Create('This server does not implement the "getResourcesByParam" function');
end;

function TFHIROperationEngine.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
begin
  raise Exception.Create('The function "LookupReference(context: TFHIRRequest; id: String): TResourceWithReference" must be overridden in '+className);
end;

function TFHIROperationEngine.ResolveSearchId(resourceName, compartmentId, compartments, baseURL, params: String): TMatchingResourceList;
begin
  raise Exception.Create('This server does not implement the "GetResourceByKey" function');
end;

procedure TFHIROperationEngine.RollbackTransaction;
begin
  raise Exception.Create('The function "RollbackTransaction" must be overridden in '+className);
end;

procedure TFHIROperationEngine.StartTransaction;
begin
  raise Exception.Create('The function "StartTransaction" must be overridden in '+className);
end;

{ TMatchingResourceList }

function TMatchingResourceList.GetEntry(iIndex: Integer): TMatchingResource;
begin
  result := TMatchingResource(ObjectByIndex[iIndex]);
end;


end.

