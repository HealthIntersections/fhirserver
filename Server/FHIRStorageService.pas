unit FHIRStorageService;

interface

uses
  SysUtils, Classes, System.Generics.Collections,
  KCritSct,
  AdvObjects, AdvGenerics, AdvStringMatches,  ThreadSupport,
  KDBDialects, DateAndTime,

  FHIRBase, FHIRSupport, FHIRTypes, FHIRResources, FHIRConstants, FHIRUtilities,
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

  TFHIROperationEngine = class (TAdvObject)
  private
    FOnPopulateConformance : TPopulateConformanceEvent;
  public
    Property OnPopulateConformance : TPopulateConformanceEvent read FOnPopulateConformance write FOnPopulateConformance;

    Function Execute(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String;  virtual;
    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; virtual;
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
    procedure updateOAuthSession(id : String; state, key : integer); virtual;
    procedure RegisterConsentRecord(session: TFhirSession); virtual;

    // server total counts:
    function FetchResourceCounts(cmpFilter : String) : TStringList; virtual;
    Property TotalResourceCount: integer read GetTotalResourceCount;


    procedure Sweep; virtual;
    procedure RecordFhirSession(session: TFhirSession); virtual;
    procedure CloseFhirSession(key: integer); virtual;
    procedure QueueResource(r: TFhirResource); overload; virtual;
    procedure QueueResource(r: TFhirResource; dateTime: TDateAndTime); overload; virtual;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); virtual;

    function ProfilesAsOptionList : String; virtual;

    procedure ProcessSubscriptions; virtual;
    procedure ProcessObservations; virtual;
    procedure RunValidation; virtual;

    procedure CloseAll; virtual;

    function createOperationContext(lang : String) : TFHIROperationEngine; virtual;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); virtual;
    function ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet; virtual;
    function LookupCode(system, version, code: String): String; virtual;
  end;


implementation

{ TFHIRStorageService }

procedure TFHIRStorageService.CloseAll;
begin
  raise Exception.Create('The function "CloseAll" must be overridden in '+className);
end;

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

function TFHIRStorageService.ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet;
begin
  raise Exception.Create('The function "ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet" must be overridden in '+className);
end;


function TFHIRStorageService.fetchOAuthDetails(key, status: integer; var client_id, name, redirect, state, scope: String): boolean;
begin
  raise Exception.Create('This server does not support OAuth');
end;

function TFHIRStorageService.FetchResourceCounts(cmpFilter : String): TStringList;
begin
  raise Exception.Create('The function "FetchResourceCounts(cmpFilter : String): TStringList" must be overridden in '+className);
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
  raise Exception.Create('The function "LookupCode(system, version, code: String): String" must be overridden in '+className);
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

procedure TFHIRStorageService.QueueResource(r: TFhirResource; dateTime: TDateAndTime);
begin
  raise Exception.Create('The function "QueueResource(r: TFhirResource; dateTime: TDateAndTime)" must be overridden in '+className);
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

procedure TFHIRStorageService.RunValidation;
begin
  raise Exception.Create('The function "RunValidation" must be overridden in '+className);
end;

procedure TFHIRStorageService.Sweep;
begin
  raise Exception.Create('The function "Sweep" must be overridden in '+className);
end;

procedure TFHIRStorageService.updateOAuthSession(id : String; state, key: integer);
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

function TFHIROperationEngine.Execute(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): String;
begin
  raise Exception.Create('The function "Execute(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): String" must be overridden in '+className);
end;

function TFHIROperationEngine.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
begin
  raise Exception.Create('The function "LookupReference(context: TFHIRRequest; id: String): TResourceWithReference" must be overridden in '+className);
end;

end.

