unit FHIRStorageService;

interface

uses
  SysUtils, Classes, System.Generics.Collections,
  KCritSct,
  AdvObjects, AdvGenerics, AdvStringMatches,  ThreadSupport,
  KDBManager,

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

  TFhirOperationManagerBase = class (TAdvObject)
  private
    FOnPopulateConformance : TPopulateConformanceEvent;
  public
    Property OnPopulateConformance : TPopulateConformanceEvent read FOnPopulateConformance write FOnPopulateConformance;

    Function Execute(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : String;  virtual;
    function LookupReference(context : TFHIRRequest; id : String) : TResourceWithReference; virtual;
  end;

  TFHIRStorageService = class (TAdvObject)
  protected
    FDB: TKDBManager;
    FSubscriptionManager: TSubscriptionManager;
    function GetTotalResourceCount: integer; virtual;
  public
    Constructor Create; override;
    Destructor Destroy; override;
    function Link : TFHIRStorageService; overload;
    Property DB: TKDBManager read FDB;

    Property TotalResourceCount: integer read GetTotalResourceCount;

    procedure RegisterConsentRecord(session: TFhirSession); virtual;
    function RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession; virtual;
    function GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean; virtual;
    function GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean; virtual;
    Function CreateImplicitSession(clientInfo: String; server: Boolean) : TFhirSession; virtual;
    Procedure EndSession(sCookie, ip: String); virtual;
    function isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean; virtual;
    procedure MarkSessionChecked(sCookie, sName: String); virtual;
    function DumpSessions : String; virtual;
    procedure Sweep; virtual;

    function ProfilesAsOptionList : String; virtual;

    procedure ProcessSubscriptions; virtual;
    procedure ProcessObservations; virtual;
    procedure RunValidation; virtual;

    procedure CloseAll; virtual;
    property SubscriptionManager : TSubscriptionManager read FSubscriptionManager;

    function createOperationContext(lang : String) : TFhirOperationManagerBase; virtual;
    Procedure Yield(op : TFhirOperationManagerBase; exception : Exception); virtual;
    function ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet; virtual;
    function LookupCode(system, version, code: String): String; virtual;
  end;


implementation

{ TFHIRStorageService }

procedure TFHIRStorageService.CloseAll;
begin
  raise Exception.Create('The function "CloseAll" must be overridden in '+className);
end;

constructor TFHIRStorageService.Create;
begin
  inherited;
end;

function TFHIRStorageService.CreateImplicitSession(clientInfo: String;  server: Boolean): TFhirSession;
begin
  raise Exception.Create('The function "CreateImplicitSession(clientInfo: String;  server: Boolean): TFhirSession" must be overridden in '+className);
end;

function TFHIRStorageService.createOperationContext(lang: String): TFhirOperationManagerBase;
begin
  raise Exception.Create('The function "createOperationContext(lang: String): TFhirOperationManagerBase" must be overridden in '+className);
end;

destructor TFHIRStorageService.Destroy;
begin
  inherited;
end;

function TFHIRStorageService.DumpSessions: String;
begin
  raise Exception.Create('The function "DumpSessions: String" must be overridden in '+className);
end;

procedure TFHIRStorageService.EndSession(sCookie, ip: String);
begin
  raise Exception.Create('The function "EndSession(sCookie, ip: String)" must be overridden in '+className);
end;

function TFHIRStorageService.ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet;
begin
  raise Exception.Create('The function "ExpandVS(vs: TFHIRValueSet; ref: TFhirReference; limit, count, offset: integer; allowIncomplete: Boolean; dependencies: TStringList): TFHIRValueSet" must be overridden in '+className);
end;

function TFHIRStorageService.GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean;
begin
  raise Exception.Create('The function "GetSession(sCookie: String; var session: TFhirSession; var check: Boolean): Boolean" must be overridden in '+className);
end;

function TFHIRStorageService.GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean;
begin
  raise Exception.Create('The function "GetSessionByToken(outerToken: String; var session: TFhirSession): Boolean" must be overridden in '+className);
end;

function TFHIRStorageService.GetTotalResourceCount: integer;
begin
  raise Exception.Create('The function "GetTotalResourceCount: integer" must be overridden in '+className);
end;

function TFHIRStorageService.isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean;
begin
  raise Exception.Create('The function "isOkBearer(token, clientInfo: String; var session: TFhirSession): Boolean" must be overridden in '+className);
end;

function TFHIRStorageService.Link: TFHIRStorageService;
begin
  result := TFHIRStorageService(inherited Link);
end;

function TFHIRStorageService.LookupCode(system, version, code: String): String;
begin
  raise Exception.Create('The function "LookupCode(system, version, code: String): String" must be overridden in '+className);
end;

procedure TFHIRStorageService.MarkSessionChecked(sCookie, sName: String);
begin
  raise Exception.Create('The function "MarkSessionChecked(sCookie, sName: String)" must be overridden in '+className);
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

procedure TFHIRStorageService.RegisterConsentRecord(session: TFhirSession);
begin
  raise Exception.Create('The function "RegisterConsentRecord(session: TFhirSession)" must be overridden in '+className);
end;

function TFHIRStorageService.RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession;
begin
  raise Exception.Create('The function "RegisterSession(provider: TFHIRAuthProvider; innerToken, outerToken, id, name, email, original, expires, ip, rights: String): TFhirSession" must be overridden in '+className);
end;

procedure TFHIRStorageService.RunValidation;
begin
  raise Exception.Create('The function "RunValidation" must be overridden in '+className);
end;

procedure TFHIRStorageService.Sweep;
begin
  raise Exception.Create('The function "Sweep" must be overridden in '+className);
end;

procedure TFHIRStorageService.Yield(op: TFhirOperationManagerBase; exception : Exception);
begin
  raise Exception.Create('The function "Yield(op: TFhirOperationManagerBase; exception : Exception)" must be overridden in '+className);
end;

{ TFhirOperationManagerBase }


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


{ TFhirOperationManagerBase }

function TFhirOperationManagerBase.Execute(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): String;
begin
  raise Exception.Create('The function "Execute(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): String" must be overridden in '+className);
end;

function TFhirOperationManagerBase.LookupReference(context: TFHIRRequest; id: String): TResourceWithReference;
begin
  raise Exception.Create('The function "LookupReference(context: TFHIRRequest; id: String): TResourceWithReference" must be overridden in '+className);
end;

end.

