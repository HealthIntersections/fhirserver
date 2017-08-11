unit VocabPocServerCore;

interface

uses
  SysUtils, Classes,
  DateSupport, HashSupport,
  FHIRSupport, FHIRResources, SCIMObjects,
  FHIRStorageService, FHIRUserProvider;

type
  TTerminologyServerUserProvider = class (TFHIRUserProvider)
  public
    Function loadUser(key : integer) : TSCIMUser; overload; override;
    Function loadUser(id : String; var key : integer) : TSCIMUser; overload; override;
    function CheckLogin(username, password : String; var key : integer) : boolean; override;
    function CheckId(id : String; var username, hash : String) : boolean; override;
    function loadOrCreateUser(id, name, email : String; var key : integer) : TSCIMUser; override;
    function allowInsecure : boolean; override;
  end;

  TTerminologyServerOperationEngine = class (TFHIROperationEngine)
  protected
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;

    function ExecuteRead(request: TFHIRRequest; response : TFHIRResponse; ignoreHeaders : boolean) : boolean; override;
    function ExecuteCreate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse; idState : TCreateIdState; iAssignedKey : Integer) : String; override;
    function ExecuteUpdate(context : TOperationContext; request: TFHIRRequest; response : TFHIRResponse) : Boolean; override;
  end;


  TTerminologyServerStorage = class (TFHIRStorageService)
  protected
    function GetTotalResourceCount: integer; override;
  public
    Constructor create();
    Destructor Destroy; override;

    // no OAuth Support

    // server total counts:
    function FetchResourceCounts(comps : String) : TStringList; override;

    procedure RecordFhirSession(session: TFhirSession); override;
    procedure CloseFhirSession(key: integer); override;
    procedure QueueResource(r: TFhirResource); overload; override;
    procedure QueueResource(r: TFhirResource; dateTime: TDateTimeEx); overload; override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;

    function ProfilesAsOptionList : String; override;

    procedure ProcessSubscriptions; override;
    procedure ProcessObservations; override;
    procedure RunValidation; override;

    function createOperationContext(lang : String) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); override;
  end;

implementation

{ TTerminologyServerStorage }

procedure TTerminologyServerStorage.CloseFhirSession(key: integer);
begin
  // nothing;
end;

constructor TTerminologyServerStorage.create;
begin
  inherited Create;
  // nothing
end;

function TTerminologyServerStorage.createOperationContext(lang: String): TFHIROperationEngine;
begin
  result := TTerminologyServerOperationEngine.create(lang);
end;

destructor TTerminologyServerStorage.Destroy;
begin
  // nohting
  inherited;
end;

function TTerminologyServerStorage.FetchResourceCounts(comps: String): TStringList;
begin
  result := TStringList.create;
end;

function TTerminologyServerStorage.GetTotalResourceCount: integer;
begin
  result := 0;
end;

procedure TTerminologyServerStorage.ProcessObservations;
begin
  // nothing
end;

procedure TTerminologyServerStorage.ProcessSubscriptions;
begin
  // nothing
end;

function TTerminologyServerStorage.ProfilesAsOptionList: String;
begin
  // nothing
end;

procedure TTerminologyServerStorage.QueueResource(r: TFhirResource; dateTime: TDateTimeEx);
begin
  // nothing
end;

procedure TTerminologyServerStorage.QueueResource(r: TFhirResource);
begin
  // nothing
end;

procedure TTerminologyServerStorage.RecordFhirSession(session: TFhirSession);
begin
  // nothing
end;

procedure TTerminologyServerStorage.RegisterAuditEvent(session: TFhirSession; ip: String);
begin
  // nothing
end;

procedure TTerminologyServerStorage.RunValidation;
begin
  // nothing
end;

procedure TTerminologyServerStorage.Yield(op: TFHIROperationEngine; exception: Exception);
begin
  op.Free;
end;

{ TTerminologyServerUserProvider }

function TTerminologyServerUserProvider.allowInsecure: boolean;
begin
  result := true;
end;

function TTerminologyServerUserProvider.CheckId(id: String; var username, hash: String): boolean;
begin
  if (id = 'user') then
  begin
    result := false;
    userName := 'User';
    hash := inttostr(HashStringToCode32('User'));
  end
  else
    result := false;
end;

function TTerminologyServerUserProvider.CheckLogin(username, password: String;
  var key: integer): boolean;
begin

end;

function TTerminologyServerUserProvider.loadOrCreateUser(id, name,
  email: String; var key: integer): TSCIMUser;
begin

end;

function TTerminologyServerUserProvider.loadUser(key: integer): TSCIMUser;
begin

end;

function TTerminologyServerUserProvider.loadUser(id: String;
  var key: integer): TSCIMUser;
begin

end;

{ TTerminologyServerOperationEngine }

procedure TTerminologyServerOperationEngine.CommitTransaction;
begin
  // nothing
end;

function TTerminologyServerOperationEngine.ExecuteCreate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse; idState: TCreateIdState; iAssignedKey: Integer): String;
begin
  // nothing
end;

function TTerminologyServerOperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse; ignoreHeaders: boolean): boolean;
begin
  // nothing
end;

function TTerminologyServerOperationEngine.ExecuteUpdate(context: TOperationContext; request: TFHIRRequest; response: TFHIRResponse): Boolean;
begin
  // nothing
end;

procedure TTerminologyServerOperationEngine.RollbackTransaction;
begin
  // nothing
end;

procedure TTerminologyServerOperationEngine.StartTransaction;
begin
  // nothing
end;

end.
