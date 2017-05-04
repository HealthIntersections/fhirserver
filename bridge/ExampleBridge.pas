{
This unit shows an example of how to integrate the FHIR Server into another
application server. It instantiates a FHIR Server, and provides storage
to allow the FHIR server to expose application functionality.

This example FHIR server pretends to provide meaningful patient services
}

unit ExampleBridge;

interface

Uses
  SysUtils, Classes, IniFiles,
  DateAndTime,
  FHIRTypes, FHIRResources, FHIRConstants, FHIRSupport, FHIRUtilities,
  FHIRServerContext, FHIRStorageService, FHIRRestServer;

Type
  TExampleFHIROperationEngine = class (TFHIROperationEngine)
  private
    function makeTestPatient : TFHIRPatient;
    procedure patientRead(request: TFHIRRequest; response : TFHIRResponse);
  protected
    procedure StartTransaction; override;
    procedure CommitTransaction; override;
    procedure RollbackTransaction; override;
    procedure ExecuteRead(request: TFHIRRequest; response : TFHIRResponse); override;
  end;

  TExampleFhirServerStorage = class (TFHIRStorageService)
  private
  protected
    function GetTotalResourceCount: integer; override;
  public
    Constructor Create; override;
    Destructor Destroy; override;

    // no OAuth Support

    // server total counts:
    function FetchResourceCounts(comps : String) : TStringList; override;

    procedure RecordFhirSession(session: TFhirSession); override;
    procedure CloseFhirSession(key: integer); override;
    procedure QueueResource(r: TFhirResource); overload; override;
    procedure QueueResource(r: TFhirResource; dateTime: TDateAndTime); overload; override;
    procedure RegisterAuditEvent(session: TFhirSession; ip: String); override;

    function ProfilesAsOptionList : String; override;

    procedure ProcessSubscriptions; override;
    procedure ProcessObservations; override;
    procedure RunValidation; override;

    function createOperationContext(lang : String) : TFHIROperationEngine; override;
    Procedure Yield(op : TFHIROperationEngine; exception : Exception); override;
  end;

  TExampleFhirServer = class
  private
    FIni : TMemIniFile;
    FIPMask: String;
    FPort: word;
    FIPClient: String;
    FDataModuleMain: TObject;
    FSystemName : String;
    FSystemUID: String;

    FWebServer : TFhirWebServer;
  public
    Constructor Create;
    Destructor Destroy; Override;

    Property Port: word read FPort write FPort;
    Property IPClient : String read FIPClient write FIPCLient;
    Property IPMask : String read FIPMask write FIPMask;
    Property DataModuleMain : TObject read FDataModuleMain write FDataModuleMain;
    Property SystemName : String read FSystemName write FSystemName;
    Property SystemUID : String read FSystemUID write FSystemUID;

    Procedure Start;
    Procedure Stop;
  end;

implementation

{ TExampleFhirServer }

constructor TExampleFhirServer.Create;
begin
  inherited Create;
  FIni := TMemIniFile.Create('');
end;

destructor TExampleFhirServer.Destroy;
begin
  FIni.Free;
  inherited;
end;

procedure TExampleFhirServer.Start;
var
  ctxt : TFHIRServerContext;
  store : TExampleFhirServerStorage;
begin
//  FTerminologyServer := TTerminologyServer.create(FDB.Link);
//  FTerminologyServer.load(FIni);

  store := TExampleFhirServerStorage.create();
  try
    ctxt := TFHIRServerContext.Create(store.Link);
    try
//      ctxt.TerminologyServer := FterminologyServer.Link;
      ctxt.ownername := FSystemName;
      FWebServer := TFhirWebServer.create(FIni.FileName, nil, FSystemname, nil {FTerminologyServer}, ctxt.link);
      FWebServer.Start(true);
    finally
      ctxt.free;
    end;
  finally
    store.free;
  end;
end;

procedure TExampleFhirServer.Stop;
begin
  FWebServer.Stop;
  FWebServer.Free;
end;

{ TExampleFHIROperationEngine }

procedure TExampleFHIROperationEngine.CommitTransaction;
begin
  // nothing (at least, for now)
end;

procedure TExampleFHIROperationEngine.ExecuteRead(request: TFHIRRequest; response: TFHIRResponse);
begin
  case request.ResourceEnum of
    frtPatient: patientRead(request, response);
  else
    raise Exception.Create('The resource "'+request.ResourceName+'" is not supported by this server');
  end;
end;

function TExampleFHIROperationEngine.makeTestPatient: TFHIRPatient;
begin
  result := TFhirPatient.Create;
  try
    result.active := true;
    result.nameList.Append.text := 'Test Name';
    result.Link;
  finally
    result.Free;
  end;
end;

procedure TExampleFHIROperationEngine.patientRead(request: TFHIRRequest; response: TFHIRResponse);
begin
  if request.Id = 'example' then
  begin
    response.HTTPCode := 200;
    response.Message := 'OK';
    response.Resource := makeTestPatient;
  end
  else
  begin
    response.HTTPCode := 404;
    response.Message := 'Not Found';
    response.Resource := BuildOperationOutcome(lang, 'not found', IssueTypeUnknown);
  end;
end;

procedure TExampleFHIROperationEngine.RollbackTransaction;
begin
  // nothing (at least, for now)
end;

procedure TExampleFHIROperationEngine.StartTransaction;
begin
  // nothing (at least, for now)
end;

{ TExampleFhirServerStorage }

constructor TExampleFhirServerStorage.Create;
begin
  inherited;

end;

destructor TExampleFhirServerStorage.Destroy;
begin

  inherited;
end;

procedure TExampleFhirServerStorage.RecordFhirSession(session: TFhirSession);
begin
  // this server doesn't track sessions
end;

procedure TExampleFhirServerStorage.CloseFhirSession(key: integer);
begin
  // this server doesn't track sessions
end;

function TExampleFhirServerStorage.createOperationContext(lang: String): TFHIROperationEngine;
begin
  result := TExampleFHIROperationEngine.create(lang);
end;


function TExampleFhirServerStorage.FetchResourceCounts(comps: String): TStringList;
begin
  // ignore comps for now
  result := TStringList.Create;
  result.AddObject('Patient', TObject(1));
end;

function TExampleFhirServerStorage.GetTotalResourceCount: integer;
begin
  result := 1;
end;


procedure TExampleFhirServerStorage.ProcessObservations;
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.ProcessSubscriptions;
begin
  // nothing in this server
end;

function TExampleFhirServerStorage.ProfilesAsOptionList: String;
begin
  result := '';
end;

procedure TExampleFhirServerStorage.QueueResource(r: TFhirResource);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.QueueResource(r: TFhirResource; dateTime: TDateAndTime);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.RegisterAuditEvent(session: TFhirSession; ip: String);
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.RunValidation;
begin
  // nothing in this server
end;

procedure TExampleFhirServerStorage.Yield(op: TFHIROperationEngine; exception: Exception);
begin
  op.Free;
end;

end.
