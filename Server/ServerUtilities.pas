unit ServerUtilities;

interface

uses
  AdvObjects,
  FHIRResources;

type
  TFHIRResourceConfig = class (TAdvObject)
  public
    name : String;
    key: integer;
    Supported: Boolean;
    IdGuids: Boolean;
    IdClient: Boolean;
    IdServer: Boolean;
    cmdUpdate: Boolean;
    cmdDelete: Boolean;
    cmdValidate: Boolean;
    cmdHistoryInstance: Boolean;
    cmdHistoryType: Boolean;
    cmdSearch: Boolean;
    cmdCreate: Boolean;
    cmdOperation: Boolean;
    versionUpdates: Boolean;
    lastResourceId : integer;

    constructor Create; override;
  end;

  TFHIRServerWorker = class (TAdvObject)
  private
    FServerContext : TAdvObject; // no link
  public
    Constructor Create(ServerContext : TAdvObject);
    Destructor Destroy; override;

    Property ServerContext : TAdvObject read FServerContext;
  end;

implementation

constructor TFHIRResourceConfig.Create;
begin
  inherited;
  Supported := true;
  IdGuids := false;
  IdClient := true;
  IdServer := true;
  cmdUpdate := true;
  cmdDelete := true;
  cmdValidate := true;
  cmdHistoryInstance := true;
  cmdHistoryType := true;
  cmdSearch := true;
  cmdCreate := true;
  cmdOperation := true;
  versionUpdates := false;
  lastResourceId  := 0;
end;

{ TFHIRServerWorker }

constructor TFHIRServerWorker.Create(ServerContext: TAdvObject);
begin
  inherited Create;
  FServerContext := ServerContext;
end;

destructor TFHIRServerWorker.Destroy;
begin
  inherited;
end;

end.
