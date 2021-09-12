unit ftk_worker_server;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Forms,
  ftk_utilities, ftk_context, ftk_store,
  ftk_worker_base,
  ftk_frame_server;

type

  { TServerWorker }

  TServerWorker = class (TBaseWorker)
  private
    FServer: TFHIRServerEntry;
    procedure SetServer(AValue: TFHIRServerEntry);
  protected
    function makeFrame(owner : TComponent) : TBaseWorkerFrame; override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    property server : TFHIRServerEntry read FServer write SetServer;
    procedure serverChanged;
  end;

implementation

{ TServerWorker }

constructor TServerWorker.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  Server := context.OnFetchServer(self, session.Address.substring(16)).Link;
  Server.workerObject := self;
end;

destructor TServerWorker.Destroy;
begin
  FServer.workerObject := nil;
  FServer.Free;
  if (Context <> nil) then
    Context.OnUpdateActions(self);
  inherited Destroy;
end;

procedure TServerWorker.serverChanged;
begin
  // nothing
end;

procedure TServerWorker.SetServer(AValue: TFHIRServerEntry);
begin
  FServer.Free;
  FServer := AValue;
end;

function TServerWorker.makeFrame(owner : TComponent): TBaseWorkerFrame;
begin
  result := TServerWorkerFrame.create(owner);
  (result as TServerWorkerFrame).Server := FServer.Link;
end;

end.

