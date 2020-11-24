unit txweb_endpoint;

{$i fhir.inc}

interface

uses
  fsl_base,
  fdb_manager,
  server_config,
  endpoint;

type
  TTerminologyWebEndPoint = class (TFHIRServerEndPoint)
  private
  public
    constructor Create(settings : TFHIRServerConfigSection; db : TFDBManager);
    destructor Destroy; override;
  end;

implementation

{ TTerminologyWebEndPoint }

constructor TTerminologyWebEndPoint.Create(settings : TFHIRServerConfigSection; db : TFDBManager);
begin

end;

destructor TTerminologyWebEndPoint.Destroy;
begin

  inherited;
end;

end.
