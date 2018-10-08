unit FHIR.Server.v2Server;

interface

uses
  FHIR.Support.Base,
  FHIR.v2.Protocol,
  FHIR.Server.Web;

type
  Tv2ServerInstance = class (TFSLObject)
  private
    FScript: String;
    FPort: word;
    FEndPoint: TFhirWebServerEndpoint;
    procedure SetEndPoint(const Value: TFhirWebServerEndpoint);
  public
    property port : word read FPort write FPort;
    property endPoint : TFhirWebServerEndpoint read FEndPoint write SetEndPoint;
    property script : String read FScript write FScript;

    procedure start;
    procedure stop;
  end;

implementation

{ Tv2ServerInstance }

procedure Tv2ServerInstance.SetEndPoint(const Value: TFhirWebServerEndpoint);
begin
  FEndPoint := Value;
end;

procedure Tv2ServerInstance.start;
begin

end;

procedure Tv2ServerInstance.stop;
begin

end;

end.
