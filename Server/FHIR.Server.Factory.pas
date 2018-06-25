unit FHIR.Server.Factory;

interface

uses
  FHIR.Support.Base,
  FHIR.Base.Factory,
  FHIR.Javascript.Base,
  FHIR.Server.Indexing, FHIR.Server.Subscriptions;

{
the main purpose of the server factory is to manage version dependencies
}
type
  TFHIRServerFactory = class abstract (TFHIRBaseServerFactory)
  public
    function link : TFHIRServerFactory; overload;
    function makeValidator: TFHIRValidatorV; virtual; abstract;
    function makeIndexer : TFHIRIndexManager; virtual; abstract;
    function makeSubscriptionManager(ServerContext : TFslObject) : TSubscriptionManager; virtual; abstract;

    procedure registerJs(js : TFHIRJavascript); virtual; abstract;
    procedure setTerminologyServer(validatorContext : TFHIRWorkerContextWithFactory; server : TFslObject{TTerminologyServer}); virtual; abstract;
  end;

implementation

{ TFHIRServerFactory }

function TFHIRServerFactory.link: TFHIRServerFactory;
begin
  result := TFHIRServerFactory(inherited link);
end;

end.
