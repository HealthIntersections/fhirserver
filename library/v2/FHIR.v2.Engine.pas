unit FHIR.v2.Engine;

interface

uses
  SysUtils, Classes,
  FHIR.Support.Base, FHIR.Support.Utilities,
  FHIR.Javascript, FHIR.Support.Javascript, FHIR.Javascript.Base,
  FHIR.v2.Message, FHIR.v2.Javascript,
  FHIR.Client.Base, FHIR.Base.PathEngine, FHIR.Base.Factory;

type
  TV2ConversionEngineScript = class (TFslObject)
  private
    FName: String;
    FContent: String;
  public
    property name : String read FName write FName;
    property content : String read FContent write FContent;
  end;

  TV2ConversionEngine = class;
  TV2ConversionEngineGetScriptEvent = function (engine : TV2ConversionEngine; name : String) : TV2ConversionEngineScript of object;

  TV2ConversionEngine = class (TFSLObject)
  private
    FMessage: TV2Message;
    FScriptName: String;
    FClient: TFhirClientV;
    FFactory: TFHIRFactory;
    FJSFactory: TRegisterFHIRTypes;
    FOnGetScript: TV2ConversionEngineGetScriptEvent;
    procedure SetClient(const Value: TFhirClientV);
    procedure SetMessage(const Value: TV2Message);
    procedure SetFactory(const Value: TFHIRFactory);
  public
    destructor Destroy; override;
    function Link : TV2ConversionEngine; overload;

    property message : TV2Message read FMessage write SetMessage;
    property client : TFhirClientV read FClient write SetClient;
    property factory : TFHIRFactory read FFactory write SetFactory;
    property jsFactory : TRegisterFHIRTypes read FJSFactory write FJSFactory;
    property scriptName : String read FScriptName write FScriptName;
    property onGetScript : TV2ConversionEngineGetScriptEvent read FOnGetScript write FOnGetScript;

    procedure execute;
  end;

implementation

{ TV2ConversionEngine }

destructor TV2ConversionEngine.Destroy;
begin
  FClient.free;
  FMessage.Free;
  FFactory.Free;

  inherited;
end;

function TV2ConversionEngine.Link: TV2ConversionEngine;
begin
  result := TV2ConversionEngine(inherited Link);
end;

procedure TV2ConversionEngine.SetClient(const Value: TFhirClientV);
begin
  FClient.free;
  FClient := Value;
end;

procedure TV2ConversionEngine.SetFactory(const Value: TFHIRFactory);
begin
  FFactory.Free;
  FFactory := Value;
end;

procedure TV2ConversionEngine.SetMessage(const Value: TV2Message);
begin
  FMessage.Free;
  FMessage := Value;
end;

procedure TV2ConversionEngine.execute;
var
  js : TFHIRJavascript;
  fpe : TFHIRPathEngineV;
  script : TV2ConversionEngineScript;
begin
  fpe := factory.makePathEngine(client.Worker.link, nil);
  try
    fpe.registerExtension(TV2FHIRPathExtensions.create);
    js := TFHIRJavascript.Create('ChakraCore.dll', nil, FJSFactory);
    try
      TV2JavascriptHelper.registerv2Objects(js, fpe);
      script := FOnGetScript(self, scriptName);
      try
        js.execute(script.content, '', script.name, [message, client, factory]);
      finally
        script.Free;
      end;
    finally
      js.free;
    end;
  finally
    fpe.free;
  end;
end;


end.
