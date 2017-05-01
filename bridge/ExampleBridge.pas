{
This unit shows an example of how to integrate the FHIR Server into another
application server. It instantiates a FHIR Server, and provides storage
to allow the FHIR server to expose application functionality.

This example FHIR server pretends to provide meaningful patient services
}

unit ExampleBridge;

interface

Uses
  IniFiles,
  FHIRTypes, FHIRResources, FHIRConstants,
  FHIRServerContext, FHIRStorageService, FHIRRestServer;

Type
  TExampleFhirServerStorage = class (TFHIRStorageService)
  private
  public
  end;

  TExampleFhirServer = class
  private
    FIni : TMemIniFile;
    FIPMask: String;
    FPort: word;
    FIPClient: String;
    FDataModuleMain: TObject;
    FSystemName : String;

    FWebServer : TFhirWebServer;
  public
    Constructor Create;
    Destructor Destroy; Override;

    Property Port: word read FPort write FPort;
    Property IPClient : String read FIPClient write FIPCLient;
    Property IPMask : String read FIPMask write FIPMask;
    Property DataModuleMain : TObject read FDataModuleMain write FDataModuleMain;
    Property SystemName : String read FSystemName write FSystemName;

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

end.
