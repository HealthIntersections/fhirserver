unit ftk_terminology_service;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  fsl_base, fsl_utilities,
  fhir_objects, fhir_client,
  fhir4_factory, fhir4_resources, fhir4_utilities;

type

  { TToolkitTerminologyService }

  TToolkitTerminologyService = class (TFHIRTerminologyService)
  private
    FClient : TFHIRClientV;
    FCacheFile : String;
    FCache : TFslStringDictionary;
    FNextSave : TDateTime;

    procedure loadCache;
    procedure saveCache;
    procedure checkSaveCache;
  public
    constructor create(url, log, cache : String);
    destructor Destroy; override;

    function lookupCode(system_, code : String) : String; override;
  end;

implementation

{ TToolkitTerminologyService }

procedure TToolkitTerminologyService.loadCache;
var
  f : System.text;
  s, n, v : String;
begin
  if FileExists(FCacheFile) then
    try
      AssignFile(f, FCacheFile);
      reset(f);
      while not eof(f) do
      begin
        readln(f, s);
        StringSplit(s, '=', n, v);
        FCache.AddOrSetValue(n.trim, v.trim);
      end;
      closeFile(f);
    except
    end;
end;

procedure TToolkitTerminologyService.saveCache;
var
  f : System.text;
  s : String;
begin
  AssignFile(f, FCacheFile);
  rewrite(f);
  for s in FCache.Keys do
    writeln(f, s+' = '+FCache[s]);
  closeFile(f);
  FNextSave := now + 60 * DATETIME_SECOND_ONE;
end;

procedure TToolkitTerminologyService.checkSaveCache;
begin
  if FNextSave < now then
    saveCache;
end;

constructor TToolkitTerminologyService.create(url, log, cache: String);
var
  factory : TFHIRFactoryR4;
begin
  inherited create;
  FCache := TFslStringDictionary.create;
  FCacheFile := cache;
  loadCache;

  factory := TFHIRFactoryR4.create;
  try
    FClient := factory.makeClient(nil, url, fctCrossPlatform, ffJson, 5000);
    if (log <> '') then
      FClient.Logger := TTextFileLogger.create(log);
  finally
    factory.free;
  end;
end;

destructor TToolkitTerminologyService.Destroy;
begin
  saveCache;
  FCache.Free;
  FClient.Free;
  inherited Destroy;
end;

function TToolkitTerminologyService.lookupCode(system_, code: String): String;
var
  pin, pout : TFhirParameters;
begin
  if (FCache.containsKey(system_+'|'+code)) then
    exit(FCache[system_+'|'+code]);

  pin := TFhirParameters.create;
  try
    pin.AddParameter('url', system_);
    pin.AddParameter('code', code);
    pout := FClient.operationV('CodeSystem', 'validate-code', pin) as TFhirParameters;
    try
      result := pout.str['display'];
      FCache.AddOrSetValue(system_+'|'+code, result);
      checkSaveCache;
    finally
      pout.free;
    end;
  finally
    pin.free;
  end;
end;

end.

